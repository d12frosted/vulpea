;;; vulpea-db-sync.el --- File watching and async updates -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; File watching and async update system for Vulpea v2.
;;
;; This module provides:
;; - File watchers using filenotify
;; - Async update queue with batching
;; - Debouncing for rapid changes
;; - Idle timer processing
;; - Dual-mode support (async vs sync)
;;
;; Design:
;; - Non-blocking: UI never waits for database updates
;; - Handles external changes: git pulls, sync tools, etc.
;; - Batch processing: 100 file changes = 1 transaction
;; - Configurable delays and thresholds
;;
;;; Code:

(require 'filenotify)
(require 'seq)
(require 'vulpea-db)
(require 'vulpea-db-extract)

;;; Customization

(defgroup vulpea-db-sync nil
  "File watching and synchronization for Vulpea."
  :group 'vulpea)

(defcustom vulpea-db-sync-batch-delay 0.01
  "Delay in seconds before processing batched updates.
After last file change, wait this long before updating database.
This allows batching multiple changes into single transaction."
  :type 'number
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-idle-delay 0.5
  "Idle delay in seconds for processing updates.
Process updates when Emacs has been idle for this duration."
  :type 'number
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-batch-size 100
  "Maximum number of files to process in single batch.
Limits memory usage and transaction size."
  :type 'integer
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-directories (list org-directory)
  "List of directories to watch for file changes.
Recursively watches these directories for .org files.
Defaults to `org-directory'."
  :type '(repeat directory)
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-external-method 'auto
  "Method to use for detecting external file changes.

Possible values:
- `auto' - try fswatch first, fallback to polling
- `fswatch' - use only fswatch (error if not available)
- `poll' - use only polling
- nil - rely only on filenotify (unreliable for external changes)"
  :type '(choice (const :tag "Automatic (fswatch or poll)" auto)
          (const :tag "FSWatch only" fswatch)
          (const :tag "Polling only" poll)
          (const :tag "Filenotify only (unreliable)" nil))
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-poll-interval 2
  "Interval in seconds for polling external changes.

Only used when `vulpea-db-sync-external-method' is `poll' or
`auto' (when fswatch is not available)."
  :type 'number
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-progress-interval 100
  "Number of files to process before reporting progress.
When syncing a directory, report progress every N files.
Set to nil to disable progress reporting.
Set to a smaller value (e.g., 100) for more frequent updates,
or a larger value (e.g., 1000) for less frequent updates."
  :type '(choice (const :tag "Disabled" nil)
          (integer :tag "Report every N files"))
  :group 'vulpea-db-sync)

(defcustom vulpea-db-sync-scan-on-enable nil
  "Whether to scan all files when enabling autosync mode.

This initial scan detects changes made while Emacs was closed (e.g.,
from git pulls, Dropbox sync, or external edits).

Options:
- nil: Skip initial scan (fast startup, manual sync when needed)
- `async': Scan asynchronously (may cause lag during processing)
- `blocking': Scan synchronously (blocks Emacs until complete)

Recommended: nil for large repositories (10000+ notes), then use
`vulpea-db-sync-full-scan' manually after external changes."
  :type '(choice (const :tag "Skip scan (fast startup)" nil)
          (const :tag "Async scan (may lag)" async)
          (const :tag "Blocking scan (wait for completion)" blocking))
  :group 'vulpea-db-sync)

;;; Variables

(defvar vulpea-db-sync--watchers nil
  "Alist of (path . descriptor) for active file watchers.")

(defvar vulpea-db-sync--queue nil
  "Queue of files pending database update.
Each entry is (path . timestamp).")

(defvar vulpea-db-sync--queue-tail nil
  "Tail pointer for the pending queue list.")

(defvar vulpea-db-sync--queue-set (make-hash-table :test 'equal)
  "Hash table tracking files already queued.")

(defvar vulpea-db-sync--timer nil
  "Timer for processing batched updates.")

(defvar vulpea-db-sync--idle-timer nil
  "Idle timer for processing updates.")

(defvar vulpea-db-sync--processing nil
  "Non-nil when currently processing updates.")

(defvar vulpea-db-sync--fswatch-process nil
  "Process handle for fswatch external monitoring.")

(defvar vulpea-db-sync--fswatch-buffer ""
  "Buffer for incomplete fswatch output lines.")

(defvar vulpea-db-sync--poll-timer nil
  "Timer for polling-based external monitoring.")

(defvar vulpea-db-sync--poll-scan-in-progress nil
  "Non-nil when an async poll scan subprocess is running.")

(defvar vulpea-db-sync--file-attributes (make-hash-table :test 'equal)
  "Cache of file attributes for external change detection.")

(defvar vulpea-db-sync--queue-total 0
  "Total number of files queued for async processing.
Set when async processing starts, used for progress reporting.")

(defvar vulpea-db-sync--processed-total 0
  "Total number of files processed in current async batch.
Reset when async processing completes.")

(defvar vulpea-db-sync--updated-total 0
  "Total number of files actually updated in current async batch.
Reset when async processing completes.")

(defvar vulpea-db-sync--sync-start-time nil
  "Start time of current sync phase.")

(defvar vulpea-db-sync-debug nil
  "When non-nil, log timing information for sync operations.")

;;; Mode

;;;###autoload
(define-minor-mode vulpea-db-autosync-mode
  "Toggle automatic database synchronization.

When enabled:
- Watch org files for changes
- Update database asynchronously
- Batch multiple changes
- Process during idle time

When disabled:
- Stop all file watchers
- Clear update queue
- Updates must be triggered manually"
  :global t
  :group 'vulpea-db-sync
  (if vulpea-db-autosync-mode
      (vulpea-db-sync--start)
    (vulpea-db-sync--stop)))

;;; Utilities

(defun vulpea-db-sync--escape-glob-pattern (str)
  "Escape special SQLite GLOB characters in STR.
Escapes *, ?, and [ so they match literally when used with GLOB.
These are escaped by wrapping in brackets: * -> [*], ? -> [?], [ -> [[]]."
  (replace-regexp-in-string
   "[][*?]"
   (lambda (m) (format "[%s]" m))
   str))

;;; Core Functions

(defun vulpea-db-sync--start ()
  "Start file watching and async update.

Optionally performs initial scan based on `vulpea-db-sync-scan-on-enable'.

When `vulpea-db-sync-scan-on-enable' is `async', this function
returns immediately without blocking.  File listing, cleanup of
deleted files, and enqueueing are all performed asynchronously via
a subprocess.  The `blocking' mode still scans synchronously."
  (let ((t-total (current-time))
        t-phase)

    ;; Kill stale scan subprocess from previous activation
    (when-let* ((proc (get-process "vulpea-scan")))
      (delete-process proc))

    ;; Start idle timer immediately (so queue is ready to process)
    (unless vulpea-db-sync--idle-timer
      (setq vulpea-db-sync--idle-timer
            (run-with-idle-timer vulpea-db-sync-idle-delay t
                                 #'vulpea-db-sync--process-queue)))

    ;; Start external monitoring (fswatch is async, no blocking)
    (setq t-phase (current-time))
    (vulpea-db-sync--setup-external-monitoring)
    (when vulpea-db-sync-debug
      (message "[vulpea-sync] setup-external-monitoring: %.0fms"
               (* 1000 (float-time (time-subtract (current-time) t-phase)))))

    ;; Start watching directories via filenotify unless fswatch is
    ;; active.  When fswatch is running it already monitors the
    ;; filesystem for all changes (including in-Emacs saves), making
    ;; filenotify redundant.  Programmatic changes (vulpea-create,
    ;; vulpea-utils-with-note-sync) call vulpea-db-update-file
    ;; directly and never rely on filenotify.
    (setq t-phase (current-time))
    (let ((watcher-count 0))
      (if vulpea-db-sync--fswatch-process
          (when vulpea-db-sync-debug
            (message "[vulpea-sync] watch-directory: skipped (fswatch active)"))
        (when vulpea-db-sync-directories
          (dolist (dir vulpea-db-sync-directories)
            (vulpea-db-sync--watch-directory dir))
          (setq watcher-count (length vulpea-db-sync--watchers)))
        (when vulpea-db-sync-debug
          (message "[vulpea-sync] watch-directory: %.0fms (%d watchers)"
                   (* 1000 (float-time (time-subtract (current-time) t-phase)))
                   watcher-count))))

    ;; Initial scan and cleanup based on configuration
    (if (and vulpea-db-sync-scan-on-enable vulpea-db-sync-directories)
        (pcase vulpea-db-sync-scan-on-enable
          ('async
           ;; Use subprocess to list files, then cleanup + enqueue
           (when vulpea-db-sync-debug
             (message "[vulpea-sync] launching async scan subprocess..."))
           (let ((scan-start (current-time)))
             (vulpea-db-sync--scan-files-async
              vulpea-db-sync-directories
              (lambda (files)
                ;; Guard: skip if autosync was disabled while
                ;; subprocess was running
                (when vulpea-db-autosync-mode
                  (when vulpea-db-sync-debug
                    (message "[vulpea-sync] async scan found %d files in %.0fms"
                             (length files)
                             (* 1000 (float-time (time-subtract (current-time) scan-start)))))
                  ;; Cleanup: remove DB entries not in the file list
                  (let ((cleanup-start (current-time)))
                    (vulpea-db-sync--cleanup-deleted-files-using files)
                    (when vulpea-db-sync-debug
                      (message "[vulpea-sync] async cleanup: %.0fms"
                               (* 1000 (float-time (time-subtract (current-time) cleanup-start))))))
                  ;; Enqueue all found files for change detection
                  (let ((enqueue-start (current-time)))
                    (dolist (file files)
                      (vulpea-db-sync--enqueue file))
                    (when vulpea-db-sync-debug
                      (message "[vulpea-sync] async enqueue: %.0fms (%d files)"
                               (* 1000 (float-time (time-subtract (current-time) enqueue-start)))
                               (length files)))))))))
          ('blocking
           ;; Scan synchronously (blocks Emacs)
           (setq t-phase (current-time))
           (vulpea-db-sync--cleanup-deleted-files)
           (when vulpea-db-sync-debug
             (message "[vulpea-sync] cleanup-deleted-files: %.0fms"
                      (* 1000 (float-time (time-subtract (current-time) t-phase)))))
           (setq t-phase (current-time))
           (dolist (dir vulpea-db-sync-directories)
             (vulpea-db-sync-update-directory dir))
           (when vulpea-db-sync-debug
             (message "[vulpea-sync] blocking-scan: %.0fms"
                      (* 1000 (float-time (time-subtract (current-time) t-phase)))))))
      ;; No scan requested, but still cleanup deleted files
      (setq t-phase (current-time))
      (vulpea-db-sync--cleanup-deleted-files)
      (when vulpea-db-sync-debug
        (message "[vulpea-sync] cleanup-deleted-files: %.0fms"
                 (* 1000 (float-time (time-subtract (current-time) t-phase))))))

    (when vulpea-db-sync-debug
      (message "[vulpea-sync] start complete: %.0fms total (sync portion)"
               (* 1000 (float-time (time-subtract (current-time) t-total)))))))

(defun vulpea-db-sync--stop ()
  "Stop file watching and clear queue."
  ;; Remove all watchers
  (dolist (entry vulpea-db-sync--watchers)
    (file-notify-rm-watch (cdr entry)))
  (setq vulpea-db-sync--watchers nil)

  ;; Stop async scan subprocess if running
  (when-let* ((proc (get-process "vulpea-scan")))
    (delete-process proc))

  ;; Stop external monitoring
  (vulpea-db-sync--stop-external-monitoring)

  ;; Cancel timers
  (when vulpea-db-sync--timer
    (cancel-timer vulpea-db-sync--timer)
    (setq vulpea-db-sync--timer nil))
  (when vulpea-db-sync--idle-timer
    (cancel-timer vulpea-db-sync--idle-timer)
    (setq vulpea-db-sync--idle-timer nil))

  ;; Clear queue
  (setq vulpea-db-sync--queue nil
        vulpea-db-sync--queue-tail nil)
  (clrhash vulpea-db-sync--queue-set))

(defun vulpea-db-sync--watch-directory (dir)
  "Watch DIR and all subdirectories for org file change."
  (when (and dir (file-directory-p dir) (not (file-symlink-p dir)))
    (unless (assoc dir vulpea-db-sync--watchers)
      (let ((descriptor (file-notify-add-watch
                         dir
                         '(change)
                         #'vulpea-db-sync--file-notify-callback)))
        (push (cons dir descriptor) vulpea-db-sync--watchers)))

    ;; Recursively watch subdirectories
    (dolist (subdir (directory-files dir t "\\`[^.]" t))
      (when (and (file-directory-p subdir)
                 (not (file-symlink-p subdir))
                 (not (string-match-p "/\\.git/" subdir)))
        (vulpea-db-sync--watch-directory subdir)))))

(defun vulpea-db-sync--watch-file (path)
  "Watch file at PATH for change."
  (unless (assoc path vulpea-db-sync--watchers)
    (when (file-exists-p path)
      (let ((descriptor (file-notify-add-watch
                         path
                         '(change)
                         #'vulpea-db-sync--file-notify-callback)))
        (push (cons path descriptor) vulpea-db-sync--watchers)))))

(defun vulpea-db-sync--unwatch-file (path)
  "Stop watching file at PATH."
  (when-let* ((entry (assoc path vulpea-db-sync--watchers)))
    (file-notify-rm-watch (cdr entry))
    (setq vulpea-db-sync--watchers
          (delq entry vulpea-db-sync--watchers))))

(defun vulpea-db-sync--org-file-p (path)
  "Return non-nil when PATH points to a tracked org file.

Excludes:
- Files not matching tracked extensions
- Files in hidden directories (paths containing /.)"
  (and path
       (seq-some (lambda (ext) (string-suffix-p ext path))
                 (vulpea-db--all-extensions))
       (not (string-match-p "/\\." path))))

(defun vulpea-db-sync--list-org-files (dir)
  "List all tracked org files in DIR recursively.

Uses `vulpea-db-sync--org-file-p' to filter files, ensuring
consistency with file watcher filtering.

DIR is expanded via `expand-file-name' to ensure returned paths
are absolute (e.g., ~/notes becomes /home/user/notes).  This
keeps paths consistent with `vulpea-db-sync--scan-files-async'
and prevents tilde-vs-absolute mismatches in database queries."
  (let ((dir (expand-file-name dir))
        (regex (mapconcat (lambda (ext)
                            (concat (regexp-quote ext) "\\'"))
                          (vulpea-db--all-extensions)
                          "\\|")))
    (seq-filter #'vulpea-db-sync--org-file-p
                (directory-files-recursively dir regex))))

(defun vulpea-db-sync--scan-files-async (dirs callback)
  "List org files in DIRS asynchronously, call CALLBACK with file list.

Uses fd (or find as fallback) subprocess to avoid blocking Emacs.
CALLBACK receives a list of absolute file paths."
  (let* ((buffer "")
         (dir (car dirs))
         (expanded-dir (expand-file-name dir))
         (extensions (vulpea-db--all-extensions))
         (cmd (if (executable-find "fd")
                  (append (list "fd" "--type" "f")
                          (mapcan (lambda (ext)
                                    (list "--extension" (substring ext 1)))
                                  extensions)
                          (list "--hidden" "--no-ignore"
                                "--exclude" ".*"
                                "." expanded-dir))
                (let* ((name-args
                        (mapcar (lambda (ext)
                                  (list "-name" (concat "*" ext)))
                                extensions))
                       (name-clause
                        (cl-loop for args in name-args
                                 for first = t then nil
                                 append (if first args (cons "-o" args)))))
                  (append (list "find" expanded-dir "-type" "f")
                          (if (> (length name-args) 1)
                              (append '("(") name-clause '(")"))
                            name-clause)
                          (list "-not" "-path" "*/.*"))))))
    (make-process
     :name "vulpea-scan"
     :command cmd
     :connection-type 'pipe
     :noquery t
     :filter (lambda (_proc output)
               (setq buffer (concat buffer output)))
     :sentinel (lambda (_proc event)
                 (when (string-prefix-p "finished" event)
                   (let ((files (seq-filter
                                 #'vulpea-db-sync--org-file-p
                                 (split-string buffer "\n" t))))
                     (if (cdr dirs)
                         ;; More directories to scan
                         (vulpea-db-sync--scan-files-async
                          (cdr dirs)
                          (lambda (more-files)
                            (funcall callback (append files more-files))))
                       (funcall callback files))))))))

(defun vulpea-db-sync--drop-from-queue (path)
  "Remove PATH from the pending queue."
  (let (result)
    (dolist (entry vulpea-db-sync--queue)
      (unless (equal (car entry) path)
        (push entry result)))
    (setq vulpea-db-sync--queue (nreverse result))
    (setq vulpea-db-sync--queue-tail (last vulpea-db-sync--queue))
    (remhash path vulpea-db-sync--queue-set)))

(defun vulpea-db-sync--handle-removed-file (path)
  "Permanently remove PATH from database tracking.

PATH can be a file or directory. If PATH is a directory (detected
by trailing slash or by having files under it in the database),
all files under that directory are removed."
  (vulpea-db-sync--drop-from-queue path)
  (vulpea-db-sync--unwatch-file path)
  (let ((db (vulpea-db)))
    (emacsql-with-transaction db
      ;; Try exact match first
      (vulpea-db--delete-file-notes path)
      (emacsql db [:delete :from files :where (= path $s1)] path)
      ;; Also handle directory removal - delete all files under this path
      ;; This handles the case when a directory is deleted externally
      ;; Use GLOB instead of LIKE - GLOB uses * and ? as wildcards,
      ;; which avoids issues with % and _ in directory names
      (let* ((dir-prefix (if (string-suffix-p "/" path)
                             path
                           (concat path "/")))
             ;; Escape GLOB special characters: *, ?, [
             (escaped-prefix (vulpea-db-sync--escape-glob-pattern dir-prefix))
             (glob-pattern (concat escaped-prefix "*")))
        (dolist (file-path (mapcar #'car
                                   (emacsql db [:select path :from files
                                                :where (glob path $s1)]
                                            glob-pattern)))
          (vulpea-db--delete-file-notes file-path))
        (emacsql db [:delete :from files :where (glob path $s1)]
                 glob-pattern)))))

(defun vulpea-db-sync--file-notify-callback (event)
  "Handle file notification EVENT."
  (pcase-let ((`(,_descriptor ,action ,file . ,rest) event))
    (pcase action
      ((or 'changed 'created 'attribute-changed)
       (cond
        ((and file (file-directory-p file) (eq action 'created))
         (vulpea-db-sync--watch-directory file))
        ((vulpea-db-sync--org-file-p file)
         (vulpea-db-sync--enqueue file))))
      ('deleted
       (when (vulpea-db-sync--org-file-p file)
         (vulpea-db-sync--handle-removed-file file)))
      ('renamed
       (pcase rest
         (`(,new-path)
          (when (vulpea-db-sync--org-file-p file)
            (vulpea-db-sync--handle-removed-file file))
          (when (vulpea-db-sync--org-file-p new-path)
            (vulpea-db-sync--enqueue new-path))))))
    nil))

(defun vulpea-db-sync--enqueue (path)
  "Add PATH to update queue."
  (let ((timestamp (float-time)))
    (unless (gethash path vulpea-db-sync--queue-set)
      (puthash path t vulpea-db-sync--queue-set)

      ;; Add new entry at the tail to maintain FIFO order
      (let ((node (list (cons path timestamp))))
        (if vulpea-db-sync--queue
            (setcdr vulpea-db-sync--queue-tail node)
          (setq vulpea-db-sync--queue node))
        (setq vulpea-db-sync--queue-tail node))

      ;; Reset batch timer
      (when vulpea-db-sync--timer
        (cancel-timer vulpea-db-sync--timer))
      (setq vulpea-db-sync--timer
            (run-with-timer vulpea-db-sync-batch-delay nil
                            #'vulpea-db-sync--process-queue))

      ;; When a sync is already in progress, account for newly discovered files
      (when (> vulpea-db-sync--queue-total 0)
        (setq vulpea-db-sync--queue-total
              (1+ vulpea-db-sync--queue-total))))))

(defun vulpea-db-sync--process-queue ()
  "Process queued file update."
  (when (and vulpea-db-sync--queue
             (not vulpea-db-sync--processing))
    (setq vulpea-db-sync--processing t)
    (unwind-protect
        (let* ((vulpea-db-sync--batch-start-time (current-time))
               (batch-size (min (length vulpea-db-sync--queue)
                                vulpea-db-sync-batch-size))
               (batch (seq-take vulpea-db-sync--queue batch-size))
               (paths (mapcar #'car batch))
               (db (vulpea-db))
               (updated 0)
               (unchanged 0))

          ;; Initialize totals if starting fresh
          (when (zerop vulpea-db-sync--processed-total)
            (setq vulpea-db-sync--queue-total (length vulpea-db-sync--queue)
                  vulpea-db-sync--updated-total 0
                  vulpea-db-sync--sync-start-time (current-time))
            (when (> vulpea-db-sync--queue-total 1)
              (message "Vulpea: Syncing %d file%s..."
                       vulpea-db-sync--queue-total
                       (if (= vulpea-db-sync--queue-total 1) "" "s"))))

          ;; Remove processed items from queue
          (setq vulpea-db-sync--queue
                (seq-drop vulpea-db-sync--queue batch-size))
          (unless vulpea-db-sync--queue
            (setq vulpea-db-sync--queue-tail nil))
          (dolist (path paths)
            (remhash path vulpea-db-sync--queue-set))

          ;; Fetch all file hashes in one query (huge speedup)
          (let* ((hash-rows (emacsql db [:select [path hash mtime size] :from files
                                         :where (in path $v1)]
                                     (vconcat paths)))
                 (hash-cache (make-hash-table :test 'equal)))
            ;; Build hash table for O(1) lookups
            (dolist (row hash-rows)
              (puthash (elt row 0)
                       (list :hash (elt row 1)
                             :mtime (elt row 2)
                             :size (elt row 3))
                       hash-cache))

            ;; Process in single transaction
            (emacsql-with-transaction db
              (dolist (path paths)
                (condition-case err
                    (when (file-exists-p path)
                      (if (vulpea-db-sync--update-file-if-changed path hash-cache)
                          (setq updated (1+ updated))
                        (setq unchanged (1+ unchanged))))
                  (error
                   (message "Vulpea: Error updating %s: %s"
                            path (error-message-string err)))))))

          ;; Update totals
          (setq vulpea-db-sync--processed-total (+ vulpea-db-sync--processed-total updated unchanged)
                vulpea-db-sync--updated-total (+ vulpea-db-sync--updated-total updated))

          (when vulpea-db-sync-debug
            (message "[vulpea-sync] batch: %.0fms (%d files, %d updated, %d unchanged)"
                     (* 1000 (float-time (time-subtract (current-time) vulpea-db-sync--batch-start-time)))
                     batch-size updated unchanged))

          ;; Report progress
          (when (and vulpea-db-sync-progress-interval
                     (> vulpea-db-sync--queue-total vulpea-db-sync-progress-interval)
                     (or (zerop (mod vulpea-db-sync--processed-total
                                     vulpea-db-sync-progress-interval))
                         (null vulpea-db-sync--queue)))
            (message "Vulpea: Progress: %d/%d files (%d updated, %d unchanged)"
                     vulpea-db-sync--processed-total
                     vulpea-db-sync--queue-total
                     vulpea-db-sync--updated-total
                     (- vulpea-db-sync--processed-total vulpea-db-sync--updated-total)))

          ;; Final summary when done
          (when (null vulpea-db-sync--queue)
            (when (> vulpea-db-sync--processed-total 0)
              (let ((duration (when vulpea-db-sync--sync-start-time
                                (float-time (time-subtract (current-time)
                                                           vulpea-db-sync--sync-start-time)))))
                (message "Vulpea: Sync complete - %d file%s (%d updated, %d unchanged%s)"
                         vulpea-db-sync--processed-total
                         (if (= vulpea-db-sync--processed-total 1) "" "s")
                         vulpea-db-sync--updated-total
                         (- vulpea-db-sync--processed-total vulpea-db-sync--updated-total)
                         (if duration
                             (format ", %.2fs" duration)
                           ""))))
            ;; Reset counters
            (setq vulpea-db-sync--queue-total 0
                  vulpea-db-sync--processed-total 0
                  vulpea-db-sync--updated-total 0
                  vulpea-db-sync--sync-start-time nil)
            (clrhash vulpea-db-sync--queue-set)))

      (setq vulpea-db-sync--processing nil)

      ;; If queue still has items, schedule another processing
      (when vulpea-db-sync--queue
        (setq vulpea-db-sync--timer
              (run-with-timer vulpea-db-sync-batch-delay nil
                              #'vulpea-db-sync--process-queue))))))

(cl-defun vulpea-db-sync--update-file-if-changed (path &optional hash-cache)
  "Update database for PATH only if file has changed.

Uses content hash and mtime to detect changes.
HASH-CACHE is an optional hash table mapping paths to stored info.
If not provided, queries database directly (slower).
Returns t if file was updated, nil if unchanged, \\='deleted if file
no longer exists."
  (let ((attrs (file-attributes path)))
    (unless attrs
      ;; File was deleted between queue and processing
      (vulpea-db--delete-file-notes path)
      (cl-return-from vulpea-db-sync--update-file-if-changed 'deleted))
    (let* ((current-mtime (float-time (file-attribute-modification-time attrs)))
           (current-size (file-attribute-size attrs))
           (stored-info (if hash-cache
                            (gethash path hash-cache)
                          (vulpea-db--get-file-hash path))))

    (if (or (null stored-info)
            (not (equal (plist-get stored-info :mtime) current-mtime))
            (not (equal (plist-get stored-info :size) current-size)))
        ;; File changed, compute hash to verify
        (let ((current-hash (with-temp-buffer
                              (insert-file-contents path)
                              (secure-hash 'sha256 (buffer-string)))))
          (if (or (null stored-info)
                  (not (equal (plist-get stored-info :hash) current-hash)))
              ;; Hash differs, update database
              (progn
                (vulpea-db-update-file path)
                t)
            ;; Hash same, no update needed
            nil))
      ;; No change detected
      nil))))

;;; Cleanup

(defun vulpea-db-sync--cleanup-deleted-files ()
  "Remove database entries for files that no longer exist.

Returns count of removed files."
  (let* ((db (vulpea-db))
         (all-paths (mapcar #'car (emacsql db [:select path :from files])))
         (deleted 0))
    (emacsql-with-transaction db
      (dolist (path all-paths)
        (unless (file-exists-p path)
          (vulpea-db--delete-file-notes path)
          (emacsql db [:delete :from files :where (= path $s1)] path)
          (setq deleted (1+ deleted)))))
    (when (> deleted 0)
      (message "Vulpea: Removed %d deleted file%s from database"
               deleted (if (= deleted 1) "" "s")))
    deleted))

(defun vulpea-db-sync--cleanup-deleted-files-using (existing-files)
  "Remove DB entries for files not in EXISTING-FILES list.

EXISTING-FILES is a list of absolute paths known to exist on disk
\(typically from an fd/find subprocess).  This avoids per-file
`file-exists-p' calls by comparing against the known set.

Returns count of removed files."
  (let* ((db (vulpea-db))
         (existing-set (make-hash-table :test 'equal :size (length existing-files)))
         (all-paths (mapcar #'car (emacsql db [:select path :from files])))
         (deleted 0))
    ;; Build set of existing files for O(1) lookup
    (dolist (f existing-files)
      (puthash f t existing-set))
    (emacsql-with-transaction db
      (dolist (path all-paths)
        (unless (gethash path existing-set)
          (vulpea-db--delete-file-notes path)
          (emacsql db [:delete :from files :where (= path $s1)] path)
          (setq deleted (1+ deleted)))))
    (when (> deleted 0)
      (message "Vulpea: Removed %d deleted file%s from database"
               deleted (if (= deleted 1) "" "s")))
    deleted))

(defun vulpea-db-sync--cleanup-untracked-files ()
  "Remove database entries for files outside tracked directories.

Only files within `vulpea-db-sync-directories' should remain in
the database. Files that exist on disk but are outside these
directories will be removed.

This is useful when narrowing `vulpea-db-sync-directories' to a
subdirectory - files outside that subdirectory will be cleaned up.

Returns count of removed files."
  (if (null vulpea-db-sync-directories)
      0  ; No cleanup if no directories configured
    (let* ((db (vulpea-db))
           (all-paths (mapcar #'car (emacsql db [:select path :from files])))
           (removed 0)
           (tracked-dirs (mapcar #'expand-file-name vulpea-db-sync-directories)))
      (emacsql-with-transaction db
        (dolist (path all-paths)
          (let ((expanded-path (expand-file-name path)))
            (unless (seq-some (lambda (dir)
                                (file-in-directory-p expanded-path dir))
                              tracked-dirs)
              (vulpea-db--delete-file-notes path)
              (emacsql db [:delete :from files :where (= path $s1)] path)
              (setq removed (1+ removed))))))
      (when (> removed 0)
        (message "Vulpea: Removed %d untracked file%s from database"
                 removed (if (= removed 1) "" "s")))
      removed)))

;;; Manual Update

;;;###autoload
(defun vulpea-db-sync-full-scan (&optional arg)
  "Manually scan all sync directories for change.

This is useful when `vulpea-db-sync-scan-on-enable' is nil and you need
to detect external changes (e.g., after git pull, Dropbox sync).

ARG controls scan behavior:

Interactive use:
- No prefix: Smart detection (only updates changed files)
- \\[universal-argument]: FORCE scan (re-index everything)

Programmatic use:
- nil: Smart detection
- \\='force or non-nil: Force re-index

FORCE scan ignores change detection and re-indexes all files. Use FORCE
when changing configuration like `vulpea-db-index-heading-level'.

Examples:
  (vulpea-db-sync-full-scan)             ; smart detection
  (vulpea-db-sync-full-scan \\='force)   ; force re-index

Also performs cleanup of:
- Deleted files (no longer exist on disk)
- Untracked files (outside `vulpea-db-sync-directories')"
  (interactive "P")
  (unless vulpea-db-sync-directories
    (user-error "No sync directories configured. Set `vulpea-db-sync-directories'"))

  ;; Decode argument (handle both prefix args and symbols)
  (let ((force (or (eq arg 'force) arg)))

    ;; Cleanup deleted and untracked files
    (vulpea-db-sync--cleanup-deleted-files)
    (vulpea-db-sync--cleanup-untracked-files)

    ;; Scan directories
    (if force
        (progn
          (message "Vulpea: Starting FORCE scan (re-indexing all files)...")
          (dolist (dir vulpea-db-sync-directories)
            (vulpea-db-sync-update-directory dir 'force)))
      ;; Smart detection
      (dolist (dir vulpea-db-sync-directories)
        (vulpea-db-sync-update-directory dir)))))

(defun vulpea-db-sync-update-file (path)
  "Manually update database for file at PATH.

If autosync is enabled, queues the update asynchronously.
Otherwise, updates immediately."
  (interactive (list (buffer-file-name)))
  (if vulpea-db-autosync-mode
      (vulpea-db-sync--enqueue path)
    (vulpea-db-update-file path)))

(defun vulpea-db-sync-update-directory (dir &optional force)
  "Update database for all org files in DIR recursively.

Only updates files that have actually changed (by checking mtime/size/hash).
Files in hidden directories (paths containing /.) are excluded.

With optional FORCE argument, bypass change detection and re-index all files.
This is useful when changing configuration like `vulpea-db-index-heading-level'.

FORCE mode is always synchronous/blocking, even when `vulpea-db-autosync-mode'
is enabled."
  (interactive "DDirectory: ")
  (let ((files (vulpea-db-sync--list-org-files dir)))
    (if (and vulpea-db-autosync-mode (not force))
        ;; Async mode: queue all files (smart detection in queue processing)
        (progn
          ;; Reset counters for fresh sync
          (setq vulpea-db-sync--queue-total 0
                vulpea-db-sync--processed-total 0
                vulpea-db-sync--updated-total 0)
          (dolist (file files)
            (vulpea-db-sync--enqueue file)))
      ;; Sync mode: use smart detection or force
      (let ((db (vulpea-db))
            (updated 0)
            (unchanged 0)
            (total (length files))
            (processed 0))
        (if force
            (message "Vulpea: FORCE syncing %d file%s (re-indexing all)..."
                     total (if (= total 1) "" "s"))
          (message "Vulpea: Syncing %d file%s..." total (if (= total 1) "" "s")))

        ;; Fetch all file hashes in one query for smart detection (huge speedup)
        (let* ((hash-rows (unless force
                            (emacsql db [:select [path hash mtime size] :from files
                                         :where (in path $v1)]
                                     (vconcat files))))
               (hash-cache (when hash-rows
                             (let ((cache (make-hash-table :test 'equal)))
                               (dolist (row hash-rows)
                                 (puthash (elt row 0)
                                          (list :hash (elt row 1)
                                                :mtime (elt row 2)
                                                :size (elt row 3))
                                          cache))
                               cache))))
          (emacsql-with-transaction db
            (dolist (file files)
              (condition-case err
                  (if force
                      ;; Force mode: always update
                      (progn
                        (vulpea-db-update-file file)
                        (setq updated (1+ updated)))
                    ;; Smart detection mode with hash cache
                    (if (vulpea-db-sync--update-file-if-changed file hash-cache)
                        (setq updated (1+ updated))
                      (setq unchanged (1+ unchanged))))
                (error
                 (message "Vulpea: Error updating %s: %s"
                          file (error-message-string err))))
              (setq processed (1+ processed))
              ;; Report progress at intervals
              (when (and vulpea-db-sync-progress-interval
                         (> total vulpea-db-sync-progress-interval)
                         (zerop (mod processed vulpea-db-sync-progress-interval)))
                (message "Vulpea: Progress: %d/%d files (%d updated, %d unchanged)"
                         processed total updated unchanged))))
          (message "Vulpea: Checked %d file%s (%d updated, %d unchanged)"
                   (+ updated unchanged)
                   (if (= (+ updated unchanged) 1) "" "s")
                   updated
                   unchanged))))))

;;; External Monitoring

(defun vulpea-db-sync--setup-external-monitoring ()
  "Setup external file monitoring based on `vulpea-db-sync-external-method'."
  (pcase vulpea-db-sync-external-method
    ('auto
     (if (executable-find "fswatch")
         (vulpea-db-sync--setup-fswatch)
       (message "Vulpea: fswatch not found, using polling for external changes")
       (vulpea-db-sync--setup-polling)))
    ('fswatch
     (if (executable-find "fswatch")
         (vulpea-db-sync--setup-fswatch)
       (user-error "Vulpea: fswatch is not available. Install it or use 'auto or 'poll")))
    ('poll
     (vulpea-db-sync--setup-polling))
    (_ nil)))

(defun vulpea-db-sync--stop-external-monitoring ()
  "Stop all external file monitoring."
  ;; Stop fswatch process
  (when vulpea-db-sync--fswatch-process
    (when (process-live-p vulpea-db-sync--fswatch-process)
      (delete-process vulpea-db-sync--fswatch-process))
    (setq vulpea-db-sync--fswatch-process nil))

  ;; Clear fswatch buffer
  (setq vulpea-db-sync--fswatch-buffer "")

  ;; Stop polling timer
  (when vulpea-db-sync--poll-timer
    (cancel-timer vulpea-db-sync--poll-timer)
    (setq vulpea-db-sync--poll-timer nil))

  ;; Clear file attributes cache
  (clrhash vulpea-db-sync--file-attributes))

(defun vulpea-db-sync--setup-fswatch ()
  "Setup file monitoring using fswatch process."
  (when vulpea-db-sync-directories
    (let* ((expanded-dirs (mapcar #'expand-file-name vulpea-db-sync-directories))
           (valid-dirs nil)
           (invalid-dirs nil))
      ;; Separate valid and invalid directories
      (dolist (dir expanded-dirs)
        (if (file-directory-p dir)
            (push dir valid-dirs)
          (push dir invalid-dirs)))

      ;; Warn about invalid directories
      (when invalid-dirs
        (display-warning 'vulpea
                         (format "Ignoring non-existent directories: %s"
                                 (string-join (nreverse invalid-dirs) ", "))
                         :warning))

      ;; Only setup fswatch if there are valid directories
      (when valid-dirs
        (setq vulpea-db-sync--fswatch-process
              (make-process
               :name "vulpea-fswatch"
               :buffer (get-buffer-create "*vulpea-fswatch-debug*")
               :command `("fswatch"
                          "--recursive"
                          "--event=Updated"
                          "--event=Created"
                          "--event=Removed"
                          "--event=Renamed"          ; needed to detect moves (e.g., to trash)
                          "--exclude" "\\.#.*$"      ; exclude auto-save files
                          "--exclude" "#.*#$"        ; exclude backup files
                          "--exclude" ".*~$"         ; exclude backup files
                          "--exclude" "\\.git/"      ; exclude .git directory
                          "--format" "%p|||%f"      ; include event flag with ||| separator
                          ,@(nreverse valid-dirs))
               :filter #'vulpea-db-sync--fswatch-filter
               :sentinel #'vulpea-db-sync--fswatch-sentinel))
        (message "Vulpea: Started fswatch monitoring")))))

(defun vulpea-db-sync--fswatch-path-valid-p (path)
  "Return non-nil if PATH is within a watched directory."
  (and path
       (file-name-absolute-p path)
       (seq-some (lambda (dir)
                   (string-prefix-p (file-name-as-directory dir) path))
                 vulpea-db-sync-directories)))

(defun vulpea-db-sync--fswatch-filter (_proc output)
  "Process fswatch OUTPUT.

OUTPUT may contain multiple events separated by newlines.
Handles partial lines by buffering incomplete output."
  ;; Prepend any buffered incomplete line from previous call
  (setq output (concat vulpea-db-sync--fswatch-buffer output))

  ;; Split into lines, keeping track of whether output ends with newline
  (let* ((ends-with-newline (string-suffix-p "\n" output))
         (lines (split-string output "\n" t)))

    ;; If output doesn't end with newline, last line is incomplete - buffer it
    (if ends-with-newline
        (setq vulpea-db-sync--fswatch-buffer "")
      (setq vulpea-db-sync--fswatch-buffer (or (car (last lines)) ""))
      (setq lines (butlast lines)))

    ;; Process complete lines
    (dolist (line lines)
      (let* ((raw (string-trim line))
             (parts (split-string raw "|||" t))
             (file (car parts))
             (flags (cadr parts)))
        (when (and file
                   (not (string-empty-p file))
                   (vulpea-db-sync--fswatch-path-valid-p file)
                   (not (string-match-p "/\\.git/" file))
                   (not (string-match-p "/\\.#" file))
                   (not (string-match-p "#$" file))
                   (not (string-match-p "~$" file)))
          (cond
           ;; Explicit removal event
           ((and flags (string-match-p "Removed" flags))
            (vulpea-db-sync--handle-removed-file file))
           ;; File/directory no longer exists (e.g., moved to trash)
           ((not (file-exists-p file))
            (vulpea-db-sync--handle-removed-file file))
           ;; Directory created/renamed - scan for org files inside
           ((file-directory-p file)
            (dolist (org-file (vulpea-db-sync--list-org-files file))
              (vulpea-db-sync--enqueue org-file)))
           ;; Regular file change
           ((vulpea-db-sync--org-file-p file)
            (vulpea-db-sync--enqueue file))))))))

(defun vulpea-db-sync--fswatch-sentinel (proc event)
  "Handle fswatch PROC sentinel EVENT."
  (unless (process-live-p proc)
    (message "Vulpea: fswatch process died (%s), restarting..." (string-trim event))
    (setq vulpea-db-sync--fswatch-process nil)
    ;; Restart after a delay
    (run-at-time 2 nil #'vulpea-db-sync--setup-fswatch)))

(defun vulpea-db-sync--setup-polling ()
  "Setup polling-based external monitoring."
  (when vulpea-db-sync-directories
    (let* ((expanded-dirs (mapcar #'expand-file-name vulpea-db-sync-directories))
           (valid-dirs (seq-filter #'file-directory-p expanded-dirs))
           (invalid-dirs (seq-remove #'file-directory-p expanded-dirs)))
      ;; Warn about invalid directories
      (when invalid-dirs
        (display-warning 'vulpea
                         (format "Ignoring non-existent directories: %s"
                                 (string-join invalid-dirs ", "))
                         :warning))

      ;; Only setup polling if there are valid directories
      (when valid-dirs
        ;; Initialize file attributes cache
        (vulpea-db-sync--update-file-attributes-cache)
        ;; Start polling timer
        (setq vulpea-db-sync--poll-timer
              (run-with-timer vulpea-db-sync-poll-interval
                              vulpea-db-sync-poll-interval
                              #'vulpea-db-sync--check-external-changes))
        (message "Vulpea: Started polling for external changes every %s seconds"
                 vulpea-db-sync-poll-interval)))))

(defun vulpea-db-sync--update-file-attributes-cache ()
  "Update cache of file attributes for all org files."
  (dolist (dir vulpea-db-sync-directories)
    (when (file-directory-p dir)
      (dolist (file (vulpea-db-sync--list-org-files dir))
        (when (file-exists-p file)
          (puthash file (file-attributes file) vulpea-db-sync--file-attributes))))))

(defun vulpea-db-sync--check-external-changes ()
  "Check for externally modified files by comparing mtimes.

Uses fd (or find) subprocess to list files asynchronously, then
compares modification times in the callback.  Skips if a previous
scan is still in progress.

Detects three types of changes:
- New files: files not previously in the cache
- Modified files: files with changed modification time
- Deleted files: files in cache but no longer on disk"
  (unless vulpea-db-sync--poll-scan-in-progress
    (setq vulpea-db-sync--poll-scan-in-progress t)
    (vulpea-db-sync--scan-files-async
     vulpea-db-sync-directories
     (lambda (files)
       (unwind-protect
           (vulpea-db-sync--check-external-changes-with-files files)
         (setq vulpea-db-sync--poll-scan-in-progress nil))))))

(defun vulpea-db-sync--check-external-changes-with-files (files)
  "Compare FILES against cached attributes and enqueue modified ones.

FILES is a list of org file paths from async directory scan."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (file-exists-p file)
        (let ((curr-attr (file-attributes file))
              (cached-attr (gethash file vulpea-db-sync--file-attributes)))
          (puthash file curr-attr vulpea-db-sync--file-attributes)
          (puthash file t seen)
          (cond
           ;; New file - not in cache before
           ((not cached-attr)
            (vulpea-db-sync--enqueue file))
           ;; Modified file - mtime changed
           ((not (equal (file-attribute-modification-time curr-attr)
                        (file-attribute-modification-time cached-attr)))
            (vulpea-db-sync--enqueue file))))))
    ;; Detect deletions - files in cache but not seen on disk
    (let (removed)
      (maphash (lambda (path _)
                 (unless (gethash path seen)
                   (push path removed)))
               vulpea-db-sync--file-attributes)
      (dolist (path removed)
        (remhash path vulpea-db-sync--file-attributes)
        (vulpea-db-sync--handle-removed-file path)))))

;;; Debug/Testing

(defun vulpea-db-sync--test-scan (dir)
  "Test function to measure directory scan performance.
Returns list of files found in DIR."
  (let* ((files nil)
         (start-time (current-time))
         (buffer "")
         (process (start-process
                   "vulpea-find-test"
                   nil
                   "find" dir
                   "-type" "f"
                   "-name" "*.org"
                   "-print")))
    (set-process-filter
     process
     (lambda (_proc output)
       (setq buffer (concat buffer output))
       (let ((last-newline (string-match-p "\n[^\n]*\\'" buffer)))
         (when last-newline
           (let ((complete-part (substring buffer 0 (1+ last-newline))))
             (dolist (file (split-string complete-part "\n" t))
               (push file files))
             (setq buffer (substring buffer (1+ last-newline))))))))

    (set-process-sentinel
     process
     (lambda (_proc _event)
       (when (not (string-empty-p buffer))
         (push buffer files))
       (let ((duration (float-time (time-subtract (current-time) start-time))))
         (message "Test scan: %d files found in %.2fs" (length files) duration))))

    ;; Return process for async handling
    process))

;;; Provide

(provide 'vulpea-db-sync)
;;; vulpea-db-sync.el ends here
