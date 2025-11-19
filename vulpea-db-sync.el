;;; vulpea-db-sync.el --- File watching and async updates -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
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
Only used when `vulpea-db-sync-external-method' is `poll' or `auto'
(when fswatch is not available)."
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

This initial scan detects changes made while Emacs was closed
(e.g., from git pulls, Dropbox sync, or external edits).

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

(defvar vulpea-db-sync--poll-timer nil
  "Timer for polling-based external monitoring.")

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

(defvar vulpea-db-sync--cleanup-remaining nil
  "List of paths remaining to check during async cleanup.")

(defvar vulpea-db-sync--cleanup-timer nil
  "Timer for processing async cleanup batches.")

(defvar vulpea-db-sync--cleanup-type nil
  "Type of cleanup being performed: \\='deleted or \\='untracked.")

(defvar vulpea-db-sync--cleanup-callback nil
  "Callback to run after async cleanup completes.")

(defvar vulpea-db-sync--scan-dirs-remaining nil
  "List of directories remaining to scan asynchronously.")

(defvar vulpea-db-sync--scan-timer nil
  "Timer for async directory scanning.")

(defvar vulpea-db-sync--scan-callback nil
  "Callback to run after async directory scan completes.")

(defvar vulpea-db-sync--scan-canceled nil
  "Non-nil when the current async scan was explicitly canceled.")

(defvar vulpea-db-sync--scan-complete t
  "Non-nil when no directory scan is currently running.")

(defvar vulpea-db-sync--scan-files-found 0
  "Number of files found during current directory scan.")

(defvar vulpea-db-sync--cleanup-start-time nil
  "Start time of current cleanup phase.")

(defvar vulpea-db-sync--scan-start-time nil
  "Start time of current scan phase.")

(defvar vulpea-db-sync--sync-start-time nil
  "Start time of current sync phase.")

(defcustom vulpea-db-sync-cleanup-batch-size 500
  "Number of paths to check per cleanup batch.
Smaller values make Emacs more responsive but cleanup takes longer."
  :type 'integer
  :group 'vulpea-db-sync)

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

;;; Core Functions

(defun vulpea-db-sync--start ()
  "Start file watching and async updates.

Optionally performs initial scan based on `vulpea-db-sync-scan-on-enable'."
  ;; Clean up deleted files first
  (vulpea-db-sync--cleanup-deleted-files)

  ;; Initial scan based on configuration
  (when (and vulpea-db-sync-scan-on-enable vulpea-db-sync-directories)
    (pcase vulpea-db-sync-scan-on-enable
      ('async
       ;; Queue all files for async processing
       (message "Vulpea: Queueing files for async scan...")
       (dolist (dir vulpea-db-sync-directories)
         (dolist (file (directory-files-recursively dir "\\.org$"))
           (vulpea-db-sync--enqueue file))))
      ('blocking
       ;; Scan synchronously (blocks Emacs)
       (dolist (dir vulpea-db-sync-directories)
         (vulpea-db-sync-update-directory dir)))))

  ;; Start watching directories if configured
  (when vulpea-db-sync-directories
    (dolist (dir vulpea-db-sync-directories)
      (vulpea-db-sync--watch-directory dir)))

  ;; Start external monitoring (fswatch or polling)
  (vulpea-db-sync--setup-external-monitoring)

  ;; Start idle timer for processing
  (unless vulpea-db-sync--idle-timer
    (setq vulpea-db-sync--idle-timer
          (run-with-idle-timer vulpea-db-sync-idle-delay t
                               #'vulpea-db-sync--process-queue))))

(defun vulpea-db-sync--stop ()
  "Stop file watching and clear queue."
  ;; Remove all watchers
  (dolist (entry vulpea-db-sync--watchers)
    (file-notify-rm-watch (cdr entry)))
  (setq vulpea-db-sync--watchers nil)

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
  "Watch DIR and all subdirectories for org file changes."
  (when (and dir (file-directory-p dir) (not (file-symlink-p dir)))
    (unless (assoc dir vulpea-db-sync--watchers)
      (let ((descriptor (file-notify-add-watch
                         dir
                         '(change)
                         #'vulpea-db-sync--file-notify-callback)))
        (push (cons dir descriptor) vulpea-db-sync--watchers)))

    ;; Recursively watch subdirectories
    (dolist (subdir (directory-files dir t "^[^.]" t))
      (when (and (file-directory-p subdir)
                 (not (file-symlink-p subdir))
                 (not (string-match-p "/\\.git/" subdir)))
        (vulpea-db-sync--watch-directory subdir)))))

(defun vulpea-db-sync--watch-file (path)
  "Watch file at PATH for changes."
  (unless (assoc path vulpea-db-sync--watchers)
    (when (file-exists-p path)
      (let ((descriptor (file-notify-add-watch
                         path
                         '(change)
                         #'vulpea-db-sync--file-notify-callback)))
        (push (cons path descriptor) vulpea-db-sync--watchers)))))

(defun vulpea-db-sync--unwatch-file (path)
  "Stop watching file at PATH."
  (when-let ((entry (assoc path vulpea-db-sync--watchers)))
    (file-notify-rm-watch (cdr entry))
    (setq vulpea-db-sync--watchers
          (delq entry vulpea-db-sync--watchers))))

(defun vulpea-db-sync--org-file-p (path)
  "Return non-nil when PATH points to a tracked org file."
  (and path
       (string-suffix-p ".org" path)
       (not (string-match-p "/\\." path))))

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
  "Permanently remove PATH from database tracking."
  (vulpea-db-sync--drop-from-queue path)
  (vulpea-db-sync--unwatch-file path)
  (let ((db (vulpea-db)))
    (emacsql-with-transaction db
      (vulpea-db--delete-file-notes path)
      (emacsql db [:delete :from files :where (= path $s1)] path))))

(defun vulpea-db-sync--file-notify-callback (event)
  "Handle file notification EVENT."
  (pcase-let ((`(,descriptor ,action ,file . ,rest) event))
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

      ;; Reset batch timer (but not during async directory scan)
      (when vulpea-db-sync--scan-complete
        (when vulpea-db-sync--timer
          (cancel-timer vulpea-db-sync--timer))
        (setq vulpea-db-sync--timer
              (run-with-timer vulpea-db-sync-batch-delay nil
                              #'vulpea-db-sync--process-queue)))

      ;; When a sync is already in progress, account for newly discovered files
      (when (> vulpea-db-sync--queue-total 0)
        (setq vulpea-db-sync--queue-total
              (1+ vulpea-db-sync--queue-total))))))

(defun vulpea-db-sync--process-queue ()
  "Process queued file updates."
  (when (and vulpea-db-sync--queue
             (not vulpea-db-sync--processing))
    (setq vulpea-db-sync--processing t)
    (unwind-protect
        (let* ((batch-size (min (length vulpea-db-sync--queue)
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
              (if vulpea-db-sync--scan-complete
                  (message "Vulpea: Syncing %d file%s..."
                           vulpea-db-sync--queue-total
                           (if (= vulpea-db-sync--queue-total 1) "" "s"))
                (message "Vulpea: Syncing files (scan in progress)..."))))

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

          ;; Update total if scan still running (new files being discovered)
          (unless vulpea-db-sync--scan-complete
            (let ((current-total (+ vulpea-db-sync--processed-total (length vulpea-db-sync--queue))))
              (when (> current-total vulpea-db-sync--queue-total)
                (setq vulpea-db-sync--queue-total current-total))))

          ;; Report progress
          (when (and vulpea-db-sync-progress-interval
                     (> vulpea-db-sync--queue-total vulpea-db-sync-progress-interval)
                     (or (zerop (mod vulpea-db-sync--processed-total
                                     vulpea-db-sync-progress-interval))
                         (null vulpea-db-sync--queue)))
            (if vulpea-db-sync--scan-complete
                (message "Vulpea: Progress: %d/%d files (%d updated, %d unchanged)"
                         vulpea-db-sync--processed-total
                         vulpea-db-sync--queue-total
                         vulpea-db-sync--updated-total
                         (- vulpea-db-sync--processed-total vulpea-db-sync--updated-total))
              (message "Vulpea: Progress: %d files (%d updated, %d unchanged, scan in progress)"
                       vulpea-db-sync--processed-total
                       vulpea-db-sync--updated-total
                       (- vulpea-db-sync--processed-total vulpea-db-sync--updated-total))))

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

(defun vulpea-db-sync--update-file-if-changed (path &optional hash-cache)
  "Update database for PATH only if file has changed.

Uses content hash and mtime to detect changes.
HASH-CACHE is an optional hash table mapping paths to stored info.
If not provided, queries database directly (slower).
Returns t if file was updated, nil otherwise."
  (let* ((attrs (file-attributes path))
         (current-mtime (float-time (file-attribute-modification-time attrs)))
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
      nil)))

;;; Async Cleanup

(defun vulpea-db-sync--cleanup-async (type &optional callback)
  "Start async cleanup of TYPE (\\='deleted or \\='untracked).
Optionally run CALLBACK when complete."
  ;; Cancel any existing cleanup
  (when vulpea-db-sync--cleanup-timer
    (cancel-timer vulpea-db-sync--cleanup-timer)
    (setq vulpea-db-sync--cleanup-timer nil))

  (if (and (eq type 'untracked)
           (null vulpea-db-sync-directories))
      (progn
        (setq vulpea-db-sync--cleanup-remaining nil
              vulpea-db-sync--cleanup-type nil
              vulpea-db-sync--cleanup-callback nil)
        (when callback
          (funcall callback)))
    (let* ((db (vulpea-db))
           (all-paths (mapcar #'car (emacsql db [:select path :from files])))
           (start-time (current-time)))

      (message "Vulpea: Starting %s cleanup (%d paths)..."
               (if (eq type 'deleted) "deleted file" "untracked file")
               (length all-paths))

      ;; For deleted cleanup: use fast synchronous approach (find existing files, compare)
      (if (eq type 'deleted)
          (progn
            ;; Build set of all existing .org files
            (let* ((existing-files (make-hash-table :test 'equal))
                   (to-remove nil))
              ;; Fast: use find to get all existing files
              (dolist (dir vulpea-db-sync-directories)
                (when (file-directory-p dir)
                  (dolist (file (split-string
                                 (shell-command-to-string
                                  (format "find %s -type f -name '*.org' -print"
                                          (shell-quote-argument dir)))
                                 "\n" t))
                    (puthash file t existing-files))))

              ;; Find files in DB that don't exist
              (dolist (path all-paths)
                (unless (gethash path existing-files)
                  (push path to-remove)))

              ;; Remove in one transaction
              (when to-remove
                (emacsql-with-transaction db
                  (dolist (path to-remove)
                    (vulpea-db--delete-file-notes path)
                    (emacsql db [:delete :from files :where (= path $s1)] path))))

              (let ((duration (float-time (time-subtract (current-time) start-time))))
                (message "Vulpea: Deleted file cleanup complete (%d removed, %.2fs)"
                         (length to-remove) duration))

              (when callback
                (funcall callback))))

        ;; For untracked cleanup: also use fast synchronous approach
        (let* ((tracked-dirs (mapcar #'expand-file-name vulpea-db-sync-directories))
               (to-remove nil))
          ;; Check which paths are outside tracked directories
          (dolist (path all-paths)
            (let ((expanded-path (expand-file-name path)))
              (unless (seq-some (lambda (dir)
                                  (file-in-directory-p expanded-path dir))
                                tracked-dirs)
                (push path to-remove))))

          ;; Remove in one transaction
          (when to-remove
            (emacsql-with-transaction db
              (dolist (path to-remove)
                (vulpea-db--delete-file-notes path)
                (emacsql db [:delete :from files :where (= path $s1)] path))))

          (let ((duration (float-time (time-subtract (current-time) start-time))))
            (message "Vulpea: Untracked file cleanup complete (%d removed, %.2fs)"
                     (length to-remove) duration))

          (when callback
            (funcall callback)))))))

(defun vulpea-db-sync--cleanup-process-batch ()
  "Process one batch of cleanup checks."
  (if (null vulpea-db-sync--cleanup-remaining)
      ;; Done
      (progn
        (setq vulpea-db-sync--cleanup-timer nil)
        ;; Save type before clearing it
        (let ((cleanup-type vulpea-db-sync--cleanup-type)
              (duration (when vulpea-db-sync--cleanup-start-time
                          (float-time (time-subtract (current-time)
                                                     vulpea-db-sync--cleanup-start-time)))))
          (setq vulpea-db-sync--cleanup-type nil
                vulpea-db-sync--cleanup-start-time nil)
          (message "Vulpea: %s cleanup complete%s"
                   (if (eq cleanup-type 'deleted)
                       "Deleted file"
                     "Untracked file")
                   (if duration
                       (format " (%.2fs)" duration)
                     "")))
        (when vulpea-db-sync--cleanup-callback
          ;; Save callback and clear it BEFORE calling (callback might set a new one)
          (let ((callback vulpea-db-sync--cleanup-callback))
            (setq vulpea-db-sync--cleanup-callback nil)
            (condition-case err
                (funcall callback)
              (error
               (message "Vulpea: Error in cleanup callback: %s" (error-message-string err)))))))

    ;; Process one batch
    (let* ((batch-size (min (length vulpea-db-sync--cleanup-remaining)
                            vulpea-db-sync-cleanup-batch-size))
           (batch (seq-take vulpea-db-sync--cleanup-remaining batch-size))
           (db (vulpea-db))
           (type vulpea-db-sync--cleanup-type)
           (to-remove nil))

      ;; Check which paths should be removed (fast, outside transaction)
      (dolist (path batch)
        (when (cond
               ((eq type 'deleted)
                (not (file-exists-p path)))
               ((eq type 'untracked)
                (let ((expanded-path (expand-file-name path))
                      (tracked-dirs (mapcar #'expand-file-name
                                            vulpea-db-sync-directories)))
                  (not (seq-some (lambda (dir)
                                   (file-in-directory-p expanded-path dir))
                                 tracked-dirs)))))
          (push path to-remove)))

      ;; Remove them in one transaction (fast)
      (when to-remove
        (emacsql-with-transaction db
          (dolist (path to-remove)
            (vulpea-db--delete-file-notes path)
            (emacsql db [:delete :from files :where (= path $s1)] path)))
        (message "Vulpea: Cleanup progress: %d checked, %d removed"
                 batch-size (length to-remove)))

      ;; Update remaining and schedule next batch
      (setq vulpea-db-sync--cleanup-remaining
            (seq-drop vulpea-db-sync--cleanup-remaining batch-size))

      ;; Schedule next batch with idle timer (allows Emacs to remain responsive)
      (setq vulpea-db-sync--cleanup-timer
            (run-with-idle-timer 0.1 nil #'vulpea-db-sync--cleanup-process-batch)))))

;;; Async Directory Scanning

(defun vulpea-db-sync--scan-directories-async (directories &optional callback)
  "Scan DIRECTORIES asynchronously, queueing files incrementally.
Optionally run CALLBACK when complete.

Calling with nil DIRECTORIES cancels any running scan."
  ;; Always cancel existing scan first (allows stopping scans)
  (when vulpea-db-sync--scan-timer
    (cancel-timer vulpea-db-sync--scan-timer)
    (setq vulpea-db-sync--scan-timer nil))

  ;; Clear queue and callback to prevent sentinel from continuing
  (setq vulpea-db-sync--scan-dirs-remaining nil
        vulpea-db-sync--scan-callback nil
        vulpea-db-sync--scan-complete t
        vulpea-db-sync--scan-start-time nil
        vulpea-db-sync--scan-files-found 0
        vulpea-db-sync--scan-canceled t)

  ;; Stop any running find process (sentinel will see nil queue and stop)
  (let ((proc (get-process "vulpea-find")))
    (when (process-live-p proc)
      (delete-process proc)))

  (if (null directories)
      ;; Canceled/no directories - just run callback
      (progn
        (message "Vulpea: Scan canceled or no directories")
        (when callback
          (funcall callback))
        (setq vulpea-db-sync--scan-canceled t))

    ;; Start new scan with new queue/callback
    (setq vulpea-db-sync--scan-dirs-remaining (copy-sequence directories)
          vulpea-db-sync--scan-callback callback
          vulpea-db-sync--scan-complete nil
          vulpea-db-sync--scan-files-found 0
          vulpea-db-sync--scan-start-time (current-time)
          vulpea-db-sync--scan-canceled nil)

    (message "Vulpea: Starting directory scan (%d %s)..."
             (length directories)
             (if (= (length directories) 1) "directory" "directories"))

    ;; Schedule first directory (returns immediately)
    (setq vulpea-db-sync--scan-timer
          (run-with-idle-timer 0.01 nil #'vulpea-db-sync--scan-next-directory))
    nil))

(defun vulpea-db-sync--scan-next-directory ()
  "Scan next directory and schedule next one."
  (if (null vulpea-db-sync--scan-dirs-remaining)
      ;; Done
      (progn
        (setq vulpea-db-sync--scan-timer nil
              vulpea-db-sync--scan-complete t)
        (let ((duration (when vulpea-db-sync--scan-start-time
                          (float-time (time-subtract (current-time)
                                                     vulpea-db-sync--scan-start-time))))
              (total-found (+ vulpea-db-sync--processed-total
                              (length vulpea-db-sync--queue))))
          (setq vulpea-db-sync--scan-files-found total-found)
          (message "Vulpea: Directory scan complete (%d file%s found%s)"
                   total-found
                   (if (= total-found 1) "" "s")
                   (if duration
                       (format ", %.2fs" duration)
                     "")))
        (setq vulpea-db-sync--scan-start-time nil)
        ;; Start processing queue after scan completes
        (when vulpea-db-sync--queue
          (setq vulpea-db-sync--timer
                (run-with-timer vulpea-db-sync-batch-delay nil
                                #'vulpea-db-sync--process-queue)))
        (when vulpea-db-sync--scan-callback
          ;; Save callback and clear it BEFORE calling
          (let ((callback vulpea-db-sync--scan-callback))
            (setq vulpea-db-sync--scan-callback nil
                  vulpea-db-sync--scan-complete t)
            (condition-case err
                (funcall callback)
              (error
               (message "Vulpea: Error in scan callback: %s" (error-message-string err)))))))

    ;; Scan one directory using async process
    (let ((dir (pop vulpea-db-sync--scan-dirs-remaining)))
      (if (not (file-directory-p dir))
          ;; Skip invalid directory, move to next
          (setq vulpea-db-sync--scan-timer
                (run-with-idle-timer 0.01 nil #'vulpea-db-sync--scan-next-directory))

        ;; Use fast synchronous scanning (fd is 37ms, find is 579ms - both acceptable)
        (message "Vulpea: Scanning %s..." dir)
        (let* ((start (current-time))
               (files (cond
                       ;; Prefer fd (15x faster than find)
                       ((executable-find "fd")
                        (split-string
                         (shell-command-to-string
                          (format "fd -t f -e org . %s" (shell-quote-argument dir)))
                         "\n" t))
                       ;; Fallback to find
                       ((executable-find "find")
                        (split-string
                         (shell-command-to-string
                          (format "find %s -type f -name '*.org' -print"
                                  (shell-quote-argument dir)))
                         "\n" t))
                       ;; Final fallback: Emacs built-in (slower but always works)
                       (t
                        (directory-files-recursively dir "\\.org$"))))
               (scan-duration (float-time (time-subtract (current-time) start))))
          (message "Vulpea: Scanned %s (%d files, %.2fs)" dir (length files) scan-duration)
          ;; Enqueue all files
          (dolist (file files)
            (vulpea-db-sync--enqueue file))
          ;; Process next directory unless scan was canceled
          (unless vulpea-db-sync--scan-canceled
            (vulpea-db-sync--scan-next-directory)))))))

;;; Sync Cleanup

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
  "Manually scan all sync directories for changes.

This is useful when `vulpea-db-sync-scan-on-enable' is nil and you
need to detect external changes (e.g., after git pull, Dropbox sync).

ARG controls scan behavior:

Interactive use (prefix arguments):
- No prefix: Synchronous scan with smart detection
- C-u: Asynchronous scan with smart detection
- C-u C-u: Synchronous FORCE scan (re-index everything)

Programmatic use (symbols):
- nil: Synchronous scan with smart detection
- \\='async: Asynchronous scan with smart detection
- \\='force: Synchronous FORCE scan (re-index everything)

FORCE scan ignores change detection and re-indexes all files.
Use FORCE when changing configuration like `vulpea-db-index-heading-level'.

Note: FORCE mode only works synchronously. Async FORCE would require
re-implementing the queue processor, and force re-index is typically
a one-time operation where blocking is acceptable.

Examples:
  (vulpea-db-sync-full-scan)        ; sync scan
  (vulpea-db-sync-full-scan \\='async) ; async scan
  (vulpea-db-sync-full-scan \\='force) ; force re-index (blocks)

Also performs cleanup of:
- Deleted files (no longer exist on disk)
- Untracked files (outside `vulpea-db-sync-directories')"
  (interactive "P")
  (unless vulpea-db-sync-directories
    (user-error "No sync directories configured. Set `vulpea-db-sync-directories'"))

  ;; Decode argument (handle both prefix args and symbols)
  (let* ((async (cond
                 ((symbolp arg) (eq arg 'async))
                 ((listp arg) (equal arg '(4)))))
         (force (cond
                 ((symbolp arg) (eq arg 'force))
                 ((listp arg) (equal arg '(16))))))

    ;; Validate: async and force are mutually exclusive
    (when (and async force)
      (user-error "Cannot combine async and force modes. Use 'force for synchronous re-index"))

    (cond
     (async
      ;; Async mode: cleanup and scan non-blocking
      (unless vulpea-db-autosync-mode
        (user-error "Async scan requires autosync-mode to be enabled"))

      ;; Track total operation start time
      (let ((operation-start (current-time)))
        ;; Chain async operations: deleted cleanup -> untracked cleanup -> directory scan
        (vulpea-db-sync--cleanup-async
         'deleted
         (lambda ()
           (vulpea-db-sync--cleanup-async
            'untracked
            (lambda ()
              (let ((cleanup-duration (float-time (time-subtract (current-time) operation-start))))
                (message "Vulpea: Total cleanup time: %.2fs" cleanup-duration))
              (vulpea-db-sync--scan-directories-async
               vulpea-db-sync-directories)))))))

     (t
      ;; Sync mode: cleanup synchronously (blocks but completes immediately)
      (vulpea-db-sync--cleanup-deleted-files)
      (vulpea-db-sync--cleanup-untracked-files)

      ;; Scan directories
      (cond
       (force
        ;; Force: re-index everything
        (message "Vulpea: Starting FORCE scan (re-indexing all files)...")
        (dolist (dir vulpea-db-sync-directories)
          (vulpea-db-sync-update-directory dir 'force)))

       (t
        ;; Smart detection
        (dolist (dir vulpea-db-sync-directories)
          (vulpea-db-sync-update-directory dir))))))))

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

With optional FORCE argument, bypass change detection and re-index all files.
This is useful when changing configuration like `vulpea-db-index-heading-level'.

FORCE mode is always synchronous/blocking, even when `vulpea-db-autosync-mode'
is enabled."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir "\\.org$")))
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

;;; Sync Mode

(defmacro vulpea-with-sync-db (&rest body)
  "Execute BODY with synchronous database updates.

Disables autosync, executes BODY, then processes all pending updates
synchronously before re-enabling autosync.

Use this for programmatic operations that create many notes."
  (declare (indent 0))
  `(let ((was-enabled vulpea-db-autosync-mode))
    (when was-enabled
     (vulpea-db-autosync-mode -1))
    (unwind-protect
        (progn ,@body)
      (when was-enabled
       ;; Process any pending updates synchronously
       (while vulpea-db-sync--queue
        (vulpea-db-sync--process-queue))
       (vulpea-db-autosync-mode +1)))))

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
                          "--exclude" "\\.#.*$"      ; exclude auto-save files
                          "--exclude" "#.*#$"        ; exclude backup files
                          "--exclude" ".*~$"         ; exclude backup files
                          "--exclude" "\\.git/"      ; exclude .git directory
                          "--format" "%p\0%f"        ; include event flag with NUL separator
                          ,@(nreverse valid-dirs))
               :filter #'vulpea-db-sync--fswatch-filter
               :sentinel #'vulpea-db-sync--fswatch-sentinel))
        (message "Vulpea: Started fswatch monitoring")))))

(defun vulpea-db-sync--fswatch-filter (_proc output)
  "Process fswatch OUTPUT."
  (let* ((raw (string-trim-right output))
         (parts (split-string raw "\0" t))
         (flags (car (last parts)))
         (file (mapconcat #'identity (butlast parts) "\0")))
    (when (and file
               (not (string-match-p "/\\.git/" file))
               (not (string-match-p "/\\.#" file))
               (not (string-match-p "#$" file))
               (not (string-match-p "~$" file)))
      (cond
       ((and flags (string-match-p "Removed" flags))
        (vulpea-db-sync--handle-removed-file file))
       ((vulpea-db-sync--org-file-p file)
        (vulpea-db-sync--enqueue file))))))

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
      (dolist (file (directory-files-recursively dir "\\.org$" t))
        (when (file-exists-p file)
          (puthash file (file-attributes file) vulpea-db-sync--file-attributes))))))

(defun vulpea-db-sync--check-external-changes ()
  "Check for externally modified files by comparing mtimes."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (dir vulpea-db-sync-directories)
      (when (file-directory-p dir)
        (dolist (file (directory-files-recursively dir "\\.org$" t))
          (when (file-exists-p file)
            (let ((curr-attr (file-attributes file))
                  (cached-attr (gethash file vulpea-db-sync--file-attributes)))
              (puthash file curr-attr vulpea-db-sync--file-attributes)
              (puthash file t seen)
              (when (and cached-attr
                         (not (equal (file-attribute-modification-time curr-attr)
                                     (file-attribute-modification-time cached-attr))))
                (vulpea-db-sync--enqueue file)))))))
    ;; Detect deletions
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
