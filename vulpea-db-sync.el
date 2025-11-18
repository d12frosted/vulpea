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
(require 'vulpea-db)
(require 'vulpea-db-extract)

;;; Customization

(defgroup vulpea-db-sync nil
  "File watching and synchronization for Vulpea."
  :group 'vulpea)

(defcustom vulpea-db-sync-batch-delay 2.0
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

;;; Variables

(defvar vulpea-db-sync--watchers nil
  "Alist of (path . descriptor) for active file watchers.")

(defvar vulpea-db-sync--queue nil
  "Queue of files pending database update.
Each entry is (path . timestamp).")

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

Performs initial scan to detect changes made while Emacs was closed."
  ;; Clean up deleted files first
  (vulpea-db-sync--cleanup-deleted-files)

  ;; Initial scan: detect external changes
  (when vulpea-db-sync-directories
    (message "Vulpea: Checking for external changes...")
    (dolist (dir vulpea-db-sync-directories)
      (dolist (file (directory-files-recursively dir "\\.org$"))
        (vulpea-db-sync--enqueue file))))

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
  (setq vulpea-db-sync--queue nil))

(defun vulpea-db-sync--watch-directory (dir)
  "Watch DIR and all subdirectories for org file changes."
  (when (file-directory-p dir)
    (let ((descriptor (file-notify-add-watch
                       dir
                       '(change)
                       #'vulpea-db-sync--file-notify-callback)))
      (push (cons dir descriptor) vulpea-db-sync--watchers))

    ;; Recursively watch subdirectories
    (dolist (subdir (directory-files dir t "^[^.]" t))
      (when (and (file-directory-p subdir)
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

(defun vulpea-db-sync--file-notify-callback (event)
  "Handle file notification EVENT."
  (pcase-let ((`(,descriptor ,action ,file . ,_) event))
    (when (and file
               (string-suffix-p ".org" file)
               (not (string-match-p "/\\." file))  ; Skip hidden files
               (member action '(changed created)))
      (vulpea-db-sync--enqueue file))))

(defun vulpea-db-sync--enqueue (path)
  "Add PATH to update queue."
  (let ((timestamp (float-time)))
    ;; Remove existing entry for this path
    (setq vulpea-db-sync--queue
          (assq-delete-all path vulpea-db-sync--queue))

    ;; Add new entry
    (push (cons path timestamp) vulpea-db-sync--queue)

    ;; Reset batch timer
    (when vulpea-db-sync--timer
      (cancel-timer vulpea-db-sync--timer))
    (setq vulpea-db-sync--timer
          (run-with-timer vulpea-db-sync-batch-delay nil
                          #'vulpea-db-sync--process-queue))))

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
               (count 0))

          ;; Remove processed items from queue
          (setq vulpea-db-sync--queue
                (seq-drop vulpea-db-sync--queue batch-size))

          ;; Process in single transaction
          (emacsql-with-transaction db
            (dolist (path paths)
              (condition-case err
                  (when (file-exists-p path)
                    (vulpea-db-sync--update-file-if-changed path)
                    (setq count (1+ count)))
                (error
                 (message "Vulpea: Error updating %s: %s"
                          path (error-message-string err))))))

          (when (> count 0)
            (message "Vulpea: Updated %d file%s"
                     count (if (= count 1) "" "s"))))

      (setq vulpea-db-sync--processing nil)

      ;; If queue still has items, schedule another processing
      (when vulpea-db-sync--queue
        (setq vulpea-db-sync--timer
              (run-with-timer vulpea-db-sync-batch-delay nil
                              #'vulpea-db-sync--process-queue))))))

(defun vulpea-db-sync--update-file-if-changed (path)
  "Update database for PATH only if file has changed.

Uses content hash and mtime to detect changes.
Returns t if file was updated, nil otherwise."
  (let* ((attrs (file-attributes path))
         (current-mtime (float-time (file-attribute-modification-time attrs)))
         (current-size (file-attribute-size attrs))
         (stored-info (vulpea-db--get-file-hash path)))

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

;;; Manual Update

(defun vulpea-db-sync-update-file (path)
  "Manually update database for file at PATH.

If autosync is enabled, queues the update asynchronously.
Otherwise, updates immediately."
  (interactive (list (buffer-file-name)))
  (if vulpea-db-autosync-mode
      (vulpea-db-sync--enqueue path)
    (vulpea-db-update-file path)))

(defun vulpea-db-sync-update-directory (dir)
  "Update database for all org files in DIR recursively.

Only updates files that have actually changed (by checking mtime/size/hash)."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir "\\.org$")))
    (if vulpea-db-autosync-mode
        ;; Async mode: queue all files (smart detection in queue processing)
        (dolist (file files)
          (vulpea-db-sync--enqueue file))
      ;; Sync mode: use smart detection
      (let ((db (vulpea-db))
            (updated 0)
            (unchanged 0))
        (emacsql-with-transaction db
          (dolist (file files)
            (condition-case err
                (if (vulpea-db-sync--update-file-if-changed file)
                    (setq updated (1+ updated))
                  (setq unchanged (1+ unchanged)))
              (error
               (message "Vulpea: Error updating %s: %s"
                        file (error-message-string err))))))
        (message "Vulpea: Checked %d file%s (%d updated, %d unchanged)"
                 (+ updated unchanged)
                 (if (= (+ updated unchanged) 1) "" "s")
                 updated
                 unchanged)))))

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
    (setq vulpea-db-sync--fswatch-process
          (make-process
           :name "vulpea-fswatch"
           :buffer nil
           :command `("fswatch"
                      "--recursive"
                      "--event=Updated"
                      "--event=Created"
                      "--event=Removed"
                      "--exclude" "\\.#.*$"      ; exclude auto-save files
                      "--exclude" "#.*#$"        ; exclude backup files
                      "--exclude" ".*~$"         ; exclude backup files
                      "--exclude" "\\.git/"      ; exclude .git directory
                      "--format" "%p"            ; only output the path
                      ,@(mapcar #'expand-file-name vulpea-db-sync-directories))
           :filter #'vulpea-db-sync--fswatch-filter
           :sentinel #'vulpea-db-sync--fswatch-sentinel))
    (message "Vulpea: Started fswatch monitoring")))

(defun vulpea-db-sync--fswatch-filter (_proc output)
  "Process fswatch OUTPUT."
  (let ((file (string-trim-right output)))
    (when (and (string-match-p "\\.org$" file)
               (file-exists-p file)
               (not (string-match-p "/\\.git/" file))
               (not (string-match-p "/\\.#" file))  ; auto-save
               (not (string-match-p "#$" file))     ; backup
               (not (string-match-p "~$" file)))    ; backup
      (vulpea-db-sync--enqueue file))))

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
    ;; Initialize file attributes cache
    (vulpea-db-sync--update-file-attributes-cache)
    ;; Start polling timer
    (setq vulpea-db-sync--poll-timer
          (run-with-timer vulpea-db-sync-poll-interval
                          vulpea-db-sync-poll-interval
                          #'vulpea-db-sync--check-external-changes))
    (message "Vulpea: Started polling for external changes every %s seconds"
             vulpea-db-sync-poll-interval)))

(defun vulpea-db-sync--update-file-attributes-cache ()
  "Update cache of file attributes for all org files."
  (dolist (dir vulpea-db-sync-directories)
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "\\.org$" t))
        (when (file-exists-p file)
          (puthash file (file-attributes file) vulpea-db-sync--file-attributes))))))

(defun vulpea-db-sync--check-external-changes ()
  "Check for externally modified files by comparing mtimes."
  (dolist (dir vulpea-db-sync-directories)
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "\\.org$" t))
        (when (file-exists-p file)
          (let ((curr-attr (file-attributes file))
                (cached-attr (gethash file vulpea-db-sync--file-attributes)))
            (when (and cached-attr curr-attr
                       (not (equal (file-attribute-modification-time curr-attr)
                                   (file-attribute-modification-time cached-attr))))
              ;; File changed, update cache and enqueue
              (puthash file curr-attr vulpea-db-sync--file-attributes)
              (vulpea-db-sync--enqueue file))))))))

;;; Provide

(provide 'vulpea-db-sync)
;;; vulpea-db-sync.el ends here
