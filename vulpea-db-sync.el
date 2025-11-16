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

(defcustom vulpea-db-sync-directories nil
  "List of directories to watch for file changes.
If nil, only watch files explicitly registered.
If non-nil, recursively watch these directories for .org files."
  :type '(repeat directory)
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
  "Start file watching and async updates."
  ;; Start watching directories if configured
  (when vulpea-db-sync-directories
    (dolist (dir vulpea-db-sync-directories)
      (vulpea-db-sync--watch-directory dir)))

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

Uses content hash and mtime to detect changes."
  (let* ((attrs (file-attributes path))
         (current-mtime (float-time (file-attribute-modification-time attrs)))
         (current-size (file-attribute-size attrs))
         (stored-info (vulpea-db--get-file-hash path)))

    (when (or (null stored-info)
              (not (equal (plist-get stored-info :mtime) current-mtime))
              (not (equal (plist-get stored-info :size) current-size)))
      ;; File changed, compute hash to verify
      (let ((current-hash (with-temp-buffer
                            (insert-file-contents path)
                            (secure-hash 'sha256 (buffer-string)))))
        (when (or (null stored-info)
                  (not (equal (plist-get stored-info :hash) current-hash)))
          ;; Hash differs, update database
          (vulpea-db-update-file path))))))

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
  "Update database for all org files in DIR recursively."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir "\\.org$")))
    (if vulpea-db-autosync-mode
        (dolist (file files)
          (vulpea-db-sync--enqueue file))
      (let ((db (vulpea-db))
            (count 0))
        (emacsql-with-transaction db
          (dolist (file files)
            (condition-case err
                (progn
                  (vulpea-db-update-file file)
                  (setq count (1+ count)))
              (error
               (message "Vulpea: Error updating %s: %s"
                        file (error-message-string err))))))
        (message "Vulpea: Updated %d file%s"
                 count (if (= count 1) "" "s"))))))

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

;;; Provide

(provide 'vulpea-db-sync)
;;; vulpea-db-sync.el ends here
