;;; vulpea-sync.el --- File monitoring and sync -*- lexical-binding: t; -*-
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 2024-01-24
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
;;; Commentary:
;;
;; This module provides reliable file monitoring and database sync using
;; both internal (file-notify) and external (fswatch/polling) methods.
;;
;; Features:
;; - Recursive monitoring of directories
;; - Transactional processing of changes
;; - External change detection via fswatch or polling
;; - Deduplication of events
;; - Auto-monitoring of new directories
;;
;; Configuration:
;; - vulpea-directories: dirs to monitor
;; - vulpea-sync-external-method: how to detect external changes
;; - vulpea-sync-poll-interval: polling frequency
;;
;; Usage:
;; - Enable monitoring: (vulpea-sync-mode 1)
;; - For read-after-write: (vulpea-sync-with-transaction file ...)
;;
;;; Code:

(require 'filenotify)
(require 'cl-lib)
(require 'seq)
(require 'org)

(defgroup vulpea-sync nil
  "File monitoring and database synchronization."
  :group 'vulpea)

;; TODO: we might want to move it
(defcustom vulpea-directories (list org-directory)
  "List of directories containing notes.
Each directory is monitored recursively for changes when
`vulpea-sync-mode' is enabled. Subdirectories are automatically
watched, so there is no need to explicitly include them."
  :type '(repeat directory)
  :group 'vulpea-sync)

(defcustom vulpea-sync-external-method 'auto
  "Method to use for detecting external file changes.
Possible values:
- `auto' - try fswatch first, fallback to polling
- `fswatch' - use only fswatch
- `poll' - use only polling
- nil - disable external file monitoring"
  :type '(choice (const :tag "Automatic" auto)
                (const :tag "FSWatch" fswatch)
                (const :tag "Polling" poll)
                (const :tag "Disabled" nil))
  :group 'vulpea-sync)

(defcustom vulpea-sync-poll-interval 2
  "Interval in seconds for polling external changes."
  :type 'number
  :group 'vulpea-sync)

(defvar vulpea-sync-file-watchers nil
  "Alist of (dir . descriptor) for file system monitoring.")

(defvar vulpea-sync-file-attributes (make-hash-table :test 'equal)
  "Cache of file attributes for external change detection.")

(defvar vulpea-sync-fswatch-process nil
  "Process handle for fswatch.")

(defclass vulpea-sync-transaction ()
  ((id :initarg :id
       :initform (cl-gensym "vulpea-sync-transaction-")
       :reader vulpea-sync-transaction-id)
   (file :initarg :file
         :reader vulpea-sync-transaction-file)
   (status :initform 'pending
           :accessor vulpea-sync-transaction-status)
   (callbacks :initform nil
             :accessor vulpea-sync-transaction-callbacks))
  "A transaction representing a file modification and sync operation.")

(defvar vulpea-sync-transactions (make-hash-table :test 'equal)
  "Hash table of pending transactions by file.")

(defvar vulpea-sync-transaction-queue nil
  "Queue of pending transactions.")

(defun vulpea-sync-transaction-add-callback (transaction callback)
  "Add CALLBACK to TRANSACTION to be called on completion."
  (push callback (vulpea-sync-transaction-callbacks transaction)))

(defun vulpea-sync-transaction-complete (transaction)
  "Mark TRANSACTION as complete and run callbacks."
  (setf (vulpea-sync-transaction-status transaction) 'complete)
  (let ((file (vulpea-sync-transaction-file transaction)))
    (remhash file vulpea-sync-transactions))
  (dolist (callback (vulpea-sync-transaction-callbacks transaction))
    (funcall callback)))

(defmacro vulpea-sync-with-transaction (file &rest body)
  "Execute BODY within a transaction for FILE."
  (declare (indent 1))
  `(let* ((transaction (make-instance 'vulpea-sync-transaction :file ,file))
          (promise (vulpea-sync-queue-transaction transaction)))
     (vulpea-sync-transaction-add-callback
      transaction
      (lambda ()
        ,@body))
     promise))

(defun vulpea-sync-queue-transaction (transaction)
  "Add TRANSACTION to queue and return a promise."
  (let* ((file (vulpea-sync-transaction-file transaction))
         (existing (gethash file vulpea-sync-transactions)))
    (when existing
      (setf (vulpea-sync-transaction-status existing) 'cancelled))
    (puthash file transaction vulpea-sync-transactions)
    (push transaction vulpea-sync-transaction-queue)
    (run-at-time 0 nil #'vulpea-sync-process-transaction-queue)
    transaction))

(defun vulpea-sync-process-transaction-queue ()
  "Process all pending transactions."
  (let ((queue vulpea-sync-transaction-queue))
    (setq vulpea-sync-transaction-queue nil)
    (dolist (transaction queue)
      (when (eq (vulpea-sync-transaction-status transaction) 'pending)
        (condition-case _
            (progn
              (vulpea-db-update-files
               (list (vulpea-sync-transaction-file transaction)))
              (vulpea-sync-transaction-complete transaction))
          (error
           (setf (vulpea-sync-transaction-status transaction) 'error)))))))

;;;###autoload
(define-minor-mode vulpea-sync-mode
  "Global minor mode for file monitoring and sync."
  :global t
  :group 'vulpea
  :init-value nil
  (let ((enabled vulpea-sync-mode))
    (cond
     (enabled
      (vulpea-sync-watch-dirs))
     (t
      (vulpea-sync-unwatch-dirs)))))

;; TODO: we might want to move it
(defun vulpea-directory-files (dir)
  "Get all org files in DIR and its subdirectories."
  (directory-files-recursively dir "\\.org$" t))

(defun vulpea-sync-watch-dirs ()
  "Setup file monitoring for note directories."
  ;; Setup internal monitoring via file-notify
  (let ((watched-dirs nil))
    (dolist (dir vulpea-directories)
      (when (file-exists-p dir)
        (dolist (subdir (cons dir
                             (seq-filter #'file-directory-p
                                       (directory-files-recursively dir "" t))))
          (unless (seq-find (lambda (watched)
                             (string-prefix-p watched subdir))
                           watched-dirs)
            (let ((desc (file-notify-add-watch
                        subdir
                        '(change attribute-change)
                        #'vulpea-sync-handle-file-event)))
              (push (cons subdir desc) vulpea-sync-file-watchers)
              (push subdir watched-dirs)))))))
  ;; Setup external monitoring
  (vulpea-sync-setup-external-monitoring))

(defun vulpea-sync-setup-fswatch ()
  "Setup file monitoring using fswatch."
  (setq vulpea-sync-fswatch-process
        (make-process
         :name "vulpea-fswatch"
         :buffer nil
         :command `("fswatch"
                    "--recursive"
                    "--event=Updated"
                    "--exclude" "\\.#.*$"     ; exclude auto-save files
                    "--exclude" "#.*#$"       ; exclude backup files
                    "--exclude" ".*~$"        ; exclude backup files
                    "--exclude" "\\.git/"     ; exclude .git directory
                    "--format" "%p"           ; only output the path
                    ,@(mapcar #'expand-file-name vulpea-directories))
         :filter (lambda (_proc output)
                   (let ((file (string-trim-right output)))
                     (when (and (string-match-p "\\.org$" file)
                                (file-exists-p file)
                                (not (string-match-p "/\\.git/" file)))
                       (vulpea-sync-handle-file-event
                        (list 'external 'changed file nil)))))
         :sentinel (lambda (_process event)
                    (when (string-match-p "\\(?:finished\\|exited\\)" event)
                      (if vulpea-sync-mode
                          (run-with-timer 1 nil #'vulpea-sync-setup-fswatch)
                        (setq vulpea-sync-fswatch-process nil)))))))

(defun vulpea-sync-update-file-attributes ()
  "Update cache of file attributes."
  (let ((files (cl-loop for dir in vulpea-directories
                       append (vulpea-directory-files dir))))
    (dolist (file files)
      (puthash file (file-attributes file) vulpea-sync-file-attributes))))

(defun vulpea-sync-check-external-changes ()
  "Check for externally modified files."
  (let ((files (cl-loop for dir in vulpea-directories
                       append (vulpea-directory-files dir))))
    (dolist (file files)
      (let ((curr-attr (file-attributes file))
            (cached-attr (gethash file vulpea-sync-file-attributes)))
        (when (and cached-attr curr-attr
                   (not (equal (nth 5 curr-attr)     ; modification time
                              (nth 5 cached-attr))))
          (vulpea-sync-handle-file-event
           (list 'external 'changed file nil))
          (puthash file curr-attr vulpea-sync-file-attributes))))))

(defun vulpea-sync-unwatch-dirs ()
  "Remove all file monitoring."
  (dolist (watcher vulpea-sync-file-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq vulpea-sync-file-watchers nil)
  (when vulpea-sync-fswatch-process
    (delete-process vulpea-sync-fswatch-process)
    (setq vulpea-sync-fswatch-process nil))
  (clrhash vulpea-sync-file-attributes))

(defun vulpea-sync-setup-external-monitoring ()
  "Setup external file monitoring based on `vulpea-sync-external-method'."
  (pcase vulpea-sync-external-method
    ('auto
     (if (executable-find "fswatch")
         (vulpea-sync-setup-fswatch)
       (progn
         (vulpea-sync-update-file-attributes)
         (run-with-timer 2 vulpea-sync-poll-interval #'vulpea-sync-check-external-changes))))
    ('fswatch
     (if (executable-find "fswatch")
         (vulpea-sync-setup-fswatch)
       (user-error "FSWatch is not available")))
    ('poll
     (vulpea-sync-update-file-attributes)
     (run-with-timer 2 vulpea-sync-poll-interval #'vulpea-sync-check-external-changes))
    (_ nil)))

(defun vulpea-sync-handle-file-event (event)
  "Handle file system EVENT."
  (message "[vulpea-sync-handle-file-event] %S" event)
  (pcase-let ((`(,_ ,action ,file ,_) event))
    (cond
     ;; Handle new directory creation
     ((and (eq action 'created)
           (file-directory-p file))
      (let ((desc (file-notify-add-watch
                   file
                   '(change attribute-change)
                   #'vulpea-sync-handle-file-event)))
        (push (cons file desc) vulpea-sync-file-watchers)))

     ;; Handle org file changes
     ((and (string-match-p "\\.org$" file)
           (not (or (string-match-p "/\\.#" file)  ; auto-save files
                    (string-match-p "#$" file)     ; auto-save backups
                    (string-match-p "~$" file)))   ; backup files
           (member action '(created changed deleted)))
      ;; Update file attributes cache if we're using polling
      (when (and (memq vulpea-sync-external-method '(auto poll))
                 (file-exists-p file))
        (puthash file (file-attributes file) vulpea-sync-file-attributes))
      ;; Create and queue transaction
      (let ((transaction (make-instance 'vulpea-sync-transaction :file file)))
        (vulpea-sync-queue-transaction transaction))))))

;; TODO: move it to DB
(defun vulpea-db-update-files (files)
  "Update FILES."
  (message "[vulpea-db-update-files] processing %d files:\n%s"
           (length files)
           (mapconcat #'(lambda (x) (concat "  - " x)) files "\n")))

(provide 'vulpea-sync)
;;; vulpea-sync.el ends here
