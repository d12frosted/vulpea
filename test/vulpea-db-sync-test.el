;;; vulpea-db-sync-test.el --- Tests for vulpea-db-sync -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for vulpea-db-sync.el
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-sync)

;;; Test Infrastructure

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
          (vulpea-db-location temp-file)
          (vulpea-db--connection nil))
     (unwind-protect
         (progn ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defun vulpea-test--create-temp-org-file (content)
  "Create temporary org file with CONTENT.

Returns absolute path. Caller responsible for cleanup."
  (let ((temp-file (make-temp-file "vulpea-test-" nil ".org")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

;;; Queue Tests

(ert-deftest vulpea-db-sync-enqueue-basic ()
  "Test basic file queueing."
  (let ((vulpea-db-sync--queue nil)
        (vulpea-db-sync--timer nil)
        (path "/tmp/test.org"))
    (vulpea-db-sync--enqueue path)

    (should (= (length vulpea-db-sync--queue) 1))
    (should (equal (caar vulpea-db-sync--queue) path))
    (should (numberp (cdar vulpea-db-sync--queue)))
    (should vulpea-db-sync--timer)))

(ert-deftest vulpea-db-sync-enqueue-deduplication ()
  "Test queue deduplicates same file."
  (let ((vulpea-db-sync--queue nil)
        (vulpea-db-sync--timer nil)
        (path "/tmp/test.org"))
    (vulpea-db-sync--enqueue path)
    (vulpea-db-sync--enqueue path)

    (should (= (length vulpea-db-sync--queue) 1))))

(ert-deftest vulpea-db-sync-enqueue-multiple-files ()
  "Test queueing multiple files."
  (let ((vulpea-db-sync--queue nil)
        (vulpea-db-sync--timer nil))
    (vulpea-db-sync--enqueue "/tmp/file1.org")
    (vulpea-db-sync--enqueue "/tmp/file2.org")
    (vulpea-db-sync--enqueue "/tmp/file3.org")

    (should (= (length vulpea-db-sync--queue) 3))))

;;; Processing Tests

(ert-deftest vulpea-db-sync-process-queue-basic ()
  "Test basic queue processing."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  "#+TITLE: Test\n#+ID: test-id\n"))
           (vulpea-db-sync--queue (list (cons path (float-time))))
           (vulpea-db-sync--processing nil))
      (unwind-protect
          (progn
            (vulpea-db-sync--process-queue)

            ;; Verify queue is empty
            (should (null vulpea-db-sync--queue))

            ;; Verify note in database
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [id title] :from notes
                                      :where (= id $s1)]
                                     "test-id"))))
              (should (equal (elt row 0) "test-id"))
              (should (equal (elt row 1) "Test"))))
        (delete-file path)))))

(ert-deftest vulpea-db-sync-process-queue-batch-limit ()
  "Test queue respects batch size limit."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-sync-batch-size 2)
          (vulpea-db-sync--queue nil)
          (vulpea-db-sync--processing nil)
          (files nil))
      (unwind-protect
          (progn
            ;; Create 3 files
            (dotimes (i 3)
              (let ((path (vulpea-test--create-temp-org-file
                           (format "#+TITLE: Test %d\n#+ID: test-%d\n" i i))))
                (push path files)
                (push (cons path (float-time)) vulpea-db-sync--queue)))

            ;; Process should handle only 2 files
            (vulpea-db-sync--process-queue)

            ;; 1 file should remain in queue
            (should (= (length vulpea-db-sync--queue) 1))

            ;; 2 files should be in database
            (let ((count (caar (emacsql (vulpea-db)
                                        [:select (funcall count *) :from notes]))))
              (should (= count 2))))

        (dolist (file files)
          (when (file-exists-p file)
            (delete-file file)))))))

(ert-deftest vulpea-db-sync-process-queue-skips-missing ()
  "Test processing skips non-existent files."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-sync--queue (list (cons "/nonexistent.org" (float-time))))
          (vulpea-db-sync--processing nil))
      (vulpea-db-sync--process-queue)

      ;; Should not error, queue should be empty
      (should (null vulpea-db-sync--queue))

      ;; Database should be empty
      (let ((count (caar (emacsql (vulpea-db)
                                  [:select (funcall count *) :from notes]))))
        (should (= count 0))))))

;;; Update Detection Tests

(ert-deftest vulpea-db-sync-update-if-changed-new-file ()
  "Test updating new file that's not in database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: New File\n#+ID: new-id\n")))
      (unwind-protect
          (progn
            (vulpea-db-sync--update-file-if-changed path)

            ;; Should be in database
            (should (emacsql (vulpea-db)
                             [:select * :from notes :where (= id $s1)]
                             "new-id")))
        (delete-file path)))))

(ert-deftest vulpea-db-sync-update-if-changed-unchanged-file ()
  "Test skipping file that hasn't changed."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: Test\n#+ID: test-id\n")))
      (unwind-protect
          (progn
            ;; First update
            (vulpea-db-update-file path)

            ;; Get modification count
            (let ((modified-at-1 (caar (emacsql (vulpea-db)
                                                 [:select [modified-at] :from notes
                                                  :where (= id $s1)]
                                                 "test-id"))))
              ;; Second update (no actual changes)
              (sleep-for 0.1)  ; Ensure different timestamp if updated
              (vulpea-db-sync--update-file-if-changed path)

              ;; Should not update (same modified-at)
              (let ((modified-at-2 (caar (emacsql (vulpea-db)
                                                   [:select [modified-at] :from notes
                                                    :where (= id $s1)]
                                                   "test-id"))))
                (should (equal modified-at-1 modified-at-2)))))
        (delete-file path)))))

(ert-deftest vulpea-db-sync-update-if-changed-modified-file ()
  "Test updating file that has changed."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: Version 1\n#+ID: test-id\n")))
      (unwind-protect
          (progn
            ;; First update
            (vulpea-db-update-file path)

            ;; Modify file
            (sleep-for 0.1)  ; Ensure different mtime
            (with-temp-file path
              (insert "#+TITLE: Version 2\n#+ID: test-id\n"))

            ;; Second update should detect change
            (vulpea-db-sync--update-file-if-changed path)

            ;; Verify title updated
            (let ((title (caar (emacsql (vulpea-db)
                                        [:select [title] :from notes
                                         :where (= id $s1)]
                                        "test-id"))))
              (should (equal title "Version 2"))))
        (delete-file path)))))

;;; Manual Update Tests

(ert-deftest vulpea-db-sync-update-file-sync ()
  "Test manual sync update when autosync disabled."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: Manual\n#+ID: manual-id\n"))
          (vulpea-db-autosync-mode nil))
      (unwind-protect
          (progn
            (vulpea-db-sync-update-file path)

            ;; Should be in database immediately
            (should (emacsql (vulpea-db)
                             [:select * :from notes :where (= id $s1)]
                             "manual-id")))
        (delete-file path)))))

(ert-deftest vulpea-db-sync-update-file-async ()
  "Test manual update queues when autosync enabled."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: Async\n#+ID: async-id\n"))
          (vulpea-db-autosync-mode t)
          (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db-sync-update-file path)

            ;; Should be queued, not in database yet
            (should (= (length vulpea-db-sync--queue) 1)))
        (delete-file path)
        (setq vulpea-db-sync--queue nil)))))

;;; Sync Mode Tests

(ert-deftest vulpea-db-sync-with-sync-db ()
  "Test synchronous mode macro."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-autosync-mode nil)
          (files nil))
      (unwind-protect
          (progn
            ;; Use sync mode
            (vulpea-with-sync-db
              (dotimes (i 3)
                (let ((path (vulpea-test--create-temp-org-file
                             (format "#+TITLE: Note %d\n#+ID: note-%d\n" i i))))
                  (push path files)
                  (vulpea-db-update-file path))))

            ;; All files should be in database
            (let ((count (caar (emacsql (vulpea-db)
                                        [:select (funcall count *) :from notes]))))
              (should (= count 3))))

        (dolist (file files)
          (when (file-exists-p file)
            (delete-file file)))))))

(provide 'vulpea-db-sync-test)
;;; vulpea-db-sync-test.el ends here
