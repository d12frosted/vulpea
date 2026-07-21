;;; vulpea-db-sync-test.el --- Tests for vulpea-db-sync -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
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
(require 'vulpea-db-query)
(require 'vulpea-db-sync)
(require 'vulpea-test-helpers)

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
        (vulpea-db-sync--queue-set (make-hash-table :test 'equal))
        (vulpea-db-sync--timer nil)
        (path "/tmp/test.org"))
    (vulpea-db-sync--enqueue path)
    ;; Use a distinct string object with the same contents
    (vulpea-db-sync--enqueue (concat path ""))

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
                  ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n"))
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
                           (format ":PROPERTIES:\n:ID: test-%d\n:END:\n#+TITLE: Test %d\n" i i))))
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
                 ":PROPERTIES:\n:ID: new-id\n:END:\n#+TITLE: New File\n")))
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
                 ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n")))
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
                 ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 1\n")))
      (unwind-protect
          (progn
            ;; First update
            (vulpea-db-update-file path)

            ;; Modify file
            (sleep-for 0.1)  ; Ensure different mtime
            (with-temp-file path
              (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 2\n"))

            ;; Second update should detect change
            (vulpea-db-sync--update-file-if-changed path)

            ;; Verify title updated
            (let ((title (caar (emacsql (vulpea-db)
                                        [:select [title] :from notes
                                         :where (= id $s1)]
                                        "test-id"))))
              (should (equal title "Version 2"))))
        (delete-file path)))))

(ert-deftest vulpea-db-note-modified-at-tracks-file-mtime ()
  "`modified-at' reflects the file's mtime, not the sync wall-clock."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: mtime-id\n:END:\n#+TITLE: Mtime Test\n")))
      (unwind-protect
          ;; Pin the file's modification time to a fixed point in the past,
          ;; clearly distinct from "now" so a sync-clock value would not match.
          (let ((mtime (encode-time (list 0 0 12 1 1 2020 nil -1 nil))))
            (set-file-times path mtime)
            (vulpea-db-update-file path)
            (let ((note (vulpea-db-get-by-id "mtime-id")))
              (should (equal (vulpea-note-modified-at note)
                             (format-time-string "%Y-%m-%d %H:%M:%S" mtime)))))
        (delete-file path)))))

(ert-deftest vulpea-db-sync-update-if-changed-deleted-file ()
  "Test updating file that was deleted after being indexed."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: deleted-id\n:END:\n#+TITLE: To Be Deleted\n")))
      ;; Index the file first
      (vulpea-db-update-file path)

      ;; Verify note exists
      (should (vulpea-db-get-by-id "deleted-id"))

      ;; Delete the file
      (delete-file path)

      ;; Call update-if-changed - should return 'deleted and clean up db
      (let ((result (vulpea-db-sync--update-file-if-changed path)))
        (should (eq result 'deleted)))

      ;; Note should be removed from database
      (should-not (vulpea-db-get-by-id "deleted-id")))))

;;; Scan-on-Enable Tests
;; https://github.com/d12frosted/vulpea/issues/277

(ert-deftest vulpea-db-sync-scan-on-enable-default-async ()
  "Default must be `async' so external changes are picked up on startup."
  (should (eq (eval (car (get 'vulpea-db-sync-scan-on-enable
                              'standard-value)))
              'async)))

(ert-deftest vulpea-db-sync-effective-scan-mode-respects-setting ()
  "Explicit scan mode is used as-is."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (dolist (mode '(async blocking))
      (let ((vulpea-db-sync-scan-on-enable mode))
        (should (eq (vulpea-db-sync--effective-scan-mode) mode))))))

(ert-deftest vulpea-db-sync-effective-scan-mode-empty-db-fallback ()
  "Empty database forces an async scan even when scanning is disabled."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-sync-scan-on-enable nil))
      (should (eq (vulpea-db-sync--effective-scan-mode) 'async)))))

(ert-deftest vulpea-db-sync-effective-scan-mode-nil-with-notes ()
  "Explicit nil is honored once the database has content."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "scan-mode-id" "Note")
    (let ((vulpea-db-sync-scan-on-enable nil))
      (should (null (vulpea-db-sync--effective-scan-mode))))))

(ert-deftest vulpea-db-sync-start-scans-when-db-empty ()
  "Activation triggers a scan when the database is empty,
even with `vulpea-db-sync-scan-on-enable' set to nil."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((dir (make-temp-file "vulpea-scan-test-" t))
           (scanned nil)
           (vulpea-db-sync-scan-on-enable nil)
           (vulpea-db-sync-external-method nil)
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--idle-timer nil)
           (vulpea-db-sync--watchers nil))
      (cl-letf (((symbol-function 'vulpea-db-sync--scan-files-async)
                 (lambda (dirs _callback) (setq scanned dirs))))
        (unwind-protect
            (progn
              (vulpea-db-sync--start)
              (should (equal scanned (list dir))))
          (vulpea-db-sync--stop)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-start-no-scan-when-db-has-notes ()
  "Activation honors nil scan mode when the database has content."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "existing-note-id" "Existing")
    (let* ((dir (make-temp-file "vulpea-scan-test-" t))
           (scanned nil)
           (vulpea-db-sync-scan-on-enable nil)
           (vulpea-db-sync-external-method nil)
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--idle-timer nil)
           (vulpea-db-sync--watchers nil))
      (cl-letf (((symbol-function 'vulpea-db-sync--scan-files-async)
                 (lambda (dirs _callback) (setq scanned dirs))))
        (unwind-protect
            (progn
              (vulpea-db-sync--start)
              (should (null scanned)))
          (vulpea-db-sync--stop)
          (delete-directory dir t))))))

;;; Manual Update Tests

(ert-deftest vulpea-db-sync-update-file-sync ()
  "Test manual sync update when autosync disabled."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: manual-id\n:END:\n#+TITLE: Manual\n"))
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
                 ":PROPERTIES:\n:ID: async-id\n:END:\n#+TITLE: Async\n"))
          (vulpea-db-autosync-mode t)
          (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db-sync-update-file path)

            ;; Should be queued, not in database yet
            (should (= (length vulpea-db-sync--queue) 1)))
        (delete-file path)
        (setq vulpea-db-sync--queue nil)))))

;;; External Monitoring Tests

(ert-deftest vulpea-db-sync-external-setup-auto-with-fswatch ()
  "Test auto mode when fswatch is available."
  (skip-unless (executable-find "fswatch"))
  (let ((vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--poll-timer nil)
        (vulpea-db-sync-external-method 'auto)
        (vulpea-db-sync-directories (list temporary-file-directory)))
    (unwind-protect
        (progn
          (vulpea-db-sync--setup-external-monitoring)
          ;; Should use fswatch
          (should vulpea-db-sync--fswatch-process)
          (should (process-live-p vulpea-db-sync--fswatch-process))
          (should-not vulpea-db-sync--poll-timer))
      (vulpea-db-sync--stop-external-monitoring))))

(ert-deftest vulpea-db-sync-external-setup-auto-without-fswatch ()
  "Test auto mode falls back to polling when fswatch unavailable."
  (skip-when (executable-find "fswatch"))
  (let ((vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--poll-timer nil)
        (vulpea-db-sync-external-method 'auto)
        (vulpea-db-sync-directories (list temporary-file-directory)))
    (unwind-protect
        (progn
          (vulpea-db-sync--setup-external-monitoring)
          ;; Should use polling
          (should-not vulpea-db-sync--fswatch-process)
          (should vulpea-db-sync--poll-timer))
      (vulpea-db-sync--stop-external-monitoring))))

(ert-deftest vulpea-db-sync-external-setup-poll ()
  "Test polling mode setup."
  (let* ((test-dir (make-temp-file "vulpea-test-poll-" t))
         (vulpea-db-sync--poll-timer nil)
         (vulpea-db-sync-external-method 'poll)
         (vulpea-db-sync-directories (list test-dir)))
    (unwind-protect
        (progn
          ;; Create a test org file in the directory
          (with-temp-file (expand-file-name "test.org" test-dir)
            (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n"))
          (vulpea-db-sync--setup-external-monitoring)
          ;; Should have polling timer
          (should vulpea-db-sync--poll-timer)
          ;; Should have file attributes cache
          (should (> (hash-table-count vulpea-db-sync--file-attributes) 0)))
      (vulpea-db-sync--stop-external-monitoring)
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest vulpea-db-sync-external-setup-nil ()
  "Test disabled external monitoring."
  (let ((vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--poll-timer nil)
        (vulpea-db-sync-external-method nil))
    (vulpea-db-sync--setup-external-monitoring)
    ;; Should not setup anything
    (should-not vulpea-db-sync--fswatch-process)
    (should-not vulpea-db-sync--poll-timer)))

(ert-deftest vulpea-db-sync-external-stop ()
  "Test stopping external monitoring cleans up resources."
  (let* ((test-dir (make-temp-file "vulpea-test-stop-" t))
         (vulpea-db-sync--fswatch-process nil)
         (vulpea-db-sync--poll-timer nil)
         (vulpea-db-sync-external-method 'poll)
         (vulpea-db-sync-directories (list test-dir)))
    (unwind-protect
        (progn
          ;; Create a test org file in the directory
          (with-temp-file (expand-file-name "test.org" test-dir)
            (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n"))
          (vulpea-db-sync--setup-external-monitoring)
          (should vulpea-db-sync--poll-timer)
          (puthash "test" '(1 2 3) vulpea-db-sync--file-attributes)

          (vulpea-db-sync--stop-external-monitoring)

          ;; Should clean everything
          (should-not vulpea-db-sync--poll-timer)
          (should (= (hash-table-count vulpea-db-sync--file-attributes) 0)))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest vulpea-db-sync-setup-fswatch-idempotent ()
  "Test repeated fswatch setup does not spawn a second process."
  (skip-unless (executable-find "fswatch"))
  (let ((vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync-directories (list temporary-file-directory)))
    (unwind-protect
        (progn
          (vulpea-db-sync--setup-fswatch)
          (should vulpea-db-sync--fswatch-process)
          (let ((proc vulpea-db-sync--fswatch-process))
            ;; Second call must be a no-op: same live process, no orphan
            (vulpea-db-sync--setup-fswatch)
            (should (eq vulpea-db-sync--fswatch-process proc))
            (should (process-live-p proc))))
      (vulpea-db-sync--stop-external-monitoring))))

(ert-deftest vulpea-db-sync-stop-detaches-fswatch-sentinel ()
  "Stopping detaches the auto-restart sentinel before killing fswatch.

Without this, the kill triggers `vulpea-db-sync--fswatch-sentinel',
which schedules a respawn 2 seconds later - after any let-bound
configuration is gone - so a zombie fswatch comes back watching the
default `vulpea-db-sync-directories' and nothing ever stops it."
  (skip-unless (executable-find "fswatch"))
  (let ((vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--fswatch-restart-timer nil)
        (vulpea-db-sync-directories (list temporary-file-directory)))
    (vulpea-db-sync--setup-fswatch)
    (let ((proc vulpea-db-sync--fswatch-process))
      (should (process-live-p proc))
      (vulpea-db-sync--stop-external-monitoring)
      ;; Inspected post-mortem: the process object outlives the kill.
      (should (eq (process-sentinel proc) #'ignore)))))

(ert-deftest vulpea-db-sync-stop-cancels-pending-fswatch-restart ()
  "Stopping cancels a restart already scheduled by the sentinel.

Covers the other half of the race: fswatch died on its own, the
sentinel scheduled the 2-second respawn, and monitoring is stopped
before the timer fires.  The timer must not survive the stop."
  (let* ((fired nil)
         (timer (run-at-time 3600 nil (lambda () (setq fired t))))
         (vulpea-db-sync--fswatch-process nil)
         (vulpea-db-sync--fswatch-restart-timer timer))
    (ignore fired)
    (vulpea-db-sync--stop-external-monitoring)
    (should-not vulpea-db-sync--fswatch-restart-timer)
    (should-not (memq timer timer-list))))

(ert-deftest vulpea-db-sync-setup-polling-idempotent ()
  "Test repeated polling setup does not leak a second timer."
  (let* ((test-dir (make-temp-file "vulpea-test-poll-idem-" t))
         (vulpea-db-sync--poll-timer nil)
         (vulpea-db-sync-directories (list test-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "test.org" test-dir)
            (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n"))
          (vulpea-db-sync--setup-polling)
          (should vulpea-db-sync--poll-timer)
          (let ((timer vulpea-db-sync--poll-timer))
            ;; Second call must be a no-op: same timer, still scheduled
            (vulpea-db-sync--setup-polling)
            (should (eq vulpea-db-sync--poll-timer timer))
            (should (memq timer timer-list))))
      (vulpea-db-sync--stop-external-monitoring)
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest vulpea-db-sync-polling-detects-changes ()
  "Test polling detects file modifications."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((test-dir (make-temp-file "vulpea-test-changes-" t))
           (path (expand-file-name "test.org" test-dir))
           (vulpea-db-sync-directories (list test-dir))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            ;; Create initial file
            (with-temp-file path
              (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 1\n"))

            ;; Initialize cache
            (vulpea-db-sync--update-file-attributes-cache)
            (should (gethash path vulpea-db-sync--file-attributes))

            ;; Modify file
            (sleep-for 0.1)  ; Ensure different mtime
            (with-temp-file path
              (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 2\n"))

            ;; Check for changes
            (vulpea-db-sync--check-external-changes-with-files
             (vulpea-db-sync--list-org-files test-dir))

            ;; Should be queued
            (should (assoc path vulpea-db-sync--queue)))
        (when (file-directory-p test-dir)
          (delete-directory test-dir t))))))

(ert-deftest vulpea-db-sync-polling-ignores-unchanged ()
  "Test polling skips unchanged files."
  (let* ((test-dir (make-temp-file "vulpea-test-unchanged-" t))
         (path (expand-file-name "test.org" test-dir))
         (vulpea-db-sync-directories (list test-dir))
         (vulpea-db-sync--queue nil))
    (unwind-protect
        (progn
          ;; Create initial file
          (with-temp-file path
            (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n"))

          ;; Initialize cache
          (vulpea-db-sync--update-file-attributes-cache)

          ;; Check without modifying
          (vulpea-db-sync--check-external-changes-with-files
           (vulpea-db-sync--list-org-files test-dir))

          ;; Should not be queued
          (should-not (assoc path vulpea-db-sync--queue)))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

;;; Cleanup Tests

(ert-deftest vulpea-db-sync-cleanup-untracked-files ()
  "Test that narrowing sync directories removes untracked files from database."
  (vulpea-test--with-temp-db
    (let* ((dir1 (make-temp-file "vulpea-test-dir1-" t))
           (dir2 (make-temp-file "vulpea-test-dir2-" t))
           (file1 (expand-file-name "note1.org" dir1))
           (file2 (expand-file-name "note2.org" dir2))
           (vulpea-db-sync-directories (list dir1 dir2)))
      (unwind-protect
          (progn
            ;; Create files in both directories
            (with-temp-file file1
              (insert ":PROPERTIES:\n:ID: id-1\n:END:\n#+TITLE: Note 1\n"))
            (with-temp-file file2
              (insert ":PROPERTIES:\n:ID: id-2\n:END:\n#+TITLE: Note 2\n"))

            ;; Index both files
            (vulpea-db)  ; Initialize database
            (vulpea-db-update-file file1)
            (vulpea-db-update-file file2)

            ;; Verify both are in database
            (should (vulpea-db-get-by-id "id-1"))
            (should (vulpea-db-get-by-id "id-2"))
            (should (= 2 (vulpea-db-count-notes)))

            ;; Narrow to only dir1
            (setq vulpea-db-sync-directories (list dir1))

            ;; Run cleanup
            (let ((removed (vulpea-db-sync--cleanup-untracked-files)))
              (should (= 1 removed)))

            ;; Verify only file1 remains
            (should (vulpea-db-get-by-id "id-1"))
            (should-not (vulpea-db-get-by-id "id-2"))
            (should (= 1 (vulpea-db-count-notes))))
        (when (file-directory-p dir1)
          (delete-directory dir1 t))
        (when (file-directory-p dir2)
          (delete-directory dir2 t))))))

(ert-deftest vulpea-db-sync-cleanup-untracked-files-strict-prefix ()
  "Ensure cleanup treats similarly named directories as untracked."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-test-dir-" t))
           (similar (concat dir "-archive"))
           (tracked-file (expand-file-name "note1.org" dir))
           (external-file (expand-file-name "note2.org" similar))
           (vulpea-db-sync-directories (list dir)))
      (unwind-protect
          (progn
            (make-directory similar t)
            (with-temp-file tracked-file
              (insert ":PROPERTIES:\n:ID: tracked\n:END:\n#+TITLE: Tracked\n"))
            (with-temp-file external-file
              (insert ":PROPERTIES:\n:ID: external\n:END:\n#+TITLE: External\n"))

            (vulpea-db)
            (vulpea-db-update-file tracked-file)
            (vulpea-db-update-file external-file)

            (should (= 1 (vulpea-db-sync--cleanup-untracked-files)))
            (should (vulpea-db-get-by-id "tracked"))
            (should-not (vulpea-db-get-by-id "external")))
        (when (file-directory-p dir)
          (delete-directory dir t))
        (when (file-directory-p similar)
          (delete-directory similar t))))))

(ert-deftest vulpea-db-sync-tracked-file-p-basic ()
  "A path counts as tracked only when it lives under a synced directory."
  (let* ((dir (file-truename (make-temp-file "vulpea-tracked-" t)))
         (vulpea-db-sync-directories (list dir)))
    (unwind-protect
        (progn
          ;; inside the directory
          (should (vulpea-db-sync-tracked-file-p
                   (expand-file-name "note.org" dir)))
          ;; a file in a subdirectory (fswatch reports these on creation)
          (should (vulpea-db-sync-tracked-file-p
                   (expand-file-name "journal/2026.org" dir)))
          ;; a sibling that merely shares a name prefix
          (should-not (vulpea-db-sync-tracked-file-p
                       (concat dir "-archive/note.org")))
          ;; somewhere else entirely
          (should-not (vulpea-db-sync-tracked-file-p "/tmp/elsewhere/x.org"))
          ;; a relative path is rejected
          (should-not (vulpea-db-sync-tracked-file-p "note.org"))
          ;; nil path
          (should-not (vulpea-db-sync-tracked-file-p nil)))
      (delete-directory dir t)))
  ;; with no directories configured, nothing is tracked
  (let ((vulpea-db-sync-directories nil))
    (should-not (vulpea-db-sync-tracked-file-p "/tmp/vault/note.org"))))

(ert-deftest vulpea-db-sync-tracked-file-p-symlink ()
  "Symlinked directories match whichever side of the check carries the link.

fswatch on macOS reports resolved paths, so a symlinked
`vulpea-db-sync-directories' entry has to resolve both sides before
comparing."
  (let* ((real (file-truename (make-temp-file "vulpea-tracked-real-" t)))
         (link (concat (file-truename
                        (make-temp-file "vulpea-tracked-link-" t))
                       "/vault")))
    (unwind-protect
        (progn
          (make-symbolic-link real link)
          ;; directory is the symlink, path is the resolved location
          (let ((vulpea-db-sync-directories (list link)))
            (should (vulpea-db-sync-tracked-file-p
                     (expand-file-name "note.org" real))))
          ;; directory is the resolved location, path goes through the link
          (let ((vulpea-db-sync-directories (list real)))
            (should (vulpea-db-sync-tracked-file-p
                     (expand-file-name "note.org" link))))
          ;; an outside file is still rejected through the link
          (let ((vulpea-db-sync-directories (list link)))
            (should-not (vulpea-db-sync-tracked-file-p "/tmp/elsewhere/x.org"))))
      (when (file-symlink-p link) (delete-file link))
      (delete-directory real t)
      (delete-directory (file-name-directory link) t))))

(ert-deftest vulpea-db-sync-full-scan-cleans-untracked ()
  "Test that full-scan automatically cleans up untracked files."
  (vulpea-test--with-temp-db
    (let* ((dir1 (make-temp-file "vulpea-test-dir1-" t))
           (dir2 (make-temp-file "vulpea-test-dir2-" t))
           (subdir (expand-file-name "journal" dir1))
           (file1 (expand-file-name "note1.org" dir1))
           (file2 (expand-file-name "note2.org" subdir))
           (file3 (expand-file-name "note3.org" dir2))
           (vulpea-db-sync-directories (list dir1 dir2)))
      (unwind-protect
          (progn
            ;; Create subdirectory
            (make-directory subdir t)

            ;; Create files
            (with-temp-file file1
              (insert ":PROPERTIES:\n:ID: id-1\n:END:\n#+TITLE: Note 1\n"))
            (with-temp-file file2
              (insert ":PROPERTIES:\n:ID: id-2\n:END:\n#+TITLE: Note 2\n"))
            (with-temp-file file3
              (insert ":PROPERTIES:\n:ID: id-3\n:END:\n#+TITLE: Note 3\n"))

            ;; Initialize and index all files
            (vulpea-db)
            (vulpea-db-update-file file1)
            (vulpea-db-update-file file2)
            (vulpea-db-update-file file3)

            ;; Verify all are in database
            (should (= 3 (vulpea-db-count-notes)))

            ;; Narrow to only journal subdirectory
            (setq vulpea-db-sync-directories (list subdir))

            ;; Run full scan (should clean up file1 and file3)
            (vulpea-db-sync-full-scan)

            ;; Verify only file2 remains
            (should-not (vulpea-db-get-by-id "id-1"))
            (should (vulpea-db-get-by-id "id-2"))
            (should-not (vulpea-db-get-by-id "id-3"))
            (should (= 1 (vulpea-db-count-notes))))
        (when (file-directory-p dir1)
          (delete-directory dir1 t))
        (when (file-directory-p dir2)
          (delete-directory dir2 t))))))

(ert-deftest vulpea-db-sync-watch-directory-ignores-symlinks ()
  "Watching a directory should not recurse into symlink loops."
  (let* ((root (make-temp-file "vulpea-watch-root-" t))
         (child (expand-file-name "notes" root))
         (link (expand-file-name "loop" child))
         (vulpea-db-sync--watchers nil))
    (unwind-protect
        (progn
          (make-directory child t)
          ;; Create a symlink pointing back to root to simulate a loop
          (make-symbolic-link root link t)
          (vulpea-db-sync--watch-directory root)
          (should (assoc root vulpea-db-sync--watchers))
          (should (assoc child vulpea-db-sync--watchers))
          (should-not (assoc link vulpea-db-sync--watchers)))
      (dolist (entry vulpea-db-sync--watchers)
        (file-notify-rm-watch (cdr entry)))
      (when (file-directory-p root)
        (delete-directory root t)))))

;;; File notification tests

(ert-deftest vulpea-db-sync-file-notify-deleted-removes-note ()
  "Deleting a watched file removes it from the database immediately."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  ":PROPERTIES:\n:ID: deleted-id\n:END:\n#+TITLE: Delete Me\n"))
           (vulpea-db-sync--queue nil)
           (vulpea-db-sync--timer nil))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (should (emacsql (vulpea-db)
                             [:select path :from files :where (= path $s1)]
                             path))
            (delete-file path)
            (vulpea-db-sync--file-notify-callback
             (list nil 'deleted path))
            (should (null (emacsql (vulpea-db)
                                   [:select * :from notes
                                    :where (= path $s1)]
                                   path)))
            (should (null (emacsql (vulpea-db)
                                   [:select path :from files
                                    :where (= path $s1)]
                                   path)))
            (should (null vulpea-db-sync--queue)))
        (when (file-exists-p path)
          (delete-file path))))))

(ert-deftest vulpea-db-sync-file-notify-renamed-handles-paths ()
  "Renaming a file removes the old record and enqueues the new path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  ":PROPERTIES:\n:ID: rename-id\n:END:\n#+TITLE: Rename Me\n"))
           (new-path (concat (file-name-sans-extension path) "-renamed.org"))
           (vulpea-db-sync--queue nil)
           (vulpea-db-sync--timer nil))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (rename-file path new-path t)
            (vulpea-db-sync--file-notify-callback
             (list nil 'renamed path new-path))
            (should (null (emacsql (vulpea-db)
                                   [:select * :from notes
                                    :where (= path $s1)]
                                   path)))
            (should (= (length vulpea-db-sync--queue) 1))
            (should (equal (caar vulpea-db-sync--queue) new-path)))
        (when (file-exists-p path)
          (delete-file path))
        (when (file-exists-p new-path)
          (delete-file new-path))))))

(ert-deftest vulpea-db-sync-fswatch-removed-cleans-db ()
  "fswatch removal events should purge deleted notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  ":PROPERTIES:\n:ID: fswatch-delete\n:END:\n#+TITLE: Remove via fswatch\n"))
           (vulpea-db-sync-directories (list temporary-file-directory))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (should (vulpea-db-get-by-id "fswatch-delete"))
            (delete-file path)
            (vulpea-db-sync--fswatch-filter nil (format "%s|||Removed\n" path))
            (should-not (vulpea-db-get-by-id "fswatch-delete")))
        (when (file-exists-p path)
          (delete-file path))))))

(ert-deftest vulpea-db-sync-file-notify-watches-new-directories ()
  "Creating a new subdirectory starts a watcher so new files are indexed."
  (vulpea-test--with-temp-db
    (let ((root (make-temp-file "vulpea-sync-root-" t))
          (vulpea-db-sync-directories nil)
          (vulpea-db-sync--watchers nil)
          (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (setq vulpea-db-sync-directories (list root))
            (vulpea-db-sync--watch-directory root)
            (let* ((subdir (expand-file-name "projects" root))
                   (file (expand-file-name "note.org" subdir)))
              ;; Directory is created outside watcher recursion, then event arrives.
              (make-directory subdir t)
              (vulpea-db-sync--file-notify-callback
               (list nil 'created subdir))
              ;; Drop existing watchers to ensure call added the directory.
              (let ((found (assoc subdir vulpea-db-sync--watchers)))
                (should found))
              (with-temp-file file
                (insert ":PROPERTIES:\n:ID: new-dir-note\n:END:\n#+TITLE: Dir Note\n"))
              (vulpea-db-sync--enqueue file)
              (should (= (length vulpea-db-sync--queue) 1))
              (should (equal (caar vulpea-db-sync--queue) file))))
        (when (file-directory-p root)
          (delete-directory root t))))))

(ert-deftest vulpea-db-sync-polling-detects-deletions ()
  "Polling fallback removes files that disappear outside Emacs."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-poll-dir-" t))
           (file (expand-file-name "note.org" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--file-attributes (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ":PROPERTIES:\n:ID: poll-delete\n:END:\n#+TITLE: Remove via poll\n"))
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (vulpea-db-get-by-id "poll-delete"))
            (puthash file (file-attributes file) vulpea-db-sync--file-attributes)
            (delete-file file)
            (vulpea-db-sync--check-external-changes-with-files
             (vulpea-db-sync--list-org-files dir))
            (should-not (vulpea-db-get-by-id "poll-delete")))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-polling-detects-new-files ()
  "Polling detects externally created files."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-poll-new-" t))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--file-attributes (make-hash-table :test 'equal))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db)
            ;; Initialize cache with empty directory
            (vulpea-db-sync--update-file-attributes-cache)
            (should (= 0 (hash-table-count vulpea-db-sync--file-attributes)))

            ;; Create a new file externally
            (let ((file (expand-file-name "new-note.org" dir)))
              (with-temp-file file
                (insert ":PROPERTIES:\n:ID: new-file\n:END:\n#+TITLE: New File\n"))

              ;; Check for external changes
              (vulpea-db-sync--check-external-changes-with-files
               (vulpea-db-sync--list-org-files dir))

              ;; File should be queued for sync
              (should (assoc file vulpea-db-sync--queue))
              ;; File should be in cache now
              (should (gethash file vulpea-db-sync--file-attributes))))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-polling-detects-directory-deletion ()
  "Polling removes all files when directory is deleted."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-poll-dirdelete-" t))
           (subdir (expand-file-name "subdir" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--file-attributes (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (make-directory subdir t)
            ;; Create files in subdir
            (let ((file1 (expand-file-name "note1.org" subdir))
                  (file2 (expand-file-name "note2.org" subdir)))
              (with-temp-file file1
                (insert ":PROPERTIES:\n:ID: dir-del-1\n:END:\n#+TITLE: Note 1\n"))
              (with-temp-file file2
                (insert ":PROPERTIES:\n:ID: dir-del-2\n:END:\n#+TITLE: Note 2\n"))

              (vulpea-db)
              (vulpea-db-update-file file1)
              (vulpea-db-update-file file2)
              (should (vulpea-db-get-by-id "dir-del-1"))
              (should (vulpea-db-get-by-id "dir-del-2"))

              ;; Initialize cache
              (vulpea-db-sync--update-file-attributes-cache)
              (should (gethash file1 vulpea-db-sync--file-attributes))
              (should (gethash file2 vulpea-db-sync--file-attributes))

              ;; Delete the subdirectory
              (delete-directory subdir t)

              ;; Check for external changes
              (vulpea-db-sync--check-external-changes-with-files
               (vulpea-db-sync--list-org-files dir))

              ;; Both files should be removed from database
              (should-not (vulpea-db-get-by-id "dir-del-1"))
              (should-not (vulpea-db-get-by-id "dir-del-2"))
              ;; Both should be removed from cache
              (should-not (gethash file1 vulpea-db-sync--file-attributes))
              (should-not (gethash file2 vulpea-db-sync--file-attributes))))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-polling-handles-directory-rename ()
  "Polling handles directory rename (old files removed, new files added)."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-poll-dirrename-" t))
           (subdir (expand-file-name "old-name" dir))
           (newdir (expand-file-name "new-name" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--file-attributes (make-hash-table :test 'equal))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (make-directory subdir t)
            (let ((file (expand-file-name "note.org" subdir)))
              (with-temp-file file
                (insert ":PROPERTIES:\n:ID: dir-rename\n:END:\n#+TITLE: Note\n"))

              (vulpea-db)
              (vulpea-db-update-file file)
              (should (vulpea-db-get-by-id "dir-rename"))

              ;; Initialize cache
              (vulpea-db-sync--update-file-attributes-cache)
              (should (gethash file vulpea-db-sync--file-attributes))

              ;; Rename directory
              (rename-file subdir newdir)
              (let ((new-file (expand-file-name "note.org" newdir)))

                ;; Check for external changes
                (vulpea-db-sync--check-external-changes-with-files
                 (vulpea-db-sync--list-org-files dir))

                ;; Old file should be removed from database
                (should-not (emacsql (vulpea-db)
                                     [:select * :from files :where (= path $s1)]
                                     file))
                ;; Old file should be removed from cache
                (should-not (gethash file vulpea-db-sync--file-attributes))
                ;; New file should be in cache and queued
                (should (gethash new-file vulpea-db-sync--file-attributes))
                (should (assoc new-file vulpea-db-sync--queue)))))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

;;; fswatch event handling tests

(ert-deftest vulpea-db-sync-fswatch-renamed-event-removes-file ()
  "fswatch Renamed event removes files that no longer exist (e.g., moved to trash)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  ":PROPERTIES:\n:ID: fswatch-trash\n:END:\n#+TITLE: Trash Me\n"))
           (vulpea-db-sync-directories (list temporary-file-directory))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (should (vulpea-db-get-by-id "fswatch-trash"))
            ;; Delete file (simulates move to trash)
            (delete-file path)
            ;; fswatch sends Renamed event when file is moved to trash
            (vulpea-db-sync--fswatch-filter nil (format "%s|||Renamed\n" path))
            ;; File should be removed from database (because file-exists-p is false)
            (should-not (vulpea-db-get-by-id "fswatch-trash")))
        (when (file-exists-p path)
          (delete-file path))))))

(ert-deftest vulpea-db-sync-fswatch-directory-removal ()
  "fswatch directory removal removes all files under that directory."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-fswatch-dirremove-" t))
           (file1 (expand-file-name "note1.org" dir))
           (file2 (expand-file-name "note2.org" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (with-temp-file file1
              (insert ":PROPERTIES:\n:ID: fswatch-dir-1\n:END:\n#+TITLE: Note 1\n"))
            (with-temp-file file2
              (insert ":PROPERTIES:\n:ID: fswatch-dir-2\n:END:\n#+TITLE: Note 2\n"))

            (vulpea-db)
            (vulpea-db-update-file file1)
            (vulpea-db-update-file file2)
            (should (vulpea-db-get-by-id "fswatch-dir-1"))
            (should (vulpea-db-get-by-id "fswatch-dir-2"))

            ;; Delete directory
            (delete-directory dir t)

            ;; fswatch sends Removed event for directory
            (vulpea-db-sync--fswatch-filter nil (format "%s/|||Removed\n" dir))

            ;; Both files should be removed from database
            (should-not (vulpea-db-get-by-id "fswatch-dir-1"))
            (should-not (vulpea-db-get-by-id "fswatch-dir-2")))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-fswatch-file-not-exists-fallback ()
  "fswatch handles files that no longer exist via file-exists-p check."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((path (vulpea-test--create-temp-org-file
                  ":PROPERTIES:\n:ID: fswatch-noexist\n:END:\n#+TITLE: Gone\n"))
           (vulpea-db-sync-directories (list temporary-file-directory))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (should (vulpea-db-get-by-id "fswatch-noexist"))
            ;; Delete file
            (delete-file path)
            ;; fswatch sends Updated event (not Removed), but file doesn't exist
            (vulpea-db-sync--fswatch-filter nil (format "%s|||Updated\n" path))
            ;; File should be removed due to file-exists-p check
            (should-not (vulpea-db-get-by-id "fswatch-noexist")))
        (when (file-exists-p path)
          (delete-file path))))))

(ert-deftest vulpea-db-sync-fswatch-multiple-events ()
  "fswatch handles multiple events in single output."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-fswatch-multi-" t))
           (file1 (expand-file-name "note1.org" dir))
           (file2 (expand-file-name "note2.org" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (with-temp-file file1
              (insert ":PROPERTIES:\n:ID: multi-1\n:END:\n#+TITLE: Note 1\n"))
            (with-temp-file file2
              (insert ":PROPERTIES:\n:ID: multi-2\n:END:\n#+TITLE: Note 2\n"))

            (vulpea-db)
            (vulpea-db-update-file file1)
            (vulpea-db-update-file file2)

            ;; Delete both files
            (delete-file file1)
            (delete-file file2)

            ;; fswatch sends multiple events in single output
            (vulpea-db-sync--fswatch-filter
             nil
             (format "%s|||Removed\n%s|||Removed\n" file1 file2))

            ;; Both files should be removed
            (should-not (vulpea-db-get-by-id "multi-1"))
            (should-not (vulpea-db-get-by-id "multi-2")))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest vulpea-db-sync-fswatch-directory-created-scans ()
  "fswatch Created event for directory scans for org files inside."
  (vulpea-test--with-temp-db
    (let* ((root (make-temp-file "vulpea-fswatch-newdir-" t))
           (subdir (expand-file-name "newdir" root))
           (vulpea-db-sync-directories (list root))
           (vulpea-db-sync--queue nil))
      (unwind-protect
          (progn
            (vulpea-db)
            ;; Create directory with org file
            (make-directory subdir t)
            (let ((file (expand-file-name "note.org" subdir)))
              (with-temp-file file
                (insert ":PROPERTIES:\n:ID: newdir-note\n:END:\n#+TITLE: New Dir Note\n"))

              ;; fswatch sends Created event for directory
              (vulpea-db-sync--fswatch-filter nil (format "%s|||Created\n" subdir))

              ;; File inside should be queued
              (should (assoc file vulpea-db-sync--queue))))
        (when (file-directory-p root)
          (delete-directory root t))))))

;;; Hidden Directory Filtering Tests

(ert-deftest vulpea-db-sync-org-file-p-excludes-hidden-directories ()
  "Test that org-file-p excludes files in hidden directories."
  ;; Regular files should pass
  (should (vulpea-db-sync--org-file-p "/path/to/note.org"))
  (should (vulpea-db-sync--org-file-p "/path/to/subdir/note.org"))

  ;; Files in hidden directories should fail
  (should-not (vulpea-db-sync--org-file-p "/path/.hidden/note.org"))
  (should-not (vulpea-db-sync--org-file-p "/path/to/.archive/note.org"))
  (should-not (vulpea-db-sync--org-file-p "/path/.git/config.org"))
  (should-not (vulpea-db-sync--org-file-p "/.hidden/note.org"))

  ;; Non-org files should fail
  (should-not (vulpea-db-sync--org-file-p "/path/to/note.txt"))
  (should-not (vulpea-db-sync--org-file-p "/path/to/note.org.bak"))

  ;; Nil should fail
  (should-not (vulpea-db-sync--org-file-p nil)))

(ert-deftest vulpea-db-sync-list-org-files-excludes-hidden ()
  "Test that list-org-files excludes files in hidden directories."
  (let* ((dir (make-temp-file "vulpea-test-hidden-" t))
         (visible-file (expand-file-name "note.org" dir))
         (hidden-dir (expand-file-name ".hidden" dir))
         (hidden-file (expand-file-name "secret.org" hidden-dir)))
    (unwind-protect
        (progn
          ;; Create visible file
          (with-temp-file visible-file
            (insert "visible"))

          ;; Create hidden directory and file
          (make-directory hidden-dir t)
          (with-temp-file hidden-file
            (insert "hidden"))

          ;; List should only include visible file
          (let ((files (vulpea-db-sync--list-org-files dir)))
            (should (= 1 (length files)))
            (should (member visible-file files))
            (should-not (member hidden-file files))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest vulpea-db-sync-update-directory-excludes-hidden ()
  "Test that update-directory excludes files in hidden directories."
  (vulpea-test--with-temp-db
    (let* ((dir (make-temp-file "vulpea-test-hidden-sync-" t))
           (visible-file (expand-file-name "visible.org" dir))
           (hidden-dir (expand-file-name ".archive" dir))
           (hidden-file (expand-file-name "archived.org" hidden-dir))
           (vulpea-db-autosync-mode nil))
      (unwind-protect
          (progn
            ;; Create visible file
            (with-temp-file visible-file
              (insert ":PROPERTIES:\n:ID: visible-id\n:END:\n#+TITLE: Visible\n"))

            ;; Create hidden directory and file
            (make-directory hidden-dir t)
            (with-temp-file hidden-file
              (insert ":PROPERTIES:\n:ID: hidden-id\n:END:\n#+TITLE: Hidden\n"))

            ;; Sync directory
            (vulpea-db)
            (vulpea-db-sync-update-directory dir)

            ;; Only visible file should be in database
            (should (vulpea-db-get-by-id "visible-id"))
            (should-not (vulpea-db-get-by-id "hidden-id")))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

;;; File Removal Tests

(ert-deftest vulpea-db-sync-handle-removed-file-directory-prefix ()
  "Handle removed file correctly cleans up directory prefix matches."
  (vulpea-test--with-temp-db
    (let* ((dir "/tmp/vulpea-test-dir")
           (file1 (concat dir "/note1.org"))
           (file2 (concat dir "/subdir/note2.org")))
      (vulpea-db)
      ;; Manually insert files into database
      (emacsql (vulpea-db)
               [:insert :into files :values $v1]
               (vector file1 "hash1" 12345 100))
      (emacsql (vulpea-db)
               [:insert :into files :values $v1]
               (vector file2 "hash2" 12346 100))

      ;; Verify files are in database
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file1))
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file2))

      ;; Remove directory
      (vulpea-db-sync--handle-removed-file dir)

      ;; Both files should be removed
      (should-not (emacsql (vulpea-db)
                           [:select path :from files :where (= path $s1)]
                           file1))
      (should-not (emacsql (vulpea-db)
                           [:select path :from files :where (= path $s1)]
                           file2)))))

(ert-deftest vulpea-db-sync-handle-removed-file-special-chars ()
  "Handle removed file with GLOB special characters in path."
  (vulpea-test--with-temp-db
    ;; Test paths with GLOB wildcards: *, ?, [
    (let* ((dir-with-star "/tmp/vulpea*test")
           (dir-with-question "/tmp/vulpea?test")
           (dir-with-bracket "/tmp/vulpea[1]test")
           (file-star (concat dir-with-star "/note.org"))
           (file-question (concat dir-with-question "/note.org"))
           (file-bracket (concat dir-with-bracket "/note.org"))
           ;; File that should NOT be matched by wildcards
           (file-similar "/tmp/vulpeaXtest/note.org"))
      (vulpea-db)
      ;; Insert all files
      (dolist (file (list file-star file-question file-bracket file-similar))
        (emacsql (vulpea-db)
                 [:insert :into files :values $v1]
                 (vector file "hash" 12345 100)))

      ;; Remove directory with * - should only remove that specific dir
      (vulpea-db-sync--handle-removed-file dir-with-star)
      (should-not (emacsql (vulpea-db)
                           [:select path :from files :where (= path $s1)]
                           file-star))
      ;; Other files should still exist
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-question))
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-bracket))
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-similar))

      ;; Remove directory with ? - should only remove that specific dir
      (vulpea-db-sync--handle-removed-file dir-with-question)
      (should-not (emacsql (vulpea-db)
                           [:select path :from files :where (= path $s1)]
                           file-question))
      ;; Remaining files should still exist
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-bracket))
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-similar))

      ;; Remove directory with [] - should only remove that specific dir
      (vulpea-db-sync--handle-removed-file dir-with-bracket)
      (should-not (emacsql (vulpea-db)
                           [:select path :from files :where (= path $s1)]
                           file-bracket))
      ;; Similar file should still exist (wasn't matched by wildcards)
      (should (emacsql (vulpea-db)
                       [:select path :from files :where (= path $s1)]
                       file-similar)))))

;;; Extra Extensions Tests

;;; FSWatch Path Validation Tests

(ert-deftest vulpea-db-sync-fswatch-path-valid-p-absolute ()
  "Test fswatch path validation with absolute directories."
  (let ((vulpea-db-sync-directories (list "/tmp/notes")))
    (should (vulpea-db-sync--fswatch-path-valid-p "/tmp/notes/file.org"))
    (should (vulpea-db-sync--fswatch-path-valid-p "/tmp/notes/sub/file.org"))
    (should-not (vulpea-db-sync--fswatch-path-valid-p "/tmp/other/file.org"))
    (should-not (vulpea-db-sync--fswatch-path-valid-p nil))))

(ert-deftest vulpea-db-sync-fswatch-path-valid-p-tilde ()
  "Test fswatch path validation with tilde (relative) directories."
  (let ((vulpea-db-sync-directories (list "~/notes")))
    ;; fswatch reports absolute paths - must match expanded tilde
    (should (vulpea-db-sync--fswatch-path-valid-p
             (expand-file-name "notes/file.org" "~")))
    (should-not (vulpea-db-sync--fswatch-path-valid-p "/other/file.org"))))

;;; Extra Extensions Tests

(ert-deftest vulpea-db-sync-org-file-p-extra-extensions ()
  "Test org-file-p recognizes extra extensions."
  (let ((vulpea-db-extra-extensions '(".org.age" ".org.gpg")))
    (should (vulpea-db-sync--org-file-p "/tmp/note.org"))
    (should (vulpea-db-sync--org-file-p "/tmp/note.org.age"))
    (should (vulpea-db-sync--org-file-p "/tmp/note.org.gpg"))
    (should-not (vulpea-db-sync--org-file-p "/tmp/note.txt"))
    (should-not (vulpea-db-sync--org-file-p "/tmp/.hidden/note.org.age"))))

(ert-deftest vulpea-db-sync-org-file-p-default-rejects-extra ()
  "Test org-file-p rejects extra extensions when not configured."
  (let ((vulpea-db-extra-extensions nil))
    (should (vulpea-db-sync--org-file-p "/tmp/note.org"))
    (should-not (vulpea-db-sync--org-file-p "/tmp/note.org.age"))
    (should-not (vulpea-db-sync--org-file-p "/tmp/note.org.gpg"))))

(ert-deftest vulpea-db-sync-list-org-files-extra-extensions ()
  "Test list-org-files finds files with extra extensions."
  (let* ((dir (make-temp-file "vulpea-test-ext-" t))
         (vulpea-db-extra-extensions '(".org.age")))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "a.org" dir))
          (write-region "" nil (expand-file-name "b.org.age" dir))
          (write-region "" nil (expand-file-name "c.txt" dir))
          (let ((files (vulpea-db-sync--list-org-files dir)))
            (should (= 2 (length files)))
            (should (seq-some (lambda (f) (string-suffix-p "a.org" f)) files))
            (should (seq-some (lambda (f) (string-suffix-p "b.org.age" f)) files))))
      (delete-directory dir t))))

;;; Verbosity Tests

(ert-deftest vulpea-db-sync-message-verbose-emits ()
  "Test that `vulpea-db-sync--message' emits when verbose is non-nil."
  (let ((vulpea-db-sync-verbose t)
        (calls nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) calls))))
      (vulpea-db-sync--message "Vulpea: hello %d" 1))
    (should (equal calls '("Vulpea: hello 1")))))

(ert-deftest vulpea-db-sync-message-silent-suppresses ()
  "Test that `vulpea-db-sync--message' is silent when verbose is nil."
  (let ((vulpea-db-sync-verbose nil)
        (calls nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) calls))))
      (vulpea-db-sync--message "Vulpea: hello %d" 1))
    (should-not calls)))

;;; fswatch path normalization (Windows)

(ert-deftest vulpea-db-sync-fswatch-normalize-mingw-backslash ()
  "mingw fswatch reports an upper-case drive and a backslash separator."
  (let ((system-type 'windows-nt))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "C:/Users/foo/notes\\bar.org")
                   "c:/Users/foo/notes/bar.org"))))

(ert-deftest vulpea-db-sync-fswatch-normalize-cygwin-prefix ()
  "Cygwin fswatch reports a /cygdrive/ prefix."
  (let ((system-type 'windows-nt))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "/cygdrive/c/Users/foo/notes/bar.org")
                   "c:/Users/foo/notes/bar.org"))))

(ert-deftest vulpea-db-sync-fswatch-normalize-drive-case ()
  "An upper-case drive letter is lowered to match `expand-file-name'."
  (let ((system-type 'windows-nt))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "C:/Users/foo/notes/bar.org")
                   "c:/Users/foo/notes/bar.org"))))

(ert-deftest vulpea-db-sync-fswatch-normalize-canonical-unchanged ()
  "An already-canonical Windows path is left as-is."
  (let ((system-type 'windows-nt))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "c:/Users/foo/notes/bar.org")
                   "c:/Users/foo/notes/bar.org"))))

(ert-deftest vulpea-db-sync-fswatch-normalize-unix-untouched ()
  "On non-Windows systems paths are returned unchanged.

A backslash is a legal file-name character on Unix and must not be
rewritten."
  (let ((system-type 'gnu/linux))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "/home/user/notes/bar.org")
                   "/home/user/notes/bar.org"))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    "/home/user/odd\\name.org")
                   "/home/user/odd\\name.org"))))

(ert-deftest vulpea-db-sync-fswatch-normalize-empty ()
  "nil or empty input yields nil."
  (let ((system-type 'windows-nt))
    (should-not (vulpea-db-sync--fswatch-normalize-path nil))
    (should-not (vulpea-db-sync--fswatch-normalize-path ""))))

(ert-deftest vulpea-db-sync-fswatch-mingw-path-enqueued ()
  "A mingw-style backslash path from fswatch is recognized and enqueued.

Regression test for paths reported by the MSYS2/mingw fswatch build on
Windows, which were silently dropped by the watched-directory check (see
issue #344)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((dir (make-temp-file "vulpea-fswatch-mingw-" t))
           (file (expand-file-name "note.org" dir))
           (vulpea-db-sync-directories (list dir))
           (vulpea-db-sync--queue nil)
           (vulpea-db-sync--queue-set (make-hash-table :test 'equal))
           (vulpea-db-sync--timer nil)
           (vulpea-db-sync-fswatch-path-style 'auto)
           (vulpea-db-sync--fswatch-effective-style nil)
           (vulpea-db-sync--fswatch-seen-valid-event nil))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ":PROPERTIES:\n:ID: mingw-note\n:END:\n#+TITLE: Mingw\n"))
            ;; Simulate mingw fswatch: backslash before the file name.
            ;; Pretend we are on Windows so backslash separators are
            ;; normalized; the temp paths themselves stay POSIX, and file
            ;; I/O above runs under the real `system-type'.  The
            ;; watched-directory check resolves paths with `file-truename',
            ;; which under a faked `windows-nt' calls `w32-long-file-name'
            ;; (void off Windows); stub it to identity, standing in for the
            ;; real function that exists on Windows.
            (cl-letf (((symbol-function 'w32-long-file-name) #'identity))
              (let ((system-type 'windows-nt))
                (vulpea-db-sync--fswatch-filter
                 nil (format "%s\\note.org|||Updated\n" dir))))
            (should (= (length vulpea-db-sync--queue) 1))
            ;; The enqueued path must be the canonical forward-slash form
            ;; so downstream DB lookups match.
            (should (equal (caar vulpea-db-sync--queue) file)))
        (when (file-directory-p dir)
          (delete-directory dir t))))))

;;; fswatch Cygwin support (Windows)

(ert-deftest vulpea-db-sync-fswatch-to-cygwin-path ()
  "A native Windows path is converted to the /cygdrive/ form."
  (should (equal (vulpea-db-sync--fswatch-to-cygwin-path "c:/Users/foo/notes")
                 "/cygdrive/c/Users/foo/notes"))
  ;; Upper-case drive is lowered.
  (should (equal (vulpea-db-sync--fswatch-to-cygwin-path "C:/X/y")
                 "/cygdrive/c/X/y"))
  ;; Backslashes are converted too.
  (should (equal (vulpea-db-sync--fswatch-to-cygwin-path "c:\\X\\y")
                 "/cygdrive/c/X/y"))
  ;; A path with no drive letter is returned unchanged.
  (should (equal (vulpea-db-sync--fswatch-to-cygwin-path "/home/foo/notes")
                 "/home/foo/notes")))

(ert-deftest vulpea-db-sync-fswatch-resolve-style ()
  "Path style honors the option, falling back to detection in auto mode."
  (let ((vulpea-db-sync--fswatch-effective-style nil))
    (let ((vulpea-db-sync-fswatch-path-style 'native))
      (should (eq (vulpea-db-sync--fswatch-resolve-style) 'native)))
    (let ((vulpea-db-sync-fswatch-path-style 'cygwin))
      (should (eq (vulpea-db-sync--fswatch-resolve-style) 'cygwin)))
    ;; auto with nothing detected yet defaults to native
    (let ((vulpea-db-sync-fswatch-path-style 'auto))
      (should (eq (vulpea-db-sync--fswatch-resolve-style) 'native)))
    ;; auto honors a previously detected style
    (let ((vulpea-db-sync-fswatch-path-style 'auto)
          (vulpea-db-sync--fswatch-effective-style 'cygwin))
      (should (eq (vulpea-db-sync--fswatch-resolve-style) 'cygwin)))))

(ert-deftest vulpea-db-sync-fswatch-cygwin-error-p ()
  "The Cygwin path-handling error is recognized; valid output is not."
  (should (vulpea-db-sync--fswatch-cygwin-error-p
           "Invalid handle when opening C:\\Users\\me\\notes."))
  (should-not (vulpea-db-sync--fswatch-cygwin-error-p
               "c:/Users/me/notes/file.org|||Updated"))
  (should-not (vulpea-db-sync--fswatch-cygwin-error-p nil)))

(ert-deftest vulpea-db-sync-fswatch-auto-switches-to-cygwin ()
  "Auto mode flips to the Cygwin style when native paths error out."
  (let ((vulpea-db-sync-fswatch-path-style 'auto)
        (vulpea-db-sync--fswatch-effective-style nil)
        (vulpea-db-sync--fswatch-seen-valid-event nil)
        (vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--fswatch-buffer "")
        (system-type 'windows-nt)
        (setup-called 0))
    (cl-letf (((symbol-function 'vulpea-db-sync--setup-fswatch)
               (lambda () (setq setup-called (1+ setup-called)))))
      (vulpea-db-sync--fswatch-filter
       nil "Invalid handle when opening C:\\Users\\me\\notes.\n")
      (should (eq vulpea-db-sync--fswatch-effective-style 'cygwin))
      (should (= setup-called 1)))))

(ert-deftest vulpea-db-sync-fswatch-auto-no-switch-after-valid ()
  "A watcher that already produced events is not switched by a stray error."
  (let ((vulpea-db-sync-fswatch-path-style 'auto)
        (vulpea-db-sync--fswatch-effective-style nil)
        ;; Pretend a valid event was already seen.
        (vulpea-db-sync--fswatch-seen-valid-event t)
        (vulpea-db-sync--fswatch-process nil)
        (vulpea-db-sync--fswatch-buffer "")
        (system-type 'windows-nt)
        (setup-called 0))
    (cl-letf (((symbol-function 'vulpea-db-sync--setup-fswatch)
               (lambda () (setq setup-called (1+ setup-called)))))
      (vulpea-db-sync--fswatch-filter
       nil "Invalid handle when opening C:\\Users\\me\\notes.\n")
      (should (null vulpea-db-sync--fswatch-effective-style))
      (should (= setup-called 0)))))

;;; Dir-Locals Tracking Tests

(defun vulpea-db-sync-test--write-note (path id title)
  "Write an org note with ID and TITLE to PATH."
  (with-temp-file path
    (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: %s\n"
                    id title))))

(defmacro vulpea-db-sync-test--with-reaction-spy (reactions &rest body)
  "Execute BODY with `vulpea-db-sync-update-directory' stubbed.
Calls are recorded in REACTIONS (a list variable bound by the
caller) as (DIR . FORCE) conses, newest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'vulpea-db-sync-update-directory)
              (lambda (dir &optional force)
                (push (cons dir force) ,reactions))))
     ,@body))

(ert-deftest vulpea-db-sync-dir-locals-file-p ()
  "Only dir-locals files outside hidden directories qualify."
  (should (vulpea-db-sync--dir-locals-file-p "/notes/.dir-locals.el"))
  (should (vulpea-db-sync--dir-locals-file-p "/notes/sub/.dir-locals-2.el"))
  (should-not (vulpea-db-sync--dir-locals-file-p nil))
  (should-not (vulpea-db-sync--dir-locals-file-p "/notes/note.org"))
  (should-not (vulpea-db-sync--dir-locals-file-p "/notes/dir-locals.el"))
  (should-not (vulpea-db-sync--dir-locals-file-p "/notes/.dir-locals.el~"))
  ;; Files under hidden directories are invisible to vulpea, their
  ;; dir-locals govern nothing that is indexed
  (should-not (vulpea-db-sync--dir-locals-file-p "/notes/.git/.dir-locals.el")))

(ert-deftest vulpea-db-sync-dir-locals-reaction-auto ()
  "In auto mode the gate follows parse method and extractor declarations.
Both `find-file' and `temp-buffer' apply dir-locals while parsing
\(the per-file `org-mode' rerun hacks local variables since Emacs
26); only `single-temp-buffer' never does."
  (let ((vulpea-db-sync-reindex-on-dir-locals-change 'auto)
        (vulpea-db--extractors nil))
    (let ((vulpea-db-parse-method 'single-temp-buffer))
      (should-not (vulpea-db-sync--dir-locals-reaction-p)))
    (let ((vulpea-db-parse-method 'temp-buffer))
      (should (vulpea-db-sync--dir-locals-reaction-p)))
    (let ((vulpea-db-parse-method 'find-file))
      (should (vulpea-db-sync--dir-locals-reaction-p)))
    ;; A registered extractor declaring :reads-dir-locals t enables the
    ;; reaction regardless of parse method
    (let ((vulpea-db-parse-method 'single-temp-buffer)
          (vulpea-db--extractors
           (list (make-vulpea-extractor :name 'x :extract-fn #'ignore
                                        :reads-dir-locals t))))
      (should (vulpea-db-sync--dir-locals-reaction-p)))
    ;; An extractor without the declaration does not
    (let ((vulpea-db-parse-method 'single-temp-buffer)
          (vulpea-db--extractors
           (list (make-vulpea-extractor :name 'x :extract-fn #'ignore))))
      (should-not (vulpea-db-sync--dir-locals-reaction-p)))))

(ert-deftest vulpea-db-sync-dir-locals-reaction-forced ()
  "Explicit t / nil override the auto detection in both directions."
  (let ((vulpea-db--extractors nil))
    (let ((vulpea-db-sync-reindex-on-dir-locals-change t)
          (vulpea-db-parse-method 'temp-buffer))
      (should (vulpea-db-sync--dir-locals-reaction-p)))
    (let ((vulpea-db-sync-reindex-on-dir-locals-change nil)
          (vulpea-db-parse-method 'find-file))
      (should-not (vulpea-db-sync--dir-locals-reaction-p)))))

(ert-deftest vulpea-db-sync-dir-locals-event-reacts-on-real-change ()
  "Create, edit and delete of a dir-locals file re-index the subtree.
A touch that leaves content unchanged does not."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-1" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        ;; Creation
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 1))
        (should (equal (caar reactions) root))
        (should (eq (cdar reactions) 'force))
        (should (vulpea-db--get-dir-locals-hash dl))
        ;; Touch without content change: tracked, but no reaction
        (set-file-times dl (time-add (current-time) 5))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 1))
        ;; Content change
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 100))))"))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 2))
        ;; Deletion
        (delete-file dl)
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 3))
        (should-not (vulpea-db--get-dir-locals-hash dl))
        ;; Deletion of an untracked file: nothing to react to
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 3))))))

(ert-deftest vulpea-db-sync-dir-locals-event-tracks-without-reaction-when-off ()
  "With the gate off, hashes are still tracked but nothing is re-indexed."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change nil)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-2" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should-not reactions)
        (should (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-dir-locals-no-reaction-without-org-files ()
  "A dir-locals change in a subtree without org files re-indexes nothing."
  (vulpea-test--with-temp-notes-dir
    (let ((dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--with-reaction-spy reactions
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should-not reactions)
        ;; Still tracked, so a later change is diffed correctly
        (should (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-dir-locals-react-message ()
  "The reaction announces the affected directory and file count."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil)
          (messages nil))
      (vulpea-db-sync-test--write-note note "dl-note-3" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (with-temp-file dl
            (insert "((org-mode . ((fill-column . 80))))"))
          (vulpea-db-sync--handle-dir-locals-event dl)))
      (should (seq-some
               (lambda (m)
                 (string-match-p "dir-locals .* under .*, re-indexing 1 file"
                                 m))
               messages)))))

(ert-deftest vulpea-db-sync-dir-locals-check-baseline-is-quiet ()
  "The first scan only records hashes; it never re-indexes.
There is no way to distinguish \"existed all along\" from \"created
while the feature was not yet tracking\", and reacting would force a
redundant full pass on every fresh database."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-4" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--check-dir-locals (list note))
        (should-not reactions)
        (should (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-dir-locals-check-detects-offline-edit ()
  "An edit made while Emacs was closed is caught by the startup scan."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-5" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        ;; Baseline
        (vulpea-db-sync--check-dir-locals (list note))
        (should-not reactions)
        ;; "Offline" edit (different size, so mtime resolution is moot)
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 100) (my-var . t))))"))
        (vulpea-db-sync--check-dir-locals (list note))
        (should (= (length reactions) 1))
        (should (equal (caar reactions) root))
        (should (eq (cdar reactions) 'force))
        ;; Unchanged on the next scan: quiet
        (vulpea-db-sync--check-dir-locals (list note))
        (should (= (length reactions) 1))))))

(ert-deftest vulpea-db-sync-dir-locals-check-detects-offline-create ()
  "A dir-locals file created while Emacs was closed is caught."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-6" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        ;; Baseline without any dir-locals file
        (vulpea-db-sync--check-dir-locals (list note))
        (should-not reactions)
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--check-dir-locals (list note))
        (should (= (length reactions) 1))))))

(ert-deftest vulpea-db-sync-dir-locals-check-detects-offline-delete ()
  "A dir-locals file deleted while Emacs was closed is caught."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-7" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--check-dir-locals (list note))
        (should-not reactions)
        (delete-file dl)
        (vulpea-db-sync--check-dir-locals (list note))
        (should (= (length reactions) 1))
        (should-not (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-dir-locals-check-finds-parent-dir-locals ()
  "A dir-locals file above the note's own directory is tracked too."
  (vulpea-test--with-temp-notes-dir
    (let* ((subdir (file-name-as-directory (expand-file-name "sub" root)))
           (note (expand-file-name "note.org" subdir))
           (dl (expand-file-name ".dir-locals.el" root))
           (vulpea-db-sync-reindex-on-dir-locals-change t)
           (reactions nil))
      (make-directory subdir)
      (vulpea-db-sync-test--write-note note "dl-note-8" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--check-dir-locals (list note))
        (should (vulpea-db--get-dir-locals-hash dl))
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 100) (my-var . t))))"))
        (vulpea-db-sync--check-dir-locals (list note))
        (should (= (length reactions) 1))
        (should (equal (caar reactions) root))))))

(ert-deftest vulpea-db-sync-dir-locals-check-drops-untracked-rows ()
  "Rows outside `vulpea-db-sync-directories' are dropped silently."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-9" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--check-dir-locals (list note))
        (vulpea-db--update-dir-locals-hash
         "/nonexistent-elsewhere/.dir-locals.el" "stale" 1.0 10)
        (vulpea-db-sync--check-dir-locals (list note))
        (should-not reactions)
        (should-not (vulpea-db--get-dir-locals-hash
                     "/nonexistent-elsewhere/.dir-locals.el"))))))

(ert-deftest vulpea-db-sync-file-notify-routes-dir-locals ()
  "File-notify events for dir-locals files reach the handler."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-10" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--file-notify-callback (list 'dummy 'changed dl))
        (should (= (length reactions) 1))
        (should (vulpea-db--get-dir-locals-hash dl))
        ;; Deletion event
        (delete-file dl)
        (vulpea-db-sync--file-notify-callback (list 'dummy 'deleted dl))
        (should (= (length reactions) 2))
        (should-not (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-file-notify-renamed-dir-locals ()
  "Renames to and from the dir-locals name are handled on both sides."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (other (expand-file-name "dir-locals.bak" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-11" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--handle-dir-locals-event dl)
        (should (= (length reactions) 1))
        ;; Renamed away: dir-locals no longer applies
        (rename-file dl other)
        (vulpea-db-sync--file-notify-callback
         (list 'dummy 'renamed dl other))
        (should (= (length reactions) 2))
        (should-not (vulpea-db--get-dir-locals-hash dl))
        ;; Renamed back into place
        (rename-file other dl)
        (vulpea-db-sync--file-notify-callback
         (list 'dummy 'renamed other dl))
        (should (= (length reactions) 3))
        (should (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-fswatch-routes-dir-locals ()
  "fswatch events for dir-locals files reach the handler."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (vulpea-db-sync--fswatch-buffer "")
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-12" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 80))))"))
        (vulpea-db-sync--fswatch-filter nil (format "%s|||Updated\n" dl))
        (should (= (length reactions) 1))
        (should (vulpea-db--get-dir-locals-hash dl))
        ;; Removal
        (delete-file dl)
        (vulpea-db-sync--fswatch-filter nil (format "%s|||Removed\n" dl))
        (should (= (length reactions) 2))
        (should-not (vulpea-db--get-dir-locals-hash dl))))))

(ert-deftest vulpea-db-sync-fswatch-directory-created-checks-dir-locals ()
  "A directory appearing via fswatch is scanned for dir-locals files too."
  (vulpea-test--with-temp-notes-dir
    (let* ((subdir (file-name-as-directory (expand-file-name "moved-in" root)))
           (note (expand-file-name "note.org" subdir))
           (dl (expand-file-name ".dir-locals.el" subdir))
           (vulpea-db-sync-reindex-on-dir-locals-change nil)
           (vulpea-db-sync--fswatch-buffer "")
           (vulpea-db-sync--queue nil)
           (vulpea-db-sync--queue-tail nil))
      (clrhash vulpea-db-sync--queue-set)
      (make-directory subdir)
      (vulpea-db-sync-test--write-note note "dl-note-13" "Note")
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync--fswatch-filter
       nil (format "%s|||Created\n" (directory-file-name subdir)))
      ;; Org file inside is queued (existing behavior) and the
      ;; dir-locals file is now tracked
      (should (assoc note vulpea-db-sync--queue))
      (should (vulpea-db--get-dir-locals-hash dl)))))

(ert-deftest vulpea-db-sync-polling-detects-dir-locals-changes ()
  "The polling backend picks up dir-locals edits between ticks."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-14" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        ;; Baseline
        (vulpea-db-sync--check-dir-locals (list note))
        (with-temp-file dl
          (insert "((org-mode . ((fill-column . 100) (my-var . t))))"))
        ;; A poll tick diffs dir-locals along with the org files
        (let ((vulpea-db-sync--queue nil)
              (vulpea-db-sync--queue-tail nil))
          (vulpea-db-sync--check-external-changes-with-files (list note)))
        (should (= (length reactions) 1))))))

(ert-deftest vulpea-db-sync-dir-locals-react-queues-under-autosync ()
  "With autosync on, the reaction enqueues force marks, never blocks.
Reactions fire from process filters, sentinels and timers;
synchronous extraction there would freeze Emacs with quits
inhibited, so the subtree must flow through the regular queue."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (vulpea-db-autosync-mode t)
          (vulpea-db-sync--queue nil)
          (vulpea-db-sync--queue-tail nil)
          (vulpea-db-sync--timer nil)
          (update-directory-called nil))
      (clrhash vulpea-db-sync--queue-set)
      (clrhash vulpea-db-sync--force-set)
      (vulpea-db-sync-test--write-note note "dl-note-15" "Note")
      (vulpea-db-update-file note)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'vulpea-db-sync-update-directory)
                       (lambda (&rest _) (setq update-directory-called t))))
              (with-temp-file dl
                (insert "((org-mode . ((fill-column . 80))))"))
              (vulpea-db-sync--handle-dir-locals-event dl))
            (should-not update-directory-called)
            (should (assoc note vulpea-db-sync--queue))
            (should (gethash note vulpea-db-sync--force-set)))
        (when vulpea-db-sync--timer
          (cancel-timer vulpea-db-sync--timer))
        (clrhash vulpea-db-sync--queue-set)
        (clrhash vulpea-db-sync--force-set)))))

(ert-deftest vulpea-db-sync-dir-locals-react-survives-deleted-directory ()
  "An event for a dir-locals file in a removed subtree must not signal.
Deleting a whole subtree delivers the dir-locals event after the
directory is gone; a signal here would abort the surrounding fswatch
batch or the startup scan callback."
  (vulpea-test--with-temp-notes-dir
    (let* ((subdir (file-name-as-directory (expand-file-name "proj" root)))
           (note (expand-file-name "note.org" subdir))
           (dl (expand-file-name ".dir-locals.el" subdir))
           (vulpea-db-sync-reindex-on-dir-locals-change t)
           (reactions nil))
      (make-directory subdir)
      (vulpea-db-sync-test--write-note note "dl-note-16" "Note")
      (vulpea-db-update-file note)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (let ((vulpea-db-sync-reindex-on-dir-locals-change nil))
        (vulpea-db-sync--handle-dir-locals-event dl))
      (should (vulpea-db--get-dir-locals-hash dl))
      (delete-directory subdir t)
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--handle-dir-locals-event dl))
      (should-not (vulpea-db--get-dir-locals-hash dl))
      ;; Nothing left under the directory, so nothing to re-index
      (should-not reactions))))

(ert-deftest vulpea-db-sync-dir-locals-event-establishes-baseline ()
  "Rows written by live events count as an established baseline.
Otherwise this sequence silently swallows an offline edit: events
track the file in session 1 (no scan ever runs, e.g.
`vulpea-db-sync-scan-on-enable' nil), the file is edited while Emacs
is closed, and the first-ever scan in session 2 treats its diff as
baseline noise."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root))
          (dl (expand-file-name ".dir-locals.el" root))
          (vulpea-db-sync-reindex-on-dir-locals-change t)
          (reactions nil))
      (vulpea-db-sync-test--write-note note "dl-note-17" "Note")
      (vulpea-db-update-file note)
      ;; Live event tracks the file; no scan has ever run
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (let ((vulpea-db-sync-reindex-on-dir-locals-change nil))
        (vulpea-db-sync--handle-dir-locals-event dl))
      ;; "Offline" edit, then the first scan: must diff, not baseline
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 100) (my-var . t))))"))
      (vulpea-db-sync-test--with-reaction-spy reactions
        (vulpea-db-sync--check-dir-locals (list note)))
      (should (= (length reactions) 1)))))

(ert-deftest vulpea-db-sync-dir-locals-clear-resets-baseline ()
  "`vulpea-db-clear' drops the baseline marker along with the rows.
A kept marker would make the post-clear rescan treat every
dir-locals file as newly created and fire spurious reactions."
  (vulpea-test--with-temp-notes-dir
    (let ((note (expand-file-name "note.org" root)))
      (vulpea-db-sync-test--write-note note "dl-note-18" "Note")
      (vulpea-db-update-file note)
      (vulpea-db-sync--check-dir-locals (list note))
      (should (vulpea-db-sync--dir-locals-baseline-p))
      (vulpea-db-clear)
      (should-not (vulpea-db-sync--dir-locals-baseline-p)))))

(ert-deftest vulpea-db-sync-handle-removed-directory-drops-dir-locals-rows ()
  "Removing a directory drops dir-locals rows under it."
  (vulpea-test--with-temp-notes-dir
    (let* ((subdir (file-name-as-directory (expand-file-name "sub" root)))
           (dl (expand-file-name ".dir-locals.el" subdir)))
      (make-directory subdir)
      (with-temp-file dl
        (insert "((org-mode . ((fill-column . 80))))"))
      (let ((vulpea-db-sync-reindex-on-dir-locals-change nil))
        (vulpea-db-sync--handle-dir-locals-event dl))
      (should (vulpea-db--get-dir-locals-hash dl))
      (delete-directory subdir t)
      (vulpea-db-sync--handle-removed-file (directory-file-name subdir))
      (should-not (vulpea-db--get-dir-locals-hash dl)))))

(provide 'vulpea-db-sync-test)
;;; vulpea-db-sync-test.el ends here
