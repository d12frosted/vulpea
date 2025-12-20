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
            (vulpea-db-sync--check-external-changes)

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
          (vulpea-db-sync--check-external-changes)

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
            (vulpea-db-sync--check-external-changes)
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
              (vulpea-db-sync--check-external-changes)

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
              (vulpea-db-sync--check-external-changes)

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
                (vulpea-db-sync--check-external-changes)

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

(provide 'vulpea-db-sync-test)
;;; vulpea-db-sync-test.el ends here
