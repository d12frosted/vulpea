;;; vulpea-tags-test.el --- Tests for vulpea-tags -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 03 Feb 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `vulpea-tags' module.
;;
;;; Code:

(require 'ert)
(require 'vulpea-tags)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)
(require 'vulpea-utils)

;;; Test Helpers

(defvar vulpea-tags-test--fixture-dir
  (expand-file-name "test/note-files"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Directory containing test fixture files.")

(defvar vulpea-tags-test--notes-dir nil
  "Temporary notes directory for current test.")

(defmacro vulpea-tags-test--with-temp-db (&rest body)
  "Execute BODY with a temporary database initialized with test fixtures."
  (declare (indent 0))
  (let ((db-file-var (make-symbol "temp-db-file"))
        (notes-dir-var (make-symbol "temp-notes-dir")))
    `(progn
       ;; Clean up orphan org buffers from previous tests
       ;; This prevents `save-some-buffers' in batch operations from
       ;; trying to save buffers pointing to deleted temp directories
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-match-p "\\.org$" (buffer-file-name buf))
                    (not (file-exists-p (buffer-file-name buf))))
           (with-current-buffer buf
             (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (let* ((,db-file-var (make-temp-file "vulpea-tags-test-" nil ".db"))
              (,notes-dir-var (expand-file-name (make-temp-name "vulpea-tags-notes-")
                               temporary-file-directory))
              (vulpea-db-location ,db-file-var)
              (vulpea-db--connection nil)
              (vulpea-tags-test--notes-dir ,notes-dir-var))
         ;; Create temp notes directory
         (make-directory ,notes-dir-var t)
         (unwind-protect
             (progn
            ;; Copy all fixture files to temp directory
            (dolist (file (directory-files vulpea-tags-test--fixture-dir t "\\.org$"))
             (copy-file file (expand-file-name (file-name-nondirectory file) ,notes-dir-var)))
            ;; Initialize database
            (vulpea-db)
            ;; Update database with all files
            (dolist (file (directory-files ,notes-dir-var t "\\.org$"))
             (vulpea-db-update-file file))
            ,@body)
        ;; Cleanup: kill all org buffers from this test
        (dolist (buf (buffer-list))
          (when (and (buffer-file-name buf)
                     (string-prefix-p ,notes-dir-var (buffer-file-name buf)))
            (kill-buffer buf)))
        (when vulpea-db--connection
         (vulpea-db-close))
        (when (file-exists-p ,db-file-var)
         (delete-file ,db-file-var))
        (when (file-exists-p ,notes-dir-var)
         (delete-directory ,notes-dir-var t)))))))

(defun vulpea-tags-test--save-all-buffers ()
  "Save all buffers visiting files in the test notes directory."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-prefix-p vulpea-tags-test--notes-dir buffer-file-name))
        (save-buffer)))))

(defun vulpea-tags-test--file-content (file-name)
  "Get the content of FILE-NAME in test notes directory."
  (let ((file-path (expand-file-name file-name vulpea-tags-test--notes-dir)))
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

;;; vulpea-tags Tests (per-note get)

(ert-deftest vulpea-tags-get-file-level-with-tags ()
  "Test getting tags from a file-level note that has tags."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (should (equal (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                  '("tag1" "tag2" "tag3")))))

(ert-deftest vulpea-tags-get-file-level-without-tags ()
  "Test getting tags from a file-level note without tags."
  (vulpea-tags-test--with-temp-db
   ;; without-meta.org has no tags
   (should (equal (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                  nil))))

(ert-deftest vulpea-tags-get-with-note-struct ()
  "Test getting tags by passing a note struct."
  (vulpea-tags-test--with-temp-db
   (let ((note (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (should (equal (vulpea-tags note)
                    '("tag1" "tag2" "tag3"))))))

;;; vulpea-tags-add Tests

(ert-deftest vulpea-tags-add-single-tag ()
  "Test adding a single tag to a note."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-add "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "newtag")
   (should (equal (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                  '("newtag")))))

(ert-deftest vulpea-tags-add-multiple-tags ()
  "Test adding multiple tags to a note."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-add "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "tag1" "tag2")
   (let ((tags (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))
     (should (member "tag1" tags))
     (should (member "tag2" tags)))))

(ert-deftest vulpea-tags-add-to-note-with-existing-tags ()
  "Test adding tags to a note that already has tags."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-add "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "tag4")
   (let ((tags (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (should (= (length tags) 4))
     (should (member "tag1" tags))
     (should (member "tag4" tags)))))

(ert-deftest vulpea-tags-add-duplicate-tag ()
  "Test that adding a duplicate tag does not create duplicates."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-add "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "tag1")
   (let ((tags (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (should (= (length tags) 3)))))

;;; vulpea-tags-remove Tests

(ert-deftest vulpea-tags-remove-single-tag ()
  "Test removing a single tag from a note."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-remove "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "tag2")
   (let ((tags (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (should (= (length tags) 2))
     (should (member "tag1" tags))
     (should (member "tag3" tags))
     (should-not (member "tag2" tags)))))

(ert-deftest vulpea-tags-remove-multiple-tags ()
  "Test removing multiple tags from a note."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-remove "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "tag1" "tag3")
   (should (equal (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                  '("tag2")))))

(ert-deftest vulpea-tags-remove-nonexistent-tag ()
  "Test removing a tag that doesn't exist has no effect."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-remove "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "nonexistent")
   (should (equal (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                  '("tag1" "tag2" "tag3")))))

(ert-deftest vulpea-tags-remove-from-note-without-tags ()
  "Test removing tags from a note without any tags has no effect."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-remove "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "sometag")
   (should (equal (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                  nil))))

;;; vulpea-tags-set Tests

(ert-deftest vulpea-tags-set-replace-all ()
  "Test that vulpea-tags-set replaces all existing tags."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "new1" "new2")
   (should (equal (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                  '("new1" "new2")))))

(ert-deftest vulpea-tags-set-in-note-without-tags ()
  "Test setting tags in a note that has no tags."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "a" "b" "c")
   (should (equal (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                  '("a" "b" "c")))))

(ert-deftest vulpea-tags-set-empty-removes-all ()
  "Test that setting no tags removes all tags."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   (vulpea-tags-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
   (should (equal (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                  nil))))

;;; File formatting Tests

(ert-deftest vulpea-tags-add-format-in-file ()
  "Test that tags are formatted correctly in file."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-add "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "newtag")
   (vulpea-tags-test--save-all-buffers)
   (should (string-match-p "#\\+filetags:.*:newtag:"
                           (vulpea-tags-test--file-content "without-meta.org")))))

(ert-deftest vulpea-tags-set-format-in-file ()
  "Test that setting tags creates correct filetags line."
  (vulpea-tags-test--with-temp-db
   (vulpea-tags-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "x" "y" "z")
   (vulpea-tags-test--save-all-buffers)
   (should (string-match-p "#\\+filetags: :x:y:z:"
                           (vulpea-tags-test--file-content "without-meta.org")))))

;;; Heading-level tag Tests

(ert-deftest vulpea-tags-heading-add ()
  "Test adding tags to a heading-level note."
  (vulpea-tags-test--with-temp-db
   ;; Section One has no tags initially
   (vulpea-tags-add "11111111-1111-1111-1111-111111111111" "htag1" "htag2")
   (let ((tags (vulpea-tags "11111111-1111-1111-1111-111111111111")))
     (should (member "htag1" tags))
     (should (member "htag2" tags)))))

(ert-deftest vulpea-tags-heading-remove ()
  "Test removing tags from a heading-level note."
  (vulpea-tags-test--with-temp-db
   ;; First add tags
   (vulpea-tags-add "11111111-1111-1111-1111-111111111111" "htag1" "htag2")
   ;; Then remove one
   (vulpea-tags-remove "11111111-1111-1111-1111-111111111111" "htag1")
   (let ((tags (vulpea-tags "11111111-1111-1111-1111-111111111111")))
     (should-not (member "htag1" tags))
     (should (member "htag2" tags)))))

(ert-deftest vulpea-tags-heading-does-not-affect-file ()
  "Test that heading tag changes don't affect file-level tags."
  (vulpea-tags-test--with-temp-db
   ;; Add tags to heading
   (vulpea-tags-add "11111111-1111-1111-1111-111111111111" "headingtag")
   ;; File-level note should not have the tag
   (should-not (member "headingtag"
                       (vulpea-tags "a1b2c3d4-e5f6-7890-abcd-ef1234567890")))))

;;; Batch operation Tests

(ert-deftest vulpea-tags-batch-add-single-note ()
  "Test batch adding a tag to a single note."
  (vulpea-tags-test--with-temp-db
   (let ((notes (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))
     (vulpea-tags-batch-add notes "batchtag")
     (should (member "batchtag" (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))))

(ert-deftest vulpea-tags-batch-add-multiple-notes ()
  "Test batch adding a tag to multiple notes."
  (vulpea-tags-test--with-temp-db
   (let ((notes (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                      (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))
     (vulpea-tags-batch-add notes "common")
     (should (member "common" (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))
     (should (member "common" (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))))

(ert-deftest vulpea-tags-batch-remove-from-multiple-notes ()
  "Test batch removing a tag from multiple notes."
  (vulpea-tags-test--with-temp-db
   ;; First add a common tag to two notes
   (vulpea-tags-add "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "common")
   (vulpea-tags-add "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "common")
   ;; Now batch remove it
   (let ((notes (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                      (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))
     (vulpea-tags-batch-remove notes "common")
     (should-not (member "common" (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))
     (should-not (member "common" (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))))

(ert-deftest vulpea-tags-batch-rename ()
  "Test renaming a tag across all notes that have it."
  (vulpea-tags-test--with-temp-db
   ;; reference.org has :tag1:tag2:tag3:
   ;; Rename tag1 -> newtag1
   (vulpea-tags-batch-rename "tag1" "newtag1")
   (let ((tags (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (should (member "newtag1" tags))
     (should-not (member "tag1" tags))
     ;; Other tags should be preserved
     (should (member "tag2" tags))
     (should (member "tag3" tags)))))

(ert-deftest vulpea-tags-batch-rename-affects-all-notes ()
  "Test that batch rename affects all notes with the tag."
  (vulpea-tags-test--with-temp-db
   ;; Add tag1 to another note and sync to DB
   (vulpea-tags-add "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "tag1")
   (vulpea-tags-test--save-all-buffers)
   (vulpea-db-update-file (expand-file-name "without-meta.org" vulpea-tags-test--notes-dir))
   ;; Rename tag1 -> renamed
   (vulpea-tags-batch-rename "tag1" "renamed")
   ;; Both notes should have the renamed tag
   (should (member "renamed" (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
   (should (member "renamed" (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))
   ;; Neither should have the old tag
   (should-not (member "tag1" (vulpea-tags "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
   (should-not (member "tag1" (vulpea-tags "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))))

(ert-deftest vulpea-tags-batch-rename-nonexistent ()
  "Test that renaming a nonexistent tag has no effect."
  (vulpea-tags-test--with-temp-db
   (let ((count (vulpea-tags-batch-rename "nonexistent" "something")))
     (should (= count 0)))))

(provide 'vulpea-tags-test)
;;; vulpea-tags-test.el ends here
