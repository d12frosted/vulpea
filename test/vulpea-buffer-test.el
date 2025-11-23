;;; vulpea-buffer-test.el --- Tests for vulpea-buffer -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2025 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 13 May 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `vulpea-buffer' module (v2).
;;
;;; Code:

(require 'ert)
(require 'vulpea-buffer)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)
(require 'vulpea-utils)

;;; Test Helpers

(defun vulpea-buffer-test--create-temp-file (id content)
  "Create temporary org file with ID and CONTENT.
Returns the file path."
  (let ((file (make-temp-file "vulpea-buffer-test-" nil ".org")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n%s" id content)))
    file))

(defmacro vulpea-buffer-test--with-temp-db-and-file (id content &rest body)
  "Execute BODY with temporary database and file.
Creates a temp file with ID and CONTENT, adds it to temp DB, then executes BODY."
  (declare (indent 2))
  `(let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
          (vulpea-db-location temp-db-file)
          (vulpea-db--connection nil)
          (temp-org-file (vulpea-buffer-test--create-temp-file ,id ,content)))
    (unwind-protect
        (progn
          (vulpea-db)
          (vulpea-db-update-file temp-org-file)
          ,@body)
      (when vulpea-db--connection
       (vulpea-db-close))
      (when (file-exists-p temp-db-file)
       (delete-file temp-db-file))
      (when (file-exists-p temp-org-file)
       (delete-file temp-org-file)))))

;;; vulpea-buffer-title-set Tests

(ert-deftest vulpea-buffer-title-set-in-note-without-title ()
  "Test setting title in note without title."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (vulpea-utils-with-note (vulpea-db-get-by-id id)
        (vulpea-buffer-title-set "Some title")
        (save-buffer)
        (vulpea-db-update-file (vulpea-note-path (vulpea-db-get-by-id id))))
      (should (equal (vulpea-note-title (vulpea-db-get-by-id id))
                     "Some title")))))

(ert-deftest vulpea-buffer-title-set-at-min-point ()
  "Test setting title when point is at beginning of buffer."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Original Title\n\nContent.\n"
      (vulpea-utils-with-note (vulpea-db-get-by-id id)
        (goto-char (point-min))
        (vulpea-buffer-title-set "Changed title")
        (save-buffer)
        (vulpea-db-update-file (vulpea-note-path (vulpea-db-get-by-id id))))
      (should (equal (vulpea-note-title (vulpea-db-get-by-id id))
                     "Changed title")))))

(ert-deftest vulpea-buffer-title-set-at-max-point ()
  "Test setting title when point is at end of buffer."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Original Title\n\nContent.\n"
      (vulpea-utils-with-note (vulpea-db-get-by-id id)
        (goto-char (point-max))
        (vulpea-buffer-title-set "Changed title")
        (save-buffer)
        (vulpea-db-update-file (vulpea-note-path (vulpea-db-get-by-id id))))
      (should (equal (vulpea-note-title (vulpea-db-get-by-id id))
                     "Changed title")))))

(ert-deftest vulpea-buffer-title-set-in-heading ()
  "Test setting title in file from heading context."
  (let ((file-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
        (heading-id "cfc39858-351d-4f1e-8f98-10d16d71f49e"))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil)
           (temp-org-file (vulpea-buffer-test--create-temp-file
                           file-id
                           (format "#+title: Original\n\n* Heading 1\n:PROPERTIES:\n:ID: %s\n:END:\n" heading-id))))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file temp-org-file)
            (vulpea-utils-with-note (vulpea-db-get-by-id heading-id)
              (goto-char (point-max))
              (vulpea-buffer-title-set "Changed title")
              (save-buffer))
            (vulpea-db-update-file temp-org-file)
            (should (equal (vulpea-note-title (vulpea-db-get-by-id file-id))
                           "Changed title")))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p temp-org-file)
          (delete-file temp-org-file))))))

;;; vulpea-buffer-tags-* Tests

(ert-deftest vulpea-buffer-tags-get-empty ()
  "Test getting empty tags list."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-get))
                     nil)))))

(ert-deftest vulpea-buffer-tags-set-multiple ()
  "Test setting multiple tags at once."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-set "super_tag_1" "super_tag_2")
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     '("super_tag_1" "super_tag_2"))))))

(ert-deftest vulpea-buffer-tags-clear ()
  "Test clearing tags."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n#+filetags: :tag1:tag2:tag3:\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-set)
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     nil)))))

(ert-deftest vulpea-buffer-tags-add-first ()
  "Test adding first tag."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-add "super_tag_1")
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     '("super_tag_1"))))))

(ert-deftest vulpea-buffer-tags-add-another ()
  "Test adding one more tag."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n#+filetags: :tag1:tag2:tag3:\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-add "super_tag_1")
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     '("tag1" "tag2" "tag3" "super_tag_1"))))))

(ert-deftest vulpea-buffer-tags-remove-one ()
  "Test removing one tag."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n#+filetags: :tag1:tag2:tag3:\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-remove "tag1")
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     '("tag2" "tag3"))))))

(ert-deftest vulpea-buffer-tags-remove-all ()
  "Test removing all tags one by one."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n#+filetags: :tag1:tag2:tag3:\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-tags-remove "tag1")
                       (vulpea-buffer-tags-remove "tag2")
                       (vulpea-buffer-tags-remove "tag3")
                       (save-buffer)
                       (vulpea-buffer-tags-get))
                     nil)))))

;;; vulpea-buffer-alias-* Tests

(ert-deftest vulpea-buffer-alias-get-empty ()
  "Test getting empty aliases list."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-alias-get))
                     nil)))))

(ert-deftest vulpea-buffer-alias-get-single ()
  "Test getting single alias."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-get))
                           '("Alias1"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-get-multiple ()
  "Test getting multiple aliases."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2 Alias3\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-get))
                           '("Alias1" "Alias2" "Alias3"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-get-with-spaces ()
  "Test getting alias with spaces (quoted)."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: \"Alias With Spaces\"\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-get))
                           '("Alias With Spaces"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-add-first ()
  "Test adding first alias."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-alias-add "FirstAlias")
                       (save-buffer)
                       (vulpea-buffer-alias-get))
                     '("FirstAlias"))))))

(ert-deftest vulpea-buffer-alias-add-another ()
  "Test adding another alias."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-add "Alias3")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           '("Alias1" "Alias2" "Alias3"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-add-with-spaces ()
  "Test adding alias containing spaces (auto-quoted)."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-alias-add "Alias With Spaces")
                       (save-buffer)
                       (vulpea-buffer-alias-get))
                     '("Alias With Spaces"))))))

(ert-deftest vulpea-buffer-alias-add-duplicate ()
  "Test that adding duplicate alias is ignored."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-add "Alias1")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           '("Alias1" "Alias2"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-remove-one ()
  "Test removing one alias."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2 Alias3\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-remove "Alias1")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           '("Alias2" "Alias3"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-remove-last ()
  "Test that removing last alias removes property entirely."
  (let* ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: OnlyAlias\n:END:\n#+title: Test\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-remove "OnlyAlias")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           nil)))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-remove-with-spaces ()
  "Test removing alias containing spaces."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Simple \"Alias With Spaces\" Another\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-remove "Alias With Spaces")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           '("Simple" "Another"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-set-new ()
  "Test setting aliases on note without existing aliases."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-alias-set "First" "Second" "Third")
                       (save-buffer)
                       (vulpea-buffer-alias-get))
                     '("First" "Second" "Third"))))))

(ert-deftest vulpea-buffer-alias-set-replace ()
  "Test replacing existing aliases."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Old1 Old2\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-set "New1" "New2" "New3")
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           '("New1" "New2" "New3"))))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-buffer-alias-set-with-spaces ()
  "Test setting aliases with spaces (auto-quoted)."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Test\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-alias-set "Simple" "Alias With Spaces")
                       (save-buffer)
                       (vulpea-buffer-alias-get))
                     '("Simple" "Alias With Spaces"))))))

(ert-deftest vulpea-buffer-alias-set-empty ()
  "Test setting empty list removes the property."
  (let* ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
         (file (vulpea-buffer-test--create-temp-file id "")))
    (with-temp-file file
      (insert (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2\n:END:\n#+title: Reference\n" id)))
    (let* ((temp-db-file (make-temp-file "vulpea-buffer-test-" nil ".db"))
           (vulpea-db-location temp-db-file)
           (vulpea-db--connection nil))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file file)
            (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                             (vulpea-buffer-alias-set)
                             (save-buffer)
                             (vulpea-buffer-alias-get))
                           nil)))
        (when vulpea-db--connection
          (vulpea-db-close))
        (when (file-exists-p temp-db-file)
          (delete-file temp-db-file))
        (when (file-exists-p file)
          (delete-file file))))))

;;; vulpea-buffer-prop-* Tests

(ert-deftest vulpea-buffer-prop-downcase-name ()
  "Test that property names are downcased."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-set "TITLE" "Title 1")
                       (save-buffer)
                       (vulpea-buffer-prop-get "title"))
                     "Title 1")))))

(ert-deftest vulpea-buffer-prop-get-ignore-case ()
  "Test that property retrieval is case-insensitive."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-set "TITLE" "Title 1")
                       (save-buffer)
                       (vulpea-buffer-prop-get "TiTle"))
                     "Title 1")))))

(ert-deftest vulpea-buffer-prop-replace-existing ()
  "Test replacing existing property."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-set "TITLE" "Title 1")
                       (vulpea-buffer-prop-set "TiTlE" "Title 2")
                       (save-buffer)
                       (vulpea-buffer-prop-get "title"))
                     "Title 2")))))

(ert-deftest vulpea-buffer-prop-remove ()
  "Test removing existing property."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-get "title"))
                     "Reference"))
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-remove "title")
                       (save-buffer)
                       (vulpea-buffer-prop-get "title"))
                     nil)))))

(ert-deftest vulpea-buffer-prop-list-values ()
  "Test list property values."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-set-list "values" '("value 1" "value 2" "single"))
                       (save-buffer)
                       (vulpea-buffer-prop-get-list "values"))
                     '("value 1" "value 2" "single"))))))

(ert-deftest vulpea-buffer-prop-ignore-trailing-spaces ()
  "Test that trailing spaces are ignored."
  (let ((id "2c3bd05d-b3d1-40bc-bd42-f019d441592c"))
    (vulpea-buffer-test--with-temp-db-and-file id "\n\nSome body.\n"
      (should (equal (vulpea-utils-with-note (vulpea-db-get-by-id id)
                       (vulpea-buffer-prop-set "value" "             ")
                       (save-buffer)
                       (vulpea-buffer-prop-get "value"))
                     nil)))))

;;; vulpea-buffer-meta-format Tests

(ert-deftest vulpea-buffer-meta-format-url ()
  "Test formatting a URL."
  (should (equal (vulpea-buffer-meta-format "https://www.wikipedia.org/")
                 "[[https://www.wikipedia.org/][wikipedia.org]]")))

(ert-deftest vulpea-buffer-meta-format-note-id ()
  "Test formatting a note ID as link."
  (let ((id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
    (vulpea-buffer-test--with-temp-db-and-file id "#+title: Reference\n"
      (should (equal (vulpea-buffer-meta-format id)
                     "[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]")))))

(ert-deftest vulpea-buffer-meta-format-attachment ()
  "Test formatting attachment with UUID in name."
  (should (equal (vulpea-buffer-meta-format "[[attachment:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7.png]]")
                 "[[attachment:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7.png]]")))

(ert-deftest vulpea-buffer-meta-format-string ()
  "Test formatting regular string."
  (should (equal (vulpea-buffer-meta-format "hello")
                 "hello")))

(ert-deftest vulpea-buffer-meta-format-unknown-note-error ()
  "Test user-error for unknown note ID."
  (should-error (vulpea-buffer-meta-format "d36125b3-39e1-4bc3-8f7d-126159d8d60e")
                :type 'user-error))

(ert-deftest vulpea-buffer-meta-format-unsupported-type-error ()
  "Test user-error for unsupported resource type."
  (should-error (vulpea-buffer-meta-format '(1 2 3))
                :type 'user-error))

(provide 'vulpea-buffer-test)
;;; vulpea-buffer-test.el ends here
