;;; vulpea-meta-test.el --- Tests for vulpea-meta -*- lexical-binding: t; -*-
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
;; Test `vulpea-meta' module (v2).
;;
;;; Code:

(require 'ert)
(require 'vulpea-meta)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)
(require 'vulpea-utils)

;;; Test Helpers

(defvar vulpea-meta-test--fixture-dir
  (expand-file-name "test/note-files"
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Directory containing test fixture files.")

(defvar vulpea-meta-test--notes-dir nil
  "Temporary notes directory for current test.")

(defmacro vulpea-meta-test--with-temp-db (&rest body)
  "Execute BODY with a temporary database initialized with test fixtures."
  (declare (indent 0))
  (let ((db-file-var (make-symbol "temp-db-file"))
        (notes-dir-var (make-symbol "temp-notes-dir")))
    `(let* ((,db-file-var (make-temp-file "vulpea-meta-test-" nil ".db"))
            (,notes-dir-var (expand-file-name (make-temp-name "vulpea-meta-notes-")
                             temporary-file-directory))
            (vulpea-db-location ,db-file-var)
            (vulpea-db--connection nil)
            (vulpea-meta-test--notes-dir ,notes-dir-var))
      ;; Create temp notes directory
      (make-directory ,notes-dir-var t)
      (unwind-protect
          (progn
            ;; Copy all fixture files to temp directory
            (dolist (file (directory-files vulpea-meta-test--fixture-dir t "\\.org$"))
             (copy-file file (expand-file-name (file-name-nondirectory file) ,notes-dir-var)))
            ;; Initialize database
            (vulpea-db)
            ;; Update database with all files
            (dolist (file (directory-files ,notes-dir-var t "\\.org$"))
             (vulpea-db-update-file file))
            ,@body)
        (when vulpea-db--connection
         (vulpea-db-close))
        (when (file-exists-p ,db-file-var)
         (delete-file ,db-file-var))
        (when (file-exists-p ,notes-dir-var)
         (delete-directory ,notes-dir-var t))))))

(defun vulpea-meta-test--save-all-buffers ()
  "Save all buffers visiting files in the test notes directory."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-prefix-p vulpea-meta-test--notes-dir buffer-file-name))
        (save-buffer)))))

(defun vulpea-meta-test--file-content (file-name)
  "Get the content of FILE-NAME in test notes directory."
  (let ((file-path (expand-file-name file-name vulpea-meta-test--notes-dir)))
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

;;; vulpea-meta-get Tests

(ert-deftest vulpea-meta-get-default-string ()
  "Test that vulpea-meta-get extracts string value by default."
  (vulpea-meta-test--with-temp-db
   (let ((a (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string))
         (b (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name")))
     (should (equal a b)))))

(ert-deftest vulpea-meta-get-raw-value ()
  "Test that vulpea-meta-get extracts raw value."
  (vulpea-meta-test--with-temp-db
   (should (equal (org-element-interpret-data
                   (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'raw))
                  "some name\n"))))

(ert-deftest vulpea-meta-get-string-value ()
  "Test that vulpea-meta-get extracts string value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string)
                  "some name"))))

(ert-deftest vulpea-meta-get-number-value ()
  "Test that vulpea-meta-get extracts number value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "answer" 'number)
                  42))))

(ert-deftest vulpea-meta-get-link-value ()
  "Test that vulpea-meta-get extracts link value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "link" 'link)
                  "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))

(ert-deftest vulpea-meta-get-url-value ()
  "Test that vulpea-meta-get extracts URL value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "url" 'link)
                  "https://en.wikipedia.org/wiki/Frappato"))))

(ert-deftest vulpea-meta-get-note-value ()
  "Test that vulpea-meta-get extracts note value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "link" 'note)
                  (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))))

(ert-deftest vulpea-meta-get-note-with-alias ()
  "Test that vulpea-meta-get extracts note value respecting alias."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-note-title
                   (vulpea-meta-get
                    (vulpea-db-get-by-id "05907606-f836-45bf-bd36-a8444308eddd")
                    "link with alias" 'note))
                  "Alias of the note without meta"))))

(ert-deftest vulpea-meta-get-note-with-arbitrary-desc ()
  "Test that vulpea-meta-get extracts note value ignoring unknown title."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-note-title
                   (vulpea-meta-get
                    (vulpea-db-get-by-id "05907606-f836-45bf-bd36-a8444308eddd")
                    "link with arbitrary desc" 'note))
                  "Note without META"))))

(ert-deftest vulpea-meta-get-symbol-value ()
  "Test that vulpea-meta-get extracts symbol value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "symbol" 'symbol)
                  'red))))

(ert-deftest vulpea-meta-get-first-of-list ()
  "Test that vulpea-meta-get extracts first element of the list."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "tags" 'string)
                  "tag 1"))))

;;; vulpea-meta-get-list Tests

(ert-deftest vulpea-meta-get-list-default-string ()
  "Test that vulpea-meta-get-list extracts string value by default."
  (vulpea-meta-test--with-temp-db
   (let ((a (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string))
         (b (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name")))
     (should (equal a b)))))

(ert-deftest vulpea-meta-get-list-raw-value ()
  "Test that vulpea-meta-get-list extracts raw value."
  (vulpea-meta-test--with-temp-db
   (should (equal (seq-map
                   #'org-element-interpret-data
                   (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'raw))
                  '("some name\n")))))

(ert-deftest vulpea-meta-get-list-string-value ()
  "Test that vulpea-meta-get-list extracts string value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string)
                  '("some name")))))

(ert-deftest vulpea-meta-get-list-number-value ()
  "Test that vulpea-meta-get-list extracts number value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "answer" 'number)
                  '(42)))))

(ert-deftest vulpea-meta-get-list-link-value ()
  "Test that vulpea-meta-get-list extracts link value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "link" 'link)
                  '("444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))))

(ert-deftest vulpea-meta-get-list-url-value ()
  "Test that vulpea-meta-get-list extracts URL value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "url" 'link)
                  '("https://en.wikipedia.org/wiki/Frappato")))))

(ert-deftest vulpea-meta-get-list-note-value ()
  "Test that vulpea-meta-get-list extracts note value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "link" 'note)
                  (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))))

(ert-deftest vulpea-meta-get-list-symbol-value ()
  "Test that vulpea-meta-get-list extracts symbol value."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "symbol" 'symbol)
                  '(red)))))

(ert-deftest vulpea-meta-get-list-strings ()
  "Test that vulpea-meta-get-list extracts list of strings."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "tags" 'string)
                  '("tag 1" "tag 2" "tag 3")))))

(ert-deftest vulpea-meta-get-list-numbers ()
  "Test that vulpea-meta-get-list extracts list of numbers."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 'number)
                  '(12 18 24)))))

(ert-deftest vulpea-meta-get-list-links ()
  "Test that vulpea-meta-get-list extracts list of links."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "references" 'link)
                  '("444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                    "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))))

(ert-deftest vulpea-meta-get-list-notes-with-aliases ()
  "Test that vulpea-meta-get-list extracts list of notes and maintains order and used aliases."
  (vulpea-meta-test--with-temp-db
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "many links" 'note)
                  (list
                   (vulpea-db-get-by-id "1cc15044-aedb-442e-b727-9e3f7346be95")
                   (let ((note (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))
                     (setf (vulpea-note-primary-title note) (vulpea-note-title note))
                     (setf (vulpea-note-title note) "Alias of the note without meta")
                     note)
                   (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                   (vulpea-db-get-by-id "68f11246-91e1-4d48-b3c6-801a2ef0160b")
                   (vulpea-db-get-by-id "6fccd4ff-d1af-43b0-840e-66f636280acb")
                   (vulpea-db-get-by-id "72522ed2-9991-482e-a365-01155c172aa5")
                   (vulpea-db-get-by-id "7de1afc6-4aef-4ed3-9939-0f2e00971705")
                   (vulpea-db-get-by-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
                   (vulpea-db-get-by-id "ff01962f-47c2-4a32-9bf4-990e41090a9b"))))))

(ert-deftest vulpea-meta-get-list-note-missing-id ()
  "Referencing a missing note in metadata should not raise errors."
  (vulpea-meta-test--with-temp-db
    (let* ((note-id "8f3afe64-2e6c-4d66-b5c8-a7a46a52f1c8")
           (missing-id "deadbeef-0000-4bad-8888-ffffffffffff")
           (file (expand-file-name "missing-link.org" vulpea-meta-test--notes-dir)))
      (with-temp-file file
        (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Broken link\n\n- ghost :: [[id:%s][Ghost note]]\n"
                        note-id missing-id)))
      (vulpea-db-update-file file)
      (should (equal (vulpea-meta-get-list note-id "ghost" 'note)
                     '(nil))))))

;;; vulpea-meta-set Tests

(ert-deftest vulpea-meta-set-string-in-note-without-meta ()
  "Test setting a single string value in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "name" "some name")
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "name" 'string)
                  "some name"))))

(ert-deftest vulpea-meta-set-number-in-note-without-meta ()
  "Test setting a single number value in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "answer" 42)
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "answer" 'number)
                  42))))

(ert-deftest vulpea-meta-set-link-in-note-without-meta ()
  "Test setting a single link value in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references" "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references" 'link)
                  "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))

(ert-deftest vulpea-meta-set-note-in-note-without-meta ()
  "Test setting a single note value in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references"
                    (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references" 'link)
                  "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))

(ert-deftest vulpea-meta-set-symbol-in-note-without-meta ()
  "Test setting a single symbol value in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "symbol" 'red)
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "symbol" 'symbol)
                  'red))))

(ert-deftest vulpea-meta-set-multiple-values ()
  "Test setting multiple values in a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "tags" '("tag 1" "tag 2" "tag 3"))
   (should (equal (vulpea-meta-get-list "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "tags" 'string)
                  '("tag 1" "tag 2" "tag 3")))))

(ert-deftest vulpea-meta-set-replace-multiple-with-single ()
  "Test replacing multiple values with a single value."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 42)
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 'number)
                  '(42)))))

(ert-deftest vulpea-meta-set-replace-multiple-with-new-values ()
  "Test replacing multiple values with new values."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "05907606-f836-45bf-bd36-a8444308eddd" "numbers" '(1 2 3 4 5))
   (should (equal (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 'number)
                  '(1 2 3 4 5)))))

;;; vulpea-meta-remove Tests

(ert-deftest vulpea-meta-remove-from-note-without-meta ()
  "Test that vulpea-meta-remove has no effect on a note without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-remove "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "numbers")
   (should (equal (vulpea-meta-get "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "numbers" 'number)
                  nil))))

(ert-deftest vulpea-meta-remove-missing-property ()
  "Test that vulpea-meta-remove has no effect on a missing property."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-remove "05907606-f836-45bf-bd36-a8444308eddd" "age")
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "age" 'number)
                  nil))))

(ert-deftest vulpea-meta-remove-single-value ()
  "Test removing a single value."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-remove "05907606-f836-45bf-bd36-a8444308eddd" "answer")
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "answer" 'number)
                  nil))))

(ert-deftest vulpea-meta-remove-multiple-values ()
  "Test removing multiple values."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-remove "05907606-f836-45bf-bd36-a8444308eddd" "numbers")
   (should (equal (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 'number)
                  nil))))

;;; vulpea-meta-clean Tests

(ert-deftest vulpea-meta-clean-note-without-meta ()
  "Test that vulpea-meta-clean has no effect on a note without meta."
  (vulpea-meta-test--with-temp-db
   (should (equal (length (org-element-map
                              (plist-get (vulpea-meta "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                                         :pl)
                              'item #'identity))
                  0))
   (vulpea-meta-clean "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
   (should (equal (length (org-element-map
                              (plist-get (vulpea-meta "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                                         :pl)
                              'item #'identity))
                  0))))

(ert-deftest vulpea-meta-clean-note-with-meta ()
  "Test removing all meta from note."
  (vulpea-meta-test--with-temp-db
   (should (equal (length (org-element-map
                              (plist-get (vulpea-meta "05907606-f836-45bf-bd36-a8444308eddd")
                                         :pl)
                              'item #'identity))
                  25))
   (vulpea-meta-clean "05907606-f836-45bf-bd36-a8444308eddd")
   (should (equal (length (org-element-map
                              (plist-get (vulpea-meta "05907606-f836-45bf-bd36-a8444308eddd")
                                         :pl)
                              'item #'identity))
                  0))))

;;; vulpea-meta formatting Tests

(ert-deftest vulpea-meta-format-string-in-file-without-meta-and-body ()
  "Test formatting single string value upon insertion to file without meta and without body."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "name" "some name")
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "reference.org")
                  ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- name :: some name
"))))

(ert-deftest vulpea-meta-format-string-in-file-without-meta ()
  "Test formatting single string value upon insertion to file without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "name" "some name")
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- name :: some name

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-number-in-file-without-meta ()
  "Test formatting single number value upon insertion to file without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "answer" 42)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- answer :: 42

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-id-in-file-without-meta ()
  "Test formatting single id value upon insertion to file without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references" "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-symbol-in-file-without-meta ()
  "Test formatting single symbol value upon insertion to file without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "symbol" 'red)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- symbol :: red

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-in-first-header ()
  "Test formatting single meta inside first header."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "7de1afc6-4aef-4ed3-9939-0f2e00971705" "status" 'done)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "meta-in-the-first-header.org")
                  ":PROPERTIES:
:ID:                     7de1afc6-4aef-4ed3-9939-0f2e00971705
:END:
#+TITLE: meta in the first header

* Metadata

- status :: done
* Description

Some fancy description
"))))

(ert-deftest vulpea-meta-format-multiple-values ()
  "Test formatting multiple values upon insertion to file without meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd"
                      "https://en.wikipedia.org/wiki/Frappato"
                      "trust me™"))
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- references :: [[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]
- references :: trust me™

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-append-nil ()
  "Test appending to the beginning of the list when APPEND is nil."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                    "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd"))
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "age" 42)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- age :: 42
- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-append-with-body ()
  "Test appending to the end of the list when APPEND is non-nil (when body is present)."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                    "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd")
                    'append)
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "age" 42 'append)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42

Just some text to make sure that meta is inserted before.
"))))

(ert-deftest vulpea-meta-format-append-without-body ()
  "Test appending to the end of the list when APPEND is non-nil (when body is missing)."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                    "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd")
                    'append)
   (vulpea-meta-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "age" 42 'append)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "reference.org")
                  ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42
"))))

(ert-deftest vulpea-meta-format-respect-trailing-newlines-no-body ()
  "Test inserting values with respect to trailing new lines in file without body."
  (vulpea-meta-test--with-temp-db
   ;; Add extra newlines to reference.org
   (let ((file-path (expand-file-name "reference.org" vulpea-meta-test--notes-dir)))
     (with-current-buffer (find-file-noselect file-path)
       ;; first line after the header (position 124)
       (goto-char 124)
       (insert "\n\n\n\n\n")
       (save-buffer)))
   (vulpea-meta-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                    "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd")
                    'append)
   (vulpea-meta-set "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" "age" 42 'append)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "reference.org")
                  ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42





"))))

(ert-deftest vulpea-meta-format-respect-trailing-newlines-with-body ()
  "Test inserting values with respect to trailing new lines in file with body."
  (vulpea-meta-test--with-temp-db
   ;; Add extra newlines to without-meta.org
   (let ((file-path (expand-file-name "without-meta.org" vulpea-meta-test--notes-dir)))
     (with-current-buffer (find-file-noselect file-path)
       ;; first line after the body (position 221)
       (goto-char 221)
       (insert "\n\n\n")
       ;; first line after the header (position 162)
       (goto-char 162)
       (insert "\n\n\n\n\n")
       (save-buffer)))
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                    "references"
                    '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                      "05907606-f836-45bf-bd36-a8444308eddd")
                    'append)
   (vulpea-meta-set "444f94d7-61e0-4b7c-bb7e-100814c6b4bb" "age" 42 'append)
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "without-meta.org")
                  ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:ALIASES:           \"Alias of the note without meta\"
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42






Just some text to make sure that meta is inserted before.



"))))

(ert-deftest vulpea-meta-clean-from-note-with-body ()
  "Test cleaning meta from a note with body."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-clean "05907606-f836-45bf-bd36-a8444308eddd")
   (vulpea-meta-test--save-all-buffers)
   (should (equal (vulpea-meta-test--file-content "with-meta.org")
                  ":PROPERTIES:
:ID:                     05907606-f836-45bf-bd36-a8444308eddd
:END:
#+title: Note with META

Don't mind me. I am a content of this note.

* Details
:PROPERTIES:
:ID:                     f210cc49-0e71-4bb6-843f-89dd2d809e02
:END:

Probably you've heard about [[https://github.com][GitHub]]. And of course, you may have links with
strange symbols like % and ', just like this [[https://en.wikipedia.org/wiki/I,_Olga_Hepnarov%C3%A1][link]] or this [[https://www.darenberg.com.au/assets/files/d'arenberg-the-stump-jump-lightly-wooded-chardonnay-2017.pdf][link]].
"))))

;;; vulpea-meta-set-batch Tests

(ert-deftest vulpea-meta-set-batch-in-note-without-meta ()
  "Test batch setting multiple properties in a note without existing meta."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set-batch "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                          '(("name" . "some name")
                            ("age" . 42)
                            ("status" . active)))
   (vulpea-meta-test--save-all-buffers)
   (should (string-match-p "- name :: some name" (vulpea-meta-test--file-content "without-meta.org")))
   (should (string-match-p "- age :: 42" (vulpea-meta-test--file-content "without-meta.org")))
   (should (string-match-p "- status :: active" (vulpea-meta-test--file-content "without-meta.org")))))

(ert-deftest vulpea-meta-set-batch-replace-existing ()
  "Test batch setting replaces existing properties."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set-batch "05907606-f836-45bf-bd36-a8444308eddd"
                          '(("name" . "new name")
                            ("numbers" . 999)))
   (vulpea-meta-test--save-all-buffers)
   (let ((content (vulpea-meta-test--file-content "with-meta.org")))
     ;; Should have new values
     (should (string-match-p "- name :: new name" content))
     (should (string-match-p "- numbers :: 999" content))
     ;; Should not have old name (Big brother)
     (should-not (string-match-p "Big brother" content)))))

(ert-deftest vulpea-meta-set-batch-with-list-values ()
  "Test batch setting with list values."
  (vulpea-meta-test--with-temp-db
   (vulpea-meta-set-batch "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                          '(("tags" . ("a" "b" "c"))
                            ("count" . 3)))
   (vulpea-meta-test--save-all-buffers)
   (let ((content (vulpea-meta-test--file-content "without-meta.org")))
     (should (string-match-p "- tags :: a" content))
     (should (string-match-p "- tags :: b" content))
     (should (string-match-p "- tags :: c" content))
     (should (string-match-p "- count :: 3" content)))))

(ert-deftest vulpea-meta-set-batch-preserves-other-props ()
  "Test batch setting preserves properties not being set."
  (vulpea-meta-test--with-temp-db
   ;; with-meta.org has: name, numbers, url, link, symbol, etc.
   (vulpea-meta-set-batch "05907606-f836-45bf-bd36-a8444308eddd"
                          '(("name" . "new name")))
   (vulpea-meta-test--save-all-buffers)
   (let ((content (vulpea-meta-test--file-content "with-meta.org")))
     ;; New value
     (should (string-match-p "- name :: new name" content))
     ;; Preserved values
     (should (string-match-p "- url ::" content))
     (should (string-match-p "- symbol ::" content)))))

(ert-deftest vulpea-meta-set-batch-with-note-values ()
  "Test batch setting with vulpea-note values."
  (vulpea-meta-test--with-temp-db
   (let ((ref-note (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
     (vulpea-meta-set-batch "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                            `(("reference" . ,ref-note)
                              ("count" . 1)))
     (vulpea-meta-test--save-all-buffers)
     (let ((content (vulpea-meta-test--file-content "without-meta.org")))
       (should (string-match-p "\\[\\[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7\\]" content))
       (should (string-match-p "- count :: 1" content))))))

(ert-deftest vulpea-meta-set-batch-empty-alist ()
  "Test batch setting with empty alist does nothing."
  (vulpea-meta-test--with-temp-db
   (let ((content-before (vulpea-meta-test--file-content "with-meta.org")))
     (vulpea-meta-set-batch "05907606-f836-45bf-bd36-a8444308eddd" nil)
     (vulpea-meta-test--save-all-buffers)
     (should (equal content-before (vulpea-meta-test--file-content "with-meta.org"))))))

(provide 'vulpea-meta-test)
;;; vulpea-meta-test.el ends here
