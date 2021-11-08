;;; vulpea-buffer-test.el --- Test `vulpea-buffer' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
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
;; Test `vulpea-buffer' module.
;;
;;; Code:

(require 'buttercup)
(require 'vulpea-test-utils)
(require 'vulpea-buffer)
(require 'vulpea-db)

(describe "vulpea-buffer-title-set"
  :var (id)
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "set a title in note without title"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (vulpea-buffer-title-set "Some title")
      (save-buffer))
    (expect (vulpea-note-title (vulpea-db-get-by-id id))
            :to-equal "Some title"))

  (it "set a title in note with title when used on min point"
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (goto-char (point-min))
      (vulpea-buffer-title-set "Changed title")
      (save-buffer))
    (expect (vulpea-note-title (vulpea-db-get-by-id id))
            :to-equal "Changed title"))

  (it "set a title in note with title when used on max point"
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (vulpea-utils-with-note (vulpea-db-get-by-id id)
      (goto-char (point-max))
      (vulpea-buffer-title-set "Changed title")
      (save-buffer))
    (expect (vulpea-note-title (vulpea-db-get-by-id id))
            :to-equal "Changed title"))

  (it "set a title in note with title when used somewhere in file"
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    ;; do something on some heading
    (vulpea-utils-with-note (vulpea-db-get-by-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
      (goto-char (point-max))
      (vulpea-buffer-title-set "Changed title")
      (save-buffer))
    (expect (vulpea-note-title (vulpea-db-get-by-id id))
            :to-equal "Changed title")))

(describe "vulpea-buffer-tags-*"
  :var (id)
  (before-each
    (vulpea-test--init))

  (after-each
    (vulpea-test--teardown))

  (it "return empty tags list in a note without tags"
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-get))
            :to-equal nil))

  (it "set several tags at once"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-set "super_tag_1" "super_tag_2")
              (vulpea-buffer-tags-get))
            :to-equal '("super_tag_1" "super_tag_2"))
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Reference
#+filetags: :super_tag_1:super_tag_2:
"
             id)))

  (it "clear tags"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:
"
             id))
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-set)
              (vulpea-buffer-tags-get))
            :to-equal nil)
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Reference
"
             id)))

  (it "add a first tag"
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-add "super_tag_1")
              (vulpea-buffer-tags-get))
            :to-equal '("super_tag_1")))

  (it "add one more tag"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-add "super_tag_1")
              (vulpea-buffer-tags-get))
            :to-equal '("tag1" "tag2" "tag3" "super_tag_1")))

  (it "delete one of the tags"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-remove "tag1")
              (vulpea-buffer-tags-get))
            :to-equal '("tag2" "tag3")))

  (it "delete last tag"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-remove "tag1")
              (vulpea-buffer-tags-remove "tag2")
              (vulpea-buffer-tags-remove "tag3")
              (vulpea-buffer-tags-get))
            :to-equal nil)))

(describe "vulpea-buffer-prop-*"
  :var (id)
  (before-each
    (vulpea-test--init))

  (after-each
    (vulpea-test--teardown))

  (it "downcase property name"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set "TITLE" "Title 1")
       (vulpea-buffer-prop-get "title"))
     :to-equal "Title 1")
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Title 1

Some body.
"
             id)))

  (it "ignore case when getting"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set "TITLE" "Title 1")
       (vulpea-buffer-prop-get "TiTle"))
     :to-equal "Title 1"))

  (it "replace existing property"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set "TITLE" "Title 1")
       (vulpea-buffer-prop-set "TiTlE" "Title 2")
       (vulpea-buffer-prop-get "title"))
     :to-equal "Title 2")
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Title 2

Some body.
"
             id)))

  (it "remove existing property"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-get "title"))
     :to-equal "Reference")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-remove "title")
       (vulpea-buffer-prop-get "title"))
     :to-equal nil)
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+filetags: :tag1:tag2:tag3:
"
             id)))

  (it "support list values"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set-list "values" '("value 1" "value 2" "single"))
       (vulpea-buffer-prop-get-list "values"))
     :to-equal '("value 1" "value 2" "single"))
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+values: \"value 1\" \"value 2\" single

Some body.
"
             id)))

  (it "ignore trailing spaces"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set "value" "             ")
       (vulpea-buffer-prop-get "value"))
     :to-equal nil)))

(describe "vulpea-buffer-meta-format"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "formats an URL"
    (expect (vulpea-buffer-meta-format "https://www.wikipedia.org/")
            :to-equal "[[https://www.wikipedia.org/][wikipedia.org]]"))

  (it "formats a note ID"
    (expect (vulpea-buffer-meta-format "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
            :to-equal "[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]"))

  (it "formats regular string"
    (expect (vulpea-buffer-meta-format "hello")
            :to-equal "hello"))

  (it "throw user-error for unknown note"
    (expect (vulpea-buffer-meta-format "d36125b3-39e1-4bc3-8f7d-126159d8d60e")
            :to-throw 'user-error '("Note with id \"d36125b3-39e1-4bc3-8f7d-126159d8d60e\" does not exist")))

  (it "throw user-error for unsupported resource type"
    (expect (vulpea-buffer-meta-format '(1 2 3))
            :to-throw 'user-error '("Unsupported type of \"(1 2 3)\""))))

(provide 'vulpea-buffer-test)
;;; vulpea-buffer-test.el ends here
