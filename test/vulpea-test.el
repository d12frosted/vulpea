;;; vulpea-test.el --- Test `vulpea' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Dec 2020
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `vulpea' module.
;;
;;; Code:

(require 'buttercup)
(require 'vulpea-test-utils)
(require 'vulpea)
(require 'vulpea-db)

(describe "vulpea-select"
  :var ((filter-count 0))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns all information about existing note"
    (spy-on
     'completing-read
     :and-return-value
     (completion-for :title "Reference"
                     :tags '("tag1" "tag2")))
    (expect (vulpea-select "Note")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "reference.org" org-roam-directory)
             :title "Reference"
             :tags '("tag2" "tag1")
             :level 0
             :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))

  (it "returns only title for non-existent note"
    (spy-on 'completing-read
            :and-return-value "Future")
    (expect (vulpea-select "Note")
            :to-equal
            (make-vulpea-note
             :title "Future"
             :level 0)))

  (it "calls FILTER-FN on each item"
    (setq filter-count 0)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-select "Note"
                   :filter-fn
                   (lambda (_)
                     (setq filter-count (+ 1 filter-count))))
    (expect filter-count :to-equal
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases])))))

  (it "calls FILTER-FN on note structure"
    (setq filter-count 0)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-select "Note"
                   :filter-fn
                   (lambda (note)
                     (when (seq-contains-p (vulpea-note-tags note) "tag1")
                       (setq filter-count (+ 1 filter-count)))))
    (expect filter-count :to-equal 2)))

(describe "vulpea-create"
  :var (note)
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "creates new file and syncs database"
    (setq note
          (vulpea-create
           "Slarina"
           "prefix-${slug}.org"
           :unnarrowed t
           :immediate-finish t))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-slarina.org" org-roam-directory)
             :title "Slarina"
             :tags nil
             :level 0
             :id (vulpea-note-id note)))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note))

  (it "creates new file with passed id"
    (setq note
          (vulpea-create
           "Frappato"
           "prefix-${slug}.org"
           :id "xyz"
           :unnarrowed t
           :immediate-finish t))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-frappato.org" org-roam-directory)
             :title "Frappato"
             :tags nil
             :level 0
             :id "xyz"))
    (expect (vulpea-db-get-by-id "xyz")
            :to-equal
            note))

  (it "creates new file without taking title or slug from passed context"
    (setq note
          (vulpea-create
           "Nerello Mascalese"
           "prefix-${slug}.org"
           :unnarrowed t
           :immediate-finish t
           :context
           (list :title "hehe"
                 :slug "xoxo")))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-nerello_mascalese.org" org-roam-directory)
             :title "Nerello Mascalese"
             :tags nil
             :level 0
             :id (vulpea-note-id note)))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note))

  (it "creates new file with additional head, tags, context and properties"
    (setq note
          (vulpea-create
           "Aglianico"
           "prefix-${slug}.org"
           :head "#+roam_key: ${url}"
           :tags '("tag1" "tag2")
           :unnarrowed t
           :immediate-finish t
           :context
           (list :url "https://d12frosted.io")
           :properties
           (list (cons "MY_TAG" "super-tag"))))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-aglianico.org" org-roam-directory)
             :title "Aglianico"
             :tags '("tag1" "tag2")
             :level 0
             :id (vulpea-note-id note)))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note)
    (expect (vulpea-note-path note)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:MY_TAG:   super-tag
:END:
#+title: Aglianico
#+filetags: tag1 tag2
#+roam_key: https://d12frosted.io

"
             (vulpea-note-id note)))))

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
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-set "super_tag_1" "super_tag_2")
              (vulpea-buffer-tags-get))
            :to-equal '("super_tag_1" "super_tag_2")))

  (it "clear tags"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-set)
              (vulpea-buffer-tags-get))
            :to-equal nil))

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
            :to-equal '("tag1" "tag2" "super_tag_1")))

  (it "delete one of the tags"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-remove "tag1")
              (vulpea-buffer-tags-get))
            :to-equal '("tag2")))

  (it "delete last tag"
    (setq id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (vulpea-buffer-tags-remove "tag1")
              (vulpea-buffer-tags-remove "tag2")
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

  (it "support list values"
    (setq id "2c3bd05d-b3d1-40bc-bd42-f019d441592c")
    (expect
     (vulpea-utils-with-note (vulpea-db-get-by-id id)
       (vulpea-buffer-prop-set "values" (string-join '("value 1" "value 2" "single") ", "))
       (vulpea-buffer-prop-get-list "values" ", "))
     :to-equal '("value 1" "value 2" "single"))
    (expect (vulpea-note-path (vulpea-db-get-by-id id))
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+values: value 1, value 2, single

Some body.
"
             id))))

(provide 'vulpea-test)
;;; vulpea-test.el ends here
