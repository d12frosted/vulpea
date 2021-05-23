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
             :tags '("tag1" "tag2")
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

(describe "vulpea-find"
  (before-all
    (vulpea-test--init))

  (before-each
    (setq vulpea-find-default-filter nil)
    (spy-on 'local-filter-fn :and-call-through)
    (spy-on 'global-filter-fn :and-call-through))

  (after-all
    (vulpea-test--teardown)
    (setq vulpea-find-default-filter nil))

  (it "finds existing note without any filters"
    (spy-on 'org-roam-node-visit)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-find)

    (expect 'org-roam-node-visit :to-have-been-called))

  (it "initiates capture process"
    (spy-on 'org-roam-capture-)
    (spy-on 'completing-read
            :and-return-value "I simply can't exist")

    (vulpea-find)

    (expect 'org-roam-capture- :to-have-been-called))

  (it "uses default filter"
    (setq vulpea-find-default-filter 'global-filter-fn)
    (spy-on 'org-roam-node-visit)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-find)

    (expect 'local-filter-fn
            :not :to-have-been-called)
    (expect 'global-filter-fn
            :to-have-been-called-times
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases])))))

  (it "uses filter override instead of default one"
    (setq vulpea-find-default-filter 'global-filter-fn)
    (spy-on 'org-roam-node-visit)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-find nil 'local-filter-fn)

    (expect 'local-filter-fn
            :to-have-been-called-times
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases]))))
    (expect 'global-filter-fn
            :not :to-have-been-called)))

(describe "vulpea-insert"
  (before-each
    (vulpea-test--init)
    (setq vulpea-insert-default-filter nil)
    (spy-on 'local-filter-fn :and-call-through)
    (spy-on 'global-filter-fn :and-call-through)
    (spy-on 'insert-handle-fn :and-call-through)
    (add-hook 'vulpea-insert-handle-functions 'insert-handle-fn))

  (after-each
    (vulpea-test--teardown)
    (setq vulpea-insert-default-filter nil))

  (it "inserts existing note"
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-insert)

    (expect 'insert-handle-fn
            :to-have-been-called-with
            (vulpea-db-get-by-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")))

  (it "inserts captured note"
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :if-new
             (file+head
              "%<%Y%m%d%H%M%S>-${slug}.org"
              "#+title: ${title}\n")
             :unnarrowed t
             :immediate-finish t)))
    (spy-on 'completing-read
            :and-return-value "I can't possibly exist")

    (vulpea-insert)

    (expect 'insert-handle-fn
            :to-have-been-called-with
            (car-safe
             (vulpea-db-query
              (lambda (note)
                (string-equal
                 (vulpea-note-title note)
                 "I can't possibly exist"))))))

  (it "uses default filter"
    (setq vulpea-insert-default-filter 'global-filter-fn)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-insert)

    (expect 'local-filter-fn
            :not :to-have-been-called)
    (expect 'global-filter-fn
            :to-have-been-called-times
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases])))))

  (it "uses filter override instead of default one"
    (setq vulpea-insert-default-filter 'global-filter-fn)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-insert 'local-filter-fn)

    (expect 'local-filter-fn
            :to-have-been-called-times
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases]))))
    (expect 'global-filter-fn
            :not :to-have-been-called)))

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

  (it "creates new file with additional head, tags, body, context and properties"
    (setq note
          (vulpea-create
           "Aglianico"
           "prefix-${slug}.org"
           :head "#+roam_key: ${url}"
           :tags '("tag1" "tag2")
           :body "Well, I am a grape!"
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

Well, I am a grape!
"
             (vulpea-note-id note)))))

(provide 'vulpea-test)
;;; vulpea-test.el ends here
