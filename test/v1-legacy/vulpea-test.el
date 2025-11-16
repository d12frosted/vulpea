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

(describe "vulpea-find"
  (before-all
    (vulpea-test--init))

  (before-each
    (setq vulpea-find-default-filter nil
          vulpea-find-default-candidates-source #'vulpea-db-query)
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

    (vulpea-find :filter-fn 'local-filter-fn)

    (expect 'local-filter-fn
            :to-have-been-called-times
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases]))))
    (expect 'global-filter-fn
            :not :to-have-been-called))

  (it "uses default candidates source"
    (setq vulpea-find-default-candidates-source 'global-find-candidates-fn)
    (spy-on 'global-find-candidates-fn
            :and-return-value
            (list (vulpea-db-get-by-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
                  (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
    (spy-on 'org-roam-node-visit)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-find)

    (expect 'global-find-candidates-fn :to-have-been-called-times 1)
    (expect 'local-find-candidates-fn :not :to-have-been-called))

  (it "uses candidates source override instead of default one"
    (setq vulpea-find-default-candidates-source 'global-find-candidates-fn)
    (spy-on 'local-find-candidates-fn
            :and-return-value
            (list (vulpea-db-get-by-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
                  (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))
    (spy-on 'org-roam-node-visit)
    (spy-on 'completing-read
            :and-return-value "Big note")

    (vulpea-find :candidates-fn 'local-find-candidates-fn)

    (expect 'global-find-candidates-fn :not :to-have-been-called)
    (expect 'local-find-candidates-fn :to-have-been-called-times 1)))

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
            :not :to-have-been-called))

  (it "uses default candidates source"
    (setq vulpea-insert-default-candidates-source 'global-find-candidates-fn)
    (spy-on 'global-find-candidates-fn
            :and-return-value
            (list (vulpea-db-get-by-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
                  (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))

    (spy-on 'completing-read :and-return-value "Big note")

    (vulpea-insert)

    (expect 'global-find-candidates-fn :to-have-been-called-times 1)))

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
             :id (vulpea-note-id note)
             :properties (list
                          (cons "CATEGORY" "prefix-slarina")
                          (cons "ID" (vulpea-note-id note))
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "prefix-slarina.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :attach-dir (let ((default-directory org-roam-directory))
                           (org-attach-dir-from-id (vulpea-note-id note) 'try-all))))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note))

  (it "creates new file with passed id"
    (setq note
          (vulpea-create
           "Frappato"
           "prefix-${slug}.org"
           :id "00000000-0000-0000-0000-000000000000"
           :unnarrowed t
           :immediate-finish t))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-frappato.org" org-roam-directory)
             :title "Frappato"
             :tags nil
             :level 0
             :id "00000000-0000-0000-0000-000000000000"
             :properties (list
                          (cons "CATEGORY" "prefix-frappato")
                          (cons "ID" "00000000-0000-0000-0000-000000000000")
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "prefix-frappato.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :attach-dir (expand-file-name "data/00/000000-0000-0000-0000-000000000000" org-roam-directory)))
    (expect (vulpea-db-get-by-id "00000000-0000-0000-0000-000000000000")
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
             :id (vulpea-note-id note)
             :properties (list
                          (cons "CATEGORY" "prefix-nerello_mascalese")
                          (cons "ID" (vulpea-note-id note))
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "prefix-nerello_mascalese.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :attach-dir (let ((default-directory org-roam-directory))
                           (org-attach-dir-from-id (vulpea-note-id note) 'try-all))))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note))

  ;; this is needed to make sure formatting is working properly
  (it "creates new file with additional meta but no extra head"
    (setq note
          (vulpea-create
           "Barbera"
           "prefix-${slug}.org"
           :meta `(("category" . "sample")
                   ("age" . 42)
                   ("links" . (,(vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                               ,(vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))
           :unnarrowed t
           :immediate-finish t))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-barbera.org" org-roam-directory)
             :title "Barbera"
             :level 0
             :id (vulpea-note-id note)
             :properties (list
                          (cons "CATEGORY" "prefix-barbera")
                          (cons "ID" (vulpea-note-id note))
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "prefix-barbera.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :links '(("id" . "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                      ("id" . "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))
             :meta '(("category" . ("sample"))
                     ("age" . ("42"))
                     ("links" . ("[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]"
                                 "[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]")))
             :attach-dir (let ((default-directory org-roam-directory))
                           (org-attach-dir-from-id (vulpea-note-id note) 'try-all))))
    (expect (vulpea-db-get-by-id (vulpea-note-id note))
            :to-equal
            note)
    (expect (vulpea-note-path note)
            :to-contain-exactly
            (format
             ":PROPERTIES:
:ID:       %s
:END:
#+title: Barbera

- category :: sample
- age :: 42
- links :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- links :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]


"
             (vulpea-note-id note))))

  (it "creates new file with additional head, tags, meta, body, context and properties"
    (setq note
          (vulpea-create
           "Aglianico"
           "prefix-${slug}.org"
           :head "#+author: ${name}"
           :tags '("tag1" "tag2")
           :meta `(("category" . "sample")
                   ("age" . 42)
                   ("links" . (,(vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                               ,(vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))
           :body "Well, I am a grape!"
           :unnarrowed t
           :immediate-finish t
           :context
           (list :name "frodo")
           :properties
           (list (cons "MY_TAG" "super-tag"))))
    (expect note
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-aglianico.org" org-roam-directory)
             :title "Aglianico"
             :tags '("tag1" "tag2")
             :level 0
             :id (vulpea-note-id note)
             :properties (list
                          (cons "CATEGORY" "prefix-aglianico")
                          (cons "MY_TAG" "super-tag")
                          (cons "ID" (vulpea-note-id note))
                          (cons "BLOCKED" "")
                          (cons "ALLTAGS" ":tag1:tag2:")
                          (cons "FILE" (expand-file-name "prefix-aglianico.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :links '(("id" . "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                      ("id" . "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))
             :meta '(("category" . ("sample"))
                     ("age" . ("42"))
                     ("links" . ("[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]"
                                 "[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]")))
             :attach-dir (let ((default-directory org-roam-directory))
                           (org-attach-dir-from-id (vulpea-note-id note) 'try-all))))
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
#+filetags: :tag1:tag2:
#+author: frodo

- category :: sample
- age :: 42
- links :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- links :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]

Well, I am a grape!
"
             (vulpea-note-id note)))))

(provide 'vulpea-test)
;;; vulpea-test.el ends here
