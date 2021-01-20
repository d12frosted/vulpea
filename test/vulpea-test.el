;;; vulpea-test.el --- Test `vulpea' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
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
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(tag1,tag2) Reference")
    (expect (vulpea-select "Note")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "reference.org" org-roam-directory)
             :title "Reference"
             :tags '("tag1" "tag2")
             :level 0
             :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))

  (it "returns only title for non-existent note"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "Future")
    (expect (vulpea-select "Note")
            :to-equal
            (make-vulpea-note
             :title "Future"
             :level 0)))

  (it "calls FILTER-FN on each item"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "(tag1,tag2) Reference")

    (vulpea-select "Note" nil nil
                   (lambda (_)
                     (setq filter-count (+ 1 filter-count))))
    (expect filter-count :to-equal
            (caar (org-roam-db-query
                   [:select (funcall count *)
                            :from titles])))))

(describe "vulpea-select"
  :var (id)
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "creates new file with ID"
    (setq id
          (vulpea-create
           "Slarina"
           `("d" "default" plain
             #'org-roam-capture--get-point
             "%?"
             :file-name "prefix-${slug}"
             :head ,(concat
                     ":PROPERTIES:\n"
                     ":ID:                     ${id}\n"
                     ":END:\n"
                     "#+TITLE: ${title}\n\n")
             :unnarrowed t
             :immediate-finish t)))
    (expect vulpea--capture-file-path :to-be nil)
    (expect (vulpea-db-get-by-id id)
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "prefix-slarina.org" org-roam-directory)
             :title "Slarina"
             :tags nil
             :level 0
             :id id))))

(provide 'vulpea-test)
;;; vulpea-test.el ends here
