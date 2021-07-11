;;; vulpea-select-test.el --- Test `vulpea' module -*- lexical-binding: t; -*-
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
;; Test `vulpea-select' module.
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

(provide 'vulpea-select-test)
;;; vulpea-select-test.el ends here
