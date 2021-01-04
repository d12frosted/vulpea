;;; vulpea-db-test.el --- Test `vulpea-db' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1") (buttercup "1.23") (org "9.4.4"))
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
;; Helpers for testing `vulpea-module'.
;;
;;; Code:

(require 'vulpea-test)
(require 'vulpea-db)

(describe "vulpea-db-search-by-title"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns empty list when searching for non existent title"
    (expect (vulpea-db-search-by-title "very unique name not existing in present")
            :to-be nil))

  (it "finds a note by alias"
    (expect (vulpea-db-search-by-title "Alias of the note with alias")
            :to-equal
            (list
             (list :path (expand-file-name "note-with-alias.org" org-roam-directory)
                   :title "Alias of the note with alias"
                   :tags nil
                   :id "72522ed2-9991-482e-a365-01155c172aa5"))))

  (it "finds multiple notes sharing the same title"
    (expect (vulpea-db-search-by-title "Duplicating Term")
            :to-have-same-items-as
            (list
             (list :path (expand-file-name "same-name-1.org" org-roam-directory)
                   :title "Duplicating Term"
                   :tags nil
                   :id "ff01962f-47c2-4a32-9bf4-990e41090a9b")
             (list :path (expand-file-name "same-name-2.org" org-roam-directory)
                   :title "Duplicating Term"
                   :tags nil
                   :id "68f11246-91e1-4d48-b3c6-801a2ef0160b"))))

  (it "returns all information about the note"
    (expect (vulpea-db-search-by-title "Reference")
            :to-equal
            (list
             (list :path (expand-file-name "reference.org" org-roam-directory)
                   :title "Reference"
                   :tags '("tag1" "tag2")
                   :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))

  (it "should use case sensitive search"
    (expect (vulpea-db-search-by-title "reference")
            :to-be nil)))

(describe "vulpea-db-get-title-by-id"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns nil when passed unknown id"
    (expect (vulpea-db-get-title-by-id "00000000-0000-0000-0000-000000000000")
            :to-be nil))

  (it "returns title of a note by id"
    (expect (vulpea-db-get-title-by-id "72522ed2-9991-482e-a365-01155c172aa5")
            :to-equal "Note with an alias"))

  (it "returns sub-heading of a note by id"
    (expect (vulpea-db-get-title-by-id "b77a4837-71d6-495e-98f1-b576464aacc1")
            :to-equal "Big note sub-heading"))

  (it "returns sub-sub-heading of a note by id"
    (expect (vulpea-db-get-title-by-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
            :to-equal "Big note sub-sub-heading")))

(provide 'vulpea-db-test)
;;; vulpea-db-test.el ends here
