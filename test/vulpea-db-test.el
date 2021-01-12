;;; vulpea-db-test.el --- Test `vulpea-db' module -*- lexical-binding: t; -*-
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
;; Helpers for testing `vulpea-module'.
;;
;;; Code:

(require 'vulpea-test-utils)
(require 'vulpea-utils)
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
             (make-vulpea-note
              :path (expand-file-name "note-with-alias.org" org-roam-directory)
              :title "Alias of the note with alias"
              :tags nil
              :level 0
              :id "72522ed2-9991-482e-a365-01155c172aa5"))))

  (it "finds multiple notes sharing the same title"
    (expect (vulpea-db-search-by-title "Duplicating Term")
            :to-have-same-items-as
            (list
             (make-vulpea-note
              :path (expand-file-name "same-name-1.org" org-roam-directory)
              :title "Duplicating Term"
              :tags nil
              :level 0
              :id "ff01962f-47c2-4a32-9bf4-990e41090a9b")
             (make-vulpea-note
              :path (expand-file-name "same-name-2.org" org-roam-directory)
              :title "Duplicating Term"
              :tags nil
              :level 0
              :id "68f11246-91e1-4d48-b3c6-801a2ef0160b"))))

  (it "returns all information about the note"
    (expect (vulpea-db-search-by-title "Reference")
            :to-equal
            (list
             (make-vulpea-note
              :path (expand-file-name "reference.org" org-roam-directory)
              :title "Reference"
              :tags '("tag1" "tag2")
              :level 0
              :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))))

  (it "should use case sensitive search"
    (expect (vulpea-db-search-by-title "reference")
            :to-be nil)))

(describe "vulpea-db-get-by-id"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns nil when passed unknown id"
    (expect (vulpea-db-get-by-id "00000000-0000-0000-0000-000000000000")
            :to-be nil))

  (it "returns note by file id"
    (expect (vulpea-db-get-by-id "72522ed2-9991-482e-a365-01155c172aa5")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "note-with-alias.org" org-roam-directory)
             :title "Note with an alias"
             :tags nil
             :level 0
             :id "72522ed2-9991-482e-a365-01155c172aa5")))

  (it "returns note by sub-heading id"
    (expect (vulpea-db-get-by-id "b77a4837-71d6-495e-98f1-b576464aacc1")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "big-note.org" org-roam-directory)
             :title "Big note sub-heading"
             :tags nil
             :level 1
             :id "b77a4837-71d6-495e-98f1-b576464aacc1")))

  (it "returns note by sub0sub-heading id"
    (expect (vulpea-db-get-by-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "big-note.org" org-roam-directory)
             :title "Big note sub-sub-heading"
             :tags nil
             :level 2
             :id "cfc39858-351d-4f1e-8f98-10d16d71f49e"))))

(describe "vulpea-db-get-file-by-id"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns nil when passed unknown id"
    (expect (vulpea-db-get-file-by-id "00000000-0000-0000-0000-000000000000")
            :to-be nil))

  (it "returns file of a note with id"
    (expect (vulpea-db-get-file-by-id "72522ed2-9991-482e-a365-01155c172aa5")
            :to-equal
            (expand-file-name "note-with-alias.org"
                              org-roam-directory)))

  (it "returns file of sub-heading with id"
    (expect (vulpea-db-get-file-by-id "b77a4837-71d6-495e-98f1-b576464aacc1")
            :to-equal
            (expand-file-name "big-note.org"
                              org-roam-directory)))

  (it "returns file of sub-sub-heading with id"
    (expect (vulpea-db-get-file-by-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
            :to-equal
            (expand-file-name "big-note.org"
                              org-roam-directory))))

(describe "vulpea-db-get-id-by-file"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns nil when passed unknown file"
    (expect (vulpea-db-get-id-by-file
             (expand-file-name "0000.org"
                               org-roam-directory))
            :to-be nil))

  (it "returns id of an absolute file path"
    (expect (vulpea-db-get-id-by-file
             (expand-file-name "reference.org"
                               org-roam-directory))
            :to-equal
            "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))

  (it "returns id of a relative file path"
    (expect (vulpea-db-get-id-by-file "reference.org")
            :to-equal
            "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))

  (it "returns top-level id of a file with subheading"
    (expect (vulpea-db-get-id-by-file
             (expand-file-name "big-note.org"
                               org-roam-directory))
            :to-equal
            "eeec8f05-927f-4c61-b39e-2fb8228cf484")))

(provide 'vulpea-db-test)
;;; vulpea-db-test.el ends here
