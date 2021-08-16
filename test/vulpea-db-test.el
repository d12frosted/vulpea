;;; vulpea-db-test.el --- Test `vulpea-db' module -*- lexical-binding: t; -*-
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
;; Test `vulpea-db' module.
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

  (it "finds a note by original name"
    (expect (vulpea-db-search-by-title "Note with an alias")
            :to-equal
            (list
             (make-vulpea-note
              :path (expand-file-name "note-with-alias.org" org-roam-directory)
              :title "Note with an alias"
              :tags nil
              :aliases '("Alias of the note with alias")
              :level 0
              :id "72522ed2-9991-482e-a365-01155c172aa5"
              :properties (list
                           (cons "CATEGORY" "note-with-alias")
                           (cons "ROAM_ALIASES" "\"Alias of the note with alias\"")
                           (cons "ID" "72522ed2-9991-482e-a365-01155c172aa5")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "note-with-alias.org" org-roam-directory))
                           (cons "PRIORITY" "B"))))))

  (it "finds a note by alias"
    (expect (vulpea-db-search-by-title "Alias of the note with alias")
            :to-equal
            (list
             (make-vulpea-note
              :path (expand-file-name "note-with-alias.org" org-roam-directory)
              :title "Alias of the note with alias"
              :primary-title "Note with an alias"
              :tags nil
              :aliases '("Alias of the note with alias")
              :level 0
              :id "72522ed2-9991-482e-a365-01155c172aa5"
              :properties (list
                           (cons "CATEGORY" "note-with-alias")
                           (cons "ROAM_ALIASES" "\"Alias of the note with alias\"")
                           (cons "ID" "72522ed2-9991-482e-a365-01155c172aa5")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "note-with-alias.org" org-roam-directory))
                           (cons "PRIORITY" "B"))))))

  (it "finds multiple notes sharing the same title"
    (expect (vulpea-db-search-by-title "Duplicating Term")
            :to-have-same-items-as
            (list
             (make-vulpea-note
              :path (expand-file-name "same-name-1.org" org-roam-directory)
              :title "Duplicating Term"
              :tags '("tag1" "tag2")
              :level 0
              :id "ff01962f-47c2-4a32-9bf4-990e41090a9b"
              :properties (list
                           (cons "CATEGORY" "same-name-1")
                           (cons "ID" "ff01962f-47c2-4a32-9bf4-990e41090a9b")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "same-name-1.org" org-roam-directory))
                           (cons "PRIORITY" "B")))
             (make-vulpea-note
              :path (expand-file-name "same-name-2.org" org-roam-directory)
              :title "Duplicating Term"
              :tags '("tag3")
              :level 0
              :id "68f11246-91e1-4d48-b3c6-801a2ef0160b"
              :properties (list
                           (cons "CATEGORY" "same-name-2")
                           (cons "ID" "68f11246-91e1-4d48-b3c6-801a2ef0160b")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "same-name-2.org" org-roam-directory))
                           (cons "PRIORITY" "B"))))))

  (it "returns all information about the note"
    (expect (vulpea-db-search-by-title "Reference")
            :to-equal
            (list
             (make-vulpea-note
              :path (expand-file-name "reference.org" org-roam-directory)
              :title "Reference"
              :tags '("tag1" "tag2")
              :level 0
              :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
              :properties (list
                           (cons "CATEGORY" "reference")
                           (cons "ID" "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "reference.org" org-roam-directory))
                           (cons "PRIORITY" "B"))))))

  (it "should use case sensitive search"
    (expect (vulpea-db-search-by-title "reference")
            :to-be nil)))

(describe "vulpea-db-query"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "returns all notes when filter function is not passed"
    (expect (length (vulpea-db-query))
            :to-equal
            (+ (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from nodes]))
               (caar (org-roam-db-query
                      [:select (funcall count *)
                       :from aliases])))))

  (it "applies filter function when passed"
    (expect (vulpea-db-query (lambda (n)
                               (string-equal (vulpea-note-title n)
                                             "Duplicating Term")))
            :to-have-same-items-as
            (list
             (make-vulpea-note
              :path (expand-file-name "same-name-1.org" org-roam-directory)
              :title "Duplicating Term"
              :tags '("tag1" "tag2")
              :level 0
              :id "ff01962f-47c2-4a32-9bf4-990e41090a9b"
              :properties (list
                           (cons "CATEGORY" "same-name-1")
                           (cons "ID" "ff01962f-47c2-4a32-9bf4-990e41090a9b")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "same-name-1.org" org-roam-directory))
                           (cons "PRIORITY" "B")))
             (make-vulpea-note
              :path (expand-file-name "same-name-2.org" org-roam-directory)
              :title "Duplicating Term"
              :tags '("tag3")
              :level 0
              :id "68f11246-91e1-4d48-b3c6-801a2ef0160b"
              :properties (list
                           (cons "CATEGORY" "same-name-2")
                           (cons "ID" "68f11246-91e1-4d48-b3c6-801a2ef0160b")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "same-name-2.org" org-roam-directory))
                           (cons "PRIORITY" "B"))))))

  (it "includes meta"
    (expect (vulpea-db-query
             (lambda (n)
               (seq-contains-p
                (cdr (assoc "singleton" (vulpea-note-meta n)))
                "only value")))
            :to-have-same-items-as
            (list
             (make-vulpea-note
              :path (expand-file-name "with-meta.org" org-roam-directory)
              :title "Note with META"
              :tags nil
              :level 0
              :id "05907606-f836-45bf-bd36-a8444308eddd"
              :properties (list
                           (cons "CATEGORY" "with-meta")
                           (cons "ID" "05907606-f836-45bf-bd36-a8444308eddd")
                           (cons "BLOCKED" "")
                           (cons "FILE" (expand-file-name "with-meta.org" org-roam-directory))
                           (cons "PRIORITY" "B"))
              :meta
              '(("name" . ("some name"))
                ("tags" . ("tag 1" "tag 2" "tag 3"))
                ("numbers" . ("12" "18" "24"))
                ("singleton" . ("only value"))
                ("symbol" . ("red"))
                ("url" . ("[[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]"))
                ("link" . ("[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]"))
                ("references" . ("[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]"
                                 "[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]"))
                ("answer" . ("42"))))))))

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
             :aliases '("Alias of the note with alias")
             :level 0
             :id "72522ed2-9991-482e-a365-01155c172aa5"
             :properties (list
                          (cons "CATEGORY" "note-with-alias")
                          (cons "ROAM_ALIASES" "\"Alias of the note with alias\"")
                          (cons "ID" "72522ed2-9991-482e-a365-01155c172aa5")
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "note-with-alias.org" org-roam-directory))
                          (cons "PRIORITY" "B")))))

  (it "returns note by sub-heading id"
    (expect (vulpea-db-get-by-id "b77a4837-71d6-495e-98f1-b576464aacc1")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "big-note.org" org-roam-directory)
             :title "Big note sub-heading"
             :tags nil
             :level 1
             :id "b77a4837-71d6-495e-98f1-b576464aacc1"
             :properties (list
                          (cons "CATEGORY" "big-note")
                          (cons "ID" "b77a4837-71d6-495e-98f1-b576464aacc1")
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "big-note.org" org-roam-directory))
                          (cons "PRIORITY" "B")
                          (cons "ITEM" "Big note sub-heading")))))

  (it "returns note by sub-sub-heading id"
    (expect (vulpea-db-get-by-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "big-note.org" org-roam-directory)
             :title "Big note sub-sub-heading"
             :tags nil
             :level 2
             :id "cfc39858-351d-4f1e-8f98-10d16d71f49e"
             :properties (list
                          (cons "CATEGORY" "big-note")
                          (cons "ID" "cfc39858-351d-4f1e-8f98-10d16d71f49e")
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "big-note.org" org-roam-directory))
                          (cons "PRIORITY" "B")
                          (cons "ITEM" "Big note sub-sub-heading")))))

  (it "includes meta in response"
    (expect (vulpea-db-get-by-id "05907606-f836-45bf-bd36-a8444308eddd")
            :to-equal
            (make-vulpea-note
             :path (expand-file-name "with-meta.org" org-roam-directory)
             :title "Note with META"
             :tags nil
             :level 0
             :id "05907606-f836-45bf-bd36-a8444308eddd"
             :properties (list
                          (cons "CATEGORY" "with-meta")
                          (cons "ID" "05907606-f836-45bf-bd36-a8444308eddd")
                          (cons "BLOCKED" "")
                          (cons "FILE" (expand-file-name "with-meta.org" org-roam-directory))
                          (cons "PRIORITY" "B"))
             :meta '(("name" . ("some name"))
                     ("tags" . ("tag 1" "tag 2" "tag 3"))
                     ("numbers" . ("12" "18" "24"))
                     ("singleton" . ("only value"))
                     ("symbol" . ("red"))
                     ("url" . ("[[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]"))
                     ("link" . ("[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]"))
                     ("references" . ("[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]"
                                      "[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]"))
                     ("answer" . ("42")))))))

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
