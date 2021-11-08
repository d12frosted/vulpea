;;; vulpea-meta-test.el --- Test metadata API  -*- lexical-binding: t; -*-
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
;; Test `vulpea-meta' module.
;;
;;; Code:

(require 'vulpea-test-utils)
(require 'org-roam)
(require 'vulpea-meta)
(require 'vulpea-db)

(describe "vulpea-meta-get"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "extracts string value by default"
    (let ((a (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string))
          (b (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name")))
      (expect a :to-equal b)))

  (it "extracts raw value"
    (expect (org-element-interpret-data
             (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'raw))
            :to-equal "some name\n"))

  (it "extracts string value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string)
            :to-equal "some name"))

  (it "extracts number value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "answer" 'number)
            :to-equal 42))

  (it "extracts link value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "link" 'link)
            :to-equal "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))

  (it "extracts URL value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "url" 'link)
            :to-equal "https://en.wikipedia.org/wiki/Frappato"))

  (it "extracts note value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "link" 'note)
            :to-equal (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))

  (it "extracts symbol value"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "symbol" 'symbol)
            :to-equal 'red))

  (it "extracts first element of the list"
    (expect (vulpea-meta-get "05907606-f836-45bf-bd36-a8444308eddd" "tags" 'string)
            :to-equal "tag 1")))

(describe "vulpea-meta-get-list"
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "extracts string value by default"
    (let ((a (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string))
          (b (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name")))
      (expect a :to-equal b)))

  (it "extracts raw value"
    (expect (seq-map
             #'org-element-interpret-data
             (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'raw))
            :to-equal '("some name\n")))

  (it "extracts string value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "name" 'string)
            :to-equal '("some name")))

  (it "extracts number value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "answer" 'number)
            :to-equal '(42)))

  (it "extracts link value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "link" 'link)
            :to-equal '("444f94d7-61e0-4b7c-bb7e-100814c6b4bb")))

  (it "extracts URL value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "url" 'link)
            :to-equal '("https://en.wikipedia.org/wiki/Frappato")))

  (it "extracts note value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "link" 'note)
            :to-equal (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"))))

  (it "extracts symbol value"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "symbol" 'symbol)
            :to-equal '(red)))

  (it "extracts list of strings"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "tags" 'string)
            :to-equal '("tag 1"
                        "tag 2"
                        "tag 3")))

  (it "extracts list of numbers"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "numbers" 'number)
            :to-equal '(12 18 24)))

  (it "extracts list of links"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "references" 'link)
            :to-equal '("444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                        "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))

  (it "extracts list of notes"
    (expect (vulpea-meta-get-list "05907606-f836-45bf-bd36-a8444308eddd" "references" 'note)
            :to-equal (list (vulpea-db-get-by-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
                            (vulpea-db-get-by-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")))))

(describe "vulpea-meta-set"
  :var ((without-meta-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
        (with-meta-id "05907606-f836-45bf-bd36-a8444308eddd")
        (reference-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "sets a singe string value in a note without meta"
    (vulpea-meta-set without-meta-id "name" "some name")
    (expect (vulpea-meta-get without-meta-id "name" 'string)
            :to-equal "some name"))

  (it "sets a singe number value in a note without meta"
    (vulpea-meta-set without-meta-id "answer" 42)
    (expect (vulpea-meta-get without-meta-id "answer" 'number)
            :to-equal 42))

  (it "sets a singe link value in a note without meta"
    (vulpea-meta-set without-meta-id "references" reference-id)
    (expect (vulpea-meta-get without-meta-id "references" 'link)
            :to-equal reference-id))

  (it "sets a singe note value in a note without meta"
    (vulpea-meta-set without-meta-id "references" (vulpea-db-get-by-id reference-id))
    (expect (vulpea-meta-get without-meta-id "references" 'link)
            :to-equal reference-id))

  (it "sets a singe symbol value in a note without meta"
    (vulpea-meta-set without-meta-id "symbol" 'red)
    (expect (vulpea-meta-get without-meta-id "symbol" 'symbol)
            :to-equal 'red))

  (it "sets multiple values in a note without meta"
    (vulpea-meta-set without-meta-id "tags" '("tag 1" "tag 2" "tag 3"))
    (expect (vulpea-meta-get-list without-meta-id "tags" 'string)
            :to-equal '("tag 1" "tag 2" "tag 3")))

  (it "replaces multiple values with a single value"
    (vulpea-meta-set with-meta-id "numbers" 42)
    (expect (vulpea-meta-get-list with-meta-id "numbers" 'number)
            :to-equal '(42)))

  (it "replaces multiple values with new values"
    (vulpea-meta-set with-meta-id "numbers" '(1 2 3 4 5))
    (expect (vulpea-meta-get-list with-meta-id "numbers" 'number)
            :to-equal '(1 2 3 4 5))))

(describe "vulpea-meta-remove"
  :var ((without-meta-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
        (with-meta-id "05907606-f836-45bf-bd36-a8444308eddd"))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "has no effect on a note without meta"
    (vulpea-meta-remove without-meta-id "numbers")
    (expect (vulpea-meta-get without-meta-id "numbers" 'number)
            :to-equal nil))

  (it "has no effect on a missing property"
    (vulpea-meta-remove with-meta-id "age")
    (expect (vulpea-meta-get with-meta-id "age" 'number)
            :to-equal nil))

  (it "removes a single value"
    (vulpea-meta-remove with-meta-id "answer")
    (expect (vulpea-meta-get with-meta-id "answer" 'number)
            :to-equal nil))

  (it "removes multiple values"
    (vulpea-meta-remove with-meta-id "numbers")
    (expect (vulpea-meta-get with-meta-id "numbers" 'number)
            :to-equal nil)))

(describe "vulpea-meta-clean"
  :var ((without-meta-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
        (with-meta-id "05907606-f836-45bf-bd36-a8444308eddd"))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "has no effect on a note without meta"
    (expect (length (org-element-map
                        (plist-get (vulpea-meta without-meta-id)
                                   :pl)
                        'item #'identity))
            :to-equal 0)
    (vulpea-meta-clean without-meta-id)
    (expect (length (org-element-map
                        (plist-get (vulpea-meta without-meta-id)
                                   :pl)
                        'item #'identity))
            :to-equal 0))

  (it "removes all meta"
    (expect (length (org-element-map
                        (plist-get (vulpea-meta with-meta-id)
                                   :pl)
                        'item #'identity))
            :to-equal 14)
    (vulpea-meta-clean with-meta-id)
    (expect (length (org-element-map
                        (plist-get (vulpea-meta with-meta-id)
                                   :pl)
                        'item #'identity))
            :to-equal 0)))

(describe "vulpea-meta formatting"
  :var ((without-meta-id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb")
        (without-meta-file "without-meta.org")
        (with-meta-id "05907606-f836-45bf-bd36-a8444308eddd")
        (with-meta-file "with-meta.org")
        (reference-id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
        (reference-file "reference.org"))
  (before-each
    (vulpea-test--init))

  (after-each
    (vulpea-test--teardown))

  (describe "vulpea-meta-set"
    (it "formats single string value upon insertion to file without meta and without body"
      (vulpea-meta-set reference-id "name" "some name")
      (expect reference-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- name :: some name
"))

    (it "formats single string value upon insertion to file without meta"
      (vulpea-meta-set without-meta-id "name" "some name")
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- name :: some name

Just some text to make sure that meta is inserted before.
"))

    (it "formats single number value upon insertion to file without meta"
      (vulpea-meta-set without-meta-id "answer" 42)
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- answer :: 42

Just some text to make sure that meta is inserted before.
"))

    (it "formats single id value upon insertion to file without meta"
      (vulpea-meta-set without-meta-id "references" "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7")
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]

Just some text to make sure that meta is inserted before.
"))

    (it "formats single symbol value upon insertion to file without meta"
      (vulpea-meta-set without-meta-id "symbol" 'red)
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- symbol :: red

Just some text to make sure that meta is inserted before.
"))

    (it "formats multiple values upon insertion to file without meta"
      (vulpea-meta-set without-meta-id "references" '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                                                      "05907606-f836-45bf-bd36-a8444308eddd"
                                                      "https://en.wikipedia.org/wiki/Frappato"
                                                      "trust me™"))
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- references :: [[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]
- references :: trust me™

Just some text to make sure that meta is inserted before.
"))

    (it "appends to the beginning of the list when APPEND is nil"
      (vulpea-meta-set without-meta-id
                       "references"
                       '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                         "05907606-f836-45bf-bd36-a8444308eddd"))
      (vulpea-meta-set without-meta-id "age" 42)
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- age :: 42
- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]

Just some text to make sure that meta is inserted before.
"))

    (it "appends to the end of the list when APPEND is non-nil (when body is present)"
      (vulpea-meta-set without-meta-id
                       "references"
                       '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                         "05907606-f836-45bf-bd36-a8444308eddd")
                       'append)
      (vulpea-meta-set without-meta-id "age" 42 'append)
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42

Just some text to make sure that meta is inserted before.
"))

    (it "appends to the end of the list when APPEND is non-nil (when body is missing)"
      (vulpea-meta-set reference-id
                       "references"
                       '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                         "05907606-f836-45bf-bd36-a8444308eddd")
                       'append)
      (vulpea-meta-set reference-id "age" 42 'append)
      (expect reference-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42
"))

    (it "inserts values with respect to trailing new lines in file without body"
      (vulpea-test--map-file (lambda (_)
                               ;; first line after the header
                               (goto-char 124)
                               (insert "\n\n\n\n\n"))
                             reference-file)
      (vulpea-meta-set reference-id
                       "references"
                       '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                         "05907606-f836-45bf-bd36-a8444308eddd")
                       'append)
      (vulpea-meta-set reference-id "age" 42 'append)
      (expect reference-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:       5093fc4e-8c63-4e60-a1da-83fc7ecd5db7
:END:
#+title: Reference
#+filetags: :tag1:tag2:tag3:

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42





"))

    (it "inserts values with respect to trailing new lines in file with body"
      (vulpea-test--map-file (lambda (_)
                               ;; first line after the body
                               (goto-char 168)
                               (insert "\n\n\n")
                               ;; first line after the header
                               (goto-char 109)
                               (insert "\n\n\n\n\n"))
                             without-meta-file)
      (vulpea-meta-set without-meta-id
                       "references"
                       '("5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                         "05907606-f836-45bf-bd36-a8444308eddd")
                       'append)
      (vulpea-meta-set without-meta-id "age" 42 'append)
      (expect without-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     444f94d7-61e0-4b7c-bb7e-100814c6b4bb
:END:
#+title: Note without META

- references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
- references :: [[id:05907606-f836-45bf-bd36-a8444308eddd][Note with META]]
- age :: 42






Just some text to make sure that meta is inserted before.



"))

    (it "cleans meta from a note with body"
      (vulpea-meta-clean with-meta-id)
      (expect with-meta-file
              :to-contain-exactly
              ":PROPERTIES:
:ID:                     05907606-f836-45bf-bd36-a8444308eddd
:END:
#+title: Note with META

Don't mind me. I am a content of this note.
"))))

(provide 'vulpea-meta-test)
;;; vulpea-meta-test.el ends here
