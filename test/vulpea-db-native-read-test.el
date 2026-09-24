;;; vulpea-db-native-read-test.el --- Tests for the native read path -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 24 Sep 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The read path runs SQL through `sqlite-select' and decodes cells
;; itself instead of going through emacsql.  Every test here compares
;; its results with what emacsql returns for the same data: they must
;; be `equal', value for value.
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-test-helpers)

(defconst vulpea-db-native-read-test--values
  (list "" "plain" "with space" "id:5093fc4e-1c2d"
        "/tmp/some path/file.org" "ünïcöd\u00e9" "日本語のタイトル"
        "emoji 🦊" "’curly’" "quote \" inside" "\"" "\"\"" "back\\slash"
        "\\" "ends with backslash\\" "new\nline" "tab\there" "cr\rhere"
        "nul\0byte" "esc\ehere" "del\177here" "\e[1m"
        "cafe\u0301" "caf\u00e9"
        "[2025-11-16 Sun 10:00]" "<2025-11-16 Sun>"
        "{\"key\": \"va\\\"lue\"}" "[\"a\",\"b\\\\c\"]" "null"
        "nil" "t" "42" "1.5" "-" "?a" "sym bol" "(paren"
        'symbol 'sym\ with\ space :keyword t 'keyword 'filename
        '("list" "of" "strings") '(:dest "x" :pos 1) [1 "two" three]
        42 -7 0 3.5 1.0e10 ?A nil)
  "Values covering what the printer escapes and what the reader treats specially.")

(defconst vulpea-db-native-read-test--raw-cells
  (list "" " " "   " "two words" "\"a\" \"b\"" "\"unterminated" "sym"
        "42" "(1 2 3)" "\"trailing\" " " \"leading\"")
  "Cell texts stored without emacsql's printing.
Rows written by other tools may hold them.  emacsql reads every
expression in such a cell and splices the results into the row, so
a cell may decode to no value or to several.")

(defun vulpea-db-native-read-test--make-table ()
  "Create a scratch table for cell round-trips."
  (sqlite-execute (oref (vulpea-db) handle)
                  "CREATE TABLE scratch (a, b, c)"))

(ert-deftest vulpea-db-select-decodes-cells-like-emacsql ()
  "Each value stored by emacsql decodes to what emacsql returns."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--make-table)
    (dolist (value vulpea-db-native-read-test--values)
      (emacsql (vulpea-db) [:insert :into scratch :values [$s1 $s2 $s3]]
               value "after" 1))
    (let ((expected (emacsql (vulpea-db) [:select * :from scratch]))
          (actual (vulpea-db--select "SELECT * FROM scratch")))
      (should (= (length actual) (length vulpea-db-native-read-test--values)))
      (should (equal actual expected))
      ;; And the values are the values stored
      (should (equal (mapcar #'car actual) vulpea-db-native-read-test--values)))))

(ert-deftest vulpea-db-select-decodes-natively-bound-cells-like-emacsql ()
  "Values written through native binding decode like emacsql reads them."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--make-table)
    (vulpea-db--insert-rows
     (oref (vulpea-db) handle) "INSERT INTO scratch (a, b, c)" 3
     (mapcar (lambda (value) (list value value nil))
             vulpea-db-native-read-test--values))
    (should (equal (vulpea-db--select "SELECT * FROM scratch")
                   (emacsql (vulpea-db) [:select * :from scratch])))))

(ert-deftest vulpea-db-select-splices-raw-cells-like-emacsql ()
  "Cells holding zero or several expressions splice like in emacsql.
A cell emacsql fails to read fails here too."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--make-table)
    (let ((handle (oref (vulpea-db) handle))
          (outcome (lambda (fn)
                     (condition-case nil
                         (list 'ok (funcall fn))
                       (error 'failed))))
          (failures 0))
      (dolist (cell vulpea-db-native-read-test--raw-cells)
        (sqlite-execute handle "DELETE FROM scratch")
        ;; First, middle and last position
        (sqlite-execute handle "INSERT INTO scratch (a, b, c) VALUES (?, ?, ?)"
                        (list cell "\"x\"" cell))
        (sqlite-execute handle "INSERT INTO scratch (a, b, c) VALUES (?, ?, ?)"
                        (list "\"x\"" cell 1))
        (let ((expected (funcall outcome
                                 (lambda ()
                                   (emacsql (vulpea-db) [:select * :from scratch])))))
          (when (eq expected 'failed)
            (setq failures (1+ failures)))
          (should (equal (funcall outcome
                                  (lambda ()
                                    (vulpea-db--select "SELECT * FROM scratch")))
                         expected))))
      ;; Both outcomes are covered
      (should (< 0 failures (length vulpea-db-native-read-test--raw-cells))))))

(ert-deftest vulpea-db-select-binds-parameters-like-emacsql ()
  "Parameters bind in the storage format, so lookups by value match."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--make-table)
    (dolist (value vulpea-db-native-read-test--values)
      (emacsql (vulpea-db) [:insert :into scratch :values [$s1 $s2 $s3]]
               value "after" 1))
    (dolist (value (remq nil vulpea-db-native-read-test--values))
      (should (equal (vulpea-db--select "SELECT a FROM scratch WHERE a = ?"
                                        (list value))
                     (emacsql (vulpea-db)
                              [:select a :from scratch :where (= a $s1)]
                              value))))))

(ert-deftest vulpea-db-sql-list-matches-emacsql-vectors ()
  "Inlined value lists select what emacsql's $v parameters select."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--make-table)
    (dolist (value vulpea-db-native-read-test--values)
      (emacsql (vulpea-db) [:insert :into scratch :values [$s1 $s2 $s3]]
               value "after" 1))
    (let ((values (append (remq nil vulpea-db-native-read-test--values)
                          '("it's" "'" "''"))))
      (should (equal (vulpea-db--select
                      (concat "SELECT a FROM scratch WHERE a IN "
                              (vulpea-db--sql-list values)))
                     (emacsql (vulpea-db)
                              [:select a :from scratch :where (in a $v1)]
                              (vconcat values)))))))

;;; Query functions

(defun vulpea-db-native-read-test--insert-fixture ()
  "Insert notes with tricky values into the current database."
  (vulpea-test--insert-test-note
   "note-1" "Title with \"quotes\" and \\backslash\\"
   :path "/tmp/cafe\u0301/one.org"
   :tags '("tag1" "täg2" "with \"quote\"")
   :aliases '("Alias \"one\"" "日本語")
   :properties '(("CREATED" . "[2025-11-16 Sun 10:00]")
                 ("QUOTED" . "va\"lue\\with\nnewline"))
   :meta '(("key" . ("value \"1\"" "value\\2"))
           ("empty" . ("")))
   :links '((:dest "note-2" :type "id" :pos 10 :description "to \"two\"")
            (:dest "note-3" :type "id" :pos 20 :description nil)
            (:dest "https://example.com/?q=\"x\"" :type "https" :pos 30
                   :description "multi\nline"))
   :todo "TODO"
   :priority ?A
   :scheduled "<2025-11-16 Sun>"
   :deadline "<2025-11-20 Thu 10:00>"
   :closed "[2025-11-17 Mon 11:00]"
   :category "cat\"egory"
   :category-source 'property
   :title-source 'keyword
   :created-at "2025-11-16")
  (vulpea-test--insert-test-note
   "note-2" "emoji 🦊 title\twith tab"
   :level 1
   :pos 42
   :path "/tmp/cafe\u0301/one.org"
   :tags '("tag1")
   :links '((:dest "note-3" :type "id" :pos 5 :description "three"))
   :outline-path '("Parent \"heading\"" "Child")
   :file-title "Title with \"quotes\" and \\backslash\\"
   :attach-dir "/tmp/data/no/te-2"
   :title-source 'heading)
  (vulpea-test--insert-test-note
   "note-3" "plain"
   :links '((:dest "note-2" :type "id" :pos 1 :description nil)))
  (vulpea-test--insert-test-note "note-4" ""))

(defun vulpea-db-native-read-test--notes (sql &rest args)
  "Return notes for emacsql SQL with ARGS, the reference decoding."
  (mapcar #'vulpea-db--row-to-note (apply #'emacsql (vulpea-db) sql args)))

(defun vulpea-db-native-read-test--links (sql &rest args)
  "Return links for emacsql SQL with ARGS, the reference decoding."
  (mapcar #'vulpea-db--row-to-link (apply #'emacsql (vulpea-db) sql args)))

(ert-deftest vulpea-db-native-read-notes-match-emacsql ()
  "Note queries return exactly what decoding emacsql rows returns."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--insert-fixture)
    (let ((all (vulpea-db-native-read-test--notes [:select * :from notes])))
      (should (= (length all) 4))
      (should (equal (vulpea-db-query) all))
      (should (equal (vulpea-db-query (lambda (n) (vulpea-note-tags n)))
                     (seq-filter #'vulpea-note-tags all)))
      (dolist (note all)
        (should (equal (vulpea-db-get-by-id (vulpea-note-id note)) note))))
    (should-not (vulpea-db-get-by-id "missing"))
    (should (equal (vulpea-db-query-by-ids '("note-3" "note-1" "missing"))
                   (vulpea-db-native-read-test--notes
                    [:select * :from notes :where (in id $v1)]
                    ["note-3" "note-1" "missing"])))))

(ert-deftest vulpea-db-native-read-tag-queries-match-emacsql ()
  "Tag queries return exactly what decoding emacsql rows returns."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--insert-fixture)
    (should (equal (vulpea-db-query-by-tags-some '("tag1" "with \"quote\""))
                   (vulpea-db-native-read-test--notes
                    [:select :distinct [notes:*]
                     :from notes
                     :inner :join tags
                     :on (= notes:id tags:note-id)
                     :where (in tags:tag $v1)]
                    ["tag1" "with \"quote\""])))
    (should (= 2 (length (vulpea-db-query-by-tags-some '("tag1")))))
    (should (equal (mapcar #'vulpea-note-id
                           (vulpea-db-query-by-tags-every '("tag1" "täg2")))
                   '("note-1")))
    (should (equal (mapcar #'vulpea-note-id
                           (vulpea-db-query-by-tags-none '("tag1")))
                   '("note-3" "note-4")))
    (should (equal (vulpea-db-query-tags)
                   (mapcar #'car (emacsql (vulpea-db)
                                          [:select :distinct [tag] :from tags
                                           :order :by tag]))))))

(ert-deftest vulpea-db-native-read-link-queries-match-emacsql ()
  "Link queries return exactly what decoding emacsql rows returns."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--insert-fixture)
    (should (= 5 (length (vulpea-db-query-links))))
    (should (equal (vulpea-db-query-links)
                   (vulpea-db-native-read-test--links [:select * :from links])))
    (should (equal (vulpea-db-query-links-by-type "https")
                   (vulpea-db-native-read-test--links
                    [:select * :from links :where (= type $s1)] "https")))
    (should (equal (vulpea-db-query-links-from "note-1")
                   (vulpea-db-native-read-test--links
                    [:select * :from links :where (= source $s1)] "note-1")))
    (should (equal (vulpea-db-query-links-to "note-2")
                   (vulpea-db-native-read-test--links
                    [:select * :from links :where (= dest $s1)] "note-2")))
    (should (equal (vulpea-db-query-links-to "https://example.com/?q=\"x\"")
                   (vulpea-db-native-read-test--links
                    [:select * :from links :where (= dest $s1)]
                    "https://example.com/?q=\"x\"")))
    (should (equal (vulpea-db-query-by-links-some '("note-2" "note-3"))
                   (vulpea-db-native-read-test--notes
                    [:select :distinct [notes:*]
                     :from notes
                     :inner :join links
                     :on (= notes:id links:source)
                     :where (in links:dest $v1)]
                    ["note-2" "note-3"])))
    (should (equal (vulpea-db-query-by-links-some '(("id" . "note-3")) "id")
                   (vulpea-db-native-read-test--notes
                    [:select :distinct [notes:*]
                     :from notes
                     :inner :join links
                     :on (= notes:id links:source)
                     :where (and (in links:dest $v1)
                                 (= links:type $s2))]
                    ["note-3"] "id")))
    (should (equal (mapcar #'vulpea-note-id
                           (vulpea-db-query-by-links-every '("note-2" "note-3")))
                   '("note-1")))
    (should (equal (mapcar #'vulpea-note-id
                           (vulpea-db-query-by-links-every
                            '("note-2" "note-3" "note-2") "id"))
                   '("note-1")))
    (should-not (vulpea-db-query-by-links-every '("note-3") "https"))
    (let ((counts (vulpea-db-query-backlink-counts))
          (id-counts (vulpea-db-query-backlink-counts "id"))
          (listed (vulpea-db-query-backlink-counts '("id" "https"))))
      (should (= (gethash "note-2" counts) 2))
      (should (= (gethash "note-3" counts) 2))
      (should (= (gethash "https://example.com/?q=\"x\"" counts) 1))
      (should (= (hash-table-count id-counts) 2))
      (should (= (hash-table-count listed) 3)))))

(ert-deftest vulpea-db-native-read-large-id-lists ()
  "Id lists beyond SQLite's bind parameter limit still work."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db-native-read-test--insert-fixture)
    (let ((ids (cons "note-1"
                     (mapcar (lambda (i) (format "missing-%d" i))
                             (number-sequence 1 40000)))))
      (should (equal (mapcar #'vulpea-note-id (vulpea-db-query-by-ids ids))
                     '("note-1")))
      (should (equal (mapcar #'vulpea-note-id
                             (vulpea-db-query-by-links-some (cons "note-3" ids)))
                     '("note-1" "note-2"))))))

(provide 'vulpea-db-native-read-test)
;;; vulpea-db-native-read-test.el ends here
