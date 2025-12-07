;;; vulpea-db-query-test.el --- Tests for vulpea-db-query -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for vulpea-db-query.el (v2 query layer)
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-query)

;;; Test Infrastructure

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
          (vulpea-db-location temp-file)
          (vulpea-db--connection nil))
     (unwind-protect
         (progn ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

;;; Test Helpers

(defun vulpea-test--insert-test-note (id title &rest args)
  "Insert a test note with ID and TITLE.

ARGS is a plist with optional fields:
  :path, :level, :pos, :tags, :links, :meta, :properties, etc."
  (apply #'vulpea-db--insert-note
         :id id
         :path (or (plist-get args :path) (format "/tmp/%s.org" id))
         :level (or (plist-get args :level) 0)
         :pos (or (plist-get args :pos) 0)
         :title title
         :properties (plist-get args :properties)
         :tags (plist-get args :tags)
         :aliases (plist-get args :aliases)
         :meta (plist-get args :meta)
         :links (plist-get args :links)
         :todo (plist-get args :todo)
         :priority (plist-get args :priority)
         :modified-at (or (plist-get args :modified-at) "2025-11-16 10:00:00")
         args))

;;; Query Tests

(ert-deftest vulpea-db-get-by-id-found ()
  "Test getting note by ID when it exists."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "test-id" "Test Note"
                                   :tags '("tag1" "tag2"))

    (let ((note (vulpea-db-get-by-id "test-id")))
      (should note)
      (should (vulpea-note-p note))
      (should (equal (vulpea-note-id note) "test-id"))
      (should (equal (vulpea-note-title note) "Test Note"))
      (should (equal (vulpea-note-tags note) '("tag1" "tag2"))))))

(ert-deftest vulpea-db-get-by-id-not-found ()
  "Test getting note by ID when it doesn't exist."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should-not (vulpea-db-get-by-id "nonexistent-id"))))

(ert-deftest vulpea-db-query-all ()
  "Test querying all notes without predicate."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1")
    (vulpea-test--insert-test-note "note2" "Note 2")
    (vulpea-test--insert-test-note "note3" "Note 3")

    (let ((notes (vulpea-db-query)))
      (should (= (length notes) 3))
      (should (cl-every #'vulpea-note-p notes))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-with-predicate ()
  "Test querying with predicate filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Wine Note" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Work Note" :tags '("work"))
    (vulpea-test--insert-test-note "note3" "Wine Cellar" :tags '("wine" "cellar"))

    (let ((wine-notes (vulpea-db-query
                       (lambda (note)
                         (member "wine" (vulpea-note-tags note))))))
      (should (= (length wine-notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id wine-notes)))
      (should (member "note3" (mapcar #'vulpea-note-id wine-notes)))
      (should-not (member "note2" (mapcar #'vulpea-note-id wine-notes))))))

;;; Tag Query Tests

(ert-deftest vulpea-db-query-by-tags-some-single ()
  "Test querying by single tag (OR)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine" "red"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("wine" "white"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("work"))

    (let ((notes (vulpea-db-query-by-tags-some '("wine"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-tags-some-multiple ()
  "Test querying by multiple tags (OR)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("beer"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("work"))

    (let ((notes (vulpea-db-query-by-tags-some '("wine" "beer"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-tags-some-empty ()
  "Test querying by empty tag list returns nil."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))

    (should-not (vulpea-db-query-by-tags-some nil))
    (should-not (vulpea-db-query-by-tags-some '()))))

(ert-deftest vulpea-db-query-by-tags-every-single ()
  "Test querying by single tag (AND)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine" "red"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("wine" "white"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("work"))

    (let ((notes (vulpea-db-query-by-tags-every '("wine"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-tags-every-multiple ()
  "Test querying by multiple tags (AND)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine" "red" "italy"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("wine" "red"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("wine" "white"))

    (let ((notes (vulpea-db-query-by-tags-every '("wine" "red"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))

    (let ((notes (vulpea-db-query-by-tags-every '("wine" "red" "italy"))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-tags-every-empty ()
  "Test querying by empty tag list returns all notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("work"))

    (let ((notes (vulpea-db-query-by-tags-every nil)))
      (should (= (length notes) 2)))))

(ert-deftest vulpea-db-query-by-tags-none-single ()
  "Test querying by single excluded tag."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("work"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("personal"))

    (let ((notes (vulpea-db-query-by-tags-none '("wine"))))
      (should (= (length notes) 2))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-tags-none-multiple ()
  "Test querying by multiple excluded tags."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("work"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("personal"))

    (let ((notes (vulpea-db-query-by-tags-none '("wine" "work"))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note3")))))

(ert-deftest vulpea-db-query-by-tags-none-empty ()
  "Test querying by empty exclusion list returns all notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("work"))

    (let ((notes (vulpea-db-query-by-tags-none nil)))
      (should (= (length notes) 2)))))

(ert-deftest vulpea-db-query-by-tags-with-duplicates ()
  "Test that duplicate tags in source list are properly handled.

Regression test for bug where duplicate tags violated PRIMARY KEY
constraint and caused transaction rollback, preventing any tags
from being inserted into the normalized tags table."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Insert note with duplicate tags (mimics org file with :tag:tag:)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :tags '("wine" "cellar" "wine"))

    ;; Verify note was inserted
    (let ((note (vulpea-db-get-by-id "note1")))
      (should note)
      (should (equal (vulpea-note-id note) "note1")))

    ;; Verify tags were deduplicated and inserted into tags table
    (let ((rows (emacsql (vulpea-db)
                         [:select [tag] :from tags
                          :where (= note-id $s1)
                          :order-by [(asc tag)]]
                         "note1")))
      (should (= (length rows) 2))
      (should (equal (mapcar #'car rows) '("cellar" "wine"))))

    ;; Verify tag queries work correctly
    (should (= (length (vulpea-db-query-by-tags-some '("wine"))) 1))
    (should (= (length (vulpea-db-query-by-tags-every '("wine" "cellar"))) 1))
    (should (= (length (vulpea-db-query-by-tags-none '("beer"))) 1))
    (should (= (length (vulpea-db-query-by-tags-none '("wine"))) 0))))

(ert-deftest vulpea-db-query-tags ()
  "Test getting all unique tags from database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("wine" "red"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("wine" "white"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("work"))

    (let ((tags (vulpea-db-query-tags)))
      (should (= (length tags) 4))
      (should (equal tags '("red" "white" "wine" "work"))))))

(ert-deftest vulpea-db-query-tags-empty ()
  "Test getting tags from empty database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should (null (vulpea-db-query-tags)))))

;;; Link Query Tests

(ert-deftest vulpea-db-query-by-links-some-single ()
  "Test querying by single link destination."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "other" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note3" "Note 3")

    (let ((notes (vulpea-db-query-by-links-some '("target"))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-some-multiple ()
  "Test querying by multiple link destinations (OR)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target1" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target2" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :links '((:dest "other" :type "id" :pos 100)))

    (let ((notes (vulpea-db-query-by-links-some '("target1" "target2"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-links-some-with-type ()
  "Test querying by links with type filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target" :type "file" :pos 100)))

    (let ((notes (vulpea-db-query-by-links-some '("target") "id")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-some-backward-compat ()
  "Test backward compatibility with cons cell format."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id" :pos 100)))

    (let ((notes (vulpea-db-query-by-links-some '(("id" . "target")))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-every-multiple ()
  "Test querying by multiple link destinations (AND)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target1" :type "id" :pos 100)
                                           (:dest "target2" :type "id" :pos 200)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target1" :type "id" :pos 100)))

    (let ((notes (vulpea-db-query-by-links-every '("target1" "target2"))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

;;; Level Query Tests

(ert-deftest vulpea-db-query-by-level-file ()
  "Test querying file-level notes (level 0)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "file1" "File 1" :level 0)
    (vulpea-test--insert-test-note "heading1" "Heading 1" :level 1)
    (vulpea-test--insert-test-note "heading2" "Heading 2" :level 2)

    (let ((notes (vulpea-db-query-by-level 0)))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "file1"))
      (should (= (vulpea-note-level (car notes)) 0)))))

(ert-deftest vulpea-db-query-by-level-heading ()
  "Test querying heading-level notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "file1" "File 1" :level 0)
    (vulpea-test--insert-test-note "heading1" "Heading 1" :level 1)
    (vulpea-test--insert-test-note "heading2" "Heading 2" :level 1)

    (let ((notes (vulpea-db-query-by-level 1)))
      (should (= (length notes) 2))
      (should (member "heading1" (mapcar #'vulpea-note-id notes)))
      (should (member "heading2" (mapcar #'vulpea-note-id notes))))))

;;; Title Search Tests

(ert-deftest vulpea-db-search-by-title-exact ()
  "Test searching by exact title match."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Wine Cellar")
    (vulpea-test--insert-test-note "note2" "Wine Collection")
    (vulpea-test--insert-test-note "note3" "Beer Garden")

    (let ((notes (vulpea-db-search-by-title "Wine")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-search-by-title-case-insensitive ()
  "Test case-insensitive title search (default)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Wine Cellar")
    (vulpea-test--insert-test-note "note2" "WINE Collection")

    (let ((notes (vulpea-db-search-by-title "wine")))
      (should (= (length notes) 2)))))

(ert-deftest vulpea-db-search-by-title-case-sensitive ()
  "Test case-sensitive title search."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Wine Cellar")
    (vulpea-test--insert-test-note "note2" "wine collection")

    (let ((notes (vulpea-db-search-by-title "Wine" t)))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

;;; Meta Query Tests

(ert-deftest vulpea-db-query-by-meta-key ()
  "Test querying notes by metadata key."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :meta '(("country" . ((:value "France" :type "string")))))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :meta '(("country" . ((:value "Italy" :type "string")))))
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :meta '(("price" . ((:value "25.50" :type "number")))))

    (let ((notes (vulpea-db-query-by-meta-key "country")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-meta-value ()
  "Test querying notes by metadata key and value."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :meta '(("country" . ("France"))))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :meta '(("country" . ("Italy"))))

    (let ((notes (vulpea-db-query-by-meta "country" "France")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-meta-with-type ()
  "Test querying notes by metadata - type filtering no longer supported."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :meta '(("region" . ("region-id"))))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :meta '(("region" . ("region-id"))))

    ;; Both notes have same key/value, can't filter by type anymore
    (let ((notes (vulpea-db-query-by-meta "region" "region-id")))
      (should (= (length notes) 2)))))

;;; Statistics Tests

(ert-deftest vulpea-db-count-notes ()
  "Test counting total notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should (= (vulpea-db-count-notes) 0))

    (vulpea-test--insert-test-note "note1" "Note 1")
    (should (= (vulpea-db-count-notes) 1))

    (vulpea-test--insert-test-note "note2" "Note 2")
    (vulpea-test--insert-test-note "note3" "Note 3")
    (should (= (vulpea-db-count-notes) 3))))

(ert-deftest vulpea-db-count-by-level ()
  "Test counting notes by level."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "file1" "File 1" :level 0)
    (vulpea-test--insert-test-note "file2" "File 2" :level 0)
    (vulpea-test--insert-test-note "heading1" "Heading 1" :level 1)

    (should (= (vulpea-db-count-file-level-notes) 2))
    (should (= (vulpea-db-count-heading-level-notes) 1))))

;;; Integration Tests

(ert-deftest vulpea-db-query-complex-filter ()
  "Test complex filtering combining multiple predicates."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Wine Note"
                                   :tags '("wine" "red")
                                   :level 0)
    (vulpea-test--insert-test-note "note2" "Wine Heading"
                                   :tags '("wine" "white")
                                   :level 1)
    (vulpea-test--insert-test-note "note3" "Beer Note"
                                   :tags '("beer")
                                   :level 0)

    ;; Filter: wine tag AND file-level
    (let ((notes (vulpea-db-query
                  (lambda (note)
                    (and (member "wine" (vulpea-note-tags note))
                         (= (vulpea-note-level note) 0))))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-note-structure ()
  "Test that query returns proper note structure with all fields."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :path "/tmp/test.org"
                                   :level 0
                                   :pos 0
                                   :tags '("tag1" "tag2")
                                   :aliases '("alias1")
                                   :properties '((key . "value"))
                                   :meta '(("country" . ("France")))
                                   :links '((:dest "target" :type "id" :pos 100))
                                   :todo "TODO"
                                   :priority "A")

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (vulpea-note-p note))
      (should (equal (vulpea-note-id note) "note1"))
      (should (equal (vulpea-note-title note) "Test Note"))
      (should (equal (vulpea-note-path note) "/tmp/test.org"))
      (should (= (vulpea-note-level note) 0))
      (should (= (vulpea-note-pos note) 0))
      (should (equal (vulpea-note-tags note) '("tag1" "tag2")))
      (should (equal (vulpea-note-aliases note) '("alias1")))
      (should (equal (vulpea-note-properties note) '((key . "value"))))
      (should (equal (vulpea-note-meta note)
                     '(("country" . ("France")))))
      (should (equal (vulpea-note-links note) '((:dest "target" :type "id" :pos 100))))
      (should (equal (vulpea-note-todo note) "TODO"))
      (should (equal (vulpea-note-priority note) "A")))))

;;; File Path Query Tests

(ert-deftest vulpea-db-query-by-file-path-basic ()
  "Test querying notes by file path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org" :level 0)
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/file1.org" :level 1)
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/file2.org" :level 0)

    (let ((notes (vulpea-db-query-by-file-path "/tmp/file1.org")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-file-path-with-level ()
  "Test querying notes by file path with level filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org" :level 0)
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/file1.org" :level 1)
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/file1.org" :level 2)

    (let ((notes (vulpea-db-query-by-file-path "/tmp/file1.org" 0)))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))

    (let ((notes (vulpea-db-query-by-file-path "/tmp/file1.org" 1)))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note2")))))

(ert-deftest vulpea-db-query-by-file-path-not-found ()
  "Test querying notes by non-existent file path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org")

    (should-not (vulpea-db-query-by-file-path "/tmp/nonexistent.org"))))

(ert-deftest vulpea-db-query-by-file-paths-basic ()
  "Test querying notes by multiple file paths."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org")
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/file2.org")
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/file3.org")

    (let ((notes (vulpea-db-query-by-file-paths '("/tmp/file1.org" "/tmp/file2.org"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-file-paths-with-level ()
  "Test querying notes by multiple file paths with level filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org" :level 0)
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/file1.org" :level 1)
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/file2.org" :level 0)
    (vulpea-test--insert-test-note "note4" "Note 4" :path "/tmp/file2.org" :level 1)

    (let ((notes (vulpea-db-query-by-file-paths '("/tmp/file1.org" "/tmp/file2.org") 0)))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-file-paths-empty ()
  "Test querying notes by empty file paths list."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/file1.org")

    (should-not (vulpea-db-query-by-file-paths nil))
    (should-not (vulpea-db-query-by-file-paths '()))))

;;; Directory Query Tests

(ert-deftest vulpea-db-query-by-directory-basic ()
  "Test querying notes by directory."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/notes/file1.org")
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/notes/file2.org")
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/other/file3.org")

    (let ((notes (vulpea-db-query-by-directory "/tmp/notes")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-directory-with-trailing-slash ()
  "Test querying notes by directory with trailing slash."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/notes/file1.org")
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/other/file2.org")

    (let ((notes (vulpea-db-query-by-directory "/tmp/notes/")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-directory-subdirectories ()
  "Test querying notes includes subdirectories."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/notes/file1.org")
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/notes/sub/file2.org")
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/notes/sub/deep/file3.org")
    (vulpea-test--insert-test-note "note4" "Note 4" :path "/tmp/other/file4.org")

    (let ((notes (vulpea-db-query-by-directory "/tmp/notes")))
      (should (= (length notes) 3))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-directory-with-level ()
  "Test querying notes by directory with level filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/notes/file1.org" :level 0)
    (vulpea-test--insert-test-note "note2" "Note 2" :path "/tmp/notes/file1.org" :level 1)
    (vulpea-test--insert-test-note "note3" "Note 3" :path "/tmp/notes/file2.org" :level 0)

    (let ((notes (vulpea-db-query-by-directory "/tmp/notes" 0)))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes))))

    (let ((notes (vulpea-db-query-by-directory "/tmp/notes" 1)))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note2")))))

(ert-deftest vulpea-db-query-by-directory-empty ()
  "Test querying notes by directory with no matches."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1" :path "/tmp/notes/file1.org")

    (should-not (vulpea-db-query-by-directory "/tmp/other"))))

(provide 'vulpea-db-query-test)
;;; vulpea-db-query-test.el ends here
