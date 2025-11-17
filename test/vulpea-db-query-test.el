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

;;; Link Query Tests

(ert-deftest vulpea-db-query-by-links-some-single ()
  "Test querying by single link destination."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "other" :type "id")))
    (vulpea-test--insert-test-note "note3" "Note 3")

    (let ((notes (vulpea-db-query-by-links-some '("target"))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-some-multiple ()
  "Test querying by multiple link destinations (OR)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target1" :type "id")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target2" :type "id")))
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :links '((:dest "other" :type "id")))

    (let ((notes (vulpea-db-query-by-links-some '("target1" "target2"))))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-links-some-with-type ()
  "Test querying by links with type filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target" :type "file")))

    (let ((notes (vulpea-db-query-by-links-some '("target") "id")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-some-backward-compat ()
  "Test backward compatibility with cons cell format."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target" :type "id")))

    (let ((notes (vulpea-db-query-by-links-some '(("id" . "target")))))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-links-every-multiple ()
  "Test querying by multiple link destinations (AND)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :links '((:dest "target1" :type "id")
                                           (:dest "target2" :type "id")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :links '((:dest "target1" :type "id")))

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
                                   :meta '(("country" . ((:value "France" :type "string")))))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :meta '(("country" . ((:value "Italy" :type "string")))))

    (let ((notes (vulpea-db-query-by-meta "country" "France")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

(ert-deftest vulpea-db-query-by-meta-with-type ()
  "Test querying notes by metadata with type filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :meta '(("region" . ((:value "region-id" :type "note")))))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :meta '(("region" . ((:value "region-id" :type "string")))))

    (let ((notes (vulpea-db-query-by-meta "region" "region-id" "note")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

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
                                   :meta '(("country" . ((:value "France" :type "string"))))
                                   :links '((:dest "target" :type "id"))
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
                     '(("country" . ((:value "France" :type "string"))))))
      (should (equal (vulpea-note-links note) '((:dest "target" :type "id"))))
      (should (equal (vulpea-note-todo note) "TODO"))
      (should (equal (vulpea-note-priority note) "A")))))

(provide 'vulpea-db-query-test)
;;; vulpea-db-query-test.el ends here
