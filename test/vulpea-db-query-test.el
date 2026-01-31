;;; vulpea-db-query-test.el --- Tests for vulpea-db-query -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
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
(require 'vulpea-test-helpers)

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

;;; Attachment Query Tests

(ert-deftest vulpea-db-query-attachments-by-path-basic ()
  "Test querying attachment links by path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "image.png" :type "attachment" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :path "/tmp/file2.org"
                                   :attach-dir "/data/note2/"
                                   :links '((:dest "other.png" :type "attachment" :pos 100)))

    (let ((attachments (vulpea-db-query-attachments-by-path "/tmp/file1.org")))
      (should (= (length attachments) 1))
      (should (equal attachments '(("image.png" . "/data/note1/")))))))

(ert-deftest vulpea-db-query-attachments-by-path-multiple ()
  "Test querying multiple attachment links from same file."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "image1.png" :type "attachment" :pos 100)
                                           (:dest "image2.jpg" :type "attachment" :pos 200)))

    (let ((attachments (vulpea-db-query-attachments-by-path "/tmp/file1.org")))
      (should (= (length attachments) 2))
      (should (member '("image1.png" . "/data/note1/") attachments))
      (should (member '("image2.jpg" . "/data/note1/") attachments)))))

(ert-deftest vulpea-db-query-attachments-by-path-multiple-notes ()
  "Test querying attachments across multiple notes in same file."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :level 0
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "image1.png" :type "attachment" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :path "/tmp/file1.org"
                                   :level 1
                                   :attach-dir "/data/note2/"
                                   :links '((:dest "image2.png" :type "attachment" :pos 200)))

    (let ((attachments (vulpea-db-query-attachments-by-path "/tmp/file1.org")))
      (should (= (length attachments) 2))
      (should (member '("image1.png" . "/data/note1/") attachments))
      (should (member '("image2.png" . "/data/note2/") attachments)))))

(ert-deftest vulpea-db-query-attachments-by-path-distinct ()
  "Test that duplicate (dest . attach-dir) pairs are deduplicated."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Two notes with same attachment in same attach-dir (deduplicated)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :level 0
                                   :attach-dir "/data/shared/"
                                   :links '((:dest "shared.png" :type "attachment" :pos 100)))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :path "/tmp/file1.org"
                                   :level 1
                                   :attach-dir "/data/shared/"
                                   :links '((:dest "shared.png" :type "attachment" :pos 200)))

    (let ((attachments (vulpea-db-query-attachments-by-path "/tmp/file1.org")))
      (should (= (length attachments) 1))
      (should (equal attachments '(("shared.png" . "/data/shared/")))))))

(ert-deftest vulpea-db-query-attachments-by-path-mixed-links ()
  "Test that only attachment links are returned, not id links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "image.png" :type "attachment" :pos 100)
                                           (:dest "other-note-id" :type "id" :pos 200)
                                           (:dest "file.pdf" :type "attachment" :pos 300)))

    (let ((attachments (vulpea-db-query-attachments-by-path "/tmp/file1.org")))
      (should (= (length attachments) 2))
      (should (member '("image.png" . "/data/note1/") attachments))
      (should (member '("file.pdf" . "/data/note1/") attachments))
      (should-not (assoc "other-note-id" attachments)))))

(ert-deftest vulpea-db-query-attachments-by-path-no-attachments ()
  "Test querying path with no attachment links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "other-note" :type "id" :pos 100)))

    (should-not (vulpea-db-query-attachments-by-path "/tmp/file1.org"))))

(ert-deftest vulpea-db-query-attachments-by-path-not-found ()
  "Test querying non-existent path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :path "/tmp/file1.org"
                                   :attach-dir "/data/note1/"
                                   :links '((:dest "image.png" :type "attachment" :pos 100)))

    (should-not (vulpea-db-query-attachments-by-path "/tmp/nonexistent.org"))))

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

;;; Property-Based Query Tests

(ert-deftest vulpea-db-query-by-property-basic ()
  "Test querying notes by property key and value."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :properties '(("CATEGORY" . "journal")
                                                 ("CREATED" . "2025-12-08")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :properties '(("CATEGORY" . "journal")))
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :properties '(("CATEGORY" . "project")))

    (let ((notes (vulpea-db-query-by-property "CATEGORY" "journal")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-property-key-basic ()
  "Test querying notes by property key."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :properties '(("CREATED" . "2025-12-08")))
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :properties '(("CREATED" . "2025-12-09")))
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :properties '(("CATEGORY" . "other")))

    (let ((notes (vulpea-db-query-by-property-key "CREATED")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-property-not-found ()
  "Test querying notes by property with no matches."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :properties '(("CATEGORY" . "journal")))

    (should-not (vulpea-db-query-by-property "CATEGORY" "project"))
    (should-not (vulpea-db-query-by-property-key "NONEXISTENT"))))

;;; Created-At Query Tests

(ert-deftest vulpea-db-query-by-created-date-basic ()
  "Test querying notes by creation date."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :created-at "2025-12-08")
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :created-at "2025-12-08")
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :created-at "2025-12-09")

    (let ((notes (vulpea-db-query-by-created-date "2025-12-08")))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note2" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note3" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-created-date-with-level ()
  "Test querying notes by creation date with level filter."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :level 0
                                   :created-at "2025-12-08")
    (vulpea-test--insert-test-note "note2" "Note 2"
                                   :level 1
                                   :created-at "2025-12-08")
    (vulpea-test--insert-test-note "note3" "Note 3"
                                   :level 0
                                   :created-at "2025-12-08")

    (let ((notes (vulpea-db-query-by-created-date "2025-12-08" 0)))
      (should (= (length notes) 2))
      (should (member "note1" (mapcar #'vulpea-note-id notes)))
      (should (member "note3" (mapcar #'vulpea-note-id notes)))
      (should-not (member "note2" (mapcar #'vulpea-note-id notes))))))

(ert-deftest vulpea-db-query-by-created-date-not-found ()
  "Test querying notes by creation date with no matches."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :created-at "2025-12-08")

    (should-not (vulpea-db-query-by-created-date "2025-12-09"))))

(ert-deftest vulpea-db-query-by-created-date-null ()
  "Test querying notes excludes those without created-at."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Note 1"
                                   :created-at "2025-12-08")
    (vulpea-test--insert-test-note "note2" "Note 2")  ; No created-at

    (let ((notes (vulpea-db-query-by-created-date "2025-12-08")))
      (should (= (length notes) 1))
      (should (equal (vulpea-note-id (car notes)) "note1")))))

;;; Dead Link Detection Tests

(ert-deftest vulpea-db-query-dead-links-found ()
  "Test finding broken ID links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)
                                            (:dest "nonexistent" :type "id" :pos 200)))
    (vulpea-test--insert-test-note "note-b" "Note B")

    (let ((dead-links (vulpea-db-query-dead-links)))
      (should (= (length dead-links) 1))
      (should (equal (vulpea-note-id (car (car dead-links))) "note-a"))
      (should (equal (cdr (car dead-links)) "nonexistent")))))

(ert-deftest vulpea-db-query-dead-links-none ()
  "Test no dead links when all targets exist."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B"
                                   :links '((:dest "note-a" :type "id" :pos 100)))

    (should-not (vulpea-db-query-dead-links))))

(ert-deftest vulpea-db-query-dead-links-empty-db ()
  "Test dead links on empty database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should-not (vulpea-db-query-dead-links))))

(ert-deftest vulpea-db-query-dead-links-ignores-non-id ()
  "Test that non-id links are not reported as dead."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "https://example.com" :type "https" :pos 100)
                                            (:dest "file.org" :type "file" :pos 200)))

    (should-not (vulpea-db-query-dead-links))))

(ert-deftest vulpea-db-query-dead-links-multiple-sources ()
  "Test dead links from multiple source notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "ghost-1" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B"
                                   :links '((:dest "ghost-2" :type "id" :pos 100)))

    (let ((dead-links (vulpea-db-query-dead-links)))
      (should (= (length dead-links) 2))
      (let ((targets (mapcar #'cdr dead-links)))
        (should (member "ghost-1" targets))
        (should (member "ghost-2" targets))))))

;;; Orphan Note Detection Tests

(ert-deftest vulpea-db-query-orphan-notes-found ()
  "Test finding notes with no incoming links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B")

    ;; A links to B, so B has incoming links (not orphan).
    ;; Nothing links to A, so A is orphan.
    (let ((orphans (vulpea-db-query-orphan-notes)))
      (should (= (length orphans) 1))
      (should (equal (vulpea-note-id (car orphans)) "note-a")))))

(ert-deftest vulpea-db-query-orphan-notes-none ()
  "Test no orphans when all notes have incoming links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B"
                                   :links '((:dest "note-a" :type "id" :pos 100)))

    (should-not (vulpea-db-query-orphan-notes))))

(ert-deftest vulpea-db-query-orphan-notes-all ()
  "Test all notes are orphans when there are no links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A")
    (vulpea-test--insert-test-note "note-b" "Note B")

    (let ((orphans (vulpea-db-query-orphan-notes)))
      (should (= (length orphans) 2)))))

(ert-deftest vulpea-db-query-orphan-notes-empty-db ()
  "Test orphans on empty database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should-not (vulpea-db-query-orphan-notes))))

(ert-deftest vulpea-db-query-orphan-notes-ignores-non-id-links ()
  "Test that non-id incoming links don't count."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "file" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B")

    ;; A has a file link to B, but only id links count.
    ;; So both A and B are orphans.
    (let ((orphans (vulpea-db-query-orphan-notes)))
      (should (= (length orphans) 2)))))

;;; Isolated Note Detection Tests

(ert-deftest vulpea-db-query-isolated-notes-found ()
  "Test finding notes with no incoming or outgoing links."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B")
    (vulpea-test--insert-test-note "note-c" "Note C")

    ;; A has outgoing (not isolated), B has incoming (not isolated),
    ;; C has nothing (isolated).
    (let ((isolated (vulpea-db-query-isolated-notes)))
      (should (= (length isolated) 1))
      (should (equal (vulpea-note-id (car isolated)) "note-c")))))

(ert-deftest vulpea-db-query-isolated-notes-none ()
  "Test no isolated notes when all are connected."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A"
                                   :links '((:dest "note-b" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "note-b" "Note B"
                                   :links '((:dest "note-a" :type "id" :pos 100)))

    (should-not (vulpea-db-query-isolated-notes))))

(ert-deftest vulpea-db-query-isolated-notes-all ()
  "Test all notes isolated when no links exist."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A")
    (vulpea-test--insert-test-note "note-b" "Note B")

    (let ((isolated (vulpea-db-query-isolated-notes)))
      (should (= (length isolated) 2)))))

(ert-deftest vulpea-db-query-isolated-notes-empty-db ()
  "Test isolated notes on empty database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should-not (vulpea-db-query-isolated-notes))))

;;; Title Collision Detection Tests

(ert-deftest vulpea-db-query-title-collisions-found ()
  "Test finding notes with duplicate titles."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "wine-1" "Wine")
    (vulpea-test--insert-test-note "wine-2" "Wine")
    (vulpea-test--insert-test-note "beer-1" "Beer")

    (let ((collisions (vulpea-db-query-title-collisions)))
      (should (= (length collisions) 1))
      (should (equal (car (car collisions)) "Wine"))
      (should (= (length (cdr (car collisions))) 2))
      (let ((ids (mapcar #'vulpea-note-id (cdr (car collisions)))))
        (should (member "wine-1" ids))
        (should (member "wine-2" ids))))))

(ert-deftest vulpea-db-query-title-collisions-none ()
  "Test no collisions when all titles are unique."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note-a" "Note A")
    (vulpea-test--insert-test-note "note-b" "Note B")
    (vulpea-test--insert-test-note "note-c" "Note C")

    (should-not (vulpea-db-query-title-collisions))))

(ert-deftest vulpea-db-query-title-collisions-multiple-groups ()
  "Test multiple collision groups."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "wine-1" "Wine")
    (vulpea-test--insert-test-note "wine-2" "Wine")
    (vulpea-test--insert-test-note "beer-1" "Beer")
    (vulpea-test--insert-test-note "beer-2" "Beer")
    (vulpea-test--insert-test-note "unique" "Unique")

    (let ((collisions (vulpea-db-query-title-collisions)))
      (should (= (length collisions) 2))
      (let ((titles (mapcar #'car collisions)))
        (should (member "Wine" titles))
        (should (member "Beer" titles))))))

(ert-deftest vulpea-db-query-title-collisions-empty-db ()
  "Test title collisions on empty database."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should-not (vulpea-db-query-title-collisions))))

(ert-deftest vulpea-db-query-title-collisions-three-duplicates ()
  "Test collision group with more than two notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "a" "Same Title")
    (vulpea-test--insert-test-note "b" "Same Title")
    (vulpea-test--insert-test-note "c" "Same Title")

    (let ((collisions (vulpea-db-query-title-collisions)))
      (should (= (length collisions) 1))
      (should (= (length (cdr (car collisions))) 3)))))

(ert-deftest vulpea-db-query-title-collisions-file-level-only ()
  "Test filtering collisions to file-level notes only."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Two file-level notes with same title
    (vulpea-test--insert-test-note "file-1" "Wine" :level 0)
    (vulpea-test--insert-test-note "file-2" "Wine" :level 0)
    ;; Heading-level note with same title — should be excluded
    (vulpea-test--insert-test-note "heading-1" "Wine" :level 1)

    (let ((collisions (vulpea-db-query-title-collisions 0)))
      (should (= (length collisions) 1))
      (should (= (length (cdr (car collisions))) 2))
      (let ((ids (mapcar #'vulpea-note-id (cdr (car collisions)))))
        (should (member "file-1" ids))
        (should (member "file-2" ids))
        (should-not (member "heading-1" ids))))))

(ert-deftest vulpea-db-query-title-collisions-heading-level-only ()
  "Test filtering collisions to heading-level notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "file-1" "Wine" :level 0)
    (vulpea-test--insert-test-note "heading-1" "Wine" :level 1)
    (vulpea-test--insert-test-note "heading-2" "Wine" :level 1)

    ;; Only heading-level collision
    (let ((collisions (vulpea-db-query-title-collisions 1)))
      (should (= (length collisions) 1))
      (should (= (length (cdr (car collisions))) 2)))
    ;; No file-level collision (only one file-level "Wine")
    (should-not (vulpea-db-query-title-collisions 0))))

(ert-deftest vulpea-db-query-title-collisions-no-level-includes-all ()
  "Test that nil level includes all notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "file-1" "Wine" :level 0)
    (vulpea-test--insert-test-note "heading-1" "Wine" :level 1)

    ;; Without level filter, file + heading collision counts
    (let ((collisions (vulpea-db-query-title-collisions)))
      (should (= (length collisions) 1))
      (should (= (length (cdr (car collisions))) 2)))))

(provide 'vulpea-db-query-test)
;;; vulpea-db-query-test.el ends here
