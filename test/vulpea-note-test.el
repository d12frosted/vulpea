;;; vulpea-note-test.el --- Tests for vulpea-note -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for vulpea-note.el functions
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-note)
(require 'vulpea-test-helpers)

;;; Meta Tests

(ert-deftest vulpea-note-meta-get-string ()
  "Test getting string metadata."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("country" . ("France"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "country") "France"))
      (should (equal (vulpea-note-meta-get note "country" 'string) "France")))))

(ert-deftest vulpea-note-meta-get-number ()
  "Test getting number metadata with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("price" . ("45.99"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "price" 'number) 45.99)))))

(ert-deftest vulpea-note-meta-get-symbol ()
  "Test getting symbol metadata with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("status" . ("active"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "status" 'symbol) 'active)))))

(ert-deftest vulpea-note-meta-get-link ()
  "Test getting link metadata."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("url" . ("https://example.com"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "url" 'link) "https://example.com")))))

(ert-deftest vulpea-note-meta-get-link-id ()
  "Test getting ID link metadata extracts ID."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("ref" . ("[[id:target-id][Target]]"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "ref" 'link) "target-id")))))

(ert-deftest vulpea-note-meta-get-note ()
  "Test getting note metadata resolves to vulpea-note."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target Note")
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("producer" . ("[[id:target][Target Note]]"))))

    (let* ((note (vulpea-db-get-by-id "note1"))
           (producer (vulpea-note-meta-get note "producer" 'note)))
      (should (vulpea-note-p producer))
      (should (equal (vulpea-note-id producer) "target"))
      (should (equal (vulpea-note-title producer) "Target Note")))))

(ert-deftest vulpea-note-meta-get-note-with-alias ()
  "Test getting note referenced via alias preserves alias information."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Full Title"
                                   :aliases '("ShortName" "AnotherAlias"))
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("producer" . ("[[id:target][ShortName]]"))))

    (let* ((note (vulpea-db-get-by-id "note1"))
           (producer (vulpea-note-meta-get note "producer" 'note)))
      (should (vulpea-note-p producer))
      (should (equal (vulpea-note-id producer) "target"))
      ;; When referenced via alias, title should be alias and primary-title should be original
      (should (equal (vulpea-note-title producer) "ShortName"))
      (should (equal (vulpea-note-primary-title producer) "Full Title")))))

(ert-deftest vulpea-note-meta-get-list-multiple ()
  "Test getting list of metadata values."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("grapes" . ("Cabernet Sauvignon"
                                                        "Merlot"
                                                        "Syrah"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get-list note "grapes")
                     '("Cabernet Sauvignon" "Merlot" "Syrah"))))))

(ert-deftest vulpea-note-meta-get-list-numbers ()
  "Test getting list of numbers with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("ratings" . ("95"
                                                         "92"
                                                         "88"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get-list note "ratings" 'number)
                     '(95 92 88))))))

(ert-deftest vulpea-note-meta-get-list-notes ()
  "Test getting list of notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "grape1" "Cabernet Sauvignon")
    (vulpea-test--insert-test-note "grape2" "Merlot")
    (vulpea-test--insert-test-note "grape3" "Syrah")
    (vulpea-test--insert-test-note "wine" "Wine Note"
                                   :meta '(("grapes" . ("[[id:grape1][Cabernet Sauvignon]]"
                                                        "[[id:grape2][Merlot]]"
                                                        "[[id:grape3][Syrah]]"))))

    (let* ((note (vulpea-db-get-by-id "wine"))
           (grapes (vulpea-note-meta-get-list note "grapes" 'note)))
      (should (= (length grapes) 3))
      (should (cl-every #'vulpea-note-p grapes))
      (should (equal (mapcar #'vulpea-note-id grapes)
                     '("grape1" "grape2" "grape3")))
      (should (equal (mapcar #'vulpea-note-title grapes)
                     '("Cabernet Sauvignon" "Merlot" "Syrah"))))))

(ert-deftest vulpea-note-meta-get-list-notes-with-aliases ()
  "Test that linked notes preserve alias information."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Full Title"
                                   :aliases '("Alias1" "Alias2"))
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("ref" . ("[[id:target][Alias1]]"))))

    (let* ((note (vulpea-db-get-by-id "note1"))
           (refs (vulpea-note-meta-get-list note "ref" 'note))
           (ref (car refs)))
      (should (= (length refs) 1))
      (should (equal (vulpea-note-id ref) "target"))
      ;; When referenced via alias, title should be alias and primary-title should be original
      (should (equal (vulpea-note-title ref) "Alias1"))
      (should (equal (vulpea-note-primary-title ref) "Full Title")))))

(ert-deftest vulpea-note-meta-get-first-value ()
  "Test that meta-get returns only first value when multiple exist."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("tags" . ("first"
                                                      "second"
                                                      "third"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "tags") "first")))))

(ert-deftest vulpea-note-meta-get-missing ()
  "Test getting non-existent metadata returns nil."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("country" . ("France"))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should-not (vulpea-note-meta-get note "nonexistent")))))

(ert-deftest vulpea-note-meta-get-empty-list ()
  "Test getting metadata from note with no meta returns nil."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note")

    (let ((note (vulpea-db-get-by-id "note1")))
      (should-not (vulpea-note-meta-get note "anything"))
      (should-not (vulpea-note-meta-get-list note "anything")))))

;;; Predicate Tests

(ert-deftest vulpea-note-tagged-all-p ()
  "Test checking if note has all specified tags."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :tags '("wine" "red" "france"))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (vulpea-note-tagged-all-p note "wine" "red"))
      (should (vulpea-note-tagged-all-p note "wine" "red" "france"))
      (should-not (vulpea-note-tagged-all-p note "wine" "white")))))

(ert-deftest vulpea-note-tagged-any-p ()
  "Test checking if note has any specified tags."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :tags '("wine" "red"))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (vulpea-note-tagged-any-p note "wine"))
      (should (vulpea-note-tagged-any-p note "beer" "wine"))
      (should-not (vulpea-note-tagged-any-p note "beer" "white")))))

(ert-deftest vulpea-note-links-to-all-p ()
  "Test checking if note links to all specified targets."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :links '((:dest "target1" :type "id")
                                           (:dest "target2" :type "id")
                                           (:dest "target3" :type "id")))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (vulpea-note-links-to-all-p note "target1" "target2"))
      (should (vulpea-note-links-to-all-p note "target1" "target2" "target3"))
      (should-not (vulpea-note-links-to-all-p note "target1" "target4")))))

(ert-deftest vulpea-note-links-to-any-p ()
  "Test checking if note links to any specified target."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :links '((:dest "target1" :type "id")
                                           (:dest "target2" :type "id")))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (vulpea-note-links-to-any-p note "target1"))
      (should (vulpea-note-links-to-any-p note "target3" "target1"))
      (should-not (vulpea-note-links-to-any-p note "target3" "target4")))))

;;; Expand Aliases Tests

(ert-deftest vulpea-note-expand-aliases-no-aliases ()
  "Test expanding note with no aliases returns single note."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Original Title"
                :level 0
                :tags '("tag1")))
         (expanded (vulpea-note-expand-aliases note)))
    (should (= (length expanded) 1))
    (should (eq (car expanded) note))
    (should (equal (vulpea-note-title (car expanded)) "Original Title"))
    (should-not (vulpea-note-primary-title (car expanded)))))

(ert-deftest vulpea-note-expand-aliases-with-aliases ()
  "Test expanding note with aliases returns note per name."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Original Title"
                :aliases '("Alias1" "Alias2")
                :level 0
                :tags '("tag1")))
         (expanded (vulpea-note-expand-aliases note)))
    ;; Should have 3 notes: original + 2 aliases
    (should (= (length expanded) 3))

    ;; First note is the original (same object)
    (should (eq (car expanded) note))
    (should (equal (vulpea-note-title (car expanded)) "Original Title"))
    (should-not (vulpea-note-primary-title (car expanded)))

    ;; Second note has first alias as title
    (let ((alias1-note (nth 1 expanded)))
      (should (equal (vulpea-note-title alias1-note) "Alias1"))
      (should (equal (vulpea-note-primary-title alias1-note) "Original Title"))
      (should (equal (vulpea-note-id alias1-note) "test-id"))
      (should (equal (vulpea-note-tags alias1-note) '("tag1"))))

    ;; Third note has second alias as title
    (let ((alias2-note (nth 2 expanded)))
      (should (equal (vulpea-note-title alias2-note) "Alias2"))
      (should (equal (vulpea-note-primary-title alias2-note) "Original Title"))
      (should (equal (vulpea-note-id alias2-note) "test-id")))))

(ert-deftest vulpea-note-expand-aliases-preserves-all-fields ()
  "Test that expanded alias notes preserve all fields from original."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :path "/path/to/note.org"
                :title "Original Title"
                :aliases '("Alias")
                :level 2
                :pos 100
                :tags '("tag1" "tag2")
                :links '((:dest "other-id" :type "id"))
                :properties '(("CATEGORY" . "test"))
                :meta '(("key" . ("value")))
                :todo "TODO"
                :priority "A"
                :outline-path '("Parent")
                :attach-dir "/attach"
                :file-title "Parent File"))
         (expanded (vulpea-note-expand-aliases note))
         (alias-note (nth 1 expanded)))
    ;; All fields should be preserved
    (should (equal (vulpea-note-id alias-note) "test-id"))
    (should (equal (vulpea-note-path alias-note) "/path/to/note.org"))
    (should (equal (vulpea-note-level alias-note) 2))
    (should (equal (vulpea-note-pos alias-note) 100))
    (should (equal (vulpea-note-tags alias-note) '("tag1" "tag2")))
    (should (equal (vulpea-note-aliases alias-note) '("Alias")))
    (should (equal (vulpea-note-links alias-note) '((:dest "other-id" :type "id"))))
    (should (equal (vulpea-note-properties alias-note) '(("CATEGORY" . "test"))))
    (should (equal (vulpea-note-meta alias-note) '(("key" . ("value")))))
    (should (equal (vulpea-note-todo alias-note) "TODO"))
    (should (equal (vulpea-note-priority alias-note) "A"))
    (should (equal (vulpea-note-outline-path alias-note) '("Parent")))
    (should (equal (vulpea-note-attach-dir alias-note) "/attach"))
    (should (equal (vulpea-note-file-title alias-note) "Parent File"))
    ;; Title and primary-title should be swapped for alias
    (should (equal (vulpea-note-title alias-note) "Alias"))
    (should (equal (vulpea-note-primary-title alias-note) "Original Title"))))

(ert-deftest vulpea-note-expand-aliases-copies-are-independent ()
  "Test that alias notes are independent copies."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Original"
                :aliases '("Alias")
                :level 0))
         (expanded (vulpea-note-expand-aliases note))
         (alias-note (nth 1 expanded)))
    ;; Modifying alias note should not affect original
    (setf (vulpea-note-tags alias-note) '("new-tag"))
    (should-not (vulpea-note-tags note))
    (should-not (vulpea-note-tags (car expanded)))))

(provide 'vulpea-note-test)
;;; vulpea-note-test.el ends here
