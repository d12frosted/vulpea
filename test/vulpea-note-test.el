;;; vulpea-note-test.el --- Tests for vulpea-note -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
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

;;; Meta Tests

(ert-deftest vulpea-note-meta-get-string ()
  "Test getting string metadata."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("country" . ((:value "France" :type "string")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "country") "France"))
      (should (equal (vulpea-note-meta-get note "country" 'string) "France")))))

(ert-deftest vulpea-note-meta-get-number ()
  "Test getting number metadata with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("price" . ((:value "45.99" :type "number")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "price" 'number) 45.99)))))

(ert-deftest vulpea-note-meta-get-symbol ()
  "Test getting symbol metadata with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("status" . ((:value "active" :type "symbol")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "status" 'symbol) 'active)))))

(ert-deftest vulpea-note-meta-get-link ()
  "Test getting link metadata."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("url" . ((:value "https://example.com" :type "link")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "url" 'link) "https://example.com")))))

(ert-deftest vulpea-note-meta-get-link-id ()
  "Test getting ID link metadata extracts ID."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("ref" . ((:value "[[id:target-id][Target]]" :type "link")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "ref" 'link) "target-id")))))

(ert-deftest vulpea-note-meta-get-note ()
  "Test getting note metadata resolves to vulpea-note."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target Note")
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("producer" . ((:value "[[id:target][Target Note]]" :type "note")))))

    (let* ((note (vulpea-db-get-by-id "note1"))
           (producer (vulpea-note-meta-get note "producer" 'note)))
      (should (vulpea-note-p producer))
      (should (equal (vulpea-note-id producer) "target"))
      (should (equal (vulpea-note-title producer) "Target Note")))))

(ert-deftest vulpea-note-meta-get-list-multiple ()
  "Test getting list of metadata values."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("grapes" . ((:value "Cabernet Sauvignon" :type "string")
                                                        (:value "Merlot" :type "string")
                                                        (:value "Syrah" :type "string")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get-list note "grapes")
                     '("Cabernet Sauvignon" "Merlot" "Syrah"))))))

(ert-deftest vulpea-note-meta-get-list-numbers ()
  "Test getting list of numbers with type coercion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("ratings" . ((:value "95" :type "number")
                                                         (:value "92" :type "number")
                                                         (:value "88" :type "number")))))

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
                                   :meta '(("grapes" . ((:value "[[id:grape1][Cabernet Sauvignon]]" :type "note")
                                                        (:value "[[id:grape2][Merlot]]" :type "note")
                                                        (:value "[[id:grape3][Syrah]]" :type "note")))))

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
                                   :meta '(("ref" . ((:value "[[id:target][Alias1]]" :type "note")))))

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
                                   :meta '(("tags" . ((:value "first" :type "string")
                                                      (:value "second" :type "string")
                                                      (:value "third" :type "string")))))

    (let ((note (vulpea-db-get-by-id "note1")))
      (should (equal (vulpea-note-meta-get note "tags") "first")))))

(ert-deftest vulpea-note-meta-get-missing ()
  "Test getting non-existent metadata returns nil."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "note1" "Test Note"
                                   :meta '(("country" . ((:value "France" :type "string")))))

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

(provide 'vulpea-note-test)
;;; vulpea-note-test.el ends here
