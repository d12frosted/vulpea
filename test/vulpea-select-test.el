;;; vulpea-select-test.el --- Tests for vulpea-select -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2025 Boris Buliga
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
;; Test `vulpea-select' module (v2).
;;
;;; Code:

(require 'ert)
(require 'vulpea-select)
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

;;; Selection Tests

(ert-deftest vulpea-select-existing-note ()
  "Test vulpea-select returns complete information for existing note."
  (vulpea-test--with-temp-db
    (vulpea-db)

    ;; Insert test note
    (vulpea-test--insert-test-note "test-id-1" "Reference"
                                   :path "/tmp/reference.org"
                                   :tags '("tag1" "tag2" "tag3"))

    ;; Mock completing-read to select the note
    (let* ((selected-completion nil)
           (result
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt collection &rest _)
                         ;; Find the completion for "Reference"
                         (setq selected-completion
                               (car (seq-find
                                     (lambda (entry)
                                       (string-match-p "Reference" (car entry)))
                                     collection)))
                         selected-completion)))
              (vulpea-select "Note"))))

      ;; Verify the result
      (should result)
      (should (vulpea-note-p result))
      (should (equal (vulpea-note-id result) "test-id-1"))
      (should (equal (vulpea-note-title result) "Reference"))
      (should (equal (vulpea-note-tags result) '("tag1" "tag2" "tag3")))
      (should (equal (vulpea-note-path result) "/tmp/reference.org"))
      (should (equal (vulpea-note-level result) 0)))))

(ert-deftest vulpea-select-nonexistent-note ()
  "Test vulpea-select returns minimal info for non-existent note."
  (vulpea-test--with-temp-db
    (vulpea-db)

    ;; Insert some notes so completing-read has options
    (vulpea-test--insert-test-note "note1" "Note 1")
    (vulpea-test--insert-test-note "note2" "Note 2")

    ;; Mock completing-read to return a new title
    (let ((result
           (cl-letf (((symbol-function 'completing-read)
                      (lambda (&rest _) "Future")))
             (vulpea-select "Note"))))

      ;; Should return note with just title and level
      (should result)
      (should (vulpea-note-p result))
      (should (null (vulpea-note-id result)))
      (should (equal (vulpea-note-title result) "Future"))
      (should (equal (vulpea-note-level result) 0)))))

(ert-deftest vulpea-select-filter-fn-called-on-all ()
  "Test that FILTER-FN is called on each note."
  (vulpea-test--with-temp-db
    (vulpea-db)

    ;; Insert test notes
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("tag1"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("tag2"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("tag1"))

    ;; Count filter calls
    (let ((filter-count 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Note 1")))
        (vulpea-select "Note"
                       :filter-fn
                       (lambda (_note)
                         (setq filter-count (1+ filter-count))
                         t)))  ; Return t to include all notes

      ;; Filter should be called for all notes in database
      (should (= filter-count 3)))))

(ert-deftest vulpea-select-filter-fn-receives-note-structure ()
  "Test that FILTER-FN receives proper vulpea-note structures."
  (vulpea-test--with-temp-db
    (vulpea-db)

    ;; Insert test notes
    (vulpea-test--insert-test-note "note1" "Note 1" :tags '("tag1"))
    (vulpea-test--insert-test-note "note2" "Note 2" :tags '("tag2"))
    (vulpea-test--insert-test-note "note3" "Note 3" :tags '("tag1"))

    ;; Count notes with tag1
    (let ((tag1-count 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Note 1")))
        (vulpea-select "Note"
                       :filter-fn
                       (lambda (note)
                         ;; Verify it's a note structure
                         (should (vulpea-note-p note))
                         ;; Count notes with tag1
                         (when (member "tag1" (vulpea-note-tags note))
                           (setq tag1-count (1+ tag1-count)))
                         t)))

      ;; Should have found 2 notes with tag1
      (should (= tag1-count 2)))))

(ert-deftest vulpea-select-from-basic ()
  "Test vulpea-select-from with provided notes."
  (let* ((note1 (make-vulpea-note
                 :id "id1"
                 :title "First Note"
                 :level 0
                 :tags '("tag1")))
         (note2 (make-vulpea-note
                 :id "id2"
                 :title "Second Note"
                 :level 0
                 :tags '("tag2")))
         (notes (list note1 note2))
         (result
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       ;; Select first note
                       (car (seq-find
                             (lambda (entry)
                               (string-match-p "First Note" (car entry)))
                             collection)))))
            (vulpea-select-from "Note" notes))))

    (should result)
    (should (vulpea-note-p result))
    (should (equal (vulpea-note-id result) "id1"))
    (should (equal (vulpea-note-title result) "First Note"))))

(ert-deftest vulpea-select-describe-basic ()
  "Test vulpea-select-describe formats note for completion."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (described (vulpea-select-describe note)))

    ;; Should contain title
    (should (string-match-p "Test Note" described))
    ;; Should contain tags
    (should (string-match-p "#tag1" described))
    (should (string-match-p "#tag2" described))
    ;; Should have id property
    (should (equal (get-text-property 0 'vulpea-note-id described) "test-id"))))

(ert-deftest vulpea-select-annotate-with-tags ()
  "Test vulpea-select-annotate includes tags."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (annotation (vulpea-select-annotate note)))

    ;; Should contain tags
    (should (string-match-p "#tag1" annotation))
    (should (string-match-p "#tag2" annotation))))

(provide 'vulpea-select-test)
;;; vulpea-select-test.el ends here
