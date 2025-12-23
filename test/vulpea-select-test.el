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
(require 'vulpea-test-helpers)

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

;;; Describe Outline Tests

(ert-deftest vulpea-select-describe-outline-file-level ()
  "Test describe-outline shows just title for file-level notes."
  (let* ((note (make-vulpea-note
                :id "file-id"
                :title "File Title"
                :level 0
                :outline-path nil))
         (description (vulpea-select-describe-outline note)))
    ;; Should just be the title, no prefix
    (should (equal description "File Title"))))

(ert-deftest vulpea-select-describe-outline-heading ()
  "Test describe-outline shows outline path for heading notes."
  (let* ((note (make-vulpea-note
                :id "heading-id"
                :title "Task"
                :level 2
                :outline-path '("Projects" "Work")))
         (description (vulpea-select-describe-outline note)))
    ;; Should show outline path before title
    (should (string-match-p "Projects" description))
    (should (string-match-p "Work" description))
    (should (string-match-p "Task" description))
    ;; Path should come before title
    (should (< (string-match "Projects" description)
               (string-match "Task" description)))))

(ert-deftest vulpea-select-describe-outline-full-file-level ()
  "Test describe-outline-full shows just title for file-level notes."
  (let* ((note (make-vulpea-note
                :id "file-id"
                :title "File Title"
                :file-title "File Title"
                :level 0
                :outline-path nil))
         (description (vulpea-select-describe-outline-full note)))
    ;; Should just be the title, no prefix (file-title equals title)
    (should (equal description "File Title"))))

(ert-deftest vulpea-select-describe-outline-full-heading ()
  "Test describe-outline-full shows file title and outline path."
  (let* ((note (make-vulpea-note
                :id "heading-id"
                :title "Task"
                :file-title "My Notes"
                :level 2
                :outline-path '("Projects")))
         (description (vulpea-select-describe-outline-full note)))
    ;; Should show file title, outline path, and note title
    (should (string-match-p "My Notes" description))
    (should (string-match-p "Projects" description))
    (should (string-match-p "Task" description))
    ;; File title should come first
    (should (< (string-match "My Notes" description)
               (string-match "Projects" description)))
    ;; Outline path should come before note title
    (should (< (string-match "Projects" description)
               (string-match "Task" description)))))

(ert-deftest vulpea-select-describe-outline-full-direct-child ()
  "Test describe-outline-full for heading directly under file."
  (let* ((note (make-vulpea-note
                :id "heading-id"
                :title "First Heading"
                :file-title "Parent File"
                :level 1
                :outline-path nil))
         (description (vulpea-select-describe-outline-full note)))
    ;; Should show file title and note title (no outline-path for level 1)
    (should (string-match-p "Parent File" description))
    (should (string-match-p "First Heading" description))
    ;; File title should come first
    (should (< (string-match "Parent File" description)
               (string-match "First Heading" description)))))

;;; Expand Aliases in Selection Tests

(ert-deftest vulpea-select-annotate-with-primary-title ()
  "Test vulpea-select-annotate shows primary title for alias notes."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Alias Name"
                :primary-title "Original Title"
                :level 0
                :tags '("tag1")))
         (annotation (vulpea-select-annotate note)))
    ;; Should contain primary title in parentheses
    (should (string-match-p "(Original Title)" annotation))
    ;; Should contain tags
    (should (string-match-p "#tag1" annotation))))

(ert-deftest vulpea-select-from-expand-aliases ()
  "Test vulpea-select-from expands aliases when requested."
  (let* ((note1 (make-vulpea-note
                 :id "id1"
                 :title "Original Title"
                 :aliases '("Alias1" "Alias2")
                 :level 0))
         (note2 (make-vulpea-note
                 :id "id2"
                 :title "Other Note"
                 :level 0))
         (notes (list note1 note2))
         (completions-seen nil)
         (result
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq completions-seen collection)
                       ;; Select the alias
                       (car (seq-find
                             (lambda (entry)
                               (string-match-p "Alias1" (car entry)))
                             collection)))))
            (vulpea-select-from "Note" notes :expand-aliases t))))

    ;; Should have 4 completions: Original + 2 aliases + Other Note
    (should (= (length completions-seen) 4))

    ;; Result should have alias as title and original as primary-title
    (should (equal (vulpea-note-title result) "Alias1"))
    (should (equal (vulpea-note-primary-title result) "Original Title"))
    (should (equal (vulpea-note-id result) "id1"))))

(ert-deftest vulpea-select-from-no-expand-aliases-by-default ()
  "Test vulpea-select-from does not expand aliases by default."
  (let* ((note (make-vulpea-note
                :id "id1"
                :title "Original Title"
                :aliases '("Alias1" "Alias2")
                :level 0))
         (notes (list note))
         (completions-seen nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq completions-seen collection)
                 (caar collection))))
      (vulpea-select-from "Note" notes))

    ;; Should have only 1 completion (no expansion)
    (should (= (length completions-seen) 1))))

(ert-deftest vulpea-select-expand-aliases ()
  "Test vulpea-select passes expand-aliases to vulpea-select-from."
  ;; Test that vulpea-select correctly passes :expand-aliases by mocking
  ;; vulpea-db-query to return a note with aliases
  (let* ((test-note (make-vulpea-note
                     :id "test-id"
                     :title "Original Title"
                     :aliases '("Alias1")
                     :level 0))
         (completions-seen nil)
         (result
          (cl-letf (((symbol-function 'vulpea-db-query)
                     (lambda (_filter-fn)
                       (list test-note)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt collection &rest _)
                       (setq completions-seen collection)
                       ;; Select the alias
                       (car (seq-find
                             (lambda (entry)
                               (string-match-p "Alias1" (car entry)))
                             collection)))))
            (vulpea-select "Note" :expand-aliases t))))

    ;; Should have 2 completions: Original + Alias
    (should (= (length completions-seen) 2))

    ;; Result should have alias as title
    (should (equal (vulpea-note-title result) "Alias1"))
    (should (equal (vulpea-note-primary-title result) "Original Title"))))

(provide 'vulpea-select-test)
;;; vulpea-select-test.el ends here
