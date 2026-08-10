;;; vulpea-select-test.el --- Tests for vulpea-select -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
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
                               (seq-find
                                (lambda (cand)
                                  (string-match-p "Reference" cand))
                                (all-completions "" collection)))
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
                       (seq-find
                        (lambda (cand)
                          (string-match-p "First Note" cand))
                        (all-completions "" collection)))))
            (vulpea-select-from "Note" notes))))

    (should result)
    (should (vulpea-note-p result))
    (should (equal (vulpea-note-id result) "id1"))
    (should (equal (vulpea-note-title result) "First Note"))))

(ert-deftest vulpea-select-describe-with-matchable-annotations ()
  "Test `vulpea-select-describe' formats note for completion.

Consider the default value of `vulpea-select-annotate-matchable', which
is t. Tags and then included in the matchable string."
  (let* ((context "CTX")
         (note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (described (vulpea-select-describe note context)))

    ;; Should contain title
    (should (string-match-p "Test Note" described))
    ;; Should contain tags
    (should (string-match-p "#tag1" described))
    (should (string-match-p "#tag2" described))
    ;; Should have id property
    (should (equal (get-text-property 0 'vulpea-note-id described) "test-id"))

    ;; Should have vulpea-note property
    (should (equal (get-text-property 0 'vulpea-note described) note))

    ;; Should have vulpea-select-context property
    (should (equal (get-text-property 0 'vulpea-select-context described) context))))

(ert-deftest vulpea-select-describe-without-matchable-annotations ()
  "Test `vulpea-select-describe' formats note for completion.

Consider the case where `vulpea-select-annotate-matchable' is nil. Tags
are not included in the matchable string."
  (let* ((vulpea-select-annotate-matchable nil)
         (context "CTX")
         (note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (described (vulpea-select-describe note context)))

    ;; Should contain title
    (should (string-match-p "Test Note" described))

    ;; Should not contain tags
    (should (not (string-match-p "#tag1" described)))
    (should (not (string-match-p "#tag2" described)))

    ;; Should have id property
    (should (equal (get-text-property 0 'vulpea-note-id described) "test-id"))

    ;; Should have vulpea-note property
    (should (equal (get-text-property 0 'vulpea-note described) note))

    ;; Should have vulpea-select-context property
    (should (equal (get-text-property 0 'vulpea-select-context described) context))))

(ert-deftest vulpea-select-describe-with-nil-annotate-fn ()
  "Test `vulpea-select-describe' with `vulpea-select-annotate-fn' set to nil.

A nil annotate function means no annotation: the candidate carries
only the describe part, and building it does not error."
  (let* ((vulpea-select-annotate-fn nil)
         (note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (described (vulpea-select-describe note)))

    ;; Should contain title
    (should (string-match-p "Test Note" described))

    ;; Should not contain tags
    (should (not (string-match-p "#tag1" described)))
    (should (not (string-match-p "#tag2" described)))

    ;; Should still carry the note
    (should (equal (get-text-property 0 'vulpea-note described) note))))

(ert-deftest vulpea-select-describe-id-is-matchable-and-invisible ()
  "Test that the id is part of the candidate string but hidden.

The id must be in the string content so completion can match it,
and carry the `invisible' property so it is not displayed."
  (let* ((note (make-vulpea-note
                :id "person:lectia"
                :title "Test Note"
                :level 0))
         (described (vulpea-select-describe note))
         (idx (string-match (regexp-quote "person:lectia")
                            (substring-no-properties described))))
    ;; the id is part of the matchable string content
    (should idx)
    ;; and that portion is invisible
    (should (get-text-property idx 'invisible described))
    ;; while the visible title is not invisible
    (should-not (get-text-property 0 'invisible described))))

(ert-deftest vulpea-select-describe-id-matching-can-be-disabled ()
  "Test that `vulpea-select-match-ids' nil drops the id from the string."
  (let* ((vulpea-select-match-ids nil)
         (note (make-vulpea-note
                :id "person:lectia"
                :title "Test Note"
                :level 0))
         (described (substring-no-properties (vulpea-select-describe note))))
    (should (string-match-p "Test Note" described))
    (should-not (string-match-p (regexp-quote "person:lectia") described))))

;;; Candidate Accessor Tests

(ert-deftest vulpea-select-candidate-note-returns-note ()
  "The note is recoverable from a candidate string via the accessor."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0))
         (candidate (vulpea-select-describe note)))
    (should (eq (vulpea-select-candidate-note candidate) note))))

(ert-deftest vulpea-select-candidate-context-returns-context ()
  "The dyncontext value is recoverable from a candidate string."
  (let* ((context '(:counts (1 2 3)))
         (note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0))
         (candidate (vulpea-select-describe note context)))
    (should (eq (vulpea-select-candidate-context candidate) context))))

(ert-deftest vulpea-select-candidate-accessors-nil-on-plain-string ()
  "A string that is not a candidate yields nil, including the empty string."
  (should-not (vulpea-select-candidate-note "just some text"))
  (should-not (vulpea-select-candidate-context "just some text"))
  (should-not (vulpea-select-candidate-note ""))
  (should-not (vulpea-select-candidate-context "")))

(ert-deftest vulpea-select-candidate-note-survives-copy ()
  "The accessor works on a copy of the candidate string.

Completion styles copy and re-propertize candidate strings; text
properties survive `copy-sequence' and `substring', so the note must
stay reachable on such copies."
  (let* ((note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0))
         (candidate (vulpea-select-describe note)))
    (should (eq (vulpea-select-candidate-note (copy-sequence candidate)) note))
    (should (eq (vulpea-select-candidate-note (substring candidate)) note))))

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

(ert-deftest vulpea-select--create-annotate-wrapper ()
  "Test that `vulpea-select--create-annotate-wrapper' correctly wraps an annotation function like `vulpea-select-annotate'.

The wrapped function can be called with a completion candidate (which
has the note as a text property) and returns the same annotation string
as the original annotation function (called with the note object)."
  (let* ((context "CTX")
         (note (make-vulpea-note
                :id "test-id"
                :title "Test Note"
                :level 0
                :tags '("tag1" "tag2")))
         (completion-candidate (vulpea-select-describe note context)))

    ;; Case where the annotation function takes one argument (note)
    (let* ((annotation-fn 'vulpea-select-annotate)
           (wrapped-annotate-fn (vulpea-select--create-annotate-wrapper annotation-fn))
           (orig-annotation (funcall annotation-fn note))
           (annotation (funcall wrapped-annotate-fn completion-candidate)))

      ;; Both annotations should be equal
      (should (equal orig-annotation annotation))

      ;; Check that wrapper returns empty string for non-candidate input
      (should (equal (funcall wrapped-annotate-fn "not-a-candidate") "")))

    ;; Case where the annotation function takes two arguments (note and context)
    (let* ((annotation-fn (lambda (n ctx) (format "%s (%s)" (vulpea-select-annotate n) ctx)))
           (wrapped-annotate-fn (vulpea-select--create-annotate-wrapper annotation-fn))
           (orig-annotation (funcall annotation-fn note context))
           (annotation (funcall wrapped-annotate-fn completion-candidate)))

      ;; Both annotations should be equal
      (should (equal orig-annotation annotation))

      ;; Check that wrapper returns empty string for non-candidate input
      (should (equal (funcall wrapped-annotate-fn "not-a-candidate") "")))))

;;; Dynamic Context Tests

(ert-deftest vulpea-select-describe-passes-context-to-2-arg-describe-fn ()
  "A describe function accepting two arguments receives the context."
  (let* ((received 'unset)
         (vulpea-select-describe-fn
          (lambda (note ctx) (setq received ctx) (vulpea-note-title note)))
         (vulpea-select-annotate-fn (lambda (_note) ""))
         (note (make-vulpea-note :id "x" :title "Title" :level 0)))
    (vulpea-select-describe note "CTX")
    (should (equal received "CTX"))))

(ert-deftest vulpea-select-describe-passes-context-to-2-arg-annotate-fn ()
  "An annotate function accepting two arguments receives the context."
  (let* ((received 'unset)
         (vulpea-select-describe-fn (lambda (note) (vulpea-note-title note)))
         (vulpea-select-annotate-fn
          (lambda (_note ctx) (setq received ctx) ""))
         (note (make-vulpea-note :id "x" :title "Title" :level 0)))
    (vulpea-select-describe note "CTX")
    (should (equal received "CTX"))))

(ert-deftest vulpea-select-describe-backward-compatible-with-1-arg-fn ()
  "Passing a context must not break describe/annotate functions taking one arg."
  ;; This checks describe-fn plumbing, not the id suffix.
  (let* ((vulpea-select-match-ids nil)
         (vulpea-select-describe-fn (lambda (note) (vulpea-note-title note)))
         (vulpea-select-annotate-fn (lambda (_note) ""))
         (note (make-vulpea-note :id "x" :title "Title" :level 0)))
    (should (equal (substring-no-properties (vulpea-select-describe note "CTX"))
                   "Title"))))

(ert-deftest vulpea-select-describe-context-defaults-to-nil ()
  "When called without a context, a two-arg describe function gets nil."
  (let* ((received 'unset)
         (vulpea-select-describe-fn
          (lambda (note ctx) (setq received ctx) (vulpea-note-title note)))
         (vulpea-select-annotate-fn (lambda (_note) ""))
         (note (make-vulpea-note :id "x" :title "Title" :level 0)))
    (vulpea-select-describe note)
    (should (null received))))

(ert-deftest vulpea-select-from-builds-context-once-with-notes ()
  "`vulpea-select-dyncontext-fn' is called once per selection with the notes."
  (let* ((calls 0)
         (seen-notes nil)
         (note1 (make-vulpea-note :id "id1" :title "First" :level 0))
         (note2 (make-vulpea-note :id "id2" :title "Second" :level 0))
         (notes (list note1 note2))
         (vulpea-select-dyncontext-fn
          (lambda (ns) (setq calls (1+ calls) seen-notes ns) "CTX"))
         (context-in-describe 'unset)
         (vulpea-select-describe-fn
          (lambda (note ctx)
            (setq context-in-describe ctx)
            (vulpea-note-title note)))
         (vulpea-select-annotate-fn (lambda (_note) "")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (seq-find (lambda (cand) (string-match-p "First" cand))
                           (all-completions "" collection)))))
      (vulpea-select-from "Note" notes))
    ;; computed exactly once, over the presented notes, and threaded through
    (should (= calls 1))
    (should (equal seen-notes notes))
    (should (equal context-in-describe "CTX"))))

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
                       (setq completions-seen (all-completions "" collection))
                       ;; Select the alias
                       (seq-find
                        (lambda (cand)
                          (string-match-p "Alias1" cand))
                        completions-seen))))
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
                 (setq completions-seen (all-completions "" collection))
                 (car completions-seen))))
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
                       (setq completions-seen (all-completions "" collection))
                       ;; Select the alias
                       (seq-find
                        (lambda (cand)
                          (string-match-p "Alias1" cand))
                        completions-seen))))
            (vulpea-select "Note" :expand-aliases t))))

    ;; Should have 2 completions: Original + Alias
    (should (= (length completions-seen) 2))

    ;; Result should have alias as title
    (should (equal (vulpea-note-title result) "Alias1"))
    (should (equal (vulpea-note-primary-title result) "Original Title"))))

(ert-deftest vulpea-select-completion-table-exposes-category ()
  "Test that the completion table reports the `vulpea-note' category."
  ;; Assert the plain candidate; the id suffix has its own tests.
  (let* ((vulpea-select-match-ids nil)
         (note (make-vulpea-note :id "id1" :title "One" :level 0))
         (table (vulpea-select--completion-table
                 (list (cons (vulpea-select-describe note) note)))))
    ;; metadata reports the vulpea-note category
    (should (eq (completion-metadata-get
                 (completion-metadata "" table nil)
                 'category)
                'vulpea-note))
    ;; and the table still completes against the candidates
    (should (member "One" (all-completions "" table)))))

(ert-deftest vulpea-select-completion-table-exposes-annotation-function ()
  "Test that the completion table reports the annotation function."
  ;; Assert the plain candidate; the id suffix has its own tests.
  (let* ((vulpea-select-match-ids nil)
         (note (make-vulpea-note :id "id1" :title "One" :level 0))
         (table (vulpea-select--completion-table
                 (list (cons (vulpea-select-describe note) note)))))

    ;; metadata does not include annotation function, since the
    ;; default value of `vulpea-select-annotate-matchable' is t, which
    ;; makes annotations part of the matchable string
    (should (null (completion-metadata-get
                   (completion-metadata "" table nil)
                   'annotation-function)))

    (let ((vulpea-select-annotate-matchable nil))
      ;; metadata includes the annotation function when
      ;; `vulpea-select-annotate-matchable' is nil
      (should (not (null (completion-metadata-get
                          (completion-metadata "" table nil)
                          'annotation-function)))))

    (let ((vulpea-select-annotate-matchable nil)
          (vulpea-select-annotate-fn nil))
      ;; a nil annotate function means no annotation, so no
      ;; annotation-function is exposed either
      (should (null (completion-metadata-get
                     (completion-metadata "" table nil)
                     'annotation-function))))))

(ert-deftest vulpea-select-from-exposes-category ()
  "Test that vulpea-select-from gives completing-read the `vulpea-note' category."
  (let* ((note (make-vulpea-note :id "id1" :title "One" :level 0))
         (seen-category nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen-category
                       (completion-metadata-get
                        (completion-metadata "" collection nil)
                        'category))
                 ;; simulate selecting the only candidate
                 (car (all-completions "" collection)))))
      (vulpea-select-from "Note" (list note)))
    (should (eq seen-category 'vulpea-note))))

(provide 'vulpea-select-test)
;;; vulpea-select-test.el ends here
