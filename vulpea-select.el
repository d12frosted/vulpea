;;; vulpea-select.el --- Note selection with completion -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga  <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; Created: 11 Jul 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functionality to select a note.
;;
;;; Code:

(require 'vulpea-utils)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-db-query)

(defgroup vulpea-select nil
  "Note selection and completion."
  :group 'vulpea)

(defcustom vulpea-select-describe-fn #'vulpea-note-title
  "Function to describe a note for completion.

Accepts a `vulpea-note'. Returns a `string'."
  :type 'function
  :group 'vulpea-select)

(defcustom vulpea-select-annotate-fn #'vulpea-select-annotate
  "Function to annotate a note for completion.

Accepts a `vulpea-note'. Returns a `string'.

The annotation is rendered in `completions-annotations', applied
underneath whatever faces the string already carries, so a section
that styles itself keeps its own face.

When nil, candidates are not annotated at all."
  :type '(choice (const :tag "No annotation" nil) function)
  :group 'vulpea-select)

(defcustom vulpea-select-match-ids t
  "When non-nil, note ids are matchable in selection completion.

Each candidate built by `vulpea-select-describe' carries the note
id as an invisible suffix: it is part of the candidate string, so
typing or pasting an id (or part of one) narrows completion to
that note, but it is hidden from display so it never clutters the
list. The id is kept in the matchable string itself rather than
shown only as an annotation, the same way tags and aliases are.

This makes ids interactive handles alongside titles in
`vulpea-find' and `vulpea-insert', which matters when ids are
structured and meaningful (person:lectia) while titles are
incidental or absent (see `vulpea-note-titled-p'). How much of an
id you must type to narrow depends on your `completion-styles', the
same as for tags and aliases: interior styles (substring, flex,
orderless) match an id anywhere in the candidate, while a strict
prefix style only matches from the start.

Set to nil to drop ids from matching, e.g. if opaque ids produce
surprising matches."
  :type 'boolean
  :group 'vulpea-select)


(defcustom vulpea-select-annotate-matchable t
  "If t, annotations are added directly in the candidate string.

When this is t, annotations from `vulpea-select-annotate-fn' are
concatenated to the candidate string. This has the advantage that
annotations can be matched during search.

When this is nil, annotations from `vulpea-select-annotate-fn' are added
through the `:annotation-function' property, which is a standard way to
add annotations for minibuffer completion. This has the advantage that
annotations are not part of the candidate string, which can be useful
for some integrations, such as add multiple annotators that can be
cycled using the marginalia package."
  :type 'boolean
  :group 'vulpea-select)


(defcustom vulpea-select-dyncontext-fn nil
  "Function computing a shared context for the current selection.

When non-nil, it is called once per selection with the list of
`vulpea-note' values being presented and may return any value - the
\"dynamic context\". That value is then passed as the second argument to
`vulpea-select-describe-fn' and `vulpea-select-annotate-fn', but only to
functions that accept a second argument.

The point is to compute expensive shared data once - for example a table
of backlink counts built with a single query (see
`vulpea-db-query-backlink-counts') - and reuse it across every candidate,
instead of recomputing it per candidate or capturing it from a wrapper
command. Describe and annotate functions that take only a NOTE argument
are unaffected."
  :type '(choice (const :tag "No shared context" nil) function)
  :group 'vulpea-select)

(defun vulpea-select--accepts-context-p (fn)
  "Return non-nil when FN can be called with a NOTE and a context argument."
  (let ((arity (func-arity fn)))
    (and (<= (car arity) 2)
         (or (eq (cdr arity) 'many)
             (>= (cdr arity) 2)))))

(defun vulpea-select--funcall (fn note context)
  "Call FN with NOTE, also passing CONTEXT when FN accepts a second argument."
  (if (vulpea-select--accepts-context-p fn)
      (funcall fn note context)
    (funcall fn note)))

(defun vulpea-select--annotation-face (annotation)
  "Return ANNOTATION with the annotation face applied under its own faces.

`completions-annotations' is applied as a base rather than across the
whole string, so a section that styles itself keeps its face while plain
text still looks like an annotation.  ANNOTATION is not modified."
  (let ((result (copy-sequence annotation)))
    (add-face-text-property 0 (length result) 'completions-annotations t result)
    result))

(defun vulpea-select-describe (note &optional context)
  "Describe a NOTE for completion.

CONTEXT is the optional shared value produced by
`vulpea-select-dyncontext-fn'. It is forwarded as the second argument to
`vulpea-select-describe-fn' and `vulpea-select-annotate-fn' when they
accept one.

When `vulpea-select-match-ids' is non-nil, the note id is appended
as an invisible, matchable suffix so an id can be typed or pasted
to narrow completion. The suffix is added here, around any custom
`vulpea-select-describe-fn', so it is present regardless of how
candidates are displayed.

The returned string carries text properties linking it back to its
data: `vulpea-note-id' (the id), `vulpea-note' (the NOTE itself) and
`vulpea-select-context' (the CONTEXT). Read them through
`vulpea-select-candidate-note' and `vulpea-select-candidate-context'
instead of `get-text-property'. They are what makes a candidate
string self-describing for code that only ever sees strings - a
`display-sort-function' in `completion-category-overrides', an
`annotation-function' added by an integration, an embark action."
  (let* ((id (vulpea-note-id note))
         (description-part
          (vulpea-select--funcall
           vulpea-select-describe-fn note context))
         (annotation-part
          (if (and vulpea-select-annotate-matchable
                   vulpea-select-annotate-fn)
              (vulpea-select--annotation-face
               (vulpea-select--funcall
                vulpea-select-annotate-fn note context))
            ""))
         (invisible-id-part
          (when (and vulpea-select-match-ids id)
            (propertize (concat " " id) 'invisible t))))
    (propertize (concat
                 description-part annotation-part invisible-id-part)
                'vulpea-note-id
                id
                'vulpea-note
                note
                'vulpea-select-context
                context)))

(defun vulpea-select-candidate-note (candidate)
  "Return the `vulpea-note' carried by CANDIDATE, or nil.

CANDIDATE is a completion candidate string built by
`vulpea-select-describe'. A string that is not such a candidate -
for example the user's free-form input naming a new note - yields
nil.

This is the supported way to get from a candidate string back to
its note in code that only receives strings, such as a
`display-sort-function' set through `completion-category-overrides'
for the `vulpea-note' category:

  (defun my-sort-by-created (candidates)
    (seq-sort-by
     (lambda (c)
       (or (vulpea-note-created-at (vulpea-select-candidate-note c))
           \"\"))
     #\\='string>
     candidates))"
  (get-text-property 0 'vulpea-note candidate))

(defun vulpea-select-candidate-context (candidate)
  "Return the dynamic context carried by CANDIDATE, or nil.

CANDIDATE is a completion candidate string built by
`vulpea-select-describe'. The context is the value produced by
`vulpea-select-dyncontext-fn' for the selection the candidate
belongs to; nil when the hook is unset or CANDIDATE is not a
candidate string. See `vulpea-select-candidate-note'."
  (get-text-property 0 'vulpea-select-context candidate))

(defun vulpea-select-annotate (note)
  "Annotate a NOTE for completion."
  (let* ((alias-str
          (if (vulpea-note-primary-title note)
              (concat "("
                      (vulpea-note-primary-title note)
                      ")")
            ""))
         (tags-str (mapconcat
                    (lambda (x) (concat "#" x))
                    (vulpea-note-tags note)
                    " "))
         (sections (seq-remove #'string-empty-p
                               (list alias-str
                                     tags-str))))
    (if (null sections)
        ""
      (concat " " (string-join sections " ")))))

(defun vulpea-select--create-annotate-wrapper (annotation-fn)
  "Return a wrapper function for ANNOTATION-FN.

Return a wrapper function that receives a candidate string and then
calls ANNOTATION-FN with the note and context extracted from the
candidate. The wrapper function is suitable for use as a completion
annotation."
  (lambda (candidate)
    (let ((note (vulpea-select-candidate-note candidate))
          (context (vulpea-select-candidate-context candidate)))
      (if note
          (vulpea-select--annotation-face
           (vulpea-select--funcall annotation-fn note context))
        ""))))

;;; Describe Functions

(defun vulpea-select-describe-outline (note)
  "Describe NOTE with outline path prefix.

Shows the parent heading hierarchy before the note title.
For example, if a note titled \"Task\" is under \"Projects > Work\",
this displays: \"Projects → Work → Task\".

File-level notes are displayed without prefix.

To use this function, set `vulpea-select-describe-fn':

  (setq vulpea-select-describe-fn
        #\\='vulpea-select-describe-outline)"
  (let ((outline-path (vulpea-note-outline-path note))
        (title (vulpea-note-title note)))
    (if outline-path
        (concat
         (propertize
          (concat (string-join outline-path " → ") " → ")
          'face 'completions-annotations)
         title)
      title)))

(defun vulpea-select-describe-outline-full (note)
  "Describe NOTE with file title and outline path prefix.

Shows the file title and parent heading hierarchy before the note title.
For example, if a note titled \"Task\" is in file \"My Notes\" under
heading \"Projects\", this displays: \"My Notes → Projects → Task\".

File-level notes show just the title (no prefix needed since
file-title equals the title).

To use this function, set `vulpea-select-describe-fn':

  (setq vulpea-select-describe-fn
        #\\='vulpea-select-describe-outline-full)"
  (let ((file-title (vulpea-note-file-title note))
        (outline-path (vulpea-note-outline-path note))
        (title (vulpea-note-title note))
        (level (vulpea-note-level note)))
    (if (and level (> level 0) file-title)
        (let ((full-path (cons file-title (or outline-path '()))))
          (concat
           (propertize
            (concat (string-join full-path " → ") " → ")
            'face 'completions-annotations)
           title))
      title)))

(cl-defun vulpea-select (prompt
                         &key
                         require-match
                         initial-prompt
                         filter-fn
                         expand-aliases)
  "Select a note.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'."
  (let ((notes (vulpea-db-query filter-fn)))
    (vulpea-select-from
     prompt notes
     :require-match require-match
     :initial-prompt initial-prompt
     :expand-aliases expand-aliases)))

(defun vulpea-select--completion-table (completions)
  "Build a completion table over COMPLETIONS exposing the `vulpea-note' category.

If `vulpea-select-annotate-matchable' is nil and
`vulpea-select-annotate-fn' is set, then `annotation-function' is also
included in the metadata.

COMPLETIONS is an alist of (description . note). The table completes
like COMPLETIONS and reports a completion category of `vulpea-note',
so that completion UIs and integrations (marginalia, embark, consult)
can recognize and act on the candidates, and so that users can target
the category from `completion-category-overrides' (e.g. to set a
`display-sort-function'). The candidate strings carry their note id,
the note itself and the dynamic context as text properties (see
`vulpea-select-describe'); read them with
`vulpea-select-candidate-note' and `vulpea-select-candidate-context'."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          (category . vulpea-note)
          ,@(when (and (not vulpea-select-annotate-matchable)
                       vulpea-select-annotate-fn)
              `((annotation-function
                 .
                 ,(vulpea-select--create-annotate-wrapper
                   vulpea-select-annotate-fn)))))

      (complete-with-action action completions string predicate))))

(cl-defun vulpea-select-from (prompt
                              notes
                              &key
                              require-match
                              initial-prompt
                              expand-aliases)
  "Select a note from the list of NOTES.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'."
  (let* ((expanded-notes (if expand-aliases
                             (seq-mapcat #'vulpea-note-expand-aliases notes)
                           notes))
         (context (when vulpea-select-dyncontext-fn
                    (funcall vulpea-select-dyncontext-fn expanded-notes)))
         (completions (seq-map
                       (lambda (n)
                         (cons (vulpea-select-describe n context)
                               n))
                       expanded-notes)))
    (let* ((note (completing-read
                  (concat prompt ": ")
                  (vulpea-select--completion-table completions)
                  nil require-match initial-prompt)))
      (or (cdr (assoc note completions))
          (make-vulpea-note
           :title (substring-no-properties note)
           :level 0)))))

(cl-defun vulpea-select-multiple-from (prompt
                                       notes
                                       &key
                                       require-match
                                       initial-prompt
                                       expand-aliases
                                       select-fn)
  "Collect multiple elements from list of NOTES.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'.

It behaves the same as the following code

  (vulpea-utils-collect-while
    #\\='vulpea-select-from nil prompt notes
    :require-match require-match
    :initial-prompt initial-prompt
    :expand-aliases expand-aliases)

The only difference, it allows to select a single note only once, i.e.
the next prompt iteration doesn't contain already selected notes.

Optionally, an interactive SELECT-FN can be provided to be used instead
of `vulpea-select-from'. Signatures must match."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value
              (funcall-interactively
               (or select-fn #'vulpea-select-from)
               (concat prompt " (C-g to stop)")
               notes
               :require-match require-match
               :initial-prompt initial-prompt
               :expand-aliases expand-aliases))
        (setq notes (--remove (string-equal (vulpea-note-id it)
                                            (vulpea-note-id value))
                              notes))
        (setq result (cons value result))))
    (setq quit-flag nil)
    (reverse result)))

(provide 'vulpea-select)
;;; vulpea-select.el ends here
