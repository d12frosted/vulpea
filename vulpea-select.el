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

(defvar vulpea-select-describe-fn #'vulpea-note-title
  "Function to describe a note for completion.

Accepts a `vulpea-note'. Returns a `string'.")

(defvar vulpea-select-annotate-fn #'vulpea-select-annotate
  "Function to annotate a note for completion.

Accepts a `vulpea-note'. Returns a `string'.")

(defun vulpea-select-describe (note)
  "Describe a NOTE for completion."
  (propertize
   (concat
    (funcall vulpea-select-describe-fn note)
    (propertize
     (funcall vulpea-select-annotate-fn note)
     'face 'completions-annotations))
   'vulpea-note-id
   (vulpea-note-id note)))

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
         (completions (seq-map
                       (lambda (n)
                         (cons (vulpea-select-describe n)
                               n))
                       expanded-notes))
         (notes-table (make-hash-table :test #'equal)))
    (seq-each (lambda (note)
                (puthash (vulpea-note-id note) note notes-table))
              expanded-notes)
    (let ((note (completing-read
                 (concat prompt ": ")
                 completions
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
