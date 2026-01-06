;;; vulpea-note.el --- Vulpea note definition -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
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
;; This file is not part of GNU Emacs.
;;
;; Created: 28 Feb 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Vulpea note structure and utility functions.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

(autoload 'vulpea-db-query-by-ids "vulpea-db-query")

;;; Note Structure

(cl-defstruct vulpea-note
  "Structure representing a note.

Slots:
  id           - Unique identifier (UUID)
  path         - File path
  level        - Heading level (0 = file-level)
  pos          - Position in file
  title        - Note title
  primary-title - Original title (set when note is referenced via alias)
  aliases      - List of aliases
  tags         - List of tags
  links        - List of links (as plists with :dest and :type)
  properties   - Alist of properties
  meta         - Alist of metadata
  todo         - TODO state
  priority     - Priority
  scheduled    - Scheduled timestamp
  deadline     - Deadline timestamp
  closed       - Closed timestamp
  outline-path - Path to heading
  attach-dir   - Attachment directory
  file-title   - Title of the file containing this note"
  id
  path
  level
  pos
  title
  primary-title
  aliases
  tags
  links
  properties
  meta
  todo
  priority
  scheduled
  deadline
  closed
  outline-path
  attach-dir
  file-title)

;;; Predicates

(cl-defmethod vulpea-note-tagged-all-p ((note vulpea-note) &rest tags)
  "Return non-nil if NOTE is tagged by all of the TAGS."
  (let ((note-tags (vulpea-note-tags note)))
    (cl-every (lambda (tag) (member tag note-tags)) tags)))

(cl-defmethod vulpea-note-tagged-any-p ((note vulpea-note) &rest tags)
  "Return non-nil if NOTE is tagged by any of the TAGS."
  (let ((note-tags (vulpea-note-tags note)))
    (cl-some (lambda (tag) (member tag note-tags)) tags)))

(cl-defmethod vulpea-note-links-to-all-p ((note vulpea-note) &rest links)
  "Return non-nil if NOTE links to all LINKS."
  (let ((note-links (mapcar (lambda (l) (plist-get l :dest))
                            (vulpea-note-links note))))
    (cl-every (lambda (link) (member link note-links)) links)))

(cl-defmethod vulpea-note-links-to-any-p ((note vulpea-note) &rest links)
  "Return non-nil if NOTE links to at least one of LINKS."
  (let ((note-links (mapcar (lambda (l) (plist-get l :dest))
                            (vulpea-note-links note))))
    (cl-some (lambda (link) (member link note-links)) links)))

;;; Note Expansion

(defun vulpea-note-expand-aliases (note)
  "Expand NOTE into multiple notes based on aliases.

Returns a list of `vulpea-note' structures:
- First element has the original title
- Subsequent elements have each alias as title, with `primary-title'
  set to the original title

This is useful for selection interfaces where you want users to be
able to select a note by any of its names (title or aliases) and
have the selected name preserved in the result.

Example:
  (vulpea-note-expand-aliases
   (make-vulpea-note :title \"Original\" :aliases \\='(\"Alias1\" \"Alias2\")))
  => list of 3 notes:
     - note with title=\"Original\"
     - note with title=\"Alias1\", primary-title=\"Original\"
     - note with title=\"Alias2\", primary-title=\"Original\""
  (let ((title (vulpea-note-title note))
        (aliases (vulpea-note-aliases note)))
    (cons note
          (mapcar
           (lambda (alias)
             (let ((copy (copy-vulpea-note note)))
               (setf (vulpea-note-title copy) alias)
               (setf (vulpea-note-primary-title copy) title)
               copy))
           aliases))))

;;; Metadata Access

(defun vulpea-note-meta-get-list (note prop &optional type)
  "Get all values of PROP from NOTE meta.

Each element value depends on TYPE:

- string (default) - raw string value
- number - parsed as number
- link - path of the link (ID for id: links, raw link otherwise)
- note - linked `vulpea-note'
- symbol - interned symbol."
  (setq type (or type 'string))
  (let ((items (cdr (assoc prop (vulpea-note-meta note)))))
    (if (eq type 'note)
        (let* ((kvps (cl-loop for value in items
                              collect (if (string-match org-link-bracket-re value)
                                          ;; Full link format: [[id:uuid][description]]
                                          (let ((link (match-string 1 value))
                                                (desc (match-string 2 value)))
                                            (if (string-prefix-p "id:" link)
                                                (cons (string-remove-prefix "id:" link) desc)
                                              (user-error "Expected id link, but got '%s'" value)))
                                        ;; Plain UUID format
                                        (cons value nil))))
               (ids (mapcar #'car kvps))
               (notes (vulpea-db-query-by-ids ids)))
          (cl-loop for it in kvps
                   collect (let* ((id (car it))
                                  (desc (cdr it))
                                  (note (--find (string-equal id (vulpea-note-id it)) notes)))
                             (when (and desc (seq-contains-p (vulpea-note-aliases note) desc))
                               (setf (vulpea-note-primary-title note) (vulpea-note-title note))
                               (setf (vulpea-note-title note) desc))
                             note)))
      (cl-loop for value in items
               collect (pcase type
                         ('string value)
                         ('symbol (intern value))
                         ('number (string-to-number value))
                         ('link (if (string-match org-link-bracket-re value)
                                    (let ((link (match-string 1 value)))
                                      (if (string-prefix-p "id:" link)
                                          (string-remove-prefix "id:" link)
                                        link))
                                  value)))))))

(defun vulpea-note-meta-get (note prop &optional type)
  "Get value of PROP from NOTE meta.

Result depends on TYPE:

- string (default) - an interpreted object (without trailing newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-note-meta-get-list'."
  (car (vulpea-note-meta-get-list note prop type)))

(provide 'vulpea-note)
;;; vulpea-note.el ends here
