;;; vulpea-note.el --- Vulpea note definition -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
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
  attach-dir   - Attachment directory"
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
  attach-dir)

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

;;; Metadata Access

(defun vulpea-note-meta-get-list (note prop &optional type)
  "Get all values of PROP from NOTE meta.

Each element value depends on TYPE:

- string (default) - an interpreted object (without trailing newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol."
  (setq type (or type 'string))
  (let ((items (cdr (assoc prop (vulpea-note-meta note)))))
    (if (eq type 'note)
        (let* ((kvps (cl-loop for it in items
                              collect (let ((value (plist-get it :value)))
                                        (if (string-match org-link-bracket-re value)
                                            ;; Full link format: [[id:uuid][description]]
                                            (let ((link (match-string 1 value))
                                                  (desc (match-string 2 value)))
                                              (if (string-prefix-p "id:" link)
                                                  (cons (string-remove-prefix "id:" link) desc)
                                                (user-error "Expected id link, but got '%s'" value)))
                                          ;; Plain UUID format (from meta extraction)
                                          (cons value nil)))))
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
      (cl-loop for it in items
               collect (pcase type
                         ('string (plist-get it :value))
                         ('symbol (intern (plist-get it :value)))
                         ('number (string-to-number (plist-get it :value)))
                         ('link (if (string-match org-link-bracket-re (plist-get it :value))
                                    (let ((link (match-string 1 (plist-get it :value))))
                                      (if (string-prefix-p "id:" link)
                                          (string-remove-prefix "id:" link)
                                        link))
                                  (plist-get it :value))))))))

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
