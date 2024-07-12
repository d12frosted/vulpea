;;; vulpea-note.el --- Vulpea note definition  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 28 Feb 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
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
;;; Commentary:
;;
;; Vulpea is a fox.
;;
;;; Code:

(require 'ol)
(require 'dash)

(cl-defstruct vulpea-note
  id
  path
  level
  title
  primary-title
  aliases
  tags
  links
  properties
  meta
  attach-dir
  outline-path)

(autoload 'vulpea-db-query-by-ids "vulpea-db")



(defun vulpea-note-meta-get-list (note prop &optional type)
  "Get all values of PROP from NOTE meta.

Each element value depends on TYPE:

- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol."
  (setq type (or type 'string))
  (let ((items (cdr (assoc prop (vulpea-note-meta note)))))
    (if (eq type 'note)
        (let* ((kvps (cl-loop for it in items
                              collect (if (string-match org-link-bracket-re it)
                                          (let ((link (match-string 1 it))
                                                (desc (match-string 2 it)))
                                            (if (string-prefix-p "id:" link)
                                                (cons (string-remove-prefix "id:" link) desc)
                                              (user-error "Expected id link, but got '%s'" it)))
                                        (user-error "Expected link, but got '%s'" it))))
               (ids (mapcar #'car kvps))
               (notes (cl-loop for note in (vulpea-db-query-by-ids ids)
                               unless (vulpea-note-primary-title note)
                               collect note)))
          (cl-loop for it in kvps
                   collect (let* ((id (car it))
                                  (desc (cdr it))
                                  (note (--find (string-equal id (vulpea-note-id it)) notes)))
                             (when (seq-contains-p (vulpea-note-aliases note) desc)
                               (setf (vulpea-note-primary-title note) (vulpea-note-title note))
                               (setf (vulpea-note-title note) desc))
                             note)))
      (cl-loop for it in items
               collect (pcase type
                         (`string it)
                         (`symbol (intern it))
                         (`number (string-to-number it))
                         (`link (if (string-match org-link-bracket-re it)
                                    (let ((link (match-string 1 it)))
                                      (if (string-prefix-p "id:" link)
                                          (string-remove-prefix "id:" link)
                                        link)))))))))

(defun vulpea-note-meta-get (note prop &optional type)
  "Get value of PROP from NOTE meta.

Result depends on TYPE:

- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-note-meta-get-list'."
  (car (vulpea-note-meta-get-list note prop type)))



(cl-defmethod vulpea-note-tagged-all-p ((note vulpea-note) &rest tags)
  "Return non-nil if a NOTE is tagged by all of the TAGS."
  (let ((note-tags (vulpea-note-tags note)))
    (--all-p (-contains-p note-tags it) tags)))

(cl-defmethod vulpea-note-tagged-any-p ((note vulpea-note) &rest tags)
  "Return non-nil if a NOTE is tagged by any of the TAGS."
  (let ((note-tags (vulpea-note-tags note)))
    (--any-p (-contains-p note-tags it) tags)))



(cl-defmethod vulpea-note-links-to-all-p ((note vulpea-note) &rest links)
  "Return non-nil if a NOTE links to all LINKS."
  (let ((note-links (vulpea-note-links note)))
    (--all-p (-contains-p note-links it) links)))

(cl-defmethod vulpea-note-links-to-any-p ((note vulpea-note) &rest links)
  "Return non-nil if a NOTE links to at least one of LINKS."
  (let ((note-links (vulpea-note-links note)))
    (--any-p (-contains-p note-links it) links)))



(provide 'vulpea-note)
;;; vulpea-note.el ends here
