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
  attach-dir)

(autoload 'vulpea-db-get-by-id "vulpea-db")



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
    (seq-map
     (lambda (item)
       (pcase type
         (`string item)
         (`symbol (intern item))
         (`number (string-to-number item))
         (`note (if (string-match org-link-bracket-re item)
                    (let ((link (match-string 1 item))
                          (desc (match-string 2 item)))
                      (if (string-prefix-p "id:" link)
                          (let* ((id (string-remove-prefix "id:" link))
                                 (note (vulpea-db-get-by-id id)))
                            (when (seq-contains-p (vulpea-note-aliases note) desc)
                              (setf (vulpea-note-primary-title note) (vulpea-note-title note))
                              (setf (vulpea-note-title note) desc))
                            note)
                        (user-error "Expected id link, but got '%s'"
                                    item)))
                  (user-error "Expected link, but got '%s'" item)))
         (`link (if (string-match org-link-bracket-re item)
                    (let ((link (match-string 1 item)))
                      (if (string-prefix-p "id:" link)
                          (string-remove-prefix "id:" link)
                        link))))))
     items)))

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



(provide 'vulpea-note)
;;; vulpea-note.el ends here
