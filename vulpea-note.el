;;; vulpea-note.el --- Vulpea note definition -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.0.0
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
;; Vulpea note structure and utility functions.
;;
;;; Code:

(require 'cl-lib)

;;; Note Structure

(cl-defstruct vulpea-note
  "Structure representing a note.

Slots:
  id           - Unique identifier (UUID)
  path         - File path
  level        - Heading level (0 = file-level)
  pos          - Position in file
  title        - Note title
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

(provide 'vulpea-note)
;;; vulpea-note.el ends here
