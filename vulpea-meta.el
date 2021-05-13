;;; vulpea-meta.el --- Metadata manipulation -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "1.2.3"))
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
;; Functionality for metadata manipulation. Metadata is defined by the
;; first description list in the note, e.g. list like:
;;
;; - key1 :: value1
;; - key2 :: value21
;; - key2 :: value22
;; - key3 :: value3
;;
;;; Code:

(require 'org-element)
(require 'seq)
(require 's)
(require 'vulpea-utils)
(require 'vulpea-db)
(require 'vulpea-buffer)

(defun vulpea-meta (note-or-id)
  "Get metadata for NOTE-OR-ID.

Return plist (:file :buffer :pl)

Metadata is defined by the first description list in the note,
e.g. list like:

- key1 :: value1
- key2 :: value21
- key2 :: value22
- key3 :: value3

In most cases, it's better to use either `vulpea-meta-get' to
retrieve a single value for a given key or
`vulpea-meta-get-list' to retrieve all values for a given
key.

In case you are doing multiple calls to meta API, it's better to
get metadata using this function and use bang version of
functions, e.g. `vulpea-buffer-meta-get!'."
  (when-let ((file (if (stringp note-or-id)
                       (vulpea-db-get-file-by-id note-or-id)
                     (vulpea-note-path note-or-id))))
    (vulpea-utils-with-file file
      (vulpea-buffer-meta))))

(defun vulpea-meta-get-list (note-or-id prop &optional type)
  "Get all values of PROP from NOTE-OR-ID.

Each element value depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol."
  (vulpea-buffer-meta-get-list! (vulpea-meta note-or-id) prop type))

(defun vulpea-meta-get (note-or-id prop &optional type)
  "Get value of PROP for NOTE-OR-ID.

Result depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-meta-get-list'."
  (vulpea-buffer-meta-get! (vulpea-meta note-or-id) prop type))

(defun vulpea-meta-set (note-or-id prop value &optional append)
  "Set VALUE of PROP for NOTE-OR-ID.

If the VALUE is a list, then each element is inserted
separately.

Please note that all occurrences of PROP are replaced by VALUE.

When PROP is not yet set, VALUE is inserted at the beginning of
the meta, unless the optional argument APPEND is non-nil, in
which case VALUE is added at the end of the meta."
  (when-let ((file (if (stringp note-or-id)
                       (vulpea-db-get-file-by-id note-or-id)
                     (vulpea-note-path note-or-id))))
    (vulpea-utils-with-file file
      (vulpea-buffer-meta-set prop value append))))

(defun vulpea-meta-remove (note-or-id prop)
  "Delete values of PROP for NOTE-OR-ID."
  (when-let ((file (if (stringp note-or-id)
                       (vulpea-db-get-file-by-id note-or-id)
                     (vulpea-note-path note-or-id))))
    (vulpea-utils-with-file file
      (vulpea-buffer-meta-remove prop))))

(defun vulpea-meta-clean (note-or-id)
  "Delete all meta from NOTE-OR-ID."
  (when-let ((file (if (stringp note-or-id)
                       (vulpea-db-get-file-by-id note-or-id)
                     (vulpea-note-path note-or-id))))
    (vulpea-utils-with-file file
      (vulpea-buffer-meta-clean))))

(provide 'vulpea-meta)
;;; vulpea-meta.el ends here
