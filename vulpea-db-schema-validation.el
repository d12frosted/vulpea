;;; vulpea-db-schema-validation.el --- Surface schema violations at sync -*- lexical-binding: t; -*-
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
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Surface schema violations as notes are indexed.
;;
;; Files are the source of truth in vulpea, so a violating note cannot be
;; blocked from being written to disk.  Instead this module reacts when a
;; note enters the database: it validates the note against every
;; applicable schema and, depending on `vulpea-db-schema-validation-action',
;; either stays silent, emits a non-blocking warning, or keeps the
;; violating note out of the database.
;;
;; It plugs into the indexing path through
;; `vulpea-db-note-index-filter-functions' and is a no-op unless schemas
;; are registered and one applies to the note being indexed.
;;
;;; Code:

(require 'vulpea-note)
(require 'vulpea-schema)
(require 'vulpea-db-extract)

(defcustom vulpea-db-schema-validation-action 'warning
  "How to react when an indexed note violates an applicable schema.

The check runs while a note is written to the database (on save,
external sync, or a full scan), and only when schemas are registered and
one applies to the note - otherwise it does nothing.

- `silent'  index the note, say nothing.
- `warning' index the note, emit a non-blocking warning naming the
            violations (the default).
- `error'   keep the violating note OUT of the database (skip indexing
            it) and emit a warning.  The file on disk is untouched, so
            the note simply stays absent from the database until it is
            fixed and re-synced.

Because files are the source of truth, none of these touch the file;
they only control whether the database mirrors a violating note."
  :type '(choice (const :tag "Index silently" silent)
                 (const :tag "Index and warn" warning)
                 (const :tag "Skip and warn" error))
  :group 'vulpea)

(defun vulpea-db-schema-validation--format (note violations)
  "Format VIOLATIONS of NOTE into a human-readable warning string."
  (format "Note %S (%s) violates its schema:\n%s"
          (vulpea-note-title note)
          (vulpea-note-id note)
          (mapconcat
           (lambda (v)
             (format "  - [%s] %s"
                     (vulpea-violation-schema v)
                     (vulpea-violation-message v)))
           violations "\n")))

(defun vulpea-db-schema-validation--warn (note violations level)
  "Emit a warning at LEVEL describing VIOLATIONS of NOTE."
  (lwarn 'vulpea-schema level "%s"
         (vulpea-db-schema-validation--format note violations)))

(defun vulpea-db-schema-validation--filter (note)
  "Validate NOTE for `vulpea-db-note-index-filter-functions'.

Return non-nil to allow indexing NOTE and nil to skip it, dispatching on
`vulpea-db-schema-validation-action'.  Fast-exits when validation is
`silent' or no schemas are registered, and never affects a note that
conforms or that no schema applies to."
  (if (or (eq vulpea-db-schema-validation-action 'silent)
          (null (vulpea-schema-list)))
      t
    (let ((violations (vulpea-schema-note-violations note)))
      (cond
       ((null violations) t)
       ((eq vulpea-db-schema-validation-action 'error)
        (vulpea-db-schema-validation--warn note violations :error)
        nil)
       (t
        (vulpea-db-schema-validation--warn note violations :warning)
        t)))))

(add-hook 'vulpea-db-note-index-filter-functions
          #'vulpea-db-schema-validation--filter)

(provide 'vulpea-db-schema-validation)
;;; vulpea-db-schema-validation.el ends here
