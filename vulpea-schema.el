;;; vulpea-schema.el --- Note schema definition and validation -*- lexical-binding: t; -*-
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
;; Created: 12 Jun 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Declarative schemas for vulpea notes.
;;
;; A schema describes the structured expectations for a class of notes:
;; which notes it applies to (a predicate) and which metadata fields they
;; should carry, with types, required-ness, and allowed values.
;;
;; This file provides schema definition and the registry.  The validation
;; engine (`vulpea-schema-validate' and friends) is layered on top in the
;; same module.
;;
;; A field spec is a plist with keys:
;;
;;   :key       metadata key string (required)
;;   :type      value type: `string' (default), `number', `symbol',
;;              `note', or `link'.  Values are read with
;;              `vulpea-note-meta-get', so the type vocabulary matches the
;;              rest of vulpea.
;;   :required  t/nil, or a function (note) -> boolean for conditional
;;              requiredness (e.g. required only for sparkling wines).
;;   :one-of    a list of allowed values, or a function (note) -> list for
;;              a dependent enum (e.g. sweetness levels depend on
;;              carbonation).
;;   :multiple  non-nil when the field holds a list of values.
;;   :validate  optional function (value note) -> t, or an error message
;;              string, for arbitrary checks.
;;
;; Example:
;;
;;   (vulpea-schema-define 'wine
;;     :predicate (lambda (note) (member "wine" (vulpea-note-tags note)))
;;     :fields
;;     '((:key "producer" :type note   :required t)
;;       (:key "name"     :type string :required t)
;;       (:key "colour"   :type symbol :required t :one-of (red white rose))
;;       (:key "grapes"   :type note   :multiple t)
;;       (:key "carbonation method" :type symbol
;;             :required (lambda (note)
;;                         (eq (vulpea-note-meta-get note "carbonation" 'symbol)
;;                             'sparkling)))))
;;
;; To read a sibling field inside a :required / :one-of function, use
;; `vulpea-note-meta-get' directly so you control the value type.
;;
;;; Code:

(require 'cl-lib)
(require 'vulpea-note)

;;; Schema structure

(cl-defstruct (vulpea-schema (:constructor vulpea-schema--create)
                             (:copier nil))
  "A schema describing structured expectations for a class of notes.

Slots:
  name      - symbol identifying the schema
  predicate - function (note) -> non-nil selecting notes it applies to
  fields    - list of field spec plists (see Commentary)"
  name
  predicate
  fields)

;;; Registry

(defvar vulpea-schema--registry (make-hash-table :test 'eq)
  "Registry of defined schemas, keyed by name symbol.")

(cl-defun vulpea-schema-define (name &key predicate fields)
  "Define and register a schema called NAME.

PREDICATE is a function (note) -> non-nil when the schema applies to
the note.  FIELDS is a list of field spec plists (see Commentary for
the supported keys).

Registering a schema with an existing NAME replaces it.  Returns the
`vulpea-schema' object."
  (unless (symbolp name)
    (error "Schema name must be a symbol: %S" name))
  (unless (functionp predicate)
    (error "Schema :predicate must be a function"))
  (dolist (field fields)
    (unless (and (plist-member field :key)
                 (stringp (plist-get field :key)))
      (error "Schema field must have a string :key: %S" field)))
  (let ((schema (vulpea-schema--create
                 :name name
                 :predicate predicate
                 :fields fields)))
    (puthash name schema vulpea-schema--registry)
    schema))

(defun vulpea-schema-get (name)
  "Return the registered schema called NAME, or nil."
  (gethash name vulpea-schema--registry))

(defun vulpea-schema-list ()
  "Return the list of registered schema names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) vulpea-schema--registry)
    (nreverse names)))

(defun vulpea-schema-unregister (name)
  "Remove the schema called NAME from the registry."
  (remhash name vulpea-schema--registry))

(defun vulpea-schema--resolve (schema-or-name)
  "Return a `vulpea-schema' from SCHEMA-OR-NAME.

SCHEMA-OR-NAME is either a `vulpea-schema' object or the symbol name of
a registered schema.  Signals an error when a name is not registered."
  (cond
   ((vulpea-schema-p schema-or-name) schema-or-name)
   ((symbolp schema-or-name)
    (or (vulpea-schema-get schema-or-name)
        (error "No schema registered as %S" schema-or-name)))
   (t (error "Not a schema or schema name: %S" schema-or-name))))

;;; Field access

(defun vulpea-schema-field-value (note field)
  "Return the value of FIELD for NOTE, read with the field's declared type.

FIELD is a field spec plist or a bare metadata key string (in which
case the type defaults to `string').  Reads from the note's
already-extracted metadata via `vulpea-note-meta-get'.  When the field
spec is :multiple, returns a list of values."
  (let* ((spec (if (stringp field) (list :key field) field))
         (key (plist-get spec :key))
         (type (or (plist-get spec :type) 'string)))
    (if (plist-get spec :multiple)
        (vulpea-note-meta-get-list note key type)
      (vulpea-note-meta-get note key type))))

;;; Predicate

(defun vulpea-schema-applies-p (note schema-or-name)
  "Return non-nil when SCHEMA-OR-NAME's predicate matches NOTE.

SCHEMA-OR-NAME is a `vulpea-schema' object or a registered schema name."
  (let ((schema (vulpea-schema--resolve schema-or-name)))
    (funcall (vulpea-schema-predicate schema) note)))

(provide 'vulpea-schema)
;;; vulpea-schema.el ends here
