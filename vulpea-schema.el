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
;;   :target-tags  for a `note' field, a list of tags its target must
;;              carry (all of them); a missing tag yields an
;;              `invalid-target' violation.
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
;; A schema may inherit fields from others with `:include' (a schema
;; name or a list of names).  Included fields are merged in before the
;; schema's own; on a :key collision the later definition wins, so a
;; schema's own field overrides an included one.  See
;; `vulpea-schema-define' for the full merge rules.
;;
;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'vulpea-note)
(require 'vulpea-utils)
(require 'vulpea-db-query)

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

(defun vulpea-schema--merge-fields (field-lists)
  "Merge FIELD-LISTS, a list of field-spec lists, into a single list.
The lists are processed in order; a later field spec overrides an
earlier one with the same :key, whether the duplicate falls within a
single list or across lists, while an overridden field keeps the
position of its first appearance.  Used to fold the fields of included
schemas into a schema that names them in :include."
  (let ((order nil)
        (by-key (make-hash-table :test 'equal)))
    (dolist (fields field-lists)
      (dolist (field fields)
        (let ((key (plist-get field :key)))
          (unless (gethash key by-key)
            (push key order))
          (puthash key field by-key))))
    (mapcar (lambda (key) (gethash key by-key)) (nreverse order))))

(cl-defun vulpea-schema-define (name &key predicate fields include)
  "Define and register a schema called NAME.

PREDICATE is a function (note) -> non-nil when the schema applies to
the note.  FIELDS is a list of field spec plists (see Commentary for
the supported keys).

INCLUDE names another schema to inherit fields from - a schema name
symbol, a `vulpea-schema' object, or a list of either.  The included
schemas contribute their fields first (multiple includes merged
left-to-right), then FIELDS.  When a :key appears more than once the
later definition wins, so FIELDS overrides an included field and a
later include overrides an earlier one; an overridden field keeps the
position of its first appearance.  Includes are resolved now, so an
included schema must already be registered, and re-defining it later
does not retroactively update schemas that included it.

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
  (let* ((includes (cond
                    ((null include) nil)
                    ((or (symbolp include) (vulpea-schema-p include)) (list include))
                    ((listp include) include)
                    (t (error "Schema :include must be a symbol, a schema, or a list of them: %S"
                              include))))
         (inherited (mapcar (lambda (s)
                              (vulpea-schema-fields (vulpea-schema--resolve s)))
                            includes))
         (schema (vulpea-schema--create
                  :name name
                  :predicate predicate
                  :fields (vulpea-schema--merge-fields
                           (append inherited (list fields))))))
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

;;; Validation

(cl-defstruct (vulpea-violation (:copier nil))
  "A single schema violation found on a note.

Slots:
  note-id    - id of the offending note
  note-title - title of the offending note
  schema     - name of the schema that was violated
  field      - metadata key of the offending field
  type       - violation type symbol (see `vulpea-schema-validate')
  message    - human-readable description
  value      - the offending value, when applicable"
  note-id note-title schema field type message value)

(defun vulpea-schema--call-or-value (spec note)
  "Resolve SPEC for NOTE: call it when a function, otherwise return it.
Used for field keys like :required and :one-of that accept either a
literal or a function of the note."
  (if (functionp spec) (funcall spec note) spec))

(defun vulpea-schema--coerce (raw type)
  "Coerce the RAW string value to TYPE for comparison."
  (pcase type
    ('number (string-to-number raw))
    ('symbol (intern raw))
    (_ raw)))

(defun vulpea-schema--link-id (raw)
  "Return the note id referenced by RAW, or nil when RAW is not an id link.
Accepts a bracket link \"[[id:UUID][desc]]\" or a bare UUID."
  (cond
   ((string-match org-link-bracket-re raw)
    (let ((link (match-string 1 raw)))
      (when (string-prefix-p "id:" link)
        (string-remove-prefix "id:" link))))
   ((string-match-p (concat "\\`" vulpea-utils--uuid-regexp "\\'") raw)
    raw)))

(defun vulpea-schema--violation (note schema field type message value)
  "Build a `vulpea-violation' for NOTE on SCHEMA's FIELD of TYPE.
MESSAGE describes the problem and VALUE is the offending value."
  (make-vulpea-violation
   :note-id (vulpea-note-id note)
   :note-title (vulpea-note-title note)
   :schema (vulpea-schema-name schema)
   :field (plist-get field :key)
   :type type
   :message message
   :value value))

(defun vulpea-schema--target-tags-violation (target id field note schema)
  "Return a violation when TARGET is missing FIELD's :target-tags, else nil.

TARGET is the resolved `vulpea-note' referenced by FIELD on NOTE, and ID
is its id (used in the message and as the offending value).  SCHEMA owns
the field.  All declared tags must be present (all-of); the missing ones
are named in the message."
  (let* ((required (plist-get field :target-tags))
         (missing (and required
                       (cl-remove-if
                        (lambda (tag) (member tag (vulpea-note-tags target)))
                        required))))
    (when missing
      (vulpea-schema--violation
       note schema field 'invalid-target
       (format "Field %S target %s is missing required tag(s) %S"
               (plist-get field :key) id missing)
       id))))

(defun vulpea-schema--value-violation (raw field note schema)
  "Return a violation for the RAW value of FIELD on NOTE, or nil when valid.
SCHEMA is the owning schema.  Checks, in order: type, reference
resolution and :target-tags (for `note' fields), allowed values, then a
custom :validate function.  Returns the first problem found."
  (let* ((key (plist-get field :key))
         (type (or (plist-get field :type) 'string))
         (one-of (vulpea-schema--call-or-value (plist-get field :one-of) note))
         (validate (plist-get field :validate)))
    (or
     ;; type / reference checks
     (pcase type
       ('number
        (unless (string-match-p "\\`[ \t]*-?[0-9][0-9.]*[ \t]*\\'" raw)
          (vulpea-schema--violation
           note schema field 'wrong-type
           (format "Field %S expected a number, got %S" key raw) raw)))
       ((or 'note 'link)
        (let ((id (vulpea-schema--link-id raw)))
          (cond
           ((null id)
            (vulpea-schema--violation
             note schema field 'wrong-type
             (format "Field %S expected an id link, got %S" key raw) raw))
           ;; `link' fields only check the id-link shape; `note' fields
           ;; additionally resolve the target and enforce :target-tags.
           ((eq type 'note)
            (let ((target (vulpea-db-get-by-id id)))
              (if (not target)
                  (vulpea-schema--violation
                   note schema field 'invalid-reference
                   (format "Field %S references missing note %s" key id) id)
                (vulpea-schema--target-tags-violation
                 target id field note schema))))))))
     ;; allowed values (not meaningful for note/link references)
     (when (and one-of (not (memq type '(note link))))
       (let ((typed (vulpea-schema--coerce raw type)))
         (unless (member typed one-of)
           (vulpea-schema--violation
            note schema field 'disallowed-value
            (format "Field %S value %S is not one of %S" key typed one-of)
            typed))))
     ;; custom validator
     (when validate
       (let* ((typed (vulpea-schema--coerce raw type))
              (result (funcall validate typed note)))
         (unless (eq result t)
           (vulpea-schema--violation
            note schema field 'invalid-value
            (if (stringp result) result
              (format "Field %S failed validation" key))
            typed)))))))

(defun vulpea-schema--field-violations (note field schema)
  "Return the list of violations for FIELD on NOTE against SCHEMA."
  (let* ((key (plist-get field :key))
         (required (vulpea-schema--call-or-value (plist-get field :required) note))
         (raws (vulpea-note-meta-get-list note key 'string)))
    (cond
     ((and required (null raws))
      (list (vulpea-schema--violation
             note schema field 'missing-required
             (format "Required field %S is missing" key) nil)))
     ((null raws) nil)
     (t (delq nil
              (mapcar (lambda (raw)
                        (vulpea-schema--value-violation raw field note schema))
                      raws))))))

(defun vulpea-schema-validate (note schema-or-name)
  "Return the list of `vulpea-violation' for NOTE against SCHEMA-OR-NAME.

SCHEMA-OR-NAME is a `vulpea-schema' object or a registered schema name.
Each field is checked for the following problem types:

- `missing-required'   a required field has no value
- `wrong-type'         a value does not match the field's declared type
- `invalid-reference'  a `note' field links to a non-existent note
- `invalid-target'     a `note' field's target is missing a :target-tags tag
- `disallowed-value'   a value is not in the field's :one-of set
- `invalid-value'      a value is rejected by the field's :validate function

The note is validated as given; the schema's predicate is not consulted
here (see `vulpea-schema-validate-all')."
  (let ((schema (vulpea-schema--resolve schema-or-name)))
    (cl-loop for field in (vulpea-schema-fields schema)
             append (vulpea-schema--field-violations note field schema))))

(defun vulpea-schema-validate-notes (notes schema-or-name)
  "Return all violations for NOTES against SCHEMA-OR-NAME.

NOTES is any list of `vulpea-note' objects; every note is validated
regardless of the schema's predicate.  To validate only the notes a
schema applies to, filter with `vulpea-schema-applies-p' first, or use
`vulpea-schema-validate-all'."
  (let ((schema (vulpea-schema--resolve schema-or-name)))
    (cl-loop for note in notes
             append (vulpea-schema-validate note schema))))

(defun vulpea-schema-validate-all (schema-or-name)
  "Return all violations for every note matched by SCHEMA-OR-NAME.

Notes are selected with the schema's predicate, then validated.  This
is sugar for `vulpea-schema-validate-notes' over the matching notes."
  (let ((schema (vulpea-schema--resolve schema-or-name)))
    (vulpea-schema-validate-notes
     (vulpea-db-query (lambda (note) (vulpea-schema-applies-p note schema)))
     schema)))

;;; Authoring

(defun vulpea-schema-applicable (note)
  "Return the names of registered schemas whose predicate matches NOTE.
Schemas are tested in registration order."
  (cl-remove-if-not (lambda (name) (vulpea-schema-applies-p note name))
                    (vulpea-schema-list)))

(defun vulpea-schema-missing-fields (note schema-or-name &optional required-only)
  "Return the fields of SCHEMA-OR-NAME that NOTE has no value for.

Required fields come first (in declared order), then optional ones;
with REQUIRED-ONLY non-nil only required fields are returned.
Requiredness is evaluated against NOTE, so a function :required is
honored.  A field counts as present when NOTE has any value for its
:key, so this never proposes a field the note already carries."
  (let ((schema (vulpea-schema--resolve schema-or-name))
        (required nil)
        (optional nil))
    (dolist (field (vulpea-schema-fields schema))
      (when (null (vulpea-note-meta-get-list note (plist-get field :key) 'string))
        (if (vulpea-schema--call-or-value (plist-get field :required) note)
            (push field required)
          (unless required-only (push field optional)))))
    (append (nreverse required) (nreverse optional))))

(provide 'vulpea-schema)
;;; vulpea-schema.el ends here
