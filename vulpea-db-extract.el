;;; vulpea-db-extract.el --- Parse and extract note data -*- lexical-binding: t; -*-
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
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Parser and extractor system for Vulpea v2.
;;
;; This module provides:
;; - Parse-once architecture with org-element
;; - Parse context shared across extractors
;; - Core extractors (notes, tags, links, meta)
;; - Extractor registry for plugins
;; - Configurable heading-level indexing
;;
;; Design:
;; - Single org-element parse per file
;; - Parse context contains AST + file metadata
;; - Extractors are pure functions: context -> data
;; - Registry allows plugins to add custom extractors
;;
;;; Code:

(require 'org-element)
(require 'seq)
(require 'cl-lib)
(require 'vulpea-db)

(defsubst vulpea-db--string-no-properties (value)
  "Return VALUE as a plain string without text properties."
  (when (and value (stringp value))
    (substring-no-properties value)))

(defsubst vulpea-db--strings-no-properties (values)
  "Return VALUES list with text properties stripped from each element."
  (when values
    (mapcar #'substring-no-properties values)))

;;; Parse Context

(cl-defstruct vulpea-parse-ctx
  "Context for parsing a single org file.

Slots:
  path          - Absolute file path
  ast           - Complete org-element AST
  file-node     - File-level heading data (plist)
  heading-nodes - List of heading-level data (plists)
  hash          - Content hash for change detection
  mtime         - File modification time
  size          - File size in bytes"
  path
  ast
  file-node
  heading-nodes
  hash
  mtime
  size)

;;; Extractor Definition

(cl-defstruct vulpea-extractor
  "Definition of a plugin extractor.

Slots:
  name       - Symbol identifying the extractor (required, unique)
  version    - Integer version number for migrations (default: 1)
  schema     - Database schema for plugin tables (optional)
  priority   - Execution priority, lower runs first (default: 100)
  extract-fn - Function (ctx note-data) -> note-data (required)

Example:
  (make-vulpea-extractor
   :name 'citations
   :version 1
   :priority 50
   :schema '((citations [(note-id :not-null)
                         (citekey :not-null)]
              (:foreign-key [note-id] :references notes [id]
               :on-delete :cascade)))
   :extract-fn #'my-extract-citations)"
  name
  (version 1)
  schema
  (priority 100)
  extract-fn)

;;; Core Parsing

(defun vulpea-db--parse-file (path)
  "Parse org file at PATH and return parse context.

Returns `vulpea-parse-ctx' structure with:
- Full org-element AST
- File-level node data
- Heading-level node data (if enabled)
- File metadata (hash, mtime, size)"
  (with-temp-buffer
    (insert-file-contents path)
    (setq buffer-file-name path)  ; Required for org-attach-dir
    (org-mode)  ; Enable org-mode for proper TODO/timestamp parsing
    (let* ((content (buffer-string))
           (ast (org-element-parse-buffer))
           (attrs (file-attributes path))
           (mtime (float-time (file-attribute-modification-time attrs)))
           (size (file-attribute-size attrs))
           (hash (secure-hash 'sha256 content)))

      (make-vulpea-parse-ctx
       :path path
       :ast ast
       :file-node (vulpea-db--extract-file-node ast path (current-buffer))
       :heading-nodes (vulpea-db--extract-heading-nodes ast path (current-buffer))
       :hash hash
       :mtime mtime
       :size size))))

(defun vulpea-db--extract-file-node (ast path buffer)
  "Extract file-level node data from AST at PATH in BUFFER.

Returns plist with:
  :id :title :aliases :tags :links :properties :meta
  :todo :priority :scheduled :deadline :closed :attach-dir

Returns nil if:
- File has no ID property in property drawer
- File has VULPEA_IGNORE property set to non-nil value"
  (let* ((keywords (org-element-map ast 'keyword
                     (lambda (kw)
                       (cons (vulpea-db--string-no-properties
                              (org-element-property :key kw))
                             (vulpea-db--string-no-properties
                              (org-element-property :value kw))))))
         (properties (vulpea-db--extract-properties ast nil))
         (id (cdr (assoc "ID" properties)))
         (ignored (cdr (assoc "VULPEA_IGNORE" properties))))

    ;; Only index if ID exists and not explicitly ignored
    (when (and id (not ignored))
      (let* ((title (or (cdr (assoc "TITLE" keywords))
                        (file-name-base path)))
             (filetags (cdr (assoc "FILETAGS" keywords)))
             (tags (when filetags
                     (split-string filetags ":" t)))
             (meta (vulpea-db--extract-meta ast))
             (links (vulpea-db--extract-links ast))
             (aliases (vulpea-db--extract-aliases properties))
             (attach-dir (with-current-buffer buffer
                           (require 'org-attach)
                           (save-excursion
                             (goto-char (point-min))
                             (org-attach-dir nil 'no-fs-check)))))

        (list :id id
              :title title
              :aliases aliases
              :tags tags
              :links links
              :properties properties
              :meta meta
              :todo nil
              :priority nil
              :scheduled nil
              :deadline nil
              :closed nil
              :attach-dir attach-dir)))))

(defun vulpea-db--extract-heading-nodes (ast path buffer)
  "Extract heading-level nodes from AST at PATH in BUFFER.

Returns list of plists, one per heading with ID property.
Each plist has same structure as file-node.

Skips headings that:
- Have no ID property
- Have VULPEA_IGNORE property set to non-nil value

Respects `vulpea-db-index-heading-level' setting."
  (when (vulpea-db--should-index-headings-p path)
    (org-element-map ast 'headline
      (lambda (headline)
        (when-let ((id (org-element-property :ID headline)))
          (let* ((properties (vulpea-db--extract-properties ast headline))
                 (ignored (cdr (assoc "VULPEA_IGNORE" properties))))
            ;; Only index if not explicitly ignored
            (unless ignored
              (let* ((title (vulpea-db--string-no-properties
                             (org-element-property :raw-value headline)))
                     (tags (vulpea-db--strings-no-properties
                            (org-element-property :tags headline)))
                     (level (org-element-property :level headline))
                     (pos (org-element-property :begin headline))
                     (todo (vulpea-db--string-no-properties
                            (org-element-property :todo-keyword headline)))
                     (priority (org-element-property :priority headline))
                     (scheduled (org-element-property :scheduled headline))
                     (deadline (org-element-property :deadline headline))
                     (closed (org-element-property :closed headline))
                     (meta (vulpea-db--extract-meta headline))
                     (links (vulpea-db--extract-links headline))
                     (outline-path (vulpea-db--strings-no-properties
                                    (org-element-property
                                     :title
                                     (org-element-lineage headline '(headline)))))
                     (attach-dir (with-current-buffer buffer
                                   (require 'org-attach)
                                   (save-excursion
                                     (goto-char pos)
                                     (org-attach-dir nil 'no-fs-check)))))

                (list :id id
                      :level level
                      :pos pos
                      :title title
                      :aliases nil  ; Only file-level has aliases
                      :tags tags
                      :links links
                      :properties properties
                      :meta meta
                      :todo todo
                      :priority priority
                      :scheduled (when scheduled
                                   (vulpea-db--string-no-properties
                                    (org-element-property :raw-value scheduled)))
                      :deadline (when deadline
                                  (vulpea-db--string-no-properties
                                   (org-element-property :raw-value deadline)))
                      :closed (when closed
                                (vulpea-db--string-no-properties
                                 (org-element-property :raw-value closed)))
                      :outline-path outline-path
                      :attach-dir attach-dir)))))))))

(defun vulpea-db--should-index-headings-p (path)
  "Check if headings should be indexed for PATH.

Respects `vulpea-db-index-heading-level' setting:
- t: always index headings
- nil: never index headings
- function: call with path, use result"
  (pcase vulpea-db-index-heading-level
    ('t t)
    ('nil nil)
    ((pred functionp) (funcall vulpea-db-index-heading-level path))
    (_ t)))

;;; Extractors

(defun vulpea-db--extract-aliases (properties)
  "Extract aliases from PROPERTIES alist.

Looks for ROAM_ALIASES property.
If the value is a quoted string, returns it as a single alias.
Otherwise, splits on spaces."
  (when-let ((aliases-str (cdr (assoc "ROAM_ALIASES" properties))))
    (setq aliases-str (string-trim aliases-str))
    ;; If it's a quoted string, treat the whole thing as one alias
    (if (and (> (length aliases-str) 1)
             (string-prefix-p "\"" aliases-str)
             (string-suffix-p "\"" aliases-str))
        (list (substring-no-properties (substring aliases-str 1 -1)))
      ;; Otherwise split on spaces
      (vulpea-db--strings-no-properties (split-string aliases-str " " t)))))

(defun vulpea-db--extract-properties (ast-or-node &optional headline)
  "Extract properties from AST-OR-NODE.

If HEADLINE is provided, extract from that headline.
Otherwise extract from file-level (property drawer).

Returns alist of (key . value) pairs."
  (let ((node (or headline ast-or-node)))
    (org-element-map node 'property-drawer
      (lambda (drawer)
        (org-element-map drawer 'node-property
          (lambda (prop)
            (cons (vulpea-db--string-no-properties
                   (org-element-property :key prop))
                  (vulpea-db--string-no-properties
                   (org-element-property :value prop))))))
      nil t)))  ; First match only

(defun vulpea-db--extract-links (ast-or-node)
  "Extract id: and roam: links from AST-OR-NODE.

Returns list of plists with :dest and :type."
  (org-element-map ast-or-node 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link)))
        (when (member type '("id" "roam"))
          (list :dest path :type type))))))

(defun vulpea-db--extract-meta (element)
  "Extract metadata from ELEMENT (AST or headline element).

Metadata is defined by the first description list in the element:
  - key :: value
  - key_type :: value

Supports type suffixes: _note, _number, _string, _link
Without suffix, type is inferred from content (link or string).

This approach is used because org-element doesn't parse links
inside properties drawer, which would break note references.

Returns alist of (key . values) where values is list of plists
with :value and :type."
  (let* ((pls (org-element-map element 'plain-list #'identity nil nil 'headline))
         (pl (seq-find
              (lambda (pl)
                (equal 'descriptive
                       (org-element-property :type pl)))
              pls))
         (meta-alist nil))
    (when pl
      (let ((items (org-element-map pl 'item #'identity)))
        (dolist (item items)
          (let* ((tag-contents (org-element-property :tag item))
                 (tag (when tag-contents
                        (substring-no-properties
                         (string-trim
                          (org-element-interpret-data
                           (org-element-contents tag-contents))))))
                 (value-el (car (org-element-contents item)))
                 (value (when value-el
                          (substring-no-properties
                           (string-trim
                            (org-element-interpret-data value-el))))))
            (when (and tag value)
              ;; Parse key and optional type suffix
              (let* ((parts (split-string tag "_" t))
                     (meta-key (substring-no-properties
                                (if (> (length parts) 1)
                                    (string-join (butlast parts) "_")
                                  tag)))
                     (type-suffix (when (> (length parts) 1)
                                    (car (last parts))))
                     (type (cond
                            ;; Explicit type suffix
                            ((and type-suffix
                                  (member type-suffix '("note" "number" "string" "link")))
                             type-suffix)
                            ;; Infer from content: check if it's a link
                            ((and value-el
                                  (eq 'link (org-element-type (car (org-element-contents value-el)))))
                             (let ((link (car (org-element-contents value-el))))
                               (if (string-equal (org-element-property :type link) "id")
                                   "note"
                                 "link")))
                            ;; Default to string
                            (t "string")))
                     ;; Extract actual value (ID for note links, URL for links)
                     (actual-value
                      (cond
                       ;; For note/link types, extract from link element
                       ((and value-el
                             (member type '("note" "link"))
                             (eq 'link (org-element-type (car (org-element-contents value-el)))))
                        (let ((link (car (org-element-contents value-el))))
                          (if (string-equal type "note")
                              (org-element-property :path link)
                            (org-element-property :raw-link link))))
                       ;; Otherwise use interpreted string
                       (t value))))
                (let ((existing (assoc meta-key meta-alist)))
                  (if existing
                      (setcdr existing
                              (append (cdr existing)
                                      (list (list :value actual-value :type type))))
                    (push (cons meta-key
                                (list (list :value actual-value :type type)))
                          meta-alist)))))))))
    meta-alist))

;;; Extractor Registry

(defvar vulpea-db--extractors nil
  "List of registered extractors.

Each extractor is a `vulpea-extractor' struct.")

(defun vulpea-db--apply-plugin-schema (extractor)
  "Apply database schema from EXTRACTOR if present.

Creates tables and indices defined in the extractor's :schema field.
Schema format matches `vulpea-db--schema':
  ((table-name
    [(column-name :constraints...)]
    (:unique [columns])
    (:foreign-key [columns] :references table [columns]))
   ...)

Also registers the schema version in schema-registry table."
  (when-let ((schema (vulpea-extractor-schema extractor))
             (name (vulpea-extractor-name extractor))
             (version (vulpea-extractor-version extractor)))
    (let ((db (vulpea-db)))
      ;; Check if schema already applied at this version
      (let ((existing-version
             (caar (emacsql db
                            [:select [version] :from schema-registry
                             :where (= name $s1)]
                            (symbol-name name)))))
        (unless (and existing-version (>= existing-version version))
          ;; Create tables from schema
          (dolist (table-spec schema)
            (emacsql db [:create-table :if-not-exists $i1 $S2]
                     (car table-spec)
                     (cdr table-spec)))

          ;; Register schema version
          (emacsql db [:insert :or :replace :into schema-registry
                       :values $v1]
                   (list (vector (symbol-name name)
                                 version
                                 (format-time-string "%Y-%m-%d %H:%M:%S")))))))))

(defun vulpea-db-register-extractor (extractor-or-name &optional fn)
  "Register an extractor.

EXTRACTOR-OR-NAME can be:
- A `vulpea-extractor' struct (recommended)
- A symbol NAME with FN function (backward compatible)

When using the struct form:
  (vulpea-db-register-extractor
   (make-vulpea-extractor
    :name \\='my-extractor
    :version 1
    :priority 50
    :extract-fn #\\='my-extract-fn))

When using the simple form (backward compatible):
  (vulpea-db-register-extractor \\='my-extractor #\\='my-extract-fn)

FN should be a function taking (ctx note-data) where:
- ctx is a `vulpea-parse-ctx' structure
- note-data is the plist being built for the note

FN should return updated note-data plist."
  (let ((extractor
         (cond
          ;; New struct form
          ((vulpea-extractor-p extractor-or-name)
           extractor-or-name)
          ;; Old simple form (backward compatible)
          ((and (symbolp extractor-or-name) fn)
           (make-vulpea-extractor
            :name extractor-or-name
            :extract-fn fn))
          (t
           (error "Invalid extractor: %S" extractor-or-name)))))
    ;; Validate extractor
    (unless (vulpea-extractor-name extractor)
      (error "Extractor must have a :name"))
    (unless (vulpea-extractor-extract-fn extractor)
      (error "Extractor must have an :extract-fn"))
    ;; Apply schema if present
    (vulpea-db--apply-plugin-schema extractor)

    ;; Remove existing extractor with same name
    (setq vulpea-db--extractors
          (cl-remove (vulpea-extractor-name extractor)
                     vulpea-db--extractors
                     :key #'vulpea-extractor-name))
    ;; Add new extractor
    (push extractor vulpea-db--extractors)
    ;; Sort by priority (lower priority runs first)
    (setq vulpea-db--extractors
          (sort vulpea-db--extractors
                (lambda (a b)
                  (< (vulpea-extractor-priority a)
                     (vulpea-extractor-priority b)))))
    extractor))

(defun vulpea-db-unregister-extractor (name)
  "Unregister extractor with NAME."
  (setq vulpea-db--extractors
        (cl-remove name vulpea-db--extractors
                   :key #'vulpea-extractor-name)))

(defun vulpea-db-get-extractor (name)
  "Get registered extractor by NAME.

Returns `vulpea-extractor' struct or nil if not found."
  (cl-find name vulpea-db--extractors
           :key #'vulpea-extractor-name))

(defun vulpea-db--run-extractors (ctx note-data)
  "Run all registered extractors on NOTE-DATA with CTX.

Extractors are run in priority order (lower priority first).
Returns updated note-data after all extractors have run."
  (cl-reduce (lambda (data extractor)
               (funcall (vulpea-extractor-extract-fn extractor) ctx data))
             vulpea-db--extractors
             :initial-value note-data))

;;; Update File

(defun vulpea-db-update-file (path)
  "Parse and update database for file at PATH.

Returns number of notes updated (file-level + headings)."
  (let* ((ctx (vulpea-db--parse-file path))
         (db (vulpea-db))
         (count 0))

    (emacsql-with-transaction db
      ;; Delete existing notes from this file
      (vulpea-db--delete-file-notes path)

      ;; Insert file-level note
      (let ((file-data (vulpea-parse-ctx-file-node ctx)))
        (when (plist-get file-data :id)
          (vulpea-db--insert-note-from-plist ctx path 0 0 file-data)
          (setq count (1+ count))))

      ;; Insert heading-level notes
      (dolist (heading-data (vulpea-parse-ctx-heading-nodes ctx))
        (vulpea-db--insert-note-from-plist
         ctx
         path
         (plist-get heading-data :level)
         (plist-get heading-data :pos)
         heading-data)
        (setq count (1+ count)))

      ;; Update file hash
      (vulpea-db--update-file-hash path
                                   (vulpea-parse-ctx-hash ctx)
                                   (vulpea-parse-ctx-mtime ctx)
                                   (vulpea-parse-ctx-size ctx)))

    count))

(defun vulpea-db--insert-note-from-plist (ctx path level pos data)
  "Insert note from DATA plist at PATH with LEVEL and POS.

CTX is the parse context containing AST and other metadata.
Runs registered extractors after insertion."
  (let* ((modified-at (format-time-string "%Y-%m-%d %H:%M:%S")))
    ;; First insert the note so foreign keys can reference it
    (vulpea-db--insert-note
     :id (plist-get data :id)
     :path path
     :level level
     :pos pos
     :title (plist-get data :title)
     :properties (plist-get data :properties)
     :tags (plist-get data :tags)
     :aliases (plist-get data :aliases)
     :meta (plist-get data :meta)
     :links (plist-get data :links)
     :todo (plist-get data :todo)
     :priority (plist-get data :priority)
     :scheduled (plist-get data :scheduled)
     :deadline (plist-get data :deadline)
     :closed (plist-get data :closed)
     :outline-path (plist-get data :outline-path)
     :attach-dir (plist-get data :attach-dir)
     :modified-at modified-at)

    ;; Then run extractors that may insert into foreign-keyed tables
    (vulpea-db--run-extractors ctx data)))

;;; Provide

(provide 'vulpea-db-extract)
;;; vulpea-db-extract.el ends here
