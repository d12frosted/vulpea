;;; vulpea-db-extract.el --- Parse and extract note data -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;;
;; Created: 16 Nov 2025
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
       :file-node (vulpea-db--extract-file-node ast path)
       :heading-nodes (vulpea-db--extract-heading-nodes ast path)
       :hash hash
       :mtime mtime
       :size size))))

(defun vulpea-db--extract-file-node (ast path)
  "Extract file-level node data from AST at PATH.

Returns plist with:
  :id :title :aliases :tags :links :properties :meta
  :todo :priority :scheduled :deadline :closed :attach-dir"
  (let* ((keywords (org-element-map ast 'keyword
                     (lambda (kw)
                       (cons (org-element-property :key kw)
                             (org-element-property :value kw)))))
         (id (or (cdr (assoc "ID" keywords))
                 (org-id-new)))
         (title (or (cdr (assoc "TITLE" keywords))
                    (file-name-base path)))
         (filetags (cdr (assoc "FILETAGS" keywords)))
         (tags (when filetags
                 (split-string filetags ":" t)))
         (properties (vulpea-db--extract-properties ast nil))
         (meta (vulpea-db--extract-meta properties))
         (links (vulpea-db--extract-links ast))
         (aliases (vulpea-db--extract-aliases keywords)))

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
          :attach-dir (cdr (assoc "ATTACH_DIR" keywords)))))

(defun vulpea-db--extract-heading-nodes (ast path)
  "Extract heading-level nodes from AST at PATH.

Returns list of plists, one per heading with ID property.
Each plist has same structure as file-node.

Respects `vulpea-db-index-heading-level' setting."
  (when (vulpea-db--should-index-headings-p path)
    (org-element-map ast 'headline
      (lambda (headline)
        (when-let ((id (org-element-property :ID headline)))
          (let* ((title (org-element-property :raw-value headline))
                 (tags (org-element-property :tags headline))
                 (level (org-element-property :level headline))
                 (pos (org-element-property :begin headline))
                 (todo (org-element-property :todo-keyword headline))
                 (priority (org-element-property :priority headline))
                 (scheduled (org-element-property :scheduled headline))
                 (deadline (org-element-property :deadline headline))
                 (closed (org-element-property :closed headline))
                 (properties (vulpea-db--extract-properties ast headline))
                 (meta (vulpea-db--extract-meta properties))
                 (links (vulpea-db--extract-links headline))
                 (outline-path (org-element-property :title
                                                     (org-element-lineage headline '(headline)))))

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
                               (org-element-property :raw-value scheduled))
                  :deadline (when deadline
                              (org-element-property :raw-value deadline))
                  :closed (when closed
                            (org-element-property :raw-value closed))
                  :outline-path outline-path
                  :attach-dir (cdr (assoc "ATTACH_DIR" properties)))))))))

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

(defun vulpea-db--extract-aliases (keywords)
  "Extract aliases from KEYWORDS alist.

Looks for ROAM_ALIASES keyword and splits on spaces."
  (when-let ((aliases-str (cdr (assoc "ROAM_ALIASES" keywords))))
    (split-string aliases-str " " t)))

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
            (cons (org-element-property :key prop)
                  (org-element-property :value prop)))))
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

(defun vulpea-db--extract-meta (properties)
  "Extract metadata from PROPERTIES alist.

Metadata properties follow pattern KEY_TYPE (e.g., country_note).
Supports types: note, number, string, link.
Property keys can end with + to indicate multiple values (e.g., grape_note+).

Returns alist of (key . values) where values is list of plists
with :value and :type."
  (let ((meta-alist nil))
    (dolist (prop properties)
      (let* ((raw-key (car prop))
             (value (cdr prop))
             ;; Strip trailing + if present
             (key (if (string-suffix-p "+" raw-key)
                      (substring raw-key 0 -1)
                    raw-key))
             (parts (split-string key "_" t)))
        (when (> (length parts) 1)
          (let* ((meta-key (string-join (butlast parts) "_"))
                 (type-str (car (last parts)))
                 (type (when (member type-str '("note" "number" "string" "link"))
                         type-str)))
            (when type
              (let ((existing (assoc meta-key meta-alist)))
                (if existing
                    (setcdr existing
                            (append (cdr existing)
                                    (list (list :value value :type type))))
                  (push (cons meta-key
                              (list (list :value value :type type)))
                        meta-alist))))))))
    meta-alist))

;;; Extractor Registry

(defvar vulpea-db--extractors nil
  "List of registered extractors.

Each extractor is a plist with:
  :name - symbol identifying the extractor
  :fn   - function taking (ctx note-data) and returning updated note-data")

(defun vulpea-db-register-extractor (name fn)
  "Register extractor with NAME and function FN.

FN should be a function taking (ctx note-data) where:
- ctx is a `vulpea-parse-ctx' structure
- note-data is the plist being built for the note

FN should return updated note-data plist.

Extractors are called in registration order after core extraction."
  (setq vulpea-db--extractors
        (append (cl-remove name vulpea-db--extractors
                           :key (lambda (e) (plist-get e :name)))
                (list (list :name name :fn fn)))))

(defun vulpea-db-unregister-extractor (name)
  "Unregister extractor with NAME."
  (setq vulpea-db--extractors
        (cl-remove name vulpea-db--extractors
                   :key (lambda (e) (plist-get e :name)))))

(defun vulpea-db--run-extractors (ctx note-data)
  "Run all registered extractors on NOTE-DATA with CTX.

Returns updated note-data after all extractors have run."
  (cl-reduce (lambda (data extractor)
               (funcall (plist-get extractor :fn) ctx data))
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
          (vulpea-db--insert-note-from-plist path 0 0 file-data)
          (setq count (1+ count))))

      ;; Insert heading-level notes
      (dolist (heading-data (vulpea-parse-ctx-heading-nodes ctx))
        (vulpea-db--insert-note-from-plist
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

(defun vulpea-db--insert-note-from-plist (path level pos data)
  "Insert note from DATA plist at PATH with LEVEL and POS.

Runs registered extractors before insertion."
  (let* ((enriched-data (vulpea-db--run-extractors
                         (make-vulpea-parse-ctx :path path)
                         data))
         (modified-at (format-time-string "%Y-%m-%d %H:%M:%S")))
    (vulpea-db--insert-note
     :id (plist-get enriched-data :id)
     :path path
     :level level
     :pos pos
     :title (plist-get enriched-data :title)
     :properties (plist-get enriched-data :properties)
     :tags (plist-get enriched-data :tags)
     :aliases (plist-get enriched-data :aliases)
     :meta (plist-get enriched-data :meta)
     :links (plist-get enriched-data :links)
     :todo (plist-get enriched-data :todo)
     :priority (plist-get enriched-data :priority)
     :scheduled (plist-get enriched-data :scheduled)
     :deadline (plist-get enriched-data :deadline)
     :closed (plist-get enriched-data :closed)
     :outline-path (plist-get enriched-data :outline-path)
     :attach-dir (plist-get enriched-data :attach-dir)
     :modified-at modified-at)))

;;; Provide

(provide 'vulpea-db-extract)
;;; vulpea-db-extract.el ends here
