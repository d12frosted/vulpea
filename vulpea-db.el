;;; vulpea-db.el --- Database layer for Vulpea -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.0.0
;; Package-Requires: ((emacs "27.1") (emacsql "4.0.0"))
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
;; Database layer for Vulpea v2.
;;
;; This module provides:
;; - Schema management
;; - Database initialization
;; - Basic CRUD operations
;; - Transaction support
;; - Plugin/extractor registry
;;
;; Design:
;; - Hybrid schema: materialized notes table + normalized tables
;; - Read-optimized: single query to get complete note
;; - Write to both: materialized and normalized tables
;; - Async-ready: non-blocking updates via file watching
;;
;;; Code:

(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'json)

;;; Customization

(defgroup vulpea-db nil
  "Database layer for Vulpea."
  :group 'vulpea)

(defcustom vulpea-db-location
  (expand-file-name "vulpea.db" user-emacs-directory)
  "Location of Vulpea database file."
  :type 'file
  :group 'vulpea-db)

(defcustom vulpea-db-index-heading-level t
  "Whether to index heading-level notes.

Can be:
- t: index all headings (default)
- nil: index only file-level notes (2-3x faster)
- function: predicate (path) -> boolean for selective indexing

For 100k+ notes, disabling heading-level indexing can provide
significant performance improvement."
  :type '(choice boolean function)
  :group 'vulpea-db)

;;; Constants

(defconst vulpea-db-version 2
  "Current database schema version.")

(defconst vulpea-db--schema
  '(;; Materialized view table (denormalized for fast retrieval)
    (notes
     [(id :not-null :primary-key)
      (path :not-null)
      (level :not-null)
      (pos :not-null)
      (title :not-null)
      (properties :not-null)       ; JSON blob
      (tags)                        ; JSON array ["tag1", "tag2"]
      (aliases)                     ; JSON array
      (meta)                        ; JSON object {key: [{type, value}]}
      (links)                       ; JSON array [{dest, type}]
      (todo)
      (priority)
      (scheduled)
      (deadline)
      (closed)
      (outline-path)
      (attach-dir)
      (created-at)
      (modified-at :not-null)]
     (:unique [path level pos]))

    ;; Normalized tables for efficient filtering
    (tags
     [(note-id :not-null)
      (tag :not-null)]
     (:primary-key [note-id tag])
     (:foreign-key [note-id] :references notes [id] :on-delete :cascade))

    (links
     [(source :not-null)
      (dest :not-null)
      (type :not-null)]
     (:primary-key [source dest type])
     (:foreign-key [source] :references notes [id] :on-delete :cascade))

    (meta
     [(note-id :not-null)
      (key :not-null)
      (value :not-null)
      (type)]                       ; 'note', 'number', 'string', 'link', nil
     (:foreign-key [note-id] :references notes [id] :on-delete :cascade))

    ;; File tracking for change detection
    (files
     [(path :not-null :primary-key)
      (hash :not-null)
      (mtime :not-null)
      (size :not-null)])

    ;; Schema versioning for migrations
    (schema-registry
     [(name :not-null :primary-key)
      (version :not-null)
      (created-at :not-null)]))
  "Database schema definition.

Uses hybrid approach:
- Materialized \`notes\` table: complete note data, fast retrieval
- Normalized tables: efficient filtering by tags/links/meta")

(defconst vulpea-db--indices
  '((idx-tags-tag tags [tag])
    (idx-tags-note tags [note-id])
    (idx-links-dest links [dest])
    (idx-links-source links [source])
    (idx-meta-key meta [key])
    (idx-meta-note meta [note-id])
    (idx-notes-path notes [path])
    (idx-notes-title notes [title])
    (idx-notes-modified notes [modified-at]))
  "Database indices for performance.")

;;; Variables

(defvar vulpea-db--connection nil
  "Database connection.")

;;; Core API

(defun vulpea-db ()
  "Return database connection, creating if necessary."
  (unless (and vulpea-db--connection
               (emacsql-live-p vulpea-db--connection))
    (setq vulpea-db--connection (vulpea-db--init)))
  vulpea-db--connection)

(defun vulpea-db-close ()
  "Close database connection."
  (when (and vulpea-db--connection
             (emacsql-live-p vulpea-db--connection))
    (emacsql-close vulpea-db--connection)
    (setq vulpea-db--connection nil)))

(defun vulpea-db-clear ()
  "Clear all data from database.

WARNING: This will delete all notes, tags, links, and metadata.
Use with caution!"
  (interactive)
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p "Clear all data from database? "))
    (let ((db (vulpea-db)))
      (emacsql-with-transaction db
        (emacsql db [:delete :from notes])
        (emacsql db [:delete :from tags])
        (emacsql db [:delete :from links])
        (emacsql db [:delete :from meta])
        (emacsql db [:delete :from files])))))

;;; Initialization

(defun vulpea-db--init ()
  "Initialize database connection and schema."
  (let ((db (emacsql-sqlite-builtin vulpea-db-location)))
    ;; Enable foreign keys
    (emacsql db [:pragma (= foreign-keys on)])

    ;; Create tables
    (vulpea-db--create-tables db)

    ;; Create indices
    (vulpea-db--create-indices db)

    ;; Register schema version
    (vulpea-db--register-schema db 'core vulpea-db-version)

    db))

(defun vulpea-db--create-tables (db)
  "Create all tables in DB if they don't exist."
  (dolist (table-spec vulpea-db--schema)
    (emacsql db [:create-table :if-not-exists $i1 $S2]
             (car table-spec)
             (cdr table-spec))))

(defun vulpea-db--create-indices (db)
  "Create all indices in DB if they don't exist."
  (dolist (index-spec vulpea-db--indices)
    (pcase-let ((`(,name ,table ,columns) index-spec))
      (emacsql db [:create-index :if-not-exists $i1 :on $i2 $S3]
               name table columns))))

(defun vulpea-db--register-schema (db name version)
  "Register schema NAME with VERSION in DB."
  (emacsql db [:insert :or :replace :into schema-registry
               :values $v1]
           (list (vector (symbol-name name)
                         version
                         (format-time-string "%Y-%m-%d %H:%M:%S")))))

;;; Utilities

(defun vulpea-db--table-exists-p (table)
  "Check if TABLE exists in database."
  (let ((table-name (symbol-name table)))
    (not (null (emacsql (vulpea-db)
                        (format "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'"
                                table-name))))))

(defun vulpea-db--index-exists-p (index)
  "Check if INDEX exists in database."
  (let ((index-name (symbol-name index)))
    (not (null (emacsql (vulpea-db)
                        (format "SELECT name FROM sqlite_master WHERE type = 'index' AND name = '%s'"
                                index-name))))))

;;; CRUD Operations

(defun vulpea-db--plist-to-alist (plist)
  "Convert PLIST to alist for JSON encoding.
Converts :key value pairs to (\"key\" . value) pairs."
  (let (result)
    (while plist
      (push (cons (substring (symbol-name (car plist)) 1)
                  (cadr plist))
            result)
      (setq plist (cddr plist)))
    (nreverse result)))

(defun vulpea-db--meta-to-json (meta)
  "Convert META to JSON-compatible alist.
META is ((key . (plist1 plist2...))...).
Converts plists to alists so json-encode creates objects."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (mapcar #'vulpea-db--plist-to-alist (cdr entry))))
          meta))

(defun vulpea-db--links-to-json (links)
  "Convert LINKS to JSON-compatible list.
LINKS is (plist1 plist2...).
Converts plists to alists so json-encode creates objects."
  (mapcar #'vulpea-db--plist-to-alist links))

(cl-defun vulpea-db--insert-note (&key id path level pos title
                                       properties tags aliases meta links
                                       todo priority scheduled deadline
                                       closed outline-path attach-dir
                                       created-at modified-at)
  "Insert note into database.

Updates both materialized notes table and normalized tables.

Arguments:
  ID - unique identifier (UUID)
  PATH - file path
  LEVEL - heading level (0 = file-level)
  POS - position in file
  TITLE - note title
  PROPERTIES - alist of properties
  TAGS - list of tags
  ALIASES - list of aliases
  META - alist of (key . values) where values is list of plists with
    :type and :value
  LINKS - list of plists with :dest and :type
  TODO - TODO state
  PRIORITY - priority level
  SCHEDULED - scheduled timestamp
  DEADLINE - deadline timestamp
  CLOSED - closed timestamp
  OUTLINE-PATH - path to heading
  ATTACH-DIR - attachment directory
  CREATED-AT - creation timestamp
  MODIFIED-AT - modification timestamp"
  (let ((db (vulpea-db)))
    (emacsql-with-transaction db
      ;; 1. Insert into materialized notes table
      (emacsql db [:insert :into notes :values $v1]
               (list
                (vector id path level pos title
                        (if properties (json-encode properties) "null")
                        (if tags (json-encode tags) "null")
                        (if aliases (json-encode aliases) "null")
                        (if meta (json-encode (vulpea-db--meta-to-json meta)) "null")
                        (if links (json-encode (vulpea-db--links-to-json links)) "null")
                        todo priority scheduled deadline closed
                        outline-path attach-dir
                        created-at modified-at)))

      ;; 2. Insert into normalized tags table
      (when tags
        (emacsql db [:insert :into tags :values $v1]
                 (mapcar (lambda (tag) (vector id tag)) tags)))

      ;; 3. Insert into normalized links table
      (when links
        (emacsql db [:insert :into links :values $v1]
                 (mapcar (lambda (link)
                           (vector id
                                   (plist-get link :dest)
                                   (plist-get link :type)))
                         links)))

      ;; 4. Insert into normalized meta table
      (when meta
        (emacsql db [:insert :into meta :values $v1]
                 (cl-loop for (key . values) in meta
                          append (mapcar (lambda (v)
                                           (vector id key
                                                   (plist-get v :value)
                                                   (plist-get v :type)))
                                         values)))))))

(defun vulpea-db--delete-file-notes (path)
  "Delete all notes from PATH.

Cascades to normalized tables automatically via foreign keys."
  (emacsql (vulpea-db)
           [:delete :from notes :where (= path $s1)]
           path))

(defun vulpea-db--delete-note (id)
  "Delete note with ID.

Cascades to normalized tables automatically via foreign keys."
  (emacsql (vulpea-db)
           [:delete :from notes :where (= id $s1)]
           id))

(defun vulpea-db--update-file-hash (path hash mtime size)
  "Update file tracking info for PATH.

HASH, MTIME and SIZE as inserted as values."
  (emacsql (vulpea-db)
           [:insert :or :replace :into files :values $v1]
           (list (vector path hash mtime size))))

(defun vulpea-db--get-file-hash (path)
  "Get stored hash for PATH.

Returns plist with :hash, :mtime, :size or nil if not tracked."
  (when-let ((row (car (emacsql (vulpea-db)
                                [:select [hash mtime size] :from files
                                 :where (= path $s1)]
                                path))))
    (list :hash (elt row 0)
          :mtime (elt row 1)
          :size (elt row 2))))

;;; Provide

(provide 'vulpea-db)
;;; vulpea-db.el ends here
