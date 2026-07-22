;;; vulpea-db.el --- Database layer for Vulpea -*- lexical-binding: t; -*-
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
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
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
(require 'ucs-normalize)

;;; Customization

(defgroup vulpea-db nil
  "Database layer for Vulpea."
  :group 'vulpea)

(defcustom vulpea-db-location
  (expand-file-name "vulpea.db" user-emacs-directory)
  "Location of Vulpea database file."
  :type 'file
  :group 'vulpea-db)

(defcustom vulpea-db-path-normalization
  (and (eq system-type 'darwin) 'nfc)
  "Unicode normalization applied to paths used as database keys.

On macOS the same file can be referred to by two strings that differ
only in Unicode normalization: filename syscalls are decoded via
`utf-8-hfs' and yield precomposed (NFC) strings, while paths arriving
from subprocesses such as fd, find or fswatch are raw decomposed
\(NFD) bytes.  HFS+ and APFS treat both as the same file, so without
canonicalization the database accumulates two independent rows per
file - one updated by each source - and consumers read stale hashes
and metadata depending on which row a query happens to hit.

When set to symbol `nfc', every path is composed to NFC before it is
used as a database key or queue key.  When nil, paths are used as
given - the correct setting for file systems where two
differently-normalized names address different files (most Linux
setups)."
  :type '(choice (const :tag "Compose to NFC" nfc)
                 (const :tag "No normalization" nil))
  :group 'vulpea-db)

(defun vulpea-db-normalize-path (path)
  "Return the canonical form of PATH for use as a database key.

Applies the normalization selected by
`vulpea-db-path-normalization'.  Returns PATH unchanged when
normalization is disabled or PATH is nil."
  (if (and path (eq vulpea-db-path-normalization 'nfc))
      (ucs-normalize-NFC-string path)
    path))

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

(defcustom vulpea-db-exclude-archived t
  "Whether to exclude archived entries from the database.

When non-nil (default), entries are excluded if they:
- Have the archive tag (value of `org-archive-tag') directly or inherited
- Have the ARCHIVE_TIME property set

Archived entries are typically historical references that don't need
to be queried. Excluding them keeps the database cleaner and faster."
  :type 'boolean
  :group 'vulpea-db)

(defcustom vulpea-db-exclude-property "VULPEA_IGNORE"
  "Property name that marks a node for exclusion from the database.

A node, whether file-level or heading-level, is excluded when it
carries this property with any value other than nil. The value is read
with `org-not-nil', mirroring how Org reads its own boolean drawer
properties such as ORDERED and UNNUMBERED, so it may even carry a
human-readable reason and still exclude the node. Setting the property
to nil, or omitting it, keeps the node indexed.

Change this to any property name you prefer, for example
\"ROAM_EXCLUDE\" to reuse the exclusion marks of an existing org-roam
collection."
  :type 'string
  :group 'vulpea-db)

(defcustom vulpea-db-extra-extensions nil
  "List of extra file extensions to track besides .org files.

Each entry is a suffix string with leading dot, e.g. \".org.age\".

Files with these extensions are always parsed using the `find-file'
method (regardless of `vulpea-db-parse-method') so that decryption
hooks (age.el, epa-file) can run.

WARNING: metadata (titles, tags, links) from encrypted files will
be stored in the plaintext database.  Only opt in if you accept
this trade-off.

Example:
  (setq vulpea-db-extra-extensions \\='(\".org.age\" \".org.gpg\"))"
  :type '(repeat string)
  :group 'vulpea-db)

;;; Constants

(defconst vulpea-db-version 5
  "Current database schema version.

Bumping this triggers a full database rebuild (the file is deleted
and recreated).  Use it only for incompatible schema changes.  For
changes to extraction logic that keep the schema intact, bump
`vulpea-db-parser-epoch' instead - it re-extracts files without
discarding the database.")

(defconst vulpea-db-parser-epoch 3
  "Epoch of the note extraction logic.

Increment this whenever the parser/extractor in `vulpea-db-extract'
changes what it produces from the same file content (e.g. a bug fix
that makes more notes recognizable).  On the next database access the
file change cache is cleared and all files are re-extracted, so users
pick up the improved parsing without a manual force scan or a full
schema rebuild.  Unlike `vulpea-db-version', the database and its
notes are preserved.")

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
      (meta)                        ; JSON object {key: [value1, value2]}
      (links)                       ; JSON array [{dest, type, pos, description}]
      (todo)
      (priority)
      (scheduled)
      (deadline)
      (closed)
      (category)                    ; Resolved org category (never null in practice)
      (outline-path)
      (attach-dir)
      (file-title)                  ; Title of file containing this note
      (created-at)
      (modified-at :not-null)
      ;; Where title came from: keyword | heading | filename.
      ;; Nullable: nil means unknown, not untitled.  New columns are
      ;; only ever appended - rows are decoded positionally.
      (title-source)]
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
      (type :not-null)
      (pos :not-null)
      description]
     (:primary-key [source dest type pos])
     (:foreign-key [source] :references notes [id] :on-delete :cascade))

    (meta
     [(note-id :not-null)
      (key :not-null)
      (value :not-null)]
     (:foreign-key [note-id] :references notes [id] :on-delete :cascade))

    (properties
     [(note-id :not-null)
      (key :not-null)
      (value :not-null)]
     (:primary-key [note-id key])
     (:foreign-key [note-id] :references notes [id] :on-delete :cascade))

    ;; File tracking for change detection
    (files
     [(path :not-null :primary-key)
      (hash :not-null)
      (mtime :not-null)
      (size :not-null)])

    ;; Dir-locals tracking for re-index on change.  A side table
    ;; rather than a marker column in `files': removed-file detection
    ;; and the startup scan iterate `files' expecting org files, and
    ;; must never treat a dir-locals file as a removed note.
    (dir-locals-files
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
- Materialized notes table: complete note data, fast retrieval
- Normalized tables: efficient filtering by tags/links/meta")

(defconst vulpea-db--indices
  '((idx-tags-tag tags [tag])
    (idx-tags-note tags [note-id])
    (idx-links-dest links [dest])
    (idx-links-source links [source])
    (idx-meta-key meta [key])
    (idx-meta-note meta [note-id])
    (idx-properties-key properties [key])
    (idx-properties-note properties [note-id])
    (idx-notes-path notes [path])
    (idx-notes-title notes [title])
    (idx-notes-modified notes [modified-at])
    (idx-notes-created notes [created-at])
    (idx-notes-category notes [category]))
  "Database indices for performance.")

;;; Variables

(defvar vulpea-db--connection nil
  "Database connection.")

(defvar vulpea-db--schema-rebuilt nil
  "Non-nil if schema was rebuilt during last init.
Checked by `vulpea-db-sync--start' to trigger automatic re-index.")

(defvar vulpea-db--settings-changed nil
  "Non-nil if extraction settings changed since last init.
See `vulpea-db--settings-fingerprint' for what is tracked.
Checked by `vulpea-db-sync--start' to trigger automatic re-index.")

(defvar vulpea-db--parser-changed nil
  "Non-nil if the parser epoch changed since last init.
Checked by `vulpea-db-sync--start' to trigger automatic re-index.")

(defvar vulpea-db--plugin-schema-changed nil
  "Non-nil if a plugin extractor migrated its schema.
Set by `vulpea-db--apply-plugin-schema' on a version increase.
Checked by `vulpea-db-sync--start' to trigger automatic re-index.")

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
        (emacsql db [:delete :from files])
        (emacsql db [:delete :from dir-locals-files])
        ;; The dir-locals baseline describes the (now deleted) rows,
        ;; not code state like the other registry entries; keeping it
        ;; would make the post-clear rescan treat every dir-locals
        ;; file as newly created and fire spurious re-index reactions
        (emacsql db [:delete :from schema-registry
                     :where (= name "dir-locals-tracking")])))))

;;; Initialization

(defun vulpea-db--needs-rebuild-p (db)
  "Return non-nil if DB schema version doesn't match `vulpea-db-version'."
  (condition-case nil
      (let ((stored (caar (emacsql db
                                   [:select [version] :from schema-registry
                                    :where (= name "core")]))))
        (and stored (not (= stored vulpea-db-version))))
    ;; schema-registry doesn't exist → brand new DB, no rebuild needed
    (error nil)))

(defun vulpea-db--settings-fingerprint ()
  "Compute a fingerprint of extraction-relevant settings.

Returns the `sxhash' of the settings whose value changes what
extraction produces from the same file content: tag inheritance
\(`org-use-tag-inheritance', `org-tags-exclude-from-inheritance')
and `vulpea-db-parse-method' (dir- and file-local variables feeding
`org-category' reach extraction under some methods and not others).
Used to detect when these settings change between sessions so the
DB can be re-indexed.

`vulpea-db-parse-method' is read guarded: it is defined in
vulpea-db-extract, which requires this file, so it may be unbound
when only the db layer is loaded.  Its name is hashed rather than
the symbol itself: `sxhash' of a regular symbol is address-based
and differs across processes (t, nil, strings, and numbers are
stable), and this fingerprint is compared across sessions and by
the extraction worker."
  (sxhash (list org-use-tag-inheritance
                org-tags-exclude-from-inheritance
                (when (bound-and-true-p vulpea-db-parse-method)
                  (symbol-name vulpea-db-parse-method)))))

(defun vulpea-db--settings-changed-p (db)
  "Return non-nil if extraction settings differ from those stored in DB."
  (condition-case nil
      (let ((stored (caar (emacsql db
                                   [:select [version] :from schema-registry
                                    :where (= name "settings")]))))
        (and stored
             (not (equal stored (vulpea-db--settings-fingerprint)))))
    (error nil)))

(defun vulpea-db--parser-epoch-changed-p (db)
  "Return non-nil if the epoch stored in DB differs from the current one.

When no epoch is recorded, the result depends on whether DB already
has cached files: an existing database (non-empty `files' table) is
treated as stale so upgrading to an epoch-aware vulpea re-extracts it
once, while a brand-new database (empty `files' table) is left alone."
  (condition-case nil
      (let ((stored (caar (emacsql db
                                   [:select [version] :from schema-registry
                                    :where (= name "parser-epoch")]))))
        (if stored
            (not (equal stored vulpea-db-parser-epoch))
          (< 0 (caar (emacsql db [:select (funcall count *) :from files])))))
    (error nil)))

(defun vulpea-db--init ()
  "Initialize database connection and schema."
  ;; Ensure the parent directory exists, otherwise emacsql fails with an
  ;; opaque `(sqlitep nil)' error. See vulpea#271.
  (make-directory (file-name-directory vulpea-db-location) t)
  (let ((db (emacsql-sqlite-builtin vulpea-db-location)))
    ;; Enable foreign keys
    (emacsql db [:pragma (= foreign-keys on)])

    ;; Check if schema version mismatches
    (when (vulpea-db--needs-rebuild-p db)
      (emacsql-close db)
      (delete-file vulpea-db-location)
      (setq db (emacsql-sqlite-builtin vulpea-db-location))
      (emacsql db [:pragma (= foreign-keys on)])
      (setq vulpea-db--schema-rebuilt t)
      (message "Vulpea: Schema version changed, rebuilding database..."))

    ;; Create tables
    (vulpea-db--create-tables db)

    ;; Create indices
    (vulpea-db--create-indices db)

    ;; Check if extraction settings changed
    (when (vulpea-db--settings-changed-p db)
      (emacsql db [:delete :from files])
      (setq vulpea-db--settings-changed t)
      (message "Vulpea: Extraction settings changed, re-index needed..."))

    ;; Check if the parser epoch changed (extraction logic updated, or
    ;; an existing pre-epoch database).  Clearing the files cache forces
    ;; every file to be re-extracted, healing notes that an older parser
    ;; failed to recognize. See vulpea#277.
    (when (vulpea-db--parser-epoch-changed-p db)
      (emacsql db [:delete :from files])
      (setq vulpea-db--parser-changed t)
      (message "Vulpea: Parser updated, re-index needed..."))

    ;; Register schema version, settings fingerprint and parser epoch
    (vulpea-db--register-schema db 'core vulpea-db-version)
    (vulpea-db--register-schema db 'settings
                                (vulpea-db--settings-fingerprint))
    (vulpea-db--register-schema db 'parser-epoch vulpea-db-parser-epoch)

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

;; NOTE: `sqlite_master' is populated by SQLite with plain strings,
;; whereas emacsql encodes string parameters in their `prin1' form
;; (e.g. "notes" becomes the quoted "notes"), which would never match.
;; Passing the type and name as symbols makes emacsql emit them as the
;; bare values 'table'/'notes', which do match - while still escaping
;; them through emacsql's parameter machinery rather than string
;; interpolation, so the name cannot break out of the query.

(defun vulpea-db--sql-name (name)
  "Return NAME as the identifier stored in `sqlite_master'.
Emacsql identifier escaping converts dashes to underscores when it
creates tables and indices, so an object declared as `my-table'
exists as `my_table'.  NAME is a symbol; the result is a symbol so
existence checks can pass it as a scalar parameter (see NOTE
above)."
  (intern (replace-regexp-in-string "-" "_" (symbol-name name))))

(defun vulpea-db--table-exists-p (table)
  "Check if TABLE exists in database.
TABLE is a symbol; dashes and underscores are interchangeable, so
`my-table' finds the table emacsql created as `my_table'."
  (not (null (emacsql (vulpea-db)
                      [:select [name] :from sqlite-master
                       :where (and (= type $s1)
                                   (= name $s2))]
                      'table (vulpea-db--sql-name table)))))

(defun vulpea-db--index-exists-p (index)
  "Check if INDEX exists in database.
INDEX is a symbol; dashes and underscores are interchangeable, so
`my-index' finds the index emacsql created as `my_index'."
  (not (null (emacsql (vulpea-db)
                      [:select [name] :from sqlite-master
                       :where (and (= type $s1)
                                   (= name $s2))]
                      'index (vulpea-db--sql-name index)))))

(defun vulpea-db--all-extensions ()
  "Return all tracked file extensions (`.org' + extras)."
  (cons ".org" vulpea-db-extra-extensions))

(defun vulpea-db--escape-glob-pattern (str)
  "Escape special SQLite GLOB characters in STR.
Escapes *, ?, and [ so they match literally when used with GLOB.
These are escaped by wrapping in brackets: * -> [*], ? -> [?], [ -> [[]]."
  (replace-regexp-in-string
   "[][*?]"
   (lambda (m) (format "[%s]" m))
   str))

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

META is ((key . (value1 value2...))...)."
  (mapcar (lambda (entry)
            (cons (car entry) (cdr entry)))
          meta))

(defun vulpea-db--links-to-json (links)
  "Convert LINKS to JSON-compatible list.

LINKS is (plist1 plist2...).
Converts plists to alists so `json-encode' creates objects."
  (mapcar #'vulpea-db--plist-to-alist links))

(defun vulpea-db--bind-scalar (value)
  "Encode VALUE for native parameter binding.

Matches the storage format of `emacsql-escape-scalar' exactly: nil
maps to NULL, numbers are stored as SQL numbers, and any other value
is stored as its readable-print form so emacsql reads it back
unchanged.  Keeping the format identical makes rows written through
`sqlite-execute' byte-compatible with rows written through emacsql."
  (cond ((null value) nil)
        ((numberp value) value)
        (t (let ((print-escape-newlines t)
                 (print-escape-control-characters t))
             (prin1-to-string value)))))

(defun vulpea-db--insert-rows (handle sql rows)
  "Execute insert SQL on HANDLE once per row in ROWS.

Each row is a list of raw Lisp values, encoded for binding via
`vulpea-db--bind-scalar'.  Native parameter binding bypasses
emacsql's statement compilation, which dominates insert cost when
indexing files with many notes (issue #359)."
  (dolist (row rows)
    (sqlite-execute handle sql (mapcar #'vulpea-db--bind-scalar row))))

(cl-defun vulpea-db--insert-note (&key id path level pos title
                                       properties tags aliases meta links
                                       todo priority scheduled deadline
                                       closed category outline-path attach-dir
                                       file-title created-at modified-at
                                       title-source)
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
  LINKS - list of plists with :dest, :type, :pos, and :description
  TODO - TODO state
  PRIORITY - priority level
  SCHEDULED - scheduled timestamp
  DEADLINE - deadline timestamp
  CLOSED - closed timestamp
  CATEGORY - resolved org category
  OUTLINE-PATH - path to heading
  ATTACH-DIR - attachment directory
  FILE-TITLE - title of the file containing this note
  CREATED-AT - creation timestamp
  MODIFIED-AT - modification timestamp
  TITLE-SOURCE - where the title comes from: symbol `keyword',
    `heading' or `filename'; nil when unknown"
  ;; All inserts use OR IGNORE: emacsql-sqlite-builtin silently
  ;; dropped constraint-violating statements (sqlite-select swallows
  ;; step errors), so messy data - duplicate IDs, duplicate property
  ;; keys - never failed indexing.  OR IGNORE preserves that tolerance
  ;; but at row granularity: a violating row is skipped instead of
  ;; taking its sibling rows down with the whole statement.
  (setq path (vulpea-db-normalize-path path))
  (let* ((db (vulpea-db))
         (handle (oref db handle)))
    (emacsql-with-transaction db
      ;; 1. Insert into materialized notes table
      (vulpea-db--insert-rows
       handle
       "INSERT OR IGNORE INTO notes (id, path, level, pos, title, properties, tags,
                           aliases, meta, links, todo, priority, scheduled,
                           deadline, closed, category, outline_path, attach_dir,
                           file_title, created_at, modified_at, title_source)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
       (list
        (list id path level pos title
              (vulpea-db--encode-note-column :properties properties)
              (vulpea-db--encode-note-column :tags tags)
              (vulpea-db--encode-note-column :aliases aliases)
              (vulpea-db--encode-note-column :meta meta)
              (vulpea-db--encode-note-column :links links)
              todo priority scheduled deadline closed category
              outline-path attach-dir file-title
              created-at modified-at title-source)))

      ;; 2-5. Insert into normalized tables
      (vulpea-db--insert-tag-rows handle id tags)
      (vulpea-db--insert-link-rows handle id links)
      (vulpea-db--insert-meta-rows handle id meta)
      (vulpea-db--insert-property-rows handle id properties))))

(defun vulpea-db--encode-note-column (field value)
  "Encode VALUE of FIELD for its materialized notes column.
JSON-blob fields (:properties, :tags, :aliases, :meta, :links) are
encoded to their JSON string; any other field is stored as is."
  (pcase field
    (:properties (if value (json-encode value) "null"))
    (:tags (if value (json-encode value) "null"))
    (:aliases (if value (json-encode value) "null"))
    (:meta (if value (json-encode (vulpea-db--meta-to-json value)) "null"))
    (:links (if value (json-encode (vulpea-db--links-to-json value)) "null"))
    (_ value)))

(defun vulpea-db--insert-tag-rows (handle id tags)
  "Insert TAGS of note ID into the normalized tags table via HANDLE."
  (when tags
    (let ((unique-tags (delete-dups (copy-sequence tags))))
      (vulpea-db--insert-rows
       handle
       "INSERT OR IGNORE INTO tags (note_id, tag) VALUES (?,?)"
       (mapcar (lambda (tag) (list id tag)) unique-tags)))))

(defun vulpea-db--insert-link-rows (handle id links)
  "Insert LINKS of note ID into the normalized links table via HANDLE."
  (when links
    (vulpea-db--insert-rows
     handle
     "INSERT OR IGNORE INTO links (source, dest, type, pos, description)
      VALUES (?,?,?,?,?)"
     (mapcar (lambda (link)
               (list id
                     (plist-get link :dest)
                     (plist-get link :type)
                     (plist-get link :pos)
                     (plist-get link :description)))
             links))))

(defun vulpea-db--insert-meta-rows (handle id meta)
  "Insert META of note ID into the normalized meta table via HANDLE."
  (when meta
    (vulpea-db--insert-rows
     handle
     "INSERT OR IGNORE INTO meta (note_id, key, value) VALUES (?,?,?)"
     (cl-loop for (key . values) in meta
              append (mapcar (lambda (v)
                               (list id key v))
                             values)))))

(defun vulpea-db--insert-property-rows (handle id properties)
  "Insert PROPERTIES of note ID into the normalized table via HANDLE."
  (when properties
    (vulpea-db--insert-rows
     handle
     "INSERT OR IGNORE INTO properties (note_id, key, value) VALUES (?,?,?)"
     (cl-loop for (key . value) in properties
              collect (list id key value)))))

(defconst vulpea-db--note-field-columns
  '((:title . "title")
    (:properties . "properties")
    (:tags . "tags")
    (:aliases . "aliases")
    (:meta . "meta")
    (:links . "links")
    (:todo . "todo")
    (:priority . "priority")
    (:scheduled . "scheduled")
    (:deadline . "deadline")
    (:closed . "closed")
    (:category . "category")
    (:outline-path . "outline_path")
    (:attach-dir . "attach_dir")
    (:file-title . "file_title")
    (:created-at . "created_at")
    (:title-source . "title_source"))
  "Mapping of updatable note-data fields to notes table columns.
Identity fields (:id, :path, :level, :pos) are deliberately absent -
they anchor foreign keys and the file association and must not be
rewritten after insertion.")

(defun vulpea-db--update-note-fields (id fields)
  "Persist FIELDS of the note ID across both storage forms.

FIELDS is an alist of (FIELD . VALUE) where FIELD is a note-data
keyword listed in `vulpea-db--note-field-columns'.  Each entry
rewrites the materialized notes column; fields with a normalized
table (:tags, :links, :meta, :properties) additionally have their
rows replaced with rows built from VALUE.  Fields not in the mapping
are ignored.

This is how extractor-plugin contributions to core fields reach the
database: the note row is inserted before extractors run (their
tables hold foreign keys into it), so whatever they change in
note-data is written as an update afterwards."
  (when fields
    (let* ((db (vulpea-db))
           (handle (oref db handle)))
      (emacsql-with-transaction db
        (pcase-dolist (`(,field . ,value) fields)
          (when-let* ((column (cdr (assq field vulpea-db--note-field-columns))))
            (sqlite-execute
             handle
             (format "UPDATE notes SET %s = ? WHERE id = ?" column)
             (list (vulpea-db--bind-scalar
                    (vulpea-db--encode-note-column field value))
                   (vulpea-db--bind-scalar id)))
            (pcase field
              (:tags
               (sqlite-execute handle "DELETE FROM tags WHERE note_id = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-tag-rows handle id value))
              (:links
               (sqlite-execute handle "DELETE FROM links WHERE source = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-link-rows handle id value))
              (:meta
               (sqlite-execute handle "DELETE FROM meta WHERE note_id = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-meta-rows handle id value))
              (:properties
               (sqlite-execute handle "DELETE FROM properties WHERE note_id = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-property-rows handle id value)))))))))

(defun vulpea-db--delete-file-notes (path)
  "Delete all notes from PATH.

Cascades to normalized tables automatically via foreign keys."
  (emacsql (vulpea-db)
           [:delete :from notes :where (= path $s1)]
           (vulpea-db-normalize-path path)))

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
           (list (vector (vulpea-db-normalize-path path) hash mtime size))))

(defun vulpea-db--get-file-hash (path)
  "Get stored hash for PATH.

Returns plist with :hash, :mtime, :size or nil if not tracked."
  (when-let* ((row (car (emacsql (vulpea-db)
                                [:select [hash mtime size] :from files
                                 :where (= path $s1)]
                                (vulpea-db-normalize-path path)))))
    (list :hash (elt row 0)
          :mtime (elt row 1)
          :size (elt row 2))))

(defun vulpea-db--update-dir-locals-hash (path hash mtime size)
  "Update dir-locals tracking info for PATH.

HASH, MTIME and SIZE are inserted as values."
  (emacsql (vulpea-db)
           [:insert :or :replace :into dir-locals-files :values $v1]
           (list (vector (vulpea-db-normalize-path path) hash mtime size))))

(defun vulpea-db--get-dir-locals-hash (path)
  "Get stored hash for the dir-locals file at PATH.

Returns plist with :hash, :mtime, :size or nil if not tracked."
  (when-let* ((row (car (emacsql (vulpea-db)
                                 [:select [hash mtime size]
                                  :from dir-locals-files
                                  :where (= path $s1)]
                                 (vulpea-db-normalize-path path)))))
    (list :hash (elt row 0)
          :mtime (elt row 1)
          :size (elt row 2))))

(defun vulpea-db--delete-dir-locals-hash (path)
  "Stop tracking the dir-locals file at PATH."
  (emacsql (vulpea-db)
           [:delete :from dir-locals-files :where (= path $s1)]
           (vulpea-db-normalize-path path)))

(defun vulpea-db--dir-locals-paths ()
  "Return paths of all tracked dir-locals files."
  (mapcar #'car (emacsql (vulpea-db)
                         [:select path :from dir-locals-files])))

;;; Provide

(provide 'vulpea-db)
;;; vulpea-db.el ends here
