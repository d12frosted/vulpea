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

(declare-function vulpea-db--unregister-id-locations "vulpea-db-extract"
                  (ids path))
(declare-function vulpea-db--resolve-released-ids "vulpea-db-extract"
                  (released-ids &optional releasing-path))

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

In a file whose headings are not indexed, a heading with an ID is
not a note, just part of the file-level note: links written under it
count as the file note's.

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
collection. The name is matched case-insensitively, as org does with
property names.

Adding or removing the property in a file takes effect on the next
sync, like any other edit. Changing this name does not, since the
files have not changed - apply that to an existing collection with
\\[universal-argument] \\[vulpea-db-sync-full-scan]."
  :type 'string
  :group 'vulpea-db)

(defcustom vulpea-db-exclude-children-property "VULPEA_IGNORE_CHILDREN"
  "Property name that keeps a node's descendants out of the database.

The node carrying it is indexed as usual; every heading below it is
skipped, however deep.  This is the marker for a container whose
children are noise: a Meetings heading worth finding by itself, holding
hundreds of individual meetings that are not.

In a file-level property drawer it applies to the whole file, so the
file-level note is indexed and no heading in it is.  Unlike
`vulpea-db-index-heading-level' set to nil, which folds headings into
the file-level note, this is an exclusion: the skipped headings take
their links out of the database as described below.

The value is read with `org-not-nil', like `vulpea-db-exclude-property',
so any value other than nil excludes the descendants and can double as a
human-readable reason.  The two properties are independent: this one
says nothing about the node carrying it, and
`vulpea-db-exclude-property' says nothing about its descendants.

Skipped headings keep whatever IDs they have, so `org-id' still resolves
links to them; they are simply absent from the database, which means no
backlinks, no `vulpea-db-get-by-id' and no query results.  Only indexed
IDs are registered with `org-id', so resolving one of these costs a
rescan the first time.  Links written inside them
go too, so notes they used to link to lose those backlinks - the
subtree leaves the graph in both directions.  A skipped heading without
an ID of its own is not a note boundary, so its links belong to the
nearest indexed ancestor and stay in the database as that ancestor's."
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

(defconst vulpea-db-version 6
  "Current database schema version.

Bumping this triggers a full database rebuild (the file is deleted
and recreated).  Use it only for incompatible schema changes.  For
changes to extraction logic that keep the schema intact, bump
`vulpea-db-parser-epoch' instead - it re-extracts files without
discarding the database.")

(defconst vulpea-db-parser-epoch 7
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
      (title-source)
      ;; Where category came from: property | keyword | variable |
      ;; filename.  Nullable: nil means unknown.
      (category-source)]
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

    ;; Pending id claims: files whose latest parse contained an id
    ;; that another file still owned, so the OR IGNORE insert dropped
    ;; the note (vulpea#469 - a heading refiled with the destination
    ;; saved first).  The claim survives until the owning file
    ;; releases the id - the claimant is then re-indexed and wins it
    ;; - or until the claimant's parse stops containing the id.  No
    ;; foreign key: a claimed id has no notes row for the claimant by
    ;; definition.
    (pending-claims
     [(id :not-null)
      (path :not-null)]
     (:primary-key [id path]))

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
\(`org-use-tag-inheritance', `org-tags-exclude-from-inheritance'),
`vulpea-db-parse-method' (dir- and file-local variables feeding
`org-category' reach extraction under some methods and not others),
and `vulpea-buffer-alias-property' (names the property aliases are
read from).  Used to detect when these settings change between
sessions so the DB can be re-indexed.

`vulpea-db-parse-method' and `vulpea-buffer-alias-property' are
read guarded: they are defined in modules that require this file,
so they may be unbound when only the db layer is loaded.  The parse
method's name is hashed rather than the symbol itself: `sxhash' of
a regular symbol is address-based and differs across processes (t,
nil, strings, and numbers are stable), and this fingerprint is
compared across sessions and by the extraction worker."
  (sxhash (list org-use-tag-inheritance
                org-tags-exclude-from-inheritance
                (when (bound-and-true-p vulpea-db-parse-method)
                  (symbol-name vulpea-db-parse-method))
                (bound-and-true-p vulpea-buffer-alias-property))))

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
`sqlite-execute' byte-compatible with rows written through emacsql.

Most strings print as themselves between double quotes; those skip
the printer, which is the bulk of the encoding cost when indexing
\(see `vulpea-db--verbatim-string-p')."
  (cond ((null value) nil)
        ((numberp value) value)
        ((and (stringp value) (vulpea-db--verbatim-string-p value))
         (concat "\"" value "\""))
        (t (let ((print-escape-newlines t)
                 (print-escape-control-characters t))
             (prin1-to-string value)))))

(defconst vulpea-db--print-escaped-re
  (concat "[\"\\\0-\37\177" (string #x3fff80) "-" (string #x3fffff) "]")
  "Characters the printer escapes in a string.
Double quote and backslash, control characters (escaped under
`print-escape-control-characters'), and raw bytes, which is also
what the bytes of a unibyte string above 127 match.")

(defun vulpea-db--verbatim-string-p (string)
  "Return non-nil when STRING prints as itself between quotes.
That is, `prin1-to-string' with the settings of
`vulpea-db--bind-scalar' returns STRING wrapped in double quotes: it
has no text properties, no character the printer escapes, and no
non-ASCII character the current printer settings would escape."
  (and (not (string-match-p vulpea-db--print-escaped-re string))
       (or (not print-escape-multibyte)
           (not (string-match-p "[^\0-\177]" string)))
       (not (text-properties-at 0 string))
       (not (next-property-change 0 string))))

(defvar vulpea-db--max-bind-params 999
  "Most parameters `vulpea-db--insert-rows' binds in one statement.
999 is SQLITE_MAX_VARIABLE_NUMBER of SQLite before 3.32.  Newer
builds allow 32766, but Emacs links whatever SQLite the system
provides, so the batch size stays within the oldest limit.")

(defun vulpea-db--insert-rows (handle head width rows)
  "Insert ROWS on HANDLE, many rows per statement.

HEAD is the statement up to its VALUES clause, for example
\"INSERT OR IGNORE INTO tags (note_id, tag)\", and WIDTH the number
of columns it names.  Each row is a list of WIDTH raw Lisp values,
encoded for binding via `vulpea-db--bind-scalar'.  Rows go out in
order, as many per statement as `vulpea-db--max-bind-params'
allows, so rowids come out exactly as with one statement per row.
OR IGNORE keeps working per row: a violating row is skipped, its
siblings in the same statement are inserted.

Native parameter binding bypasses emacsql's statement compilation
\(issue #359), and a multi-row statement pays SQLite's prepare and
step once for many rows; both dominate insert cost when indexing
files with many notes."
  (when rows
    (let* ((per-statement (max 1 (/ vulpea-db--max-bind-params width)))
           (tuple (concat "(" (mapconcat #'identity (make-list width "?") ",") ")"))
           (full-sql nil))
      (while rows
        (let ((count 0)
              (params nil))
          (while (and rows (< count per-statement))
            (dolist (value (car rows))
              (push (vulpea-db--bind-scalar value) params))
            (setq rows (cdr rows)
                  count (1+ count)))
          (sqlite-execute
           handle
           (if (and full-sql (= count per-statement))
               full-sql
             (let ((sql (concat head " VALUES "
                                (mapconcat #'identity
                                           (make-list count tuple) ","))))
               (when (= count per-statement)
                 (setq full-sql sql))
               sql))
           (nreverse params)))))))

(defun vulpea-db--insert-note (&rest note)
  "Insert NOTE into database.

Updates both materialized notes table and normalized tables.  NOTE
is a plist of the following keys; see `vulpea-db--insert-notes' for
inserting many notes at once.

  :id - unique identifier (UUID)
  :path - file path
  :level - heading level (0 = file-level)
  :pos - position in file
  :title - note title
  :properties - alist of properties
  :tags - list of tags
  :aliases - list of aliases
  :meta - alist of (key . values) where values is a list of strings
  :links - list of plists with :dest, :type, :pos, and :description
  :todo - TODO state
  :priority - priority level
  :scheduled - scheduled timestamp
  :deadline - deadline timestamp
  :closed - closed timestamp
  :category - resolved org category
  :outline-path - path to heading
  :attach-dir - attachment directory
  :file-title - title of the file containing this note
  :created-at - creation timestamp
  :modified-at - modification timestamp
  :title-source - where the title comes from: symbol `keyword',
    `heading' or `filename'; nil when unknown
  :category-source - where the category comes from: symbol
    `property', `keyword', `variable' or `filename'; nil when unknown"
  (vulpea-db--insert-notes (list note)))

(defun vulpea-db--insert-notes (notes)
  "Insert NOTES into database in one transaction.

NOTES is a list of plists with the keys `vulpea-db--insert-note'
takes.  Stores exactly what inserting them one by one in order
stores, with the same rowid order in every table, but with a
handful of multi-row statements per table instead of one statement
per row (see `vulpea-db--insert-rows').

All inserts use OR IGNORE: `emacsql-sqlite-builtin' silently dropped
constraint-violating statements (`sqlite-select' swallows step
errors), so messy data - duplicate IDs, duplicate property keys -
never failed indexing.  OR IGNORE preserves that tolerance at row
granularity: a violating row is skipped, the first note inserted
with an id keeps it.

A row that already holds the id of one of NOTES under another path,
whose file no longer exists, is stale (the file was moved or deleted
and its removal was missed); it is evicted so the insert wins.  When
that other file still exists the id is genuinely duplicated and the
stored row keeps it.  Only rows stored before the call are
considered, so NOTES must not claim one id from two paths.

Returns the ids of NOTES left with other files: the ids the batch
lost to duplicates, which a re-indexed file records as pending
claims."
  (when notes
    (let* ((db (vulpea-db))
           (handle (oref db handle))
           (raw-path nil)
           (norm-path nil)
           (lost nil)
           (notes
            (mapcar (lambda (note)
                      (let ((path (plist-get note :path)))
                        ;; Batches come from one file: normalize once
                        (unless (and raw-path (equal path raw-path))
                          (setq raw-path path
                                norm-path (vulpea-db-normalize-path path)))
                        (plist-put (copy-sequence note) :path norm-path)))
                    notes)))
      (emacsql-with-transaction db
        (setq lost (vulpea-db--evict-stale-ids notes))

        ;; 1. Materialized notes table
        (vulpea-db--insert-rows
         handle
         "INSERT OR IGNORE INTO notes (id, path, level, pos, title, properties,
                            tags, aliases, meta, links, todo, priority,
                            scheduled, deadline, closed, category,
                            outline_path, attach_dir, file_title,
                            created_at, modified_at, title_source,
                            category_source)"
         23
         (mapcar #'vulpea-db--note-row notes))

        ;; 2-5. Normalized tables
        (vulpea-db--insert-tag-rows
         handle
         (cl-loop for note in notes
                  append (vulpea-db--tag-rows
                          (plist-get note :id) (plist-get note :tags))))
        (vulpea-db--insert-link-rows
         handle
         (cl-loop for note in notes
                  append (vulpea-db--link-rows
                          (plist-get note :id) (plist-get note :links))))
        (vulpea-db--insert-meta-rows
         handle
         (cl-loop for note in notes
                  append (vulpea-db--meta-rows
                          (plist-get note :id) (plist-get note :meta))))
        (vulpea-db--insert-property-rows
         handle
         (cl-loop for note in notes
                  append (vulpea-db--property-rows
                          (plist-get note :id)
                          (plist-get note :properties)))))
      lost)))

(defun vulpea-db--evict-stale-ids (notes)
  "Delete stored rows holding ids of NOTES whose file is gone.

A row holding the id of a note under another path is stale when no
file exists at that path anymore; it is deleted so the note can take
the id.  When the file does exist the row keeps the id.  Returns the
ids kept that way: the ids NOTES lose to other files.  See
`vulpea-db--insert-notes'."
  (let ((paths (make-hash-table :test #'equal :size (length notes)))
        (ids nil)
        (lost nil))
    (dolist (note notes)
      (when-let* ((id (plist-get note :id)))
        (unless (gethash id paths)
          (push id ids))
        (puthash id (plist-get note :path) paths)))
    (when ids
      (pcase-dolist (`(,id ,existing-path)
                     (emacsql (vulpea-db)
                              [:select [id path] :from notes
                               :where (in id $v1)]
                              (vconcat ids)))
        (unless (equal existing-path (gethash id paths))
          (if (file-exists-p existing-path)
              (push id lost)
            (vulpea-db--delete-note id)))))
    lost))

(defun vulpea-db--note-row (note)
  "Return the notes table row for NOTE, a plist.
Columns are in the order of the insert in `vulpea-db--insert-notes'."
  (list (plist-get note :id)
        (plist-get note :path)
        (plist-get note :level)
        (plist-get note :pos)
        (plist-get note :title)
        (vulpea-db--encode-note-column :properties (plist-get note :properties))
        (vulpea-db--encode-note-column :tags (plist-get note :tags))
        (vulpea-db--encode-note-column :aliases (plist-get note :aliases))
        (vulpea-db--encode-note-column :meta (plist-get note :meta))
        (vulpea-db--encode-note-column :links (plist-get note :links))
        (plist-get note :todo)
        (plist-get note :priority)
        (plist-get note :scheduled)
        (plist-get note :deadline)
        (plist-get note :closed)
        (plist-get note :category)
        (plist-get note :outline-path)
        (plist-get note :attach-dir)
        (plist-get note :file-title)
        (plist-get note :created-at)
        (plist-get note :modified-at)
        (plist-get note :title-source)
        (plist-get note :category-source)))

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

(defun vulpea-db--tag-rows (id tags)
  "Return tags table rows for TAGS of note ID, duplicates dropped."
  (mapcar (lambda (tag) (list id tag))
          (delete-dups (copy-sequence tags))))

(defun vulpea-db--link-rows (id links)
  "Return links table rows for LINKS of note ID."
  (mapcar (lambda (link)
            (list id
                  (plist-get link :dest)
                  (plist-get link :type)
                  (plist-get link :pos)
                  (plist-get link :description)))
          links))

(defun vulpea-db--meta-rows (id meta)
  "Return meta table rows for META of note ID."
  (cl-loop for (key . values) in meta
           append (mapcar (lambda (v) (list id key v)) values)))

(defun vulpea-db--property-rows (id properties)
  "Return properties table rows for PROPERTIES of note ID."
  (cl-loop for (key . value) in properties
           collect (list id key value)))

(defun vulpea-db--insert-tag-rows (handle rows)
  "Insert tags table ROWS via HANDLE."
  (vulpea-db--insert-rows
   handle "INSERT OR IGNORE INTO tags (note_id, tag)" 2 rows))

(defun vulpea-db--insert-link-rows (handle rows)
  "Insert links table ROWS via HANDLE."
  (vulpea-db--insert-rows
   handle "INSERT OR IGNORE INTO links (source, dest, type, pos, description)"
   5 rows))

(defun vulpea-db--insert-meta-rows (handle rows)
  "Insert meta table ROWS via HANDLE."
  (vulpea-db--insert-rows
   handle "INSERT OR IGNORE INTO meta (note_id, key, value)" 3 rows))

(defun vulpea-db--insert-property-rows (handle rows)
  "Insert properties table ROWS via HANDLE."
  (vulpea-db--insert-rows
   handle "INSERT OR IGNORE INTO properties (note_id, key, value)" 3 rows))

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
    (:title-source . "title_source")
    (:category-source . "category_source"))
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
               (vulpea-db--insert-tag-rows
                handle (vulpea-db--tag-rows id value)))
              (:links
               (sqlite-execute handle "DELETE FROM links WHERE source = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-link-rows
                handle (vulpea-db--link-rows id value)))
              (:meta
               (sqlite-execute handle "DELETE FROM meta WHERE note_id = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-meta-rows
                handle (vulpea-db--meta-rows id value)))
              (:properties
               (sqlite-execute handle "DELETE FROM properties WHERE note_id = ?"
                               (list (vulpea-db--bind-scalar id)))
               (vulpea-db--insert-property-rows
                handle (vulpea-db--property-rows id value))))))))))

(defun vulpea-db--delete-file-notes (path)
  "Delete all notes from PATH.

Cascades to normalized tables automatically via foreign keys."
  (emacsql (vulpea-db)
           [:delete :from notes :where (= path $s1)]
           (vulpea-db-normalize-path path)))

(defun vulpea-db--get-file-note-ids (path)
  "Return ids of the notes stored for PATH."
  (mapcar #'car (emacsql (vulpea-db)
                         [:select id :from notes :where (= path $s1)]
                         (vulpea-db-normalize-path path))))

(defun vulpea-db--get-pending-claims (&optional id)
  "Return pending id claims.

With ID, return the paths of the files claiming it.  Without ID,
return every claim as a list of (ID . PATH) cells.  See the
`pending-claims' table in `vulpea-db--schema' for what a claim is."
  (if id
      (mapcar #'car (emacsql (vulpea-db)
                             [:select path :from pending-claims
                              :where (= id $s1)
                              :order-by path]
                             id))
    (mapcar (lambda (row) (cons (car row) (cadr row)))
            (emacsql (vulpea-db)
                     [:select [id path] :from pending-claims
                      :order-by [id path]]))))

(defun vulpea-db--record-pending-claims (path ids)
  "Replace the claims made by PATH with claims for IDS.

Claims by a path always reflect its latest parse: previous claims
are withdrawn, so an id no longer present in the file cannot be
resurrected when its owner releases it."
  (setq path (vulpea-db-normalize-path path))
  (emacsql (vulpea-db)
           [:delete :from pending-claims :where (= path $s1)]
           path)
  (dolist (id ids)
    (emacsql (vulpea-db)
             [:insert :or :ignore :into pending-claims :values $v1]
             (vector id path))))

(defun vulpea-db--delete-pending-claims (path)
  "Withdraw every claim made by PATH."
  (emacsql (vulpea-db)
           [:delete :from pending-claims :where (= path $s1)]
           (vulpea-db-normalize-path path)))

(defun vulpea-db--delete-file-hash (path)
  "Remove PATH's change-detection row.

Without the row every change-detection path treats PATH as changed,
so its next visit re-reads the file regardless of mtime or hash."
  (emacsql (vulpea-db)
           [:delete :from files :where (= path $s1)]
           (vulpea-db-normalize-path path)))

(defvar vulpea-db-updated-functions nil
  "Abnormal hook run after database content for a file has changed.

Each function is called with (PATH COUNT), where PATH is the file
whose database content changed and COUNT is the number of notes
written (file-level + headings).  COUNT is 0 when the file's notes
were removed from the database - the file was deleted, or it left
the tracked set.  The hook runs after the write (or delete)
transaction commits, so a handler reading the database sees the
new content.

This is the single data-changed signal: it fires for synchronous
updates (`vulpea-db-update-file', including saves going through
`vulpea-utils-with-note-sync'), for results arriving from the
extraction worker, and for removals.
`vulpea-db-worker-done-functions' is a worker-lifecycle hook and
never fires for synchronous writes or removals.

A file whose content did not change (only its stamp was refreshed)
produces no call, and neither does removing a file the database
never tracked.  Bulk operations - a directory scan, a full
rebuild - run the hook once per file; a handler that triggers
expensive work (a UI refresh) is expected to debounce.

This is an extension point, not a setting: attach to it with
`add-hook', which is why it is deliberately not a `defcustom'.")

(defvar vulpea-db--pending-removal-announcements nil
  "Removed paths whose announcement waits for a transaction to commit.

`vulpea-db--announce-removal' queues here instead of running
`vulpea-db-updated-functions' when a transaction is open: the hook
promises that a handler reading the database sees the deletion, and
mid-transaction it would not.  The transaction owner let-binds this
to nil around its transaction and flushes with
`vulpea-db--flush-removal-announcements' after the commit - the
binding also guarantees a rolled-back transaction announces
nothing.")

(defun vulpea-db--announce-removal (path)
  "Announce on `vulpea-db-updated-functions' that PATH was removed.

Runs the hook with (PATH 0) - or, when a transaction is open, queues
PATH on `vulpea-db--pending-removal-announcements' for the
transaction owner to flush after the commit."
  (if (> emacsql--transaction-level 0)
      (push path vulpea-db--pending-removal-announcements)
    (run-hook-with-args 'vulpea-db-updated-functions path 0)))

(defun vulpea-db--flush-removal-announcements ()
  "Announce removals queued while a transaction was open.
Runs `vulpea-db-updated-functions' with (PATH 0) for each queued
path, in the order the removals happened, and clears the queue."
  (let ((paths (nreverse vulpea-db--pending-removal-announcements)))
    (setq vulpea-db--pending-removal-announcements nil)
    (dolist (path paths)
      (run-hook-with-args 'vulpea-db-updated-functions path 0))))

(defun vulpea-db--forget-file (path)
  "Forget PATH entirely: its notes and its change-detection row.

For callers that mean \"this file is gone\", as opposed to
`vulpea-db--delete-file-notes', which leaves the `files' row in place
for the re-index case: clearing a file's notes to write them again from
a fresh parse must not throw away what is known about the file.

The `files' row holds the hash, mtime and size used to decide whether a
file changed since it was last read.  Kept after the file is gone, it
answers that question about a file that no longer exists, so a file
restored at the same path with the same content compares equal and is
never indexed again.

Both deletions happen together: dropping the notes while keeping the
row is the very state this exists to avoid.

A transaction is opened only when there is not one already.
`emacsql-with-transaction' claims to nest, and it does until the
database is locked: its retry issues a rollback without looking at the
nesting level, so at depth two a transient SQLITE_BUSY throws away the
outer transaction's work and then retries the inner body alone, leaving
the caller to finish in autocommit and report success.  A cleanup loop
that forgets several files would announce them all and leave some of
their rows behind, which is the exact state this function exists to
prevent.

The ids PATH held are released: files with a pending claim on one of
them (see `vulpea-db--schema') are handed to
`vulpea-db--resolve-released-ids' so a note refiled out of PATH
resurfaces at its new home.

When the database knew PATH (notes or a change-detection row), the
removal is announced on `vulpea-db-updated-functions' via
`vulpea-db--announce-removal'; forgetting a path that was never
tracked announces nothing."
  (let* ((tracked (or (vulpea-db--get-file-note-ids path)
                      (vulpea-db--get-file-hash path)))
         (released (if (> emacsql--transaction-level 0)
                       (vulpea-db--forget-file-1 path)
                     (emacsql-with-transaction (vulpea-db)
                       (vulpea-db--forget-file-1 path)))))
    (when (and released (fboundp 'vulpea-db--unregister-id-locations))
      (vulpea-db--unregister-id-locations released path))
    (when (and released (fboundp 'vulpea-db--resolve-released-ids))
      (vulpea-db--resolve-released-ids
       released (vulpea-db-normalize-path path)))
    (when tracked
      (vulpea-db--announce-removal path))))

(defun vulpea-db--forget-file-1 (path)
  "Delete PATH's notes, its claims and its change-detection row.

The body of `vulpea-db--forget-file', without a transaction of its own
and without claim resolution.  Call that instead unless you already
hold a transaction.  Returns the ids PATH held."
  (let ((ids (vulpea-db--get-file-note-ids path)))
    (vulpea-db--delete-file-notes path)
    (vulpea-db--delete-pending-claims path)
    (emacsql (vulpea-db)
             [:delete :from files :where (= path $s1)]
             (vulpea-db-normalize-path path))
    ids))

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
