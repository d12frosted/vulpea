;;; vulpea-db-query.el --- Query layer for Vulpea -*- lexical-binding: t; -*-
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
;; Query layer for Vulpea v2.
;;
;; This module provides high-level query functions for retrieving notes
;; from the database. All queries use the hybrid schema design:
;; - Simple queries use the materialized notes table (fast, no JOINs)
;; - Filtered queries use normalized tables (tags, links, meta)
;;
;; Performance characteristics:
;; - vulpea-db-get-by-id: <5ms (single table lookup)
;; - vulpea-db-query-by-tags-*: <50ms for 10k notes (indexed lookup)
;; - vulpea-db-query with predicate: <500ms for 10k notes (full scan)
;;
;;; Code:

(require 'vulpea-db)
(require 'vulpea-note)
(require 'json)

;;; Row Conversion

(defun vulpea-db--json-value-to-elisp (value)
  "Convert JSON VALUE to appropriate Elisp structure.

Hash tables become plists, vectors become lists.
Handles the special case of meta values which are lists of plists."
  (cond
   ((hash-table-p value)
    ;; Convert hash-table to plist with consistent key ordering
    (let (keys-values)
      (maphash (lambda (k v)
                 (push (cons k (vulpea-db--json-value-to-elisp v)) keys-values))
               value)
      ;; Sort by key for consistent ordering (reverse alpha to get :value before :type)
      (setq keys-values (sort keys-values (lambda (a b) (string> (car a) (car b)))))
      ;; Build plist: (:key1 val1 :key2 val2 ...)
      (let (result)
        (dolist (kv keys-values)
          (push (intern (concat ":" (car kv))) result)
          (push (cdr kv) result))
        (nreverse result))))
   ((vectorp value)
    ;; Convert vector to list, recursively converting elements
    (mapcar #'vulpea-db--json-value-to-elisp (append value nil)))
   (t value)))

(defun vulpea-db--json-to-alist (json-str)
  "Convert JSON-STR to alist with string keys.

Uses hash-table as intermediate to preserve string keys.
Recursively converts nested structures."
  (when (and json-str (not (string= json-str "null")))
    (let ((ht (json-parse-string json-str)))
      (let (result)
        (maphash (lambda (k v)
                   (push (cons k (vulpea-db--json-value-to-elisp v)) result))
                 ht)
        (nreverse result)))))

(defun vulpea-db--row-to-note (row)
  "Convert database ROW to `vulpea-note' struct.

ROW is a vector from the notes table with all fields in schema order."
  (let ((id (elt row 0))
        (path (elt row 1))
        (level (elt row 2))
        (pos (elt row 3))
        (title (elt row 4))
        (properties (elt row 5))
        (tags (elt row 6))
        (aliases (elt row 7))
        (meta (elt row 8))
        (links (elt row 9))
        (todo (elt row 10))
        (priority (elt row 11))
        (scheduled (elt row 12))
        (deadline (elt row 13))
        (closed (elt row 14))
        (outline-path (elt row 15))
        (attach-dir (elt row 16))
        (file-title (elt row 17)))
    (make-vulpea-note
     :id id
     :path path
     :level level
     :pos pos
     :title title
     :properties (when (and properties (not (string= properties "null")))
                   (json-parse-string properties :object-type 'alist))
     :tags (when (and tags (not (string= tags "null")))
             (append (json-parse-string tags :array-type 'list) nil))
     :aliases (when (and aliases (not (string= aliases "null")))
                (append (json-parse-string aliases :array-type 'list) nil))
     :meta (vulpea-db--json-to-alist meta)
     :links (when (and links (not (string= links "null")))
              (append (json-parse-string links :array-type 'list :object-type 'plist) nil))
     :todo todo
     :priority priority
     :scheduled scheduled
     :deadline deadline
     :closed closed
     :outline-path outline-path
     :attach-dir attach-dir
     :file-title file-title)))

;;; Core Query Functions

(defun vulpea-db-get-by-id (id)
  "Get note by ID.

Returns complete note from materialized table - single query, no JOINs.
This is the fastest query operation (<5ms).

Returns `vulpea-note' or nil if not found."
  (when-let* ((row (car (emacsql (vulpea-db)
                                [:select * :from notes
                                 :where (= id $s1)]
                                id))))
    (vulpea-db--row-to-note row)))

(defun vulpea-db-query (&optional predicate)
  "Query all notes, optionally filtering by PREDICATE.

PREDICATE is a function that takes a `vulpea-note' and returns non-nil
if the note should be included in results.

This function loads all notes from the materialized table and filters
in Elisp. For large databases (10k+ notes), prefer specialized query
functions like `vulpea-db-query-by-tags-some' which use indexed lookups.

Performance: ~500ms for 10k notes with predicate.

Returns list of `vulpea-note' structs."
  (let ((notes (mapcar #'vulpea-db--row-to-note
                       (emacsql (vulpea-db)
                                [:select * :from notes]))))
    (if predicate
        (seq-filter predicate notes)
      notes)))

;;; Tag-Based Queries

(defun vulpea-db-query-by-tags-some (tags)
  "Get notes that have ANY of TAGS.

Uses normalized tags table with index for efficient filtering.
Returns notes that have at least one tag from TAGS.

Performance: <50ms for 10k notes.

TAGS is a list of tag strings.
Returns list of `vulpea-note' structs."
  (if (null tags)
      nil
    (let ((rows (emacsql (vulpea-db)
                         [:select :distinct [notes:*]
                          :from notes
                          :inner :join tags
                          :on (= notes:id tags:note-id)
                          :where (in tags:tag $v1)]
                         (vconcat tags))))
      (mapcar #'vulpea-db--row-to-note rows))))

(defun vulpea-db-query-by-tags-every (tags)
  "Get notes that have ALL of TAGS.

Uses normalized tags table with GROUP BY + HAVING for efficient filtering.
Returns notes that have every tag from TAGS.

Performance: <100ms for 10k notes.

TAGS is a list of tag strings.
Returns list of `vulpea-note' structs."
  (if (null tags)
      (vulpea-db-query nil)  ; Return all notes
    (let* ((tag-count (length tags))
           (rows (emacsql (vulpea-db)
                          [:select * :from notes
                           :where (in id [:select [note-id]
                                          :from tags
                                          :where (in tag $v1)
                                          :group :by note-id
                                          :having (= (funcall count (distinct tag)) $s2)])]
                          (vconcat tags)
                          tag-count)))
      (mapcar #'vulpea-db--row-to-note rows))))

(defun vulpea-db-query-by-tags-none (tags)
  "Get notes that have NONE of TAGS.

Returns notes that don't have any tag from TAGS.

Performance: <50ms for 10k notes.

TAGS is a list of tag strings.
Returns list of `vulpea-note' structs."
  (if (null tags)
      (vulpea-db-query nil)  ; Return all notes
    (let ((rows (emacsql (vulpea-db)
                         [:select * :from notes
                          :where (not (in id [:select :distinct [note-id]
                                              :from tags
                                              :where (in tag $v1)]))]
                         (vconcat tags))))
      (mapcar #'vulpea-db--row-to-note rows))))

;;; Link-Based Queries

(defun vulpea-db-query-by-links-some (dest-ids &optional link-type)
  "Get notes that link to ANY of DEST-IDS.

Uses normalized links table for efficient filtering.

DEST-IDS is a list of destination note IDs (strings) or a list of cons
cells (TYPE . ID) for backward compatibility.

LINK-TYPE is optional link type filter (e.g., \"id\", \"file\").
If nil, all link types are considered.

Returns list of `vulpea-note' structs."
  (when dest-ids
    ;; Handle both formats: ("id1" "id2") or (("id" . "id1") ("id" . "id2"))
    (let* ((normalized-ids
            (if (consp (car dest-ids))
                ;; Format: (("id" . "id1") ("id" . "id2"))
                (mapcar #'cdr dest-ids)
              ;; Format: ("id1" "id2")
              dest-ids))
           (rows (if link-type
                     (emacsql (vulpea-db)
                              [:select :distinct [notes:*]
                               :from notes
                               :inner :join links
                               :on (= notes:id links:source)
                               :where (and (in links:dest $v1)
                                           (= links:type $s2))]
                              (vconcat normalized-ids)
                              link-type)
                   (emacsql (vulpea-db)
                            [:select :distinct [notes:*]
                             :from notes
                             :inner :join links
                             :on (= notes:id links:source)
                             :where (in links:dest $v1)]
                            (vconcat normalized-ids)))))
      (mapcar #'vulpea-db--row-to-note rows))))

(defun vulpea-db-query-by-links-every (dest-ids &optional link-type)
  "Get notes that link to ALL of DEST-IDS.

Uses GROUP BY + HAVING for efficient filtering.

DEST-IDS is a list of destination note IDs (strings).
LINK-TYPE is optional link type filter.

Returns list of `vulpea-note' structs."
  (if (null dest-ids)
      (vulpea-db-query nil)
    (let* ((id-count (length dest-ids))
           (rows (if link-type
                     (emacsql (vulpea-db)
                              [:select * :from notes
                               :where (in id [:select [source]
                                              :from links
                                              :where (and (in dest $v1)
                                                          (= type $s2))
                                              :group :by source
                                              :having (= (funcall count (distinct dest)) $s3)])]
                              (vconcat dest-ids)
                              link-type
                              id-count)
                   (emacsql (vulpea-db)
                            [:select * :from notes
                             :where (in id [:select [source]
                                            :from links
                                            :where (in dest $v1)
                                            :group :by source
                                            :having (= (funcall count (distinct dest)) $s2)])]
                            (vconcat dest-ids)
                            id-count))))
      (mapcar #'vulpea-db--row-to-note rows))))

;;; Attachment Queries

(defun vulpea-db-query-attachments-by-path (path)
  "Get all attachment link destinations with attach dirs for notes at PATH.

This is an optimized query that retrieves attachment destinations
along with their attachment directories in a single database query,
avoiding the N+1 query problem when processing multiple notes in a file.

PATH is an absolute path to an org file.

Returns a list of cons cells (DEST . ATTACH-DIR) where DEST is the
attachment file name and ATTACH-DIR is the attachment directory of the
note containing the link."
  (mapcar (lambda (row)
            (cons (car row) (cadr row)))
          (emacsql (vulpea-db)
                   [:select :distinct [l:dest n:attach-dir]
                    :from notes n
                    :inner :join links l :on (= n:id l:source)
                    :where (and (= n:path $s1)
                                (= l:type "attachment"))]
                   path)))

;;; Additional Query Functions

(defun vulpea-db-query-by-level (level)
  "Get all notes at LEVEL.

LEVEL 0 means file-level notes, LEVEL 1+ means heading-level notes.

Returns list of `vulpea-note' structs."
  (let ((rows (emacsql (vulpea-db)
                       [:select * :from notes
                        :where (= level $s1)]
                       level)))
    (mapcar #'vulpea-db--row-to-note rows)))

;;; File Path Queries

(defun vulpea-db-query-by-file-path (file-path &optional level)
  "Get notes at FILE-PATH, optionally filtered by LEVEL.

FILE-PATH is an absolute path to an org file.
LEVEL is optional filter: 0 for file-level, 1+ for headings.
When LEVEL is nil, returns all notes from the file.

Returns list of `vulpea-note' structs."
  (let ((rows (if level
                  (emacsql (vulpea-db)
                           [:select * :from notes
                            :where (and (= path $s1)
                                        (= level $s2))]
                           file-path level)
                (emacsql (vulpea-db)
                         [:select * :from notes
                          :where (= path $s1)]
                         file-path))))
    (mapcar #'vulpea-db--row-to-note rows)))

(defun vulpea-db-query-by-file-paths (file-paths &optional level)
  "Get notes at any of FILE-PATHS, optionally filtered by LEVEL.

FILE-PATHS is a list of absolute paths to org files.
LEVEL is optional filter: 0 for file-level, 1+ for headings.
When LEVEL is nil, returns all notes from the files.

Returns list of `vulpea-note' structs."
  (when file-paths
    (let ((rows (if level
                    (emacsql (vulpea-db)
                             [:select * :from notes
                              :where (and (in path $v1)
                                          (= level $s2))]
                             (vconcat file-paths) level)
                  (emacsql (vulpea-db)
                           [:select * :from notes
                            :where (in path $v1)]
                           (vconcat file-paths)))))
      (mapcar #'vulpea-db--row-to-note rows))))

(defun vulpea-db-query-by-directory (directory &optional level)
  "Get notes under DIRECTORY, optionally filtered by LEVEL.

DIRECTORY is an absolute path to a directory.
LEVEL is optional filter: 0 for file-level, 1+ for headings.
When LEVEL is nil, returns all notes from the directory.

Uses prefix matching on file paths, so includes all subdirectories.

Returns list of `vulpea-note' structs."
  (let* ((dir (file-name-as-directory directory))
         (pattern (concat dir "%"))
         (rows (if level
                   (emacsql (vulpea-db)
                            [:select * :from notes
                             :where (and (like path $s1)
                                         (= level $s2))]
                            pattern level)
                 (emacsql (vulpea-db)
                          [:select * :from notes
                           :where (like path $s1)]
                          pattern))))
    (mapcar #'vulpea-db--row-to-note rows)))

(defun vulpea-db-search-by-title (pattern &optional case-sensitive)
  "Search notes by PATTERN in title.

PATTERN is matched using SQL LIKE (supports % and _ wildcards).
If CASE-SENSITIVE is non-nil, search is case-sensitive.

Returns list of `vulpea-note' structs."
  (let ((rows (if case-sensitive
                  ;; GLOB is case-sensitive in SQLite
                  (emacsql (vulpea-db)
                           [:select * :from notes
                            :where (glob title $s1)]
                           (format "*%s*" pattern))
                (emacsql (vulpea-db)
                         [:select * :from notes
                          :where (like (lower title) $s1)]
                         (format "%%%s%%" (downcase pattern))))))
    (mapcar #'vulpea-db--row-to-note rows)))

;;; Created-At Queries

(defun vulpea-db-query-by-created-date (date &optional level)
  "Get notes created on DATE, optionally filtered by LEVEL.

DATE is a string in YYYY-MM-DD format.
LEVEL is optional filter: 0 for file-level, 1+ for headings.
When LEVEL is nil, returns all notes created on DATE.

Uses the created-at column which is populated from the CREATED property.

Performance: <50ms for 10k notes (indexed lookup).

Returns list of `vulpea-note' structs."
  (let ((rows (if level
                  (emacsql (vulpea-db)
                           [:select * :from notes
                            :where (and (= created-at $s1)
                                        (= level $s2))]
                           date level)
                (emacsql (vulpea-db)
                         [:select * :from notes
                          :where (= created-at $s1)]
                         date))))
    (mapcar #'vulpea-db--row-to-note rows)))

;;; Property-Based Queries

(defun vulpea-db-query-by-property (key value)
  "Get notes that have property KEY with VALUE.

Uses normalized properties table for efficient filtering.

KEY is a property key string (e.g., \"CREATED\", \"CATEGORY\").
VALUE is the property value to match.

Performance: <50ms for 10k notes (indexed lookup).

Returns list of `vulpea-note' structs."
  (let ((rows (emacsql (vulpea-db)
                       [:select :distinct [notes:*]
                        :from notes
                        :inner :join properties
                        :on (= notes:id properties:note-id)
                        :where (and (= properties:key $s1)
                                    (= properties:value $s2))]
                       key value)))
    (mapcar #'vulpea-db--row-to-note rows)))

(defun vulpea-db-query-by-property-key (key)
  "Get all notes that have property KEY.

Uses normalized properties table for efficient filtering.

KEY is a property key string (e.g., \"CREATED\", \"CATEGORY\").

Returns list of `vulpea-note' structs."
  (let ((rows (emacsql (vulpea-db)
                       [:select :distinct [notes:*]
                        :from notes
                        :inner :join properties
                        :on (= notes:id properties:note-id)
                        :where (= properties:key $s1)]
                       key)))
    (mapcar #'vulpea-db--row-to-note rows)))

;;; Meta-Based Queries

(defun vulpea-db-query-by-meta-key (key)
  "Get all notes that have metadata KEY.

Uses normalized meta table for efficient filtering.

KEY is a metadata key string.
Returns list of `vulpea-note' structs."
  (let ((rows (emacsql (vulpea-db)
                       [:select :distinct [notes:*]
                        :from notes
                        :inner :join meta
                        :on (= notes:id meta:note-id)
                        :where (= meta:key $s1)]
                       key)))
    (mapcar #'vulpea-db--row-to-note rows)))

(defun vulpea-db-query-by-meta (key value)
  "Get notes that have metadata KEY with VALUE.

Uses normalized meta table for efficient filtering.

KEY is a metadata key string.
VALUE is the metadata value to match.

Returns list of `vulpea-note' structs."
  (let ((rows (emacsql (vulpea-db)
                       [:select :distinct [notes:*]
                        :from notes
                        :inner :join meta
                        :on (= notes:id meta:note-id)
                        :where (and (= meta:key $s1)
                                    (= meta:value $s2))]
                       key value)))
    (mapcar #'vulpea-db--row-to-note rows)))

;;; Tag Queries

(defun vulpea-db-query-tags ()
  "Return list of all unique tags in the database.

Queries the normalized tags table directly for efficient retrieval.
Returns a sorted list of tag strings."
  (mapcar #'car
          (emacsql (vulpea-db)
                   [:select :distinct [tag] :from tags
                    :order :by tag])))

;;; Statistics

(defun vulpea-db-count-notes ()
  "Return total number of notes in database."
  (caar (emacsql (vulpea-db)
                 [:select (funcall count *) :from notes])))

(defun vulpea-db-count-file-level-notes ()
  "Return number of file-level notes (level = 0)."
  (caar (emacsql (vulpea-db)
                 [:select (funcall count *) :from notes
                  :where (= level 0)])))

(defun vulpea-db-count-heading-level-notes ()
  "Return number of heading-level notes (level > 0)."
  (caar (emacsql (vulpea-db)
                 [:select (funcall count *) :from notes
                  :where (> level 0)])))

;;; Convenience Functions

(defun vulpea-db-get-file-by-id (id)
  "Get file path for note with ID.

Returns absolute path string or nil if note not found."
  (when-let* ((note (vulpea-db-get-by-id id)))
    (vulpea-note-path note)))

(defun vulpea-db-query-by-ids (ids)
  "Get notes with IDS.

IDS is a list of note ID strings.
Returns list of `vulpea-note' structs in same order as IDS.
Notes that don't exist are omitted from results."
  (when ids
    (let ((rows (emacsql (vulpea-db)
                         [:select * :from notes
                          :where (in id $v1)]
                         (vconcat ids))))
      (mapcar #'vulpea-db--row-to-note rows))))

(provide 'vulpea-db-query)
;;; vulpea-db-query.el ends here
