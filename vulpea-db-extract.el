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
(require 'org-id)
(require 'seq)
(require 'cl-lib)
(require 'vulpea-db)
(require 'vulpea-buffer)

(defsubst vulpea-db--string-no-properties (value)
  "Return VALUE as a plain string without text properties."
  (when (and value (stringp value))
    (substring-no-properties value)))

(defsubst vulpea-db--strings-no-properties (values)
  "Return VALUES list with text properties stripped from each element."
  (when values
    (mapcar #'substring-no-properties values)))

(defun vulpea-db--extract-created-date (properties)
  "Extract date from CREATED property in PROPERTIES alist.

Supports various date formats:
- \"[2025-12-08 Sun 14:30]\" - Org timestamp with time
- \"[2025-12-08]\" - Org timestamp without time
- \"2025-12-08\" - ISO date

Returns date string in YYYY-MM-DD format, or nil if not found."
  (when-let ((created (cdr (assoc "CREATED" properties))))
    (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" created)
      (format "%s-%s-%s"
              (match-string 1 created)
              (match-string 2 created)
              (match-string 3 created)))))

;;; Archive Detection

(defun vulpea-db--archived-p (element properties filetags)
  "Check if ELEMENT is archived and should be excluded.

ELEMENT is either nil (for file-level) or a headline element.
PROPERTIES is the alist of properties for the element.
FILETAGS is the list of file-level tags.

Returns non-nil if the element is archived, which means:
- It has ARCHIVE_TIME property, or
- It has `org-archive-tag' directly or inherited from parent headlines,
  or from filetags."
  (when vulpea-db-exclude-archived
    (or
     ;; Check ARCHIVE_TIME property
     (assoc "ARCHIVE_TIME" properties)
     ;; Check for archive tag
     (let ((archive-tag (bound-and-true-p org-archive-tag)))
       (when archive-tag
         (or
          ;; Check filetags
          (member archive-tag filetags)
          ;; Check headline tags (direct and inherited)
          (when element
            (vulpea-db--headline-has-tag-p element archive-tag))))))))

(defun vulpea-db--headline-has-tag-p (headline tag)
  "Check if HEADLINE has TAG directly or inherited from ancestors."
  (or
   ;; Direct tag on this headline
   (member tag (org-element-property :tags headline))
   ;; Inherited from parent headline
   (let ((parent (org-element-property :parent headline)))
     (when (and parent (eq (org-element-type parent) 'headline))
       (vulpea-db--headline-has-tag-p parent tag)))))

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

(defcustom vulpea-db-parse-method 'temp-buffer
  "Method to use for parsing org files during sync.

This setting has dramatic performance implications. Choose the mode
that matches your configuration:

  \\='single-temp-buffer (FASTEST)
    Reuses one hidden buffer and never re-runs `org-mode'.
    - ⚡ Best throughput (~1.3k files/sec)
    - ✗ Skips `org-mode-hook' entirely
    - ✗ Ignores per-file `#+TODO', `#+PROPERTY', `org-attach-dir'
    Use when your Org setup is 100% global.

  \\='temp-buffer (DEFAULT)
    Reuses one hidden buffer but re-runs `org-mode' per file.
    - ✓ Honors file-level keywords and `org-mode-hook'
    - ✓ Supports hook-based per-file tweaks
    - ⚠️ Slower if hooks are heavy (e.g., org-roam)
    Hooks can check `vulpea-db--active-parse-method' to skip work.

  \\='find-file (SLOWEST)
    Visits files with `find-file-noselect' as if opened manually.
    - ✓ Respects `.dir-locals.el' and file-visiting hooks
    - ✗ 30-40x slower than temp-buffer strategies

See README.org and bench/PERFORMANCE.md for guidance."
  :group 'vulpea
  :type '(choice (const :tag "Single temp buffer (fastest, skips hooks)" single-temp-buffer)
          (const :tag "Temp buffer (default, runs org-mode per file)" temp-buffer)
          (const :tag "Find file (slow, respects dir-locals)" find-file)))

(defvar vulpea-db--active-parse-method nil
  "Current `vulpea-db-parse-method' while parsing.

This is let-bound during parsing so hook authors can skip expensive
work, e.g. `(unless (bound-and-true-p 'vulpea-db--active-parse-method) …)'.")

(defvar vulpea-db--parse-buffer nil
  "Reusable buffer for parsing org files.
Caching the buffer with `org-mode' already initialized provides
significant performance improvement (12x faster) by avoiding
repeated `org-mode' activation overhead.")

(defvar vulpea-db--parse-buffer-run-hooks t
  "Whether `org-mode' should run hooks when initializing parse buffer.")

(defmacro vulpea-db--with-parse-buffer (&rest body)
  "Execute BODY in a reusable parse buffer with `org-mode' initialized.

The buffer is created once and reused across multiple file parses.
This avoids the expensive `org-mode' initialization overhead (0.7ms per file)
by initializing `org-mode' only once and reusing the buffer."
  (declare (indent 0))
  `(progn
     (unless (and vulpea-db--parse-buffer
              (buffer-live-p vulpea-db--parse-buffer))
      (setq vulpea-db--parse-buffer (generate-new-buffer " *vulpea-parse*"))
      (with-current-buffer vulpea-db--parse-buffer
       (let ((delay-mode-hooks (not vulpea-db--parse-buffer-run-hooks)))
        (org-mode))))
     (with-current-buffer vulpea-db--parse-buffer
      ,@body)))

(defun vulpea-db--parse-with-temp-buffer (path rerun-org-mode)
  "Parse org file at PATH using shared temp buffer.

If RERUN-ORG-MODE is non-nil, `org-mode' (and its hooks) are executed
after loading PATH so file-local keywords and hooks are respected."
  (let ((vulpea-db--parse-buffer-run-hooks rerun-org-mode))
    (vulpea-db--with-parse-buffer
      (let* ((t0 (current-time))
             (_ (progn
                  (erase-buffer)
                  (insert-file-contents path)))
             (t1 (current-time))
             (_ (progn
                  (setq buffer-file-name path)  ; Required for org-attach-dir
                  (setq default-directory (file-name-directory path))))  ; Fix attach-dir paths
             (_ (when rerun-org-mode
                  (let ((delay-mode-hooks nil))
                    (org-mode))))
             (t2 (current-time))
             (content (buffer-string))
             (ast (org-element-parse-buffer))
             (t3 (current-time))
             (attrs (file-attributes path))
             (mtime (float-time (file-attribute-modification-time attrs)))
             (size (file-attribute-size attrs))
             (hash (secure-hash 'sha256 content))
             (file-title (vulpea-db--extract-file-title ast path))
             (file-node (vulpea-db--extract-file-node ast path (current-buffer) file-title))
             (heading-nodes (vulpea-db--extract-heading-nodes ast path (current-buffer) file-title))
             (t4 (current-time)))

        ;; Accumulate detailed timing if enabled
        (when vulpea-db--timing-data
          (let ((io-time (* 1000 (float-time (time-subtract t1 t0))))
                (org-mode-time (* 1000 (float-time (time-subtract t2 t1))))
                (parse-ast-time (* 1000 (float-time (time-subtract t3 t2))))
                (extract-time (* 1000 (float-time (time-subtract t4 t3)))))
            (dolist (entry `((parse-io . ,io-time)
                             (parse-org-mode . ,org-mode-time)
                             (parse-ast . ,parse-ast-time)
                             (parse-extract . ,extract-time)))
              (let ((existing (assoc (car entry) vulpea-db--timing-data)))
                (if existing
                    (setcdr existing (+ (cdr existing) (cdr entry)))
                  (push entry vulpea-db--timing-data))))))

        ;; Clear buffer state to avoid file change tracking
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil)

        (make-vulpea-parse-ctx
         :path path
         :ast ast
         :file-node file-node
         :heading-nodes heading-nodes
         :hash hash
         :mtime mtime
         :size size)))))

(defun vulpea-db--parse-file (path)
  "Parse org file at PATH and return parse context.

Returns `vulpea-parse-ctx' structure with:
- Full org-element AST
- File-level node data
- Heading-level node data (if enabled)
- File metadata (hash, mtime, size)

Respects `vulpea-db-parse-method' setting for parsing approach."
  (let ((vulpea-db--active-parse-method vulpea-db-parse-method))
    (pcase vulpea-db-parse-method
      ('single-temp-buffer
       (vulpea-db--parse-with-temp-buffer path nil))

      ('temp-buffer
       (vulpea-db--parse-with-temp-buffer path t))

      ('find-file
       ;; Use find-file-noselect: slower but respects hooks and dir-locals
       (let ((buffer (find-file-noselect path t)))
         (unwind-protect
             (with-current-buffer buffer
               (let* ((content (buffer-string))
                      (ast (org-element-parse-buffer))
                      (attrs (file-attributes path))
                      (mtime (float-time (file-attribute-modification-time attrs)))
                      (size (file-attribute-size attrs))
                      (hash (secure-hash 'sha256 content))
                      (file-title (vulpea-db--extract-file-title ast path)))

                 (make-vulpea-parse-ctx
                  :path path
                  :ast ast
                  :file-node (vulpea-db--extract-file-node ast path (current-buffer) file-title)
                  :heading-nodes (vulpea-db--extract-heading-nodes ast path (current-buffer) file-title)
                  :hash hash
                  :mtime mtime
                  :size size)))
           ;; Always kill the buffer after parsing
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))

      (_
       (error "Unsupported vulpea-db-parse-method: %s" vulpea-db-parse-method)))))

(defun vulpea-db--extract-file-title (ast path)
  "Extract file title from AST at PATH.

Returns the #+TITLE keyword value if present, otherwise the filename base."
  (let ((keywords (org-element-map ast 'keyword
                    (lambda (kw)
                      (cons (vulpea-db--string-no-properties
                             (org-element-property :key kw))
                            (vulpea-db--string-no-properties
                             (org-element-property :value kw)))))))
    (or (cdr (assoc "TITLE" keywords))
        (file-name-base path))))

(defun vulpea-db--extract-file-node (ast path buffer file-title)
  "Extract file-level node data from AST at PATH in BUFFER.

FILE-TITLE is the title of the file (from #+TITLE or filename).

Returns plist with:
  :id :title :aliases :tags :links :properties :meta
  :todo :priority :scheduled :deadline :closed :attach-dir :file-title

Returns nil if:
- File has no ID property in property drawer
- File has VULPEA_IGNORE property set to non-nil value
- File is archived (when `vulpea-db-exclude-archived' is non-nil)"
  (let* ((keywords (org-element-map ast 'keyword
                     (lambda (kw)
                       (cons (vulpea-db--string-no-properties
                              (org-element-property :key kw))
                             (vulpea-db--string-no-properties
                              (org-element-property :value kw))))))
         (properties (vulpea-db--extract-properties ast nil))
         (id (cdr (assoc "ID" properties)))
         (ignored (cdr (assoc "VULPEA_IGNORE" properties)))
         (filetags-str (cdr (assoc "FILETAGS" keywords)))
         (filetags (when filetags-str
                     (split-string filetags-str ":" t)))
         (archived (vulpea-db--archived-p nil properties filetags)))

    ;; Only index if ID exists, not explicitly ignored, and not archived
    (when (and id (not ignored) (not archived))
      (let* ((meta (vulpea-db--extract-meta ast))
             (links (vulpea-db--extract-links ast t))  ; Don't recurse into headlines
             (aliases (vulpea-db--extract-aliases properties))
             (attach-dir (with-current-buffer buffer
                           (require 'org-attach)
                           (save-excursion
                             (goto-char (point-min))
                             (org-attach-dir nil 'no-fs-check)))))
        (list :id id
              :title file-title
              :aliases aliases
              :tags filetags
              :links links
              :properties properties
              :meta meta
              :todo nil
              :priority nil
              :scheduled nil
              :deadline nil
              :closed nil
              :attach-dir attach-dir
              :file-title file-title)))))

(defun vulpea-db--extract-heading-nodes (ast path buffer file-title)
  "Extract heading-level nodes from AST at PATH in BUFFER.

FILE-TITLE is the title of the file containing the headings.

Returns list of plists, one per heading with ID property.
Each plist has same structure as file-node.

Skips headings that:
- Have no ID property
- Have VULPEA_IGNORE property set to non-nil value
- Are archived (when `vulpea-db-exclude-archived' is non-nil)

Respects `vulpea-db-index-heading-level' setting."
  (when (vulpea-db--should-index-headings-p path)
    ;; Extract filetags once for archive checking
    (let* ((filetags-str (org-element-map ast 'keyword
                           (lambda (kw)
                             (when (string= "FILETAGS"
                                            (org-element-property :key kw))
                               (org-element-property :value kw)))
                           nil t))
           (filetags (when filetags-str
                       (split-string filetags-str ":" t))))
      (org-element-map ast 'headline
        (lambda (headline)
          (when-let ((id (org-element-property :ID headline)))
            (let* ((properties (vulpea-db--extract-properties ast headline))
                   (ignored (cdr (assoc "VULPEA_IGNORE" properties)))
                   (archived (vulpea-db--archived-p headline properties filetags)))
              ;; Only index if not explicitly ignored and not archived
              (unless (or ignored archived)
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
                       (outline-path (let (path
                                           (current headline))
                                       (while (setq current (org-element-property :parent current))
                                         (when (eq (org-element-type current) 'headline)
                                           (push (vulpea-db--string-no-properties
                                                  (org-element-property :raw-value current))
                                                 path)))
                                       path))
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
                        :attach-dir attach-dir
                        :file-title file-title))))))))))

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

Looks for property defined by `vulpea-buffer-alias-property'.
Handles both quoted aliases (with spaces) and unquoted aliases properly."
  (when-let ((aliases-str (cdr (assoc vulpea-buffer-alias-property properties))))
    (setq aliases-str (string-trim aliases-str))
    (let ((result nil)
          (pos 0))
      (while (< pos (length aliases-str))
        ;; Skip whitespace
        (while (and (< pos (length aliases-str))
                    (= (aref aliases-str pos) ? ))
          (setq pos (1+ pos)))
        (when (< pos (length aliases-str))
          (let ((char (aref aliases-str pos)))
            (cond
             ;; Quoted alias - find matching closing quote
             ((= char ?\")
              (let ((end (string-match "\"" aliases-str (1+ pos))))
                (if end
                    (progn
                      (push (substring aliases-str (1+ pos) end) result)
                      (setq pos (1+ end)))
                  (error "Unmatched quote in %s" vulpea-buffer-alias-property))))
             ;; Unquoted alias - find next space or end of string
             (t
              (let ((end (or (string-match " " aliases-str pos)
                             (length aliases-str))))
                (push (substring aliases-str pos end) result)
                (setq pos end)))))))
      (nreverse result))))

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

(defun vulpea-db--extract-links (ast-or-node &optional no-recursion)
  "Extract all links from AST-OR-NODE.

Returns list of plists with :dest, :type, and :pos.
Captures all link types: id:, roam:, file:, http:, https:,
attachment:, elisp:, and any other `org-mode' link type.

If NO-RECURSION is non-nil, stops recursion at headline boundaries.
This is useful for file-level extraction to avoid collecting links
from child headlines."
  (org-element-map ast-or-node 'link
    (lambda (link)
      (let ((type (org-element-property :type link))
            (path (org-element-property :path link))
            (pos (org-element-property :begin link)))
        (when (and type path)
          (list :dest path :type type :pos pos))))
    nil nil (when no-recursion 'headline)))

(defun vulpea-db--extract-meta (element)
  "Extract metadata from ELEMENT (AST or headline element).

Metadata is defined by the first description list:
  - key :: value
  - key :: value2

Returns alist of (key . values) where values is list of strings.
Link values are stored as interpreted strings."
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
                 (key (when tag-contents
                        (substring-no-properties
                         (string-trim
                          (org-element-interpret-data
                           (org-element-contents tag-contents))))))
                 (value-el (car (org-element-contents item)))
                 (value (when value-el
                          (substring-no-properties
                           (string-trim
                            (org-element-interpret-data value-el))))))
            (when (and key value)
              (let ((existing (assoc key meta-alist)))
                (if existing
                    (setcdr existing (append (cdr existing) (list value)))
                  (push (cons key (list value)) meta-alist))))))))
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

(defvar vulpea-db--timing-data nil
  "Accumulated timing data for profiling.
Format: ((phase . total-time-ms) ...)")

(defun vulpea-db-update-file (path)
  "Parse and update database for file at PATH.

Returns number of notes updated (file-level + headings)."
  (let* ((t0 (current-time))
         (ctx (vulpea-db--parse-file path))
         (t1 (current-time))
         (parse-time (* 1000 (float-time (time-subtract t1 t0))))
         (db (vulpea-db))
         (count 0)
         (ids nil)  ; Track IDs to register with org-id
         (t2 nil)
         (db-time 0))

    (setq t2 (current-time))
    (emacsql-with-transaction db
      ;; Delete existing notes from this file
      (vulpea-db--delete-file-notes path)

      ;; Insert file-level note
      (let ((file-data (vulpea-parse-ctx-file-node ctx)))
        (when-let ((id (plist-get file-data :id)))
          (vulpea-db--insert-note-from-plist ctx path 0 0 file-data)
          (push id ids)
          (setq count (1+ count))))

      ;; Insert heading-level notes
      (dolist (heading-data (vulpea-parse-ctx-heading-nodes ctx))
        (vulpea-db--insert-note-from-plist
         ctx
         path
         (plist-get heading-data :level)
         (plist-get heading-data :pos)
         heading-data)
        (when-let ((id (plist-get heading-data :id)))
          (push id ids))
        (setq count (1+ count)))

      ;; Update file hash
      (vulpea-db--update-file-hash path
                                   (vulpea-parse-ctx-hash ctx)
                                   (vulpea-parse-ctx-mtime ctx)
                                   (vulpea-parse-ctx-size ctx)))
    (setq db-time (* 1000 (float-time (time-subtract (current-time) t2))))

    ;; Register all IDs with org-id so links can be followed
    (dolist (id ids)
      (org-id-add-location id path))

    ;; Accumulate timing data
    (when vulpea-db--timing-data
      (let ((parse-entry (assoc 'parse vulpea-db--timing-data))
            (db-entry (assoc 'db vulpea-db--timing-data)))
        (if parse-entry
            (setcdr parse-entry (+ (cdr parse-entry) parse-time))
          (push (cons 'parse parse-time) vulpea-db--timing-data))
        (if db-entry
            (setcdr db-entry (+ (cdr db-entry) db-time))
          (push (cons 'db db-time) vulpea-db--timing-data))))

    count))

(defun vulpea-db--insert-note-from-plist (ctx path level pos data)
  "Insert note from DATA plist at PATH with LEVEL and POS.

CTX is the parse context containing AST and other metadata.
Runs registered extractors after insertion."
  (let* ((modified-at (format-time-string "%Y-%m-%d %H:%M:%S"))
         (properties (plist-get data :properties))
         (created-at (vulpea-db--extract-created-date properties)))
    ;; First insert the note so foreign keys can reference it
    (vulpea-db--insert-note
     :id (plist-get data :id)
     :path path
     :level level
     :pos pos
     :title (plist-get data :title)
     :properties properties
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
     :file-title (plist-get data :file-title)
     :created-at created-at
     :modified-at modified-at)

    ;; Then run extractors that may insert into foreign-keyed tables
    (vulpea-db--run-extractors ctx data)))

;;; Provide

(provide 'vulpea-db-extract)
;;; vulpea-db-extract.el ends here
