;;; vulpea.el --- Note management library for Org mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.4.0
;; Package-Requires: ((emacs "27.2") (org "9.4.4") (emacsql "4.3.0") (s "1.12") (dash "2.19"))
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
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vulpea is a note management library for Org mode that maintains its
;; own SQLite database for efficient querying and organization.
;;
;; Key features:
;; - Fast note lookup via SQLite database with automatic sync
;; - Rich note structure: titles, aliases, tags, links, properties, metadata
;; - Flexible querying by tags, links, properties, dates, and more
;; - Metadata system using Org description lists
;; - Note creation with customizable templates
;; - Selection interface with filtering and alias expansion
;;
;; Quick start:
;;   (setq vulpea-directory "~/org/")
;;   (vulpea-db-sync-start)
;;
;; Main entry points:
;; - `vulpea-find' - find and open a note
;; - `vulpea-insert' - insert a link to a note
;; - `vulpea-create' - create a new note
;; - `vulpea-db-query' - query notes with a predicate
;; - `vulpea-select' - select a note with completion
;;
;; See https://github.com/d12frosted/vulpea for documentation.
;;
;;; Code:

(require 'package)
(require 'org-capture)
(require 'org-id)
(require 'vulpea-buffer)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-sync)
(require 'vulpea-db-schema-validation)
(require 'vulpea-mentions)
(require 'vulpea-meta)
(require 'vulpea-note)
(require 'vulpea-schema)
(require 'vulpea-select)
(require 'vulpea-tags)
(require 'vulpea-utils)
(require 's)

;;; Version

(defconst vulpea-version "2.4.0"
  "Version of the vulpea package.

Keep in sync with the Version header in vulpea.el; releases bump
both. For precise version information including commits past a
release, use the function `vulpea-version' instead.")

(defun vulpea-version--git ()
  "Return version from \"git describe\", or nil if unavailable.

Works when vulpea is loaded from a git checkout and git is
available. The result looks like \"v2.2.0\" exactly on a release
tag, \"v2.2.0-15-g2938416\" when 15 commits past it, with a
\"-dirty\" suffix when there are uncommitted changes."
  (when-let* ((file (locate-library "vulpea"))
              (dir (locate-dominating-file file ".git"))
              ((executable-find "git")))
    (with-temp-buffer
      (let ((default-directory dir))
        (when (eql 0 (ignore-errors
                       (call-process "git" nil t nil "describe"
                                     "--tags" "--dirty" "--always")))
          (string-trim (buffer-string)))))))

(defun vulpea-version--package ()
  "Return version of the installed vulpea package, or nil.

For MELPA snapshot installs the result looks like
\"20260610.1234 (commit 2938416)\"."
  (when-let* ((desc (cadr (assq 'vulpea package-alist)))
              (version (package-version-join
                        (package-desc-version desc))))
    (if-let* ((commit (cdr (assq :commit (package-desc-extras desc)))))
        (format "%s (commit %s)"
                version (substring commit 0 (min 7 (length commit))))
      version)))

(defun vulpea-version (&optional show)
  "Return the vulpea version with as much precision as available.

The version is resolved in the following order:

1. \"git describe\" output when running from a git checkout,
   e.g. \"v2.2.0\" or \"v2.2.0-15-g2938416\".
2. Installed package version, including the commit for MELPA
   snapshot installs, e.g. \"20260610.1234 (commit 2938416)\".
3. The `vulpea-version' constant as a fallback.

When SHOW is non-nil (always when called interactively), also
display the version in the echo area. Please include this
version in bug reports."
  (interactive (list t))
  (let ((version (or (vulpea-version--git)
                     (vulpea-version--package)
                     vulpea-version)))
    (when show
      (message "vulpea %s" version))
    version))

;;; Doctor

(defun vulpea-doctor--db-file-info ()
  "Return a human-readable description of the database file."
  (if (file-exists-p vulpea-db-location)
      (format "exists (%s)"
              (file-size-human-readable
               (file-attribute-size
                (file-attributes vulpea-db-location))))
    "missing"))

(defun vulpea-doctor--note-count ()
  "Return the number of indexed notes, or nil when unavailable.

Returns nil instead of creating the database file when it does
not exist - the doctor must not modify state."
  (when (file-exists-p vulpea-db-location)
    (ignore-errors (vulpea-db-count-notes))))

(defun vulpea-doctor--cached-file-stats ()
  "Return (TOTAL . NOTE-LESS) file change-detection cache counts.

TOTAL is how many files are tracked in the `files' table; NOTE-LESS
is how many of them have no note in the `notes' table.  A non-zero
NOTE-LESS is expected for genuinely note-less files (READMEs,
drafts), but a surprising count can indicate notes that failed to
index and are now skipped by change detection (see vulpea#277);
`vulpea-db-sync-full-scan' with a force argument re-extracts them.

Returns nil when the database file is absent; does not create it."
  (when (file-exists-p vulpea-db-location)
    (ignore-errors
      (let ((db (vulpea-db)))
        (cons
         (caar (emacsql db [:select (funcall count *) :from files]))
         (caar (emacsql db
                        [:select (funcall count *) :from files
                         :where (not (in path
                                         [:select :distinct [path]
                                          :from notes]))])))))))

(defun vulpea-doctor--monitoring-status ()
  "Return a string describing the active external file monitoring."
  (cond
   ((and vulpea-db-sync--fswatch-process
         (process-live-p vulpea-db-sync--fswatch-process))
    "fswatch (process running)")
   (vulpea-db-sync--poll-timer
    (format "polling (every %ss)" vulpea-db-sync-poll-interval))
   (t "none")))

(defun vulpea-doctor--issues ()
  "Return a list of detected setup issues as human-readable strings."
  (let ((issues nil)
        (fswatch (executable-find "fswatch"))
        (fd (executable-find "fd"))
        (notes (vulpea-doctor--note-count)))
    ;; Sync directories
    (if (null vulpea-db-sync-directories)
        (push (concat "`vulpea-db-sync-directories' is empty - nothing will"
                      " be indexed. Set it (or `org-directory') to where"
                      " your notes live.")
              issues)
      (dolist (dir vulpea-db-sync-directories)
        (unless (file-directory-p dir)
          (push (format "Sync directory %s does not exist." dir) issues))))
    ;; External tools. A missing executable on a GUI Emacs is often a
    ;; PATH problem rather than a missing install (Doom env file,
    ;; minimal GUI PATH), hence the exec-path hint.
    (when (and (not fswatch)
               (memq vulpea-db-sync-external-method '(auto fswatch)))
      (push (concat "fswatch not found on `exec-path'"
                    (if (eq vulpea-db-sync-external-method 'fswatch)
                        (concat " but `vulpea-db-sync-external-method' is"
                                " 'fswatch - external monitoring will fail"
                                " to start.")
                      " - external changes are detected via slower polling.")
                    " Install fswatch, or fix Emacs's PATH if it is already"
                    " installed (Doom users: re-run 'doom env').")
            issues))
    (unless fd
      (push (concat "fd not found on `exec-path' - directory scans fall"
                    " back to find, which is much slower on large"
                    " collections. Install fd, or fix Emacs's PATH if it"
                    " is already installed (Doom users: re-run 'doom env').")
            issues))
    ;; Sync state
    (unless vulpea-db-autosync-mode
      (push (concat "`vulpea-db-autosync-mode' is disabled - the database"
                    " will not stay up to date as notes change. Enable it"
                    " with (vulpea-db-autosync-mode +1).")
            issues))
    (when (and vulpea-db-autosync-mode
               (memq vulpea-db-sync-external-method '(auto fswatch poll))
               (string= "none" (vulpea-doctor--monitoring-status)))
      (push (concat "External monitoring is configured but not active -"
                    " changes made outside Emacs will not be picked up."
                    " Try toggling `vulpea-db-autosync-mode'.")
            issues))
    ;; Database
    (cond
     ((not (file-exists-p vulpea-db-location))
      (push (concat "Database file does not exist yet. Run"
                    " M-x vulpea-db-sync-full-scan to build it.")
            issues))
     ((and notes (zerop notes))
      (push (concat "Database exists but contains no notes. Run"
                    " M-x vulpea-db-sync-full-scan; if it stays empty,"
                    " check that your notes have ID properties and live"
                    " under `vulpea-db-sync-directories'.")
            issues)))
    (nreverse issues)))

(defun vulpea-doctor--report ()
  "Build the doctor report as a string."
  (let* ((issues (vulpea-doctor--issues))
         (notes (vulpea-doctor--note-count))
         (stats (vulpea-doctor--cached-file-stats))
         (line (lambda (label value) (format "  %-32s %s" label value))))
    (string-join
     (append
      (list
       "Vulpea Doctor"
       "============="
       ""
       "Versions"
       (funcall line "vulpea" (vulpea-version))
       (funcall line "emacs" emacs-version)
       (funcall line "org" (org-version))
       (funcall line "system" (format "%s" system-type))
       ""
       "Configuration"
       (funcall line "vulpea-db-sync-directories"
                (format "%S" vulpea-db-sync-directories))
       (funcall line "vulpea-db-location" vulpea-db-location)
       (funcall line "vulpea-db-parse-method"
                (format "%s" vulpea-db-parse-method))
       (funcall line "vulpea-db-index-heading-level"
                (format "%s" vulpea-db-index-heading-level))
       (funcall line "vulpea-db-sync-external-method"
                (format "%s" vulpea-db-sync-external-method))
       (funcall line "vulpea-db-sync-scan-on-enable"
                (format "%s" vulpea-db-sync-scan-on-enable))
       ""
       "Database"
       (funcall line "file" (vulpea-doctor--db-file-info))
       (funcall line "schema version" (format "%s" vulpea-db-version))
       (funcall line "notes" (if notes (format "%d" notes) "n/a"))
       (funcall line "cached files"
                (if stats (format "%d" (car stats)) "n/a"))
       (funcall line "files without notes"
                (if stats (format "%d" (cdr stats)) "n/a"))
       ""
       "Sync"
       (funcall line "autosync"
                (if vulpea-db-autosync-mode "enabled" "disabled"))
       (funcall line "external monitoring" (vulpea-doctor--monitoring-status))
       (funcall line "pending queue"
                (format "%d" (length vulpea-db-sync--queue)))
       ""
       "External Tools"
       (funcall line "fd" (or (executable-find "fd") "not found"))
       (funcall line "fswatch" (or (executable-find "fswatch") "not found"))
       (funcall line "rg" (or (executable-find "rg") "not found"))
       (funcall line "git" (or (executable-find "git") "not found"))
       ""
       "Issues")
      (if issues
          (mapcar (lambda (issue) (concat "  - " issue)) issues)
        (list "  No issues detected.")))
     "\n")))

;;;###autoload
(defun vulpea-doctor (&optional show)
  "Diagnose the Vulpea setup and return a report string.

The report covers versions, configuration, database state, sync
state, external tool availability, and a list of detected issues.
It is read-only: nothing is created or modified, even when the
database does not exist yet. Please include the report in bug
reports.

When SHOW is non-nil (always when called interactively), also
display the report in the *vulpea-doctor* buffer."
  (interactive (list t))
  (let ((report (vulpea-doctor--report)))
    (when show
      (with-current-buffer (get-buffer-create "*vulpea-doctor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min)))
        (special-mode)
        (display-buffer (current-buffer))))
    report))

;;; Customization

(defgroup vulpea nil
  "Vulpea note-taking system."
  :group 'org)

(defcustom vulpea-default-notes-directory nil
  "Default directory for creating new notes.

When nil (the default), dynamically resolves to the first entry in
`vulpea-db-sync-directories', which itself defaults to
`org-directory'.

Set this explicitly only if you want notes created in a different
directory than the first sync directory."
  :type '(choice (const :tag "Use first sync directory" nil)
                 (directory :tag "Explicit directory"))
  :group 'vulpea)

(defcustom vulpea-create-default-function nil
  "Function to compute default parameters for note creation.
Called with (title) and should return a plist of default parameters.
When nil, uses `vulpea-create-default-template' instead.

The function allows dynamic parameter computation based on context:

  (setq vulpea-create-default-function
        (lambda (title)
          (list :tags (if (string-match-p \"TODO\" title)
                          \\='(\"task\" \"inbox\")
                        \\='(\"note\" \"inbox\"))
                :head (format \"#+created: %s\"
                              (format-time-string \"[%Y-%m-%d]\"))
                :properties (list (cons \"SOURCE\"
                                        (buffer-name))))))

Parameters explicitly passed to `vulpea-create' override these defaults.

These defaults seed file-level note creation only.  When
`vulpea-create' is called with a non-nil `:parent' (a heading-level
note), this function is not called and no defaults are applied."
  :type '(choice (const :tag "Use template instead" nil)
          (function :tag "Function returning plist"))
  :group 'vulpea)

(defcustom vulpea-create-default-template
  '(:file-name "${timestamp}_${slug}.org")
  "Default template (plist) for note creation.
Only used when `vulpea-create-default-function' is nil.
Parameters explicitly passed to `vulpea-create' override these defaults.

These defaults seed file-level note creation only.  When
`vulpea-create' is called with a non-nil `:parent' (a heading-level
note), no defaults are consulted and the heading is built solely
from the explicitly passed arguments.

Supports all template expansion features:
  ${var}     - Variable substitution
  %(elisp)   - Elisp evaluation
  %<format>  - Timestamp formatting

Default configuration:
  \\='(:file-name \"${timestamp}_${slug}.org\")

Example customization:

  (setq vulpea-create-default-template
        \\='(:file-name \"inbox/${slug}.org\"
          :tags (\"fleeting\")
          :head \"#+created: %<[%Y-%m-%d]>\"
          :properties ((\"CREATED\" . \"%<[%Y-%m-%d]>\")
                       (\"AUTHOR\" . \"%(user-full-name)\"))
          :context (:source \"manual\")))

Note: %(elisp) and %<format> directives are honored only inside
the template fields themselves (e.g. :head, :properties values).
Context values are inserted literally and are not re-evaluated.

Available parameters:
  :file-name   - File name template (relative to default directory)
                 Can also be a function: (lambda (title) ...)
  :tags        - List of tag strings
  :head        - Header content after #+filetags
  :body        - Note body content
  :properties  - Alist of (key . value) for property drawer
  :meta        - Alist of (key . value) for metadata
  :context     - Plist of custom template variables

Template variables for :file-name:
  ${title}     - Note title
  ${slug}      - URL-friendly version of title
  ${timestamp} - Current timestamp (%Y%m%d%H%M%S)
  ${id}        - Note ID (UUID)"
  :type 'plist
  :group 'vulpea)

;;; Variables

(defvar vulpea-db-sync-directories)  ; Defined in vulpea-db
(defvar vulpea-find-default-filter nil
  "Default filter to use in `vulpea-find'.")

(defvar vulpea-find-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-find'.

Must be a function that accepts one argument - optional note
filter function.")

(defvar vulpea-find-default-create-fn #'vulpea-find-create-note
  "Default function to create a note in `vulpea-find'.

Called with two arguments - the title typed by the user and
capture properties (currently always nil, reserved for future
use) - mirroring the CREATE-FN argument of `vulpea-insert'. It
should create the note and return the resulting `vulpea-note' to
visit, or nil to skip visiting (e.g. when creation is interactive,
asynchronous, or was aborted).

This is the hook for \"capture on empty\" workflows: set it to a
function that routes to `org-capture' or your own command to turn
a fruitless search straight into note creation.")

;;; Helper Functions

(defun vulpea-title-to-slug (title)
  "Convert TITLE to URL-friendly slug.

Uses Unicode normalization to properly handle international characters
and diacritical marks. Implementation adapted from org-roam.

Credits: USAMI Kenta (@zonuexe)
See: https://github.com/org-roam/org-roam/pull/1460"
  (require 'ucs-normalize)
  (let ((slug-trim-chars
         ;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
         ;; For why these specific glyphs: https://github.com/org-roam/org-roam/pull/1460
         '( #x300 #x301 #x302 #x303 #x304 #x306 #x307
            #x308 #x309 #x30A #x30B #x30C #x31B #x323
            #x324 #x325 #x327 #x32D #x32E #x330 #x331)))
    (thread-last title
                 (ucs-normalize-NFD-string) ;; aka. `string-glyph-decompose' from Emacs 29
                 (seq-remove (lambda (char) (memq char slug-trim-chars)))
                 (apply #'string)
                 (ucs-normalize-NFC-string) ;; aka. `string-glyph-compose' from Emacs 29
                 (replace-regexp-in-string "[^[:alnum:]]" "_") ;; convert anything not alphanumeric
                 (replace-regexp-in-string "__*" "_")          ;; remove sequential underscores
                 (replace-regexp-in-string "^_" "")            ;; remove starting underscore
                 (replace-regexp-in-string "_$" "")            ;; remove ending underscore
                 (downcase))))

(define-obsolete-function-alias 'vulpea--title-to-slug #'vulpea-title-to-slug "2.0.0")

;;; Link Categorization

(defun vulpea--get-incoming-links-with-descriptions (note-id)
  "Get all links pointing to NOTE-ID with their descriptions.
Returns list of plists with :source-id :source-path :pos :description."
  (let* ((links (vulpea-db-query-links-to note-id))
         ;; Collect unique source IDs and batch fetch for paths
         (source-ids (delete-dups (mapcar (lambda (l) (plist-get l :source)) links)))
         (source-notes (vulpea-db-query-by-ids source-ids))
         ;; Build id->path lookup table
         (id-to-path (make-hash-table :test 'equal))
         result)
    (dolist (note source-notes)
      (puthash (vulpea-note-id note) (vulpea-note-path note) id-to-path))
    ;; Process links - description comes from database now
    (dolist (link links)
      (let* ((source-id (plist-get link :source))
             (source-path (gethash source-id id-to-path)))
        (when source-path
          (push (list :source-id source-id
                      :source-path source-path
                      :pos (plist-get link :pos)
                      :description (plist-get link :description))
                result))))
    (nreverse result)))

(defun vulpea--categorize-links (links old-title)
  "Categorize LINKS into exact and partial matches.
Case-insensitive matching against OLD-TITLE.
Returns plist (:exact :partial).

Exact matches: description equals old title (case-insensitive).
Partial matches: description contains old title but isn't exact.
Links using aliases are left unchanged (alias is still valid).
Links with nil descriptions or custom descriptions are excluded."
  (let ((exact '())
        (partial '())
        (title-down (downcase old-title)))
    (dolist (link links)
      (let ((desc (plist-get link :description)))
        (when desc
          (let ((desc-down (downcase desc)))
            (cond
             ;; Exact match (case-insensitive)
             ((string= desc-down title-down)
              (push link exact))
             ;; Partial match - contains but not exact
             ((string-match-p (regexp-quote title-down) desc-down)
              (push link partial)))))))
    (list :exact (nreverse exact)
          :partial (nreverse partial))))

(defun vulpea--update-link-description (file pos new-description)
  "Update link description at POS in FILE to NEW-DESCRIPTION.
Works for both bare links [[id:xxx]] and links with
descriptions [[id:xxx][old]]."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (cond
       ;; Link with existing description: [[id:xxx][old]]
       ((looking-at "\\(\\[\\[id:[^]]+\\]\\)\\[\\([^]]*\\)\\]\\]")
        (let ((link-part (match-string 1)))
          ;; LITERAL (3rd arg) so backslashes in NEW-DESCRIPTION are not
          ;; interpreted as match-group backreferences.
          (replace-match (concat link-part "[" new-description "]]") t t)))
       ;; Bare link without description: [[id:xxx]]
       ((looking-at "\\(\\[\\[id:[^]]+\\)\\]\\]")
        (let ((link-part (match-string 1)))
          (replace-match (concat link-part "][" new-description "]]") t t)))))))

(defun vulpea--default-directory ()
  "Return the default directory for creating new notes.

Resolution order:
  1. `vulpea-default-notes-directory' if set
  2. First directory from `vulpea-db-sync-directories' if set
  3. `org-directory' as fallback"
  (or vulpea-default-notes-directory
      (car vulpea-db-sync-directories)
      org-directory))

(defun vulpea--expand-template (template title &optional id context)
  "Expand TEMPLATE with TITLE, optional ID, and CONTEXT.
TEMPLATE is a string with placeholders:
  ${var}     - Variable substitution
  %(elisp)   - Elisp evaluation
  %<format>  - Timestamp formatting

CONTEXT is a plist of additional variables (e.g., :url \"...\").
Returns expanded string with placeholders replaced.

Built-in variables: ${title}, ${slug}, ${timestamp}, ${id}
Context variables: ${key} for each :key in CONTEXT.

Evaluation order matters for safety: the %(elisp) and %<format>
directives are expanded first, on the template itself, and only
then are ${var} and context values substituted in.  Consequently
%(...) and %<...> are honored only when written by the template
author - they are NOT re-evaluated when they appear inside a
substituted value such as TITLE or a CONTEXT value.  This keeps
untrusted data (e.g. a note title) from being executed as code.

Note: Does not support %a (annotation) or %i (initial content)
from org-capture as they don't make sense for programmatic creation."
  (let* ((slug (vulpea-title-to-slug title))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (id (or id (org-id-new)))
         (result template))

    ;; SECURITY: expand the active directives (%(elisp) and %<format>)
    ;; on the raw template FIRST, before substituting ${var} and context
    ;; values.  The directives are written by the template author and are
    ;; trusted; the substituted values (note title, slug, id, context)
    ;; are data and may be untrusted.  Substituting first and scanning
    ;; afterwards would let a value containing "%(...)" be evaluated as
    ;; code - an arbitrary code execution hazard.  Expanding before
    ;; substitution keeps all substituted data strictly literal.

    ;; Expand %(elisp) - evaluate elisp expressions
    ;; Note: save-match-data is critical because eval'd expressions may
    ;; call functions that do string matching, corrupting our match data
    (while (string-match "%\\((.+?)\\)" result)
      (let* ((expr (match-string 1 result))
             (match-beg (match-beginning 0))
             (match-end (match-end 0))
             (value (save-match-data
                      (condition-case err
                          (eval (car (read-from-string expr)))
                        (error (format "ERROR: %S" err))))))
        (setq result (concat (substring result 0 match-beg)
                             (format "%s" value)
                             (substring result match-end)))))

    ;; Expand %<format> - format timestamps
    (while (string-match "%<\\(.+?\\)>" result)
      (let* ((format-str (match-string 1 result))
             (value (format-time-string format-str)))
        (setq result (replace-match value t t result))))

    ;; Expand ${var} placeholders.  Done AFTER directive expansion so
    ;; substituted values are never re-scanned for %(...) or %<...>.
    (setq result (thread-last result
                              (s-replace "${title}" title)
                              (s-replace "${slug}" slug)
                              (s-replace "${timestamp}" timestamp)
                              (s-replace "${id}" id)))

    ;; Expand context variables (treated as literal data, like ${var})
    (when context
      (let ((ctx context))
        (while ctx
          (let* ((key (pop ctx))
                 (val (pop ctx))
                 (placeholder (format "${%s}" (substring (symbol-name key) 1))))
            (setq result (s-replace placeholder (format "%s" val) result))))))

    result))

(defun vulpea--expand-file-name-template (title &optional id template context)
  "Expand file name template with TITLE, ID, TEMPLATE, and CONTEXT.
If TEMPLATE is nil, uses `:file-name' from `vulpea-create-default-template'.
CONTEXT is a plist of additional template variables.
Returns absolute file path."
  (let* ((template (or template
                       (plist-get vulpea-create-default-template :file-name)
                       "${slug}.org"))  ; Absolute fallback
         (template-resolved (if (functionp template)
                                (funcall template title)
                              template))
         (file-name (vulpea--expand-template template-resolved title id context))
         (dir (vulpea--default-directory)))
    (expand-file-name file-name dir)))

(defun vulpea--format-note-content (id title &optional head meta tags properties)
  "Format note content for `org-capture' template.

ID and TITLE are required. Optional: HEAD, META (alist), TAGS (list),
PROPERTIES (alist)."
  (string-join
   (append
    (list
     ":PROPERTIES:"
     (format org-property-format ":ID:" id))
    (mapcar
     (lambda (prop)
       (format org-property-format
               (concat ":" (car prop) ":")
               (cdr prop)))
     properties)
    (list
     ":END:"
     (format "#+title: %s" title))
    (when tags
      (list (concat "#+filetags: :"
                    (string-join tags ":")
                    ":")))
    (when head (list head))
    (when meta
      (list ""))  ; blank line before meta
    (when meta
      (mapcar
       (lambda (kvp)
         (if (listp (cdr kvp))
             (mapconcat
              (lambda (val)
                (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format val)))
              (cdr kvp) "\n")
           (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format (cdr kvp)))))
       meta)))
   "\n"))


(defun vulpea-find-create-note (title &optional _props)
  "Create a new note with TITLE selected in `vulpea-find'.

Creates a file-level note via `vulpea-create' and returns the
resulting `vulpea-note'. PROPS mirrors the capture properties
argument of `vulpea-insert' CREATE-FN and is currently unused.

This is the default value of `vulpea-find-default-create-fn'."
  (vulpea-create title))

;;;###autoload
(cl-defun vulpea-find (&key other-window
                            filter-fn
                            candidates-fn
                            create-fn
                            require-match
                            (expand-aliases t))
  "Select and find a note.

If OTHER-WINDOW, visit the NOTE in another window.

CANDIDATES-FN is the function to query candidates for selection,
which takes as its argument a filtering function (see FILTER-FN).
Unless specified, `vulpea-find-default-candidates-source' is
used.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'. Unless specified,
`vulpea-find-default-filter' is used.

CREATE-FN controls how a new note is created when user selects a
non-existent note (only possible when REQUIRE-MATCH is nil). Like
the CREATE-FN of `vulpea-insert', it is called with two arguments
- the typed title and capture properties (currently always nil).
It should return the created `vulpea-note' to visit, or nil to
skip visiting. Unless specified, `vulpea-find-default-create-fn'
is used.

When REQUIRE-MATCH is nil user may select a non-existent note,
which is then created via CREATE-FN. When non-nil, only existing
notes may be selected.

When EXPAND-ALIASES is non-nil (the default), each note with
aliases will appear multiple times in the completion list - once
for the original title and once for each alias."
  (interactive)
  (let* ((region-text
          (when (region-active-p)
            (org-link-display-format
             (buffer-substring-no-properties
              (set-marker
               (make-marker) (region-beginning))
              (set-marker
               (make-marker) (region-end))))))
         (note (vulpea-select-from
                "Note"
                (funcall
                 (or
                  candidates-fn
                  vulpea-find-default-candidates-source)
                 (or
                  filter-fn
                  vulpea-find-default-filter))
                :require-match require-match
                :initial-prompt region-text
                :expand-aliases expand-aliases)))
    (if (vulpea-note-id note)
        ;; Existing note - visit it
        (vulpea-visit note other-window)
      ;; New note - create it
      (when (not require-match)
        (let ((new-note (funcall (or create-fn
                                     vulpea-find-default-create-fn)
                                 (vulpea-note-title note)
                                 nil)))
          (when new-note
            (vulpea-visit new-note other-window)))))))

;;;###autoload
(defun vulpea-find-backlink ()
  "Select and find a note linked to current note."
  (interactive)
  (let* ((id (or (org-entry-get nil "ID" t)
                 (user-error "Current location has no ID property")))
         (_ (unless (vulpea-db-get-by-id id)
              (user-error
               "%s is not a known note" id)))
         (backlinks (vulpea-db-query-by-links-some
                     (list (cons "id" id)))))
    (unless backlinks
      (user-error "There are no backlinks to the current note"))
    (vulpea-find
     :candidates-fn (lambda (_) backlinks)
     :require-match t)))



;;;###autoload
(defun vulpea-visit (note-or-id &optional other-window)
  "Visit NOTE-OR-ID.

If OTHER-WINDOW, visit the NOTE in another window."
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id))))
    (unless note
      (user-error "Cannot find note with ID: %s"
                  (if (vulpea-note-p note-or-id)
                      (vulpea-note-id note-or-id)
                    note-or-id)))
    (let ((file (vulpea-note-path note))
          (id (vulpea-note-id note)))
      ;; Visit the file
      (if (or current-prefix-arg other-window)
          (find-file-other-window file)
        (find-file file))
      ;; Go to the note position
      (if (= (vulpea-note-level note) 0)
          ;; File-level note: go to beginning
          (goto-char (point-min))
        ;; Heading-level note: search for the ID property
        (goto-char (point-min))
        (unless (re-search-forward
                 (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                 nil t)
          (user-error "Could not find heading with ID: %s" id))
        ;; Move to the heading
        (org-back-to-heading t))
      (if (fboundp 'org-fold-show-context)
          (org-fold-show-context)
        ;; Fallback for Org < 9.6; the function is obsolete there but
        ;; still the correct entry point, so silence the warning.
        (with-suppressed-warnings ((obsolete org-show-context))
          (org-show-context))))))



(defvar vulpea-insert-default-filter nil
  "Default filter to use in `vulpea-insert'.")

(defvar vulpea-insert-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-insert'.

Must be a function that accepts one argument - optional note
filter function.")

(defvar vulpea-insert-default-create-fn nil
  "Default function to create a note in `vulpea-insert'.

When non-nil, used as the CREATE-FN of `vulpea-insert' for a note
that does not exist yet (see its CREATE-FN argument): it is called
with the typed title and capture properties and is responsible for
both creating the note and inserting the link. When nil, the
built-in behavior is used (create via `vulpea-create' and insert
an id: link).

This mirrors `vulpea-find-default-create-fn' for \"capture on
empty\" workflows. Note that, unlike the `vulpea-find' hook, this
function must perform the link insertion itself, since inserting a
link is what `vulpea-insert' does with a new note.")

(defvar vulpea-insert-handle-functions nil
  "Abnormal hooks to run after `vulpea-note' is inserted.

Each function accepts a note that was inserted via
`vulpea-insert'.

The current point is the point of the new node. The hooks must
not move the point.")

;;;###autoload
(cl-defun vulpea-insert (&key filter-fn candidates-fn create-fn
                              (expand-aliases t))
  "Select a note and insert a link to it.

Allows capturing new notes. After link is inserted,
`vulpea-insert-handle-functions' are called with the inserted
note as the only argument regardless involvement of capture
process.

CANDIDATES-FN is the function to query candidates for selection,
which takes as its argument a filtering function (see FILTER-FN).
Unless specified, `vulpea-insert-default-candidates-source' is
used.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'. Unless specified,
`vulpea-insert-default-filter' is used.

CREATE-FN allows to control how a new note is created when user picks a
non-existent note. This function is called with two arguments - title
and capture properties. When CREATE-FN is nil,
`vulpea-insert-default-create-fn' is used; when that is also nil, the
default implementation is used.

When EXPAND-ALIASES is non-nil (the default), each note with
aliases will appear multiple times in the completion list - once
for the original title and once for each alias."
  (interactive)
  (unwind-protect
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq
                     beg (set-marker
                          (make-marker) (region-beginning))
                     end (set-marker
                          (make-marker) (region-end))
                     region-text
                     (org-link-display-format
                      (buffer-substring-no-properties
                       beg end)))))
               (notes (funcall (or candidates-fn
                                   vulpea-insert-default-candidates-source)
                               (or filter-fn vulpea-insert-default-filter)))
               (note (vulpea-select-from "Note" notes
                                         :initial-prompt region-text
                                         :expand-aliases expand-aliases))
               (description (or region-text
                                (vulpea-note-title note))))
          (if (vulpea-note-id note)
              ;; Existing note - insert link immediately
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (insert (org-link-make-string
                         (concat "id:" (vulpea-note-id note))
                         description))
                (run-hook-with-args
                 'vulpea-insert-handle-functions
                 note))
            ;; New note - create it then insert link
            (let ((cfn (or create-fn vulpea-insert-default-create-fn)))
              (if cfn
                  (funcall cfn (vulpea-note-title note) nil)
                ;; Create the note programmatically
                (let ((new-note (vulpea-create (vulpea-note-title note))))
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  (insert (org-link-make-string
                           (concat "id:" (vulpea-note-id new-note))
                           description))
                  (run-hook-with-args
                   'vulpea-insert-handle-functions
                   new-note)))))))
    (deactivate-mark)))



(defun vulpea--format-heading-content (level id title &optional tags properties body)
  "Format a heading-level note content.

LEVEL is the heading depth (number of stars).
ID and TITLE are required.
TAGS is a list of tag strings (inserted as headline tags).
PROPERTIES is an alist of (key . value) for the property drawer.
BODY is optional body text after the property drawer."
  (string-join
   (append
    ;; Heading line with optional tags
    (list (concat (make-string level ?*)
                  " "
                  title
                  (when tags
                    (concat " :" (string-join tags ":") ":"))))
    ;; Property drawer
    (list ":PROPERTIES:"
          (format org-property-format ":ID:" id))
    (mapcar
     (lambda (prop)
       (format org-property-format
               (concat ":" (car prop) ":")
               (cdr prop)))
     properties)
    (list ":END:")
    ;; Body
    (when body (list body)))
   "\n"))

(defun vulpea--find-heading-insertion-point (parent-note _level after)
  "Move point to where a new child heading should be inserted.

PARENT-NOTE is the parent vulpea-note.
_LEVEL is accepted for call-site symmetry but is not used; the
insertion point is derived from PARENT-NOTE.
AFTER controls position:
  \\='last (default) - append as last child
  nil - insert as first child
  string - insert after the child heading with that ID.

Return non-nil when the new heading will follow an existing sibling
subtree (so a separating blank line is wanted), and nil when it will
be the first child or the first heading in the file.  This function
only positions point; surrounding blank lines are managed by
`vulpea--insert-heading-content'."
  (let ((parent-level (vulpea-note-level parent-note)))
    (cond
     ;; Insert as first child
     ((null after)
      (if (= parent-level 0)
          ;; File-level parent: position before the first heading
          (progn
            (goto-char (point-min))
            (if (re-search-forward "^\\*+ " nil t)
                (goto-char (match-beginning 0))
              (goto-char (point-max))))
        ;; Heading parent: position after the parent's property drawer
        (goto-char (vulpea-note-pos parent-note))
        (forward-line 1)
        ;; Skip past property drawer if present
        (when (looking-at-p "[ \t]*:PROPERTIES:")
          (re-search-forward "^[ \t]*:END:" nil t)
          (forward-line 1)))
      nil)

     ;; Insert after specific sibling
     ((stringp after)
      (let ((sibling-pos (org-id-find after 'marker)))
        (unless sibling-pos
          (error "vulpea-create: Sibling note with ID %s not found" after))
        (goto-char sibling-pos)
        ;; Move past the sibling's entire subtree
        (org-end-of-subtree t))
      t)

     ;; Insert as last child (default)
     (t
      (if (= parent-level 0)
          ;; File-level parent: append at end of file
          (let ((has-sibling (save-excursion
                               (goto-char (point-min))
                               (and (re-search-forward "^\\*+ " nil t) t))))
            (goto-char (point-max))
            has-sibling)
        ;; Heading parent: append at end of the parent's subtree
        (let* ((parent-pos (vulpea-note-pos parent-note))
               (subtree-end (save-excursion
                              (goto-char parent-pos)
                              (org-end-of-subtree t)
                              (point)))
               (has-sibling (save-excursion
                              (goto-char parent-pos)
                              (forward-line 1)
                              (and (re-search-forward "^\\*+ " subtree-end t) t))))
          (goto-char parent-pos)
          (org-end-of-subtree t)
          has-sibling))))))

(defun vulpea--insert-heading-content (content blank-before)
  "Insert heading CONTENT at point with normalized blank lines.

CONTENT is heading text with no leading or trailing blank lines.
Any whitespace already surrounding point is removed first so the
result is deterministic.  When there is preceding content, CONTENT
starts on its own line, preceded by exactly one blank line when
BLANK-BEFORE is non-nil and none otherwise.  CONTENT is followed by
a single newline, which also guarantees a single trailing newline
when inserting at the end of the buffer."
  (skip-chars-backward " \t\n")
  (let ((preceding (not (bobp)))
        (following (save-excursion
                     (skip-chars-forward " \t\n")
                     (point))))
    (delete-region (point) following)
    (when preceding
      (insert "\n")
      (when blank-before (insert "\n")))
    (insert content "\n")))

;;;###autoload
(cl-defun vulpea-create (title
                         &optional file-name
                         &key
                         id
                         head
                         meta
                         body
                         context
                         properties
                         tags
                         parent
                         (after 'last))
  "Create a new note with TITLE programmatically.

This function is designed for programmatic note creation with
immediate finalization. For interactive note capture with user
editing, use `org-capture' with vulpea-compatible templates.

FILE-NAME is optional. When nil, uses `:file-name' from
`vulpea-create-default-template' to generate the file name.
Ignored when PARENT is provided (file is determined by parent).

Defaults from `vulpea-create-default-function' and
`vulpea-create-default-template' are applied only when creating a
file-level note (PARENT is nil).  When PARENT is provided no
defaults are consulted; the heading is built solely from the
arguments passed here.

Returns the created `vulpea-note' object.

ID is automatically generated unless explicitly passed.

When PARENT is nil, creates a file-level note:

  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  #+title: TITLE
  #+filetags: TAGS if present
  HEAD if present

  META if present

  BODY if present

When PARENT is a `vulpea-note', creates a heading-level note
inside the parent's file at level (parent-level + 1):

  * TITLE :tags:
  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  BODY if present

AFTER controls insertion position among siblings (only when
PARENT is provided):
  \\='last (default) - append as last child
  nil - insert as first child
  string (note ID) - insert after the child with that ID

Optional parameters:

- PROPERTIES: Alist of (key_str . val_str) for property drawer
- META: Alist of (key . value) or (key . (list of values))
- TAGS: List of tag strings
- BODY: Note body content (supports template expansion)
- HEAD: Additional header content (supports template expansion)
- CONTEXT: Plist of template variables (e.g., :url \"...\")

Template expansion is supported in FILE-NAME, HEAD, BODY, TAGS,
PROPERTIES values, and META values:
  ${var}     - Variable substitution (title, slug, timestamp, id, custom)
  %(elisp)   - Elisp evaluation (e.g., %(user-full-name))
  %<format>  - Timestamp formatting (e.g., %<[%Y-%m-%d]>)

Note: Does not support %a or %i from org-capture."
  (let* ((id (or id (org-id-new)))
         (context (or context nil)))
    (if parent
        ;; Heading-level note creation
        (vulpea--create-heading title id parent after
                               body tags properties context)
      ;; File-level note creation (original behavior)
      (vulpea--create-file title file-name id head meta body
                           tags properties context))))

(defun vulpea--create-file (title file-name id head meta body tags properties context)
  "Create a file-level note with TITLE.

FILE-NAME, ID, HEAD, META, BODY, TAGS, PROPERTIES, and CONTEXT
are as documented in `vulpea-create'."
  ;; Get defaults from function or template
  (let* ((defaults (cond
                    (vulpea-create-default-function
                     (funcall vulpea-create-default-function title))
                    (vulpea-create-default-template
                     vulpea-create-default-template)
                    (t nil)))
         ;; Merge explicit parameters with defaults (explicit takes precedence)
         (file-name (or file-name (plist-get defaults :file-name)))
         (head (or head (plist-get defaults :head)))
         (body (or body (plist-get defaults :body)))
         (tags (or tags (plist-get defaults :tags)))
         (properties (or properties (plist-get defaults :properties)))
         (meta (or meta (plist-get defaults :meta)))
         (context (or context (plist-get defaults :context)))
         (file-path (vulpea--expand-file-name-template title id file-name context))
         ;; Expand templates everywhere with context
         (expanded-head (when head
                          (vulpea--expand-template head title id context)))
         (expanded-body (when body
                          (vulpea--expand-template body title id context)))
         (expanded-tags (when tags
                          (mapcar (lambda (tag)
                                    (vulpea--expand-template tag title id context))
                                  tags)))
         (expanded-properties (when properties
                                (mapcar (lambda (prop)
                                          (cons (car prop)
                                                (vulpea--expand-template (cdr prop) title id context)))
                                        properties)))
         (expanded-meta (when meta
                          (mapcar (lambda (kvp)
                                    (cons (car kvp)
                                          (if (listp (cdr kvp))
                                              ;; List of values
                                              (mapcar (lambda (val)
                                                        (if (stringp val)
                                                            (vulpea--expand-template val title id context)
                                                          val))
                                                      (cdr kvp))
                                            ;; Single value
                                            (if (stringp (cdr kvp))
                                                (vulpea--expand-template (cdr kvp) title id context)
                                              (cdr kvp)))))
                                  meta)))
         (content (vulpea--format-note-content id title expanded-head expanded-meta expanded-tags expanded-properties))
         (full-content (if expanded-body
                           (concat content "\n\n" expanded-body)
                         content))
         (dir (file-name-directory file-path)))

    ;; Ensure directory exists
    (unless (file-directory-p dir)
      (make-directory dir t))

    ;; Safety check: refuse to overwrite existing files
    (when (file-exists-p file-path)
      (error "vulpea-create: File %s already exists; refusing to overwrite" file-path))

    ;; Write file directly (no org-capture, no hooks, no blank lines)
    (with-temp-buffer
      (insert full-content)
      (write-region (point-min) (point-max) file-path nil 'silent))

    ;; Register ID with org-id so links can be followed
    (org-id-add-location id file-path)

    ;; Update database with the new file
    (let ((update-count (vulpea-db-update-file file-path)))
      (when (zerop update-count)
        (error "vulpea-create: No notes extracted from file %s (expected ID %s)"
               file-path id)))

    ;; Return the note
    (or (vulpea-db-get-by-id id)
        (error "vulpea-create: Note with ID %s not found in database after creation" id))))

(defun vulpea--create-heading (title id parent after body tags properties context)
  "Create a heading-level note with TITLE under PARENT.

ID is the note identifier.
PARENT is the parent `vulpea-note'.
AFTER controls insertion position (see `vulpea-create').
BODY, TAGS, PROPERTIES, and CONTEXT are as in `vulpea-create'."
  (let* ((file-path (vulpea-note-path parent))
         (level (1+ (vulpea-note-level parent)))
         ;; Expand templates
         (expanded-body (when body
                          (vulpea--expand-template body title id context)))
         (expanded-tags (when tags
                          (mapcar (lambda (tag)
                                    (vulpea--expand-template tag title id context))
                                  tags)))
         (expanded-properties (when properties
                                (mapcar (lambda (prop)
                                          (cons (car prop)
                                                (vulpea--expand-template (cdr prop) title id context)))
                                        properties)))
         (heading-content (vulpea--format-heading-content
                           level id title expanded-tags
                           expanded-properties expanded-body)))

    ;; Validate parent file exists
    (unless (file-exists-p file-path)
      (error "vulpea-create: Parent file %s does not exist" file-path))

    ;; Insert heading into parent's file
    (with-current-buffer (find-file-noselect file-path)
      (org-with-wide-buffer
       (let ((blank-before (vulpea--find-heading-insertion-point
                            parent level after)))
         (vulpea--insert-heading-content (string-trim-right heading-content)
                                         blank-before)))
      (save-buffer))

    ;; Register ID with org-id
    (org-id-add-location id file-path)

    ;; Update database
    (vulpea-db-update-file file-path)

    ;; Return the note
    (or (vulpea-db-get-by-id id)
        (error "vulpea-create: Heading note with ID %s not found in database after creation" id))))



;;; Title Change Detection Mode

(defvar-local vulpea--title-before-save nil
  "Title of note before save, for change detection.")

(defvar-local vulpea--note-id-before-save nil
  "ID of note before save, for change detection.")

(defun vulpea--capture-before-save ()
  "Capture note ID and title before save for change detection.
The old title is read from the database, not the buffer."
  (when (derived-mode-p 'org-mode)
    (setq vulpea--note-id-before-save (org-entry-get nil "ID"))
    (setq vulpea--title-before-save
          (when vulpea--note-id-before-save
            (caar (emacsql (vulpea-db)
                           [:select title :from notes :where (= id $s1)]
                           vulpea--note-id-before-save))))))

(defun vulpea--notify-title-change ()
  "After save, check if title changed and notify user."
  (when (and vulpea--note-id-before-save
             vulpea--title-before-save
             (derived-mode-p 'org-mode))
    (let ((new-title (vulpea-buffer-title-get)))
      (when (and new-title
                 (not (string= new-title vulpea--title-before-save)))
        (message
         (concat "Title changed from \"%s\" to \"%s\". "
                 "Run M-x vulpea-propagate-title-change to update.")
         vulpea--title-before-save new-title)))))

;;;###autoload
(define-minor-mode vulpea-title-change-detection-mode
  "Minor mode to detect title changes and notify user.

When enabled, this mode tracks the note's title before each save.
After saving, if the title has changed, it notifies the user and
suggests running `vulpea-propagate-title-change' to update incoming
link descriptions."
  :lighter " VulpTD"
  :group 'vulpea
  (if vulpea-title-change-detection-mode
      (progn
        (add-hook 'before-save-hook #'vulpea--capture-before-save nil t)
        (add-hook 'after-save-hook #'vulpea--notify-title-change nil t))
    (remove-hook 'before-save-hook #'vulpea--capture-before-save t)
    (remove-hook 'after-save-hook #'vulpea--notify-title-change t)))

;;; Title Propagation Command

;;;###autoload
(cl-defun vulpea-propagate-title-change (&optional note-or-id)
  "Propagate title change for NOTE-OR-ID to filename and links.

With prefix arg (\\[universal-argument]), preview changes without
applying (dry-run).

When called interactively:
- Determines the note from current buffer or prompts user
- Prompts for old title if not recently detected
- Offers to rename the file based on new title
- Updates exact-match link descriptions to new title
- Shows partial matches for manual review

Interactive flow:
1. Prompt for file rename (y/n)
2. For exact matches: [!] Update all, [r] Review, [s] Skip, [q] Quit
3. Partial matches shown with option to open files"
  (interactive)
  (let* ((dry-run current-prefix-arg)
         ;; Determine the note
         (note (cond
                ((vulpea-note-p note-or-id) note-or-id)
                ((stringp note-or-id) (vulpea-db-get-by-id note-or-id))
                (t (when-let* ((id (org-entry-get nil "ID")))
                     (vulpea-db-get-by-id id)))))
         (note (or note
                   (vulpea-select "Note to propagate")))
         (note-id (vulpea-note-id note))
         (new-title (vulpea-note-title note))
         ;; Get old title - from detection or prompt
         (old-title
          (or vulpea--title-before-save
              (read-string (format "Old title (new: \"%s\"): " new-title))))
         ;; Get incoming links
         (links (vulpea--get-incoming-links-with-descriptions note-id))
         (categorized (vulpea--categorize-links links old-title))
         (exact-links (plist-get categorized :exact))
         (partial-links (plist-get categorized :partial))
         (exact-count (length exact-links))
         (partial-count (length partial-links)))

    ;; Check if title actually changed
    (when (string= old-title new-title)
      (user-error "Title has not changed (\"%s\")" new-title))

    ;; Dry-run: just show summary
    (when dry-run
      (with-output-to-temp-buffer "*vulpea-propagate-preview*"
        (princ (format "Title propagation preview for: %s\n" note-id))
        (princ (format "Old title: %s\n" old-title))
        (princ (format "New title: %s\n\n" new-title))
        (princ (format "File rename: %s → %s\n\n"
                       (file-name-nondirectory (vulpea-note-path note))
                       (concat (vulpea-title-to-slug new-title) ".org")))
        (princ (format "Exact matches (%d):\n" exact-count))
        (dolist (link exact-links)
          (princ (format "  %s at pos %d: \"%s\"\n"
                         (file-name-nondirectory (plist-get link :source-path))
                         (plist-get link :pos)
                         (plist-get link :description))))
        (princ (format "\nPartial matches (%d):\n" partial-count))
        (dolist (link partial-links)
          (princ (format "  %s at pos %d: \"%s\"\n"
                         (file-name-nondirectory (plist-get link :source-path))
                         (plist-get link :pos)
                         (plist-get link :description)))))
      (message "Dry-run complete. See *vulpea-propagate-preview* buffer.")
      (cl-return-from vulpea-propagate-title-change))

    ;; Offer file rename for file-level notes
    (when (and (= (vulpea-note-level note) 0)
               (y-or-n-p
                (format "Rename file \"%s\" → \"%s\"? "
                        (file-name-nondirectory (vulpea-note-path note))
                        (concat (vulpea-title-to-slug new-title) ".org"))))
      (condition-case err
          (vulpea-rename-file note new-title)
        (error (message "File rename failed: %s" (error-message-string err)))))

    ;; Handle exact matches
    (when (> exact-count 0)
      (message "Found %d exact match%s, %d partial match%s"
               exact-count (if (= exact-count 1) "" "es")
               partial-count (if (= partial-count 1) "" "es"))
      (let ((action
             (read-char-choice
              (format "Exact (%d): [!] All  [r] Review  [s] Skip  [q] Quit: "
                      exact-count)
              '(?! ?r ?s ?q))))
        (pcase action
          (?! ;; Update all exact matches
           (dolist (link exact-links)
             (vulpea--update-link-description
              (plist-get link :source-path)
              (plist-get link :pos)
              new-title)
             (when-let* ((buf (get-file-buffer (plist-get link :source-path))))
               (with-current-buffer buf
                 (save-buffer))))
           (message "Updated %d link%s"
                    exact-count (if (= exact-count 1) "" "s")))
          (?r ;; Review individually
           (let ((updated 0))
             (dolist (link exact-links)
               (let ((path (plist-get link :source-path))
                     (pos (plist-get link :pos))
                     (desc (plist-get link :description)))
                 (when (y-or-n-p (format "Update \"%s\" in %s? "
                                         desc (file-name-nondirectory path)))
                   (vulpea--update-link-description path pos new-title)
                   (when-let* ((buf (get-file-buffer path)))
                     (with-current-buffer buf
                       (save-buffer)))
                   (cl-incf updated))))
             (message "Updated %d of %d link%s"
                      updated exact-count (if (= exact-count 1) "" "s"))))
          (?s ;; Skip exact matches
           (message "Skipped exact matches"))
          (?q ;; Quit
           (user-error "Aborted")))))

    ;; Handle partial matches
    (when (> partial-count 0)
      (when (y-or-n-p
             (format "Open %d file%s with partial matches for editing? "
                     partial-count (if (= partial-count 1) "" "s")))
        (let ((files (delete-dups
                      (mapcar (lambda (l) (plist-get l :source-path))
                              partial-links))))
          (dolist (file files)
            (find-file-other-window file)))))

    ;; Clear detection state
    (setq vulpea--title-before-save nil)

    (message "Title propagation complete.")))

;;;###autoload
(defun vulpea-rename-file (note-or-id new-title)
  "Rename NOTE-OR-ID's file based on NEW-TITLE slug.
Updates the file on disk and database.

The new filename is generated as NEW-TITLE converted to slug with
.org extension, placed in the same directory as the original file.

Returns the new file path.

Signals an error if:
- The note cannot be found
- The target file already exists
- The note is a heading-level note (level > 0)"
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id)))
         (old-path (when note (vulpea-note-path note)))
         (dir (when old-path (file-name-directory old-path)))
         (new-filename (concat (vulpea-title-to-slug new-title) ".org"))
         (new-path (when dir (expand-file-name new-filename dir))))
    (unless note
      (error "vulpea-rename-file: Cannot find note with ID: %s"
             (if (vulpea-note-p note-or-id)
                 (vulpea-note-id note-or-id)
               note-or-id)))
    (when (> (vulpea-note-level note) 0)
      (error "vulpea-rename-file: Cannot rename file for heading-level note"))
    (when (file-exists-p new-path)
      (error "vulpea-rename-file: Target file already exists: %s" new-path))
    ;; Kill buffer if file is open
    (let ((buf (get-file-buffer old-path)))
      (when buf
        (with-current-buffer buf
          (save-buffer))
        (kill-buffer buf)))
    ;; Rename file on disk
    (rename-file old-path new-path)
    ;; Update org-id location
    (org-id-add-location (vulpea-note-id note) new-path)
    ;; Delete old file from database and add new one
    (vulpea-db--delete-file-notes old-path)
    (vulpea-db-update-file new-path)
    new-path))



;;; Schema authoring

(defun vulpea--schema-buffer-note (&optional schema)
  "Build a synthetic `vulpea-note' from the current buffer.

The note always carries the buffer's title and tags.  When SCHEMA is
given, it also carries the current values of that schema's field keys,
so predicates and conditional :required / :one-of rules see in-buffer
content while authoring."
  (make-vulpea-note
   :title (vulpea-buffer-title-get)
   :tags (vulpea-buffer-tags-get)
   :meta (when schema
           (delq nil
                 (mapcar
                  (lambda (field)
                    (let* ((key (plist-get field :key))
                           (vals (vulpea-buffer-meta-get-list key 'string)))
                      (when vals (cons key vals))))
                  (vulpea-schema-fields (vulpea-schema--resolve schema)))))))

(defun vulpea--schema-read-schema (note)
  "Choose a schema to author NOTE against, prompting when ambiguous.

Returns a schema name symbol.  Uses the schema applicable to NOTE when
exactly one matches, prompts among the matches when several do, and
prompts over all registered schemas when none match."
  (let ((applicable (vulpea-schema-applicable note)))
    (cond
     ((= (length applicable) 1) (car applicable))
     (applicable
      (intern (completing-read "Schema: " (mapcar #'symbol-name applicable) nil t)))
     (t
      (let ((all (vulpea-schema-list)))
        (unless all (user-error "No schemas are registered"))
        (intern (completing-read "Schema: " (mapcar #'symbol-name all) nil t)))))))

(defun vulpea--schema-prompt-field (field note required)
  "Prompt for a value for FIELD.

NOTE gives context and REQUIRED is non-nil when the field is required.
Honors :type (note selection for `note' / `link'), :one-of (completion)
and :one-of with :multiple (multi-selection).  Quitting a note prompt
skips that field.  Returns the entered value, a list of values, or an
empty value when skipped."
  (let* ((type (or (plist-get field :type) 'string))
         (one-of (vulpea-schema--call-or-value (plist-get field :one-of) note))
         (multiple (plist-get field :multiple))
         (prompt (format "%s%s: " (plist-get field :key)
                         (if required " (required)" "")))
         (candidates (lambda () (mapcar (lambda (v) (format "%s" v)) one-of))))
    (cond
     ((memq type '(note link))
      (condition-case nil
          (vulpea-select prompt :require-match t)
        (quit nil)))
     ((and one-of multiple)
      (completing-read-multiple prompt (funcall candidates)))
     (one-of
      (completing-read prompt (funcall candidates)))
     (t (read-string prompt)))))

(defun vulpea--schema-prompt-fields (fields note)
  "Prompt for each field in FIELDS, returning a (KEY . VALUE) alist.

NOTE supplies context for conditional :required and :one-of.  An empty
answer drops an optional field but keeps a required one as an empty
placeholder, so the author is still reminded of it."
  (let (values)
    (dolist (field fields)
      (let* ((key (plist-get field :key))
             (required (vulpea-schema--call-or-value
                        (plist-get field :required) note))
             (value (vulpea--schema-prompt-field field note required))
             ;; a multi-value answer may contain blank entries (e.g. an
             ;; empty `completing-read-multiple'); drop them so an empty
             ;; optional field is not written as a stray placeholder
             (value (if (listp value) (remove "" value) value)))
        (cond
         ((and value (not (equal value "")))
          (push (cons key value) values))
         (required (push (cons key "") values)))))
    (nreverse values)))

(defun vulpea--schema-insert-field-values (fields values &optional bound)
  "Write FIELDS into the current buffer, taking values from VALUES.

FIELDS is an ordered list of field specs.  VALUES is an alist mapping a
field :key to a value or list of values; a field absent from VALUES is
written as an empty placeholder.  Fields are appended in order, so a
`note' value (or a bare id) becomes a proper link via
`vulpea-buffer-meta-format'.  BOUND limits the scope as in
`vulpea-buffer-meta-set'."
  (dolist (field fields)
    (let* ((key (plist-get field :key))
           (cell (assoc key values)))
      (vulpea-buffer-meta-set key (if cell (cdr cell) "") 'append bound))))

;;;###autoload
(defun vulpea-schema-insert-fields (&optional schema-or-name skeleton)
  "Insert an applicable schema's fields into the current buffer.

The schema is taken from SCHEMA-OR-NAME when given, otherwise chosen
from the schemas applicable to the current buffer (prompting when
several apply, or over all registered schemas when none do).

For each field the note does not already carry, prompt for a value -
offering :one-of values as completion and selecting a note for `note'
fields - and insert it; required fields are handled first.  With a
prefix argument (SKELETON non-nil), skip prompting and insert empty
placeholders for every missing field instead."
  (interactive (list nil current-prefix-arg))
  (let* ((schema (or schema-or-name
                     (vulpea--schema-read-schema (vulpea--schema-buffer-note))))
         (note (vulpea--schema-buffer-note schema))
         (fields (vulpea-schema-missing-fields note schema)))
    (if skeleton
        (vulpea--schema-insert-field-values fields nil)
      (let ((values (vulpea--schema-prompt-fields fields note)))
        (vulpea--schema-insert-field-values
         (cl-remove-if-not (lambda (f) (assoc (plist-get f :key) values)) fields)
         values)))))

(provide 'vulpea)
;;; vulpea.el ends here
