;;; vulpea.el --- Note management library for Org mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.1.0
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

(require 'org-capture)
(require 'org-id)
(require 'vulpea-buffer)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-sync)
(require 'vulpea-meta)
(require 'vulpea-note)
(require 'vulpea-select)
(require 'vulpea-tags)
(require 'vulpea-utils)
(require 's)

;;; Customization

(defgroup vulpea nil
  "Vulpea note-taking system."
  :group 'org)

(defcustom vulpea-default-notes-directory
  (car vulpea-db-sync-directories)
  "Default directory for creating new notes.

Defaults to the first entry in `vulpea-db-sync-directories', which
itself defaults to `org-directory'.

This allows you to control where `vulpea-create' places new notes
without specifying an explicit file path."
  :type 'directory
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

Parameters explicitly passed to `vulpea-create' override these defaults."
  :type '(choice (const :tag "Use template instead" nil)
          (function :tag "Function returning plist"))
  :group 'vulpea)

(defcustom vulpea-create-default-template
  '(:file-name "${timestamp}_${slug}.org")
  "Default template (plist) for note creation.
Only used when `vulpea-create-default-function' is nil.
Parameters explicitly passed to `vulpea-create' override these defaults.

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
          :context (:source \"%(buffer-name)\")))

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

;;; Link Description Extraction and Categorization

(defun vulpea--extract-link-description-at-pos (file pos)
  "Extract link description from FILE at POS.
Returns the description string or nil if link has no description.

The link format is [[id:xxx][description]] or [[id:xxx]].
POS should point to the opening brackets of the link."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      ;; We're at the start of [[id:...
      (when (looking-at "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]")
        (match-string-no-properties 1)))))

(defun vulpea--get-incoming-links-with-descriptions (note-id)
  "Get all links pointing to NOTE-ID with their descriptions.
Returns list of plists with :source-id :source-path :pos :description."
  (let* ((links (vulpea-db-query-links-to note-id))
         result)
    (dolist (link links)
      (let* ((source-id (plist-get link :source))
             (source-note (vulpea-db-get-by-id source-id))
             (source-path (when source-note (vulpea-note-path source-note)))
             (pos (plist-get link :pos))
             (description
              (when source-path
                (vulpea--extract-link-description-at-pos source-path pos))))
        (when source-path
          (push (list :source-id source-id
                      :source-path source-path
                      :pos pos
                      :description description)
                result))))
    (nreverse result)))

(defun vulpea--categorize-links (links old-title old-aliases)
  "Categorize LINKS into exact and partial matches.
Case-insensitive matching against OLD-TITLE and OLD-ALIASES.
Returns plist (:exact :partial).

Exact matches: description equals old title or any alias (case-insensitive).
Partial matches: description contains old title or any alias but isn't exact.
Links with nil descriptions or custom descriptions are excluded."
  (let ((exact '())
        (partial '())
        (match-strings (cons old-title (or old-aliases '()))))
    (dolist (link links)
      (let ((desc (plist-get link :description)))
        (when desc
          (let ((desc-down (downcase desc))
                (is-exact nil)
                (is-partial nil))
            ;; Check against title and all aliases
            (dolist (match-str match-strings)
              (let ((match-down (downcase match-str)))
                (cond
                 ;; Exact match (case-insensitive)
                 ((string= desc-down match-down)
                  (setq is-exact t))
                 ;; Partial match - contains but not exact
                 ((and (not is-exact)
                       (string-match-p (regexp-quote match-down) desc-down))
                  (setq is-partial t)))))
            ;; Categorize
            (cond
             (is-exact (push link exact))
             (is-partial (push link partial)))))))
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
          (replace-match (concat link-part "[" new-description "]]"))))
       ;; Bare link without description: [[id:xxx]]
       ((looking-at "\\(\\[\\[id:[^]]+\\)\\]\\]")
        (let ((link-part (match-string 1)))
          (replace-match (concat link-part "][" new-description "]]"))))))))

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

Note: Does not support %a (annotation) or %i (initial content)
from org-capture as they don't make sense for programmatic creation."
  (let* ((slug (vulpea-title-to-slug title))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (id (or id (org-id-new)))
         (result template))

    ;; Expand ${var} placeholders
    (setq result (thread-last result
                              (s-replace "${title}" title)
                              (s-replace "${slug}" slug)
                              (s-replace "${timestamp}" timestamp)
                              (s-replace "${id}" id)))

    ;; Expand context variables
    (when context
      (let ((ctx context))
        (while ctx
          (let* ((key (pop ctx))
                 (val (pop ctx))
                 (placeholder (format "${%s}" (substring (symbol-name key) 1))))
            (setq result (s-replace placeholder (format "%s" val) result))))))

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


;;;###autoload
(cl-defun vulpea-find (&key other-window
                            filter-fn
                            candidates-fn
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

When REQUIRE-MATCH is nil user may select a non-existent note and
start the capture process.

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
        (let ((new-note (vulpea-create (vulpea-note-title note))))
          (when new-note
            (vulpea-visit new-note other-window)))))))

;;;###autoload
(defun vulpea-find-backlink ()
  "Select and find a note linked to current note."
  (interactive)
  (let* ((id (or (org-entry-get nil "ID")
                 (user-error "Current location has no ID property")))
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
        (org-show-context)))))



(defvar vulpea-insert-default-filter nil
  "Default filter to use in `vulpea-insert'.")

(defvar vulpea-insert-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-insert'.

Must be a function that accepts one argument - optional note
filter function.")

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
and capture properties. When CREATE-FN is nil, default implementation is
used.

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
            (if create-fn
                (funcall create-fn (vulpea-note-title note) nil)
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
                 new-note))))))
    (deactivate-mark)))



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
                         tags)
  "Create a new note file with TITLE programmatically.

This function is designed for programmatic note creation with
immediate finalization. For interactive note capture with user
editing, use `org-capture' with vulpea-compatible templates.

FILE-NAME is optional. When nil, uses `:file-name' from
`vulpea-create-default-template' to generate the file name.

Returns the created `vulpea-note' object.

ID is automatically generated unless explicitly passed.

Structure of the generated file:

  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  #+title: TITLE
  #+filetags: TAGS if present
  HEAD if present

  META if present

  BODY if present

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
  ;; Get defaults from function or template
  (let* ((defaults (cond
                    (vulpea-create-default-function
                     (funcall vulpea-create-default-function title))
                    (vulpea-create-default-template
                     vulpea-create-default-template)
                    (t nil)))
         ;; Merge explicit parameters with defaults (explicit takes precedence)
         (id (or id (org-id-new)))
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



;;; Title Change Detection Mode

(defvar-local vulpea--title-before-save nil
  "Title of note before save, for change detection.")

(defvar-local vulpea--aliases-before-save nil
  "Aliases of note before save, for change detection.")

(defvar-local vulpea--note-id-before-save nil
  "ID of note before save, for change detection.")

(defun vulpea--capture-before-save ()
  "Capture title/aliases/id before save for change detection."
  (when (derived-mode-p 'org-mode)
    (setq vulpea--note-id-before-save (org-entry-get nil "ID"))
    (setq vulpea--title-before-save (vulpea-buffer-title-get))
    (setq vulpea--aliases-before-save (vulpea-buffer-alias-get))))

(defun vulpea--notify-title-change ()
  "After save, check if title changed and notify user."
  (when (and vulpea--note-id-before-save
             vulpea--title-before-save
             (derived-mode-p 'org-mode))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+title:[ \t]*\\(.+\\)$" nil t)
        (let ((new-title (match-string-no-properties 1)))
          (unless (string= new-title vulpea--title-before-save)
            (message
             (concat "Title changed from \"%s\" to \"%s\". "
                     "Run M-x vulpea-propagate-title-change to update.")
             vulpea--title-before-save new-title)))))))

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
                (t (when-let ((id (org-entry-get nil "ID")))
                     (vulpea-db-get-by-id id)))))
         (note (or note
                   (vulpea-select "Note to propagate")))
         (note-id (vulpea-note-id note))
         (new-title (vulpea-note-title note))
         ;; Get old title - from detection or prompt
         (old-title
          (or vulpea--title-before-save
              (read-string (format "Old title (new: \"%s\"): " new-title))))
         (old-aliases (or vulpea--aliases-before-save
                          (vulpea-note-aliases note)))
         ;; Get incoming links
         (links (vulpea--get-incoming-links-with-descriptions note-id))
         (categorized (vulpea--categorize-links links old-title old-aliases))
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
             (when-let ((buf (get-file-buffer (plist-get link :source-path))))
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
                   (when-let ((buf (get-file-buffer path)))
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
    (setq vulpea--title-before-save nil
          vulpea--aliases-before-save nil)

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



(provide 'vulpea)
;;; vulpea.el ends here
