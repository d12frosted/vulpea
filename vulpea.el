;;; vulpea.el --- A collection of note-taking functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.0.0
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
;; Vulpea is a collection of functions for note taking based on `org'.
;; In most cases, you should simply load `vulpea' module to get all
;; available functions.
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
                            require-match)
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
start the capture process."
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
                :initial-prompt region-text)))
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
(defun vulpea-insert (&optional filter-fn create-fn)
  "Select a note and insert a link to it.

Allows capturing new notes. After link is inserted,
`vulpea-insert-handle-functions' are called with the inserted
note as the only argument regardless involvement of capture
process.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'. Unless specified,
`vulpea-insert-default-filter' is used.

CREATE-FN allows to control how a new note is created when user picks a
non-existent note. This function is called with two arguments - title
and capture properties. When CREATE-FN is nil, default implementation is
used."
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
               (notes (funcall vulpea-insert-default-candidates-source
                               (or filter-fn vulpea-insert-default-filter)))
               (note (vulpea-select-from "Note" notes
                                         :initial-prompt region-text))
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



(provide 'vulpea)
;;; vulpea.el ends here
