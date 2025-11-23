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

(defcustom vulpea-default-notes-directory org-directory
  "Default directory for creating new notes.

Defaults to `org-directory'. Override this if you want notes to be
created in a different location than the first sync directory.

This allows you to control where `vulpea-create' places new notes
without specifying an explicit file path."
  :type 'directory
  :group 'vulpea)

(defcustom vulpea-file-name-template "${timestamp}_${slug}.org"
  "Template for generating file names for new notes.
Available variables:
  ${title}     - Note title
  ${slug}      - URL-friendly version of title
  ${timestamp} - Current timestamp (format: %Y%m%d%H%M%S)
  ${id}        - Note ID (UUID)

Can be a string template or a function that accepts title and returns file name."
  :type '(choice (string :tag "Template string")
          (function :tag "Function"))
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

(defun vulpea--expand-template (template title &optional id)
  "Expand TEMPLATE with TITLE and optional ID.
TEMPLATE is a string with ${...} placeholders.
Returns expanded string with placeholders replaced."
  (let* ((slug (vulpea-title-to-slug title))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (id (or id (org-id-new))))
    (thread-last template
                 (s-replace "${title}" title)
                 (s-replace "${slug}" slug)
                 (s-replace "${timestamp}" timestamp)
                 (s-replace "${id}" id))))

(defun vulpea--expand-file-name-template (title &optional id template)
  "Expand file name template with TITLE and optional ID and TEMPLATE.
If TEMPLATE is nil, uses `vulpea-file-name-template'.
Returns absolute file path."
  (let* ((template (or template
                       (if (functionp vulpea-file-name-template)
                           (funcall vulpea-file-name-template title)
                         vulpea-file-name-template)))
         (file-name (vulpea--expand-template template title id))
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
        (let ((new-note (vulpea-create (vulpea-note-title note)
                                       nil  ; Let template generate filename
                                       :immediate-finish nil)))  ; Allow editing
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
            (let* ((insert-buffer (current-buffer))
                   (insert-point (point-marker)))
              (if create-fn
                  (funcall create-fn (vulpea-note-title note) nil)
                ;; Create the note
                (let ((new-note (vulpea-create (vulpea-note-title note)
                                               nil
                                               :immediate-finish nil)))
                  ;; Return to original buffer and insert link
                  (when new-note
                    (with-current-buffer insert-buffer
                      (goto-char insert-point)
                      (when region-text
                        (delete-region beg end)
                        (set-marker beg nil)
                        (set-marker end nil))
                      (insert (org-link-make-string
                               (concat "id:" (vulpea-note-id new-note))
                               description))
                      (run-hook-with-args
                       'vulpea-insert-handle-functions
                       new-note)))))))))
    (deactivate-mark)))



(cl-defun vulpea-create (title
                         &optional file-name
                         &key
                         id
                         head
                         meta
                         body
                         unnarrowed
                         immediate-finish
                         _context  ; Reserved for future use
                         properties
                         tags
                         _capture-properties)  ; Reserved for future use
  "Create a new note file with TITLE.

FILE-NAME is optional. When nil, uses `vulpea-file-name-template' to
generate.

Returns created `vulpea-note'.

ID is automatically generated unless explicitly passed.

Structure of the generated file is:

  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  #+title: TITLE
  #+filetags: TAGS if present
  HEAD if present

  META if present

  BODY if present

CONTEXT is a property list of :key val (currently unused, for future
compatibility).

PROPERTIES is a list of (key_str . val_str).

UNNARROWED and IMMEDIATE-FINISH are passed to `org-capture'.
IMMEDIATE-FINISH defaults to nil (allows editing). Pass t for
programmatic note creation without user interaction.

META is an alist of (key . value) or (key . (list of values)).

CAPTURE-PROPERTIES are additional properties for
`org-capture' (currently unused).

See Info node `(org) Template elements' for BODY template syntax."
  (let* ((id (or id (org-id-new)))
         (file-path (vulpea--expand-file-name-template title id file-name))
         (content (vulpea--format-note-content id title head meta tags properties))
         (full-template (if body
                            (concat content "\n\n" body)
                          content))
         (template `("v" "vulpea-note" plain
                     (file ,file-path)
                     ,full-template
                     :immediate-finish ,immediate-finish
                     :unnarrowed ,unnarrowed))
         (org-capture-templates (list template)))

    ;; Run org-capture
    (org-capture nil "v")

    ;; When immediate-finish is t, update DB and return note
    ;; When immediate-finish is nil, capture is still in progress, return nil
    (when immediate-finish
      ;; Verify file was created
      (unless (file-exists-p file-path)
        (error "vulpea-create: File %s was not created by org-capture" file-path))

      ;; Fix: org-capture with 'plain' type adds leading newline
      ;; This breaks property drawer recognition, so we remove it
      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        ;; Remove all leading blank lines (critical for property drawer recognition)
        (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
          (delete-line))
        (write-region (point-min) (point-max) file-path nil 'silent))

      ;; Check for duplicate property drawers (diagnostic)
      (let ((prop-count 0))
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (while (re-search-forward "^:PROPERTIES:" nil t)
            (setq prop-count (1+ prop-count)))
          (when (> prop-count 1)
            (warn "vulpea-create: Found %d property drawers in %s - check your org-capture hooks!"
                  prop-count file-path))))

      ;; Update database with the new file
      (let ((update-count (vulpea-db-update-file file-path)))
        (when (zerop update-count)
          (error "vulpea-create: No notes extracted from file %s (expected ID %s)" file-path id)))

      ;; Return the note, with diagnostic if ID mismatch
      (let ((note (vulpea-db-get-by-id id)))
        (unless note
          ;; Debug: show what IDs are actually in the file
          (let ((actual-ids (emacsql (vulpea-db)
                                     [:select id :from notes :where (= path $s1)]
                                     file-path)))
            (error "vulpea-create: Expected ID %s but found %S in database. Check for hooks adding IDs!"
                   id actual-ids)))
        note))))



(provide 'vulpea)
;;; vulpea.el ends here
