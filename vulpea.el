;;; vulpea.el --- A collection of note-taking functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.3.0
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

(require 'vulpea-utils)
(require 'vulpea-buffer)
(require 'vulpea-meta)
(require 'vulpea-select)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'org-capture)
(require 's)
;;; Customization

(defgroup vulpea nil
  "Vulpea note-taking system."
  :group 'org)

(defcustom vulpea-directory nil
  "Default directory for vulpea notes.
When nil, notes are created in `org-directory'."
  :type '(choice (const :tag "Use org-directory" nil)
                 (directory :tag "Custom directory"))
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




(defvar vulpea-find-default-filter nil
  "Default filter to use in `vulpea-find'.")

(defvar vulpea-find-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-find'.

Must be a function that accepts one argument - optional note
filter function.")

;;; Helper Functions

(defun vulpea--title-to-slug (title)
  "Convert TITLE to URL-friendly slug."
  (s-replace " " "-" 
             (s-downcase
              (s-replace-regexp "[^a-zA-Z0-9 -]" "" title))))

(defun vulpea--expand-file-name-template (title &optional id)
  "Expand `vulpea-file-name-template' with TITLE and optional ID.
Returns absolute file path."
  (let* ((slug (vulpea--title-to-slug title))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (id (or id (org-id-new)))
         (template (if (functionp vulpea-file-name-template)
                       (funcall vulpea-file-name-template title)
                     vulpea-file-name-template))
         (file-name (thread-last template
                      (s-replace "${title}" title)
                      (s-replace "${slug}" slug)
                      (s-replace "${timestamp}" timestamp)
                      (s-replace "${id}" id)))
         (dir (or vulpea-directory org-directory)))
    (expand-file-name file-name dir)))

(defun vulpea--format-note-content (id title &optional head meta tags properties)
  "Format note content for org-capture template.
ID and TITLE are required. Optional: HEAD, META (alist), TAGS (list), PROPERTIES (alist)."
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
      (org-show-context))))



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

FILE-NAME is optional. When nil, uses `vulpea-file-name-template' to generate.

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

CONTEXT is a property list of :key val (currently unused, for future compatibility).

PROPERTIES is a list of (key_str . val_str).

UNNARROWED and IMMEDIATE-FINISH are passed to `org-capture'.
IMMEDIATE-FINISH defaults to t for programmatic use.

META is an alist of (key . value) or (key . (list of values)).

CAPTURE-PROPERTIES are additional properties for org-capture (currently unused).

See Info node `(org) Template elements' for BODY template syntax."
  (let* ((id (or id (org-id-new)))
         (file-path (or file-name (vulpea--expand-file-name-template title id)))
         (content (vulpea--format-note-content id title head meta tags properties))
         (template `("v" "vulpea-note" plain
                     ,(or body "")
                     :target (file ,file-path)
                     :template ,content
                     :immediate-finish ,(if (null immediate-finish) t immediate-finish)
                     :unnarrowed ,unnarrowed
                     :empty-lines 1))
         (org-capture-templates (list template)))
    ;; Run org-capture
    (org-capture nil "v")

    ;; Update database with the new file
    (vulpea-db-update-file file-path)

    ;; Return the note
    (vulpea-db-get-by-id id)))



(provide 'vulpea)
;;; vulpea.el ends here
