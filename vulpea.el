;;; vulpea.el --- A collection of org-roam note-taking functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "1.2.3") (s "1.12"))
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
;; Vulpea is a collection of functions for note taking based on `org'
;; and `org-roam'. In most cases, you should simply load `vulpea'
;; module to get all available functions.
;;
;;; Code:

(require 'org-roam)
(require 'vulpea-utils)
(require 'vulpea-meta)
(require 'vulpea-db)



(cl-defun vulpea-select (prompt
                         &key
                         require-match
                         initial-prompt
                         filter-fn)
  "Select a note.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'."
  (let* ((notes (seq-map (lambda (data)
                           (setf (cdr data)
                                 (vulpea-note-from-node (cdr data)))
                           data)
                         (org-roam-node--completions)))
         (notes (if filter-fn
                    (seq-filter (lambda (data)
                                  (funcall filter-fn (cdr data)))
                                notes)
                  notes))
         (note (completing-read
                (concat prompt ": ")
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function
                         .
                         (lambda (title)
                           (funcall
                            org-roam-node-annotation-function
                            (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (complete-with-action action notes string pred)))
                nil require-match initial-prompt)))
    (message "user typed: %s => %s" note (assoc note notes))
    (or (cdr (assoc note notes))
        (make-vulpea-note
         :title note
         :level 0))))



(cl-defun vulpea-create (title
                         file-name
                         &key
                         id
                         head
                         body
                         unnarrowed
                         immediate-finish
                         context
                         properties
                         tags)
  "Create a new note file with TITLE in FILE-NAME.

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

  BODY if present

CONTEXT is a property list of :key val.

PROPERTIES is a list of (key_str . val_str).

UNNARROWED and IMMEDIATE-FINISH are passed to `org-capture'.

Available variables in the capture context are:

- slug
- title
- id (passed via CONTEXT or generated)
- all other values from CONTEXT"
  (let* ((id (or id (org-id-new)))
         (node (org-roam-node-create
                :id id
                :title title))
         (roam-template
          `("d" "default" plain
            ,(or body "%?")
            :if-new (file+head
                     ,file-name
                     ,(concat
                       ":PROPERTIES:\n"
                       (format org-property-format ":ID:" id)
                       (when properties
                         "\n")
                       (mapconcat
                        (lambda (data)
                          (format org-property-format
                                  (concat ":" (car data) ":")
                                  (cdr data)))
                        properties "\n")
                       "\n:END:\n"
                       "#+title: ${title}\n"
                       (when tags
                         (concat
                          "#+filetags: "
                          (string-join tags " ")
                          "\n"))
                       head))
            :unnarrowed ,unnarrowed
            :immediate-finish ,immediate-finish)))
    (org-roam-capture-
     :info context
     :node node
     :props (list :immediate-finish immediate-finish)
     :templates (list roam-template))
    (org-roam-db-update-file (org-roam-capture--get :new-file))
    (vulpea-db-get-by-id id)))



(defun vulpea-buffer-title-set (title)
  "Set TITLE in current file.

If the title is already set, replace its value."
  (vulpea-buffer-prop-set "title" title))

(defun vulpea-buffer-tags-get ()
  "Return filetags value in current file."
  (vulpea-buffer-prop-get-list "filetags" " "))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current file.

If filetags value is already set, replace it."
  (vulpea-buffer-prop-set "filetags" (string-join tags " ")))

(defun vulpea-buffer-tags-add (tag)
  "Add a TAG to filetags in current file."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (append tags (list tag))))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-tags-remove (tag)
  "Remove a TAG from filetags in current file."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in current file.

If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun vulpea-buffer-prop-set-list (name values &optional separators)
  "Set a file property called NAME to VALUES in current file.

VALUES are quoted and combined into single string using
`combine-and-quote-strings'.

If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.

If the property is already set, replace its value."
  (vulpea-buffer-prop-set name (combine-and-quote-strings values separators)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.

If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (vulpea-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))



(provide 'vulpea)
;;; vulpea.el ends here
