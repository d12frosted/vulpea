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
(require 'vulpea-buffer)
(require 'vulpea-meta)
(require 'vulpea-db)



(defvar vulpea-select-describe-fn #'vulpea-note-title
  "Function to describe a note for completion.

Accepts a `vulpea-note'. Returns a `string'.")

(defvar vulpea-select-annotate-fn #'vulpea-select-annotate
  "Function to annotate a note for completion.

Accepts a `vulpea-note'. Returns a `string'.")

(defun vulpea-select-describe (note)
  "Describe a NOTE for completion."
  (propertize
   (concat
    (funcall vulpea-select-describe-fn note)
    (propertize
     (funcall vulpea-select-annotate-fn note)
     'face 'completions-annotations))
   'vulpea-note-id
   (vulpea-note-id note)))

(defun vulpea-select-annotate (note)
  "Annotate a NOTE for completion."
  (let* ((alias-str
          (if (vulpea-note-aliases note)
              (concat "("
                      (string-join
                       (vulpea-note-aliases note)
                       ", ")
                      ")")
            ""))
         (tags-str (mapconcat
                    (lambda (x) (concat "#" x))
                    (vulpea-note-tags note)
                    " "))
         (sections (seq-remove #'string-empty-p
                               (list alias-str
                                     tags-str))))
    (if (null sections)
        ""
      (concat " " (string-join sections " ")))))

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
  (let* ((notes (vulpea-db-query filter-fn))
         (completions (seq-map
                       (lambda (n)
                         (cons (vulpea-select-describe n)
                               n))
                       notes))
         (notes-table (make-hash-table :test #'equal)))
    (seq-each (lambda (note)
                (puthash (vulpea-note-id note) note notes-table))
              notes)
    (let ((note (completing-read
                 (concat prompt ": ")
                 completions
                 nil require-match initial-prompt)))
      (or (cdr (assoc note completions))
          (make-vulpea-note
           :title (substring-no-properties note)
           :level 0)))))



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
            :immediate-finish ,immediate-finish
            :empty-lines-before 1)))
    (org-roam-capture-
     :info context
     :node node
     :props (list :immediate-finish immediate-finish)
     :templates (list roam-template))
    (vulpea-db-get-by-id id)))



(provide 'vulpea)
;;; vulpea.el ends here
