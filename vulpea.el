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

(let ((data (cons :key "value")))
  (setf (cdr data) 42))

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
    (or (cdr (assoc note notes))
        (make-vulpea-note
         :title note
         :level 0))))

(cl-defun vulpea-create (title template &key context properties)
  "Create a new note file with TITLE using TEMPLATE.

Returns created `vulpea-note'.

TEMPLATE is a property list of the following format

  (:body :file-name :head :unnarrowed :immediate-finish)

The only mandatory value is :file-name. This property list is
converted into proper `org-roam-capture-templates'.

Please note that generated file will contain PROPERTIES block
with an ID and any extra PROPERTIES passed as a list of

  (key_str . val_str).

Available variables in the capture context are:

- slug
- title
- id (passed via CONTEXT or generated)
- all other values from CONTEXT"
  (let* ((id (or (and context
                      (plist-get context :id))
                 (org-id-new)))
         (node (org-roam-node-create
                :id id
                :title title))
         (roam-template
          `("d" "default" plain
            ,(or (plist-get template :body)
                 "%?")
            :if-new (file+head
                     ,(plist-get template :file-name)
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
                       (plist-get template :head)))
            :unnarrowed
            ,(plist-get template :unnarrowed)
            :immediate-finish
            ,(plist-get template :immediate-finish))))
    (org-roam-capture-
     :info context
     :node node
     :props (list :immediate-finish
                  (plist-get template :immediate-finish))
     :templates (list roam-template))
    (org-roam-db-update-file (org-roam-capture--get :new-file))
    (vulpea-db-get-by-id id)))

(provide 'vulpea)
;;; vulpea.el ends here
