;;; vulpea.el --- A collection of org-roam note-taking functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
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
  (unless org-roam-mode (org-roam-mode))
  (let* ((notes (seq-sort-by
                 (lambda (n)
                   (vulpea-note-meta-mtime
                    (vulpea-note-meta n)))
                 #'time-less-p
                 (vulpea-db-query filter-fn)))
         (completions (seq-map
                       (lambda (n)
                         (cons
                          (org-roam--add-tag-string
                           (vulpea-note-title n)
                           (vulpea-note-tags n))
                          n))
                       notes))
         (title-with-tags (org-roam-completion--completing-read
                           (concat prompt ": ")
                           completions
                           :require-match require-match
                           :initial-input initial-prompt)))
    (or (cdr (assoc title-with-tags completions))
        (make-vulpea-note
         :title title-with-tags
         :level 0))))

(defvar vulpea--capture-file-path nil
  "Path to file created during `vulpea-create'.")

(defun vulpea--capture-new-file (orig-func &optional
                                           allow-existing-file-p)
  "Advice around `org-roam-capture--new-file'.

The only purpose of this advice is to set the value of
`vulpea--capture-file-path' to file path returned by
`org-roam-capture--new-file', so `vulpea-create' may update
`org-roam-db' efficiently.

Calls ORIG-FUNC with ALLOW-EXISTING-FILE-P."
  (let ((file-path (funcall orig-func allow-existing-file-p)))
    (setq vulpea--capture-file-path file-path)
    file-path))

(defun vulpea-create (title template &optional context)
  "Create a new note file with TITLE using TEMPLATE.

Returns created `vulpea-note'.

TEMPLATE is a property list of the following format

  (:body :file-name :head :unnarrowed :immediate-finish)

The only mandatory value is :file-name. This property list is
converted into proper `org-roam-capture-templates'.

Please note that generated file will contain PROPERTIES block
with an ID.

Available variables in the capture context are:

- slug
- title
- id (passed via CONTEXT or generated)
- all other values from CONTEXT"
  (let* ((id (or (and context
                      (alist-get 'id context))
                 (org-id-new)))
         (org-roam-capture--info
          (append
           (list
            (cons 'title title)
            (cons 'slug (funcall org-roam-title-to-slug-function
                                 title)))
           context
           (list
            (cons 'id id))))
         (org-roam-capture--context 'title)
         (roam-template
          `("d" "default" plain
            #'org-roam-capture--get-point
            ,(or (plist-get template :body)
                 "%?")
            :file-name
            ,(plist-get template :file-name)
            :head
            ,(concat
              ":PROPERTIES:\n"
              (format org-property-format ":ID:" id)
              "\n:END:\n"
              (plist-get template :head))
            :unnarrowed
            ,(plist-get template :unnarrowed)
            :immediate-finish
            ,(plist-get template :immediate-finish)))
         (org-roam-capture-templates (list roam-template)))
    (org-roam-capture--capture)
    (unless vulpea--capture-file-path
      (error "Could not capture created file, use `vulpea-setup'"))
    (org-roam-db-update-file vulpea--capture-file-path)
    (setq vulpea--capture-file-path nil)
    (vulpea-db-get-by-id id)))

;;;###autoload
(defun vulpea-setup ()
  "Setup `vulpea' library."
  (advice-add 'org-roam-capture--new-file
              :around
              #'vulpea--capture-new-file))

(provide 'vulpea)
;;; vulpea.el ends here
