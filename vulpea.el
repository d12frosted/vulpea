;;; vulpea.el --- Vulpea is a fox -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.0.1
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
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vulpea is a fox.
;;
;;; Code:

(require 'org-roam)
(require 'vulpea-utils)
(require 'vulpea-meta)
(require 'vulpea-db)

(defun vulpea-select (prompt &optional
                             initial-prompt
                             completions
                             filter-fn)
  "Select a note.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

COMPLETIONS is a list of completions to be used instead of
`vulpea--get-title-path-completions`.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'."
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (or completions
                          (vulpea--get-title-path-completions)))
         (completions (if filter-fn
                          (seq-filter (lambda (kvp)
                                        (funcall filter-fn (cdr kvp)))
                                      completions)
                        completions))
         (title-with-tags (org-roam-completion--completing-read
                           (concat prompt ": ")
                           completions
                           :initial-input initial-prompt))
         (res (cdr (assoc title-with-tags completions))))
    (if res
        (progn
          (setf (vulpea-note-id res)
                (vulpea-db-get-id-by-file (vulpea-note-path res)))
          res)
      (make-vulpea-note
       :title title-with-tags
       :level 0))))

(defun vulpea--get-title-path-completions ()
  "Return an alist for completion.

The car is the displayed title for completion, and the cdr
contains all the funny stuff."
  (let*
      ((rows (org-roam-db-query
              [:select [files:file titles:title tags:tags files:meta]
               :from titles
               :left :join tags
               :on (= titles:file tags:file)
               :left :join files
               :on (= titles:file files:file)]))
       completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--prepend-tag-string title tags))
              (v (make-vulpea-note
                  :path file-path
                  :title title
                  :tags tags
                  :level 0)))
          (push (cons k v) completions))))))

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
  (let ((file-path (apply orig-func allow-existing-file-p)))
    (setq vulpea--capture-file-path file-path)
    file-path))

(advice-add 'org-roam-capture--new-file
            :around
            #'vulpea--capture-new-file)

(defun vulpea-create (title template &optional id)
  "Create a new note file with TITLE using TEMPLATE.

Returns ID of created note.

See `org-roam-capture-templates' for description of TEMPLATE.

Available variables in the capture context are:

- slug
- title
- ID (passed or generated)"
  (let* ((id (or id (org-id-new)))
         (org-roam-capture--info
          (list
           (cons 'title title)
           (cons 'slug (funcall org-roam-title-to-slug-function
                                title))
           (cons 'id id)))
         (org-roam-capture--context 'title)
         (org-roam-capture-templates (list template)))
    (org-roam-capture--capture)
    (when vulpea--capture-file-path
      (org-roam-db-update-file vulpea--capture-file-path)
      (setq vulpea--capture-file-path nil))
    (vulpea-db-get-by-id id)))

(provide 'vulpea)
;;; vulpea.el ends here
