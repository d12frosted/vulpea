;;; vulpea.el --- A collection of org-roam note-taking functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.2") (org "9.4.4") (org-roam "2.0.0") (s "1.12") (dash "2.19"))
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
(require 'vulpea-select)
(require 'vulpea-db)



(defvar vulpea-find-default-filter nil
  "Default filter to use in `vulpea-find'.")

(defvar vulpea-find-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-find'.

Must be a function that accepts one argument - optional note
filter function.")

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
        (org-roam-node-visit
         (org-roam-node-from-id (vulpea-note-id note))
         (or current-prefix-arg
             other-window))
      (when (not require-match)
        (org-roam-capture-
         :node (org-roam-node-create :title (vulpea-note-title note))
         :props '(:finalize find-file))))))

;;;###autoload
(defun vulpea-find-backlink ()
  "Select and find a note linked to current note."
  (interactive)
  (let* ((node (org-roam-node-at-point 'assert))
         (backlinks (vulpea-db-query-by-links-some
                     (list (cons "id"
                                 (org-roam-node-id node))))))
    (unless backlinks
      (user-error "There are no backlinks to the current note"))
    (vulpea-find
     :candidates-fn (lambda (_) backlinks)
     :require-match t)))



(defun vulpea-visit (note-or-id &optional other-window)
  "Visit NOTE-OR-ID.

If OTHER-WINDOW, visit the NOTE in another window."
  (let ((id (if (vulpea-note-p note-or-id)
                (vulpea-note-id note-or-id)
              note-or-id)))
    (org-roam-node-visit
     (org-roam-node-from-id id)
     (or current-prefix-arg
         other-window))))



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
            (let ((props (append
                          (when (and beg end)
                            (list :region (cons beg end)))
                          (list
                           :insert-at (point-marker)
                           :link-description description
                           :finalize #'vulpea-insert--capture-finalize))))
              (if create-fn
                  (funcall create-fn (vulpea-note-title note) props)
                (org-roam-capture-
                 :node (org-roam-node-create
                        :title (vulpea-note-title note))
                 :props props))))))
    (deactivate-mark)))

(defun vulpea-insert--capture-finalize ()
  "Finalize capture process initiated by `vulpea-insert'."
  (org-roam-capture--finalize-insert-link)
  (when-let* ((id (org-roam-capture--get :id))
              (note (vulpea-db-get-by-id id)))
    (run-hook-with-args 'vulpea-insert-handle-functions note)))



(cl-defun vulpea-create (title
                         file-name
                         &key
                         id
                         head
                         meta
                         body
                         unnarrowed
                         immediate-finish
                         context
                         properties
                         tags
                         capture-properties)
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

  META if present

  BODY if present

CONTEXT is a property list of :key val.

PROPERTIES is a list of (key_str . val_str).

UNNARROWED and IMMEDIATE-FINISH are passed to `org-capture'.

Available variables in the capture context are:

- slug
- title
- id (passed via CONTEXT or generated)
- all other values from CONTEXT

CAPTURE-PROPERTIES are used by capture mechanism. See 'Template
elements' of Org Capture feature for more information."
  (let* ((id (or id (org-id-new)))
         (node (org-roam-node-create
                :id id
                :title title))
         (content (string-join
                   (append
                    (list
                     ":PROPERTIES:"
                     (format org-property-format ":ID:" id))
                    (seq-map
                     (lambda (data)
                       (format org-property-format
                               (concat ":" (car data) ":")
                               (cdr data)))
                     properties)
                    (list
                     ":END:"
                     "#+title: ${title}")
                    (when tags
                      (list (concat
                             "#+filetags: :"
                             (string-join tags ":")
                             ":")))
                    (when head (list head))
                    (when meta
                      ;; extra newline
                      (list ""))
                    (when meta
                      (seq-map
                       (lambda (kvp)
                         (if (listp (cdr kvp))
                             (mapconcat
                              (lambda (val)
                                (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format val)))
                              (cdr kvp) "\n")
                           (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format (cdr kvp)))))
                       meta)))
                   "\n"))
         (roam-template
          `("d" "default" plain
            ,(or body "%?")
            :if-new (file+head
                     ,file-name
                     ,content)
            :unnarrowed ,unnarrowed
            :immediate-finish ,immediate-finish
            :empty-lines-before 1)))
    (org-roam-capture-
     :info context
     :node node
     :props (append (list :immediate-finish immediate-finish)
                    capture-properties)
     :templates (list roam-template))
    (vulpea-db-get-by-id id)))



(provide 'vulpea)
;;; vulpea.el ends here
