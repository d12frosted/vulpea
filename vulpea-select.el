;;; vulpea-select.el --- Metadata manipulation -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "2.0.0") (s "1.12"))
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
;; Created: 11 Jul 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functionality to select a note.
;;
;;; Code:

(require 'org-roam)
(require 'vulpea-utils)
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
          (if (vulpea-note-primary-title note)
              (concat "("
                      (vulpea-note-primary-title note)
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

(provide 'vulpea-select)
;;; vulpea-select.el ends here
