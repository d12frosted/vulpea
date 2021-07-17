;;; vulpea-db.el --- Data Base querying -*- lexical-binding: t; -*-
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
;; Created: 29 Dec 2020
;;
;; URL: https://github.com/d12frosted/vulpea
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
(require 'org-roam-db)
(require 'vulpea-utils)

;;
;; Searching

(defun vulpea-db-search-by-title (title)
  "Return a list of `vulpea-note' that has TITLE.

Does not support headings in the note."
  (let* ((matches
          ;; would be nice if org-roam provides a helper for this
          (seq-uniq
           (append
            (org-roam-db-query [:select [id] :from nodes
                                :where (= title $s1)]
                               title)
            (org-roam-db-query [:select [node-id] :from aliases
                                :where (= alias $s1)]
                               title))))
         ;; `org-roam-populate' is expensive
         (nodes (seq-map
                 (lambda (data)
                   (org-roam-populate
                    (org-roam-node-create :id (car data))))
                 matches)))
    (seq-map
     (lambda (note)
       (unless (string-equal (vulpea-note-title note)
                             title)
         (setf (vulpea-note-primary-title note)
               (vulpea-note-title note))
         (setf (vulpea-note-title note) title))
       note)
     (seq-map #'vulpea-note-from-node nodes))))

;;
;; Querying

(defun vulpea-db-query (&optional filter-fn)
  "Query list of `vulpea-note' from database.

When FILTER-FN is non-nil, only notes that satisfy it are
returned."
  (let* ((rows
          (org-roam-db-query
           "select
  id,
  file,
  title,
  \"level\",
  '(' || group_concat(tags,
  ' ') || ')' as tags,
  aliases
from
  (
  select
    nodes.id as id,
    nodes.title as title,
    nodes.file as file,
    nodes.\"level\" as \"level\",
    tags.tag as tags,
    '(' || group_concat(aliases.alias, ' ') || ')' as aliases
  from nodes
  left join tags on tags.node_id = nodes.id
  left join aliases on aliases.node_id = nodes.id
  group by nodes.id, tags.tag )
group by id, aliases;"))
         notes)
    (dolist (row rows notes)
      (let ((id (nth 0 row))
            (file (nth 1 row))
            (title (nth 2 row))
            (level (nth 3 row))
            (tags (nth 4 row))
            (aliases (nth 5 row)))
        (dolist (name (cons title aliases))
          (let ((note (make-vulpea-note
                       :path file
                       :title name
                       :primary-title
                       (unless (string-equal title name)
                         title)
                       :tags tags
                       :aliases aliases
                       :id id
                       :level level)))
            (when (or (null filter-fn)
                      (funcall filter-fn note))
              (push note notes))))))))

;;
;; Exchanging ID to X

(defun vulpea-db-get-by-id (id)
  "Find a `vulpea-note' by ID.

Supports headings in the note."
  (vulpea-note-from-node
   (org-roam-populate (org-roam-node-create :id id))))

(defun vulpea-db-get-file-by-id (id)
  "Get file of `vulpea-note' with ID.

Supports headings in the note."
  (caar
   (org-roam-db-query
    [:select file
     :from nodes
     :where (= id $s1)]
    id)))

;;
;; Exchange FILE to X

(defun vulpea-db-get-id-by-file (file)
  "Get ID of `vulpea-note' represented by FILE.

If the FILE is relative, it is considered to be relative to
`org-roam-directory'."
  (caar
   (org-roam-db-query
    [:select id
     :from nodes
     :where (and (= file $s1)
                 (= level $s2))]
    (if (file-name-absolute-p file)
        file
      (expand-file-name file org-roam-directory))
    0)))

;;
;; Update

(defun vulpea-db-update (note-or-id)
  "Update db for NOTE-OR-ID."
  (let ((file (if (stringp note-or-id)
                  (vulpea-db-get-file-by-id note-or-id)
                (vulpea-note-path note-or-id))))
    (org-roam-db-update-file file)))

(provide 'vulpea-db)
;;; vulpea-db.el ends here
