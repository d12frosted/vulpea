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
     (seq-map #'vulpea-db--from-node nodes))))

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
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  properties,
  meta
from
  (
  select
    id,
    file,
    title,
    \"level\",
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    properties,
    meta
  from
    (
    select
      nodes.id as id,
      nodes.file as file,
      nodes.title as title,
      nodes.\"level\" as \"level\",
      tags.tag as tags,
      aliases.alias as aliases,
      nodes.properties as properties,
      '(' || group_concat('(' || meta.prop
                              || ' '
                              || meta.value
                              || ')',
                          ' ')
          || ')' as meta
    from nodes
    left join tags on tags.node_id = nodes.id
    left join aliases on aliases.node_id = nodes.id
    left join meta on meta.node_id = nodes.id
    group by nodes.id, tags.tag, aliases.alias )
  group by id, tags )
group by id"))
         notes)
    (dolist (row rows notes)
      (let ((id (nth 0 row))
            (file (nth 1 row))
            (title (nth 2 row))
            (level (nth 3 row))
            (tags (nth 4 row))
            (aliases (nth 5 row))
            (properties (nth 6 row))
            (meta (nth 7 row)))
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
                       :level level
                       :properties properties
                       :meta (seq-map
                              (lambda (row)
                                (cons (nth 0 row) (nth 1 row)))
                              meta))))
            (when (or (null filter-fn)
                      (funcall filter-fn note))
              (push note notes))))))))

;;
;; Exchanging ID to X

(defun vulpea-db-get-by-id (id)
  "Find a `vulpea-note' by ID.

Supports headings in the note."
  (vulpea-db--from-node
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



;;
;; Populate

(defun vulpea-db--from-node (node)
  "Convert Org-roam NODE to note."
  (when-let ((title (org-roam-node-title node))
             (id (org-roam-node-id node)))
    (let ((meta (seq-map
                 (lambda (row)
                   (cons (nth 0 row) (nth 1 row)))
                 (org-roam-db-query
                  [:select [prop value]
                   :from meta
                   :where (= node-id $s1)]
                  id))))
      (make-vulpea-note
       :id id
       :path (org-roam-node-file node)
       :level (or (org-roam-node-level node) 0)
       :title title
       :aliases (org-roam-node-aliases node)
       :tags (org-roam-node-tags node)
       :properties (org-roam-node-properties node)
       :meta meta))))



(defconst vulpea-db--schemata
  '((meta
     ([(node-id :not-null)
       (prop :not-null)
       (value :not-null)]
      (:foreign-key
       [node-id]
       :references
       nodes [id]
       :on-delete
       :cascade))))
  "Vulpea db schemata.")

(defconst vulpea-db--indices
  '((meta-node-id meta [node-id]))
  "Vulpea db indices.")

(defvar vulpea-db--initalized nil
  "Non-nil when database was initialized.")

(defun vulpea-db--init (get-db)
  "Initialize database by creating missing tables if needed.

GET-DB is a function that returns connection to database."
  (when-let ((db (funcall get-db)))
    (unless vulpea-db--initalized
      (emacsql-with-transaction db
        (pcase-dolist (`(,table ,schema) vulpea-db--schemata)
          (unless (emacsql db
                           [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name $r1))]
                           (emacsql-escape-identifier table))
            (emacsql db [:create-table $i1 $S2] table schema)))
        (pcase-dolist (`(,index-name ,table ,columns)
                       vulpea-db--indices)
          (unless (emacsql db
                           [:select name
                            :from sqlite_master
                            :where (and (= type 'index)
                                        (= name $r1))]
                           (emacsql-escape-identifier index-name))
            (emacsql db [:create-index $i1 :on $i2 $S3]
                     index-name table columns))))
      (setq vulpea-db--initalized t))
    db))



;;;###autoload
(define-minor-mode vulpea-db-autosync-mode
  "Global minor mode to automatically synchronise vulpea db."
  :global t
  :group 'vulpea
  :init-value nil
  (let ((enabled vulpea-db-autosync-mode))
    (cond
     (enabled
      (setq vulpea-db--initalized nil)
      ;; attach custom schemata
      (seq-each
       (lambda (schema)
         (add-to-list 'org-roam-db--table-schemata schema 'append))
       vulpea-db--schemata)

      ;; attach custom indices
      (seq-each
       (lambda (index)
         (add-to-list 'org-roam-db--table-indices index 'append))
       vulpea-db--indices)

      ;; make sure that meta is inserted into table
      (advice-add 'org-roam-db-insert-file-node
                  :after
                  #'vulpea-db-meta-insert)

      ;; make sure that meta table exists
      (advice-add 'org-roam-db
                  :around
                  #'vulpea-db--init))
     (t
      (setq vulpea-db--initalized nil)
      (advice-remove 'org-roam-db-insert-file-node
                     #'vulpea-db-meta-insert)
      (advice-remove 'org-roam-db #'vulpea-db--init)
      (seq-each
       (lambda (schema)
         (setq org-roam-db--table-schemata
               (delete schema org-roam-db--table-schemata)))
       vulpea-db--schemata)
      (seq-each
       (lambda (index)
         (setq org-roam-db--table-indices
               (delete index org-roam-db--table-indices)))
       vulpea-db--indices)))))

;;;###autoload
(defun vulpea-db-autosync-enable ()
  "Activate function `vulpea-db-autosync-mode'."
  (vulpea-db-autosync-mode +1))

(defun vulpea-db-autosync-disable ()
  "Deactivate function `vulpea-db-autosync-mode'."
  (vulpea-db-autosync-mode -1))

(defun vulpea-db-autosync-toggle ()
  "Toggle status of function `vulpea-db-autosync-mode'."
  (vulpea-db-autosync-mode 'toggle))

(define-obsolete-function-alias
  'vulpea-db-setup
  'vulpea-db-autosync-enable "vulpea 0.2.0")



;; this is a great indicator of a poor module design
(autoload 'vulpea-buffer-meta "vulpea-buffer")
(autoload 'vulpea-buffer-meta-get-list! "vulpea-buffer")

(defun vulpea-db-meta-insert ()
  "Update meta in Org-roam cache for FILE-PATH."
  (org-with-point-at 1
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let* ((id (org-id-get))
                  (meta (vulpea-buffer-meta))
                  (pl (plist-get meta :pl))
                  (items-all (org-element-map pl 'item #'identity))
                  (props-all (seq-uniq
                              (seq-map
                               (lambda (item)
                                 (org-element-interpret-data
                                  (org-element-contents
                                   (org-element-property :tag item))))
                               items-all)))
                  (kvps (seq-map
                         (lambda (prop)
                           (cons
                            (substring-no-properties prop)
                            (seq-map
                             (lambda (x)
                               (substring-no-properties
                                (s-trim-right
                                 (org-element-interpret-data x))))
                             (vulpea-buffer-meta-get-list!
                              meta prop 'raw))))
                         props-all)))
        (org-roam-db-query
         [:insert :into meta
          :values $v1]
         (seq-map
          (lambda (kvp)
            (vector id (car kvp) (cdr kvp)))
          kvps))))))



(provide 'vulpea-db)
;;; vulpea-db.el ends here
