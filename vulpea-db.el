;;; vulpea-db.el --- Data Base querying -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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

(defun vulpea-db--notes-from-row (row)
  "Parse list of `vulpea-note' from ROW.

The secret ingredient is list of aliases!"
  (let ((id (nth 0 row))
        (file (nth 1 row))
        (level (nth 2 row))
        (title (nth 3 row))
        (properties (nth 4 row))
        (aliases (nth 5 row))
        (tags (nth 6 row))
        (meta (nth 7 row))
        (links (nth 8 row)))
    (seq-map
     (lambda (name)
       (make-vulpea-note
        :path file
        :title name
        :primary-title
        (unless (string-equal title name)
          title)
        :tags (seq-map #'substring-no-properties tags)
        :aliases aliases
        :id id
        :level level
        :links (seq-uniq (seq-map
                          #'vulpea-db--parse-link-pair
                          links))
        :properties properties
        :meta (seq-map
               (lambda (row)
                 (cons (nth 0 row) (nth 1 row)))
               meta)))
     (cons title aliases))))

(defun vulpea-db-query (&optional filter-fn)
  "Query list of `vulpea-note' from database.

When FILTER-FN is non-nil, only notes that satisfy it are
returned."
  (let* ((rows
          (org-roam-db-query
           "select
  id,
  path,
  \"level\",
  title,
  properties,
  aliases,
  tags,
  meta,
  links
from notes")))
    (seq-filter
     (or filter-fn #'identity)
     (seq-mapcat
      #'vulpea-db--notes-from-row
      rows))))

(defun vulpea-db-query-by-ids (ids)
  "Query list of `vulpea-note' by IDS."
  (let* ((rows
          (org-roam-db-query
           (format
            "select
  id,
  path,
  \"level\",
  title,
  properties,
  aliases,
  tags,
  meta,
  links
from notes
where notes.id in %s"
            (emacsql-escape-vector (apply #'vector ids))))))
    (seq-mapcat
     #'vulpea-db--notes-from-row
     rows)))

(defun vulpea-db-query-by-tags-some (tags)
  "Query a list of `vulpea-note' from database.

Only notes that are tagged by at least one tag from the list of
TAGS are returned."
  (emacsql-with-transaction (org-roam-db)
    (vulpea-db-query-by-ids
     (seq-map
      #'car
      (org-roam-db-query
       (format "select distinct node_id from tags where tag in %s"
               (emacsql-escape-vector (apply #'vector tags))))))))

(defun vulpea-db-query-by-tags-every (tags)
  "Query a list of `vulpea-note' from database.

Only notes that are tagged by each and every tag from the list of
TAGS are returned."
  (emacsql-with-transaction (org-roam-db)
    (vulpea-db-query-by-ids
     (seq-map
      #'car
      (org-roam-db-query
       (string-join
        (seq-map
         (lambda (tag)
           (format "select node_id from tags where tag = '\"%s\"'"
                   tag))
         tags)
        "\nintersect\n"))))))

(defun vulpea-db-query-by-links-some (destinations)
  "Query a list of `vulpea-note' from database.

Only notes that link to at least one destination from the list of
DESTINATIONS are returned."
  (emacsql-with-transaction (org-roam-db)
    (vulpea-db-query-by-ids
     (seq-map
      #'car
      (org-roam-db-query
       (format "select distinct source from links where dest in %s"
               (emacsql-escape-vector
                (apply #'vector (seq-map #'cdr destinations)))))))))

(defun vulpea-db-query-by-links-every (destinations)
  "Query a list of `vulpea-note' from database.

Only notes that link to each and every destination from the list of
DESTINATIONS are returned."
  (emacsql-with-transaction (org-roam-db)
    (vulpea-db-query-by-ids
     (seq-map
      #'car
      (org-roam-db-query
       (string-join
        (seq-map
         (lambda (link)
           (format "select source from links
 where type = '\"%s\"'
   and dest = '\"%s\"'"
                   (car link)
                   (cdr link)))
         destinations)
        "\nintersect\n"))))))

;;
;; Exchanging ID to X

(defun vulpea-db-get-by-id (id)
  "Find a `vulpea-note' by ID.

Supports headings in the note."
  (when-let ((row
              (org-roam-db-query
               (format
                "select
  id,
  path,
  \"level\",
  title,
  properties,
  aliases,
  tags,
  meta,
  links
from notes
where notes.id = '\"%s\"'"
                id))))
    (car (vulpea-db--notes-from-row (car row)))))

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
                  id)))
          (links (seq-map
                  #'vulpea-db--parse-link-pair
                  (org-roam-db-query
                   [:select [type dest]
                    :from links
                    :where (and (= source $s1))]
                   id))))
      (make-vulpea-note
       :id id
       :path (org-roam-node-file node)
       :level (or (org-roam-node-level node) 0)
       :title title
       :aliases (org-roam-node-aliases node)
       :tags (org-roam-node-tags node)
       :links (seq-uniq links)
       :properties (org-roam-node-properties node)
       :meta meta))))

(defun vulpea-db--parse-link-pair (link)
  "Parse LINK pair."
  (let ((type (car link))
        (value (cadr link)))
    (pcase type
      (`"http" (cons type (concat type ":" value)))
      (`"https" (cons type (concat type ":" value)))
      (_ (cons type value)))))



(defconst vulpea-db--schemata
  '((notes
     ([(id :not-null :primary-key)
       (path :not-null)
       (level :not-null)
       (title :not-null)
       (properties :not-null)
       aliases
       tags
       meta
       links]
      (:foreign-key [path] :references files [file] :on-delete :cascade)))
    (meta
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

      ;; make sure that extra tables exist table exists
      (advice-add 'org-roam-db :around #'vulpea-db--init)

      ;; make sure that all data is inserted into table
      (advice-add
       'org-roam-db-insert-file-node
       :after
       #'vulpea-db-insert-file-note)
      (advice-add
       'org-roam-db-insert-node-data
       :after
       #'vulpea-db-insert-outline-note)
      (advice-add
       'org-roam-db-map-links :after #'vulpea-db-insert-links))
     (t
      (setq vulpea-db--initalized nil)
      (advice-remove 'org-roam-db-map-links #'vulpea-db-insert-links)
      (advice-remove
       'org-roam-db-insert-node-data #'vulpea-db-insert-outline-note)
      (advice-remove
       'org-roam-db-insert-file-node #'vulpea-db-insert-file-note)
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

(defun vulpea-db-insert-file-note ()
  "Insert file level note into `vulpea' database."
  (org-with-point-at 1
    (when (and (= (org-outline-level) 0)
               (org-roam-db-node-p))
      (when-let ((id (org-id-get)))
        (let* ((file (buffer-file-name (buffer-base-buffer)))
               (title (org-link-display-format
                       (or (cadr
                            (assoc "TITLE"
                                   (org-collect-keywords '("title"))
                                   #'string-equal))
                           (file-relative-name
                            file org-roam-directory))))
               (level 0)
               (aliases (org-entry-get (point) "ROAM_ALIASES"))
               (aliases (when aliases
                          (split-string-and-unquote aliases)))
               (tags org-file-tags)
               (properties (org-entry-properties))
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
           [:delete :from notes
            :where (= id $s1)]
           id)
          (org-roam-db-query!
           (lambda (err)
             (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                    (error-message-string err)
                    title id file))
           [:insert :into notes
            :values $v1]
           (vector id
                   file
                   level
                   title
                   properties
                   aliases
                   tags
                   (seq-map
                    (lambda (kvp)
                      (list (car kvp) (cdr kvp)))
                    kvps)
                   nil))
          (when kvps
            (org-roam-db-query
             [:insert :into meta
              :values $v1]
             (seq-map
              (lambda (kvp)
                (vector id (car kvp) (cdr kvp)))
              kvps))))))))

(defun vulpea-db-insert-outline-note ()
  "Insert outline level note into `vulpea' database."
  (when-let ((id (org-id-get)))
    (let* ((file (buffer-file-name (buffer-base-buffer)))
           (heading-components (org-heading-components))
           (level (nth 1 heading-components))
           (title
            (or (nth 4 heading-components)
                (progn (lwarn
                        'org-roam
                        :warning
                        "Node in %s:%s:%s has no title, skipping..."
                        file
                        (line-number-at-pos)
                        (1+ (- (point) (line-beginning-position))))
                       (cl-return-from
                           org-roam-db-insert-node-data))))
           (properties (org-entry-properties))
           (title (org-link-display-format title))
           (aliases (org-entry-get (point) "ROAM_ALIASES"))
           (aliases (when aliases (split-string-and-unquote aliases)))
           (tags (org-get-tags)))
      (org-roam-db-query
       [:delete :from notes
        :where (= id $s1)]
       id)
      (org-roam-db-query!
       (lambda (err)
         (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                (error-message-string err)
                title id file))
       [:insert :into notes
        :values $v1]
       (vector id
               file
               level
               title
               properties
               aliases
               tags
               nil
               nil)))))

(defun vulpea-db-insert-links (&rest _)
  "Insert links into `vulpea' database."
  (org-with-point-at 1
    (let* ((links (org-element-map (org-element-parse-buffer) 'link
                    #'vulpea-db--parse-link-element))
           (kvps (seq-group-by #'car (apply #'append links))))
      (seq-map
       (lambda (kvp)
         (org-roam-db-query
          [:update notes
           :set (= links $s2)
           :where (= id $s1)]
          (car kvp)
          (seq-map
           (lambda (link)
             (list
              (nth 1 link)
              (nth 2 link)))
           (cdr kvp))))
       kvps))))

(defun vulpea-db--parse-link-element (link)
  "Parse LINK element.

Return list of triplets source, type and target."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (when-let ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (source (org-roam-id-at-point)))
      ;; For Org-ref links, we need to split the path into the cite
      ;; keys
      (when (and (boundp 'org-ref-cite-types)
                 (fboundp 'org-ref-split-and-strip-string)
                 (member type org-ref-cite-types))
        (setq path (org-ref-split-and-strip-string path)))
      (unless (listp path)
        (setq path (list path)))
      (seq-map
       (lambda (p)
         (list source type p))
       path))))



(provide 'vulpea-db)
;;; vulpea-db.el ends here
