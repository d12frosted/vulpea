;;; vulpea-db.el --- Data Base querying -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "1.2.3"))
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
  (let ((files
         (seq-map
          #'car
          (org-roam-db-query
           [:select file
            :from titles
            :where (= title $s1)]
           title))))
    (seq-map
     (lambda (file)
       (make-vulpea-note
        :path file
        :title title
        :tags (vulpea-utils-with-file file
                (org-roam--extract-tags file))
        :level 0
        :id (vulpea-db-get-id-by-file file)
        :meta (vulpea-db-get-meta-by-file file)))
     files)))

;;
;; Querying

(defun vulpea-db-query (&optional filter-fn)
  "Query list of `vulpea-note' from database.

When FILTER-FN is non-nil, only notes that satisfy it are
returned."
  (let*
      ((rows (org-roam-db-query
              [:select [files:file
                        titles:title
                        tags:tags
                        ids:id
                        files:meta]
               :from titles
               :left :join tags
               :on (= titles:file tags:file)
               :left :join files
               :on (= titles:file files:file)
               :left :join ids
               :on (and (= titles:file ids:file)
                        (= ids:level 0))]))
       notes)
    (dolist (row rows notes)
      (pcase-let ((`(,file-path ,title ,tags ,id ,meta) row))
        (let ((note (make-vulpea-note
                     :path file-path
                     :title title
                     :tags tags
                     :id id
                     :level 0
                     :meta (make-vulpea-note-meta
                            :atime (plist-get meta :atime)
                            :mtime (plist-get meta :mtime)))))
          (when (or (null filter-fn)
                    (funcall filter-fn note))
            (push note notes)))))))

;;
;; Exchanging ID to X

(defun vulpea-db-get-by-id (id)
  "Find a `vulpea-note' by ID.

Supports headings in the note."
  (when-let*
      ((fls
        (org-roam-db-query
         [:select [file level]
          :from ids
          :where (= id $s1)]
         id))
       (fl (car fls))
       (file (car fl))
       (level (nth 1 fl))
       (title (if (= 0 level)
                  (org-roam-db--get-title file)
                (vulpea-utils-with-file file
                  (goto-char (cdr (org-id-find-id-in-file id file)))
                  (org-entry-get (point) "ITEM")))))
    (make-vulpea-note
     :path file
     :title title
     :tags (vulpea-utils-with-file file
             (org-roam--extract-tags file))
     :level level
     :id id
     :meta (vulpea-db-get-meta-by-file file))))

(defun vulpea-db-get-file-by-id (id)
  "Get file of `vulpea-note' with ID.

Supports headings in the note."
  (caar
   (org-roam-db-query
    [:select file
     :from ids
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
     :from ids
     :where (and (= file $s1)
                 (= level $s2))]
    (if (file-name-absolute-p file)
        file
      (expand-file-name file org-roam-directory))
    0)))

(defun vulpea-db-get-meta-by-file (file)
  "Get `vulpea-note-meta' of `vulpea-note' represented by FILE.

If the FILE is relative, it is considered to be relative to
`org-roam-directory'."
  (when-let ((meta
              (caar
               (org-roam-db-query
                [:select meta
                 :from files
                 :where (= file $s1)]
                (if (file-name-absolute-p file)
                    file
                  (expand-file-name file org-roam-directory))))))
    (make-vulpea-note-meta
     :atime (plist-get meta :atime)
     :mtime (plist-get meta :mtime))))

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
