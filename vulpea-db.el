;;; vulpea-db.el --- Data Base querying -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
(require '+seq)
(require 'vulpea-macs)

;;
;; Searching

;;;###autoload
(defun vulpea-db-search-by-title (title)
  "Return a list of notes with TITLE.

Each note is represented as a property list of the following
form: (:path :title :tags :id)."
  (let ((files
         (seq-map
          #'+seq-singleton
          (org-roam-db-query [:select file
                              :from titles
                              :where (= title $s1)]
                             title))))
    (seq-map
     (lambda (file)
       (list :path file
             :title title
             :tags (vulpea-with-file file
                     (org-roam--extract-tags file))
             :id (vulpea-db-get-id-by-file file)))
     files)))

;;
;; Exchanging ID to X

;;;###autoload
(defun vulpea-db-get-by-id (id)
  "Find a note by ID.

Supports headings in the note."
  (when-let ((file (vulpea-db-get-file-by-id id))
             (title (vulpea-db-get-title-by-id id)))
    (list :path file
          :title title
          :tags (vulpea-with-file file
                  (org-roam--extract-tags file))
          :id id)))

;;;###autoload
(defun vulpea-db-get-title-by-id (id)
  "Find a note title by ID.

Supports headings in the note."
  (when-let* ((fls
               (org-roam-db-query
                [:select [file level]
                 :from ids
                 :where (= id $s1)]
                id))
              (fl (+seq-singleton fls))
              (file (car fl))
              (level (nth 1 fl)))
    (if (= 0 level)
        (org-roam-db--get-title file)
      (vulpea-with-file file
        (goto-char (cdr (org-id-find-id-in-file id file)))
        (org-entry-get (point) "ITEM")))))

;;;###autoload
(defun vulpea-db-get-file-by-id (id)
  "Get file of note with ID.

Supports headings in the note."
  (+seq-singleton
   (car
    (org-roam-db-query
     [:select file
      :from ids
      :where (= id $s1)]
     id))))

;;
;; Exchange FILE to X

;;;###autoload
(defun vulpea-db-get-id-by-file (file)
  "Get ID of note represented by FILE.

If the FILE is relative, it is considered to be relative to
`org-roam-directory'."
  (+seq-singleton
   (car
    (org-roam-db-query
     [:select id
              :from ids
              :where (and (= file $s1)
                          (= level $s2))]
     (if (file-name-absolute-p file)
         file
       (expand-file-name file org-roam-directory))
     0))))

(provide 'vulpea-db)
;;; vulpea-db.el ends here
