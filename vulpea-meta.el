;;; vulpea-meta.el --- Metadata manipulation -*- lexical-binding: t; -*-
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
;; Functionality for metadata manipulation. Metadata is defined by the
;; first description list in the note, e.g. list like:
;;
;; - key1 :: value1
;; - key2 :: value21
;; - key2 :: value22
;; - key3 :: value3
;;
;;; Code:

(require 'org-element)
(require 'seq)
(require 's)
(require 'vulpea-utils)
(require 'vulpea-db)

(defvar vulpea-meta--uuid-regexp
  (concat
   "\\("
   "[a-zA-Z0-9]\\{8\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{4\\}"
   "-"
   "[a-zA-Z0-9]\\{12\\}"
   "\\)")
  "UUID regexp.")

(defun vulpea-meta (note-or-id)
  "Get metadata for NOTE-OR-ID.

Return plist (:file :buffer :pl)

Metadata is defined by the first description list in the note,
e.g. list like:

- key1 :: value1
- key2 :: value21
- key2 :: value22
- key3 :: value3

In most cases, it's better to use either `vulpea-meta-get' to
retrieve a single value for a given key or
`vulpea-meta-get-list' to retrieve all values for a given
key.

In case you are doing multiple calls to meta API, it's better to
get metadata using this function and use bang version of
functions, e.g. `vulpea-meta-get!'."
  (when-let ((file (if (stringp note-or-id)
                       (vulpea-db-get-file-by-id note-or-id)
                     (vulpea-note-path note-or-id))))
    (vulpea-utils-with-file file
      (let* ((buf (org-element-parse-buffer))
             (pls (org-element-map buf 'plain-list #'identity))
             (pl (seq-find
                  (lambda (pl)
                    (equal 'descriptive
                           (org-element-property :type pl)))
                  pls)))
        (list :file file
              :buffer buf
              :pl pl)))))

(defun vulpea-meta--get (meta prop)
  "Get all values of PROP from META.

Return plist (:file :buffer :pl :items)"
  (let* ((pl (plist-get meta :pl))
         (items-all (org-element-map pl 'item #'identity))
         (items
          (seq-filter
           (lambda (item)
             (string-equal
              prop
              (org-element-interpret-data
               (org-element-contents
                (org-element-property :tag item)))))
           items-all)))
    (plist-put meta :items items)))

(defun vulpea-meta-get-list! (meta prop &optional type)
  "Get all values of PROP from META.

Each element value depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol."
  (setq type (or type 'string))
  (let* ((meta (vulpea-meta--get meta prop))
         (items (plist-get meta :items)))
    (seq-map
     (lambda (item)
       (let ((val (car (org-element-contents item))))
         (pcase type
           (`raw val)
           (`symbol
            (intern
             (s-trim-right
              (substring-no-properties
               (org-element-interpret-data
                (org-element-contents val))))))
           (`string
            (s-trim-right
             (substring-no-properties
              (org-element-interpret-data
               (org-element-contents val)))))
           (`number
            (string-to-number
             (s-trim-right
              (substring-no-properties
               (org-element-interpret-data
                (org-element-contents val))))))
           (`note
            (let ((el (car (org-element-contents val))))
              (when (equal 'link
                           (org-element-type el))
                (pcase (org-element-property :type el)
                  ("id" (vulpea-db-get-by-id
                         (org-element-property :path el)))))))
           (`link
            (let ((el (car (org-element-contents val))))
              (when (equal 'link
                           (org-element-type el))
                (pcase (org-element-property :type el)
                  ("id" (org-element-property :path el))
                  (_ (org-element-property :raw-link el)))))))))
     items)))

(defun vulpea-meta-get-list (note-or-id prop &optional type)
  "Get all values of PROP from NOTE-OR-ID.

Each element value depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol."
  (vulpea-meta-get-list! (vulpea-meta note-or-id) prop type))

(defun vulpea-meta-get! (meta prop &optional type)
  "Get value of PROP from META.

Result depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-meta-get-list'."
  (car (vulpea-meta-get-list! meta prop type)))

(defun vulpea-meta-get (note-or-id prop &optional type)
  "Get value of PROP for NOTE-OR-ID.

Result depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- link - path of the link (either ID of the linked note or raw link)
- note - linked `vulpea-note'
- symbol - an interned symbol.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-meta-get-list'."
  (vulpea-meta-get! (vulpea-meta note-or-id) prop type))

(defun vulpea-meta-set (note-or-id prop value &optional append)
  "Set VALUE of PROP for NOTE-OR-ID.

If the VALUE is a list, then each element is inserted
separately.

Please note that all occurrences of PROP are replaced by VALUE.

When PROP is not yet set, VALUE is inserted at the beginning of
the meta, unless the optional argument APPEND is non-nil, in
which case VALUE is added at the end of the meta."
  (let* ((values (if (listp value) value (list value)))
         (meta (vulpea-meta--get (vulpea-meta note-or-id) prop))
         (file (plist-get meta :file))
         (buffer (plist-get meta :buffer))
         (pl (plist-get meta :pl))
         (items (plist-get meta :items))
         (img (org-element-copy (car items))))
    (vulpea-utils-with-file file
      (cond
       ;; descriptive plain list exists, update it
       (pl
        ;; TODO: inline
        (vulpea-meta-remove note-or-id prop)
        (cond
         ;; property already set, remove it and set again
         (img
          (goto-char (org-element-property :begin img))
          (seq-do
           (lambda (val)
             (insert
              (org-element-interpret-data
               (org-element-set-contents (org-element-copy img)
                                         (vulpea-meta-format val)))))
           values)
          (when (equal (length items)
                       (length (org-element-contents pl)))
            (insert "\n")))

         ;; property is not yet set, simply set it
         (t
          (let* ((items-all (org-element-map pl 'item #'identity))
                 ;; we copy any item from the list so we don't need to
                 ;; deal with :bullet and other properties
                 (img (org-element-copy (car items-all)))
                 (point (if append
                            (- (org-element-property :end pl)
                               (org-element-property :post-blank pl))
                          (org-element-property :begin pl))))
            ;; when APPEND and body is present, insert new item on the
            ;; next line after the last item
            (goto-char point)
            (seq-do
             (lambda (val)
               (insert
                (org-element-interpret-data
                 (org-element-set-contents
                  (org-element-put-property
                   (org-element-copy img)
                   :tag
                   prop)
                  (vulpea-meta-format val)))))
             values)))))

       ;; descriptive plain list does not exist, create one
       (t
        ;; insert either after the last keyword in the buffer, or
        ;; after the property drawer if it is present on the first
        ;; line or on the fist line
        (let*
            ((element
              (or
               (car (last (org-element-map
                           buffer 'keyword #'identity)))
               (car (org-element-map
                     buffer 'property-drawer #'identity))))
             (point
              (if element
                  (- (org-element-property :end element)
                     (org-element-property :post-blank element))
                (point-min))))
          (goto-char point)
          (insert "\n")
          (seq-do
           (lambda (val)
             (insert "- " prop " :: "
                     (vulpea-meta-format val)
                     "\n"))
           values)))))))

(defun vulpea-meta-remove (note-or-id prop)
  "Delete values of PROP for NOTE-OR-ID."
  (let* ((meta (vulpea-meta--get (vulpea-meta note-or-id) prop))
         (items (plist-get meta :items))
         (pl (plist-get meta :pl))
         (file (plist-get meta :file)))
    (when (car items)
      (vulpea-utils-with-file file
        (if (equal (length items)
                   (length (org-element-contents pl)))
            (delete-region (org-element-property :begin pl)
                           (org-element-property :end pl))
          (seq-do
           (lambda (item)
             (when-let* ((begin (org-element-property :begin item))
                         (end (org-element-property :end item)))
               (delete-region begin end)))
           (seq-reverse items)))))))

(defun vulpea-meta-clean (note-or-id)
  "Delete all meta from NOTE-OR-ID."
  (when-let* ((meta (vulpea-meta note-or-id))
              (pl (plist-get meta :pl))
              (file (plist-get meta :file)))
    (vulpea-utils-with-file file
      (delete-region (org-element-property :begin pl)
                     (org-element-property :end pl)))))

(defun vulpea-meta-format (value)
  "Format a VALUE depending on it's type."
  (cond
   ((vulpea-note-p value)
    (vulpea-utils-link-make-string value))
   ((and (stringp value)
         (string-match-p vulpea-meta--uuid-regexp value))
    (if-let* ((note (vulpea-db-get-by-id value)))
        (vulpea-utils-link-make-string note)
      (user-error "Note with id \"%s\" does not exist" value)))
   ((stringp value)
    (let ((domain (ignore-errors
                    (url-domain (url-generic-parse-url value)))))
      (if domain
          (org-link-make-string value domain)
        value)))
   ((numberp value)
    (number-to-string value))
   ((symbolp value)
    (symbol-name value))
   (t (user-error "Unsupported type of \"%s\"" value))))

(provide 'vulpea-meta)
;;; vulpea-meta.el ends here
