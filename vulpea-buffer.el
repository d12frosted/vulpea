;;; vulpea-buffer.el --- Buffer related utilities -*- lexical-binding: t; -*-
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
;; Created: 13 May 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Various utilities to modify `org-mode' buffer, namely properties
;; and metadata.
;;
;; Properties are buffer-wide key-values defined as #+KEY: VALUE in
;; the header of file.
;;
;; Metadata is defined by the first description list in the file, e.g.
;; list like:
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



(defun vulpea-buffer-title-get ()
  "Get TITLE in current buffer."
  (vulpea-buffer-prop-get "title"))

(defun vulpea-buffer-title-set (title)
  "Set TITLE in current buffer.

If the title is already set, replace its value."
  (vulpea-buffer-prop-set "title" title))

(defun vulpea-buffer-tags-get ()
  "Return filetags value in current buffer."
  (vulpea-buffer-prop-get-list "filetags" "[ :]"))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.

If filetags value is already set, replace it."
  (if tags
      (vulpea-buffer-prop-set
       "filetags" (concat ":" (string-join tags ":") ":"))
    (vulpea-buffer-prop-remove "filetags")))

(defun vulpea-buffer-tags-add (tag)
  "Add a TAG to filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (append tags (list tag))))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-tags-remove (tag)
  "Remove a TAG from filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.

If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun vulpea-buffer-prop-set-list (name values &optional separators)
  "Set a file property called NAME to VALUES in current buffer.

VALUES are quoted and combined into single string using
`combine-and-quote-strings'.

If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.

If the property is already set, replace its value."
  (vulpea-buffer-prop-set
   name (combine-and-quote-strings values separators)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (let ((value (string-trim
                    (buffer-substring-no-properties
                     (match-beginning 1)
                     (match-end 1)))))
        (unless (string-empty-p value)
          value)))))

(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.

If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (vulpea-buffer-prop-get name)))
    (when value
      (split-string-and-unquote value separators))))

(defun vulpea-buffer-prop-remove (name)
  "Remove a buffer property called NAME."
  (org-with-point-at 1
    (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                             (point-max) t)
      (replace-match ""))))



(defun vulpea-buffer-meta ()
  "Get metadata from the current buffer.

Return plist (:file :buffer :pl)

Metadata is defined by the first description list in the note,
e.g. list like:

- key1 :: value1
- key2 :: value21
- key2 :: value22
- key3 :: value3

In most cases, it's better to use either `vulpea-buffer-meta-get'
to retrieve a single value for a given key or
`vulpea-buffer-meta-get-list' to retrieve all values for a given
key.

In case you are doing multiple calls to meta API, it's better to
get metadata using this function and use bang version of
functions, e.g. `vulpea-buffer-meta-get!'."
  (let* ((file (buffer-file-name (current-buffer)))
         (buf (org-element-parse-buffer))
         (pls (org-element-map buf 'plain-list #'identity))
         (pl (seq-find
              (lambda (pl)
                (equal 'descriptive
                       (org-element-property :type pl)))
              pls)))
    (list :file file
          :buffer buf
          :pl pl)))

(defun vulpea-buffer-meta--get (meta prop)
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

(defun vulpea-buffer-meta-get-list! (meta prop &optional type)
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
  (let* ((meta (vulpea-buffer-meta--get meta prop))
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

(defun vulpea-buffer-meta-get! (meta prop &optional type)
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
`vulpea-buffer-meta-get-list'."
  (car (vulpea-buffer-meta-get-list! meta prop type)))

(defun vulpea-buffer-meta-set (prop value &optional append)
  "Set VALUE of PROP in current buffer.

If the VALUE is a list, then each element is inserted
separately.

Please note that all occurrences of PROP are replaced by VALUE.

When PROP is not yet set, VALUE is inserted at the beginning of
the meta, unless the optional argument APPEND is non-nil, in
which case VALUE is added at the end of the meta."
  (let* ((values (if (listp value) value (list value)))
         (meta (vulpea-buffer-meta--get (vulpea-buffer-meta) prop))
         (buffer (plist-get meta :buffer))
         (pl (plist-get meta :pl))
         (items (plist-get meta :items))
         (img (org-element-copy (car items))))
    (cond
     ;; descriptive plain list exists, update it
     (pl
      ;; TODO: inline
      (vulpea-buffer-meta-remove prop)
      (cond
       ;; property already set, remove it and set again
       (img
        (goto-char (org-element-property :begin img))
        (seq-do
         (lambda (val)
           (insert
            (org-element-interpret-data
             (org-element-set-contents
              (org-element-copy img)
              (vulpea-buffer-meta-format val)))))
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
                (vulpea-buffer-meta-format val)))))
           values)))))

     ;; descriptive plain list does not exist, create one
     (t
      ;; insert either after the last keyword in the buffer, or
      ;; after the property drawer if it is present on the first
      ;; line or on the fist line
      (let*
          ((element
            (or
             (car
              (last
               (org-element-map buffer 'keyword #'identity)))
             (car
              (org-element-map buffer 'property-drawer #'identity))))
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
                   (vulpea-buffer-meta-format val)
                   "\n"))
         values))))))

(defun vulpea-buffer-meta-remove (prop)
  "Delete values of PROP from current buffer."
  (let* ((meta (vulpea-buffer-meta--get (vulpea-buffer-meta) prop))
         (items (plist-get meta :items))
         (pl (plist-get meta :pl)))
    (when (car items)
      (if (equal (length items)
                 (length (org-element-contents pl)))
          (delete-region (org-element-property :begin pl)
                         (org-element-property :end pl))
        (seq-do
         (lambda (item)
           (when-let* ((begin (org-element-property :begin item))
                       (end (org-element-property :end item)))
             (delete-region begin end)))
         (seq-reverse items))))))

(defun vulpea-buffer-meta-clean ()
  "Delete all meta from current buffer."
  (when-let* ((meta (vulpea-buffer-meta))
              (pl (plist-get meta :pl)))
    (delete-region
     (org-element-property :begin pl)
     (org-element-property :end pl))))

(defun vulpea-buffer-meta-format (value)
  "Format a VALUE depending on it's type."
  (cond
   ((vulpea-note-p value)
    (vulpea-utils-link-make-string value))
   ((and (stringp value)
         (string-match-p vulpea-utils--uuid-regexp value))
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



(provide 'vulpea-buffer)
;;; vulpea-buffer.el ends here
