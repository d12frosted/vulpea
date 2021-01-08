;;; vulpea-meta.el --- Metadata manipulation -*- lexical-binding: t; -*-
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
;; Functionality for metadata manipulation. Metadata is defined by the first
;; description list in the note, e.g. list like:
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
(require 'vulpea-const)
(require 'vulpea-macs)
(require 'vulpea-db)

;;;###autoload
(defun vulpea-meta (id)
  "Get metadata for note with ID.

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
key."
  (when-let ((file (vulpea-db-get-file-by-id id)))
    (vulpea-with-file file
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

(defun vulpea-meta--get (id prop)
  "Get all values of PROP for note with ID.

Return plist (:file :buffer :pl :items)"
  (let* ((meta (vulpea-meta id))
         (pl (plist-get meta :pl))
         (items-all (org-element-map pl 'item #'identity))
         (items
          (seq-filter
           (lambda (item)
             (string-equal
              prop
              (org-element-interpret-data
               (org-element-contents (org-element-property :tag item)))))
           items-all)))
    (plist-put meta :items items)))

;;;###autoload
(defun vulpea-meta-get-list (id prop &optional type)
  "Get all values of PROP for note with ID.

Each element value depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- id - id of the linked note."
  (setq type (or type 'string))
  (let* ((meta (vulpea-meta--get id prop))
         (items (plist-get meta :items)))
    (seq-map
     (lambda (item)
       (let ((val (car (org-element-contents item))))
         (pcase type
           (`raw val)
           (`string (s-trim-right
                     (substring-no-properties
                      (org-element-interpret-data (org-element-contents val)))))
           (`number (string-to-number
                     (s-trim-right
                      (substring-no-properties
                       (org-element-interpret-data (org-element-contents val))))))
           (`id (let ((el (car (org-element-contents val))))
                  (when (equal 'link
                               (org-element-type el))
                    (org-element-property :path el)))))))
     items)))

;;;###autoload
(defun vulpea-meta-get (id prop &optional type)
  "Get value of PROP for note with ID.

Result depends on TYPE:

- raw - org element object
- string (default) - an interpreted object (without trailing
  newline)
- number - an interpreted number
- id - id of the linked note.

If the note contains multiple values for a given PROP, the first
one is returned. In case all values are required, use
`vulpea-meta-get-list'."
  (car (vulpea-meta-get-list id prop type)))

;;;###autoload
(defun vulpea-meta-set (id prop value)
  "Set VALUE of PROP for note with ID.

If the VALUE is a list, then each element is inserted
separately.

Please note that all occurrences of PROP are replaced by VALUE."
  (let* ((values (if (listp value) value (list value)))
         (meta (vulpea-meta--get id prop))
         (file (plist-get meta :file))
         (buffer (plist-get meta :buffer))
         (pl (plist-get meta :pl))
         (items (plist-get meta :items))
         (img (org-element-copy (car items))))
    (vulpea-with-file file
      (cond
       (pl
        ;; TODO: inline
        (vulpea-meta-remove id prop)
        (cond
         (img
          (goto-char (org-element-property :begin img))
          (seq-do
           (lambda (val)
             (insert
              (org-element-interpret-data
               (org-element-set-contents (org-element-copy img)
                                         (vulpea-meta--format val)))))
           values)
          (when (equal (length items)
                       (length (org-element-contents pl)))
            (insert "\n")))
         (t
          (let* ((items-all (org-element-map pl 'item #'identity))
                 ;; we copy any item from the list so we don't need to deal with
                 ;; :bullet and other properties
                 (img (org-element-copy (car items-all))))
            (goto-char (org-element-property :begin pl))
            (seq-do
             (lambda (val)
               (insert
                (org-element-interpret-data
                 (org-element-set-contents
                  (org-element-put-property (org-element-copy img) :tag prop)
                  (vulpea-meta--format val)))))
             values)))))
       (t
        ;; insert either after the last keyword in the buffer, or after the
        ;; property drawer if it is present on the first line or on the fist
        ;; line
        (let* ((element (or (car (last (org-element-map buffer 'keyword #'identity)))
                            (car (org-element-map buffer 'property-drawer #'identity))))
               (point (if element (org-element-property :end element) 1))
               (eob (eq point (point-max))))
          (goto-char point)
          (when eob
            (insert "\n"))
          (seq-do
           (lambda (val)
             (insert "- " prop " :: "
                     (vulpea-meta--format val)
                     "\n"))
           values)
          (unless eob
            (insert "\n"))))))))

;;;###autoload
(defun vulpea-meta-remove (id prop)
  "Delete values of PROP for note with ID."
  (let* ((meta (vulpea-meta--get id prop))
         (items (plist-get meta :items))
         (pl (plist-get meta :pl))
         (file (plist-get meta :file)))
    (when (car items)
      (vulpea-with-file file
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

;;;###autoload
(defun vulpea-meta-clean (id)
  "Delete all meta from note with ID."
  (when-let* ((meta (vulpea-meta id))
              (pl (plist-get meta :pl))
              (file (plist-get meta :file)))
    (vulpea-with-file file
      (delete-region (org-element-property :begin pl)
                     (org-element-property :end pl)))))

(defun vulpea-meta--format (value)
  "Format a VALUE depending on it's type."
  (cond
   ((stringp value)
    (if (string-match-p vulpea-uuid-regexp value)
        (org-link-make-string (concat "id:" value)
                              (vulpea-db-get-title-by-id value))
      value))
   ((numberp value)
    (number-to-string value))
   (t (user-error "Unsupported type of '%s'" value))))

(provide 'vulpea-meta)
;;; vulpea-meta.el ends here
