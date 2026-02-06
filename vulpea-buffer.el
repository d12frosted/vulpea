;;; vulpea-buffer.el --- Buffer related utilities -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
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

(require 'dash)
(require 'org-element)
(require 'seq)

(declare-function vulpea-db-query-tags "vulpea-db-query")
(require 's)

(require 'url-parse)
(require 'vulpea-utils)
(require 'vulpea-db)
(require 'vulpea-db-query)

;;; Customization

(defcustom vulpea-buffer-alias-property "ALIASES"
  "Property name for note aliases.

You can change this to any property name you prefer, such as
\"ROAM_ALIASES\" for org-roam compatibility."
  :type 'string
  :group 'vulpea)

(defun vulpea-buffer-title-get ()
  "Get TITLE in current buffer."
  (vulpea-buffer-prop-get "title"))

(defun vulpea-buffer-title-set (title)
  "Set TITLE in current buffer.

If the title is already set, replace its value."
  (vulpea-buffer-prop-set "title" title))

(defun vulpea-buffer-tags-get (&optional local)
  "Return tags for the note at point.

At file level (outline-level 0), returns filetags.
At heading level, returns all applicable tags including those
inherited from the file and parent headings, respecting
`org-use-tag-inheritance'.  When LOCAL is non-nil, returns only
the heading's own tags without inheritance."
  (if (= (org-outline-level) 0)
      (vulpea-buffer-prop-get-list "filetags" "[ :]")
    (mapcar #'substring-no-properties
            (org-get-tags nil local))))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS for the note at point.

At file level (outline-level 0), sets filetags.
At heading level, sets heading tags.
Duplicate tags are automatically removed."
  (let ((tags (seq-uniq tags)))
    (if (= (org-outline-level) 0)
        (if tags
            (vulpea-buffer-prop-set
             "filetags" (concat ":" (string-join tags ":") ":"))
          (vulpea-buffer-prop-remove "filetags"))
      (save-excursion
        (org-back-to-heading t)
        (org-set-tags tags)))))

(defun vulpea-buffer-tags-add (&optional tags)
  "Add TAGS to the note at point.

At file level (outline-level 0), modifies filetags.
At heading level, modifies heading tags.

When called interactively, prompt for tags to add with completion
from existing tags in the database."
  (interactive
   (list (completing-read-multiple
          "Tag: "
          (ignore-errors (vulpea-db-query-tags)))))
  (let* ((tags (if (listp tags) tags (list tags)))
         (current-tags (vulpea-buffer-tags-get t))
         (new-tags (append current-tags tags)))
    (apply #'vulpea-buffer-tags-set new-tags)))

(defun vulpea-buffer-tags-remove (&optional tags)
  "Remove TAGS from the note at point.

At file level (outline-level 0), modifies filetags.
At heading level, modifies heading tags.

When called interactively, prompt for tags to remove from current tags."
  (interactive)
  (let* ((current-tags (vulpea-buffer-tags-get t)))
    (unless current-tags
      (user-error "No tags to remove"))
    (let* ((tags (or (if (listp tags) tags (list tags))
                     (completing-read-multiple "Remove tag: " current-tags)))
           (new-tags (seq-difference current-tags tags #'string-equal)))
      (apply #'vulpea-buffer-tags-set new-tags))))

(defun vulpea-buffer-alias-get ()
  "Get list of aliases for the note at point.

Returns list of alias strings from the property defined by
`vulpea-buffer-alias-property'. Handles both quoted aliases (with spaces)
and unquoted aliases properly."
  (when-let* ((aliases-str (org-entry-get nil vulpea-buffer-alias-property)))
    (setq aliases-str (string-trim aliases-str))
    (let ((result nil)
          (pos 0))
      (while (< pos (length aliases-str))
        (let ((char (aref aliases-str pos)))
          (cond
           ;; Skip whitespace
           ((= char ?\s)
            (setq pos (1+ pos)))
           ;; Quoted alias - find closing quote
           ((= char ?\")
            (let ((end (string-match "\"" aliases-str (1+ pos))))
              (if end
                  (progn
                    (push (substring aliases-str (1+ pos) end) result)
                    (setq pos (1+ end)))
                (error "Unmatched quote in %s" vulpea-buffer-alias-property))))
           ;; Unquoted alias - find next space or end of string
           (t
            (let ((end (or (string-match " " aliases-str pos)
                           (length aliases-str))))
              (push (substring aliases-str pos end) result)
              (setq pos end))))))
      (nreverse result))))

(defun vulpea-buffer-alias-add (alias)
  "Add ALIAS to the note at point.

ALIAS is added to the property defined by `vulpea-buffer-alias-property'.
If ALIAS contains spaces, it will be quoted automatically."
  (interactive "sAlias: ")
  (let* ((aliases (vulpea-buffer-alias-get))
         (alias (string-trim alias)))
    (unless (member alias aliases)
      (setq aliases (append aliases (list alias)))
      ;; Format aliases: quote ones with spaces, leave others unquoted
      (let ((formatted-aliases
             (mapcar (lambda (a)
                       (if (string-match-p " " a)
                           (format "\"%s\"" a)
                         a))
                     aliases)))
        (org-entry-put nil vulpea-buffer-alias-property (string-join formatted-aliases " "))))))

(defun vulpea-buffer-alias-set (&rest aliases)
  "Set ALIASES for the note at point, replacing any existing aliases.

ALIASES is a list of alias strings. If empty, removes the alias property.
Aliases containing spaces will be quoted automatically."
  (if aliases
      (let ((formatted-aliases
             (mapcar (lambda (a)
                       (if (string-match-p " " a)
                           (format "\"%s\"" a)
                         a))
                     aliases)))
        (org-entry-put nil vulpea-buffer-alias-property (string-join formatted-aliases " ")))
    (org-entry-delete nil vulpea-buffer-alias-property)))

(defun vulpea-buffer-alias-remove (&optional alias)
  "Remove ALIAS from the note at point.

If ALIAS is nil, prompt for an alias to remove from available aliases."
  (interactive)
  (let* ((aliases (vulpea-buffer-alias-get)))
    (when aliases
      (let* ((alias (or alias
                        (completing-read "Remove alias: " aliases nil t)))
             (aliases (delete alias aliases)))
        (if aliases
            ;; Format aliases: quote ones with spaces, leave others unquoted
            (let ((formatted-aliases
                   (mapcar (lambda (a)
                             (if (string-match-p " " a)
                                 (format "\"%s\"" a)
                               a))
                           aliases)))
              (org-entry-put nil vulpea-buffer-alias-property (string-join formatted-aliases " ")))
          ;; No aliases left, remove the property entirely
          (org-entry-delete nil vulpea-buffer-alias-property))))))

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



(defun vulpea-buffer-meta (&optional bound)
  "Get metadata from the current buffer.

Return plist (:file :buffer :pl :bound)

BOUND controls the scope of metadata extraction:
- nil or \\='buffer: search the entire buffer (legacy behavior)
- \\='heading: if point is in a heading, scope to that subtree;
  otherwise scope to content before first heading
- A position (number): scope to the subtree at that position

Metadata is defined by the first description list in the scope,
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
         ;; Determine the element to search within
         (scope-element
          (cond
           ;; No bound or 'buffer - search whole buffer
           ((or (null bound) (eq bound 'buffer))
            buf)
           ;; 'heading - auto-detect based on current position
           ((eq bound 'heading)
            (save-excursion
              (if (org-before-first-heading-p)
                  ;; Before first heading - get file-level section
                  (vulpea-buffer-meta--file-level-section buf)
                ;; At or after a heading - find the section within the headline
                (org-back-to-heading-or-point-min t)
                (let ((heading-pos (point)))
                  (vulpea-buffer-meta--heading-section buf heading-pos)))))
           ;; Position - find heading at that position
           ((numberp bound)
            (save-excursion
              (goto-char bound)
              (if (org-before-first-heading-p)
                  (vulpea-buffer-meta--file-level-section buf)
                (org-back-to-heading-or-point-min t)
                (let ((heading-pos (point)))
                  (vulpea-buffer-meta--heading-section buf heading-pos)))))
           (t buf)))
         ;; Find the first descriptive list, stopping at headlines
         (pls (org-element-map scope-element 'plain-list #'identity nil nil 'headline))
         (pl (seq-find
              (lambda (pl)
                (equal 'descriptive
                       (org-element-property :type pl)))
              pls)))
    (list :file file
          :buffer buf
          :pl pl
          :bound bound)))

(defun vulpea-buffer-meta--file-level-section (buf)
  "Get the file-level section from parsed buffer BUF.
Returns the section element before any headlines, or nil if none exists."
  ;; The file-level section is a direct child of org-data, before any headline
  (let ((children (org-element-contents buf)))
    (cl-find-if
     (lambda (el)
       (eq (org-element-type el) 'section))
     children)))

(defun vulpea-buffer-meta--heading-section (buf heading-pos)
  "Get the section element from the heading at HEADING-POS in BUF.
Returns the section child of the headline, which contains the
metadata but not nested subheadings."
  (let ((heading-el (org-element-map buf 'headline
                      (lambda (hl)
                        (when (= (org-element-property :begin hl) heading-pos)
                          hl))
                      nil t)))
    (when heading-el
      ;; Get the first section child of the headline
      (cl-find-if
       (lambda (el)
         (eq (org-element-type el) 'section))
       (org-element-contents heading-el)))))

(defun vulpea-buffer-meta-props (&optional meta)
  "Return list of all props from META."
  (let* ((meta (or meta (vulpea-buffer-meta)))
         (pl (plist-get meta :pl)))
    (->> (org-element-map pl 'item #'identity)
         (--map (substring-no-properties
                 (org-element-interpret-data
                  (org-element-contents
                   (org-element-property :tag it))))))))

(defsubst vulpea-buffer-meta-get (prop type &optional bound)
  "Get all values of metadata PROP of TYPE from buffer.
BOUND controls the scope - see `vulpea-buffer-meta' for details."
  (vulpea-buffer-meta-get! (vulpea-buffer-meta bound) prop type))

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
              (substring-no-properties
               (org-element-interpret-data
                (org-element-contents
                 (org-element-property :tag item))))))
           items-all)))
    (plist-put meta :items items)))

(defsubst vulpea-buffer-meta-get-list (prop &optional type bound)
  "Get all values of metadata PROP of TYPE as a list from buffer.
BOUND controls the scope - see `vulpea-buffer-meta' for details."
  (vulpea-buffer-meta-get-list! (vulpea-buffer-meta bound) prop type))

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
    (if (eq type 'note)
        (let* ((kvps (cl-loop
                      for item in items
                      for val = (car (org-element-contents item))
                      for el = (car (org-element-contents val))
                      when (equal 'link (org-element-type el))
                      when (string-equal (org-element-property :type el) "id")
                      collect (cons (org-element-property :path el)
                                    (substring-no-properties (car (org-element-contents el))))))
               (ids (mapcar #'car kvps))
               (notes (cl-loop for note in (vulpea-db-query-by-ids ids)
                               unless (vulpea-note-primary-title note)
                               collect note)))
          (cl-loop
           for it in kvps
           collect (let* ((id (car it))
                          (desc (cdr it))
                          (note (--find (string-equal id (vulpea-note-id it)) notes)))
                     (when (and note
                                desc
                                (seq-contains-p (vulpea-note-aliases note) desc))
                       (setf (vulpea-note-primary-title note) (vulpea-note-title note))
                       (setf (vulpea-note-title note) desc))
                     note)))
      (cl-loop
       for item in items
       collect (let ((val (car (org-element-contents item))))
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
                   (`link
                    (let ((el (car (org-element-contents val))))
                      (when (equal 'link
                                   (org-element-type el))
                        (pcase (org-element-property :type el)
                          ("id" (org-element-property :path el))
                          (_ (org-element-property :raw-link el))))))))))))

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

(defun vulpea-buffer-meta-set (prop value &optional append bound)
  "Set VALUE of PROP in current buffer.

If the VALUE is a list, then each element is inserted
separately.

Please note that all occurrences of PROP are replaced by VALUE.

When PROP is not yet set, VALUE is inserted at the beginning of
the meta, unless the optional argument APPEND is non-nil, in
which case VALUE is added at the end of the meta.

BOUND controls the scope - see `vulpea-buffer-meta' for details.
When BOUND is \\='heading or a position, operates within that
heading's subtree."
  (let* ((values (if (listp value) value (list value)))
         (meta (vulpea-buffer-meta--get (vulpea-buffer-meta bound) prop))
         (buffer (plist-get meta :buffer))
         (pl (plist-get meta :pl))
         (items (plist-get meta :items))
         (img (org-element-copy (car items))))
    (cond
     ;; descriptive plain list exists, update it
     (pl
      ;; TODO: inline
      (vulpea-buffer-meta-remove prop bound)
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
        (when (and (equal (length items)
                          (length (org-element-contents pl)))
                   (> (length items) 1))
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
      (let ((point (vulpea-buffer-meta--insertion-point buffer bound)))
        (goto-char point)
        (insert "\n")
        (seq-do
         (lambda (val)
           (insert "- " prop " :: "
                   (vulpea-buffer-meta-format val)
                   "\n"))
         values))))))

(defun vulpea-buffer-meta--insertion-point (buffer bound)
  "Find the insertion point for new metadata.
BUFFER is the parsed org buffer.
BOUND controls the scope - see `vulpea-buffer-meta' for details."
  (cond
   ;; Heading scope - find insertion point within heading
   ((or (eq bound 'heading) (numberp bound))
    (save-excursion
      (when (numberp bound)
        (goto-char bound))
      (if (org-before-first-heading-p)
          ;; File level - use original logic
          (let ((element
                 (or
                  (car (last (org-element-map buffer 'keyword #'identity)))
                  (car (org-element-map buffer 'property-drawer #'identity)))))
            (if element
                (- (org-element-property :end element)
                   (org-element-property :post-blank element))
              (point-min)))
        ;; In a heading - find property drawer or end of heading line
        (org-back-to-heading-or-point-min t)
        (let* ((heading-pos (point))
               (section (vulpea-buffer-meta--heading-section buffer heading-pos))
               (prop-drawer (when section
                              (org-element-map section 'property-drawer
                                #'identity nil t))))
          (if prop-drawer
              (- (org-element-property :end prop-drawer)
                 (org-element-property :post-blank prop-drawer))
            ;; No property drawer - insert after heading line
            (end-of-line)
            (point))))))
   ;; Buffer scope - original logic
   (t
    (let ((element
           (or
            (car (last (org-element-map buffer 'keyword #'identity)))
            (car (org-element-map buffer 'property-drawer #'identity)))))
      (if element
          (- (org-element-property :end element)
             (org-element-property :post-blank element))
        (point-min))))))

(defun vulpea-buffer-meta-set-batch (props-alist &optional bound)
  "Set multiple meta properties in current buffer efficiently.

PROPS-ALIST is an alist where each element is (PROP . VALUE).
VALUE can be a single value or a list of values.

BOUND controls the scope - see `vulpea-buffer-meta' for details.

This function parses the buffer only once, making it much more
efficient than calling `vulpea-buffer-meta-set' multiple times.

Example:
  (vulpea-buffer-meta-set-batch
    \\='((\"status\" . \"active\")
      (\"priority\" . 1)
      (\"tags\" . (\"a\" \"b\" \"c\"))))"
  (when props-alist
    (let* ((meta (vulpea-buffer-meta bound))
           (buffer (plist-get meta :buffer))
           (pl (plist-get meta :pl))
           (items-all (when pl (org-element-map pl 'item #'identity)))
           (template-item (org-element-copy (car items-all)))
           (props-to-set (mapcar #'car props-alist))
           ;; Collect all items that need to be deleted
           (items-to-delete
            (when items-all
              (seq-filter
               (lambda (item)
                 (member
                  (substring-no-properties
                   (org-element-interpret-data
                    (org-element-contents
                     (org-element-property :tag item))))
                  props-to-set))
               items-all)))
           ;; Check if we're removing all items (need to delete whole list)
           (removing-all (and items-to-delete
                              (= (length items-to-delete)
                                 (length items-all)))))
      (cond
       ;; Case 1: descriptive list exists
       (pl
        (let ((insert-point (org-element-property :begin pl)))
          ;; Delete items in reverse order to preserve positions
          (if removing-all
              ;; Delete whole list if removing all items
              (delete-region (org-element-property :begin pl)
                             (org-element-property :end pl))
            ;; Delete individual items
            (dolist (item (sort (copy-sequence items-to-delete)
                                (lambda (a b)
                                  (> (org-element-property :begin a)
                                     (org-element-property :begin b)))))
              (delete-region (org-element-property :begin item)
                             (org-element-property :end item))))
          ;; Insert new values
          (goto-char insert-point)
          (if removing-all
              ;; Need to create new list from scratch
              (dolist (pair props-alist)
                (let ((prop (car pair))
                      (values (if (listp (cdr pair)) (cdr pair) (list (cdr pair)))))
                  (dolist (val values)
                    (insert "- " prop " :: "
                            (vulpea-buffer-meta-format val)
                            "\n"))))
            ;; Use template item for formatting
            (dolist (pair props-alist)
              (let ((prop (car pair))
                    (values (if (listp (cdr pair)) (cdr pair) (list (cdr pair)))))
                (dolist (val values)
                  (insert
                   (org-element-interpret-data
                    (org-element-set-contents
                     (org-element-put-property
                      (org-element-copy template-item)
                      :tag
                      prop)
                     (vulpea-buffer-meta-format val))))))))))

       ;; Case 2: no descriptive list, create one
       (t
        (let ((point (vulpea-buffer-meta--insertion-point buffer bound)))
          (goto-char point)
          (insert "\n")
          (dolist (pair props-alist)
            (let ((prop (car pair))
                  (values (if (listp (cdr pair)) (cdr pair) (list (cdr pair)))))
              (dolist (val values)
                (insert "- " prop " :: "
                        (vulpea-buffer-meta-format val)
                        "\n"))))))))))

(defun vulpea-buffer-meta-remove (prop &optional bound)
  "Delete values of PROP from current buffer.
BOUND controls the scope - see `vulpea-buffer-meta' for details."
  (let* ((meta (vulpea-buffer-meta--get (vulpea-buffer-meta bound) prop))
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

(defun vulpea-buffer-meta-clean (&optional bound)
  "Delete all meta from current buffer.
BOUND controls the scope - see `vulpea-buffer-meta' for details."
  (when-let* ((meta (vulpea-buffer-meta bound))
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
         (string-match-p (concat "^" vulpea-utils--uuid-regexp "$") value))
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

(defun vulpea-buffer-meta-sort (props)
  "Sort meta in current buffer using list of PROPS.

Whatever is not part of PROPS is left in the same order but appended to
the end after PROPS."
  (let* ((meta (vulpea-buffer-meta))
         (props-all (->> (org-element-map (plist-get meta :pl) 'item #'identity)
                         (--map (substring-no-properties
                                 (org-element-interpret-data
                                  (org-element-contents
                                   (org-element-property :tag it)))))))
         (props-extra (-difference props-all props)))
    (vulpea-buffer-meta-clean)
    (--each props
      (vulpea-buffer-meta-set it (vulpea-buffer-meta-get-list! meta it) 'append))
    (--each props-extra
      (vulpea-buffer-meta-set it (vulpea-buffer-meta-get-list! meta it) 'append))))



(provide 'vulpea-buffer)
;;; vulpea-buffer.el ends here
