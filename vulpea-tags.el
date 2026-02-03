;;; vulpea-tags.el --- Tag manipulation -*- lexical-binding: t; -*-
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
;; Created: 03 Feb 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functionality for tag manipulation. Provides both per-note
;; primitives and batch operations for managing tags across notes.
;;
;; Per-note functions:
;; - `vulpea-tags' - get tags for a note
;; - `vulpea-tags-add' - add tags to a note
;; - `vulpea-tags-remove' - remove tags from a note
;; - `vulpea-tags-set' - set all tags for a note
;;
;; Batch functions:
;; - `vulpea-tags-batch-add' - add a tag to multiple notes
;; - `vulpea-tags-batch-remove' - remove a tag from multiple notes
;; - `vulpea-tags-batch-rename' - rename a tag across all notes
;;
;;; Code:

(require 'seq)
(require 'vulpea-utils)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-buffer)

;;; Per-note tag operations

(defun vulpea-tags (note-or-id)
  "Get tags for NOTE-OR-ID.

For file-level notes (level = 0), returns filetags.
For heading-level notes (level > 0), returns heading tags."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id)))
    (when note
      (vulpea-utils-with-note note
        (vulpea-buffer-tags-get)))))

(defun vulpea-tags-add (note-or-id &rest tags)
  "Add TAGS to NOTE-OR-ID.

For file-level notes (level = 0), modifies filetags.
For heading-level notes (level > 0), modifies heading tags."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id)))
    (when note
      (vulpea-utils-with-note note
        (vulpea-buffer-tags-add tags)))))

(defun vulpea-tags-remove (note-or-id &rest tags)
  "Remove TAGS from NOTE-OR-ID.

For file-level notes (level = 0), modifies filetags.
For heading-level notes (level > 0), modifies heading tags.

Does nothing if the note has no tags."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id)))
    (when note
      (vulpea-utils-with-note note
        (when (vulpea-buffer-tags-get)
          (vulpea-buffer-tags-remove tags))))))

(defun vulpea-tags-set (note-or-id &rest tags)
  "Set TAGS for NOTE-OR-ID, replacing any existing tags.

For file-level notes (level = 0), modifies filetags.
For heading-level notes (level > 0), modifies heading tags."
  (let* ((note (if (stringp note-or-id)
                   (vulpea-db-get-by-id note-or-id)
                 note-or-id)))
    (when note
      (vulpea-utils-with-note note
        (apply #'vulpea-buffer-tags-set tags)))))

;;; Batch tag operations

(defun vulpea-tags-batch-add (notes tag)
  "Add TAG to all NOTES.

Uses `vulpea-utils-process-notes' for efficient batch processing.
Returns the count of notes processed."
  (let ((count 0))
    (vulpea-utils-process-notes notes
      (vulpea-buffer-tags-add (list tag))
      (setq count (1+ count)))
    count))

(defun vulpea-tags-batch-remove (notes tag)
  "Remove TAG from all NOTES.

Uses `vulpea-utils-process-notes' for efficient batch processing.
Returns the count of notes processed."
  (let ((count 0))
    (vulpea-utils-process-notes notes
      (vulpea-buffer-tags-remove (list tag))
      (setq count (1+ count)))
    count))

(defun vulpea-tags-batch-rename (old-tag new-tag)
  "Rename OLD-TAG to NEW-TAG across all notes.

Finds all notes with OLD-TAG, removes it, and adds NEW-TAG.
Returns the count of notes modified.

When called interactively, prompts for OLD-TAG from existing tags
and NEW-TAG as a string."
  (interactive
   (let* ((tags (vulpea-db-query-tags))
          (old (completing-read "Rename tag: " tags nil t))
          (new (read-string (format "Rename '%s' to: " old))))
     (list old new)))
  (let* ((notes (vulpea-db-query-by-tags-some (list old-tag)))
         (count 0))
    (vulpea-utils-process-notes notes
      (vulpea-buffer-tags-remove (list old-tag))
      (vulpea-buffer-tags-add (list new-tag))
      (setq count (1+ count)))
    (when (called-interactively-p 'any)
      (message "Renamed '%s' to '%s' in %d note%s"
               old-tag new-tag count (if (= count 1) "" "s")))
    count))

(provide 'vulpea-tags)
;;; vulpea-tags.el ends here
