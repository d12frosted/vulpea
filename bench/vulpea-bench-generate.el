;;; vulpea-bench-generate.el --- Generate test notes for benchmarking -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
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
;; Created: 20 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Fast note generator for performance benchmarking.
;; Can generate millions of notes with realistic content.
;;
;;; Code:

(require 'seq)

(defvar vulpea-bench-words
  '("lorem" "ipsum" "dolor" "sit" "amet" "consectetur" "adipiscing" "elit"
    "sed" "do" "eiusmod" "tempor" "incididunt" "ut" "labore" "et" "dolore"
    "magna" "aliqua" "enim" "ad" "minim" "veniam" "quis" "nostrud"
    "exercitation" "ullamco" "laboris" "nisi" "aliquip" "ex" "ea" "commodo"
    "consequat" "duis" "aute" "irure" "in" "reprehenderit" "voluptate"
    "velit" "esse" "cillum" "fugiat" "nulla" "pariatur" "excepteur" "sint"
    "occaecat" "cupidatat" "non" "proident" "sunt" "culpa" "qui" "officia"
    "deserunt" "mollit" "anim" "id" "est" "laborum")
  "Word pool for generating content.")

(defvar vulpea-bench-tags
  '("project" "person" "concept" "reference" "area" "resource" "meeting"
    "idea" "review" "draft" "archived" "active" "backlog" "urgent")
  "Tag pool for generating tags.")

(defun vulpea-bench-uuid ()
  "Generate a random UUID."
  (format "%08x-%04x-%04x-%04x-%012x"
          (random (expt 16 8))
          (random (expt 16 4))
          (logior (ash #x4 12) (random (expt 16 3)))
          (logior (ash #x8 12) (random (expt 16 3)))
          (random (expt 16 12))))

(defun vulpea-bench-random-words (n)
  "Generate N random words from the word pool."
  (mapconcat #'identity
             (cl-loop repeat n
                      collect (seq-random-elt vulpea-bench-words))
             " "))

(defun vulpea-bench-random-title ()
  "Generate a random title (2-5 words, capitalized)."
  (let ((words (cl-loop repeat (+ 2 (random 4))
                        collect (seq-random-elt vulpea-bench-words))))
    (mapconcat #'capitalize words " ")))

(defun vulpea-bench-random-tags (max-count)
  "Generate 0 to MAX-COUNT random tags."
  (when (> (random 100) 30)  ; 70% chance of having tags
    (let ((count (1+ (random max-count))))
      (cl-loop repeat count
               collect (seq-random-elt vulpea-bench-tags)
               into tags
               finally return (seq-uniq tags)))))

(defun vulpea-bench-random-aliases (max-count)
  "Generate 0 to MAX-COUNT random aliases."
  (when (> (random 100) 60)  ; 40% chance of having aliases
    (cl-loop repeat (1+ (random max-count))
             collect (vulpea-bench-random-title))))

(defun vulpea-bench-generate-note-content (id &optional with-headings max-headings)
  "Generate note content with ID.
If WITH-HEADINGS, add up to MAX-HEADINGS random headings."
  (let* ((title (vulpea-bench-random-title))
         (tags (vulpea-bench-random-tags 4))
         (aliases (vulpea-bench-random-aliases 3))
         (paragraphs (+ 1 (random 4))))

    (with-output-to-string
      ;; Property drawer
      (princ ":PROPERTIES:\n")
      (princ (format ":ID:       %s\n" id))
      (princ ":END:\n")

      ;; Title
      (princ (format "#+TITLE: %s\n" title))

      ;; Tags
      (when tags
        (princ (format "#+FILETAGS: :%s:\n" (string-join tags ":"))))

      ;; Aliases
      (when aliases
        (princ ":PROPERTIES:\n")
        (princ (format ":ALIASES: %s\n"
                       (mapconcat (lambda (a)
                                    (if (string-match-p " " a)
                                        (format "\"%s\"" a)
                                      a))
                                  aliases " ")))
        (princ ":END:\n"))

      (princ "\n")

      ;; Body paragraphs
      (dotimes (_ paragraphs)
        (princ (vulpea-bench-random-words (+ 20 (random 30))))
        (princ "\n\n"))

      ;; Headings
      (when with-headings
        (dotimes (_ (random (or max-headings 3)))
          (princ (format "* %s\n\n" (vulpea-bench-random-title)))
          (princ (vulpea-bench-random-words (+ 15 (random 20))))
          (princ "\n\n"))))))

(defun vulpea-bench-generate-notes (output-dir count &optional with-headings)
  "Generate COUNT notes in OUTPUT-DIR.
If WITH-HEADINGS is non-nil, add random headings to notes.

Returns list of (file-path . id) pairs."
  (unless (file-directory-p output-dir)
    (make-directory output-dir t))

  (message "Generating %d notes in %s..." count output-dir)
  (let ((start-time (current-time))
        (files nil)
        (progress-interval (max 1 (/ count 20))))  ; Report every 5%

    (dotimes (i count)
      (let* ((id (vulpea-bench-uuid))
             (filename (format "%05d-%s.org" i (substring id 0 8)))
             (filepath (expand-file-name filename output-dir))
             (content (vulpea-bench-generate-note-content id with-headings 3)))

        ;; Write file efficiently
        (with-temp-file filepath
          (insert content))

        (push (cons filepath id) files)

        ;; Progress reporting
        (when (zerop (mod (1+ i) progress-interval))
          (message "  Generated %d/%d notes (%.1f%%)"
                   (1+ i) count (* 100.0 (/ (float (1+ i)) count))))))

    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Generated %d notes in %.2f seconds (%.0f notes/sec)"
               count elapsed (/ count elapsed)))

    (nreverse files)))

(defun vulpea-bench-clean-directory (dir)
  "Remove all .org files from DIR."
  (when (file-directory-p dir)
    (dolist (file (directory-files dir t "\\.org$"))
      (delete-file file))
    (message "Cleaned %s" dir)))

(provide 'vulpea-bench-generate)
;;; vulpea-bench-generate.el ends here
