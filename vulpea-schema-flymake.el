;;; vulpea-schema-flymake.el --- Flymake backend for schema violations -*- lexical-binding: t; -*-
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
;; This file is not part of GNU Emacs.
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; A flymake backend that flags schema violations in the current note
;; buffer as you edit.
;;
;; Files are the source of truth in vulpea, so a violating note cannot be
;; blocked from being written; in-buffer linting is the closest thing to
;; "enforce on write" that fits the model - you see the problem while
;; authoring.  Enable `vulpea-schema-flymake-mode' in a note buffer (it is
;; off by default).  The notes in the buffer (file-level and heading-level)
;; are validated against every applicable schema with
;; `vulpea-schema-note-violations', and each violation is shown on the
;; offending field's line, or on the note's heading for a missing required
;; field.
;;
;;; Code:

(require 'flymake)
(require 'org-element)
(require 'vulpea-note)
(require 'vulpea-schema)
(require 'vulpea-buffer)
(require 'vulpea-db-extract)

(defun vulpea-schema-flymake--note-region (level pos)
  "Return (BEG . END) of the header line of a note at LEVEL and POS.
For a heading-level note (LEVEL above zero) this is the heading line at
POS; for a file-level note it is the =#+title= line, or the first line
when there is none."
  (save-excursion
    (if (and level (> level 0) pos)
        (goto-char pos)
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+title:" nil t)
        (goto-char (point-min))))
    (cons (line-beginning-position) (line-end-position))))

(defun vulpea-schema-flymake--field-region (field bound)
  "Return (BEG . END) of FIELD's metadata line within BOUND, or nil.
BOUND scopes the lookup to a heading subtree (see `vulpea-buffer-meta')
and is nil for file-level metadata."
  (when-let* ((meta (vulpea-buffer-meta--get (vulpea-buffer-meta bound) field))
              (item (car (plist-get meta :items)))
              (beg (org-element-property :begin item)))
    (cons beg (save-excursion (goto-char beg) (line-end-position)))))

(defun vulpea-schema-flymake--violation-region (violation data level)
  "Return (BEG . END) in the current buffer for VIOLATION.
DATA is the extraction plist of the offending note and LEVEL its level.
A value violation points at its field's line; a missing required field
\(which has no line) points at the note's header line."
  (let ((field (vulpea-violation-field violation))
        (bound (when (and level (> level 0)) (plist-get data :pos))))
    (or (and field
             (not (eq (vulpea-violation-type violation) 'missing-required))
             (vulpea-schema-flymake--field-region field bound))
        (vulpea-schema-flymake--note-region level (plist-get data :pos)))))

(defun vulpea-schema-flymake--collect ()
  "Return schema-violation diagnostics for the notes in the current buffer.
Each element is (BEG END TYPE TEXT), ready for `flymake-make-diagnostic'."
  (let* ((ast (org-element-parse-buffer))
         (path (or (buffer-file-name) (buffer-name)))
         (file-title (vulpea-db--extract-file-title ast path))
         (datas (cons (vulpea-db--extract-file-node ast path (current-buffer) file-title)
                      (vulpea-db--extract-heading-nodes ast path (current-buffer) file-title)))
         (diagnostics nil))
    (dolist (data datas)
      (when (plist-get data :id)
        (let* ((level (or (plist-get data :level) 0))
               (note (vulpea-db--note-from-data data path level)))
          (dolist (v (vulpea-schema-note-violations note))
            (let ((region (vulpea-schema-flymake--violation-region v data level)))
              (push (list (car region) (cdr region) :warning
                          (format "[%s] %s"
                                  (vulpea-violation-schema v)
                                  (vulpea-violation-message v)))
                    diagnostics))))))
    (nreverse diagnostics)))

(defun vulpea-schema-flymake (report-fn &rest _)
  "Report schema violations in the current buffer to REPORT-FN.
A flymake backend; see `flymake-diagnostic-functions'."
  (funcall report-fn
           (mapcar (lambda (d)
                     (apply #'flymake-make-diagnostic (current-buffer) d))
                   (vulpea-schema-flymake--collect))))

;;;###autoload
(define-minor-mode vulpea-schema-flymake-mode
  "Toggle live schema-violation linting in the current buffer.
When enabled, register `vulpea-schema-flymake' as a flymake backend and
turn on `flymake-mode', so violations of any schema applicable to a note
in the buffer are flagged as you edit."
  :lighter " vulpea-schema"
  (if vulpea-schema-flymake-mode
      (progn
        (add-hook 'flymake-diagnostic-functions #'vulpea-schema-flymake nil t)
        (flymake-mode 1))
    (remove-hook 'flymake-diagnostic-functions #'vulpea-schema-flymake t)))

(provide 'vulpea-schema-flymake)
;;; vulpea-schema-flymake.el ends here
