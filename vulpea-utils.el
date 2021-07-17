;;; vulpea-utils.el --- Vulpea utilities   -*- lexical-binding: t; -*-
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

(require 'org)
(require 'vulpea-note)

(defvar vulpea-utils--uuid-regexp
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

(defmacro vulpea-utils-with-file (file &rest body)
  "Execute BODY in `org-mode' FILE.

In most cases you should use `vulpea-utils-with-note', because
that macro properly handles notes with level greater than 0."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

(defmacro vulpea-utils-with-note (note &rest body)
  "Execute BODY in with buffer visiting NOTE.

If note level is equal to 0, then the point is placed at the
beginning of the buffer. Otherwise at the heading with note id."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect (vulpea-note-path ,note))
     (when (> (vulpea-note-level ,note) 0)
       (goto-char (org-find-entry-with-id (vulpea-note-id ,note))))
     ,@body))

(defun vulpea-utils-link-make-string (note)
  "Make a bracket link to NOTE."
  (org-link-make-string
   (concat "id:" (vulpea-note-id note))
   (vulpea-note-title note)))

(defun vulpea-utils-note-hash (note)
  "Compute the hash of NOTE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (vulpea-note-path note))
    (secure-hash 'sha1 (current-buffer))))

(defun vulpea-utils-collect-while (fn filter &rest args)
  "Repeat FN and collect it's results until `C-g` is used.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

If FILTER is nil, it does not affect repeat cycle.

If FILTER returns nil, the computed value is not added to result.

ARGS are passed to FN."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (if (and filter
                 (null (funcall filter value)))
            (setq continue nil)
          (setq result (cons value result)))))
    (setq quit-flag nil)
    (seq-reverse result)))

(defun vulpea-utils-repeat-while (fn filter &rest args)
  "Repeat FN and return the first unfiltered result.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

ARGS are passed to FN."
  (let (value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (when (null (funcall filter value))
          (setq continue nil))))
    (setq quit-flag nil)
    (when (null continue)
      value)))

(provide 'vulpea-utils)
;;; vulpea-utils.el ends here
