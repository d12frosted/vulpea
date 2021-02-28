;;; vulpea-test-utils.el --- Helpers for testing Vulpea -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 09 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'buttercup)
(require 'org)
(require 'org-roam)
(require 'vulpea-db)

(defvar vulpea-test-directory (expand-file-name "test/note-files")
  "Directory containing test notes.")

(defun vulpea-test--abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (expand-file-name file-path org-roam-directory))

(defun vulpea-test--map-file (fn file)
  "Execute FN with buffer visiting FILE."
  (let* ((fname (vulpea-test--abs-path file))
         (buf (find-file-noselect fname)))
    (with-current-buffer buf
      (funcall fn fname))))

(defun vulpea-test--init ()
  "Initialize testing environment."
  (let ((original-dir vulpea-test-directory)
        (new-dir (expand-file-name (make-temp-name "note-files") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-mode +1)
    (sleep-for 2)))

(defun vulpea-test--teardown ()
  "Teardown testing environment."
  (org-roam-mode -1)
  (delete-file org-roam-db-location)
  (org-roam-db--close))

(defun vulpea-test-meta (file)
  "Return `vulpea-note-meta' for FILE.

If the FILE is relative, it is considered to be relative to
`org-roam-directory'."
  (let* ((attr (file-attributes
                (if (file-name-absolute-p file)
                    file
                  (expand-file-name file org-roam-directory))))
         (atime (file-attribute-access-time attr))
         (mtime (file-attribute-modification-time attr)))
    (make-vulpea-note-meta
     :atime atime
     :mtime mtime)))

(buttercup-define-matcher :to-contain-exactly (file value)
  (cl-destructuring-bind
      ((file-expr . file) (value-expr . value))
      (mapcar #'buttercup--expr-and-value (list file value))
    (let* ((content (vulpea-test--map-file
                     (lambda (_)
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))
                     file))
           (spec (format-spec-make
                  ?F (format "%S" file-expr)
                  ?f (format "%S" file)
                  ?V (format "%S" value-expr)
                  ?v (format "%S" value)
                  ?c (format "%S" content))))
      (if (string-equal content value)
          (cons t (buttercup-format-spec
                   "Expected `%F' not to have content equal to `%v'"
                   spec))
        (cons nil (buttercup-format-spec
                   "Expected `%F' to have content equal to `%v', but instead `%F' has content equal to `%c'"
                   spec))))))

(provide 'vulpea-test-utils)
;;; vulpea-test-utils.el ends here
