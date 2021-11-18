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
(require 'vulpea)

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

(defun vulpea-test--init (&optional no-setup)
  "Initialize testing environment.

Unless NO-SETUP is non-nil, setup vulpea db."
  (let ((original-dir vulpea-test-directory)
        (new-dir (expand-file-name (make-temp-name "note-files") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir
          org-roam-db-location (expand-file-name "org-roam.db" new-dir))
    (unless no-setup
      (vulpea-db-autosync-enable))
    (org-roam-db-autosync-enable)))

(defun vulpea-test--teardown ()
  "Teardown testing environment."
  (vulpea-db-autosync-disable)
  (org-roam-db-autosync-disable)
  (setq org-roam-db--connection (make-hash-table :test #'equal)))

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

(cl-defun completion-for (&key title tags)
  "Return completion for TITLE and TAGS matchers."
  (when-let ((note
              (seq-find
                (lambda (note)
                  (let ((res (and (or (null title) (string-equal title (vulpea-note-title note)))
                                  (or (null tags)
                                      (seq-every-p
                                       (lambda (x)
                                         (seq-contains-p (vulpea-note-tags note) x))
                                       tags)))))
                    res))
                (vulpea-db-query))))
    (vulpea-select-describe note)))

(defun global-filter-fn (x)
  "Just some dummy global filter for X."
  x)

(defun local-filter-fn (x)
  "Just some dummy local filter for X."
  x)

(defun global-candidates-fn (filter)
  "Just some dummy global candidates source for FILTER."
  filter)

(defun local-candidates-fn (filter)
  "Just some dummy global candidates source for FILTER."
  filter)

(defun insert-handle-fn (x)
  "Just some dummy insertion handler for X."
  x)

(provide 'vulpea-test-utils)
;;; vulpea-test-utils.el ends here
