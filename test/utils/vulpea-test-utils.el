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

(provide 'vulpea-test-utils)
;;; vulpea-test-utils.el ends here
