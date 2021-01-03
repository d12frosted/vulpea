;;; vulpea-test.el --- Helpers for testing Vulpea -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1") (buttercup "1.23"))
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
;; Helpers for testing `vulpea'.
;;
;;; Code:

;; install the latest version of org-mode
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-refresh-contents)
(package-install 'org)
(package-install 'org-plus-contrib)
(package-initialize)

(require 'buttercup)
(require 'org)
(require 'org-roam)
(require 'vulpea-db)

(defvar test-vulpea-directory (expand-file-name "test/note-files")
  "Directory containing test notes.")

(defun test-vulpea--abs-path (file-path)
  "Get absolute FILE-PATH from `org-roam-directory'."
  (expand-file-name file-path org-roam-directory))

(defun test-vulpea--map-file (fn file)
  "Execute FN with buffer visiting FILE."
  (let* ((fname (test-vulpea--abs-path file))
         (buf (find-file-noselect fname)))
    (with-current-buffer buf
      (funcall fn fname))))

(defun test-vulpea--init ()
  "Initialize testing environment."
  (let ((original-dir test-vulpea-directory)
        (new-dir (expand-file-name (make-temp-name "note-files") temporary-file-directory)))
    (copy-directory original-dir new-dir)
    (setq org-roam-directory new-dir)
    (org-roam-mode +1)
    (sleep-for 2)))

(defun test-vulpea--teardown ()
  "Teardown testing environment."
  (org-roam-mode -1)
  (delete-file org-roam-db-location)
  (org-roam-db--close))

(buttercup-define-matcher-for-binary-function
    :to-be-version< version<
  :expect-match-phrase "Expected `%A' < %b, but `%A' was %a."
  :expect-mismatch-phrase "Expected `%A' >= %b, but `%A' was %a.")

(provide 'vulpea-test)
;;; vulpea-test.el ends here
