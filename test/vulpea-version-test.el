;;; vulpea-version-test.el --- Tests for vulpea-version -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 10 Jun 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for `vulpea-version' (constant and function).
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-mnt)
(require 'vulpea)

(defun vulpea-version-test--source-file ()
  "Return path to vulpea.el source file."
  (concat (file-name-sans-extension (locate-library "vulpea")) ".el"))

(ert-deftest vulpea-version-constant-matches-header ()
  "The `vulpea-version' constant must match the Version header."
  (should (equal vulpea-version
                 (lm-version (vulpea-version-test--source-file)))))

(ert-deftest vulpea-version-returns-non-empty-string ()
  "Function `vulpea-version' returns a non-empty string."
  (let ((version (vulpea-version)))
    (should (stringp version))
    (should (> (length version) 0))))

(ert-deftest vulpea-version-git-describe ()
  "From a git checkout, version comes from \"git describe\"."
  (skip-unless (and (executable-find "git")
                    (locate-dominating-file
                     (vulpea-version-test--source-file) ".git")))
  (let ((git-version (vulpea-version--git)))
    (should (stringp git-version))
    (should (> (length git-version) 0))
    ;; Git information takes precedence over everything else.
    (should (equal (vulpea-version) git-version))))

(ert-deftest vulpea-version-fallback-to-constant ()
  "Without git checkout and package install, fall back to the constant."
  (cl-letf (((symbol-function 'vulpea-version--git) #'ignore)
            ((symbol-function 'vulpea-version--package) #'ignore))
    (should (equal (vulpea-version) vulpea-version))))

(ert-deftest vulpea-version-package-nil-when-not-installed ()
  "Package version resolution returns nil when vulpea is not in
`package-alist' (e.g. running from a checkout)."
  (let ((package-alist nil))
    (should (null (vulpea-version--package)))))

(ert-deftest vulpea-version-show-messages ()
  "With SHOW non-nil, version is displayed in the echo area."
  (let (captured)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (vulpea-version t))
    (should (stringp captured))
    (should (string-prefix-p "vulpea " captured))))

(ert-deftest vulpea-version-no-message-without-show ()
  "Without SHOW, nothing is displayed in the echo area."
  (let (captured)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (vulpea-version))
    (should (null captured))))

(provide 'vulpea-version-test)
;;; vulpea-version-test.el ends here
