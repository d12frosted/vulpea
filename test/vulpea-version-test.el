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

(ert-deftest vulpea-version-git-resolves-symlinked-library ()
  "Git version detection must follow symlinks to the checkout.
Package managers like elpaca load libraries from a build directory
of symlinks into the git checkout; locating .git from the symlink
path fails, from the truename it succeeds."
  (skip-unless (executable-find "git"))
  (let* ((build-dir (make-temp-file "vulpea-build" t))
         (real (vulpea-version-test--source-file))
         (link (expand-file-name "vulpea.el" build-dir)))
    (unwind-protect
        (progn
          (make-symbolic-link real link)
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _) link)))
            ;; The real file lives in this git checkout, so following
            ;; the symlink must yield a git-described version
            (should (vulpea-version--git))))
      (delete-directory build-dir t))))

(defun vulpea-version-test--fake-config-repo ()
  "Create a fake versioned Emacs config holding an elpaca-style build.
The result is a temporary directory laid out the way elpaca lays
out builds inside a user's ~/.emacs.d: a git repository with one
commit, containing a plain copy of vulpea.el under
elpaca/builds/vulpea. The caller must delete the directory."
  (let* ((config-dir (make-temp-file "vulpea-fake-config" t))
         (build-dir (expand-file-name "elpaca/builds/vulpea" config-dir))
         (checkout (file-name-directory (vulpea-version-test--source-file))))
    (make-directory build-dir t)
    (copy-file (expand-file-name "vulpea.el" checkout)
               (expand-file-name "vulpea.el" build-dir))
    (let ((default-directory config-dir))
      (call-process "git" nil nil nil "init")
      (call-process "git" nil nil nil
                    "-c" "user.name=Test"
                    "-c" "user.email=test@example.com"
                    "-c" "commit.gpgsign=false"
                    "commit" "--allow-empty" "-m" "init"))
    config-dir))

(ert-deftest vulpea-version-git-rejects-foreign-repo-above-build ()
  "A foreign repo above the loaded build must not supply the version.
Elpaca's build directory is a plain copy inside ~/.emacs.d; when
the config is itself a git repository, walking up from the loaded
library reaches the config's .git. Describing that repository
yields a commit that does not exist in vulpea (vulpea#427), so it
must be rejected rather than reported."
  (skip-unless (executable-find "git"))
  (let* ((config-dir (vulpea-version-test--fake-config-repo))
         (build-dir (expand-file-name "elpaca/builds/vulpea" config-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-library)
                   (lambda (&rest _)
                     (expand-file-name "vulpea.el" build-dir))))
          ;; With no elpaca or straight to ask, the only .git up the
          ;; tree belongs to the fake config: better no version than
          ;; a foreign one
          (should (null (vulpea-version--git))))
      (delete-directory config-dir t))))

(ert-deftest vulpea-version-git-foreign-repo-defers-to-elpaca-source ()
  "The elpaca source dir must win over a foreign repo above the build.
Finding some .git above the loaded library is not the end of the
search: when that repository is not a vulpea checkout, the source
directory elpaca knows about is the one that must answer."
  (skip-unless (and (executable-find "git")
                    (locate-dominating-file
                     (vulpea-version-test--source-file) ".git")))
  (let* ((expected (vulpea-version--git))
         (config-dir (vulpea-version-test--fake-config-repo))
         (build-dir (expand-file-name "elpaca/builds/vulpea" config-dir))
         (checkout (file-name-directory (vulpea-version-test--source-file))))
    (unwind-protect
        (progn
          (should expected)
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "vulpea.el" build-dir)))
                    ((symbol-function 'elpaca-get)
                     (lambda (_) 'fake-record))
                    ((symbol-function 'elpaca-source-dir)
                     (lambda (_) checkout)))
            (should (equal (vulpea-version--git) expected))))
      (delete-directory config-dir t))))

(ert-deftest vulpea-version-git-found-through-elpaca-source-dir ()
  "Git detection asks elpaca for the checkout when the build is a copy.
Elpaca (without symlinks) copies files into its build directory, so
neither the library path nor its truename leads to .git; the source
directory from elpaca's API does."
  (skip-unless (executable-find "git"))
  (let ((copy-dir (make-temp-file "vulpea-copy-build" t))
        ;; Resolve the checkout BEFORE mocking locate-library: the
        ;; helper resolves through it too
        (checkout (file-name-directory (vulpea-version-test--source-file))))
    (unwind-protect
        (progn
          ;; A plain copy: truename reveals nothing
          (copy-file (expand-file-name "vulpea.el" checkout)
                     (expand-file-name "vulpea.el" copy-dir))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "vulpea.el" copy-dir)))
                    ((symbol-function 'elpaca-get)
                     (lambda (_) 'fake-record))
                    ((symbol-function 'elpaca-source-dir)
                     (lambda (_) checkout)))
            (should (vulpea-version--git))))
      (delete-directory copy-dir t))))

(provide 'vulpea-version-test)
;;; vulpea-version-test.el ends here
