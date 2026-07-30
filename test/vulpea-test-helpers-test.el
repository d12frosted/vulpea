;;; vulpea-test-helpers-test.el --- Tests for test helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 30 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The test harness must never write into the notes directory of whoever
;; runs the suite.  Any test reaching `vulpea-create' - directly, or
;; through `vulpea-insert' with no candidates - creates a real file in
;; `vulpea--default-directory', so the helpers have to point that
;; somewhere temporary.
;;
;;; Code:

(require 'ert)
(require 'vulpea)
(require 'vulpea-test-helpers)

(defmacro vulpea-test-helpers-test--with-sentinel-directory (&rest body)
  "Execute BODY with the notes directory settings pointed at a sentinel.

Binds `org-directory', `vulpea-default-notes-directory' and
`vulpea-db-sync-directories' to a temporary directory available to BODY
as the anaphoric variable `sentinel', standing in for the directory of
whoever runs the suite.  Nothing is expected to be created there."
  (declare (indent 0))
  `(let* ((sentinel (file-name-as-directory
                     (make-temp-file "vulpea-sentinel-" t)))
          (org-directory sentinel)
          (vulpea-default-notes-directory nil)
          (vulpea-db-sync-directories nil))
     (ignore sentinel)
     (unwind-protect
         (progn ,@body)
       (delete-directory sentinel t))))

(ert-deftest vulpea-test-helpers-temp-db-keeps-notes-out-of-org-directory ()
  "Notes created within `vulpea-test--with-temp-db' land in a temp directory."
  (vulpea-test-helpers-test--with-sentinel-directory
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((note (vulpea-create "Frodo" nil)))
        (should note)
        (should-not (file-in-directory-p (vulpea-note-path note) sentinel))))
    (should-not (directory-files sentinel nil "\\.org\\'"))))

(ert-deftest vulpea-test-helpers-temp-db-and-file-keeps-notes-out-of-org-directory ()
  "Notes created within `vulpea-test--with-temp-db-and-file' stay temporary."
  (vulpea-test-helpers-test--with-sentinel-directory
    (vulpea-test--with-temp-db-and-file "some-id" "#+title: Some note\n"
      (let ((note (vulpea-create "Frodo" nil)))
        (should note)
        (should-not (file-in-directory-p (vulpea-note-path note) sentinel))))
    (should-not (directory-files sentinel nil "\\.org\\'"))))

(ert-deftest vulpea-test-helpers-temp-db-cleans-up-created-notes ()
  "The notes directory of `vulpea-test--with-temp-db' is removed afterwards."
  (vulpea-test-helpers-test--with-sentinel-directory
    (let (path)
      (vulpea-test--with-temp-db
        (vulpea-db)
        (setq path (vulpea-note-path (vulpea-create "Frodo" nil)))
        (should (file-exists-p path)))
      (should-not (file-exists-p path)))))

(provide 'vulpea-test-helpers-test)
;;; vulpea-test-helpers-test.el ends here
