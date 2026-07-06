;;; vulpea-doctor-test.el --- Tests for vulpea-doctor -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 12 Jun 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for `vulpea-doctor' - the setup diagnostics command.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vulpea)
(require 'vulpea-test-helpers)

(defmacro vulpea-doctor-test--with-tools (tools &rest body)
  "Execute BODY with `executable-find' mocked against TOOLS.

TOOLS is an alist of (NAME . PATH); lookups of names not in the
alist return nil."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'executable-find)
              (lambda (name &rest _) (cdr (assoc name ,tools)))))
     ,@body))

;;; Report

(ert-deftest vulpea-doctor-returns-report-string ()
  "Doctor returns a non-empty report including the vulpea version."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((report (vulpea-doctor)))
      (should (stringp report))
      (should (string-match-p "Vulpea Doctor" report))
      (should (string-match-p (regexp-quote (vulpea-version)) report)))))

(ert-deftest vulpea-doctor-reports-database-state ()
  "Report includes the database location and note count."
  (vulpea-test--with-temp-db-and-file "doctor-test-id" "#+title: Doctor\n"
    (let ((report (vulpea-doctor)))
      (should (string-match-p (regexp-quote vulpea-db-location) report))
      (should (string-match-p "notes +1\\b" report)))))

(ert-deftest vulpea-doctor-reports-missing-database ()
  "When the database file does not exist, report says so without
creating it as a side effect."
  (let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
         (vulpea-db-location temp-file)
         (vulpea-db--connection nil))
    (delete-file temp-file)
    (let ((report (vulpea-doctor)))
      (should (string-match-p "missing" report))
      (should-not (file-exists-p temp-file)))))

(ert-deftest vulpea-doctor-show-displays-buffer ()
  "With SHOW non-nil, the report is rendered in *vulpea-doctor*."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (when (get-buffer "*vulpea-doctor*")
      (kill-buffer "*vulpea-doctor*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (vulpea-doctor t))
          (with-current-buffer "*vulpea-doctor*"
            (should (string-match-p "Vulpea Doctor" (buffer-string)))))
      (when (get-buffer "*vulpea-doctor*")
        (kill-buffer "*vulpea-doctor*")))))

(ert-deftest vulpea-doctor-reports-external-tools ()
  "Report lists fd, fswatch, rg, and git under External Tools with paths."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd")
                                    ("fswatch" . "/usr/bin/fswatch")
                                    ("rg" . "/usr/bin/rg")
                                    ("git" . "/usr/bin/git"))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((report (vulpea-doctor)))
        (should (string-match-p "fd +/usr/bin/fd" report))
        (should (string-match-p "fswatch +/usr/bin/fswatch" report))
        (should (string-match-p "rg +/usr/bin/rg" report))
        (should (string-match-p "git +/usr/bin/git" report))))))

(ert-deftest vulpea-doctor-reports-ripgrep-missing ()
  "Report shows ripgrep as not found when it is absent from PATH."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd"))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (should (string-match-p "rg +not found" (vulpea-doctor))))))

;;; Issue Detection

(ert-deftest vulpea-doctor-issue-fswatch-missing-with-auto ()
  "Method `auto' without fswatch on PATH yields a polling-fallback warning."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd"))
    (let* ((vulpea-db-sync-external-method 'auto)
           (issues (vulpea-doctor--issues)))
      (should (seq-some (lambda (i) (string-match-p "fswatch" i)) issues)))))

(ert-deftest vulpea-doctor-issue-fswatch-missing-with-explicit ()
  "Method `fswatch' without fswatch on PATH yields an issue."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd"))
    (let* ((vulpea-db-sync-external-method 'fswatch)
           (issues (vulpea-doctor--issues)))
      (should (seq-some (lambda (i) (string-match-p "fswatch" i)) issues)))))

(ert-deftest vulpea-doctor-no-fswatch-issue-with-poll ()
  "Method `poll' does not require fswatch, so no fswatch issue."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd"))
    (let* ((vulpea-db-sync-external-method 'poll)
           (issues (vulpea-doctor--issues)))
      (should-not (seq-some (lambda (i) (string-match-p "fswatch" i)) issues)))))

(ert-deftest vulpea-doctor-issue-fd-missing ()
  "Missing fd yields a performance warning."
  (vulpea-doctor-test--with-tools '(("fswatch" . "/usr/bin/fswatch"))
    (let ((issues (vulpea-doctor--issues)))
      (should (seq-some (lambda (i) (string-match-p "\\bfd\\b" i)) issues)))))

(ert-deftest vulpea-doctor-issue-missing-directory ()
  "A non-existent sync directory yields an issue naming it."
  (let* ((vulpea-db-sync-directories '("/nonexistent/vulpea-doctor-test/"))
         (issues (vulpea-doctor--issues)))
    (should (seq-some
             (lambda (i)
               (string-match-p "/nonexistent/vulpea-doctor-test/" i))
             issues))))

(ert-deftest vulpea-doctor-issue-empty-database ()
  "An existing but empty database suggests a full scan."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((issues (vulpea-doctor--issues)))
      (should (seq-some
               (lambda (i) (string-match-p "vulpea-db-sync-full-scan" i))
               issues)))))

(ert-deftest vulpea-doctor-issue-autosync-disabled ()
  "Disabled autosync yields an issue."
  (let* ((vulpea-db-autosync-mode nil)
         (issues (vulpea-doctor--issues)))
    (should (seq-some
             (lambda (i) (string-match-p "autosync" i))
             issues))))

(ert-deftest vulpea-doctor-no-issues-when-healthy ()
  "A healthy setup reports no issues."
  (vulpea-doctor-test--with-tools '(("fd" . "/usr/bin/fd")
                                    ("fswatch" . "/usr/bin/fswatch")
                                    ("git" . "/usr/bin/git"))
    (vulpea-test--with-temp-db-and-file "doctor-healthy-id" "#+title: Doctor\n"
      (let* ((vulpea-db-sync-directories (list temporary-file-directory))
             (vulpea-db-autosync-mode t)
             (vulpea-db-sync-external-method 'poll)
             (vulpea-db-sync--poll-timer t)
             (issues (vulpea-doctor--issues)))
        (should (null issues))
        (should (string-match-p "No issues detected" (vulpea-doctor)))))))

;;; Cached File Diagnostics
;; https://github.com/d12frosted/vulpea/issues/277

(ert-deftest vulpea-doctor-cached-file-stats-counts-note-less ()
  "Stats report total cached files and how many produced no note."
  (vulpea-test--with-temp-db
    (let ((db (vulpea-db)))
      (vulpea-test--insert-test-note "n1" "Note 1" :path "/tmp/a.org")
      (emacsql db [:insert :into files :values $v1]
               (list (vector "/tmp/a.org" "h" "t" 1)))
      (emacsql db [:insert :into files :values $v1]
               (list (vector "/tmp/orphan.org" "h" "t" 1))))
    (should (equal (vulpea-doctor--cached-file-stats) '(2 . 1)))))

(ert-deftest vulpea-doctor-cached-file-stats-nil-without-db ()
  "Stats are nil (no side effect) when the database file is absent."
  (let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
         (vulpea-db-location temp-file)
         (vulpea-db--connection nil))
    (delete-file temp-file)
    (should (null (vulpea-doctor--cached-file-stats)))
    (should-not (file-exists-p temp-file))))

(ert-deftest vulpea-doctor-reports-cached-files ()
  "Report includes cached file counts in the Database section."
  (vulpea-test--with-temp-db
    (let ((db (vulpea-db)))
      (vulpea-test--insert-test-note "n1" "Note 1" :path "/tmp/a.org")
      (emacsql db [:insert :into files :values $v1]
               (list (vector "/tmp/a.org" "h" "t" 1)))
      (emacsql db [:insert :into files :values $v1]
               (list (vector "/tmp/orphan.org" "h" "t" 1))))
    (let ((report (vulpea-doctor)))
      (should (string-match-p "cached files +2\\b" report))
      (should (string-match-p "files without notes +1\\b" report)))))

(ert-deftest vulpea-doctor-flags-async-disabled-by-extractors ()
  "Doctor must expose async extraction being silently bypassed.
The trap: async is enabled, but a registered AST-reading extractor
makes every file take the synchronous path with no visible sign."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-async-extraction t)
          (vulpea-db--extractors
           (list (make-vulpea-extractor :name 'legacy :extract-fn #'ignore))))
      (let ((report (vulpea-doctor)))
        (should (string-match-p "will NOT use the worker" report))
        (should (string-match-p ":requires-ast nil" report))))))

(ert-deftest vulpea-doctor-reports-async-state ()
  "The report carries an async extraction section."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-async-extraction 'full)
          (vulpea-db--extractors nil)
          (vulpea-db-note-index-filter-functions nil))
      (let ((report (vulpea-doctor)))
        (should (string-match-p "Async Extraction" report))
        (should (string-match-p "mode.*full" report))
        (should (string-match-p "handles .org files.*yes" report))))))

(ert-deftest vulpea-doctor-no-async-issues-when-disabled ()
  "With async off, no async issues appear no matter the extractors."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db-async-extraction nil)
          (vulpea-db--extractors
           (list (make-vulpea-extractor :name 'legacy :extract-fn #'ignore))))
      (should-not (string-match-p "will NOT use the worker"
                                  (vulpea-doctor))))))

(provide 'vulpea-doctor-test)
;;; vulpea-doctor-test.el ends here
