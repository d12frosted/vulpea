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

(ert-deftest vulpea-doctor-issue-duplicate-id-claims ()
  "A pending id claim (duplicate :ID: across files) is reported."
  (vulpea-test--with-temp-notes-dir
    (let ((a (expand-file-name "a.org" root))
          (b (expand-file-name "b.org" root)))
      (with-temp-file a
        (insert ":PROPERTIES:\n:ID: dup-id\n:END:\n#+TITLE: A\n"))
      (with-temp-file b
        (insert ":PROPERTIES:\n:ID: dup-id\n:END:\n#+TITLE: B\n"))
      (vulpea-db-update-file a)
      (vulpea-db-update-file b)
      (let ((issues (vulpea-doctor--issues)))
        (should (seq-some (lambda (i) (string-match-p "dup-id" i))
                          issues))))))

(ert-deftest vulpea-doctor-no-duplicate-id-issue-without-claims ()
  "No duplicate-id issue when every id lives in one file."
  (vulpea-test--with-temp-notes-dir
    (let ((a (expand-file-name "a.org" root)))
      (with-temp-file a
        (insert ":PROPERTIES:\n:ID: unique-id\n:END:\n#+TITLE: A\n"))
      (vulpea-db-update-file a)
      (let ((issues (vulpea-doctor--issues)))
        (should-not (seq-some
                     (lambda (i) (string-match-p "Duplicate note id" i))
                     issues))))))

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

;;; Watcher/Config Divergence
;; https://github.com/d12frosted/vulpea/issues/427
;;
;; `vulpea-db-autosync-mode' reads `vulpea-db-sync-directories' when it
;; starts its watchers; a later setq never reaches the running fswatch
;; process or the filenotify watch list, while manual sync commands read
;; the current value. The doctor must make that split visible.

(defmacro vulpea-doctor-test--with-temp-dirs (names &rest body)
  "Bind each symbol in NAMES to a fresh temporary directory around BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (name)
                   `(,name (make-temp-file "vulpea-doctor-dir" t)))
                 names)
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (name)
                   `(ignore-errors (delete-directory ,name t)))
                 names))))

(defmacro vulpea-doctor-test--with-fake-fswatch (dirs &rest body)
  "Execute BODY with a fake live fswatch process watching DIRS.

The fake process reproduces the argv shape of
`vulpea-db-sync--setup-fswatch': the watched directories are the
trailing arguments after the --format flag and its value."
  (declare (indent 1))
  `(let ((vulpea-db-sync--fswatch-process 'vulpea-doctor-test--fake-proc))
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (p) (eq p 'vulpea-doctor-test--fake-proc)))
               ((symbol-function 'process-command)
                (lambda (_)
                  (append '("fswatch"
                            "--recursive"
                            "--event=Updated"
                            "--exclude" "\\.#.*$"
                            "--format" "%p|||%f")
                          ,dirs))))
       ,@body)))

(defun vulpea-doctor-test--divergence-issues (issues)
  "Return the watcher/config divergence entries of ISSUES."
  (seq-filter (lambda (i) (string-match-p "started with a different" i))
              issues))

(ert-deftest vulpea-doctor-issue-fswatch-watching-old-directories ()
  "A directory added to the config after fswatch started is flagged."
  (vulpea-doctor-test--with-temp-dirs (dir-a dir-b)
    (vulpea-doctor-test--with-fake-fswatch (list dir-a)
      (let* ((vulpea-db-sync-directories (list dir-a dir-b))
             (found (vulpea-doctor-test--divergence-issues
                     (vulpea-doctor--issues))))
        (should found)
        (should (seq-some
                 (lambda (i)
                   (and (string-match-p "not watched" i)
                        (string-match-p (regexp-quote dir-b) i)
                        (string-match-p "vulpea-db-autosync-mode" i)))
                 found))))))

(ert-deftest vulpea-doctor-issue-fswatch-watching-removed-directory ()
  "A directory removed from the config but still watched is flagged."
  (vulpea-doctor-test--with-temp-dirs (dir-a dir-b)
    (vulpea-doctor-test--with-fake-fswatch (list dir-a dir-b)
      (let* ((vulpea-db-sync-directories (list dir-a))
             (found (vulpea-doctor-test--divergence-issues
                     (vulpea-doctor--issues))))
        (should (seq-some
                 (lambda (i)
                   (and (string-match-p "no longer configured" i)
                        (string-match-p (regexp-quote dir-b) i)))
                 found))))))

(ert-deftest vulpea-doctor-no-fswatch-issue-when-directories-match ()
  "Matching directories yield no issue, whatever their spelling.

The config keeps a trailing slash while the process argv holds the
expanded form; that is the same directory, not a divergence."
  (vulpea-doctor-test--with-temp-dirs (dir-a)
    (vulpea-doctor-test--with-fake-fswatch (list (expand-file-name dir-a))
      (let* ((vulpea-db-sync-directories (list (file-name-as-directory dir-a)))
             (found (vulpea-doctor-test--divergence-issues
                     (vulpea-doctor--issues))))
        (should (null found))))))

(ert-deftest vulpea-doctor-no-fswatch-issue-for-nonexistent-config-dir ()
  "A configured directory that does not exist is not a divergence.

fswatch skips non-existent directories on startup, so restarting
would not watch it either; the missing directory has its own issue."
  (vulpea-doctor-test--with-temp-dirs (dir-a)
    (vulpea-doctor-test--with-fake-fswatch (list dir-a)
      (let* ((vulpea-db-sync-directories
              (list dir-a "/nonexistent/vulpea-doctor-divergence/"))
             (found (vulpea-doctor-test--divergence-issues
                     (vulpea-doctor--issues))))
        (should (null found))))))

(ert-deftest vulpea-doctor-issue-filenotify-missing-root ()
  "Without fswatch, a configured root absent from the watch list is flagged."
  (vulpea-doctor-test--with-temp-dirs (dir-a dir-b)
    (let* ((vulpea-db-sync--fswatch-process nil)
           (vulpea-db-sync--watchers (list (cons dir-a 'fake)))
           (vulpea-db-sync-directories (list dir-a dir-b))
           (found (vulpea-doctor-test--divergence-issues
                   (vulpea-doctor--issues))))
      (should (seq-some
               (lambda (i)
                 (and (string-match-p "not watched" i)
                      (string-match-p (regexp-quote dir-b) i)
                      (string-match-p "vulpea-db-autosync-mode" i)))
               found)))))

(ert-deftest vulpea-doctor-no-filenotify-issue-for-subdirectory-watchers ()
  "Watchers on subdirectories of a configured root are not stale."
  (vulpea-doctor-test--with-temp-dirs (dir-a)
    (let* ((vulpea-db-sync--fswatch-process nil)
           (vulpea-db-sync--watchers
            (list (cons dir-a 'fake)
                  (cons (expand-file-name "sub" dir-a) 'fake)))
           (vulpea-db-sync-directories (list dir-a))
           (found (vulpea-doctor-test--divergence-issues
                   (vulpea-doctor--issues))))
      (should (null found)))))

(ert-deftest vulpea-doctor-issue-filenotify-stale-root ()
  "A watched root no longer configured is flagged, its subdirs are not.

Only the root is worth naming; listing every watched subdirectory
of a removed root would drown the message."
  (vulpea-doctor-test--with-temp-dirs (dir-a dir-b)
    (let* ((vulpea-db-sync--fswatch-process nil)
           (sub (expand-file-name "sub" dir-b))
           (vulpea-db-sync--watchers
            (list (cons dir-a 'fake)
                  (cons dir-b 'fake)
                  (cons sub 'fake)))
           (vulpea-db-sync-directories (list dir-a))
           (found (vulpea-doctor-test--divergence-issues
                   (vulpea-doctor--issues))))
      (should (seq-some
               (lambda (i)
                 (and (string-match-p "no longer configured" i)
                      (string-match-p (regexp-quote dir-b) i)
                      (not (string-match-p (regexp-quote sub) i))))
               found)))))

(ert-deftest vulpea-doctor-no-divergence-issue-when-nothing-watched ()
  "No fswatch process and no watchers means no divergence to report."
  (vulpea-doctor-test--with-temp-dirs (dir-a)
    (let* ((vulpea-db-sync--fswatch-process nil)
           (vulpea-db-sync--watchers nil)
           (vulpea-db-sync-directories (list dir-a))
           (found (vulpea-doctor-test--divergence-issues
                   (vulpea-doctor--issues))))
      (should (null found)))))

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
           (list (make-vulpea-extractor :name 'reader :requires-ast t
                                        :extract-fn #'ignore))))
      (let ((report (vulpea-doctor)))
        (should (string-match-p "will NOT use the worker" report))
        (should (string-match-p ":requires-ast t" report))))))

(ert-deftest vulpea-doctor-nudges-undeclared-requires-ast ()
  "An extractor without an explicit :requires-ast declaration is flagged.
Since the default flipped to fast-by-default (nil AST), authors should
declare their intent; the doctor names the extractor and both options."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((vulpea-db--extractors
            (list (make-vulpea-extractor :name 'legacy
                                         :extract-fn #'ignore)))
           (issues (vulpea-doctor--issues)))
      (should (seq-some
               (lambda (i)
                 (and (string-match-p "legacy" i)
                      (string-match-p ":requires-ast t" i)
                      (string-match-p ":requires-ast nil" i)))
               issues)))))

(ert-deftest vulpea-doctor-no-nudge-when-requires-ast-declared ()
  "Extractors that declare :requires-ast explicitly are not flagged."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((vulpea-db-async-extraction nil)
           (vulpea-db--extractors
            (list (make-vulpea-extractor :name 'reader :requires-ast t
                                         :extract-fn #'ignore)
                  (make-vulpea-extractor :name 'scanner :requires-ast nil
                                         :extract-fn #'ignore)))
           (issues (vulpea-doctor--issues)))
      (should-not (seq-some
                   (lambda (i) (string-match-p "declare :requires-ast" i))
                   issues)))))

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
