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

;;; Mode hooks vs the worker

(defun vulpea-doctor-test--hook-issue (hooks &rest bindings)
  "Return the mode-hook issue the doctor raises, or nil.
HOOKS is an alist of (HOOK-VARIABLE . FUNCTIONS) installed as global
hook values; the other mode hooks org runs are emptied.  BINDINGS is
a plist overriding the async mode (:async), parse method
\(:parse-method) and sync directories (:dirs); the first two default
to the shipped defaults."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((org-mode-hook (alist-get 'org-mode-hook hooks))
          (outline-mode-hook (alist-get 'outline-mode-hook hooks))
          (text-mode-hook (alist-get 'text-mode-hook hooks))
          (vulpea-db-sync-directories (plist-get bindings :dirs))
          (enable-local-variables :all)
          (vulpea-db-async-extraction
           (if (plist-member bindings :async) (plist-get bindings :async) t))
          (vulpea-db-parse-method
           (or (plist-get bindings :parse-method) 'temp-buffer))
          (vulpea-db--extractors nil)
          (vulpea-db-index-heading-level t)
          (vulpea-db-worker--broken nil))
      (seq-find (lambda (i) (string-match-p "mode hooks" i))
                (vulpea-doctor--issues)))))

(defun vulpea-doctor-test--set-tag-inheritance ()
  "Stand-in for a user hook that changes what extraction reads."
  (setq-local org-use-tag-inheritance nil))

(defun vulpea-doctor-test--local-todo-keywords ()
  "Stand-in for a hook org itself ignores.
`org-set-regexps-and-options' reads the default value of
`org-todo-keywords', so a buffer-local one changes no parse."
  (setq-local org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))))

(defun vulpea-doctor-test--guarded-tag-inheritance ()
  "The same hook, skipped while vulpea parses."
  (unless (bound-and-true-p vulpea-db--active-parse-method)
    (setq-local org-use-tag-inheritance nil)))

(defun vulpea-doctor-test--cosmetic ()
  "Stand-in for a user hook that only touches display settings."
  (setq-local fill-column 72)
  (visual-line-mode 1))

(defun vulpea-doctor-test--broken ()
  "Stand-in for a user hook that signals."
  (error "Boom"))

(ert-deftest vulpea-doctor-flags-hook-changing-extraction-setting ()
  "A hook setting something extraction reads is named with its setting.
The issue explains the drift and the ways out."
  (let ((issue (vulpea-doctor-test--hook-issue
                `((org-mode-hook vulpea-doctor-test--cosmetic
                                 vulpea-doctor-test--set-tag-inheritance)))))
    (should issue)
    (should (string-match-p "`org-use-tag-inheritance'" issue))
    (should (string-match-p "`vulpea-doctor-test--set-tag-inheritance'" issue))
    (should-not (string-match-p "vulpea-doctor-test--cosmetic" issue))
    (should (string-match-p "#\\+TODO" issue))
    (should (string-match-p (regexp-quote "(setq vulpea-db-async-extraction nil)")
                            issue))))

(ert-deftest vulpea-doctor-flags-anonymous-and-parent-mode-hooks ()
  "Anonymous functions and the parent mode hooks org runs count too."
  (let ((issue (vulpea-doctor-test--hook-issue
                `((text-mode-hook
                   ,(lambda () (setq-local org-category "from-hook")))))))
    (should issue)
    (should (string-match-p "`org-category'" issue))
    (should (string-match-p "an anonymous function" issue))))

(ert-deftest vulpea-doctor-no-hook-issue-for-display-hooks ()
  "Hooks that leave extraction settings alone raise nothing.
Includes org's own default hook functions."
  (should-not (vulpea-doctor-test--hook-issue
               `((org-mode-hook ,@(default-value 'org-mode-hook)
                                vulpea-doctor-test--cosmetic
                                org-indent-mode)))))

(ert-deftest vulpea-doctor-no-hook-issue-when-guarded ()
  "A hook guarded with `vulpea-db--active-parse-method' is consistent."
  (should-not (vulpea-doctor-test--hook-issue
               '((org-mode-hook vulpea-doctor-test--guarded-tag-inheritance)))))

(ert-deftest vulpea-doctor-no-hook-issue-when-empty ()
  "Empty hooks give the worker nothing to miss."
  (should-not (vulpea-doctor-test--hook-issue nil)))

(ert-deftest vulpea-doctor-no-hook-issue-when-async-off ()
  "With async off every file runs the hooks, so nothing differs."
  (should-not (vulpea-doctor-test--hook-issue
               '((org-mode-hook vulpea-doctor-test--set-tag-inheritance))
               :async nil)))

(ert-deftest vulpea-doctor-no-hook-issue-with-single-temp-buffer ()
  "`single-temp-buffer' skips the hooks in the session too."
  (should-not (vulpea-doctor-test--hook-issue
               '((org-mode-hook vulpea-doctor-test--set-tag-inheritance))
               :parse-method 'single-temp-buffer)))

(ert-deftest vulpea-doctor-hook-check-survives-failing-hook ()
  "A hook function that signals does not break the doctor."
  (let ((issue (vulpea-doctor-test--hook-issue
                '((org-mode-hook vulpea-doctor-test--broken
                                 vulpea-doctor-test--set-tag-inheritance)))))
    (should issue)
    (should (string-match-p "vulpea-doctor-test--set-tag-inheritance" issue))))

(defun vulpea-doctor-test--set-todo-keywords-globally ()
  "Stand-in for a hook that sets TODO keywords globally and applies them."
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))
  (org-set-regexps-and-options))

(defun vulpea-doctor-test--deferred-tag-inheritance ()
  "Stand-in for a hook deferring work to local variables, like Doom."
  (add-hook 'hack-local-variables-hook
            #'vulpea-doctor-test--set-tag-inheritance nil t))

(defun vulpea-doctor-test--set-category ()
  "Stand-in for a hook setting the category."
  (setq-local org-category "from-hook"))

(ert-deftest vulpea-doctor-hook-check-accepts-single-function-hook ()
  "A hook whose value is one function, not a list, is probed too."
  (let ((issue (vulpea-doctor-test--hook-issue
                '((org-mode-hook . vulpea-doctor-test--set-tag-inheritance)))))
    (should issue)
    (should (string-match-p "vulpea-doctor-test--set-tag-inheritance" issue))))

(ert-deftest vulpea-doctor-hook-check-blames-only-global-setter ()
  "A hook setting a value globally is blamed alone and undone.
Later hook functions must not inherit the blame, and the doctor
must leave the global value as it found it."
  (let* ((before (default-value 'org-todo-keywords))
         (issue (vulpea-doctor-test--hook-issue
                 '((org-mode-hook vulpea-doctor-test--set-todo-keywords-globally
                                  vulpea-doctor-test--cosmetic)))))
    (should issue)
    (should (string-match-p "vulpea-doctor-test--set-todo-keywords-globally"
                            issue))
    (should (string-match-p "`org-todo-keywords'" issue))
    (should-not (string-match-p "vulpea-doctor-test--cosmetic" issue))
    (should (equal (default-value 'org-todo-keywords) before))))

(ert-deftest vulpea-doctor-hook-check-sees-deferred-work ()
  "Work a hook defers to `hack-local-variables-hook' is caught."
  (let ((issue (vulpea-doctor-test--hook-issue
                '((org-mode-hook vulpea-doctor-test--deferred-tag-inheritance)))))
    (should issue)
    (should (string-match-p "vulpea-doctor-test--deferred-tag-inheritance"
                            issue))))

(ert-deftest vulpea-doctor-hook-check-respects-dir-locals ()
  "A hook value that dir-locals override everywhere is harmless.
Dir-locals apply after the mode hooks, in the session and in the
worker alike, so the indexed value is the dir-local one either way."
  (let ((dir (make-temp-file "vulpea-doctor-dirlocals-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" dir)
            (prin1 '((org-mode . ((org-category . "from-dir-locals"))))
                   (current-buffer)))
          (should-not (vulpea-doctor-test--hook-issue
                       '((org-mode-hook vulpea-doctor-test--set-category))
                       :dirs (list dir))))
      (delete-directory dir t))))

(ert-deftest vulpea-doctor-no-hook-issue-for-settings-org-ignores ()
  "A buffer-local `org-todo-keywords' changes no parse, so no issue.
Org derives the TODO regexps from the default value when the mode
starts; comparing the raw variable would cry wolf."
  (should-not (vulpea-doctor-test--hook-issue
               '((org-mode-hook vulpea-doctor-test--local-todo-keywords)))))

(defun vulpea-doctor-test--load-late-setting ()
  "Stand-in for a hook that loads a library defining a setting.
Loading `org-attach' from a hook does exactly this for its options."
  (unless (boundp 'vulpea-doctor-test--late-setting)
    (set-default 'vulpea-doctor-test--late-setting "from-library")))

(ert-deftest vulpea-doctor-hook-check-leaves-new-settings-alone ()
  "A setting a hook's library defines is neither blamed nor clobbered.
Before the probe it was unbound, so there is nothing to compare it
with and nothing to restore; writing nil over it would break the
library (for org-attach, the worker would get a nil attach dir)."
  (unwind-protect
      (let ((vulpea-db-worker--settings-vars
             (cons 'vulpea-doctor-test--late-setting
                   vulpea-db-worker--settings-vars)))
        (should-not (vulpea-doctor-test--hook-issue
                     '((org-mode-hook vulpea-doctor-test--load-late-setting))))
        (should (equal (default-value 'vulpea-doctor-test--late-setting)
                       "from-library")))
    (makunbound 'vulpea-doctor-test--late-setting)))

(defvar vulpea-doctor-test--doomed-setting "original"
  "A mirrored setting a hook unbinds, for the restore test.")

(defun vulpea-doctor-test--unbind-setting ()
  "Stand-in for a hook that unbinds a setting."
  (makunbound 'vulpea-doctor-test--doomed-setting))

(ert-deftest vulpea-doctor-hook-check-survives-unbinding-hook ()
  "A hook that unbinds a setting neither crashes the doctor nor
leaves the setting unbound."
  (let ((vulpea-db-worker--settings-vars
         (cons 'vulpea-doctor-test--doomed-setting
               vulpea-db-worker--settings-vars)))
    (vulpea-doctor-test--hook-issue
     '((org-mode-hook vulpea-doctor-test--unbind-setting)))
    (should (equal (default-value 'vulpea-doctor-test--doomed-setting)
                   "original"))))

;;; Session vs worker consistency

(defmacro vulpea-doctor-test--with-indexed-file (content &rest body)
  "Run BODY with CONTENT indexed and async extraction eligible.
Mode hooks are emptied; BODY binds them as needed."
  (declare (indent 1))
  `(vulpea-test--with-temp-db-and-file "consistency-file" ,content
     (let ((vulpea-db-async-extraction t)
           (vulpea-db-parse-method 'temp-buffer)
           (vulpea-db--extractors nil)
           (vulpea-db-index-heading-level t)
           (vulpea-db-worker--broken nil)
           (vulpea-db-sync-directories nil)
           (org-mode-hook nil)
           (outline-mode-hook nil)
           (text-mode-hook nil))
       ,@body)))

(ert-deftest vulpea-doctor-flags-files-the-worker-indexes-differently ()
  "Sampled files that index differently in the worker are named.
The check compares outcomes, so it catches causes no list predicts."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let* ((org-mode-hook (list (lambda () (setq-local org-category "hooked"))))
           (issue (seq-find (lambda (i) (string-match-p "sampled files" i))
                            (vulpea-doctor--issues))))
      (should issue)
      (should (string-match-p (regexp-quote (file-name-nondirectory temp-org-file))
                              issue))
      (should (string-match-p ":category" issue))
      (should (string-match-p (regexp-quote "(setq vulpea-db-async-extraction nil)")
                              issue)))))

(ert-deftest vulpea-doctor-reports-consistent-sample ()
  "A clean setup raises no issue and says how much was checked.
The comparison spawns a worker, so one report runs it once."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let* ((calls 0)
           (compare (symbol-function 'vulpea-db-worker-compare-files))
           (report (cl-letf (((symbol-function 'vulpea-db-worker-compare-files)
                              (lambda (paths)
                                (setq calls (1+ calls))
                                (funcall compare paths))))
                     (vulpea-doctor))))
      (should (= calls 1))
      (should-not (string-match-p "sampled files" report))
      (should (string-match-p "session vs worker +1 sampled, all match" report)))))

(ert-deftest vulpea-doctor-skips-consistency-when-async-off ()
  "With async extraction off there is nothing to compare."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let* ((vulpea-db-async-extraction nil)
           (org-mode-hook (list (lambda () (setq-local org-category "hooked"))))
           (report (vulpea-doctor)))
      (should-not (string-match-p "sampled files" report))
      (should (string-match-p "session vs worker +n/a" report)))))

(ert-deftest vulpea-doctor-consistency-skips-visited-files ()
  "Files open in a buffer are left out of the sample.
Parsing with `find-file' reuses and then kills a visiting buffer,
and an unsaved one would compare its edits against the file."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let* ((vulpea-db-parse-method 'find-file)
           (buffer (find-file-noselect temp-org-file)))
      (unwind-protect
          (progn
            (should-not (member temp-org-file
                                (vulpea-doctor--consistency-sample)))
            (should (string-match-p "session vs worker +nothing to sample"
                                    (vulpea-doctor)))
            (should (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defmacro vulpea-doctor-test--with-worker-command (form &rest body)
  "Run BODY with the worker replaced by an Emacs evaluating FORM."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'vulpea-db-worker--command)
              (lambda ()
                (list (expand-file-name invocation-name invocation-directory)
                      "--batch" "-Q" "--eval" ,form))))
     ,@body))

(ert-deftest vulpea-doctor-consistency-reports-broken-worker ()
  "A worker that dies on startup is a failure, not drift.
Otherwise every sampled file comes back without a result and the
doctor blames the user's setup for a broken worker.  The report
keeps to one line: the last line of the worker's output, which is
where batch Emacs prints the error after any backtrace."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (vulpea-doctor-test--with-worker-command
        "(progn (message \"frame one\nframe two\") (message \"worker exploded\") (kill-emacs 3))"
      (let ((report (vulpea-doctor)))
        (should (string-match-p "session vs worker +FAILED: [^\n]*worker exploded" report))
        (should-not (string-match-p "frame one" report))
        (should (string-match-p "worker used for the comparison failed" report))
        (should-not (string-match-p "sampled files differently" report))))))

(ert-deftest vulpea-doctor-consistency-reports-silent-worker ()
  "A worker that exits cleanly without answering says so."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (vulpea-doctor-test--with-worker-command "(kill-emacs 0)"
      (should (string-match-p "session vs worker +FAILED: the worker exited without answering"
                              (vulpea-doctor))))))

(ert-deftest vulpea-doctor-consistency-sample-respects-budget ()
  "The sample stops at the total size budget.
Each sampled file is parsed twice while the doctor blocks Emacs, so
the cost has to stay bounded however large the files are."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let ((extra (mapcar (lambda (i)
                           (let ((path (vulpea-test--create-temp-org-file
                                        (format ":PROPERTIES:\n:ID: budget-%d\n:END:\n#+title: B%d\n"
                                                i i))))
                             (vulpea-db-update-file path)
                             path))
                         '(1 2))))
      (unwind-protect
          (let ((vulpea-doctor--consistency-max-total 60))
            (should (= 1 (length (vulpea-doctor--consistency-sample)))))
        (mapc #'delete-file extra)))))

(ert-deftest vulpea-doctor-flags-outline-mode-hook ()
  "Functions on `outline-mode-hook' run for org buffers too."
  (let ((issue (vulpea-doctor-test--hook-issue
                '((outline-mode-hook vulpea-doctor-test--set-tag-inheritance)))))
    (should issue)
    (should (string-match-p "vulpea-doctor-test--set-tag-inheritance" issue))))

(ert-deftest vulpea-doctor-consistency-sample-selection ()
  "The sample prefers recent files and skips what it cannot compare.
Non-.org files, missing files and files over the size limit stay out;
the most recently modified file is always in."
  (vulpea-test--with-temp-db
    (let* ((db (vulpea-db))
           (paths (mapcar (lambda (i)
                            (vulpea-test--create-temp-org-file
                             (format ":PROPERTIES:\n:ID: pick-%d\n:END:\n" i)))
                          '(1 2 3)))
           (vulpea-doctor--consistency-sample-size 2))
      (unwind-protect
          (progn
            (cl-loop for path in paths
                     for mtime in '(100 200 300)
                     do (emacsql db [:insert :into files :values $v1]
                                 (vector path "h" mtime 10)))
            (emacsql db [:insert :into files :values $v1]
                     (vector "/tmp/vulpea-pick.txt" "h" 900 10))
            (emacsql db [:insert :into files :values $v1]
                     (vector "/tmp/vulpea-pick-missing.org" "h" 900 10))
            (let ((big (vulpea-test--create-temp-org-file
                        ":PROPERTIES:\n:ID: pick-big\n:END:\n")))
              (push big paths)
              (emacsql db [:insert :into files :values $v1]
                       (vector big "h" 999 (* 10 1024 1024))))
            (dotimes (_ 10)
              (let ((sample (vulpea-doctor--consistency-sample)))
                (should (= (length sample) 2))
                (should (member (nth 3 paths) sample))
                (should-not (member (car paths) sample))
                (should-not (seq-some (lambda (p) (string-prefix-p "/tmp/vulpea-pick" p))
                                      sample)))))
        (mapc #'delete-file paths)))))

(defun vulpea-doctor-test--set-parse-method ()
  "Stand-in for a hook setting a setting read outside the parse buffer."
  (setq-local vulpea-db-parse-method 'find-file)
  (setq-local vulpea-db-path-normalization nil))

(ert-deftest vulpea-doctor-no-hook-issue-for-settings-read-outside ()
  "Settings read before the parse buffer exists cannot drift by hook.
`vulpea-db-parse-method' picks the buffer and path normalization
keys the database, both outside the buffer a hook runs in."
  (should-not (vulpea-doctor-test--hook-issue
               '((org-mode-hook vulpea-doctor-test--set-parse-method)))))

(ert-deftest vulpea-doctor-asks-for-report-only-without-hook ()
  "Drift a reported hook explains needs no bug report; other drift does."
  (vulpea-doctor-test--with-indexed-file "#+title: C\n"
    (let* ((org-mode-hook (list #'vulpea-doctor-test--set-category))
           (issue (seq-find (lambda (i) (string-match-p "sampled files" i))
                            (vulpea-doctor--issues))))
      (should issue)
      (should-not (string-match-p "please report" issue))
      (should (string-match-p "hook" issue)))
    (let* ((title-fn (symbol-function 'vulpea-db--extract-file-title))
           (issue (cl-letf (((symbol-function 'vulpea-db--extract-file-title)
                             (lambda (&rest args)
                               (concat "session " (apply title-fn args)))))
                    (seq-find (lambda (i) (string-match-p "sampled files" i))
                              (vulpea-doctor--issues)))))
      (should issue)
      (should (string-match-p "please report" issue)))))

(ert-deftest vulpea-doctor-explains-custom-attach-path-functions ()
  "The doctor says why custom attach path functions bypass the worker."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((vulpea-db-async-extraction t)
           (vulpea-db--extractors nil)
           (vulpea-db-index-heading-level t)
           (vulpea-db-worker--broken nil)
           (org-attach-id-to-path-function-list (list (lambda (id) id)))
           (issue (seq-find (lambda (i) (string-match-p "will NOT use the worker" i))
                            (vulpea-doctor--issues))))
      (should issue)
      (should (string-match-p "org-attach-id-to-path-function-list" issue)))))

(ert-deftest vulpea-doctor-consistency-sample-checks-only-candidates ()
  "Expensive per-file checks run on the files being picked, not all rows.
`find-buffer-visiting' and friends touch the file system; on a
database with 100k files they took seconds before sampling."
  (vulpea-test--with-temp-db
    (let* ((db (vulpea-db))
           (paths (mapcar (lambda (i)
                            (vulpea-test--create-temp-org-file
                             (format ":PROPERTIES:\n:ID: cheap-%d\n:END:\n" i)))
                          (number-sequence 1 50)))
           (vulpea-doctor--consistency-sample-size 2)
           (checks 0))
      (unwind-protect
          (progn
            (cl-loop for path in paths
                     for mtime from 1
                     do (emacsql db [:insert :into files :values $v1]
                                 (vector path "h" mtime 10)))
            (cl-letf* ((visiting (symbol-function 'find-buffer-visiting))
                       ((symbol-function 'find-buffer-visiting)
                        (lambda (&rest args)
                          (setq checks (1+ checks))
                          (apply visiting args))))
              (should (= 2 (length (vulpea-doctor--consistency-sample))))
              (should (<= checks 4))))
        (mapc #'delete-file paths)))))

(provide 'vulpea-doctor-test)
;;; vulpea-doctor-test.el ends here
