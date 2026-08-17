;;; vulpea.el --- Note management library for Org mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 2.7.0
;; Package-Requires: ((emacs "29.1") (org "9.4.4") (emacsql "4.3.0") (s "1.12") (dash "2.19"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vulpea is a note management library for Org mode that maintains its
;; own SQLite database for efficient querying and organization.
;;
;; Key features:
;; - Fast note lookup via SQLite database with automatic sync
;; - Rich note structure: titles, aliases, tags, links, properties, metadata
;; - Flexible querying by tags, links, properties, dates, and more
;; - Metadata system using Org description lists
;; - Note creation with customizable templates
;; - Selection interface with filtering and alias expansion
;;
;; Quick start:
;;   (setq vulpea-directory "~/org/")
;;   (vulpea-db-sync-start)
;;
;; Main entry points:
;; - `vulpea-find' - find and open a note
;; - `vulpea-insert' - insert a link to a note
;; - `vulpea-create' - create a new note
;; - `vulpea-db-query' - query notes with a predicate
;; - `vulpea-select' - select a note with completion
;;
;; See https://github.com/d12frosted/vulpea for documentation.
;;
;;; Code:

(require 'package)
(require 'org-capture)
(require 'org-id)
(require 'vulpea-buffer)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-sync)
(require 'vulpea-db-schema-validation)
(require 'vulpea-mentions)
(require 'vulpea-meta)
(require 'vulpea-note)
(require 'vulpea-schema)
(require 'vulpea-select)
(require 'vulpea-tags)
(require 'vulpea-timestamp)
(require 'vulpea-utils)
(require 's)

;;; Version

(declare-function elpaca-get "ext:elpaca" (id))
(declare-function elpaca-source-dir "ext:elpaca" (e))
(declare-function elpaca<-repo-dir "ext:elpaca" (e))
(declare-function straight--repos-dir "ext:straight" (&rest segments))

(defconst vulpea-version "2.7.0"
  "Version of the vulpea package.

Keep in sync with the Version header in vulpea.el; releases bump
both. For precise version information including commits past a
release, use the function `vulpea-version' instead.")

(defun vulpea-version--checkout-p (dir)
  "Return non-nil when DIR is a vulpea git checkout.

DIR must hold both a \".git\" entry (a directory in regular
clones, a plain file in worktrees) and \"vulpea.el\". Finding
\".git\" alone proves nothing: build directories often sit
inside an unrelated repository, e.g. elpaca's builds inside a
version-controlled Emacs configuration, and \"git describe\"
there would report that repository's history instead of
vulpea's."
  (and (file-exists-p (expand-file-name ".git" dir))
       (file-exists-p (expand-file-name "vulpea.el" dir))))

(defun vulpea-version--git ()
  "Return version from \"git describe\", or nil if unavailable.

Works when vulpea is loaded from a git checkout and git is
available. The result looks like \"v2.2.0\" exactly on a release
tag, \"v2.2.0-15-g2938416\" when 15 commits past it, with a
\"-dirty\" suffix when there are uncommitted changes.

The checkout is searched in several places, because package
managers separate the loaded build from the git source: the
resolved truename of the loaded library (plain checkouts, and
managers that symlink their build directory), then elpaca's and
straight's source directories when those managers are present
\(their builds are plain copies, revealing nothing about the
checkout).

A place is only accepted when the walk up from it ends in an
actual vulpea checkout (see `vulpea-version--checkout-p'), so an
unrelated repository above a build directory, like a
version-controlled Emacs configuration, does not end the search."
  (when-let* ((dir (seq-some
                    (lambda (candidate)
                      (and candidate
                           (locate-dominating-file
                            candidate #'vulpea-version--checkout-p)))
                    (list
                     (when-let* ((file (locate-library "vulpea")))
                       (file-truename file))
                     (when (fboundp 'elpaca-get)
                       (when-let* ((e (elpaca-get 'vulpea)))
                         (cond
                          ((fboundp 'elpaca-source-dir)
                           (elpaca-source-dir e))
                          ((fboundp 'elpaca<-repo-dir)
                           (elpaca<-repo-dir e)))))
                     (when (fboundp 'straight--repos-dir)
                       (straight--repos-dir "vulpea")))))
              ((executable-find "git")))
    (with-temp-buffer
      (let ((default-directory dir))
        (when (eql 0 (ignore-errors
                       (call-process "git" nil t nil "describe"
                                     "--tags" "--dirty" "--always")))
          (string-trim (buffer-string)))))))

(defun vulpea-version--package ()
  "Return version of the installed vulpea package, or nil.

For MELPA snapshot installs the result looks like
\"20260610.1234 (commit 2938416)\"."
  (when-let* ((desc (cadr (assq 'vulpea package-alist)))
              (version (package-version-join
                        (package-desc-version desc))))
    (if-let* ((commit (cdr (assq :commit (package-desc-extras desc)))))
        (format "%s (commit %s)"
                version (substring commit 0 (min 7 (length commit))))
      version)))

(defun vulpea-version (&optional show)
  "Return the vulpea version with as much precision as available.

The version is resolved in the following order:

1. \"git describe\" output when running from a git checkout,
   e.g. \"v2.2.0\" or \"v2.2.0-15-g2938416\".
2. Installed package version, including the commit for MELPA
   snapshot installs, e.g. \"20260610.1234 (commit 2938416)\".
3. The `vulpea-version' constant as a fallback.

When SHOW is non-nil (always when called interactively), also
display the version in the echo area. Please include this
version in bug reports."
  (interactive (list t))
  (let ((version (or (vulpea-version--git)
                     (vulpea-version--package)
                     vulpea-version)))
    (when show
      (message "vulpea %s" version))
    version))

;;; Doctor

(defun vulpea-doctor--db-file-info ()
  "Return a human-readable description of the database file."
  (if (file-exists-p vulpea-db-location)
      (format "exists (%s)"
              (file-size-human-readable
               (file-attribute-size
                (file-attributes vulpea-db-location))))
    "missing"))

(defun vulpea-doctor--note-count ()
  "Return the number of indexed notes, or nil when unavailable.

Returns nil instead of creating the database file when it does
not exist - the doctor must not modify state."
  (when (file-exists-p vulpea-db-location)
    (ignore-errors (vulpea-db-count-notes))))

(defun vulpea-doctor--cached-file-stats ()
  "Return (TOTAL . NOTE-LESS) file change-detection cache counts.

TOTAL is how many files are tracked in the `files' table; NOTE-LESS
is how many of them have no note in the `notes' table.  A non-zero
NOTE-LESS is expected for genuinely note-less files (READMEs,
drafts), but a surprising count can indicate notes that failed to
index and are now skipped by change detection (see vulpea#277);
`vulpea-db-sync-full-scan' with a force argument re-extracts them.

Returns nil when the database file is absent; does not create it."
  (when (file-exists-p vulpea-db-location)
    (ignore-errors
      (let ((db (vulpea-db)))
        (cons
         (caar (emacsql db [:select (funcall count *) :from files]))
         (caar (emacsql db
                        [:select (funcall count *) :from files
                         :where (not (in path
                                         [:select :distinct [path]
                                          :from notes]))])))))))

(defun vulpea-doctor--monitoring-status ()
  "Return a string describing the active external file monitoring."
  (cond
   ((and vulpea-db-sync--fswatch-process
         (process-live-p vulpea-db-sync--fswatch-process))
    "fswatch (process running)")
   (vulpea-db-sync--poll-timer
    (format "polling (every %ss)" vulpea-db-sync-poll-interval))
   (t "none")))

(defun vulpea-doctor--dir-key (dir)
  "Return the canonical comparison key for directory DIR.

Expansion strips spelling differences (abbreviations, trailing
slashes) that do not make two paths different directories."
  (directory-file-name (expand-file-name dir)))

(defun vulpea-doctor--fswatch-argv-directories ()
  "Return the directories the running fswatch process was started with.

Reads them from the process command line, where they are the
trailing arguments after the --format flag and its value (see
`vulpea-db-sync--setup-fswatch').  Returns nil when fswatch is not
running or the command line does not have the expected shape."
  (when (and vulpea-db-sync--fswatch-process
             (process-live-p vulpea-db-sync--fswatch-process))
    (when-let* ((tail (member "--format"
                              (process-command
                               vulpea-db-sync--fswatch-process))))
      (cddr tail))))

(defun vulpea-doctor--watch-divergence ()
  "Detect watchers running on an outdated `vulpea-db-sync-directories'.

The variable is read when `vulpea-db-autosync-mode' starts its
watchers: fswatch receives the directories as command-line
arguments and filenotify watches are created for them.  A later
setq never reaches the running watcher, while manual sync commands
read the current value - files under a newly configured directory
are silently never synced on save, which is easy to misread as
anything but a configuration problem (see vulpea#427).

Returns nil when watching and configuration agree, or a list
\(MONITOR MISSING STALE) where MONITOR is \"fswatch\" or
\"filenotify\", MISSING are configured directories not being
watched and STALE are watched directories no longer configured.
Directories that do not exist are ignored: a restart would not
watch them either, and they are reported separately."
  (cond
   ;; fswatch monitors each directory recursively, so its argv is the
   ;; exact set of roots to compare against the configuration.  While
   ;; it runs no filenotify watchers exist, so the second branch must
   ;; not be consulted even when the argv cannot be parsed.
   ((and vulpea-db-sync--fswatch-process
         (process-live-p vulpea-db-sync--fswatch-process))
    (when-let* ((argv-dirs (vulpea-doctor--fswatch-argv-directories)))
      (let* ((watched
              (seq-uniq
               (mapcar (lambda (dir)
                         (vulpea-doctor--dir-key
                          (vulpea-db-sync--fswatch-normalize-path dir)))
                       argv-dirs)))
             (configured
              (seq-uniq
               (seq-filter #'file-directory-p
                           (mapcar #'vulpea-doctor--dir-key
                                   vulpea-db-sync-directories))))
             (missing (seq-difference configured watched))
             (stale (seq-difference watched configured)))
        (when (or missing stale)
          (list "fswatch" missing stale)))))
   ;; filenotify holds one watch per directory, subdirectories
   ;; included, so only the roots of the watch list are compared:
   ;; every configured root must be watched, and every watched root
   ;; must still be configured.  Symlinked configured directories are
   ;; skipped - `vulpea-db-sync--watch-directory' never watches them,
   ;; so they would read as diverging forever.
   (vulpea-db-sync--watchers
    (let* ((watched
            (seq-uniq
             (mapcar (lambda (entry) (vulpea-doctor--dir-key (car entry)))
                     vulpea-db-sync--watchers)))
           (configured
            (seq-uniq
             (seq-filter (lambda (dir)
                           (and (file-directory-p dir)
                                (not (file-symlink-p dir))))
                         (mapcar #'vulpea-doctor--dir-key
                                 vulpea-db-sync-directories))))
           (under-p (lambda (dir root)
                      (or (equal dir root)
                          (string-prefix-p (file-name-as-directory root)
                                           dir))))
           (missing (seq-remove (lambda (dir) (member dir watched))
                                configured))
           (roots (seq-remove
                   (lambda (dir)
                     (seq-some (lambda (other)
                                 (and (not (equal other dir))
                                      (funcall under-p dir other)))
                               watched))
                   watched))
           (stale (seq-remove
                   (lambda (root)
                     (seq-some (lambda (dir) (funcall under-p root dir))
                               configured))
                   roots)))
      (when (or missing stale)
        (list "filenotify" missing stale))))))

(defun vulpea-doctor--issues ()
  "Return a list of detected setup issues as human-readable strings."
  (let ((issues nil)
        (fswatch (executable-find "fswatch"))
        (fd (executable-find "fd"))
        (notes (vulpea-doctor--note-count)))
    ;; Sync directories
    (if (null vulpea-db-sync-directories)
        (push (concat "`vulpea-db-sync-directories' is empty - nothing will"
                      " be indexed. Set it (or `org-directory') to where"
                      " your notes live.")
              issues)
      (dolist (dir vulpea-db-sync-directories)
        (unless (file-directory-p dir)
          (push (format "Sync directory %s does not exist." dir) issues))))
    ;; External tools. A missing executable on a GUI Emacs is often a
    ;; PATH problem rather than a missing install (Doom env file,
    ;; minimal GUI PATH), hence the exec-path hint.
    (when (and (not fswatch)
               (memq vulpea-db-sync-external-method '(auto fswatch)))
      (push (concat "fswatch not found on `exec-path'"
                    (if (eq vulpea-db-sync-external-method 'fswatch)
                        (concat " but `vulpea-db-sync-external-method' is"
                                " 'fswatch - external monitoring will fail"
                                " to start.")
                      " - external changes are detected via slower polling.")
                    " Install fswatch, or fix Emacs's PATH if it is already"
                    " installed (Doom users: re-run 'doom env').")
            issues))
    (unless fd
      (push (concat "fd not found on `exec-path' - directory scans fall"
                    " back to find, which is much slower on large"
                    " collections. Install fd, or fix Emacs's PATH if it"
                    " is already installed (Doom users: re-run 'doom env').")
            issues))
    ;; Sync state
    (unless vulpea-db-autosync-mode
      (push (concat "`vulpea-db-autosync-mode' is disabled - the database"
                    " will not stay up to date as notes change. Enable it"
                    " with (vulpea-db-autosync-mode +1).")
            issues))
    (when (and vulpea-db-autosync-mode
               (memq vulpea-db-sync-external-method '(auto fswatch poll))
               (string= "none" (vulpea-doctor--monitoring-status)))
      (push (concat "External monitoring is configured but not active -"
                    " changes made outside Emacs will not be picked up."
                    " Try toggling `vulpea-db-autosync-mode'.")
            issues))
    ;; Watchers running on an outdated directory list (vulpea#427)
    (when-let* ((divergence (vulpea-doctor--watch-divergence)))
      (pcase-let ((`(,monitor ,missing ,stale) divergence))
        (push (format
               (concat "File watching (%s) was started with a different"
                       " `vulpea-db-sync-directories' value - %s."
                       " The variable is read when autosync starts, so"
                       " changing it does not reach the running watcher:"
                       " files under a directory added since are never"
                       " synced on save, even though manual sync commands"
                       " see them. Toggle `vulpea-db-autosync-mode' off"
                       " and on to apply the current value.")
               monitor
               (mapconcat
                #'identity
                (delq nil
                      (list
                       (when missing
                         (format "not watched: %s"
                                 (string-join missing ", ")))
                       (when stale
                         (format "still watched but no longer configured: %s"
                                 (string-join stale ", ")))))
                "; "))
              issues)))
    ;; Database
    (cond
     ((not (file-exists-p vulpea-db-location))
      (push (concat "Database file does not exist yet. Run"
                    " M-x vulpea-db-sync-full-scan to build it.")
            issues))
     ((and notes (zerop notes))
      (push (concat "Database exists but contains no notes. Run"
                    " M-x vulpea-db-sync-full-scan; if it stays empty,"
                    " check that your notes have ID properties and live"
                    " under `vulpea-db-sync-directories'.")
            issues)))
    ;; Duplicate ids: pending claims that never resolved (vulpea#469).
    ;; A claim normally lives only for the moment of a refile - the
    ;; old file releases the id and the claimant wins it.  One that is
    ;; still here means several files durably contain the same :ID:.
    (when-let* ((claims (and (file-exists-p vulpea-db-location)
                             (vulpea-db--get-pending-claims))))
      (let ((groups (seq-group-by #'car claims)))
        (push (format
               (concat "Duplicate note id%s: %s. An id can be indexed"
                       " from only one file; the copies are invisible to"
                       " queries. If a note was moved recently this heals"
                       " itself once the old file is saved or rescanned;"
                       " a copy meant to be a separate note needs a fresh"
                       " id of its own.")
               (if (cdr groups) "s" "")
               (mapconcat
                (lambda (group)
                  (let* ((id (car group))
                         (claimants (mapcar #'cdr (cdr group)))
                         (owner (when-let* ((note (vulpea-db-get-by-id id)))
                                  (vulpea-note-path note))))
                    (format "%s lives in %s"
                            id
                            (string-join (delq nil (cons owner claimants))
                                         ", "))))
                groups
                "; "))
              issues)))
    ;; Extractor plugins
    (when-let* ((undeclared
                 (seq-filter
                  (lambda (extractor)
                    (eq (vulpea-extractor-requires-ast extractor) 'unset))
                  vulpea-db--extractors)))
      (push (format
             (concat "Extractor%s %s do%s not declare :requires-ast."
                     " Undeclared extractors receive a parse context whose"
                     " AST slot is always nil. Add :requires-ast t to the"
                     " definition if its extract-fn reads"
                     " (vulpea-parse-ctx-ast ctx), or :requires-ast nil to"
                     " confirm it works purely from note data (and keep"
                     " extraction fast and worker-eligible).")
             (if (cdr undeclared) "s" "")
             (mapconcat (lambda (extractor)
                          (format "`%s'" (vulpea-extractor-name extractor)))
                        undeclared ", ")
             (if (cdr undeclared) "" "es"))
            issues))
    ;; Async extraction
    (when vulpea-db-async-extraction
      (when-let* ((reasons (vulpea-db-worker-rejection-reasons "probe.org")))
        (push (format
               (concat "`vulpea-db-async-extraction' is enabled but your"
                       " .org files will NOT use the worker (%s) - every"
                       " file takes the synchronous path. %s")
               (mapconcat #'symbol-name reasons ", ")
               (cond
                ((memq 'ast-extractors reasons)
                 (concat "An extractor plugin declares :requires-ast t;"
                         " the AST cannot cross the process boundary, so"
                         " AST-reading extractors and async extraction"
                         " do not combine."))
                ((memq 'broken reasons)
                 (concat "The worker crash-looped; see *Warnings*, then"
                         " M-x vulpea-db-worker-reset to retry."))
                (t "Run M-x vulpea-db-worker-diagnose for details.")))
              issues))
      (when (and (eq vulpea-db-async-extraction 'full)
                 (bound-and-true-p vulpea-db-worker--wal-failed))
        (push (concat "Full-write mode is configured but WAL journaling"
                      " could not be enabled on the database (filesystem"
                      " without shared-memory support?) - degraded to"
                      " extract-only. The database write runs on the main"
                      " thread.")
              issues))
      (when (and (eq vulpea-db-async-extraction 'full)
                 (not (vulpea-db-worker--filters-inert-p)))
        (push (concat "Full-write mode is configured but note index"
                      " filters are active (schema validation with a"
                      " non-silent action, or a custom filter) - degraded"
                      " to extract-only so the filters keep running in"
                      " your session.")
              issues)))
    (nreverse issues)))

(defun vulpea-doctor--report ()
  "Build the doctor report as a string."
  (let* ((issues (vulpea-doctor--issues))
         (notes (vulpea-doctor--note-count))
         (stats (vulpea-doctor--cached-file-stats))
         (line (lambda (label value) (format "  %-32s %s" label value))))
    (string-join
     (append
      (list
       "Vulpea Doctor"
       "============="
       ""
       "Versions"
       (funcall line "vulpea" (vulpea-version))
       (funcall line "emacs" emacs-version)
       (funcall line "org" (org-version))
       (funcall line "system" (format "%s" system-type))
       ""
       "Configuration"
       (funcall line "vulpea-db-sync-directories"
                (format "%S" vulpea-db-sync-directories))
       (funcall line "vulpea-db-location" vulpea-db-location)
       (funcall line "vulpea-db-parse-method"
                (format "%s" vulpea-db-parse-method))
       (funcall line "vulpea-db-index-heading-level"
                (format "%s" vulpea-db-index-heading-level))
       (funcall line "vulpea-db-sync-external-method"
                (format "%s" vulpea-db-sync-external-method))
       (funcall line "vulpea-db-sync-scan-on-enable"
                (format "%s" vulpea-db-sync-scan-on-enable))
       ""
       "Database"
       (funcall line "file" (vulpea-doctor--db-file-info))
       (funcall line "schema version" (format "%s" vulpea-db-version))
       (funcall line "notes" (if notes (format "%d" notes) "n/a"))
       (funcall line "cached files"
                (if stats (format "%d" (car stats)) "n/a"))
       (funcall line "files without notes"
                (if stats (format "%d" (cdr stats)) "n/a"))
       ""
       "Sync"
       (funcall line "autosync"
                (if vulpea-db-autosync-mode "enabled" "disabled"))
       (funcall line "external monitoring" (vulpea-doctor--monitoring-status))
       (funcall line "pending queue"
                (format "%d" (length vulpea-db-sync--queue)))
       ""
       "Async Extraction"
       (funcall line "mode" (format "%s" vulpea-db-async-extraction))
       (funcall line "worker"
                (cond
                 ((not vulpea-db-async-extraction) "n/a")
                 ((bound-and-true-p vulpea-db-worker--broken)
                  "BROKEN (crash loop; M-x vulpea-db-worker-reset)")
                 ((process-live-p
                   (bound-and-true-p vulpea-db-worker--process))
                  (format "running (%d in flight)"
                          (vulpea-db-worker-in-flight-count)))
                 (t "not running (spawns on first change)")))
       (funcall line "handles .org files"
                (if vulpea-db-async-extraction
                    (if-let* ((reasons (vulpea-db-worker-rejection-reasons
                                        "probe.org")))
                        (format "NO: %s"
                                (mapconcat #'symbol-name reasons ", "))
                      "yes")
                  "n/a"))
       ""
       "External Tools"
       (funcall line "fd" (or (executable-find "fd") "not found"))
       (funcall line "fswatch" (or (executable-find "fswatch") "not found"))
       (funcall line "rg" (or (executable-find "rg") "not found"))
       (funcall line "git" (or (executable-find "git") "not found"))
       ""
       "Issues")
      (if issues
          (mapcar (lambda (issue) (concat "  - " issue)) issues)
        (list "  No issues detected.")))
     "\n")))

;;;###autoload
(defun vulpea-doctor (&optional show)
  "Diagnose the Vulpea setup and return a report string.

The report covers versions, configuration, database state, sync
state, external tool availability, and a list of detected issues.
It is read-only: nothing is created or modified, even when the
database does not exist yet. Please include the report in bug
reports.

When SHOW is non-nil (always when called interactively), also
display the report in the *vulpea-doctor* buffer."
  (interactive (list t))
  (let ((report (vulpea-doctor--report)))
    (when show
      (with-current-buffer (get-buffer-create "*vulpea-doctor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min)))
        (special-mode)
        (display-buffer (current-buffer))))
    report))

;;; Customization

(defgroup vulpea nil
  "Vulpea note-taking system."
  :group 'org)

(defcustom vulpea-default-notes-directory nil
  "Default directory for creating new notes.

When nil (the default), dynamically resolves to the sync directory
holding `default-directory' - a note is born in the corpus being
visited - and falls back to the first entry in
`vulpea-db-sync-directories' outside any of them.

Set this explicitly to pin new notes to one directory regardless of
where they are created from."
  :type '(choice (const :tag "Corpus of the current buffer" nil)
                 (directory :tag "Explicit directory"))
  :group 'vulpea)

(defcustom vulpea-create-default-function nil
  "Function to compute default parameters for note creation.
Called with (title) and should return a plist of default parameters.
When nil, uses `vulpea-create-default-template' instead.

The function allows dynamic parameter computation based on context:

  (setq vulpea-create-default-function
        (lambda (title)
          (list :tags (if (string-match-p \"TODO\" title)
                          \\='(\"task\" \"inbox\")
                        \\='(\"note\" \"inbox\"))
                :head (format \"#+created: %s\"
                              (format-time-string \"[%Y-%m-%d]\"))
                :properties (list (cons \"SOURCE\"
                                        (buffer-name))))))

Parameters explicitly passed to `vulpea-create' override these defaults,
except for the title. If the template returned by this function includes
`:title', it will take precedence over the title passed to
`vulpea-create' in order to allow for this function to modify the note
title.

These defaults seed file-level note creation only.  When
`vulpea-create' is called with a non-nil `:parent' (a heading-level
note), this function is not called and no defaults are applied.

When `vulpea-create' is called with a nil TITLE (an untitled note,
see vulpea#399), this function receives nil - account for that if
you opt into untitled creation.  A returned `:title' still wins and
turns the note into a regular titled one."
  :type '(choice (const :tag "Use template instead" nil)
          (function :tag "Function returning plist"))
  :group 'vulpea)

(defcustom vulpea-create-default-template
  '(:file-name "${timestamp}_${slug}.org")
  "Default template (plist) for note creation.
Only used when `vulpea-create-default-function' is nil.
Parameters explicitly passed to `vulpea-create' override these defaults.

These defaults seed file-level note creation only.  When
`vulpea-create' is called with a non-nil `:parent' (a heading-level
note), no defaults are consulted and the heading is built solely
from the explicitly passed arguments.

Supports all template expansion features:
  ${var}     - Variable substitution
  %(elisp)   - Elisp evaluation
  %<format>  - Timestamp formatting

Default configuration:
  \\='(:file-name \"${timestamp}_${slug}.org\")

Example customization:

  (setq vulpea-create-default-template
        \\='(:file-name \"inbox/${slug}.org\"
          :tags (\"fleeting\")
          :head \"#+created: %<[%Y-%m-%d]>\"
          :properties ((\"CREATED\" . \"%<[%Y-%m-%d]>\")
                       (\"AUTHOR\" . \"%(user-full-name)\"))
          :context (:source \"manual\")))

Note: %(elisp) and %<format> directives are honored only inside
the template fields themselves (e.g. :head, :properties values).
Context values are inserted literally and are not re-evaluated.

Available parameters:
  :file-name   - File name template (relative to default directory)
                 Can also be a function: (lambda (title) ...)
  :tags        - List of tag strings
  :head        - Header content after #+filetags
  :body        - Note body content
  :properties  - Alist of (key . value) for property drawer
  :meta        - Alist of (key . value) for metadata
  :context     - Plist of custom template variables
  :title       - Note title. Takes precedence over the title passed to
                 `vulpea-create'. Mainly used to allow
                 `vulpea-create-default-function' to modify the note
                 title.

Template variables for :file-name:
  ${title}     - Note title
  ${slug}      - URL-friendly version of title
  ${timestamp} - Current timestamp (%Y%m%d%H%M%S)
  ${id}        - Note ID (UUID)"
  :type 'plist
  :group 'vulpea)

;;; Variables

(defvar vulpea-db-sync-directories)  ; Defined in vulpea-db
(defcustom vulpea-find-default-filter nil
  "Default filter to use in `vulpea-find'."
  :type '(choice (const :tag "No filter" nil) function)
  :group 'vulpea)

(defcustom vulpea-find-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-find'.

Must be a function that accepts one argument - optional note
filter function."
  :type 'function
  :group 'vulpea)

(defcustom vulpea-find-default-create-fn #'vulpea-find-create-note
  "Default function to create a note in `vulpea-find'.

Called with two arguments - the title typed by the user and
capture properties (currently always nil, reserved for future
use) - mirroring the CREATE-FN argument of `vulpea-insert'. It
should create the note and return the resulting `vulpea-note' to
visit, or nil to skip visiting (e.g. when creation is interactive,
asynchronous, or was aborted).

This is the hook for \"capture on empty\" workflows: set it to a
function that routes to `org-capture' or your own command to turn
a fruitless search straight into note creation."
  :type 'function
  :group 'vulpea)

;;; Helper Functions

(defun vulpea-title-to-slug (title)
  "Convert TITLE to URL-friendly slug.

Uses Unicode normalization to properly handle international characters
and diacritical marks. Implementation adapted from org-roam.

Credits: USAMI Kenta (@zonuexe)
See: https://github.com/org-roam/org-roam/pull/1460"
  (require 'ucs-normalize)
  (let ((slug-trim-chars
         ;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
         ;; For why these specific glyphs: https://github.com/org-roam/org-roam/pull/1460
         '( #x300 #x301 #x302 #x303 #x304 #x306 #x307
            #x308 #x309 #x30A #x30B #x30C #x31B #x323
            #x324 #x325 #x327 #x32D #x32E #x330 #x331)))
    (thread-last title
                 (ucs-normalize-NFD-string) ;; aka. `string-glyph-decompose' from Emacs 29
                 (seq-remove (lambda (char) (memq char slug-trim-chars)))
                 (apply #'string)
                 (ucs-normalize-NFC-string) ;; aka. `string-glyph-compose' from Emacs 29
                 (replace-regexp-in-string "[^[:alnum:]]" "_") ;; convert anything not alphanumeric
                 (replace-regexp-in-string "__*" "_")          ;; remove sequential underscores
                 (replace-regexp-in-string "^_" "")            ;; remove starting underscore
                 (replace-regexp-in-string "_$" "")            ;; remove ending underscore
                 (downcase))))

(define-obsolete-function-alias 'vulpea--title-to-slug #'vulpea-title-to-slug "2.0.0")

;;; Link Categorization

(defun vulpea--get-incoming-links-with-descriptions (note-id)
  "Get all links pointing to NOTE-ID with their descriptions.
Returns list of plists with :source-id :source-path :pos :description."
  (let* ((links (vulpea-db-query-links-to note-id))
         ;; Collect unique source IDs and batch fetch for paths
         (source-ids (delete-dups (mapcar (lambda (l) (plist-get l :source)) links)))
         (source-notes (vulpea-db-query-by-ids source-ids))
         ;; Build id->path lookup table
         (id-to-path (make-hash-table :test 'equal))
         result)
    (dolist (note source-notes)
      (puthash (vulpea-note-id note) (vulpea-note-path note) id-to-path))
    ;; Process links - description comes from database now
    (dolist (link links)
      (let* ((source-id (plist-get link :source))
             (source-path (gethash source-id id-to-path)))
        (when source-path
          (push (list :source-id source-id
                      :source-path source-path
                      :pos (plist-get link :pos)
                      :description (plist-get link :description))
                result))))
    (nreverse result)))

(defun vulpea--categorize-links (links old-title)
  "Categorize LINKS into exact and partial matches.
Case-insensitive matching against OLD-TITLE.
Returns plist (:exact :partial).

Exact matches: description equals old title (case-insensitive).
Partial matches: description contains old title but isn't exact.
Links using aliases are left unchanged (alias is still valid).
Links with nil descriptions or custom descriptions are excluded."
  (let ((exact '())
        (partial '())
        (title-down (downcase old-title)))
    (dolist (link links)
      (let ((desc (plist-get link :description)))
        (when desc
          (let ((desc-down (downcase desc)))
            (cond
             ;; Exact match (case-insensitive)
             ((string= desc-down title-down)
              (push link exact))
             ;; Partial match - contains but not exact
             ((string-match-p (regexp-quote title-down) desc-down)
              (push link partial)))))))
    (list :exact (nreverse exact)
          :partial (nreverse partial))))

(defun vulpea--update-link-description (file pos new-description)
  "Update link description at POS in FILE to NEW-DESCRIPTION.
Works for both bare links [[id:xxx]] and links with
descriptions [[id:xxx][old]]."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (cond
       ;; Link with existing description: [[id:xxx][old]]
       ((looking-at "\\(\\[\\[id:[^]]+\\]\\)\\[\\([^]]*\\)\\]\\]")
        (let ((link-part (match-string 1)))
          ;; LITERAL (3rd arg) so backslashes in NEW-DESCRIPTION are not
          ;; interpreted as match-group backreferences.
          (replace-match (concat link-part "[" new-description "]]") t t)))
       ;; Bare link without description: [[id:xxx]]
       ((looking-at "\\(\\[\\[id:[^]]+\\)\\]\\]")
        (let ((link-part (match-string 1)))
          (replace-match (concat link-part "][" new-description "]]") t t)))))))

(defun vulpea--default-directory ()
  "Return the default directory for creating new notes.

Resolution order:
  1. `vulpea-default-notes-directory' if set
  2. The sync directory holding `default-directory', so a note is
     born in the corpus being visited
  3. First directory from `vulpea-db-sync-directories' if set
  4. `org-directory' as fallback"
  (or vulpea-default-notes-directory
      (vulpea-db-sync-directory-of)
      (car vulpea-db-sync-directories)
      org-directory))

(defun vulpea--expand-template (template title &optional id context)
  "Expand TEMPLATE with TITLE, optional ID, and CONTEXT.
TEMPLATE is a string with placeholders:
  ${var}     - Variable substitution
  %(elisp)   - Elisp evaluation
  %<format>  - Timestamp formatting

CONTEXT is a plist of additional variables (e.g., :url \"...\").
Returns expanded string with placeholders replaced.

Built-in variables: ${title}, ${slug}, ${timestamp}, ${id}
Context variables: ${key} for each :key in CONTEXT.

Evaluation order matters for safety: the %(elisp) and %<format>
directives are expanded first, on the template itself, and only
then are ${var} and context values substituted in.  Consequently
%(...) and %<...> are honored only when written by the template
author - they are NOT re-evaluated when they appear inside a
substituted value such as TITLE or a CONTEXT value.  This keeps
untrusted data (e.g. a note title) from being executed as code.

Note: Does not support %a (annotation) or %i (initial content)
from org-capture as they don't make sense for programmatic creation.

TITLE may be nil (untitled notes, see vulpea#399); a template
referencing ${title} or ${slug} then signals `user-error', since
there is nothing to substitute."
  (let* ((slug (when title (vulpea-title-to-slug title)))
         (timestamp (format-time-string "%Y%m%d%H%M%S"))
         (id (or id (org-id-new)))
         (result template))

    ;; SECURITY: expand the active directives (%(elisp) and %<format>)
    ;; on the raw template FIRST, before substituting ${var} and context
    ;; values.  The directives are written by the template author and are
    ;; trusted; the substituted values (note title, slug, id, context)
    ;; are data and may be untrusted.  Substituting first and scanning
    ;; afterwards would let a value containing "%(...)" be evaluated as
    ;; code - an arbitrary code execution hazard.  Expanding before
    ;; substitution keeps all substituted data strictly literal.

    ;; Expand %(elisp) - evaluate elisp expressions
    ;; Note: save-match-data is critical because eval'd expressions may
    ;; call functions that do string matching, corrupting our match data
    (while (string-match "%\\((.+?)\\)" result)
      (let* ((expr (match-string 1 result))
             (match-beg (match-beginning 0))
             (match-end (match-end 0))
             (value (save-match-data
                      (condition-case err
                          (eval (car (read-from-string expr)))
                        (error (format "ERROR: %S" err))))))
        (setq result (concat (substring result 0 match-beg)
                             (format "%s" value)
                             (substring result match-end)))))

    ;; Expand %<format> - format timestamps
    (while (string-match "%<\\(.+?\\)>" result)
      (let* ((format-str (match-string 1 result))
             (value (format-time-string format-str)))
        (setq result (replace-match value t t result))))

    ;; Expand ${var} placeholders.  Done AFTER directive expansion so
    ;; substituted values are never re-scanned for %(...) or %<...>.
    (if title
        (setq result (thread-last result
                                  (s-replace "${title}" title)
                                  (s-replace "${slug}" slug)))
      (when (or (s-contains-p "${title}" result)
                (s-contains-p "${slug}" result))
        (user-error
         "Cannot expand %S: ${title} and ${slug} are unavailable for a note without a title; pass an explicit file name or use a title-free template"
         template)))
    (setq result (thread-last result
                              (s-replace "${timestamp}" timestamp)
                              (s-replace "${id}" id)))

    ;; Expand context variables (treated as literal data, like ${var})
    (when context
      (let ((ctx context))
        (while ctx
          (let* ((key (pop ctx))
                 (val (pop ctx))
                 (placeholder (format "${%s}" (substring (symbol-name key) 1))))
            (setq result (s-replace placeholder (format "%s" val) result))))))

    result))

(defun vulpea--expand-file-name-template (title &optional id template context)
  "Expand file name template with TITLE, ID, TEMPLATE, and CONTEXT.
If TEMPLATE is nil, uses `:file-name' from `vulpea-create-default-template'.
CONTEXT is a plist of additional template variables.
Returns absolute file path."
  (let* ((template (or template
                       (plist-get vulpea-create-default-template :file-name)
                       "${slug}.org"))  ; Absolute fallback
         (template-resolved (if (functionp template)
                                (funcall template title)
                              template))
         (file-name (vulpea--expand-template template-resolved title id context))
         (dir (vulpea--default-directory)))
    (expand-file-name file-name dir)))

(defun vulpea--format-note-content (id title &optional head meta tags properties)
  "Format note content for `org-capture' template.

ID is required.  TITLE may be nil for an untitled note (see
vulpea#399): no `#+title' line is written at all, so extraction
falls back to the file base name with title-source `filename'.
Optional: HEAD, META (alist), TAGS (list), PROPERTIES (alist)."
  (string-join
   (append
    (list
     ":PROPERTIES:"
     (format org-property-format ":ID:" id))
    (mapcar
     (lambda (prop)
       (format org-property-format
               (concat ":" (car prop) ":")
               (cdr prop)))
     properties)
    (list ":END:")
    (when title
      (list (format "#+title: %s" title)))
    (when tags
      (list (concat "#+filetags: :"
                    (string-join tags ":")
                    ":")))
    (when head (list head))
    (when meta
      (list ""))  ; blank line before meta
    (when meta
      (mapcar
       (lambda (kvp)
         (if (listp (cdr kvp))
             (mapconcat
              (lambda (val)
                (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format val)))
              (cdr kvp) "\n")
           (concat "- " (car kvp) " :: " (vulpea-buffer-meta-format (cdr kvp)))))
       meta)))
   "\n"))


(defun vulpea-find-create-note (title &optional _props)
  "Create a new note with TITLE selected in `vulpea-find'.

Creates a file-level note via `vulpea-create' and returns the
resulting `vulpea-note'. PROPS mirrors the capture properties
argument of `vulpea-insert' CREATE-FN and is currently unused.

This is the default value of `vulpea-find-default-create-fn'."
  (vulpea-create title))

;;;###autoload
(cl-defun vulpea-find (&key other-window
                            filter-fn
                            candidates-fn
                            create-fn
                            require-match
                            (expand-aliases t))
  "Select and find a note.

If OTHER-WINDOW, visit the NOTE in another window.

CANDIDATES-FN is the function to query candidates for selection,
which takes as its argument a filtering function (see FILTER-FN).
Unless specified, `vulpea-find-default-candidates-source' is
used.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'. Unless specified,
`vulpea-find-default-filter' is used.

CREATE-FN controls how a new note is created when user selects a
non-existent note (only possible when REQUIRE-MATCH is nil). Like
the CREATE-FN of `vulpea-insert', it is called with two arguments
- the typed title and capture properties (currently always nil).
It should return the created `vulpea-note' to visit, or nil to
skip visiting. Unless specified, `vulpea-find-default-create-fn'
is used.

When REQUIRE-MATCH is nil user may select a non-existent note,
which is then created via CREATE-FN. When non-nil, only existing
notes may be selected.

A note can be selected by typing its id: ids are matchable in
completion (see `vulpea-select-match-ids'), which makes them handy
handles when titles are incidental or absent (vulpea#400).

When EXPAND-ALIASES is non-nil (the default), each note with
aliases will appear multiple times in the completion list - once
for the original title and once for each alias."
  (interactive)
  (let* ((region-text
          (when (region-active-p)
            (org-link-display-format
             (buffer-substring-no-properties
              (set-marker
               (make-marker) (region-beginning))
              (set-marker
               (make-marker) (region-end))))))
         (note (vulpea-select-from
                "Note"
                (funcall
                 (or
                  candidates-fn
                  vulpea-find-default-candidates-source)
                 (or
                  filter-fn
                  vulpea-find-default-filter))
                :require-match require-match
                :initial-prompt region-text
                :expand-aliases expand-aliases)))
    (if (vulpea-note-id note)
        ;; Existing note - visit it
        (vulpea-visit note other-window)
      ;; New note - create it
      (when (not require-match)
        (let ((new-note (funcall (or create-fn
                                     vulpea-find-default-create-fn)
                                 (vulpea-note-title note)
                                 nil)))
          (when new-note
            (vulpea-visit new-note other-window)))))))

;;;###autoload
(defun vulpea-find-backlink ()
  "Select and find a note linked to current note.

Point lands on the first link pointing back to the current note,
so you see the mention itself instead of the beginning of the
selected note. When the link cannot be found in the buffer (e.g.
the file changed since the last sync), point stays at the note."
  (interactive)
  (let* ((id (or (org-entry-get nil "ID" t)
                 (user-error "Current location has no ID property")))
         (_ (unless (vulpea-db-get-by-id id)
              (user-error
               "%s is not a known note" id)))
         (backlinks (vulpea-db-query-by-links-some
                     (list (cons "id" id)))))
    (unless backlinks
      (user-error "There are no backlinks to the current note"))
    (let ((note (vulpea-select-from "Note" backlinks
                                    :require-match t
                                    :expand-aliases t)))
      (when (vulpea-note-id note)
        (vulpea-visit note)
        ;; Land on the link itself rather than the beginning of the note
        (when (re-search-forward
               (format "\\[\\[id:%s[]\\[]" (regexp-quote id))
               nil t)
          (goto-char (match-beginning 0))
          (vulpea--show-context))))))



(defun vulpea--show-context ()
  "Reveal the org context around point."
  (if (fboundp 'org-fold-show-context)
      (org-fold-show-context)
    ;; Fallback for Org < 9.6; the function is obsolete there but
    ;; still the correct entry point, so silence the warning.
    (with-suppressed-warnings ((obsolete org-show-context))
      (org-show-context))))

;;;###autoload
(defun vulpea-visit (note-or-id &optional other-window)
  "Visit NOTE-OR-ID.

If OTHER-WINDOW, visit the NOTE in another window."
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id))))
    (unless note
      (user-error "Cannot find note with ID: %s"
                  (if (vulpea-note-p note-or-id)
                      (vulpea-note-id note-or-id)
                    note-or-id)))
    (let ((file (vulpea-note-path note))
          (id (vulpea-note-id note)))
      ;; Visit the file
      (if (or current-prefix-arg other-window)
          (find-file-other-window file)
        (find-file file))
      ;; Go to the note position
      (if (= (vulpea-note-level note) 0)
          ;; File-level note: go to beginning
          (goto-char (point-min))
        ;; Heading-level note: search for the ID property
        (goto-char (point-min))
        (unless (re-search-forward
                 (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id))
                 nil t)
          (user-error "Could not find heading with ID: %s" id))
        ;; Move to the heading
        (org-back-to-heading t))
      (vulpea--show-context))))



(defcustom vulpea-insert-default-filter nil
  "Default filter to use in `vulpea-insert'."
  :type '(choice (const :tag "No filter" nil) function)
  :group 'vulpea)

(defcustom vulpea-insert-default-candidates-source #'vulpea-db-query
  "Default source to get the list of candidates in `vulpea-insert'.

Must be a function that accepts one argument - optional note
filter function."
  :type 'function
  :group 'vulpea)

(defcustom vulpea-insert-default-create-fn nil
  "Default function to create a note in `vulpea-insert'.

When non-nil, used as the CREATE-FN of `vulpea-insert' for a note
that does not exist yet (see its CREATE-FN argument): it is called
with the typed title and capture properties and is responsible for
both creating the note and inserting the link. When nil, the
built-in behavior is used (create via `vulpea-create' and insert
an id: link).

This mirrors `vulpea-find-default-create-fn' for \"capture on
empty\" workflows. Note that, unlike the `vulpea-find' hook, this
function must perform the link insertion itself, since inserting a
link is what `vulpea-insert' does with a new note. If you would
rather only create the note and let `vulpea-insert' handle the
link, use `vulpea-insert-default-note-fn' instead; when both
variables are set, the note-fn wins."
  :type '(choice (const :tag "Built-in behavior" nil) function)
  :group 'vulpea)

(defcustom vulpea-insert-default-note-fn nil
  "Default function to create a note in `vulpea-insert'.

When non-nil, used as the NOTE-FN of `vulpea-insert' for a note
that does not exist yet (see its NOTE-FN argument): it is called
with the typed title and capture properties (currently always
nil) and should return the created `vulpea-note'; `vulpea-insert'
then replaces the selected region, inserts the link and runs
`vulpea-insert-handle-functions' itself. Return nil to skip link
insertion.

Unlike `vulpea-insert-default-create-fn', the function is not
responsible for inserting the link - this is the same contract as
`vulpea-find-default-create-fn'. When both variables are set,
this one wins."
  :type '(choice (const :tag "Built-in behavior" nil) function)
  :group 'vulpea)

(defvar vulpea-insert-handle-functions nil
  "Abnormal hooks to run after `vulpea-note' is inserted.

Each function accepts a note that was inserted via
`vulpea-insert'.

The current point is the point of the new node. The hooks must
not move the point.

This is an extension point, not a setting: attach to it with
`add-hook', which is why it is deliberately not a `defcustom'.")

(defcustom vulpea-insert-default-description-fn #'vulpea-note-title
  "Function computing the link description in `vulpea-insert'.

Called with the inserted `vulpea-note' and returns the string
used as the description of the inserted id link. A selected
region always wins over this function; taking the value from the
note otherwise means a title rewritten during creation (e.g. by
`vulpea-create-default-function') is respected. The default uses
the note title.

The id is always the link target, so it stays hidden behind
whatever description this returns - and it is available as a
description itself. For a note without an explicit title (see
`vulpea-note-titled-p') the title falls back to the file base
name, which may be noise; you might then prefer the id or a
combination:

  ;; use the id (handy for structured ids like person:lectia)
  (setq vulpea-insert-default-description-fn #\\='vulpea-note-id)

  ;; title and id together
  (setq vulpea-insert-default-description-fn
        (lambda (note)
          (format \"%s (%s)\"
                  (vulpea-note-title note)
                  (vulpea-note-id note))))

Return nil or an empty string to insert a bare id link with no
description."
  :type 'function
  :group 'vulpea)

(defun vulpea--insert-note-link (note region-text beg end)
  "Insert a link to NOTE at point and run insert hooks.

When REGION-TEXT is non-nil, the region between markers BEG and
END is deleted first and REGION-TEXT becomes the link
description. Otherwise the description is computed by
`vulpea-insert-default-description-fn' (the title of NOTE by
default); when it returns nil or an empty string a bare id link
is inserted.

After the link is inserted, `vulpea-insert-handle-functions' are
called with NOTE."
  (when region-text
    (delete-region beg end)
    (set-marker beg nil)
    (set-marker end nil))
  (insert (org-link-make-string
           (concat "id:" (vulpea-note-id note))
           (or region-text
               (funcall vulpea-insert-default-description-fn note))))
  (run-hook-with-args 'vulpea-insert-handle-functions note))

;;;###autoload
(cl-defun vulpea-insert (&key filter-fn candidates-fn create-fn note-fn
                              (expand-aliases t))
  "Select a note and insert a link to it.

Allows capturing new notes. After link is inserted,
`vulpea-insert-handle-functions' are called with the inserted
note as the only argument regardless involvement of capture
process.

CANDIDATES-FN is the function to query candidates for selection,
which takes as its argument a filtering function (see FILTER-FN).
Unless specified, `vulpea-insert-default-candidates-source' is
used.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'. Unless specified,
`vulpea-insert-default-filter' is used.

CREATE-FN allows to control how a new note is created when user picks a
non-existent note. This function is called with two arguments - title
and capture properties - and owns the whole flow: it must create the
note and insert the link itself. When CREATE-FN is nil,
`vulpea-insert-default-create-fn' is used; when that is also nil, the
default implementation is used.

NOTE-FN is an alternative to CREATE-FN with the same calling
convention, but it only creates and returns a `vulpea-note' -
`vulpea-insert' then replaces the selected region, inserts the
link with a proper description and runs
`vulpea-insert-handle-functions', exactly as for an existing
note. Return nil to skip link insertion. This mirrors the
CREATE-FN contract of `vulpea-find'. When NOTE-FN is nil,
`vulpea-insert-default-note-fn' is used.

Passing both NOTE-FN and CREATE-FN is an error. An explicit
argument beats both default variables; when only the defaults are
set, `vulpea-insert-default-note-fn' wins over
`vulpea-insert-default-create-fn'.

A note can be selected by typing its id: ids are matchable in
completion (see `vulpea-select-match-ids'), which makes them handy
handles when titles are incidental or absent (vulpea#400). The
link description of the inserted note is the region text when a
region is active, otherwise the value of
`vulpea-insert-default-description-fn', which defaults to the note
title.

When EXPAND-ALIASES is non-nil (the default), each note with
aliases will appear multiple times in the completion list - once
for the original title and once for each alias."
  (interactive)
  (when (and note-fn create-fn)
    (error "vulpea-insert: Specify either NOTE-FN or CREATE-FN, not both"))
  (unwind-protect
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq
                     beg (set-marker
                          (make-marker) (region-beginning))
                     end (set-marker
                          (make-marker) (region-end))
                     region-text
                     (org-link-display-format
                      (buffer-substring-no-properties
                       beg end)))))
               (notes (funcall (or candidates-fn
                                   vulpea-insert-default-candidates-source)
                               (or filter-fn vulpea-insert-default-filter)))
               (note (vulpea-select-from "Note" notes
                                         :initial-prompt region-text
                                         :expand-aliases expand-aliases)))
          (if (vulpea-note-id note)
              ;; Existing note - insert link immediately
              (vulpea--insert-note-link note region-text beg end)
            ;; New note - create it then insert link. An explicit
            ;; argument beats both defaults, so an explicit CREATE-FN
            ;; suppresses `vulpea-insert-default-note-fn'.
            (let ((nfn (or note-fn (unless create-fn
                                     vulpea-insert-default-note-fn)))
                  (cfn (or create-fn vulpea-insert-default-create-fn)))
              (cond
               ;; note-fn contract: it returns the note (or nil to
               ;; skip), core inserts the link
               (nfn
                (let ((new-note (funcall nfn (vulpea-note-title note) nil)))
                  (when new-note
                    (vulpea--insert-note-link new-note region-text beg end))))
               ;; create-fn contract: it owns the whole flow,
               ;; including link insertion
               (cfn
                (funcall cfn (vulpea-note-title note) nil))
               ;; Create the note programmatically
               (t
                (vulpea--insert-note-link
                 (vulpea-create (vulpea-note-title note))
                 region-text beg end)))))))
    (deactivate-mark)))



(defun vulpea--format-heading-content (level id title &optional tags properties body)
  "Format a heading-level note content.

LEVEL is the heading depth (number of stars).
ID and TITLE are required.
TAGS is a list of tag strings (inserted as headline tags).
PROPERTIES is an alist of (key . value) for the property drawer.
BODY is optional body text after the property drawer."
  (string-join
   (append
    ;; Heading line with optional tags
    (list (concat (make-string level ?*)
                  " "
                  title
                  (when tags
                    (concat " :" (string-join tags ":") ":"))))
    ;; Property drawer
    (list ":PROPERTIES:"
          (format org-property-format ":ID:" id))
    (mapcar
     (lambda (prop)
       (format org-property-format
               (concat ":" (car prop) ":")
               (cdr prop)))
     properties)
    (list ":END:")
    ;; Body
    (when body (list body)))
   "\n"))

(defun vulpea--find-heading-insertion-point (parent-note _level after)
  "Move point to where a new child heading should be inserted.

PARENT-NOTE is the parent vulpea-note.
_LEVEL is accepted for call-site symmetry but is not used; the
insertion point is derived from PARENT-NOTE.
AFTER controls position:
  \\='last (default) - append as last child
  nil - insert as first child
  string - insert after the child heading with that ID.

Return non-nil when the new heading will follow an existing sibling
subtree (so a separating blank line is wanted), and nil when it will
be the first child or the first heading in the file.  This function
only positions point; surrounding blank lines are managed by
`vulpea--insert-heading-content'."
  (let ((parent-level (vulpea-note-level parent-note)))
    (cond
     ;; Insert as first child
     ((null after)
      (if (= parent-level 0)
          ;; File-level parent: position before the first heading
          (progn
            (goto-char (point-min))
            (if (re-search-forward "^\\*+ " nil t)
                (goto-char (match-beginning 0))
              (goto-char (point-max))))
        ;; Heading parent: position after the parent's property drawer
        (goto-char (vulpea-note-pos parent-note))
        (forward-line 1)
        ;; Skip past property drawer if present
        (when (looking-at-p "[ \t]*:PROPERTIES:")
          (re-search-forward "^[ \t]*:END:" nil t)
          (forward-line 1)))
      nil)

     ;; Insert after specific sibling
     ((stringp after)
      (let ((sibling-pos (org-id-find after 'marker)))
        (unless sibling-pos
          (error "vulpea-create: Sibling note with ID %s not found" after))
        (goto-char sibling-pos)
        ;; Move past the sibling's entire subtree
        (org-end-of-subtree t))
      t)

     ;; Insert as last child (default)
     (t
      (if (= parent-level 0)
          ;; File-level parent: append at end of file
          (let ((has-sibling (save-excursion
                               (goto-char (point-min))
                               (and (re-search-forward "^\\*+ " nil t) t))))
            (goto-char (point-max))
            has-sibling)
        ;; Heading parent: append at end of the parent's subtree
        (let* ((parent-pos (vulpea-note-pos parent-note))
               (subtree-end (save-excursion
                              (goto-char parent-pos)
                              (org-end-of-subtree t)
                              (point)))
               (has-sibling (save-excursion
                              (goto-char parent-pos)
                              (forward-line 1)
                              (and (re-search-forward "^\\*+ " subtree-end t) t))))
          (goto-char parent-pos)
          (org-end-of-subtree t)
          has-sibling))))))

(defun vulpea--insert-heading-content (content blank-before)
  "Insert heading CONTENT at point with normalized blank lines.

CONTENT is heading text with no leading or trailing blank lines.
Any whitespace already surrounding point is removed first so the
result is deterministic.  When there is preceding content, CONTENT
starts on its own line, preceded by exactly one blank line when
BLANK-BEFORE is non-nil and none otherwise.  CONTENT is followed by
a single newline, which also guarantees a single trailing newline
when inserting at the end of the buffer."
  (skip-chars-backward " \t\n")
  (let ((preceding (not (bobp)))
        (following (save-excursion
                     (skip-chars-forward " \t\n")
                     (point))))
    (delete-region (point) following)
    (when preceding
      (insert "\n")
      (when blank-before (insert "\n")))
    (insert content "\n")))

;;;###autoload
(cl-defun vulpea-create (title
                         &optional file-name
                         &key
                         id
                         head
                         meta
                         body
                         context
                         properties
                         tags
                         parent
                         (after 'last))
  "Create a new note with TITLE programmatically.

This function is designed for programmatic note creation with
immediate finalization. For interactive note capture with user
editing, use `org-capture' with vulpea-compatible templates.

FILE-NAME is optional. When nil, uses `:file-name' from
`vulpea-create-default-template' to generate the file name.
Ignored when PARENT is provided (file is determined by parent).

Defaults from `vulpea-create-default-function' and
`vulpea-create-default-template' are applied only when creating a
file-level note (PARENT is nil).  When PARENT is provided no
defaults are consulted; the heading is built solely from the
arguments passed here.

If `:title' is present in the template, it takes precedence over the
TITLE argument. This allows the function set in
`vulpea-create-default-function' to override the title based on the
template or other logic.

Returns the created `vulpea-note' object.

ID is automatically generated unless explicitly passed.

When PARENT is nil, creates a file-level note:

  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  #+title: TITLE
  #+filetags: TAGS if present
  HEAD if present

  META if present

  BODY if present

When PARENT is a `vulpea-note', creates a heading-level note
inside the parent's file at level (parent-level + 1):

  * TITLE :tags:
  :PROPERTIES:
  :ID: ID
  PROPERTIES if present
  :END:
  BODY if present

AFTER controls insertion position among siblings (only when
PARENT is provided):
  \\='last (default) - append as last child
  nil - insert as first child
  string (note ID) - insert after the child with that ID

Optional parameters:

- PROPERTIES: Alist of (key_str . val_str) for property drawer
- META: Alist of (key . value) or (key . (list of values))
- TAGS: List of tag strings
- BODY: Note body content (supports template expansion)
- HEAD: Additional header content (supports template expansion)
- CONTEXT: Plist of template variables (e.g., :url \"...\")

Template expansion is supported in FILE-NAME, HEAD, BODY, TAGS,
PROPERTIES values, and META values:
  ${var}     - Variable substitution (title, slug, timestamp, id, custom)
  %(elisp)   - Elisp evaluation (e.g., %(user-full-name))
  %<format>  - Timestamp formatting (e.g., %<[%Y-%m-%d]>)

Note: Does not support %a or %i from org-capture.

TITLE must be a string, or nil to create an untitled file-level
note (see vulpea#399): no `#+title' line is written, so extraction
falls back to the file base name and records title-source
`filename'.  The returned note mirrors that post-extraction state.
With a nil TITLE the file name template cannot reference ${title}
or ${slug} - pass an explicit FILE-NAME or use a title-free
template; a clear `user-error' is signaled otherwise (same for a
:file-name function template, which is then called with nil, and
for `vulpea-create-default-function').  When PARENT is provided,
TITLE stays mandatory - the heading text is the title - and any
non-string signals `user-error' (see vulpea#379)."
  (unless (or (stringp title)
              (and (null title) (not parent)))
    (user-error
     (if parent
         "Heading note title must be a string, got %S; the heading text is the title, so heading-level notes cannot be untitled"
       "Note title must be a string or nil, got %S")
     title))
  (let* ((id (or id (org-id-new)))
         (context (or context nil)))
    (if parent
        ;; Heading-level note creation
        (vulpea--create-heading title id parent after
                               body tags properties context)
      ;; File-level note creation (original behavior)
      (vulpea--create-file title file-name id head meta body
                           tags properties context))))

(defun vulpea--create-file (title file-name id head meta body tags properties context)
  "Create a file-level note with TITLE.

TITLE may be nil for an untitled note (see `vulpea-create').
FILE-NAME, ID, HEAD, META, BODY, TAGS, PROPERTIES, and CONTEXT
are as documented in `vulpea-create'."
  ;; Get defaults from function or template
  (let* ((defaults (cond
                    (vulpea-create-default-function
                     (funcall vulpea-create-default-function title))
                    (vulpea-create-default-template
                     vulpea-create-default-template)
                    (t nil)))
         ;; Merge explicit parameters with defaults (explicit takes precedence)
         (title (or (plist-get defaults :title) title))
         (file-name (or file-name (plist-get defaults :file-name)))
         (head (or head (plist-get defaults :head)))
         (body (or body (plist-get defaults :body)))
         (tags (or tags (plist-get defaults :tags)))
         (properties (or properties (plist-get defaults :properties)))
         (meta (or meta (plist-get defaults :meta)))
         (context (or context (plist-get defaults :context)))
         (file-path (vulpea--expand-file-name-template title id file-name context))
         ;; Expand templates everywhere with context
         (expanded-head (when head
                          (vulpea--expand-template head title id context)))
         (expanded-body (when body
                          (vulpea--expand-template body title id context)))
         (expanded-tags (when tags
                          (mapcar (lambda (tag)
                                    (vulpea--expand-template tag title id context))
                                  tags)))
         (expanded-properties (when properties
                                (mapcar (lambda (prop)
                                          (cons (car prop)
                                                (vulpea--expand-template (cdr prop) title id context)))
                                        properties)))
         (expanded-meta (when meta
                          (mapcar (lambda (kvp)
                                    (cons (car kvp)
                                          (if (listp (cdr kvp))
                                              ;; List of values
                                              (mapcar (lambda (val)
                                                        (if (stringp val)
                                                            (vulpea--expand-template val title id context)
                                                          val))
                                                      (cdr kvp))
                                            ;; Single value
                                            (if (stringp (cdr kvp))
                                                (vulpea--expand-template (cdr kvp) title id context)
                                              (cdr kvp)))))
                                  meta)))
         (content (vulpea--format-note-content id title expanded-head expanded-meta expanded-tags expanded-properties))
         (full-content (if expanded-body
                           (concat content "\n\n" expanded-body)
                         content))
         (dir (file-name-directory file-path)))

    ;; Ensure directory exists
    (unless (file-directory-p dir)
      (make-directory dir t))

    ;; Safety check: refuse to overwrite existing files
    (when (file-exists-p file-path)
      (error "vulpea-create: File %s already exists; refusing to overwrite" file-path))

    ;; Write file directly (no org-capture, no hooks, no blank lines)
    (with-temp-buffer
      (insert full-content)
      (write-region (point-min) (point-max) file-path nil 'silent))

    ;; Register ID with org-id so links can be followed
    (org-id-add-location id file-path)

    ;; Update database with the new file
    (let ((update-count (vulpea-db-update-file file-path)))
      (when (zerop update-count)
        (error "vulpea-create: No notes extracted from file %s (expected ID %s)"
               file-path id)))

    ;; Return the note
    (or (vulpea-db-get-by-id id)
        (error "vulpea-create: Note with ID %s not found in database after creation" id))))

(defun vulpea--create-heading (title id parent after body tags properties context)
  "Create a heading-level note with TITLE under PARENT.

ID is the note identifier.
PARENT is the parent `vulpea-note'.
AFTER controls insertion position (see `vulpea-create').
BODY, TAGS, PROPERTIES, and CONTEXT are as in `vulpea-create'."
  (let* ((file-path (vulpea-note-path parent))
         (level (1+ (vulpea-note-level parent)))
         ;; Expand templates
         (expanded-body (when body
                          (vulpea--expand-template body title id context)))
         (expanded-tags (when tags
                          (mapcar (lambda (tag)
                                    (vulpea--expand-template tag title id context))
                                  tags)))
         (expanded-properties (when properties
                                (mapcar (lambda (prop)
                                          (cons (car prop)
                                                (vulpea--expand-template (cdr prop) title id context)))
                                        properties)))
         (heading-content (vulpea--format-heading-content
                           level id title expanded-tags
                           expanded-properties expanded-body)))

    ;; Validate parent file exists
    (unless (file-exists-p file-path)
      (error "vulpea-create: Parent file %s does not exist" file-path))

    ;; Insert heading into parent's file
    (with-current-buffer (find-file-noselect file-path)
      (org-with-wide-buffer
       (let ((blank-before (vulpea--find-heading-insertion-point
                            parent level after)))
         (vulpea--insert-heading-content (string-trim-right heading-content)
                                         blank-before)))
      (save-buffer))

    ;; Register ID with org-id
    (org-id-add-location id file-path)

    ;; Update database
    (vulpea-db-update-file file-path)

    ;; Return the note
    (or (vulpea-db-get-by-id id)
        (error "vulpea-create: Heading note with ID %s not found in database after creation" id))))



;;; Title Change Detection Mode

(defvar-local vulpea--title-before-save nil
  "Title of note before save, for change detection.")

(defvar-local vulpea--note-id-before-save nil
  "ID of note before save, for change detection.")

(defun vulpea--capture-before-save ()
  "Capture note ID and title before save for change detection.
The old title is read from the database, not the buffer."
  (when (derived-mode-p 'org-mode)
    (setq vulpea--note-id-before-save (org-entry-get nil "ID"))
    (setq vulpea--title-before-save
          (when vulpea--note-id-before-save
            (caar (emacsql (vulpea-db)
                           [:select title :from notes :where (= id $s1)]
                           vulpea--note-id-before-save))))))

(defun vulpea--notify-title-change ()
  "After save, check if title changed and notify user."
  (when (and vulpea--note-id-before-save
             vulpea--title-before-save
             (derived-mode-p 'org-mode))
    (let ((new-title (vulpea-buffer-title-get)))
      (when (and new-title
                 (not (string= new-title vulpea--title-before-save)))
        (message
         (concat "Title changed from \"%s\" to \"%s\". "
                 "Run M-x vulpea-propagate-title-change to update.")
         vulpea--title-before-save new-title)))))

;;;###autoload
(define-minor-mode vulpea-title-change-detection-mode
  "Minor mode to detect title changes and notify user.

When enabled, this mode tracks the note's title before each save.
After saving, if the title has changed, it notifies the user and
suggests running `vulpea-propagate-title-change' to update incoming
link descriptions."
  :lighter " VulpTD"
  :group 'vulpea
  (if vulpea-title-change-detection-mode
      (progn
        (add-hook 'before-save-hook #'vulpea--capture-before-save nil t)
        (add-hook 'after-save-hook #'vulpea--notify-title-change nil t))
    (remove-hook 'before-save-hook #'vulpea--capture-before-save t)
    (remove-hook 'after-save-hook #'vulpea--notify-title-change t)))

;;; Title Propagation Command

;;;###autoload
(cl-defun vulpea-propagate-title-change (&optional note-or-id)
  "Propagate title change for NOTE-OR-ID to filename and links.

With prefix arg (\\[universal-argument]), preview changes without
applying (dry-run).

When called interactively:
- Determines the note from current buffer or prompts user
- Prompts for old title if not recently detected
- Offers to rename the file based on new title
- Updates exact-match link descriptions to new title
- Shows partial matches for manual review

Interactive flow:
1. Prompt for file rename (y/n)
2. For exact matches: [!] Update all, [r] Review, [s] Skip, [q] Quit
3. Partial matches shown with option to open files"
  (interactive)
  (let* ((dry-run current-prefix-arg)
         ;; Determine the note
         (note (cond
                ((vulpea-note-p note-or-id) note-or-id)
                ((stringp note-or-id) (vulpea-db-get-by-id note-or-id))
                (t (when-let* ((id (org-entry-get nil "ID")))
                     (vulpea-db-get-by-id id)))))
         (note (or note
                   (vulpea-select "Note to propagate")))
         (note-id (vulpea-note-id note))
         (new-title (vulpea-note-title note))
         ;; Get old title - from detection or prompt
         (old-title
          (or vulpea--title-before-save
              (read-string (format "Old title (new: \"%s\"): " new-title))))
         ;; Get incoming links
         (links (vulpea--get-incoming-links-with-descriptions note-id))
         (categorized (vulpea--categorize-links links old-title))
         (exact-links (plist-get categorized :exact))
         (partial-links (plist-get categorized :partial))
         (exact-count (length exact-links))
         (partial-count (length partial-links)))

    ;; Check if title actually changed
    (when (string= old-title new-title)
      (user-error "Title has not changed (\"%s\")" new-title))

    ;; Dry-run: just show summary
    (when dry-run
      (with-output-to-temp-buffer "*vulpea-propagate-preview*"
        (princ (format "Title propagation preview for: %s\n" note-id))
        (princ (format "Old title: %s\n" old-title))
        (princ (format "New title: %s\n\n" new-title))
        (princ (format "File rename: %s → %s\n\n"
                       (file-name-nondirectory (vulpea-note-path note))
                       (concat (vulpea-title-to-slug new-title) ".org")))
        (princ (format "Exact matches (%d):\n" exact-count))
        (dolist (link exact-links)
          (princ (format "  %s at pos %d: \"%s\"\n"
                         (file-name-nondirectory (plist-get link :source-path))
                         (plist-get link :pos)
                         (plist-get link :description))))
        (princ (format "\nPartial matches (%d):\n" partial-count))
        (dolist (link partial-links)
          (princ (format "  %s at pos %d: \"%s\"\n"
                         (file-name-nondirectory (plist-get link :source-path))
                         (plist-get link :pos)
                         (plist-get link :description)))))
      (message "Dry-run complete. See *vulpea-propagate-preview* buffer.")
      (cl-return-from vulpea-propagate-title-change))

    ;; Offer file rename for file-level notes
    (when (and (= (vulpea-note-level note) 0)
               (y-or-n-p
                (format "Rename file \"%s\" → \"%s\"? "
                        (file-name-nondirectory (vulpea-note-path note))
                        (concat (vulpea-title-to-slug new-title) ".org"))))
      (condition-case err
          (vulpea-rename-file note new-title)
        (error (message "File rename failed: %s" (error-message-string err)))))

    ;; Handle exact matches
    (when (> exact-count 0)
      (message "Found %d exact match%s, %d partial match%s"
               exact-count (if (= exact-count 1) "" "es")
               partial-count (if (= partial-count 1) "" "es"))
      (let ((action
             (read-char-choice
              (format "Exact (%d): [!] All  [r] Review  [s] Skip  [q] Quit: "
                      exact-count)
              '(?! ?r ?s ?q))))
        (pcase action
          (?! ;; Update all exact matches
           (dolist (link exact-links)
             (vulpea--update-link-description
              (plist-get link :source-path)
              (plist-get link :pos)
              new-title)
             (when-let* ((buf (get-file-buffer (plist-get link :source-path))))
               (with-current-buffer buf
                 (save-buffer))))
           (message "Updated %d link%s"
                    exact-count (if (= exact-count 1) "" "s")))
          (?r ;; Review individually
           (let ((updated 0))
             (dolist (link exact-links)
               (let ((path (plist-get link :source-path))
                     (pos (plist-get link :pos))
                     (desc (plist-get link :description)))
                 (when (y-or-n-p (format "Update \"%s\" in %s? "
                                         desc (file-name-nondirectory path)))
                   (vulpea--update-link-description path pos new-title)
                   (when-let* ((buf (get-file-buffer path)))
                     (with-current-buffer buf
                       (save-buffer)))
                   (cl-incf updated))))
             (message "Updated %d of %d link%s"
                      updated exact-count (if (= exact-count 1) "" "s"))))
          (?s ;; Skip exact matches
           (message "Skipped exact matches"))
          (?q ;; Quit
           (user-error "Aborted")))))

    ;; Handle partial matches
    (when (> partial-count 0)
      (when (y-or-n-p
             (format "Open %d file%s with partial matches for editing? "
                     partial-count (if (= partial-count 1) "" "s")))
        (let ((files (delete-dups
                      (mapcar (lambda (l) (plist-get l :source-path))
                              partial-links))))
          (dolist (file files)
            (find-file-other-window file)))))

    ;; Clear detection state
    (setq vulpea--title-before-save nil)

    (message "Title propagation complete.")))

;;;###autoload
(defun vulpea-rename-file (note-or-id new-title)
  "Rename NOTE-OR-ID's file based on NEW-TITLE slug.
Updates the file on disk and database.

The new filename is generated as NEW-TITLE converted to slug with
.org extension, placed in the same directory as the original file.

Returns the new file path.

Signals an error if:
- The note cannot be found
- The target file already exists
- The note is a heading-level note (level > 0)"
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id)))
         (old-path (when note (vulpea-note-path note)))
         (dir (when old-path (file-name-directory old-path)))
         (new-filename (concat (vulpea-title-to-slug new-title) ".org"))
         (new-path (when dir (expand-file-name new-filename dir))))
    (unless note
      (error "vulpea-rename-file: Cannot find note with ID: %s"
             (if (vulpea-note-p note-or-id)
                 (vulpea-note-id note-or-id)
               note-or-id)))
    (when (> (vulpea-note-level note) 0)
      (error "vulpea-rename-file: Cannot rename file for heading-level note"))
    (when (file-exists-p new-path)
      (error "vulpea-rename-file: Target file already exists: %s" new-path))
    (vulpea--relocate-file note new-path)))

(defun vulpea--relocate-file (note new-path)
  "Move NOTE's file to NEW-PATH.

Saves and kills the file's buffer when it is open, moves the file on
disk, points `org-id' at the new location, and replaces the old file's
notes in the database with the new file's.

This is the shared mechanical half of `vulpea-rename-file' and
`vulpea-move-file': it performs no validation, so callers must have
checked NOTE and NEW-PATH already.

Returns NEW-PATH."
  (let ((old-path (vulpea-note-path note)))
    ;; Kill buffer if file is open
    (let ((buf (get-file-buffer old-path)))
      (when buf
        (with-current-buffer buf
          (save-buffer))
        (kill-buffer buf)))
    ;; Move file on disk
    (rename-file old-path new-path)
    ;; Update org-id location
    (org-id-add-location (vulpea-note-id note) new-path)
    ;; Forget the old path and add the new one.  Forget rather than
    ;; just drop its notes: nothing is at the old path any more, so a
    ;; file that later appears there is a new file, not an unchanged
    ;; one.
    (vulpea-db--forget-file old-path)
    (vulpea-db-update-file new-path)
    new-path))

(defun vulpea--note-directories ()
  "Return directories that currently hold notes.

The result is the directories of all file-level notes in the database,
unioned with `vulpea-db-sync-directories' themselves so that an empty
but configured vault root is still offered."
  (seq-uniq
   (append
    (vulpea-db--note-directories)
    (seq-map (lambda (dir)
               (file-name-as-directory (expand-file-name dir)))
             vulpea-db-sync-directories))
   #'string-equal))

(defun vulpea--buffer-file-note ()
  "Return the file-level note of the current buffer, or nil.

Deliberately resolves the file rather than the ID at point: moving or
renaming a file is a file-level operation, while point usually sits
inside some heading, and heading IDs are indexed too."
  (when-let* ((path (buffer-file-name)))
    (car (vulpea-db-query-by-file-path path 0))))

(defun vulpea--ensure-id ()
  "Ensure the note at point carries an id, creating one when missing.

The target is the heading at point, or the file when point is before
the first heading.  A created id is registered in `org-id-locations'
when the buffer visits a file.  A blank `:ID:' property counts as
missing, matching `org-id-get-create'.  Returns the id."
  (or (org-string-nw-p (org-entry-get (point) "ID"))
      (let ((id (org-id-new)))
        (org-entry-put (point) "ID" id)
        (when-let* ((file (buffer-file-name (buffer-base-buffer))))
          (org-id-add-location id file))
        id)))

;;;###autoload
(defun vulpea-move-file (note-or-id directory)
  "Move NOTE-OR-ID's file into DIRECTORY.

The file name is left alone: this moves the note, it does not rename
it.  Use `vulpea-rename-file' for the other half.

Links are not touched, because they are ids and not paths, so nothing
needs rewriting.  The file is moved on disk, `org-id' is pointed at the
new location and the database is updated.

When called interactively, the note is the one the current buffer
visits (the file, never a heading inside it), and DIRECTORY is
completed over directories that already hold notes.  The completion is
not restricted to those, so a directory that exists but holds no notes
yet can be typed in and confirmed.

When the note's file is visited by a buffer, that buffer is saved and
replaced by one visiting the new location, with point kept.

Returns the new file path.

Signals a `user-error' if:
- The note cannot be found
- The note is a heading-level note (level > 0)
- The note's file is gone from disk (a stale database row)
- DIRECTORY does not exist or is not writable
- DIRECTORY is outside `vulpea-db-sync-directories' (when any are
  configured), since the note would silently fall out of the database
  on the next full scan
- The note already lives in DIRECTORY
- A file of the same name already exists in DIRECTORY"
  (interactive
   (let* ((note (or (vulpea--buffer-file-note)
                    (vulpea-select "Note to move" :require-match t)))
          (dir (completing-read
                (format "Move \"%s\" to directory: " (vulpea-note-title note))
                (vulpea--note-directories)
                nil 'confirm)))
     (when (string-empty-p (string-trim dir))
       (user-error "vulpea-move-file: No directory given"))
     (list note dir)))
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id)))
         (old-path (when note (vulpea-note-path note)))
         (dir (file-name-as-directory (expand-file-name directory)))
         (new-path (when old-path
                     (expand-file-name (file-name-nondirectory old-path) dir))))
    (unless note
      (user-error "vulpea-move-file: Cannot find note with ID: %s"
                  (if (vulpea-note-p note-or-id)
                      (vulpea-note-id note-or-id)
                    note-or-id)))
    (when (> (vulpea-note-level note) 0)
      (user-error
       "vulpea-move-file: Cannot move file for heading-level note"))
    (unless (and old-path (file-exists-p old-path))
      (user-error "vulpea-move-file: File does not exist: %s" old-path))
    (unless (file-directory-p dir)
      (user-error "vulpea-move-file: Directory does not exist: %s" dir))
    (when (and vulpea-db-sync-directories
               (not (vulpea-db-sync-tracked-file-p dir)))
      (user-error
       "vulpea-move-file: %s is outside `vulpea-db-sync-directories'" dir))
    (when (string-equal (file-truename (file-name-directory old-path))
                        (file-truename dir))
      (user-error "vulpea-move-file: Note already lives in %s" dir))
    (when (file-exists-p new-path)
      (user-error "vulpea-move-file: Target file already exists: %s" new-path))
    (unless (file-writable-p new-path)
      (user-error "vulpea-move-file: Directory is not writable: %s" dir))
    ;; Remember how the note was being looked at, so the move does not
    ;; just kill the buffer from under the user.
    (let* ((buffer (get-file-buffer old-path))
           (selected (eq buffer (window-buffer (selected-window))))
           (old-point (when buffer
                        (with-current-buffer buffer (point))))
           (moved (vulpea--relocate-file note new-path)))
      (when buffer
        (let ((new-buffer (find-file-noselect moved)))
          (with-current-buffer new-buffer
            (goto-char (min old-point (point-max))))
          (when selected
            (switch-to-buffer new-buffer))))
      (message "Moved \"%s\" to %s" (vulpea-note-title note) dir)
      moved)))

(defun vulpea-split--heading-facts-at-point ()
  "Return what the split needs to know about the heading at point.

The heading is the one point sits inside.  Read widened, so a
narrowed buffer does not cut the subtree short.  The keys are
:raw-title, :todo, :priority, :commented, :planning, :logbook and
:body."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((components (org-heading-components))
         (section-end (save-excursion (outline-next-heading) (point))))
     (list :raw-title (org-get-heading t t t t)
           :todo (nth 2 components)
           :priority (nth 3 components)
           :commented (org-in-commented-heading-p)
           :planning (or (org-entry-get nil "SCHEDULED")
                         (org-entry-get nil "DEADLINE")
                         (org-entry-get nil "CLOSED"))
           ;; Only the heading's own drawer: a child keeps its
           ;; heading, and with it a logbook that stays valid.
           :logbook (save-excursion
                      (and (re-search-forward
                            "^[ \t]*\\(:LOGBOOK:\\|CLOCK:[ \t]+\\[\\)"
                            section-end t)
                           t))
           :body (vulpea-split--extract-body
                  (buffer-substring-no-properties
                   (point)
                   (save-excursion (org-end-of-subtree t t) (point))))))))

(defun vulpea-split--heading-facts (note)
  "Return what the split needs to know about NOTE's heading, or nil.

Everything is read from the file in one widened pass.  Widened, because
`org-find-entry-with-id' searches the whole buffer while `goto-char'
clamps to the current restriction, so in a narrowed buffer the position
of one heading can land point on another.  From the file rather than
from the database, because a database row describes the last save: a
todo keyword or planning line added since would otherwise slip past the
checks that exist to catch them.

See `vulpea-split--heading-facts-at-point' for the keys.  Returns nil
when the heading is not in the file."
  (with-current-buffer (find-file-noselect (vulpea-note-path note))
    (org-with-wide-buffer
     (when-let* ((pos (org-find-entry-with-id (vulpea-note-id note))))
       (goto-char pos)
       (vulpea-split--heading-facts-at-point)))))

(defun vulpea-split--check-splittable (facts)
  "Signal a `user-error' when FACTS cannot become a file-level note.

A file-level note carries no todo keyword, priority, planning
information or logbook, and cannot be commented out, so none of them
would survive the move.  Refuse rather than drop them silently."
  (when (plist-get facts :todo)
    (user-error
     "vulpea-split-heading: Cannot split a heading with a todo keyword"))
  (when (plist-get facts :planning)
    (user-error
     "vulpea-split-heading: Cannot split a heading with planning info"))
  (when (plist-get facts :priority)
    (user-error
     "vulpea-split-heading: Cannot split a heading with a priority"))
  (when (plist-get facts :commented)
    (user-error
     "vulpea-split-heading: Cannot split a commented heading"))
  ;; A clock entry belongs to an entry.  In a file it parses as a
  ;; plain drawer that `org-clock-sum' does not count, so the time
  ;; would survive as text and stop being time.
  (when (plist-get facts :logbook)
    (user-error
     "vulpea-split-heading: Cannot split a heading with a logbook")))

(defun vulpea-split--extract-body (text)
  "Return TEXT, a subtree, as the body of a file-level note.

Strips the heading line, its planning line and its property drawer, all
of which are rebuilt at the file level, then promotes the children so
the shallowest one lands at level 1.

Meta is deliberately left where it is: it already sits at the start of
the section, which is exactly where file-level meta belongs, so moving
it through `vulpea-note-meta' and back would only risk dropping or
duplicating lines that the two do not agree about."
  (let ((text text))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert text)
      (goto-char (point-min))
      ;; The heading itself becomes the file: drop its line, its
      ;; planning line and its drawer.  A planning line sits between
      ;; the heading and the drawer, so skipping it is what keeps the
      ;; drawer findable.
      (delete-region (point) (progn (forward-line 1) (point)))
      (when (looking-at-p org-planning-line-re)
        (delete-region (point) (progn (forward-line 1) (point))))
      (when (looking-at-p "^[ \t]*:PROPERTIES:")
        (delete-region (point)
                       (progn (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                              (forward-line 1)
                              (point))))
      ;; Children follow their parent, promoted as a block so the
      ;; shallowest lands at level 1.  Promotion goes through org so
      ;; that level gaps and `org-odd-levels-only' are its problem and
      ;; not ours.
      (let ((shallowest most-positive-fixnum)
            tops)
        (goto-char (point-min))
        (while (re-search-forward org-outline-regexp-bol nil t)
          (setq shallowest (min shallowest (org-current-level))))
        (when (< shallowest most-positive-fixnum)
          (goto-char (point-min))
          (while (re-search-forward org-outline-regexp-bol nil t)
            (beginning-of-line)
            (when (= (org-current-level) shallowest)
              (push (point) tops))
            (end-of-line))
          ;; Bottom-up, so promoting one subtree cannot invalidate the
          ;; position of an earlier one, and a promoted descendant is
          ;; never mistaken for a top-level sibling.
          (dolist (pos tops)
            (goto-char pos)
            (while (> (org-current-level) 1)
              (org-promote-subtree)))))
      (string-trim (buffer-string)))))

(defun vulpea-split--write-file (path id title head tags properties body)
  "Write a file-level note to PATH and return it.

ID, TITLE, HEAD, TAGS, PROPERTIES and BODY are written verbatim.  This
deliberately does not go through `vulpea-create': that expands its
arguments as templates, which would execute `%(...)' found in the
content being moved and rewrite anything looking like `${var}', and it
merges in `vulpea-create-default-template' values that have no business
in a note that already exists."
  (let ((content (vulpea--format-note-content id title head nil tags
                                              properties)))
    (with-temp-buffer
      (insert (if (string-empty-p body)
                  content
                (concat content "\n\n" body))
              "\n")
      (write-region (point-min) (point-max) path nil 'silent)))
  path)

(defun vulpea-split--category-head (note)
  "Return a `#+category' line carrying NOTE's category, or nil.

Category resolves through the file when a note does not set it, so a
heading leaving its file would silently take the new file's name as its
category.  Carrying it over keeps the note answering the same queries.
The file name fallback itself is not carried: it describes the old file
rather than the note."
  (let ((category (vulpea-note-category note)))
    (when (and category
               ;; Already in the drawer, which travels with the note.
               (not (assoc "CATEGORY" (vulpea-note-properties note)))
               (not (equal category
                           (file-name-base (vulpea-note-path note)))))
      (concat "#+category: " category))))

(defun vulpea-split--read-directory (title initial)
  "Prompt for the directory to split TITLE into, seeded with INITIAL.

Completion runs over directories that already hold notes, without
being restricted to them.  Signals a `user-error' on a blank answer."
  (let ((dir (completing-read
              (format "Split \"%s\" into directory: " title)
              (vulpea--note-directories)
              nil 'confirm initial)))
    (when (string-empty-p (string-trim dir))
      (user-error "vulpea-split-heading: No directory given"))
    dir))

(defun vulpea-split--note-at-point ()
  "Return the heading at point as a note the database knows.

The heading is given an id when it has none, the buffer is saved and
the file indexed, so a heading the database has not seen yet - just
typed, or not yet picked up by sync - still resolves.  The buffer's
base buffer is the one saved, so an indirect buffer works too.

A row sending the id to another file is not trusted blindly: when
that file is gone it is forgotten, and when it exists it is
re-indexed first, so only its current content may keep the claim.
This settles the cut-and-paste case, where the subtree moved here
and the database still remembers its old home.

Signals a `user-error' when the id genuinely identifies another
living note - links resolve by id, so neither side can just take it
over - and when the heading does not index even after all that,
which means it is excluded: by
`vulpea-db-note-index-filter-functions', by the property named in
`vulpea-db-exclude-property', as archived, or because heading
indexing is off (see `vulpea-db-index-heading-level')."
  (let ((id (vulpea--ensure-id))
        (path (buffer-file-name (buffer-base-buffer))))
    (with-current-buffer (or (buffer-base-buffer) (current-buffer))
      (when (buffer-modified-p)
        (save-buffer)))
    ;; Settle who owns the id before indexing this file: insertion
    ;; keeps the first claim, so a stale row for another file would
    ;; shadow the heading at point.
    (when-let* ((other (vulpea-db-get-by-id id))
                (other-path (vulpea-note-path other)))
      (unless (string-equal (file-truename other-path)
                            (file-truename path))
        (if (file-exists-p other-path)
            ;; A file that cannot be parsed (unreadable, an encrypted
            ;; file whose passphrase is declined) keeps its claim and
            ;; degrades to the refusal below, rather than aborting.
            (ignore-errors (vulpea-db-update-file other-path))
          (vulpea-db--forget-file other-path))))
    (vulpea-db-update-file path)
    (let ((note (vulpea-db-get-by-id id)))
      (when (and note
                 (not (string-equal (file-truename (vulpea-note-path note))
                                    (file-truename path))))
        (user-error
         (concat "vulpea-split-heading: Id at point already identifies"
                 " another note in %s; save that file if the subtree just"
                 " moved here, or give one of them a fresh id")
         (vulpea-note-path note)))
      (unless (and note (> (vulpea-note-level note) 0))
        (user-error
         (concat "vulpea-split-heading: Heading at point is excluded"
                 " from indexing (a filter, `vulpea-db-exclude-property',"
                 " an archive tag, or `vulpea-db-index-heading-level')")))
      note)))

;;;###autoload
(defun vulpea-split-heading (note-or-id &optional directory leave-link)
  "Extract NOTE-OR-ID's subtree into a file-level note of its own.

DIRECTORY is where the new file lands, defaulting to the directory of
the file the heading currently lives in.  The file name is the title
slug, as for any other created note.

The heading becomes the file: its text becomes the title, its own tags
and the ones it inherited become `#+filetags', its property drawer and
meta move to the file level.  Children follow and are promoted so the
shallowest lands at level 1; a child with an id of its own stays a
heading note and keeps that id rather than being split in turn.  The
subtree is removed from the source file.

With LEAVE-LINK non-nil (interactively, a prefix argument) the subtree
is replaced by a heading of the same level whose text is a link to the
new note, instead of being removed.  The stub carries no id of its own,
so it stays a pointer rather than becoming a note.  Useful when reading
order in the source file matters; the default suits extracting many
headings at once, where a stub per heading is only noise.

Tag inheritance follows `org-use-tag-inheritance': what a heading
inherits is what vulpea already reports as its tags, and leaving those
behind would drop the note out of queries that used to find it.

The note keeps its id, so links into it need no rewriting.

When called interactively with point inside a heading - in an org
buffer visiting a file - that heading is the target, whether or not
the database knows it yet: a missing id is created, the buffer saved
and the file indexed first.  The refusals below are checked before
any of that, so a refused heading is left exactly as it was.
Elsewhere - another buffer, or a file's preamble - the heading is
picked from the database.

Returns the created `vulpea-note'.

Signals a `user-error' if:
- The note cannot be found
- The note is a file-level note (level 0), so there is nothing to
  extract
- The heading carries a todo keyword, a priority, planning
  information, a logbook, or is commented out, none of which a
  file-level note can hold
- The note's file no longer holds the heading (a stale database row)
- DIRECTORY is not writable
- The title has no file name to slug (punctuation only, say)
- DIRECTORY does not exist, or is outside `vulpea-db-sync-directories'
  when any are configured
- A file of that name already exists in DIRECTORY
- Interactively: the file at point is outside
  `vulpea-db-sync-directories' when any are configured, or the
  heading at point is excluded from indexing"
  (interactive
   (if (and (derived-mode-p 'org-mode)
            (buffer-file-name (buffer-base-buffer))
            (not (org-before-first-heading-p)))
       ;; Point sits inside a heading: that heading is the target,
       ;; whether or not the database knows it yet (#450).
       (let* ((path (buffer-file-name (buffer-base-buffer)))
              ;; The database's answer counts only when it describes
              ;; this very heading: a row at another level or in
              ;; another file is stale, or a duplicated id.  Those
              ;; are re-resolved from disk below, which refuses a
              ;; true duplicate by name.
              (known (when-let* ((id (org-entry-get nil "ID"))
                                 (note (vulpea-db-get-by-id id)))
                       (and (> (vulpea-note-level note) 0)
                            (string-equal
                             (file-truename (vulpea-note-path note))
                             (file-truename path))
                            note))))
         (when (and vulpea-db-sync-directories
                    (not (vulpea-db-sync-tracked-file-p path)))
           (user-error
            "vulpea-split-heading: File %s is outside `vulpea-db-sync-directories'"
            path))
         ;; Refuse before prompting and before minting an id, so a
         ;; heading that cannot split is left exactly as it was.
         (vulpea-split--check-splittable (vulpea-split--heading-facts-at-point))
         (let ((dir (vulpea-split--read-directory
                     (if known
                         (vulpea-note-title known)
                       (org-get-heading t t t t))
                     (file-name-directory path))))
           (list (or known (vulpea-split--note-at-point))
                 dir current-prefix-arg)))
     ;; Elsewhere - another buffer, or a file's preamble - any heading
     ;; note can be picked.
     (let* ((note (vulpea-select "Heading to split out"
                                 :require-match t
                                 :filter-fn (lambda (note)
                                              (> (vulpea-note-level note) 0))))
            (dir (vulpea-split--read-directory
                  (vulpea-note-title note)
                  (file-name-directory (vulpea-note-path note)))))
       (list note dir current-prefix-arg))))
  (let* ((note (if (vulpea-note-p note-or-id)
                   note-or-id
                 (vulpea-db-get-by-id note-or-id)))
         (source-path (when note (vulpea-note-path note)))
         (dir (when (or directory source-path)
                (file-name-as-directory
                 (expand-file-name (or directory
                                       (file-name-directory source-path))))))
         (slug (when note (vulpea-title-to-slug (vulpea-note-title note))))
         (new-path (when (and note dir slug (not (string-empty-p slug)))
                     (expand-file-name (concat slug ".org") dir))))
    (unless note
      (user-error "vulpea-split-heading: Cannot find note with ID: %s"
                  (if (vulpea-note-p note-or-id)
                      (vulpea-note-id note-or-id)
                    note-or-id)))
    (when (= (vulpea-note-level note) 0)
      (user-error
       "vulpea-split-heading: Note is already a file, expected a heading"))
    (unless (and source-path (file-exists-p source-path))
      (user-error "vulpea-split-heading: File does not exist: %s" source-path))
    (when (string-empty-p slug)
      (user-error
       "vulpea-split-heading: Title \"%s\" has no file name to slug"
       (vulpea-note-title note)))
    (unless (file-directory-p dir)
      (user-error "vulpea-split-heading: Directory does not exist: %s" dir))
    (when (and vulpea-db-sync-directories
               (not (vulpea-db-sync-tracked-file-p dir)))
      (user-error
       "vulpea-split-heading: %s is outside `vulpea-db-sync-directories'"
       dir))
    (when (file-exists-p new-path)
      (user-error "vulpea-split-heading: Target file already exists: %s"
                  new-path))
    (unless (file-writable-p new-path)
      (user-error "vulpea-split-heading: Directory is not writable: %s" dir))
    (let ((facts (vulpea-split--heading-facts note)))
      (unless facts
        (user-error
         "vulpea-split-heading: Heading is not in %s (stale database row)"
         source-path))
      ;; The facts are read from the file, not from the database, so an
      ;; edit made since the last index is still seen by the checks.
      (vulpea-split--check-splittable facts)
      (let (;; The raw heading, not the note title: the database stores
            ;; the display form, so a title holding a link or emphasis
            ;; would lose it here and nowhere keep it.
            (title (plist-get facts :raw-title))
            (id (vulpea-note-id note))
            (tags (vulpea-note-tags note))
            (head (vulpea-split--category-head note))
            (properties (seq-remove (lambda (kvp) (equal (car kvp) "ID"))
                                    (vulpea-note-properties note)))
            (body (plist-get facts :body)))
      ;; Write first: if anything below fails the source still holds the
      ;; subtree, so the worst case is a duplicate rather than a loss.
      (vulpea-split--write-file new-path id title head tags properties body)
      (condition-case err
          (with-current-buffer (find-file-noselect source-path)
            ;; Widened, for the same reason the facts were read widened.
            (org-with-wide-buffer
             (goto-char (org-find-entry-with-id id))
             (org-back-to-heading t)
             (let ((level (org-current-level)))
               (org-cut-subtree)
               (when leave-link
                 ;; The note's title, not the raw heading: a raw
                 ;; heading can hold a link, and a link inside a link
                 ;; description is not a link.
                 (insert (make-string level ?*) " "
                         (org-link-make-string
                          (concat "id:" id)
                          (vulpea-note-title note))
                         "\n"))))
            (save-buffer))
        (error
         (delete-file new-path)
         (signal (car err) (cdr err))))
      ;; Source first, so the heading's row is gone before the new file
      ;; claims the same id.
      (vulpea-db-update-file source-path)
      (org-id-add-location id new-path)
      (vulpea-db-update-file new-path)
      (message "Split \"%s\" into %s" title new-path)
      (vulpea-db-get-by-id id)))))

(defun vulpea-merge--file-body (path)
  "Return the body of the file-level note in PATH.

Everything before the body belongs to the note as a note rather than as
content: the property drawer, the keywords and the meta.  Those are
merged into the target separately, so they must not travel twice."
  (with-current-buffer (find-file-noselect path)
    (org-with-wide-buffer
     (goto-char (point-min))
     ;; Both boundaries are org's answer rather than a line pattern of
     ;; our own.  A pattern for the leading keywords cannot tell
     ;; `#+title: x' from `#+begin_src elisp :tangle yes', and a
     ;; pattern for the meta disagrees with org about wrapped items,
     ;; which copies meta into the body as well as merging it.
     (let* ((meta-pl (plist-get (vulpea-buffer-meta) :pl))
            (meta-beg (when meta-pl (org-element-property :begin meta-pl)))
            (meta-end (when meta-pl (org-element-property :end meta-pl)))
            (start (progn
                     (goto-char (point-min))
                     (let ((element (org-element-at-point)))
                       ;; Walk off the note's own preamble: the drawer
                       ;; and the keywords, nothing else.
                       (while (and (memq (org-element-type element)
                                         '(property-drawer keyword comment))
                                   (< (point) (point-max)))
                         (goto-char (org-element-property :end element))
                         (setq element (org-element-at-point)))
                       (point)))))
       (string-trim
        (if (and meta-beg (>= meta-beg start))
            ;; Meta is merged separately, so it is cut out of the body
            ;; while whatever sits on either side of it is kept.
            (concat
             (buffer-substring-no-properties start meta-beg)
             "\n"
             (buffer-substring-no-properties (min meta-end (point-max))
                                             (point-max)))
          (buffer-substring-no-properties start (point-max))))))))

(defun vulpea-merge--demote (text)
  "Return TEXT with every heading demoted one level.

The merged body hangs under a heading carrying the source's title, so
its own headings have to make room.  Every heading is shifted by the
same amount, which keeps the tree's shape exactly: relative depth,
level gaps and all.

Deliberately not `org-demote-subtree' per top-level heading, the way
`vulpea-split-heading' promotes.  Promotion moves later siblings out of
an earlier sibling's subtree, so doing it one subtree at a time is
safe; demotion moves them in, so the second sibling becomes a child of
the first and every later one is demoted again with it."
  (if (string-empty-p text)
      text
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert text)
      (let ((stars (make-string (if org-odd-levels-only 2 1) ?*))
            positions)
        (goto-char (point-min))
        (while (re-search-forward org-outline-regexp-bol nil t)
          (push (match-beginning 0) positions))
        ;; Bottom-up, so an earlier position cannot be invalidated.
        (dolist (pos positions)
          (goto-char pos)
          (insert stars)))
      (string-trim (buffer-string)))))

(defun vulpea-merge--meta (path)
  "Return the meta of the file-level note in PATH, in document order.

Read from the buffer rather than from `vulpea-note-meta', which hands
its keys back in reverse (vulpea#409)."
  (with-current-buffer (find-file-noselect path)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((meta (vulpea-buffer-meta)))
       (seq-map (lambda (key)
                  (cons key (vulpea-buffer-meta-get-list! meta key 'string)))
                (seq-uniq (vulpea-buffer-meta-props meta) #'string-equal))))))

(defun vulpea-merge--title-survives-as-heading-p (title)
  "Return non-nil when TITLE survives being put in a heading.

The source's title becomes a heading, and a heading line is not plain
text: a title like \"TODO list\" is read as a todo keyword plus the
title \"list\", one ending in `:word:' is read as tags, and one
starting with COMMENT hides the subtree.  Each of those changes the
note's meaning rather than its looks, so a merge that would do it is
refused instead."
  (and title
       (not (string-empty-p title))
       (with-temp-buffer
         (delay-mode-hooks (org-mode))
         (insert "* " title "\n")
         (goto-char (point-min))
         (let ((components (org-heading-components)))
           (and (null (nth 2 components))       ; todo keyword
                (null (nth 3 components))       ; priority
                (null (nth 5 components))       ; tags
                (not (org-in-commented-heading-p))
                (equal (org-get-heading t t t t) title))))))

(defun vulpea-merge--linking-paths (id)
  "Return the files holding a link to ID, without duplicates."
  (seq-uniq (seq-map #'vulpea-note-path
                     (vulpea-db-query-by-links-some (list id)))
            #'string-equal))

(defun vulpea-merge--repoint-links (from-id to-id paths)
  "Rewrite every `id:' link to FROM-ID in PATHS so it points at TO-ID.

Descriptions are left alone: only the target of the link changes, so a
link that read the old note's name still reads it."
  (let ((paths (seq-filter #'file-exists-p (seq-uniq paths #'string-equal))))
    (dolist (path paths)
      (with-current-buffer (find-file-noselect path)
        (let ((rewrote nil))
          (org-with-wide-buffer
           ;; Every match has to end at the id, or an id that merely
           ;; starts with FROM-ID gets its prefix swapped: merging
           ;; `note' would rewrite a link to `note-2' into a link to
           ;; nothing.
           (goto-char (point-min))
           (while (re-search-forward
                   (concat "\\[\\[id:" (regexp-quote from-id) "\\(\\]\\|::\\)")
                   nil t)
             (replace-match (concat "[[id:" to-id (match-string 1)) t t)
             (setq rewrote t))
           ;; Plain links too.  They are ordinary links to org and the
           ;; database records them, so leaving them alone leaves them
           ;; pointing at a note that no longer exists.  The bracketed
           ;; pass has already run, so nothing is rewritten twice.
           (goto-char (point-min))
           (while (re-search-forward
                   (concat "\\_<id:" (regexp-quote from-id) "\\_>")
                   nil t)
             (replace-match (concat "id:" to-id) t t)
             (setq rewrote t)))
          ;; Only save what was actually touched, so unrelated unsaved
          ;; work in a linking buffer is not committed by a merge.
          (when (and rewrote (buffer-modified-p))
            (save-buffer))
          (when rewrote
            (vulpea-db-update-file path)))))
    paths))

;;;###autoload
(defun vulpea-merge (source-or-id target-or-id)
  "Fold SOURCE-OR-ID into TARGET-OR-ID, leaving one note.

The source's body is appended to the target under a heading carrying
the source's title, with the source's own headings demoted to sit under
it.  This is the inverse of `vulpea-split-heading', so a merge can be
undone by splitting that heading back out.

Everything else the source was is folded in too: its tags are unioned
into the target's, its meta is merged key by key keeping both values
where they differ, and its title becomes an alias of the target so the
old name still resolves.

Links into the source are re-pointed at the target, descriptions
untouched, so nothing dangles.  The source file is then deleted.

Both notes must be file-level.  Merging heading-level notes is a
different operation on both sides and is refused for now.

Returns the target `vulpea-note', as it is after the merge.

Signals a `user-error' if either note cannot be found, either is a
heading-level note, they are the same note, either file is missing, or
the target is not writable."
  (interactive
   (let* ((source (vulpea-select "Merge note"
                                 :require-match t
                                 :filter-fn (lambda (note)
                                              (= (vulpea-note-level note) 0))))
          (target (vulpea-select (format "Merge \"%s\" into"
                                         (vulpea-note-title source))
                                 :require-match t
                                 :filter-fn (lambda (note)
                                              (and (= (vulpea-note-level note) 0)
                                                   (not (equal
                                                         (vulpea-note-id note)
                                                         (vulpea-note-id source))))))))
     (list source target)))
  (let* ((source (if (vulpea-note-p source-or-id)
                     source-or-id
                   (vulpea-db-get-by-id source-or-id)))
         (target (if (vulpea-note-p target-or-id)
                     target-or-id
                   (vulpea-db-get-by-id target-or-id))))
    (unless source
      (user-error "vulpea-merge: Cannot find note with ID: %s"
                  (if (vulpea-note-p source-or-id)
                      (vulpea-note-id source-or-id)
                    source-or-id)))
    (unless target
      (user-error "vulpea-merge: Cannot find note with ID: %s"
                  (if (vulpea-note-p target-or-id)
                      (vulpea-note-id target-or-id)
                    target-or-id)))
    (when (equal (vulpea-note-id source) (vulpea-note-id target))
      (user-error "vulpea-merge: Cannot merge a note into itself"))
    (when (or (> (vulpea-note-level source) 0)
              (> (vulpea-note-level target) 0))
      (user-error
       "vulpea-merge: Both notes must be file-level, got levels %s and %s"
       (vulpea-note-level source) (vulpea-note-level target)))
    (let ((source-path (vulpea-note-path source))
          (target-path (vulpea-note-path target)))
      (unless (file-exists-p source-path)
        (user-error "vulpea-merge: File does not exist: %s" source-path))
      (unless (file-exists-p target-path)
        (user-error "vulpea-merge: File does not exist: %s" target-path))
      (unless (file-writable-p target-path)
        (user-error "vulpea-merge: File is not writable: %s" target-path))
      (unless (vulpea-merge--title-survives-as-heading-p
               (or (with-current-buffer (find-file-noselect source-path)
                     (org-with-wide-buffer (vulpea-buffer-title-get)))
                   (vulpea-note-title source)))
        (user-error
         "vulpea-merge: Source title \"%s\" would not read as itself in a heading"
         (vulpea-note-title source)))
      (let* ((title (or (with-current-buffer (find-file-noselect source-path)
                          (org-with-wide-buffer (vulpea-buffer-title-get)))
                        ;; A note with no `#+title' still has one, from
                        ;; its file name.
                        (vulpea-note-title source)))
             ;; Collected before anything moves: once the source is gone
             ;; the database can no longer answer who linked to it.
             (link-paths (cons target-path
                               (vulpea-merge--linking-paths
                                (vulpea-note-id source))))
             (tags (vulpea-note-tags source))
             ;; Its title is only one of the names the source answered
             ;; to; the rest have to survive too, or the promise that
             ;; the old name still resolves holds for one name only.
             (aliases (vulpea-note-aliases source))
             (meta (vulpea-merge--meta source-path))
             (body (vulpea-merge--demote
                    (vulpea-merge--file-body source-path)))
             (id (vulpea-note-id target))
             (restore (progn
                        ;; Snapshot what is on disk, so the target has
                        ;; to be on disk first: otherwise a rollback
                        ;; would restore a version of the file that
                        ;; predates the user's unsaved work and revert
                        ;; the buffer on top of it.
                        (when-let* ((buf (get-file-buffer target-path)))
                          (with-current-buffer buf
                            (when (buffer-modified-p)
                              (save-buffer))))
                        (with-temp-buffer
                          (insert-file-contents target-path)
                          (buffer-string)))))
        ;; The target takes everything on before the source goes away.
        ;; If that fails the target is put back as it was, so a merge
        ;; either happens or leaves no trace.
        (condition-case err
            (with-current-buffer (find-file-noselect target-path)
              (org-with-wide-buffer
               ;; Tags and aliases are written at point, and widening
               ;; does not move it.  Without this they land on whatever
               ;; heading the target buffer happens to be sitting in.
               (goto-char (point-min))
               (when tags
                 (vulpea-buffer-tags-add tags))
               (dolist (alias (cons title aliases))
                 (when (and alias
                            (not (string-empty-p alias))
                            ;; A note listing its own title as an alias
                            ;; is noise, not a second name.
                            (not (string-equal alias
                                               (vulpea-note-title target))))
                   (vulpea-buffer-alias-add alias)))
               (dolist (kvp meta)
                 (let* ((key (car kvp))
                        (merged (seq-uniq
                                 (append
                                  (vulpea-buffer-meta-get-list key 'string)
                                  (cdr kvp))
                                 #'string-equal)))
                   (vulpea-buffer-meta-set key merged 'append)))
               (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (insert "\n* " title "\n")
               (unless (string-empty-p body)
                 (insert body "\n")))
              (save-buffer))
          (error
           (with-temp-buffer
             (insert restore)
             (write-region (point-min) (point-max) target-path nil 'silent))
           (when-let* ((buf (get-file-buffer target-path)))
             (with-current-buffer buf
               (set-buffer-modified-p nil)
               (revert-buffer t t t)))
           (signal (car err) (cdr err))))
        ;; The target holds everything now, so the source can go.  It
        ;; has to go before the target is indexed: the merged body
        ;; carries the source's heading ids, and a note row already
        ;; claiming one of them would make the new row be ignored.
        (let ((buf (get-file-buffer source-path)))
          (when buf
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))
        (delete-file source-path)
        (vulpea-db--forget-file source-path)
        (vulpea-db-update-file target-path)
        ;; Re-point last, including inside the target itself, since the
        ;; merged body may have brought links to the note it came from.
        (vulpea-merge--repoint-links (vulpea-note-id source) id link-paths)
        (message "Merged \"%s\" into \"%s\"" title (vulpea-note-title target))
        (vulpea-db-get-by-id id)))))



;;; Schema authoring

(defun vulpea--schema-buffer-note (&optional schema)
  "Build a synthetic `vulpea-note' from the note at point.

The note carries the title and tags of the note at point - the heading
when point is inside one, otherwise the file.  When SCHEMA is given, it
also carries the current values of that schema's field keys within the
same scope, so predicates, conditional :required / :one-of rules and the
missing-field computation see in-buffer content while authoring."
  (make-vulpea-note
   :title (if (org-before-first-heading-p)
              (vulpea-buffer-title-get)
            (substring-no-properties (org-get-heading t t t t)))
   :tags (vulpea-buffer-tags-get)
   :meta (when schema
           (delq nil
                 (mapcar
                  (lambda (field)
                    (let* ((key (plist-get field :key))
                           (vals (vulpea-buffer-meta-get-list key 'string 'heading)))
                      (when vals (cons key vals))))
                  (vulpea-schema-fields (vulpea-schema--resolve schema)))))))

(defun vulpea--schema-read-schema (note)
  "Choose a schema to author NOTE against, prompting when ambiguous.

Returns a schema name symbol.  Uses the schema applicable to NOTE when
exactly one matches, prompts among the matches when several do, and
prompts over all registered schemas when none match."
  (let ((applicable (vulpea-schema-applicable note)))
    (cond
     ((= (length applicable) 1) (car applicable))
     (applicable
      (intern (completing-read "Schema: " (mapcar #'symbol-name applicable) nil t)))
     (t
      (let ((all (vulpea-schema-list)))
        (unless all (user-error "No schemas are registered"))
        (intern (completing-read "Schema: " (mapcar #'symbol-name all) nil t)))))))

(defconst vulpea--schema-date-default-regexp
  (concat "\\`\\(?:"
          "\\."                             ; today
          "\\|\\+\\{1,2\\}[0-9]+[dwmy]?"    ; +3, +3d, ++2w
          "\\|-[0-9]+[dwmy]?"               ; -1, -1d
          "\\|[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" ; absolute date
          "\\)\\(?: [0-9]\\{1,2\\}:[0-9]\\{2\\}\\)?\\'")
  "Shape of a string :default accepted for a date field.
`org-read-date' resolves anything, quietly falling back to today for
input it cannot read - exactly wrong for a silently written default -
so the accepted shapes are pinned down and everything else errors.")

(defun vulpea--schema-field-default (field note)
  "Resolve FIELD's :default against NOTE, or nil when there is none.

The :default is a literal value or a function of the note.  For a
`date' / `datetime' field a string default is `org-read-date' input,
either relative to today - \"+3d\", \"+1w\", \"+3d 14:00\" - or an
absolute \"2026-12-31\"; the result honors the field's :active, a
`vulpea-timestamp' value is used as is, and a string of any other
shape signals `user-error' rather than silently resolving to today.
A list is resolved element-wise (for :multiple fields).  Returns a
value ready for `vulpea-buffer-meta-set', or nil when the field
declares no default or the default resolves to nil or \"\" (the way
for a function default to decline)."
  (when (plist-member field :default)
    (let* ((type (or (plist-get field :type) 'string))
           (active (if (plist-member field :active)
                       (plist-get field :active)
                     t))
           (value (vulpea-schema--call-or-value
                   (plist-get field :default) note))
           (resolve (lambda (v)
                      (if (and (memq type '(date datetime)) (stringp v))
                          (progn
                            (unless (string-match-p
                                     vulpea--schema-date-default-regexp v)
                              (user-error
                               "Field %S has an unrecognized date default %S"
                               (plist-get field :key) v))
                            (vulpea-timestamp-create
                             (org-read-date (eq type 'datetime) t v)
                             (eq type 'datetime) active))
                        v))))
      (cond
       ((or (null value) (equal value "")) nil)
       ((listp value) (mapcar resolve value))
       (t (funcall resolve value))))))

(defun vulpea--schema-field-defaults (fields note)
  "Return a (KEY . VALUE) alist of the resolved defaults of FIELDS.
NOTE gives context for function defaults.  Fields without a default
\(or whose function default returns nil) are absent from the result."
  (let (values)
    (dolist (field fields)
      (when-let* ((default (vulpea--schema-field-default field note)))
        (push (cons (plist-get field :key) default) values)))
    (nreverse values)))

(defun vulpea--schema-prompt-field (field note required &optional default)
  "Prompt for a value for FIELD.

NOTE gives context and REQUIRED is non-nil when the field is required.
Honors :type (note selection for `note' / `link', `org-read-date' with
its calendar for `date' / `datetime' - a datetime prompt also asks for
a time - writing an active timestamp, or an inactive one when the
field says :active nil), :one-of (completion) and the target-tag
restrictions \(:target-tags and :target-tags-any, restricting note
selection to the targets validation would accept).  A
field marked :multiple collects several values: note fields select
repeatedly - each pick leaves the candidate pool, and quitting via
`keyboard-quit' or confirming empty input ends the collection - date
fields read timestamps until `keyboard-quit', :one-of fields use
`completing-read-multiple', and free-form fields read strings until a
blank answer.  Quitting a note or date prompt before the first pick
skips that field.  DEFAULT is the field's already-resolved :default
\(see `vulpea--schema-field-default'; resolution is the caller's job,
so a function default runs exactly once): it prefills the single-value
prompts - initial input for free-form, default candidate for :one-of,
starting date for `org-read-date' - while silently writing it instead
is the guided flow's business, not this function's.  Returns the
entered value, a list of values, or an empty value when skipped."
  (let* ((type (or (plist-get field :type) 'string))
         (one-of (vulpea-schema--call-or-value (plist-get field :one-of) note))
         (multiple (plist-get field :multiple))
         (target-tags (plist-get field :target-tags))
         (target-tags-any (plist-get field :target-tags-any))
         (label (format "%s%s" (plist-get field :key)
                        (if required " (required)" "")))
         (filter-fn (when (or target-tags target-tags-any)
                      (lambda (n)
                        (let ((tags (vulpea-note-tags n)))
                          (and (cl-every (lambda (tag) (member tag tags))
                                         target-tags)
                               (or (null target-tags-any)
                                   (cl-some (lambda (tag) (member tag tags))
                                            target-tags-any)))))))
         (candidates (lambda () (mapcar (lambda (v) (format "%s" v)) one-of)))
         (active (if (plist-member field :active)
                     (plist-get field :active)
                   t))
         ;; only atoms make sense as prompt prefill; a misdeclared
         ;; default (a timestamp on a free-form field) is dropped
         ;; rather than rendered as "#s(vulpea-timestamp ...)"
         (prefill (when (and default
                             (atom default)
                             (not (vulpea-timestamp-p default)))
                    (format "%s" default))))
    (cond
     ((and (memq type '(note link)) multiple)
      (vulpea-select-multiple-from
       label (vulpea-db-query filter-fn)
       :require-match t
       :select-fn
       (lambda (prompt notes &rest args)
         ;; confirming empty input yields a non-existing note; treat it
         ;; as "done" rather than collecting a phantom
         (let ((pick (apply #'vulpea-select-from prompt notes args)))
           (if (vulpea-note-id pick) pick (keyboard-quit))))))
     ((memq type '(note link))
      (condition-case nil
          (let ((pick (vulpea-select label :require-match t :filter-fn filter-fn)))
            ;; confirming empty input yields a non-existing note; skip
            ;; the field rather than writing a broken link
            (when (vulpea-note-id pick) pick))
        (quit nil)))
     ((and (memq type '(date datetime)) multiple)
      (vulpea-utils-collect-while
       (lambda ()
         (vulpea-timestamp-create
          (org-read-date (eq type 'datetime) t nil
                         (format "%s (C-g to stop)" label))
          (eq type 'datetime) active))
       nil))
     ((memq type '(date datetime))
      (condition-case nil
          (vulpea-timestamp-create
           (org-read-date (eq type 'datetime) t nil label
                          (when (vulpea-timestamp-p default)
                            (vulpea-timestamp-time default)))
           (eq type 'datetime) active)
        (quit nil)))
     ((and one-of multiple)
      (completing-read-multiple (concat label ": ") (funcall candidates)))
     (one-of
      (completing-read (concat label ": ") (funcall candidates)
                       nil nil nil nil prefill))
     (multiple
      (vulpea-utils-collect-while
       (lambda () (read-string (format "%s (empty to stop): " label)))
       (lambda (s) (not (string-blank-p s)))))
     (t (read-string (concat label ": ") prefill)))))

(defun vulpea--schema-prompt-fields (fields note)
  "Prompt for each field in FIELDS, returning a (KEY . VALUE) alist.

NOTE supplies context for conditional :required and :one-of.  A field
with a resolvable :default is filled with it silently, no prompt.  An
empty answer drops an optional field but keeps a required one as an
empty placeholder, so the author is still reminded of it."
  (let (values)
    (dolist (field fields)
      (let* ((key (plist-get field :key))
             (required (vulpea-schema--call-or-value
                        (plist-get field :required) note))
             (value (or (vulpea--schema-field-default field note)
                        (vulpea--schema-prompt-field field note required)))
             ;; a multi-value answer may contain blank entries (e.g. an
             ;; empty `completing-read-multiple'); drop them so an empty
             ;; optional field is not written as a stray placeholder
             (value (if (listp value) (remove "" value) value)))
        (cond
         ((and value (not (equal value "")))
          (push (cons key value) values))
         (required (push (cons key "") values)))))
    (nreverse values)))

(defun vulpea--schema-insert-field-values (fields values &optional bound)
  "Write FIELDS into the current buffer, taking values from VALUES.

FIELDS is an ordered list of field specs.  VALUES is an alist mapping a
field :key to a value or list of values; a field absent from VALUES is
written as an empty placeholder.  Fields are appended in order, so a
`note' value (or a bare id) becomes a proper link via
`vulpea-buffer-meta-format'.  BOUND limits the scope as in
`vulpea-buffer-meta-set'."
  (dolist (field fields)
    (let* ((key (plist-get field :key))
           (cell (assoc key values)))
      (vulpea-buffer-meta-set key (if cell (cdr cell) "") 'append bound))))

;;;###autoload
(defun vulpea-schema-insert-fields (&optional schema-or-name skeleton)
  "Insert an applicable schema's fields into the current buffer.

The schema is taken from SCHEMA-OR-NAME when given, otherwise chosen
from the schemas applicable to the current buffer (prompting when
several apply, or over all registered schemas when none do).

For each field the note does not already carry, prompt for a value -
offering :one-of values as completion and selecting a note for `note'
fields - and insert it; required fields are handled first.  A field
declaring a :default is filled with it silently, no prompt - edit it
after, or use `vulpea-schema-insert-field', which always prompts with
the default prefilled.  With a prefix argument (SKELETON non-nil),
skip prompting and insert placeholders for every missing field
instead: the field's default when it declares one, empty otherwise.

The fields are inserted into the note at point: the heading's subtree
when point is inside one, otherwise the file-level metadata.  A
target that lacks an `:ID:' gets one created right before the fields
are written - fields on something that is not a note would be
invisible to the database.  The id is ensured even when there is no
field to write - a schema with no fields, or a note already carrying
them all - since invoking the command is signal enough that the
target is meant to be a note; only skipping every prompt leaves an
id-less target untouched."
  (interactive (list nil current-prefix-arg))
  (let* ((schema (or schema-or-name
                     (vulpea--schema-read-schema (vulpea--schema-buffer-note))))
         (note (vulpea--schema-buffer-note schema))
         (fields (vulpea-schema-missing-fields note schema)))
    (cond
     ((null fields) (vulpea--ensure-id))
     (skeleton
      (vulpea--ensure-id)
      (vulpea--schema-insert-field-values
       fields (vulpea--schema-field-defaults fields note) 'heading))
     (t
      (let ((values (vulpea--schema-prompt-fields fields note)))
        (when values
          (vulpea--ensure-id)
          (vulpea--schema-insert-field-values
           (cl-remove-if-not (lambda (f) (assoc (plist-get f :key) values)) fields)
           values 'heading)))))))

;;;###autoload
(defun vulpea-schema-insert-field (&optional schema-or-name)
  "Insert a single schema field into the current buffer.

The schema is taken from SCHEMA-OR-NAME when given, otherwise chosen
from the schemas applicable to the current buffer (prompting when
several apply, or over all registered schemas when none do).

Prompts for one of the schema's fields - the ones the note does not
carry yet come first, required before optional, then the fields
already present - and then for its value, the way
`vulpea-schema-insert-fields' does: :one-of values as completion,
note selection for `note' fields, restricted to the field's
target-tag restrictions.

The field is written into the note at point: the heading's subtree
when point is inside one, otherwise the file-level metadata.  A
field marked :multiple keeps its existing values and the answer is
appended after them, except when it holds nothing but empty
placeholders (as left by the skeleton flow) - those are replaced by
the answer; any other field is replaced when already present.  An
empty answer (or quitting a note prompt) writes nothing.  A target
that lacks an `:ID:' gets one created right before the write, as in
`vulpea-schema-insert-fields'.  A schema with no fields skips the
prompts but still ensures the id - invoking the command is signal
enough that the target is meant to be a note.  Returns the value
written, or nil when skipped.

This is the one-field counterpart of `vulpea-schema-insert-fields' -
for adding an optional field that was skipped during the guided
flow, or one more value to a :multiple field."
  (interactive)
  (let* ((schema (vulpea-schema--resolve
                  (or schema-or-name
                      (vulpea--schema-read-schema (vulpea--schema-buffer-note)))))
         (fields (vulpea-schema-fields schema)))
    (if (null fields)
        ;; a fieldless schema still marks its target as a note, matching
        ;; `vulpea-schema-insert-fields'; there is just nothing to prompt for
        (progn
          (vulpea--ensure-id)
          (message "Schema %s has no fields" (vulpea-schema-name schema))
          nil)
      (let* ((note (vulpea--schema-buffer-note schema))
             (missing (vulpea-schema-missing-fields note schema))
             (ordered (append missing
                              (cl-remove-if (lambda (f) (memq f missing)) fields)))
             (keys (mapcar (lambda (f) (plist-get f :key)) ordered))
             (key (completing-read
                   "Field: "
                   ;; a plain list would be re-sorted by the completion UI;
                   ;; this table keeps the missing-first order
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action action keys string pred)))
                   nil t))
             (field (or (cl-find key fields
                                 :key (lambda (f) (plist-get f :key))
                                 :test #'equal)
                        ;; require-match still lets empty input through
                        (user-error "No field chosen")))
             (required (vulpea-schema--call-or-value (plist-get field :required) note))
             (value (vulpea--schema-prompt-field
                     field note required
                     (vulpea--schema-field-default field note)))
             (value (if (listp value) (remove "" value) value)))
        (when (and value (not (equal value "")))
          (vulpea--ensure-id)
          (if (and (plist-get field :multiple)
                   (seq-remove #'string-blank-p
                               (vulpea-buffer-meta-get-list key 'string 'heading)))
              ;; a :multiple field with real values grows in place, existing
              ;; ones untouched
              (vulpea-buffer-meta-add key value 'heading)
            ;; otherwise plain set: replaces a single-value field, fills an
            ;; empty skeleton placeholder, or starts the list
            (vulpea-buffer-meta-set key value 'append 'heading))
          value)))))

;;;###autoload
(defun vulpea-schema-fix-violation (violation &optional bound)
  "Fix VIOLATION in the current buffer by prompting for a corrected value.

Resolves the violated field from VIOLATION's schema and prompts for a
value the way `vulpea-schema-insert-fields' does - offering :one-of
values as completion, selecting a note for `note' fields, restricting to
the field's target-tag restrictions - then writes it, replacing the
offending value or inserting the field when it was missing.  For a
:multiple field every
value of the key is replaced by the collected answer, since the write
has set semantics.  Returns the value written, or nil when the prompt
is skipped.

BOUND limits the scope as in `vulpea-buffer-meta-set' and defaults to
\\='heading, so the fix is written into the note at point - the heading's
subtree when point is inside one, otherwise the file-level metadata.
This matches how the violating note is read, so a heading-level fix does
not rewrite an unrelated file-level value; pass \\='buffer to force
file-level scope.

This is the headless building block UIs use to offer one-key fixes for a
`vulpea-schema-validate' violation."
  (let* ((schema (vulpea-schema--resolve (vulpea-violation-schema violation)))
         (field (cl-find (vulpea-violation-field violation)
                         (vulpea-schema-fields schema)
                         :key (lambda (f) (plist-get f :key))
                         :test #'equal))
         (note (vulpea--schema-buffer-note schema))
         (required (vulpea-schema--call-or-value (plist-get field :required) note))
         (value (vulpea--schema-prompt-field
                 field note required
                 (vulpea--schema-field-default field note)))
         (value (if (listp value) (remove "" value) value)))
    (when (and field value (not (equal value "")))
      (vulpea-buffer-meta-set (plist-get field :key) value 'append (or bound 'heading))
      value)))

(provide 'vulpea)
;;; vulpea.el ends here
