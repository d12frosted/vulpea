;;; vulpea-test-helpers.el --- Shared test utilities for Vulpea -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 20 Dec 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Shared test infrastructure for all Vulpea test files.
;;
;;; Code:

(require 'ert)
(require 'org)
(require 'vulpea-db)
;; `vulpea-default-notes-directory' lives in vulpea.el, and the macros below
;; bind it.  Without the library loaded the symbol is not special yet, and in
;; a lexical-binding file that binding would be lexical, so
;; `vulpea--default-directory' would keep pointing at the caller's collection.
(require 'vulpea)

;;; Database Helpers

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database.

Creates a fresh temporary database file, binds `vulpea-db-location'
to it, and ensures cleanup after BODY completes (even on error).

Also points `vulpea-default-notes-directory' and `org-directory' at a
fresh temporary directory, so that a BODY reaching `vulpea-create' - on
its own, or through `vulpea-insert' with no candidates - writes into
that directory instead of the collection of whoever runs the suite."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
          (temp-notes-dir (file-name-as-directory
                           (make-temp-file "vulpea-test-notes-" t)))
          (vulpea-db-location temp-file)
          (vulpea-db--connection nil)
          (vulpea-default-notes-directory temp-notes-dir)
          (org-directory temp-notes-dir))
     (unwind-protect
         (progn ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (when (file-directory-p temp-notes-dir)
         (delete-directory temp-notes-dir t)))))

(defmacro vulpea-test--with-temp-db-and-file (id content &rest body)
  "Execute BODY with temporary database and org file.

Creates a temp org file with ID and CONTENT, initializes a temp
database, indexes the file, then executes BODY. Cleans up both
the database and org file afterward.

Like `vulpea-test--with-temp-db', keeps note creation inside a
temporary directory."
  (declare (indent 2))
  `(let* ((temp-db-file (make-temp-file "vulpea-test-" nil ".db"))
          (temp-notes-dir (file-name-as-directory
                           (make-temp-file "vulpea-test-notes-" t)))
          (vulpea-db-location temp-db-file)
          (vulpea-db--connection nil)
          (vulpea-default-notes-directory temp-notes-dir)
          (org-directory temp-notes-dir)
          (temp-org-file (make-temp-file "vulpea-test-" nil ".org")))
     (with-temp-file temp-org-file
       (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n%s" ,id ,content)))
     (unwind-protect
         (progn
           (vulpea-db)
           (vulpea-db-update-file temp-org-file)
           ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-db-file)
         (delete-file temp-db-file))
       (when (file-exists-p temp-org-file)
         (delete-file temp-org-file))
       (when (file-directory-p temp-notes-dir)
         (delete-directory temp-notes-dir t)))))

(defmacro vulpea-test--with-temp-db-and-files (files &rest body)
  "Execute BODY with temporary database constructed upon FILES.

FILES is a list of plists, where each plist has two properties `:name'
and `:content', which specifies the name and content of a temporary file
respectively.  Cleans up both the database and the temporary files
afterward."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vulpea-mentions-" t))
          (vulpea-db-location (make-temp-file "vulpea-mentions-" nil ".db"))
          (vulpea-db--connection nil)
          (vulpea-db-sync-directories (list dir))
          (vulpea-default-notes-directory dir)
          (org-directory dir))
     (unwind-protect
         (progn
           (vulpea-db)
           (dolist (file ,files)
             (let ((path (expand-file-name (plist-get file :name) dir)))
               (with-temp-file path
                 (insert (plist-get file :content)))
               (vulpea-db-update-file path)))
           ,@body)
       (when vulpea-db--connection (vulpea-db-close))
       (when (file-exists-p vulpea-db-location) (delete-file vulpea-db-location))
       (delete-directory dir t))))

(defmacro vulpea-test--with-temp-notes-dir (&rest body)
  "Execute BODY with a temporary database and notes directory.

Creates a fresh temporary database and an empty temporary
directory, binds `vulpea-db-sync-directories' to that directory and
makes it available to BODY as the anaphoric variable `root' (with a
trailing slash).  Cleans up the database and the directory after
BODY completes (even on error)."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory
                 (make-temp-file "vulpea-notes-" t)))
          (vulpea-db-location (make-temp-file "vulpea-test-" nil ".db"))
          (vulpea-db--connection nil)
          (vulpea-db-sync-directories (list root))
          (vulpea-default-notes-directory root)
          (org-directory root))
     (ignore root)
     (unwind-protect
         (progn
           (vulpea-db)
           ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p vulpea-db-location)
         (delete-file vulpea-db-location))
       (when (file-directory-p root)
         (delete-directory root t)))))

;;; File Helpers

(defun vulpea-test--create-temp-org-file (content)
  "Create temporary org file with CONTENT.

Returns absolute path to the created file.
Caller is responsible for cleanup."
  (let ((temp-file (make-temp-file "vulpea-test-" nil ".org")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

;;; External Tool Helpers

(defun vulpea-test--require-rg ()
  "Skip the calling test when ripgrep is not available.

Only `vulpea-mentions' shells out to rg, and it degrades on its own
when the binary is missing, so a collection without rg is a supported
setup and its tests simply do not apply.  A silent skip hides the loss
of coverage though, so a suite that must exercise mentions - CI, for
one - sets VULPEA_TESTS_REQUIRE_RG and turns the skip into a failure."
  (unless (executable-find "rg")
    (if (getenv "VULPEA_TESTS_REQUIRE_RG")
        (ert-fail (concat "rg not found on `exec-path', but"
                          " VULPEA_TESTS_REQUIRE_RG is set - the mentions"
                          " tests must not be skipped here"))
      (ert-skip "rg not found on `exec-path'"))))

;;; Note Insertion Helpers

(defun vulpea-test--insert-test-note (id title &rest args)
  "Insert a test note directly into the database.

ID is the note's unique identifier.
TITLE is the note's title.

ARGS is a plist with optional fields:
  :path       - file path (default: /tmp/ID.org)
  :level      - heading level, 0 for file-level (default: 0)
  :pos        - position in file (default: 0)
  :tags       - list of tags
  :aliases    - list of aliases
  :links      - list of link plists with :dest, :type, :pos
  :meta       - alist of metadata ((key . (values...)))
  :properties - alist of properties ((key . value))
  :todo       - TODO state
  :priority   - priority character
  :file-title - title of the file containing this note
  :modified-at - modification timestamp (default: 2025-11-16 10:00:00)

This inserts directly into the database without creating actual
org files. Useful for testing query functions."
  (let ((level (or (plist-get args :level) 0)))
    (apply #'vulpea-db--insert-note
           :id id
           :path (or (plist-get args :path) (format "/tmp/%s.org" id))
           :level level
           :pos (or (plist-get args :pos) 0)
           :title title
           :properties (plist-get args :properties)
           :tags (plist-get args :tags)
           :aliases (plist-get args :aliases)
           :meta (plist-get args :meta)
           :links (plist-get args :links)
           :todo (plist-get args :todo)
           :priority (plist-get args :priority)
           ;; For file-level notes, file-title equals title by default
           :file-title (or (plist-get args :file-title)
                           (if (= level 0) title nil))
           :modified-at (or (plist-get args :modified-at) "2025-11-16 10:00:00")
           args)))

(provide 'vulpea-test-helpers)
;;; vulpea-test-helpers.el ends here
