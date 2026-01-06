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

(require 'vulpea-db)

;;; Database Helpers

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database.

Creates a fresh temporary database file, binds `vulpea-db-location'
to it, and ensures cleanup after BODY completes (even on error)."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
          (vulpea-db-location temp-file)
          (vulpea-db--connection nil))
     (unwind-protect
         (progn ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

(defmacro vulpea-test--with-temp-db-and-file (id content &rest body)
  "Execute BODY with temporary database and org file.

Creates a temp org file with ID and CONTENT, initializes a temp
database, indexes the file, then executes BODY. Cleans up both
the database and org file afterward."
  (declare (indent 2))
  `(let* ((temp-db-file (make-temp-file "vulpea-test-" nil ".db"))
          (vulpea-db-location temp-db-file)
          (vulpea-db--connection nil)
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
         (delete-file temp-org-file)))))

;;; File Helpers

(defun vulpea-test--create-temp-org-file (content)
  "Create temporary org file with CONTENT.

Returns absolute path to the created file.
Caller is responsible for cleanup."
  (let ((temp-file (make-temp-file "vulpea-test-" nil ".org")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

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
