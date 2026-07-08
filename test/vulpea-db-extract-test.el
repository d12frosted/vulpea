;;; vulpea-db-extract-test.el --- Tests for vulpea-db-extract -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 16 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for vulpea-db-extract.el
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-db-extract)
(require 'vulpea-note)
(require 'vulpea-test-helpers)

;;; Parse Context Tests

(ert-deftest vulpea-db-extract-parse-context ()
  "Test parse context creation."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test Note\n\n* Heading\n")))
    (unwind-protect
        (let ((ctx (vulpea-db--parse-file path)))
          (should (vulpea-parse-ctx-p ctx))
          (should (equal (vulpea-parse-ctx-path ctx) path))
          (should (vulpea-parse-ctx-ast ctx))
          (should (vulpea-parse-ctx-file-node ctx))
          (should (vulpea-parse-ctx-hash ctx))
          (should (vulpea-parse-ctx-mtime ctx))
          (should (vulpea-parse-ctx-size ctx)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-basic ()
  "Test basic file-level node extraction."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: wine-db-id\n:END:\n#+TITLE: Wine Database\n#+FILETAGS: :wine:database:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :id) "wine-db-id"))
          (should (equal (plist-get node :title) "Wine Database"))
          (should (member "wine" (plist-get node :tags)))
          (should (member "database" (plist-get node :tags))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-no-id ()
  "Test file node without ID returns nil."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: No ID Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (null node)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-ignore ()
  "Test file node with VULPEA_IGNORE returns nil."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:VULPEA_IGNORE: t\n:END:\n#+TITLE: Ignored Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (null node)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-ignore-nil ()
  "Test file node with exclusion property set to nil is not excluded."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:VULPEA_IGNORE: nil\n:END:\n#+TITLE: Kept Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should node)
          (should (equal (plist-get node :id) "test-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-ignore-reason ()
  "Test file node with a non-nil exclusion reason is still excluded."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:VULPEA_IGNORE: private, keep out\n:END:\n#+TITLE: Ignored Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (null node)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-ignore-custom-property ()
  "Test file node honors a custom `vulpea-db-exclude-property'.
The configured property excludes, and the default VULPEA_IGNORE no
longer does."
  (let ((excluded (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: excluded-id\n:ROAM_EXCLUDE: t\n:END:\n#+TITLE: Excluded\n"))
        (kept (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: kept-id\n:VULPEA_IGNORE: t\n:END:\n#+TITLE: Kept\n")))
    (unwind-protect
        (let ((vulpea-db-exclude-property "ROAM_EXCLUDE"))
          (should (null (vulpea-parse-ctx-file-node (vulpea-db--parse-file excluded))))
          (let ((node (vulpea-parse-ctx-file-node (vulpea-db--parse-file kept))))
            (should node)
            (should (equal (plist-get node :id) "kept-id"))))
      (delete-file excluded)
      (delete-file kept))))

(ert-deftest vulpea-db-extract-file-node-auto-title ()
  "Test file node uses filename as title if missing."
  (let ((path (vulpea-test--create-temp-org-file ":PROPERTIES:\n:ID: test-id\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (title (plist-get node :title)))
          (should (equal title (file-name-base path))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-parse-with-readonly-buffer ()
  "Test that parsing succeeds when parse buffer has read-only text properties.
This simulates the scenario where org-transclusion (or similar packages)
mark regions as read-only during org-mode hooks."
  (let* ((path1 (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: readonly-test-1\n:END:\n#+TITLE: First\n"))
         (path2 (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: readonly-test-2\n:END:\n#+TITLE: Second\n")))
    (unwind-protect
        (progn
          ;; Parse first file to initialize the shared buffer
          (vulpea-db--parse-file path1)
          ;; Simulate org-transclusion: add read-only text properties
          ;; to the shared parse buffer
          (when (and vulpea-db--parse-buffer
                     (buffer-live-p vulpea-db--parse-buffer))
            (with-current-buffer vulpea-db--parse-buffer
              (add-text-properties (point-min) (point-max)
                                   '(read-only t))))
          ;; Parsing second file should still succeed
          (let ((ctx (vulpea-db--parse-file path2)))
            (should (vulpea-parse-ctx-p ctx))
            (should (equal (plist-get (vulpea-parse-ctx-file-node ctx) :title)
                           "Second"))))
      (delete-file path1)
      (delete-file path2))))

;;; Drawer Case Sensitivity Tests
;; https://github.com/d12frosted/vulpea/issues/277

(ert-deftest vulpea-db-extract-file-node-lowercase-drawer ()
  "Test file node extraction with lowercase :properties: drawer.
Org treats drawer names case-insensitively, so vulpea must too.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (let ((path (vulpea-test--create-temp-org-file
               ":properties:\n:ID: lowercase-drawer-id\n:end:\n#+TITLE: Lowercase Drawer\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should node)
          (should (equal (plist-get node :id) "lowercase-drawer-id"))
          (should (equal (plist-get node :title) "Lowercase Drawer")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-lowercase-drawer-all-methods ()
  "Test lowercase :properties: drawer across all parse methods.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (let ((path (vulpea-test--create-temp-org-file
               ":properties:\n:ID: lowercase-methods-id\n:end:\n#+TITLE: Lowercase Methods\n")))
    (unwind-protect
        (dolist (method '(single-temp-buffer temp-buffer find-file))
          (let* ((vulpea-db-parse-method method)
                 (ctx (vulpea-db--parse-file path))
                 (node (vulpea-parse-ctx-file-node ctx)))
            (should node)
            (should (equal (plist-get node :id) "lowercase-methods-id"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-lowercase-id-key ()
  "Test file node extraction with lowercase :id: property key.
Org treats property names case-insensitively, so vulpea must too.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:id: lowercase-key-id\n:END:\n#+TITLE: Lowercase Key\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should node)
          (should (equal (plist-get node :id) "lowercase-key-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-mixed-case-drawer ()
  "Test file node extraction with mixed-case :Properties: drawer.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (let ((path (vulpea-test--create-temp-org-file
               ":Properties:\n:ID: mixed-case-drawer-id\n:End:\n#+TITLE: Mixed Case Drawer\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should node)
          (should (equal (plist-get node :id) "mixed-case-drawer-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-lowercase-drawer ()
  "Test heading node extraction with lowercase :properties: drawer.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* Heading\n:properties:\n:ID: lowercase-heading-id\n:end:\n\nBody.\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "lowercase-heading-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-lowercase-drawer-indexed-in-db ()
  "Test end-to-end indexing of a file with lowercase :properties: drawer.
Regression test for https://github.com/d12frosted/vulpea/issues/277."
  (vulpea-test--with-temp-db
    (let ((path (vulpea-test--create-temp-org-file
                 ":properties:\n:ID: lowercase-db-id\n:end:\n#+TITLE: Lowercase In DB\n")))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file path)
            (let ((note (vulpea-db-get-by-id "lowercase-db-id")))
              (should note)
              (should (equal (vulpea-note-title note) "Lowercase In DB"))))
        (delete-file path)))))

;;; Heading Node Tests

(ert-deftest vulpea-db-extract-heading-nodes ()
  "Test heading-level node extraction."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n#+ID: file-id\n\n* Heading 1\n:PROPERTIES:\n:ID: heading-1-id\n:END:\n\n* Heading 2\n:PROPERTIES:\n:ID: heading-2-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 2))
          (should (member "heading-1-id" (mapcar (lambda (n) (plist-get n :id)) nodes)))
          (should (member "heading-2-id" (mapcar (lambda (n) (plist-get n :id)) nodes))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-no-file-id ()
  "Test that all headings are extracted when file has no ID.
Regression test for https://github.com/d12frosted/vulpea/issues/260."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: Testing File\n\n* First Heading\n:PROPERTIES:\n:ID: first-heading-id\n:END:\n\n* Second Heading\n:PROPERTIES:\n:ID: second-heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 2))
          (should (member "first-heading-id" (mapcar (lambda (n) (plist-get n :id)) nodes)))
          (should (member "second-heading-id" (mapcar (lambda (n) (plist-get n :id)) nodes))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-disabled ()
  "Test heading extraction respects index setting."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level nil)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (null nodes)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-todo ()
  "Test heading extraction captures TODO state."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* TODO Task\n:PROPERTIES:\n:ID: task-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :todo) "TODO")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-closed-planning ()
  "Test heading extraction reads CLOSED from the planning line."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* DONE Task\nCLOSED: [2024-08-13 Tue 09:41]\n:PROPERTIES:\n:ID: task-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :closed) "[2024-08-13 Tue 09:41]")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-closed-from-logbook ()
  "Test CLOSED falls back to the LOGBOOK State \"DONE\" entry.

Most org tasks record completion in the LOGBOOK drawer (via
`org-log-done' state logging) rather than on a CLOSED planning
line.  Such notes must still get a populated `:closed'."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* DONE Task\n:PROPERTIES:\n:ID: task-id\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2024-08-13 Tue 09:41]\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :closed) "[2024-08-13 Tue 09:41]")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-closed-logbook-most-recent ()
  "Test CLOSED uses the most recent LOGBOOK State \"DONE\" entry.

A task reopened and completed again has several State \"DONE\"
entries; the latest one is the effective completion time."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* DONE Task\n:PROPERTIES:\n:ID: task-id\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2024-08-15 Thu 10:00]\n- State \"TODO\"       from \"DONE\"       [2024-08-14 Wed 12:00]\n- State \"DONE\"       from \"TODO\"       [2024-08-13 Tue 09:41]\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :closed) "[2024-08-15 Thu 10:00]")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-closed-planning-precedence ()
  "Test the CLOSED planning line wins over the LOGBOOK fallback."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* DONE Task\nCLOSED: [2024-08-20 Tue 09:41]\n:PROPERTIES:\n:ID: task-id\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2024-08-13 Tue 09:41]\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :closed) "[2024-08-20 Tue 09:41]")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-closed-not-done-no-fallback ()
  "Test the LOGBOOK fallback is skipped for non-done headings.

A task that was completed and then reopened keeps the old State
\"DONE\" entry in its LOGBOOK, but org removes its CLOSED planning
line.  The extractor must mirror that: a TODO heading has no
`:closed', even with a stale State \"DONE\" log entry."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* TODO Task\n:PROPERTIES:\n:ID: task-id\n:END:\n:LOGBOOK:\n- State \"TODO\"       from \"DONE\"       [2024-08-14 Wed 12:00]\n- State \"DONE\"       from \"TODO\"       [2024-08-13 Tue 09:41]\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (null (plist-get node :closed))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-closed-logbook-db-roundtrip ()
  "Test the LOGBOOK-derived CLOSED reaches the DB and `vulpea-note-closed'.

Exercises the full path the recipe relies on: index a DONE note
whose completion is recorded only in its LOGBOOK, then read it back
from the database."
  (vulpea-test--with-temp-db-and-file "file-id"
      "#+TITLE: File\n\n* DONE Task\n:PROPERTIES:\n:ID: e2e-task-id\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [2024-08-13 Tue 09:41]\n:END:\n"
    (let ((note (vulpea-db-get-by-id "e2e-task-id")))
      (should note)
      (should (equal (vulpea-note-closed note) "[2024-08-13 Tue 09:41]")))))

(ert-deftest vulpea-db-extract-heading-nodes-ignore ()
  "Test heading with VULPEA_IGNORE is not extracted."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: File\n\n* Heading 1\n:PROPERTIES:\n:ID: heading-1\n:END:\n\n* Heading 2\n:PROPERTIES:\n:ID: heading-2\n:VULPEA_IGNORE: t\n:END:\n" (org-id-new)))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "heading-1")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-ignore-nil ()
  "Test heading with exclusion property set to nil is not excluded."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: File\n\n* Heading 1\n:PROPERTIES:\n:ID: heading-1\n:VULPEA_IGNORE: nil\n:END:\n" (org-id-new)))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "heading-1")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-nodes-ignore-custom-property ()
  "Test heading honors a custom `vulpea-db-exclude-property'.
The configured property excludes its heading, and the default
VULPEA_IGNORE no longer does."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: File\n\n* Heading 1\n:PROPERTIES:\n:ID: heading-1\n:ROAM_EXCLUDE: t\n:END:\n\n* Heading 2\n:PROPERTIES:\n:ID: heading-2\n:VULPEA_IGNORE: t\n:END:\n" (org-id-new)))))
    (unwind-protect
        (let* ((vulpea-db-exclude-property "ROAM_EXCLUDE")
               (vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "heading-2")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-with-timestamp ()
  "Test heading extraction when title contains timestamp."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Meetings\n:PROPERTIES:\n:ID: meetings-id\n:END:\n\n** [2020-08-07 Fri]\n:PROPERTIES:\n:ID: meeting-id\n:END:\n\nMeeting notes here.\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (meeting-node (seq-find (lambda (n) (equal (plist-get n :id) "meeting-id")) nodes)))
          ;; Should extract timestamp in title as string, not fail
          (should meeting-node)
          (should (equal (plist-get meeting-node :title) "[2020-08-07 Fri]"))
          ;; Outline path should also handle timestamp correctly
          (should (equal (plist-get meeting-node :outline-path) '("Meetings"))))
      (delete-file path))))

;;; Extractor Tests

(ert-deftest vulpea-db-extract-aliases ()
  "Test alias extraction."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:ALIASES: Alias1 Alias2\n:END:\n#+TITLE: Main Title\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (aliases (plist-get node :aliases)))
          (should (member "Alias1" aliases))
          (should (member "Alias2" aliases)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-aliases-mixed-quoted ()
  "Test alias extraction with mixed quoted and unquoted aliases.

Quoted aliases (with spaces) and unquoted aliases should be
handled correctly when mixed together."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:ALIASES: \"Pinot Nero\" \"Rulandské modré\" Spätburgunder Blauburgunder\n:END:\n#+TITLE: Pinot Noir\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (aliases (plist-get node :aliases)))
          (should (= (length aliases) 4))
          (should (member "Pinot Nero" aliases))
          (should (member "Rulandské modré" aliases))
          (should (member "Spätburgunder" aliases))
          (should (member "Blauburgunder" aliases)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-aliases-lowercase-property-name ()
  "Test alias extraction when `vulpea-buffer-alias-property' is lowercase.

Property keys are upcased during extraction, so the lookup must
upcase the configured property name too, otherwise aliases silently
fail to extract. Regression test for
https://github.com/d12frosted/vulpea/issues/277."
  (let ((vulpea-buffer-alias-property "roam_aliases")
        (path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:ROAM_ALIASES: Alias1 Alias2\n:END:\n#+TITLE: Main Title\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (aliases (plist-get node :aliases)))
          (should (member "Alias1" aliases))
          (should (member "Alias2" aliases)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-aliases ()
  "Test alias extraction on heading-level notes."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: File\n\n"
                "* Main Title\n"
                ":PROPERTIES:\n"
                ":ID: heading-id\n"
                ":ALIASES: Alias1 Alias2\n"
                ":END:\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes))
               (aliases (plist-get node :aliases)))
          (should (member "Alias1" aliases))
          (should (member "Alias2" aliases)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-aliases-round-trip ()
  "Test heading-level aliases survive database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 (concat
                  ":PROPERTIES:\n:ID: file-id\n:END:\n"
                  "#+TITLE: File\n\n"
                  "* Main Title\n"
                  ":PROPERTIES:\n"
                  ":ID: heading-alias-rt\n"
                  ":ALIASES: \"Some Alias\" Another\n"
                  ":END:\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)
            (let* ((note (vulpea-db-get-by-id "heading-alias-rt"))
                   (aliases (vulpea-note-aliases note)))
              (should (member "Some Alias" aliases))
              (should (member "Another" aliases))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-links ()
  "Test link extraction."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Test\n\nSome [[id:note-1][link]] and [[id:note-2][another]].\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links)))
          (should (= (length links) 2))
          (should (member "note-1" (mapcar (lambda (l) (plist-get l :dest)) links)))
          (should (member "note-2" (mapcar (lambda (l) (plist-get l :dest)) links)))
          ;; All links should have position
          (should (cl-every (lambda (l) (plist-get l :pos)) links)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-all-types ()
  "Test extraction of all link types."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:END:\n#+TITLE: Test\n\n- [[id:note-1][Note link]]\n- [[file:test.org][File link]]\n- [[https://example.com][Web link]]\n- [[attachment:image.png][Attachment]]\n- [[elisp:(message \"hi\")][Elisp]]\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links))
               (types (mapcar (lambda (l) (plist-get l :type)) links)))
          (should (= (length links) 5))
          ;; Check all link types are present
          (should (member "id" types))
          (should (member "file" types))
          (should (member "https" types))
          ;; attachment: can be parsed as either "attachment" or "fuzzy" depending on org-mode version
          (should (or (member "attachment" types) (member "fuzzy" types)))
          (should (member "elisp" types))
          ;; Check specific destinations
          (should (member "note-1" (mapcar (lambda (l) (plist-get l :dest)) links)))
          (should (member "test.org" (mapcar (lambda (l) (plist-get l :dest)) links)))
          (should (member "//example.com" (mapcar (lambda (l) (plist-get l :dest)) links))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-positions ()
  "Test that link positions are correctly extracted."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n\nFirst [[id:link-1][link]] then [[id:link-2][second]].\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links))
               (link-1 (seq-find (lambda (l) (equal (plist-get l :dest) "link-1")) links))
               (link-2 (seq-find (lambda (l) (equal (plist-get l :dest) "link-2")) links)))
          (should (= (length links) 2))
          ;; Both links should have positions
          (should (plist-get link-1 :pos))
          (should (plist-get link-2 :pos))
          ;; First link should have smaller position than second
          (should (< (plist-get link-1 :pos) (plist-get link-2 :pos))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-positions-round-trip ()
  "Test link positions survive database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: test-links-pos\n:END:\n#+TITLE: Test\n\nFirst [[id:link-1][link]] then [[id:link-2][second]].\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let* ((note (vulpea-db-get-by-id "test-links-pos"))
                   (links (vulpea-note-links note))
                   (link-1 (seq-find (lambda (l) (equal (plist-get l :dest) "link-1")) links))
                   (link-2 (seq-find (lambda (l) (equal (plist-get l :dest) "link-2")) links)))
              (should (= (length links) 2))
              ;; Both links should have positions
              (should (plist-get link-1 :pos))
              (should (plist-get link-2 :pos))
              ;; Positions should be numbers
              (should (numberp (plist-get link-1 :pos)))
              (should (numberp (plist-get link-2 :pos)))
              ;; First link should have smaller position than second
              (should (< (plist-get link-1 :pos) (plist-get link-2 :pos)))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-links-non-note-subtree-round-trip ()
  "Test that non-note subtree links survive database round-trip."
  (vulpea-test--with-temp-db
    (let ((path (vulpea-test--create-temp-org-file
                 (concat
                  ":PROPERTIES:\n:ID: file-id\n:END:\n"
                  "#+TITLE: Test\n\n"
                  "* Parent\n"
                  ":PROPERTIES:\n"
                  ":ID: parent-id\n"
                  ":END:\n\n"
                  "Parent link [[id:link-1][One]].\n\n"
                  "** Note Child\n"
                  ":PROPERTIES:\n"
                  ":ID: child-id\n"
                  ":END:\n\n"
                  "Note child link [[id:link-2][Two]].\n\n"
                  "** Non-note Child\n\n"
                  "Non-note link [[id:link-3][Three]].\n"))))
      (unwind-protect
          (progn
            (vulpea-db)
            (vulpea-db-update-file path)
            (let* ((parent (vulpea-db-get-by-id "parent-id"))
                   (child (vulpea-db-get-by-id "child-id"))
                   (parent-links (vulpea-note-links parent))
                   (child-links (vulpea-note-links child))
                   (parent-dests (mapcar (lambda (l) (plist-get l :dest)) parent-links))
                   (child-dests (mapcar (lambda (l) (plist-get l :dest)) child-links)))
              ;; Parent should have link-1 and link-3 but NOT link-2
              (should (= (length parent-links) 2))
              (should (member "link-1" parent-dests))
              (should (member "link-3" parent-dests))
              (should-not (member "link-2" parent-dests))
              ;; Child should have only link-2
              (should (= (length child-links) 1))
              (should (member "link-2" child-dests))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-links-file-vs-heading ()
  "Test that file-level links don't include heading links."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: Test File\n\nFile-level [[https://file-link.com][link]].\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n\nHeading-level [[https://heading-link.com][link]].\n"))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (file-links (plist-get file-node :links))
               (heading-links (plist-get (car heading-nodes) :links)))
          ;; File-level node should only have file-level link
          (should (= (length file-links) 1))
          (should (equal (plist-get (car file-links) :dest) "//file-link.com"))
          ;; Heading node should only have heading-level link
          (should (= (length heading-links) 1))
          (should (equal (plist-get (car heading-links) :dest) "//heading-link.com")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-nested-headings ()
  "Test that parent heading links don't include child heading links."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test File\n\n"
                "* Parent Heading\n"
                ":PROPERTIES:\n"
                ":ID: parent-id\n"
                ":END:\n\n"
                "Parent-level [[https://parent-link.com][link]].\n\n"
                "** Child Heading\n"
                ":PROPERTIES:\n"
                ":ID: child-id\n"
                ":END:\n\n"
                "Child-level [[https://child-link.com][link]].\n"
                "Child attachment [[attachment:child-image.png]].\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (parent-node (seq-find (lambda (n) (equal (plist-get n :id) "parent-id")) heading-nodes))
               (child-node (seq-find (lambda (n) (equal (plist-get n :id) "child-id")) heading-nodes))
               (parent-links (plist-get parent-node :links))
               (child-links (plist-get child-node :links))
               (parent-dests (mapcar (lambda (l) (plist-get l :dest)) parent-links))
               (child-dests (mapcar (lambda (l) (plist-get l :dest)) child-links)))
          ;; Parent node should only have its own link, not child's links
          (should (= (length parent-links) 1))
          (should (member "//parent-link.com" parent-dests))
          (should-not (member "//child-link.com" parent-dests))
          (should-not (member "child-image.png" parent-dests))
          ;; Child node should have its own links
          (should (= (length child-links) 2))
          (should (member "//child-link.com" child-dests))
          (should (or (member "child-image.png" child-dests)
                      ;; attachment: can be parsed as fuzzy in some org versions
                      (member "attachment:child-image.png" child-dests))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-non-note-subtree-file-level ()
  "Test that file note collects links from non-note subtrees."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test\n\n"
                "File link [[id:link-1][One]].\n\n"
                "* Heading\n\n"
                "Subtree link [[id:link-2][Two]].\n"))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (file-links (plist-get file-node :links))
               (file-dests (mapcar (lambda (l) (plist-get l :dest)) file-links)))
          (should (= (length file-links) 2))
          (should (member "link-1" file-dests))
          (should (member "link-2" file-dests)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-non-note-subtree-heading-level ()
  "Test that heading note collects links from non-note child headings."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test\n\n"
                "* Parent\n"
                ":PROPERTIES:\n"
                ":ID: parent-id\n"
                ":END:\n\n"
                "Parent link [[id:link-1][One]].\n\n"
                "** Child\n\n"
                "Child link [[id:link-2][Two]].\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (parent-node (seq-find (lambda (n) (equal (plist-get n :id) "parent-id")) heading-nodes))
               (parent-links (plist-get parent-node :links))
               (parent-dests (mapcar (lambda (l) (plist-get l :dest)) parent-links))
               (file-links (plist-get file-node :links)))
          ;; Parent should have both links
          (should (= (length parent-links) 2))
          (should (member "link-1" parent-dests))
          (should (member "link-2" parent-dests))
          ;; File node should have no links
          (should (= (length file-links) 0)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-non-note-subtree-under-note-heading ()
  "Test that note heading collects links from non-note grandchild."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test\n\n"
                "* Heading\n"
                ":PROPERTIES:\n"
                ":ID: heading-id\n"
                ":END:\n\n"
                "Heading link [[id:link-1][One]].\n\n"
                "** Sub\n\n"
                "Sub link [[id:link-2][Two]].\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (heading-node (seq-find (lambda (n) (equal (plist-get n :id) "heading-id")) heading-nodes))
               (heading-links (plist-get heading-node :links))
               (heading-dests (mapcar (lambda (l) (plist-get l :dest)) heading-links))
               (file-links (plist-get file-node :links)))
          ;; Heading should have both links
          (should (= (length heading-links) 2))
          (should (member "link-1" heading-dests))
          (should (member "link-2" heading-dests))
          ;; File node should have no links
          (should (= (length file-links) 0)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-mixed-note-and-non-note-siblings ()
  "Test that parent collects non-note child links but not note child links."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test\n\n"
                "* Parent\n"
                ":PROPERTIES:\n"
                ":ID: parent-id\n"
                ":END:\n\n"
                "Parent link [[id:link-1][One]].\n\n"
                "** Note Child\n"
                ":PROPERTIES:\n"
                ":ID: child-id\n"
                ":END:\n\n"
                "Note child link [[id:link-2][Two]].\n\n"
                "** Non-note Child\n\n"
                "Non-note link [[id:link-3][Three]].\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (parent-node (seq-find (lambda (n) (equal (plist-get n :id) "parent-id")) heading-nodes))
               (child-node (seq-find (lambda (n) (equal (plist-get n :id) "child-id")) heading-nodes))
               (parent-links (plist-get parent-node :links))
               (parent-dests (mapcar (lambda (l) (plist-get l :dest)) parent-links))
               (child-links (plist-get child-node :links))
               (child-dests (mapcar (lambda (l) (plist-get l :dest)) child-links)))
          ;; Parent should have link-1 and link-3 but NOT link-2
          (should (= (length parent-links) 2))
          (should (member "link-1" parent-dests))
          (should (member "link-3" parent-dests))
          (should-not (member "link-2" parent-dests))
          ;; Child should have only link-2
          (should (= (length child-links) 1))
          (should (member "link-2" child-dests)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-links-deeply-nested-non-note-headings ()
  "Test that parent collects links from deeply nested non-note headings."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: Test\n\n"
                "* Parent\n"
                ":PROPERTIES:\n"
                ":ID: parent-id\n"
                ":END:\n\n"
                "** Sub1\n\n"
                "Link [[id:link-1][One]].\n\n"
                "*** Sub2\n\n"
                "Link [[id:link-2][Two]].\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (parent-node (seq-find (lambda (n) (equal (plist-get n :id) "parent-id")) heading-nodes))
               (parent-links (plist-get parent-node :links))
               (parent-dests (mapcar (lambda (l) (plist-get l :dest)) parent-links)))
          ;; Parent should have both links from nested non-note headings
          (should (= (length parent-links) 2))
          (should (member "link-1" parent-dests))
          (should (member "link-2" parent-dests)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-meta-with-types ()
  "Test metadata extraction stores values as strings."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: wine-id\n:END:\n#+TITLE: Wine\n\n- country :: France\n- price :: 25.50\n- region :: [[id:region-id][Region Name]]\n- url :: https://example.com\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (meta (plist-get node :meta)))
          ;; Check country
          (let ((country-vals (cdr (assoc "country" meta))))
            (should (= (length country-vals) 1))
            (should (equal (car country-vals) "France")))

          ;; Check price
          (let ((price-vals (cdr (assoc "price" meta))))
            (should (= (length price-vals) 1))
            (should (equal (car price-vals) "25.50")))

          ;; Check region - link stored as interpreted string
          (let ((region-vals (cdr (assoc "region" meta))))
            (should (= (length region-vals) 1))
            (should (equal (car region-vals) "[[id:region-id][Region Name]]")))

          ;; Check URL
          (let ((url-vals (cdr (assoc "url" meta))))
            (should (= (length url-vals) 1))
            (should (equal (car url-vals) "https://example.com"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-meta-multiple-values ()
  "Test metadata with multiple values for same key."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: wine-multi-id\n:END:\n#+TITLE: Wine\n\n- grape :: [[id:grape-1][Grape One]]\n- grape :: [[id:grape-2][Grape Two]]\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (meta (plist-get node :meta))
               (grape-vals (cdr (assoc "grape" meta))))
          (should (= (length grape-vals) 2))
          (should (member "[[id:grape-1][Grape One]]" grape-vals))
          (should (member "[[id:grape-2][Grape Two]]" grape-vals)))
      (delete-file path))))

;;; Registry Tests

(ert-deftest vulpea-db-extract-registry-basic ()
  "Test extractor registration."
  (let ((vulpea-db--extractors nil))
    (vulpea-db-register-extractor 'test-extractor
                                  (lambda (ctx data) data))
    (should (= (length vulpea-db--extractors) 1))
    (should (eq (vulpea-extractor-name (car vulpea-db--extractors))
                'test-extractor))))

(ert-deftest vulpea-db-extract-registry-unregister ()
  "Test extractor unregistration."
  (let ((vulpea-db--extractors nil))
    (vulpea-db-register-extractor 'test-extractor
                                  (lambda (ctx data) data))
    (should (= (length vulpea-db--extractors) 1))
    (vulpea-db-unregister-extractor 'test-extractor)
    (should (= (length vulpea-db--extractors) 0))))

(ert-deftest vulpea-db-extract-registry-execution ()
  "Test extractors are executed."
  (let ((vulpea-db--extractors nil)
        (executed nil))
    (vulpea-db-register-extractor 'test-extractor
                                  (lambda (ctx data)
                                    (setq executed t)
                                    data))
    (vulpea-db--run-extractors
     (make-vulpea-parse-ctx)
     (list :id "test"))
    (should executed)))

(ert-deftest vulpea-db-extract-registry-data-enrichment ()
  "Test extractors can enrich note data."
  (let ((vulpea-db--extractors nil))
    (vulpea-db-register-extractor 'add-custom-field
                                  (lambda (ctx data)
                                    (plist-put data :custom "enriched")
                                    data))
    (let ((result (vulpea-db--run-extractors
                   (make-vulpea-parse-ctx)
                   (list :id "test"))))
      (should (equal (plist-get result :custom) "enriched")))))

(ert-deftest vulpea-db-extract-extractor-links-persisted ()
  "Links an extractor adds to note-data reach both storage forms.
An extractor contributing to :links through the documented
contract (plist-put + return note-data) must land in the
normalized links table and the materialized notes.links column;
previously the returned note-data was discarded."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-link
       (lambda (_ctx data)
         (plist-put data :links
                    (append (plist-get data :links)
                            (list (list :dest "target-id"
                                        :type "id"
                                        :pos 1
                                        :description "Target"))))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: source-id\n:END:\n#+TITLE: Source\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; Normalized links table
              (should (equal '(("source-id" "target-id" "id"))
                             (emacsql (vulpea-db)
                                      [:select [source dest type] :from links
                                       :where (= source $s1)]
                                      "source-id")))
              ;; Materialized notes.links column, via the note struct
              (let* ((note (vulpea-db-get-by-id "source-id"))
                     (link (car (vulpea-note-links note))))
                (should (= 1 (length (vulpea-note-links note))))
                (should (equal (plist-get link :dest) "target-id"))
                (should (equal (plist-get link :type) "id"))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-links-merge-with-extracted ()
  "Extractor-contributed links coexist with links parsed from content."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-link
       (lambda (_ctx data)
         (plist-put data :links
                    (append (plist-get data :links)
                            (list (list :dest "plugin-dest"
                                        :type "id"
                                        :pos 1
                                        :description nil))))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: merge-id\n:END:\n"
                           "#+TITLE: Merge\n\n"
                           "A [[id:organic-dest][real link]].\n"))))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (let ((dests (mapcar #'car
                                   (emacsql (vulpea-db)
                                            [:select [dest] :from links
                                             :where (= source $s1)
                                             :order-by [(asc dest)]]
                                            "merge-id"))))
                (should (equal dests '("organic-dest" "plugin-dest"))))
              (let ((note (vulpea-db-get-by-id "merge-id")))
                (should (= 2 (length (vulpea-note-links note))))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-tags-persisted ()
  "Tags an extractor adds to note-data reach both storage forms."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-tag
       (lambda (_ctx data)
         (plist-put data :tags
                    (append (plist-get data :tags) (list "plugin-tag")))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: tagged-id\n:END:\n"
                           "#+TITLE: Tagged\n#+FILETAGS: :organic:\n"))))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; Normalized tags table (drives tag queries)
              (let ((notes (vulpea-db-query-by-tags-some '("plugin-tag"))))
                (should (= 1 (length notes)))
                (should (equal (vulpea-note-id (car notes)) "tagged-id")))
              ;; Materialized column keeps organic and contributed tags
              (let ((note (vulpea-db-get-by-id "tagged-id")))
                (should (member "organic" (vulpea-note-tags note)))
                (should (member "plugin-tag" (vulpea-note-tags note)))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-meta-persisted ()
  "Meta an extractor adds to note-data reaches both storage forms."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-meta
       (lambda (_ctx data)
         (plist-put data :meta
                    (append (plist-get data :meta)
                            (list (cons "kind" (list "plugin")))))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: meta-id\n:END:\n#+TITLE: Meta\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; Normalized meta table
              (should (equal '(("kind" "plugin"))
                             (emacsql (vulpea-db)
                                      [:select [key value] :from meta
                                       :where (= note-id $s1)]
                                      "meta-id")))
              ;; Materialized column
              (let ((note (vulpea-db-get-by-id "meta-id")))
                (should (equal (cdr (assoc "kind" (vulpea-note-meta note)))
                               '("plugin")))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-scalar-field-persisted ()
  "A scalar note-data field changed by an extractor is persisted."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-todo
       (lambda (_ctx data)
         (plist-put data :todo "TODO")
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: scalar-id\n:END:\n#+TITLE: Scalar\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (equal (vulpea-note-todo
                              (vulpea-db-get-by-id "scalar-id"))
                             "TODO")))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-removal-persisted ()
  "An extractor removing a core value removes it from both forms."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'drop-tag
       (lambda (_ctx data)
         (plist-put data :tags (remove "secret" (plist-get data :tags)))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: removal-id\n:END:\n"
                           "#+TITLE: Removal\n#+FILETAGS: :keep:secret:\n"))))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; Normalized tags table
              (should (equal '(("keep"))
                             (emacsql (vulpea-db)
                                      [:select [tag] :from tags
                                       :where (= note-id $s1)]
                                      "removal-id")))
              ;; Materialized column
              (should (equal (vulpea-note-tags
                              (vulpea-db-get-by-id "removal-id"))
                             '("keep"))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-nil-return-tolerated ()
  "An extractor returning nil leaves note-data unchanged.
Before extractor results were persisted, a nil return was silently
discarded; plugins ending in a `when'-guarded insert relied on that.
A nil return must keep behaving as \"no contribution\", not wipe
every core field."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor 'returns-nil (lambda (_ctx _data) nil))
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: nil-return-id\n:END:\n"
                           "#+TITLE: Untouched\n#+FILETAGS: :tag1:\n"))))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (let ((note (vulpea-db-get-by-id "nil-return-id")))
                (should (equal (vulpea-note-title note) "Untouched"))
                (should (equal (vulpea-note-tags note) '("tag1")))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-created-at-recomputed ()
  "A :properties change recomputes the derived created_at column."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'contribute-created
       (lambda (_ctx data)
         (plist-put data :properties
                    (append (plist-get data :properties)
                            (list (cons "CREATED" "[2020-01-02 Thu]"))))
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: created-id\n:END:\n#+TITLE: Created\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (equal '(("2020-01-02"))
                             (emacsql (vulpea-db)
                                      [:select [created-at] :from notes
                                       :where (= id $s1)]
                                      "created-id"))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-extractor-id-change-ignored ()
  "Identity stays fixed even when an extractor rewrites :id in place.
The persistence writeback must target the note as inserted; a
misbehaving extractor changing :id must neither redirect the
writeback to a phantom row nor lose sibling contributions."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       'rogue-id
       (lambda (_ctx data)
         (plist-put data :tags
                    (append (plist-get data :tags) (list "contributed")))
         (plist-put data :id "phantom-id")
         data))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: real-id\n:END:\n#+TITLE: Rogue\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; The contribution lands on the real note...
              (should (equal '(("contributed"))
                             (emacsql (vulpea-db)
                                      [:select [tag] :from tags
                                       :where (= note-id $s1)]
                                      "real-id")))
              ;; ...and no phantom rows appear anywhere
              (should-not (emacsql (vulpea-db)
                                   [:select [tag] :from tags
                                    :where (= note-id $s1)]
                                   "phantom-id")))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-schema-registration ()
  "Test plugin schema is applied when extractor is registered."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      ;; Register extractor with schema
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'test-citations
        :version 1
        :schema '((citations
                   [(note-id :not-null)
                    (citekey :not-null)]
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))
        :extract-fn (lambda (ctx data) data)))

      ;; Verify table was created
      (should (vulpea-db--table-exists-p 'citations))

      ;; Verify schema version was registered
      (let ((row (car (emacsql (vulpea-db)
                               [:select [version] :from schema-registry
                                :where (= name $s1)]
                               "test-citations"))))
        (should (equal (car row) 1))))))

(ert-deftest vulpea-db-extract-schema-versioning ()
  "Test schema versioning prevents re-application."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      ;; Register extractor v1
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'test-versioned
        :version 1
        :schema '((test-table [(id :not-null)]))
        :extract-fn (lambda (ctx data) data)))

      ;; Verify version 1 registered
      (let ((v1 (caar (emacsql (vulpea-db)
                               [:select [version] :from schema-registry
                                :where (= name $s1)]
                               "test-versioned"))))
        (should (= v1 1)))

      ;; Re-register same version (should not error)
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'test-versioned
        :version 1
        :schema '((test-table [(id :not-null)]))
        :extract-fn (lambda (ctx data) data)))

      ;; Register higher version
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'test-versioned
        :version 2
        :schema '((test-table [(id :not-null) (name)]))
        :extract-fn (lambda (ctx data) data)))

      ;; Verify version updated to 2
      (let ((v2 (caar (emacsql (vulpea-db)
                               [:select [version] :from schema-registry
                                :where (= name $s1)]
                               "test-versioned"))))
        (should (= v2 2))))))

;;; Plugin Example: Citation Extractor

(defun vulpea-test--extract-citations (ctx note-data)
  "Example citation extractor for demonstration.

Extracts citations from org content in format [@citekey].
This is a complete working example showing how to write a plugin.

The extractor:
1. Extracts citations from the AST
2. Stores them in the custom citations table
3. Adds them to note-data (optional, for completeness)"
  (let* ((ast (vulpea-parse-ctx-ast ctx))
         (note-id (plist-get note-data :id))
         ;; Extract citations from the AST
         ;; Simple regex-based extraction for demonstration
         (citations
          (apply #'append
                 (org-element-map ast 'paragraph
                   (lambda (para)
                     (let ((content (org-element-interpret-data para))
                           (result nil)
                           (pos 0))
                       (while (string-match "\\[@\\([^]]+\\)\\]" content pos)
                         (push (substring-no-properties (match-string 1 content)) result)
                         (setq pos (match-end 0)))
                       result))))))

    ;; Insert citations into custom table
    (when citations
      (let ((unique-citations (delete-dups (nreverse citations))))
        (emacsql (vulpea-db)
                 [:insert :into citations :values $v1]
                 (mapcar (lambda (citekey)
                           (vector note-id citekey))
                         unique-citations))

        ;; Optionally add to note-data for other extractors
        (plist-put note-data :citations unique-citations)))

    note-data))

(ert-deftest vulpea-db-extract-plugin-example-citations ()
  "Complete example: citation extractor plugin with schema and queries.

This test demonstrates a complete working plugin that:
1. Defines a custom schema for citations table
2. Extracts citation data from notes
3. Stores citations in the database
4. Queries citation data

This serves as documentation for plugin authors."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      ;; Step 1: Register the citation extractor with schema
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'citations
        :version 1
        :priority 50
        :schema '((citations
                   [(note-id :not-null)
                    (citekey :not-null)]
                   (:primary-key [note-id citekey])
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))
        :extract-fn #'vulpea-test--extract-citations))

      ;; Verify schema was created
      (should (vulpea-db--table-exists-p 'citations))

      ;; Step 2: Create a test note with citations
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: paper-id\n:END:\n#+TITLE: Research Paper\n\nThis work builds on [@smith2020] and [@jones2019].\n")))
        (unwind-protect
            (progn
              ;; Step 3: Update file (triggers extractor)
              (vulpea-db-update-file path)

              ;; Step 4: Verify citations were extracted and stored
              (let ((citations (emacsql (vulpea-db)
                                        [:select [citekey] :from citations
                                         :where (= note-id $s1)
                                         :order-by [(asc citekey)]]
                                        "paper-id")))
                (should (= (length citations) 2))
                (should (equal (caar citations) "jones2019"))
                (should (equal (caadr citations) "smith2020")))

              ;; Step 5: Query notes by citation (demonstrates plugin queries)
              (let* ((citing-notes
                      (emacsql (vulpea-db)
                               [:select [notes:id notes:title]
                                :from notes
                                :inner :join citations
                                :on (= notes:id citations:note-id)
                                :where (= citations:citekey $s1)]
                               "smith2020")))
                (should (= (length citing-notes) 1))
                (should (equal (caar citing-notes) "paper-id"))
                (should (equal (cadar citing-notes) "Research Paper"))))
          (delete-file path))))))

;;; Integration Tests

(ert-deftest vulpea-db-extract-update-file-basic ()
  "Test updating database from file."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test Note\n#+FILETAGS: :wine:\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)

            ;; Verify note in database
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [id title] :from notes
                                      :where (= id $s1)]
                                     "test-id"))))
              (should (equal (elt row 0) "test-id"))
              (should (equal (elt row 1) "Test Note")))

            ;; Verify tag in database
            (let ((tags (emacsql (vulpea-db)
                                 [:select [tag] :from tags
                                  :where (= note-id $s1)]
                                 "test-id")))
              (should (member '("wine") tags))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-update-file-with-headings ()
  "Test updating file with heading-level notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (let ((count (vulpea-db-update-file path)))
              (should (= count 2))  ; File + 1 heading

              ;; Verify both notes exist
              (should (emacsql (vulpea-db)
                               [:select * :from notes :where (= id $s1)]
                               "file-id"))
              (should (emacsql (vulpea-db)
                               [:select * :from notes :where (= id $s1)]
                               "heading-id"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-update-file-no-file-id ()
  "Test updating file without file-level ID indexes all headings.
Regression test for https://github.com/d12frosted/vulpea/issues/260."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 "#+TITLE: Testing File\n\n* First Heading\n:PROPERTIES:\n:ID: B691D184-1003-47E7-A3E3-FDE51AF99A07\n:END:\n\n* Second Heading\n:PROPERTIES:\n:ID: B7FB938B-02DA-489D-8AB9-4F3D23B1C5D1\n:END:\n")))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)

            ;; Verify both heading notes exist
            (let ((note1 (vulpea-db-get-by-id "B691D184-1003-47E7-A3E3-FDE51AF99A07"))
                  (note2 (vulpea-db-get-by-id "B7FB938B-02DA-489D-8AB9-4F3D23B1C5D1")))
              (should note1)
              (should note2)

              ;; Verify titles - first heading should be "First Heading",
              ;; not the file title "Testing File"
              (should (equal (vulpea-note-title note1) "First Heading"))
              (should (equal (vulpea-note-title note2) "Second Heading"))

              ;; Verify both are heading-level notes (level > 0)
              (should (= (vulpea-note-level note1) 1))
              (should (= (vulpea-note-level note2) 1))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-update-file-replaces-existing ()
  "Test updating file replaces existing notes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 1\n")))
      (unwind-protect
          (progn
            ;; First update
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [title] :from notes
                                      :where (= id $s1)]
                                     "test-id"))))
              (should (equal (elt row 0) "Version 1")))

            ;; Update file content
            (with-temp-file path
              (insert ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Version 2\n"))

            ;; Second update
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [title] :from notes
                                      :where (= id $s1)]
                                     "test-id"))))
              (should (equal (elt row 0) "Version 2"))))
        (delete-file path)))))

;;; Parse Granularity Tests

(defconst vulpea-db-extract-test--granularity-corpus
  ":PROPERTIES:
:ID: corpus-file-id
:ROAM_REFS: [[https://drawer.example.com][drawer link]]
:END:
#+TITLE: Title with [[id:title-target][title link]]
#+DESCRIPTION: keyword [[id:kw-target][kw link]]
#+FILETAGS: :corpus:

Paragraph with [[id:para-target][bracket]], plain https://plain.example.com
and <https://angle.example.com> angle link.
A fuzzy [[Some Heading]] and a [[file:other.org::*Section][file search link]].
A nested description [[id:outer][desc with https://inner.example.com]].

- category :: wine
- link :: [[id:meta-target][meta desc]]
- multi :: value one
- multi :: *bold* value
- when :: <2026-08-01 sat 10:00>
- [[id:tag-target][tagged key]] :: tagged value

Inline markup hides links from org: ~[[id:in-code][code]]~ and
=[[id:in-verbatim][verb]]= and {{{m([[id:in-macro][arg]])}}} stay out.

| cell | [[id:table-target][table link]] |

Unbalanced [[bracket text that never closes

Followed by a real [[id:after-unbalanced][link after unbalanced]].

#+begin_src org
[[id:src-target][src link should NOT be indexed]]
#+end_src

#+begin_example
[[id:example-target][example link should NOT be indexed]]
#+end_example

# comment [[id:comment-target][comment link should NOT be indexed]]

#+begin_quote
Quote with [[id:quote-target][quote link]].
#+end_quote

#+begin_verse
Verse with [[id:verse-target][verse link]]
#+end_verse

: fixed-width [[id:fixed-target][fixed link NOT indexed]]

* TODO [#A] Heading with [[id:h-title-target][h title link]] :htag:
SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-09-01 Tue>
:PROPERTIES:
:ID: corpus-heading-id
:CUSTOM: custom value
:END:
- rating :: 10

Heading body [[id:h-body-target][h body link]].

** Plain subheading without id
Sub body [[id:sub-target][sub link]] belongs to parent note.

* DONE Task heading
CLOSED: [2026-06-01 Mon 10:00]
:PROPERTIES:
:ID: corpus-task-id
:END:
:LOGBOOK:
- State \"DONE\"       from \"TODO\"       [2026-06-01 Mon 10:00]
:END:

Task body.
"
  "Adversarial org content exercising every extraction feature.")

(ert-deftest vulpea-db-extract-corpus-is-adversarial ()
  "Validate the corpus exercises what the equivalence test relies on.
Uses the object-granularity pipeline as reference: links in
paragraphs, tables, quotes, verses, and meta values are indexed;
links in src/example blocks, comments, fixed-width areas, property
drawers, and non-title keywords are not."
  (let ((path (vulpea-test--create-temp-org-file
               vulpea-db-extract-test--granularity-corpus)))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (vulpea-db-parse-granularity 'object)
               (ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (dests (mapcar (lambda (l) (plist-get l :dest))
                              (plist-get file-node :links))))
          (dolist (dest '("para-target" "//plain.example.com"
                          "//angle.example.com" "meta-target"
                          "table-target" "quote-target" "verse-target"
                          "title-target" "outer" "after-unbalanced"))
            (should (member dest dests)))
          (dolist (dest '("src-target" "example-target" "comment-target"
                          "fixed-target" "kw-target" "//drawer.example.com"
                          "//inner.example.com"
                          "in-code" "in-verbatim" "in-macro"))
            (should-not (member dest dests)))
          ;; Timestamp day names are normalized, as org interprets them
          (should (equal (cdr (assoc "when" (plist-get file-node :meta)))
                         '("<2026-08-01 Sat 10:00>"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-element-granularity-equivalence ()
  "Element-granularity parsing produces identical extraction output.
Parses the adversarial corpus with both granularities and requires
the file node and every heading node to be plist-equal.  This is the
contract that lets the cheaper element-granularity parse replace the
full object parse without a behavior change."
  (let ((path (vulpea-test--create-temp-org-file
               vulpea-db-extract-test--granularity-corpus)))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx-object (let ((vulpea-db-parse-granularity 'object))
                             (vulpea-db--parse-file path)))
               (ctx-element (let ((vulpea-db-parse-granularity 'element))
                              (vulpea-db--parse-file path))))
          (should (equal (vulpea-parse-ctx-file-node ctx-object)
                         (vulpea-parse-ctx-file-node ctx-element)))
          (should (equal (length (vulpea-parse-ctx-heading-nodes ctx-object))
                         (length (vulpea-parse-ctx-heading-nodes ctx-element))))
          (cl-loop for obj-node in (vulpea-parse-ctx-heading-nodes ctx-object)
                   for el-node in (vulpea-parse-ctx-heading-nodes ctx-element)
                   do (should (equal obj-node el-node))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-bracket-only-links ()
  "With `vulpea-db-index-plain-links' nil, only bracket links index.
Plain (https://...) and angle (<https://...>) links disappear from
the corpus output while every bracketed link stays, and both
granularities agree on that reduced output."
  (let ((path (vulpea-test--create-temp-org-file
               vulpea-db-extract-test--granularity-corpus))
        (vulpea-db-index-plain-links nil))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx-object (let ((vulpea-db-parse-granularity 'object))
                             (vulpea-db--parse-file path)))
               (ctx-element (let ((vulpea-db-parse-granularity 'element))
                              (vulpea-db--parse-file path)))
               (node (vulpea-parse-ctx-file-node ctx-element))
               (dests (mapcar (lambda (l) (plist-get l :dest))
                              (plist-get node :links))))
          ;; Bracketed links stay
          (dolist (dest '("title-target" "para-target" "meta-target"
                          "table-target" "quote-target" "verse-target"
                          "after-unbalanced" "outer" "Some Heading"
                          "other.org"))
            (should (member dest dests)))
          ;; Plain and angle links are gone
          (dolist (dest '("//plain.example.com" "//angle.example.com"))
            (should-not (member dest dests)))
          ;; Both granularities agree on the reduced output
          (should (equal (vulpea-parse-ctx-file-node ctx-object)
                         (vulpea-parse-ctx-file-node ctx-element)))
          (cl-loop for obj-node in (vulpea-parse-ctx-heading-nodes ctx-object)
                   for el-node in (vulpea-parse-ctx-heading-nodes ctx-element)
                   do (should (equal obj-node el-node))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-element-granularity-equivalence-single-temp-buffer ()
  "Granularity equivalence also holds under \\='single-temp-buffer.
That parse method reuses one buffer and never re-runs `org-mode',
so buffer-state assumptions of the element-granularity link scan and
the attach-dir buffer scan get no per-file re-initialization to hide
behind."
  (let ((path (vulpea-test--create-temp-org-file
               vulpea-db-extract-test--granularity-corpus))
        (vulpea-db-parse-method 'single-temp-buffer))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx-object (let ((vulpea-db-parse-granularity 'object))
                             (vulpea-db--parse-file path)))
               (ctx-element (let ((vulpea-db-parse-granularity 'element))
                              (vulpea-db--parse-file path))))
          (should (equal (vulpea-parse-ctx-file-node ctx-object)
                         (vulpea-parse-ctx-file-node ctx-element)))
          (cl-loop for obj-node in (vulpea-parse-ctx-heading-nodes ctx-object)
                   for el-node in (vulpea-parse-ctx-heading-nodes ctx-element)
                   do (should (equal obj-node el-node))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-single-temp-buffer-no-state-leak ()
  "Reused parse buffer must not leak one file's state into the next.
Parses a DIR- and link-heavy file, then a plain file, in
\\='single-temp-buffer mode: the plain file must take the ID-derived
attach-dir fast path (previous file's DIR property must not linger)
and carry none of the previous file's links or meta."
  (let ((rich (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: rich-id\n:DIR: /abs/rich/attachments\n:END:\n#+TITLE: Rich\n\nBody [[id:rich-target][rich link]].\n\n- key :: value\n"))
        (plain (vulpea-test--create-temp-org-file
                ":PROPERTIES:\n:ID: plain-id\n:END:\n#+TITLE: Plain\n\nNo links here.\n"))
        (vulpea-db-parse-method 'single-temp-buffer))
    (unwind-protect
        (progn
          ;; Parse the rich file first to poison the reused buffer
          (let ((node (vulpea-parse-ctx-file-node (vulpea-db--parse-file rich))))
            (should (equal (plist-get node :attach-dir) "/abs/rich/attachments"))
            (should (equal (mapcar (lambda (l) (plist-get l :dest))
                                   (plist-get node :links))
                           '("rich-target")))
            (should (equal (plist-get node :meta) '(("key" "value")))))
          ;; The plain file parsed right after must be unaffected
          (let ((node (vulpea-parse-ctx-file-node (vulpea-db--parse-file plain))))
            (require 'org-attach)
            (should (equal (plist-get node :attach-dir)
                           (let ((default-directory (file-name-directory plain)))
                             (org-attach-dir-from-id "plain-id" 'existing))))
            (should (null (plist-get node :links)))
            (should (null (plist-get node :meta)))))
      (delete-file rich)
      (delete-file plain))))

;;; Org-id Registration Tests

(ert-deftest vulpea-db-extract-registers-ids-with-org-id ()
  "Indexing a file registers every note ID in `org-id-locations'.
Locks the org-id contract: after an update, each ID maps to the
file's abbreviated path and the file is listed in `org-id-files',
exactly as per-ID `org-id-add-location' calls would have done."
  (let ((org-id-track-globally t)
        (org-id-locations (make-hash-table :test #'equal))
        (org-id-files nil))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: org-id-file-id\n:END:\n#+TITLE: F\n\n* H\n:PROPERTIES:\n:ID: org-id-heading-id\n:END:\n")))
        (unwind-protect
            (let ((vulpea-db-index-heading-level t))
              (vulpea-db-update-file path)
              (let ((afile (abbreviate-file-name path)))
                (should (equal (gethash "org-id-file-id" org-id-locations) afile))
                (should (equal (gethash "org-id-heading-id" org-id-locations) afile))
                (should (member afile org-id-files))))
          (delete-file path))))))

(ert-deftest vulpea-db-extract-no-org-id-registration-when-tracking-off ()
  "With `org-id-track-globally' nil, indexing must not touch org-id."
  (let ((org-id-track-globally nil)
        (org-id-locations (make-hash-table :test #'equal))
        (org-id-files nil))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: no-track-id\n:END:\n#+TITLE: F\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (zerop (hash-table-count org-id-locations)))
              (should (null org-id-files)))
          (delete-file path))))))

;;; Content Hash Tests

(ert-deftest vulpea-db-extract-hash-matches-string-hash ()
  "Parse ctx hash equals the sha256 of the file content as a string.
Locks compatibility with hashes stored in existing databases (computed
via `buffer-string'), so a hash implementation change cannot silently
force a full re-index or, worse, miss real changes."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: hash-id\n:END:\n#+TITLE: Ünïcode — привіт 🦊\n\nBody with ünïcode and\ttabs.\n")))
    (unwind-protect
        (let ((ctx (vulpea-db--parse-file path)))
          (should (equal (vulpea-parse-ctx-hash ctx)
                         (with-temp-buffer
                           (insert-file-contents path)
                           (secure-hash 'sha256 (buffer-string))))))
      (delete-file path))))

;;; Attachment Directory Tests

(ert-deftest vulpea-db-extract-attach-dir-file-level ()
  "Test ATTACH_DIR property extraction at file level."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-with-attachments\n:ATTACH_DIR: data/attachments/file-id\n:END:\n#+TITLE: File With Attachments\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :attach-dir) "data/attachments/file-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-heading-level ()
  "Test ATTACH_DIR property extraction at heading level."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: heading-with-attachments\n:ATTACH_DIR: data/attachments/heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :attach-dir) "data/attachments/heading-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-defaults-to-id-derived ()
  "Without DIR/ATTACH_DIR properties, attach-dir derives from the ID.
Locks equivalence with `org-attach-dir': for a note carrying only an
ID, the attachment directory is what `org-attach-dir-from-id'
computes for that ID relative to the note's file."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: plain-file-id\n:END:\n#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: plain-heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-node (car (vulpea-parse-ctx-heading-nodes ctx)))
               (default-directory (file-name-directory path)))
          (require 'org-attach)
          (should (equal (plist-get file-node :attach-dir)
                         (org-attach-dir-from-id "plain-file-id" 'existing)))
          (should (equal (plist-get heading-node :attach-dir)
                         (org-attach-dir-from-id "plain-heading-id" 'existing))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-dir-property ()
  "The modern DIR property sets attach-dir, same as ATTACH_DIR."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: dir-heading-id\n:DIR: /abs/attachments/dir\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (node (car (vulpea-parse-ctx-heading-nodes ctx))))
          (should (equal (plist-get node :attach-dir)
                         "/abs/attachments/dir")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-inherited-dir ()
  "A parent heading's DIR is inherited when property inheritance is on."
  (let ((org-use-property-inheritance t)
        (path (vulpea-test--create-temp-org-file
               "#+TITLE: File\n\n* Parent\n:PROPERTIES:\n:DIR: /abs/parent/attachments\n:END:\n** Child\n:PROPERTIES:\n:ID: child-heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (node (car (vulpea-parse-ctx-heading-nodes ctx))))
          (should (equal (plist-get node :attach-dir)
                         "/abs/parent/attachments")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-file-property-keyword ()
  "A file-wide #+PROPERTY: DIR keyword sets attach-dir when inherited."
  (let ((org-use-property-inheritance t)
        (path (vulpea-test--create-temp-org-file
               "#+PROPERTY: DIR /abs/keyword/attachments\n#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: kw-heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (node (car (vulpea-parse-ctx-heading-nodes ctx))))
          (should (equal (plist-get node :attach-dir)
                         "/abs/keyword/attachments")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-attach-dir-round-trip ()
  "Test attach-dir full round-trip: extract → database → query."
  (let* ((temp-db-file (make-temp-file "vulpea-test-" nil ".db"))
         (vulpea-db-location temp-db-file)
         (vulpea-db--connection nil)
         (path (vulpea-test--create-temp-org-file
                ":PROPERTIES:\n:ID: note-with-attach\n:ATTACH_DIR: /path/to/attachments\n:END:\n#+TITLE: Note With Attachments\n")))
    (unwind-protect
        (progn
          (vulpea-db)
          (vulpea-db-update-file path)
          (let ((note (vulpea-db-get-by-id "note-with-attach")))
            (should (equal (vulpea-note-attach-dir note) "/path/to/attachments"))))
      (when vulpea-db--connection
        (vulpea-db-close))
      (when (file-exists-p temp-db-file)
        (delete-file temp-db-file))
      (delete-file path))))

;;; Archive Exclusion Tests

(ert-deftest vulpea-db-extract-archive-file-by-tag ()
  "Test file-level node with archive tag is excluded."
  (let ((org-archive-tag "Archive")
        (vulpea-db-exclude-archived t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: archived-file\n:END:\n#+TITLE: Archived File\n#+FILETAGS: :Archive:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (null node)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-file-by-property ()
  "Test file-level node with ARCHIVE_TIME property is excluded."
  (let ((vulpea-db-exclude-archived t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: archived-file\n:ARCHIVE_TIME: 2025-12-07 Sun 11:30\n:END:\n#+TITLE: Archived File\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (null node)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-heading-direct-tag ()
  "Test heading with direct archive tag is excluded."
  (let ((org-archive-tag "Archive")
        (vulpea-db-exclude-archived t)
        (vulpea-db-index-heading-level t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Normal Heading\n:PROPERTIES:\n:ID: normal-id\n:END:\n\n* Archived Heading :Archive:\n:PROPERTIES:\n:ID: archived-id\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "normal-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-heading-inherited-tag ()
  "Test heading with inherited archive tag is excluded."
  (let ((org-archive-tag "Archive")
        (vulpea-db-exclude-archived t)
        (vulpea-db-index-heading-level t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Normal Heading\n:PROPERTIES:\n:ID: normal-id\n:END:\n\n* Archive :Archive:\n\n** Nested Archived Heading\n:PROPERTIES:\n:ID: nested-archived-id\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          ;; Only normal-id should be present, nested-archived-id excluded
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "normal-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-heading-by-property ()
  "Test heading with ARCHIVE_TIME property is excluded."
  (let ((vulpea-db-exclude-archived t)
        (vulpea-db-index-heading-level t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Normal Heading\n:PROPERTIES:\n:ID: normal-id\n:END:\n\n* Archived Heading\n:PROPERTIES:\n:ID: archived-id\n:ARCHIVE_TIME: 2025-12-07 Sun 11:30\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          (should (= (length nodes) 1))
          (should (equal (plist-get (car nodes) :id) "normal-id")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-heading-filetag ()
  "Test heading is excluded when file has archive filetag."
  (let ((org-archive-tag "Archive")
        (vulpea-db-exclude-archived t)
        (vulpea-db-index-heading-level t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n#+FILETAGS: :Archive:\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx)))
          ;; Both file and heading should be excluded
          (should (null file-node))
          (should (null heading-nodes)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-archive-disabled ()
  "Test archived entries are included when exclusion is disabled."
  (let ((org-archive-tag "Archive")
        (vulpea-db-exclude-archived nil)
        (vulpea-db-index-heading-level t)
        (path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n#+FILETAGS: :Archive:\n\n* Archived Heading :Archive:\n:PROPERTIES:\n:ID: archived-id\n:ARCHIVE_TIME: 2025-12-07 Sun 11:30\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx)))
          ;; Both should be present when exclusion is disabled
          (should file-node)
          (should (= (length heading-nodes) 1)))
      (delete-file path))))

;;; Created-At Extraction Tests

(ert-deftest vulpea-db-extract-created-at-from-property ()
  "Test created-at is populated from CREATED property."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: note-with-created\n:CREATED: [2025-12-08 Sun 14:30]\n:END:\n#+TITLE: Note With Created\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            ;; Verify created-at was stored
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [created-at] :from notes
                                      :where (= id $s1)]
                                     "note-with-created"))))
              (should row)
              (should (equal (car row) "2025-12-08"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-created-at-iso-format ()
  "Test created-at extraction from ISO date format."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: note-iso-date\n:CREATED: 2025-12-08\n:END:\n#+TITLE: Note ISO Date\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [created-at] :from notes
                                      :where (= id $s1)]
                                     "note-iso-date"))))
              (should row)
              (should (equal (car row) "2025-12-08"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-created-at-heading-level ()
  "Test created-at extraction at heading level."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: heading-with-created\n:CREATED: [2025-12-09]\n:END:\n")))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [created-at] :from notes
                                      :where (= id $s1)]
                                     "heading-with-created"))))
              (should row)
              (should (equal (car row) "2025-12-09"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-created-at-missing ()
  "Test created-at is null when CREATED property is missing."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: note-no-created\n:END:\n#+TITLE: Note Without Created\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [created-at] :from notes
                                      :where (= id $s1)]
                                     "note-no-created"))))
              (should row)
              (should (null (car row)))))
        (delete-file path)))))

;;; File-Title Tests

(ert-deftest vulpea-db-extract-file-title-file-level ()
  "Test file-level note has file-title equal to its title."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: My File Title\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :title) "My File Title"))
          (should (equal (plist-get node :file-title) "My File Title")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-heading-level ()
  "Test heading-level note has file-title from file."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: Parent File\n\n* Child Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :title) "Child Heading"))
          (should (equal (plist-get node :file-title) "Parent File")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-fallback-to-filename ()
  "Test file-title falls back to filename when no #+TITLE."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (file-node (vulpea-parse-ctx-file-node ctx))
               (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
               (expected-title (file-name-base path)))
          ;; File-level note should use filename as title and file-title
          (should (equal (plist-get file-node :title) expected-title))
          (should (equal (plist-get file-node :file-title) expected-title))
          ;; Heading-level note should have file-title from filename
          (should (equal (plist-get (car heading-nodes) :file-title) expected-title)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-round-trip ()
  "Test file-title survives database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: Parent File\n\n* Child Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)
            ;; Check file-level note
            (let ((file-note (vulpea-db-get-by-id "file-id")))
              (should (equal (vulpea-note-title file-note) "Parent File"))
              (should (equal (vulpea-note-file-title file-note) "Parent File")))
            ;; Check heading-level note
            (let ((heading-note (vulpea-db-get-by-id "heading-id")))
              (should (equal (vulpea-note-title heading-note) "Child Heading"))
              (should (equal (vulpea-note-file-title heading-note) "Parent File"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-file-title-nested-headings ()
  "Test file-title is consistent for deeply nested headings."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: Root File\n\n* Level 1\n:PROPERTIES:\n:ID: level1-id\n:END:\n\n** Level 2\n:PROPERTIES:\n:ID: level2-id\n:END:\n\n*** Level 3\n:PROPERTIES:\n:ID: level3-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx)))
          ;; All heading nodes should have the same file-title
          (should (= (length nodes) 3))
          (dolist (node nodes)
            (should (equal (plist-get node :file-title) "Root File"))))
      (delete-file path))))

;;; Properties Table Tests

(ert-deftest vulpea-db-extract-properties-table-populated ()
  "Test properties table is populated during extraction."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: note-with-props\n:CREATED: [2025-12-08]\n:CATEGORY: journal\n:END:\n#+TITLE: Note With Properties\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            ;; Verify properties were stored in normalized table
            (let ((props (emacsql (vulpea-db)
                                  [:select [key value] :from properties
                                   :where (= note-id $s1)
                                   :order-by [(asc key)]]
                                  "note-with-props")))
              (should (>= (length props) 2))
              (should (member '("CATEGORY" "journal") props))
              (should (member '("CREATED" "[2025-12-08]") props))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-properties-table-heading ()
  "Test properties table is populated for headings."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Heading\n:PROPERTIES:\n:ID: heading-props\n:CUSTOM_PROP: custom-value\n:END:\n")))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)
            (let ((props (emacsql (vulpea-db)
                                  [:select [key value] :from properties
                                   :where (= note-id $s1)]
                                  "heading-props")))
              (should props)
              (should (member '("CUSTOM_PROP" "custom-value") props))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-category-keyword-as-property ()
  "Test that #+CATEGORY keyword at file level is extracted as a property.

See https://github.com/d12frosted/vulpea/issues/257."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: category-kw-note\n:END:\n#+TITLE: Note With Category Keyword\n#+CATEGORY: journal\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let ((props (emacsql (vulpea-db)
                                  [:select [key value] :from properties
                                   :where (= note-id $s1)
                                   :order-by [(asc key)]]
                                  "category-kw-note")))
              (should (member '("CATEGORY" "journal") props))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-category-drawer-overrides-keyword ()
  "Test that :CATEGORY: in property drawer takes precedence over #+CATEGORY.

See https://github.com/d12frosted/vulpea/issues/257."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: category-both-note\n:CATEGORY: from-drawer\n:END:\n#+TITLE: Note With Both\n#+CATEGORY: from-keyword\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let ((props (emacsql (vulpea-db)
                                  [:select [key value] :from properties
                                   :where (and (= note-id $s1)
                                               (= key "CATEGORY"))]
                                  "category-both-note")))
              (should (equal 1 (length props)))
              (should (equal '("CATEGORY" "from-drawer") (car props)))))
        (delete-file path)))))

;;; Title Link Stripping Tests

(ert-deftest vulpea-db-extract-file-title-strips-links ()
  "Test file-level title with org links has markup stripped."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: linked-title-id\n:END:\n#+TITLE: The Memory Illusion [[id:book-id][book]] by Dr. [[id:julia-id][Julia Shaw]]\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :title)
                         "The Memory Illusion book by Dr. Julia Shaw")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-title-strips-links ()
  "Test heading-level title with org links has markup stripped."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: File\n\n"
                "* The Memory Illusion [[id:book-id][book]] by Dr. [[id:julia-id][Julia Shaw]]\n"
                ":PROPERTIES:\n"
                ":ID: heading-linked-id\n"
                ":END:\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes)))
          (should (equal (plist-get node :title)
                         "The Memory Illusion book by Dr. Julia Shaw")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-title-links-preserved ()
  "Test that links in heading titles are still extracted as links."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: File\n\n"
                "* Review of [[id:book-id][the book]]\n"
                ":PROPERTIES:\n"
                ":ID: review-id\n"
                ":END:\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (node (car nodes))
               (links (plist-get node :links))
               (dests (mapcar (lambda (l) (plist-get l :dest)) links)))
          ;; Title should be clean
          (should (equal (plist-get node :title) "Review of the book"))
          ;; Link should still be extracted
          (should (member "book-id" dests)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-links-preserved ()
  "Test that links in file-level titles are still extracted as links."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-linked-id\n:END:\n#+TITLE: Review of [[id:book-id][the book]]\n\nSome body text.\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links))
               (dests (mapcar (lambda (l) (plist-get l :dest)) links)))
          ;; Title should be clean
          (should (equal (plist-get node :title) "Review of the book"))
          ;; Link from title should be in links list
          (should (member "book-id" dests)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-outline-path-strips-links ()
  "Test outline path strips link markup from parent headings."
  (let ((path (vulpea-test--create-temp-org-file
               (concat
                ":PROPERTIES:\n:ID: file-id\n:END:\n"
                "#+TITLE: File\n\n"
                "* Books about [[id:topic-id][AI]]\n"
                ":PROPERTIES:\n"
                ":ID: parent-id\n"
                ":END:\n\n"
                "** Child Note\n"
                ":PROPERTIES:\n"
                ":ID: child-id\n"
                ":END:\n"))))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (child (seq-find (lambda (n) (equal (plist-get n :id) "child-id")) nodes)))
          (should (equal (plist-get child :outline-path)
                         '("Books about AI"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-links-round-trip ()
  "Test that links in file-level titles survive database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: file-title-link-rt\n:END:\n#+TITLE: Review of [[id:book-rt][the book]]\n\nBody text.\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let* ((note (vulpea-db-get-by-id "file-title-link-rt"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links)))
              ;; Title should be clean
              (should (equal (vulpea-note-title note) "Review of the book"))
              ;; Link from title should be in DB
              (should (member "book-rt" dests))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-heading-title-links-round-trip ()
  "Test that links in heading titles survive database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 (concat
                  ":PROPERTIES:\n:ID: file-id\n:END:\n"
                  "#+TITLE: File\n\n"
                  "* Review of [[id:book-rt][the book]]\n"
                  ":PROPERTIES:\n"
                  ":ID: heading-title-link-rt\n"
                  ":END:\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file path)
            (let* ((note (vulpea-db-get-by-id "heading-title-link-rt"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links)))
              ;; Title should be clean
              (should (equal (vulpea-note-title note) "Review of the book"))
              ;; Link from title should be in DB
              (should (member "book-rt" dests))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-heading-title-link-pos-accounts-for-cookies ()
  "A title link's stored :pos is exact regardless of TODO/priority cookies.

The heading offset must count the trailing space after both the TODO
keyword and the priority cookie; a priority cookie is written as \"[#X] \"
\(five characters).  A stored position that is off by one lands on the
space before the link and silently breaks `vulpea-propagate-title-change'."
  (dolist (head '("* Review of [[id:bk][t]]"
                  "* TODO Review of [[id:bk][t]]"
                  "* [#A] Review of [[id:bk][t]]"
                  "* TODO [#A] Review of [[id:bk][t]]"))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n"
                           head "\n:PROPERTIES:\n:ID: h-cookie-pos\n:END:\n"))))
        (unwind-protect
            (let ((vulpea-db-index-heading-level t))
              (vulpea-db-update-file path)
              (let* ((link (car (vulpea-db-query-links-to "bk")))
                     (db-pos (plist-get link :pos))
                     (actual (with-temp-buffer
                               (insert-file-contents path)
                               (goto-char (point-min))
                               (search-forward "[[id:bk")
                               (match-beginning 0))))
                (should link)
                ;; the stored position must land exactly on the link
                (should (= db-pos actual))))
          (delete-file path))))))

;;; Case-insensitive Property Key Tests

(ert-deftest vulpea-db-extract-file-node-lowercase-id ()
  "Test file-level node with lowercase :id: property is extracted."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:id: lowercase-id\n:END:\n#+TITLE: Lowercase ID Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should node)
          (should (equal (plist-get node :id) "lowercase-id"))
          (should (equal (plist-get node :title) "Lowercase ID Note")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-lowercase-id-round-trip ()
  "Test file-level node with lowercase :id: survives database round-trip."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:id: lowercase-rt\n:END:\n#+TITLE: Lowercase RT\n")))
      (unwind-protect
          (progn
            (vulpea-db-update-file path)
            (let ((note (vulpea-db-get-by-id "lowercase-rt")))
              (should note)
              (should (equal (vulpea-note-title note) "Lowercase RT"))))
        (delete-file path)))))

(ert-deftest vulpea-db-extract-properties-case-insensitive ()
  "Test that property keys are normalized to uppercase."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:id: test-id\n:Custom_Prop: value\n:END:\n#+TITLE: Test\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (properties (plist-get node :properties)))
          (should (cdr (assoc "ID" properties)))
          (should (cdr (assoc "CUSTOM_PROP" properties))))
      (delete-file path))))

;;; Title Link Backlink Scenarios
;;
;; These tests verify that links in heading/file titles are persisted
;; in both the notes JSON column AND the normalized links table, so
;; that vulpea-db-query-by-links-some (used by vulpea-find-backlink)
;; can find them.

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-1 ()
  "Scenario 1: non-note heading with title link -> file note.
File abc / * Mention [[id:foo][person]] (no ID) -> abc -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Mention [[id:foo][person]]\n"))))
      (unwind-protect
          (progn
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "abc"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              ;; Link should be in notes JSON
              (should (member "foo" dests))
              ;; Link should be in links table (backlink query)
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "abc"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-2 ()
  "Scenario 2: note heading with title link.
File abc / * Mention [[id:foo][person]] :ID: xyz -> xyz -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Mention [[id:foo][person]]\n"
                      ":PROPERTIES:\n"
                      ":ID: xyz\n"
                      ":END:\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "xyz"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              ;; Link should be in notes JSON
              (should (member "foo" dests))
              ;; Link should be in links table (backlink query)
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "xyz"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-3 ()
  "Scenario 3: nested note heading with title link under note parent.
File abc / * Parent :ID: jkl / ** Mention [[id:foo][person]] :ID: xyz -> xyz -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Parent header\n"
                      ":PROPERTIES:\n"
                      ":ID: jkl\n"
                      ":END:\n\n"
                      "** Mention [[id:foo][person]]\n"
                      ":PROPERTIES:\n"
                      ":ID: xyz\n"
                      ":END:\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "xyz"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              (should (member "foo" dests))
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "xyz"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-4 ()
  "Scenario 4: note heading with title link under non-note parent.
File abc / * Parent (no ID) / ** Mention [[id:foo][person]] :ID: xyz -> xyz -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Parent header\n\n"
                      "** Mention [[id:foo][person]]\n"
                      ":PROPERTIES:\n"
                      ":ID: xyz\n"
                      ":END:\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "xyz"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              (should (member "foo" dests))
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "xyz"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-5 ()
  "Scenario 5: non-note heading with title link under note parent.
File abc / * Parent :ID: jkl / ** Mention [[id:foo][person]] (no ID) -> jkl -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Parent header\n"
                      ":PROPERTIES:\n"
                      ":ID: jkl\n"
                      ":END:\n\n"
                      "** Mention [[id:foo][person]]\n"))))
      (unwind-protect
          (let ((vulpea-db-index-heading-level t))
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "jkl"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              (should (member "foo" dests))
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "jkl"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

(ert-deftest vulpea-db-extract-title-link-backlink-scenario-6 ()
  "Scenario 6: non-note heading with title link under non-note parent.
File abc / * Parent (no ID) / ** Mention [[id:foo][person]] (no ID) -> abc -> foo."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((foo-path (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: foo\n:END:\n#+TITLE: Person\n"))
          (src-path (vulpea-test--create-temp-org-file
                     (concat
                      ":PROPERTIES:\n:ID: abc\n:END:\n"
                      "#+TITLE: file-level note\n\n"
                      "* Parent header\n\n"
                      "** Mention [[id:foo][person]]\n"))))
      (unwind-protect
          (progn
            (vulpea-db-update-file foo-path)
            (vulpea-db-update-file src-path)
            (let* ((note (vulpea-db-get-by-id "abc"))
                   (links (vulpea-note-links note))
                   (dests (mapcar (lambda (l) (plist-get l :dest)) links))
                   (backlinks (vulpea-db-query-by-links-some
                               (list (cons "id" "foo")))))
              (should (member "foo" dests))
              (should (seq-find (lambda (n) (equal (vulpea-note-id n) "abc"))
                                backlinks))))
        (delete-file foo-path)
        (delete-file src-path)))))

;;; Link Description Extraction Tests

(ert-deftest vulpea-db-extract-link-description-basic ()
  "Test that link descriptions are extracted from org-element links."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test Note\n\nSee [[id:target][My Description]] for details.\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links)))
          (should (= (length links) 1))
          (should (equal (plist-get (car links) :dest) "target"))
          (should (equal (plist-get (car links) :type) "id"))
          (should (equal (plist-get (car links) :description) "My Description")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-link-description-nil ()
  "Test that links without descriptions have nil description."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test Note\n\nSee [[id:target]] for details.\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links)))
          (should (= (length links) 1))
          (should (equal (plist-get (car links) :dest) "target"))
          (should (null (plist-get (car links) :description))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-link-description-with-markup ()
  "Test that link descriptions preserve org markup."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test Note\n\nSee [[id:target][*bold* text]] for details.\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links)))
          (should (= (length links) 1))
          (should (equal (plist-get (car links) :description) "*bold* text")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-link-description-from-string ()
  "Test description extraction from raw string (e.g., title)."
  (let ((links (vulpea-db--extract-links-from-string
                "Hello [[id:foo][Foo Link]] world [[id:bar]] end")))
    (should (= (length links) 2))
    (should (equal (plist-get (car links) :dest) "foo"))
    (should (equal (plist-get (car links) :description) "Foo Link"))
    (should (equal (plist-get (cadr links) :dest) "bar"))
    (should (null (plist-get (cadr links) :description)))))

(ert-deftest vulpea-db-extract-link-description-round-trip ()
  "Test full round-trip: extract → database → query."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Insert a note with a link that has a description
    (vulpea-test--insert-test-note
     "src-note" "Source"
     :links '((:dest "target-note" :type "id" :pos 100 :description "Target Title")))
    ;; Check note's links field
    (let* ((note (vulpea-db-get-by-id "src-note"))
           (links (vulpea-note-links note)))
      (should (= (length links) 1))
      (should (equal (plist-get (car links) :description) "Target Title")))
    ;; Check normalized links table via query
    (let ((links (vulpea-db-query-links-from "src-note")))
      (should (= (length links) 1))
      (should (equal (plist-get (car links) :description) "Target Title")))))

;;; Emphasis Stripping Tests

(ert-deftest vulpea-db-extract-strip-emphasis-basic ()
  "Test basic emphasis marker stripping."
  ;; Bold
  (should (equal (vulpea-db--strip-emphasis "*bold*") "bold"))
  ;; Italic
  (should (equal (vulpea-db--strip-emphasis "/italic/") "italic"))
  ;; Underline
  (should (equal (vulpea-db--strip-emphasis "_underline_") "underline"))
  ;; Strikethrough
  (should (equal (vulpea-db--strip-emphasis "+strike+") "strike"))
  ;; Code/verbatim
  (should (equal (vulpea-db--strip-emphasis "=code=") "code"))
  (should (equal (vulpea-db--strip-emphasis "~verbatim~") "verbatim"))
  ;; No emphasis
  (should (equal (vulpea-db--strip-emphasis "plain text") "plain text")))

(ert-deftest vulpea-db-extract-strip-emphasis-mixed ()
  "Test stripping multiple emphasis markers in one string."
  (should (equal (vulpea-db--strip-emphasis "This has *bold* and /italic/ text")
                 "This has bold and italic text"))
  (should (equal (vulpea-db--strip-emphasis "=code= with ~verbatim~")
                 "code with verbatim")))

(ert-deftest vulpea-db-extract-strip-emphasis-nested ()
  "Test that nested/adjacent markers are handled correctly."
  ;; Adjacent markers without space - Org doesn't recognize these
  ;; because post-marker must be space/punctuation, not another marker
  (should (equal (vulpea-db--strip-emphasis "*bold*/italic/")
                 "*bold*/italic/"))
  ;; Adjacent markers with space - both are recognized
  (should (equal (vulpea-db--strip-emphasis "*bold* /italic/")
                 "bold italic"))
  ;; Word boundaries required - emphasis in middle of word not recognized
  (should (equal (vulpea-db--strip-emphasis "not*bold*here")
                 "not*bold*here")))

(ert-deftest vulpea-db-extract-strip-emphasis-multiword ()
  "Test emphasis spanning multiple words."
  (should (equal (vulpea-db--strip-emphasis "*bold text here*")
                 "bold text here"))
  (should (equal (vulpea-db--strip-emphasis "=code with spaces=")
                 "code with spaces")))

(ert-deftest vulpea-db-extract-strip-emphasis-nil ()
  "Test that nil input returns nil."
  (should (null (vulpea-db--strip-emphasis nil))))

(ert-deftest vulpea-db-extract-file-title-with-emphasis ()
  "Test that emphasis is stripped from file titles."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: My =code= Project\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :title) "My code Project")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-title-with-multiple-emphasis ()
  "Test that multiple emphasis markers are stripped from file titles."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: *Bold* and /italic/ title\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :title) "Bold and italic title")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-heading-title-with-emphasis ()
  "Test that emphasis is stripped from heading titles."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Heading with =code=\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (heading (car nodes)))
          (should (equal (plist-get heading :title) "Heading with code")))
      (delete-file path))))

(ert-deftest vulpea-db-extract-outline-path-with-emphasis ()
  "Test that emphasis is stripped from outline path."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: file-id\n:END:\n#+TITLE: File\n\n* Parent with *bold*\n** Child heading\n:PROPERTIES:\n:ID: child-id\n:END:\n")))
    (unwind-protect
        (let* ((vulpea-db-index-heading-level t)
               (ctx (vulpea-db--parse-file path))
               (nodes (vulpea-parse-ctx-heading-nodes ctx))
               (child (car nodes)))
          (should (equal (plist-get child :outline-path) '("Parent with bold"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-alias-with-emphasis ()
  "Test that emphasis is stripped from aliases."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: test-id\n:ALIASES: \"=Code= Alias\" /italic/\n:END:\n#+TITLE: Test\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (member "Code Alias" (plist-get node :aliases)))
          (should (member "italic" (plist-get node :aliases))))
      (delete-file path))))

;;; Parse Method Override Tests

(ert-deftest vulpea-db-extract-non-org-uses-find-file ()
  "Non-.org files are parsed via find-file even when parse-method is temp-buffer."
  (let* ((vulpea-db-parse-method 'temp-buffer)
         (base (make-temp-file "vulpea-test-" nil ".org"))
         (path (concat base ".age")))
    (rename-file base path)
    (with-temp-file path
      (insert ":PROPERTIES:\n:ID: enc-id\n:END:\n#+TITLE: Encrypted\n"))
    (unwind-protect
        (let ((ctx (vulpea-db--parse-file path)))
          (should (vulpea-parse-ctx-p ctx))
          (should (equal (vulpea-parse-ctx-path ctx) path))
          (should (vulpea-parse-ctx-file-node ctx))
          (should (equal (plist-get (vulpea-parse-ctx-file-node ctx) :id) "enc-id")))
      (delete-file path))))

(ert-deftest vulpea-db--parse-with-temp-buffer-clears-state-on-error ()
  "A parse error must not leave the reused buffer associated with the file.
Otherwise the shared parse buffer keeps a stale `buffer-file-name' that
could be picked up by file-change tracking on a later save."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: parse-err\n:END:\n#+title: T\n")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'vulpea-db--extract-file-node)
                     (lambda (&rest _) (error "boom"))))
            (should-error (vulpea-db--parse-with-temp-buffer path t)))
          (when (buffer-live-p vulpea-db--parse-buffer)
            (should (null (buffer-local-value 'buffer-file-name
                                              vulpea-db--parse-buffer)))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-parse-buffer-init-no-let-bound-warning ()
  "Initializing the parse buffer must not warn about `delay-mode-hooks'.
Let-binding `delay-mode-hooks' around `org-mode' triggers a \"Making
delay-mode-hooks buffer-local while locally let-bound!\" warning.
Regression test for https://github.com/d12frosted/vulpea/issues/301."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: issue-301\n:END:\n#+title: T\n")))
    (unwind-protect
        (progn
          ;; Force a fresh parse buffer so init runs during this test.
          (when (buffer-live-p vulpea-db--parse-buffer)
            (kill-buffer vulpea-db--parse-buffer))
          (setq vulpea-db--parse-buffer nil)
          ;; The C-level warning is written to *Messages*, so watch how
          ;; many matching lines exist before and after init.
          (let ((count (lambda ()
                         (with-current-buffer (get-buffer-create "*Messages*")
                           (count-matches "locally let-bound"
                                          (point-min) (point-max))))))
            (let ((before (funcall count)))
              ;; `single-temp-buffer' takes the delay-mode-hooks branch.
              (let ((vulpea-db-parse-method 'single-temp-buffer))
                (vulpea-db--parse-file path))
              (should (= before (funcall count))))))
      (delete-file path))))

(provide 'vulpea-db-extract-test)
;;; vulpea-db-extract-test.el ends here
