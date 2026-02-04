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

(ert-deftest vulpea-db-extract-file-node-auto-title ()
  "Test file node uses filename as title if missing."
  (let ((path (vulpea-test--create-temp-org-file ":PROPERTIES:\n:ID: test-id\n:END:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (title (plist-get node :title)))
          (should (equal title (file-name-base path))))
      (delete-file path))))

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

(provide 'vulpea-db-extract-test)
;;; vulpea-db-extract-test.el ends here
