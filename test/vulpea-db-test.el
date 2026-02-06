;;; vulpea-db-test.el --- Tests for vulpea-db -*- lexical-binding: t; -*-
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
;; Tests for vulpea-db.el (v2)
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-test-helpers)

;;; Schema Tests

(ert-deftest vulpea-db-init ()
  "Test database initialization."
  (vulpea-test--with-temp-db
    (let ((db (vulpea-db)))
      (should (emacsql-live-p db))
      ;; Check foreign keys enabled
      (should (equal (caar (emacsql db [:pragma foreign-keys]))
                     1)))))

(ert-deftest vulpea-db-schema-creation ()
  "Test all tables and indices are created."
  (vulpea-test--with-temp-db
    (vulpea-db)  ; Initialize

    ;; Check tables exist
    (let ((tables (mapcar (lambda (row) (symbol-name (car row)))
                          (emacsql (vulpea-db)
                                   [:select name :from sqlite-master
                                    :where (= type 'table)]))))
      (should (member "notes" tables))
      (should (member "tags" tables))
      (should (member "links" tables))
      (should (member "meta" tables))
      (should (member "files" tables))
      (should (member "schema_registry" tables)))))

(ert-deftest vulpea-db-schema-version ()
  "Test schema version is registered."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((version (caar (emacsql (vulpea-db)
                                  [:select [version] :from schema-registry
                                   :where (= name "core")]))))
      (should (equal version vulpea-db-version)))))

;;; CRUD Tests

(ert-deftest vulpea-db-insert-note-basic ()
  "Test basic note insertion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test Note"
     :properties '((foo . "bar"))
     :tags '("tag1" "tag2")
     :modified-at "2025-11-16 10:00:00")

    ;; Verify in notes table
    (let ((row (car (emacsql (vulpea-db)
                             [:select [id title] :from notes
                              :where (= id $s1)]
                             "test-id"))))
      (should (equal (elt row 0) "test-id"))
      (should (equal (elt row 1) "Test Note")))))

(ert-deftest vulpea-db-insert-note-normalized-tables ()
  "Test note insertion populates normalized tables."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test"
     :properties nil
     :tags '("wine" "red")
     :links '((:dest "other-id" :type "id" :pos 100))
     :modified-at "2025-11-16")

    ;; Check tags table
    (let ((tags (emacsql (vulpea-db)
                         [:select [tag] :from tags
                          :where (= note-id $s1)
                          :order :by tag]
                         "test-id")))
      (should (equal tags '(("red") ("wine")))))

    ;; Check links table
    (let ((links (emacsql (vulpea-db)
                          [:select [dest type] :from links
                           :where (= source $s1)]
                          "test-id")))
      (should (equal links '(("other-id" "id")))))))

(ert-deftest vulpea-db-insert-note-with-meta ()
  "Test note insertion with metadata."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test"
     :properties nil
     :meta '(("country" . ("France"))
             ("price" . ("25.50"))
             ("region" . ("region-id")))
     :modified-at "2025-11-16")

    ;; Check meta table - no type column anymore
    (let ((meta (emacsql (vulpea-db)
                         [:select [key value] :from meta
                          :where (= note-id $s1)
                          :order :by key]
                         "test-id")))
      (should (= (length meta) 3))
      (should (member (list "country" "France") meta))
      (should (member (list "price" "25.50") meta))
      (should (member (list "region" "region-id") meta)))))

(ert-deftest vulpea-db-delete-note ()
  "Test note deletion."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test"
     :properties nil
     :tags '("tag1")
     :modified-at "2025-11-16")

    ;; Verify exists
    (should (emacsql (vulpea-db)
                     [:select * :from notes :where (= id $s1)]
                     "test-id"))

    ;; Delete
    (vulpea-db--delete-note "test-id")

    ;; Verify deleted
    (should-not (emacsql (vulpea-db)
                         [:select * :from notes :where (= id $s1)]
                         "test-id"))))

(ert-deftest vulpea-db-delete-cascades ()
  "Test deleting note cascades to normalized tables."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test"
     :properties nil
     :tags '("tag1")
     :links '((:dest "other" :type "id" :pos 100))
     :meta '(("key" . ((:value "val" :type "string"))))
     :modified-at "2025-11-16")

    ;; Verify normalized data exists
    (should (emacsql (vulpea-db)
                     [:select * :from tags :where (= note-id $s1)]
                     "test"))
    (should (emacsql (vulpea-db)
                     [:select * :from links :where (= source $s1)]
                     "test"))
    (should (emacsql (vulpea-db)
                     [:select * :from meta :where (= note-id $s1)]
                     "test"))

    ;; Delete note
    (vulpea-db--delete-file-notes "/tmp/test.org")

    ;; Verify cascade
    (should-not (emacsql (vulpea-db)
                         [:select * :from tags :where (= note-id $s1)]
                         "test"))
    (should-not (emacsql (vulpea-db)
                         [:select * :from links :where (= source $s1)]
                         "test"))
    (should-not (emacsql (vulpea-db)
                         [:select * :from meta :where (= note-id $s1)]
                         "test"))))

(ert-deftest vulpea-db-delete-file-notes ()
  "Test deleting all notes from a file."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Insert file-level and heading-level notes
    (vulpea-db--insert-note
     :id "file-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "File"
     :properties nil
     :modified-at "2025-11-16")
    (vulpea-db--insert-note
     :id "heading-id"
     :path "/tmp/test.org"
     :level 1
     :pos 100
     :title "Heading"
     :properties nil
     :modified-at "2025-11-16")

    ;; Verify both exist
    (should (= (length (emacsql (vulpea-db)
                                [:select * :from notes
                                 :where (= path "/tmp/test.org")]))
               2))

    ;; Delete all notes from file
    (vulpea-db--delete-file-notes "/tmp/test.org")

    ;; Verify both deleted
    (should (= (length (emacsql (vulpea-db)
                                [:select * :from notes
                                 :where (= path "/tmp/test.org")]))
               0))))

;;; File Tracking Tests

(ert-deftest vulpea-db-file-tracking ()
  "Test file hash tracking."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; No hash initially
    (should-not (vulpea-db--get-file-hash "/tmp/test.org"))

    ;; Update hash
    (vulpea-db--update-file-hash "/tmp/test.org" "abc123" 1234567890 1024)

    ;; Verify stored
    (let ((info (vulpea-db--get-file-hash "/tmp/test.org")))
      (should (equal (plist-get info :hash) "abc123"))
      (should (equal (plist-get info :mtime) 1234567890))
      (should (equal (plist-get info :size) 1024)))

    ;; Update again (should replace)
    (vulpea-db--update-file-hash "/tmp/test.org" "def456" 1234567891 2048)

    (let ((info (vulpea-db--get-file-hash "/tmp/test.org")))
      (should (equal (plist-get info :hash) "def456"))
      (should (equal (plist-get info :mtime) 1234567891))
      (should (equal (plist-get info :size) 2048)))))

;;; Transaction Tests

(ert-deftest vulpea-db-transaction-rollback ()
  "Test transaction rollback on error."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; This should fail due to forced error
    (should-error
     (emacsql-with-transaction (vulpea-db)
       (vulpea-db--insert-note
        :id "test-id"
        :path "/tmp/test.org"
        :level 0
        :pos 0
        :title "Test"
        :properties nil
        :modified-at "2025-11-16")
       ;; Force an error
       (error "Rollback test")))

    ;; Verify nothing was inserted
    (should-not (emacsql (vulpea-db)
                         [:select * :from notes :where (= id $s1)]
                         "test-id"))))

;;; Schema Upgrade Tests

(ert-deftest vulpea-db-schema-upgrade-rebuilds ()
  "Test that version mismatch triggers DB rebuild."
  (vulpea-test--with-temp-db
    ;; Initialize DB and insert a note
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test Note"
     :properties nil
     :modified-at "2025-11-16 10:00:00")

    ;; Verify note exists
    (should (emacsql (vulpea-db)
                     [:select * :from notes :where (= id $s1)]
                     "test-id"))

    ;; Simulate old DB: downgrade stored version
    (emacsql (vulpea-db)
             [:update schema-registry :set (= version 0)
              :where (= name "core")])

    ;; Close connection to simulate restart
    (vulpea-db-close)
    (setq vulpea-db--connection nil)

    ;; Re-initialize — should detect mismatch and rebuild
    (vulpea-db)

    ;; Old note should be gone (DB was deleted and recreated)
    (should-not (emacsql (vulpea-db)
                         [:select * :from notes :where (= id $s1)]
                         "test-id"))

    ;; Version should be current
    (let ((version (caar (emacsql (vulpea-db)
                                  [:select [version] :from schema-registry
                                   :where (= name "core")]))))
      (should (equal version vulpea-db-version)))

    ;; Schema rebuilt flag should be set
    (should vulpea-db--schema-rebuilt)

    ;; Tables should still exist (freshly created)
    (should (vulpea-db--table-exists-p 'notes))
    (should (vulpea-db--table-exists-p 'tags))
    (should (vulpea-db--table-exists-p 'schema_registry))))

(ert-deftest vulpea-db-same-version-no-rebuild ()
  "Test that same version does not trigger rebuild."
  (vulpea-test--with-temp-db
    ;; Initialize DB and insert a note
    (vulpea-db)
    (vulpea-db--insert-note
     :id "test-id"
     :path "/tmp/test.org"
     :level 0
     :pos 0
     :title "Test Note"
     :properties nil
     :modified-at "2025-11-16 10:00:00")

    ;; Close connection to simulate restart
    (vulpea-db-close)
    (setq vulpea-db--connection nil)
    (setq vulpea-db--schema-rebuilt nil)

    ;; Re-initialize — same version, no rebuild
    (vulpea-db)

    ;; Note should still be there
    (should (emacsql (vulpea-db)
                     [:select * :from notes :where (= id $s1)]
                     "test-id"))

    ;; Flag should not be set
    (should-not vulpea-db--schema-rebuilt)))

;;; Tag Inheritance Tests

(ert-deftest vulpea-db-heading-inherits-filetags ()
  "Test that heading-level notes inherit filetags from their file."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: Parent File\n#+filetags: :ftag1:ftag2:\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n\nContent.\n"
    (let ((note (vulpea-db-get-by-id "heading-id")))
      (should note)
      (should (member "ftag1" (vulpea-note-tags note)))
      (should (member "ftag2" (vulpea-note-tags note))))))

(ert-deftest vulpea-db-heading-inherits-parent-heading-tags ()
  "Test that nested heading inherits tags from parent headings."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: Parent File\n\n* Parent :ptag:\n:PROPERTIES:\n:ID: parent-id\n:END:\n\n** Child\n:PROPERTIES:\n:ID: child-id\n:END:\n\nContent.\n"
    (let ((note (vulpea-db-get-by-id "child-id")))
      (should note)
      (should (member "ptag" (vulpea-note-tags note))))))

(ert-deftest vulpea-db-heading-inherits-all-tags ()
  "Test heading gets filetags + parent heading tags + own tags."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: File\n#+filetags: :ftag:\n\n* Parent :ptag:\n:PROPERTIES:\n:ID: parent-id\n:END:\n\n** Child :ctag:\n:PROPERTIES:\n:ID: child-id\n:END:\n\nContent.\n"
    (let ((note (vulpea-db-get-by-id "child-id")))
      (should note)
      (should (member "ftag" (vulpea-note-tags note)))
      (should (member "ptag" (vulpea-note-tags note)))
      (should (member "ctag" (vulpea-note-tags note))))))

(ert-deftest vulpea-db-heading-no-tag-inheritance-when-disabled ()
  "Test that heading notes respect org-use-tag-inheritance = nil."
  (let ((org-use-tag-inheritance nil))
    (vulpea-test--with-temp-db-and-file "file-id"
      "#+title: Parent File\n#+filetags: :ftag1:ftag2:\n\n* Heading :htag:\n:PROPERTIES:\n:ID: heading-id\n:END:\n\nContent.\n"
      (let ((note (vulpea-db-get-by-id "heading-id")))
        (should note)
        (should (equal (vulpea-note-tags note) '("htag")))))))

(ert-deftest vulpea-db-heading-deduplicates-tags ()
  "Test that inherited tags don't create duplicates."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: File\n#+filetags: :shared:\n\n* Heading :shared:own:\n:PROPERTIES:\n:ID: heading-id\n:END:\n\nContent.\n"
    (let* ((note (vulpea-db-get-by-id "heading-id"))
           (tags (vulpea-note-tags note)))
      (should note)
      (should (member "shared" tags))
      (should (member "own" tags))
      ;; "shared" should appear only once
      (should (= (length (seq-filter (lambda (t) (string= t "shared")) tags)) 1)))))

(ert-deftest vulpea-db-heading-selective-tag-inheritance ()
  "Test that heading notes respect selective tag inheritance."
  (let ((org-use-tag-inheritance '("inherit_me")))
    (vulpea-test--with-temp-db-and-file "file-id"
      "#+title: File\n#+filetags: :inherit_me:skip_me:\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n\nContent.\n"
      (let ((note (vulpea-db-get-by-id "heading-id")))
        (should note)
        (should (member "inherit_me" (vulpea-note-tags note)))
        (should-not (member "skip_me" (vulpea-note-tags note)))))))

(ert-deftest vulpea-db-heading-tag-order-matches-org-mode ()
  "Test that inherited tag order matches org-mode: filetags, then
parent headings top-down, then own tags."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: File\n#+filetags: :f1:f2:\n\n* GP :g1:g2:\n:PROPERTIES:\n:ID: gp-id\n:END:\n\n** Parent :p1:p2:\n:PROPERTIES:\n:ID: parent-id\n:END:\n\n*** Child :c1:c2:\n:PROPERTIES:\n:ID: child-id\n:END:\n\nContent.\n"
    (let ((note (vulpea-db-get-by-id "child-id")))
      (should note)
      (should (equal (vulpea-note-tags note)
                     '("f1" "f2" "g1" "g2" "p1" "p2" "c1" "c2"))))))

(ert-deftest vulpea-db-file-note-tags-unchanged ()
  "Test that file-level note tags are not affected by inheritance change."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: File\n#+filetags: :ftag1:ftag2:\n\n* Heading :htag:\n:PROPERTIES:\n:ID: heading-id\n:END:\n"
    (let ((note (vulpea-db-get-by-id "file-id")))
      (should note)
      (should (equal (vulpea-note-tags note) '("ftag1" "ftag2"))))))

(provide 'vulpea-db-test)
;;; vulpea-db-test.el ends here
