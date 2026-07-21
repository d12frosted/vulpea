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

(ert-deftest vulpea-db-init-creates-parent-directory ()
  "Test that initialization creates a missing parent directory.

See https://github.com/d12frosted/vulpea/issues/271."
  (let* ((temp-dir (make-temp-file "vulpea-test-" t))
         (nested-dir (expand-file-name "nested/data" temp-dir))
         (vulpea-db-location (expand-file-name "vulpea.db" nested-dir))
         (vulpea-db--connection nil))
    (unwind-protect
        (progn
          ;; Parent directory does not exist yet.
          (should-not (file-exists-p nested-dir))
          ;; Initialization should create it and succeed.
          (let ((db (vulpea-db)))
            (should (emacsql-live-p db))
            (should (file-directory-p nested-dir))
            (should (file-exists-p vulpea-db-location))))
      (when vulpea-db--connection
        (vulpea-db-close))
      (delete-directory temp-dir t))))

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

(ert-deftest vulpea-db-insert-note-category-round-trip ()
  "Category is stored in the notes table and decoded back.
https://github.com/d12frosted/vulpea/issues/389"
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "cat-id"
     :path "/tmp/cat.org"
     :level 0
     :pos 0
     :title "Categorized"
     :category "journal"
     :modified-at "2025-11-16 10:00:00")

    ;; Raw column
    (should (equal "journal"
                   (caar (emacsql (vulpea-db)
                                  [:select [category] :from notes
                                   :where (= id $s1)]
                                  "cat-id"))))
    ;; Decoded struct
    (should (equal "journal"
                   (vulpea-note-category (vulpea-db-get-by-id "cat-id"))))))

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

(ert-deftest vulpea-db-insert-note-tricky-values-round-trip ()
  "Values with quotes, unicode, and newlines survive insert and read.
Characterization test for the insert path: whatever writes the rows,
reading them back through emacsql must return the original values."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((title "He said \"hi\" — привіт, it's\na multi-line\ttitle")
          (tag "böse-tag")
          (meta-value "value with 'quotes' and \"doubles\"\nand a newline")
          (desc "link description with ’unicode’ and \\backslash"))
      (vulpea-db--insert-note
       :id "tricky-id"
       :path "/tmp/tricky's \"file\".org"
       :level 3
       :pos 1234
       :title title
       :properties `(("CUSTOM_PROP" . ,meta-value))
       :tags (list tag "plain")
       :aliases (list "alias one" title)
       :meta `(("note" . (,meta-value)))
       :links `((:dest "other-id" :type "id" :pos 100 :description ,desc))
       :todo "TODO"
       :priority 65
       :scheduled "2026-01-01"
       :modified-at "2025-11-16 10:00:00")

      (let ((row (car (emacsql (vulpea-db)
                               [:select [title path level pos todo priority
                                         scheduled deadline]
                                :from notes :where (= id $s1)]
                               "tricky-id"))))
        (should (equal (elt row 0) title))
        (should (equal (elt row 1) "/tmp/tricky's \"file\".org"))
        (should (equal (elt row 2) 3))
        (should (equal (elt row 3) 1234))
        (should (equal (elt row 4) "TODO"))
        (should (equal (elt row 5) 65))
        (should (equal (elt row 6) "2026-01-01"))
        ;; nil scalar must come back as nil (stored as NULL)
        (should (null (elt row 7))))
      (should (equal (emacsql (vulpea-db)
                              [:select [tag] :from tags
                               :where (= note-id $s1) :order :by tag]
                              "tricky-id")
                     (list (list tag) (list "plain"))))
      (should (equal (caar (emacsql (vulpea-db)
                                    [:select [value] :from meta
                                     :where (= note-id $s1)]
                                    "tricky-id"))
                     meta-value))
      (should (equal (caar (emacsql (vulpea-db)
                                    [:select [description] :from links
                                     :where (= source $s1)]
                                    "tricky-id"))
                     desc))
      (should (equal (caar (emacsql (vulpea-db)
                                    [:select [value] :from properties
                                     :where (= note-id $s1)]
                                    "tricky-id"))
                     meta-value)))))

(ert-deftest vulpea-db-insert-note-storage-encoding ()
  "Stored column text matches emacsql's readable-print encoding.
Locks the on-disk format: rows written by `vulpea-db--insert-note'
must be byte-identical to rows emacsql would write, so databases
written before and after any insert-path change are interchangeable."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((title "line one\nline two with \"quotes\" and ’unicode’"))
      (vulpea-db--insert-note
       :id "enc-id"
       :path "/tmp/enc.org"
       :level 0
       :pos 0
       :title title
       :properties nil
       :modified-at "2025-11-16 10:00:00")
      (let* ((handle (oref (vulpea-db) handle))
             (row (car (sqlite-select
                        handle
                        "SELECT title, level, todo FROM notes WHERE id = ?"
                        (list (let ((print-escape-newlines t)
                                    (print-escape-control-characters t))
                                (prin1-to-string "enc-id")))))))
        (should row)
        ;; Text scalars are stored as their readable-print form
        (should (equal (elt row 0)
                       (let ((print-escape-newlines t)
                             (print-escape-control-characters t))
                         (prin1-to-string title))))
        ;; Numbers are stored as SQL numbers, not printed strings
        (should (equal (elt row 1) 0))
        ;; nil is stored as NULL
        (should (null (elt row 2)))))))

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

(ert-deftest vulpea-db-table-exists-p-handles-special-chars ()
  "Existence checks must treat the name as data, not raw SQL.
A name with a quote would break a format-built query; a
parameterized query returns nil safely."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (should (vulpea-db--table-exists-p 'notes))
    (should-not (vulpea-db--table-exists-p (intern "no_such_table")))
    (should-not (vulpea-db--table-exists-p (intern "weird'name")))
    (should-not (vulpea-db--index-exists-p (intern "weird'index")))))

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

;;; #+filetags inside blocks Tests

(ert-deftest vulpea-db-file-note-ignores-filetags-in-blocks ()
  "Filetags quoted inside src/example blocks must not become real tags.
A note that merely quotes org markup (e.g. a blog post about PARA)
keeps only its genuine #+filetags."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: PARA, not GTD\n#+filetags: :realtag:\n\n#+begin_src org\n#+filetags: :agenda:area:\n#+end_src\n\n#+begin_example\n#+filetags: :project:\n#+end_example\n"
    (let ((note (vulpea-db-get-by-id "file-id")))
      (should note)
      (should (equal (vulpea-note-tags note) '("realtag"))))))

(ert-deftest vulpea-db-file-note-only-block-filetags-yields-none ()
  "When the only #+filetags lives inside a block, the note has no tags."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: PARA, not GTD\n\n#+begin_src org\n#+filetags: :agenda:area:\n#+end_src\n"
    (let ((note (vulpea-db-get-by-id "file-id")))
      (should note)
      (should (equal (vulpea-note-tags note) nil)))))

;;; Multiple #+filetags Tests

(ert-deftest vulpea-db-multiple-filetags-file-level ()
  "Test that multiple #+filetags lines are accumulated for file notes."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: Test\n#+filetags: :tag1:tag2:\n#+filetags: :tag3:tag4:\n"
    (let ((note (vulpea-db-get-by-id "file-id")))
      (should note)
      (should (equal (vulpea-note-tags note)
                     '("tag1" "tag2" "tag3" "tag4"))))))

(ert-deftest vulpea-db-multiple-filetags-inherited-by-heading ()
  "Test that heading notes inherit tags from all #+filetags lines."
  (vulpea-test--with-temp-db-and-file "file-id"
    "#+title: Test\n#+filetags: :tag1:tag2:\n#+filetags: :tag3:\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n"
    (let ((note (vulpea-db-get-by-id "heading-id")))
      (should note)
      (should (member "tag1" (vulpea-note-tags note)))
      (should (member "tag2" (vulpea-note-tags note)))
      (should (member "tag3" (vulpea-note-tags note))))))

;;; Extraction Settings Invalidation Tests

(ert-deftest vulpea-db-settings-fingerprint ()
  "Test that fingerprint captures extraction-relevant settings."
  ;; Default settings should produce a stable fingerprint
  (let ((org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (let ((fp1 (vulpea-db--settings-fingerprint))
          (fp2 (vulpea-db--settings-fingerprint)))
      (should (equal fp1 fp2))))

  ;; Different settings should produce different fingerprints
  (let ((fp-default (let ((org-use-tag-inheritance t)
                          (org-tags-exclude-from-inheritance nil))
                      (vulpea-db--settings-fingerprint)))
        (fp-disabled (let ((org-use-tag-inheritance nil)
                           (org-tags-exclude-from-inheritance nil))
                       (vulpea-db--settings-fingerprint)))
        (fp-selective (let ((org-use-tag-inheritance '("foo")))
                        (vulpea-db--settings-fingerprint)))
        (fp-excluded (let ((org-use-tag-inheritance t)
                           (org-tags-exclude-from-inheritance '("bar")))
                       (vulpea-db--settings-fingerprint))))
    (should-not (equal fp-default fp-disabled))
    (should-not (equal fp-default fp-selective))
    (should-not (equal fp-default fp-excluded)))

  ;; The parse method is part of the fingerprint: extraction output
  ;; depends on it (org-category via dir-locals under find-file), so
  ;; switching methods must trigger a re-index.
  ;; https://github.com/d12frosted/vulpea/issues/389
  (let ((fp-temp (let ((vulpea-db-parse-method 'temp-buffer))
                   (vulpea-db--settings-fingerprint)))
        (fp-find-file (let ((vulpea-db-parse-method 'find-file))
                        (vulpea-db--settings-fingerprint))))
    (should-not (equal fp-temp fp-find-file))))

(ert-deftest vulpea-db-settings-stored-on-init ()
  "Test that settings fingerprint is stored in schema-registry on init."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((stored (caar (emacsql (vulpea-db)
                                 [:select [version] :from schema-registry
                                  :where (= name "settings")]))))
      (should stored)
      (should (equal stored (vulpea-db--settings-fingerprint))))))

(ert-deftest vulpea-db-settings-change-triggers-reindex ()
  "Test that changing tag inheritance settings triggers re-index flag."
  (let ((org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (vulpea-test--with-temp-db
      ;; Initialize DB with current settings
      (vulpea-db)
      (vulpea-db--insert-note
       :id "test-id"
       :path "/tmp/test.org"
       :level 0
       :pos 0
       :title "Test Note"
       :properties nil
       :modified-at "2025-11-16 10:00:00")

      ;; Close connection
      (vulpea-db-close)
      (setq vulpea-db--connection nil)
      (setq vulpea-db--settings-changed nil)

      ;; Change settings and re-initialize
      (let ((org-use-tag-inheritance nil))
        (vulpea-db)

        ;; Flag should be set
        (should vulpea-db--settings-changed)

        ;; Data should still be there (no full rebuild)
        (should (emacsql (vulpea-db)
                         [:select * :from notes :where (= id $s1)]
                         "test-id"))

        ;; files table should be cleared (to force re-extraction)
        (should-not (emacsql (vulpea-db)
                             [:select * :from files]))

        ;; Stored fingerprint should be updated to new settings
        (let ((stored (caar (emacsql (vulpea-db)
                                     [:select [version] :from schema-registry
                                      :where (= name "settings")]))))
          (should (equal stored (vulpea-db--settings-fingerprint))))))))

(ert-deftest vulpea-db-settings-parse-method-change-triggers-reindex ()
  "Test that changing the parse method triggers the re-index flag."
  (let ((vulpea-db-parse-method 'temp-buffer))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (emacsql (vulpea-db)
               [:insert :into files :values $v1]
               (list (vector "/tmp/test.org" "abc123" "2025-01-01" 100)))
      (vulpea-db-close)
      (setq vulpea-db--connection nil)
      (setq vulpea-db--settings-changed nil)

      (let ((vulpea-db-parse-method 'find-file))
        (vulpea-db)
        (should vulpea-db--settings-changed)
        ;; files table cleared to force re-extraction
        (should-not (emacsql (vulpea-db)
                             [:select * :from files]))))))

(ert-deftest vulpea-db-settings-unchanged-no-reindex ()
  "Test that same settings don't trigger re-index."
  (let ((org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (vulpea-test--with-temp-db
      ;; Initialize DB
      (vulpea-db)

      ;; Add a file record
      (emacsql (vulpea-db)
               [:insert :into files :values $v1]
               (list (vector "/tmp/test.org" "abc123" "2025-01-01" 100)))

      ;; Close and re-init with same settings
      (vulpea-db-close)
      (setq vulpea-db--connection nil)
      (setq vulpea-db--settings-changed nil)

      (vulpea-db)

      ;; Flag should not be set
      (should-not vulpea-db--settings-changed)

      ;; files table should still have data
      (should (emacsql (vulpea-db)
                       [:select * :from files])))))

;;; Parser Epoch Tests
;; https://github.com/d12frosted/vulpea/issues/277

(ert-deftest vulpea-db-parser-epoch-stored-on-init ()
  "Parser epoch is stored in schema-registry on init."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((stored (caar (emacsql (vulpea-db)
                                 [:select [version] :from schema-registry
                                  :where (= name "parser-epoch")]))))
      (should stored)
      (should (equal stored vulpea-db-parser-epoch)))))

(ert-deftest vulpea-db-parser-epoch-change-triggers-reindex ()
  "Changing the parser epoch clears the files cache and flags a
re-index, while preserving the notes table."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-db--insert-note
     :id "epoch-id" :path "/tmp/epoch.org" :level 0 :pos 0
     :title "Epoch Note" :properties nil
     :modified-at "2025-11-16 10:00:00")
    (emacsql (vulpea-db)
             [:insert :into files :values $v1]
             (list (vector "/tmp/epoch.org" "hash" "2025-01-01" 100)))
    ;; Simulate a DB written by an older parser epoch
    (vulpea-db--register-schema (vulpea-db) 'parser-epoch
                                (1- vulpea-db-parser-epoch))
    (vulpea-db-close)
    (setq vulpea-db--connection nil
          vulpea-db--parser-changed nil)

    (vulpea-db)

    ;; Flag should be set
    (should vulpea-db--parser-changed)
    ;; Notes preserved (no full rebuild)
    (should (emacsql (vulpea-db)
                     [:select * :from notes :where (= id $s1)] "epoch-id"))
    ;; files cache cleared to force re-extraction
    (should-not (emacsql (vulpea-db) [:select * :from files]))
    ;; Stored epoch updated to current
    (should (equal (caar (emacsql (vulpea-db)
                                  [:select [version] :from schema-registry
                                   :where (= name "parser-epoch")]))
                   vulpea-db-parser-epoch))))

(ert-deftest vulpea-db-parser-epoch-unchanged-no-reindex ()
  "Same parser epoch leaves the files cache intact."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (emacsql (vulpea-db)
             [:insert :into files :values $v1]
             (list (vector "/tmp/epoch.org" "hash" "2025-01-01" 100)))
    (vulpea-db-close)
    (setq vulpea-db--connection nil
          vulpea-db--parser-changed nil)

    (vulpea-db)

    (should-not vulpea-db--parser-changed)
    (should (emacsql (vulpea-db) [:select * :from files]))))

(ert-deftest vulpea-db-parser-epoch-missing-with-cached-files-triggers ()
  "An existing DB with cached files but no recorded parser epoch is
treated as stale, so upgrading to an epoch-aware vulpea re-extracts
all files once.  Regression test for
https://github.com/d12frosted/vulpea/issues/277."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (emacsql (vulpea-db)
             [:insert :into files :values $v1]
             (list (vector "/tmp/epoch.org" "hash" "2025-01-01" 100)))
    ;; Simulate a pre-epoch database: no parser-epoch row
    (emacsql (vulpea-db)
             [:delete :from schema-registry :where (= name "parser-epoch")])
    (vulpea-db-close)
    (setq vulpea-db--connection nil
          vulpea-db--parser-changed nil)

    (vulpea-db)

    (should vulpea-db--parser-changed)
    (should-not (emacsql (vulpea-db) [:select * :from files]))))

(ert-deftest vulpea-db-parser-epoch-missing-brand-new-db-no-trigger ()
  "A brand-new DB (empty files cache) does not trigger a parser
re-index even before an epoch is recorded."
  (vulpea-test--with-temp-db
    (vulpea-db)
    ;; Drop the freshly-registered epoch but keep the files cache empty
    (emacsql (vulpea-db)
             [:delete :from schema-registry :where (= name "parser-epoch")])
    (vulpea-db-close)
    (setq vulpea-db--connection nil
          vulpea-db--parser-changed nil)

    (vulpea-db)

    (should-not vulpea-db--parser-changed)))

(provide 'vulpea-db-test)
;;; vulpea-db-test.el ends here
