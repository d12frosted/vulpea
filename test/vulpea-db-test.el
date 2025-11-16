;;; vulpea-db-test.el --- Tests for vulpea-db -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2025 Boris Buliga <boris@d12frosted.io>
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

;;; Test Infrastructure

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database."
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
     :links '((:dest "other-id" :type "id"))
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
     :meta '(("country" . ((:value "France" :type "string")))
             ("price" . ((:value "25.50" :type "number")))
             ("region" . ((:value "region-id" :type "note"))))
     :modified-at "2025-11-16")

    ;; Check meta table
    (let ((meta (emacsql (vulpea-db)
                         [:select [key value type] :from meta
                          :where (= note-id $s1)
                          :order :by key]
                         "test-id")))
      (should (= (length meta) 3))
      (should (member (list "country" "France" "string") meta))
      (should (member (list "price" "25.50" "number") meta))
      (should (member (list "region" "region-id" "note") meta)))))

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
     :links '((:dest "other" :type "id"))
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

(provide 'vulpea-db-test)
;;; vulpea-db-test.el ends here
