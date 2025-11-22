;;; vulpea-db-extract-test.el --- Tests for vulpea-db-extract -*- lexical-binding: t; -*-
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
;; Tests for vulpea-db-extract.el
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-extract)

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

(defun vulpea-test--create-temp-org-file (content)
  "Create temporary org file with CONTENT.

Returns absolute path. Caller responsible for cleanup."
  (let ((temp-file (make-temp-file "vulpea-test-" nil ".org")))
    (with-temp-file temp-file
      (insert content))
    temp-file))

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

;;; Extractor Tests

(ert-deftest vulpea-db-extract-aliases ()
  "Test alias extraction."
  (let ((path (vulpea-test--create-temp-org-file
               (format ":PROPERTIES:\n:ID: %s\n:ROAM_ALIASES: Alias1 Alias2\n:END:\n#+TITLE: Main Title\n" (org-id-new)))))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (aliases (plist-get node :aliases)))
          (should (member "Alias1" aliases))
          (should (member "Alias2" aliases)))
      (delete-file path))))

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
          (should (member "note-2" (mapcar (lambda (l) (plist-get l :dest)) links))))
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

(provide 'vulpea-db-extract-test)
;;; vulpea-db-extract-test.el ends here
