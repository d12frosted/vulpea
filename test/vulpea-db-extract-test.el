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
               "#+TITLE: Test Note\n#+ID: test-id\n\n* Heading\n")))
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
               "#+TITLE: Wine Database\n#+ID: wine-db-id\n#+FILETAGS: :wine:database:\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx)))
          (should (equal (plist-get node :id) "wine-db-id"))
          (should (equal (plist-get node :title) "Wine Database"))
          (should (member "wine" (plist-get node :tags)))
          (should (member "database" (plist-get node :tags))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-auto-id ()
  "Test file node gets auto-generated ID if missing."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: No ID Note\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (id (plist-get node :id)))
          (should id)
          (should (stringp id))
          (should (> (length id) 0)))
      (delete-file path))))

(ert-deftest vulpea-db-extract-file-node-auto-title ()
  "Test file node uses filename as title if missing."
  (let ((path (vulpea-test--create-temp-org-file "#+ID: test-id\n")))
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

;;; Extractor Tests

(ert-deftest vulpea-db-extract-aliases ()
  "Test alias extraction."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: Main Title\n#+ROAM_ALIASES: Alias1 Alias2\n")))
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
               "#+TITLE: Test\n\nSome [[id:note-1][link]] and [[id:note-2][another]].\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (links (plist-get node :links)))
          (should (= (length links) 2))
          (should (member "note-1" (mapcar (lambda (l) (plist-get l :dest)) links)))
          (should (member "note-2" (mapcar (lambda (l) (plist-get l :dest)) links))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-meta-with-types ()
  "Test metadata extraction with type coercion."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: Wine\n#+ID: wine-id\n\n- country_string :: France\n- price_number :: 25.50\n- region_note :: [[id:region-id][Region Name]]\n- url_link :: https://example.com\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (meta (plist-get node :meta)))
          ;; Check country
          (let ((country-vals (cdr (assoc "country" meta))))
            (should (= (length country-vals) 1))
            (should (equal (plist-get (car country-vals) :value) "France"))
            (should (equal (plist-get (car country-vals) :type) "string")))

          ;; Check price
          (let ((price-vals (cdr (assoc "price" meta))))
            (should (= (length price-vals) 1))
            (should (equal (plist-get (car price-vals) :value) "25.50"))
            (should (equal (plist-get (car price-vals) :type) "number")))

          ;; Check region
          (let ((region-vals (cdr (assoc "region" meta))))
            (should (= (length region-vals) 1))
            (should (equal (plist-get (car region-vals) :value) "region-id"))
            (should (equal (plist-get (car region-vals) :type) "note")))

          ;; Check URL
          (let ((url-vals (cdr (assoc "url" meta))))
            (should (= (length url-vals) 1))
            (should (equal (plist-get (car url-vals) :value) "https://example.com"))
            (should (equal (plist-get (car url-vals) :type) "link"))))
      (delete-file path))))

(ert-deftest vulpea-db-extract-meta-multiple-values ()
  "Test metadata with multiple values for same key."
  (let ((path (vulpea-test--create-temp-org-file
               "#+TITLE: Wine\n#+ID: wine-multi-id\n\n- grape_note :: [[id:grape-1][Grape One]]\n- grape_note :: [[id:grape-2][Grape Two]]\n")))
    (unwind-protect
        (let* ((ctx (vulpea-db--parse-file path))
               (node (vulpea-parse-ctx-file-node ctx))
               (meta (plist-get node :meta))
               (grape-vals (cdr (assoc "grape" meta))))
          (should (= (length grape-vals) 2))
          (should (member "grape-1" (mapcar (lambda (v) (plist-get v :value)) grape-vals)))
          (should (member "grape-2" (mapcar (lambda (v) (plist-get v :value)) grape-vals))))
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
         (citations nil))
    ;; Extract citations from the AST
    ;; Simple regex-based extraction for demonstration
    (org-element-map ast 'paragraph
      (lambda (para)
        (let ((content (org-element-interpret-data para)))
          (when (string-match "\\[@\\([^]]+\\)\\]" content)
            (push (match-string 1 content) citations)))))

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
                   "#+TITLE: Research Paper\n#+ID: paper-id\n\nThis work builds on [@smith2020] and [@jones2019].\n")))
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
                 "#+TITLE: Test Note\n#+ID: test-id\n#+FILETAGS: :wine:\n")))
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
                 "#+TITLE: File\n#+ID: file-id\n\n* Heading\n:PROPERTIES:\n:ID: heading-id\n:END:\n")))
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
                 "#+TITLE: Version 1\n#+ID: test-id\n")))
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
              (insert "#+TITLE: Version 2\n#+ID: test-id\n"))

            ;; Second update
            (vulpea-db-update-file path)
            (let ((row (car (emacsql (vulpea-db)
                                     [:select [title] :from notes
                                      :where (= id $s1)]
                                     "test-id"))))
              (should (equal (elt row 0) "Version 2"))))
        (delete-file path)))))

(provide 'vulpea-db-extract-test)
;;; vulpea-db-extract-test.el ends here
