;;; plugin-guide-examples-test.el --- Executable examples from the plugin guide -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2026 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 21 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Every code example in docs/plugin-guide.org, run against the real
;; extraction machinery.  Example bodies are copied verbatim from the
;; guide; only the function names carry a test prefix.  When an
;; example in the guide changes, the matching test here must change
;; with it - that is the point.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vulpea-test-helpers)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)

;;; Parse Context accessors (guide: "Parse Context", "API Reference")

(ert-deftest vulpea-plugin-guide-parse-ctx-accessors ()
  "Context accessors named in the guide exist and return real data."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (seen nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'guide-recorder
        :requires-ast nil
        :extract-fn (lambda (ctx note-data)
                      (push ctx seen)
                      note-data)))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: ctx-id\n:END:\n#+TITLE: Ctx\n\nBody.\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (= (length seen) 1))
              (let ((ctx (car seen)))
                (should (equal (file-truename (vulpea-parse-ctx-path ctx))
                               (file-truename path)))
                (should (stringp (vulpea-parse-ctx-hash ctx)))
                (should (vulpea-parse-ctx-mtime ctx))
                (should (= (vulpea-parse-ctx-size ctx)
                           (file-attribute-size (file-attributes path))))
                (should (plist-get (vulpea-parse-ctx-file-node ctx) :id))
                (should (listp (vulpea-parse-ctx-heading-nodes ctx)))
                ;; AST is nil unless :requires-ast t is declared.
                (should-not (vulpea-parse-ctx-ast ctx))))
          (delete-file path))))))

;;; Conditional Extraction (guide: "Conditional Extraction")

(defun vulpea-plugin-guide-test--conditional-extractor (ctx note-data)
  "Verbatim body of my-conditional-extractor from the guide."
  ;; Only extract from files in specific directory
  (let ((path (vulpea-parse-ctx-path ctx)))
    (if (string-match-p "/projects/" path)
        (progn
          ;; Do extraction
          note-data)
      ;; Skip extraction
      note-data)))

(ert-deftest vulpea-plugin-guide-conditional-extraction ()
  (let ((note-data '(:id "x")))
    (should (eq note-data
                (vulpea-plugin-guide-test--conditional-extractor
                 (make-vulpea-parse-ctx :path "/tmp/projects/note.org")
                 note-data)))
    (should (eq note-data
                (vulpea-plugin-guide-test--conditional-extractor
                 (make-vulpea-parse-ctx :path "/tmp/other/note.org")
                 note-data)))))

;;; Inspecting Parse Context (guide: "Debugging")

(defun vulpea-plugin-guide-test--debug-extractor (ctx note-data)
  "Verbatim body of my-debug-extractor from the guide."
  ;; Print context information
  (message "Processing: %s" (vulpea-parse-ctx-path ctx))
  (message "File size: %s bytes" (vulpea-parse-ctx-size ctx))
  (message "Note ID: %s" (plist-get note-data :id))
  (message "Note title: %s" (plist-get note-data :title))

  ;; Inspect AST structure (nil unless registered with :requires-ast t)
  (let ((ast (vulpea-parse-ctx-ast ctx)))
    (message "AST type: %s" (org-element-type ast)))

  note-data)

(ert-deftest vulpea-plugin-guide-debug-extractor ()
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (ran nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'guide-debug
        :requires-ast nil
        :extract-fn (lambda (ctx note-data)
                      (setq ran t)
                      (vulpea-plugin-guide-test--debug-extractor
                       ctx note-data))))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: dbg-id\n:END:\n#+TITLE: Debug\n")))
        (unwind-protect
            (progn
              (should (= (vulpea-db-update-file path) 1))
              (should ran))
          (delete-file path))))))

;;; Schema, versioning and migration (guide: "Step 2: Define the
;;; Schema", "Version Tracking and Migrations")

(ert-deftest vulpea-plugin-guide-schema-and-versioning ()
  "Step 2 schema registers; migration flow reads the old version
before re-registering, exactly as the guide now instructs."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (migrated nil))
      ;; Register version 1 with the guide's Step 2 schema, verbatim.
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'my-extractor
        :version 1
        :extract-fn #'ignore
        :schema '((my_table
                   [(note-id :not-null)
                    (my-field :not-null)
                    (my-value)]
                   (:primary-key [note-id my-field])
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))))
      ;; Migration Helpers: both spellings match; the check
      ;; normalizes dashes to underscores the same way emacsql does
      ;; when it creates the table (locked here because the guide
      ;; explains exactly this).
      (should (vulpea-db--table-exists-p 'my_table))
      (should (vulpea-db--table-exists-p 'my-table))
      ;; Migration Helpers: check current version, verbatim query.
      (should (= 1 (caar (emacsql (vulpea-db)
                                  [:select [version] :from schema-registry
                                   :where (= name $s1)]
                                  "my-extractor"))))
      ;; Schema Versioning: migrate before registering version 2.
      (let ((old-version
             (caar (emacsql (vulpea-db)
                            [:select [version] :from schema-registry
                             :where (= name $s1)]
                            "my-extractor"))))
        (when (and old-version (< old-version 2))
          ;; Migration Helpers: manual schema update, verbatim.
          (emacsql (vulpea-db)
                   [:alter :table my_table :add :column new-field])
          (setq migrated t)))
      (should migrated)
      ;; The new column is queryable.
      (should-not (emacsql (vulpea-db)
                           [:select [new-field] :from my_table]))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'my-extractor
        :version 2
        :extract-fn #'ignore
        :schema '((my_table
                   [(note-id :not-null)
                    (my-field :not-null)
                    (my-value)]
                   (:primary-key [note-id my-field])
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))))
      (should (= 2 (caar (emacsql (vulpea-db)
                                  [:select [version] :from schema-registry
                                   :where (= name $s1)]
                                  "my-extractor")))))))

;;; Complete Working Example: Citation Extractor

(defun vulpea-plugin-guide-test--extract-citations (ctx note-data)
  "Verbatim body of my-extract-citations from the guide."
  (let* ((ast (vulpea-parse-ctx-ast ctx))
         (note-id (plist-get note-data :id))
         (citations nil))

    ;; Extract citations from all paragraphs
    (org-element-map ast 'paragraph
      (lambda (para)
        (let ((content (org-element-interpret-data para))
              (pos 0))
          (while (string-match "\\[@\\([^]]+\\)\\]" content pos)
            ;; Strip text properties: emacsql stores a propertized
            ;; string in its printed #("...") form
            (push (substring-no-properties (match-string 1 content))
                  citations)
            (setq pos (match-end 0))))))

    ;; Insert unique citations into custom table
    (when citations
      (let ((unique-citations (delete-dups (nreverse citations))))
        (emacsql (vulpea-db)
                 [:insert :into citations :values $v1]
                 (mapcar (lambda (citekey)
                           (vector note-id citekey))
                         unique-citations))

        ;; Add to note-data for other extractors
        (plist-put note-data :citations unique-citations)))

    note-data))

(ert-deftest vulpea-plugin-guide-citations-example ()
  "The citation extractor catches every citation in a paragraph and
the guide's query examples run as written."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'citations
        :version 1
        :priority 50
        :requires-ast t  ; my-extract-citations maps the AST
        :schema '((citations
                   [(note-id :not-null)
                    (citekey :not-null)]
                   (:primary-key [note-id citekey])
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))
        :extract-fn #'vulpea-plugin-guide-test--extract-citations))
      ;; Two citations in ONE paragraph - the case the guide's
      ;; original single string-match silently dropped.
      (let ((path-1 (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: paper-1-id\n:END:\n#+TITLE: My Paper\n\nBuilds on [@smith2020] and [@jones2019].\n"))
            (path-2 (vulpea-test--create-temp-org-file
                     ":PROPERTIES:\n:ID: paper-2-id\n:END:\n#+TITLE: Another Paper\n\nExtends [@smith2020].\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path-1)
              (vulpea-db-update-file path-2)

              ;; Find all notes that cite a specific paper (verbatim).
              (let ((citing (emacsql (vulpea-db)
                                     [:select [notes:id notes:title]
                                      :from notes
                                      :inner :join citations
                                      :on (= notes:id citations:note-id)
                                      :where (= citations:citekey $s1)]
                                     "smith2020")))
                (should (= (length citing) 2)))

              ;; Get all citations from a note (verbatim).
              (should (equal (emacsql (vulpea-db)
                                      [:select [citekey] :from citations
                                       :where (= note-id $s1)
                                       :order-by [(asc citekey)]]
                                      "paper-1-id")
                             '(("jones2019") ("smith2020"))))

              ;; Count citations per paper (verbatim).
              (should (equal (emacsql (vulpea-db)
                                      [:select [citekey (funcall count note-id)]
                                       :from citations
                                       :group :by citekey
                                       :order :by [(desc (funcall count note-id))]])
                             '(("smith2020" 2) ("jones2019" 1)))))
          (delete-file path-1)
          (delete-file path-2))))))

;;; Contributing to Core Data (guide: my-promote-meta-links)

(defun vulpea-plugin-guide-test--promote-meta-links (_ctx note-data)
  "Turn id: links in metadata into real note links.
Verbatim body of my-promote-meta-links from the guide."
  (let ((meta-links
         (cl-loop for (_key . values) in (plist-get note-data :meta)
                  append (cl-loop for value in values
                                  when (string-match
                                        "\\[\\[id:\\([^]]+\\)\\]" value)
                                  collect (list :dest (match-string 1 value)
                                                :type "id"
                                                :pos 0
                                                :description nil)))))
    (when meta-links
      (plist-put note-data :links
                 (append (plist-get note-data :links) meta-links))))
  note-data)

(ert-deftest vulpea-plugin-guide-promote-meta-links ()
  "A link contributed via note-data lands in the links table and in
backlink queries, as the guide promises."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'promote-meta-links
        :requires-ast nil
        :extract-fn #'vulpea-plugin-guide-test--promote-meta-links))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: source-id\n:END:\n#+TITLE: Source\n\n- related :: [[id:target-id][Target]]\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; The promoted link is the pos-0 row; the body link
              ;; parsed by core has a real buffer position.
              (should (emacsql (vulpea-db)
                               [:select [pos] :from links
                                :where (and (= source $s1)
                                            (= dest $s2)
                                            (= type $s3)
                                            (= pos 0))]
                               "source-id" "target-id" "id"))
              ;; And it is first-class: visible to backlink queries,
              ;; which return link plists (:source :dest :type :pos
              ;; :description).
              (should (cl-find-if
                       (lambda (link)
                         (and (equal (plist-get link :source) "source-id")
                              (equal (plist-get link :pos) 0)))
                       (vulpea-db-query-links-to "target-id"))))
          (delete-file path))))))

;;; Aggregate Data (guide: my-summary-extractor)

(defun vulpea-plugin-guide-test--summary-extractor (ctx note-data)
  "Verbatim body of my-summary-extractor from the guide."
  (let* ((file-node (vulpea-parse-ctx-file-node ctx))
         (heading-nodes (vulpea-parse-ctx-heading-nodes ctx))
         (all-nodes (cons file-node heading-nodes)))

    ;; Calculate aggregate data
    (let ((total-todos (seq-count
                        (lambda (n) (plist-get n :todo))
                        all-nodes)))
      ;; Store summary for file-level note only
      (when (= (plist-get note-data :level) 0)
        (emacsql (vulpea-db)
                 [:insert :into file-summaries :values $v1]
                 (list (vector (plist-get note-data :id)
                              total-todos)))))

    note-data))

(ert-deftest vulpea-plugin-guide-aggregate-example ()
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (vulpea-db-index-heading-level t))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'guide-summary
        :requires-ast nil
        :schema '((file-summaries
                   [(note-id :not-null :primary-key)
                    (total-todos :not-null)]))
        :extract-fn #'vulpea-plugin-guide-test--summary-extractor))
      (let ((path (vulpea-test--create-temp-org-file
                   (concat ":PROPERTIES:\n:ID: agg-file-id\n:END:\n#+TITLE: Agg\n\n"
                           "* TODO Task one\n:PROPERTIES:\n:ID: agg-h1\n:END:\n\n"
                           "* Plain section\n:PROPERTIES:\n:ID: agg-h2\n:END:\n"))))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (equal (emacsql (vulpea-db)
                                      [:select [note-id total-todos]
                                       :from file-summaries])
                             '(("agg-file-id" 1)))))
          (delete-file path))))))

;;; External Data Integration (guide: my-external-enrichment)

(defvar vulpea-plugin-guide-test--api-calls nil)

(defun vulpea-plugin-guide-test--fetch-from-external-api (external-id)
  "Stub for the guide's fetch-from-external-api placeholder."
  (push external-id vulpea-plugin-guide-test--api-calls)
  (format "data-for-%s" external-id))

(defun vulpea-plugin-guide-test--external-enrichment (_ctx note-data)
  "Body of my-external-enrichment from the guide; only the fetch
function is stubbed."
  ;; Get external data based on note properties
  (let* ((properties (plist-get note-data :properties))
         (external-id (alist-get "EXTERNAL_ID" properties nil nil #'equal)))

    (when external-id
      (let ((external-data
             (vulpea-plugin-guide-test--fetch-from-external-api external-id)))
        (when external-data
          (emacsql (vulpea-db)
                   [:insert :or :replace :into external-cache :values $v1]
                   (list (vector (plist-get note-data :id)
                                external-id
                                external-data
                                (format-time-string "%Y-%m-%d %H:%M:%S")))))))

    note-data))

(ert-deftest vulpea-plugin-guide-external-data ()
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (vulpea-plugin-guide-test--api-calls nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'guide-external
        :requires-ast nil
        :schema '((external-cache
                   [(note-id :not-null :primary-key)
                    (external-id :not-null)
                    (data)
                    (fetched-at)]))
        :extract-fn #'vulpea-plugin-guide-test--external-enrichment))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: ext-note-id\n:EXTERNAL_ID: ext-42\n:END:\n#+TITLE: Ext\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (equal vulpea-plugin-guide-test--api-calls '("ext-42")))
              (should (equal (emacsql (vulpea-db)
                                      [:select [note-id external-id data]
                                       :from external-cache])
                             '(("ext-note-id" "ext-42" "data-for-ext-42")))))
          (delete-file path))))))

;;; Extractor Dependencies (guide: extractor-a / extractor-b)

(defvar vulpea-plugin-guide-test--b-saw nil)

(defun vulpea-plugin-guide-test--extract-something (_ctx)
  "Stub for the guide's extract-something placeholder."
  "a-data-value")

(defun vulpea-plugin-guide-test--process-a-data (a-data)
  "Stub for the guide's process-a-data placeholder."
  (push a-data vulpea-plugin-guide-test--b-saw))

(defun vulpea-plugin-guide-test--extractor-a (ctx note-data)
  "Verbatim body of extractor-a from the guide (stubs aside)."
  (let ((data (vulpea-plugin-guide-test--extract-something ctx)))
    (plist-put note-data :a-data data)
    note-data))

(defun vulpea-plugin-guide-test--extractor-b (_ctx note-data)
  "Verbatim body of extractor-b from the guide (stubs aside)."
  (let ((a-data (plist-get note-data :a-data)))
    (when a-data
      (vulpea-plugin-guide-test--process-a-data a-data))
    note-data))

(ert-deftest vulpea-plugin-guide-extractor-dependencies ()
  "B (priority 50) sees transient note-data written by A (priority 30)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil)
          (vulpea-plugin-guide-test--b-saw nil))
      ;; Register B first to prove priority ordering, not registration
      ;; order, decides execution.
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'extractor-b
        :priority 50
        :requires-ast nil
        :extract-fn #'vulpea-plugin-guide-test--extractor-b))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'extractor-a
        :priority 30
        :requires-ast nil
        :extract-fn #'vulpea-plugin-guide-test--extractor-a))
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: dep-id\n:END:\n#+TITLE: Dep\n")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              (should (equal vulpea-plugin-guide-test--b-saw
                             '("a-data-value"))))
          (delete-file path))))))

;;; Testing Extractors in Isolation (guide: "Debugging")

(ert-deftest vulpea-plugin-guide-isolation-example ()
  "The guide's test-your-extractor skeleton works with the corrected
property-drawer content."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      ;; Register only your extractor
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'my-test
        :version 1
        :priority 50
        :requires-ast nil
        :schema '((guide-isolation [(note-id :not-null)]))
        :extract-fn (lambda (_ctx note-data)
                      (emacsql (vulpea-db)
                               [:insert :into guide-isolation :values $v1]
                               (list (vector (plist-get note-data :id))))
                      note-data)))

      ;; Test with specific content
      (let ((path (vulpea-test--create-temp-org-file
                   ":PROPERTIES:\n:ID: test-id\n:END:\n#+TITLE: Test\n\nTest content")))
        (unwind-protect
            (progn
              (vulpea-db-update-file path)
              ;; Verify extraction results
              (should (= (length (emacsql (vulpea-db)
                                          [:select [note-id]
                                           :from guide-isolation]))
                         1)))
          (delete-file path))))))

;;; Extractors and Async Extraction (guide: my-attachments declaration)

(defun vulpea-plugin-guide-test--attachments-fn (_ctx note-data)
  "Stand-in for the guide's my-attachments-fn."
  note-data)

(ert-deftest vulpea-plugin-guide-async-declarations ()
  "The worker-interaction declarations from the guide register cleanly."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'my-attachments
        :requires-ast nil    ; works purely from NOTE-DATA
        :worker-safe t       ; may run inside the worker (full mode)
        :worker-lib 'my-lib  ; feature/file the worker loads for the fn
        :schema '((attachments [(note-id :not-null) (file :not-null)]))
        :extract-fn #'vulpea-plugin-guide-test--attachments-fn))
      (let ((ext (vulpea-db-get-extractor 'my-attachments)))
        (should ext)
        (should (vulpea-extractor-worker-safe ext))
        (should (eq (vulpea-extractor-worker-lib ext) 'my-lib))
        (should-not (vulpea-extractor-requires-ast-p ext)))
      (should (vulpea-db--table-exists-p 'attachments)))))

;;; Storing Per-Note Data (guide: note-category / note-attrs)

(ert-deftest vulpea-plugin-guide-storing-per-note-data ()
  "The 1:1 side table and the generic (note-id, axis) table register,
accept the guide's insert, and join as shown."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((vulpea-db--extractors nil))
      (vulpea-db-register-extractor
       (make-vulpea-extractor
        :name 'guide-category
        :requires-ast nil
        :schema '((note-category
                   [(note-id :not-null :primary-key)
                    (category :not-null)]
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade))
                  (note-attrs
                   [(note-id :not-null)
                    (axis :not-null)
                    (value :not-null)]
                   (:primary-key [note-id axis])
                   (:foreign-key [note-id] :references notes [id]
                    :on-delete :cascade)))
        :extract-fn #'ignore))
      (vulpea-test--insert-test-note "cat-1" "Categorized")
      (vulpea-test--insert-test-note "cat-2" "Uncategorized")
      ;; The extractor writes at most one row per note (verbatim).
      (let ((note-id "cat-1")
            (category "project"))
        (emacsql (vulpea-db)
                 [:insert :or :replace :into note-category :values $v1]
                 (list (vector note-id category))))
      ;; And queries join it in (verbatim).
      (let ((rows (emacsql (vulpea-db)
                           [:select [notes:id notes:title note-category:category]
                            :from notes
                            :left :join note-category
                            :on (= notes:id note-category:note-id)])))
        (should (member '("cat-1" "Categorized" "project") rows))
        (should (member '("cat-2" "Uncategorized" nil) rows))))))

(provide 'plugin-guide-examples-test)
;;; plugin-guide-examples-test.el ends here
