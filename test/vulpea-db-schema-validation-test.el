;;; vulpea-db-schema-validation-test.el --- Tests -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for surfacing schema violations at sync (`vulpea-db-schema-validation').
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vulpea-db-schema-validation)
(require 'vulpea-schema)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-test-helpers)

(defvar vulpea-dbsv-test--warnings nil
  "Captured (LEVEL . MESSAGE) pairs from `lwarn' during a test.")

(defmacro vulpea-dbsv-test--with (action &rest body)
  "Run BODY with a fresh registry, ACTION, and `lwarn' captured."
  (declare (indent 1))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq))
         (vulpea-db-schema-validation-action ,action)
         (vulpea-dbsv-test--warnings nil))
     (cl-letf (((symbol-function 'lwarn)
                (lambda (_type level msg &rest args)
                  (push (cons level (apply #'format msg args))
                        vulpea-dbsv-test--warnings))))
       ,@body)))

(defun vulpea-dbsv-test--wine ()
  "Define a wine schema requiring a name."
  (vulpea-schema-define 'wine
    :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
    :fields '((:key "name" :required t))))

;;; Handler decision logic

(ert-deftest vulpea-db-schema-validation-silent-keeps-quiet ()
  "With `silent', a violating note is kept and nothing is emitted."
  (vulpea-dbsv-test--with 'silent
    (vulpea-dbsv-test--wine)
    (should (vulpea-db-schema-validation--filter
             (make-vulpea-note :id "w" :title "W" :tags '("wine"))))
    (should-not vulpea-dbsv-test--warnings)))

(ert-deftest vulpea-db-schema-validation-warning-keeps-and-warns ()
  "With `warning', a violating note is kept and a warning is emitted."
  (vulpea-dbsv-test--with 'warning
    (vulpea-dbsv-test--wine)
    (should (vulpea-db-schema-validation--filter
             (make-vulpea-note :id "w" :title "W" :tags '("wine"))))
    (should (= (length vulpea-dbsv-test--warnings) 1))
    (should (eq (caar vulpea-dbsv-test--warnings) :warning))
    (should (string-match-p "name" (cdar vulpea-dbsv-test--warnings)))))

(ert-deftest vulpea-db-schema-validation-error-skips-and-warns ()
  "With `error', a violating note is vetoed and a warning is emitted."
  (vulpea-dbsv-test--with 'error
    (vulpea-dbsv-test--wine)
    (should-not (vulpea-db-schema-validation--filter
                 (make-vulpea-note :id "w" :title "W" :tags '("wine"))))
    (should (eq (caar vulpea-dbsv-test--warnings) :error))
    (should (string-match-p "name" (cdar vulpea-dbsv-test--warnings)))))

(ert-deftest vulpea-db-schema-validation-valid-note-keeps-quiet ()
  "A conformant note is kept with no warning, even under `error'."
  (vulpea-dbsv-test--with 'error
    (vulpea-dbsv-test--wine)
    (should (vulpea-db-schema-validation--filter
             (make-vulpea-note :id "w" :title "W" :tags '("wine")
                               :meta '(("name" "Chablis")))))
    (should-not vulpea-dbsv-test--warnings)))

(ert-deftest vulpea-db-schema-validation-no-schema-keeps-quiet ()
  "A note no schema applies to is kept with no warning."
  (vulpea-dbsv-test--with 'error
    (vulpea-dbsv-test--wine)
    (should (vulpea-db-schema-validation--filter
             (make-vulpea-note :id "x" :title "X" :tags '("misc"))))
    (should-not vulpea-dbsv-test--warnings)))

;;; Integration: the filter hook in `vulpea-db-update-file'

(defmacro vulpea-dbsv-test--with-file (action id content &rest body)
  "Run BODY with a temp DB and a temp org file (ID, CONTENT) indexed under ACTION."
  (declare (indent 3))
  `(let* ((vulpea-schema--registry (make-hash-table :test 'eq))
          (vulpea-db-schema-validation-action ,action)
          (temp-db (make-temp-file "vulpea-dbsv-" nil ".db"))
          (vulpea-db-location temp-db)
          (vulpea-db--connection nil)
          (file (make-temp-file "vulpea-dbsv-" nil ".org")))
     (unwind-protect
         (progn
           (vulpea-dbsv-test--wine)
           (with-temp-file file
             (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n%s" ,id ,content)))
           (vulpea-db)
           (cl-letf (((symbol-function 'lwarn) #'ignore))
             (vulpea-db-update-file file))
           ,@body)
       (when vulpea-db--connection (vulpea-db-close))
       (when (file-exists-p temp-db) (delete-file temp-db))
       (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-db-schema-validation-error-skips-note-from-db ()
  "With `error', a violating note is not indexed into the DB."
  (vulpea-dbsv-test--with-file 'error
      "11111111-1111-1111-1111-111111111111"
      "#+title: Bad Wine\n#+filetags: :wine:\n"
    (should-not (vulpea-db-get-by-id "11111111-1111-1111-1111-111111111111"))))

(ert-deftest vulpea-db-schema-validation-warning-still-indexes ()
  "With `warning', a violating note is still indexed."
  (vulpea-dbsv-test--with-file 'warning
      "22222222-2222-2222-2222-222222222222"
      "#+title: Bad Wine\n#+filetags: :wine:\n"
    (should (vulpea-db-get-by-id "22222222-2222-2222-2222-222222222222"))))

(ert-deftest vulpea-db-schema-validation-error-keeps-valid-note ()
  "With `error', a conformant note is indexed normally."
  (vulpea-dbsv-test--with-file 'error
      "33333333-3333-3333-3333-333333333333"
      "#+title: Good Wine\n#+filetags: :wine:\n\n- name :: Chablis\n"
    (should (vulpea-db-get-by-id "33333333-3333-3333-3333-333333333333"))))

(provide 'vulpea-db-schema-validation-test)
;;; vulpea-db-schema-validation-test.el ends here
