;;; vulpea-schema-test.el --- Tests for vulpea-schema -*- lexical-binding: t; -*-
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
;; Tests for the schema definition layer (`vulpea-schema').
;;
;;; Code:

(require 'ert)
(require 'vulpea-schema)
(require 'vulpea-note)

(defmacro vulpea-schema-test--with-registry (&rest body)
  "Run BODY with a fresh, isolated schema registry."
  (declare (indent 0))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq)))
     ,@body))

;;; Definition and registry

(ert-deftest vulpea-schema-define-registers-and-retrieves ()
  "A defined schema is stored and retrievable by name."
  (vulpea-schema-test--with-registry
    (let ((schema (vulpea-schema-define 'test-schema
                    :predicate (lambda (_n) t)
                    :fields '((:key "name" :required t)))))
      (should (vulpea-schema-p schema))
      (should (eq (vulpea-schema-name schema) 'test-schema))
      (should (eq (vulpea-schema-get 'test-schema) schema))
      (should (memq 'test-schema (vulpea-schema-list))))))

(ert-deftest vulpea-schema-define-replaces-existing ()
  "Re-defining a schema with the same name replaces it."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'dup :predicate #'ignore :fields nil)
    (vulpea-schema-define 'dup :predicate (lambda (_n) t)
                          :fields '((:key "x")))
    (should (= (length (vulpea-schema-list)) 1))
    (should (vulpea-schema-applies-p (make-vulpea-note) 'dup))))

(ert-deftest vulpea-schema-define-validates-inputs ()
  "Invalid schema definitions are rejected."
  (vulpea-schema-test--with-registry
    (should-error (vulpea-schema-define "not-a-symbol" :predicate #'ignore))
    (should-error (vulpea-schema-define 'x :predicate "not-a-function"))
    ;; field without a string :key
    (should-error (vulpea-schema-define 'x :predicate #'ignore
                                        :fields '((:type symbol))))))

(ert-deftest vulpea-schema-unregister-removes ()
  "Unregistering a schema removes it from the registry."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'gone :predicate #'ignore :fields nil)
    (should (vulpea-schema-get 'gone))
    (vulpea-schema-unregister 'gone)
    (should-not (vulpea-schema-get 'gone))
    (should-not (memq 'gone (vulpea-schema-list)))))

(ert-deftest vulpea-schema-resolve-accepts-name-or-object ()
  "`vulpea-schema--resolve' accepts a name or a schema object."
  (vulpea-schema-test--with-registry
    (let ((schema (vulpea-schema-define 'r :predicate #'ignore :fields nil)))
      (should (eq (vulpea-schema--resolve 'r) schema))
      (should (eq (vulpea-schema--resolve schema) schema))
      (should-error (vulpea-schema--resolve 'nonexistent)))))

;;; Predicate

(ert-deftest vulpea-schema-applies-p-runs-predicate ()
  "`vulpea-schema-applies-p' runs the schema predicate against a note."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'tagged-wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields nil)
    (should (vulpea-schema-applies-p
             (make-vulpea-note :tags '("wine")) 'tagged-wine))
    (should-not (vulpea-schema-applies-p
                 (make-vulpea-note :tags '("beer")) 'tagged-wine))))

;;; Field access

(ert-deftest vulpea-schema-field-value-reads-typed ()
  "`vulpea-schema-field-value' reads a field with its declared type."
  (let ((note (make-vulpea-note
               :meta '(("name" "Chardonnay")
                       ("vintage" "2019")
                       ("colour" "white")))))
    (should (equal (vulpea-schema-field-value note '(:key "name")) "Chardonnay"))
    (should (equal (vulpea-schema-field-value note '(:key "vintage" :type number)) 2019))
    (should (eq (vulpea-schema-field-value note '(:key "colour" :type symbol)) 'white))
    ;; a bare key string defaults to the string type
    (should (equal (vulpea-schema-field-value note "name") "Chardonnay"))
    ;; missing field yields nil
    (should-not (vulpea-schema-field-value note '(:key "absent")))))

(ert-deftest vulpea-schema-field-value-multiple ()
  "A :multiple field reads all of its values as a list."
  (let ((note (make-vulpea-note
               :meta '(("grapes" "Chardonnay" "Pinot Noir")))))
    (should (equal (vulpea-schema-field-value note '(:key "grapes" :multiple t))
                   '("Chardonnay" "Pinot Noir")))))

(provide 'vulpea-schema-test)
;;; vulpea-schema-test.el ends here
