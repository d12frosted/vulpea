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
(require 'cl-lib)
(require 'vulpea-schema)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-test-helpers)

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

;;; Validation

(defun vulpea-schema-test--wine-schema ()
  "Define and return a self-contained wine schema for validation tests."
  (vulpea-schema-define 'wine
    :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
    :fields
    (list
     '(:key "name" :type string :required t)
     '(:key "producer" :type note :required t)
     '(:key "colour" :type symbol :required t :one-of (red white rose))
     '(:key "vintage" :type number)
     '(:key "carbonation" :type symbol :one-of (still sparkling))
     (list :key "carbonation method" :type 'symbol
           :required (lambda (n)
                       (eq (vulpea-note-meta-get n "carbonation" 'symbol)
                           'sparkling)))
     (list :key "sweetness" :type 'symbol
           :one-of (lambda (n)
                     (if (eq (vulpea-note-meta-get n "carbonation" 'symbol)
                             'sparkling)
                         '(brut extra-brut doux)
                       '(dry semi-dry sweet)))))))

(defmacro vulpea-schema-test--with-wine (var &rest body)
  "Run BODY with a temp db (with producer prod-1) and VAR bound to the wine schema."
  (declare (indent 1))
  `(vulpea-test--with-temp-db
     (vulpea-db)
     (vulpea-test--insert-test-note "prod-1" "Producer One")
     (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
            (,var (vulpea-schema-test--wine-schema)))
       ,@body)))

(ert-deftest vulpea-schema-validate-valid-note ()
  "A fully conformant note produces no violations."
  (vulpea-schema-test--with-wine schema
    (let ((note (make-vulpea-note
                 :id "w1" :title "Wine 1" :tags '("wine")
                 :meta '(("name" "Chablis")
                         ("producer" "[[id:prod-1][Producer One]]")
                         ("colour" "white")
                         ("vintage" "2019")))))
      (should-not (vulpea-schema-validate note schema)))))

(ert-deftest vulpea-schema-validate-missing-required ()
  "A required field with no value yields a missing-required violation."
  (vulpea-schema-test--with-wine schema
    (let* ((note (make-vulpea-note
                  :id "w" :title "W" :tags '("wine")
                  :meta '(("producer" "[[id:prod-1][P]]") ("colour" "red"))))
           (vs (vulpea-schema-validate note schema)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'missing-required))
      (should (equal (vulpea-violation-field (car vs)) "name")))))

(ert-deftest vulpea-schema-validate-disallowed-value ()
  "A value outside :one-of yields a disallowed-value violation."
  (vulpea-schema-test--with-wine schema
    (let* ((note (make-vulpea-note
                  :id "w" :title "W" :tags '("wine")
                  :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                          ("colour" "blue"))))
           (vs (vulpea-schema-validate note schema)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'disallowed-value))
      (should (equal (vulpea-violation-field (car vs)) "colour")))))

(ert-deftest vulpea-schema-validate-wrong-type ()
  "A non-numeric value in a number field yields a wrong-type violation."
  (vulpea-schema-test--with-wine schema
    (let* ((note (make-vulpea-note
                  :id "w" :title "W" :tags '("wine")
                  :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                          ("colour" "red") ("vintage" "ancient"))))
           (vs (vulpea-schema-validate note schema)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'wrong-type))
      (should (equal (vulpea-violation-field (car vs)) "vintage")))))

(ert-deftest vulpea-schema-validate-invalid-reference ()
  "A note field pointing to a missing note yields invalid-reference."
  (vulpea-schema-test--with-wine schema
    (let* ((note (make-vulpea-note
                  :id "w" :title "W" :tags '("wine")
                  :meta '(("name" "X") ("producer" "[[id:ghost][Ghost]]")
                          ("colour" "red"))))
           (vs (vulpea-schema-validate note schema)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'invalid-reference))
      (should (equal (vulpea-violation-field (car vs)) "producer")))))

(ert-deftest vulpea-schema-validate-conditional-required ()
  "A function :required is honored (carbonation method only for sparkling)."
  (vulpea-schema-test--with-wine schema
    (let ((sparkling (make-vulpea-note
                      :id "s" :title "S" :tags '("wine")
                      :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                              ("colour" "white") ("carbonation" "sparkling")))))
      (should (cl-some
               (lambda (v) (and (eq (vulpea-violation-type v) 'missing-required)
                                (equal (vulpea-violation-field v) "carbonation method")))
               (vulpea-schema-validate sparkling schema))))
    (let ((still (make-vulpea-note
                  :id "t" :title "T" :tags '("wine")
                  :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                          ("colour" "red") ("carbonation" "still")))))
      (should-not (cl-some
                   (lambda (v) (equal (vulpea-violation-field v) "carbonation method"))
                   (vulpea-schema-validate still schema))))))

(ert-deftest vulpea-schema-validate-dependent-one-of ()
  "A function :one-of is honored (allowed sweetness depends on carbonation)."
  (vulpea-schema-test--with-wine schema
    ;; "dry" is valid for still wine
    (let ((still (make-vulpea-note
                  :id "t" :title "T" :tags '("wine")
                  :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                          ("colour" "red") ("carbonation" "still")
                          ("sweetness" "dry")))))
      (should-not (cl-some
                   (lambda (v) (equal (vulpea-violation-field v) "sweetness"))
                   (vulpea-schema-validate still schema))))
    ;; "dry" is not valid for sparkling wine
    (let ((sparkling (make-vulpea-note
                      :id "s" :title "S" :tags '("wine")
                      :meta '(("name" "X") ("producer" "[[id:prod-1][P]]")
                              ("colour" "white") ("carbonation" "sparkling")
                              ("carbonation method" "traditional")
                              ("sweetness" "dry")))))
      (should (cl-some
               (lambda (v) (and (eq (vulpea-violation-type v) 'disallowed-value)
                                (equal (vulpea-violation-field v) "sweetness")))
               (vulpea-schema-validate sparkling schema))))))

(ert-deftest vulpea-schema-validate-custom-validator ()
  "A :validate function yields an invalid-value violation with its message."
  (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
         (schema (vulpea-schema-define 'rated
                   :predicate (lambda (_n) t)
                   :fields
                   (list (list :key "score" :type 'number
                               :validate (lambda (v _n)
                                           (if (<= 0 v 100) t
                                             "score must be between 0 and 100")))))))
    (should-not (vulpea-schema-validate
                 (make-vulpea-note :meta '(("score" "87"))) schema))
    (let ((vs (vulpea-schema-validate
               (make-vulpea-note :meta '(("score" "150"))) schema)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'invalid-value))
      (should (equal (vulpea-violation-message (car vs))
                     "score must be between 0 and 100")))))

(ert-deftest vulpea-schema-validate-notes-aggregates ()
  "`vulpea-schema-validate-notes' validates an arbitrary list of notes."
  (vulpea-schema-test--with-wine schema
    (let ((good (make-vulpea-note
                 :id "g" :title "G" :tags '("wine")
                 :meta '(("name" "G") ("producer" "[[id:prod-1][P]]")
                         ("colour" "red"))))
          (bad (make-vulpea-note
                :id "b" :title "B" :tags '("wine")
                :meta '(("producer" "[[id:prod-1][P]]") ("colour" "red")))))
      (let ((vs (vulpea-schema-validate-notes (list good bad) schema)))
        (should (= (length vs) 1))
        (should (equal (vulpea-violation-note-id (car vs)) "b"))))))

(ert-deftest vulpea-schema-validate-all-uses-predicate ()
  "`vulpea-schema-validate-all' validates only notes matching the predicate."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "prod-1" "Producer One")
    (vulpea-test--insert-test-note
     "good" "Good" :tags '("wine")
     :meta '(("name" "G") ("producer" "[[id:prod-1][P]]") ("colour" "red")))
    (vulpea-test--insert-test-note
     "bad" "Bad" :tags '("wine")
     :meta '(("producer" "[[id:prod-1][P]]") ("colour" "red")))
    (vulpea-test--insert-test-note "beer" "Beer" :tags '("beer"))
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq)))
      (vulpea-schema-test--wine-schema)
      (let ((vs (vulpea-schema-validate-all 'wine)))
        (should (= (length vs) 1))
        (should (equal (vulpea-violation-note-id (car vs)) "bad"))
        (should (eq (vulpea-violation-type (car vs)) 'missing-required))))))

;;; Link-target restrictions (#329)

(ert-deftest vulpea-schema-validate-target-tags-satisfied ()
  "A note field whose target carries every :target-tags passes."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note
     "acc-1" "Adobe Account" :tags '("adobe" "account"))
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
           (schema (vulpea-schema-define 'client
                     :predicate (lambda (_n) t)
                     :fields
                     (list '(:key "account" :type note
                                  :target-tags ("adobe"))))))
      (should-not
       (vulpea-schema-validate
        (make-vulpea-note
         :id "c" :title "C"
         :meta '(("account" "[[id:acc-1][Adobe Account]]")))
        schema)))))

(ert-deftest vulpea-schema-validate-target-tags-missing ()
  "A note field whose target lacks a required tag yields invalid-target."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "acc-2" "Plain Account" :tags '("account"))
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
           (schema (vulpea-schema-define 'client
                     :predicate (lambda (_n) t)
                     :fields
                     (list '(:key "account" :type note
                                  :target-tags ("adobe"))))))
      (let ((vs (vulpea-schema-validate
                 (make-vulpea-note
                  :id "c" :title "C"
                  :meta '(("account" "[[id:acc-2][Plain Account]]")))
                 schema)))
        (should (= (length vs) 1))
        (should (eq (vulpea-violation-type (car vs)) 'invalid-target))
        (should (equal (vulpea-violation-field (car vs)) "account"))))))

(ert-deftest vulpea-schema-validate-target-tags-all-of ()
  "All :target-tags must be present; the missing one is named in the message."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "acc-3" "Partial" :tags '("adobe"))
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
           (schema (vulpea-schema-define 'client
                     :predicate (lambda (_n) t)
                     :fields
                     (list '(:key "account" :type note
                                  :target-tags ("adobe" "enterprise"))))))
      (let ((vs (vulpea-schema-validate
                 (make-vulpea-note
                  :id "c" :title "C"
                  :meta '(("account" "[[id:acc-3][Partial]]")))
                 schema)))
        (should (= (length vs) 1))
        (should (eq (vulpea-violation-type (car vs)) 'invalid-target))
        (should (string-match-p "enterprise" (vulpea-violation-message (car vs))))))))

(ert-deftest vulpea-schema-validate-target-tags-missing-reference ()
  "A missing target yields invalid-reference, not invalid-target."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
           (schema (vulpea-schema-define 'client
                     :predicate (lambda (_n) t)
                     :fields
                     (list '(:key "account" :type note
                                  :target-tags ("adobe"))))))
      (let ((vs (vulpea-schema-validate
                 (make-vulpea-note
                  :id "c" :title "C"
                  :meta '(("account" "[[id:ghost][Ghost]]")))
                 schema)))
        (should (= (length vs) 1))
        (should (eq (vulpea-violation-type (car vs)) 'invalid-reference))))))

(ert-deftest vulpea-schema-validate-target-tags-multiple-field ()
  "Each value of a :multiple note field is checked against :target-tags."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "g-ok" "Good Grape" :tags '("grape"))
    (vulpea-test--insert-test-note "g-no" "Bad Grape" :tags '("misc"))
    (let* ((vulpea-schema--registry (make-hash-table :test 'eq))
           (schema (vulpea-schema-define 'wine
                     :predicate (lambda (_n) t)
                     :fields
                     (list '(:key "grapes" :type note :multiple t
                                  :target-tags ("grape"))))))
      (let ((vs (vulpea-schema-validate
                 (make-vulpea-note
                  :id "w" :title "W"
                  :meta '(("grapes" "[[id:g-ok][Good Grape]]"
                           "[[id:g-no][Bad Grape]]")))
                 schema)))
        (should (= (length vs) 1))
        (should (eq (vulpea-violation-type (car vs)) 'invalid-target))
        (should (equal (vulpea-violation-value (car vs)) "g-no"))))))

;;; Composition / inheritance (#327)

(defun vulpea-schema-test--field (schema key)
  "Return the field spec for KEY in SCHEMA, or nil."
  (cl-find key (vulpea-schema-fields schema)
           :key (lambda (f) (plist-get f :key)) :test #'equal))

(defun vulpea-schema-test--field-keys (schema)
  "Return the ordered list of field keys in SCHEMA."
  (mapcar (lambda (f) (plist-get f :key)) (vulpea-schema-fields schema)))

(ert-deftest vulpea-schema-include-inherits-fields ()
  "A schema with :include gains the included schema's fields, before its own."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'base
      :predicate #'ignore
      :fields '((:key "username" :required t)
                (:key "domain" :required t)))
    (let ((child (vulpea-schema-define 'adobe
                   :include 'base
                   :predicate (lambda (_n) t)
                   :fields '((:key "cloud-tier" :one-of (free pro))))))
      (should (equal (vulpea-schema-test--field-keys child)
                     '("username" "domain" "cloud-tier")))
      ;; the inherited spec is carried verbatim
      (should (plist-get (vulpea-schema-test--field child "username") :required)))))

(ert-deftest vulpea-schema-include-child-overrides-collision ()
  "On a :key collision the child's field spec wins, keeping the base position."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'base
      :predicate #'ignore
      :fields '((:key "domain" :required t)
                (:key "username" :required t)))
    (let ((child (vulpea-schema-define 'child
                   :include 'base
                   :predicate (lambda (_n) t)
                   :fields '((:key "domain" :required nil :one-of (com org))))))
      (let ((domain (vulpea-schema-test--field child "domain")))
        (should-not (plist-get domain :required))
        (should (equal (plist-get domain :one-of) '(com org))))
      ;; "domain" keeps its base position; no duplicate is introduced
      (should (equal (vulpea-schema-test--field-keys child)
                     '("domain" "username"))))))

(ert-deftest vulpea-schema-include-multiple ()
  "Multiple includes merge left-to-right; a later one overrides an earlier one."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'a :predicate #'ignore
      :fields '((:key "shared" :required t) (:key "a-only")))
    (vulpea-schema-define 'b :predicate #'ignore
      :fields '((:key "shared" :required nil) (:key "b-only")))
    (let ((child (vulpea-schema-define 'child
                   :include '(a b)
                   :predicate (lambda (_n) t)
                   :fields '((:key "c-only")))))
      (should (equal (vulpea-schema-test--field-keys child)
                     '("shared" "a-only" "b-only" "c-only")))
      ;; b's "shared" (later include) overrides a's
      (should-not (plist-get (vulpea-schema-test--field child "shared") :required)))))

(ert-deftest vulpea-schema-include-transitive ()
  "Includes flatten transitively (a includes b, b includes c)."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'c :predicate #'ignore :fields '((:key "c1")))
    (vulpea-schema-define 'b :include 'c :predicate #'ignore :fields '((:key "b1")))
    (let ((a (vulpea-schema-define 'a :include 'b :predicate (lambda (_n) t)
                                   :fields '((:key "a1")))))
      (should (equal (vulpea-schema-test--field-keys a) '("c1" "b1" "a1"))))))

(ert-deftest vulpea-schema-include-accepts-symbol-or-list ()
  "`:include' accepts a single symbol or a list of symbols equivalently."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'base :predicate #'ignore :fields '((:key "x")))
    (let ((sym (vulpea-schema-define 's :include 'base
                                     :predicate #'ignore :fields nil))
          (lst (vulpea-schema-define 'l :include '(base)
                                     :predicate #'ignore :fields nil)))
      (should (equal (vulpea-schema-test--field-keys sym) '("x")))
      (should (equal (vulpea-schema-test--field-keys lst) '("x"))))))

(ert-deftest vulpea-schema-include-unregistered-errors ()
  "Including an unregistered schema errors at define time."
  (vulpea-schema-test--with-registry
    (should-error (vulpea-schema-define 'child :include 'nope
                                        :predicate #'ignore :fields nil))))

(ert-deftest vulpea-schema-include-validation-inherits ()
  "Validation honors inherited fields (a missing inherited required field)."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'base :predicate (lambda (_n) t)
      :fields '((:key "name" :required t)))
    (vulpea-schema-define 'child :include 'base :predicate (lambda (_n) t)
      :fields '((:key "extra")))
    (let ((vs (vulpea-schema-validate
               (make-vulpea-note :id "n" :title "N" :meta '(("extra" "x")))
               'child)))
      (should (= (length vs) 1))
      (should (eq (vulpea-violation-type (car vs)) 'missing-required))
      (should (equal (vulpea-violation-field (car vs)) "name")))))

(ert-deftest vulpea-schema-include-dedups-duplicate-own-keys ()
  "A duplicate :key within a schema's own :fields collapses last-wins."
  (vulpea-schema-test--with-registry
    (let ((s (vulpea-schema-define 'dup
               :predicate #'ignore
               :fields '((:key "a" :required t)
                         (:key "b")
                         (:key "a" :required nil :one-of (x y))))))
      (should (equal (vulpea-schema-test--field-keys s) '("a" "b")))
      (let ((a (vulpea-schema-test--field s "a")))
        (should-not (plist-get a :required))
        (should (equal (plist-get a :one-of) '(x y)))))))

(ert-deftest vulpea-schema-include-cycle-snapshots-safely ()
  "Cyclic and self includes snapshot at define time without looping."
  (vulpea-schema-test--with-registry
    ;; self-include: redefining a registered schema to include itself just
    ;; folds in its own previous (snapshot) fields - no infinite loop.
    (vulpea-schema-define 'selfy :predicate #'ignore :fields '((:key "orig")))
    (let ((s (vulpea-schema-define 'selfy :include 'selfy
                                   :predicate #'ignore :fields '((:key "new")))))
      (should (equal (vulpea-schema-test--field-keys s) '("orig" "new"))))
    ;; a <-> b cycle, resolved against current snapshots
    (vulpea-schema-define 'cyc-b :predicate #'ignore :fields '((:key "b1")))
    (vulpea-schema-define 'cyc-a :include 'cyc-b :predicate #'ignore
                          :fields '((:key "a1")))
    (let ((b (vulpea-schema-define 'cyc-b :include 'cyc-a :predicate #'ignore
                                   :fields '((:key "b2")))))
      (should (equal (vulpea-schema-test--field-keys b) '("b1" "a1" "b2"))))))

(ert-deftest vulpea-schema-include-redefinition-is-snapshot ()
  "Re-defining a parent after a child does not update the child."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'par :predicate #'ignore :fields '((:key "p1")))
    (vulpea-schema-define 'kid :include 'par :predicate #'ignore
                          :fields '((:key "k1")))
    (should (equal (vulpea-schema-test--field-keys (vulpea-schema-get 'kid))
                   '("p1" "k1")))
    ;; redefine the parent with an extra field
    (vulpea-schema-define 'par :predicate #'ignore
                          :fields '((:key "p1") (:key "p2")))
    ;; the child keeps the fields it snapshotted at its own define time
    (should (equal (vulpea-schema-test--field-keys (vulpea-schema-get 'kid))
                   '("p1" "k1")))))

(ert-deftest vulpea-schema-include-accepts-schema-object ()
  "`:include' accepts a `vulpea-schema' object, bare or inside a list."
  (vulpea-schema-test--with-registry
    (let ((base (vulpea-schema-define 'base :predicate #'ignore
                                      :fields '((:key "x")))))
      (let ((bare (vulpea-schema-define 'b1 :include base
                                        :predicate #'ignore :fields nil))
            (listed (vulpea-schema-define 'b2 :include (list base)
                                          :predicate #'ignore :fields nil)))
        (should (equal (vulpea-schema-test--field-keys bare) '("x")))
        (should (equal (vulpea-schema-test--field-keys listed) '("x")))))))

(ert-deftest vulpea-schema-include-rejects-bad-input ()
  "`:include' rejects non-symbol / non-schema inputs."
  (vulpea-schema-test--with-registry
    (vulpea-schema-define 'base :predicate #'ignore :fields '((:key "x")))
    ;; a bare string or number is not a schema reference
    (should-error (vulpea-schema-define 'a :include "base"
                                        :predicate #'ignore :fields nil))
    (should-error (vulpea-schema-define 'b :include 99
                                        :predicate #'ignore :fields nil))
    ;; a list element that is not a symbol/schema fails on resolution
    (should-error (vulpea-schema-define 'c :include (list 'base 99)
                                        :predicate #'ignore :fields nil))))

(provide 'vulpea-schema-test)
;;; vulpea-schema-test.el ends here
