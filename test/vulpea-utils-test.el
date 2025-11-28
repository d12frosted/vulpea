;;; vulpea-utils-test.el --- Tests for vulpea-utils -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2025 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `vulpea-utils' module (v2).
;;
;;; Code:

(require 'ert)
(require 'vulpea-utils)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)
(require 'vulpea-buffer)
(require 'org-id)

;;; Test Helpers

(defun vulpea-test--create-temp-org-file (content)
  "Create temporary org file with CONTENT.
Returns the file path."
  (let ((file (make-temp-file "vulpea-test-" nil ".org")))
    (with-temp-file file
      (insert content))
    file))

;;; vulpea-utils-with-note Tests

(ert-deftest vulpea-utils-with-note-file-level ()
  "Test vulpea-utils-with-note places point at beginning for file-level note."
  (let* ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
         (file (vulpea-test--create-temp-org-file
                (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Test Note\n\nContent here." id)))
         (note (make-vulpea-note
                :id id
                :path file
                :level 0
                :title "Test Note")))
    (unwind-protect
        (let ((result (vulpea-utils-with-note note
                        (org-entry-get (point) "ID"))))
          (should (equal result id)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest vulpea-utils-with-note-heading-level ()
  "Test vulpea-utils-with-note places point at heading for heading-level note."
  (let* ((id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
         (file (vulpea-test--create-temp-org-file
                (format "#+title: Document\n\n* Heading 1\n:PROPERTIES:\n:ID: %s\n:END:\n\nHeading content." id)))
         (note (make-vulpea-note
                :id id
                :path file
                :level 1
                :title "Heading 1")))
    (unwind-protect
        (let ((result (vulpea-utils-with-note note
                        (org-entry-get (point) "ID"))))
          (should (equal result id)))
      (when (file-exists-p file)
        (delete-file file)))))

;;; UUID Regexp Tests

(ert-deftest vulpea-utils-uuid-regexp-matches ()
  "Test vulpea-utils--uuid-regexp matches valid UUID."
  (should (= 0 (string-match vulpea-utils--uuid-regexp (org-id-new)))))

(ert-deftest vulpea-utils-uuid-regexp-no-match ()
  "Test vulpea-utils--uuid-regexp doesn't match non-UUID."
  (should (null (string-match vulpea-utils--uuid-regexp "not UUID"))))

;;; Collection Utilities Tests

(ert-deftest vulpea-utils-collect-while-basic ()
  "Test vulpea-utils-collect-while repeats until filter returns nil."
  (let ((n 0))
    (should (equal (vulpea-utils-collect-while
                    (lambda () (setq n (1+ n)))
                    (lambda (v) (< v 5)))
                   '(1 2 3 4)))))

(ert-deftest vulpea-utils-repeat-while-basic ()
  "Test vulpea-utils-repeat-while repeats until filter returns nil."
  (let ((n 0))
    (should (equal (vulpea-utils-repeat-while
                    (lambda () (setq n (1+ n)))
                    (lambda (v) (< v 5)))
                   5))))

;;; vulpea-utils-with-note-sync Tests

(defmacro vulpea-test--with-temp-db-and-file (id content &rest body)
  "Execute BODY with temporary database and file.
Creates a temp file with ID and CONTENT, adds it to temp DB."
  (declare (indent 2))
  `(let* ((temp-db-file (make-temp-file "vulpea-utils-test-" nil ".db"))
          (vulpea-db-location temp-db-file)
          (vulpea-db--connection nil)
          (temp-org-file (make-temp-file "vulpea-utils-test-" nil ".org")))
     (with-temp-file temp-org-file
       (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n%s" ,id ,content)))
     (unwind-protect
         (progn
           (vulpea-db)
           (vulpea-db-update-file temp-org-file)
           ,@body)
       (when vulpea-db--connection
         (vulpea-db-close))
       (when (file-exists-p temp-db-file)
         (delete-file temp-db-file))
       (when (file-exists-p temp-org-file)
         (delete-file temp-org-file)))))

(ert-deftest vulpea-utils-with-note-sync-saves-and-syncs ()
  "Test vulpea-utils-with-note-sync saves buffer and syncs database."
  (let ((id "eeec8f05-927f-4c61-b39e-2fb8228cf484"))
    (vulpea-test--with-temp-db-and-file id "#+title: Original\n"
      (let ((note (vulpea-db-get-by-id id)))
        ;; Modify the title using the sync macro
        (vulpea-utils-with-note-sync note
          (vulpea-buffer-title-set "Modified"))
        ;; Database should be updated immediately
        (should (equal (vulpea-note-title (vulpea-db-get-by-id id))
                       "Modified"))))))

(ert-deftest vulpea-utils-with-note-sync-returns-result ()
  "Test vulpea-utils-with-note-sync returns result of body."
  (let ((id "cfc39858-351d-4f1e-8f98-10d16d71f49e"))
    (vulpea-test--with-temp-db-and-file id "#+title: Test\n"
      (let* ((note (vulpea-db-get-by-id id))
             (result (vulpea-utils-with-note-sync note
                       (vulpea-buffer-title-set "New Title")
                       "return-value")))
        (should (equal result "return-value"))))))

(ert-deftest vulpea-utils-with-note-sync-heading-level ()
  "Test vulpea-utils-with-note-sync works for heading-level notes."
  (let* ((file-id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
         (heading-id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
         (temp-db-file (make-temp-file "vulpea-utils-test-" nil ".db"))
         (vulpea-db-location temp-db-file)
         (vulpea-db--connection nil)
         (temp-org-file (make-temp-file "vulpea-utils-test-" nil ".org")))
    (with-temp-file temp-org-file
      (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: Document\n\n* Heading\n:PROPERTIES:\n:ID: %s\n:END:\n\nContent."
                      file-id heading-id)))
    (unwind-protect
        (progn
          (vulpea-db)
          (vulpea-db-update-file temp-org-file)
          (let ((heading-note (vulpea-db-get-by-id heading-id)))
            ;; Add a property to the heading
            (vulpea-utils-with-note-sync heading-note
              (org-entry-put nil "CUSTOM" "value"))
            ;; Verify the property was set (read from file)
            (vulpea-utils-with-note heading-note
              (should (equal (org-entry-get nil "CUSTOM") "value")))))
      (when vulpea-db--connection
        (vulpea-db-close))
      (when (file-exists-p temp-db-file)
        (delete-file temp-db-file))
      (when (file-exists-p temp-org-file)
        (delete-file temp-org-file)))))

(provide 'vulpea-utils-test)
;;; vulpea-utils-test.el ends here
