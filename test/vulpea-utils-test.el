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

(provide 'vulpea-utils-test)
;;; vulpea-utils-test.el ends here
