;;; vulpea-utils-test.el --- Test `vulpea-utils' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
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
;; Test `vulpea-utils' module.
;;
;;; Code:

(require 'vulpea-test-utils)
(require 'vulpea-utils)
(require 'vulpea-db)

(describe "vulpea-utils-with-note"
  :var (id)
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "places point at the beginning of file when level is 0"
    ;; file-level id
    (setq id "eeec8f05-927f-4c61-b39e-2fb8228cf484")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (org-entry-get (point) "ID"))
            :to-equal id))

  (it "places point at the headline when level is bigger than 0"
    ;; some heading id
    (setq id "cfc39858-351d-4f1e-8f98-10d16d71f49e")
    (expect (vulpea-utils-with-note (vulpea-db-get-by-id id)
              (org-entry-get (point) "ID"))
            :to-equal id)))

(describe "vulpea-utils--uuid-regexp"
  (it "matches some UUID"
    (expect (string-match vulpea-utils--uuid-regexp (org-id-new))
            :to-be 0))
  (it "doesn't match non-UUID"
    (expect (string-match vulpea-utils--uuid-regexp "not UUID")
            :to-be nil)))

(describe "vulpea-utils-collect-while"
  (it "repeats a function until filter returns nil"
    (let ((n 0))
      (expect (vulpea-utils-collect-while
               (lambda () (setq n (+ 1 n)))
               (lambda (v) (< v 5)))
              :to-equal '(1 2 3 4)))))

(describe "vulpea-utils-repeat-while"
  (it "repeats a function until filter returns nil"
    (let ((n 0))
      (expect (vulpea-utils-repeat-while
               (lambda () (setq n (+ 1 n)))
               (lambda (v) (< v 5)))
              :to-equal 5))))

(provide 'vulpea-utils-test)
;;; vulpea-utils-test.el ends here
