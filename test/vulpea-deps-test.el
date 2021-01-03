;;; vulpea-deps-test.el --- Test for vulpea dependencies -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 02 Jan 2021
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for checking existence of `vulpea' dependencies.
;;
;; Unfortunately, `org' is built in, but `vulpea' requires the latest version.
;; This test simplifies analysis of failing tests.
;;
;;; Code:

(require 'vulpea-test)
(require 'buttercup)
(require 'org)

(describe "vulpea dependencies"
  (it "org-mode-version must be >= 9.4"
    (expect org-version
            :not :to-be-version< "9.4")))

(provide 'vulpea-deps-test)
;;; vulpea-deps-test.el ends here
