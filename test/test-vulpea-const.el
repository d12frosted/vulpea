;;; test-vulpea-const.el --- Test consts -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1") (buttercup "1.23"))
;;
;; Created: 29 Dec 2020
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test `vulpea-const' module.
;;
;;; Code:

(require 'test-vulpea)
(require 'buttercup)
(require 'org-id)
(require 'vulpea-const)

(describe "UUID regexp"
  (it "matches some UUID"
    (expect (string-match vulpea-uuid-regexp (org-id-new))
            :to-be 0))
  (it "doesn't match non-UUID"
    (expect (string-match vulpea-uuid-regexp "not UUID")
            :to-be nil)))

(provide 'test-vulpea-const)
;;; test-vulpea-const.el ends here
