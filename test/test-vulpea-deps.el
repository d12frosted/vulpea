;;; test-vulpea-deps.el --- Test for vulpea dependencies -*- lexical-binding: t; -*-
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
;;
;;
;;; Code:

(require 'test-vulpea)
(require 'buttercup)
(require 'org)

(describe "vulpea dependencies"
  (it "org-mode-version must be >= 9.4"
    (expect org-version
            :not :to-be-version< "9.4")))

(provide 'test-vulpea-deps)
;;; test-vulpea-deps.el ends here
