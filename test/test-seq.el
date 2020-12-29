;;; test-seq.el --- Test seq extensions -*- lexical-binding: t; -*-
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
;; Test `+seq' module.
;;
;;; Code:

(require 'buttercup)
(require '+seq)

(describe "+seq-singleton"
  (it "returns first element from the list with one element"
    (expect (+seq-singleton '(1))
            :to-be 1))
  (it "returns nil from the list with more than one element"
    (expect (+seq-singleton '(1 2))
            :to-be nil))
  (it "returns nil from the empty list"
    (expect (+seq-singleton nil)
            :to-be nil)))

(provide 'test-seq)
;;; test-seq.el ends here
