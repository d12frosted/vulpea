;;; vulpea-meta-perf-test.el --- Performance test of `vulpea-meta' module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 18 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'vulpea-test-utils)
(require 'buttercup)
(require 'org-roam)
(require 'vulpea-meta)
(require 'vulpea-db)

(describe "vulpea-meta-get"
  :var ((id "05907606-f836-45bf-bd36-a8444308eddd"))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "using parsed meta is faster than using id"
    (let* ((meta (vulpea-meta id))
           (res-id
            (benchmark-run 10
              (vulpea-meta-get id "symbol")))
           (res-meta
            (benchmark-run 10
              (vulpea-meta-get! meta "symbol"))))
      (message "res-id   = %s" res-id)
      (message "res-meta = %s" res-meta)
      (expect (car res-meta) :to-be-less-than (car res-id)))))

(describe "vulpea-meta-get-list"
  :var ((id "05907606-f836-45bf-bd36-a8444308eddd"))
  (before-all
    (vulpea-test--init))

  (after-all
    (vulpea-test--teardown))

  (it "using parsed meta is faster than using id"
    (let* ((meta (vulpea-meta id))
           (res-id
            (benchmark-run 10
              (vulpea-meta-get-list id "tags")))
           (res-meta
            (benchmark-run 10
              (vulpea-meta-get-list! meta "tags"))))
      (message "res-id   = %s" res-id)
      (message "res-meta = %s" res-meta)
      (expect (car res-meta) :to-be-less-than (car res-id)))))

(provide 'vulpea-meta-perf-test)
;;; vulpea-meta-perf-test.el ends here
