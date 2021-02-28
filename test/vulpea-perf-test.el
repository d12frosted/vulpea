;;; vulpea-perf-test.el --- Performance tests -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 28 Feb 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'vulpea-test-utils)
(require 'buttercup)
(require 'org-roam)
(require 'vulpea)

(defconst vulpea-perf-zip-url
  "https://github.com/org-roam/test-org-files/archive/master.zip"
  "Path to zip for test org-roam files.")

(defun vulpea-perf--init ()
  "Initialize performance testing environment."
  (let* ((temp-loc (expand-file-name (make-temp-name "test-org-files-") temporary-file-directory))
         (zip-file-loc (concat temp-loc ".zip"))
         (_ (url-copy-file vulpea-perf-zip-url zip-file-loc))
         (_ (shell-command (format "mkdir -p %s && unzip -j -qq %s -d %s" temp-loc zip-file-loc temp-loc))))
    (setq org-roam-directory temp-loc)
    (org-roam-mode +1)
    (org-roam-db-build-cache)))

(describe "vulpea-select performance"
  :var (duration)
  (before-all
    (vulpea-perf--init))

  (after-all
    (vulpea-test--teardown))

  (it "select without filter is fast"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "bell-bottom sprue Presently the roots")
    (setq duration
          (benchmark-run 100
            (vulpea-select "Note")))
    (message "duration = %s" duration)
    ;; 10 seconds for 100 runs is pretty fast for 1000 notes
    (expect (car duration) :to-be-less-than 10))

  (it "select with filter is fast"
    (spy-on 'org-roam-completion--completing-read
            :and-return-value "bell-bottom sprue Presently the roots")
    (setq duration
          (benchmark-run 100
            (vulpea-select "Note"
                           :filter-fn
                           (lambda (n)
                             (string-prefix-p
                              "bell-bottom"
                              (vulpea-note-title n))))))
    (message "duration = %s" duration)
    ;; 10 seconds for 100 runs is pretty fast for 1000 notes
    (expect (car duration) :to-be-less-than 10)))

(provide 'vulpea-perf-test)
;;; vulpea-perf-test.el ends here
