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

(defconst vulpea-perf-zip-branch "view-table")

(defconst vulpea-perf-zip-url
  (format
   "https://github.com/d12frosted/vulpea-test-notes/archive/refs/heads/%s.zip"
   vulpea-perf-zip-branch)
  "Path to zip for test org-roam files.")

(defun vulpea-perf--init ()
  "Initialize performance testing environment."
  (let* ((temp-loc (expand-file-name (make-temp-name "note-files") temporary-file-directory))
         (zip-file-loc (concat temp-loc ".zip"))
         (_ (url-copy-file vulpea-perf-zip-url zip-file-loc))
         (_ (shell-command (format "mkdir -p %s && unzip -qq %s -d %s" temp-loc zip-file-loc temp-loc)))
         (test-notes-dir (expand-file-name
                          (format "vulpea-test-notes-%s/"
                                  vulpea-perf-zip-branch)
                          temp-loc)))
    (setq org-roam-directory (expand-file-name "notes/" test-notes-dir)
          org-roam-db-location (expand-file-name "org-roam.db" test-notes-dir))
    (message "Initializing vulpea in %s" org-roam-directory)
    ;; fix file path values
    (let ((db (emacsql-sqlite org-roam-db-location)))
      (emacsql db [:pragma (= foreign_keys 0)])
      (emacsql db (format "update nodes set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                          org-roam-directory))
      (emacsql db (format "update files set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                          org-roam-directory))
      (emacsql db (format "update notes set path = '\"' || '%s' || replace(path, '\"', '') || '\"'"
                          org-roam-directory)))
    (vulpea-db-autosync-enable)
    (org-roam-db-autosync-enable)))

(defmacro vulpea-benchmark-run (n fn &optional to-str &rest args)
  "Benchmark FN by running it N times with ARGS.

It also prints some possibly useful information. Result of the
last FN evaluation is converted to string using provided TO-STR
function.

Return a list of the last FN evaluation result, the total elapsed
time for execution, the number of garbage collections that ran,
and the time taken by garbage collection. See also
`benchmark-run-compiled'."
  (declare (indent 1) (debug t))
  `(let* ((v)
          (result)
          (name (symbol-name ,fn)))
     (message "[%s] begin benchmark with %s invocations"
              name ,n)
     (setq result
           (benchmark-run ,n
             (setq v (funcall ,fn ,@args))))
     (message "[%s] benchmark result is %s after %s invocations%s"
              name result ,n
              (if ,to-str
                  (concat " => " (funcall ,to-str v))
                ""))
     (cons v result)))

(defun vulpea-query-result-to-str (notes)
  "Convert NOTES to string."
  (format "%s notes"
          (seq-length notes)))

(describe "vulpea performance"
  (before-all
    (vulpea-perf--init))

  (before-each
    (message "Count of notes: %s"
             (caar (org-roam-db-query "select count(*) from nodes"))))

  (after-all
    (vulpea-test--teardown))

  (describe "vulpea-select"
    (it "select without filter is relatively fast on 9500+ notes"
      (spy-on 'completing-read
              :and-return-value "bell-bottom sprue Presently the roots")
      (let* ((runs 10)
             (bres (vulpea-benchmark-run runs
                     #'vulpea-select nil "Note")))
        ;; 60 seconds for 10 runs is pretty fast for 9500+ notes
        (expect (nth 1 bres) :to-be-less-than 60)))

    (it "select with filter is relatively fast on 9500+ notes"
      (spy-on 'completing-read
              :and-return-value "bell-bottom sprue Presently the roots")
      (let* ((runs 10)
             (bres (vulpea-benchmark-run runs
                     #'vulpea-select nil
                     "Note"
                     :filter-fn
                     (lambda (n)
                       (string-prefix-p
                        "bell-bottom"
                        (vulpea-note-title n))))))
        ;; 60 seconds for 10 runs is pretty fast for 9500+ notes
        (expect (nth 1 bres) :to-be-less-than 60))))

  (describe "vulpea-db-query-by-tags-some"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (tags '("grape" "region"))
             (bres1 (vulpea-benchmark-run runs
                      #'vulpea-db-query-by-tags-some
                      #'vulpea-query-result-to-str
                      tags))
             (bres2 (vulpea-benchmark-run runs
                      #'vulpea-db-query
                      #'vulpea-query-result-to-str
                      (lambda (note)
                        (let ((note-tags (vulpea-note-tags note)))
                          (seq-some
                           (lambda (tag)
                             (seq-contains-p note-tags tag))
                           tags))))))
        (expect (seq-length (nth 0 bres1)) :to-be-greater-than 0)
        (expect (seq-length (nth 0 bres1)) :to-equal (seq-length (nth 0 bres2)))
        (expect (nth 1 bres1) :to-be-less-than (nth 1 bres2)))))

  (describe "vulpea-db-query-by-tags-every"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (tags '("wine" "rating"))
             (bres1 (vulpea-benchmark-run runs
                      #'vulpea-db-query-by-tags-every
                      #'vulpea-query-result-to-str
                      tags))
             (bres2 (vulpea-benchmark-run runs
                      #'vulpea-db-query
                      #'vulpea-query-result-to-str
                      (lambda (note)
                        (let ((note-tags (vulpea-note-tags note)))
                          (seq-every-p
                           (lambda (tag)
                             (seq-contains-p note-tags tag))
                           tags))))))
        (expect (seq-length (nth 0 bres1)) :to-be-greater-than 0)
        (expect (seq-length (nth 0 bres1)) :to-equal (seq-length (nth 0 bres2)))
        (expect (nth 1 bres1) :to-be-less-than (nth 1 bres2)))))

  (describe "vulpea-db-query-by-links-some"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (links-all (seq-map
                         (lambda (link)
                           (cons "id" (car link)))
                         (org-roam-db-query "select dest from links where \"type\" = '\"id\"' group by dest order by count(1) desc")))
             (links (append (seq-take links-all 2)
                            (last links-all 2)))
             (bres1 (vulpea-benchmark-run runs
                      #'vulpea-db-query-by-links-some
                      #'vulpea-query-result-to-str
                      links))
             (bres2 (vulpea-benchmark-run runs
                      #'vulpea-db-query
                      #'vulpea-query-result-to-str
                      (lambda (note)
                        (let ((note-links (vulpea-note-links note)))
                          (seq-some
                           (lambda (link)
                             (seq-contains-p note-links link))
                           links))))))
        (expect (seq-length (nth 0 bres1)) :to-be-greater-than 0)
        (expect (seq-length (nth 0 bres1)) :to-equal (seq-length (nth 0 bres2)))
        (expect (nth 1 bres1) :to-be-less-than (nth 1 bres2)))))

  (describe "vulpea-db-query-by-links-every"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (links-all (seq-map
                         (lambda (link)
                           (cons "id" (car link)))
                         (org-roam-db-query "select dest from links where \"type\" = '\"id\"' group by dest order by count(1) desc")))
             (links (seq-take links-all 2))
             (bres1 (vulpea-benchmark-run runs
                      #'vulpea-db-query-by-links-every
                      #'vulpea-query-result-to-str
                      links))
             (bres2 (vulpea-benchmark-run runs
                      #'vulpea-db-query
                      #'vulpea-query-result-to-str
                      (lambda (note)
                        (let ((note-links (vulpea-note-links note)))
                          (seq-every-p
                           (lambda (link)
                             (seq-contains-p note-links link))
                           links))))))
        (expect (seq-length (nth 0 bres1)) :to-be-greater-than 0)
        (expect (seq-length (nth 0 bres1)) :to-equal (seq-length (nth 0 bres2)))
        (expect (nth 1 bres1) :to-be-less-than (nth 1 bres2))))))

(provide 'vulpea-perf-test)
;;; vulpea-perf-test.el ends here
