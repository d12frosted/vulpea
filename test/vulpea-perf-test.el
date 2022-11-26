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

(defconst vulpea-perf-zip-branch "master")

(defconst vulpea-perf-zip-url
  (format
   "https://github.com/d12frosted/vulpea-test-notes/archive/refs/heads/%s.zip"
   vulpea-perf-zip-branch)
  "Path to zip for test org-roam files.")

(cl-defun vulpea-perf--init (&key db-file-name
                                  disable-org-roam-autosync
                                  disable-vulpea-autosync)
  "Initialize performance testing environment.

DB-FILE-NAME allows to override default file name of
`org-roam-db-location'.

When DISABLE-ORG-ROAM-AUTOSYNC is non-nil,
`org-roam-db-autosync-mode' will not be enabled.

When DISABLE-VULPEA-AUTOSYNC is non-nil,
`vulpea-db-autosync-mode' will not be enabled."
  (let* ((temp-loc (expand-file-name (make-temp-name "note-files") temporary-file-directory))
         (zip-file-loc (concat temp-loc ".zip"))
         (_ (url-copy-file vulpea-perf-zip-url zip-file-loc))
         (_ (shell-command (format "mkdir -p %s && unzip -qq %s -d %s" temp-loc zip-file-loc temp-loc)))
         (test-notes-dir (expand-file-name
                          (format "vulpea-test-notes-%s/"
                                  vulpea-perf-zip-branch)
                          temp-loc))
         (db-file-name (or db-file-name "org-roam.db")))
    (setq org-roam-directory (expand-file-name "notes/" test-notes-dir)
          org-roam-db-location (expand-file-name db-file-name org-roam-directory))
    (message "Initializing vulpea in %s" org-roam-directory)
    ;; fix file path values
    (when (file-exists-p org-roam-db-location)
      (let ((db (emacsql-sqlite org-roam-db-location)))
        (message "Count of notes: %s"
                 (caar (emacsql db "select count(*) from nodes")))
        (emacsql db [:pragma (= foreign_keys 0)])
        (emacsql db (format "update nodes set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                            (file-name-as-directory org-roam-directory)))
        (emacsql db (format "update files set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                            (file-name-as-directory org-roam-directory)))
        (emacsql db (format "update notes set path = '\"' || '%s' || replace(path, '\"', '') || '\"'"
                            (file-name-as-directory org-roam-directory)))
        (emacsql db (format "update notes set attach = '\"' || '%s' || replace(attach, '\"', '') || '\"'"
                            (file-name-as-directory org-roam-directory)))))
    (unless disable-vulpea-autosync
      (vulpea-db-autosync-enable))
    (unless disable-org-roam-autosync
      (org-roam-db-autosync-enable))))

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
          (name (if (symbolp ,fn)
                    (symbol-name ,fn)
                  "*lambda*")))
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

(defun single-note-bench (id times with-vulpea &optional init-hook)
  "Benchmark synchronisation of single note with ID.

Synchronisation is run TIMES.

When WITH-VULPEA is non-nil, configure `vulpea'.

Optionally, run INIT-HOOK after test env initialisation."
  (vulpea-test--init (not with-vulpea))
  (when init-hook (funcall init-hook))
  (let ((file (vulpea-db-get-file-by-id id)))
    (garbage-collect)
    (prog1 (vulpea-benchmark-run times
             (lambda ()
               (org-roam-db-clear-file file)
               (org-roam-db-update-file file)))
      (vulpea-test--teardown))))

(defun single-note-perf (id times &optional init-hook)
  "Test performance of syncing a note with ID for TIMES.

Optionally, run INIT-HOOK after test env initialisation."
  (let* ((bres-bare (single-note-bench id times nil init-hook))
         (bres-vulpea (single-note-bench id times 'with-vulpea init-hook)))
    ;; common sense - we expect it doesn't take more than 2 times of
    ;; the bare org-roam sync, but it really depends on the machine;
    ;; usually it runs for the same time
    (expect (nth 1 bres-vulpea)
            :to-be-less-than
            (* 2 (nth 1 bres-bare)))))

(describe "vulpea sync performance"
  (xit "should not create huge footprint on synchronisation"
    (let ((runs 1)
          (bres-bare)
          (bres-vulpea))

      ;; bare
      (vulpea-test--teardown)
      (vulpea-perf--init :db-file-name "org-roam-bare.db"
                         :disable-org-roam-autosync t
                         :disable-vulpea-autosync t)
      (setq bres-bare (vulpea-benchmark-run
                          runs
                        #'org-roam-db-sync))
      (expect (caar (org-roam-db-query "select count(*) from nodes")) :to-be-greater-than 0)
      (list-buffers)

      ;; vulpea tables
      (vulpea-test--teardown)
      (vulpea-perf--init :db-file-name "org-roam-vulpea.db"
                         :disable-org-roam-autosync t
                         :disable-vulpea-autosync nil)
      (setq bres-vulpea (vulpea-benchmark-run
                            runs
                          #'org-roam-db-sync))
      (expect (caar (org-roam-db-query "select count(*) from nodes")) :to-be-greater-than 0)

      ;; common sense - we expect it doesn't take more than 3 times of
      ;; the bare org-roam sync
      (expect (nth 1 bres-vulpea) :to-be-less-than (* 3 (nth 1 bres-bare)))))

  (describe "single note synchronisation"
    (it "synchronisation of small note is fast"
      (single-note-perf "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7" 1000))

    (it "synchronisation of medium note is fast"
      (single-note-perf "eeec8f05-927f-4c61-b39e-2fb8228cf484" 1000))

    (it "synchronisation of huge note is fast"
      (single-note-perf "b0dae07d-8789-4737-b830-db775715cbf0" 10
                        (lambda ()
                          (copy-file "test/note-files-extra/sicily.org"
                                     (expand-file-name "sicily.org" org-roam-directory))
                          (org-roam-db-update-file (expand-file-name "sicily.org" org-roam-directory)))))))

(provide 'vulpea-perf-test)
;;; vulpea-perf-test.el ends here
