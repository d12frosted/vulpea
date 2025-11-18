;;; vulpea-perf-test.el --- Performance tests for V2 -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2025 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.2"))
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
;; Performance tests for Vulpea V2.
;;
;; These tests benchmark core query operations against a large dataset
;; (~9500+ notes) to ensure V2's indexed queries perform as expected.
;;
;; Note: These tests download a large dataset and are expensive to run.
;; Run them explicitly with: eldev test vulpea-perf-
;;
;;; Code:

(require 'ert)
(require 'vulpea)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-db-sync)
(require 'cl-lib)

(defconst vulpea-perf-zip-branch "master")

(defconst vulpea-perf-zip-url
  (format
   "https://github.com/d12frosted/vulpea-test-notes/archive/refs/heads/%s.zip"
   vulpea-perf-zip-branch)
  "Path to zip for test note files.")

(defvar vulpea-perf--temp-loc nil
  "Temporary location for test notes.")

(defvar vulpea-perf--notes-dir nil
  "Directory containing test notes.")

(defvar vulpea-perf--initialized nil
  "Whether the performance test environment has been initialized.")

(cl-defun vulpea-perf--init ()
  "Initialize performance testing environment.

Downloads test notes and initializes vulpea database."
  (unless vulpea-perf--initialized
    (let* ((temp-loc (expand-file-name (make-temp-name "note-files") temporary-file-directory))
           (zip-file-loc (concat temp-loc ".zip")))
      (message "Downloading test notes from %s..." vulpea-perf-zip-url)
      (url-copy-file vulpea-perf-zip-url zip-file-loc)
      (message "Extracting test notes to %s..." temp-loc)
      (shell-command (format "mkdir -p %s && unzip -qq %s -d %s" temp-loc zip-file-loc temp-loc))
      (setq vulpea-perf--temp-loc temp-loc
            vulpea-perf--notes-dir (expand-file-name
                                    (format "vulpea-test-notes-%s/notes/"
                                            vulpea-perf-zip-branch)
                                    temp-loc))
      (setq vulpea-db-sync-directories (list vulpea-perf--notes-dir)
            vulpea-db-location (expand-file-name "vulpea.db" vulpea-perf--notes-dir))
      (message "Initializing vulpea database in %s" vulpea-perf--notes-dir)

      ;; Initialize database
      (vulpea-db-autosync-mode 1)

      ;; Wait for initial sync to complete
      (message "Waiting for database sync...")
      (sleep-for 2)

      (let ((count (vulpea-db-count-notes)))
        (message "Database initialized with %d notes" count)
        (when (< count 1000)
          (error "Expected at least 1000 notes, got %d. Database may not be properly initialized" count)))

      (setq vulpea-perf--initialized t))))

(defun vulpea-perf--teardown ()
  "Clean up performance testing environment."
  (when vulpea-perf--initialized
    (vulpea-db-autosync-mode -1)
    (vulpea-db-close)
    (when (and vulpea-perf--temp-loc
               (file-exists-p vulpea-perf--temp-loc))
      (delete-directory vulpea-perf--temp-loc t))
    (setq vulpea-perf--temp-loc nil
          vulpea-perf--notes-dir nil
          vulpea-perf--initialized nil)))

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

(defmacro vulpea-perf--with-env (&rest body)
  "Execute BODY with performance test environment initialized."
  `(progn
     (vulpea-perf--init)
     (unwind-protect
         (progn ,@body)
       ;; Don't teardown - reuse environment for other tests
       )))

(ert-deftest vulpea-perf-select-without-filter ()
  "Test that vulpea-select without filter is fast on 9500+ notes."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) "bell-bottom sprue Presently the roots")))
     (let* ((runs 10)
            (bres (vulpea-benchmark-run runs
                                        #'vulpea-select nil "Note")))
       ;; 60 seconds for 10 runs is acceptable for 9500+ notes
       (should (< (nth 1 bres) 60))))))

(ert-deftest vulpea-perf-select-with-filter ()
  "Test that vulpea-select with filter is fast on 9500+ notes."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) "bell-bottom sprue Presently the roots")))
     (let* ((runs 10)
            (bres (vulpea-benchmark-run runs
                                        #'vulpea-select nil
                                        "Note"
                                        :filter-fn
                                        (lambda (n)
                                          (string-prefix-p
                                           "bell-bottom"
                                           (vulpea-note-title n))))))
       ;; 60 seconds for 10 runs is acceptable for 9500+ notes
       (should (< (nth 1 bres) 60))))))

(ert-deftest vulpea-perf-query-by-tags-some ()
  "Test that vulpea-db-query-by-tags-some is faster than generic query."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
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
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(ert-deftest vulpea-perf-query-by-tags-every ()
  "Test that vulpea-db-query-by-tags-every is faster than generic query."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
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
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(ert-deftest vulpea-perf-query-by-links-some ()
  "Test that vulpea-db-query-by-links-some is faster than generic query."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (let* ((runs 10)
          ;; Get some popular link destinations
          (all-notes (vulpea-db-query))
          (notes-with-links (seq-filter
                             (lambda (note)
                               (> (length (vulpea-note-links note)) 0))
                             all-notes))
          (links (seq-take
                  (seq-uniq
                   (seq-mapcat
                    (lambda (note)
                      (seq-map
                       (lambda (link)
                         (cons "id" (plist-get link :dest)))
                       (vulpea-note-links note)))
                    notes-with-links))
                  4))
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
                                              (seq-some
                                               (lambda (note-link)
                                                 (and (equal (plist-get note-link :type) (or (cdr link) "id"))
                                                      (equal (plist-get note-link :dest) (car link))))
                                               note-links))
                                            links))))))
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(ert-deftest vulpea-perf-query-by-links-every ()
  "Test that vulpea-db-query-by-links-every is faster than generic query."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (let* ((runs 10)
          ;; Get some popular link destinations
          (all-notes (vulpea-db-query))
          (notes-with-links (seq-filter
                             (lambda (note)
                               (> (length (vulpea-note-links note)) 0))
                             all-notes))
          (links (seq-take
                  (seq-uniq
                   (seq-mapcat
                    (lambda (note)
                      (seq-map
                       (lambda (link)
                         (cons "id" (plist-get link :dest)))
                       (vulpea-note-links note)))
                    notes-with-links))
                  2))
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
                                              (seq-some
                                               (lambda (note-link)
                                                 (and (equal (plist-get note-link :type) (or (cdr link) "id"))
                                                      (equal (plist-get note-link :dest) (car link))))
                                               note-links))
                                            links))))))
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(ert-deftest vulpea-perf-query-by-meta ()
  "Test that vulpea-db-query-by-meta is faster than generic query (V2 NEW)."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (let* ((runs 10)
          ;; Find a common metadata key
          (all-notes (vulpea-db-query))
          (notes-with-meta (seq-filter
                            (lambda (note)
                              (> (length (vulpea-note-meta note)) 0))
                            all-notes)))
     (when notes-with-meta
       ;; Get first common key from first note with metadata
       (let* ((test-note (car notes-with-meta))
              (test-key (caar (vulpea-note-meta test-note)))
              (test-value (cdar (vulpea-note-meta test-note)))
              (bres1 (vulpea-benchmark-run runs
                                           #'vulpea-db-query-by-meta
                                           #'vulpea-query-result-to-str
                                           test-key test-value))
              (bres2 (vulpea-benchmark-run runs
                                           #'vulpea-db-query
                                           #'vulpea-query-result-to-str
                                           (lambda (note)
                                             (let ((meta-val (vulpea-meta-get note test-key)))
                                               (and meta-val
                                                    (equal meta-val test-value)))))))
         (when (> (seq-length (nth 0 bres1)) 0)
           (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
           ;; Indexed query should be faster
           (should (< (nth 1 bres1) (nth 1 bres2)))))))))

(ert-deftest vulpea-perf-search-by-title ()
  "Test that vulpea-db-search-by-title is faster than generic query (V2 NEW)."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (let* ((runs 10)
          (pattern "wine*")
          (bres1 (vulpea-benchmark-run runs
                                       #'vulpea-db-search-by-title
                                       #'vulpea-query-result-to-str
                                       pattern))
          (bres2 (vulpea-benchmark-run runs
                                       #'vulpea-db-query
                                       #'vulpea-query-result-to-str
                                       (lambda (note)
                                         (string-prefix-p
                                          "wine"
                                          (vulpea-note-title note))))))
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(ert-deftest vulpea-perf-query-by-level ()
  "Test that vulpea-db-query-by-level is faster than generic query (V2 NEW)."
  :tags '(:performance :expensive)
  (vulpea-perf--with-env
   (let* ((runs 10)
          (level 0)
          (bres1 (vulpea-benchmark-run runs
                                       #'vulpea-db-query-by-level
                                       #'vulpea-query-result-to-str
                                       level))
          (bres2 (vulpea-benchmark-run runs
                                       #'vulpea-db-query
                                       #'vulpea-query-result-to-str
                                       (lambda (note)
                                         (= (vulpea-note-level note) level)))))
     (should (> (seq-length (nth 0 bres1)) 0))
     (should (= (seq-length (nth 0 bres1)) (seq-length (nth 0 bres2))))
     ;; Indexed query should be faster
     (should (< (nth 1 bres1) (nth 1 bres2))))))

(provide 'vulpea-perf-test)
;;; vulpea-perf-test.el ends here
