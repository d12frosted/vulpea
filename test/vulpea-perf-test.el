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
  "https://github.com/d12frosted/vulpea-test-notes/archive/master.zip"
  "Path to zip for test org-roam files.")

(defun vulpea-perf--init ()
  "Initialize performance testing environment."
  (let* ((temp-loc (expand-file-name (make-temp-name "note-files") temporary-file-directory))
         (zip-file-loc (concat temp-loc ".zip"))
         (_ (url-copy-file vulpea-perf-zip-url zip-file-loc))
         (_ (shell-command (format "mkdir -p %s && unzip -qq %s -d %s" temp-loc zip-file-loc temp-loc)))
         (test-notes-dir (expand-file-name "vulpea-test-notes-master/" temp-loc)))
    (setq org-roam-directory (expand-file-name "notes-generated/notes/" test-notes-dir)
          org-roam-db-location (expand-file-name "org-roam.db" test-notes-dir))
    ;; fix file path values
    (let ((db (emacsql-sqlite org-roam-db-location)))
      (emacsql db (format "update nodes set file = '\"' || '%s' || replace(file, '\"', '') || '\"'"
                          org-roam-directory)))
    (vulpea-db-autosync-enable)
    (org-roam-db-autosync-enable)
    (message "Count of notes: %s" (length (vulpea-db-query)))))

(describe "vulpea performance"
  (before-all
    (vulpea-perf--init))

  (after-all
    (vulpea-test--teardown))

  (describe "vulpea-select"
    :var (duration)
    (it "select without filter is relatively fast on 9500+ notes"
      (spy-on 'completing-read
              :and-return-value "bell-bottom sprue Presently the roots")
      (setq duration
            (benchmark-run 10
              (vulpea-select "Note")))
      (message "duration = %s" duration)
      ;; 40 seconds for 10 runs is pretty fast for 9500+ notes
      (expect (car duration) :to-be-less-than 40))

    (it "select with filter is relatively fast on 9500+ notes"
      (spy-on 'completing-read
              :and-return-value "bell-bottom sprue Presently the roots")
      (setq duration
            (benchmark-run 10
              (vulpea-select "Note"
                             :filter-fn
                             (lambda (n)
                               (string-prefix-p
                                "bell-bottom"
                                (vulpea-note-title n))))))
      (message "duration = %s" duration)
      ;; 40 seconds for 10 runs is pretty fast for 9500+ notes
      (expect (car duration) :to-be-less-than 40)))

  (describe "vulpea-db-query-by-tags-some"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (tags-all (seq-map
                        #'car
                        (org-roam-db-query "select tag from tags group by tag order by count(1) desc")))
             (tags (append (seq-take tags-all 2)
                           (last tags-all 2)))
             (duration1 (benchmark-run runs
                          (vulpea-db-query-by-tags-some tags)))
             (duration2 (benchmark-run runs
                          (vulpea-db-query (lambda (note)
                                             (let ((note-tags (vulpea-note-tags note)))
                                               (seq-some
                                                (lambda (tag)
                                                  (seq-contains-p note-tags tag))
                                                tags)))))))
        (message "runs = %s" runs)
        (message "duration specialized = %s" duration1)
        (message "duration generic     = %s" duration2)
        (expect (car duration1)
                :to-be-less-than
                (car duration2)))))

  (describe "vulpea-db-query-by-tags-every"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (tags-all (seq-map
                        #'car
                        (org-roam-db-query "select tag from tags group by tag order by count(1) desc")))
             (tags (append (seq-take tags-all 2)
                           (last tags-all 2)))
             (duration1 (benchmark-run runs
                          (vulpea-db-query-by-tags-every tags)))
             (duration2 (benchmark-run runs
                          (vulpea-db-query (lambda (note)
                                             (let ((note-tags (vulpea-note-tags note)))
                                               (seq-every-p
                                                (lambda (tag)
                                                  (seq-contains-p note-tags tag))
                                                tags)))))))
        (message "runs = %s" runs)
        (message "duration specialized = %s" duration1)
        (message "duration generic     = %s" duration2)
        (expect (car duration1)
                :to-be-less-than
                (car duration2)))))

  (describe "vulpea-db-query-by-links-some"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (links-all (seq-map
                         (lambda (link)
                           (cons "id" link))
                         (org-roam-db-query "select dest from links where \"type\" = '\"id\"' group by dest order by count(1) desc")))
             (links (append (seq-take links-all 2)
                            (last links-all 2)))
             (duration1 (benchmark-run runs
                          (vulpea-db-query-by-links-some links)))
             (duration2 (benchmark-run runs
                          (vulpea-db-query (lambda (note)
                                             (let ((note-links (vulpea-note-links note)))
                                               (seq-some
                                                (lambda (link)
                                                  (seq-contains-p note-links link))
                                                links)))))))
        (message "runs = %s" runs)
        (message "duration specialized = %s" duration1)
        (message "duration generic     = %s" duration2)
        (expect (car duration1)
                :to-be-less-than
                (car duration2)))))

  (describe "vulpea-db-query-by-links-every"
    (it "specialized query is faster than generic vulpea-db-query"
      (let* ((runs 10)
             (links-all (seq-map
                         (lambda (link)
                           (cons "id" link))
                         (org-roam-db-query "select dest from links where \"type\" = '\"id\"' group by dest order by count(1) desc")))
             (links (append (seq-take links-all 2)
                            (last links-all 2)))
             (duration1 (benchmark-run runs
                          (vulpea-db-query-by-links-every links)))
             (duration2 (benchmark-run runs
                          (vulpea-db-query (lambda (note)
                                             (let ((note-links (vulpea-note-links note)))
                                               (seq-every-p
                                                (lambda (link)
                                                  (seq-contains-p note-links link))
                                                links)))))))
        (message "runs = %s" runs)
        (message "duration specialized = %s" duration1)
        (message "duration generic     = %s" duration2)
        (expect (car duration1)
                :to-be-less-than
                (car duration2))))))

(provide 'vulpea-perf-test)
;;; vulpea-perf-test.el ends here
