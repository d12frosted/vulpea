;;; vulpea-bench-file.el --- Single large file benchmark -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Created: 04 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Benchmark for indexing a single large org file (issue #359).
;;
;; The many-small-notes benchmarks measure throughput across files;
;; this one measures the latency of updating ONE file as its size
;; grows.  That latency is what a user experiences as a UI freeze:
;; the sync queue is scheduled asynchronously (timers), but the work
;; itself - hashing, org-element parsing, extraction, DB write - runs
;; on the main thread once the timer fires.
;;
;; For each target size it generates a realistic org file (headings,
;; a fraction of them with IDs, id-links, meta) and measures:
;;
;; 1. First index - `vulpea-db-update-file' with phase breakdown
;;    (io, org-mode, AST parse, extract, db) via
;;    `vulpea-db--timing-data'.
;; 2. Save path - `vulpea-db-sync--update-file-if-changed' on a
;;    modified file: hash verification + full re-index.  This is
;;    exactly what runs after a save with autosync enabled, and its
;;    wall time is the UI freeze duration.
;; 3. Unchanged check - same entry point when only mtime changed:
;;    the hash-only cost of confirming there is nothing to do.
;;
;;; Code:

(require 'vulpea-bench)
(require 'vulpea-bench-generate)
(require 'vulpea-db-extract)
(require 'vulpea-db-sync)

(defvar vulpea-bench-file-sizes
  (list (* 100 1024)          ; 100 KB
        (* 1024 1024)         ; 1 MB
        (* 10 1024 1024)      ; 10 MB
        (* 100 1024 1024))    ; 100 MB
  "Default file sizes (bytes) for `vulpea-bench-file-run'.")

(defvar vulpea-bench-file-id-fraction 0.3
  "Fraction of generated headings that carry an ID property.
Headings with IDs become heading-level notes and exercise the
extraction and DB-write paths; headings without IDs still cost
org-element parse time, like in real files.")

(defun vulpea-bench-file--format-size (bytes)
  "Format BYTES as human-readable size."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%dKB" (/ bytes 1024)))
   ((< bytes (* 1024 1024 1024)) (format "%dMB" (/ bytes 1024 1024)))
   (t (format "%dGB" (/ bytes 1024 1024 1024)))))

(defun vulpea-bench-file-generate (path target-bytes)
  "Generate a single org file at PATH of roughly TARGET-BYTES.

The file has a file-level note (ID, title, tags) followed by
top-level headings until the target size is reached.  A fraction
of headings (see `vulpea-bench-file-id-fraction') get an ID and
occasionally meta and an id-link to an earlier heading, so
heading-level extraction and link collection are exercised.

Returns a plist (:path :bytes :headings :ids :links)."
  (let ((file-id (vulpea-bench-uuid))
        (headings 0)
        (ids nil)
        (links 0))
    (with-temp-buffer
      (insert ":PROPERTIES:\n"
              (format ":ID:       %s\n" file-id)
              ":END:\n"
              (format "#+TITLE: %s\n" (vulpea-bench-random-title))
              "#+FILETAGS: :bench:large:\n\n"
              (vulpea-bench-random-words 100)
              "\n\n")
      (while (< (buffer-size) target-bytes)
        (setq headings (1+ headings))
        (insert (format "* %s\n" (vulpea-bench-random-title)))
        (when (< (/ (random 1000) 1000.0) vulpea-bench-file-id-fraction)
          (let ((id (vulpea-bench-uuid)))
            (insert ":PROPERTIES:\n"
                    (format ":ID:       %s\n" id)
                    ":END:\n")
            ;; Some meta for extraction cost realism
            (insert (format "- category :: %s\n"
                            (seq-random-elt vulpea-bench-tags)))
            (push id ids)))
        ;; Occasionally link to an earlier note
        (when (and ids (zerop (random 4)))
          (setq links (1+ links))
          (insert (format "See [[id:%s][%s]].\n\n"
                          (seq-random-elt ids)
                          (vulpea-bench-random-title))))
        (dotimes (_ (1+ (random 3)))
          (insert (vulpea-bench-random-words (+ 40 (random 40))) "\n\n")))
      (write-region (point-min) (point-max) path nil 'silent))
    (list :path path
          :bytes (file-attribute-size (file-attributes path))
          :headings headings
          :ids (length ids)
          :links links)))

(defun vulpea-bench-file--report-phases (total-ms)
  "Report `vulpea-db--timing-data' phases as share of TOTAL-MS."
  (dolist (phase '((parse-io . "io (read file)")
                   (parse-org-mode . "org-mode init")
                   (parse-ast . "AST parse")
                   (parse-extract . "extract+hash")
                   (db . "db write")))
    (let ((ms (or (cdr (assoc (car phase) vulpea-db--timing-data)) 0)))
      (message "    %-16s %10.0f ms  (%2.0f%%)"
               (cdr phase) ms (* 100 (/ ms total-ms))))))

(defun vulpea-bench-file-measure (path)
  "Measure indexing latency for the org file at PATH.

Uses a fresh database.  Returns an alist with wall times in
seconds for `first-index', `save-path' and `unchanged-check'."
  (let* ((db-file (expand-file-name
                   (format "bench-file-%s.db" (file-name-base path))
                   vulpea-bench-output-dir))
         (vulpea-db-location db-file)
         (vulpea-db--connection nil)
         (vulpea-db-autosync-mode nil)
         (results nil))
    (when (file-exists-p db-file)
      (delete-file db-file))
    (unwind-protect
        (progn
          (vulpea-db)

          ;; 1. First index with phase breakdown
          (setq vulpea-db--timing-data '((parse . 0) (db . 0)))
          (let ((r (vulpea-bench-measure "first index (vulpea-db-update-file)"
                     (vulpea-db-update-file path))))
            (push (cons 'first-index (car r)) results)
            (vulpea-bench-file--report-phases (* 1000 (car r)))
            (message "    notes indexed: %d" (cdr r)))
          (setq vulpea-db--timing-data nil)

          ;; 2. Save path: content changed -> hash + full re-index.
          ;; This is what autosync runs after a save; its wall time
          ;; is the UI freeze duration from issue #359.
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-max))
            (insert "\nedited after save\n")
            (write-region (point-min) (point-max) path nil 'silent))
          (let ((r (vulpea-bench-measure "save path (hash + re-index)"
                     (vulpea-db-sync--update-file-if-changed path))))
            (push (cons 'save-path (car r)) results))

          ;; 3. Unchanged: mtime differs, content identical -> hash only
          (set-file-times path (time-add (current-time) 1))
          (let ((r (vulpea-bench-measure "unchanged check (hash only)"
                     (vulpea-db-sync--update-file-if-changed path))))
            (push (cons 'unchanged-check (car r)) results)))
      (when vulpea-db--connection
        (vulpea-db-close))
      (when (file-exists-p db-file)
        (delete-file db-file)))
    (nreverse results)))

(defun vulpea-bench-file-run (&optional sizes)
  "Run the single-file benchmark for each size in SIZES.

SIZES is a list of byte counts, defaulting to
`vulpea-bench-file-sizes'.  Prints per-size phase breakdowns and
a final summary table."
  (let ((sizes (or sizes vulpea-bench-file-sizes))
        (summary nil))
    (unless (file-directory-p vulpea-bench-output-dir)
      (make-directory vulpea-bench-output-dir t))
    ;; Warmup: index a small file once so one-time costs (org-id
    ;; loading, first org-mode init, autoloads) don't skew the first
    ;; measured size.
    (let ((path (expand-file-name "bench-file-warmup.org"
                                  vulpea-bench-output-dir)))
      (message "\n=== Warmup (1KB, not counted) ===")
      (vulpea-bench-file-generate path 1024)
      (vulpea-bench-file-measure path)
      (delete-file path))
    (dolist (size sizes)
      (let* ((label (vulpea-bench-file--format-size size))
             (path (expand-file-name (format "bench-file-%s.org" label)
                                     vulpea-bench-output-dir)))
        (message "\n=== File size: %s ===" label)
        (let ((info (vulpea-bench-file-generate path size)))
          (message "Generated %s: %d headings, %d ids, %d links"
                   (vulpea-bench-file--format-size (plist-get info :bytes))
                   (plist-get info :headings)
                   (plist-get info :ids)
                   (plist-get info :links))
          (let ((r (vulpea-bench-file-measure path)))
            (push (append (list :label label
                                :bytes (plist-get info :bytes)
                                :notes (1+ (plist-get info :ids)))
                          r)
                  summary)))
        (delete-file path)))
    ;; Summary table
    (message "\n%-8s %12s %12s %12s %8s"
             "size" "first-index" "save-path" "unchanged" "notes")
    (message "%s" (make-string 58 ?-))
    (dolist (row (nreverse summary))
      (message "%-8s %12s %12s %12s %8d"
               (plist-get row :label)
               (vulpea-bench--format-time (cdr (assq 'first-index row)))
               (vulpea-bench--format-time (cdr (assq 'save-path row)))
               (vulpea-bench--format-time (cdr (assq 'unchanged-check row)))
               (plist-get row :notes)))
    summary))

(provide 'vulpea-bench-file)
;;; vulpea-bench-file.el ends here
