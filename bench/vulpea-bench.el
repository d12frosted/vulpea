;;; vulpea-bench.el --- Benchmarking infrastructure -*- lexical-binding: t; -*-
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
;; Created: 20 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Benchmarking infrastructure for vulpea performance testing.
;;
;;; Code:

(require 'benchmark)
(require 'emacsql)
(require 'vulpea-db)
(require 'vulpea-db-sync)

(defvar vulpea-bench-output-dir
  (expand-file-name "bench-output" (file-name-directory load-file-name))
  "Directory for benchmark output.")

(defun vulpea-bench--format-time (seconds)
  "Format SECONDS as human-readable time."
  (cond
   ((< seconds 0.001) (format "%.2f μs" (* seconds 1000000)))
   ((< seconds 1) (format "%.2f ms" (* seconds 1000)))
   ((< seconds 60) (format "%.2f s" seconds))
   (t (format "%.2f min" (/ seconds 60.0)))))

(defun vulpea-bench--format-throughput (count seconds)
  "Format throughput from COUNT operations in SECONDS."
  (let ((rate (/ count seconds)))
    (cond
     ((< rate 1) (format "%.2f ops/s" rate))
     ((< rate 1000) (format "%.0f ops/s" rate))
     (t (format "%.2fk ops/s" (/ rate 1000.0))))))

(defmacro vulpea-bench-measure (name &rest body)
  "Measure execution of BODY and report with NAME.
Returns (time-in-seconds . result)."
  (declare (indent 1))
  `(let ((start-time (current-time))
         (gc-cons-threshold most-positive-fixnum)  ; Disable GC during measurement
         (result nil))
     (setq result (progn ,@body))
     (garbage-collect)  ; Clean GC after measurement
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       (message "[%s] %s" ,name (vulpea-bench--format-time elapsed))
       (cons elapsed result))))

(defun vulpea-bench-sync (notes-dir db-file)
  "Benchmark full sync of NOTES-DIR into DB-FILE.
Returns (time count) where time is in seconds and count is notes synced."
  (let ((vulpea-db-location db-file)
        (vulpea-db--connection nil)
        (vulpea-db-autosync-mode nil))  ; Force synchronous mode
    (when (file-exists-p db-file)
      (delete-file db-file))

    (message "\n=== Benchmarking Full Sync ===")
    (message "Notes directory: %s" notes-dir)
    (message "Database: %s" db-file)

    (let* ((file-count (length (directory-files-recursively notes-dir "\\.org\\'")))
           (result (vulpea-bench-measure "Full sync"
                     (vulpea-db)
                     (vulpea-db-sync-update-directory notes-dir)))
           (time (car result)))

      ;; Get total note count from database
      (let ((note-count (caar (emacsql (vulpea-db) [:select (funcall count *) :from notes]))))
        (message "Files found: %d" file-count)
        (message "Notes synced: %d" note-count)
        (message "Throughput: %s" (vulpea-bench--format-throughput file-count time))
        (message "Average: %s per file" (vulpea-bench--format-time (/ time file-count)))

        (when vulpea-db--connection
          (vulpea-db-close))

        (list time note-count)))))

(defun vulpea-bench-query (db-file query-name query-fn)
  "Benchmark QUERY-FN against DB-FILE with QUERY-NAME.
Returns (time result-count)."
  (let ((vulpea-db-location db-file)
        (vulpea-db--connection nil))

    (unless (file-exists-p db-file)
      (user-error "Database not found: %s" db-file))

    (vulpea-db)

    (let ((result (vulpea-bench-measure (format "Query: %s" query-name)
                    (funcall query-fn))))

      (let ((time (car result))
            (notes (cdr result)))
        (message "  Results: %d notes" (length notes))
        (message "  Throughput: %s" (vulpea-bench--format-throughput (length notes) time))

        (when vulpea-db--connection
          (vulpea-db-close))

        (list time (length notes))))))

(defun vulpea-bench-extraction (notes-dir sample-size)
  "Benchmark file extraction on SAMPLE-SIZE random files from NOTES-DIR.
Returns (time file-count)."
  (message "\n=== Benchmarking Extraction ===")

  (let* ((all-files (directory-files notes-dir t "\\.org$"))
         (sample-files (if (<= sample-size (length all-files))
                           (seq-take (seq-sort-by (lambda (_) (random)) #'< all-files)
                                     sample-size)
                         all-files))
         (result (vulpea-bench-measure "File extraction"
                   (dolist (file sample-files)
                     (vulpea-db--parse-file file)))))

    (let ((time (car result))
          (count (length sample-files)))
      (message "Files parsed: %d" count)
      (message "Throughput: %s" (vulpea-bench--format-throughput count time))
      (message "Average: %s per file" (vulpea-bench--format-time (/ time count)))

      (list time count))))

(defun vulpea-bench-org-id-registration (file-count)
  "Benchmark registering FILE-COUNT files with org-id in a fresh session.

Models a session right after `org-id-locations-load': `org-id-files'
already lists FILE-COUNT paths and vulpea's shadow of it is empty.
Then registers one id for each of FILE-COUNT other paths, one call
per file, the way a forced scan does.  Nothing touches the disk or
the database; this isolates the org-id side of indexing.
Returns time in seconds."
  (let* ((org-id-track-globally t)
         (org-id-locations (make-hash-table :test #'equal))
         (org-id-files (mapcar (lambda (i) (format "~/vault/old/note-%06d.org" i))
                               (number-sequence 1 file-count)))
         (vulpea-db--org-id-files-seen (make-hash-table :test #'equal))
         (paths (mapcar (lambda (i)
                          (expand-file-name
                           (format "~/vault/new/note-%06d.org" i)))
                        (number-sequence 1 file-count)))
         (result (vulpea-bench-measure
                     (format "org-id registration: %d files" file-count)
                   (dolist (path paths)
                     (vulpea-db--register-id-locations
                      (list (concat "id-" path)) path))
                   file-count)))
    (message "  Throughput: %s"
             (vulpea-bench--format-throughput file-count (car result)))
    (car result)))

(defun vulpea-bench-org-id-repair (note-count)
  "Benchmark `vulpea-db-register-org-ids' over NOTE-COUNT database rows.

Fills a temporary database with NOTE-COUNT notes (one per file, no
files on disk) and times the pass twice: cold, with an empty org-id
index, so every id is written; and warm, right after, so every id is
found in place and nothing is written.  The warm number is what a
normal start of `vulpea-db-autosync-mode' pays.  Returns (cold . warm)
in seconds."
  (let* ((db-file (make-temp-file "vulpea-bench-" nil ".db"))
         (vulpea-db-location db-file)
         (vulpea-db--connection nil)
         (org-id-track-globally t)
         (org-id-locations (make-hash-table :test #'equal))
         (org-id-files nil)
         (vulpea-db--org-id-files-seen (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (vulpea-db)
          (emacsql-with-transaction (vulpea-db)
            (dotimes (i note-count)
              (emacsql (vulpea-db) [:insert :into notes :values $v1]
                       (vector (format "id-%06d" i)
                               (expand-file-name
                                (format "~/vault/dir%02d/note-%06d.org"
                                        (% i 100) i))
                               0 1 (format "Note %d" i) "{}"
                               nil nil nil nil nil nil nil nil nil nil
                               nil nil nil nil
                               (format-time-string "%FT%T") nil))))
          (let ((cold (car (vulpea-bench-measure
                               (format "org-id repair, cold: %d notes" note-count)
                             (vulpea-db-register-org-ids))))
                (warm (car (vulpea-bench-measure
                               (format "org-id repair, warm: %d notes" note-count)
                             (vulpea-db-register-org-ids)))))
            (cons cold warm)))
      (when vulpea-db--connection (vulpea-db-close))
      (delete-file db-file))))

(defun vulpea-bench-queue-unchanged (notes-dir db-file)
  "Benchmark the sync queue over the unchanged files of NOTES-DIR.

Indexes NOTES-DIR into DB-FILE synchronously, then pushes every file
through `vulpea-db-sync--process-queue' twice: cold, with an empty
org-id index, so each batch registers the ids of the files it skips;
and warm, right after, so each batch finds them in place.  Reports
the total, mean and maximum batch time, which is what the main thread
pays per idle tick during the initial scan.  Extraction stays in this
process.  Returns ((cold-total cold-max) (warm-total warm-max)) in
seconds."
  (let* ((vulpea-db-location db-file)
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list notes-dir))
         (vulpea-db-async-extraction nil)
         (vulpea-db-sync-progress-interval nil)
         (org-id-track-globally t)
         (org-id-locations (make-hash-table :test #'equal))
         (org-id-files nil)
         (vulpea-db--org-id-files-seen (make-hash-table :test #'equal))
         (results nil))
    (when (file-exists-p db-file) (delete-file db-file))
    (vulpea-db)
    (vulpea-bench-measure "Index for queue bench"
      (vulpea-db-sync-update-directory notes-dir))
    (let ((files (vulpea-db-sync--list-org-files notes-dir)))
      (dolist (label '("cold" "warm"))
        (when (equal label "cold")
          (clrhash org-id-locations)
          (setq org-id-files nil)
          (clrhash vulpea-db--org-id-files-seen))
        (let ((vulpea-db-autosync-mode t)
              (vulpea-db-sync--queue nil)
              (vulpea-db-sync--queue-tail nil)
              (vulpea-db-sync--queue-set (make-hash-table :test #'equal))
              (vulpea-db-sync--processed-total 0)
              (batches 0) (total 0.0) (max-batch 0.0))
          (dolist (f files) (vulpea-db-sync--enqueue f))
          (while vulpea-db-sync--queue
            (let ((t0 (current-time)))
              (vulpea-db-sync--process-queue)
              (let ((b (float-time (time-subtract (current-time) t0))))
                (setq batches (1+ batches)
                      total (+ total b)
                      max-batch (max max-batch b)))))
          (message "[queue %s] %d files, %d batches: total %s, mean %s, max %s, org-id has %d ids"
                   label (length files) batches
                   (vulpea-bench--format-time total)
                   (vulpea-bench--format-time (/ total batches))
                   (vulpea-bench--format-time max-batch)
                   (hash-table-count org-id-locations))
          (push (list total max-batch) results))))
    (when vulpea-db--connection (vulpea-db-close))
    (nreverse results)))

(defun vulpea-bench-report (name results)
  "Print formatted benchmark report for NAME with RESULTS.
RESULTS is an alist of (label . (time count)) pairs."
  (message "\n=== Benchmark Report: %s ===" name)
  (message "%-30s %12s %12s %15s" "Operation" "Time" "Count" "Throughput")
  (message "%s" (make-string 70 ?-))

  (dolist (entry results)
    (let* ((label (car entry))
           (time (cadr entry))
           (count (caddr entry)))
      (message "%-30s %12s %12d %15s"
               label
               (vulpea-bench--format-time time)
               count
               (vulpea-bench--format-throughput count time)))))

(provide 'vulpea-bench)
;;; vulpea-bench.el ends here
