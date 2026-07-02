;;; vulpea-bench-schema.el --- Benchmark schema validation -*- lexical-binding: t; -*-
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
;; Created: 02 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Benchmark for the schema validation pipeline (`vulpea-schema').
;;
;; This measures how long `vulpea-schema-collection-health' takes to
;; validate a collection of notes, and nothing else.  The notes are
;; fabricated in memory with `make-vulpea-note'; there is no database, no
;; generator, and nothing is written to disk.  Fetching notes from the DB
;; is a separate concern already covered by `vulpea-bench-query', and the
;; schema dashboard is a vulpea-ui concern benchmarked there.
;;
;; Reuses the shared bench helpers: `vulpea-bench-measure' for timing and
;; `vulpea-bench-uuid' for ids.
;;
;; Run it with:
;;
;;   eldev -dtT exec "(progn \
;;     (add-to-list 'load-path (expand-file-name \"bench\")) \
;;     (require 'vulpea-bench-schema) \
;;     (vulpea-bench-schema-run))"
;;
;; See bench/README.md ("Schema Pipeline Benchmarks").
;;
;;; Code:

(require 'cl-lib)
(require 'vulpea-note)
(require 'vulpea-schema)
(require 'vulpea-bench)               ; for `vulpea-bench-measure'
(require 'vulpea-bench-generate)      ; for `vulpea-bench-uuid'

;;; Schema under test

(defun vulpea-bench-schema-register ()
  "Register the wine schema used by the benchmark.

Clears any previously registered schemas first so a run is
deterministic regardless of what a config might have loaded.

The schema is intentionally DB-free: every field validates against the
note in hand (plain strings, a number, a `:one-of' symbol) with no
`:type' note / `:target-tags' fields, so validation issues no DB
lookups.  Reference-heavy schemas (note/link targets) are a separate
performance regime and are not what this benchmark measures."
  (dolist (n (vulpea-schema-list))
    (vulpea-schema-unregister n))
  (vulpea-schema-define 'wine
    :predicate (lambda (note) (member "wine" (vulpea-note-tags note)))
    :fields '((:key "name"      :type string :required t)
              (:key "producer"  :type string :required t)
              (:key "region"    :type string :required t)
              (:key "vintage"   :type number)
              (:key "colour"    :type symbol :required t
                    :one-of (red white rose orange))
              (:key "sweetness" :type string)
              (:key "rating"    :type string))))

;;; In-memory note fabrication
;;
;; The meta shape matters: `vulpea-note' meta is an alist
;; (KEY . (VALUE1 VALUE2 ...)) of bare strings, which is what
;; `vulpea-note-meta-get-list' (and so the schema) reads.

(defconst vulpea-bench-schema--regions
  ["Bordeaux" "Burgundy" "Champagne" "Rioja" "Tuscany" "Mosel"
   "Napa Valley" "Barossa" "Douro" "Loire"]
  "A handful of regions to vary the fabricated notes.")

(defconst vulpea-bench-schema--producers
  ["Chateau Margaux" "Domaine Leroy" "Krug" "Vega Sicilia" "Gaja"
   "Egon Muller" "Screaming Eagle" "Penfolds" "Niepoort" "Huet"]
  "A handful of producers to vary the fabricated notes.")

(defconst vulpea-bench-schema--colours ["red" "white" "rose" "orange"]
  "Valid colours (the schema's :one-of).")

(defun vulpea-bench-schema--make-note (i invalid)
  "Fabricate wine note number I in memory.

When INVALID is non-nil the note drops the required \"producer\" field
and sets \"colour\" to a disallowed value, so it fails validation with a
missing-required violation and a not-one-of violation.

The note carries the wine fields plus a little extra (a couple more meta
keys and an alias) so the timing is representative of real notes rather
than a bare floor."
  (let* ((region (aref vulpea-bench-schema--regions
                       (% i (length vulpea-bench-schema--regions))))
         (producer (aref vulpea-bench-schema--producers
                         (% i (length vulpea-bench-schema--producers))))
         (vintage (+ 1990 (% i 35)))
         (colour (if invalid "chartreuse"
                   (aref vulpea-bench-schema--colours
                         (% i (length vulpea-bench-schema--colours)))))
         (title (format "Cuvee %d" i))
         (meta (append
                (list (cons "name" (list title))
                      (cons "region" (list region)))
                (unless invalid
                  (list (cons "producer" (list producer))))
                (list (cons "vintage" (list (number-to-string vintage)))
                      (cons "colour" (list colour))
                      (cons "rating" (list (number-to-string (+ 80 (% i 20)))))
                      ;; A couple of extra keys beyond the schema fields so
                      ;; the meta alist is not trivially short.
                      (cons "country" (list "France"))
                      (cons "cellar" (list (format "rack-%d" (% i 50))))))))
    (make-vulpea-note
     :id (vulpea-bench-uuid)
     :title title
     :tags '("wine")
     :aliases (list (format "Cuvee No. %d" i))
     :meta meta)))

(defun vulpea-bench-schema-fabricate (count invalid-fraction)
  "Fabricate COUNT wine notes in memory.

INVALID-FRACTION of them (every Nth) carry violations.  Returns the list
of `vulpea-note' structs."
  (let ((invalid-every (if (> invalid-fraction 0)
                           (max 1 (round (/ 1.0 invalid-fraction)))
                         0))
        notes)
    (dotimes (i count)
      (push (vulpea-bench-schema--make-note
             i (and (> invalid-every 0) (zerop (% i invalid-every))))
            notes))
    (nreverse notes)))

;;; Driver

(defun vulpea-bench-schema-run (&optional scales invalid-fraction)
  "Benchmark schema validation across SCALES (note counts).

SCALES defaults to (1000 10000 100000).  INVALID-FRACTION defaults to
0.2 (20% of notes carry violations).  For each scale it fabricates the
notes in memory, does one warmup validation, then times
`vulpea-schema-collection-health' with `vulpea-bench-measure' and prints
a summary line (scale, time, invalid count).  Nothing is written to
disk."
  (let ((scales (or scales '(1000 10000 100000)))
        (frac (or invalid-fraction 0.2)))
    (vulpea-bench-schema-register)
    (message "\n=== Benchmarking Schema Validation ===")
    (message "Schema: wine (7 fields, DB-free)")
    (message "Invalid fraction: %.2f" frac)
    (message "%-10s %14s %12s" "Scale" "Time" "Invalid")
    (message "%s" (make-string 38 ?-))
    (dolist (scale scales)
      (let ((notes (vulpea-bench-schema-fabricate scale frac)))
        ;; Warmup (result discarded) so the timed call is not paying
        ;; first-touch costs.
        (vulpea-schema-collection-health notes)
        (let* ((result (vulpea-bench-measure (format "schema-validate %d" scale)
                         (vulpea-schema-collection-health notes)))
               (time (car result))
               (health (car (cdr result)))
               (invalid (if health (vulpea-schema-health-invalid health) 0)))
          (message "%-10d %14s %12d"
                   scale
                   (vulpea-bench--format-time time)
                   invalid))
        (setq notes nil)
        (garbage-collect)))
    (message "\nSchema validation benchmark done")))

(provide 'vulpea-bench-schema)
;;; vulpea-bench-schema.el ends here
