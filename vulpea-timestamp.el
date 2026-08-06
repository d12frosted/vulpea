;;; vulpea-timestamp.el --- Org timestamp value type -*- lexical-binding: t; -*-
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
;; This file is not part of GNU Emacs.
;;
;; Created: 06 Aug 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; `vulpea-timestamp' is the value type behind the `date' and
;; `datetime' metadata types.  It represents a plain org timestamp -
;; active "<2026-08-05 Wed>" or inactive "[2026-08-05 Wed]", with or
;; without a time part - and round-trips exactly: parsing a timestamp
;; and serializing it back reproduces the original string (modulo two
;; normalizations: the day name is recomputed from the date rather
;; than trusted, so a localized or wrong name is normalized rather
;; than preserved, and a single-digit hour is zero-padded).
;;
;; Ranges, repeaters, and warning periods are not plain timestamps and
;; do not parse.
;;
;;; Code:

(require 'cl-lib)
(require 'time-date)

(cl-defstruct (vulpea-timestamp (:copier nil))
  "A plain org timestamp.

Slots:
  time      - the moment as an Emacs time value (midnight when the
              timestamp carries no time part)
  with-time - non-nil when the timestamp carries a time part
  active    - non-nil for an active timestamp (angle brackets)"
  time with-time active)

(defconst vulpea-timestamp--regexp
  (concat "\\`\\([<[]\\)"
          "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
          ;; optional day name: opaque text, never validated
          "\\(?: [[:alpha:]][[:alpha:].]*\\)?"
          ;; optional time part
          "\\(?: \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?"
          "\\([]>]\\)\\'")
  "Regexp matching a plain org timestamp, active or inactive.")

(defun vulpea-timestamp-create (time with-time active)
  "Build a `vulpea-timestamp' for TIME, an Emacs time value.

WITH-TIME non-nil keeps TIME's time part; otherwise TIME is truncated
to midnight, keeping the slot invariant that a date carries no time.
ACTIVE non-nil makes the timestamp active."
  (let ((time (if with-time time
                (let ((d (decode-time time)))
                  (encode-time (list 0 0 0
                                     (decoded-time-day d)
                                     (decoded-time-month d)
                                     (decoded-time-year d)
                                     nil -1 nil))))))
    (make-vulpea-timestamp :time time :with-time with-time :active active)))

(defun vulpea-timestamp-parse (value)
  "Parse VALUE, an org timestamp string, into a `vulpea-timestamp'.

Accepts a plain active or inactive timestamp - \"<2026-08-05 Wed>\",
\"[2026-08-05 Wed 14:30]\" - with an optional day name (treated as
opaque text) and an optional HH:MM time part.  Returns nil when VALUE
is not a string, is not shaped like a timestamp, mixes bracket kinds,
carries a range, repeater, or warning period, or names an impossible
date or time."
  (when (and (stringp value)
             (string-match vulpea-timestamp--regexp value))
    (let* ((open (match-string 1 value))
           (close (match-string 7 value))
           (year (string-to-number (match-string 2 value)))
           (month (string-to-number (match-string 3 value)))
           (day (string-to-number (match-string 4 value)))
           (with-time (and (match-string 5 value) t))
           (hour (if with-time (string-to-number (match-string 5 value)) 0))
           (minute (if with-time (string-to-number (match-string 6 value)) 0)))
      (when (and (equal close (if (equal open "<") ">" "]"))
                 (<= 1 month 12)
                 (<= 1 day 31)
                 (<= 0 hour 23)
                 (<= 0 minute 59))
        (let* ((time (encode-time (list 0 minute hour day month year nil -1 nil)))
               (decoded (decode-time time)))
          ;; `encode-time' normalizes impossible dates (Feb 30 becomes
          ;; Mar 2); a changed day or month means the date never existed
          (when (and (= day (decoded-time-day decoded))
                     (= month (decoded-time-month decoded)))
            (make-vulpea-timestamp
             :time time
             :with-time with-time
             :active (equal open "<"))))))))

(defun vulpea-timestamp-to-string (timestamp)
  "Serialize TIMESTAMP, a `vulpea-timestamp', to an org timestamp string.

Activeness and the presence of a time part are preserved; the day name
is computed from the date (in the current locale) and the time is
zero-padded."
  (format-time-string
   (concat (if (vulpea-timestamp-active timestamp) "<" "[")
           "%Y-%m-%d %a"
           (when (vulpea-timestamp-with-time timestamp) " %H:%M")
           (if (vulpea-timestamp-active timestamp) ">" "]"))
   (vulpea-timestamp-time timestamp)))

(provide 'vulpea-timestamp)
;;; vulpea-timestamp.el ends here
