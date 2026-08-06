;;; vulpea-timestamp-test.el --- Tests for vulpea-timestamp -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for the `vulpea-timestamp' value type: parsing org timestamp
;; strings and serializing them back.
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vulpea-timestamp)

(defun vulpea-timestamp-test--time (year month day &optional hour minute)
  "Build the local time for YEAR MONTH DAY HOUR MINUTE."
  (encode-time (list 0 (or minute 0) (or hour 0) day month year nil -1 nil)))

;;; Parsing

(ert-deftest vulpea-timestamp-parse-active-date ()
  "An active date timestamp parses with no time part."
  (let ((ts (vulpea-timestamp-parse "<2026-08-05 Wed>")))
    (should (vulpea-timestamp-p ts))
    (should (vulpea-timestamp-active ts))
    (should-not (vulpea-timestamp-with-time ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5)))))

(ert-deftest vulpea-timestamp-parse-inactive-date ()
  "An inactive date timestamp parses as not active."
  (let ((ts (vulpea-timestamp-parse "[2026-08-05 Wed]")))
    (should (vulpea-timestamp-p ts))
    (should-not (vulpea-timestamp-active ts))
    (should-not (vulpea-timestamp-with-time ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5)))))

(ert-deftest vulpea-timestamp-parse-active-datetime ()
  "An active timestamp with a time part parses with `with-time'."
  (let ((ts (vulpea-timestamp-parse "<2026-08-05 Wed 14:30>")))
    (should (vulpea-timestamp-p ts))
    (should (vulpea-timestamp-active ts))
    (should (vulpea-timestamp-with-time ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5 14 30)))))

(ert-deftest vulpea-timestamp-parse-inactive-datetime ()
  "An inactive timestamp with a time part parses fully."
  (let ((ts (vulpea-timestamp-parse "[2026-08-05 Wed 14:30]")))
    (should (vulpea-timestamp-p ts))
    (should-not (vulpea-timestamp-active ts))
    (should (vulpea-timestamp-with-time ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5 14 30)))))

(ert-deftest vulpea-timestamp-parse-without-day-name ()
  "The day name is optional."
  (let ((ts (vulpea-timestamp-parse "<2026-08-05>")))
    (should (vulpea-timestamp-p ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5)))))

(ert-deftest vulpea-timestamp-parse-localized-day-name ()
  "A localized day name is accepted (and never validated)."
  ;; "qua" is Portuguese for Wednesday; "Xyz" is not a day at all -
  ;; the name is treated as opaque text either way.
  (dolist (s '("<2026-08-05 qua>" "<2026-08-05 Xyz>" "[2026-08-05 qua 09:00]"))
    (should (vulpea-timestamp-p (vulpea-timestamp-parse s)))))

(ert-deftest vulpea-timestamp-parse-rejects-non-timestamps ()
  "Free-form date-ish strings are not timestamps."
  (dolist (s '("today" "yesterday" "2026-08-05" "2026-08-05 Wed"
               "" "not a date" "<>" "<2026-08-05 Wed> trailing"
               "leading <2026-08-05 Wed>"))
    (should-not (vulpea-timestamp-parse s))))

(ert-deftest vulpea-timestamp-parse-rejects-impossible-dates ()
  "A well-shaped timestamp with an impossible date is rejected."
  (dolist (s '("<2026-02-30 Mon>" "<2026-13-01 Mon>" "<2026-00-10 Mon>"
               "<2026-04-31 Fri>" "<2026-08-00 Sat>"))
    (should-not (vulpea-timestamp-parse s))))

(ert-deftest vulpea-timestamp-parse-rejects-impossible-times ()
  "A well-shaped timestamp with an impossible time is rejected."
  (dolist (s '("<2026-08-05 Wed 24:00>" "<2026-08-05 Wed 12:60>"))
    (should-not (vulpea-timestamp-parse s))))

(ert-deftest vulpea-timestamp-parse-rejects-mismatched-brackets ()
  "Opening and closing brackets must agree."
  (dolist (s '("<2026-08-05 Wed]" "[2026-08-05 Wed>"))
    (should-not (vulpea-timestamp-parse s))))

(ert-deftest vulpea-timestamp-parse-rejects-ranges-and-repeaters ()
  "Ranges, repeaters, and warning periods are not plain timestamps."
  (dolist (s '("<2026-08-05 Wed>--<2026-08-06 Thu>"
               "<2026-08-05 Wed +1w>"
               "<2026-08-05 Wed .+1w>"
               "<2026-08-05 Wed ++1w>"
               "<2026-08-05 Wed -1d>"
               "<2026-08-05 Wed 10:00-12:00>"))
    (should-not (vulpea-timestamp-parse s))))

(ert-deftest vulpea-timestamp-parse-non-string ()
  "Non-string input yields nil rather than an error."
  (should-not (vulpea-timestamp-parse nil))
  (should-not (vulpea-timestamp-parse 42)))

;;; Construction

(ert-deftest vulpea-timestamp-create-truncates-date-to-midnight ()
  "Without WITH-TIME the time is truncated to midnight."
  (let ((ts (vulpea-timestamp-create
             (vulpea-timestamp-test--time 2026 8 5 14 30) nil t)))
    (should-not (vulpea-timestamp-with-time ts))
    (should (vulpea-timestamp-active ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5)))))

(ert-deftest vulpea-timestamp-create-keeps-time ()
  "With WITH-TIME the time part is kept."
  (let ((ts (vulpea-timestamp-create
             (vulpea-timestamp-test--time 2026 8 5 14 30) t nil)))
    (should (vulpea-timestamp-with-time ts))
    (should-not (vulpea-timestamp-active ts))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-test--time 2026 8 5 14 30)))))

;;; Serialization

(ert-deftest vulpea-timestamp-to-string-round-trips ()
  "parse . to-string = id for canonical timestamps."
  (let ((system-time-locale "C"))
    (dolist (s '("<2026-08-05 Wed>"
                 "[2026-08-05 Wed]"
                 "<2026-08-05 Wed 14:30>"
                 "[2026-08-05 Wed 14:30]"))
      (should (equal (vulpea-timestamp-to-string (vulpea-timestamp-parse s))
                     s)))))

(ert-deftest vulpea-timestamp-to-string-recomputes-day-name ()
  "The day name is recomputed from the date, not preserved."
  (let ((system-time-locale "C"))
    (should (equal (vulpea-timestamp-to-string
                    (vulpea-timestamp-parse "<2026-08-05 qua>"))
                   "<2026-08-05 Wed>"))
    (should (equal (vulpea-timestamp-to-string
                    (vulpea-timestamp-parse "<2026-08-05>"))
                   "<2026-08-05 Wed>"))))

(ert-deftest vulpea-timestamp-parse-to-string-identity ()
  "to-string . parse = id for constructed timestamps."
  (let* ((system-time-locale "C")
         (ts (make-vulpea-timestamp
              :time (vulpea-timestamp-test--time 2026 8 5 14 30)
              :with-time t
              :active nil))
         (ts2 (vulpea-timestamp-parse (vulpea-timestamp-to-string ts))))
    (should (time-equal-p (vulpea-timestamp-time ts)
                          (vulpea-timestamp-time ts2)))
    (should (eq (vulpea-timestamp-with-time ts)
                (vulpea-timestamp-with-time ts2)))
    (should (eq (vulpea-timestamp-active ts)
                (vulpea-timestamp-active ts2)))))

(provide 'vulpea-timestamp-test)
;;; vulpea-timestamp-test.el ends here
