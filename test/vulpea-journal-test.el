;;; vulpea-journal-test.el --- Tests for vulpea-journal -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Boris Buliga <boris@d12frosted.io>
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
;;; Commentary:
;;
;; Tests for vulpea-journal.
;;
;;; Code:

(require 'ert)
(require 'vulpea)
(require 'vulpea-db)
(require 'vulpea-journal)
(require 'vulpea-journal-widget)
(require 'vulpea-journal-widgets)

;; Declare variables for dynamic binding in lexical-binding mode
(defvar vulpea-directory)
(defvar vulpea-db-location)
(defvar vulpea-db-sync-directories)
(defvar vulpea-journal-default-template)
(defvar vulpea-journal-widget-previous-years-handle-leap-year)

;;; Test Infrastructure

(defmacro vulpea-test--with-temp-db (&rest body)
  "Execute BODY with temporary database."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "vulpea-test-" nil ".db"))
          (temp-dir (make-temp-file "vulpea-test-notes-" t))
          (vulpea-db-location temp-file)
          (vulpea-directory temp-dir)
          (vulpea-db-sync-directories (list temp-dir)))
     (unwind-protect
         (progn
           (vulpea-db)  ;; Initialize database
           ,@body)
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

;;; Journal File Path Tests

(ert-deftest vulpea-journal-file-path-relative ()
  "Test file path generation with relative directory."
  (let ((vulpea-directory "/test/notes/")
        (vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal")))
        (date (encode-time 0 0 12 25 11 2024)))
    (should (string= (vulpea-journal--file-for-date date)
                     "/test/notes/journal/20241125.org"))))

(ert-deftest vulpea-journal-file-path-nested ()
  "Test file path with nested directory structure."
  (let ((vulpea-directory "/notes/")
        (vulpea-journal-default-template '(:file-name "journal/%Y/%m/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal")))
        (date (encode-time 0 0 12 25 11 2024)))
    (should (string= (vulpea-journal--file-for-date date)
                     "/notes/journal/2024/11/20241125.org"))))

;;; Note Identification Tests

(ert-deftest vulpea-journal-note-p-true ()
  "Test journal note identification."
  (let ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal"))))
    (should (vulpea-journal-note-p
             (make-vulpea-note :id "test" :tags '("journal" "daily"))))))

(ert-deftest vulpea-journal-note-p-false ()
  "Test non-journal note identification."
  (let ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal"))))
    (should-not (vulpea-journal-note-p
                 (make-vulpea-note :id "test" :tags '("project" "work"))))))

(ert-deftest vulpea-journal-note-p-nil ()
  "Test nil note identification."
  (should-not (vulpea-journal-note-p nil)))

;;; Title Generation Tests

(ert-deftest vulpea-journal-title-for-date ()
  "Test title generation for date."
  (let ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal")))
        (date (encode-time 0 0 12 25 11 2024)))
    (should (string= (vulpea-journal--title-for-date date)
                     "2024-11-25 Monday"))))

;;; Date Extraction Tests

(ert-deftest vulpea-journal-date-from-note ()
  "Test date extraction from journal note."
  (let ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal")))
        (note (make-vulpea-note
               :id "test"
               :path "/notes/journal/20241125.org"
               :tags '("journal"))))
    (let ((date (vulpea-journal--date-from-note note)))
      (should date)
      (let ((decoded (decode-time date)))
        (should (= (decoded-time-year decoded) 2024))
        (should (= (decoded-time-month decoded) 11))
        (should (= (decoded-time-day decoded) 25))))))

(ert-deftest vulpea-journal-date-from-note-not-journal ()
  "Test date extraction from non-journal note."
  (let ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                           :title "%Y-%m-%d %A"
                                           :tags ("journal")))
        (note (make-vulpea-note
               :id "test"
               :path "/notes/project.org"
               :tags '("project"))))
    (should-not (vulpea-journal--date-from-note note))))

;;; Widget Definition Tests

(ert-deftest vulpea-journal-define-widget-basic ()
  "Test widget definition macro."
  (vulpea-journal-define-widget test-widget
    :title "Test Widget"
    :order 100
    :query (lambda (_date) '("item1" "item2"))
    :render (lambda (item) (format "- %s" item)))
  (let ((widget (vulpea-journal-get-widget 'test-widget)))
    (should widget)
    (should (string= (vulpea-journal-widget-title widget) "Test Widget"))
    (should (= (vulpea-journal-widget-order widget) 100))
    (should (vulpea-journal-widget-collapsible widget))))

(ert-deftest vulpea-journal-define-widget-defaults ()
  "Test widget definition with defaults."
  (vulpea-journal-define-widget test-widget-defaults
    :title "Defaults Test"
    :query #'ignore
    :render #'identity)
  (let ((widget (vulpea-journal-get-widget 'test-widget-defaults)))
    (should widget)
    (should (= (vulpea-journal-widget-order widget) 50))
    (should (vulpea-journal-widget-collapsible widget))
    (should-not (vulpea-journal-widget-default-collapsed widget))))

;;; Widget State Tests

(ert-deftest vulpea-journal-widget-collapsed-state ()
  "Test widget collapsed state tracking."
  (vulpea-journal-define-widget state-test-widget
    :title "State Test"
    :default-collapsed t
    :query #'ignore
    :render #'identity)
  (with-temp-buffer
    (setq-local vulpea-journal--widget-states nil)
    ;; Default state
    (should (vulpea-journal--widget-collapsed-p 'state-test-widget))
    ;; Change state
    (vulpea-journal--set-widget-collapsed 'state-test-widget nil)
    (should-not (vulpea-journal--widget-collapsed-p 'state-test-widget))
    ;; Change back
    (vulpea-journal--set-widget-collapsed 'state-test-widget t)
    (should (vulpea-journal--widget-collapsed-p 'state-test-widget))))

;;; Built-in Widget Query Tests

(ert-deftest vulpea-journal-widget-calendar-query ()
  "Test calendar widget query structure."
  (let ((date (encode-time 0 0 12 25 11 2024)))
    (let* ((result (vulpea-journal-widget-calendar-query date))
           (data (car result)))  ; query returns single-item list
      (should (= (length result) 1))
      (should (plist-get data :month))
      (should (plist-get data :year))
      (should (= (plist-get data :month) 11))
      (should (= (plist-get data :year) 2024)))))

;;; Leap Year Tests

(ert-deftest vulpea-journal-anniversary-regular ()
  "Test anniversary dates for regular day."
  (let ((vulpea-journal-widget-previous-years-handle-leap-year t))
    (let ((dates (vulpea-journal--dates-for-anniversary 6 15 2023)))
      (should (= (length dates) 1)))))

(ert-deftest vulpea-journal-anniversary-march-1-leap ()
  "Test March 1 includes Feb 29 in leap year."
  (let ((vulpea-journal-widget-previous-years-handle-leap-year t))
    (let ((dates (vulpea-journal--dates-for-anniversary 3 1 2024)))
      ;; Should include both Feb 29 and Mar 1 for leap year
      (should (= (length dates) 2)))))

(ert-deftest vulpea-journal-anniversary-feb-29-non-leap ()
  "Test Feb 29 includes Feb 28 in non-leap year."
  (let ((vulpea-journal-widget-previous-years-handle-leap-year t))
    (let ((dates (vulpea-journal--dates-for-anniversary 2 29 2023)))
      ;; Should fallback to Feb 28 in non-leap year
      (should (= (length dates) 1))
      (let* ((decoded (decode-time (car dates))))
        (should (= (decoded-time-day decoded) 28))))))

;;; Full Integration Test

(ert-deftest vulpea-journal-create-note ()
  "Test creating a journal note."
  (vulpea-test--with-temp-db
    (let* ((vulpea-journal-default-template '(:file-name "journal/%Y%m%d.org"
                                              :title "%Y-%m-%d %A"
                                              :tags ("journal")))
           (date (encode-time 0 0 12 25 11 2024)))
      ;; Create note (directory is created automatically)
      (let ((note (vulpea-journal-note date)))
        (should note)
        (should (vulpea-note-id note))
        (should (vulpea-journal-note-p note))
        ;; Should find same note again
        (let ((found (vulpea-journal-find-note date)))
          (should found)
          (should (string= (vulpea-note-id found) (vulpea-note-id note))))))))

(provide 'vulpea-journal-test)
;;; vulpea-journal-test.el ends here
