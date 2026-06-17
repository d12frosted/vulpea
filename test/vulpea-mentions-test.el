;;; vulpea-mentions-test.el --- Tests for vulpea-mentions -*- lexical-binding: t; -*-
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
;; Tests for the unlinked-mention detection in `vulpea-mentions'.
;;
;;; Code:

(require 'ert)
(require 'vulpea-mentions)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-test-helpers)

;;; Pure helpers

(ert-deftest vulpea-mentions--note-terms-title-and-aliases ()
  "Terms are the title and aliases, trimmed and de-duplicated."
  (let ((note (make-vulpea-note :title "Cabernet Sauvignon"
                                :aliases '("Cab Sauv" "cabernet sauvignon"))))
    ;; the duplicate alias (case-insensitive) is dropped
    (should (equal (vulpea-mentions--note-terms note)
                   '("Cabernet Sauvignon" "Cab Sauv")))))

(ert-deftest vulpea-mentions--note-terms-drops-short ()
  "Terms shorter than the minimum length are dropped."
  (let ((vulpea-mentions-min-term-length 3)
        (note (make-vulpea-note :title "Ok" :aliases '("Fine"))))
    (should (equal (vulpea-mentions--note-terms note) '("Fine")))))

(ert-deftest vulpea-mentions--parse-rg-json-extracts-matches ()
  "Match events are parsed; other events and junk are ignored."
  (let ((output (concat
                 "{\"type\":\"begin\",\"data\":{\"path\":{\"text\":\"/n/a.org\"}}}\n"
                 "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/a.org\"},"
                 "\"lines\":{\"text\":\"see Cabernet here\\n\"},\"line_number\":7,"
                 "\"submatches\":[{\"match\":{\"text\":\"Cabernet\"},\"start\":4,\"end\":12}]}}\n"
                 "not json\n")))
    (let ((hits (vulpea-mentions--parse-rg-json output)))
      (should (= (length hits) 1))
      (should (equal (plist-get (car hits) :path) "/n/a.org"))
      (should (equal (plist-get (car hits) :line) 7))
      (should (equal (plist-get (car hits) :line-text) "see Cabernet here")))))

(ert-deftest vulpea-mentions--link-spans-and-in-link-p ()
  "Link spans are found and positions tested against them."
  (let* ((line "[[id:abc][Cabernet]] and bare Cabernet")
         (spans (vulpea-mentions--link-spans line)))
    (should (= (length spans) 1))
    ;; "Cabernet" inside the link (around column 10) is in a span
    (should (vulpea-mentions--in-link-p 10 spans))
    ;; the bare "Cabernet" near the end is not
    (should-not (vulpea-mentions--in-link-p (string-match "bare" line) spans))))

(ert-deftest vulpea-mentions--line-unlinked-p ()
  "A line counts only when a term occurs outside any link."
  (let ((terms '("Cabernet")))
    ;; bare mention -> unlinked
    (should (vulpea-mentions--line-unlinked-p "a lovely Cabernet today" terms))
    ;; only inside a link -> not unlinked
    (should-not (vulpea-mentions--line-unlinked-p "see [[id:x][Cabernet]]" terms))
    ;; word boundary: "Cabernets" should not match "Cabernet"
    (should-not (vulpea-mentions--line-unlinked-p "many Cabernets" terms))
    ;; no term at all
    (should-not (vulpea-mentions--line-unlinked-p "nothing here" terms))))

(ert-deftest vulpea-mentions--rg-command-shape ()
  "The ripgrep command carries the fixed-string, word, org-glob flags."
  (let ((cmd (vulpea-mentions--rg-command "rg" '("A" "B") '("/dir"))))
    (should (equal (car cmd) "rg"))
    (should (member "--fixed-strings" cmd))
    (should (member "--word-regexp" cmd))
    (should (member "--json" cmd))
    (should (member "/dir" cmd))
    ;; each term passed via -e
    (should (equal (seq-filter (lambda (x) (member x '("A" "B"))) cmd) '("A" "B")))))

;;; Collection (DB-backed)

(ert-deftest vulpea-mentions--collect-maps-and-filters ()
  "Collect maps hits to notes, skips own file and already-linked text."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Cabernet" :path "/n/target.org")
    (vulpea-test--insert-test-note "mentioner" "Tasting" :path "/n/tasting.org")
    (let* ((note (vulpea-db-get-by-id "target"))
           (own (expand-file-name "/n/target.org"))
           (output (concat
                    ;; a genuine unlinked mention in tasting.org
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/tasting.org\"},"
                    "\"lines\":{\"text\":\"a lovely Cabernet\\n\"},\"line_number\":3}}\n"
                    ;; an already-linked mention -> excluded
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/tasting.org\"},"
                    "\"lines\":{\"text\":\"see [[id:target][Cabernet]]\\n\"},\"line_number\":4}}\n"
                    ;; a hit in the note's own file -> excluded
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/target.org\"},"
                    "\"lines\":{\"text\":\"Cabernet is me\\n\"},\"line_number\":1}}\n"))
           (mentions (vulpea-mentions--collect output note own)))
      (should (= (length mentions) 1))
      (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "mentioner"))
      (should (equal (plist-get (car mentions) :line) 3))
      (should (equal (plist-get (car mentions) :context) "a lovely Cabernet")))))

;;; Integration with real ripgrep

;; The full subprocess pipeline is exercised by running ripgrep
;; synchronously (via `call-process') and feeding its real output to
;; `vulpea-mentions--collect'.  This is deterministic in batch mode,
;; unlike waiting on an async sentinel, which is timing-sensitive across
;; Emacs versions.  The async wiring's error path has its own test below.

(ert-deftest vulpea-mentions-collect-with-real-rg ()
  "Real ripgrep finds the bare mention; the linked and own-file hits are excluded."
  (skip-unless (executable-find "rg"))
  (let* ((dir (make-temp-file "vulpea-mentions-" t))
         (target (expand-file-name "target.org" dir))
         (other (expand-file-name "other.org" dir))
         (vulpea-db-location (make-temp-file "vulpea-mentions-" nil ".db"))
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list dir)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n"))
          (with-temp-file other
            (insert ":PROPERTIES:\n:ID: other\n:END:\n#+title: Other\n\n"
                    "A bare Cabernet mention.\n"
                    "A linked [[id:target][Cabernet]] mention.\n"))
          (vulpea-db)
          (vulpea-db-update-file target)
          (vulpea-db-update-file other)
          (let* ((note (vulpea-db-get-by-id "target"))
                 (cmd (vulpea-mentions--rg-command
                       (executable-find "rg")
                       (vulpea-mentions--note-terms note)
                       (list dir)))
                 (output (with-temp-buffer
                           (apply #'call-process (car cmd) nil t nil (cdr cmd))
                           (buffer-string)))
                 (mentions (vulpea-mentions--collect
                            output note (expand-file-name target))))
            (should (= (length mentions) 1))
            (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "other"))
            (should (string-match-p "bare Cabernet"
                                    (plist-get (car mentions) :context)))))
      (when vulpea-db--connection (vulpea-db-close))
      (when (file-exists-p vulpea-db-location) (delete-file vulpea-db-location))
      (delete-directory dir t))))

(ert-deftest vulpea-mentions-async-rejects-without-rg ()
  "When ripgrep is unavailable, REJECT is called."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (let ((rejected nil))
      (vulpea-note-unlinked-mentions-async
       (make-vulpea-note :title "X" :path "/n/x.org")
       (lambda (_ms) (setq rejected 'resolved))
       (lambda (_e) (setq rejected t)))
      (should (eq rejected t)))))

(provide 'vulpea-mentions-test)
;;; vulpea-mentions-test.el ends here
