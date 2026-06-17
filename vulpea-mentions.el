;;; vulpea-mentions.el --- Unlinked mention detection -*- lexical-binding: t; -*-
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
;; Created: 17 Jun 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Unlinked mention detection.
;;
;; An unlinked mention of a note is a place where the note's title or one
;; of its aliases appears as plain text in another note, without an
;; `id:' link pointing to it.  This module answers the "incoming"
;; question: which notes mention this one without linking?
;;
;; The scan is delegated to ripgrep (fast multi-pattern search over all
;; files) and the result is post-filtered in Emacs: occurrences already
;; inside an Org link are dropped, the note's own file is skipped, and
;; each hit is mapped back to the mentioning note.
;;
;; The entry point is asynchronous and follows a promise-style contract
;; (RESOLVE / REJECT callbacks), so it drops directly into reactive UIs -
;; e.g. as a `vui-use-async' loader via
;; `(apply-partially #\\='vulpea-note-unlinked-mentions-async note)'.
;;
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'json)
(require 'ol)
(require 'vulpea-note)
(require 'vulpea-db-query)

(defvar vulpea-db-sync-directories)     ; defined in vulpea-db-sync

(defgroup vulpea-mentions nil
  "Unlinked mention detection for Vulpea."
  :group 'vulpea)

(defcustom vulpea-mentions-min-term-length 3
  "Minimum length of a title or alias to search for as a mention.

Very short titles (e.g. \"a\") produce too much noise, so titles and
aliases shorter than this are ignored when looking for unlinked
mentions."
  :type 'integer
  :group 'vulpea-mentions)

;;; Pure helpers

(defun vulpea-mentions--note-terms (note)
  "Return the search terms for NOTE: its title and aliases.

Terms are trimmed, de-duplicated case-insensitively, and those shorter
than `vulpea-mentions-min-term-length' are dropped."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (term (cons (vulpea-note-title note)
                        (vulpea-note-aliases note)))
      (let ((trimmed (and term (string-trim term))))
        (when (and trimmed
                   (>= (length trimmed) vulpea-mentions-min-term-length))
          (let ((key (downcase trimmed)))
            (unless (gethash key seen)
              (puthash key t seen)
              (push trimmed result))))))
    (nreverse result)))

(defun vulpea-mentions--rg-command (rg terms dirs)
  "Build the ripgrep command, run as RG, for TERMS over DIRS.

Produces a JSON stream of fixed-string, case-insensitive,
word-boundary matches restricted to Org files."
  (append
   (list rg "--json" "--fixed-strings" "--ignore-case" "--word-regexp"
         "--glob" "*.org")
   (mapcan (lambda (term) (list "-e" term)) terms)
   dirs))

(defun vulpea-mentions--parse-rg-json (output)
  "Parse ripgrep --json OUTPUT into a list of raw hit plists.

Each hit is a plist with :path, :line, and :line-text.  Non-match
events and unparseable lines are ignored."
  (let ((result nil))
    (dolist (line (split-string output "\n" t))
      (let ((obj (ignore-errors (json-parse-string line :object-type 'alist))))
        (when (and obj (equal (alist-get 'type obj) "match"))
          (let* ((data (alist-get 'data obj))
                 (path (alist-get 'text (alist-get 'path data)))
                 (line-no (alist-get 'line_number data))
                 (text (alist-get 'text (alist-get 'lines data))))
            (when (and path text)
              (push (list :path path
                          :line line-no
                          :line-text (string-trim-right text "[\n\r]+"))
                    result))))))
    (nreverse result)))

(defun vulpea-mentions--link-spans (line)
  "Return the (BEG . END) character spans of Org bracket links in LINE."
  (let ((spans nil)
        (start 0))
    (while (string-match org-link-bracket-re line start)
      (push (cons (match-beginning 0) (match-end 0)) spans)
      (setq start (match-end 0)))
    (nreverse spans)))

(defun vulpea-mentions--in-link-p (pos spans)
  "Return non-nil when POS falls inside one of SPANS."
  (seq-some (lambda (span) (and (>= pos (car span)) (< pos (cdr span))))
            spans))

(defun vulpea-mentions--line-unlinked-p (line terms)
  "Return non-nil when some term in TERMS occurs in LINE outside any link.

Matching is case-insensitive and at word boundaries, mirroring the
ripgrep pre-filter; occurrences inside an Org link are not counted."
  (let ((spans (vulpea-mentions--link-spans line))
        (case-fold-search t))
    (catch 'found
      (dolist (term terms)
        (let ((re (concat "\\b" (regexp-quote term) "\\b"))
              (start 0))
          (while (string-match re line start)
            (setq start (match-end 0))
            (unless (vulpea-mentions--in-link-p (match-beginning 0) spans)
              (throw 'found t)))))
      nil)))

(defun vulpea-mentions--file-note (path cache)
  "Return a note representing file PATH, memoized in the CACHE hash table.

Prefers the file-level note; falls back to the first note in the file.
Returns nil (also cached) when PATH holds no indexed note."
  (let ((cached (gethash path cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let* ((notes (vulpea-db-query-by-file-path path))
             (note (or (seq-find (lambda (n) (= (vulpea-note-level n) 0)) notes)
                       (car notes))))
        (puthash path note cache)
        note))))

(defun vulpea-mentions--collect (output note own-path)
  "Collect unlinked mentions of NOTE from ripgrep OUTPUT.

OWN-PATH is NOTE's own expanded file path, whose hits are skipped.
Returns a list of plists with :note (the mentioning note), :path,
:line, and :context."
  (let ((terms (vulpea-mentions--note-terms note))
        (path->note (make-hash-table :test 'equal))
        (result nil))
    (dolist (hit (vulpea-mentions--parse-rg-json output))
      (let ((path (plist-get hit :path))
            (line-text (plist-get hit :line-text)))
        (when (and (not (equal (expand-file-name path) own-path))
                   (vulpea-mentions--line-unlinked-p line-text terms))
          (let ((mentioning (vulpea-mentions--file-note path path->note)))
            (when mentioning
              (push (list :note mentioning
                          :path path
                          :line (plist-get hit :line)
                          :context (string-trim line-text))
                    result))))))
    (nreverse result)))

;;; Async entry point

;;;###autoload
(defun vulpea-note-unlinked-mentions-async (note resolve reject)
  "Find notes mentioning NOTE's title or aliases without linking to it.

Searches the files under `vulpea-db-sync-directories' with ripgrep for
NOTE's title and aliases, drops occurrences that are already inside an
Org link or that live in NOTE's own file, and maps each remaining hit
to the mentioning note.

This is asynchronous and promise-style: exactly one of RESOLVE or
REJECT is called.

- RESOLVE is called with a list of plists, each with :note (the
  mentioning `vulpea-note'), :path, :line, and :context (the matching
  line, trimmed).
- REJECT is called with an error message string when ripgrep is
  unavailable or fails.

The (NOTE RESOLVE REJECT) shape matches a reactive loader, so a UI can
use `(apply-partially #\\='vulpea-note-unlinked-mentions-async note)'
directly."
  (let ((rg (executable-find "rg")))
    (cond
     ((not rg)
      (funcall reject "ripgrep (rg) not found on `exec-path'"))
     (t
      (let ((terms (vulpea-mentions--note-terms note))
            (dirs (seq-filter #'file-directory-p
                              (mapcar #'expand-file-name
                                      vulpea-db-sync-directories)))
            (own-path (expand-file-name (vulpea-note-path note))))
        (if (or (null terms) (null dirs))
            (funcall resolve nil)
          (let ((output ""))
            (make-process
             :name "vulpea-mentions"
             :command (vulpea-mentions--rg-command rg terms dirs)
             :connection-type 'pipe
             :noquery t
             :filter (lambda (_proc chunk)
                       (setq output (concat output chunk)))
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (let ((code (process-exit-status proc)))
                   ;; rg exits 0 with matches, 1 with none, >1 on error.
                   (if (memq code '(0 1))
                       (condition-case err
                           (funcall resolve
                                    (vulpea-mentions--collect
                                     output note own-path))
                         (error (funcall reject (error-message-string err))))
                     (funcall reject
                              (format "ripgrep failed (exit %s)" code))))))))))))))

(provide 'vulpea-mentions)
;;; vulpea-mentions.el ends here
