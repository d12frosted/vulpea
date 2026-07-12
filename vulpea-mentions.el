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

(require 'cl-lib)
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

(defun vulpea-mentions-file-level-note-p (note)
  "Return non-nil when NOTE is a file-level note.
The default value of `vulpea-mentions-note-filter'."
  (= (vulpea-note-level note) 0))

(defcustom vulpea-mentions-note-filter #'vulpea-mentions-file-level-note-p
  "Predicate selecting which notes are searched for as candidates.

A function called with a `vulpea-note' that returns non-nil to keep the
note.  Its title and aliases are then searched for when looking for the
unlinked mentions a buffer makes (`vulpea-buffer-unlinked-mentions-async').

The default keeps only file-level notes, which avoids noise from
heading-level notes.  Set it to your own predicate to, say, exclude a
journal: (lambda (note) (not (member \"journal\" (vulpea-note-tags note)))),
or to `always' to consider every note."
  :type 'function
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-exclude-linked t
  "Exclude mentions from/to linked notes.

If set to non-nil value, then mentions from/to linked notes will be
excluded. That is, if a note has a link to the current note, then any
other mentions from that note to the current one is excluded. On the
other hand, if the current note contains a link to another note, then
any mentions from the current one to the other one will be excluded."
  :type 'boolean
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

Each hit is a plist with :path, :line, :line-text, and :matched (the
list of matched substrings on the line).  Non-match events and
unparseable lines are ignored."
  (let ((result nil))
    (dolist (line (split-string output "\n" t))
      (let ((obj (ignore-errors (json-parse-string line :object-type 'alist))))
        (when (and obj (equal (alist-get 'type obj) "match"))
          (let* ((data (alist-get 'data obj))
                 (path (alist-get 'text (alist-get 'path data)))
                 (line-no (alist-get 'line_number data))
                 (text (alist-get 'text (alist-get 'lines data)))
                 (submatches (alist-get 'submatches data))
                 (matched (and submatches
                               (delq nil
                                     (mapcar (lambda (sm)
                                               (alist-get 'text (alist-get 'match sm)))
                                             (append submatches nil))))))
            (when text
              (push (list :path path
                          :line line-no
                          :line-text (string-trim-right text "[\n\r]+")
                          :matched matched)
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

(defun vulpea-mentions--metadata-line-p (line)
  "Return non-nil when LINE is an Org keyword or property-drawer line.

Lines such as #+title:, #+filetags:, :PROPERTIES:, :ID:, or :END:
declare a note's own metadata rather than prose, so a title that appears
on them (often a same-titled note's own #+title:) is not a real mention."
  (string-match-p "\\`[ \t]*\\(#\\+\\|:[A-Za-z0-9_@%-]+:\\)" line))

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

(defun vulpea-mentions--shares-name-p (note terms)
  "Return non-nil when NOTE's title or an alias matches one of TERMS.

Comparison is case-insensitive.  Used to drop a hit whose mentioning
note shares a name with the searched note (a title collision): an
occurrence in a same-named note's file is more likely that note's own
title in prose than a reference to a different note, so it is excluded -
the same reasoning as skipping the searched note's own file."
  (let ((names (mapcar #'downcase
                       (cons (or (vulpea-note-title note) "")
                             (vulpea-note-aliases note))))
        (lc-terms (mapcar #'downcase terms)))
    (seq-intersection names lc-terms #'string=)))

(defun vulpea-mentions--paths-link-to-note (note)
  "Return a hash table of paths of note that contain links to NOTE."
  (let* ((result (make-hash-table :test 'equal))
         (links (vulpea-db-query-links-to (vulpea-note-id note)))
         (ids (mapcar (lambda (link) (plist-get link :source)) links))
         (notes (vulpea-db-query-by-ids ids))
         (paths (mapcar #'vulpea-note-path notes)))
    (mapc (lambda (path) (puthash (expand-file-name path) t result)) paths)
    result))

(defun vulpea-mentions--collect (output note own-path)
  "Collect unlinked mentions of NOTE from ripgrep OUTPUT.

OWN-PATH is NOTE's own expanded file path, whose hits are skipped.  Hits
whose mentioning note shares a name with NOTE (a title collision) are
skipped too. Hits whose mentioning note contains at least one explicit
link to NOTE are skipped as well. Set `vulpea-mentions-exclude-linked'
to nil to disable this behavior. Returns a list of plists with
:note (the mentioning note), :path, :line, and :context."
  (let* ((terms (vulpea-mentions--note-terms note))
         (path->note (make-hash-table :test 'equal))
         (hits (vulpea-mentions--parse-rg-json output))
         (paths-link-to-note
          (when (and vulpea-mentions-exclude-linked hits)
            (vulpea-mentions--paths-link-to-note note)))
         (result nil))
    (dolist (hit hits)
      (let* ((path (plist-get hit :path))
             (line-text (plist-get hit :line-text))
             (expanded-path (expand-file-name path)))
        (when (and (not (equal expanded-path own-path))
                   (not (and vulpea-mentions-exclude-linked
                             (gethash expanded-path paths-link-to-note)))
                   (not (vulpea-mentions--metadata-line-p line-text))
                   (vulpea-mentions--line-unlinked-p line-text terms))
          (let ((mentioning (vulpea-mentions--file-note path path->note)))
            (when (and mentioning
                       (not (vulpea-mentions--shares-name-p mentioning terms)))
              (push (list :note mentioning
                          :path path
                          :line (plist-get hit :line)
                          :context (string-trim line-text))
                    result))))))
    (nreverse result)))

;;; Outgoing: mentions made by a buffer

(defun vulpea-mentions--title-dictionary ()
  "Return (DICT . TERMS) describing the candidate notes' names.

Candidates are the notes kept by `vulpea-mentions-note-filter'.  DICT is
a hash table mapping a downcased title or alias to the list of note ids
that bear it.  TERMS is the de-duplicated list of the original title and
alias strings to search for."
  (let ((dict (make-hash-table :test 'equal))
        (terms nil))
    (dolist (note (vulpea-db-query vulpea-mentions-note-filter))
      (let ((id (vulpea-note-id note))
            (names (cons (vulpea-note-title note) (vulpea-note-aliases note))))
        (dolist (name names)
          (when (stringp name)
            (let ((trimmed (string-trim name)))
              (when (>= (length trimmed) vulpea-mentions-min-term-length)
                (push trimmed terms)
                (push id (gethash (downcase trimmed) dict))))))))
    (cons dict (delete-dups terms))))

(defun vulpea-mentions--collect-outgoing-link-ids-in-buffer ()
  "Return a hash table of ids appeared in links in the current buffer.

The hash table will be empty if `vulpea-mentions-exclude-linked' is nil."
  (let ((result (make-hash-table :test 'equal))
        (vulpea-db-index-plain-links nil))
    (when vulpea-mentions-exclude-linked
      (vulpea-db--region-links
       (point-min)
       (point-max)
       (lambda (link)
         (puthash (plist-get link :dest) t result))))
    result))

(defun vulpea-mentions--collect-outgoing (output dict self-ids linked-ids)
  "Collect outgoing unlinked mentions from ripgrep OUTPUT over one buffer.

DICT maps a downcased title/alias to candidate note ids (see
`vulpea-mentions--title-dictionary').  SELF-IDS are the note ids in the
buffer's own file, excluded as candidates. LINKED-IDS is a hash table of
note ids that appears in the links in the buffer. It would be empty if
`vulpea-mentions-exclude-linked' is nil.

Returns a list of plists with :note (a candidate note to link to),
:line, :context, and :matched (the text that matched)."
  (let ((id->note (make-hash-table :test 'equal))
        (result nil))
    (cl-flet ((resolve-note (id)
                (let ((cached (gethash id id->note 'miss)))
                  (if (not (eq cached 'miss)) cached
                    (puthash id (vulpea-db-get-by-id id) id->note)))))
      (let ((hits (vulpea-mentions--parse-rg-json output)))
        (dolist (hit hits)
          (let ((line-text (plist-get hit :line-text))
                (line-no (plist-get hit :line)))
            (unless (vulpea-mentions--metadata-line-p line-text)
              (dolist (term (seq-uniq (plist-get hit :matched)))
                (when (vulpea-mentions--line-unlinked-p line-text (list term))
                  (dolist (id (gethash (downcase term) dict))
                    (unless (or (member id self-ids)
                                (gethash id linked-ids))
                      (when-let* ((cand (resolve-note id)))
                        (push (list :note cand :line line-no
                                    :context (string-trim line-text)
                                    :matched term)
                              result))))))))))
      (nreverse result))))

;;; Async entry point

;;;###autoload
(defun vulpea-note-unlinked-mentions-async (note resolve reject)
  "Find notes mentioning NOTE's title or aliases without linking to it.

Searches the files under `vulpea-db-sync-directories' with ripgrep for
NOTE's title and aliases, drops occurrences that are already inside an
Org link, that live in NOTE's own file or in the file of a note sharing
NOTE's title (a title collision), or that fall on an Org metadata
line (a keyword or property-drawer line), or that belong to file already
containing a link to NOTE (if `vulpea-mentions-exclude-linked' is
non-nil, which is the default) and maps each remaining hit to the
mentioning note.

This is asynchronous and promise-style: exactly one of RESOLVE or
REJECT is called.

- RESOLVE is called with a list of plists, each with :note (the
  mentioning `vulpea-note'), :path, :line, and :context (the matching
  line, trimmed).
- REJECT is called with an error message string when ripgrep is
  unavailable or fails.

The (NOTE RESOLVE REJECT) shape matches a reactive loader, so a UI can
use `(apply-partially #\\='vulpea-note-unlinked-mentions-async note)'
directly.

Returns the ripgrep process, so the caller can wait on or
`delete-process' it, or nil when the result is delivered synchronously
\(no ripgrep, no search terms, or no directories)."
  (let ((rg (executable-find "rg")))
    (cond
     ((not rg)
      (funcall reject "ripgrep (rg) not found on `exec-path'")
      nil)
     (t
      (let ((terms (vulpea-mentions--note-terms note))
            (dirs (seq-filter #'file-directory-p
                              (mapcar #'expand-file-name
                                      vulpea-db-sync-directories)))
            (own-path (expand-file-name (vulpea-note-path note))))
        (if (or (null terms) (null dirs))
            (progn (funcall resolve nil) nil)
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

;;;###autoload
(defun vulpea-buffer-unlinked-mentions-async (resolve reject)
  "Find notes mentioned as plain text in the current buffer without a link.

Scans the current buffer's content with ripgrep for the titles and
aliases of the candidate notes (those kept by
`vulpea-mentions-note-filter', file-level notes by default), drops
occurrences inside an Org link or on an Org metadata line, ignores:

1. Notes in the buffer's own file

2. Notes that are already linked by this buffer if
`vulpea-mentions-exclude-linked' is non-nil.

Maps each remaining match to the candidate note(s) it could link to.
The buffer's live content is searched (via the process's standard
input), so unsaved edits are included.

Asynchronous and promise-style: exactly one of RESOLVE or REJECT is
called.  RESOLVE receives a list of plists with :note (a candidate note
to link to), :line, :context, and :matched (the text that matched).
REJECT receives an error message string.

Returns the ripgrep process, or nil when answered synchronously.  As
with `vulpea-note-unlinked-mentions-async', the (RESOLVE REJECT) shape
is a ready-made reactive loader; wrap it in `with-current-buffer' to
target a specific buffer."
  (let ((rg (executable-find "rg")))
    (cond
     ((not rg)
      (funcall reject "ripgrep (rg) not found on `exec-path'")
      nil)
     (t
      (let* ((content (buffer-string))
             (file (and buffer-file-name (expand-file-name buffer-file-name)))
             (self-ids (when file
                         (mapcar #'vulpea-note-id
                                 (vulpea-db-query-by-file-path file))))
             (dict-terms (vulpea-mentions--title-dictionary))
             (dict (car dict-terms))
             (terms (cdr dict-terms))
             (linked-ids (vulpea-mentions--collect-outgoing-link-ids-in-buffer)))
        (if (null terms)
            (progn (funcall resolve nil) nil)
          (let ((patterns-file (make-temp-file "vulpea-mentions-pat-"))
                (output ""))
            (with-temp-file patterns-file
              (insert (mapconcat #'identity terms "\n") "\n"))
            (let ((proc (make-process
                         :name "vulpea-mentions-out"
                         :command (list rg "--json" "--fixed-strings"
                                        "--ignore-case" "--word-regexp"
                                        "-f" patterns-file "-")
                         :connection-type 'pipe
                         :noquery t
                         :filter (lambda (_proc chunk)
                                   (setq output (concat output chunk)))
                         :sentinel
                         (lambda (proc _event)
                           (when (memq (process-status proc) '(exit signal))
                             (ignore-errors (delete-file patterns-file))
                             (let ((code (process-exit-status proc)))
                               (if (memq code '(0 1))
                                   (condition-case err
                                       (funcall resolve
                                                (vulpea-mentions--collect-outgoing
                                                 output dict self-ids linked-ids))
                                     (error (funcall reject (error-message-string err))))
                                 (funcall reject
                                          (format "ripgrep failed (exit %s)" code)))))))))
              (process-send-string proc content)
              (process-send-eof proc)
              proc))))))))

(provide 'vulpea-mentions)
;;; vulpea-mentions.el ends here
