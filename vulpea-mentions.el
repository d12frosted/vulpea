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
;; Word boundaries are script-aware.  Scripts that separate words with
;; spaces (Latin, Cyrillic, ...) require a match to stand on its own:
;; "art" does not match inside "smart".  Scripts without word
;; separators (Han, Kana, Thai, ...) match as substrings: 金阁寺 is
;; found inside 又名金阁寺, since there is no boundary to respect.  See
;; `vulpea-mentions-word-boundaries' and
;; `vulpea-mentions-spaceless-scripts'.
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
(require 'org)
(require 'vulpea-note)
(require 'vulpea-db-query)
(require 'vulpea-db-extract)
(require 'vulpea-utils)

(defvar vulpea-db-sync-directories)     ; defined in vulpea-db-sync

(defgroup vulpea-mentions nil
  "Unlinked mention detection for Vulpea."
  :group 'vulpea)

(defcustom vulpea-mentions-min-term-length 3
  "Minimum length of a title or alias to search for as a mention.

Very short titles (e.g. \"a\") produce too much noise, so titles and
aliases shorter than this are ignored when looking for unlinked
mentions.

Length is measured with `string-width', not `length': a CJK character
occupies two columns, so a 2-character title like 北京 (width 4)
clears the default of 3, while a 2-letter Latin title does not.  This
matters because 2-character titles are the norm in Chinese and
Japanese."
  :type 'integer
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-word-boundaries 'auto
  "How word boundaries apply in the ripgrep pre-filter.

This tunes recall and speed of the ripgrep pass only.  The exact
matching semantics - \"Wine\" counts only as a standalone word, 金阁寺
counts anywhere inside CJK text - are enforced in Emacs on every
candidate line either way (see `vulpea-mentions--line-unlinked-p' and
`vulpea-mentions-spaceless-scripts').

- `auto': each term edge gets a boundary assertion only when it can
  glue to neighboring text at all (see
  `vulpea-mentions--edge-boundary'): ASCII alphanumeric edges get a
  fast ASCII assertion, while spaceless-script edges (金阁寺),
  punctuation edges and non-ASCII spaced edges (Київ) get none and
  rely on the Emacs re-check.  A mixed term like Emacs入门 keeps the
  assertion only on its Latin edge.  The ASCII assertion treats
  non-ASCII text as a boundary, so a Latin title glued into CJK
  prose (\"Emacs\" inside 我爱用Emacs写作) is found as well.

- t: a strict Unicode \\=\\b on both sides of every term, the old
  behavior.  It misses mentions embedded in CJK prose, and on large
  title dictionaries it is also an order of magnitude slower than
  `auto', because a Unicode \\=\\b over non-ASCII text forces the
  regex engine off its fast literal path.

- nil: no boundaries in the pre-filter at all.  Over `auto' this
  only adds matches next to an underscore (\"foo\" inside
  \"foo_bar\"), which `auto' leaves behind because ripgrep counts _
  as a word character.  The cost is that every glued occurrence of
  every Latin title (\"art\" inside \"smart\") travels to the Emacs
  re-check just to be rejected there."
  :type '(choice (const :tag "Per term edge (script-aware)" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-spaceless-scripts
  '(han kana cjk-misc bopomofo hangul thai lao khmer burmese)
  "Scripts whose text does not separate words with spaces.

Symbols are compared against `char-script-table'.  A term edge or a
neighboring character in one of these scripts never demands a word
boundary: boundaries are meaningless between, say, two Han characters,
and requiring them loses legitimate mentions (金阁寺 inside
又名金阁寺).  Hangul is included because Korean particles attach
directly to nouns.  Set to nil to demand strict word boundaries
everywhere, restoring the old behavior."
  :type '(repeat symbol)
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
  "Exclude mentions to and from notes that are already linked.

When non-nil, a note that links to the target no longer shows up
among the target's incoming unlinked mentions, and notes already
linked from a buffer are dropped from that buffer's outgoing
mentions.  The reasoning: once a link exists, further plain-text
occurrences are prose, not a missing connection.  Set to nil to see
every occurrence regardless of existing links."
  :type 'boolean
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-ignore-property-key "MENTIONS"
  "Org property marking a note as excluded from mention detection.

A note whose property drawer sets this property to
`vulpea-mentions-ignore-property-value' opts out of mentions entirely:
no incoming mentions are searched for it, and its title and aliases
are not offered as outgoing mention candidates in other notes.  Useful
for notes whose titles are common words (e.g. \"Sets\") that would
otherwise produce mostly false positives."
  :type 'string
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-ignore-property-value "ignore"
  "Value of `vulpea-mentions-ignore-property-key' that opts a note out."
  :type 'string
  :group 'vulpea-mentions)

(defcustom vulpea-mentions-per-note-ignore-property-key "IGNORE_MENTIONS_FROM"
  "Org property marking notes ignored from mention detection for this note.

Its value is a whitespace-separated list of file level note ids.
Mentions from those notes to this note are excluded from mention lists."
  :type 'string
  :group 'vulpea-mentions)

;;; Pure helpers

(defun vulpea-mentions--note-terms (note)
  "Return the search terms for NOTE: its title and aliases.

Terms are trimmed, de-duplicated case-insensitively, and those
narrower than `vulpea-mentions-min-term-length' are dropped."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (term (cons (vulpea-note-title note)
                        (vulpea-note-aliases note)))
      (let ((trimmed (and term (string-trim term))))
        (when (and trimmed
                   (> (length trimmed) 0)
                   (>= (string-width trimmed) vulpea-mentions-min-term-length))
          (let ((key (downcase trimmed)))
            (unless (gethash key seen)
              (puthash key t seen)
              (push trimmed result))))))
    (nreverse result)))

(defun vulpea-mentions--spaceless-char-p (char)
  "Return non-nil when CHAR belongs to a script without word separators.
See `vulpea-mentions-spaceless-scripts'."
  (and char
       (memq (aref char-script-table char)
             vulpea-mentions-spaceless-scripts)))

(defun vulpea-mentions--rg-quote (term)
  "Escape TERM for literal use in a ripgrep (Rust) regular expression."
  (replace-regexp-in-string "[][\\^$.|?*+(){}]" "\\\\\\&" term))

(defun vulpea-mentions--edge-boundary (edge-char)
  "Return the boundary assertion to place next to EDGE-CHAR, or nil.

Under `auto' (see `vulpea-mentions-word-boundaries') an edge gets an
assertion only when gluing is possible at all: the edge must be
alphanumeric - next to punctuation a \\=\\b inverts into demanding
adjacent text - and must not belong to a spaceless script, where a
boundary loses embedded mentions.

ASCII edges get the ASCII assertion (?-u:\\=\\b) rather than the
Unicode one.  It matches everywhere the Unicode assertion does (ASCII
word characters are a subset of Unicode word characters, so the
negated neighbor test is weaker), and it keeps ripgrep on its fast
literal engine: a Unicode \\=\\b over a non-ASCII haystack forces a
slow fallback, an order of magnitude on a large title dictionary.
Non-ASCII spaced edges (Cyrillic, Greek, ...) get no pre-filter
assertion for the same reason.  Either way the pre-filter only
over-approximates; `vulpea-mentions--line-unlinked-p' enforces the
exact boundary rules on every candidate line."
  (pcase vulpea-mentions-word-boundaries
    ('auto (cond
            ((vulpea-mentions--spaceless-char-p edge-char) nil)
            ((not (string-match-p "[[:alnum:]]" (char-to-string edge-char)))
             nil)
            ((< edge-char 128) "(?-u:\\b)")
            (t nil)))
    ('nil nil)
    (_ "\\b")))

(defun vulpea-mentions--rg-pattern (term)
  "Return the ripgrep regex searching for TERM.

The literal is escaped with `vulpea-mentions--rg-quote' and each side
gets the boundary assertion `vulpea-mentions--edge-boundary' picks for
its edge character."
  (concat
   (or (vulpea-mentions--edge-boundary (aref term 0)) "")
   (vulpea-mentions--rg-quote term)
   (or (vulpea-mentions--edge-boundary (aref term (1- (length term)))) "")))

(defun vulpea-mentions--rg-command (rg terms dirs)
  "Build the ripgrep command, run as RG, for TERMS over DIRS.

Produces a JSON stream of case-insensitive matches restricted to Org
files.  Each term is compiled to a regex by
`vulpea-mentions--rg-pattern', which applies script-aware word
boundaries."
  (append
   (list rg "--json" "--ignore-case" "--glob" "*.org")
   (mapcan (lambda (term)
             (list "-e" (vulpea-mentions--rg-pattern term)))
           terms)
   dirs))

(defun vulpea-mentions--rg-stdin-command (rg patterns-file)
  "Build the ripgrep command, run as RG, over standard input.

PATTERNS-FILE holds one regex per line (see
`vulpea-mentions--rg-pattern'); the text to search arrives on stdin."
  (list rg "--json" "--ignore-case" "-f" patterns-file "-"))

(defun vulpea-mentions--parse-rg-json (output)
  "Parse ripgrep --json OUTPUT into a list of raw hit plists.

Each hit is a plist with :path, :line, :line-text, and :matched (the
list of matched substrings on the line).  Non-match events and
unparseable lines are ignored, as are hits whose path or line content
ripgrep encodes as base64 bytes rather than text (the value was not
valid UTF-8, e.g. a mis-encoded file name); one such file must not
take down the rest of the scan."
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
            ;; A value that is not valid UTF-8 arrives as {"bytes":
            ;; base64} instead of {"text": ...}, leaving the binding
            ;; nil.  Such a path cannot name an indexed note (the
            ;; database holds decoded strings, and reconstructing a
            ;; file name from raw bytes means guessing its encoding),
            ;; so the hit is dropped rather than decoded.
            (when (and path text)
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

(defun vulpea-mentions--glued-p (edge neighbor)
  "Return non-nil when NEIGHBOR glues to EDGE into one word.

EDGE is the first or last character of a match, NEIGHBOR the character
just outside it (nil at the start or end of a line).  They glue - the
match is not a standalone word - only when both are alphanumeric and
neither belongs to `vulpea-mentions-spaceless-scripts', whose scripts
have no word separators to respect."
  (and neighbor
       (not (vulpea-mentions--spaceless-char-p edge))
       (not (vulpea-mentions--spaceless-char-p neighbor))
       (string-match-p "[[:alnum:]]" (char-to-string edge))
       (string-match-p "[[:alnum:]]" (char-to-string neighbor))))

(defun vulpea-mentions--line-unlinked-p (line terms)
  "Return non-nil when some term in TERMS occurs in LINE outside any link.

Matching is case-insensitive.  A match must stand on its own where its
edges touch spaced-script text (see `vulpea-mentions--glued-p'), while
spaceless scripts match as substrings; occurrences inside an Org link
are not counted."
  (let ((spans (vulpea-mentions--link-spans line))
        (case-fold-search t)
        (len (length line)))
    (catch 'found
      (dolist (term terms)
        (unless (string-empty-p term)
          (let ((re (regexp-quote term))
                (start 0))
            (while (and (<= start len) (string-match re line start))
              (let* ((beg (match-beginning 0))
                     (end (match-end 0))
                     (before (and (> beg 0) (aref line (1- beg))))
                     (after (and (< end len) (aref line end))))
                (setq start (1+ beg))
                (unless (or (vulpea-mentions--in-link-p beg spans)
                            (vulpea-mentions--glued-p (aref line beg) before)
                            (vulpea-mentions--glued-p (aref line (1- end)) after))
                  (throw 'found t)))))))
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

(defun vulpea-mentions--ignore-note-p (note)
  "Return non-nil when NOTE opts out of mention detection.

A note opts out by setting `vulpea-mentions-ignore-property-key' to
`vulpea-mentions-ignore-property-value' in its property drawer; it then
gets no incoming mention scan and is excluded from the outgoing
candidate dictionary."
  (equal vulpea-mentions-ignore-property-value
         (cdr (assoc
               (upcase vulpea-mentions-ignore-property-key)
               (vulpea-note-properties note)))))

(defun vulpea-mentions--paths-link-to-note (note)
  "Return a hash table of note paths that contain links to NOTE."
  (let* ((result (make-hash-table :test 'equal))
         (links (vulpea-db-query-links-to (vulpea-note-id note)))
         (ids (mapcar (lambda (link) (plist-get link :source)) links))
         (notes (vulpea-db-query-by-ids ids))
         (paths (mapcar #'vulpea-note-path notes)))
    (mapc (lambda (path) (puthash (expand-file-name path) t result)) paths)
    result))

(defun vulpea-mentions--ignore-mention-ids (note)
  "Return note ids that mentions from them are ignored by NOTE."
  (let* ((result (make-hash-table :test 'equal))
         (properties (vulpea-note-properties note))
         (ignore-mentions
          (assoc (upcase vulpea-mentions-per-note-ignore-property-key)
                 properties)))
    (when ignore-mentions
      (let ((ignored-ids (split-string (cdr ignore-mentions))))
        (mapc (lambda (id) (puthash id t result)) ignored-ids)))
    result))

(defun vulpea-mentions-ignore-from (note-or-id from-note-or-id &optional revert)
  "Silence mentions of NOTE-OR-ID coming from FROM-NOTE-OR-ID.

Add FROM-NOTE-OR-ID's file level note id to
`vulpea-mentions-per-note-ignore-property-key' in NOTE-OR-ID's property
drawer, saves the file and syncs the database.  If REVERT is non-nil,
then remove the id from the property value list."
  (let* ((note (vulpea-utils-normalize-id-note note-or-id))
         (from-note (vulpea-utils-normalize-id-note from-note-or-id))
         (from-file-note (vulpea-utils-get-file-level-note from-note)))
    (if from-file-note
        (vulpea-utils-with-note-sync note
          (let ((func (if revert
                          #'org-entry-remove-from-multivalued-property
                        #'org-entry-add-to-multivalued-property)))
            (funcall func
                     (point)
                     vulpea-mentions-per-note-ignore-property-key
                     (vulpea-note-id from-file-note))
            ;; Clean up the property line
            (when revert
              (when (null (org-entry-get-multivalued-property
                           (point)
                           vulpea-mentions-per-note-ignore-property-key))
                (org-delete-property vulpea-mentions-per-note-ignore-property-key)))))
      (message "No file level note found for note %s" (vulpea-note-title from-note)))))

(defun vulpea-mentions-unignore-from (note-or-id from-note-or-id)
  "Make mentions from FROM-NOTE-OR-ID to NOTE-OR-ID visible.

Remove the file level note id of FROM-NOTE-OR-ID from the
`vulpea-mentions-per-note-ignore-property-key' value list of NOTE-OR-ID,
saves the file and syncs the database."
  (vulpea-mentions-ignore-from note-or-id from-note-or-id t))

(defun vulpea-mentions--collect (output note own-path)
  "Collect unlinked mentions of NOTE from ripgrep OUTPUT.

OWN-PATH is NOTE's own expanded file path, whose hits are skipped.  Hits
whose mentioning note shares a name with NOTE (a title collision) are
skipped too.  Hits whose mentioning note contains at least one explicit
link to NOTE are skipped as well.  Set `vulpea-mentions-exclude-linked'
to nil to disable this behavior.  Hits whose mentioning note are ignored
explicitly by `vulpea-mentions-per-note-ignore-property-key' are skipped
as well.  Returns a list of plists with :note (the mentioning note),
:path, :line, and :context."
  (let* ((terms (vulpea-mentions--note-terms note))
         (path->note (make-hash-table :test 'equal))
         (hits (vulpea-mentions--parse-rg-json output))
         (paths-link-to-note
          (when (and vulpea-mentions-exclude-linked hits)
            (vulpea-mentions--paths-link-to-note note)))
         (ignore-mention-ids (vulpea-mentions--ignore-mention-ids note))
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
                       (not (vulpea-mentions--shares-name-p mentioning terms))
                       (not (gethash (vulpea-note-id mentioning) ignore-mention-ids)))
              (push (list :note mentioning
                          :path path
                          :line (plist-get hit :line)
                          :context (string-trim line-text))
                    result))))))
    (nreverse result)))

;;; Outgoing: mentions made by a buffer

(defun vulpea-mentions--title-dictionary ()
  "Return (DICT . TERMS) describing the candidate notes' names.

Candidates are the notes kept by `vulpea-mentions-note-filter', minus
those opting out via `vulpea-mentions--ignore-note-p'.  DICT is a hash
table
mapping a downcased title or alias to the list of note ids that bear it.
TERMS is the de-duplicated list of the original title and alias strings
to search for."
  (let ((dict (make-hash-table :test 'equal))
        (terms nil)
        (filter (lambda (note)
                  (and (not (vulpea-mentions--ignore-note-p note))
                       (funcall vulpea-mentions-note-filter note)))))
    (dolist (note (vulpea-db-query filter))
      (let ((id (vulpea-note-id note))
            (names (cons (vulpea-note-title note) (vulpea-note-aliases note))))
        (dolist (name names)
          (when (stringp name)
            (let ((trimmed (string-trim name)))
              (when (and (> (length trimmed) 0)
                         (>= (string-width trimmed)
                             vulpea-mentions-min-term-length))
                (push trimmed terms)
                (push id (gethash (downcase trimmed) dict))))))))
    (cons dict (delete-dups terms))))

(defun vulpea-mentions--buffer-link-ids ()
  "Return a hash table keyed by the ids of `id:' links in the current buffer.

Only bracket links are considered: `vulpea-mentions--line-unlinked-p'
recognizes bracket links alone when deciding whether an occurrence is
already linked, so the exclusion set must match, and the literal
search for \"[[\" is also much cheaper than the plain-link regexp."
  (let ((result (make-hash-table :test 'equal))
        (vulpea-db-index-plain-links nil))
    (vulpea-db--region-links
     (point-min)
     (point-max)
     (lambda (link)
       (when (equal (plist-get link :type) "id")
         (puthash (plist-get link :dest) t result))))
    result))

(defun vulpea-mentions--ignored-by-note-p (ids note)
  "Return non-nil if at least one of IDS is ignored by NOTE."
  (let ((ignore-mentions-id (vulpea-mentions--ignore-mention-ids note)))
    (seq-some (lambda (id) (gethash id ignore-mentions-id)) ids)))

(defun vulpea-mentions--collect-outgoing (output dict self-ids linked-ids)
  "Collect outgoing unlinked mentions from ripgrep OUTPUT over one buffer.

DICT maps a downcased title/alias to candidate note ids (see
`vulpea-mentions--title-dictionary').  SELF-IDS are the note ids in the
buffer's own file, excluded as candidates.  If one of SELF-IDS are
explicitly ignored by the mentioned note, then the mention will be
dropped (see `vulpea-mentions-per-note-ignore-property-key').
LINKED-IDS is a hash table keyed by the note ids the buffer already
links to (see `vulpea-mentions--buffer-link-ids'); candidates found in
it are dropped.  Pass an empty table to keep them all.

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
                        (unless (vulpea-mentions--ignored-by-note-p self-ids cand)
                          (push (list :note cand :line line-no
                                    :context (string-trim line-text)
                                    :matched term)
                                result)))))))))))
      (nreverse result))))

;;; Async entry point

;;;###autoload
(defun vulpea-note-unlinked-mentions-async (note resolve reject)
  "Find notes mentioning NOTE's title or aliases without linking to it.

Searches the files under `vulpea-db-sync-directories' with ripgrep for
NOTE's title and aliases, drops occurrences that are already inside an
Org link, that live in NOTE's own file or in the file of a note sharing
NOTE's title (a title collision), or that fall on an Org metadata
line (a keyword or property-drawer line), or that live in a file
already linking to NOTE (unless `vulpea-mentions-exclude-linked' is
nil), and maps each remaining hit to the mentioning note.  When NOTE
itself opts out of mention detection (see
`vulpea-mentions-ignore-property-key'), no search runs at all and
RESOLVE is called synchronously with nil.

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
        (if (or (null terms) (null dirs)
                (vulpea-mentions--ignore-note-p note))
            (progn (funcall resolve nil) nil)
          (let ((output ""))
            (make-process
             :name "vulpea-mentions"
             :command (vulpea-mentions--rg-command rg terms dirs)
             :connection-type 'pipe
             :noquery t
             ;; Emacs encodes the :command arguments with the process
             ;; coding system, so this pins both the non-ASCII search
             ;; terms and the JSON output to UTF-8 no matter what
             ;; `default-process-coding-system' says.
             :coding 'utf-8
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
occurrences inside an Org link or on an Org metadata line, ignores
notes in the buffer's own file and notes the buffer already links
to (unless `vulpea-mentions-exclude-linked' is nil), and maps each
remaining match to the candidate note(s) it could link to.  The
buffer's live content is searched (via the process's standard input),
so unsaved edits are included - both for the mentions and for the
links that exclude them.

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
             (linked-ids (if vulpea-mentions-exclude-linked
                             (vulpea-mentions--buffer-link-ids)
                           (make-hash-table :test 'equal))))
        (if (null terms)
            (progn (funcall resolve nil) nil)
          (let ((patterns-file (make-temp-file "vulpea-mentions-pat-"))
                (output ""))
            ;; rg rejects a patterns file that is not valid UTF-8, so
            ;; the user's `coding-system-for-write' must not leak in.
            (let ((coding-system-for-write 'utf-8))
              (with-temp-file patterns-file
                (insert (mapconcat #'vulpea-mentions--rg-pattern terms "\n") "\n")))
            (let ((proc (make-process
                         :name "vulpea-mentions-out"
                         :command (vulpea-mentions--rg-stdin-command
                                   rg patterns-file)
                         :connection-type 'pipe
                         :noquery t
                         ;; Pin the buffer content sent on stdin and
                         ;; the JSON output to UTF-8 no matter what
                         ;; `default-process-coding-system' says.
                         :coding 'utf-8
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
