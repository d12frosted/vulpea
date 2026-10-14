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
;; A mention is a title in prose.  Where the title is Org syntax (a
;; heading tag, a todo keyword, a date in a timestamp, code) a link in
;; its place would break what is there, so the Org parser is asked
;; about every occurrence that is otherwise a mention.
;;
;; The "outgoing" question is the dual: which notes does this buffer
;; mention without linking?  There ripgrep searches one buffer for the
;; titles of the whole collection, so it is given the cheapest thing it
;; can match, plain strings with case already erased on both sides,
;; and everything else is decided in Emacs.
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
;; `vulpea-mentions-link' turns the mentions on a line into links.  It
;; reads the line with the code the searches read it with, so it links
;; what they list and nothing else.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'ol)
(require 'org)
(require 'org-element)
(require 'vulpea-note)
(require 'vulpea-db-query)
(require 'vulpea-db-extract)
(require 'vulpea-utils)

(defvar vulpea-db-sync-directories)     ; defined in vulpea-db-sync
(defvar org-element-cache-persistent)   ; Org 9.6
(defvar org-resource-download-policy)   ; Org 9.6
(defvar org-agenda-file-menu-enabled)   ; defined in org

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
  "How word boundaries apply in the ripgrep pre-filter of the incoming scan.

This tunes recall and speed of the ripgrep pass of
`vulpea-note-unlinked-mentions-async' only.  The exact matching
semantics - \"Wine\" counts only as a standalone word, 金阁寺 counts
anywhere inside CJK text - are enforced in Emacs on every candidate
line either way (see `vulpea-mentions--line-unlinked-p' and
`vulpea-mentions-spaceless-scripts').

The outgoing scan (`vulpea-buffer-unlinked-mentions-async') does not
read this option.  It looks for the titles of a whole collection at
once, which ripgrep can only do quickly for plain strings, so its
pre-filter carries no boundaries and finds what nil finds here.

- `auto': each term edge gets a boundary assertion only when it can
  glue to neighboring text at all (see
  `vulpea-mentions--edge-boundary'): ASCII alphanumeric edges get a
  fast ASCII assertion, while spaceless-script edges (金阁寺),
  punctuation edges and non-ASCII spaced edges (Київ) get none and
  rely on the Emacs re-check.  A mixed term like Emacs入门 keeps the
  assertion only on its Latin edge.  The ASCII assertion treats
  non-ASCII text as a boundary, so a Latin title glued into CJK
  prose (\"Emacs\" inside 我爱用Emacs写作) is found as well, and it
  lets an underscore through, which ripgrep counts as a word
  character and Emacs does not: \"Emacs\" is found in _Emacs_.

- t: a strict Unicode \\=\\b on both sides of every term, the old
  behavior.  It misses mentions embedded in CJK prose and next to an
  underscore, and it can be slower than `auto', because a Unicode
  \\=\\b over non-ASCII text forces the regex engine off its fast
  literal path.

- nil: no boundaries in the pre-filter at all.  It finds what `auto'
  finds, only slower: every glued occurrence of every Latin title
  (\"art\" inside \"smart\") travels to the Emacs re-check just to
  be rejected there."
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
every occurrence regardless of existing links.

For incoming mentions a file links to the target when the database
has a link to it from a note of that file, or when the text of the
file has a link to it that the Org parser confirms.  The second covers
links no note owns: in a file without a file-level id, a link that no
heading with an id encloses, like one above the first heading.  The
text of a link in a comment or a code block is no link, and one in
generated content does not count, as the database leaves it out too."
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

Its value is a whitespace-separated list of note ids.  Mentions from
those notes to this note are excluded from mention lists.  An id
silences the whole file it lives in, so it does not matter whether it is
a file-level id or a heading id."
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
slow fallback.  Non-ASCII spaced edges (Cyrillic, Greek, ...) get no
pre-filter assertion for the same reason.  Either way the pre-filter
only over-approximates; `vulpea-mentions--line-unlinked-p' enforces
the exact boundary rules on every candidate line."
  (pcase vulpea-mentions-word-boundaries
    ('auto (cond
            ((vulpea-mentions--spaceless-char-p edge-char) nil)
            ((not (string-match-p "[[:alnum:]]" (char-to-string edge-char)))
             nil)
            ((< edge-char 128) "(?:(?-u:\\b)|_)")
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

(defun vulpea-mentions--rg-id-link-pattern (id)
  "Return the ripgrep regex for the start of a bracket link to the note ID.

The id is matched as written, whatever case the search ignores
elsewhere, and up to the bracket that closes it: a longer id starting
with ID is the id of another note."
  (concat "(?-i:\\[\\[id:" (vulpea-mentions--rg-quote id) "\\])"))

(defun vulpea-mentions--rg-command (rg terms dirs &optional link-id)
  "Build the ripgrep command, run as RG, for TERMS over DIRS.

Produces a JSON stream of case-insensitive matches restricted to Org
files.  Each term is compiled to a regex by
`vulpea-mentions--rg-pattern', which applies script-aware word
boundaries.  ripgrep reports lines, and whether a term on one is
inside a link, also a link that wraps across lines, is for the Org
parser to tell (see `vulpea-mentions--keep-prose').

LINK-ID, when given, is the id of the note searched for.  The lines
with a bracket link to it are reported as well (see
`vulpea-mentions--rg-id-link-pattern'), which is how
`vulpea-mentions--collect' learns of the links no note owns."
  (append
   (list rg "--json" "--ignore-case" "--glob" "*.org")
   (mapcan (lambda (term)
             (list "-e" (vulpea-mentions--rg-pattern term)))
           terms)
   (when link-id
     (list "-e" (vulpea-mentions--rg-id-link-pattern link-id)))
   dirs))

(defvar vulpea-mentions--fold-table nil
  "Translation table erasing case.
Built by the function `vulpea-mentions--fold-table'.")

(defun vulpea-mentions--fold-table ()
  "Return the translation table that erases case.

It maps each character to the one a case-insensitive Emacs search
takes it for, as the standard case table has it."
  (or vulpea-mentions--fold-table
      (let ((table (make-char-table 'translation-table)))
        (map-char-table
         (lambda (key value)
           (when (characterp value)
             (if (consp key)
                 (cl-loop for char from (car key) to (cdr key)
                          unless (eq char value)
                          do (aset table char value))
               (unless (eq key value)
                 (aset table key value)))))
         (case-table-get-table (standard-case-table) 'canon))
        (setq vulpea-mentions--fold-table table))))

(defun vulpea-mentions--fold (string)
  "Return STRING with case erased, character for character.

Two strings fold to the same text exactly when a case-insensitive
Emacs search takes one for the other.  No character is added or
dropped on the way, which `downcase' does not promise (it writes a
dotted capital I as two characters), so a position in the folded text
is the same position in STRING."
  (with-temp-buffer
    (insert string)
    ;; Every changed character would announce itself to the change
    ;; hooks otherwise, which makes folding a megabyte of titles
    ;; several times slower.
    (let ((inhibit-modification-hooks t))
      (translate-region (point-min) (point-max) (vulpea-mentions--fold-table)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun vulpea-mentions--rg-file-command (rg patterns-file input-file)
  "Build the ripgrep command, run as RG, over the single INPUT-FILE.

PATTERNS-FILE holds one literal per line.  Both files are expected
case-folded (see `vulpea-mentions--fold'), which leaves ripgrep a
case-sensitive search for fixed strings: the one shape it hands to
its literal multi-pattern matcher, whose cost barely depends on how
many patterns there are or how long the input is.  Anything else is
compiled into one automaton that grows with every pattern, a
case-insensitive one fastest of all: seconds per search at ten
thousand titles, and past fifty thousand it overflows ripgrep's size
limit and the search fails.

--case-sensitive is spelled out because the user's ripgrep
configuration file may say --smart-case or --ignore-case.

INPUT-FILE is given as an explicit path, which ripgrep searches
whatever its extension and regardless of any ignore rules; \"-\" makes
it read standard input instead."
  (list rg "--json" "--fixed-strings" "--case-sensitive"
        "-f" patterns-file input-file))

(defun vulpea-mentions--snapshot-buffer-p ()
  "Return non-nil when the current buffer may be written to a temporary file.

A buffer with no file, or visiting a plain .org file, may be
snapshotted to disk for ripgrep to read.  An indirect buffer is judged
by its base buffer's file.  Any other extension is one
of `vulpea-db-extra-extensions', which exist for encrypted notes
\(.org.gpg, .org.age): their decrypted content must not leave the
buffer, so they are fed to ripgrep over a pipe instead."
  (let ((file (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
    (or (null file)
        (string-suffix-p ".org" file))))

(defun vulpea-mentions--parse-rg-json (output)
  "Parse ripgrep --json OUTPUT into a list of raw hit plists.

Each hit is a plist with :path, :line, :line-text, and :matched (the
list of matched substrings on the line).  Non-match
events and unparseable lines are ignored, as are hits whose path or
line content ripgrep encodes as base64 bytes rather than text (the
value was not valid UTF-8, e.g. a mis-encoded file name); one such
file must not take down the rest of the scan."
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

(defun vulpea-mentions--occurrences (line term)
  "Return the occurrences of TERM in LINE.

Each one is a list (BEG END GLUED LINKED).  BEG and END delimit it in
LINE.  GLUED is non-nil when it is part of a longer word rather than a
word of its own: one of its edges touches spaced-script text (see
`vulpea-mentions--glued-p').  LINKED is non-nil when it starts inside
an Org link that fits LINE.  Matching is case-insensitive.

A link that crosses a line boundary cannot be recognized from LINE
alone.  The Org parser sees it, see `vulpea-mentions--prose-p'."
  (let ((spans (vulpea-mentions--link-spans line))
        (case-fold-search t)
        (len (length line))
        (re (regexp-quote term))
        (start 0)
        (result nil))
    (unless (string-empty-p term)
      (while (and (<= start len) (string-match re line start))
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (before (and (> beg 0) (aref line (1- beg))))
               (after (and (< end len) (aref line end))))
          (setq start (1+ beg))
          (push (list beg end
                      (and (or (vulpea-mentions--glued-p (aref line beg) before)
                               (vulpea-mentions--glued-p (aref line (1- end)) after))
                           t)
                      (and (vulpea-mentions--in-link-p beg spans) t))
                result))))
    (nreverse result)))

(defun vulpea-mentions--mention-p (occurrence)
  "Return non-nil when OCCURRENCE is neither glued nor linked.
OCCURRENCE is an element of `vulpea-mentions--occurrences'."
  (not (or (nth 2 occurrence) (nth 3 occurrence))))

(defun vulpea-mentions--line-unlinked-p (line terms)
  "Return non-nil when some term in TERMS occurs in LINE outside any link.

Matching is case-insensitive.  A match must stand on its own where its
edges touch spaced-script text (see `vulpea-mentions--glued-p'), while
spaceless scripts match as substrings; occurrences inside an Org link
that fits LINE are not counted."
  (seq-some (lambda (term)
              (seq-some #'vulpea-mentions--mention-p
                        (vulpea-mentions--occurrences line term)))
            terms))

(defun vulpea-mentions--boundary-p (line pos)
  "Return non-nil when a word of its own may start or end at POS in LINE."
  (or (= pos 0)
      (= pos (length line))
      (not (vulpea-mentions--glued-p (aref line pos) (aref line (1- pos))))))

(defun vulpea-mentions--compound-p (term)
  "Return non-nil when a word boundary falls inside TERM."
  (cl-loop for pos from 1 below (length term)
           thereis (vulpea-mentions--boundary-p term pos)))

(defun vulpea-mentions--line-terms (line dict max-length)
  "Return the terms of DICT that stand in LINE as words of their own.

LINE is read from left to right.  At each word boundary the longest
stretch that ends on another boundary and is a key of DICT is taken,
MAX-LENGTH characters at most, and reading goes on after it.  These
are the matches ripgrep would report if it knew what a word is, given
the patterns longest first."
  (let* ((len (length line))
         ;; A term starts and ends on a word boundary and never with
         ;; whitespace, which leaves few stretches worth looking up.
         (opens (make-bool-vector (1+ len) nil))
         (closes (make-bool-vector (1+ len) nil))
         (pos 0)
         (result nil))
    (dotimes (i (1+ len))
      (when (vulpea-mentions--boundary-p line i)
        (aset opens i (and (< i len)
                           (not (memq (aref line i) '(?\s ?\t)))))
        (aset closes i (and (> i 0)
                            (not (memq (aref line (1- i)) '(?\s ?\t)))))))
    (while (< pos len)
      (let ((end (and (aref opens pos)
                      (cl-loop for end downfrom (min len (+ pos max-length))
                               above pos
                               when (and (aref closes end)
                                         (gethash (substring line pos end) dict))
                               return end))))
        (if (not end)
            (setq pos (1+ pos))
          (push (substring line pos end) result)
          (setq pos end))))
    (delete-dups (nreverse result))))

;;; Prose

;; A title is a mention only where it is prose.  As a heading tag, a
;; todo keyword, a date in a timestamp, or anywhere in code it is Org
;; syntax, and a link written in its place would break what is there.
;; Telling the two apart takes the Org parser, so the questions below
;; are asked in an Org buffer: the buffer a mention is linked in, or a
;; scratch copy of the text a search read.

(defconst vulpea-mentions--prose-types
  '(paragraph headline inlinetask item table-cell verse-block
    bold italic underline strike-through footnote-reference)
  "Org element and object types whose text can be prose.

A heading is prose in its title only, an item in its tag, and a
footnote reference in the definition it carries inline.  The text of
any other type is Org syntax: timestamps, planning and clock lines,
links and targets, code and verbatim, source and example blocks,
comments, macros, subscripts, LaTeX, citations.")

(defvar vulpea-mentions--state-note-re nil
  "The regexp matching a state note, with the heading it was built from.
A cons (HEADING . REGEXP) kept by the function
`vulpea-mentions--state-note-re'.")

(defun vulpea-mentions--state-note-re ()
  "Return the regexp matching a line that records a state change.

Org writes such a line into the log of a heading, shaped by the
`state' entry of `org-log-note-headings':

    - State \"DONE\"       from \"TODO\"       [2024-01-15 Mon 10:00]

The todo keywords on it are no more prose than the one in the heading.
Return nil when the entry is empty."
  (let ((heading (cdr (assq 'state org-log-note-headings))))
    (unless (and vulpea-mentions--state-note-re
                 (equal heading (car vulpea-mentions--state-note-re)))
      (setq vulpea-mentions--state-note-re
            (cons heading
                  (when (and (stringp heading)
                             (not (string-blank-p heading)))
                    ;; The regexp `org-skip-over-state-notes' builds,
                    ;; except that either state may be missing: Org
                    ;; leaves it empty when a heading had no keyword,
                    ;; or has none left.
                    (concat "[ \t]*[-+*] +"
                            (replace-regexp-in-string
                             " +" " +"
                             (org-replace-escapes
                              (regexp-quote heading)
                              `(("%d" . ,org-ts-regexp-inactive)
                                ("%D" . ,org-ts-regexp)
                                ("%s" . "\\(?:\"\\S-+\"\\)?")
                                ("%S" . "\\(?:\"\\S-+\"\\)?")
                                ("%t" . ,org-ts-regexp-inactive)
                                ("%T" . ,org-ts-regexp)
                                ("%u" . ".*?")
                                ("%U" . ".*?")))
                             t t))))))
    (cdr vulpea-mentions--state-note-re)))

(defun vulpea-mentions--heading-title-p (pos begin)
  "Return non-nil when POS is in the title of the heading at BEGIN.
The stars, the todo keyword, the priority and the tags are not the
title, and neither is the word that comments a heading out.  Nor is
anything past the first line: the END line of an inline task belongs
to the task, but is not its title."
  (save-excursion
    (goto-char begin)
    (let ((case-fold-search nil))
      (when (and (looking-at org-complex-heading-regexp)
                 (match-beginning 4))
        (let ((beg (match-beginning 4))
              (end (match-end 4)))
          (goto-char beg)
          (when (looking-at (concat (regexp-quote org-comment-string)
                                    "\\(?:[ \t]+\\|$\\)"))
            (setq beg (match-end 0)))
          (and (<= beg pos) (< pos end)))))))

(defvar-local vulpea-mentions--lexed nil
  "The objects lexed so far in the containers of a scratch buffer.
A hash table from the start of a container's contents to a vector
[OBJECTS TAIL NEXT DONE CURSOR CURSOR-POS]: the objects found in it, in
order, the last cons of that list, the position lexing goes on from,
whether it came to an end, and the first of the objects that does not
end before CURSOR-POS.  Nil outside the scratch buffers of the
searches, see `vulpea-mentions--context'.")

(defun vulpea-mentions--lexed-objects (element pos)
  "Return the objects of ELEMENT from the first that does not end before POS.

The objects are lexed up to the first one past POS and kept in
`vulpea-mentions--lexed', so a container is lexed once however many
positions in it are asked about, and the ones that end before POS are
passed over in one step when the positions come in order.  The buffer
is narrowed to the contents of ELEMENT."
  (let* ((key (org-element-property :contents-begin element))
         (entry (or (gethash key vulpea-mentions--lexed)
                    (puthash key (vector nil nil key nil nil key)
                             vulpea-mentions--lexed)))
         (restriction (org-element-restriction (org-element-type element))))
    (while (and (not (aref entry 3))
                (or (null (aref entry 1))
                    (<= (org-element-property :begin (car (aref entry 1)))
                        pos)))
      (goto-char (aref entry 2))
      (let ((next (org-element--object-lex restriction)))
        (if (not next)
            (aset entry 3 t)
          (org-element-put-property next :parent element)
          (let ((cell (list next)))
            (if (aref entry 1)
                (setcdr (aref entry 1) cell)
              (aset entry 0 cell))
            (aset entry 1 cell))
          ;; An object that ends where lexing started is empty, like the
          ;; last cell of a table row at the end of the buffer.  Lexed
          ;; again from there it comes back again, so it is the last.
          (if (> (org-element-property :end next) (aref entry 2))
              (aset entry 2 (org-element-property :end next))
            (aset entry 3 t)))))
    (let ((objects (if (and (aref entry 4) (>= pos (aref entry 5)))
                       (aref entry 4)
                     (aref entry 0))))
      (while (and objects (< (org-element-property :end (car objects)) pos))
        (setq objects (cdr objects)))
      (aset entry 4 objects)
      (aset entry 5 pos)
      objects)))

(defun vulpea-mentions--context (element)
  "Return the smallest element or object around point.

This is `org-element-context' given ELEMENT, the element at point.  In
the scratch buffer of a search (see `vulpea-mentions--lexed') the
objects of a paragraph, a verse block or a table row are lexed once,
and the choice among them is made the way `org-element-context' makes
it.  That function lexes from the start of the container every time,
which turns quadratic for a long paragraph that holds a title on many
lines."
  (let ((pos (point))
        (type (org-element-type element))
        (post (org-element-property :post-affiliated element))
        (cbeg (org-element-property :contents-begin element))
        (cend (org-element-property :contents-end element)))
    (cond
     ((or (null vulpea-mentions--lexed)
          (not (memq type '(paragraph table-row verse-block)))
          (and post (< pos post)))
      (org-element-context element))
     ((not (and cbeg cend (>= pos cbeg)
                (or (< pos cend) (and (= pos cend) (eobp)))))
      element)
     (t
      (save-excursion
        (save-restriction
          (narrow-to-region cbeg cend)
          (let ((objects (vulpea-mentions--lexed-objects element pos))
                (cached t)
                (restriction nil)
                (parent element)
                (last nil))
            (goto-char cbeg)
            ;; The loop of `org-element-context', fed from the objects
            ;; lexed before until it moves into one of them.
            (catch 'exit
              (while t
                (let ((next (if cached
                                (pop objects)
                              (org-element--object-lex restriction))))
                  ;; Objects from the cache know their parent already.
                  (when (and next (not cached))
                    (org-element-put-property next :parent parent))
                  (if (or (not next) (> (org-element-property :begin next) pos))
                      (throw 'exit (or last parent))
                    (let ((end (org-element-property :end next))
                          (next-cbeg
                           (org-element-property :contents-begin next))
                          (next-cend (org-element-property :contents-end next)))
                      (cond
                       ((and (<= end pos) (/= (point-max) end))
                        (goto-char end)
                        (when (and (= end pos)
                                   (not (memq (char-before) '(?\s ?\t))))
                          (setq last next)))
                       ((and next-cbeg next-cend
                             (>= pos next-cbeg)
                             (or (< pos next-cend)
                                 (and (= pos next-cend)
                                      (or (= (point-max) pos)
                                          (not (memq (char-before pos)
                                                     '(?\s ?\t)))))))
                        (goto-char next-cbeg)
                        (narrow-to-region (point) next-cend)
                        (setq parent next
                              cached nil
                              restriction (org-element-restriction next)))
                       (t (throw 'exit next)))))))))))))))

(defun vulpea-mentions--prose-p (pos)
  "Return non-nil when the text at POS is prose.

The current buffer has to be in Org mode: its parser decides, and it
reads the whole buffer whatever the restriction.  Text is prose when
its type is one of `vulpea-mentions--prose-types' and so is every
object around it, which leaves out a link's description, also when the
link wraps across lines or has brackets in it.

Generated content is not prose either: a dynamic block or the results
of a source block are rewritten the next time they are updated (see
`vulpea-db--generated-element-p').  Nor is the line that records a
state change, see the function `vulpea-mentions--state-note-re'."
  (save-excursion
    (save-restriction
      ;; A restriction may cut a line in two, and the half left of it
      ;; says what the line is: the stars of a heading, for one.
      (widen)
      (goto-char pos)
      (unless (when-let* ((re (vulpea-mentions--state-note-re)))
                (save-excursion
                  (forward-line 0)
                  (looking-at-p re)))
        (let* ((context (vulpea-mentions--context (org-element-at-point)))
               (type (org-element-type context)))
          (and (memq type vulpea-mentions--prose-types)
               (pcase type
                 ((or 'headline 'inlinetask)
                  (vulpea-mentions--heading-title-p
                   pos (org-element-property :begin context)))
                 ('footnote-reference
                  (let ((beg (org-element-property :contents-begin context))
                        (end (org-element-property :contents-end context)))
                    (and beg end (<= beg pos) (< pos end))))
                 (_ t))
               (not (seq-some
                     (lambda (node)
                       (let ((type (org-element-type node)))
                         (and (memq type org-element-all-objects)
                              (not (memq type vulpea-mentions--prose-types)))))
                     (org-element-lineage context)))
               (not (vulpea-db--inside-generated-p context))))))))

(defun vulpea-mentions--line-mentions (terms)
  "Return the mentions of TERMS on the line at point as (BEG . END) pairs.

This is what a mention is, for the searches and for
`vulpea-mentions-link' alike: an occurrence of a term that stands on
its own, outside any link (see `vulpea-mentions--occurrences'), in
prose (see `vulpea-mentions--prose-p'), on a line that is not an Org
metadata line.  The current buffer has to be in Org mode.

The line is read whole, also when the restriction cuts it, so the part
of a word inside the restriction is not taken for a word of its own.
Only mentions inside the restriction are returned.

The pairs are buffer positions, in order.  Where two occurrences
overlap, the one that starts first wins, and of two that start
together the longer one: an alias \"Emacs Lisp\" is not cut short by
the title \"Emacs\"."
  (let ((low (point-min))
        (high (point-max))
        (spans nil)
        (result nil))
    (save-restriction
      (widen)
      (let* ((bol (line-beginning-position))
             (line (buffer-substring-no-properties bol (line-end-position)))
             (end bol))
        (unless (vulpea-mentions--metadata-line-p line)
          (dolist (term terms)
            (dolist (occurrence (vulpea-mentions--occurrences line term))
              (when (vulpea-mentions--mention-p occurrence)
                (push (cons (+ bol (car occurrence)) (+ bol (cadr occurrence)))
                      spans))))
          (pcase-dolist (`(,beg . ,span-end)
                         (sort spans (lambda (a b)
                                       (or (< (car a) (car b))
                                           (and (= (car a) (car b))
                                                (> (cdr a) (cdr b)))))))
            (when (and (>= beg end)
                       (<= low beg)
                       (<= span-end high)
                       (vulpea-mentions--prose-p beg))
              (push (cons beg span-end) result)
              (setq end span-end))))))
    (nreverse result)))

(defun vulpea-mentions--goto-line (line from)
  "Move from the beginning of line FROM to the beginning of LINE.
Return non-nil when there is such a line.  Lines are those of the
accessible portion of the buffer."
  (and (> line 0)
       (zerop (forward-line (- line from)))
       ;; Running into the end of a last line that has no newline
       ;; counts as a line moved, and leaves point at the end of it.
       (bolp)))

(defun vulpea-mentions--scratch-fill (fill)
  "Make the current scratch buffer hold a note in Org mode.

FILL is called with no arguments to insert the text of the note into
the buffer, which is empty by then.

Org mode is turned on while the buffer is still empty.  Turned on over
the text, it reads all of it for things the parser has no use for,
macro templates above all: on a note of 2 MB that takes 70 ms where
the rest takes 10.  What the parser does need from the text is read
afterwards: the keywords the note declares, its todo keywords among
them, and its radio targets if it has any.

Mode hooks do not run and nothing Org does on startup happens, the
agenda menu is not rebuilt either: the buffer is only parsed.  A setup
file on another machine is read only when it was already marked as
safe, as there is nobody to ask.

The caller binds `org-element-use-cache' to t around this and around
its questions: with the cache on, asking about one position after
another costs one pass over the text, without it one pass each."
  (let ((org-inhibit-startup t)
        (org-resource-download-policy 'safe)
        (org-agenda-file-menu-enabled nil)
        (inhibit-message t))
    (let ((inhibit-modification-hooks t))
      (erase-buffer))
    (delay-mode-hooks (org-mode))
    (let ((inhibit-modification-hooks t))
      (funcall fill))
    ;; The text went in behind the back of the cache, and reading the
    ;; keywords and radio targets below asks the parser already.
    (org-element-cache-reset)
    (org-set-regexps-and-options)
    (goto-char (point-min))
    (when (search-forward "<<<" nil t)
      (org-update-radio-target-regexp))
    (org-element-cache-reset)
    (setq vulpea-mentions--lexed (make-hash-table :test 'eql))))

(defun vulpea-mentions--scratch-file (path)
  "Make the current scratch buffer hold the file PATH in Org mode.
Return nil when the file cannot be read.  Each file gets Org mode set
up anew, so the todo keywords it declares are its own, and the
buffer's directory is that of the file, where a relative #+SETUPFILE
is looked for."
  (condition-case nil
      (progn
        (setq default-directory
              (file-name-directory (expand-file-name path)))
        (vulpea-mentions--scratch-fill
         (lambda () (insert-file-contents path)))
        (goto-char (point-min))
        t)
    (error nil)))

(defun vulpea-mentions--scratch-line ()
  "Return the text of the line at point, as ripgrep reports it.
That is without the carriage return a file with mixed line endings
leaves at the end of some lines."
  (string-trim-right
   (buffer-substring-no-properties (point) (line-end-position))
   "\r+"))

(defun vulpea-mentions--scratch-links-to-p (id hits)
  "Return non-nil when one of HITS has an Org link to the note ID.

HITS are ripgrep hits on lines where the text of a bracket link to ID
was found, and the current buffer is the scratch copy of their file.
The Org parser has the last word: the text of a link in a comment or
a source block is no link, and a link in generated content does not
count either, as the database leaves those out too.  A hit whose line
no longer reads the way ripgrep saw it does not count, nor does one
the parser gives up on: unconfirmed, the file is left to the database.
Point is left at the start of the buffer."
  (let ((needle (concat "[[id:" id "]"))
        (case-fold-search nil))
    (prog1
        (seq-some
         (lambda (hit)
           (goto-char (point-min))
           (condition-case nil
               (if (not (and (vulpea-mentions--goto-line
                              (plist-get hit :line) 1)
                             (equal (plist-get hit :line-text)
                                    (vulpea-mentions--scratch-line))))
                   nil
                 (let ((eol (line-end-position))
                       (found nil))
                   (while (and (not found) (search-forward needle eol t))
                     (let ((link (save-excursion
                                   (goto-char (match-beginning 0))
                                   (org-element-context))))
                       (setq found
                             (and (eq (org-element-type link) 'link)
                                  (equal (org-element-property :type link) "id")
                                  (equal (org-element-property :path link) id)
                                  (not (vulpea-db--inside-generated-p link))))))
                   found))
             (error nil)))
         hits)
      (goto-char (point-min)))))

(defun vulpea-mentions--keep-prose (candidates terms &optional id links)
  "Return the mentions of CANDIDATES that hold one of TERMS in prose.

CANDIDATES is a list of (HIT . MENTION): a ripgrep hit that passed
every other test, and the mention to report for it.  Only the Org
parser can tell whether the title on its line is prose (see
`vulpea-mentions--line-mentions'), so the file is read into a scratch
buffer, once: ripgrep reports the hits of a file together.

LINKS, when given, maps the path of a file to the ripgrep hits where
the text of a bracket link to the note ID was found in it.  A file
whose text has such a link that the parser confirms (see
`vulpea-mentions--scratch-links-to-p') already links to the note, and
none of its mentions is returned.

A hit that cannot be read there is kept as ripgrep reported it: its
file is gone, its line no longer reads the way ripgrep saw it, or the
parser gave up on it.  One such file must not take down the rest of
the search, and `vulpea-mentions-link' looks at the line again before
it writes."
  (when candidates
    (let ((org-element-use-cache t)
          (org-element-cache-persistent nil)
          (loaded nil)
          (readable nil)
          (linked nil)
          (line 1)
          (result nil))
      (with-temp-buffer
        (pcase-dolist (`(,hit . ,mention) candidates)
          (let ((path (plist-get hit :path))
                (line-no (plist-get hit :line)))
            (unless (equal path loaded)
              (setq loaded path
                    readable (vulpea-mentions--scratch-file path)
                    line 1
                    linked (when-let* ((link-hits (and links readable
                                                       (gethash path links))))
                             (vulpea-mentions--scratch-links-to-p
                              id link-hits))))
            (when (and (not linked)
                       (or (not readable)
                           (not (if (vulpea-mentions--goto-line line-no line)
                                    (setq line line-no)
                                  (goto-char (point-min))
                                  (setq line 1)
                                  nil))
                           (not (equal (plist-get hit :line-text)
                                       (vulpea-mentions--scratch-line)))
                           (condition-case nil
                               (vulpea-mentions--line-mentions terms)
                             (error t))))
              (push mention result)))))
      (nreverse result))))

(defun vulpea-mentions--file-notes (path cache)
  "Return the notes of file PATH, memoized in the CACHE hash table.

The result (possibly nil, also cached) is shared by
`vulpea-mentions--file-note' and `vulpea-mentions--file-note-ids', so a
mentioning file is queried once no matter how many hits it has."
  (let ((cached (gethash path cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (puthash path (vulpea-db-query-by-file-path path) cache))))

(defun vulpea-mentions--file-note (path cache)
  "Return a note representing file PATH, memoized in the CACHE hash table.

Prefers the file-level note; falls back to the first note in the file.
Returns nil when PATH holds no indexed note."
  (let ((notes (vulpea-mentions--file-notes path cache)))
    (or (seq-find (lambda (n) (= (vulpea-note-level n) 0)) notes)
        (car notes))))

(defun vulpea-mentions--file-note-ids (path cache)
  "Return the ids of every note in file PATH.

CACHE is the hash table used by `vulpea-mentions--file-notes'."
  (mapcar #'vulpea-note-id (vulpea-mentions--file-notes path cache)))

(defun vulpea-mentions--ignored-file-p (path cache ignored-ids)
  "Return non-nil when an id of a note in file PATH is in IGNORED-IDS.

CACHE is the hash table used by `vulpea-mentions--file-notes'.  Any id
of the file counts, so a heading id silences it just like a file-level
one does."
  (unless (zerop (hash-table-count ignored-ids))
    (seq-some (lambda (id) (gethash id ignored-ids))
              (vulpea-mentions--file-note-ids path cache))))

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

(defun vulpea-mentions--text-links (hits id)
  "Return the HITS whose line has the text of a bracket link to ID.

The result is a hash table from the path of a file to its hits, for
`vulpea-mentions--keep-prose' to confirm with the parser.  The
database only knows the links some note owns.  In a file without a
file-level id, text that no heading with an id encloses belongs to no
note, and a link written there is known to ripgrep alone."
  (let ((needle (concat "[[id:" id "]"))
        (result (make-hash-table :test 'equal)))
    (dolist (hit hits)
      (when (string-search needle (plist-get hit :line-text))
        (push hit (gethash (plist-get hit :path) result))))
    (maphash (lambda (path hits) (puthash path (nreverse hits) result))
             result)
    result))

(defun vulpea-mentions--ignore-mention-ids (note)
  "Return note ids that mentions from them are ignored by NOTE in a list."
  (when-let* ((properties (vulpea-note-properties note))
              (ignore-mentions
               (assoc (upcase vulpea-mentions-per-note-ignore-property-key)
                      properties))
              (ignored-ids (split-string (cdr ignore-mentions))))
    ignored-ids))

(defun vulpea-mentions--ignore-mention-ids-table (note)
  "Return note ids that mentions from them are ignored by NOTE in a table."
  (let ((result (make-hash-table :test 'equal))
        (ignored-ids (vulpea-mentions--ignore-mention-ids note)))
    (mapc (lambda (id) (puthash id t result)) ignored-ids)
    result))

(defun vulpea-mentions-ignore-from (note-or-id from-note-or-id)
  "Silence mentions of NOTE-OR-ID coming from FROM-NOTE-OR-ID.

Adds an id from FROM-NOTE-OR-ID, which is determined by
`vulpea-mentions--file-note', to
`vulpea-mentions-per-note-ignore-property-key' in NOTE-OR-ID's property
drawer, saves the file and syncs the database."
  (let* ((note (vulpea-utils-ensure-note note-or-id))
         (from-note (vulpea-utils-ensure-note from-note-or-id))
         (from-file-note (vulpea-mentions--file-note
                          (vulpea-note-path from-note)
                          (make-hash-table :test 'equal))))
    (vulpea-utils-with-note-sync note
      (org-entry-add-to-multivalued-property
       (point)
       vulpea-mentions-per-note-ignore-property-key
       (vulpea-note-id from-file-note)))))

(defun vulpea-mentions-unignore-from (note-or-id from-note-or-id)
  "Show mentions of NOTE-OR-ID coming from FROM-NOTE-OR-ID again.

Removes each id appears in FROM-NOTE-OR-ID's note file from
`vulpea-mentions-per-note-ignore-property-key' in NOTE-OR-ID's property
drawer, dropping the property once nothing is left, saves the file and
syncs the database."
  (let* ((note (vulpea-utils-ensure-note note-or-id))
         (from-note (vulpea-utils-ensure-note from-note-or-id))
         (note-ids (vulpea-mentions--file-note-ids
                    (vulpea-note-path from-note)
                    (make-hash-table :test 'equal))))
    (vulpea-utils-with-note-sync note
      (dolist (id note-ids)
        (org-entry-remove-from-multivalued-property
         (point)
         vulpea-mentions-per-note-ignore-property-key
         id))
      ;; Clean up the property line
      (when (null (org-entry-get-multivalued-property
                   (point)
                   vulpea-mentions-per-note-ignore-property-key))
        (org-delete-property vulpea-mentions-per-note-ignore-property-key)))))

(defun vulpea-mentions-ignored-notes (note)
  "Return the notes whose mentions are ignored by NOTE.

Each id in `vulpea-mentions-per-note-ignore-property-key' is resolved
to the note a mention from its file is attributed to: the file-level
note, or the first note in the file when there is none.  Two ids of one
file therefore yield one note.  Ids that no longer belong to any note
are dropped."
  (let* ((cache (make-hash-table :test 'equal))
         (ignored-ids (vulpea-mentions--ignore-mention-ids note))
         (ignored-notes (vulpea-db-query-by-ids ignored-ids))
         (result-table (make-hash-table :test 'equal))
         (result '()))
    (dolist (ignored-note ignored-notes)
      (let ((resolved-note (vulpea-mentions--file-note
                            (vulpea-note-path ignored-note)
                            cache)))
        (puthash resolved-note t result-table)))
    (maphash (lambda (key _) (push key result))
             result-table)
    result))

(defun vulpea-mentions--collect (output note own-path)
  "Collect unlinked mentions of NOTE from ripgrep OUTPUT.

OWN-PATH is NOTE's own expanded file path, whose hits are skipped.  Hits
whose mentioning note shares a name with NOTE (a title collision) are
skipped too.  Hits in a file that holds at least one explicit link to
NOTE are skipped as well: a link the database knows, or one ripgrep
found in the text and the parser confirms (see
`vulpea-mentions--text-links').
Set `vulpea-mentions-exclude-linked' to nil to disable this behavior.
Hits are skipped as well when NOTE ignores any note of the mentioning
file through `vulpea-mentions-per-note-ignore-property-key', so which
id of that file was written there does not matter.  The hits left are
read in their files, and those whose title is not prose there are
dropped, see `vulpea-mentions--keep-prose'.  Returns a list of plists
with :note (the mentioning note), :path, :line, and :context."
  (let* ((terms (vulpea-mentions--note-terms note))
         (path->notes (make-hash-table :test 'equal))
         (hits (vulpea-mentions--parse-rg-json output))
         (paths-link-to-note
          (when (and vulpea-mentions-exclude-linked hits)
            (vulpea-mentions--paths-link-to-note note)))
         (text-links
          (when (and vulpea-mentions-exclude-linked hits)
            (vulpea-mentions--text-links hits (vulpea-note-id note))))
         (ignore-mention-ids (vulpea-mentions--ignore-mention-ids-table note))
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
          (let ((mentioning (vulpea-mentions--file-note path path->notes)))
            (when (and mentioning
                       (not (vulpea-mentions--shares-name-p mentioning terms))
                       (not (vulpea-mentions--ignored-file-p
                             path path->notes ignore-mention-ids)))
              (push (cons hit
                          (list :note mentioning
                                :path path
                                :line (plist-get hit :line)
                                :context (string-trim line-text)))
                    result))))))
    ;; Reading a file is the expensive test, so it comes last, for the
    ;; hits nothing else has turned down.
    (vulpea-mentions--keep-prose
     (nreverse result) terms (vulpea-note-id note) text-links)))

;;; Outgoing: mentions made by a buffer

(defun vulpea-mentions--title-dictionary ()
  "Return (DICT . TERMS) describing the candidate notes' names.

Candidates are the notes kept by `vulpea-mentions-note-filter', minus
those opting out via `vulpea-mentions--ignore-note-p'.  DICT is a hash
table mapping a case-folded title or alias (see `vulpea-mentions--fold')
to the list of note ids that bear it.  TERMS lists its keys, which are
the terms to search for, longest first: ripgrep reports the first
pattern that fits at a position, and a title must not lose its place
to a shorter one it starts with."
  (let ((dict (make-hash-table :test 'equal))
        (names nil)
        (ids nil)
        (filter (lambda (note)
                  (and (not (vulpea-mentions--ignore-note-p note))
                       (funcall vulpea-mentions-note-filter note)))))
    (dolist (note (vulpea-db-query filter))
      (let ((id (vulpea-note-id note)))
        (dolist (name (cons (vulpea-note-title note) (vulpea-note-aliases note)))
          (when (stringp name)
            (let ((trimmed (string-trim name)))
              ;; The names are folded together, one per line, so a
              ;; name spanning lines stays out.  It could not be
              ;; found anyway: the search goes line by line.
              (when (and (> (length trimmed) 0)
                         (>= (string-width trimmed)
                             vulpea-mentions-min-term-length)
                         (not (string-search "\n" trimmed)))
                (push trimmed names)
                (push id ids)))))))
    (setq names (nreverse names)
          ids (nreverse ids))
    (cl-loop for term in (split-string
                          (vulpea-mentions--fold (string-join names "\n"))
                          "\n")
             for id in ids
             do (push id (gethash term dict)))
    (cons dict (sort (hash-table-keys dict)
                     (lambda (a b) (> (length a) (length b)))))))

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
  (let ((ignore-mentions-id (vulpea-mentions--ignore-mention-ids-table note)))
    (seq-some (lambda (id) (gethash id ignore-mentions-id)) ids)))

(defun vulpea-mentions--line-reader (text)
  "Return a function mapping a line number to that line of TEXT.

Lines are numbered from 1, and the function returns nil past the last
one.  It remembers where it stopped, so reading lines in increasing
order, which is how ripgrep reports them, costs a single pass over
TEXT."
  (let ((line 1)
        (pos 0))
    (lambda (n)
      (when (or (null pos) (< n line))
        (setq line 1
              pos 0))
      (while (and pos (< line n))
        (setq pos (when-let* ((eol (string-search "\n" text pos)))
                    (1+ eol))
              line (1+ line)))
      (when pos
        (substring text pos (string-search "\n" text pos))))))

(defun vulpea-mentions--collect-outgoing (output text dict self-ids linked-ids
                                                &optional before after
                                                directory)
  "Collect outgoing unlinked mentions from ripgrep OUTPUT over one buffer.

TEXT is the text of the buffer.  OUTPUT comes from searching the
case-folded copy of it (see `vulpea-mentions--fold') for case-folded
terms, so the lines and matches it quotes are folded as well.  What a
mention quotes is read back from TEXT, where a line keeps its number
and a match its position.

DICT maps a case-folded title/alias to candidate note ids (see
`vulpea-mentions--title-dictionary').  SELF-IDS are the note ids in the
buffer's own file, excluded as candidates.  If one of SELF-IDS are
explicitly ignored by the mentioned note, then the mention will be
dropped (see `vulpea-mentions-per-note-ignore-property-key').
LINKED-IDS is a hash table keyed by the note ids the buffer already
links to (see `vulpea-mentions--buffer-link-ids'); candidates found in
it are dropped.  Pass an empty table to keep them all.

An occurrence that is not prose is dropped as well (see
`vulpea-mentions--prose-p'), which covers one inside a link that wraps
across lines.  That is read from a scratch copy of the buffer in Org
mode, made when the first occurrence that passed every other test asks
for it.  BEFORE and AFTER are the text of the buffer around TEXT when
TEXT is only the accessible portion of it: what a line is depends on
the lines around it, a block that opens above the restriction or the
todo keywords the file declares at its top, and a restriction may cut
a word in two.  DIRECTORY is the directory of the buffer, where a
relative #+SETUPFILE is looked for.  An occurrence the parser gives up
on is kept.

Ripgrep matches plain strings, so it also reports \"red wine\" inside
\"red wines\".  Such a glued match is dropped here.  But ripgrep's
matches never overlap: the glued one has already used up the text of
anything starting inside it (\"red\", a mention in its own right), and
the search went on from the middle of a word.  Only a match of several
words can do that, and a line holding one is read again by
`vulpea-mentions--line-terms'.

Returns a list of plists with :note (a candidate note to link to),
:line, :context, and :matched (the text that matched)."
  (let ((id->note (make-hash-table :test 'equal))
        ;; The ignore check walks every one of SELF-IDS, and a candidate
        ;; can be hit on hundreds of lines: decide it once per candidate.
        (id->ignored (make-hash-table :test 'equal))
        (term->notes (make-hash-table :test 'equal))
        (line-at (vulpea-mentions--line-reader text))
        (org-element-use-cache t)
        (org-element-cache-persistent nil)
        (scratch nil)
        (scratch-line 1)
        (longest nil)
        (result nil))
    (cl-flet* ((resolve-note (id)
                 (let ((cached (gethash id id->note 'miss)))
                   (if (not (eq cached 'miss)) cached
                     (puthash id (vulpea-db-get-by-id id) id->note))))
               (ignored-p (id cand)
                 (let ((cached (gethash id id->ignored 'miss)))
                   (if (not (eq cached 'miss)) cached
                     (puthash id
                              (vulpea-mentions--ignored-by-note-p self-ids cand)
                              id->ignored))))
               ;; The notes TERM may be a mention of.  Most lines of a
               ;; long note repeat a few terms, and a term without any
               ;; spares its occurrences the parser.
               (candidates (term)
                 (let ((cached (gethash term term->notes 'miss)))
                   (if (not (eq cached 'miss)) cached
                     (puthash
                      term
                      (delq nil
                            (mapcar
                             (lambda (id)
                               (unless (or (member id self-ids)
                                           (gethash id linked-ids))
                                 (when-let* ((cand (resolve-note id)))
                                   (unless (ignored-p id cand)
                                     cand))))
                             (gethash term dict)))
                      term->notes))))
               ;; Whether the occurrence from column BEG to END of line
               ;; LINE-NO of TEXT is a mention: a word of its own also
               ;; where the restriction cuts the line, and prose.
               (mention-p (line-no beg end)
                 (condition-case nil
                     (progn
                       (unless scratch
                         ;; Marked as failed before it is set up, so a
                         ;; copy that breaks half way is never asked.
                         (setq scratch 'failed)
                         (let ((buffer (generate-new-buffer
                                        " *vulpea-mentions-prose*" t)))
                           (unwind-protect
                               (with-current-buffer buffer
                                 (when directory
                                   (setq default-directory directory))
                                 (vulpea-mentions--scratch-fill
                                  (lambda ()
                                    (insert (or before "") text
                                            (or after ""))))
                                 (goto-char (1+ (length before)))
                                 (setq scratch buffer))
                             (unless (eq scratch buffer)
                               (kill-buffer buffer)))))
                       (when (eq scratch 'failed)
                         (error "No scratch copy"))
                       (with-current-buffer scratch
                         ;; Point rests where the line starts in TEXT,
                         ;; which for its first line may be the middle
                         ;; of a line of the buffer.  Ripgrep reports
                         ;; lines in order, so point moves forward.
                         (cond
                          ((> line-no scratch-line)
                           (forward-line (- line-no scratch-line)))
                          ((< line-no scratch-line)
                           (goto-char (1+ (length before)))
                           (forward-line (1- line-no))))
                         (setq scratch-line line-no)
                         (let ((beg (+ (point) beg))
                               (end (+ (point) end)))
                           (and (not (vulpea-mentions--glued-p
                                      (char-after beg) (char-before beg)))
                                (not (vulpea-mentions--glued-p
                                      (char-before end) (char-after end)))
                                (vulpea-mentions--prose-p beg)))))
                   (error t)))
               (max-length ()
                 (or longest
                     (progn
                       (setq longest 0)
                       (maphash (lambda (term _)
                                  (setq longest (max longest (length term))))
                                dict)
                       longest))))
      (unwind-protect
          (dolist (hit (vulpea-mentions--parse-rg-json output))
            (let* ((line-no (plist-get hit :line))
                   (folded (plist-get hit :line-text))
                   (line (funcall line-at line-no)))
              (unless (vulpea-mentions--metadata-line-p line)
                (let* ((occurrences (lambda (term)
                                      (cons term (vulpea-mentions--occurrences
                                                  folded term))))
                       (found (mapcar occurrences
                                      (seq-uniq (plist-get hit :matched)))))
                  (when (seq-some (pcase-lambda (`(,term . ,spans))
                                    (and (seq-some #'cl-third spans)
                                         (vulpea-mentions--compound-p term)))
                                  found)
                    (setq found (mapcar occurrences
                                        (vulpea-mentions--line-terms
                                         folded dict (max-length)))))
                  (pcase-dolist (`(,term . ,spans) found)
                    (when-let* ((notes (candidates term))
                                (span (seq-find
                                       (lambda (span)
                                         (and (vulpea-mentions--mention-p span)
                                              (mention-p line-no
                                                         (car span)
                                                         (cadr span))))
                                       spans))
                                (matched (substring line
                                                    (car span) (cadr span))))
                      (dolist (cand notes)
                        (push (list :note cand :line line-no
                                    :context (string-trim line)
                                    :matched matched)
                              result))))))))
        (when (buffer-live-p scratch)
          (kill-buffer scratch))))
    (nreverse result)))

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

A title counts where it is prose.  As a heading tag, a todo keyword or
a date in a timestamp, in code, a source or example block, or a
comment, it is Org syntax, and a link in its place would break what is
there.  In generated content, a dynamic block or the results of a
source block, a link would be lost on the next update.  Such an
occurrence is not reported, so `vulpea-mentions-link' can link every
line reported as long as the file has not changed since.  Only the Org
parser can tell prose from syntax, so the files that still have hits
after every other test are read.

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
             :command (vulpea-mentions--rg-command
                       rg terms dirs
                       (and vulpea-mentions-exclude-linked
                            (vulpea-note-id note)))
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
               (let ((status (process-status proc)))
                 (when (memq status '(exit signal))
                   (let ((code (process-exit-status proc)))
                     ;; rg exits 0 with matches, 1 with none, >1 on
                     ;; error.  For a signalled process the exit status
                     ;; is the signal number, and SIGHUP is 1: only an
                     ;; actual exit counts.
                     (cond
                      ((and (eq status 'exit) (memq code '(0 1)))
                       (condition-case err
                           (funcall resolve
                                    (vulpea-mentions--collect
                                     output note own-path))
                         (error (funcall reject (error-message-string err)))))
                      ((eq status 'exit)
                       (funcall reject
                                (format "ripgrep failed (exit %s)" code)))
                      (t
                       (funcall reject
                                (format "ripgrep killed (signal %s)"
                                        code))))))))))))))))

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
buffer's live content is searched, so unsaved edits are included -
both for the mentions and for the links that exclude them.

A title counts where it is prose, as it does for
`vulpea-note-unlinked-mentions-async': not as a heading tag, a todo
keyword or a date in a timestamp, not in code, a source or example
block, a comment or generated content.  The text is parsed as Org for
that, whatever the mode of the buffer, and as a part of the whole
buffer when the buffer is narrowed.  In an Org buffer
`vulpea-mentions-link' can link every line reported, and :matched
quotes the first mention of the term on that line.

The accessible portion of the buffer is snapshotted to a temporary file
for ripgrep to read, so the call returns as soon as the process is
started.  A buffer visiting an encrypted note (any extension other
than .org, see `vulpea-mentions--snapshot-buffer-p') is fed to
ripgrep over a pipe instead, so its decrypted content never touches
the disk; for such a buffer the call blocks until ripgrep has
consumed the text.

Case is erased from that copy and from the names before ripgrep sees
them (see `vulpea-mentions--fold'), so it only has plain strings to
match.  That keeps the search fast with tens of thousands of names,
and its time hardly depends on the size of the buffer.

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
      (let* ((file (and buffer-file-name (expand-file-name buffer-file-name)))
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
          ;; Three resources back the search: the patterns file, the
          ;; snapshot of the buffer, and a buffer for ripgrep's JSON
          ;; output (concatenating each chunk onto a string copied
          ;; everything received so far on every chunk).  One cleanup
          ;; serves both the sentinel and a setup that fails part way.
          ;;
          ;; Ripgrep reads the folded text; the text as written stays
          ;; here, for the sentinel to quote mentions from and to read
          ;; as Org.
          (let* ((snapshot (vulpea-mentions--snapshot-buffer-p))
                 (beg (point-min))
                 (end (point-max))
                 (text (buffer-substring-no-properties beg end))
                 ;; What a line is, prose or not, depends on the lines
                 ;; around it, so the sentinel gets the rest of a
                 ;; narrowed buffer as well.
                 (before (when (buffer-narrowed-p)
                           (save-restriction
                             (widen)
                             (buffer-substring-no-properties (point-min) beg))))
                 (after (when (buffer-narrowed-p)
                          (save-restriction
                            (widen)
                            (buffer-substring-no-properties end (point-max)))))
                 (directory default-directory)
                 folded patterns-file input-file output-buffer proc)
            (cl-flet ((cleanup ()
                        (when patterns-file
                          (ignore-errors (delete-file patterns-file)))
                        (when input-file
                          (ignore-errors (delete-file input-file)))
                        (when (buffer-live-p output-buffer)
                          (kill-buffer output-buffer))))
              (condition-case err
                  (progn
                    (setq folded (vulpea-mentions--fold text)
                          patterns-file (make-temp-file "vulpea-mentions-pat-")
                          input-file (when snapshot
                                       (make-temp-file "vulpea-mentions-in-"))
                          output-buffer (generate-new-buffer
                                         " *vulpea-mentions-out*" t))
                    ;; rg rejects a patterns file that is not valid
                    ;; UTF-8, and the snapshot must decode as UTF-8 on
                    ;; the way back out, so the user's
                    ;; `coding-system-for-write' must not leak in.
                    (let ((coding-system-for-write 'utf-8))
                      (with-temp-file patterns-file
                        (insert (string-join terms "\n") "\n"))
                      (when input-file
                        (write-region folded nil input-file nil 'silent)))
                    (setq proc
                          (make-process
                           :name "vulpea-mentions-out"
                           :buffer output-buffer
                           :command (vulpea-mentions--rg-file-command
                                     rg patterns-file (or input-file "-"))
                           :connection-type 'pipe
                           :noquery t
                           ;; Pin the JSON output to UTF-8 no matter what
                           ;; `default-process-coding-system' says.
                           :coding 'utf-8
                           :sentinel
                           (lambda (proc _event)
                             (let ((status (process-status proc)))
                               (when (memq status '(exit signal))
                                 (let ((code (process-exit-status proc))
                                       (output (with-current-buffer output-buffer
                                                 (buffer-string))))
                                   (cleanup)
                                   ;; rg exits 0 with matches, 1 with none, >1
                                   ;; on error.  For a signalled process the
                                   ;; exit status is the signal number, and
                                   ;; SIGHUP is 1: only an actual exit counts.
                                   (cond
                                    ((and (eq status 'exit) (memq code '(0 1)))
                                     (condition-case err
                                         (funcall resolve
                                                  (vulpea-mentions--collect-outgoing
                                                   output text dict self-ids linked-ids
                                                   before after directory))
                                       (error
                                        (funcall reject (error-message-string err)))))
                                    ((eq status 'exit)
                                     (funcall reject
                                              (format "ripgrep failed (exit %s)" code)))
                                    (t
                                     (funcall reject
                                              (format "ripgrep killed (signal %s)"
                                                      code))))))))))
                    (unless input-file
                      ;; An encrypted note goes over the pipe, and the
                      ;; send blocks the caller.  If it fails, ripgrep
                      ;; is gone (a broken pipe) or has to go; if the
                      ;; user quits while it blocks, ripgrep has to go
                      ;; too, or it waits on stdin forever.  Either way
                      ;; the sentinel settles the promise.
                      (let ((sent nil))
                        (unwind-protect
                            (condition-case nil
                                (progn
                                  (process-send-string proc folded)
                                  (process-send-eof proc)
                                  (setq sent t))
                              (error nil))
                          (unless sent
                            (when (process-live-p proc)
                              (delete-process proc))))))
                    proc)
                ;; The promise settles even when the search never
                ;; starts - a full TMPDIR, ripgrep gone since
                ;; `executable-find' saw it - and nothing is left behind.
                (error
                 (cleanup)
                 (funcall reject (error-message-string err))
                 nil))))))))))

;;; Linking a mention

;;;###autoload
(defun vulpea-mentions-link (buffer line note &optional context)
  "Turn the mentions of NOTE on LINE of BUFFER into links to it.

A mention is read the way `vulpea-note-unlinked-mentions-async' and
`vulpea-buffer-unlinked-mentions-async' read it: the title or an alias
of NOTE standing on its own, outside any link, in prose.  Every one on
the line is replaced with an `id:' link to NOTE that keeps the text as
written for its description.  Return the number of mentions linked.

LINE is counted from the start of the accessible portion of BUFFER,
which is how the search of a buffer numbers the lines it reports.  The
search for the mentions of a note numbers the lines of the file, so
widen the buffer before linking one of those.

CONTEXT, when given, is the :context a search reported for the line.
Nothing is linked unless the line still reads the same, so a line that
moved since the search does not get the link put on its neighbor.

Nothing is linked, and 0 returned, when there is no such line, when it
holds no mention of NOTE, or when BUFFER is not in Org mode: only the
Org parser can tell prose from syntax.

The buffer is edited and not saved.  The search for the mentions of a
note reads files, so it goes on listing the mention until the buffer
is saved."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((terms (vulpea-mentions--note-terms note))
            (count 0))
        (when (and terms
                   (derived-mode-p 'org-mode)
                   (vulpea-mentions--goto-line line 1)
                   (or (null context)
                       (string= context
                                (string-trim
                                 (buffer-substring-no-properties
                                  (point) (line-end-position))))))
          ;; The mentions are found before any of them is replaced,
          ;; and replaced from the end of the line: a link never moves
          ;; a mention still to be replaced, and is never read as one.
          (pcase-dolist (`(,beg . ,end)
                         (nreverse (vulpea-mentions--line-mentions terms)))
            (let ((text (buffer-substring-no-properties beg end)))
              ;; The link goes in before the text comes out, so a
              ;; position after the mention, or inside it, ends up
              ;; after the link.
              (goto-char beg)
              (insert (vulpea-utils-link-make-string note text))
              (delete-region (point) (+ (point) (- end beg)))
              (setq count (1+ count)))))
        count))))

(provide 'vulpea-mentions)
;;; vulpea-mentions.el ends here
