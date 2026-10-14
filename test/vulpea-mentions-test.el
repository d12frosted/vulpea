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

(defun vulpea-mentions-test--collect-incoming-mentions-for-note (id)
  "Return mentions for note specified by ID."
  (let* ((note (vulpea-db-get-by-id id))
         (path (vulpea-note-path note))
         (cmd (vulpea-mentions--rg-command
               (executable-find "rg")
               (vulpea-mentions--note-terms note)
               vulpea-db-sync-directories
               (and vulpea-mentions-exclude-linked (vulpea-note-id note))))
         (output (with-temp-buffer
                   (apply #'call-process (car cmd) nil t nil (cdr cmd))
                   (buffer-string)))
         (mentions (vulpea-mentions--collect
                    output note (expand-file-name path))))
    mentions))

(defun vulpea-mentions-test--collect-outgoing-mentions-for-note (note)
  "Return outgoing mentions for NOTE, searched in a buffer visiting its file.

NOTE can be either an ID or a `vulpea-note' object."
  (unless (vulpea-note-p note)
    (setq note (vulpea-db-get-by-id note)))
  (let ((path (expand-file-name (vulpea-note-path note))))
    (vulpea-mentions-test--outgoing
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))
     nil path)))

(defun vulpea-mentions-test--await (state-fn &optional timeout)
  "Pump process output until STATE-FN returns non-nil.

Give up after TIMEOUT seconds (default 10); the caller's assertions
then see whatever state the search reached."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall state-fn))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defun vulpea-mentions-test--outgoing (content &optional timeout file)
  "Return the outgoing mentions of a buffer holding CONTENT.

Runs `vulpea-buffer-unlinked-mentions-async' to completion, waiting up
to TIMEOUT seconds, and fails the calling test unless it resolved.
FILE, when given, is the file the buffer claims to visit."
  (with-temp-buffer
    (insert content)
    (let ((state nil)
          (result nil))
      (unwind-protect
          (progn
            (when file
              (setq buffer-file-name file)
              (set-buffer-modified-p nil))
            (vulpea-buffer-unlinked-mentions-async
             (lambda (ms) (setq state 'resolved result ms))
             (lambda (err) (setq state (list 'rejected err))))
            (vulpea-mentions-test--await (lambda () state) timeout))
        (setq buffer-file-name nil))
      (should (eq state 'resolved))
      result)))

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

(ert-deftest vulpea-mentions--note-terms-measures-width ()
  "The minimum length measures display width, so short CJK titles pass.
A CJK character is two columns wide, so 北京 (width 4) clears the
default minimum of 3 while a 2-letter Latin title does not."
  (let ((vulpea-mentions-min-term-length 3)
        (note (make-vulpea-note :title "北京" :aliases '("Ok"))))
    (should (equal (vulpea-mentions--note-terms note) '("北京")))))

(ert-deftest vulpea-mentions--title-dictionary-measures-width ()
  "The candidate dictionary applies the same width-based minimum."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "bj" "北京")
    (vulpea-test--insert-test-note "ok" "Ok")
    (let* ((dt (vulpea-mentions--title-dictionary))
           (dict (car dt))
           (terms (cdr dt)))
      (should (equal (gethash "北京" dict) '("bj")))
      (should-not (gethash "ok" dict))
      (should (equal terms '("北京"))))))

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
      (should (equal (plist-get (car hits) :line-text) "see Cabernet here"))
      (should (equal (plist-get (car hits) :matched) '("Cabernet"))))))

(ert-deftest vulpea-mentions--parse-rg-json-drops-bytes-values ()
  "Values ripgrep encodes as base64 bytes are dropped, not read as nil.
ripgrep emits {\"bytes\": ...} instead of {\"text\": ...} when a path,
line, or match is not valid UTF-8.  A bytes path or line drops the hit;
a bytes submatch is dropped from :matched while text ones survive."
  (let ((output (concat
                 ;; path is not valid UTF-8 -> hit dropped
                 "{\"type\":\"match\",\"data\":{\"path\":{\"bytes\":\"L24vY2Fm6S5vcmc=\"},"
                 "\"lines\":{\"text\":\"see Cabernet here\\n\"},\"line_number\":3,"
                 "\"submatches\":[{\"match\":{\"text\":\"Cabernet\"},\"start\":4,\"end\":12}]}}\n"
                 ;; line content is not valid UTF-8 -> hit dropped
                 "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/a.org\"},"
                 "\"lines\":{\"bytes\":\"Y2Fm6SBub3RlCg==\"},\"line_number\":5,"
                 "\"submatches\":[{\"match\":{\"text\":\"Cabernet\"},\"start\":0,\"end\":8}]}}\n"
                 ;; one submatch is bytes -> that submatch alone is dropped
                 "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/a.org\"},"
                 "\"lines\":{\"text\":\"Cabernet and more\\n\"},\"line_number\":7,"
                 "\"submatches\":[{\"match\":{\"bytes\":\"Q2Fm6Q==\"},\"start\":0,\"end\":4},"
                 "{\"match\":{\"text\":\"Cabernet\"},\"start\":0,\"end\":8}]}}\n")))
    (let ((hits (vulpea-mentions--parse-rg-json output)))
      (should (= (length hits) 1))
      (should (equal (plist-get (car hits) :path) "/n/a.org"))
      (should (equal (plist-get (car hits) :line) 7))
      (should (equal (plist-get (car hits) :matched) '("Cabernet"))))))

(ert-deftest vulpea-mentions--parse-rg-json-drops-the-line-ending ()
  "A line ripgrep quotes from a CRLF file loses its carriage return."
  (let* ((output (concat
                  "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/a.org\"},"
                  "\"lines\":{\"text\":\"a Cabernet\\r\\n\"},"
                  "\"line_number\":2,"
                  "\"submatches\":["
                  "{\"match\":{\"text\":\"Cabernet\"},\"start\":2,\"end\":10}]}}\n"))
         (hits (vulpea-mentions--parse-rg-json output)))
    (should (equal (mapcar (lambda (hit) (plist-get hit :line-text)) hits)
                   '("a Cabernet")))))

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

(ert-deftest vulpea-mentions--line-unlinked-p-script-aware ()
  "Boundaries are enforced per script: spaceless scripts match as substrings."
  ;; CJK term inside CJK prose: no word separators, so it counts
  (should (vulpea-mentions--line-unlinked-p "鹿苑寺，又名金阁寺" '("金阁寺")))
  ;; the same term inside a link does not
  (should-not (vulpea-mentions--line-unlinked-p "看[[id:x][金阁寺]]" '("金阁寺")))
  ;; a Latin term embedded in CJK prose counts too
  (should (vulpea-mentions--line-unlinked-p "我爱用Emacs写作" '("Emacs")))
  ;; mixed-script terms match with CJK neighbors on their CJK edge
  (should (vulpea-mentions--line-unlinked-p "我的Emacs入门笔记" '("Emacs入门")))
  ;; spaced scripts keep strict boundaries: Cyrillic
  (should (vulpea-mentions--line-unlinked-p "місто Київ." '("Київ")))
  (should-not (vulpea-mentions--line-unlinked-p "розмова триває" '("мова")))
  (should-not (vulpea-mentions--line-unlinked-p "Київський вокзал" '("Київ")))
  ;; and Latin glued to Latin still does not match
  (should-not (vulpea-mentions--line-unlinked-p "smart move" '("art"))))

(ert-deftest vulpea-mentions--metadata-line-p ()
  "Org keyword and property-drawer lines are recognized as metadata."
  ;; keyword lines
  (should (vulpea-mentions--metadata-line-p "#+title: Cabernet Sauvignon"))
  (should (vulpea-mentions--metadata-line-p "#+filetags: :wine:"))
  ;; property-drawer lines
  (should (vulpea-mentions--metadata-line-p ":PROPERTIES:"))
  (should (vulpea-mentions--metadata-line-p ":END:"))
  (should (vulpea-mentions--metadata-line-p ":ID: abc-123"))
  (should (vulpea-mentions--metadata-line-p "  :CREATED: [2026-01-01]"))
  ;; prose is not metadata
  (should-not (vulpea-mentions--metadata-line-p "We drank Cabernet Sauvignon."))
  (should-not (vulpea-mentions--metadata-line-p "- a list item")))

(ert-deftest vulpea-mentions--rg-command-shape ()
  "The ripgrep command carries regex patterns and the org glob."
  (let ((cmd (vulpea-mentions--rg-command "rg" '("Wine" "金阁寺") '("/dir"))))
    (should (equal (car cmd) "rg"))
    (should (member "--json" cmd))
    (should (member "--ignore-case" cmd))
    (should (member "/dir" cmd))
    ;; terms are compiled to regexes, so fixed-string and global word
    ;; modes are gone
    (should-not (member "--fixed-strings" cmd))
    (should-not (member "--word-regexp" cmd))
    ;; each term passed via -e as a pattern with script-aware boundaries
    (should (member "(?:(?-u:\\b)|_)Wine(?:(?-u:\\b)|_)" cmd))
    (should (member "金阁寺" cmd))
    ;; whether a term is in a link is for the Org parser to tell, so
    ;; ripgrep reports lines and nothing more
    (should-not (member "--multiline" cmd))))

(ert-deftest vulpea-mentions--rg-file-command-shape ()
  "The single-file ripgrep command reads literal patterns and input from files."
  (let ((cmd (vulpea-mentions--rg-file-command "rg" "/tmp/patterns" "/tmp/input")))
    (should (equal (car cmd) "rg"))
    (should (member "--json" cmd))
    ;; Titles are literals, and case is folded in Emacs before ripgrep
    ;; sees either side: both keep it on its literal matcher.  A
    ;; --smart-case from the user's ripgrep config must not undo that.
    (should (member "--fixed-strings" cmd))
    (should (member "--case-sensitive" cmd))
    (should-not (member "--ignore-case" cmd))
    (should-not (member "--word-regexp" cmd))
    (should-not (member "-" cmd))
    ;; wrapped links come from the buffer here, not from ripgrep
    (should-not (member "--multiline" cmd))
    (should (equal (last cmd 3) '("-f" "/tmp/patterns" "/tmp/input")))))

(ert-deftest vulpea-mentions--fold ()
  "Folding erases case one character at a time, so positions survive it."
  (should (equal (vulpea-mentions--fold "Cabernet SAUVIGNON") "cabernet sauvignon"))
  (should (equal (vulpea-mentions--fold "КИЇВ 金阁寺")
                 "київ 金阁寺"))
  ;; the three sigmas are one letter to a case-insensitive search
  (should (equal (vulpea-mentions--fold "ΟΔΟΣ")
                 (vulpea-mentions--fold "οδος")))
  ;; `downcase' turns a dotted capital I into two characters, which
  ;; would shift every position after it
  (let ((text "İstanbul\nWine\n"))
    (should (= (length (vulpea-mentions--fold text)) (length text)))
    (should (string-suffix-p "\nwine\n" (vulpea-mentions--fold text))))
  (should (equal (vulpea-mentions--fold "") "")))

(ert-deftest vulpea-mentions--rg-quote-escapes-metacharacters ()
  "Rust regex metacharacters are escaped; plain text is untouched."
  (should (equal (vulpea-mentions--rg-quote "plain text") "plain text"))
  (should (equal (vulpea-mentions--rg-quote "C++") "C\\+\\+"))
  (should (equal (vulpea-mentions--rg-quote "Foo (Bar)") "Foo \\(Bar\\)"))
  (should (equal (vulpea-mentions--rg-quote "a.b|c") "a\\.b\\|c"))
  (should (equal (vulpea-mentions--rg-quote "x[1]{2}^$?*\\")
                 "x\\[1\\]\\{2\\}\\^\\$\\?\\*\\\\")))

(ert-deftest vulpea-mentions--rg-pattern-script-aware ()
  "Boundary assertions are chosen per side from the edge character."
  ;; ASCII alphanumeric edges get the fast ASCII assertion, which
  ;; lets an underscore through: ripgrep takes it for a word
  ;; character, Emacs does not
  (should (equal (vulpea-mentions--rg-pattern "Wine")
                 "(?:(?-u:\\b)|_)Wine(?:(?-u:\\b)|_)"))
  ;; non-ASCII spaced edges (Cyrillic) rely on the Emacs re-check:
  ;; a Unicode \\b here would force the regex engine off its fast path
  (should (equal (vulpea-mentions--rg-pattern "Київ") "Київ"))
  ;; punctuation edges get no assertion (a \\b next to punctuation
  ;; inverts into demanding adjacent text); escaping still applies
  (should (equal (vulpea-mentions--rg-pattern "C++") "(?:(?-u:\\b)|_)C\\+\\+"))
  ;; spaceless scripts match as substrings
  (should (equal (vulpea-mentions--rg-pattern "金阁寺") "金阁寺"))
  ;; mixed terms keep the assertion only on the ASCII edge
  (should (equal (vulpea-mentions--rg-pattern "Emacs入门")
                 "(?:(?-u:\\b)|_)Emacs入门"))
  (should (equal (vulpea-mentions--rg-pattern "入门Emacs")
                 "入门Emacs(?:(?-u:\\b)|_)")))

(ert-deftest vulpea-mentions--rg-pattern-respects-configuration ()
  "The boundary strategy and script list are customizable."
  ;; always: strict Unicode boundaries, the old behavior
  (let ((vulpea-mentions-word-boundaries t))
    (should (equal (vulpea-mentions--rg-pattern "金阁寺") "\\b金阁寺\\b")))
  ;; never
  (let ((vulpea-mentions-word-boundaries nil))
    (should (equal (vulpea-mentions--rg-pattern "Wine") "Wine")))
  ;; an empty script list makes CJK behave like a spaced script: the
  ;; pre-filter stays substring (non-ASCII edge), but the Emacs
  ;; re-check now demands standalone words, so embedded CJK mentions
  ;; are rejected again - the old semantics, enforced in the layer
  ;; that owns semantics
  (let ((vulpea-mentions-spaceless-scripts nil))
    (should (equal (vulpea-mentions--rg-pattern "金阁寺") "金阁寺"))
    (should-not (vulpea-mentions--line-unlinked-p
                 "鹿苑寺，又名金阁寺" '("金阁寺")))))

(defun vulpea-mentions-test--ignore-notes-files ()
  "Return the file specs backing the per note ignore tests.

Sets holds a Maps heading and mentions MapTool, fileless.org mentions
Git from a heading with no file level note, and the remaining files
give the ignore commands both file level and heading level targets."
  `((:name "sets.org"
           :content
           ,(concat ":PROPERTIES:\n:ID: sets\n"
                    ":END:\n#+title: Sets\n\n"
                    "A contrived link to [[id:gone-with-the-wind][Gone with the Wind]].\n"
                    "* Maps\n:PROPERTIES:\n:ID: maps\n:END:\n#+title: Maps\n\n"
                    "A map is a functional relation...\n"
                    "A contrived mention to MapTool.\n"))
    (:name "gone-with-the-wind.org"
           :content
           ,(concat ":PROPERTIES:\n:ID: gone-with-the-wind\n:END:\n#+title: Gone with the Wind\n\n"
                    "And I'm not denying that when he sets out to drink he can put even the Tarletons under the table."))
    (:name "git.org"
           :content
           ,(concat ":PROPERTIES:\n:ID: git\n:END:\n#+title: Git\n\n"
                    "This command also sets the local branch to track the remote branch."))
    (:name "maptool.org"
           :content
           ,(concat ":PROPERTIES:\n:ID: maptool\n:END:\n#+title: MapTool\n\n"
                    "MapTool helps you play DnD online with digital maps!"))
    (:name "fileless.org"
           :content
           ,(concat "A file contains no file level note id!\n"
                    "* Heading\n"
                    ":PROPERTIES:\n:ID: fileless\n:END:\n"
                    "Git rebasing sometimes can be confusing.\n"))
    (:name "ignored-heading-note.org"
           :content
           ,(concat ":PROPERTIES:\n:ID: ignored-heading-note\n"
                    (format ":%s: maps\n" vulpea-mentions-per-note-ignore-property-key)
                    ":END:\n"
                    "If we have an id of a heading note ignored, unignore it should work."))))

(defun vulpea-mentions-test--id-ignored-p (id)
  "Return non-nil when ID is ignored by the note at point."
  (org-entry-member-in-multivalued-property
   (point)
   vulpea-mentions-per-note-ignore-property-key
   id))

(ert-deftest vulpea-mentions-ignore-from ()
  "Adding ignored note id to per note ignore property in various settings.

Covers the property manipulation only; the effect on mentions is
`vulpea-mentions-ignore-from-silences-mentions', which needs rg."
  (vulpea-test--with-temp-db-and-files
   (vulpea-mentions-test--ignore-notes-files)
   (let ((sets-note (vulpea-db-get-by-id "sets"))
         (gw-note (vulpea-db-get-by-id "gone-with-the-wind"))
         (git-note (vulpea-db-get-by-id "git"))
         (maps-note (vulpea-db-get-by-id "maps"))
         (maptool-note (vulpea-db-get-by-id "maptool"))
         (fileless-note (vulpea-db-get-by-id "fileless"))
         (ihn-note (vulpea-db-get-by-id "ignored-heading-note")))

     (vulpea-utils-with-note sets-note
       ;; At the beginning, there is no such property
       (should (null (org-find-property vulpea-mentions-per-note-ignore-property-key)))
       (vulpea-mentions-ignore-from sets-note gw-note)
       ;; After we ignore Gone with the Wind, its id should appear as one of the property values
       (should (vulpea-mentions-test--id-ignored-p "gone-with-the-wind"))
       (vulpea-mentions-ignore-from sets-note git-note)
       ;; After we ignore Git, both its id and previously ignored id should be both part of the value list
       (should (vulpea-mentions-test--id-ignored-p "git"))
       (should (vulpea-mentions-test--id-ignored-p "gone-with-the-wind"))
       ;; The database should also have the property updated right now
       (should (let* ((properties (vulpea-note-properties (vulpea-db-get-by-id "sets")))
                      (prop-value (cdr (assoc vulpea-mentions-per-note-ignore-property-key properties))))
                 (and (string-match-p "gone-with-the-wind" prop-value)
                      (string-match-p "git" prop-value))))
       ;; The buffer should not change when we ignore an already-ignored note again
       (let ((buffer-string-before (buffer-string)))
         (vulpea-mentions-ignore-from sets-note git-note)
         (let ((buffer-string-after (buffer-string)))
           (should (equal buffer-string-before buffer-string-after)))))
     ;; If we ignore from a heading note, then the property should be
     ;; created for it rather than the file level property drawer
     (vulpea-mentions-ignore-from maps-note maptool-note)
     (vulpea-utils-with-note maps-note
       (should (vulpea-mentions-test--id-ignored-p "maptool")))
     ;; If we ignore mentions from a heading note, we should add its
     ;; file level note id to the property value list
     (vulpea-mentions-ignore-from maptool-note maps-note)
     (vulpea-utils-with-note maptool-note
       (should (vulpea-mentions-test--id-ignored-p "sets")))

     ;; Reverse the process to test the unignore part
     (vulpea-mentions-unignore-from maptool-note maps-note)
     (vulpea-utils-with-note maptool-note
       (should (not (vulpea-mentions-test--id-ignored-p "sets"))))
     ;; Unignore from a heading note only affects its own property
     (vulpea-mentions-unignore-from maps-note maptool-note)
     (vulpea-utils-with-note maps-note
       (should (not (vulpea-mentions-test--id-ignored-p "maptool"))))
     (vulpea-utils-with-note sets-note
       (vulpea-mentions-unignore-from sets-note git-note)
       (vulpea-mentions-unignore-from sets-note gw-note)
       ;; Idempotence test
       (let ((string-before (buffer-string)))
         (vulpea-mentions-unignore-from sets-note gw-note)
         (let ((string-after (buffer-string)))
           (should (equal string-before string-after))))
       ;; Property should be gone in the database now
       (let* ((properties (vulpea-note-properties (vulpea-db-get-by-id "sets")))
              (prop-record (assoc vulpea-mentions-per-note-ignore-property-key properties)))
         (should (null prop-record)))
       ;; Property should also be cleared
       (should (null (org-find-property vulpea-mentions-per-note-ignore-property-key))))
     ;; When we ignore from a heading note which does not reside in a file level note
     (vulpea-mentions-ignore-from git-note fileless-note)
     (vulpea-utils-with-note git-note
       (should (vulpea-mentions-test--id-ignored-p "fileless")))
     ;; Unignore a heading note id should work when it also has a file level note id
     (vulpea-mentions-unignore-from ihn-note maps-note)
     (vulpea-utils-with-note ihn-note
       (should (null (org-find-property vulpea-mentions-per-note-ignore-property-key)))))))

(ert-deftest vulpea-mentions-ignore-from-silences-mentions ()
  "Ignoring a note drops its mentions, unignoring brings them back."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
   (vulpea-mentions-test--ignore-notes-files)
   (let ((git-note (vulpea-db-get-by-id "git"))
         (maps-note (vulpea-db-get-by-id "maps"))
         (maptool-note (vulpea-db-get-by-id "maptool"))
         (fileless-note (vulpea-db-get-by-id "fileless")))
     ;; Sets mentions MapTool from its Maps heading
     (should (equal 1 (length (vulpea-mentions-test--collect-incoming-mentions-for-note
                               "maptool"))))
     ;; Ignoring the heading note silences the mention, because the
     ;; ignore list holds the id of its file level note
     (vulpea-mentions-ignore-from maptool-note maps-note)
     (should (equal 0 (length (vulpea-mentions-test--collect-incoming-mentions-for-note
                               "maptool"))))
     (vulpea-mentions-unignore-from maptool-note maps-note)
     (should (equal 1 (length (vulpea-mentions-test--collect-incoming-mentions-for-note
                               "maptool"))))
     ;; The mentioning file has no file level note, so the heading id
     ;; is what silences it
     (should (equal 1 (length (vulpea-mentions-test--collect-incoming-mentions-for-note
                               "git"))))
     (vulpea-mentions-ignore-from git-note fileless-note)
     (should (equal 0 (length (vulpea-mentions-test--collect-incoming-mentions-for-note
                               "git")))))))

(ert-deftest vulpea-mentions-ignored-notes ()
  "Note ids should be resolved to notes in the same way as collecting mentions."
  (vulpea-test--with-temp-db-and-files
   `((:name "source.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: source\n"
                     ":END:\n#+title: Source\n\n"))
     (:name "file.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: file\n:END:\n"
                     "#+title: File\n"
                     "Source\n"))
     (:name "heading-only.org"
            :content
            ,(concat "* Heading Only\n"
                     ":PROPERTIES:\n:ID: heading-only\n:END:\n"
                     "Source\n"))
     (:name "heading-with-file.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: heading-with-file\n:END:\n"
                     "#+title: Heading With File\n"
                     "* Heading\n"
                     ":PROPERTIES:\n:ID: heading\n:END:\n"
                     "Source\n"))
     (:name "headings.org"
            :content
            ,(concat "* Heading 1\n"
                     ":PROPERTIES:\n:ID: heading1\n:END:\n"
                     "* Heading 2\n"
                     ":PROPERTIES:\n:ID: heading2\n:END:\n"
                     "Source\n"))
     (:name "merge-dup.org"
            :content
            ,(concat "* Duplicate 1\n"
                     ":PROPERTIES:\n:ID: duplicate1\n:END:\n"
                     "* Duplicate 2\n"
                     ":PROPERTIES:\n:ID: duplicate2\n:END:\n"
                     "Source\n") ))
   ;; Initially, there is nothing ignored by the source note.
   (let ((ignored-notes (vulpea-mentions-ignored-notes (vulpea-db-get-by-id "source"))))
     (should (equal (length ignored-notes) 0)))
   ;; We need to test it resolves note ids to notes the same way as
   ;; `vulpea-mentions--collect'. Compute the mentions first.
   (let* ((mentions (vulpea-mentions-test--collect-incoming-mentions-for-note "source"))
          (notes-collected-by-mention (mapcar (lambda (mention) (plist-get mention :note))
                                              mentions)))
     ;; Add ignore ids, we have to edit the file directly to insert
     ;; the ids, so we can test the function correctly resolve certain
     ;; ids to notes follow the same rule as the collector.
     (vulpea-utils-with-note-sync (vulpea-db-get-by-id "source")
       (dolist (id '("file" "heading-only" "heading" "heading2" "duplicate1" "duplicate2"))
         (org-entry-add-to-multivalued-property
          (point)
          vulpea-mentions-per-note-ignore-property-key
          id)))
     ;; Verify each note appearing in mentions also appears in the ignored
     ;; note list, since we have ignored them all.
     (let ((ignored-notes (vulpea-mentions-ignored-notes (vulpea-db-get-by-id "source"))))
       (dolist (note-resolved-by-mention notes-collected-by-mention)
         (should (seq-find (lambda (ignored-note)
                             (equal (vulpea-note-id ignored-note)
                                    (vulpea-note-id note-resolved-by-mention)))
                           ignored-notes)))
       ;; Also, the ignored note list has no false positive entries
       (should (equal (length ignored-notes) (length notes-collected-by-mention)))))))

;;; Collection (DB-backed)

(ert-deftest vulpea-mentions--collect-maps-and-filters ()
  "Collect maps hits to notes; skips own file, linked text, and metadata lines."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Cabernet" :path "/n/target.org")
    (vulpea-test--insert-test-note "mentioner" "Tasting" :path "/n/tasting.org")
    (vulpea-test--insert-test-note "twin" "Cabernet" :path "/n/twin.org")
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
                    "\"lines\":{\"text\":\"Cabernet is me\\n\"},\"line_number\":1}}\n"
                    ;; a same-titled note's #+title line -> metadata, excluded
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/twin.org\"},"
                    "\"lines\":{\"text\":\"#+title: Cabernet\\n\"},\"line_number\":4}}\n"
                    ;; a same-titled note's prose line -> title collision, excluded
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/twin.org\"},"
                    "\"lines\":{\"text\":\"Cabernet appears again here\\n\"},\"line_number\":8}}\n"))
           (mentions (vulpea-mentions--collect output note own)))
      (should (= (length mentions) 1))
      (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "mentioner"))
      (should (equal (plist-get (car mentions) :line) 3))
      (should (equal (plist-get (car mentions) :context) "a lovely Cabernet")))))

(ert-deftest vulpea-mentions--collect-survives-bytes-path ()
  "A hit in a file with a non-UTF-8 name must not sink the whole scan.
ripgrep encodes such a path as {\"bytes\": ...}; the hit is dropped and
the remaining hits are still collected, instead of a nil path blowing
up in `expand-file-name' and rejecting the entire result."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Cabernet" :path "/n/target.org")
    (vulpea-test--insert-test-note "mentioner" "Tasting" :path "/n/tasting.org")
    (let* ((note (vulpea-db-get-by-id "target"))
           (own (expand-file-name "/n/target.org"))
           (output (concat
                    ;; a file whose name is not valid UTF-8 -> dropped
                    "{\"type\":\"match\",\"data\":{\"path\":{\"bytes\":\"L24vY2Fm6S5vcmc=\"},"
                    "\"lines\":{\"text\":\"a lovely Cabernet\\n\"},\"line_number\":1}}\n"
                    ;; a genuine unlinked mention -> still collected
                    "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"/n/tasting.org\"},"
                    "\"lines\":{\"text\":\"a lovely Cabernet\\n\"},\"line_number\":3}}\n"))
           (mentions (vulpea-mentions--collect output note own)))
      (should (= (length mentions) 1))
      (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "mentioner")))))

(ert-deftest vulpea-mentions--shares-name-p ()
  "A note that shares a title or alias with the search terms is detected."
  (let ((terms '("Cabernet Sauvignon" "Cab Sauv")))
    ;; same title (case-insensitive)
    (should (vulpea-mentions--shares-name-p
             (make-vulpea-note :title "cabernet sauvignon") terms))
    ;; shares via alias
    (should (vulpea-mentions--shares-name-p
             (make-vulpea-note :title "Other" :aliases '("Cab Sauv")) terms))
    ;; unrelated note
    (should-not (vulpea-mentions--shares-name-p
                 (make-vulpea-note :title "Merlot") terms))))

(ert-deftest vulpea-mentions--ignore-note-p ()
  "Ignore notes with certain property set to certain value."
  (should (vulpea-mentions--ignore-note-p
           (make-vulpea-note
            :title "Ignore This"
            :properties `((,vulpea-mentions-ignore-property-key
                           .
                           ,vulpea-mentions-ignore-property-value)))))
  (should (not (vulpea-mentions--ignore-note-p
                (make-vulpea-note :title "Do Not Ignore This")))))

;;; Integration with real ripgrep

;; The full subprocess pipeline is exercised by running ripgrep
;; synchronously (via `call-process') and feeding its real output to
;; `vulpea-mentions--collect'.  This is deterministic in batch mode,
;; unlike waiting on an async sentinel, which is timing-sensitive across
;; Emacs versions.  The async wiring's error path has its own test below.

(ert-deftest vulpea-mentions-collect-with-real-rg ()
  "Real ripgrep finds the bare mention; the linked and own-file hits are excluded."
  (vulpea-test--require-rg)
  (let* ((dir (make-temp-file "vulpea-mentions-" t))
         (target (expand-file-name "target.org" dir))
         (link-and-mention (expand-file-name "link-and-mention.org" dir))
         (mention-only (expand-file-name "mention-only.org" dir))
         (vulpea-db-location (make-temp-file "vulpea-mentions-" nil ".db"))
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list dir)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n"))
          (with-temp-file link-and-mention
            (insert ":PROPERTIES:\n:ID: link-and-mention\n:END:\n#+title: Link and Mention\n\n"
                    "A bare Cabernet mention, but there is another link in the buffer.\n"
                    "* Heading\n"
                    ":PROPERTIES:\n:ID: heading\n:END:\n"
                    "A linked [[id:target][Cabernet]] mention.\n"))
          (with-temp-file mention-only
            (insert ":PROPERTIES:\n:ID: mention-only\n:END:\n#+title: Mention Only\n\n"
                    "A bare Cabernet mention without other links.\n"))
          (vulpea-db)
          (vulpea-db-update-file target)
          (vulpea-db-update-file link-and-mention)
          (vulpea-db-update-file mention-only)
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
            (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "mention-only"))
            (should (string-match-p "bare Cabernet mention without other links"
                                    (plist-get (car mentions) :context)))
            ;; test for the original behavior
            (let ((vulpea-mentions-exclude-linked nil))
              (setq mentions (vulpea-mentions--collect
                              output note (expand-file-name target)))
              (should (= (length mentions) 2)))))
      (when vulpea-db--connection (vulpea-db-close))
      (when (file-exists-p vulpea-db-location) (delete-file vulpea-db-location))
      (delete-directory dir t))))

(ert-deftest vulpea-mentions-collect-cjk-with-real-rg ()
  "A CJK title embedded in CJK prose is found; the linking file is excluded."
  (vulpea-test--require-rg)
  (let* ((dir (make-temp-file "vulpea-mentions-" t))
         (target (expand-file-name "target.org" dir))
         (mention (expand-file-name "mention.org" dir))
         (linked (expand-file-name "linked.org" dir))
         (vulpea-db-location (make-temp-file "vulpea-mentions-" nil ".db"))
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list dir)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert ":PROPERTIES:\n:ID: target\n:END:\n#+title: 金阁寺\n"))
          (with-temp-file mention
            (insert ":PROPERTIES:\n:ID: mention\n:END:\n#+title: 京都游记\n\n"
                    "鹿苑寺，又名金阁寺。\n"))
          (with-temp-file linked
            (insert ":PROPERTIES:\n:ID: linked\n:END:\n#+title: 链接\n\n"
                    "参观[[id:target][金阁寺]]。\n"))
          (vulpea-db)
          (vulpea-db-update-file target)
          (vulpea-db-update-file mention)
          (vulpea-db-update-file linked)
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
            (should (equal (vulpea-note-id (plist-get (car mentions) :note))
                           "mention"))
            (should (string-match-p "又名金阁寺"
                                    (plist-get (car mentions) :context)))))
      (when vulpea-db--connection (vulpea-db-close))
      (when (file-exists-p vulpea-db-location) (delete-file vulpea-db-location))
      (delete-directory dir t))))

(ert-deftest vulpea-mentions-wrapped-link-with-real-rg ()
  "A term inside a link that wraps across lines is not a mention.
Filling a paragraph breaks a link description like any other text, and
ripgrep then reports each line of the link on its own.  The term may sit
on the last, a middle or the first line of the link; a bare occurrence
next to it, and matches on adjacent lines, are still reported with
their own line numbers."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Sauvignon\n")
        (:name "other.org"
         :content ":PROPERTIES:\n:ID: other\n:END:\n#+title: Cabernet Sauvignon\n")
        (:name "wrapped.org"
         :content ,(concat ":PROPERTIES:\n:ID: wrapped\n:END:\n#+title: Wrapped\n\n"
                           "We opened a [[id:other][Cabernet\n"
                           "Sauvignon]] last night.\n"
                           "\n"
                           "Then a [[id:other][very long description of\n"
                           "that Sauvignon which goes\n"
                           "on and on]] too.\n"
                           "\n"
                           "And a [[id:other][Sauvignon of\n"
                           "note]] to finish, or a [[Cabernet\n"
                           "Sauvignon]] target.\n"))
        (:name "mixed.org"
         :content ,(concat ":PROPERTIES:\n:ID: mixed\n:END:\n#+title: Mixed\n\n"
                           "A [[id:other][Cabernet\n"
                           "Sauvignon]] and a bare Sauvignon after it.\n"
                           "Sauvignon again on the next line.\n")))
    (let ((mentions (vulpea-mentions-test--collect-incoming-mentions-for-note
                     "target")))
      (should (equal (sort (mapcar (lambda (m)
                                     (list (vulpea-note-id (plist-get m :note))
                                           (plist-get m :line)
                                           (plist-get m :context)))
                                   mentions)
                           (lambda (a b) (< (cadr a) (cadr b))))
                     '(("mixed" 7 "Sauvignon]] and a bare Sauvignon after it.")
                       ("mixed" 8 "Sauvignon again on the next line.")))))))

(ert-deftest vulpea-mentions-wrapped-link-cjk-with-real-rg ()
  "A wrapped link is recognized in CJK prose too."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: 金阁寺\n")
        (:name "wrapped.org"
         :content ,(concat ":PROPERTIES:\n:ID: wrapped\n:END:\n#+title: 京都游记\n\n"
                           "参观[[id:other][鹿苑寺，又名\n"
                           "金阁寺]]。\n"))
        (:name "mixed.org"
         :content ,(concat ":PROPERTIES:\n:ID: mixed\n:END:\n#+title: 日记\n\n"
                           "参观[[id:other][鹿苑寺，又名\n"
                           "金阁寺]]，再访金阁寺。\n")))
    (let ((mentions (vulpea-mentions-test--collect-incoming-mentions-for-note
                     "target")))
      (should (= (length mentions) 1))
      (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "mixed"))
      (should (equal (plist-get (car mentions) :line) 7))
      (should (equal (plist-get (car mentions) :context)
                     "金阁寺]]，再访金阁寺。")))))

(ert-deftest vulpea-mentions-latin-in-cjk-prefilter-with-real-rg ()
  "The default pre-filter surfaces a Latin title glued into CJK prose.
The ASCII assertion treats non-ASCII text as a boundary, so this works
under `auto' without dropping boundaries entirely; the post-filter
agrees.  Pins the behavior the docs promise for 我爱用Emacs写作."
  (vulpea-test--require-rg)
  (let* ((dir (make-temp-file "vulpea-mentions-" t))
         (file (expand-file-name "prose.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "我爱用Emacs写作\n"))
          (let* ((cmd (vulpea-mentions--rg-command
                       (executable-find "rg") '("Emacs") (list dir)))
                 (output (with-temp-buffer
                           (apply #'call-process (car cmd) nil t nil (cdr cmd))
                           (buffer-string)))
                 (hits (vulpea-mentions--parse-rg-json output)))
            (should (= (length hits) 1))
            (should (vulpea-mentions--line-unlinked-p
                     (plist-get (car hits) :line-text) '("Emacs")))))
      (delete-directory dir t))))

(ert-deftest vulpea-mentions-ignore-note-with-property ()
  "When a note set the ignore property, skip searching for its mentions."
  (vulpea-test--require-rg)
  (let* ((dir (make-temp-file "vulpea-mentions-" t))
         (ignored (expand-file-name "ignored.org" dir))
         (not-ignored (expand-file-name "not-ignored.org" dir))
         (mention (expand-file-name "mention.org" dir))
         (vulpea-db-location (make-temp-file "vulpea-mentions-" nil ".db"))
         (vulpea-db--connection nil)
         (vulpea-db-sync-directories (list dir)))
    (unwind-protect
        (progn
          (with-temp-file ignored
            (insert ":PROPERTIES:\n:ID: ignored\n"
                    (format ":%s: %s\n"
                            vulpea-mentions-ignore-property-key
                            vulpea-mentions-ignore-property-value)
                    ":END:\n#+title: Ignored\n"))
          (with-temp-file not-ignored
            (insert ":PROPERTIES:\n:ID: not-ignored\n:END:\n"
                    "#+title: Not Ignored\n\n"))
          (with-temp-file mention
            (insert ":PROPERTIES:\n:ID: mention\n:END:\n#+title: Mention\n\n"
                    "An ignored mention.\n"
                    "A not ignored mention.\n"))
          (vulpea-db)
          (vulpea-db-update-file ignored)
          (vulpea-db-update-file not-ignored)
          (vulpea-db-update-file mention)
          ;; Notes set the ignore property short-circuit
          ;; `vulpea-note-unlinked-mentions-async' without spawning
          ;; the rg process. RESOLVE is called synchronously.
          (let (result done)
            (should (null (vulpea-note-unlinked-mentions-async
                           (vulpea-db-get-by-id "ignored")
                           (lambda (mentions) (setq done t
                                                    result mentions))
                           (lambda (_e) (setq done 'error)))))
            (should (eq done t))
            (should (null result)))
          ;; Notes without the ignore property are discovered by the
          ;; rg scan.
          (let* ((note (vulpea-db-get-by-id "not-ignored"))
                 (cmd (vulpea-mentions--rg-command
                       (executable-find "rg")
                       (vulpea-mentions--note-terms note)
                       (list dir)))
                 (output (with-temp-buffer
                           (apply #'call-process (car cmd) nil t nil (cdr cmd))
                           (buffer-string)))
                 (mentions (vulpea-mentions--collect
                            output note (expand-file-name not-ignored))))
            (should (= (length mentions) 1))))
      (when vulpea-db--connection (vulpea-db-close))
      (when (file-exists-p vulpea-db-location) (delete-file vulpea-db-location))
      (delete-directory dir t))))

(ert-deftest vulpea-mentions-per-note-ignore ()
  "Mentions from explicitly ignored notes should be dropped."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
   `((:name "stems.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: stems\n"
                     (format ":%s: pc-prediction\n" vulpea-mentions-per-note-ignore-property-key)
                     ":END:\n#+title: Stems\n\n"))
     (:name "notes.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: notes\n:END:\n#+title: Notes\n\n"
                     "Notes may have stems attached to them."))
     (:name "pc-prediction.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: pc-prediction\n:END:\n#+title: PC Prediction\n\n"
                     "This strategy stems from ...")))
   ;; Only one incoming mentions from Notes
   (let ((mentions (vulpea-mentions-test--collect-incoming-mentions-for-note "stems")))
     (should (eq 1 (length mentions)))
     (should (string-match-p "Notes may have"
                             (plist-get (car mentions) :context))))
   ;; Normal outgoing mentions
   (let ((mentions (vulpea-mentions-test--collect-outgoing-mentions-for-note "notes")))
     (should (eq (length mentions) 1)))
   ;; Outgoing mentions to notes explicitly ignore us are dropped.
   (let ((mentions (vulpea-mentions-test--collect-outgoing-mentions-for-note "pc-prediction")))
     (should (eq (length mentions) 0)))))

(ert-deftest vulpea-mentions-per-note-ignore-matches-heading-id ()
  "An id of any note in the mentioning file silences the mention.

The ignore list holds the id of a heading, while the mentioning file also
has a file-level note.  Incoming mentions must honour it, just like
outgoing mentions already do."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "stems.org"
         :content
         ,(concat ":PROPERTIES:\n:ID: stems\n"
                  (format ":%s: notes-heading\n"
                          vulpea-mentions-per-note-ignore-property-key)
                  ":END:\n#+title: Stems\n\n"))
        (:name "notes.org"
         :content
         ,(concat ":PROPERTIES:\n:ID: notes\n:END:\n#+title: Notes\n\n"
                  "* Section\n"
                  ":PROPERTIES:\n:ID: notes-heading\n:END:\n\n"
                  "Notes may have stems attached to them.\n")))
    (should (null (vulpea-mentions-test--collect-incoming-mentions-for-note
                   "stems")))))

(ert-deftest vulpea-mentions-per-note-ignore-survives-new-file-id ()
  "A file gaining a file-level id stays ignored.

The mentioning file starts with a heading id only, so that id is what
lands in the ignore list.  Once the file gains a file-level id, the note
representing it changes, but the mention must stay silenced."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "stems.org"
         :content
         ,(concat ":PROPERTIES:\n:ID: stems\n"
                  (format ":%s: notes-heading\n"
                          vulpea-mentions-per-note-ignore-property-key)
                  ":END:\n#+title: Stems\n\n"))
        (:name "notes.org"
         :content
         ,(concat "#+title: Notes\n\n"
                  "* Section\n"
                  ":PROPERTIES:\n:ID: notes-heading\n:END:\n\n"
                  "Notes may have stems attached to them.\n")))
    (should (null (vulpea-mentions-test--collect-incoming-mentions-for-note
                   "stems")))
    (let ((path (expand-file-name "notes.org" dir)))
      (with-temp-file path
        (insert ":PROPERTIES:\n:ID: notes\n:END:\n#+title: Notes\n\n"
                "* Section\n"
                ":PROPERTIES:\n:ID: notes-heading\n:END:\n\n"
                "Notes may have stems attached to them.\n"))
      (vulpea-db-update-file path))
    (should (vulpea-db-get-by-id "notes"))
    (should (null (vulpea-mentions-test--collect-incoming-mentions-for-note
                   "stems")))))

(ert-deftest vulpea-mentions-async-rejects-without-rg ()
  "When ripgrep is unavailable, REJECT is called."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (let ((rejected nil))
      (vulpea-note-unlinked-mentions-async
       (make-vulpea-note :title "X" :path "/n/x.org")
       (lambda (_ms) (setq rejected 'resolved))
       (lambda (_e) (setq rejected t)))
      (should (eq rejected t)))))

(ert-deftest vulpea-mentions-async-cjk-hostile-process-coding ()
  "CJK mentions survive a non-UTF-8 process coding configuration.

The entry point pins UTF-8 on the ripgrep process itself, so a hostile
`default-process-coding-system' (say, latin-1 from a user's setup) must
mangle neither the CJK terms passed as arguments nor the JSON output
coming back."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ,(concat ":PROPERTIES:\n:ID: target\n:END:\n"
                           "#+title: 金阁寺\n"))
        (:name "mention.org"
         :content ,(concat ":PROPERTIES:\n:ID: mention\n:END:\n"
                           "#+title: 京都游记\n\n"
                           "鹿苑寺，又名金阁寺。\n")))
    (let* ((note (vulpea-db-get-by-id "target"))
           (state nil)
           (result nil)
           (default-process-coding-system '(latin-1 . latin-1))
           (coding-system-for-write 'latin-1))
      (vulpea-note-unlinked-mentions-async
       note
       (lambda (ms) (setq state 'resolved result ms))
       (lambda (err) (setq state (list 'rejected err))))
      (vulpea-mentions-test--await (lambda () state))
      (should (eq state 'resolved))
      (should (= (length result) 1))
      (should (equal (vulpea-note-id (plist-get (car result) :note))
                     "mention"))
      (should (string-match-p "又名金阁寺"
                              (plist-get (car result) :context))))))

;;; Outgoing (what a buffer mentions)

(ert-deftest vulpea-mentions--title-dictionary ()
  "The dictionary maps case-folded names to ids (collisions kept) and lists them."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "a" "Cabernet Sauvignon" :aliases '("Cab Sauv"))
    (vulpea-test--insert-test-note "b" "Merlot")
    (vulpea-test--insert-test-note "c" "Cabernet Sauvignon")
    (let* ((dt (vulpea-mentions--title-dictionary))
           (dict (car dt))
           (terms (cdr dt)))
      (should (equal (sort (gethash "cabernet sauvignon" dict) #'string<) '("a" "c")))
      (should (equal (gethash "cab sauv" dict) '("a")))
      (should (equal (gethash "merlot" dict) '("b")))
      ;; One term per name however many notes bear it, longest first:
      ;; ripgrep reports the first pattern that fits at a position.
      (should (equal terms '("cabernet sauvignon" "cab sauv" "merlot"))))))

(ert-deftest vulpea-mentions--title-dictionary-respects-filter ()
  "The candidate dictionary honors `vulpea-mentions-note-filter'."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "f" "Cabernet" :path "/n/f.org" :level 0)
    (vulpea-test--insert-test-note "h" "Heading Wine" :path "/n/f.org" :level 1 :pos 50)
    ;; default keeps only file-level notes
    (let ((dict (car (vulpea-mentions--title-dictionary))))
      (should (gethash "cabernet" dict))
      (should-not (gethash "heading wine" dict)))
    ;; a custom filter can include heading-level notes
    (let* ((vulpea-mentions-note-filter (lambda (_n) t))
           (dict (car (vulpea-mentions--title-dictionary))))
      (should (gethash "heading wine" dict)))))

(ert-deftest vulpea-mentions--title-dictionary-respects-ignore-property ()
  "The candidate dictionary honors `vulpea-mentions--ignore-note-p'."
  (vulpea-test--with-temp-db
   (vulpea-db)
   (vulpea-test--insert-test-note "c" "Cabernet"
                                  :properties
                                  `((,vulpea-mentions-ignore-property-key
                                     .
                                     ,vulpea-mentions-ignore-property-value)))
   (vulpea-test--insert-test-note "m" "Merlot")
   (let* ((dt (vulpea-mentions--title-dictionary))
          (dict (car dt))
          (terms (cdr dt)))
     (should (equal terms '("merlot")))))
  (vulpea-test--with-temp-db
   (vulpea-db)
   (vulpea-test--insert-test-note "c1" "Cabernet"
                                  :properties
                                  `((,vulpea-mentions-ignore-property-key
                                     .
                                     ,vulpea-mentions-ignore-property-value)))
   (vulpea-test--insert-test-note "c2" "Cabernet")
   (let* ((dt (vulpea-mentions--title-dictionary))
          (dict (car dt))
          (terms (cdr dt)))
     (should (equal (gethash "cabernet" dict) '("c2"))))))

(defun vulpea-mentions-test--rg-match (text line-no term)
  "Return the ripgrep JSON event for TERM matching on line LINE-NO of TEXT.

Ripgrep reads the case-folded text, so that is what the event quotes."
  (format (concat "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"<stdin>\"},"
                  "\"lines\":{\"text\":%S},\"line_number\":%d,"
                  "\"submatches\":[{\"match\":{\"text\":%S},\"start\":0,\"end\":1}]}}\n")
          (vulpea-mentions--fold (nth (1- line-no) (split-string text "\n")))
          line-no
          (vulpea-mentions--fold term)))

(ert-deftest vulpea-mentions--collect-outgoing ()
  "Outgoing collect maps matched terms to candidate notes and applies filters."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet" :path "/n/cab.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (vulpea-test--insert-test-note "self" "Diary" :path "/n/diary.org")
    (let* ((dict (car (vulpea-mentions--title-dictionary)))
           (self-ids '("self"))
           (text (concat "#+title: Merlot\n"
                         "had some Cabernet\n"
                         "see [[id:cab][Cabernet]]\n"
                         "my Diary entry\n"))
           (output (concat
                    ;; bare mention -> candidate cab
                    (vulpea-mentions-test--rg-match text 2 "Cabernet")
                    ;; already linked -> excluded
                    (vulpea-mentions-test--rg-match text 3 "Cabernet")
                    ;; metadata line -> excluded
                    (vulpea-mentions-test--rg-match text 1 "Merlot")
                    ;; mention of the buffer's own note -> excluded via self-ids
                    (vulpea-mentions-test--rg-match text 4 "Diary")))
           (build-linked-ids (lambda ()
                               (let ((result (make-hash-table :test 'equal)))
                                 (puthash "cab" t result)
                                 result)))
           (linked-ids (funcall build-linked-ids)))
      (let ((mentions (vulpea-mentions--collect-outgoing
                       output text dict self-ids linked-ids)))
        (should (= (length mentions) 0)))
      (let ((mentions (vulpea-mentions--collect-outgoing
                       output text dict self-ids (make-hash-table :test 'equal))))
        (should (= (length mentions) 1))
        (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "cab"))
        ;; quoted from the text, not from ripgrep's folded copy of it
        (should (equal (plist-get (car mentions) :matched) "Cabernet"))
        (should (equal (plist-get (car mentions) :context) "had some Cabernet"))
        (should (equal (plist-get (car mentions) :line) 2))))))

(ert-deftest vulpea-mentions--collect-outgoing-skips-wrapped-links ()
  "A term on a line of a wrapped link is not an outgoing mention."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet" :path "/n/cab.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (let* ((dict (car (vulpea-mentions--title-dictionary)))
           (text (concat "see [[id:x][a fine\n"
                         "Cabernet]] and Merlot\n"))
           (output (concat (vulpea-mentions-test--rg-match text 2 "Cabernet")
                           (vulpea-mentions-test--rg-match text 2 "Merlot")))
           (none (make-hash-table :test 'equal)))
      ;; The tail of the link reads as prose to the line check, and it
      ;; is the Org parser that turns it down
      (should (vulpea-mentions--line-unlinked-p
               "Cabernet]] and Merlot" '("Cabernet")))
      (should (equal (mapcar (lambda (m) (plist-get m :matched))
                             (vulpea-mentions--collect-outgoing
                              output text dict nil none))
                     '("Merlot"))))))

(ert-deftest vulpea-mentions--collect-outgoing-decides-ignore-once-per-candidate ()
  "The per-note ignore check runs once per candidate, not once per hit.

`vulpea-mentions--ignored-by-note-p' scans every id of the buffer's
file, and a large file holds hundreds of them; a candidate mentioned on
every other line asked the same question hundreds of times over.  With
6743 hits against 758 self ids that check alone took 0.8 s in the
process sentinel, after the search itself had finished in 0.1 s."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet" :path "/n/cab.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (let* ((dict (car (vulpea-mentions--title-dictionary)))
           (self-ids (mapcar (lambda (i) (format "self-%d" i))
                             (number-sequence 1 50)))
           (text (mapconcat (lambda (_) "had some Cabernet\nand Merlot\n")
                            (number-sequence 1 100)
                            ""))
           (output (mapconcat
                    (lambda (n)
                      (concat (vulpea-mentions-test--rg-match text n "Cabernet")
                              (vulpea-mentions-test--rg-match text (1+ n) "Merlot")))
                    (number-sequence 1 200 2)
                    ""))
           (calls 0))
      (cl-letf* ((orig (symbol-function 'vulpea-mentions--ignored-by-note-p))
                 ((symbol-function 'vulpea-mentions--ignored-by-note-p)
                  (lambda (&rest args)
                    (cl-incf calls)
                    (apply orig args))))
        (let ((mentions (vulpea-mentions--collect-outgoing
                         output text dict self-ids (make-hash-table :test 'equal))))
          (should (= (length mentions) 200))
          (should (= calls 2)))))))

(ert-deftest vulpea-mentions--compound-p ()
  "A term is compound when a word boundary falls inside it."
  (should (vulpea-mentions--compound-p "red wine"))
  (should (vulpea-mentions--compound-p "C++"))
  (should (vulpea-mentions--compound-p "Emacs入门"))
  (should-not (vulpea-mentions--compound-p "wine"))
  (should-not (vulpea-mentions--compound-p "Київ")))

(ert-deftest vulpea-mentions--line-terms ()
  "A line is read against the dictionary the way ripgrep reads it, minding words."
  (let ((dict (make-hash-table :test 'equal)))
    (dolist (term '("red" "red wine" "wine" "wines of" "of note" "ed" "金阁寺"))
      (puthash term '("id") dict))
    ;; "red wine" is no word of its own in "red wines", so "red" is
    ;; taken there.  "wines of" comes next, and "of note", which
    ;; starts inside it, is passed over.
    (should (equal (vulpea-mentions--line-terms "two red wines of note" dict 8)
                   '("red" "wines of")))
    ;; the longest term at a position wins
    (should (equal (vulpea-mentions--line-terms "a red wine, red wine" dict 8)
                   '("red wine")))
    ;; nothing longer than the longest term is tried
    (should (equal (vulpea-mentions--line-terms "a red wine" dict 4)
                   '("red" "wine")))
    ;; a spaceless script has a boundary at every character
    (should (equal (vulpea-mentions--line-terms "又名金阁寺red" dict 8)
                   '("金阁寺" "red")))
    (should-not (vulpea-mentions--line-terms "reddish wined" dict 8))))

(ert-deftest vulpea-mentions-outgoing-with-real-rg ()
  "Real ripgrep over a snapshot of buffer content yields candidate notes; links excluded."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (vulpea-test--insert-test-note "syrah" "Syrah"
                                   :path "/n/syrah.org"
                                   :properties
                                   `((,vulpea-mentions-ignore-property-key
                                      .
                                      ,vulpea-mentions-ignore-property-value)))
    (let ((content (concat "We had Cabernet Sauvignon and [[id:merlot][Merlot]].\n"
                           "More Merlot and Syrah later.\n")))
      ;; when `vulpea-mentions-exclude-linked' is non-nil
      (let ((mentions (vulpea-mentions-test--outgoing content)))
        (should (= (length mentions) 1))
        (should (equal (plist-get (car mentions) :matched) "Cabernet Sauvignon")))
      ;; when `vulpea-mentions-exclude-linked' is nil
      (let* ((vulpea-mentions-exclude-linked nil)
             (mentions (vulpea-mentions-test--outgoing content))
             (ids (sort (mapcar (lambda (m) (vulpea-note-id (plist-get m :note)))
                                mentions)
                        #'string<)))
        ;; "Cabernet Sauvignon" (bare) -> cab; "Merlot" bare on line 2 -> merlot;
        ;; the linked Merlot on line 1 is excluded.
        (should (equal ids '("cab" "merlot")))
        (let ((merlot (seq-find
                       (lambda (m) (equal (vulpea-note-id (plist-get m :note)) "merlot"))
                       mentions)))
          (should (equal (plist-get merlot :line) 2))
          (should (equal (plist-get merlot :matched) "Merlot"))
          (should (equal (plist-get merlot :context) "More Merlot and Syrah later.")))))))

(ert-deftest vulpea-mentions-outgoing-quotes-the-buffer-as-written ()
  "Case is ignored when matching, but the result quotes the buffer as written.

Ripgrep searches a case-folded copy of the buffer, so :matched and
:context have to be read back from the buffer's own text.  The dotted
capital I is there because `downcase' makes two characters of it: a
fold that changed the length of a line would quote the wrong span."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (vulpea-test--insert-test-note "kyiv" "Київ" :path "/n/kyiv.org")
    (let ((mentions (vulpea-mentions-test--outgoing
                     (concat "İstanbul, then a CABERNET sauvignon.\n"
                             "  Їдемо в КИЇВ.\n"))))
      (should (equal (mapcar (lambda (m)
                               (list (plist-get m :line)
                                     (plist-get m :matched)
                                     (plist-get m :context)))
                             mentions)
                     '((1 "CABERNET sauvignon" "İstanbul, then a CABERNET sauvignon.")
                       (2 "КИЇВ" "Їдемо в КИЇВ.")))))))

(ert-deftest vulpea-mentions-outgoing-prefers-the-longest-title ()
  "When titles nest, the longest one standing at a position is the mention.

Ripgrep reports one match per position, the first pattern that fits.
Listed in collection order, \"Wine\" could come ahead of \"Wine Tasting\"
and a tasting was then reported as a mention of Wine.  And since the
patterns carry no word boundaries, \"Java\" would claim the start of
JavaScript and hide it."
  (vulpea-test--require-rg)
  (dolist (titles '(("Wine" "Wine Tasting" "Java" "JavaScript")
                    ("JavaScript" "Java" "Wine Tasting" "Wine")))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (dolist (title titles)
        (vulpea-test--insert-test-note title title))
      (should (equal (mapcar (lambda (m) (plist-get m :matched))
                             (vulpea-mentions-test--outgoing
                              "A wine tasting, some JavaScript, then Java and wine.\n"))
                     '("wine tasting" "JavaScript" "Java" "wine"))))))

(ert-deftest vulpea-mentions-outgoing-finds-mentions-behind-a-glued-match ()
  "A title glued into a longer word does not hide a real mention next to it.

Ripgrep's matches never overlap, and it knows nothing of words.  It
reports \"red wine\" inside \"red wines\", which is not a mention of Red
Wine, and so says nothing of the \"red\" starting at the same place, nor
of the \"barn\" that \"foo bar\" ran into.  And once \"est qui\" has taken
the start of \"quis\", it goes on from the middle of that word and pairs
up the rest of the line differently."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (dolist (title '("Red Wine" "Red" "Foo Bar" "Barn"
                     "Est Qui" "Quis Qui" "Qui Culpa" "Culpa Incididunt"))
      (vulpea-test--insert-test-note title title))
    (should (equal (mapcar (lambda (m)
                             (list (plist-get m :line) (plist-get m :matched)))
                           (vulpea-mentions-test--outgoing
                            (concat "Two red wines.\n"
                                    "A foo barn.\n"
                                    "Reddish barns.\n"
                                    "Est quis qui culpa incididunt.\n")))
                   '((1 "red")
                     (2 "barn")
                     (4 "quis qui") (4 "culpa incididunt"))))))

(ert-deftest vulpea-mentions-outgoing-cjk-with-real-rg ()
  "The outgoing scan finds CJK candidates mentioned inline in prose.
Cyrillic candidates keep strict word boundaries: an inflected or glued
occurrence is not a mention."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
    `((:name "kinkakuji.org"
       :content ,(concat ":PROPERTIES:\n:ID: kinkakuji\n:END:\n"
                         "#+title: 金阁寺\n\n"))
      (:name "kyiv.org"
       :content ,(concat ":PROPERTIES:\n:ID: kyiv\n:END:\n"
                         "#+title: Київ\n\n"))
      (:name "diary.org"
       :content ,(concat ":PROPERTIES:\n:ID: diary\n:END:\n#+title: 日记\n\n"
                         "鹿苑寺，又名金阁寺。\n"
                         "Київський вокзал.\n")))
    (let ((mentions (vulpea-mentions-test--collect-outgoing-mentions-for-note
                     "diary")))
      (should (= (length mentions) 1))
      (should (equal (vulpea-note-id (plist-get (car mentions) :note))
                     "kinkakuji"))
      (should (equal (plist-get (car mentions) :matched) "金阁寺")))))

(ert-deftest vulpea-mentions-outgoing-cjk-hostile-process-coding ()
  "Outgoing CJK and Cyrillic mentions survive a non-UTF-8 coding setup.

The patterns file, the snapshot of the buffer and ripgrep's output
must stay UTF-8 even when `coding-system-for-write' and
`default-process-coding-system' say otherwise."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "kinkakuji.org"
         :content ,(concat ":PROPERTIES:\n:ID: kinkakuji\n:END:\n"
                           "#+title: 金阁寺\n\n"))
        (:name "kyiv.org"
         :content ,(concat ":PROPERTIES:\n:ID: kyiv\n:END:\n"
                           "#+title: Київ\n\n")))
    (with-temp-buffer
      (insert "鹿苑寺，又名金阁寺。\n"
              "Їдемо в Київ.\n")
      (let* ((state nil)
             (result nil)
             (default-process-coding-system '(latin-1 . latin-1))
             (coding-system-for-write 'latin-1))
        (vulpea-buffer-unlinked-mentions-async
         (lambda (ms) (setq state 'resolved result ms))
         (lambda (err) (setq state (list 'rejected err))))
        (vulpea-mentions-test--await (lambda () state))
        (should (eq state 'resolved))
        (should (equal (sort (mapcar (lambda (m) (plist-get m :matched))
                                     result)
                             #'string<)
                       '("Київ" "金阁寺")))))))

(ert-deftest vulpea-mentions-outgoing-survives-a-large-collection ()
  "The outgoing scan still answers when the collection has 60,000 titles.

Each title used to become a case-insensitive regex, and ripgrep compiles
those to about 2 KB apiece: past some 53,000 ordinary titles the set
crossed its 100 MB regex size limit, ripgrep exited with 2, and the scan
rejected instead of returning mentions.

The candidates come from a stubbed `vulpea-db-query', because inserting
60,000 rows takes longer than the rest of the test put together."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (let ((notes (cons (vulpea-db-get-by-id "cab")
                       (mapcar (lambda (i)
                                 (make-vulpea-note
                                  :id (format "n%d" i)
                                  :level 0
                                  :title (format "Nisi Duis Pariatur Excepteur %d" i)))
                               (number-sequence 1 60000)))))
      (cl-letf (((symbol-function 'vulpea-db-query)
                 (lambda (&optional predicate)
                   (if predicate (seq-filter predicate notes) notes))))
        (let ((mentions (vulpea-mentions-test--outgoing
                         "We opened a Cabernet Sauvignon.\n" 60)))
          (should (= (length mentions) 1))
          (should (equal (vulpea-note-id (plist-get (car mentions) :note))
                         "cab")))))))

(ert-deftest vulpea-mentions-outgoing-skips-wrapped-links ()
  "The outgoing scan drops a term inside a link that wraps across lines.
The link's description continues on the next line, where \"Sauvignon\"
is the title of another note; the bare \"Merlot\" after the link is
still a mention."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "sauv" "Sauvignon" :path "/n/sauv.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (let ((mentions (vulpea-mentions-test--outgoing
                     (concat "We opened a [[id:other][Cabernet\n"
                             "Sauvignon]] and then a Merlot.\n"))))
      (should (equal (mapcar (lambda (m)
                               (list (plist-get m :matched) (plist-get m :line)))
                             mentions)
                     '(("Merlot" 2)))))))

(defun vulpea-mentions-test--outgoing-temp-files (proc)
  "Return the temporary files PROC's ripgrep command reads, patterns first."
  (let ((cmd (process-command proc)))
    (list (cadr (member "-f" cmd)) (car (last cmd)))))

(ert-deftest vulpea-mentions-outgoing-searches-a-snapshot-of-the-buffer ()
  "The outgoing scan hands ripgrep a snapshot file, not its standard input.

Writing the buffer to a file lets `make-process' return at once: with
stdin, `process-send-string' blocked until ripgrep had drained the
whole buffer, and on a multi-megabyte note that was a second-long
freeze inside a loader that promised to be asynchronous.  The snapshot
carries the buffer's live text, so unsaved edits still count, and both
temporary files are gone once the search has settled."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Unsaved prose about Cabernet Sauvignon.\n")
      (let* ((state nil)
             (result nil)
             (proc (vulpea-buffer-unlinked-mentions-async
                    (lambda (ms) (setq state 'resolved result ms))
                    (lambda (err) (setq state (list 'rejected err)))))
             (files (vulpea-mentions-test--outgoing-temp-files proc)))
        (should (processp proc))
        ;; Nothing has been pumped yet, so the sentinel has not run and
        ;; the snapshot is still on disk: it must be the buffer's text,
        ;; case-folded for the search.
        (should (equal (with-temp-buffer
                         (insert-file-contents (cadr files))
                         (buffer-string))
                       "unsaved prose about cabernet sauvignon.\n"))
        (vulpea-mentions-test--await (lambda () state))
        (should (eq state 'resolved))
        (should (equal (mapcar (lambda (m) (plist-get m :matched)) result)
                       '("Cabernet Sauvignon")))
        (should (equal (plist-get (car result) :line) 1))
        (dolist (file files)
          (should-not (file-exists-p file)))
        (should-not (buffer-live-p (process-buffer proc)))))))

(ert-deftest vulpea-mentions-outgoing-cleans-up-when-cancelled ()
  "Killing the ripgrep process mid-search still removes its temporary files.
A reactive UI cancels a superseded load with `delete-process'; the
sentinel runs for that signal too, so the patterns file, the snapshot
and the output buffer must not outlive it."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Cabernet Sauvignon.\n")
      (let* ((state nil)
             (proc (vulpea-buffer-unlinked-mentions-async
                    (lambda (_ms) (setq state 'resolved))
                    (lambda (err) (setq state (list 'rejected err)))))
             (files (vulpea-mentions-test--outgoing-temp-files proc))
             (out-buf (process-buffer proc)))
        (should (buffer-live-p out-buf))
        (delete-process proc)
        (vulpea-mentions-test--await (lambda () state))
        (should state)
        (dolist (file files)
          (should-not (file-exists-p file)))
        (should-not (buffer-live-p out-buf))))))

(ert-deftest vulpea-mentions-incoming-rejects-when-ripgrep-is-killed ()
  "A signalled incoming search REJECTs instead of resolving truncated output.
Same shape as the outgoing loader: a reactive UI cancels a superseded
search with `delete-process', and `process-exit-status' reports the
signal number, which for SIGHUP is ripgrep's no-match code 1."
  (vulpea-test--with-temp-db-and-files
      `((:name "cab.org"
         :content ,(concat ":PROPERTIES:\n:ID: cab\n:END:\n"
                           "#+title: Cabernet Sauvignon\n\n")))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "rg"))
              ((symbol-function 'vulpea-mentions--rg-command)
               (lambda (&rest _) (list "sh" "-c" "exec sleep 30"))))
      (let* ((state nil)
             (proc (vulpea-note-unlinked-mentions-async
                    (vulpea-db-get-by-id "cab")
                    (lambda (_ms) (setq state 'resolved))
                    (lambda (err) (setq state (list 'rejected err))))))
        (should (processp proc))
        (signal-process proc 1)
        (vulpea-mentions-test--await (lambda () state))
        (should (eq (car-safe state) 'rejected))
        (should (string-match-p "signal" (cadr state)))))))

(ert-deftest vulpea-mentions-outgoing-rejects-when-ripgrep-is-killed ()
  "A ripgrep that dies from a signal REJECTs instead of resolving truncated output.
`process-exit-status' reports the signal number for a signalled
process, and SIGHUP is 1 - the code ripgrep exits with when nothing
matched.  Read as an exit code, a hangup would resolve with whatever
output had arrived so far."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Cabernet Sauvignon.\n")
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "rg"))
                ((symbol-function 'vulpea-mentions--rg-file-command)
                 (lambda (&rest _) (list "sh" "-c" "exec sleep 30"))))
        (let* ((state nil)
               (proc (vulpea-buffer-unlinked-mentions-async
                      (lambda (_ms) (setq state 'resolved))
                      (lambda (err) (setq state (list 'rejected err))))))
          (signal-process proc 1)
          (vulpea-mentions-test--await (lambda () state))
          (should (eq (car-safe state) 'rejected))
          (should (string-match-p "signal" (cadr state))))))))

(ert-deftest vulpea-mentions-outgoing-rejects-when-the-search-cannot-start ()
  "A failure before ripgrep runs REJECTs and leaves nothing behind.
The promise contract is that exactly one of RESOLVE or REJECT is
called; a `make-process' that signals (ripgrep gone since
`executable-find' found it, say) must not escape the loader with the
snapshot still on disk and an output buffer still live."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Cabernet Sauvignon.\n")
      (let ((temp-files nil)
            (state nil))
        (cl-letf* ((orig-make-temp-file (symbol-function 'make-temp-file))
                   ((symbol-function 'make-temp-file)
                    (lambda (&rest args)
                      (let ((file (apply orig-make-temp-file args)))
                        (push file temp-files)
                        file)))
                   ((symbol-function 'executable-find) (lambda (&rest _) "rg"))
                   ((symbol-function 'make-process)
                    (lambda (&rest _)
                      (signal 'file-missing '("Searching for program" "rg")))))
          (should-not (vulpea-buffer-unlinked-mentions-async
                       (lambda (_ms) (setq state 'resolved))
                       (lambda (err) (setq state (list 'rejected err))))))
        (should (eq (car-safe state) 'rejected))
        (should (= (length temp-files) 2))
        (dolist (file temp-files)
          (should-not (file-exists-p file)))
        (should-not (seq-some (lambda (b)
                                (string-prefix-p " *vulpea-mentions-out*"
                                                 (buffer-name b)))
                              (buffer-list)))))))

(ert-deftest vulpea-mentions--snapshot-buffer-p-only-for-plain-org ()
  "Only a buffer with no file or a plain .org file may be snapshotted to disk.
Any other extension is one of `vulpea-db-extra-extensions', which exist
for encrypted notes (.org.gpg, .org.age): their decrypted content must
never be written outside the buffer."
  (with-temp-buffer
    (should (vulpea-mentions--snapshot-buffer-p))
    (dolist (case '(("/n/plain.org" . t)
                    ("/n/secret.org.gpg" . nil)
                    ("/n/secret.org.age" . nil)
                    ("/n/Notes.ORG" . nil)))
      (setq buffer-file-name (car case))
      (should (eq (and (vulpea-mentions--snapshot-buffer-p) t) (cdr case))))
    ;; An indirect buffer has no `buffer-file-name' of its own; the
    ;; base buffer's file decides (org makes such buffers routinely).
    (setq buffer-file-name "/n/secret.org.gpg")
    (set-buffer-modified-p nil)
    (let ((indirect (make-indirect-buffer (current-buffer) " *vmt-indirect*")))
      (unwind-protect
          (with-current-buffer indirect
            (should-not buffer-file-name)
            (should-not (vulpea-mentions--snapshot-buffer-p)))
        (kill-buffer indirect)))
    (setq buffer-file-name nil)))

(ert-deftest vulpea-mentions-outgoing-feeds-an-encrypted-buffer-over-stdin ()
  "A buffer visiting an encrypted note is searched over stdin, not a snapshot.
The configuration guide promises that decrypted content is never
persisted outside the parse buffer, and epa-file itself turns off
auto-save for such buffers.  The search still runs on the live text and
still cleans up the patterns file and the output buffer."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Secret prose about Cabernet Sauvignon.\n")
      (setq buffer-file-name "/n/secret.org.gpg")
      (set-buffer-modified-p nil)
      (let ((temp-files nil)
            (state nil)
            (result nil))
        (cl-letf* ((orig-make-temp-file (symbol-function 'make-temp-file))
                   ((symbol-function 'make-temp-file)
                    (lambda (&rest args)
                      (let ((file (apply orig-make-temp-file args)))
                        (push file temp-files)
                        file))))
          (let* ((proc (vulpea-buffer-unlinked-mentions-async
                        (lambda (ms) (setq state 'resolved result ms))
                        (lambda (err) (setq state (list 'rejected err)))))
                 (out-buf (process-buffer proc)))
            (should (processp proc))
            (should (equal (car (last (process-command proc))) "-"))
            ;; Only the patterns file was created: no snapshot of the
            ;; decrypted buffer ever touched the disk.
            (should (= (length temp-files) 1))
            (should (string-match-p "vulpea-mentions-pat-" (car temp-files)))
            (vulpea-mentions-test--await (lambda () state))
            (should (eq state 'resolved))
            (should (equal (mapcar (lambda (m) (plist-get m :matched)) result)
                           '("Cabernet Sauvignon")))
            (should-not (file-exists-p (car temp-files)))
            (should-not (buffer-live-p out-buf))))
        (setq buffer-file-name nil)))))

(ert-deftest vulpea-mentions-outgoing-quit-during-send-still-settles ()
  "Quitting while the pipe send blocks kills ripgrep and REJECTs.
The send of an encrypted buffer blocks the caller, so `C-g' is the one
thing a user will press there.  A quit is not an `error', so it must
not slip past the handlers and leave the promise unsettled, ripgrep
waiting on stdin, and the patterns file on disk."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet Sauvignon" :path "/n/cab.org")
    (with-temp-buffer
      (insert "Cabernet Sauvignon.\n")
      (setq buffer-file-name "/n/secret.org.gpg")
      (set-buffer-modified-p nil)
      (let ((state nil)
            (temp-files nil)
            (procs nil))
        (cl-letf* ((orig-make-temp-file (symbol-function 'make-temp-file))
                   ((symbol-function 'make-temp-file)
                    (lambda (&rest args)
                      (let ((file (apply orig-make-temp-file args)))
                        (push file temp-files)
                        file)))
                   (orig-make-process (symbol-function 'make-process))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (let ((proc (apply orig-make-process args)))
                        (push proc procs)
                        proc)))
                   ((symbol-function 'process-send-string)
                    (lambda (&rest _) (signal 'quit nil))))
          ;; `should-error' catches `error' conditions only.
          (should (eq 'quit
                      (condition-case nil
                          (progn
                            (vulpea-buffer-unlinked-mentions-async
                             (lambda (_ms) (setq state 'resolved))
                             (lambda (err) (setq state (list 'rejected err))))
                            'no-quit)
                        (quit 'quit)))))
        (vulpea-mentions-test--await (lambda () state) 3)
        (should (eq (car-safe state) 'rejected))
        (should (= (length procs) 1))
        (should-not (process-live-p (car procs)))
        (should-not (buffer-live-p (process-buffer (car procs))))
        (dolist (file temp-files)
          (should-not (file-exists-p file)))
        (setq buffer-file-name nil)))))

(ert-deftest vulpea-mentions-outgoing-rejects-without-rg ()
  "When ripgrep is unavailable, the outgoing search REJECTs."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (with-temp-buffer
      (let ((state nil))
        (vulpea-buffer-unlinked-mentions-async
         (lambda (_ms) (setq state 'resolved))
         (lambda (_e) (setq state 'rejected)))
        (should (eq state 'rejected))))))

;;; Prose

;; A mention is a title in prose.  The scans and `vulpea-mentions-link'
;; ask the same question of the same code, so what one lists the other
;; links; the corpora below are read by all three.

(defun vulpea-mentions-test--link (content line note &optional context)
  "Link the mentions of NOTE on LINE of an Org buffer holding CONTENT.

Return (COUNT TEXT): what `vulpea-mentions-link' returned and the text
of the buffer after it.  CONTEXT is passed on."
  (with-temp-buffer
    (insert content)
    (delay-mode-hooks (org-mode))
    (list (vulpea-mentions-link (current-buffer) line note context)
          (buffer-string))))

(defun vulpea-mentions-test--linkable-lines (content note)
  "Return the lines of CONTENT on which a mention of NOTE gets linked.
Each line is tried in a fresh Org buffer holding CONTENT."
  (seq-filter
   (lambda (line)
     (> (car (vulpea-mentions-test--link content line note)) 0))
   (number-sequence 1 (length (split-string content "\n")))))

(defun vulpea-mentions-test--incoming-lines (content note)
  "Return the lines of CONTENT the incoming search lists for NOTE.

CONTENT becomes a file of a collection that also holds NOTE's own
file, and real ripgrep searches it."
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ,(format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n"
                           (vulpea-note-id note) (vulpea-note-title note)))
        (:name "mentioner.org" :content ,content))
    (sort (mapcar (lambda (m) (plist-get m :line))
                  (vulpea-mentions-test--collect-incoming-mentions-for-note
                   (vulpea-note-id note)))
          #'<)))

(defun vulpea-mentions-test--outgoing-lines (content note)
  "Return the lines of CONTENT the outgoing search lists for NOTE.
NOTE is the only note of the collection."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note (vulpea-note-id note) (vulpea-note-title note)
                                   :path "/n/target.org")
    (sort (mapcar (lambda (m) (plist-get m :line))
                  (vulpea-mentions-test--outgoing content))
          #'<)))

(defconst vulpea-mentions-test--prose-corpus
  (concat
   ":PROPERTIES:\n"                                              ; 1
   ":ID: mentioner\n"                                            ; 2
   ":END:\n"                                                     ; 3
   "#+title: Mentioner\n"                                        ; 4
   "#+filetags: :emacs:\n"                                       ; 5
   "\n"                                                          ; 6
   "Plain Emacs in a paragraph.\n"                               ; 7 *
   "Glued Emacsen and GNUEmacs do not count.\n"                  ; 8
   "Possessive Emacs's and quoted \"Emacs\" do.\n"               ; 9 *
   "A link [[id:other][Emacs]] is taken.\n"                      ; 10
   "A link [[id:other][to Emacs with [brackets] inside]] too.\n" ; 11
   "A wrapped [[id:other][link to\n"                             ; 12
   "Emacs]] and a bare Emacs after it.\n"                        ; 13 *
   "Code ~Emacs~ and verbatim =Emacs= are syntax.\n"             ; 14
   "A URL https://example.com/Emacs is a link.\n"                ; 15
   "A <<Emacs>> target, a {{{Emacs}}} macro, a note[fn:Emacs].\n" ; 16
   "# Emacs in a comment\n"                                      ; 17
   "Stamp <2024-01-15 Mon> then Emacs follows.\n"                ; 18 *
   "*Emacs* in bold and /Emacs/ in italics.\n"                   ; 19 *
   "\n"                                                          ; 20
   "#+begin_src emacs-lisp\n"                                    ; 21
   "(message \"Emacs\")\n"                                       ; 22
   "#+end_src\n"                                                 ; 23
   "\n"                                                          ; 24
   "#+begin_example\n"                                           ; 25
   "Emacs\n"                                                     ; 26
   "#+end_example\n"                                             ; 27
   "\n"                                                          ; 28
   "#+begin_quote\n"                                             ; 29
   "Emacs in a quote.\n"                                         ; 30 *
   "#+end_quote\n"                                               ; 31
   "\n"                                                          ; 32
   "- Emacs :: a description term\n"                             ; 33 *
   "- an item about Emacs\n"                                     ; 34 *
   "\n"                                                          ; 35
   "| Emacs | in a table |\n"                                    ; 36 *
   "\n"                                                          ; 37
   "#+BEGIN: clocktable\n"                                       ; 38
   "| Emacs | generated |\n"                                     ; 39
   "#+END:\n"                                                    ; 40
   "\n"                                                          ; 41
   "#+RESULTS:\n"                                                ; 42
   "| Emacs | computed |\n"                                      ; 43
   "\n"                                                          ; 44
   "An inline note[fn:: about Emacs] counts.\n"                  ; 45 *
   ": Emacs in fixed width\n"                                    ; 46
   "\n"                                                          ; 47
   "* Setup notes :emacs:work:\n"                                ; 48
   "* TODO Learn Emacs :emacs:\n"                                ; 49 *
   "SCHEDULED: <2024-01-15 Mon>\n"                               ; 50
   ":PROPERTIES:\n"                                              ; 51
   ":CUSTOM: Emacs\n"                                            ; 52
   ":END:\n"                                                     ; 53
   "* Emacs\n")                                                  ; 54 *
  "A note whose lines hold the word Emacs in every kind of place.")

(defconst vulpea-mentions-test--prose-lines '(7 9 13 18 19 30 33 34 36 45 49 54)
  "The lines of `vulpea-mentions-test--prose-corpus' with Emacs in prose.")

(ert-deftest vulpea-mentions-link-and-scans-agree-on-prose ()
  "The link function links exactly the lines both searches list.
A title counts where it is prose.  Where it is Org syntax - a tag, a
keyword, a timestamp, a link, code, a block, a comment, generated
content - a link in its place would break what is there, so no search
lists it and nothing is linked."
  (vulpea-test--require-rg)
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs"))
        (corpus vulpea-mentions-test--prose-corpus)
        (lines vulpea-mentions-test--prose-lines))
    (should (equal (vulpea-mentions-test--linkable-lines corpus note) lines))
    (should (equal (vulpea-mentions-test--incoming-lines corpus note) lines))
    (should (equal (vulpea-mentions-test--outgoing-lines corpus note) lines))))

(ert-deftest vulpea-mentions-link-and-scans-agree-on-todo-keywords ()
  "A todo keyword is not a mention of a note called after it.
Neither is the keyword in the record Org writes on a state change.  The
keywords a file declares for itself count like the global ones."
  (vulpea-test--require-rg)
  (let ((corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "#+todo: NEXT | DONE FINISHED\n"                           ; 4
                 "\n"                                                       ; 5
                 "* DONE Ship it\n"                                         ; 6
                 "CLOSED: [2024-01-15 Mon 10:00]\n"                         ; 7
                 "- State \"DONE\"       from \"NEXT\"       [2024-01-15 Mon 10:00]\n" ; 8
                 "* DONE Done is a word of this title\n"                    ; 9 *
                 "We are done here, and next comes lunch.\n"                ; 10 *
                 "* NEXT Step\n"                                            ; 11
                 "* FINISHED Next to nothing\n"                             ; 12 *
                 "* COMMENT Comment on the plan\n")))                       ; 13 *
    (dolist (case '(("Done" (9 10)) ("Next" (10 12)) ("Comment" (13))))
      (let ((note (make-vulpea-note :id "word" :title (car case)))
            (lines (cadr case)))
        (should (equal (vulpea-mentions-test--linkable-lines corpus note) lines))
        (should (equal (vulpea-mentions-test--incoming-lines corpus note) lines))
        (should (equal (vulpea-mentions-test--outgoing-lines corpus note) lines))))
    ;; in a heading only the title is linked
    (should (equal (vulpea-mentions-test--link
                    corpus 9 (make-vulpea-note :id "done" :title "Done"))
                   (list 1 (string-replace
                            "* DONE Done is"
                            "* DONE [[id:done][Done]] is"
                            corpus))))
    ;; and the word that comments a heading out is not its title
    (should (equal (vulpea-mentions-test--link
                    corpus 13 (make-vulpea-note :id "c" :title "Comment"))
                   (list 1 (string-replace
                            "* COMMENT Comment on"
                            "* COMMENT [[id:c][Comment]] on"
                            corpus))))))

(ert-deftest vulpea-mentions-link-and-scans-agree-on-dates ()
  "A date in a timestamp is not a mention of the daily note of that day."
  (vulpea-test--require-rg)
  (let ((note (make-vulpea-note :id "day" :title "2024-01-15"))
        (corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "* Meeting\n"                                              ; 4
                 "SCHEDULED: <2024-01-15 Mon>\n"                            ; 5
                 ":LOGBOOK:\n"                                              ; 6
                 "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:00] =>  1:00\n" ; 7
                 ":END:\n"                                                  ; 8
                 "Met on [2024-01-15 Mon] and again on <2024-01-15 Mon>.\n" ; 9
                 "See the 2024-01-15 entry.\n")))                           ; 10 *
    (should (equal (vulpea-mentions-test--linkable-lines corpus note) '(10)))
    (should (equal (vulpea-mentions-test--incoming-lines corpus note) '(10)))
    (should (equal (vulpea-mentions-test--outgoing-lines corpus note) '(10)))))

(ert-deftest vulpea-mentions-link-and-scans-agree-on-radio-targets ()
  "Text that Org links to a radio target is a link already."
  (vulpea-test--require-rg)
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs"))
        (corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "The word <<<Emacs>>> is defined here.\n"                  ; 4
                 "Later Emacs is a link to it, on its own.\n")))            ; 5
    (should-not (vulpea-mentions-test--linkable-lines corpus note))
    (should-not (vulpea-mentions-test--incoming-lines corpus note))
    (should-not (vulpea-mentions-test--outgoing-lines corpus note))))

(ert-deftest vulpea-mentions-link-and-scans-agree-on-odd-text ()
  "Bracket pairs that are no link, underscores and logged states.
Brackets on two lines read as a link to a regexp, but not to Org.  An
underscore next to a title is no word character to Emacs.  A state
note may have no new state at all."
  (vulpea-test--require-rg)
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs"))
        (corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "Type =[[= to open a link, and\n"                          ; 4
                 "Emacs closes it with =]]=.\n"                             ; 5 *
                 "\n"                                                       ; 6
                 "Typing [[ starts a link.\n"                               ; 7
                 "\n"                                                       ; 8
                 "Emacs closes it with ]].\n"                               ; 9 *
                 "An _Emacs_ word, and Emacs_ alone.\n")))                  ; 10 *
    (should (equal (vulpea-mentions-test--linkable-lines corpus note) '(5 9 10)))
    (should (equal (vulpea-mentions-test--incoming-lines corpus note) '(5 9 10)))
    (should (equal (vulpea-mentions-test--outgoing-lines corpus note) '(5 9 10))))
  (let ((note (make-vulpea-note :id "next" :title "Next"))
        (corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "* Task\n"                                                 ; 4
                 "- State             from \"NEXT\"       [2024-01-15 Mon 10:00]\n" ; 5
                 "What comes next?\n")))                                    ; 6 *
    (should (equal (vulpea-mentions-test--linkable-lines corpus note) '(6)))
    (should (equal (vulpea-mentions-test--incoming-lines corpus note) '(6)))
    (should (equal (vulpea-mentions-test--outgoing-lines corpus note) '(6)))))

(ert-deftest vulpea-mentions-link-and-scans-agree-on-inline-tasks ()
  "The END line of an inline task is no title."
  (vulpea-test--require-rg)
  (require 'org-inlinetask)
  (let ((note (make-vulpea-note :id "end" :title "End"))
        (corpus (concat
                 ":PROPERTIES:\n"                                           ; 1
                 ":ID: mentioner\n"                                         ; 2
                 ":END:\n"                                                  ; 3
                 "*************** TODO Fix the end\n"                       ; 4 *
                 "Body text.\n"                                             ; 5
                 "*************** END\n")))                                 ; 6
    (should (equal (vulpea-mentions-test--linkable-lines corpus note) '(4)))
    (should (equal (vulpea-mentions-test--incoming-lines corpus note) '(4)))
    (should (equal (vulpea-mentions-test--outgoing-lines corpus note) '(4)))))

(ert-deftest vulpea-mentions-scans-read-setup-files-next-to-the-note ()
  "A relative #+SETUPFILE is read from the directory of the note.
Whatever buffer is current when the search settles, the todo keywords
the setup file declares are the ones the note sees when visited."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Waiting\n")
        (:name "setup.setup" :content "#+todo: TODO WAITING | DONE\n")
        (:name "tasks.org"
         :content ,(concat ":PROPERTIES:\n:ID: tasks\n:END:\n"
                           "#+setupfile: setup.setup\n\n"
                           "* WAITING Reply from Bob\n"
                           "Still waiting for it.\n")))
    (let ((path (expand-file-name "tasks.org" dir))
          (note (vulpea-db-get-by-id "target")))
      ;; the note itself, visited
      (let ((buffer (find-file-noselect path)))
        (unwind-protect
            (with-current-buffer buffer
              (should (= (vulpea-mentions-link buffer 6 note) 0))
              (should (= (vulpea-mentions-link buffer 7 note) 1)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      ;; the incoming search, settling in a buffer somewhere else
      (let ((default-directory temporary-file-directory))
        (with-temp-buffer
          (should (equal (mapcar (lambda (m) (plist-get m :line))
                                 (vulpea-mentions-test--collect-incoming-mentions-for-note
                                  "target"))
                         '(7)))))
      ;; the outgoing search of a buffer visiting the note
      (with-temp-buffer
        (insert-file-contents path)
        (setq default-directory (file-name-directory path))
        (let ((state nil) (result nil))
          (vulpea-buffer-unlinked-mentions-async
           (lambda (ms) (setq state 'resolved result ms))
           (lambda (err) (setq state (list 'rejected err))))
          (let ((default-directory temporary-file-directory))
            (with-temp-buffer
              (vulpea-mentions-test--await (lambda () state))))
          (should (eq state 'resolved))
          (should (equal (mapcar (lambda (m) (plist-get m :line)) result)
                         '(7))))))))

(ert-deftest vulpea-mentions-incoming-counts-only-links-org-sees ()
  "Link text that Org does not read as a note's link leaves the file listed.
In a comment or a code block the text of a link is no link, and in a
dynamic block or babel results it is generated, which the database
leaves out as well."
  (vulpea-test--require-rg)
  (let ((files
         (mapcar
          (lambda (case)
            (list :name (concat (car case) ".org")
                  :content (concat ":PROPERTIES:\n:ID: " (car case) "\n:END:\n"
                                   "A lovely Cabernet.\n\n" (cadr case))))
          '(("dblock" "#+BEGIN: related\n- [[id:target][a wine]]\n#+END:\n")
            ("results" "#+RESULTS:\n- [[id:target][a wine]]\n")
            ("comment" "# old: [[id:target][a wine]]\n")
            ("code" "#+begin_src org\n[[id:target][a wine]]\n#+end_src\n")
            ("verbatim" "See =[[id:target][a wine]]= here.\n")))))
    (vulpea-test--with-temp-db-and-files
        (cons '(:name "target.org"
                :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
              files)
      (should (equal (sort (mapcar (lambda (m) (vulpea-note-id (plist-get m :note)))
                                   (vulpea-mentions-test--collect-incoming-mentions-for-note
                                    "target"))
                           #'string<)
                     '("code" "comment" "dblock" "results" "verbatim"))))))

(ert-deftest vulpea-mentions-incoming-reads-files-with-mixed-line-endings ()
  "A line that ends in a carriage return in Emacs still reads as ripgrep saw it."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      '((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
        (:name "mixed.org"
         :content ":PROPERTIES:\n:ID: mixed\n:END:\n#+title: Mixed\n\nA lovely Cabernet.\r\n~Cabernet~ as code.\r\n"))
    (should (equal (mapcar (lambda (m) (plist-get m :line))
                           (vulpea-mentions-test--collect-incoming-mentions-for-note
                            "target"))
                   '(6)))))

(ert-deftest vulpea-mentions-link-and-scans-read-a-word-cut-by-the-restriction ()
  "A restriction that cuts a word does not make a word of its part."
  (vulpea-test--require-rg)
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (dolist (case '(("Use GNUEmacs daily.\n" "Emacs" nil)
                    ("Use Emacsen daily.\n" nil "en")))
      (with-temp-buffer
        (insert (car case))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (narrow-to-region
         (if (cadr case) (progn (search-forward (cadr case)) (match-beginning 0))
           (point-min))
         (if (nth 2 case)
             (progn (goto-char (point-min)) (search-forward (nth 2 case))
                    (match-beginning 0))
           (point-max)))
        (should (= (vulpea-mentions-link (current-buffer) 1 note) 0))
        (should (equal (buffer-string) (buffer-substring (point-min) (point-max))))
        (vulpea-test--with-temp-db
          (vulpea-db)
          (vulpea-test--insert-test-note "emacs" "Emacs" :path "/n/emacs.org")
          (let ((state nil) (result nil))
            (vulpea-buffer-unlinked-mentions-async
             (lambda (ms) (setq state 'resolved result ms))
             (lambda (err) (setq state (list 'rejected err))))
            (vulpea-mentions-test--await (lambda () state))
            (should (eq state 'resolved))
            (should-not result)))))))

(ert-deftest vulpea-mentions-link-leaves-point-after-the-mention ()
  "Point after a mention, or inside it, ends up after the link."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (dolist (offset '(0 2))
      (with-temp-buffer
        (insert "I use Emacs daily.\n")
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (search-forward "Emacs")
        (backward-char offset)
        (should (= (vulpea-mentions-link (current-buffer) 1 note) 1))
        (should (looking-at-p " daily\\.$"))))))

(ert-deftest vulpea-mentions-scans-keep-quiet-about-radio-targets ()
  "Reading a note with a radio target does not upset Org's element cache."
  (vulpea-test--require-rg)
  (let ((warnings nil)
        (note (make-vulpea-note :id "emacs" :title "Emacs"))
        (corpus (concat ":PROPERTIES:\n:ID: mentioner\n:END:\n"
                        "The word <<<Emacs>>> is defined here.\n"
                        "Later Emacs is a link to it.\n")))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (vulpea-mentions-test--incoming-lines corpus note)
      (vulpea-mentions-test--outgoing-lines corpus note))
    (should-not warnings)))

(defconst vulpea-mentions-test--object-corpus
  (concat
   "Plain *bold* /it/ _under_ +strike+ =verb= ~code~ text.\n"
   "Adjacent *a*/b/ and [[id:x][link]]*bold* and [[id:x][y]]_sub and x_{s}.\n"
   "Emphasis *across\nlines* and a [[id:x][link *with bold*\nwrapped]] here.\n"
   "Inline note[fn:: with *bold* inside] and [fn:label] and \\alpha entity.\n"
   "Stamp <2024-01-15 Mon> and [2024-01-15 Mon] and {{{macro(x)}}} and $x$.\n"
   "Ends with an object right before it: [[id:x][y]]Emacs and src_elisp{(+ 1 2)}.\n"
   "\n"
   "#+begin_verse\n"
   "A verse *with bold*\n"
   "and ~code~ in it\n"
   "#+end_verse\n"
   "\n"
   "| *cell* | [[id:x][link]] | plain |\n"
   "| ~c~    | x_{s}          | 2024  |\n")
  "Text whose every position the object lookup is compared on.")

(ert-deftest vulpea-mentions--context-matches-org-element-context ()
  "Looking objects up in what was lexed before gives what Org gives.
The scratch copy a search reads remembers the objects of a container,
and the result must be the one `org-element-context' returns, at every
position, in any order.  Both are asked in the same buffer: what
`org-element-at-point' returns at the start of a table depends on what
its cache holds already."
  (let ((org-element-use-cache t))
    (with-temp-buffer
      (vulpea-mentions--scratch-fill
       (lambda () (insert vulpea-mentions-test--object-corpus)))
      (should vulpea-mentions--lexed)
      (cl-flet ((same-p (pos)
                  (goto-char pos)
                  (let ((mine (vulpea-mentions--context (org-element-at-point)))
                        (org (org-element-context)))
                    (or (equal (list (org-element-type mine)
                                     (org-element-property :begin mine)
                                     (org-element-property :end mine))
                               (list (org-element-type org)
                                     (org-element-property :begin org)
                                     (org-element-property :end org)))
                        (list pos mine org)))))
        ;; in order, the way the searches ask, and then backwards, past
        ;; what was lexed and remembered
        (dolist (pos (append (number-sequence 1 (buffer-size))
                             (number-sequence (buffer-size) 1 -1)))
          (should (eq t (same-p pos))))))))

(ert-deftest vulpea-mentions--context-stops-at-an-empty-object ()
  "An empty object at the end of a container ends the lexing.
The last cell of a table row at the end of the buffer is empty, and
lexed again from its end it comes back again: remembering objects
until one starts past the end never came to an end."
  (let ((org-element-use-cache t)
        (text "| a | b |\n| _ Emacs | +x+ |"))
    (with-temp-buffer
      (vulpea-mentions--scratch-fill (lambda () (insert text)))
      (dolist (pos (number-sequence 1 (1+ (buffer-size))))
        (goto-char pos)
        (let ((mine (vulpea-mentions--context (org-element-at-point)))
              (org (org-element-context)))
          (should (equal (list (org-element-type mine)
                               (org-element-property :begin mine)
                               (org-element-property :end mine))
                         (list (org-element-type org)
                               (org-element-property :begin org)
                               (org-element-property :end org)))))))))

(defconst vulpea-mentions-test--fuzz-tokens
  ["Emacs" "word" " " " " " " "\n" "\t" "*" "/" "_" "+" "=" "~" "|" "\\"
   "*b*" "/i/" "+s+" "=v=" "~c~" "*a\nb*" "*bold /it/ x*"
   "[[id:x][" "]]" "[[id:x][d]]" "[[id:x][*b* d]]" "[[id:x][a\nb]]"
   "[[https://e.org]]" "https://e.org/x" "<https://e.org>" "[[x]]"
   "[fn::" "]" "[fn:l]" "[fn::a\nb]" "\\\\" "\\alpha" "\\alpha{}"
   "src_elisp{(+ 1 2)}" "call_f()" "@@html:<b>@@" "[cite:@a;@b]"
   "[1/2]" "[33%]" "<2024-01-15 Mon>" "[2024-01-15 Mon]" "$x$" "\\(x\\)"
   "{{{m(a)}}}" "<<t>>" "<<<radio>>>" "x^2" "x_{s}" "a_b" "^{up}"]
  "Pieces the documents of `vulpea-mentions--context-survives-random-text' are made of.")

(defun vulpea-mentions-test--fuzz-doc ()
  "Return a random Org document made of `vulpea-mentions-test--fuzz-tokens'."
  (cl-flet ((inline (n &optional flat)
              (mapconcat
               (lambda (_)
                 (let ((token (aref vulpea-mentions-test--fuzz-tokens
                                    (random (length vulpea-mentions-test--fuzz-tokens)))))
                   (if (and flat (string-match-p "[\n|]" token)) " " token)))
               (number-sequence 1 n) "")))
    (mapconcat
     (lambda (_)
       (pcase (random 5)
         ((or 0 1) (concat (string-trim-left (inline (+ 5 (random 30)))) "\n"))
         (2 (concat "| " (inline (+ 1 (random 5)) t) " | "
                    (inline (+ 1 (random 5)) t) " |\n"))
         (3 (concat "#+begin_verse\n" (inline (+ 5 (random 20))) "\n#+end_verse\n"))
         (4 (concat "* " (inline (+ 1 (random 5)) t) "\n"))))
     (number-sequence 0 (+ 2 (random 4)))
     (if (zerop (random 2)) "\n" ""))))

(ert-deftest vulpea-mentions--context-survives-random-text ()
  "The object lookup agrees with `org-element-context' on random documents.
Every position of every document is asked about forwards, backwards
and in random order, and a document may end without a newline."
  (random "vulpea")
  (let ((org-element-use-cache t))
    (dotimes (_ 12)
      (let ((doc (vulpea-mentions-test--fuzz-doc)))
        (when (zerop (random 2))
          (setq doc (string-trim-right doc)))
        (dolist (order '(forward backward random))
          (with-temp-buffer
            (vulpea-mentions--scratch-fill (lambda () (insert doc)))
            (let ((positions (number-sequence 1 (1+ (buffer-size)))))
              (pcase order
                ('backward (setq positions (nreverse positions)))
                ('random (setq positions
                               (sort positions (lambda (_a _b) (zerop (random 2)))))))
              (dolist (pos positions)
                (goto-char pos)
                (let* ((element (org-element-at-point))
                       (mine (vulpea-mentions--context element))
                       (org (progn (goto-char pos) (org-element-context element))))
                  (should (equal (list doc pos
                                       (org-element-type mine)
                                       (org-element-property :begin mine)
                                       (org-element-property :end mine))
                                 (list doc pos
                                       (org-element-type org)
                                       (org-element-property :begin org)
                                       (org-element-property :end org)))))))))))))

(ert-deftest vulpea-mentions-outgoing-lexes-a-long-paragraph-once ()
  "A long paragraph with a title on every line costs one pass, not one per line.
Asking `org-element-context' about every occurrence lexes the
paragraph from its start each time, which took seconds for a thousand
lines, in a process sentinel that cannot be interrupted."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "emacs" "Emacs" :path "/n/emacs.org")
    (let ((lexed 0)
          (lines 1000))
      (cl-letf* ((orig (symbol-function 'org-element--object-lex))
                 ((symbol-function 'org-element--object-lex)
                  (lambda (&rest args)
                    (cl-incf lexed)
                    (apply orig args))))
        (let ((mentions (vulpea-mentions-test--outgoing
                         (mapconcat (lambda (i)
                                      (format "Line %d talks about Emacs and ~code~.\n" i))
                                    (number-sequence 1 lines)
                                    ""))))
          (should (= (length mentions) lines))
          (should (< lexed (* 3 lines))))))))

;;; Linking a mention

(ert-deftest vulpea-mentions-link-replaces-mentions-on-the-line ()
  "Every mention on the line becomes a link, and no other line is touched."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    ;; the text as written is kept as the description
    (should (equal (vulpea-mentions-test--link "I use emacs daily.\n" 1 note)
                   '(1 "I use [[id:emacs][emacs]] daily.\n")))
    (should (equal (vulpea-mentions-test--link
                    "Emacs here.\nEmacs and Emacs there.\nEmacs last.\n" 2 note)
                   (list 2 (concat "Emacs here.\n"
                                   "[[id:emacs][Emacs]] and [[id:emacs][Emacs]] there.\n"
                                   "Emacs last.\n"))))
    ;; a last line without a newline is a line too
    (should (equal (vulpea-mentions-test--link "one\nEmacs" 2 note)
                   '(1 "one\n[[id:emacs][Emacs]]")))
    ;; nothing left to link the second time
    (should (equal (vulpea-mentions-test--link
                    "[[id:emacs][Emacs]] and [[id:emacs][Emacs]] there.\n" 1 note)
                   '(0 "[[id:emacs][Emacs]] and [[id:emacs][Emacs]] there.\n")))))

(ert-deftest vulpea-mentions-link-keeps-point-and-narrowing ()
  "Linking leaves point on the text it was on, and the restriction as it was."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (with-temp-buffer
      (insert "First line.\nAn Emacs line.\nLast line.\n")
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (search-forward "Last")
      (should (= (vulpea-mentions-link (current-buffer) 2 note) 1))
      (should (looking-at-p " line\\.$"))
      (should (looking-back "Last" (line-beginning-position)))
      (should-not (buffer-narrowed-p)))))

(ert-deftest vulpea-mentions-link-checks-the-line ()
  "A line that is not there, or reads differently than expected, is left alone."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs"))
        (text "  An Emacs line.  \nAnother Emacs line.\n"))
    (should (equal (vulpea-mentions-test--link text 0 note) (list 0 text)))
    (should (equal (vulpea-mentions-test--link text -1 note) (list 0 text)))
    (should (equal (vulpea-mentions-test--link text 3 note) (list 0 text)))
    (should (equal (vulpea-mentions-test--link text 9 note) (list 0 text)))
    ;; the context is the trimmed line, as the searches report it
    (should (equal (vulpea-mentions-test--link text 1 note "An Emacs line.")
                   (list 1 "  An [[id:emacs][Emacs]] line.  \nAnother Emacs line.\n")))
    ;; the line moved since the search: nothing lands on its neighbor
    (should (equal (vulpea-mentions-test--link text 2 note "An Emacs line.")
                   (list 0 text)))))

(ert-deftest vulpea-mentions-link-reads-terms-like-the-search ()
  "Titles and aliases are looked for the way the searches look for them."
  ;; an alias too short to be searched for is not linked either, and a
  ;; longer name wins over a shorter one it starts with
  (let ((vulpea-mentions-min-term-length 3)
        (note (make-vulpea-note :id "emacs" :title "Emacs"
                                :aliases '("Emacs Lisp" "EL"))))
    (should (equal (vulpea-mentions-test--link
                    "Emacs Lisp, EL and Emacs.\n" 1 note)
                   '(2 "[[id:emacs][Emacs Lisp]], EL and [[id:emacs][Emacs]].\n"))))
  ;; a name that reads as part of a longer word is not a mention
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (should (equal (vulpea-mentions-test--link "Emacsen and GNUEmacs.\n" 1 note)
                   '(0 "Emacsen and GNUEmacs.\n")))
    ;; a possessive, and neighbors a word regexp takes for word
    ;; characters in an Org buffer
    (should (equal (vulpea-mentions-test--link "Emacs's manual.\n" 1 note)
                   '(1 "[[id:emacs][Emacs]]'s manual.\n")))
    (should (equal (vulpea-mentions-test--link
                    "An $Emacs fee and an Emacs% share.\n" 1 note)
                   '(2 "An $[[id:emacs][Emacs]] fee and an [[id:emacs][Emacs]]% share.\n")))
    ;; a Latin title glued into a script that has no spaces
    (should (equal (vulpea-mentions-test--link "我爱用Emacs写作\n" 1 note)
                   '(1 "我爱用[[id:emacs][Emacs]]写作\n"))))
  ;; a title that ends in punctuation
  (let ((note (make-vulpea-note :id "cpp" :title "C++")))
    (should (equal (vulpea-mentions-test--link "I like C++ and C++.\n" 1 note)
                   '(2 "I like [[id:cpp][C++]] and [[id:cpp][C++]].\n"))))
  ;; a title in a script without spaces matches inside running text
  (let ((note (make-vulpea-note :id "kinkaku" :title "金阁寺")))
    (should (equal (vulpea-mentions-test--link "鹿苑寺，又名金阁寺。\n" 1 note)
                   '(1 "鹿苑寺，又名[[id:kinkaku][金阁寺]]。\n")))))

(ert-deftest vulpea-mentions-link-survives-brackets-in-a-title ()
  "A title with square brackets is linked once, not wrapped again and again.
The link written for it holds the title, so a linker that searches the
line again after each replacement never comes to an end."
  (let* ((note (make-vulpea-note :id "foo" :title "Foo [bar]"))
         (link (org-link-make-string "id:foo" "Foo [bar]"))
         (linked (concat "About " link " today.\n")))
    (should (equal (vulpea-mentions-test--link "About Foo [bar] today.\n" 1 note)
                   (list 1 linked)))
    (should (equal (vulpea-mentions-test--link linked 1 note)
                   (list 0 linked)))))

(ert-deftest vulpea-mentions-link-skips-links ()
  "A title inside a link is left alone, whatever the link looks like."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (dolist (text '("See [[id:other][Emacs]].\n"
                    "See [[id:other][the Emacs manual]].\n"
                    "See [[id:other][Emacs [1] notes]].\n"
                    "See [[Emacs]].\n"
                    "See [[id:other][*the Emacs manual*]].\n"
                    "See https://example.com/Emacs today.\n"
                    "See <https://example.com/Emacs> today.\n"))
      (should (equal (vulpea-mentions-test--link text 1 note) (list 0 text))))
    ;; a link wrapped across lines covers its part of each line
    (let ((text "A [[id:other][long description of\nEmacs and more]] then Emacs.\n"))
      (should (equal (vulpea-mentions-test--link text 2 note)
                     (list 1 (concat "A [[id:other][long description of\n"
                                     "Emacs and more]] then [[id:emacs][Emacs]].\n")))))))

(ert-deftest vulpea-mentions-link-needs-an-org-buffer ()
  "Outside Org mode nothing tells prose from syntax, so nothing is linked."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (with-temp-buffer
      (insert "Plain Emacs text.\n")
      (should (= (vulpea-mentions-link (current-buffer) 1 note) 0))
      (should (equal (buffer-string) "Plain Emacs text.\n")))))

(ert-deftest vulpea-mentions-link-counts-lines-in-the-accessible-portion ()
  "Lines are counted from the start of the restriction, as the search does.
What the text is, prose or not, is still read from the whole buffer."
  (let ((note (make-vulpea-note :id "emacs" :title "Emacs")))
    (with-temp-buffer
      (insert "#+begin_src emacs-lisp\n(message \"Emacs\")\n#+end_src\n\n"
              "* Heading\nEmacs in prose.\n")
      (delay-mode-hooks (org-mode))
      ;; the body of the block, cut off from the lines that make it one
      (goto-char (point-min))
      (forward-line 1)
      (narrow-to-region (point) (line-end-position))
      (should (= (vulpea-mentions-link (current-buffer) 1 note) 0))
      (widen)
      (search-forward "* Heading")
      (narrow-to-region (line-beginning-position) (point-max))
      (should (= (vulpea-mentions-link (current-buffer) 2 note) 1))
      (should (buffer-narrowed-p))
      (widen)
      (should (equal (buffer-string)
                     (concat "#+begin_src emacs-lisp\n(message \"Emacs\")\n#+end_src\n\n"
                             "* Heading\n[[id:emacs][Emacs]] in prose.\n"))))))

;;; Prose in the searches

(ert-deftest vulpea-mentions--collect-drops-what-is-not-prose ()
  "A hit is read in its file, and dropped when the title is not prose there.
A hit that cannot be read there is kept as ripgrep reported it: the
file is gone, or no longer says what ripgrep saw."
  (vulpea-test--with-temp-db-and-files
      '((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
        (:name "tasting.org"
         :content ":PROPERTIES:\n:ID: tasting\n:END:\n#+title: Tasting\n\nA lovely Cabernet.\n~Cabernet~ as code.\nRewritten since.\n"))
    (let* ((note (vulpea-db-get-by-id "target"))
           (path (expand-file-name "tasting.org" dir))
           (event (lambda (file line-no text)
                    (format (concat "{\"type\":\"match\",\"data\":{\"path\":{\"text\":%S},"
                                    "\"lines\":{\"text\":%S},\"line_number\":%d}}\n")
                            file text line-no)))
           (output (concat
                    (funcall event path 6 "A lovely Cabernet.")
                    (funcall event path 7 "~Cabernet~ as code.")
                    ;; the file has changed under the search
                    (funcall event path 8 "More Cabernet here.")
                    ;; and this line is past its end
                    (funcall event path 20 "Cabernet at the end.")))
           (mentions (vulpea-mentions--collect
                      output note (vulpea-note-path note))))
      (should (equal (mapcar (lambda (m) (plist-get m :line)) mentions)
                     '(6 8 20))))))

(ert-deftest vulpea-mentions--keep-prose-reads-files-in-any-order ()
  "Hits are read in the right file also when they do not come file by file."
  (vulpea-test--with-temp-db-and-files
      '((:name "a.org" :content "~Cabernet~ here.\nCabernet there.\n")
        (:name "b.org" :content "Cabernet here.\n~Cabernet~ there.\n"))
    (let* ((a (expand-file-name "a.org" dir))
           (b (expand-file-name "b.org" dir))
           (hit (lambda (path line-no text)
                  (cons (list :path path :line line-no :line-text text)
                        (list path line-no)))))
      (should (equal (vulpea-mentions--keep-prose
                      (list (funcall hit a 2 "Cabernet there.")
                            (funcall hit b 2 "~Cabernet~ there.")
                            (funcall hit a 1 "~Cabernet~ here.")
                            (funcall hit b 1 "Cabernet here."))
                      '("Cabernet"))
                     (list (list a 2) (list b 1)))))))

(ert-deftest vulpea-mentions--keep-prose-survives-the-parser ()
  "A hit the parser gives up on is kept, and the rest is still read."
  (vulpea-test--with-temp-db-and-files
      '((:name "a.org" :content "Cabernet here.\n~Cabernet~ there.\n"))
    (let* ((a (expand-file-name "a.org" dir))
           (orig (symbol-function 'vulpea-mentions--prose-p))
           (calls 0))
      (cl-letf (((symbol-function 'vulpea-mentions--prose-p)
                 (lambda (pos)
                   (if (= (cl-incf calls) 1)
                       (error "Parser gave up")
                     (funcall orig pos)))))
        (should (equal (vulpea-mentions--keep-prose
                        (list (cons (list :path a :line 1
                                          :line-text "Cabernet here.")
                                    1)
                              (cons (list :path a :line 2
                                          :line-text "~Cabernet~ there.")
                                    2))
                        '("Cabernet"))
                       '(1)))
        (should (= calls 2))))))

(ert-deftest vulpea-mentions-incoming-reads-files-with-dos-line-endings ()
  "A file with CRLF line endings reads the same to ripgrep and to Emacs."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      '((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
        (:name "dos.org"
         :content ":PROPERTIES:\r\n:ID: dos\r\n:END:\r\n#+title: Dos\r\n\r\nA lovely Cabernet.\r\n~Cabernet~ as code.\r\n"))
    (should (equal (mapcar (lambda (m)
                             (list (plist-get m :line) (plist-get m :context)))
                           (vulpea-mentions-test--collect-incoming-mentions-for-note
                            "target"))
                   '((6 "A lovely Cabernet."))))))

(ert-deftest vulpea-mentions-incoming-reads-each-file-with-its-own-keywords ()
  "Todo keywords declared by one file do not leak into the next one."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      '((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Review\n")
        (:name "a.org"
         :content ":PROPERTIES:\n:ID: a\n:END:\n#+title: Tasks\n#+todo: REVIEW | DONE\n\n* REVIEW the draft\n")
        (:name "b.org"
         :content ":PROPERTIES:\n:ID: b\n:END:\n#+title: Notes\n\n* REVIEW the draft\n"))
    (let ((mentions (vulpea-mentions-test--collect-incoming-mentions-for-note
                     "target")))
      (should (equal (mapcar (lambda (m) (vulpea-note-id (plist-get m :note)))
                             mentions)
                     '("b"))))))

(ert-deftest vulpea-mentions-outgoing-quotes-the-mention-in-prose ()
  "The text a mention quotes is the first occurrence that is prose."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "emacs" "Emacs" :path "/n/emacs.org")
    (let ((mentions (vulpea-mentions-test--outgoing
                     "Run ~emacs~ to start EMACS, or Emacs.\n")))
      (should (equal (mapcar (lambda (m) (plist-get m :matched)) mentions)
                     '("EMACS")))
      ;; the copy of the buffer the parser read is gone
      (should-not (seq-find (lambda (buffer)
                              (string-prefix-p " *vulpea-mentions-prose*"
                                               (buffer-name buffer)))
                            (buffer-list))))))

(ert-deftest vulpea-mentions-outgoing-reads-prose-from-the-whole-buffer ()
  "A narrowed buffer is searched in part, but read as the part of a whole.
The restriction may start inside a block or leave the todo keywords of
the file outside, and both decide what the text in it is."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "emacs" "Emacs" :path "/n/emacs.org")
    (vulpea-test--insert-test-note "next" "Next" :path "/n/next.org")
    (with-temp-buffer
      (insert "#+todo: NEXT | DONE\n\n"
              "#+begin_src emacs-lisp\n(message \"Emacs\")\n#+end_src\n\n"
              "* NEXT Install Emacs\nNext, read the manual.\n")
      (cl-flet ((search ()
                  (let ((state nil) (result nil))
                    (vulpea-buffer-unlinked-mentions-async
                     (lambda (ms) (setq state 'resolved result ms))
                     (lambda (err) (setq state (list 'rejected err))))
                    (vulpea-mentions-test--await (lambda () state))
                    (should (eq state 'resolved))
                    (mapcar (lambda (m)
                              (list (vulpea-note-id (plist-get m :note))
                                    (plist-get m :line)
                                    (plist-get m :matched)))
                            result))))
        ;; the body of the block alone
        (goto-char (point-min))
        (forward-line 3)
        (narrow-to-region (point) (line-end-position))
        (should-not (search))
        ;; the subtree, without the line that declares NEXT
        (widen)
        (search-forward "* NEXT")
        (narrow-to-region (line-beginning-position) (point-max))
        (should (equal (search)
                       '(("emacs" 1 "Emacs") ("next" 2 "Next"))))))))

;;; Links no note owns

(ert-deftest vulpea-mentions--rg-command-asks-for-links-to-the-note ()
  "Given an id, the ripgrep command looks for bracket links to it as well."
  (let ((cmd (vulpea-mentions--rg-command "rg" '("Wine") '("/dir") "a.b")))
    ;; the id is quoted, matched as written, and closed by a bracket so
    ;; that a longer id starting with it does not count
    (should (member "(?-i:\\[\\[id:a\\.b\\])" cmd)))
  (should-not (seq-find (lambda (arg) (string-search "id:" arg))
                        (vulpea-mentions--rg-command "rg" '("Wine") '("/dir")))))

(ert-deftest vulpea-mentions-incoming-counts-links-no-note-owns ()
  "A file counts as linking wherever in it the link is.
In a file whose ids live on headings, a link above the first of them
belongs to no note, so the database does not know it.  The file still
says it, and its other mentions are no longer missing connections."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
        (:name "headings.org"
         :content ,(concat "#+title: Tastings\n\n"
                           "We opened a [[id:target][fine bottle]] first.\n\n"
                           "* First\n:PROPERTIES:\n:ID: first\n:END:\n"
                           "The Cabernet was fine.\n"))
        (:name "wrapped.org"
         :content ,(concat "#+title: More tastings\n\n"
                           "We opened a [[id:target][very fine\n"
                           "Cabernet]] first.\n\n"
                           "* Second\n:PROPERTIES:\n:ID: second\n:END:\n"
                           "The Cabernet was fine again.\n"))
        ;; a longer id that starts with the id of the note is another note
        (:name "other.org"
         :content ,(concat "#+title: Other tastings\n\n"
                           "We opened a [[id:target-2][fine bottle]] first.\n\n"
                           "* Third\n:PROPERTIES:\n:ID: third\n:END:\n"
                           "The Cabernet was something else.\n")))
    ;; the database has nothing on these links
    (should-not (vulpea-db-query-links-to "target"))
    (should (equal (mapcar (lambda (m) (vulpea-note-id (plist-get m :note)))
                           (vulpea-mentions-test--collect-incoming-mentions-for-note
                            "target"))
                   '("third")))
    (let ((vulpea-mentions-exclude-linked nil))
      (should (equal (sort (mapcar (lambda (m)
                                     (vulpea-note-id (plist-get m :note)))
                                   (vulpea-mentions-test--collect-incoming-mentions-for-note
                                    "target"))
                           #'string<)
                     '("first" "second" "third"))))))

(ert-deftest vulpea-mentions-link-then-search-drops-the-file ()
  "Linking one mention takes the other mentions of that file off the list.
This is the round trip a UI makes: search, link, save, search again."
  (vulpea-test--require-rg)
  (vulpea-test--with-temp-db-and-files
      `((:name "target.org"
         :content ":PROPERTIES:\n:ID: target\n:END:\n#+title: Cabernet\n")
        (:name "headings.org"
         :content ,(concat "#+title: Tastings\n\n"
                           "We opened a Cabernet first.\n\n"
                           "* First\n:PROPERTIES:\n:ID: first\n:END:\n"
                           "The Cabernet was fine.\n")))
    (let* ((note (vulpea-db-get-by-id "target"))
           (path (expand-file-name "headings.org" dir))
           (mentions (vulpea-mentions-test--collect-incoming-mentions-for-note
                      "target"))
           (mention (car mentions)))
      (should (equal (mapcar (lambda (m) (plist-get m :line)) mentions) '(3 9)))
      (let ((buffer (find-file-noselect path)))
        (unwind-protect
            (with-current-buffer buffer
              (should (= (vulpea-mentions-link buffer
                                               (plist-get mention :line)
                                               note
                                               (plist-get mention :context))
                         1))
              (save-buffer))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (vulpea-db-update-file path)
      (should-not (vulpea-mentions-test--collect-incoming-mentions-for-note
                   "target")))))

(provide 'vulpea-mentions-test)
;;; vulpea-mentions-test.el ends here
