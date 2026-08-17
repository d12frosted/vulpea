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
               vulpea-db-sync-directories))
         (output (with-temp-buffer
                   (apply #'call-process (car cmd) nil t nil (cdr cmd))
                   (buffer-string)))
         (mentions (vulpea-mentions--collect
                    output note (expand-file-name path))))
    mentions))

(defun vulpea-mentions-test--collect-outgoing-mentions-for-note (note)
  "Returns outgoing mentions for NOTE.

NOTE can be either an ID or a `vulpea-note' object."
  (let* ((terms-dict (vulpea-mentions--title-dictionary))
         (terms (cdr terms-dict))
         (dict (car terms-dict))
         (patterns (make-temp-file "vmp-")))
    (unwind-protect
        (progn
          (unless (vulpea-note-p note)
            (setq note (vulpea-db-get-by-id note)))
          (with-temp-file patterns
            (insert (mapconcat #'vulpea-mentions--rg-pattern terms "\n") "\n"))
          (let* (linked-ids
                 (cmd (vulpea-mentions--rg-stdin-command
                       (executable-find "rg") patterns))
                 (note-path (expand-file-name (vulpea-note-path note)))
                 (output (with-temp-buffer
                           (insert-file-contents note-path)
                           (setq linked-ids
                                 (vulpea-mentions--buffer-link-ids))
                           (let ((out (generate-new-buffer " *rg*")))
                             (apply #'call-process-region
                                    (point-min) (point-max) (car cmd)
                                    nil out nil (cdr cmd))
                             (prog1 (with-current-buffer out (buffer-string))
                               (kill-buffer out)))))
                 (self-ids (mapcar #'vulpea-note-id
                                   (vulpea-db-query-by-file-path note-path))))
            (vulpea-mentions--collect-outgoing
             output
             dict
             self-ids
             linked-ids)))
      (delete-file patterns))))

(defun vulpea-mentions-test--await (state-fn &optional timeout)
  "Pump process output until STATE-FN returns non-nil.

Give up after TIMEOUT seconds (default 10); the caller's assertions
then see whatever state the search reached."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall state-fn))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

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
    (should (member "(?-u:\\b)Wine(?-u:\\b)" cmd))
    (should (member "金阁寺" cmd))))

(ert-deftest vulpea-mentions--rg-stdin-command-shape ()
  "The stdin ripgrep command reads patterns from a file and input from -."
  (let ((cmd (vulpea-mentions--rg-stdin-command "rg" "/tmp/patterns")))
    (should (equal (car cmd) "rg"))
    (should (member "--json" cmd))
    (should (member "--ignore-case" cmd))
    (should-not (member "--fixed-strings" cmd))
    (should-not (member "--word-regexp" cmd))
    (should (equal (last cmd 3) '("-f" "/tmp/patterns" "-")))))

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
  ;; ASCII alphanumeric edges get the fast ASCII assertion
  (should (equal (vulpea-mentions--rg-pattern "Wine")
                 "(?-u:\\b)Wine(?-u:\\b)"))
  ;; non-ASCII spaced edges (Cyrillic) rely on the Emacs re-check:
  ;; a Unicode \\b here would force the regex engine off its fast path
  (should (equal (vulpea-mentions--rg-pattern "Київ") "Київ"))
  ;; punctuation edges get no assertion (a \\b next to punctuation
  ;; inverts into demanding adjacent text); escaping still applies
  (should (equal (vulpea-mentions--rg-pattern "C++") "(?-u:\\b)C\\+\\+"))
  ;; spaceless scripts match as substrings
  (should (equal (vulpea-mentions--rg-pattern "金阁寺") "金阁寺"))
  ;; mixed terms keep the assertion only on the ASCII edge
  (should (equal (vulpea-mentions--rg-pattern "Emacs入门")
                 "(?-u:\\b)Emacs入门"))
  (should (equal (vulpea-mentions--rg-pattern "入门Emacs")
                 "入门Emacs(?-u:\\b)")))

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
                    "Git rebasing sometimes can be confusing.\n"))))

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
         (fileless-note (vulpea-db-get-by-id "fileless")))

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
       (should (vulpea-mentions-test--id-ignored-p "fileless"))))))

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
  "The dictionary maps downcased names to ids (collisions kept) and lists terms."
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
      (should (member "Cabernet Sauvignon" terms))
      (should (member "Cab Sauv" terms))
      (should (member "Merlot" terms)))))

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
     (should (equal terms '("Merlot")))))
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

(ert-deftest vulpea-mentions--collect-outgoing ()
  "Outgoing collect maps matched terms to candidate notes and applies filters."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "cab" "Cabernet" :path "/n/cab.org")
    (vulpea-test--insert-test-note "merlot" "Merlot" :path "/n/merlot.org")
    (vulpea-test--insert-test-note "self" "Diary" :path "/n/diary.org")
    (let* ((dict (car (vulpea-mentions--title-dictionary)))
           (self-ids '("self"))
           (mk (lambda (line n term)
                 (format (concat "{\"type\":\"match\",\"data\":{\"path\":{\"text\":\"<stdin>\"},"
                                 "\"lines\":{\"text\":%S},\"line_number\":%d,"
                                 "\"submatches\":[{\"match\":{\"text\":%S},\"start\":0,\"end\":1}]}}\n")
                         line n term)))
           (output (concat
                    ;; bare mention -> candidate cab
                    (funcall mk "had some Cabernet" 2 "Cabernet")
                    ;; already linked -> excluded
                    (funcall mk "see [[id:cab][Cabernet]]" 3 "Cabernet")
                    ;; metadata line -> excluded
                    (funcall mk "#+title: Merlot" 1 "Merlot")
                    ;; mention of the buffer's own note -> excluded via self-ids
                    (funcall mk "my Diary entry" 4 "Diary")))
           (build-linked-ids (lambda ()
                               (let ((result (make-hash-table :test 'equal)))
                                 (puthash "cab" t result)
                                 result)))
           (linked-ids (funcall build-linked-ids)))
      (let ((mentions (vulpea-mentions--collect-outgoing
                       output dict self-ids linked-ids)))
        (should (= (length mentions) 0)))
      (let ((mentions (vulpea-mentions--collect-outgoing
                       output dict self-ids (make-hash-table :test 'equal))))
        (should (= (length mentions) 1))
        (should (equal (vulpea-note-id (plist-get (car mentions) :note)) "cab"))
        (should (equal (plist-get (car mentions) :matched) "Cabernet"))
        (should (equal (plist-get (car mentions) :line) 2))))))

(ert-deftest vulpea-mentions-outgoing-with-real-rg ()
  "Real ripgrep over buffer content (stdin) yields candidate notes; links excluded."
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
    (let* ((terms (cdr (vulpea-mentions--title-dictionary)))
           (dict (car (vulpea-mentions--title-dictionary)))
           (patterns (make-temp-file "vmp-"))
           (content (concat "We had Cabernet Sauvignon and [[id:merlot][Merlot]].\n"
                            "More Merlot and Syrah later.\n")))
      (unwind-protect
          (progn
            (with-temp-file patterns
              (insert (mapconcat #'vulpea-mentions--rg-pattern terms "\n") "\n"))
            (let* (linked-ids-exclude-linked
                   (linked-ids-no-exclude-linked (make-hash-table :test 'equal))
                   (cmd (vulpea-mentions--rg-stdin-command
                         (executable-find "rg") patterns))
                   (output (with-temp-buffer
                             (insert content)
                             (setq linked-ids-exclude-linked
                                   (vulpea-mentions--buffer-link-ids))
                             (let ((out (generate-new-buffer " *rg*")))
                               (apply #'call-process-region
                                      (point-min) (point-max) (car cmd)
                                      nil out nil (cdr cmd))
                               (prog1 (with-current-buffer out (buffer-string))
                                 (kill-buffer out))))))
              ;; when `vulpea-mentions-exclude-linked' is non-nil
              (let ((mentions
                     (vulpea-mentions--collect-outgoing
                      output
                      dict
                      nil
                      linked-ids-exclude-linked)))
                (should (= (length mentions) 1))
                (should (equal (plist-get (car mentions) :matched) "Cabernet Sauvignon")))
              ;; when `vulpea-mentions-exclude-linked' is nil
              (let* ((mentions
                      (vulpea-mentions--collect-outgoing
                       output
                       dict
                       nil
                       linked-ids-no-exclude-linked))
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
                  (should (equal (plist-get merlot :context) "More Merlot and Syrah later."))))))
        (delete-file patterns)))))

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

The patterns file and the ripgrep stdin/stdout must stay UTF-8 even
when `coding-system-for-write' and `default-process-coding-system'
say otherwise."
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

(ert-deftest vulpea-mentions-outgoing-rejects-without-rg ()
  "When ripgrep is unavailable, the outgoing search REJECTs."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (with-temp-buffer
      (let ((state nil))
        (vulpea-buffer-unlinked-mentions-async
         (lambda (_ms) (setq state 'resolved))
         (lambda (_e) (setq state 'rejected)))
        (should (eq state 'rejected))))))

(provide 'vulpea-mentions-test)
;;; vulpea-mentions-test.el ends here
