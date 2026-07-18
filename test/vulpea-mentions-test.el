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
  "Return mentions for note specified by ID. "
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
          (with-temp-file patterns (insert (mapconcat #'identity terms "\n") "\n"))
          (let* (linked-ids
                 (note-path (expand-file-name (vulpea-note-path note)))
                 (output (with-temp-buffer
                           (insert-file note-path)
                           (setq linked-ids
                                 (vulpea-mentions--buffer-link-ids))
                           (let ((out (generate-new-buffer " *rg*")))
                             (call-process-region
                              (point-min) (point-max) (executable-find "rg")
                              nil out nil "--json" "--fixed-strings" "--ignore-case"
                              "--word-regexp" "-f" patterns "-")
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
      (should (equal (plist-get (car hits) :line-text) "see Cabernet here"))
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
  (skip-unless (executable-find "rg"))
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

(ert-deftest vulpea-mentions-ignore-note-with-property ()
  "When a note set the ignore property, skip searching for its mentions."
  (skip-unless (executable-find "rg"))
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
     (eldev-debug "%s" (plist-get (car mentions) :context))
     (should (eq (length mentions) 0)))))

(ert-deftest vulpea-mentions-async-rejects-without-rg ()
  "When ripgrep is unavailable, REJECT is called."
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (let ((rejected nil))
      (vulpea-note-unlinked-mentions-async
       (make-vulpea-note :title "X" :path "/n/x.org")
       (lambda (_ms) (setq rejected 'resolved))
       (lambda (_e) (setq rejected t)))
      (should (eq rejected t)))))

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
  (skip-unless (executable-find "rg"))
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
            (with-temp-file patterns (insert (mapconcat #'identity terms "\n") "\n"))
            (let* (linked-ids-exclude-linked
                   (linked-ids-no-exclude-linked (make-hash-table :test 'equal))
                   (output (with-temp-buffer
                             (insert content)
                             (setq linked-ids-exclude-linked
                                   (vulpea-mentions--buffer-link-ids))
                             (let ((out (generate-new-buffer " *rg*")))
                               (call-process-region
                                (point-min) (point-max) (executable-find "rg")
                                nil out nil "--json" "--fixed-strings" "--ignore-case"
                                "--word-regexp" "-f" patterns "-")
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

(ert-deftest vulpea-mentions-outgoing-ignore-per-note ()
  "When this note is ignored by another note, drop its mentions to that note."
  (vulpea-test--with-temp-db-and-files
   `((:name "target.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: target\n"
                     (format ":%s: ignored-mention\n" vulpea-mentions-per-note-ignore-property-key)
                     ":END:\n#+title: Target\n\n"))
     (:name "mention.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: mention\n:END:\n#+title: Mention\n\n"
                     "A mention to target that should not be ignored.\n"))
     (:name "ignored-mention.org"
            :content
            ,(concat ":PROPERTIES:\n:ID: ignored-mention\n:END:\n#+title: Ignored Mention\n\n"
                     "A mention to target that should be ignored\n")))

   ))

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
