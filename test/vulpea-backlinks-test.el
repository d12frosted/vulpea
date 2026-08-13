;;; vulpea-backlinks-test.el --- Tests for vulpea-backlinks -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Pavel Popov <pavel@vio.com>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; This file is not part of GNU Emacs.
;;
;; Created: 13 Aug 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; Tests for the backlinks buffer in `vulpea-backlinks'.
;;
;;; Code:

(require 'ert)
(require 'magit-section)
(require 'vulpea-backlinks)
(require 'vulpea-db)
(require 'vulpea-note)
(require 'vulpea-test-helpers)

;;; Helpers

(defmacro vulpea-backlinks-test--with-buffer (&rest body)
  "Execute BODY with the backlinks buffer scoped to this test.

Binds `vulpea-backlinks-buffer-name' to a unique name and kills
that buffer afterwards, so a test never inherits or leaves behind
a rendered buffer."
  (declare (indent 0))
  `(let ((vulpea-backlinks-buffer-name
          (generate-new-buffer-name "*vulpea-backlinks-test*")))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buf (get-buffer vulpea-backlinks-buffer-name)))
         (kill-buffer buf)))))

(defun vulpea-backlinks-test--sections (type)
  "Return sections of TYPE in the current buffer, in insertion order."
  (let (result)
    (letrec ((walk (lambda (section)
                     (when (object-of-class-p section type)
                       (push section result))
                     (dolist (child (oref section children))
                       (funcall walk child)))))
      (funcall walk magit-root-section))
    (nreverse result)))

(defun vulpea-backlinks-test--write (path content)
  "Write CONTENT to PATH and index it."
  (with-temp-file path
    (insert content))
  (vulpea-db-update-file path))

;;; vulpea-backlinks-ids

(ert-deftest vulpea-backlinks-ids-collects-id-links ()
  "Notes linking with an `id' link are reported as backlinks."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target")
    (vulpea-test--insert-test-note "source-1" "Source 1"
                                   :links '((:dest "target" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "source-2" "Source 2"
                                   :links '((:dest "target" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "unrelated" "Unrelated"
                                   :links '((:dest "other" :type "id" :pos 100)))

    (should (equal (sort (vulpea-backlinks-ids "target") #'string-lessp)
                   '("source-1" "source-2")))))

(ert-deftest vulpea-backlinks-ids-ignores-other-link-types ()
  "Only `id' links count; file, https and attachment links are skipped."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target")
    (vulpea-test--insert-test-note "linker" "Linker"
                                   :links '((:dest "target" :type "id" :pos 100)))
    (vulpea-test--insert-test-note "filer" "Filer"
                                   :links '((:dest "target" :type "file" :pos 100)))
    (vulpea-test--insert-test-note "webber" "Webber"
                                   :links '((:dest "target" :type "https" :pos 100)))

    (should (equal (vulpea-backlinks-ids "target") '("linker")))))

(ert-deftest vulpea-backlinks-ids-collapses-duplicates ()
  "A note linking several times is reported once."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target")
    (vulpea-test--insert-test-note "source" "Source"
                                   :links '((:dest "target" :type "id" :pos 100)
                                            (:dest "target" :type "id" :pos 200)
                                            (:dest "target" :type "id" :pos 300)))

    (should (equal (vulpea-backlinks-ids "target") '("source")))))

(ert-deftest vulpea-backlinks-ids-accepts-note-and-id ()
  "The argument is either a `vulpea-note' or an id string."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target")
    (vulpea-test--insert-test-note "source" "Source"
                                   :links '((:dest "target" :type "id" :pos 100)))

    (let ((note (vulpea-db-get-by-id "target")))
      (should (equal (vulpea-backlinks-ids note)
                     (vulpea-backlinks-ids "target"))))))

(ert-deftest vulpea-backlinks-ids-unknown-note-is-nil ()
  "A note nothing links to - or one that does not exist - has no backlinks."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "lonely" "Lonely")

    (should-not (vulpea-backlinks-ids "lonely"))
    (should-not (vulpea-backlinks-ids "no-such-note"))))

(ert-deftest vulpea-backlinks-ids-without-argument-reads-point ()
  "Without an argument the note at point is used, nil outside one."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (vulpea-test--insert-test-note "target" "Target")
    (vulpea-test--insert-test-note "source" "Source"
                                   :links '((:dest "target" :type "id" :pos 100)))

    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID: target\n:END:\n#+TITLE: Target\n\nBody.\n")
      (delay-mode-hooks (org-mode))
      (goto-char (point-max))
      (should (equal (vulpea-backlinks-ids) '("source"))))

    (with-temp-buffer
      (insert "#+TITLE: Nameless\n\nBody.\n")
      (delay-mode-hooks (org-mode))
      (goto-char (point-max))
      (should-not (vulpea-backlinks-ids)))))

;;; vulpea-backlinks--mention-lines-regexp

(ert-deftest vulpea-backlinks--mention-lines-regexp-matches-whole-lines ()
  "A mention line matches whole, with surrounding whitespace allowed."
  (let ((re (vulpea-backlinks--mention-lines-regexp
             '("Cabernet Sauvignon is a grape."))))
    (should (string-match-p re "Cabernet Sauvignon is a grape."))
    (should (string-match-p re "   Cabernet Sauvignon is a grape.  "))
    (should (string-match-p re "\tCabernet Sauvignon is a grape.\t"))))

(ert-deftest vulpea-backlinks--mention-lines-regexp-ignores-partial-lines ()
  "A line that merely contains the mention text does not match."
  (let ((re (vulpea-backlinks--mention-lines-regexp '("a grape"))))
    (should-not (string-match-p re "about a grape variety"))
    (should-not (string-match-p re "a grape variety"))
    (should (string-match-p re "a grape"))))

(ert-deftest vulpea-backlinks--mention-lines-regexp-quotes-specials ()
  "Regexp specials in a mention line are matched literally."
  (let ((re (vulpea-backlinks--mention-lines-regexp '("Cost (approx.) $5.00 [a-z]"))))
    (should (string-match-p re "Cost (approx.) $5.00 [a-z]"))
    ;; The bracket expression is data, not a character class.
    (should-not (string-match-p re "Cost (approx.) $5.00 q"))
    ;; The dot is data too.
    (should-not (string-match-p re "Cost (approx.) $5X00 [a-z]"))))

(ert-deftest vulpea-backlinks--mention-lines-regexp-matches-every-line ()
  "Every context is matched, and repeats do not multiply the alternatives."
  (let* ((contexts '("first mention" "second mention" "first mention"))
         (re (vulpea-backlinks--mention-lines-regexp contexts)))
    (should (string-match-p re "first mention"))
    (should (string-match-p re "second mention"))
    ;; "first mention" appears twice in CONTEXTS but once in the regexp.
    (should (= 1 (cl-count "first mention"
                           (split-string re "\\\\|")
                           :test #'string-match-p)))))

;;; vulpea-backlinks--olp-string

(ert-deftest vulpea-backlinks--olp-string-joins-path ()
  "An outline path reads as \"a > b\"."
  (should (equal (vulpea-backlinks--olp-string '("Parent" "Child"))
                 "Parent > Child"))
  (should (equal (vulpea-backlinks--olp-string '("Only")) "Only")))

(ert-deftest vulpea-backlinks--olp-string-top-when-nil ()
  "A link at file level has no outline path and reads as \"Top\"."
  (should (equal (vulpea-backlinks--olp-string nil) "Top")))

(ert-deftest vulpea-backlinks--olp-string-displays-links ()
  "A heading that is a link shows its description, not the raw link."
  (should (equal (vulpea-backlinks--olp-string '("[[id:abc][Wine]]" "Notes"))
                 "Wine > Notes")))

;;; vulpea-backlinks--context-at

(ert-deftest vulpea-backlinks--context-at-under-heading ()
  "The context of a link under a heading is that heading's path and body."
  (vulpea-test--with-temp-notes-dir
    (let ((path (expand-file-name "source.org" root)))
      (with-temp-file path
        (insert ":PROPERTIES:\n:ID: source\n:END:\n"
                "#+TITLE: Source\n"
                "\n"
                "* Parent\n"
                "** Child\n"
                "SCHEDULED: <2026-08-13 Thu>\n"
                ":PROPERTIES:\n:CUSTOM_ID: child\n:END:\n"
                "Mentions [[id:target][Target]] here.\n"))
      (let* ((pt (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (search-forward "[[id:target]")
                   (match-beginning 0)))
             (ctx (vulpea-backlinks--context-at path pt)))
        (should (equal (car ctx) '("Parent" "Child")))
        ;; Planning line and property drawer stay out of the preview.
        (should (equal (cdr ctx) "Mentions [[id:target][Target]] here."))))))

(ert-deftest vulpea-backlinks--context-at-file-level ()
  "A link before the first heading has no outline path."
  (vulpea-test--with-temp-notes-dir
    (let ((path (expand-file-name "source.org" root)))
      (with-temp-file path
        (insert ":PROPERTIES:\n:ID: source\n:END:\n"
                "#+TITLE: Source\n"
                "\n"
                "Mentions [[id:target][Target]] here.\n"
                "\n"
                "* Later heading\n"
                "Not part of the preview.\n"))
      (let* ((pt (with-temp-buffer
                   (insert-file-contents path)
                   (goto-char (point-min))
                   (search-forward "[[id:target]")
                   (match-beginning 0)))
             (ctx (vulpea-backlinks--context-at path pt)))
        (should-not (car ctx))
        ;; The preview stops at the next heading.
        (should (equal (cdr ctx)
                       (concat "#+TITLE: Source\n"
                               "\n"
                               "Mentions [[id:target][Target]] here.")))))))

;;; Rendering

(ert-deftest vulpea-backlinks-render-lists-backlinks ()
  "Rendering a note lists the notes linking to it, sorted by title.

Sections carry the source file and the link position, which is
what makes them visitable."
  (vulpea-test--with-temp-notes-dir
    (vulpea-backlinks-test--with-buffer
      (let ((vulpea-backlinks-show-unlinked nil)
            (target (expand-file-name "target.org" root))
            (beta (expand-file-name "beta.org" root))
            (alpha (expand-file-name "alpha.org" root))
            (mute (expand-file-name "mute.org" root)))
        (vulpea-backlinks-test--write
         target ":PROPERTIES:\n:ID: target\n:END:\n#+TITLE: Target\n")
        (vulpea-backlinks-test--write
         beta (concat ":PROPERTIES:\n:ID: beta\n:END:\n#+TITLE: Beta\n"
                      "\n* Section\nSees [[id:target][Target]].\n"))
        (vulpea-backlinks-test--write
         alpha (concat ":PROPERTIES:\n:ID: alpha\n:END:\n#+TITLE: Alpha\n"
                       "\nAlso sees [[id:target][Target]].\n"))
        (vulpea-backlinks-test--write
         mute ":PROPERTIES:\n:ID: mute\n:END:\n#+TITLE: Mute\n\nNothing here.\n")

        (with-current-buffer (vulpea-backlinks--render "target")
          (should (derived-mode-p 'vulpea-backlinks-mode))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "^Target$" text))
            (should (string-match-p "Backlinks (2)" text))
            ;; Sorted by title: Alpha before Beta.
            (should (< (string-match "Alpha" text) (string-match "Beta" text)))
            ;; The outline path of each link is shown.
            (should (string-match-p "Alpha (Top)" text))
            (should (string-match-p "Beta (Section)" text))
            ;; The preview carries the linking text.
            (should (string-match-p "Also sees" text))
            (should-not (string-match-p "Mute" text)))

          (let ((nodes (vulpea-backlinks-test--sections
                        'vulpea-backlinks-node-section)))
            (should (= 2 (length nodes)))
            (should (equal (mapcar (lambda (s) (file-name-nondirectory
                                                (oref s file)))
                                   nodes)
                           '("alpha.org" "beta.org")))
            (dolist (node nodes)
              (should (integerp (oref node point)))
              ;; The position points at the link in the source file.
              (with-temp-buffer
                (insert-file-contents (oref node file))
                (goto-char (oref node point))
                (should (looking-at-p "\\[\\[id:target\\]"))))))))))

(ert-deftest vulpea-backlinks-render-without-backlinks ()
  "A note nothing links to renders an empty backlinks section."
  (vulpea-test--with-temp-notes-dir
    (vulpea-backlinks-test--with-buffer
      (let ((vulpea-backlinks-show-unlinked nil)
            (target (expand-file-name "target.org" root)))
        (vulpea-backlinks-test--write
         target ":PROPERTIES:\n:ID: target\n:END:\n#+TITLE: Target\n")

        (with-current-buffer (vulpea-backlinks--render "target")
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Backlinks (0)" text)))
          (should-not (vulpea-backlinks-test--sections
                       'vulpea-backlinks-node-section)))))))

(ert-deftest vulpea-backlinks-render-without-note ()
  "Rendering nothing says so instead of failing."
  (vulpea-test--with-temp-notes-dir
    (vulpea-backlinks-test--with-buffer
      (let ((vulpea-backlinks-show-unlinked nil))
        (with-current-buffer (vulpea-backlinks--render nil)
          (should (derived-mode-p 'vulpea-backlinks-mode))
          (should (string-match-p "not on a Vulpea note"
                                  (substring-no-properties (buffer-string)))))))))

(provide 'vulpea-backlinks-test)
;;; vulpea-backlinks-test.el ends here
