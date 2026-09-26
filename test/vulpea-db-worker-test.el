;;; vulpea-db-worker-test.el --- Tests for the extraction worker -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 05 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for vulpea-db-worker.el (v2), including the async-vs-sync
;; database equivalence contract.
;;
;;; Code:

(require 'ert)
(require 'vulpea-db-worker)
(require 'vulpea-test-helpers)

;; The adversarial corpus lives in the extract tests
(require 'vulpea-db-extract-test
         (expand-file-name "vulpea-db-extract-test.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name))))

(defun vulpea-db-worker-test--wait (&optional seconds)
  "Wait up to SECONDS (default 60) until the worker is stably idle.
Idle must survive a timer drain: crash salvage re-enqueues in-flight
work through a deferred timer, so an empty in-flight list can become
busy again a moment later."
  (let ((deadline (+ (float-time) (or seconds 60)))
        (stable nil))
    (while (and (not stable) (< (float-time) deadline))
      (if (vulpea-db-worker-busy-p)
          (accept-process-output vulpea-db-worker--process 0.05)
        ;; Drain deferred timers (salvage re-enqueue), then re-check
        (sit-for 0.1)
        (setq stable (not (vulpea-db-worker-busy-p)))))
    (should-not (vulpea-db-worker-busy-p))))

(defun vulpea-db-worker-test--db-dump ()
  "Return all indexed data from the current database, ordered."
  (let ((db (vulpea-db)))
    (list
     :notes (emacsql db [:select [id level pos title properties tags
                                  aliases meta links todo priority
                                  scheduled deadline closed category
                                  outline-path attach-dir file-title
                                  created-at]
                         :from notes :order-by [(asc id)]])
     :tags (emacsql db [:select [note-id tag] :from tags
                        :order-by [(asc note-id) (asc tag)]])
     :links (emacsql db [:select [source dest type pos description]
                         :from links
                         :order-by [(asc source) (asc pos)]])
     :meta (emacsql db [:select [note-id key value] :from meta
                        :order-by [(asc note-id) (asc key) (asc value)]])
     :properties (emacsql db [:select [note-id key value] :from properties
                              :order-by [(asc note-id) (asc key)]])
     :files (emacsql db [:select [hash size] :from files]))))

(defmacro vulpea-db-worker-test--with-file (content &rest body)
  "Run BODY with PATH bound to a temp org file holding CONTENT.
Ensures the worker and the file are cleaned up.  The crash
bookkeeping starts clean: deaths caused by earlier tests must not
mark this test's worker broken."
  (declare (indent 1))
  `(let ((path (vulpea-test--create-temp-org-file ,content))
         (vulpea-db-worker--broken nil)
         (vulpea-db-worker--crash-times nil))
     (unwind-protect
         (progn ,@body)
       (vulpea-db-worker-stop)
       (when (file-exists-p path)
         (delete-file path)))))

(ert-deftest vulpea-db-worker-async-extraction-default ()
  "Async extraction ships enabled in extract-only mode; `full' is opt-in."
  (should (eq t (eval (car (get 'vulpea-db-async-extraction 'standard-value))
                      t))))

;;; Settings the worker needs from libraries the session may not load

(ert-deftest vulpea-db-worker-sends-attach-settings-without-org-attach ()
  "The settings message carries org-attach options in a fresh session.
With async extraction the session may never load org-attach itself,
yet the worker needs the user's attach settings; a value set in
`with-eval-after-load' must reach it too."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (output
          (with-temp-buffer
            (apply #'call-process emacs nil t nil
                   (append
                    (list "--batch" "-Q")
                    (mapcan (lambda (dir) (list "-L" dir))
                            (seq-filter #'stringp load-path))
                    (list "--eval"
                          (prin1-to-string
                           '(progn
                              (with-eval-after-load 'org-attach
                                (setq org-attach-id-dir "custom-attach/"))
                              (require 'vulpea-db-worker)
                              (princ (format "ATTACH=%S"
                                             (alist-get
                                              'org-attach-id-dir
                                              (nth 1 (vulpea-db-worker--settings-form))))))))))
            (buffer-string))))
    (should (string-match-p "ATTACH=\"custom-attach/\"" output))))

(defun vulpea-db-worker-test--attach-path (id)
  "Map ID to an attachment path, as a user function would."
  (concat "mine/" id))

(ert-deftest vulpea-db-worker-rejects-custom-attach-path-functions ()
  "Files need the session when attach paths come from its functions.
Every note with an id gets its attach dir through
`org-attach-id-to-path-function-list'; a function the worker does
not have would fail every file there, only to be parsed again in the
session.  Org's own functions are fine."
  (let ((vulpea-db--extractors nil)
        (vulpea-db-index-heading-level t)
        (vulpea-db-worker--broken nil))
    (let ((org-attach-id-to-path-function-list
           (default-value 'org-attach-id-to-path-function-list)))
      (should-not (vulpea-db-worker-rejection-reasons "x.org")))
    (let ((org-attach-id-to-path-function-list
           '(vulpea-db-worker-test--attach-path
             org-attach-id-uuid-folder-format)))
      (should (memq 'attach-path-functions
                    (vulpea-db-worker-rejection-reasons "x.org"))))
    (let ((org-attach-id-to-path-function-list
           (list (lambda (id) id))))
      (should (memq 'attach-path-functions
                    (vulpea-db-worker-rejection-reasons "x.org"))))))

;;; Settings classification

(defun vulpea-db-worker-test--worker-sources ()
  "Return the source files of every vulpea feature the worker loads.
Follows `require' forms from vulpea-db-worker.el, so a module the
worker starts loading is scanned without updating this test."
  (let ((queue (list 'vulpea-db-worker))
        seen files)
    (while queue
      (let ((feature (pop queue)))
        (unless (memq feature seen)
          (push feature seen)
          (let ((file (locate-library (format "%s.el" feature) t)))
            (push file files)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (condition-case nil
                  (while t
                    (pcase (read (current-buffer))
                      (`(require ',(and dep (pred symbolp)) . ,_)
                       (when (string-prefix-p "vulpea" (symbol-name dep))
                         (push dep queue)))))
                (end-of-file nil)))))))
    files))

(defun vulpea-db-worker-test--referenced-customs (files)
  "Return the customizable variables whose symbols appear in FILES.
The two classification lists are skipped: naming a setting there must
not count as the worker's code reading it, or dropping a setting from
the allowlist would also drop it from the candidates."
  (let ((found (make-hash-table :test #'eq)))
    (cl-labels ((walk (form)
                  (cond
                   ((symbolp form)
                    (when (custom-variable-p form)
                      (puthash form t found)))
                   ((consp form)
                    (walk (car form))
                    (walk (cdr form)))
                   ((vectorp form)
                    (mapc #'walk form)))))
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (unless (and (eq (car-safe form) 'defconst)
                               (memq (cadr form)
                                     '(vulpea-db-worker--settings-vars
                                       vulpea-db-worker--settings-not-mirrored)))
                    (walk form))))
            (end-of-file nil)))))
    (hash-table-keys found)))

(defconst vulpea-db-worker-test--org-internal-settings
  '(org-archive-tag
    org-use-tag-inheritance
    org-tags-exclude-from-inheritance
    org-use-property-inheritance
    org-category
    enable-local-variables
    enable-dir-local-variables
    safe-local-variable-values
    ignored-local-variables
    ignored-local-variable-values
    safe-local-variable-directories
    enable-local-eval
    org-attach-id-dir
    org-attach-id-to-path-function-list
    org-attach-use-inheritance
    org-link-abbrev-alist
    org-todo-keywords
    org-plain-list-ordered-item-terminator
    org-list-allow-alphabetical)
  "Settings extraction reads inside org and Emacs internals.
The classification test only sees what vulpea's code names, so these
are pinned by hand: dropping one from the allowlist must fail.")

(ert-deftest vulpea-db-worker-mirrors-org-internal-settings ()
  "Settings read inside org's own functions stay on the allowlist."
  (should (equal (seq-remove (lambda (sym)
                               (memq sym vulpea-db-worker--settings-vars))
                             vulpea-db-worker-test--org-internal-settings)
                 nil)))

(ert-deftest vulpea-db-worker-settings-are-classified ()
  "Every setting extraction may read is either mirrored or excused.
The worker is a clean `emacs --batch' process: a setting missing from
`vulpea-db-worker--settings-vars' keeps its default there, so users
who customize it get different results from the worker than from
their session, and the async-vs-sync equivalence tests, which run
with defaults, cannot notice.  Candidates are all vulpea-db- and
vulpea-buffer- options plus every option the worker's code names.  A
failure lists the unclassified ones: add each to the allowlist, or
to `vulpea-db-worker--settings-not-mirrored' with the reason."
  ;; The worker loads org-attach on demand; load it here so its
  ;; options are custom variables the scan can see
  (require 'org-attach)
  (let* ((prefixed nil)
         (_ (mapatoms
             (lambda (sym)
               (when (and (custom-variable-p sym)
                          (string-match-p "\\`vulpea-\\(?:db\\|buffer\\)-"
                                          (symbol-name sym)))
                 (push sym prefixed)))))
         (candidates (seq-uniq
                      (append prefixed
                              (vulpea-db-worker-test--referenced-customs
                               (vulpea-db-worker-test--worker-sources)))))
         (unclassified
          (sort (seq-remove
                 (lambda (sym)
                   (or (memq sym vulpea-db-worker--settings-vars)
                       (assq sym vulpea-db-worker--settings-not-mirrored)))
                 candidates)
                (lambda (a b) (string< a b)))))
    (should (equal unclassified nil))))

(ert-deftest vulpea-db-worker-settings-classification-consistent ()
  "No setting is both mirrored and excused, and every excuse has a reason."
  (dolist (entry vulpea-db-worker--settings-not-mirrored)
    (should (symbolp (car entry)))
    (should (stringp (cdr entry)))
    (should-not (memq (car entry) vulpea-db-worker--settings-vars))))

(ert-deftest vulpea-db-worker-command-prefers-newer ()
  "The worker command forces `load-prefer-newer'.

The worker is spawned with -Q, so without this it silently loads a
stale .elc even when the .el next to it is newer - and then runs
different extraction code than the main process (which, under eldev,
prefers the newer source).  The mismatch is invisible until an
extractor observes behavior the stale bytecode predates."
  (let ((cmd (vulpea-db-worker--command)))
    (should (member "(setq load-prefer-newer t)" cmd))
    ;; It must take effect before the worker library itself is loaded.
    (should (< (seq-position cmd "(setq load-prefer-newer t)")
               (seq-position cmd "-l")))))

(ert-deftest vulpea-db-worker-async-database-equals-sync ()
  "Worker-extracted data lands in the database byte-identically.
Indexes the adversarial corpus twice - synchronously in one database,
through the worker in another - and requires every table to match.
This is the correctness contract of async extraction."
  (vulpea-db-worker-test--with-file
      vulpea-db-extract-test--granularity-corpus
    (let ((vulpea-db-index-heading-level t)
          sync-dump async-dump)
      ;; Sync reference
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-db-update-file path)
        (setq sync-dump (vulpea-db-worker-test--db-dump)))
      ;; Async via worker
      (vulpea-test--with-temp-db
        (vulpea-db)
        (should (vulpea-db-worker-can-handle-p path))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (setq async-dump (vulpea-db-worker-test--db-dump)))
      (should (equal (plist-get sync-dump :files)
                     (plist-get async-dump :files)))
      (dolist (table '(:notes :tags :links :meta :properties))
        (should (equal (plist-get sync-dump table)
                       (plist-get async-dump table)))))))

(defun vulpea-db-worker-test--dumps (path)
  "Index PATH synchronously and through the worker; return both dumps.
Returns (SYNC . ASYNC), each from its own temporary database."
  (let (sync-dump async-dump)
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (setq sync-dump (vulpea-db-worker-test--db-dump)))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait)
          (setq async-dump (vulpea-db-worker-test--db-dump)))
      (vulpea-db-worker-stop))
    (cons sync-dump async-dump)))

(ert-deftest vulpea-db-worker-honors-approved-dir-locals ()
  "An unsafe dir-local the user approved reaches the worker too.
Approving one stores it in `safe-local-variable-values'; the worker
must see that list, or it silently drops the value."
  (let* ((dir (make-temp-file "vulpea-worker-approved-" t))
         (path (expand-file-name "note.org" dir))
         (vulpea-db-parse-method 'temp-buffer)
         (enable-local-variables t)
         (org-use-tag-inheritance t)
         (safe-local-variable-values '((org-use-tag-inheritance))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" dir)
            (prin1 '((org-mode . ((org-use-tag-inheritance . nil))))
                   (current-buffer)))
          (with-temp-file path
            (insert ":PROPERTIES:\n:ID: approved-file\n:END:\n"
                    "#+title: F\n#+filetags: :ftag:\n\n"
                    "* H\n:PROPERTIES:\n:ID: approved-h\n:END:\n"))
          (let* ((dumps (vulpea-db-worker-test--dumps path))
                 (sync-tags (plist-get (car dumps) :tags)))
            (should-not (assoc "approved-h" sync-tags))
            (should (equal sync-tags (plist-get (cdr dumps) :tags)))))
      (delete-directory dir t))))

(ert-deftest vulpea-db-worker-applies-safe-dir-locals-despite-unknown-ones ()
  "A dir-local only the session knows is safe does not cost the rest.
Packages mark their variables safe with a `safe-local-variable'
property, and the worker does not load them.  With
`enable-local-variables' t one unknown variable sends the whole set
to a prompt, which a batch process answers with no; the worker must
still apply the variables it does know are safe, like the category."
  (let* ((dir (make-temp-file "vulpea-worker-unknown-" t))
         (path (expand-file-name "note.org" dir))
         (vulpea-db-parse-method 'temp-buffer)
         (enable-local-variables t))
    (put 'vulpea-db-worker-test--package-var 'safe-local-variable #'stringp)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" dir)
            (prin1 '((org-mode . ((org-category . "dirlocal")
                                  (vulpea-db-worker-test--package-var . "x"))))
                   (current-buffer)))
          (with-temp-file path
            (insert ":PROPERTIES:\n:ID: unknown-file\n:END:\n#+title: F\n"))
          (let ((dumps (vulpea-db-worker-test--dumps path)))
            (should (equal (nth 14 (assoc "unknown-file"
                                          (plist-get (car dumps) :notes)))
                           "dirlocal"))
            (should (equal (plist-get (car dumps) :notes)
                           (plist-get (cdr dumps) :notes)))))
      (put 'vulpea-db-worker-test--package-var 'safe-local-variable nil)
      (delete-directory dir t))))

(ert-deftest vulpea-db-worker-honors-link-abbreviations ()
  "Links written with an `org-link-abbrev-alist' abbreviation match.
Org expands abbreviations while parsing, so a worker without them
indexes such a link as a fuzzy one and the backlink disappears."
  (let ((org-link-abbrev-alist '(("nt" . "id:%s"))))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: abbrev-src\n:END:\n#+title: S\n\n[[nt:abbrev-target]]\n"
      (let ((dumps (vulpea-db-worker-test--dumps path)))
        (should (equal (mapcar (lambda (row) (list (nth 1 row) (nth 2 row)))
                               (plist-get (car dumps) :links))
                       '(("abbrev-target" "id"))))
        (should (equal (plist-get (car dumps) :links)
                       (plist-get (cdr dumps) :links)))))))

(defun vulpea-db-worker-test--abbrev-fn (tag)
  "Expand link abbreviation TAG, as a session-only function would."
  (concat "id:" tag))

(ert-deftest vulpea-db-worker-sends-only-portable-link-abbreviations ()
  "Function-valued abbreviations are named, not sent, to the worker.
The function exists only in the session; depending on the org
version, the worker would either fail or silently index the link as
a fuzzy one."
  (let* ((org-link-abbrev-alist '(("str" . "https://example.com/%s")
                                  ("fn" . vulpea-db-worker-test--abbrev-fn)
                                  ("pct" . "https://example.com/%(upcase)")))
         (sent (alist-get 'org-link-abbrev-alist
                          (nth 1 (vulpea-db-worker--settings-form)))))
    (should (equal sent '(("str" . "https://example.com/%s")
                          ("fn" . :vulpea-session-function)
                          ("pct" . :vulpea-session-function))))))

(ert-deftest vulpea-db-worker-hands-back-files-using-session-abbreviations ()
  "The worker refuses a file that uses a session-only abbreviation.
It answers with an error, which the main process turns into a
synchronous index; files that do not use it are extracted as usual."
  (let ((org-link-abbrev-alist '(("str" . "https://example.com/%s")))
        (vulpea-db-worker--session-abbrev-tags nil))
    (setq org-link-abbrev-alist '(("str" . "https://example.com/%s")
                                  ("fn" . :vulpea-session-function)))
    (vulpea-db-worker--apply-session-abbrevs)
    (should (equal org-link-abbrev-alist '(("str" . "https://example.com/%s"))))
    (should (equal vulpea-db-worker--session-abbrev-tags '("fn")))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: uses-fn\n:END:\n#+title: U\n\n[[fn:target]]\n"
      (should (vulpea-db-worker--session-abbrev-used-p path)))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: no-fn\n:END:\n#+title: N\n\n[[str:target]] and fn:plain\n"
      (should-not (vulpea-db-worker--session-abbrev-used-p path)))))

(defun vulpea-db-worker-test--pct-fn (tag)
  "Expand TAG for a %(...) link abbreviation."
  (concat "id:" tag))
(put 'vulpea-db-worker-test--pct-fn 'org-link-abbrev-safe t)

(ert-deftest vulpea-db-worker-percent-function-abbreviations ()
  "A %(fn) link abbreviation indexes the same through the worker."
  (let ((org-link-abbrev-alist
         '(("pf" . "%(vulpea-db-worker-test--pct-fn)"))))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: pct-src\n:END:\n#+title: S\n\n[[pf:pct-target]]\n"
      (let ((dumps (let ((inhibit-message t))
                     (vulpea-db-worker-test--dumps path))))
        (should (equal (mapcar (lambda (row) (list (nth 1 row) (nth 2 row)))
                               (plist-get (car dumps) :links))
                       '(("pct-target" "id"))))
        (should (equal (plist-get (car dumps) :links)
                       (plist-get (cdr dumps) :links)))))))

(ert-deftest vulpea-db-worker-percent-function-link-keywords ()
  "A file whose own #+LINK: calls a function indexes the same.
The worker parses it, finds the keyword needs a session function and
hands the file back; a file with a plain #+LINK: stays in the worker."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: kw-src\n:END:\n#+title: K\n#+LINK: pk %(vulpea-db-worker-test--pct-fn)\n\n[[pk:kw-target]]\n"
    (let ((dumps (let ((inhibit-message t))
                   (vulpea-db-worker-test--dumps path))))
      (should (equal (mapcar (lambda (row) (list (nth 1 row) (nth 2 row)))
                             (plist-get (car dumps) :links))
                     '(("kw-target" "id"))))
      (should (equal (plist-get (car dumps) :links)
                     (plist-get (cdr dumps) :links)))))
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: kw-plain\n:END:\n#+title: K\n#+LINK: pk https://example.com/%s\n\n[[pk:x]]\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let (statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_p status _c) (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal statuses '(applied)))))))

(ert-deftest vulpea-db-worker-link-abbreviation-functions ()
  "A function-valued link abbreviation is expanded by the session.
The function exists only in the session, so the worker hands files
that use it back to the main process; files that do not use it are
still extracted in the worker."
  (let ((org-link-abbrev-alist
         '(("fn" . vulpea-db-worker-test--abbrev-fn))))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: abbrev-fn-src\n:END:\n#+title: S\n\n[[fn:abbrev-fn-target]]\n"
      (let ((dumps (let ((inhibit-message t))
                     (vulpea-db-worker-test--dumps path))))
        (should (equal (mapcar (lambda (row) (list (nth 1 row) (nth 2 row)))
                               (plist-get (car dumps) :links))
                       '(("abbrev-fn-target" "id"))))
        (should (equal (plist-get (car dumps) :links)
                       (plist-get (cdr dumps) :links)))))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: abbrev-fn-plain\n:END:\n#+title: P\n"
      (vulpea-test--with-temp-db
        (vulpea-db)
        (let (statuses)
          (let ((vulpea-db-worker-done-functions
                 (list (lambda (_p status _c) (push status statuses)))))
            (vulpea-db-worker-request path)
            (vulpea-db-worker-test--wait))
          (should (equal statuses '(applied))))))))

(ert-deftest vulpea-db-worker-honors-file-keywords-and-dir-locals ()
  "In-file keywords and dir-locals reach the worker as they reach the session.
The worker skips the user's mode hooks but re-runs `org-mode' per
file under the default parse method, which reads #+TODO and applies
dir-locals; the doctor points people with hook-set settings there."
  (let* ((dir (make-temp-file "vulpea-worker-dirlocals-" t))
         (path (expand-file-name "note.org" dir))
         (vulpea-db-parse-method 'temp-buffer)
         (enable-local-variables :all)
         sync-dump async-dump)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" dir)
            (prin1 '((org-mode . ((org-category . "dirlocal"))))
                   (current-buffer)))
          (with-temp-file path
            (insert ":PROPERTIES:\n:ID: dirlocal-file\n:END:\n#+title: F\n"
                    "#+TODO: TODO WAITING | DONE\n\n"
                    "* WAITING Call Bob\n:PROPERTIES:\n:ID: dirlocal-h\n:END:\n"))
          (vulpea-test--with-temp-db
            (vulpea-db)
            (vulpea-db-update-file path)
            (setq sync-dump (vulpea-db-worker-test--db-dump)))
          (vulpea-test--with-temp-db
            (vulpea-db)
            (vulpea-db-worker-request path)
            (vulpea-db-worker-test--wait)
            (setq async-dump (vulpea-db-worker-test--db-dump)))
          (let ((heading (assoc "dirlocal-h" (plist-get sync-dump :notes))))
            (should (equal (nth 3 heading) "Call Bob"))
            (should (equal (nth 9 heading) "WAITING"))
            (should (equal (nth 14 heading) "dirlocal")))
          (should (equal (plist-get sync-dump :notes)
                         (plist-get async-dump :notes))))
      (vulpea-db-worker-stop)
      (delete-directory dir t))))

(ert-deftest vulpea-db-worker-async-database-equals-sync-headings-off ()
  "The worker agrees with sync indexing when heading notes are off.
Heading content then belongs to the file note, so the links table is
where a mismatch in the mirrored setting would show."
  (vulpea-db-worker-test--with-file
      vulpea-db-extract-test--granularity-corpus
    (let ((vulpea-db-index-heading-level nil)
          sync-dump async-dump)
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-db-update-file path)
        (setq sync-dump (vulpea-db-worker-test--db-dump)))
      (vulpea-test--with-temp-db
        (vulpea-db)
        (should (vulpea-db-worker-can-handle-p path))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (setq async-dump (vulpea-db-worker-test--db-dump)))
      (should (member "h-body-target"
                      (mapcar #'cadr (plist-get sync-dump :links))))
      (dolist (table '(:notes :tags :links :meta :properties))
        (should (equal (plist-get sync-dump table)
                       (plist-get async-dump table)))))))

(ert-deftest vulpea-db-worker-unchanged-content-refreshes-stamp ()
  "Touching a file without changing content only refreshes the stamp.
The worker reports the same content hash; no notes are rewritten and
the done hook reports `unchanged'."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: stamp-id\n:END:\n#+TITLE: Stamp\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (set-file-times path (time-add (current-time) 10))
      (let (statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal statuses '(unchanged)))
        ;; Stored mtime caught up with the touch
        (let ((stored (vulpea-db--get-file-hash path))
              (attrs (file-attributes path)))
          (should (equal (plist-get stored :mtime)
                         (float-time
                          (file-attribute-modification-time attrs)))))))))

(ert-deftest vulpea-db-worker-updated-hook-fires-on-worker-apply ()
  "`vulpea-db-updated-functions' announces a worker-indexed file.
Regardless of write mode - streamed apply in the main process, or a
full-write reply - one file indexed through the worker produces one
data-changed announcement with the path and note count."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: updated-hook-worker\n:END:\n#+TITLE: U\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let (calls)
        (let ((vulpea-db-updated-functions
               (list (lambda (p count) (push (list p count) calls)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal calls (list (list path 1))))))))

(ert-deftest vulpea-db-worker-updated-hook-fires-on-written-reply ()
  "A full-write `written' reply announces the data change.
In full-write mode `vulpea-db--apply-parse-ctx' runs inside the
worker process, out of reach of main-process listeners; the main
process must run `vulpea-db-updated-functions' when the reply lands."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: written-hook-note\n:END:\n#+TITLE: W\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let* ((attrs (file-attributes path))
             (mtime (float-time (file-attribute-modification-time attrs)))
             (size (file-attribute-size attrs))
             calls statuses)
        (let ((vulpea-db-updated-functions
               (list (lambda (p count) (push (list p count) calls))))
              (vulpea-db-worker-done-functions
               (list (lambda (_p status _c) (push status statuses)))))
          (vulpea-db-worker--dispatch
           (list 'written path "somehash" mtime size 1
                 (list "written-hook-note") nil)))
        (should (equal statuses '(applied)))
        (should (equal calls (list (list path 1))))))))

(ert-deftest vulpea-db-worker-updated-hook-fires-on-written-missing ()
  "Ghost-row cleanup on a written reply announces the removal.
A written reply for a vanished file removes what the worker
committed; that is a data change and must reach
`vulpea-db-updated-functions' as (PATH 0)."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path "/tmp/vulpea-ghost-hook-test.org")
          calls statuses)
      (vulpea-db--insert-note
       :id "ghost-hook-id" :path path :level 0 :pos 0 :title "Ghost"
       :properties nil :modified-at "2026-07-06 10:00:00")
      (vulpea-db--update-file-hash path "somehash" 1.0 10)
      (let ((vulpea-db-updated-functions
             (list (lambda (p count) (push (list p count) calls))))
            (vulpea-db-worker-done-functions
             (list (lambda (_p status _c) (push status statuses)))))
        (vulpea-db-worker--dispatch
         (list 'written path "somehash" 1.0 10 1 (list "ghost-hook-id") nil)))
      (should (equal statuses '(missing)))
      (should (equal calls (list (list path 0)))))))

(ert-deftest vulpea-db-worker-updated-hook-silent-on-unchanged ()
  "An unchanged-content completion announces no data change.
Only the stored stamp is refreshed; `vulpea-db-updated-functions'
stays silent while `vulpea-db-worker-done-functions' reports
`unchanged'."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: updated-hook-unchanged\n:END:\n#+TITLE: S\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (set-file-times path (time-add (current-time) 10))
      (let (calls statuses)
        (let ((vulpea-db-updated-functions
               (list (lambda (p count) (push (list p count) calls))))
              (vulpea-db-worker-done-functions
               (list (lambda (_p status _c) (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal statuses '(unchanged)))
        (should-not calls)))))

(ert-deftest vulpea-db-worker-stale-result-discarded-and-requeued ()
  "A file that changes mid-parse is not applied from the stale result.
Simulated by rewriting the file after `done' data is fabricated: the
completion handler must report `stale' and leave the database alone."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: stale-id\n:END:\n#+TITLE: Old\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let* ((attrs (file-attributes path))
             (old-mtime (float-time (file-attribute-modification-time attrs)))
             (old-size (file-attribute-size attrs))
             statuses)
        ;; Change the file so the recorded stamp no longer matches
        (with-temp-buffer
          (insert ":PROPERTIES:\n:ID: stale-id\n:END:\n#+TITLE: Newer and longer\n")
          (write-region (point-min) (point-max) path nil 'silent))
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          ;; Complete with the outdated stamp, as a slow worker would
          (vulpea-db-worker--complete
           path "some-hash" old-mtime old-size
           (list :path path :file-node (list :id "stale-id" :title "Old"))))
        (should (equal statuses '(stale)))
        (should (= 0 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "stale-id"))))))))

(ert-deftest vulpea-db-worker-restarts-after-crash ()
  "Killing the worker mid-session does not lose the ability to index."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: crash-id\n:END:\n#+TITLE: Crash\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      ;; Spawn and let it settle, then kill it
      (vulpea-db-worker--ensure)
      (let ((proc vulpea-db-worker--process))
        (delete-process proc)
        (while (process-live-p proc)
          (accept-process-output nil 0.05)))
      ;; A new request must transparently spawn a fresh worker
      (vulpea-db-worker-request path)
      (vulpea-db-worker-test--wait)
      (should (= 1 (caar (emacsql (vulpea-db)
                                  [:select (funcall count *)
                                   :from notes :where (= id $s1)]
                                  "crash-id")))))))

(ert-deftest vulpea-db-worker-respects-settings ()
  "The worker mirrors allowlisted settings from the main process.
With `vulpea-db-index-plain-links' nil, plain links must be absent
from worker-extracted results too."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: settings-id\n:END:\n#+TITLE: S\n\nSee [[id:bracket-target][b]] and https://plain.example.com here.\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-index-plain-links nil))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait))
      (let ((dests (mapcar #'car
                           (emacsql (vulpea-db)
                                    [:select [dest] :from links
                                     :where (= source $s1)]
                                    "settings-id"))))
        (should (member "bracket-target" dests))
        (should-not (member "//plain.example.com" dests))))))

(ert-deftest vulpea-db-worker-mirrors-org-category ()
  "The worker mirrors the default value of `org-category'.

Extraction reads `org-category' from the parse buffer, which for
temp-buffer methods resolves to the default value; the worker must
see the same default the main process has.
https://github.com/d12frosted/vulpea/issues/389"
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: org-cat-id\n:END:\n#+TITLE: C\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((org-category "mirrored-cat"))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait))
      (should (equal "mirrored-cat"
                     (caar (emacsql (vulpea-db)
                                    [:select [category] :from notes
                                     :where (= id $s1)]
                                    "org-cat-id")))))))

(ert-deftest vulpea-db-worker-can-handle-p-rejections ()
  "The worker refuses work it cannot replicate faithfully."
  (should (vulpea-db-worker-can-handle-p "/tmp/note.org"))
  ;; Encrypted files need interactive decryption
  (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org.gpg"))
  ;; AST-reading extractor plugins never cross processes
  (let ((vulpea-db--extractors
         (list (make-vulpea-extractor :name 'fake :requires-ast t
                                      :extract-fn #'ignore))))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org")))
  ;; ...but only when declared: undeclared extractors run against a
  ;; nil-AST context in the main process, so the worker stays usable
  (let ((vulpea-db--extractors
         (list (make-vulpea-extractor :name 'fake :extract-fn #'ignore))))
    (should (vulpea-db-worker-can-handle-p "/tmp/note.org")))
  ;; Predicate-valued heading-level indexing is not serializable
  (let ((vulpea-db-index-heading-level (lambda (_) t)))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org"))))

(ert-deftest vulpea-db-worker-full-write-database-equals-sync ()
  "Full-write mode produces the same database as synchronous indexing.
The worker parses AND writes through its own connection (WAL); the
tables must match the synchronous reference byte for byte, and the
note IDs must still be registered with org-id in the main process."
  (vulpea-db-worker-test--with-file
      vulpea-db-extract-test--granularity-corpus
    (let ((vulpea-db-index-heading-level t)
          (org-id-track-globally t)
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-files nil)
          sync-dump full-dump)
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-db-update-file path)
        (setq sync-dump (vulpea-db-worker-test--db-dump)))
      (vulpea-test--with-temp-db
        (vulpea-db)
        (let ((vulpea-db-async-extraction 'full)
              (vulpea-db-note-index-filter-functions nil)
              statuses)
          (let ((vulpea-db-worker-done-functions
                 (list (lambda (_path status _count)
                         (push status statuses)))))
            (vulpea-db-worker-request path)
            (vulpea-db-worker-test--wait))
          (should (equal statuses '(applied)))
          ;; WAL is active on this database
          (should (equal "wal"
                         (caar (sqlite-select
                                (oref (vulpea-db) handle)
                                "PRAGMA journal_mode"))))
          (setq full-dump (vulpea-db-worker-test--db-dump))
          ;; org-id registration happened in the main process
          (should (equal (gethash "corpus-file-id" org-id-locations)
                         (abbreviate-file-name path)))))
      (dolist (table '(:notes :tags :links :meta :properties :files))
        (should (equal (plist-get sync-dump table)
                       (plist-get full-dump table)))))))

(ert-deftest vulpea-db-worker-full-write-unregisters-released-ids ()
  "Full-write mode drops the org-id registration of an id a re-parse lost.
The worker's apply runs with SKIP-ORG-ID, so the released ids travel
in the `written' reply and are unregistered in the main process."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: fw-released-file\n:END:\n#+TITLE: F\n\n* H\n:PROPERTIES:\n:ID: fw-released-heading\n:END:\n"
    (let ((vulpea-db-index-heading-level t)
          (org-id-track-globally t)
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-files nil))
      (vulpea-test--with-temp-db
        (vulpea-db)
        (let ((vulpea-db-async-extraction 'full)
              (vulpea-db-note-index-filter-functions nil)
              (afile (abbreviate-file-name path)))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait)
          (should (equal (gethash "fw-released-heading" org-id-locations) afile))
          (with-temp-file path
            (insert ":PROPERTIES:\n:ID: fw-released-file\n:END:\n#+TITLE: F\n"))
          (vulpea-db-worker-request path 'force)
          (vulpea-db-worker-test--wait)
          (should (equal (vulpea-db--get-file-note-ids path) '("fw-released-file")))
          (should-not (gethash "fw-released-heading" org-id-locations))
          (should (equal (gethash "fw-released-file" org-id-locations) afile)))))))

(ert-deftest vulpea-db-worker-full-write-honors-main-process-filters ()
  "Full-write degrades to extract-only while index filters exist.
`vulpea-db-note-index-filter-functions' run in the main process, so
with one registered the worker must not write directly - the filter
still decides what is indexed."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: kept-note\n:END:\n#+TITLE: Kept\n\n* Rejected\n:PROPERTIES:\n:ID: rejected-note\n:END:\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-index-heading-level t)
            (vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions
             (list (lambda (note)
                     (not (equal (vulpea-note-id note) "rejected-note"))))))
        (should-not (vulpea-db-worker--full-write-p))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (should (= 1 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "kept-note"))))
        (should (= 0 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "rejected-note"))))))))

(ert-deftest vulpea-db-worker-honors-exclude-children-property ()
  "The worker mirrors `vulpea-db-exclude-children-property'.
Subtree exclusion happens during extraction, so it has to work in the
worker too.  A non-default property name is used deliberately: the
default would be honored by the worker's own defaults even if the
setting were never sent across."
  (vulpea-db-worker-test--with-file
      (concat ":PROPERTIES:\n:ID: w-ic-file\n:END:\n#+TITLE: Area\n\n"
              "* Container\n"
              ":PROPERTIES:\n:ID: w-ic-container\n:NO_KIDS: t\n:END:\n\n"
              "** Child\n:PROPERTIES:\n:ID: w-ic-child\n:END:\n")
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-index-heading-level t)
            (vulpea-db-exclude-children-property "NO_KIDS")
            (vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions nil))
        (vulpea-db-worker-refresh-settings)
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (dolist (id '("w-ic-file" "w-ic-container"))
          (should (= 1 (caar (emacsql (vulpea-db)
                                      [:select (funcall count *)
                                       :from notes :where (= id $s1)]
                                      id)))))
        (should (= 0 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "w-ic-child"))))))))

(ert-deftest vulpea-db-worker-honors-exclude-property-name-case ()
  "The worker matches `vulpea-db-exclude-property' case-insensitively.
Exclusion happens during extraction, so the worker has to normalize the
configured name the same way the main process does.  A lowercase
non-default name is used deliberately: it fails both when the setting
never crosses the process boundary and when it crosses unnormalized."
  (vulpea-db-worker-test--with-file
      (concat ":PROPERTIES:\n:ID: w-ic-kept\n:END:\n#+TITLE: Area\n\n"
              "* Excluded\n"
              ":PROPERTIES:\n:ID: w-ic-excluded\n:roam_exclude: t\n:END:\n")
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-index-heading-level t)
            (vulpea-db-exclude-property "roam_exclude")
            (vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions nil))
        (vulpea-db-worker-refresh-settings)
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (should (= 1 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "w-ic-kept"))))
        (should (= 0 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "w-ic-excluded"))))))))

(ert-deftest vulpea-db-worker-full-write-resolves-pending-claims ()
  "A refile with the destination written first heals through the worker.
End-to-end vulpea#469 in full-write mode: the worker's write of the
destination records the losing insert as a pending claim, its write
of the origin releases the id and carries the claimant in the reply,
and the main process re-requests the destination, which wins the id."
  (let ((origin (vulpea-test--create-temp-org-file
                 (concat ":PROPERTIES:\n:ID: fw-origin\n:END:\n"
                         "#+TITLE: Origin\n\n"
                         "* Task\n:PROPERTIES:\n:ID: fw-task\n:END:\n")))
        (dest (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: fw-dest\n:END:\n#+TITLE: Destination\n")))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          (let ((vulpea-db-index-heading-level t)
                (vulpea-db-async-extraction 'full)
                (vulpea-db-note-index-filter-functions nil))
            ;; The point of this test is the full-write leg; fail
            ;; loudly if the worker would degrade to streaming.
            (should (vulpea-db-worker--full-write-p))
            ;; Index both; the origin owns the task id.
            (vulpea-db-worker-request origin)
            (vulpea-db-worker-request dest)
            (vulpea-db-worker-test--wait)
            ;; Refile with the destination written and indexed first:
            ;; its insert loses to the origin's row, leaving a claim.
            (with-temp-file dest
              (insert ":PROPERTIES:\n:ID: fw-dest\n:END:\n"
                      "#+TITLE: Destination\n\n"
                      "* Task\n:PROPERTIES:\n:ID: fw-task\n:END:\n"))
            (vulpea-db-worker-request dest)
            (vulpea-db-worker-test--wait)
            (should (equal origin
                           (caar (emacsql (vulpea-db)
                                          [:select path :from notes
                                           :where (= id $s1)]
                                          "fw-task"))))
            (should (equal (list dest)
                           (vulpea-db--get-pending-claims "fw-task")))
            ;; The origin saved without the heading releases the id;
            ;; the destination is re-requested and wins it.
            (with-temp-file origin
              (insert ":PROPERTIES:\n:ID: fw-origin\n:END:\n"
                      "#+TITLE: Origin\n"))
            (vulpea-db-worker-request origin)
            (vulpea-db-worker-test--wait)
            (should (equal dest
                           (caar (emacsql (vulpea-db)
                                          [:select path :from notes
                                           :where (= id $s1)]
                                          "fw-task"))))
            (should-not (vulpea-db--get-pending-claims "fw-task"))))
      (vulpea-db-worker-stop)
      (dolist (file (list origin dest))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest vulpea-db-worker-full-write-unchanged-content-stamps ()
  "Full-write mode also short-circuits unchanged content."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: full-stamp-id\n:END:\n#+TITLE: Stamp\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (set-file-times path (time-add (current-time) 10))
      (let ((vulpea-db-async-extraction 'full)
            statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal statuses '(unchanged)))))))

(ert-deftest vulpea-db-worker-filters-inert-logic ()
  "Full-write activates when index filters provably cannot matter.
The schema-validation filter is installed unconditionally at load;
it only counts as active when schemas are registered with a
non-silent action."
  (let ((vulpea-db-async-extraction 'full))
    ;; No filters at all
    (let ((vulpea-db-note-index-filter-functions nil))
      (should (vulpea-db-worker--full-write-p)))
    ;; Only the schema filter, no schemas registered: inert
    (let ((vulpea-db-note-index-filter-functions
           '(vulpea-db-schema-validation--filter))
          (vulpea-schema--registry (make-hash-table :test 'eq)))
      (should (vulpea-db-worker--full-write-p)))
    ;; Only the schema filter, silent action: inert even with schemas
    (let ((vulpea-db-note-index-filter-functions
           '(vulpea-db-schema-validation--filter))
          (vulpea-db-schema-validation-action 'silent))
      (should (vulpea-db-worker--full-write-p)))
    ;; A foreign filter: never inert
    (let ((vulpea-db-note-index-filter-functions (list #'ignore)))
      (should-not (vulpea-db-worker--full-write-p)))))

(ert-deftest vulpea-db-worker-guarded-apply-detects-conflict ()
  "A worker result loses against a concurrent programmatic re-index.
Deterministic replay of the full-write race: capture a parse result
and the stored stamp, let a synchronous `vulpea-db-update-file' land
newer content, then apply the old result through the guard - it must
report a conflict and leave the newer data untouched."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: cas-id\n:END:\n#+TITLE: V1\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      ;; The worker's view: stamp and parse result of V1
      (let ((stored (vulpea-db--get-file-hash path))
            (ctx (vulpea-db--parse-file path)))
        ;; Programmatic update to V2 lands first (vino's pattern)
        (with-temp-buffer
          (insert ":PROPERTIES:\n:ID: cas-id\n:END:\n#+TITLE: V2\n")
          (write-region (point-min) (point-max) path nil 'silent))
        (vulpea-db-update-file path)
        ;; The worker's late write must detect the moved stamp
        (should (eq 'conflict (vulpea-db-worker--apply-guarded ctx stored)))
        (should (equal "V2" (vulpea-note-title
                             (vulpea-db-get-by-id "cas-id"))))
        ;; And with an up-to-date stamp the guard applies normally
        (let ((stored2 (vulpea-db--get-file-hash path))
              (ctx2 (vulpea-db--parse-file path)))
          (should (equal 1 (vulpea-db-worker--apply-guarded ctx2 stored2))))))))

(ert-deftest vulpea-db-worker-programmatic-write-wins ()
  "Read-your-writes survives an in-flight worker request (mode t).
A file is sent to the worker, then rewritten and synchronously
re-indexed before the worker answers.  The programmatic content must
be readable immediately and still be there after the worker settles."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: race-id\n:END:\n#+TITLE: V1\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (vulpea-db-worker-request path)
      ;; Programmatic rewrite while the request is in flight
      (with-temp-buffer
        (insert ":PROPERTIES:\n:ID: race-id\n:END:\n#+TITLE: V2\n")
        (write-region (point-min) (point-max) path nil 'silent))
      (vulpea-db-update-file path)
      ;; Read-your-writes, right now
      (should (equal "V2" (vulpea-note-title (vulpea-db-get-by-id "race-id"))))
      (vulpea-db-worker-test--wait)
      ;; And after the worker settled (stale results discarded)
      (should (equal "V2" (vulpea-note-title
                           (vulpea-db-get-by-id "race-id")))))))

(ert-deftest vulpea-db-worker-programmatic-write-wins-full-mode ()
  "Read-your-writes survives an in-flight full-write request.
Same race as `vulpea-db-worker-programmatic-write-wins', but the
worker owns the database write - the transaction guard must keep the
programmatic content authoritative."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: race-full-id\n:END:\n#+TITLE: V1\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions nil))
        (vulpea-db-update-file path)
        (vulpea-db-worker-request path)
        (with-temp-buffer
          (insert ":PROPERTIES:\n:ID: race-full-id\n:END:\n#+TITLE: V2\n")
          (write-region (point-min) (point-max) path nil 'silent))
        (vulpea-db-update-file path)
        (should (equal "V2" (vulpea-note-title
                             (vulpea-db-get-by-id "race-full-id"))))
        (vulpea-db-worker-test--wait)
        (should (equal "V2" (vulpea-note-title
                             (vulpea-db-get-by-id "race-full-id"))))))))

(ert-deftest vulpea-db-worker-force-reapplies-unchanged-content ()
  "A forced request re-applies even when the content hash matches.
Force re-index exists for parser or settings changes: content is
identical, but extraction output is not, so the unchanged-content
shortcut must be skipped.  Covers mode t."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: force-id\n:END:\n#+TITLE: Force\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (let (statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          ;; Non-force on unchanged content: stamps only
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait)
          ;; Forced: must re-apply
          (vulpea-db-worker-request path 'force)
          (vulpea-db-worker-test--wait))
        (should (equal (nreverse statuses) '(unchanged applied)))))))

(ert-deftest vulpea-db-worker-force-reapplies-unchanged-content-full-mode ()
  "Forced requests re-apply in full-write mode too."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: force-full-id\n:END:\n#+TITLE: ForceFull\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions nil)
            statuses)
        (vulpea-db-update-file path)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait)
          (vulpea-db-worker-request path 'force)
          (vulpea-db-worker-test--wait))
        (should (equal (nreverse statuses) '(unchanged applied)))))))

(ert-deftest vulpea-db-worker-force-survives-crash-requeue ()
  "Force marks are preserved when a dead worker's files are re-queued."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: force-crash-id\n:END:\n#+TITLE: FC\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      (let (statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          (vulpea-db-worker-request path 'force)
          ;; Kill the worker before it can answer
          (let ((proc vulpea-db-worker--process))
            (delete-process proc)
            (while (process-live-p proc)
              (accept-process-output nil 0.05)))
          ;; The sentinel re-queues; without autosync it goes straight
          ;; back to a fresh worker, force mark intact
          (vulpea-db-worker-test--wait))
        ;; Content unchanged, so only a preserved force mark explains
        ;; an `applied' result
        (should (memq 'applied statuses))))))

(ert-deftest vulpea-db-sync-force-scan-routes-through-worker ()
  "A force directory scan dispatches to the worker when async is on.
This is the parser-epoch migration path: it must re-apply every file
\(not stamp them as unchanged) without the blocking loop."
  (let* ((dir (make-temp-file "vulpea-force-scan" t))
         (vulpea-db-async-extraction t))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          (dotimes (i 3)
            (with-temp-file (expand-file-name (format "note-%d.org" i) dir)
              (insert (format ":PROPERTIES:\n:ID: scan-%d\n:END:\n#+TITLE: N%d\n" i i))))
          ;; Everything indexed and up to date
          (dolist (f (directory-files dir t "\\.org\\'"))
            (vulpea-db-update-file f))
          ;; Force scan with autosync-like async processing
          (let (statuses)
            (let ((vulpea-db-worker-done-functions
                   (list (lambda (_path status _count)
                           (push status statuses))))
                  (vulpea-db-autosync-mode t)
                  (vulpea-db-sync-verbose nil))
              (vulpea-db-sync-update-directory dir 'force)
              ;; Drain the queue manually (no timers in batch tests)
              (while vulpea-db-sync--queue
                (vulpea-db-sync--process-queue))
              (vulpea-db-worker-test--wait))
            ;; Every file re-applied despite unchanged content
            (should (equal statuses '(applied applied applied))))
          (vulpea-db-worker-stop))
      (delete-directory dir t))))

(ert-deftest vulpea-db-worker-ast-free-extractor-allows-async ()
  "Extractors that declare no AST dependency do not disable async.
An attachment-style extractor reads only note data and writes its own
table; with :requires-ast nil the worker handles the file, extraction
stays at element granularity, and the extractor still runs in the
main process during apply."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: attach-note\n:END:\n#+TITLE: A\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db--extractors nil)
            (extractor-ran nil))
        (vulpea-db-register-extractor
         (make-vulpea-extractor
          :name 'test-attachments
          :version 1
          :requires-ast nil
          :schema '((test-attachments
                     [(note-id :not-null) (file :not-null)]
                     (:primary-key [note-id file])))
          :extract-fn (lambda (ctx note-data)
                        ;; AST-free contract: ctx may carry no AST
                        (setq extractor-ran (null (vulpea-parse-ctx-ast ctx)))
                        (emacsql (vulpea-db)
                                 [:insert :into test-attachments :values $v1]
                                 (vector (plist-get note-data :id) "file.png"))
                        note-data)))
        ;; Async is allowed, element granularity preserved
        (should (vulpea-db-worker-can-handle-p path))
        (should (eq 'element (vulpea-db--effective-granularity)))
        ;; But full-write is not: the extractor function only exists here
        (let ((vulpea-db-async-extraction 'full))
          (should-not (vulpea-db-worker--full-write-p)))
        ;; End to end through the worker: extractor ran on the main side
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (should extractor-ran)
        (should (equal '(("attach-note" "file.png"))
                       (emacsql (vulpea-db)
                                [:select [note-id file]
                                 :from test-attachments])))))))

(ert-deftest vulpea-db-worker-worker-safe-extractor-runs-in-worker ()
  "A :worker-safe extractor runs inside the worker in full-write mode.
The extractor function lives in a library the worker loads
\(:worker-lib); it records the executing process id in its table, so
the test can prove the work happened in the subprocess."
  (let ((lib (make-temp-file "vulpea-worker-ext-" nil ".el"
                             "(require 'vulpea-db)\n(require 'emacsql)\n(defun vulpea-test-worker-ext-fn (_ctx note-data)\n  (emacsql (vulpea-db)\n           [:insert :into worker-ext :values $v1]\n           (vector (plist-get note-data :id)\n                   (number-to-string (emacs-pid))))\n  note-data)\n")))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: worker-ext-note\n:END:\n#+TITLE: W\n"
      (unwind-protect
          (vulpea-test--with-temp-db
            (vulpea-db)
            (let ((vulpea-db--extractors nil)
                  (vulpea-db-async-extraction 'full)
                  (vulpea-db-note-index-filter-functions nil)
                  statuses)
              ;; Define the fn locally too (registration side)
              (load lib nil t)
              (vulpea-db-register-extractor
               (make-vulpea-extractor
                :name 'worker-ext
                :version 1
                :requires-ast nil
                :worker-safe t
                :worker-lib lib
                :schema '((worker-ext
                           [(note-id :not-null) (pid :not-null)]))
                :extract-fn #'vulpea-test-worker-ext-fn))
              (should (vulpea-db-worker--full-write-p))
              (let ((vulpea-db-worker-done-functions
                     (list (lambda (_path status _count)
                             (push status statuses)))))
                (vulpea-db-worker-request path)
                (vulpea-db-worker-test--wait))
              (should (equal statuses '(applied)))
              (let ((row (car (emacsql (vulpea-db)
                                       [:select [note-id pid]
                                        :from worker-ext]))))
                (should (equal (car row) "worker-ext-note"))
                ;; Ran in the worker, not in this process
                (should-not (equal (cadr row)
                                   (number-to-string (emacs-pid)))))))
        (delete-file lib)))))

(ert-deftest vulpea-db-worker-worker-safe-extractor-note-data-persisted ()
  "Core-field contributions persist when the extractor runs in the worker.
A :worker-safe extractor that adds a link to note-data in full-write
mode must land it in both the normalized links table and the
materialized notes.links column, written through the worker's own
connection."
  (let ((lib (make-temp-file "vulpea-worker-linkext-" nil ".el"
                             "(defun vulpea-test-worker-linkext-fn (_ctx note-data)\n  (plist-put note-data :links\n             (append (plist-get note-data :links)\n                     (list (list :dest \"worker-dest\" :type \"id\"\n                                 :pos 1 :description \"W\"))))\n  note-data)\n")))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: worker-link-note\n:END:\n#+TITLE: WL\n"
      (unwind-protect
          (vulpea-test--with-temp-db
            (vulpea-db)
            (let ((vulpea-db--extractors nil)
                  (vulpea-db-async-extraction 'full)
                  (vulpea-db-note-index-filter-functions nil)
                  statuses)
              (load lib nil t)
              (vulpea-db-register-extractor
               (make-vulpea-extractor
                :name 'worker-linkext
                :version 1
                :requires-ast nil
                :worker-safe t
                :worker-lib lib
                :extract-fn #'vulpea-test-worker-linkext-fn))
              (should (vulpea-db-worker--full-write-p))
              (let ((vulpea-db-worker-done-functions
                     (list (lambda (_path status _count)
                             (push status statuses)))))
                (vulpea-db-worker-request path)
                (vulpea-db-worker-test--wait))
              (should (equal statuses '(applied)))
              ;; Normalized links table
              (should (equal '(("worker-link-note" "worker-dest" "id"))
                             (emacsql (vulpea-db)
                                      [:select [source dest type]
                                       :from links
                                       :where (= source $s1)]
                                      "worker-link-note")))
              ;; Materialized notes.links column
              (let ((links-json
                     (caar (emacsql (vulpea-db)
                                    [:select [links] :from notes
                                     :where (= id $s1)]
                                    "worker-link-note"))))
                (should (stringp links-json))
                (should (string-match-p "worker-dest" links-json)))))
        (delete-file lib)))))

(ert-deftest vulpea-db-worker-worker-safe-extractor-sees-nil-ast ()
  "The nil-AST guarantee holds inside the worker too.
A worker-safe extractor without :requires-ast t runs in the worker
process, where a parsed tree actually exists - the stripping in
`vulpea-db--run-extractors' must hide it there as well, so the
guarantee stays deterministic in every mode."
  (let ((lib (make-temp-file "vulpea-worker-nilast-" nil ".el"
                             "(require 'vulpea-db)\n(require 'emacsql)\n(defun vulpea-test-worker-nilast-fn (ctx note-data)\n  (emacsql (vulpea-db)\n           [:insert :into worker-nilast :values $v1]\n           (vector (plist-get note-data :id)\n                   (if (vulpea-parse-ctx-ast ctx) \"ast\" \"nil\")))\n  note-data)\n")))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: nilast-note\n:END:\n#+TITLE: N\n"
      (unwind-protect
          (vulpea-test--with-temp-db
            (vulpea-db)
            (let ((vulpea-db--extractors nil)
                  (vulpea-db-async-extraction 'full)
                  (vulpea-db-note-index-filter-functions nil)
                  statuses)
              ;; Define the fn locally too (registration side)
              (load lib nil t)
              (vulpea-db-register-extractor
               (make-vulpea-extractor
                :name 'worker-nilast
                :version 1
                :requires-ast nil
                :worker-safe t
                :worker-lib lib
                :schema '((worker-nilast
                           [(note-id :not-null) (ast :not-null)]))
                :extract-fn #'vulpea-test-worker-nilast-fn))
              (should (vulpea-db-worker--full-write-p))
              (let ((vulpea-db-worker-done-functions
                     (list (lambda (_path status _count)
                             (push status statuses)))))
                (vulpea-db-worker-request path)
                (vulpea-db-worker-test--wait))
              (should (equal statuses '(applied)))
              (should (equal '(("nilast-note" "nil"))
                             (emacsql (vulpea-db)
                                      [:select [note-id ast]
                                       :from worker-nilast])))))
        (delete-file lib)))))

(ert-deftest vulpea-db-worker-worker-safe-unresolved-degrades ()
  "A worker-safe extractor the worker cannot resolve degrades safely.
Without a loadable :worker-lib the worker falls back to streaming the
results, and the extractor runs in the main process instead - no
notes are lost, no extractor output is lost."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: unresolved-note\n:END:\n#+TITLE: U\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db--extractors nil)
            (vulpea-db-async-extraction 'full)
            (vulpea-db-note-index-filter-functions nil)
            (ran-in-main nil)
            statuses)
        (fset 'vulpea-test-unresolved-fn
              (lambda (_ctx note-data)
                (setq ran-in-main t)
                note-data))
        (vulpea-db-register-extractor
         (make-vulpea-extractor
          :name 'unresolved-ext
          :version 1
          :requires-ast nil
          :worker-safe t
          ;; No :worker-lib and the fn is not defined in the worker
          :extract-fn 'vulpea-test-unresolved-fn))
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_path status _count)
                       (push status statuses)))))
          (vulpea-db-worker-request path)
          (vulpea-db-worker-test--wait))
        (should (equal statuses '(applied)))
        (should ran-in-main)
        (should (= 1 (caar (emacsql (vulpea-db)
                                    [:select (funcall count *)
                                     :from notes :where (= id $s1)]
                                    "unresolved-note"))))))))

(ert-deftest vulpea-db-worker-mixed-extractors-block-full-write ()
  "One non-worker-safe extractor is enough to block full-write."
  (let ((vulpea-db-async-extraction 'full)
        (vulpea-db-note-index-filter-functions nil)
        (vulpea-db--extractors
         (list (make-vulpea-extractor
                :name 'safe :requires-ast nil :worker-safe t
                :extract-fn #'ignore)
               (make-vulpea-extractor
                :name 'unsafe :requires-ast nil
                :extract-fn #'ignore))))
    (should-not (vulpea-db-worker--full-write-p))))

(ert-deftest vulpea-db-worker-ast-extractor-still-disables-async ()
  "Extractors declaring :requires-ast t keep the conservative behavior."
  (let ((vulpea-db-parse-granularity 'element)
        (vulpea-db--extractors
         (list (make-vulpea-extractor
                :name 'needs-ast
                :version 1
                :requires-ast t
                :extract-fn #'ignore))))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org"))
    (should (eq 'object (vulpea-db--effective-granularity)))))

(ert-deftest vulpea-db-worker-undeclared-extractor-stays-async ()
  "An extractor without a :requires-ast declaration keeps async alive.
Fast by default: it runs in the main process against a nil-AST
context, and extraction stays at element granularity."
  (let ((vulpea-db-parse-granularity 'element)
        (vulpea-db--extractors
         (list (make-vulpea-extractor
                :name 'undeclared
                :version 1
                :extract-fn #'ignore))))
    (should (vulpea-db-worker-can-handle-p "/tmp/note.org"))
    (should (eq 'element (vulpea-db--effective-granularity)))))

(ert-deftest vulpea-db-worker-threshold-routing ()
  "The size threshold routes small files to the synchronous path."
  (let ((path (vulpea-test--create-temp-org-file
               ":PROPERTIES:\n:ID: tiny-id\n:END:\n#+TITLE: Tiny\n")))
    (unwind-protect
        (progn
          ;; nil threshold: everything goes to the worker
          (let ((vulpea-db-async-extraction-threshold nil))
            (should (vulpea-db-worker-should-handle-p path)))
          ;; Threshold above the file size: stays synchronous
          (let ((vulpea-db-async-extraction-threshold (* 1024 1024)))
            (should-not (vulpea-db-worker-should-handle-p path)))
          ;; Threshold below the file size: goes to the worker
          (let ((vulpea-db-async-extraction-threshold 1))
            (should (vulpea-db-worker-should-handle-p path))))
      (delete-file path))))

(ert-deftest vulpea-db-sync-async-completion-message ()
  "The background-sync summary fires when work actually lands.
Dispatch bumps the in-flight counter; terminal statuses drain it;
the summary is emitted exactly once, with honest numbers, when the
last file completes.  A requeued file counts anew on re-dispatch."
  (require 'vulpea-db-sync)
  (let ((vulpea-db-sync--async-dispatched 3)
        (vulpea-db-sync--async-applied 0)
        (vulpea-db-sync--async-unchanged 0)
        (vulpea-db-sync--async-start-time (current-time))
        (vulpea-db-sync-verbose t)
        (messages nil))
    (cl-letf (((symbol-function 'vulpea-db-sync--message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (vulpea-db-sync--worker-done "/tmp/a.org" 'applied 5)
      (vulpea-db-sync--worker-done "/tmp/b.org" 'unchanged nil)
      (should (null messages))          ; nothing until the last one
      (vulpea-db-sync--worker-done "/tmp/c.org" 'requeued nil)
      ;; requeued is terminal for this burst: summary fires now
      (should (= 1 (length messages)))
      (should (string-match-p "background sync complete - 2 files (1 updated, 1 unchanged"
                              (car messages)))
      ;; counters reset for the next burst
      (should (zerop vulpea-db-sync--async-dispatched))
      (should (zerop vulpea-db-sync--async-applied)))))

(ert-deftest vulpea-db-worker-respawn-before-sentinel-salvages ()
  "A request racing a dead worker's deferred sentinel loses no files.
Reproduces the timer-before-sentinel window: the worker dies with a
file in flight, its sentinel is prevented from running (as Emacs
defers it), and a new request arrives.  --ensure must salvage the
dead worker's in-flight file - both files end up indexed."
  (let ((file-a (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: salvage-a\n:END:\n#+TITLE: A\n"))
        (file-b (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: salvage-b\n:END:\n#+TITLE: B\n")))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          ;; Dispatch A, then simulate death-without-sentinel: kill the
          ;; process with its sentinel neutralized, exactly the state
          ;; --ensure observes when a timer beats the sentinel
          (vulpea-db-worker-request file-a)
          (let ((w1 vulpea-db-worker--process))
            (set-process-sentinel w1 #'ignore)
            (delete-process w1)
            (while (process-live-p w1)
              (accept-process-output nil 0.05)))
          (should (equal vulpea-db-worker--in-flight (list file-a)))
          ;; The next request must salvage A before spawning W2
          (vulpea-db-worker-request file-b)
          (vulpea-db-worker-test--wait)
          (dolist (id '("salvage-a" "salvage-b"))
            (should (= 1 (caar (emacsql (vulpea-db)
                                        [:select (funcall count *)
                                         :from notes :where (= id $s1)]
                                        id))))))
      (vulpea-db-worker-stop)
      (delete-file file-a)
      (delete-file file-b))))

(ert-deftest vulpea-db-worker-stale-sentinel-ignored ()
  "A dead worker's deferred sentinel must not clobber its replacement.
After the replacement worker W2 is live with work in flight, firing
W1's sentinel by hand (as Emacs eventually does) must not reset the
process, the in-flight list, or re-enqueue W2's files."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: stale-sentinel-note\n:END:\n#+TITLE: S\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-worker--ensure)
      (let ((w1 vulpea-db-worker--process))
        ;; Replace W1 the way the salvage path does
        (set-process-sentinel w1 #'ignore)
        (delete-process w1)
        (while (process-live-p w1)
          (accept-process-output nil 0.05))
        (vulpea-db-worker-request path)
        (let ((w2 vulpea-db-worker--process)
              (in-flight (copy-sequence vulpea-db-worker--in-flight))
              (requeued nil))
          (should-not (eq w1 w2))
          ;; Fire W1's stale sentinel by hand against the guard
          (let ((vulpea-db-worker-done-functions
                 (list (lambda (_p status _c)
                         (when (eq status 'requeued)
                           (setq requeued t))))))
            (vulpea-db-worker--sentinel w1 "killed\n"))
          ;; W2 and its state must be untouched
          (should (eq vulpea-db-worker--process w2))
          (should (equal vulpea-db-worker--in-flight in-flight))
          (should-not requeued)
          (vulpea-db-worker-test--wait))))))

(ert-deftest vulpea-db-worker-stale-filter-output-discarded ()
  "Output from a replaced worker process must not reach dispatch.
A stale process draining its pipe into the shared assembly state
would corrupt the current worker's stream."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (unwind-protect
        (progn
          (vulpea-db-worker--ensure)
          (let ((current vulpea-db-worker--current))
            ;; A begin line from a non-current process is ignored
            (vulpea-db-worker--filter
             'not-the-current-process "(begin \"/tmp/ghost.org\")\n")
            (should (equal vulpea-db-worker--current current))))
      (vulpea-db-worker-stop))))

(ert-deftest vulpea-db-worker-dispatch-error-does-not-drop-later-lines ()
  "An error handling one protocol line must not swallow the rest.
Two done messages arrive in one chunk; the first apply errors (hook
signals); the second file must still complete."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: after-error-note\n:END:\n#+TITLE: AE\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-worker--ensure)
      (let ((proc vulpea-db-worker--process)
            (completed nil))
        ;; Fake two in-flight entries
        (vulpea-db-worker--forget "/tmp/nonexistent-a.org") ; no-op, keeps state sane
        (let ((vulpea-db-worker-done-functions
               (list (lambda (p status _c)
                       (when (equal p path)
                         (setq completed status))
                       (when (equal p "/tmp/error-note.org")
                         (error "boom"))))))
          ;; First line errors in the hook, second must still apply.
          ;; missing file -> 'missing status for the first
          (vulpea-db-worker--filter
           proc
           (concat "(done \"/tmp/error-note.org\" \"h\" 1.0 10)\n"
                   (format "(begin %S)\n" path)
                   (format "(file-node (:id \"after-error-note\" :title \"AE\"))\n")
                   (format "(done %S \"hash\" %s %d)\n"
                           path
                           (float-time (file-attribute-modification-time
                                        (file-attributes path)))
                           (file-attribute-size (file-attributes path))))))
        (should (eq completed 'applied))))))

(ert-deftest vulpea-db-worker-written-for-vanished-file-removes-ghosts ()
  "A written reply for a file that no longer exists removes its rows.
Reproduces the ghost-note race: a never-indexed file is deleted after
the worker's stat but before its commit; the removal event deleted
nothing (no rows yet), so the written handler must clean up what the
worker committed."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path "/tmp/vulpea-ghost-note-test.org")
          statuses)
      ;; Simulate the worker's commit for a path that does not exist
      (vulpea-db--insert-note
       :id "ghost-id" :path path :level 0 :pos 0 :title "Ghost"
       :properties nil :modified-at "2026-07-06 10:00:00")
      (vulpea-db--update-file-hash path "somehash" 1.0 10)
      ;; The written reply arrives; file-attributes is nil
      (let ((vulpea-db-worker-done-functions
             (list (lambda (_p status _c) (push status statuses)))))
        (vulpea-db-worker--dispatch
         (list 'written path "somehash" 1.0 10 1 (list "ghost-id") nil)))
      (should (equal statuses '(missing)))
      (should (= 0 (caar (emacsql (vulpea-db)
                                  [:select (funcall count *)
                                   :from notes :where (= id $s1)]
                                  "ghost-id"))))
      (should (= 0 (caar (emacsql (vulpea-db)
                                  [:select (funcall count *)
                                   :from files :where (= path $s1)]
                                  path)))))))

(ert-deftest vulpea-db-worker-version-mismatch-degrades-to-streaming ()
  "A worker seeing foreign db constants must not open the database.
Simulated on the worker side: apply-settings with a mismatched
db-version sets the guard, and parse-and-write streams results
instead of writing (no written reply, a done reply instead)."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: vmismatch-note\n:END:\n#+TITLE: V\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-db-update-file path)
      ;; Run the worker-side handlers in-process with fake constants
      (let ((vulpea-db-worker--version-mismatch nil)
            (replies nil))
        (cl-letf (((symbol-function 'vulpea-db-worker--reply)
                   (lambda (form) (push (car form) replies))))
          (vulpea-db-worker--apply-settings
           nil (org-link-types) nil
           (list :db-version -1 :parser-epoch -1))
          (should vulpea-db-worker--version-mismatch)
          (vulpea-db-worker--handle-parse-and-write path "/tmp/other.db")
          ;; Streaming replies, never a written (no db was opened)
          (should (equal (nreverse replies) '(extractors begin file-node done)))
          (should-not (member 'written replies)))))))

(ert-deftest vulpea-db-worker-watchdog-kills-silent-worker ()
  "The watchdog kills a worker that goes silent with work in flight.
Simulated by backdating last-activity past the timeout; the salvage
path must re-enqueue the in-flight file, and two consecutive hang
kills must mark the worker broken."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: hang-note\n:END:\n#+TITLE: H\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-worker-hang-timeout 300)
            (vulpea-db-worker--hang-kills 0)
            (vulpea-db-worker--broken nil)
            statuses)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_p status _c) (push status statuses)))))
          (vulpea-db-worker-request path)
          ;; Backdate: worker "silent" for longer than the timeout
          (setq vulpea-db-worker--last-activity (- (float-time) 301))
          (let ((w1 vulpea-db-worker--process))
            (vulpea-db-worker--watchdog)
            (should (= 1 vulpea-db-worker--hang-kills))
            ;; The kill triggers salvage via sentinel
            (while (process-live-p w1)
              (accept-process-output nil 0.05))
            (sit-for 0.2))
          (should (memq 'requeued statuses))
          ;; Second consecutive hang marks broken
          (setq vulpea-db-worker--last-activity (- (float-time) 301))
          (let ((w2 vulpea-db-worker--process))
            (when (process-live-p w2)
              (vulpea-db-worker--watchdog))
            ;; Let the kill's salvage run here: left pending, it would
            ;; requeue this test's file into whatever test runs next
            (while (process-live-p w2)
              (accept-process-output nil 0.05))
            (sit-for 0.2))
          (should (>= vulpea-db-worker--hang-kills 1)))))))

(ert-deftest vulpea-db-worker-spawn-failure-marks-broken ()
  "A worker that cannot start is marked broken with one warning.
Without this, every queued file retried the spawn and printed a
message of its own - thousands of lines on a full scan - while
nothing told the user why.  The files still get indexed, through the
synchronous path."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((paths (mapcar (lambda (i)
                            (vulpea-test--create-temp-org-file
                             (format ":PROPERTIES:\n:ID: spawn-fail-%d\n:END:\n#+title: S%d\n"
                                     i i)))
                          '(1 2 3)))
           (invocation-directory "/nonexistent/vulpea-test-emacs/")
           (vulpea-db-async-extraction t)
           (vulpea-db-worker--broken nil)
           (vulpea-db-sync--queue nil)
           (vulpea-db-sync--queue-tail nil)
           (vulpea-db-sync--queue-set (make-hash-table :test #'equal))
           (vulpea-db-sync--processing nil)
           (warnings 0)
           (dispatch-errors 0))
      (unwind-protect
          (cl-letf* ((orig-message (symbol-function 'message))
                     ((symbol-function 'display-warning)
                      (lambda (&rest _) (setq warnings (1+ warnings))))
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (when (and fmt (string-prefix-p "Vulpea: Error dispatching" fmt))
                          (setq dispatch-errors (1+ dispatch-errors)))
                        (apply orig-message fmt args))))
            (dolist (path paths)
              (vulpea-db-sync--enqueue path))
            (vulpea-db-sync--process-queue)
            (should vulpea-db-worker--broken)
            (should (= 1 warnings))
            (should (<= dispatch-errors 1))
            (dolist (i '(1 2 3))
              (should (vulpea-db-get-by-id (format "spawn-fail-%d" i)))))
        (vulpea-db-worker-stop)
        (mapc #'delete-file paths)))))

(ert-deftest vulpea-db-worker-learns-link-types-registered-later ()
  "A link type registered after the worker spawned still reaches it.
`org-link-set-parameters' changes `org-link-parameters' in place, so
no variable watcher fires; packages registering types lazily would
otherwise leave the worker indexing their links as fuzzy ones."
  (let ((org-link-parameters (copy-tree org-link-parameters))
        (first (vulpea-test--create-temp-org-file
                ":PROPERTIES:\n:ID: late-first\n:END:\n#+title: F\n"))
        (second (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: late-second\n:END:\n#+title: S\n\n[[vulpealate:target]]\n")))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          ;; Spawn the worker before the type exists
          (vulpea-db-worker-request first)
          (vulpea-db-worker-test--wait)
          (org-link-set-parameters "vulpealate")
          (vulpea-db-worker-request second)
          (vulpea-db-worker-test--wait)
          (should (equal (mapcar (lambda (row) (list (nth 1 row) (nth 2 row)))
                                 (plist-get (vulpea-db-worker-test--db-dump)
                                            :links))
                         '(("target" "vulpealate")))))
      (vulpea-db-worker-stop)
      (delete-file first)
      (delete-file second))))

(ert-deftest vulpea-db-worker-refreshes-settings-changed-in-place ()
  "A mirrored setting changed in place reaches the live worker.
Editing a list in place (say `setf' on an `alist-get') fires no
variable watcher; comparing the whole settings message catches it."
  (let ((org-link-abbrev-alist (list (cons "ddg" "https://a.example/%s")))
        (first (vulpea-test--create-temp-org-file
                ":PROPERTIES:\n:ID: inplace-first\n:END:\n#+title: F\n")))
    (unwind-protect
        (vulpea-test--with-temp-db
          (vulpea-db)
          (vulpea-db-worker-request first)
          (vulpea-db-worker-test--wait)
          (setcdr (car org-link-abbrev-alist) "https://b.example/%s")
          (let ((sent nil)
                (send (symbol-function 'vulpea-db-worker--send)))
            (cl-letf (((symbol-function 'vulpea-db-worker--send)
                       (lambda (form)
                         (push form sent)
                         (funcall send form))))
              (vulpea-db-worker-refresh-if-changed)
              (vulpea-db-worker-refresh-if-changed))
            (should (= 1 (length sent)))
            (should (equal (alist-get 'org-link-abbrev-alist
                                      (nth 1 (car sent)))
                           '(("ddg" . "https://b.example/%s"))))))
      (vulpea-db-worker-stop)
      (delete-file first))))

(ert-deftest vulpea-db-worker-completion-resets-hang-counter ()
  "A successful completion proves liveness and resets the hang count."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: alive-note\n:END:\n#+TITLE: A\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-worker--hang-kills 1))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (should (= 0 vulpea-db-worker--hang-kills))))))

(ert-deftest vulpea-db-worker-live-settings-refresh ()
  "A settings variable changed mid-session reaches the live worker.
The variable watcher schedules a debounced refresh; a file extracted
afterwards must honor the new value (plain links dropped)."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: live-settings-note\n:END:\n#+TITLE: L\n\nSee [[id:bt][b]] and https://plain.example.com here.\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      ;; Spawn with plain links ON (dynamic binding so the watcher
      ;; fires on setq below without leaking globally)
      (let ((vulpea-db-index-plain-links t))
        (vulpea-db-worker--ensure)
        ;; Change mid-session; watcher schedules the refresh
        (setq vulpea-db-index-plain-links nil)
        (let ((deadline (+ (float-time) 5)))
          (while (and vulpea-db-worker--refresh-timer
                      (< (float-time) deadline))
            (sit-for 0.1)))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (let ((dests (mapcar #'car
                             (emacsql (vulpea-db)
                                      [:select [dest] :from links
                                       :where (= source $s1)]
                                      "live-settings-note"))))
          (should (member "bt" dests))
          (should-not (member "//plain.example.com" dests)))))))

(ert-deftest vulpea-db-worker-honors-alias-property ()
  "A customized `vulpea-buffer-alias-property' reaches the worker.
The file carries both the default ALIASES property and a custom one;
extraction must read only the property the setting names, in the
worker exactly as in the main process.
https://github.com/d12frosted/vulpea/issues/457"
  (vulpea-db-worker-test--with-file
      (concat ":PROPERTIES:\n"
              ":ID: alias-prop-note\n"
              ":ALIASES: Diff\n"
              ":ROAM_ALIASES: Job\n"
              ":END:\n"
              "#+TITLE: Test\n")
    (let ((vulpea-buffer-alias-property "ROAM_ALIASES")
          sync-dump async-dump)
      ;; Sync reference: the custom property wins
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-db-update-file path)
        (should (equal '("Job")
                       (vulpea-note-aliases
                        (vulpea-db-get-by-id "alias-prop-note"))))
        (setq sync-dump (vulpea-db-worker-test--db-dump)))
      ;; Async via worker must extract the same aliases
      (vulpea-test--with-temp-db
        (vulpea-db)
        (should (vulpea-db-worker-can-handle-p path))
        (vulpea-db-worker-request path)
        (vulpea-db-worker-test--wait)
        (should (equal '("Job")
                       (vulpea-note-aliases
                        (vulpea-db-get-by-id "alias-prop-note"))))
        (setq async-dump (vulpea-db-worker-test--db-dump)))
      (dolist (table '(:notes :tags :links :meta :properties))
        (should (equal (plist-get sync-dump table)
                       (plist-get async-dump table)))))))

(defun vulpea-db-worker-test--drain-fallbacks ()
  "Run the deferred synchronous fallbacks until none is left."
  (let ((deadline (+ (float-time) 10)))
    (while (and vulpea-db-worker--fallback-queue
                (< (float-time) deadline))
      (sit-for 0.01))))

(ert-deftest vulpea-db-worker-error-falls-back-to-sync ()
  "A file the worker fails on is indexed in the main process instead.
The worker can fail where the session would not - a setting that
names a function only the session defines, a package it does not
load - and a failure must not leave the file out of the database.
The fallback runs from a timer, not inside the reply handler, where
quitting is inhibited and a burst of failures would parse file after
file; the file then counts as applied."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: worker-error-note\n:END:\n#+title: E\n"
    (vulpea-test--with-temp-db
      (vulpea-db)
      (let ((vulpea-db-worker--in-flight (list path))
            (vulpea-db-worker--in-flight-tail nil)
            (vulpea-db-worker--in-flight-count 1)
            (vulpea-db-worker--reported-failures (make-hash-table :test #'equal))
            (vulpea-db-worker--fallback-queue nil)
            statuses)
        (setq vulpea-db-worker--in-flight-tail vulpea-db-worker--in-flight)
        (let ((vulpea-db-worker-done-functions
               (list (lambda (_p status _c) (push status statuses))))
              (inhibit-message t))
          (vulpea-db-worker--dispatch `(error ,path "boom"))
          (should-not (vulpea-db-get-by-id "worker-error-note"))
          (vulpea-db-worker-test--drain-fallbacks))
        (should (equal statuses '(applied)))
        (should (vulpea-db-get-by-id "worker-error-note"))))))

(ert-deftest vulpea-db-worker-error-reported-once ()
  "The same worker failure is announced once, not per file.
A setting that breaks the worker breaks it for every file using it,
and a full scan must not print one line per file."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let* ((paths (mapcar (lambda (i)
                            (vulpea-test--create-temp-org-file
                             (format ":PROPERTIES:\n:ID: once-%d\n:END:\n#+title: O\n" i)))
                          '(1 2 3)))
           (vulpea-db-worker--reported-failures (make-hash-table :test #'equal))
           (vulpea-db-worker--fallback-queue nil)
           (vulpea-db-worker--in-flight nil)
           (vulpea-db-worker--in-flight-tail nil)
           (vulpea-db-worker--in-flight-count 0)
           (messages 0))
      (unwind-protect
          (cl-letf* ((orig (symbol-function 'message))
                     ((symbol-function 'message)
                      (lambda (fmt &rest args)
                        (when (and fmt (string-match-p "worker failed" fmt))
                          (setq messages (1+ messages)))
                        (apply orig fmt args))))
            (dolist (path paths)
              (let ((vulpea-db-worker--in-flight (list path))
                    (vulpea-db-worker--in-flight-tail nil))
                (vulpea-db-worker--dispatch
                 `(error ,path "Symbol's function definition is void: f"))))
            (vulpea-db-worker-test--drain-fallbacks)
            (should (= messages 1))
            (dolist (i '(1 2 3))
              (should (vulpea-db-get-by-id (format "once-%d" i)))))
        (mapc #'delete-file paths)))))

;;; Session vs worker comparison

(ert-deftest vulpea-db-worker-compare-files-sees-heading-only-drift ()
  "A difference only a heading carries is still reported.
With tag inheritance turned off by a session-only hook, the file
note keeps its own tags and only the heading's inherited ones move."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: head-file\n:END:\n#+title: F\n#+filetags: :ftag:\n\n* H\n:PROPERTIES:\n:ID: head-h\n:END:\n"
    (let ((vulpea-db-parse-method 'temp-buffer)
          (org-use-tag-inheritance t)
          (org-mode-hook
           (list (lambda () (setq-local org-use-tag-inheritance nil)))))
      (let ((result (vulpea-db-worker-compare-files (list path))))
        (should (equal (mapcar #'car result) (list path)))
        (should (equal (cdar result) '(:tags)))))))

(ert-deftest vulpea-db-worker-compare-files-sees-heading-count-drift ()
  "A different number of heading notes is reported as :headings."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: count-file\n:END:\n#+title: F\n\n* H\n:PROPERTIES:\n:ID: count-h\n:END:\n"
    (let ((vulpea-db-parse-method 'temp-buffer)
          (vulpea-db-index-heading-level t)
          (org-mode-hook
           (list (lambda () (setq-local vulpea-db-index-heading-level nil)))))
      (let ((result (vulpea-db-worker-compare-files (list path))))
        (should (equal (mapcar #'car result) (list path)))
        (should (memq :headings (cdar result)))))))

(ert-deftest vulpea-db-worker-compare-files-agrees-by-default ()
  "With nothing configured differently, worker and session agree."
  (vulpea-db-worker-test--with-file
      vulpea-db-extract-test--granularity-corpus
    (let ((vulpea-db-parse-method 'temp-buffer)
          (org-mode-hook nil))
      (should (equal (vulpea-db-worker-compare-files (list path))
                     nil)))))

(ert-deftest vulpea-db-worker-compare-files-names-differing-fields ()
  "A session-only setting shows up as the fields it changes.
Here a mode hook the worker never runs sets the category, so every
note's category (and where it came from) differs."
  (vulpea-db-worker-test--with-file
      ":PROPERTIES:\n:ID: cmp-file\n:END:\n#+title: F\n\n* H\n:PROPERTIES:\n:ID: cmp-h\n:END:\n"
    (let ((vulpea-db-parse-method 'temp-buffer)
          (org-mode-hook
           (list (lambda () (setq-local org-category "from-hook")))))
      (let ((result (vulpea-db-worker-compare-files (list path))))
        (should (equal (mapcar #'car result) (list path)))
        (should (memq :category (cdar result)))))))

(ert-deftest vulpea-db-worker-compare-files-accepts-handed-back-files ()
  "A file the worker hands back is not a difference.
It is indexed in the session, so session and database agree."
  (let ((org-link-abbrev-alist '(("fn" . vulpea-db-worker-test--abbrev-fn))))
    (vulpea-db-worker-test--with-file
        ":PROPERTIES:\n:ID: handed-back\n:END:\n#+title: H\n\n[[fn:target]]\n"
      (let ((vulpea-db-parse-method 'temp-buffer)
            (org-mode-hook nil))
        (should (equal (vulpea-db-worker-compare-files (list path)) nil))))))

(ert-deftest vulpea-db-worker-compare-files-reports-errors ()
  "A file that cannot be compared is reported, not fatal."
  (let ((missing (expand-file-name "vulpea-no-such-file.org"
                                   temporary-file-directory)))
    (let ((result (vulpea-db-worker-compare-files (list missing))))
      (should (equal (mapcar #'car result) (list missing)))
      (should (stringp (cdar result))))))

(provide 'vulpea-db-worker-test)
;;; vulpea-db-worker-test.el ends here
