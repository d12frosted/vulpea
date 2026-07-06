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
                                  scheduled deadline closed outline-path
                                  attach-dir file-title created-at]
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
Ensures the worker and the file are cleaned up."
  (declare (indent 1))
  `(let ((path (vulpea-test--create-temp-org-file ,content)))
     (unwind-protect
         (progn ,@body)
       (vulpea-db-worker-stop)
       (when (file-exists-p path)
         (delete-file path)))))

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

(ert-deftest vulpea-db-worker-can-handle-p-rejections ()
  "The worker refuses work it cannot replicate faithfully."
  (should (vulpea-db-worker-can-handle-p "/tmp/note.org"))
  ;; Encrypted files need interactive decryption
  (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org.gpg"))
  ;; AST-reading extractor plugins never cross processes
  (let ((vulpea-db--extractors
         (list (make-vulpea-extractor :name 'fake :extract-fn #'ignore))))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org")))
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
  "Extractors without the declaration keep the conservative behavior."
  (let ((vulpea-db--extractors
         (list (make-vulpea-extractor
                :name 'needs-ast
                :version 1
                :extract-fn #'ignore))))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org"))
    (should (eq 'object (vulpea-db--effective-granularity)))))

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
         (list 'written path "somehash" 1.0 10 1 (list "ghost-id"))))
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
          (when (process-live-p vulpea-db-worker--process)
            (vulpea-db-worker--watchdog))
          (should (>= vulpea-db-worker--hang-kills 1)))))))

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

(provide 'vulpea-db-worker-test)
;;; vulpea-db-worker-test.el ends here
