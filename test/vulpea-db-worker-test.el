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
  "Wait up to SECONDS (default 60) until the worker is idle."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (vulpea-db-worker-busy-p)
                (< (float-time) deadline))
      (accept-process-output vulpea-db-worker--process 0.05))
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
  ;; Extractor plugins receive the AST, which never crosses processes
  (let ((vulpea-db--extractors (list 'fake)))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org")))
  ;; Predicate-valued heading-level indexing is not serializable
  (let ((vulpea-db-index-heading-level (lambda (_) t)))
    (should-not (vulpea-db-worker-can-handle-p "/tmp/note.org"))))

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

(provide 'vulpea-db-worker-test)
;;; vulpea-db-worker-test.el ends here
