;;; vulpea-db-path-test.el --- Tests for path normalization -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 17 Jul 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for Unicode normalization of file paths used as database
;; keys.  On macOS the same file can be referred to by an NFC string
;; (filename syscalls decoded via utf-8-hfs) and an NFD string (raw
;; output of fd/find/fswatch subprocesses).  Without canonicalization
;; the database accumulates duplicate rows per file that update
;; independently, so consumers read stale hashes and metadata.
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-db-sync)
(require 'vulpea-test-helpers)

;; "яйце" with precomposed й (U+0439) vs decomposed и + U+0306.
(defconst vulpea-db-path-test--nfc
  (concat "/tmp/vulpea-path-test/" (string ?я ?й ?ц ?е) ".org"))

(defconst vulpea-db-path-test--nfd
  (concat "/tmp/vulpea-path-test/" (string ?я ?и #x0306 ?ц ?е) ".org"))

;;; vulpea-db-normalize-path

(ert-deftest vulpea-db-normalize-path/composes-to-nfc ()
  "NFD input is composed to NFC when normalization is enabled."
  (let ((vulpea-db-path-normalization 'nfc))
    (should (equal (vulpea-db-normalize-path vulpea-db-path-test--nfd)
                   vulpea-db-path-test--nfc))
    (should (equal (vulpea-db-normalize-path vulpea-db-path-test--nfc)
                   vulpea-db-path-test--nfc))))

(ert-deftest vulpea-db-normalize-path/identity-when-disabled ()
  "Paths are returned unchanged when normalization is disabled."
  (let ((vulpea-db-path-normalization nil))
    (should (equal (vulpea-db-normalize-path vulpea-db-path-test--nfd)
                   vulpea-db-path-test--nfd))))

(ert-deftest vulpea-db-normalize-path/nil-path ()
  "nil input returns nil."
  (let ((vulpea-db-path-normalization 'nfc))
    (should-not (vulpea-db-normalize-path nil))))

;;; files table

(ert-deftest vulpea-db-file-hash/nfd-and-nfc-share-one-row ()
  "Updates through NFD and NFC paths hit the same files row."
  (vulpea-test--with-temp-db
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db--update-file-hash vulpea-db-path-test--nfd "hash1" 1.0 10)
      (vulpea-db--update-file-hash vulpea-db-path-test--nfc "hash2" 2.0 20)
      (should (= 1 (caar (emacsql (vulpea-db)
                                  [:select (funcall count *) :from files]))))
      ;; Lookup through either form sees the latest write.
      (should (equal "hash2"
                     (plist-get (vulpea-db--get-file-hash
                                 vulpea-db-path-test--nfd)
                                :hash)))
      (should (equal "hash2"
                     (plist-get (vulpea-db--get-file-hash
                                 vulpea-db-path-test--nfc)
                                :hash))))))

;;; notes table

(ert-deftest vulpea-db-insert-note/normalizes-path ()
  "Note rows are keyed by the canonical path."
  (vulpea-test--with-temp-db
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db)
      (vulpea-test--insert-test-note "id-nfd" "NFD note"
                                     :path vulpea-db-path-test--nfd)
      (should (equal (list vulpea-db-path-test--nfc)
                     (mapcar #'car
                             (emacsql (vulpea-db)
                                      [:select path :from notes])))))))

(ert-deftest vulpea-db-delete-file-notes/normalizes-path ()
  "Deleting by NFD path removes notes stored under NFC path."
  (vulpea-test--with-temp-db
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db)
      (vulpea-test--insert-test-note "id-1" "note"
                                     :path vulpea-db-path-test--nfc)
      (vulpea-db--delete-file-notes vulpea-db-path-test--nfd)
      (should-not (emacsql (vulpea-db) [:select * :from notes])))))

(ert-deftest vulpea-db-query-by-file-path/normalizes-path ()
  "Querying by NFD path finds notes stored under NFC path."
  (vulpea-test--with-temp-db
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db)
      (vulpea-test--insert-test-note "id-1" "note"
                                     :path vulpea-db-path-test--nfc)
      (should (= 1 (length (vulpea-db-query-by-file-path
                            vulpea-db-path-test--nfd)))))))

;;; sync queue

(ert-deftest vulpea-db-sync-enqueue/normalizes-path ()
  "Queue entries are keyed by the canonical path."
  (let ((vulpea-db-path-normalization 'nfc)
        (vulpea-db-sync--queue nil)
        (vulpea-db-sync--queue-tail nil)
        (vulpea-db-sync--queue-set (make-hash-table :test 'equal))
        (vulpea-db-sync--force-set (make-hash-table :test 'equal))
        (vulpea-db-sync--queue-total 0)
        (vulpea-db-sync--timer nil))
    (unwind-protect
        (progn
          (vulpea-db-sync--enqueue vulpea-db-path-test--nfd)
          (should (gethash vulpea-db-path-test--nfc
                           vulpea-db-sync--queue-set))
          ;; The NFC twin does not enqueue a duplicate entry.
          (vulpea-db-sync--enqueue vulpea-db-path-test--nfc)
          (should (= 1 (length vulpea-db-sync--queue))))
      (when vulpea-db-sync--timer
        (cancel-timer vulpea-db-sync--timer)))))

;;; fswatch paths

(ert-deftest vulpea-db-sync-fswatch-normalize-path/composes-to-nfc ()
  "fswatch-reported paths (raw NFD bytes) are canonicalized."
  (let ((vulpea-db-path-normalization 'nfc))
    (should (equal (vulpea-db-sync--fswatch-normalize-path
                    vulpea-db-path-test--nfd)
                   vulpea-db-path-test--nfc))))

;;; cleanup self-heal

(ert-deftest vulpea-db-sync-cleanup/purges-legacy-denormalized-rows ()
  "Cleanup removes rows keyed by non-canonical paths.

Legacy NFD-keyed rows can never be updated once writes are
canonicalized, and `file-exists-p' reports them as existing on
normalization-insensitive file systems, so regular deleted-file
cleanup never removes them.  They must be purged explicitly, along
with the canonical files row, so the file is re-indexed from
scratch on the next scan."
  (vulpea-test--with-temp-db
    ;; Seed legacy split-brain state with normalization disabled:
    ;; notes + files keyed by NFD, plus a fresh files row keyed by NFC.
    (let ((vulpea-db-path-normalization nil))
      (vulpea-db)
      (vulpea-test--insert-test-note "id-1" "note"
                                     :path vulpea-db-path-test--nfd)
      (vulpea-db--update-file-hash vulpea-db-path-test--nfd "stale" 1.0 10)
      (vulpea-db--update-file-hash vulpea-db-path-test--nfc "fresh" 2.0 20))
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db-sync--cleanup-deleted-files-using
       (list vulpea-db-path-test--nfc))
      ;; Legacy NFD rows are gone.
      (should-not (emacsql (vulpea-db)
                           [:select * :from notes :where (= path $s1)]
                           vulpea-db-path-test--nfd))
      (should-not (emacsql (vulpea-db)
                           [:select * :from files :where (= path $s1)]
                           vulpea-db-path-test--nfd))
      ;; The canonical files row is dropped as well, forcing a fresh
      ;; re-index of the file (its notes were just purged).
      (should-not (emacsql (vulpea-db)
                           [:select * :from files :where (= path $s1)]
                           vulpea-db-path-test--nfc)))))

(ert-deftest vulpea-db-sync-cleanup/keeps-canonical-rows ()
  "Cleanup does not touch rows already keyed canonically."
  (vulpea-test--with-temp-db
    (let ((vulpea-db-path-normalization 'nfc))
      (vulpea-db)
      (vulpea-test--insert-test-note "id-1" "note"
                                     :path vulpea-db-path-test--nfc)
      (vulpea-db--update-file-hash vulpea-db-path-test--nfc "fresh" 2.0 20)
      (vulpea-db-sync--cleanup-deleted-files-using
       (list vulpea-db-path-test--nfd))
      (should (emacsql (vulpea-db)
                       [:select * :from files :where (= path $s1)]
                       vulpea-db-path-test--nfc))
      (should (emacsql (vulpea-db)
                       [:select * :from notes :where (= path $s1)]
                       vulpea-db-path-test--nfc)))))

;;; end-to-end on a normalization-insensitive file system

(ert-deftest vulpea-db-update-file/normalizes-path-e2e ()
  "Indexing a file through its NFD name stores canonical NFC rows.

Only runs on file systems where both normalizations address the
same file (macOS HFS+/APFS)."
  (let* ((dir (make-temp-file "vulpea-path-test-" t))
         (nfd (concat (file-name-as-directory dir)
                      (string ?я ?и #x0306 ?ц ?е) ".org"))
         (nfc (concat (file-name-as-directory dir)
                      (string ?я ?й ?ц ?е) ".org")))
    (unwind-protect
        (progn
          (with-temp-file nfd
            (insert ":PROPERTIES:\n:ID: id-e2e\n:END:\n#+title: Egg\n"))
          (skip-unless (file-exists-p nfc))
          (let* ((temp-db (make-temp-file "vulpea-test-" nil ".db"))
                 (vulpea-db-location temp-db)
                 (vulpea-db--connection nil)
                 (vulpea-db-path-normalization 'nfc))
            (unwind-protect
                (progn
                  (vulpea-db)
                  (vulpea-db-update-file nfd)
                  (let ((paths (mapcar #'car (emacsql (vulpea-db)
                                                      [:select path :from notes])))
                        (fpaths (mapcar #'car (emacsql (vulpea-db)
                                                       [:select path :from files]))))
                    (should (equal (list nfc) paths))
                    (should (equal (list nfc) fpaths))))
              (when vulpea-db--connection
                (vulpea-db-close))
              (when (file-exists-p temp-db)
                (delete-file temp-db)))))
      (delete-directory dir t))))

(provide 'vulpea-db-path-test)
;;; vulpea-db-path-test.el ends here
