;;; vulpea-db-claims-test.el --- Tests for pending id claims -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 10 Aug 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for pending id claims: a note moved between files must not
;; vanish from the database when the destination is indexed before
;; the origin releases the id (vulpea#469).
;;
;;; Code:

(require 'ert)
(require 'vulpea-db)
(require 'vulpea-db-extract)
(require 'vulpea-db-query)
(require 'vulpea-db-sync)
(require 'vulpea-test-helpers)

;;; Content templates

(defconst vulpea-db-claims-test--task-subtree
  "* Task
:PROPERTIES:
:ID: task-id
:END:
Some body.
")

(defconst vulpea-db-claims-test--origin-with-task
  (concat ":PROPERTIES:\n:ID: origin-id\n:END:\n#+TITLE: Origin\n\n"
          vulpea-db-claims-test--task-subtree))

(defconst vulpea-db-claims-test--origin-without-task
  ":PROPERTIES:\n:ID: origin-id\n:END:\n#+TITLE: Origin\n")

(defconst vulpea-db-claims-test--dest-without-task
  ":PROPERTIES:\n:ID: dest-id\n:END:\n#+TITLE: Destination\n")

(defconst vulpea-db-claims-test--dest-with-task
  (concat ":PROPERTIES:\n:ID: dest-id\n:END:\n#+TITLE: Destination\n\n"
          vulpea-db-claims-test--task-subtree))

;;; Helpers

(defun vulpea-db-claims-test--write (path content)
  "Write CONTENT to file at PATH."
  (with-temp-file path
    (insert content)))

(defun vulpea-db-claims-test--owner (id)
  "Return path of the note with ID, or nil when not in the database."
  (when-let* ((note (vulpea-db-get-by-id id)))
    (vulpea-note-path note)))

(defmacro vulpea-db-claims-test--with-refile (&rest body)
  "Execute BODY after an unlucky-order refile of task-id.

Sets up origin.org (owning heading task-id) and destination.org,
indexes both, then simulates the refile with the destination saved
first: destination.org contains the heading and is re-indexed while
origin.org still holds the id on disk.  BODY runs with anaphoric
`origin' and `dest' bound to the two absolute paths, inside a
temporary notes directory (anaphoric `root')."
  (declare (indent 0))
  `(vulpea-test--with-temp-notes-dir
     (let ((origin (expand-file-name "origin.org" root))
           (dest (expand-file-name "destination.org" root)))
       (vulpea-db-claims-test--write
        origin vulpea-db-claims-test--origin-with-task)
       (vulpea-db-claims-test--write
        dest vulpea-db-claims-test--dest-without-task)
       (vulpea-db-update-file origin)
       (vulpea-db-update-file dest)
       ;; Refile with the unlucky ordering: destination saved and
       ;; indexed while origin still holds the heading on disk.
       (vulpea-db-claims-test--write
        dest vulpea-db-claims-test--dest-with-task)
       (vulpea-db-update-file dest)
       ,@body)))

;;; The vulpea#469 scenario

(ert-deftest vulpea-db-claims-unlucky-refile-order-heals ()
  "Destination indexed before origin releases the id still wins it.

The exact ordering from vulpea#469: cut heading, paste into
destination, save destination, save origin.  The heading must end up
in the database at the destination path."
  (vulpea-db-claims-test--with-refile
    ;; Origin still holds the id on disk, so it keeps the row for now.
    (should (equal origin (vulpea-db-claims-test--owner "task-id")))
    ;; Origin is saved without the heading: the id is released and the
    ;; destination's claim resolves.
    (vulpea-db-claims-test--write
     origin vulpea-db-claims-test--origin-without-task)
    (vulpea-db-update-file origin)
    (should (equal dest (vulpea-db-claims-test--owner "task-id")))))

(ert-deftest vulpea-db-claims-lucky-refile-order-still-works ()
  "Origin saved before destination keeps working as before."
  (vulpea-test--with-temp-notes-dir
    (let ((origin (expand-file-name "origin.org" root))
          (dest (expand-file-name "destination.org" root)))
      (vulpea-db-claims-test--write
       origin vulpea-db-claims-test--origin-with-task)
      (vulpea-db-claims-test--write
       dest vulpea-db-claims-test--dest-without-task)
      (vulpea-db-update-file origin)
      (vulpea-db-update-file dest)
      ;; Refile with the working ordering: origin saved first.
      (vulpea-db-claims-test--write
       origin vulpea-db-claims-test--origin-without-task)
      (vulpea-db-update-file origin)
      (vulpea-db-claims-test--write
       dest vulpea-db-claims-test--dest-with-task)
      (vulpea-db-update-file dest)
      (should (equal dest (vulpea-db-claims-test--owner "task-id"))))))

(ert-deftest vulpea-db-claims-origin-deletion-releases-id ()
  "Deleting the origin file releases its ids to pending claimants."
  (vulpea-db-claims-test--with-refile
    (delete-file origin)
    (vulpea-db--forget-file origin)
    (should (equal dest (vulpea-db-claims-test--owner "task-id")))))

;;; Claim lifecycle

(ert-deftest vulpea-db-claims-recorded-for-losing-insert ()
  "An insert dropped by an id conflict leaves a pending claim behind."
  (vulpea-db-claims-test--with-refile
    (should (equal (list dest)
                   (vulpea-db--get-pending-claims "task-id")))))

(ert-deftest vulpea-db-claims-cleared-when-claimant-drops-id ()
  "Re-indexing a claimant without the id withdraws its claim."
  (vulpea-db-claims-test--with-refile
    ;; The heading is removed from the destination again before origin
    ;; releases the id.
    (vulpea-db-claims-test--write
     dest vulpea-db-claims-test--dest-without-task)
    (vulpea-db-update-file dest)
    (should-not (vulpea-db--get-pending-claims "task-id"))
    ;; Releasing the id now must not resurrect the heading anywhere.
    (vulpea-db-claims-test--write
     origin vulpea-db-claims-test--origin-without-task)
    (vulpea-db-update-file origin)
    (should-not (vulpea-db-claims-test--owner "task-id"))))

(ert-deftest vulpea-db-claims-dropped-when-claimant-file-gone ()
  "A claim from a since-deleted file is dropped instead of resolved."
  (vulpea-db-claims-test--with-refile
    (delete-file dest)
    (vulpea-db--forget-file dest)
    (vulpea-db-claims-test--write
     origin vulpea-db-claims-test--origin-without-task)
    (vulpea-db-update-file origin)
    ;; The heading is gone from disk everywhere, so it stays gone.
    (should-not (vulpea-db-claims-test--owner "task-id"))
    (should-not (vulpea-db--get-pending-claims "task-id"))
    ;; And the deleted file must not grow a change-detection row back.
    (should-not (vulpea-db--get-file-hash dest))))

(ert-deftest vulpea-db-claims-duplicate-id-stays-pending ()
  "A durable duplicate id keeps its claim without re-index churn.

When two files genuinely contain the same id, the loser's claim
stays recorded (visible to doctor), but the loser is still stamped
as indexed so scans do not re-parse it forever."
  (vulpea-db-claims-test--with-refile
    ;; Nobody releases the id: origin.org keeps the heading.
    (should (equal origin (vulpea-db-claims-test--owner "task-id")))
    (should (equal (list dest)
                   (vulpea-db--get-pending-claims "task-id")))
    ;; The claimant is stamped clean: smart detection skips it.
    (should (vulpea-db--get-file-hash dest))
    (should-not (vulpea-db-sync--update-file-if-changed dest))))

;;; Resolution under autosync

(ert-deftest vulpea-db-claims-release-enqueues-claimant-under-autosync ()
  "With autosync on, a released id enqueues the claimant with force."
  (vulpea-db-claims-test--with-refile
    (let ((vulpea-db-autosync-mode t)
          (vulpea-db-sync--queue nil)
          (vulpea-db-sync--queue-tail nil)
          (vulpea-db-sync--queue-set (make-hash-table :test 'equal))
          (vulpea-db-sync--force-set (make-hash-table :test 'equal))
          (vulpea-db-sync--timer nil))
      (vulpea-db-claims-test--write
       origin vulpea-db-claims-test--origin-without-task)
      (vulpea-db-update-file origin)
      ;; Not resolved synchronously - the id is released and the
      ;; claimant is queued for the sync machinery.
      (should-not (vulpea-db-claims-test--owner "task-id"))
      (let ((queued (vulpea-db-sync--configured-path
                     (vulpea-db-normalize-path dest))))
        (should (equal (list queued)
                       (mapcar #'car vulpea-db-sync--queue)))
        (should (gethash queued vulpea-db-sync--force-set)))
      ;; Processing the queue completes the move.
      (let ((vulpea-db-async-extraction nil)
            (vulpea-db-sync--processing nil))
        (vulpea-db-sync--process-queue))
      (should (equal dest (vulpea-db-claims-test--owner "task-id"))))))

;;; Manual update command

(ert-deftest vulpea-db-sync-update-file-enqueues-with-force ()
  "The manual per-file update command bypasses change detection.

A user reaching for `vulpea-db-sync-update-file' wants the file
actually re-read, so under autosync the entry carries the force
mark instead of being dropped by the unchanged-content check."
  (vulpea-test--with-temp-db
    (vulpea-db)
    (let ((path (vulpea-test--create-temp-org-file
                 ":PROPERTIES:\n:ID: force-id\n:END:\n#+TITLE: Force\n"))
          (vulpea-db-autosync-mode t)
          (vulpea-db-sync--queue nil)
          (vulpea-db-sync--queue-tail nil)
          (vulpea-db-sync--queue-set (make-hash-table :test 'equal))
          (vulpea-db-sync--force-set (make-hash-table :test 'equal))
          (vulpea-db-sync--timer nil))
      (unwind-protect
          (progn
            (vulpea-db-sync-update-file path)
            (let ((queued (vulpea-db-sync--configured-path
                           (vulpea-db-normalize-path path))))
              (should (equal (list queued)
                             (mapcar #'car vulpea-db-sync--queue)))
              (should (gethash queued vulpea-db-sync--force-set))))
        (delete-file path)))))

(provide 'vulpea-db-claims-test)
;;; vulpea-db-claims-test.el ends here
