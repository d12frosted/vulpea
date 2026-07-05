;;; vulpea-db-worker.el --- Async extraction worker -*- lexical-binding: t; -*-
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
;; Off-main-thread extraction for issue #359.
;;
;; The sync queue is scheduled asynchronously, but parsing and
;; extraction run on the main thread once a timer fires, freezing the
;; UI for as long as a file takes to index.  This module moves that
;; work into a persistent `emacs --batch' subprocess:
;;
;;   main                              worker
;;   ----                              ------
;;   spawn, send (settings ...)   -->  apply settings
;;   send (parse "path")          -->  vulpea-db--parse-file
;;                                <--  (begin "path")
;;                                <--  (file-node <plist>)
;;                                <--  (heading-node <plist>) ...
;;                                <--  (done "path" HASH MTIME SIZE)
;;   freshness check, then
;;   vulpea-db--apply-parse-ctx
;;
;; Only extracted data crosses the boundary - printable plists, never
;; the AST - so results flow through the exact same write path as
;; synchronous updates (`vulpea-db--apply-parse-ctx').  The wire
;; format is pure ASCII: everything is printed with
;; `print-escape-nonascii', so process coding can never corrupt it.
;; The worker's stderr goes to a separate buffer; in batch mode
;; `message' writes to stderr, keeping stdout clean for the protocol.
;;
;; The worker mirrors an explicit allowlist of settings (vulpea
;; customs, org link types, tag inheritance, org-attach
;; configuration).  Like `vulpea-db-parse-method' `single-temp-buffer',
;; it does not run the user's `org-mode-hook'.  Requests the worker
;; cannot handle faithfully - extractor plugins registered,
;; function-valued `vulpea-db-index-heading-level', non-.org files
;; that need decryption - are rejected by
;; `vulpea-db-worker-can-handle-p' and the caller falls back to the
;; synchronous path.
;;
;;; Code:

(require 'vulpea-db)
(require 'vulpea-db-extract)

(declare-function vulpea-db-sync--enqueue "vulpea-db-sync" (path))

;;; Customization

(defcustom vulpea-db-async-extraction nil
  "Whether the sync queue extracts files in a background process.

When t, `vulpea-db-sync' sends changed files to a persistent
`emacs --batch' worker instead of parsing them on the main thread.
The UI then only blocks for the database write of the results, not
for reading, hashing, parsing, or extraction - the bulk of indexing
time.

When `full', the worker also writes the results to the database
through its own connection, so the main thread only registers the
note IDs with org-id - the save-path freeze disappears at any file
size.  This switches the database to WAL journaling (persistent;
-wal/-shm sidecar files appear next to it), which is not suitable
for databases on network filesystems.  While
`vulpea-db-note-index-filter-functions' are registered (schema
validation actions), `full' behaves like t, since those filters run
in the main process.

The worker mirrors vulpea and org settings from an allowlist and,
like `vulpea-db-parse-method' \\='single-temp-buffer, does not run
`org-mode-hook'.  Files the worker cannot handle faithfully
\(extractor plugins registered, function-valued
`vulpea-db-index-heading-level', non-.org files) are processed
synchronously as before."
  :type '(choice (const :tag "Off (parse on the main thread)" nil)
          (const :tag "Extract in worker, write in main" t)
          (const :tag "Extract and write in worker (WAL)" full))
  :group 'vulpea-db-sync)

(defcustom vulpea-db-async-extraction-threshold nil
  "Minimum file size in bytes for extraction in the worker.

Files smaller than this are processed synchronously even when
`vulpea-db-async-extraction' is enabled; nil sends files of any size
to the worker.

Measurements do not demand a threshold - the worker blocks the main
thread less at every size, and bulk syncs complete faster through it
\(worker parsing overlaps writes in the main process).  The reason to set one
is consistency semantics: a synchronously indexed file is queryable
the moment the sync queue has run, while a worker-extracted file
becomes queryable a moment later.  If code queries small notes right
after saving them, a threshold like 102400 keeps those synchronous."
  :type '(choice (const :tag "All sizes through the worker" nil)
          (integer :tag "Minimum size in bytes"))
  :group 'vulpea-db-sync)

(defvar vulpea-db-worker-done-functions nil
  "Abnormal hook run when the worker finishes a file.

Each function is called with (PATH STATUS COUNT) where STATUS is one
of `applied' (notes written), `unchanged' (content hash matched, only
the file stamp was refreshed), `stale' (file changed while parsing;
result discarded and the file re-enqueued), `missing' (file deleted
while parsing), or `error'.  COUNT is the number of notes written for
`applied', nil otherwise.")

;;; Wire format

(defun vulpea-db-worker--print (form)
  "Return FORM printed for the wire: one ASCII-only line."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-length nil)
        (print-level nil)
        (print-circle nil))
    (prin1-to-string form)))

(defun vulpea-db-worker--printable-p (value)
  "Return non-nil when VALUE survives a print/read round trip."
  (condition-case nil
      (equal value (car (read-from-string (vulpea-db-worker--print value))))
    (error nil)))

;;; Settings allowlist

(defconst vulpea-db-worker--settings-vars
  '(vulpea-db-parse-method
    vulpea-db-parse-granularity
    vulpea-db-index-plain-links
    vulpea-db-index-heading-level
    vulpea-db-exclude-archived
    vulpea-db-exclude-property
    org-archive-tag
    org-use-tag-inheritance
    org-tags-exclude-from-inheritance
    org-use-property-inheritance
    org-attach-id-dir
    org-attach-id-to-path-function-list
    org-todo-keywords
    org-plain-list-ordered-item-terminator
    org-list-allow-alphabetical)
  "Variables mirrored into the extraction worker.

Grow this list when extraction starts depending on new
configuration; the async-vs-sync equivalence test is the safety
net.")

(defun vulpea-db-worker--settings-form ()
  "Build the settings message for the worker.

Unprintable values (closures, buffers) are silently dropped; the
worker keeps its default for those variables.  Callers that need a
faithful result for such settings must use
`vulpea-db-worker-can-handle-p', which rejects the known-critical
cases."
  (let (vars)
    (dolist (sym vulpea-db-worker--settings-vars)
      (when (boundp sym)
        (let ((value (symbol-value sym)))
          (when (vulpea-db-worker--printable-p value)
            (push (cons sym value) vars)))))
    `(settings ,(nreverse vars) ,(org-link-types))))

;;; Client: process management

(defvar vulpea-db-worker--process nil
  "The extraction worker process, or nil.")

(defvar vulpea-db-worker--output ""
  "Buffered incomplete output line from the worker.")

(defvar vulpea-db-worker--in-flight nil
  "Paths sent to the worker and not yet completed, oldest first.")

(defvar vulpea-db-worker--in-flight-tail nil
  "Tail cons of `vulpea-db-worker--in-flight' for O(1) appends.")

(defvar vulpea-db-worker--current nil
  "Assembly state for the file currently streaming in.
A plist with :path, :file-node and :heading-nodes (reversed).")

(defun vulpea-db-worker--command ()
  "Build the worker process command line."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (lib (locate-library "vulpea-db-worker")))
    (append (list emacs "--batch" "-Q")
            (mapcan (lambda (dir) (list "-L" dir))
                    (seq-filter #'stringp load-path))
            (list "-l" lib "-f" "vulpea-db-worker-batch-main"))))

(defun vulpea-db-worker--ensure ()
  "Return a live worker process, spawning one if needed."
  (unless (process-live-p vulpea-db-worker--process)
    (setq vulpea-db-worker--output ""
          vulpea-db-worker--in-flight nil
          vulpea-db-worker--in-flight-tail nil
          vulpea-db-worker--current nil)
    (setq vulpea-db-worker--process
          (make-process
           :name "vulpea-worker"
           :command (vulpea-db-worker--command)
           :connection-type 'pipe
           :noquery t
           :coding 'utf-8-unix
           :stderr (get-buffer-create " *vulpea-worker-stderr*")
           :filter #'vulpea-db-worker--filter
           :sentinel #'vulpea-db-worker--sentinel))
    (vulpea-db-worker--send (vulpea-db-worker--settings-form)))
  vulpea-db-worker--process)

(defun vulpea-db-worker--send (form)
  "Send FORM to the worker as one line."
  (process-send-string vulpea-db-worker--process
                       (concat (vulpea-db-worker--print form) "\n")))

(defun vulpea-db-worker-stop ()
  "Stop the extraction worker, discarding any in-flight work."
  (when (process-live-p vulpea-db-worker--process)
    ;; Suppress the sentinel's re-enqueueing: an explicit stop
    ;; means the caller does not want the work back.
    (set-process-sentinel vulpea-db-worker--process #'ignore)
    (delete-process vulpea-db-worker--process))
  (setq vulpea-db-worker--process nil
        vulpea-db-worker--output ""
        vulpea-db-worker--in-flight nil
        vulpea-db-worker--in-flight-tail nil
        vulpea-db-worker--current nil))

(defun vulpea-db-worker-busy-p ()
  "Return non-nil while the worker has unfinished requests."
  (and vulpea-db-worker--in-flight t))

(defun vulpea-db-worker-can-handle-p (path)
  "Return non-nil when PATH can be extracted in the worker faithfully.

Rejects the cases where a subprocess cannot replicate in-process
behavior: extractor plugins are registered (they receive the AST,
which never crosses the process boundary), heading-level indexing is
a predicate function (not serializable), or PATH is not a plain .org
file (decryption may require user interaction)."
  (and (null vulpea-db--extractors)
       (booleanp vulpea-db-index-heading-level)
       (string-suffix-p ".org" path)))

(defun vulpea-db-worker-should-handle-p (path)
  "Return non-nil when PATH should be extracted in the worker.

Combines `vulpea-db-worker-can-handle-p' (faithfulness) with
`vulpea-db-async-extraction-threshold' (size routing policy)."
  (and (vulpea-db-worker-can-handle-p path)
       (or (null vulpea-db-async-extraction-threshold)
           (let ((attrs (file-attributes path)))
             (and attrs
                  (>= (file-attribute-size attrs)
                      vulpea-db-async-extraction-threshold))))))

(defun vulpea-db-worker--filters-inert-p ()
  "Return non-nil when index filters cannot affect indexing.

`vulpea-db-note-index-filter-functions' run in the main process, so
full-write mode is only faithful while they can make no difference.
The schema-validation filter installs itself unconditionally at
load, but fast-exits allowing everything unless schemas are
registered with a non-silent action - that state is inert.  Any
other filter, or active non-silent schema validation, is not: even
the `warning' action must run in the main process to be seen."
  (let ((filters vulpea-db-note-index-filter-functions))
    (or (null filters)
        (and (equal filters '(vulpea-db-schema-validation--filter))
             (or (eq (bound-and-true-p vulpea-db-schema-validation-action)
                     'silent)
                 (not (and (fboundp 'vulpea-schema-list)
                           (vulpea-schema-list))))))))

(defun vulpea-db-worker--full-write-p ()
  "Return non-nil when the worker should write results itself.

Requires `vulpea-db-async-extraction' to be `full' and inert
`vulpea-db-note-index-filter-functions' (see
`vulpea-db-worker--filters-inert-p'); active filters live in the
main process, so their presence degrades `full' to extract-only."
  (and (eq vulpea-db-async-extraction 'full)
       (vulpea-db-worker--filters-inert-p)))

(defun vulpea-db-worker--enable-wal (connection)
  "Enable WAL journaling and a busy timeout on CONNECTION.

WAL lets the worker write while the main process reads (and vice
versa); the busy timeout makes concurrent write transactions wait
for each other instead of failing."
  (let ((handle (oref connection handle)))
    (sqlite-pragma handle "busy_timeout=5000")
    (sqlite-pragma handle "journal_mode=WAL")))

(defun vulpea-db-worker-request (path)
  "Ask the worker to extract PATH.

The result is applied to the database when it arrives; see
`vulpea-db-worker-done-functions'.  Callers should check
`vulpea-db-worker-can-handle-p' first."
  (vulpea-db-worker--ensure)
  (if (vulpea-db-worker--full-write-p)
      (progn
        (vulpea-db-worker--enable-wal (vulpea-db))
        (vulpea-db-worker--send
         `(parse-and-write ,path ,(expand-file-name vulpea-db-location))))
    (vulpea-db-worker--send `(parse ,path)))
  (let ((node (list path)))
    (if vulpea-db-worker--in-flight
        (setcdr vulpea-db-worker--in-flight-tail node)
      (setq vulpea-db-worker--in-flight node))
    (setq vulpea-db-worker--in-flight-tail node)))

;;; Client: response handling

(defun vulpea-db-worker--filter (_proc output)
  "Process protocol OUTPUT lines from the worker."
  (setq output (concat vulpea-db-worker--output output))
  (let* ((ends-with-newline (string-suffix-p "\n" output))
         (lines (split-string output "\n" t)))
    (if ends-with-newline
        (setq vulpea-db-worker--output "")
      (setq vulpea-db-worker--output (or (car (last lines)) ""))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (when (string-prefix-p "(" line)
        (let ((msg (condition-case nil
                       (car (read-from-string line))
                     (error nil))))
          (when msg
            (vulpea-db-worker--dispatch msg)))))))

(defun vulpea-db-worker--dispatch (msg)
  "Handle one protocol MSG from the worker."
  (pcase msg
    (`(begin ,path)
     (setq vulpea-db-worker--current (list :path path)))
    (`(file-node ,data)
     (setq vulpea-db-worker--current
           (plist-put vulpea-db-worker--current :file-node data)))
    (`(heading-node ,data)
     (setq vulpea-db-worker--current
           (plist-put vulpea-db-worker--current :heading-nodes
                      (cons data (plist-get vulpea-db-worker--current
                                            :heading-nodes)))))
    (`(done ,path ,hash ,mtime ,size)
     (let ((current vulpea-db-worker--current))
       (setq vulpea-db-worker--current nil)
       (vulpea-db-worker--forget path)
       (vulpea-db-worker--complete path hash mtime size current)))
    ;; Full-write mode: the worker wrote the database itself; the
    ;; main process only registers org-ids and re-checks freshness.
    (`(written ,path ,_hash ,mtime ,size ,count ,ids)
     (vulpea-db-worker--forget path)
     (vulpea-db--register-id-locations ids path)
     (let ((attrs (file-attributes path)))
       (when (and attrs
                  (or (not (equal (float-time
                                   (file-attribute-modification-time attrs))
                                  mtime))
                      (not (equal (file-attribute-size attrs) size))))
         ;; Changed again while the worker was writing: what landed
         ;; reflects older content, re-parse to catch up.
         (vulpea-db-worker--reenqueue path)))
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'applied count))
    (`(stamped ,path)
     (vulpea-db-worker--forget path)
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'unchanged nil))
    (`(stale ,path)
     (vulpea-db-worker--forget path)
     (vulpea-db-worker--reenqueue path)
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'stale nil))
    (`(error ,path ,message)
     (setq vulpea-db-worker--current nil)
     (vulpea-db-worker--forget path)
     (message "Vulpea: worker failed on %s: %s" path message)
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'error nil))))

(defun vulpea-db-worker--reenqueue (path)
  "Schedule PATH for another pass, via the sync queue when active."
  (if (and (bound-and-true-p vulpea-db-autosync-mode)
           (fboundp 'vulpea-db-sync--enqueue))
      (vulpea-db-sync--enqueue path)
    (vulpea-db-worker-request path)))

(defun vulpea-db-worker--forget (path)
  "Drop PATH from the in-flight list.
The worker answers in request order, so PATH is almost always the
head; falling back to a full scan keeps this correct either way."
  (if (equal (car vulpea-db-worker--in-flight) path)
      (pop vulpea-db-worker--in-flight)
    (setq vulpea-db-worker--in-flight
          (delete path vulpea-db-worker--in-flight)))
  (setq vulpea-db-worker--in-flight-tail
        (last vulpea-db-worker--in-flight)))

(defun vulpea-db-worker--complete (path hash mtime size current)
  "Apply a completed extraction of PATH to the database.

HASH, MTIME and SIZE describe the file as the worker read it;
CURRENT carries the extracted nodes.  Stale results - the file
changed or disappeared while the worker was parsing - are discarded,
and changed files are re-enqueued with the sync queue."
  (let ((attrs (file-attributes path)))
    (cond
     ;; File vanished while parsing: the deletion event handles the
     ;; database; nothing to apply.
     ((null attrs)
      (run-hook-with-args 'vulpea-db-worker-done-functions
                          path 'missing nil))
     ;; File changed while parsing: discard and re-parse.
     ((or (not (equal (float-time (file-attribute-modification-time attrs))
                      mtime))
          (not (equal (file-attribute-size attrs) size)))
      (vulpea-db-worker--reenqueue path)
      (run-hook-with-args 'vulpea-db-worker-done-functions
                          path 'stale nil))
     ;; Content identical to what is already indexed: refresh the
     ;; stored stamp so the cheap mtime/size check passes next time.
     ((equal (plist-get (vulpea-db--get-file-hash path) :hash) hash)
      (vulpea-db--update-file-hash path hash mtime size)
      (run-hook-with-args 'vulpea-db-worker-done-functions
                          path 'unchanged nil))
     (t
      (let* ((ctx (make-vulpea-parse-ctx
                   :path path
                   :ast nil
                   :file-node (plist-get current :file-node)
                   :heading-nodes (nreverse
                                   (plist-get current :heading-nodes))
                   :hash hash
                   :mtime mtime
                   :size size))
             (count (vulpea-db--apply-parse-ctx ctx)))
        (run-hook-with-args 'vulpea-db-worker-done-functions
                            path 'applied count))))))

(defun vulpea-db-worker--sentinel (proc _event)
  "Handle worker PROC death: re-enqueue in-flight work."
  (unless (process-live-p proc)
    (let ((pending vulpea-db-worker--in-flight))
      (setq vulpea-db-worker--process nil
            vulpea-db-worker--output ""
            vulpea-db-worker--in-flight nil
            vulpea-db-worker--in-flight-tail nil
            vulpea-db-worker--current nil)
      (when pending
        (message "Vulpea: extraction worker died, re-queueing %d file%s"
                 (length pending) (if (= (length pending) 1) "" "s"))
        (dolist (path pending)
          (vulpea-db-worker--reenqueue path))))))

;;; Worker side (runs in emacs --batch)

(defun vulpea-db-worker--reply (form)
  "Print FORM as one protocol line on stdout."
  (princ (vulpea-db-worker--print form))
  (princ "\n"))

(defun vulpea-db-worker--apply-settings (vars link-types)
  "Set allowlisted VARS and register LINK-TYPES in this worker."
  (pcase-dolist (`(,sym . ,value) vars)
    (set sym value))
  (require 'ol)
  (dolist (type link-types)
    (unless (assoc type org-link-parameters)
      (org-link-set-parameters type)))
  (org-link-make-regexps)
  (when (fboundp 'org-element-update-syntax)
    (org-element-update-syntax)))

(defvar vulpea-db-worker--db-location nil
  "Database location this worker currently has open, or nil.")

(defun vulpea-db-worker--ensure-db (db)
  "Open (or reuse) a connection to the database at DB in this worker.
Enables WAL journaling and a busy timeout so writes interleave
safely with the main process."
  (unless (equal db vulpea-db-worker--db-location)
    (when vulpea-db--connection
      (vulpea-db-close))
    (setq vulpea-db-location db
          vulpea-db-worker--db-location db))
  (vulpea-db-worker--enable-wal (vulpea-db)))

(defun vulpea-db-worker--ctx-ids (ctx)
  "Return the note IDs carried by CTX.
Only used in full-write mode, where no index filters are active, so
every extracted note is written."
  (let (ids)
    (when-let* ((id (plist-get (vulpea-parse-ctx-file-node ctx) :id)))
      (push id ids))
    (dolist (node (vulpea-parse-ctx-heading-nodes ctx))
      (when-let* ((id (plist-get node :id)))
        (push id ids)))
    (nreverse ids)))

(defun vulpea-db-worker--apply-guarded (ctx stored)
  "Apply CTX unless the stored file stamp moved from STORED.

Compare-and-swap for full-write mode: the main process may have
re-indexed the same file (a programmatic `vulpea-db-update-file',
e.g. from `vulpea-meta-set') between this worker's checks and its
write.  Re-reading the stored stamp inside the write transaction
detects that; a moved stamp means our result is outdated and must
not clobber the newer data.  A transaction that fails to commit
under write contention counts as a conflict too - retrying via the
queue is always safe.

Returns the number of notes written, or `conflict'."
  (condition-case nil
      (emacsql-with-transaction (vulpea-db)
        (if (not (equal (vulpea-db--get-file-hash
                         (vulpea-parse-ctx-path ctx))
                        stored))
            'conflict
          (vulpea-db--apply-parse-ctx ctx 'skip-org-id)))
    (error 'conflict)))

(defun vulpea-db-worker--handle-parse-and-write (path db)
  "Extract PATH and write the results to the database at DB.

Full-write mode: this worker owns the database write; the reply
tells the main process what happened so it can register org-ids
\(`written'), note the no-op (`stamped'), or re-queue a file whose
result became outdated (`stale') - because the file changed
mid-parse, or because the main process indexed newer content
concurrently (see `vulpea-db-worker--apply-guarded')."
  (condition-case err
      (progn
        (vulpea-db-worker--ensure-db db)
        (let* ((stored (vulpea-db--get-file-hash path))
               (ctx (vulpea-db--parse-file path))
               (attrs (file-attributes path)))
          (cond
           ;; File changed or vanished while parsing: the result
           ;; does not represent the file anymore
           ((or (null attrs)
                (not (equal (float-time
                             (file-attribute-modification-time attrs))
                            (vulpea-parse-ctx-mtime ctx)))
                (not (equal (file-attribute-size attrs)
                            (vulpea-parse-ctx-size ctx))))
            (vulpea-db-worker--reply `(stale ,path)))
           ;; Content identical to what is indexed: refresh the stamp
           ((equal (plist-get stored :hash)
                   (vulpea-parse-ctx-hash ctx))
            (vulpea-db--update-file-hash path
                                         (vulpea-parse-ctx-hash ctx)
                                         (vulpea-parse-ctx-mtime ctx)
                                         (vulpea-parse-ctx-size ctx))
            (vulpea-db-worker--reply `(stamped ,path)))
           (t
            (let ((count (vulpea-db-worker--apply-guarded ctx stored)))
              (if (eq count 'conflict)
                  (vulpea-db-worker--reply `(stale ,path))
                (vulpea-db-worker--reply
                 `(written ,path
                           ,(vulpea-parse-ctx-hash ctx)
                           ,(vulpea-parse-ctx-mtime ctx)
                           ,(vulpea-parse-ctx-size ctx)
                           ,count
                           ,(vulpea-db-worker--ctx-ids ctx)))))))))
    (error
     (vulpea-db-worker--reply
      `(error ,path ,(error-message-string err))))))

(defun vulpea-db-worker--handle-parse (path)
  "Extract PATH and stream the results to stdout."
  (condition-case err
      (let ((ctx (vulpea-db--parse-file path)))
        (vulpea-db-worker--reply `(begin ,path))
        (vulpea-db-worker--reply
         `(file-node ,(vulpea-parse-ctx-file-node ctx)))
        (dolist (node (vulpea-parse-ctx-heading-nodes ctx))
          (vulpea-db-worker--reply `(heading-node ,node)))
        (vulpea-db-worker--reply
         `(done ,path
                ,(vulpea-parse-ctx-hash ctx)
                ,(vulpea-parse-ctx-mtime ctx)
                ,(vulpea-parse-ctx-size ctx))))
    (error
     (vulpea-db-worker--reply
      `(error ,path ,(error-message-string err))))))

(defun vulpea-db-worker-batch-main ()
  "Protocol loop for the extraction worker.

Runs in `emacs --batch': reads one request per line from stdin,
writes protocol lines to stdout.  Exits when stdin closes."
  (require 'org-attach)
  (while t
    (let* ((line (condition-case nil
                     (read-from-minibuffer "")
                   (error (kill-emacs 0))))
           (msg (condition-case nil
                    (car (read-from-string line))
                  (error nil))))
      (pcase msg
        (`(settings ,vars ,link-types)
         (vulpea-db-worker--apply-settings vars link-types))
        (`(parse ,path)
         (vulpea-db-worker--handle-parse path))
        (`(parse-and-write ,path ,db)
         (vulpea-db-worker--handle-parse-and-write path db))
        (_ nil)))))

(provide 'vulpea-db-worker)
;;; vulpea-db-worker.el ends here
