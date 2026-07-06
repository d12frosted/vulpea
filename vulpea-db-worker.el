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

(declare-function vulpea-db-sync--enqueue "vulpea-db-sync"
                  (path &optional force))

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

(defcustom vulpea-db-worker-debug nil
  "When non-nil, log worker lifecycle and protocol to a buffer.

The log lands in `vulpea-db-worker-log-buffer' with millisecond
timestamps: process spawns (including the full command), settings
sent, every request and reply (truncated), completion outcomes with
time spent in the main process, process deaths with the tail of the worker's
stderr, and WAL negotiation results.  Enable it when async extraction
misbehaves and attach the buffer to a bug report."
  :type 'boolean
  :group 'vulpea-db-sync)

(defconst vulpea-db-worker-log-buffer "*vulpea-worker-log*"
  "Name of the worker debug log buffer.")

(defun vulpea-db-worker--log (format-string &rest args)
  "Append a timestamped line to the worker log when debugging.
FORMAT-STRING and ARGS are passed to `format'."
  (when vulpea-db-worker-debug
    (with-current-buffer (get-buffer-create vulpea-db-worker-log-buffer)
      (goto-char (point-max))
      (insert (format-time-string "%H:%M:%S.%3N ")
              (apply #'format format-string args)
              "\n"))))

(defun vulpea-db-worker--log-truncate (string &optional limit)
  "Return STRING truncated to LIMIT characters (default 180) for logs."
  (let ((limit (or limit 180)))
    (if (> (length string) limit)
        (concat (substring string 0 limit)
                (format "... [%d chars]" (length string)))
      string)))

(defvar vulpea-db-worker-done-functions nil
  "Abnormal hook run when the worker finishes a file.

Each function is called with (PATH STATUS COUNT) where STATUS is one
of `applied' (notes written), `unchanged' (content hash matched, only
the file stamp was refreshed), `stale' (file changed while parsing;
result discarded and the file re-enqueued), `requeued' (worker died
with the file in flight; re-enqueued), `missing' (file deleted while
parsing), or `error'.  COUNT is the number of notes written for
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
    `(settings ,(nreverse vars) ,(org-link-types)
               ,(vulpea-db-worker--extractor-specs)
               (:db-version ,vulpea-db-version
                :parser-epoch ,vulpea-db-parser-epoch))))

(defun vulpea-db-worker--extractor-specs ()
  "Build wire specs for worker-safe extractors.

Only extractors declared :worker-safe with a symbol :extract-fn can
cross the process boundary; the worker loads :worker-lib and
registers them locally.  Anything it cannot resolve makes it fall
back to streaming results, so the extractor runs in the main process
instead."
  (let (specs)
    (dolist (extractor vulpea-db--extractors)
      (when (vulpea-extractor-worker-safe extractor)
        (push (list :name (vulpea-extractor-name extractor)
                    :fn (let ((fn (vulpea-extractor-extract-fn extractor)))
                          (and (symbolp fn) fn))
                    :lib (vulpea-extractor-worker-lib extractor)
                    :priority (vulpea-extractor-priority extractor)
                    :requires-ast (vulpea-extractor-requires-ast extractor))
              specs)))
    (nreverse specs)))

(defvar vulpea-db-worker--process)

(defun vulpea-db-worker-refresh-settings ()
  "Send current settings to a running worker, if any.
Called by `vulpea-db-register-extractor' so a live worker mirrors
the extractor registry."
  (when (process-live-p vulpea-db-worker--process)
    (vulpea-db-worker--log "settings refresh")
    (vulpea-db-worker--send (vulpea-db-worker--settings-form))))

;;; Client: process management

(defvar vulpea-db-worker--process nil
  "The extraction worker process, or nil.")

(defvar vulpea-db-worker--output ""
  "Buffered incomplete output line from the worker.")

(defvar vulpea-db-worker--output-pending nil
  "Reversed list of chunks of the current incomplete protocol line.
Accumulated as a list instead of string concatenation: a single
protocol line can be megabytes (a large file node), and re-concating
it per 4KB process chunk would cost quadratic time in the main process.")

(defvar vulpea-db-worker--wal-failed nil
  "Non-nil when WAL journaling could not be enabled on the database.
Without WAL, a worker write transaction blocks main-process reads -
the exact freeze full-write mode exists to avoid - so full-write is
refused and requests degrade to extract-only (see
`vulpea-db-worker--full-write-p').  Happens on filesystems without
shared-memory support (some network or FUSE mounts).")

(defvar vulpea-db-worker--in-flight nil
  "Paths sent to the worker and not yet completed, oldest first.")

(defvar vulpea-db-worker--in-flight-tail nil
  "Tail cons of `vulpea-db-worker--in-flight' for O(1) appends.")

(defcustom vulpea-db-worker-hang-timeout 300
  "Seconds of worker silence with work in flight before it is killed.

A hung parse (pathological file, wedged subprocess) would otherwise
stall all background indexing forever while the queue waits on the
flow-control window.  When the worker produces no output for this
long despite having requests in flight, it is killed; the salvage
path re-enqueues its files.  Two consecutive hang kills without a
successful completion in between mark the worker broken (see
`vulpea-db-worker-reset'), so a file that reliably hangs the parser
cannot loop forever.

The default is generous: a 100MB file parses in under a minute on
2020s hardware.  Set to nil to disable the watchdog."
  :type '(choice (const :tag "Disabled" nil) (integer :tag "Seconds"))
  :group 'vulpea-db-sync)

(defcustom vulpea-db-worker-max-in-flight 128
  "Maximum requests in flight to the worker at once.

Flow control for bulk syncs: without a window, the queue stuffs the
worker's stdin pipe until `process-send-string' blocks the main
thread waiting for the worker to drain it - measured as multi-hundred
millisecond UI stalls during full rebuilds.  The sync queue keeps the
overflow queued and retries as completions free the window."
  :type 'integer
  :group 'vulpea-db-sync)

(defvar vulpea-db-worker--in-flight-count 0
  "Length of `vulpea-db-worker--in-flight', maintained incrementally.")

(defun vulpea-db-worker-in-flight-count ()
  "Return the number of requests currently in flight."
  vulpea-db-worker--in-flight-count)

(defun vulpea-db-worker-saturated-p ()
  "Return non-nil when the worker request window is full.
Callers should keep their files queued and retry later instead of
requesting more (see `vulpea-db-worker-max-in-flight')."
  (>= vulpea-db-worker--in-flight-count vulpea-db-worker-max-in-flight))

(defvar vulpea-db-worker--current nil
  "Assembly state for the file currently streaming in.
A plist with :path, :file-node and :heading-nodes (reversed).")

(defvar vulpea-db-worker--last-activity nil
  "Timestamp of the last output received from the worker.")

(defvar vulpea-db-worker--watchdog-timer nil
  "Repeating timer that detects a hung worker.")

(defvar vulpea-db-worker--hang-kills 0
  "Consecutive watchdog kills without a successful completion.")

(defvar vulpea-db-worker--extractors-warned nil
  "Non-nil after warning about worker-unresolvable extractors once.")

(defvar vulpea-db-worker--spawn-time nil
  "Timestamp of the most recent worker spawn.")

(defvar vulpea-db-worker--crash-times nil
  "Timestamps of recent early worker deaths (died within 10s of spawn).
Used to detect crash loops: a worker that keeps dying right after
spawning would otherwise be respawned in a tight loop that starves
the UI.")

(defvar vulpea-db-worker--broken nil
  "Non-nil after a worker crash loop was detected.
While set, `vulpea-db-worker-can-handle-p' refuses everything, so the
sync queue falls back to synchronous processing.  Reset with
`vulpea-db-worker-reset' (or by re-enabling `vulpea-db-autosync-mode',
which stops the worker).")

(defun vulpea-db-worker-reset ()
  "Forget a detected crash loop and allow the worker again."
  (interactive)
  (setq vulpea-db-worker--broken nil
        vulpea-db-worker--crash-times nil)
  (message "Vulpea: extraction worker re-enabled"))

(defvar vulpea-db-worker--force (make-hash-table :test 'equal)
  "In-flight paths requested with force re-indexing.

Forced results skip the unchanged-content shortcut: force exists for
parser or settings changes, where content is identical but extraction
output is not.")

(defun vulpea-db-worker--command ()
  "Build the worker process command line."
  (let ((emacs (expand-file-name invocation-name invocation-directory))
        (lib (locate-library "vulpea-db-worker")))
    (unless lib
      (error "Cannot locate vulpea-db-worker library for the worker process"))
    (append (list emacs "--batch" "-Q")
            (mapcan (lambda (dir) (list "-L" dir))
                    (seq-filter #'stringp load-path))
            (list "-l" lib "-f" "vulpea-db-worker-batch-main"))))

(defun vulpea-db-worker--salvage ()
  "Re-enqueue in-flight work of a dead worker and reset client state.

Emacs reports a child's death through `process-live-p' immediately,
but runs its sentinel only at the next wait point - and timers run
first.  Any code that observes a dead worker before its sentinel has
run must salvage through this function; the sentinel itself also
lands here.  Idempotent: the second caller finds no pending work."
  (let ((pending vulpea-db-worker--in-flight)
        (forced (copy-hash-table vulpea-db-worker--force)))
    (when vulpea-db-worker--process
      ;; The deferred sentinel of this process must not run its own
      ;; salvage against the replacement's state later
      (set-process-sentinel vulpea-db-worker--process #'ignore))
    (setq vulpea-db-worker--process nil
          vulpea-db-worker--output ""
          vulpea-db-worker--output-pending nil
          vulpea-db-worker--in-flight nil
          vulpea-db-worker--in-flight-tail nil
          vulpea-db-worker--in-flight-count 0
          vulpea-db-worker--current nil)
    (clrhash vulpea-db-worker--force)
    (when pending
      (message "Vulpea: extraction worker died, re-queueing %d file%s"
               (length pending) (if (= (length pending) 1) "" "s"))
      ;; Deferred: salvage may run inside vulpea-db-worker--ensure,
      ;; and re-requesting synchronously would recurse into the spawn
      ;; path and orphan the process being created
      (run-at-time 0 nil #'vulpea-db-worker--salvage-requeue
                   pending forced))))

(defun vulpea-db-worker--salvage-requeue (pending forced)
  "Re-enqueue salvaged PENDING paths with their FORCED marks."
  (dolist (path pending)
    (vulpea-db-worker--reenqueue path (gethash path forced))
    ;; Terminal for this dispatch: the re-enqueued entry counts
    ;; anew when the queue dispatches it again
    (run-hook-with-args 'vulpea-db-worker-done-functions
                        path 'requeued nil)))

(defun vulpea-db-worker--watchdog ()
  "Kill the worker when it has been silent too long with work pending."
  (when (and vulpea-db-worker-hang-timeout
             vulpea-db-worker--in-flight
             (process-live-p vulpea-db-worker--process)
             vulpea-db-worker--last-activity
             (> (- (float-time) vulpea-db-worker--last-activity)
                vulpea-db-worker-hang-timeout))
    (setq vulpea-db-worker--hang-kills (1+ vulpea-db-worker--hang-kills))
    (vulpea-db-worker--log "watchdog: killing silent worker (%d in flight, hang #%d)"
                           vulpea-db-worker--in-flight-count
                           vulpea-db-worker--hang-kills)
    (message "Vulpea: extraction worker unresponsive for %ss, restarting"
             vulpea-db-worker-hang-timeout)
    (when (>= vulpea-db-worker--hang-kills 2)
      ;; The retry hung too: something reliably wedges the parser.
      ;; Stop feeding it; the sync queue falls back to synchronous.
      (setq vulpea-db-worker--broken t)
      (display-warning
       'vulpea
       "Extraction worker hangs repeatedly; falling back to synchronous indexing.  Run M-x vulpea-db-worker-reset to retry."
       :error))
    ;; delete-process triggers the sentinel -> salvage re-enqueues
    (delete-process vulpea-db-worker--process)))

(defun vulpea-db-worker--ensure ()
  "Return a live worker process, spawning one if needed."
  (unless (process-live-p vulpea-db-worker--process)
    ;; A dead-but-not-yet-sentineled worker still owns in-flight work;
    ;; salvage it instead of silently discarding it
    (vulpea-db-worker--salvage)
    (setq vulpea-db-worker--spawn-time (float-time))
    (setq vulpea-db-worker--last-activity (float-time))
    (unless vulpea-db-worker--watchdog-timer
      (setq vulpea-db-worker--watchdog-timer
            (run-with-timer 30 30 #'vulpea-db-worker--watchdog)))
    (let ((command (vulpea-db-worker--command)))
      (vulpea-db-worker--log "spawn: %s ... (%d args)"
                             (vulpea-db-worker--log-truncate
                              (string-join (seq-take command 6) " "))
                             (length command))
      (setq vulpea-db-worker--process
            (make-process
             :name "vulpea-worker"
             :command command
             :connection-type 'pipe
             :noquery t
             :coding 'utf-8-unix
             :stderr (get-buffer-create " *vulpea-worker-stderr*")
             :filter #'vulpea-db-worker--filter
             :sentinel #'vulpea-db-worker--sentinel)))
    (let ((settings (vulpea-db-worker--settings-form)))
      (vulpea-db-worker--log "settings: %d vars, %d link types"
                             (length (nth 1 settings))
                             (length (nth 2 settings)))
      (vulpea-db-worker--send settings)))
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
  (when vulpea-db-worker--watchdog-timer
    (cancel-timer vulpea-db-worker--watchdog-timer)
    (setq vulpea-db-worker--watchdog-timer nil))
  (setq vulpea-db-worker--process nil
        vulpea-db-worker--output ""
        vulpea-db-worker--output-pending nil
        vulpea-db-worker--in-flight nil
        vulpea-db-worker--in-flight-tail nil
        vulpea-db-worker--in-flight-count 0
        vulpea-db-worker--current nil)
  (clrhash vulpea-db-worker--force))

(defun vulpea-db-worker-busy-p ()
  "Return non-nil while the worker has unfinished requests."
  (and vulpea-db-worker--in-flight t))

(defun vulpea-db-worker-rejection-reasons (path)
  "Return the reasons PATH cannot be extracted in the worker, if any.

A list of symbols, nil when the worker can handle PATH faithfully:
- `broken': the worker crash-looped (see `vulpea-db-worker-reset')
- `ast-extractors': registered extractors read the AST, which never
  crosses the process boundary (extractors declaring :requires-ast
  nil are fine - they run in the main process during apply)
- `heading-level-predicate': `vulpea-db-index-heading-level' is a
  function, which is not serializable
- `extension': PATH is not a plain .org file (decryption may require
  user interaction)"
  (let (reasons)
    (when vulpea-db-worker--broken
      (push 'broken reasons))
    (when (seq-some #'vulpea-extractor-requires-ast vulpea-db--extractors)
      (push 'ast-extractors reasons))
    (unless (booleanp vulpea-db-index-heading-level)
      (push 'heading-level-predicate reasons))
    (unless (string-suffix-p ".org" path)
      (push 'extension reasons))
    (nreverse reasons)))

(defun vulpea-db-worker-can-handle-p (path)
  "Return non-nil when PATH can be extracted in the worker faithfully.
See `vulpea-db-worker-rejection-reasons' for what disqualifies a
file."
  (null (vulpea-db-worker-rejection-reasons path)))

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
       (not vulpea-db-worker--wal-failed)
       ;; Extractors run during apply; only ones declared :worker-safe
       ;; can do that inside the worker.  If the worker turns out
       ;; unable to resolve one, it falls back to streaming per file.
       (seq-every-p #'vulpea-extractor-worker-safe vulpea-db--extractors)
       (vulpea-db-worker--filters-inert-p)))

(defvar vulpea-db-worker--wal-connection nil
  "Connection object whose WAL pragmas were already applied.
The pragmas contend with the worker's write transactions, so running
them once per request stalls the main thread during bulk syncs; once
per connection is enough (WAL persists in the database, the busy
timeout persists on the connection).")

(defun vulpea-db-worker--wal-ready-p ()
  "Ensure WAL pragmas on the current connection, once."
  (let ((connection (vulpea-db)))
    (if (eq connection vulpea-db-worker--wal-connection)
        t
      (when (vulpea-db-worker--enable-wal connection)
        (setq vulpea-db-worker--wal-connection connection)
        t))))

(defun vulpea-db-worker--enable-wal (connection)
  "Enable WAL journaling and a busy timeout on CONNECTION.

WAL lets the worker write while the main process reads (and vice
versa); the busy timeout makes concurrent write transactions wait
for each other instead of failing.  Returns non-nil when the
database is in WAL mode afterwards; a failure (filesystem without
shared-memory support) marks `vulpea-db-worker--wal-failed'."
  (let ((handle (oref connection handle)))
    (sqlite-pragma handle "busy_timeout=5000")
    (sqlite-pragma handle "journal_mode=WAL")
    (let ((mode (caar (sqlite-select handle "PRAGMA journal_mode"))))
      (if (equal mode "wal")
          t
        (unless vulpea-db-worker--wal-failed
          (setq vulpea-db-worker--wal-failed t)
          (vulpea-db-worker--log "WAL refused, journal_mode=%s" mode)
          (display-warning
           'vulpea
           (format (concat "Could not enable WAL journaling "
                           "(journal_mode stays %S); full-write mode "
                           "degrades to extract-only so worker writes "
                           "cannot block the UI.")
                   mode)
           :warning))
        nil))))

(defun vulpea-db-worker-request (path &optional force)
  "Ask the worker to extract PATH.

The result is applied to the database when it arrives; see
`vulpea-db-worker-done-functions'.  Callers should check
`vulpea-db-worker-can-handle-p' first.

With FORCE non-nil the unchanged-content shortcut is skipped and the
result is applied even when the content hash matches - required when
extraction output changed while content did not (parser epoch or
settings changes)."
  (vulpea-db-worker--ensure)
  (when force
    (puthash path t vulpea-db-worker--force))
  ;; Track the path BEFORE sending: a send that errors on a dying
  ;; worker must leave the path recorded so the salvage path (or the
  ;; caller's fallback) recovers it instead of silently losing it
  (let ((node (list path)))
    (if vulpea-db-worker--in-flight
        (setcdr vulpea-db-worker--in-flight-tail node)
      (setq vulpea-db-worker--in-flight node))
    (setq vulpea-db-worker--in-flight-tail node)
    (setq vulpea-db-worker--in-flight-count
          (1+ vulpea-db-worker--in-flight-count)))
  (condition-case err
      (if (and (vulpea-db-worker--full-write-p)
               ;; WAL is a hard requirement for full-write: without it a
               ;; worker write transaction blocks main-process reads.
               ;; Failure degrades this and future requests to extract-only.
               (vulpea-db-worker--wal-ready-p))
          (progn
            (vulpea-db-worker--log "request parse-and-write%s: %s"
                                   (if force " (force)" "") path)
            (vulpea-db-worker--send
             `(parse-and-write ,path ,(expand-file-name vulpea-db-location)
                               ,(and force t))))
        (vulpea-db-worker--log "request parse%s: %s"
                               (if force " (force)" "") path)
        (vulpea-db-worker--send `(parse ,path)))
    (error
     ;; The send failed (worker died mid-send); un-track and re-raise
     ;; so the caller can fall back
     (vulpea-db-worker--forget path)
     (signal (car err) (cdr err)))))

;;; Client: response handling

(defun vulpea-db-worker--filter (proc output)
  "Process protocol OUTPUT lines from the worker PROC.

Output from a process that is no longer the current worker (a
replaced worker still draining its pipe) is discarded - the shared
assembly state belongs to the current worker only.  A dispatch error
on one line is confined to that line: later lines in the same chunk
still dispatch, so one failing apply cannot wedge every in-flight
file behind it."
  (when (eq proc vulpea-db-worker--process)
    (setq vulpea-db-worker--last-activity (float-time))
    (let ((start 0)
          (t0 (current-time))
          line-end)
      (while (setq line-end (string-match "\n" output start))
        (let* ((tail (substring output start line-end))
               (line (if vulpea-db-worker--output-pending
                         (apply #'concat
                                (nreverse (cons tail
                                                vulpea-db-worker--output-pending)))
                       tail)))
          (setq vulpea-db-worker--output-pending nil)
          (setq start (1+ line-end))
          (when (string-prefix-p "(" line)
            (let ((msg (condition-case nil
                           (car (read-from-string line))
                         (error nil))))
              (when msg
                (condition-case err
                    (vulpea-db-worker--dispatch msg)
                  (error
                   (vulpea-db-worker--log "dispatch error on %S: %s"
                                          (car-safe msg)
                                          (error-message-string err))
                   (message "Vulpea: error handling worker reply: %s"
                            (error-message-string err)))))))))
      (when (< start (length output))
        (push (substring output start) vulpea-db-worker--output-pending))
      (when vulpea-db-worker-debug
        (let ((ms (* 1000 (float-time (time-subtract (current-time) t0)))))
          (when (> ms 50)
            (vulpea-db-worker--log "filter: %.0fms on %d bytes (slow)"
                                   ms (length output))))))))

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
     (vulpea-db-worker--note-success)
     (let ((current vulpea-db-worker--current)
           (force (gethash path vulpea-db-worker--force))
           (t0 (current-time)))
       (setq vulpea-db-worker--current nil)
       (vulpea-db-worker--forget path)
       (vulpea-db-worker--complete path hash mtime size current force)
       (vulpea-db-worker--log "done %s: applied in %.0fms (main)"
                              path
                              (* 1000 (float-time
                                       (time-subtract (current-time) t0))))))
    ;; Full-write mode: the worker wrote the database itself; the
    ;; main process only registers org-ids and re-checks freshness.
    (`(written ,path ,_hash ,mtime ,size ,count ,ids)
     (vulpea-db-worker--note-success)
     (vulpea-db-worker--log "written %s: %s notes" path count)
     (vulpea-db-worker--forget path)
     (let ((attrs (file-attributes path)))
       (cond
        ;; File vanished while the worker was writing (deleted or
        ;; renamed after the worker's stat, before its commit): what
        ;; just landed are ghost notes for a nonexistent path - remove
        ;; them.  The removal event handler was a no-op if it ran
        ;; before the commit.
        ((null attrs)
         (let ((db (vulpea-db)))
           (emacsql-with-transaction db
             (vulpea-db--delete-file-notes path)
             (emacsql db [:delete :from files :where (= path $s1)] path)))
         (run-hook-with-args 'vulpea-db-worker-done-functions
                             path 'missing nil))
        (t
         (vulpea-db--register-id-locations ids path)
         (when (or (not (equal (float-time
                                (file-attribute-modification-time attrs))
                               mtime))
                   (not (equal (file-attribute-size attrs) size)))
           ;; Changed again while the worker was writing: what landed
           ;; reflects older content, re-parse to catch up.
           (vulpea-db-worker--reenqueue path))
         (run-hook-with-args 'vulpea-db-worker-done-functions
                             path 'applied count)))))
    (`(stamped ,path ,ids)
     (vulpea-db-worker--note-success)
     (vulpea-db-worker--forget path)
     ;; Repair org-id registrations a lost written reply never made
     (vulpea-db--register-id-locations ids path)
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'unchanged nil))
    (`(stale ,path)
     (let ((force (gethash path vulpea-db-worker--force)))
       (vulpea-db-worker--forget path)
       (vulpea-db-worker--reenqueue path force))
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'stale nil))
    (`(extractors ,resolved ,missing)
     (vulpea-db-worker--log "worker extractors: resolved %S, missing %S"
                            resolved missing)
     (when (and missing (not vulpea-db-worker--extractors-warned))
       (setq vulpea-db-worker--extractors-warned t)
       (display-warning
        'vulpea
        (format (concat "Worker could not resolve extractor(s) %S; "
                        "their files fall back to main-process apply.  "
                        "Check :worker-lib on the extractor definition.")
                missing)
        :warning)))
    (`(error ,path ,message)
     (setq vulpea-db-worker--current nil)
     (vulpea-db-worker--forget path)
     (vulpea-db-worker--log "error %s: %s" path message)
     (message "Vulpea: worker failed on %s: %s" path message)
     (run-hook-with-args 'vulpea-db-worker-done-functions
                         path 'error nil))))

(defun vulpea-db-worker--reenqueue (path &optional force)
  "Schedule PATH for another pass, via the sync queue when active.
FORCE is carried along so a forced re-index cannot be lost to the
unchanged-content shortcut on the retry.

Without the sync queue, falls back to a direct worker request - or,
when the worker cannot take it (crash-looped and marked broken), a
synchronous `vulpea-db-update-file', so the file is never dropped."
  (cond
   ((and (bound-and-true-p vulpea-db-autosync-mode)
         (fboundp 'vulpea-db-sync--enqueue))
    (vulpea-db-sync--enqueue path force))
   ((vulpea-db-worker-can-handle-p path)
    (vulpea-db-worker-request path force))
   (t
    (condition-case err
        (when (file-exists-p path)
          (vulpea-db-update-file path))
      (error
       (message "Vulpea: failed to re-index %s: %s"
                path (error-message-string err)))))))

(defun vulpea-db-worker--note-success ()
  "Record a successful completion: the worker is demonstrably alive."
  (setq vulpea-db-worker--hang-kills 0))

(defun vulpea-db-worker--forget (path)
  "Drop PATH from the in-flight list.
The worker answers in request order, so PATH is almost always the
head; falling back to a full scan keeps this correct either way."
  (remhash path vulpea-db-worker--force)
  (if (equal (car vulpea-db-worker--in-flight) path)
      (pop vulpea-db-worker--in-flight)
    (setq vulpea-db-worker--in-flight
          (delete path vulpea-db-worker--in-flight)))
  (setq vulpea-db-worker--in-flight-tail
        (last vulpea-db-worker--in-flight))
  (setq vulpea-db-worker--in-flight-count
        (length vulpea-db-worker--in-flight)))

(defun vulpea-db-worker--complete (path hash mtime size current &optional force)
  "Apply a completed extraction of PATH to the database.

HASH, MTIME and SIZE describe the file as the worker read it;
CURRENT carries the extracted nodes.  Stale results - the file
changed or disappeared while the worker was parsing - are discarded,
and changed files are re-enqueued with the sync queue.

With FORCE non-nil the unchanged-content shortcut is skipped: the
result is applied even when the content hash matches what is stored
\(parser or settings changed, content did not)."
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
      (vulpea-db-worker--reenqueue path force)
      (run-hook-with-args 'vulpea-db-worker-done-functions
                          path 'stale nil))
     ;; Content identical to what is already indexed: refresh the
     ;; stored stamp so the cheap mtime/size check passes next time.
     ;; Skipped for forced results - extraction output changed even
     ;; though content did not.
     ((and (not force)
           (equal (plist-get (vulpea-db--get-file-hash path) :hash) hash))
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

(defun vulpea-db-worker--crash-loop-p ()
  "Record the current death and detect a crash loop.
A crash loop is three deaths within 10 seconds of their spawn,
inside one minute.  Long-lived workers dying (or being killed) do
not count."
  (when (and vulpea-db-worker--spawn-time
             (< (- (float-time) vulpea-db-worker--spawn-time) 10))
    (push (float-time) vulpea-db-worker--crash-times))
  (setq vulpea-db-worker--crash-times
        (seq-filter (lambda (time) (< (- (float-time) time) 60))
                    vulpea-db-worker--crash-times))
  (>= (length vulpea-db-worker--crash-times) 3))

(defun vulpea-db-worker--stderr-tail ()
  "Return the last few lines of the worker's stderr, for diagnostics."
  (if-let* ((buffer (get-buffer " *vulpea-worker-stderr*")))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (forward-line -10)
          (string-trim (buffer-substring-no-properties
                        (point) (point-max)))))
    ""))

(defun vulpea-db-worker--sentinel (proc event)
  "Handle worker PROC death (EVENT): re-enqueue in-flight work.

Only acts while PROC is still the current worker: a deferred
sentinel firing after `vulpea-db-worker--ensure' already salvaged
and replaced the process must not clobber the replacement's state
\(orphaning the live process, double re-enqueueing its files).

Detects crash loops: a worker that keeps dying right after spawning
marks itself broken (see `vulpea-db-worker--broken') instead of
respawning forever, and the sync queue falls back to synchronous
processing."
  (when (and (not (process-live-p proc))
             (eq proc vulpea-db-worker--process))
    (vulpea-db-worker--log "worker died: %s; stderr tail: %s"
                           (string-trim event)
                           (vulpea-db-worker--log-truncate
                            (vulpea-db-worker--stderr-tail) 500))
    (when (vulpea-db-worker--crash-loop-p)
      (setq vulpea-db-worker--broken t)
      (display-warning
       'vulpea
       (format (concat "Extraction worker keeps dying; falling back to "
                       "synchronous indexing.  Last stderr:\n%s\n"
                       "Run M-x vulpea-db-worker-diagnose to investigate, "
                       "M-x vulpea-db-worker-reset to retry.")
               (vulpea-db-worker--stderr-tail))
       :error))
    (vulpea-db-worker--salvage)))

;;; Worker side (runs in emacs --batch)

(defun vulpea-db-worker--reply (form)
  "Print FORM as one protocol line on stdout."
  (princ (vulpea-db-worker--print form))
  (princ "\n"))

(defvar vulpea-db-worker--unresolved-extractors nil
  "Names of declared worker-safe extractors this worker cannot run.
Non-nil makes `parse-and-write' requests fall back to streaming, so
those extractors run in the main process instead.")

(defvar vulpea-db-worker--version-mismatch nil
  "Non-nil when this worker's vulpea differs from the main process's.

Set from the db constants in the settings message.  A worker running
different code must never open the database: `vulpea-db--init' deletes
and rebuilds the database file on a schema version mismatch, which
would destroy the main process's data from underneath it.  While set,
`parse-and-write' requests fall back to streaming results.")

(defun vulpea-db-worker--apply-settings (vars link-types extractors
                                              &optional db-constants)
  "Set allowlisted VARS, register LINK-TYPES and EXTRACTORS here.

EXTRACTORS are wire specs of worker-safe extractors (see
`vulpea-db-worker--extractor-specs').  Each is resolved by loading
its :lib and checking its :fn; resolved ones are installed in this
worker's extractor registry (without schema - the main process owns
DDL), unresolved ones are reported back and disable full-write for
this worker."
  (pcase-dolist (`(,sym . ,value) vars)
    (set sym value))
  ;; A code-version mismatch (main upgraded vulpea while running, or
  ;; stale byte-code) forbids opening the database from this worker:
  ;; vulpea-db--init would delete and rebuild it on a schema mismatch
  (setq vulpea-db-worker--version-mismatch
        (not (and (equal (plist-get db-constants :db-version)
                         vulpea-db-version)
                  (equal (plist-get db-constants :parser-epoch)
                         vulpea-db-parser-epoch))))
  (when vulpea-db-worker--version-mismatch
    (message "vulpea worker: version mismatch (main %S/%S, worker %S/%S), full-write disabled"
             (plist-get db-constants :db-version)
             (plist-get db-constants :parser-epoch)
             vulpea-db-version vulpea-db-parser-epoch))
  (require 'ol)
  (dolist (type link-types)
    (unless (assoc type org-link-parameters)
      (org-link-set-parameters type)))
  (org-link-make-regexps)
  (when (fboundp 'org-element-update-syntax)
    (org-element-update-syntax))
  ;; Resolve worker-safe extractors
  (let (resolved missing)
    (dolist (spec extractors)
      (let ((name (plist-get spec :name))
            (fn (plist-get spec :fn))
            (lib (plist-get spec :lib)))
        (when lib
          (condition-case nil
              (if (stringp lib)
                  (load lib nil t)
                (require lib))
            (error nil)))
        (if (and fn (fboundp fn))
            (push (make-vulpea-extractor
                   :name name
                   :priority (or (plist-get spec :priority) 100)
                   :extract-fn fn
                   :requires-ast (plist-get spec :requires-ast)
                   :worker-safe t)
                  resolved)
          (push name missing))))
    (setq vulpea-db--extractors
          (sort (nreverse resolved)
                (lambda (a b)
                  (< (vulpea-extractor-priority a)
                     (vulpea-extractor-priority b)))))
    (setq vulpea-db-worker--unresolved-extractors (nreverse missing))
    (vulpea-db-worker--reply
     `(extractors ,(mapcar #'vulpea-extractor-name vulpea-db--extractors)
                  ,vulpea-db-worker--unresolved-extractors))))

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
  (vulpea-db-worker--wal-ready-p))

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

Returns the number of notes written, `conflict', or (error . MSG)
for a deterministic failure - only lock contention retries; anything
else must surface instead of looping through silent retries forever."
  (condition-case err
      (emacsql-with-transaction (vulpea-db)
        (if (not (equal (vulpea-db--get-file-hash
                         (vulpea-parse-ctx-path ctx))
                        stored))
            'conflict
          (vulpea-db--apply-parse-ctx ctx 'skip-org-id)))
    ((emacsql-locked sqlite-locked-error) 'conflict)
    (sqlite-error
     ;; SQLITE_BUSY surfaces as a generic sqlite-error whose message
     ;; mentions locking; treat those as contention, the rest as real
     (if (string-match-p "locked\\|busy"
                         (downcase (error-message-string err)))
         'conflict
       (cons 'error (error-message-string err))))
    (error (cons 'error (error-message-string err)))))

(defun vulpea-db-worker--handle-parse-and-write (path db &optional force)
  "Extract PATH and write the results to the database at DB.

Full-write mode: this worker owns the database write; the reply
tells the main process what happened so it can register org-ids
\(`written'), note the no-op (`stamped'), or re-queue a file whose
result became outdated (`stale') - because the file changed
mid-parse, or because the main process indexed newer content
concurrently (see `vulpea-db-worker--apply-guarded').

With FORCE non-nil the unchanged-content shortcut is skipped and the
result is written even when the content hash matches."
  (if (or vulpea-db-worker--unresolved-extractors
          ;; Version mismatch: opening the db from here could destroy
          ;; it (vulpea-db--init rebuilds on schema mismatch)
          vulpea-db-worker--version-mismatch)
      ;; Stream the results instead; the main process applies them
      (vulpea-db-worker--handle-parse path)
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
           ;; Content identical to what is indexed: refresh the stamp.
           ;; Skipped when forced - extraction output changed even
           ;; though content did not.
           ((and (not force)
                 (equal (plist-get stored :hash)
                        (vulpea-parse-ctx-hash ctx)))
            (vulpea-db--update-file-hash path
                                         (vulpea-parse-ctx-hash ctx)
                                         (vulpea-parse-ctx-mtime ctx)
                                         (vulpea-parse-ctx-size ctx))
            ;; Carry the note IDs: if a previous written reply was
            ;; lost (worker crash after commit), the retry lands here
            ;; and the main process can still register org-ids
            (vulpea-db-worker--reply
             `(stamped ,path ,(vulpea-db-worker--ctx-ids ctx))))
           (t
            (let ((count (vulpea-db-worker--apply-guarded ctx stored)))
              (cond
               ((eq count 'conflict)
                (vulpea-db-worker--reply `(stale ,path)))
               ((eq (car-safe count) 'error)
                (vulpea-db-worker--reply `(error ,path ,(cdr count))))
               (t
                (vulpea-db-worker--reply
                 `(written ,path
                           ,(vulpea-parse-ctx-hash ctx)
                           ,(vulpea-parse-ctx-mtime ctx)
                           ,(vulpea-parse-ctx-size ctx)
                           ,count
                           ,(vulpea-db-worker--ctx-ids ctx))))))))))
      (error
       (vulpea-db-worker--reply
        `(error ,path ,(error-message-string err)))))))

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
        (`(settings ,vars ,link-types ,extractors ,db-constants)
         (vulpea-db-worker--apply-settings vars link-types extractors
                                           db-constants))
        (`(parse ,path)
         (vulpea-db-worker--handle-parse path))
        (`(parse-and-write ,path ,db ,force)
         (vulpea-db-worker--handle-parse-and-write path db force))
        (_ nil)))))

;;; Diagnostics

;;;###autoload
(defun vulpea-db-worker-diagnose ()
  "Run an end-to-end health check of async extraction and report.

Spawns a fresh worker, round-trips a small file through the current
`vulpea-db-async-extraction' mode against the real database location,
and reports every stage - spawn time, journal mode, mode degradations
and why, round-trip time, and the worker's stderr if anything went
wrong - in a dedicated buffer.  Safe to run at any time; the check
file is temporary and removed from the database afterwards."
  (interactive)
  (let ((report (get-buffer-create "*vulpea-worker-diagnose*"))
        (vulpea-db-worker-debug t))
    (with-current-buffer report
      (erase-buffer)
      (insert (format "vulpea async extraction diagnosis (%s)\n\n"
                      (format-time-string "%F %T")))
      (insert (format "emacs          : %s\n" emacs-version))
      (insert (format "mode           : %S\n" vulpea-db-async-extraction))
      (insert (format "threshold      : %S\n"
                      vulpea-db-async-extraction-threshold))
      (insert (format "db location    : %s\n" vulpea-db-location))
      (insert (format "extractors     : %S\n" vulpea-db--extractors))
      (insert (format "index filters  : %S\n"
                      vulpea-db-note-index-filter-functions))
      (insert (format "filters inert  : %S\n"
                      (vulpea-db-worker--filters-inert-p)))
      (insert (format "worker broken  : %S\n" vulpea-db-worker--broken))
      (let ((reasons (vulpea-db-worker-rejection-reasons "probe.org")))
        (insert (format "can handle .org: %s\n"
                        (if reasons
                            (format "NO - %S - production files will use the SYNCHRONOUS path!"
                                    reasons)
                          "yes")))))
    (vulpea-db-worker-stop)
    (setq vulpea-db-worker--broken nil
          vulpea-db-worker--crash-times nil
          vulpea-db-worker--wal-failed nil)
    ;; Journal mode of the real database
    (let ((mode (caar (sqlite-select (oref (vulpea-db) handle)
                                     "PRAGMA journal_mode"))))
      (with-current-buffer report
        (insert (format "journal mode   : %s\n" mode))))
    ;; Round trip
    (let* ((path (make-temp-file "vulpea-diagnose-" nil ".org"
                                 ":PROPERTIES:\n:ID: vulpea-diagnose-id\n:END:\n#+TITLE: Diagnose\n"))
           (t0 (current-time))
           (status nil)
           (vulpea-db-worker-done-functions
            (list (lambda (_path s _count) (setq status s)))))
      (unwind-protect
          (progn
            (condition-case err
                (progn
                  (vulpea-db-worker-request path)
                  (with-current-buffer report
                    (insert (format "spawn + request: ok (%.0fms)\n"
                                    (* 1000 (float-time
                                             (time-subtract (current-time) t0))))
                            (format "full-write     : %S\n"
                                    (vulpea-db-worker--full-write-p))
                            (format "wal failed     : %S\n"
                                    vulpea-db-worker--wal-failed)))
                  (let ((deadline (+ (float-time) 30)))
                    (while (and (not status) (< (float-time) deadline))
                      (accept-process-output vulpea-db-worker--process 0.1)))
                  (with-current-buffer report
                    (insert (format "round trip     : %s in %.0fms\n"
                                    (or status "TIMEOUT after 30s")
                                    (* 1000 (float-time
                                             (time-subtract (current-time) t0)))))))
              (error
               (with-current-buffer report
                 (insert (format "FAILED         : %s\n"
                                 (error-message-string err))))))
            (with-current-buffer report
              (insert (format "\nworker process : %S\n"
                              vulpea-db-worker--process))
              (let ((stderr (vulpea-db-worker--stderr-tail)))
                (unless (string-empty-p stderr)
                  (insert "\nworker stderr tail:\n" stderr "\n")))
              (when-let* ((log (get-buffer vulpea-db-worker-log-buffer)))
                (insert "\nprotocol log:\n"
                        (with-current-buffer log (buffer-string))))))
        ;; Cleanup: temp note out of the database, worker down
        (ignore-errors (vulpea-db--delete-file-notes path))
        (ignore-errors
          (emacsql (vulpea-db) [:delete :from files :where (= path $s1)]
                   path))
        (delete-file path)
        (vulpea-db-worker-stop)))
    (pop-to-buffer report)))

(provide 'vulpea-db-worker)
;;; vulpea-db-worker.el ends here
