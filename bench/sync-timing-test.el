;;; sync-timing-test.el --- Benchmark autosync startup -*- lexical-binding: t; -*-

;; Measures the time spent in each phase of vulpea-db-sync--start
;; and subsequent queue processing.
;;
;; Usage with eldev:
;;   eldev -dtT exec -f sync-timing-test-run
;;
;; Or from Emacs:
;;   (require 'sync-timing-test)
;;   (sync-timing-test-run)
;;
;; Set VULPEA_NOTES_DIR env var to override notes directory
;; (default: ~/vulpea).

(require 'vulpea-db)
(require 'vulpea-db-sync)
(require 'vulpea-db-extract)

(defun sync-timing-test-run ()
  "Benchmark autosync startup against real notes."
  (let* ((notes-dir (or (getenv "VULPEA_NOTES_DIR")
                        (expand-file-name "~/vulpea")))
         (db-file (make-temp-file "vulpea-bench-" nil ".db"))
         (vulpea-directory notes-dir)
         (vulpea-db-location db-file)
         (vulpea-db-sync-directories (list notes-dir))
         (vulpea-db-sync-scan-on-enable 'async)
         (vulpea-db-sync-external-method
          (if (executable-find "fswatch") 'fswatch nil))
         (vulpea-db-parse-method 'single-temp-buffer)
         (vulpea-db-index-heading-level t)
         (vulpea-db-sync-debug t))

    (message "========================================")
    (message "Sync Timing Test")
    (message "========================================")
    (message "Notes dir: %s" notes-dir)
    (message "DB file:   %s" db-file)
    (message "")

    ;; Phase 1: Cold start - full scan to build DB
    (message "--- Phase 1: Cold start (full scan) ---")
    (vulpea-db)  ; ensure DB is initialized
    (let ((start (current-time)))
      (vulpea-db-sync-full-scan t)
      (message "[sync-timing] full-scan: %.0fms"
               (* 1000 (float-time (time-subtract (current-time) start)))))

    (message "")

    ;; Phase 2: Warm start - autosync enable (the real startup path)
    ;; With async scan, this should return almost instantly.
    ;; The subprocess sentinel will populate the queue later.
    (message "--- Phase 2: Warm start (autosync enable) ---")
    (let ((start (current-time)))
      (vulpea-db-autosync-mode 1)
      (message "[sync-timing] autosync-mode returned in: %.0fms"
               (* 1000 (float-time (time-subtract (current-time) start)))))

    ;; Phase 3: Wait for async subprocess to finish and queue to populate
    (message "")
    (message "--- Phase 3: Waiting for async scan subprocess ---")
    (let ((start (current-time))
          (timeout 60))
      ;; Accept process output until the vulpea-scan process finishes
      (while (and (get-process "vulpea-scan")
                  (< (float-time (time-subtract (current-time) start)) timeout))
        (accept-process-output nil 0.1))
      (message "[sync-timing] subprocess finished in: %.0fms"
               (* 1000 (float-time (time-subtract (current-time) start)))))

    ;; Phase 4: Drain the queue
    (message "")
    (message "--- Phase 4: Queue processing ---")
    (message "Queue length: %d" (length vulpea-db-sync--queue))
    (let ((start (current-time))
          (batches 0))
      (while vulpea-db-sync--queue
        (vulpea-db-sync--process-queue)
        (setq batches (1+ batches)))
      (message "[sync-timing] queue drain: %.0fms (%d batches)"
               (* 1000 (float-time (time-subtract (current-time) start)))
               batches))

    ;; Cleanup
    (vulpea-db-autosync-mode -1)
    (delete-file db-file)

    (message "")
    (message "========================================")))

(provide 'sync-timing-test)
;;; sync-timing-test.el ends here
