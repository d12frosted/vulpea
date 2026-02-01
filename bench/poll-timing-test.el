;;; poll-timing-test.el --- Benchmark polling check -*- lexical-binding: t; -*-

;; Measures the time spent in vulpea-db-sync--check-external-changes
;; (the function that runs every poll interval).
;;
;; Usage with eldev:
;;   eldev -dtT exec -f bench/poll-timing-test.el "(poll-timing-test-run)"
;;
;; Set VULPEA_NOTES_DIR env var to override notes directory
;; (default: ~/vulpea).

(require 'vulpea-db)
(require 'vulpea-db-sync)
(require 'vulpea-db-extract)

(defun poll-timing-test--measure-async (label iterations)
  "Measure async check-external-changes ITERATIONS times, printing LABEL."
  (dotimes (i iterations)
    (setq vulpea-db-sync--poll-scan-in-progress nil)
    (let* ((callback-done nil)
           (advice-fn (lambda (&rest _) (setq callback-done t)))
           (start (current-time))
           blocking-time)
      (advice-add 'vulpea-db-sync--check-external-changes-with-files
                  :after advice-fn)
      (unwind-protect
          (progn
            (vulpea-db-sync--check-external-changes)
            (setq blocking-time (* 1000 (float-time (time-subtract (current-time) start))))
            (let ((timeout 30))
              (while (and (not callback-done)
                          (< (float-time (time-subtract (current-time) start)) timeout))
                (accept-process-output nil 0.01)))
            (message "[poll-timing] %s #%d: blocking=%.0fms total=%.0fms"
                     label (1+ i) blocking-time
                     (* 1000 (float-time (time-subtract (current-time) start)))))
        (advice-remove 'vulpea-db-sync--check-external-changes-with-files
                       advice-fn)))))

(defun poll-timing-test-run ()
  "Benchmark polling check against real notes."
  (let* ((notes-dir (or (getenv "VULPEA_NOTES_DIR")
                        (expand-file-name "~/vulpea")))
         (db-file (make-temp-file "vulpea-bench-" nil ".db"))
         (vulpea-directory notes-dir)
         (vulpea-db-location db-file)
         (vulpea-db-sync-directories (list notes-dir))
         (vulpea-db-sync-external-method 'poll)
         (vulpea-db-parse-method 'single-temp-buffer)
         (vulpea-db-index-heading-level t)
         (vulpea-db-sync--file-attributes (make-hash-table :test 'equal)))

    (message "========================================")
    (message "Poll Timing Test")
    (message "========================================")
    (message "Notes dir: %s" notes-dir)
    (message "fd available: %s" (if (executable-find "fd") "yes" "no"))
    (message "")

    ;; Phase 1: Measure directory-files-recursively alone
    (message "--- Phase 1: directory-files-recursively (baseline) ---")
    (let* ((start (current-time))
           (files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (elapsed (* 1000 (float-time (time-subtract (current-time) start)))))
      (message "[poll-timing] directory-files-recursively: %.0fms (%d files)"
               elapsed (length files)))

    ;; Phase 2: Measure fd and find separately
    (message "")
    (message "--- Phase 2: subprocess file listing ---")
    (let ((expanded-dir (expand-file-name notes-dir)))
      (when (executable-find "fd")
        (let* ((start (current-time))
               (output (shell-command-to-string
                        (format "fd --type f --extension org --hidden --no-ignore --exclude '.*' . %s"
                                (shell-quote-argument expanded-dir))))
               (files (seq-filter #'vulpea-db-sync--org-file-p
                                  (split-string output "\n" t))))
          (message "[poll-timing] fd: %.0fms (%d files)"
                   (* 1000 (float-time (time-subtract (current-time) start)))
                   (length files))))
      (let* ((start (current-time))
             (output (shell-command-to-string
                      (format "find %s -type f -name '*.org' -not -path '*/.*'"
                              (shell-quote-argument expanded-dir))))
             (files (seq-filter #'vulpea-db-sync--org-file-p
                                (split-string output "\n" t))))
        (message "[poll-timing] find: %.0fms (%d files)"
                 (* 1000 (float-time (time-subtract (current-time) start)))
                 (length files))))

    ;; Phase 3: Measure file-attributes calls
    (message "")
    (message "--- Phase 3: file-attributes ---")
    (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (start (current-time))
           (_attrs (mapcar #'file-attributes files))
           (elapsed (* 1000 (float-time (time-subtract (current-time) start)))))
      (message "[poll-timing] file-attributes x%d: %.0fms"
               (length files) elapsed))

    ;; Phase 4: Measure synchronous check-with-files
    (message "")
    (message "--- Phase 4: check-external-changes-with-files (sync callback) ---")
    (clrhash vulpea-db-sync--file-attributes)
    (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (start (current-time)))
      (vulpea-db-sync--check-external-changes-with-files files)
      (message "[poll-timing] cold: %.0fms (cache size: %d)"
               (* 1000 (float-time (time-subtract (current-time) start)))
               (hash-table-count vulpea-db-sync--file-attributes)))
    (dotimes (i 3)
      (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                                (directory-files-recursively notes-dir "\\.org\\'")))
             (start (current-time)))
        (vulpea-db-sync--check-external-changes-with-files files)
        (message "[poll-timing] warm #%d: %.0fms"
                 (1+ i)
                 (* 1000 (float-time (time-subtract (current-time) start))))))

    ;; Phase 5: Async with fd (if available)
    (when (executable-find "fd")
      (message "")
      (message "--- Phase 5: async check-external-changes (with fd) ---")
      (clrhash vulpea-db-sync--file-attributes)
      (poll-timing-test--measure-async "fd cold" 1)
      (poll-timing-test--measure-async "fd warm" 3))

    ;; Phase 6: Async with find (force fallback by hiding fd)
    (message "")
    (message "--- Phase 6: async check-external-changes (with find, no fd) ---")
    (clrhash vulpea-db-sync--file-attributes)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &rest _)
                 (unless (string= cmd "fd")
                   (funcall (symbol-function 'executable-find) cmd)))))
      ;; Need real executable-find for find itself, so just override
      ;; the scan function's fd detection
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (cmd &rest _)
                   (if (string= cmd "fd") nil
                     ;; Use the raw path lookup
                     (locate-file cmd exec-path exec-suffixes 1)))))
        (poll-timing-test--measure-async "find cold" 1)
        (poll-timing-test--measure-async "find warm" 3)))

    ;; Cleanup
    (delete-file db-file)

    (message "")
    (message "========================================")))

(provide 'poll-timing-test)
;;; poll-timing-test.el ends here
