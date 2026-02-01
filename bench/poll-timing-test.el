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
    (message "")

    ;; Phase 1: Measure directory-files-recursively alone
    (message "--- Phase 1: directory-files-recursively ---")
    (let* ((start (current-time))
           (files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (elapsed (* 1000 (float-time (time-subtract (current-time) start)))))
      (message "[poll-timing] directory-files-recursively: %.0fms (%d files)"
               elapsed (length files)))

    ;; Phase 2: Measure fd equivalent
    (message "")
    (message "--- Phase 2: fd subprocess (sync wait) ---")
    (let* ((start (current-time))
           (expanded-dir (expand-file-name notes-dir))
           (output (shell-command-to-string
                    (if (executable-find "fd")
                        (format "fd --type f --extension org --hidden --no-ignore --exclude '.*' . %s"
                                (shell-quote-argument expanded-dir))
                      (format "find %s -type f -name '*.org' -not -path '*/.*'"
                              (shell-quote-argument expanded-dir)))))
           (files (seq-filter #'vulpea-db-sync--org-file-p
                              (split-string output "\n" t)))
           (elapsed (* 1000 (float-time (time-subtract (current-time) start)))))
      (message "[poll-timing] fd/find: %.0fms (%d files)"
               elapsed (length files)))

    ;; Phase 3: Measure file-attributes calls
    (message "")
    (message "--- Phase 3: file-attributes for all files ---")
    (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (start (current-time))
           (_attrs (mapcar #'file-attributes files))
           (elapsed (* 1000 (float-time (time-subtract (current-time) start)))))
      (message "[poll-timing] file-attributes x%d: %.0fms"
               (length files) elapsed))

    ;; Phase 4: Measure check-external-changes-with-files (synchronous part)
    ;; This is what runs in the callback after async fd completes
    (message "")
    (message "--- Phase 4: check-external-changes-with-files (cold cache) ---")
    (clrhash vulpea-db-sync--file-attributes)
    (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                              (directory-files-recursively notes-dir "\\.org\\'")))
           (start (current-time)))
      (vulpea-db-sync--check-external-changes-with-files files)
      (message "[poll-timing] cold check-with-files: %.0fms (cache size: %d)"
               (* 1000 (float-time (time-subtract (current-time) start)))
               (hash-table-count vulpea-db-sync--file-attributes)))

    ;; Phase 5: Warm cache (no changes)
    (message "")
    (message "--- Phase 5: check-external-changes-with-files (warm cache) ---")
    (dotimes (i 4)
      (let* ((files (seq-filter #'vulpea-db-sync--org-file-p
                                (directory-files-recursively notes-dir "\\.org\\'")))
             (start (current-time)))
        (vulpea-db-sync--check-external-changes-with-files files)
        (message "[poll-timing] warm check-with-files #%d: %.0fms"
                 (1+ i)
                 (* 1000 (float-time (time-subtract (current-time) start))))))

    ;; Phase 6: Measure async check-external-changes (blocking time + total)
    (message "")
    (message "--- Phase 6: async check-external-changes ---")
    (clrhash vulpea-db-sync--file-attributes)
    (setq vulpea-db-sync--poll-scan-in-progress nil)
    (let* ((callback-done nil)
           ;; Advice to detect when callback finishes
           (orig-fn (symbol-function 'vulpea-db-sync--check-external-changes-with-files))
           (start (current-time))
           blocking-time)
      (advice-add 'vulpea-db-sync--check-external-changes-with-files
                  :after (lambda (&rest _) (setq callback-done t)))
      (unwind-protect
          (progn
            (vulpea-db-sync--check-external-changes)
            (setq blocking-time (* 1000 (float-time (time-subtract (current-time) start))))
            (message "[poll-timing] async call returned in: %.0fms (non-blocking)" blocking-time)

            ;; Wait for subprocess + callback to finish
            (let ((timeout 30))
              (while (and (not callback-done)
                          (< (float-time (time-subtract (current-time) start)) timeout))
                (accept-process-output nil 0.01)))

            (message "[poll-timing] async total (subprocess + callback): %.0fms"
                     (* 1000 (float-time (time-subtract (current-time) start))))
            (message "[poll-timing] cache size: %d"
                     (hash-table-count vulpea-db-sync--file-attributes)))
        (advice-remove 'vulpea-db-sync--check-external-changes-with-files
                       (lambda (&rest _) (setq callback-done t)))))

    ;; Phase 7: Warm async checks
    (message "")
    (message "--- Phase 7: async check-external-changes (warm, repeated) ---")
    (dotimes (i 3)
      (setq vulpea-db-sync--poll-scan-in-progress nil)
      (let* ((callback-done nil)
             (start (current-time))
             blocking-time)
        (advice-add 'vulpea-db-sync--check-external-changes-with-files
                    :after (lambda (&rest _) (setq callback-done t)))
        (unwind-protect
            (progn
              (vulpea-db-sync--check-external-changes)
              (setq blocking-time (* 1000 (float-time (time-subtract (current-time) start))))
              (let ((timeout 30))
                (while (and (not callback-done)
                            (< (float-time (time-subtract (current-time) start)) timeout))
                  (accept-process-output nil 0.01)))
              (message "[poll-timing] async warm #%d: blocking=%.0fms total=%.0fms"
                       (1+ i) blocking-time
                       (* 1000 (float-time (time-subtract (current-time) start)))))
          (advice-remove 'vulpea-db-sync--check-external-changes-with-files
                         (lambda (&rest _) (setq callback-done t))))))

    ;; Cleanup
    (delete-file db-file)

    (message "")
    (message "========================================")))

(provide 'poll-timing-test)
;;; poll-timing-test.el ends here
