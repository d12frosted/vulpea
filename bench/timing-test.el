;;; timing-test.el --- Test with timing instrumentation -*- lexical-binding: t; -*-

(require 'vulpea-db-sync)
(require 'vulpea-db-extract)

(defun timing-test-run (notes-dir sample-size)
  "Run sync on first SAMPLE-SIZE files from NOTES-DIR with timing."
  (let* ((files (seq-take (directory-files-recursively notes-dir "\\.org\\'") sample-size))
         (total (length files))
         (db (vulpea-db)))

    (message "========================================")
    (message "Timing Test: %d files" total)
    (message "========================================\n")

    ;; Initialize timing
    (setq vulpea-db--timing-data '((parse . 0) (db . 0)))

    ;; Process files
    (let ((start (current-time))
          (processed 0))
      (emacsql-with-transaction db
        (dolist (file files)
          (vulpea-db-update-file file)
          (setq processed (1+ processed))

          ;; Report every 1000 files
          (when (zerop (mod processed 1000))
            (let* ((elapsed (* 1000 (float-time (time-subtract (current-time) start))))
                   (avg (/ elapsed processed))
                   (parse-total (cdr (assoc 'parse vulpea-db--timing-data)))
                   (db-total (cdr (assoc 'db vulpea-db--timing-data)))
                   (parse-avg (/ parse-total processed))
                   (db-avg (/ db-total processed)))
              (message "Progress: %6d/%d | Total:%.2fms/file | Parse:%.2fms (%.0f%%) | DB:%.2fms (%.0f%%)"
                       processed total avg
                       parse-avg (* 100 (/ parse-avg avg))
                       db-avg (* 100 (/ db-avg avg)))))))

      ;; Final report
      (let* ((end (current-time))
             (total-time (* 1000 (float-time (time-subtract end start))))
             (avg (/ total-time total))
             (parse-total (cdr (assoc 'parse vulpea-db--timing-data)))
             (db-total (cdr (assoc 'db vulpea-db--timing-data)))
             (parse-avg (/ parse-total total))
             (db-avg (/ db-total total))
             (other-avg (- avg parse-avg db-avg))
             ;; Detailed parse breakdown
             (parse-io-total (or (cdr (assoc 'parse-io vulpea-db--timing-data)) 0))
             (parse-org-mode-total (or (cdr (assoc 'parse-org-mode vulpea-db--timing-data)) 0))
             (parse-ast-total (or (cdr (assoc 'parse-ast vulpea-db--timing-data)) 0))
             (parse-extract-total (or (cdr (assoc 'parse-extract vulpea-db--timing-data)) 0))
             (parse-io-avg (/ parse-io-total total))
             (parse-org-mode-avg (/ parse-org-mode-total total))
             (parse-ast-avg (/ parse-ast-total total))
             (parse-extract-avg (/ parse-extract-total total)))

        (message "\n========================================")
        (message "FINAL TIMING BREAKDOWN")
        (message "========================================")
        (message "Total: %d files in %.2fs (%.2f min)" total (/ total-time 1000) (/ total-time 60000))
        (message "Average: %.2fms per file" avg)
        (message "  Parse:  %.2fms (%.0f%%)" parse-avg (* 100 (/ parse-avg avg)))
        (message "    I/O:        %.2fms (%.0f%% of parse)" parse-io-avg (* 100 (/ parse-io-avg parse-avg)))
        (message "    org-mode:   %.2fms (%.0f%% of parse)" parse-org-mode-avg (* 100 (/ parse-org-mode-avg parse-avg)))
        (message "    AST parse:  %.2fms (%.0f%% of parse)" parse-ast-avg (* 100 (/ parse-ast-avg parse-avg)))
        (message "    Extract:    %.2fms (%.0f%% of parse)" parse-extract-avg (* 100 (/ parse-extract-avg parse-avg)))
        (message "  DB:     %.2fms (%.0f%%)" db-avg (* 100 (/ db-avg avg)))
        (message "  Other:  %.2fms (%.0f%%)" other-avg (* 100 (/ other-avg avg)))
        (message "========================================")))))

(provide 'timing-test)
