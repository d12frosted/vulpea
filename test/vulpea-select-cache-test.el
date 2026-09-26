;;; vulpea-select-cache-test.el --- Tests for the selection candidate cache -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Test the in-memory candidate cache behind `vulpea-find' and
;; `vulpea-insert'.
;;
;;; Code:

(require 'ert)
(require 'vulpea)
(require 'vulpea-select)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'vulpea-db-worker)
(require 'vulpea-test-helpers)

;;; Helpers

(defmacro vulpea-select-cache-test--with-cache (&rest body)
  "Run BODY with the candidate cache enabled and dropped before and after."
  (declare (indent 0))
  `(let ((vulpea-select-cache t)
         (vulpea-select-cache-prewarm nil)
         (vulpea-select-dyncontext-fn nil)
         (vulpea-find-default-filter nil)
         (vulpea-insert-default-filter nil)
         (vulpea-find-default-candidates-source #'vulpea-db-query)
         (vulpea-insert-default-candidates-source #'vulpea-db-query))
     (vulpea-select-cache-drop)
     (unwind-protect
         (progn ,@body)
       (vulpea-select-cache-drop))))

(defun vulpea-select-cache-test--uncached ()
  "Return the candidates the uncached `vulpea-find' path builds."
  (mapcar #'car (vulpea-select--completions (vulpea-db-query) t)))

(defun vulpea-select-cache-test--sorted (candidates)
  "Return CANDIDATES as (STRING . ID) pairs, sorted."
  (sort (mapcar (lambda (c)
                  (cons (substring-no-properties c)
                        (get-text-property 0 'vulpea-note-id c)))
                candidates)
        (lambda (a b) (string< (car a) (car b)))))

(defun vulpea-select-cache-test--same-as-uncached ()
  "Assert that cached and uncached candidates agree."
  (should (equal (vulpea-select-cache-test--sorted
                  (vulpea-select-cache-candidates))
                 (vulpea-select-cache-test--sorted
                  (vulpea-select-cache-test--uncached)))))

(defun vulpea-select-cache-test--insert-fixture ()
  "Insert notes with aliases, tags and headings."
  (vulpea-test--insert-test-note "id-a" "Alpha"
                                 :path "/tmp/a.org"
                                 :tags '("t1" "t2")
                                 :aliases '("First" "Primus"))
  (vulpea-test--insert-test-note "id-a-h" "Alpha heading"
                                 :path "/tmp/a.org"
                                 :level 1
                                 :pos 10
                                 :file-title "Alpha"
                                 :tags '("t3"))
  (vulpea-test--insert-test-note "id-b" "Beta" :path "/tmp/b.org"))

(defun vulpea-select-cache-test--write (path id title)
  "Write an org file at PATH holding a note with ID and TITLE."
  (with-temp-file path
    (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n"
                    id title))))

(defun vulpea-select-cache-test--pick (target)
  "Return a `completing-read' stand-in picking the candidate TARGET.
TARGET is compared to candidates without their invisible id suffix."
  (lambda (_prompt collection &rest _)
    (or (seq-find (lambda (c)
                    (string-prefix-p (concat target " ")
                                     (concat (substring-no-properties c) " ")))
                  (all-completions "" collection))
        target)))

;;; Equality with the uncached path

(ert-deftest vulpea-select-cache-matches-uncached ()
  "The cache holds exactly the candidates the uncached path builds."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      ;; same order on a fresh build
      (should (equal (vulpea-select-cache-candidates)
                     (vulpea-select-cache-test--uncached)))
      (vulpea-select-cache-test--same-as-uncached)
      ;; the alias rows are there and annotated with the primary title
      (should (seq-find (lambda (c) (string-prefix-p "First (Alpha)" c))
                        (vulpea-select-cache-candidates))))))

(ert-deftest vulpea-select-cache-matches-uncached-non-matchable ()
  "The cache agrees with the uncached path when annotations are separate."
  (vulpea-select-cache-test--with-cache
    (let ((vulpea-select-annotate-matchable nil)
          (vulpea-select-match-ids nil))
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-select-cache-test--insert-fixture)
        (should (equal (vulpea-select-cache-candidates)
                       (vulpea-select-cache-test--uncached)))))))

(ert-deftest vulpea-select-cache-candidates-do-not-hold-notes ()
  "Cached candidates carry the id but not the note struct."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (dolist (c (vulpea-select-cache-candidates))
        (should (get-text-property 0 'vulpea-note-id c))
        (should-not (text-property-not-all 0 (length c) 'vulpea-note nil c))))))

(ert-deftest vulpea-select-cache-candidate-note ()
  "`vulpea-select-candidate-note' resolves cached candidates, aliases too."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (let* ((cands (vulpea-select-cache-candidates))
             (alias (seq-find (lambda (c) (string-prefix-p "Primus" c)) cands))
             (beta (seq-find (lambda (c) (string-prefix-p "Beta" c)) cands))
             (alias-note (vulpea-select-candidate-note alias)))
        (should (equal (vulpea-note-id alias-note) "id-a"))
        (should (equal (vulpea-note-title alias-note) "Primus"))
        (should (equal (vulpea-note-primary-title alias-note) "Alpha"))
        (should (equal (vulpea-note-id (vulpea-select-candidate-note beta))
                       "id-b"))
        (should-not (vulpea-select-candidate-note "free text"))))))

;;; Frontend API

(ert-deftest vulpea-select-cache-candidate-path ()
  "`vulpea-select-candidate-path' answers without reading the note."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (let* ((cands (vulpea-select-cache-candidates))
             (alias (seq-find (lambda (c) (string-prefix-p "Primus" c)) cands))
             (beta (seq-find (lambda (c) (string-prefix-p "Beta" c)) cands)))
        (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                   (lambda (&rest _) (error "Should not read the note"))))
          (should (equal (vulpea-select-candidate-path alias) "/tmp/a.org"))
          (should (equal (vulpea-select-candidate-path beta) "/tmp/b.org")))
        (should-not (vulpea-select-candidate-path "free text"))
        ;; uncached candidates carry their note
        (let ((uncached (car (vulpea-select-cache-test--uncached))))
          (should (equal (vulpea-select-candidate-path uncached)
                         (vulpea-note-path
                          (vulpea-select-candidate-note uncached)))))))))

;;; Selection

(ert-deftest vulpea-select-cache-find-returns-same-note ()
  "`vulpea-find' through the cache visits the note the uncached path would."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (dolist (target '("Beta" "Primus" "Alpha heading"))
        (let (cached uncached)
          (cl-letf (((symbol-function 'completing-read)
                     (vulpea-select-cache-test--pick target))
                    ((symbol-function 'vulpea-visit)
                     (lambda (note &optional _) (setq cached note))))
            (vulpea-find))
          (cl-letf (((symbol-function 'completing-read)
                     (vulpea-select-cache-test--pick target))
                    ((symbol-function 'vulpea-visit)
                     (lambda (note &optional _) (setq uncached note))))
            (vulpea-find :candidates-fn #'vulpea-db-query))
          (should cached)
          (should (equal cached uncached))))
      ;; the cache was used, not bypassed
      (should vulpea-select--cache))))

(ert-deftest vulpea-select-cache-find-creates-unmatched-input ()
  "Input matching no candidate still goes to the create function."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (let (created)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "Brand new"))
                  ((symbol-function 'vulpea-visit) #'ignore))
          (vulpea-find :create-fn (lambda (title &optional _)
                                    (setq created title)
                                    nil)))
        (should (equal created "Brand new"))
        (should vulpea-select--cache)))))

(ert-deftest vulpea-select-cache-insert-uses-cache ()
  "`vulpea-insert' shares the cache and inserts the picked note."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (with-temp-buffer
        (org-mode)
        (cl-letf (((symbol-function 'completing-read)
                   (vulpea-select-cache-test--pick "First")))
          (vulpea-insert))
        (should (string-match-p "\\[\\[id:id-a\\]\\[First\\]\\]"
                                (buffer-string))))
      (should vulpea-select--cache))))

;;; Bypass

(ert-deftest vulpea-select-cache-bypassed-for-custom-selection ()
  "Filters, custom sources, dynamic context and nil setting skip the cache."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (let ((cases
             (list
              (lambda () (vulpea-find :filter-fn (lambda (n)
                                                   (equal (vulpea-note-id n)
                                                          "id-b"))))
              (lambda () (vulpea-find :candidates-fn
                                      (lambda (_) (vulpea-db-query-by-ids
                                                   '("id-b")))))
              (lambda () (vulpea-find :expand-aliases nil))
              (lambda () (let ((vulpea-find-default-filter
                                (lambda (n) (equal (vulpea-note-id n) "id-b"))))
                           (vulpea-find)))
              (lambda () (let ((vulpea-find-default-candidates-source
                                (lambda (f) (vulpea-db-query f))))
                           (vulpea-find)))
              (lambda () (let ((vulpea-select-dyncontext-fn (lambda (_) 1)))
                           (vulpea-find)))
              (lambda () (let ((vulpea-select-cache nil))
                           (vulpea-find)))
              (lambda () (let ((vulpea-insert-default-filter #'always))
                           (with-temp-buffer (org-mode) (vulpea-insert)))))))
        (dolist (case cases)
          (vulpea-select-cache-drop)
          (cl-letf (((symbol-function 'completing-read)
                     (vulpea-select-cache-test--pick "Beta"))
                    ((symbol-function 'vulpea-visit) #'ignore))
            (funcall case))
          (should-not vulpea-select--cache))))))

(defun vulpea-select-cache-test--override (prompt notes &rest _)
  "Stand in for a frontend overriding `vulpea-select-from'.
Return the first of NOTES, recording PROMPT."
  (ignore prompt)
  (car notes))

(defun vulpea-select-cache-test--override-cached (&rest _)
  "Stand in for a frontend overriding `vulpea-select-from-cache'."
  (make-vulpea-note :id "from-override" :title "Override" :level 0))

(ert-deftest vulpea-select-cache-bypassed-when-select-from-advised ()
  "An advised `vulpea-select-from' keeps receiving `vulpea-find' calls.
Frontends such as consult-vulpea override it; serving the cache would
silently skip them."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (let (visited)
        (advice-add 'vulpea-select-from :override
                    #'vulpea-select-cache-test--override)
        (unwind-protect
            (cl-letf (((symbol-function 'vulpea-visit)
                       (lambda (note &optional _) (setq visited note)))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "picked by plain completion")))
              (vulpea-find)
              (should (vulpea-note-p visited))
              (should (member (vulpea-note-id visited) '("id-a" "id-a-h" "id-b")))
              (should-not vulpea-select--cache)
              ;; a frontend that also overrides the cached entry point
              ;; opts into the cache
              (advice-add 'vulpea-select-from-cache :override
                          #'vulpea-select-cache-test--override-cached)
              (unwind-protect
                  (progn
                    (vulpea-find)
                    (should (equal (vulpea-note-id visited) "from-override")))
                (advice-remove 'vulpea-select-from-cache
                               #'vulpea-select-cache-test--override-cached)))
          (advice-remove 'vulpea-select-from
                         #'vulpea-select-cache-test--override))))))

(ert-deftest vulpea-select-cache-filter-still-filters ()
  "A default filter keeps working: the cache does not leak other notes."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      ;; warm the cache first
      (vulpea-select-cache-candidates)
      (let ((vulpea-find-default-filter
             (lambda (n) (equal (vulpea-note-id n) "id-b")))
            seen)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p collection &rest _)
                     (setq seen (all-completions "" collection))
                     (car seen)))
                  ((symbol-function 'vulpea-visit) #'ignore))
          (vulpea-find))
        (should (= 1 (length seen)))))))

;;; Invalidation

(ert-deftest vulpea-select-cache-settings-change-invalidates ()
  "Changing describe or annotate settings rebuilds the candidates."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (vulpea-select-cache-candidates)
      (let ((vulpea-select-describe-fn
             (lambda (n) (upcase (vulpea-note-title n)))))
        (should (seq-find (lambda (c) (string-prefix-p "BETA" c))
                          (vulpea-select-cache-candidates)))
        (vulpea-select-cache-test--same-as-uncached))
      (let ((vulpea-select-annotate-fn nil))
        (vulpea-select-cache-test--same-as-uncached))
      (let ((vulpea-select-match-ids nil))
        (vulpea-select-cache-test--same-as-uncached))
      (let ((vulpea-select-annotate-matchable nil))
        (vulpea-select-cache-test--same-as-uncached))
      (vulpea-select-cache-test--same-as-uncached))))

(ert-deftest vulpea-select-cache-new-database-invalidates ()
  "A different database connection starts from scratch."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (vulpea-select-cache-candidates))
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-test--insert-test-note "id-z" "Zeta")
      (should (equal (mapcar #'substring-no-properties
                             (vulpea-select-cache-candidates))
                     (mapcar #'substring-no-properties
                             (vulpea-select-cache-test--uncached))))
      (vulpea-select-cache-test--same-as-uncached))))

(ert-deftest vulpea-select-cache-db-clear-invalidates ()
  "Clearing the database drops the cache."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (vulpea-select-cache-candidates)
      (vulpea-db-clear)
      (should-not (vulpea-select-cache-candidates)))))

(ert-deftest vulpea-select-cache-drop-command ()
  "`vulpea-select-cache-drop' forgets everything."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-db
      (vulpea-db)
      (vulpea-select-cache-test--insert-fixture)
      (vulpea-select-cache-candidates)
      (should vulpea-select--cache)
      (vulpea-select-cache-drop)
      (should-not vulpea-select--cache))))

;;; Incremental updates

(ert-deftest vulpea-select-cache-follows-file-edits ()
  "Editing, adding and deleting files updates the cache incrementally."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-notes-dir
      (let ((a (expand-file-name "a.org" root))
            (b (expand-file-name "b.org" root)))
        (vulpea-select-cache-test--write a "id-a" "Alpha")
        (vulpea-db-update-file a)
        (vulpea-select-cache-candidates)
        (let ((cache vulpea-select--cache))
          ;; edit
          (vulpea-select-cache-test--write a "id-a" "Alpha renamed")
          (vulpea-db-update-file a)
          (vulpea-select-cache-test--same-as-uncached)
          (should (seq-find (lambda (c) (string-prefix-p "Alpha renamed" c))
                            (vulpea-select-cache-candidates)))
          ;; add
          (vulpea-select-cache-test--write b "id-b" "Beta")
          (vulpea-db-update-file b)
          (vulpea-select-cache-test--same-as-uncached)
          (should (= 2 (length (vulpea-select-cache-candidates))))
          ;; delete
          (delete-file a)
          (vulpea-db--forget-file a)
          (vulpea-select-cache-test--same-as-uncached)
          (should (= 1 (length (vulpea-select-cache-candidates))))
          ;; all of that without a full rebuild
          (should (eq cache vulpea-select--cache)))))))

(ert-deftest vulpea-select-cache-follows-moved-ids ()
  "An id moving to another file is not listed twice."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-notes-dir
      (let ((a (expand-file-name "a.org" root))
            (b (expand-file-name "b.org" root)))
        (vulpea-select-cache-test--write a "id-a" "Alpha")
        (vulpea-db-update-file a)
        (vulpea-select-cache-candidates)
        ;; the note moves to b; b is indexed before a is forgotten
        (delete-file a)
        (vulpea-select-cache-test--write b "id-a" "Alpha moved")
        (vulpea-db-update-file b)
        (should (equal (mapcar #'substring-no-properties
                               (vulpea-select-cache-candidates))
                       (list (concat "Alpha moved id-a"))))
        ;; forgetting a later must not drop the moved note
        (vulpea-db--forget-file a)
        (vulpea-select-cache-test--same-as-uncached)
        (should (= 1 (length (vulpea-select-cache-candidates))))))))

(ert-deftest vulpea-select-cache-follows-worker-results ()
  "A result written by the extraction worker reaches the cache."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-notes-dir
      (let ((a (expand-file-name "a.org" root)))
        (vulpea-select-cache-test--write a "id-a" "Alpha")
        (vulpea-db-update-file a)
        (vulpea-select-cache-candidates)
        (let ((cache vulpea-select--cache))
          ;; the worker writes the database itself, then replies
          (vulpea-select-cache-test--write a "id-a" "Alpha from worker")
          (emacsql (vulpea-db) [:delete :from notes :where (= path $s1)]
                   (vulpea-db-normalize-path a))
          (vulpea-test--insert-test-note "id-a" "Alpha from worker"
                                         :path (vulpea-db-normalize-path a))
          (let ((attrs (file-attributes a)))
            (vulpea-db-worker--dispatch
             `(written ,a "hash"
                       ,(float-time (file-attribute-modification-time attrs))
                       ,(file-attribute-size attrs)
                       1 ("id-a") nil nil)))
          (should (equal (mapcar #'substring-no-properties
                                 (vulpea-select-cache-candidates))
                         (list "Alpha from worker id-a")))
          (should (eq cache vulpea-select--cache)))))))

(defmacro vulpea-select-cache-test--from-other-connection (&rest body)
  "Run BODY writing through a second connection to the same database.
Nothing is announced on `vulpea-db-updated-functions', like a write
made by another Emacs or by the extraction worker."
  (declare (indent 0))
  `(let ((main vulpea-db--connection)
         (other (emacsql-sqlite-builtin vulpea-db-location)))
     (unwind-protect
         (let ((vulpea-db--connection other)
               (vulpea-db-updated-functions nil))
           ,@body)
       (emacsql-close other)
       (setq vulpea-db--connection main))))

(ert-deftest vulpea-select-cache-follows-other-connections ()
  "Writes by another connection reach the cache, though never announced."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-notes-dir
      (let ((a (expand-file-name "a.org" root))
            (b (expand-file-name "b.org" root))
            (c (expand-file-name "c.org" root)))
        (vulpea-select-cache-test--write a "id-a" "Alpha")
        (vulpea-select-cache-test--write b "id-b" "Beta")
        (vulpea-db-update-file a)
        (vulpea-db-update-file b)
        (vulpea-select-cache-candidates)
        (let ((cache vulpea-select--cache))
          ;; edit, add and delete, all in another session
          (vulpea-select-cache-test--write a "id-a" "Alpha renamed")
          (vulpea-select-cache-test--write c "id-c" "Gamma")
          (delete-file b)
          (vulpea-select-cache-test--from-other-connection
            (vulpea-db-update-file a)
            (vulpea-db-update-file c)
            (vulpea-db--forget-file b))
          ;; this session's watcher finds nothing to do: the stored
          ;; hashes already match the files
          (should-not (vulpea-db-sync--update-file-if-changed a))
          (vulpea-select-cache-test--same-as-uncached)
          (should (equal (sort (mapcar #'substring-no-properties
                                       (vulpea-select-cache-candidates))
                               #'string<)
                         '("Alpha renamed id-a" "Gamma id-c")))
          ;; patched, not rebuilt
          (should (eq cache vulpea-select--cache))
          ;; nothing changed since: the next open does not look again
          (cl-letf (((symbol-function 'vulpea-select--cache-diff-stamps)
                     (lambda (&rest _) (error "Should not diff"))))
            (vulpea-select-cache-candidates)))))))

(ert-deftest vulpea-select-cache-follows-lost-worker-reply ()
  "A worker commit whose reply was lost still reaches the cache.
The retry finds the file unchanged and answers `stamped', which
announces nothing."
  (vulpea-select-cache-test--with-cache
    (vulpea-test--with-temp-notes-dir
      (let ((a (expand-file-name "a.org" root)))
        (vulpea-select-cache-test--write a "id-a" "Alpha")
        (vulpea-db-update-file a)
        (vulpea-select-cache-candidates)
        (vulpea-select-cache-test--write a "id-a" "Alpha from worker")
        (vulpea-select-cache-test--from-other-connection
          (vulpea-db-update-file a))
        (vulpea-db-worker--dispatch `(stamped ,a ("id-a")))
        (should (equal (mapcar #'substring-no-properties
                               (vulpea-select-cache-candidates))
                       '("Alpha from worker id-a")))))))

(ert-deftest vulpea-select-cache-bulk-updates-rebuild ()
  "Past the pending threshold the cache rebuilds instead of patching."
  (vulpea-select-cache-test--with-cache
    (let ((vulpea-select-cache--pending-limit 2))
      (vulpea-test--with-temp-notes-dir
        (let ((files (mapcar (lambda (i) (expand-file-name
                                          (format "%d.org" i) root))
                             '(1 2 3 4))))
          (vulpea-select-cache-test--write (car files) "id-1" "N1")
          (vulpea-db-update-file (car files))
          (vulpea-select-cache-candidates)
          (let ((cache vulpea-select--cache)
                (i 1))
            (dolist (f files)
              (vulpea-select-cache-test--write f (format "id-%d" i)
                                               (format "Note %d" i))
              (vulpea-db-update-file f)
              (setq i (1+ i)))
            (vulpea-select-cache-test--same-as-uncached)
            (should (= 4 (length (vulpea-select-cache-candidates))))
            (should-not (eq cache vulpea-select--cache))))))))

(ert-deftest vulpea-select-cache-prewarm-in-chunks ()
  "The idle prewarm builds the same candidates in several steps."
  (vulpea-select-cache-test--with-cache
    (let ((vulpea-select-cache--chunk-size 1))
      (vulpea-test--with-temp-db
        (vulpea-db)
        (vulpea-select-cache-test--insert-fixture)
        (let ((steps 0))
          (cl-letf (((symbol-function 'run-with-idle-timer)
                     (lambda (_secs _repeat fn &rest args)
                       (setq steps (1+ steps))
                       (apply fn args)
                       nil))
                    ((symbol-function 'input-pending-p) #'always))
            (vulpea-select-cache-prewarm))
          (should (> steps 2))
          (should (vulpea-select--cache-complete-p))
          (vulpea-select-cache-test--same-as-uncached))))))

(provide 'vulpea-select-cache-test)
;;; vulpea-select-cache-test.el ends here
