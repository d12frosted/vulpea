;;; vulpea-schema-flymake-test.el --- Tests -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for the in-buffer schema flymake backend (`vulpea-schema-flymake').
;;
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vulpea-schema-flymake)
(require 'vulpea-schema)
(require 'vulpea-note)
(require 'vulpea-db-extract)

(defmacro vulpea-flymake-test--with (content &rest body)
  "Run BODY in a buffer visiting a temp org file of CONTENT, fresh registry."
  (declare (indent 1))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq))
         (file (make-temp-file "vulpea-flymake-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (let ((buf (find-file-noselect file)))
             (unwind-protect
                 (with-current-buffer buf
                   (goto-char (point-min))
                   ,@body)
               (kill-buffer buf))))
       (when (file-exists-p file) (delete-file file)))))

(defun vulpea-flymake-test--wine ()
  "Define a wine schema requiring a name and constraining colour."
  (vulpea-schema-define 'wine
    :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
    :fields '((:key "name" :required t)
              (:key "colour" :type symbol :one-of (red white rose)))))

(defun vulpea-flymake-test--on-line-p (pos prefix)
  "Return non-nil when POS is on a line starting with PREFIX."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (looking-at-p (regexp-quote prefix))))

(ert-deftest vulpea-schema-flymake-collect-file-level ()
  "A file-level note's violations become diagnostics at sensible regions."
  (vulpea-flymake-test--with
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: My Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
    (vulpea-flymake-test--wine)
    (let* ((ds (vulpea-schema-flymake--collect))
           (msgs (mapcar (lambda (d) (nth 3 d)) ds)))
      (should (= (length ds) 2))
      (should (cl-some (lambda (m) (string-match-p "name" m)) msgs))
      (should (cl-some (lambda (m) (string-match-p "colour" m)) msgs))
      ;; the colour diagnostic sits on the colour meta line
      (let ((cd (cl-find-if (lambda (d) (string-match-p "colour" (nth 3 d))) ds)))
        (should (vulpea-flymake-test--on-line-p (nth 0 cd) "- colour"))))))

(ert-deftest vulpea-schema-flymake-collect-conformant ()
  "A conformant note yields no diagnostics."
  (vulpea-flymake-test--with
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: My Wine\n#+filetags: :wine:\n\n- name :: Chablis\n- colour :: red\n"
    (vulpea-flymake-test--wine)
    (should-not (vulpea-schema-flymake--collect))))

(ert-deftest vulpea-schema-flymake-collect-no-schema ()
  "A note no schema applies to yields no diagnostics."
  (vulpea-flymake-test--with
      ":PROPERTIES:\n:ID: b1\n:END:\n#+title: Beer\n#+filetags: :beer:\n"
    (vulpea-flymake-test--wine)
    (should-not (vulpea-schema-flymake--collect))))

(ert-deftest vulpea-schema-flymake-collect-heading-level ()
  "A heading note's violations become diagnostics scoped to the heading."
  (vulpea-flymake-test--with
      (concat ":PROPERTIES:\n:ID: file1\n:END:\n#+title: File\n\n"
              "* Wine Entry :wine:\n:PROPERTIES:\n:ID: h1\n:END:\n\n- colour :: blue\n")
    (vulpea-flymake-test--wine)
    (let* ((ds (vulpea-schema-flymake--collect))
           (msgs (mapcar (lambda (d) (nth 3 d)) ds)))
      ;; only the heading note carries the wine tag; the file note does not
      (should (= (length ds) 2))
      (should (cl-some (lambda (m) (string-match-p "name" m)) msgs))
      (let ((cd (cl-find-if (lambda (d) (string-match-p "colour" (nth 3 d))) ds)))
        (should (vulpea-flymake-test--on-line-p (nth 0 cd) "- colour"))))))

(ert-deftest vulpea-schema-flymake-backend-reports-diagnostics ()
  "The backend hands flymake real diagnostic objects."
  (vulpea-flymake-test--with
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: My Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
    (vulpea-flymake-test--wine)
    (let (reported)
      (vulpea-schema-flymake (lambda (ds &rest _) (setq reported ds)))
      (should (= (length reported) 2))
      (should (cl-every (lambda (d) (stringp (flymake-diagnostic-text d))) reported))
      (should (cl-some (lambda (d) (string-match-p "colour" (flymake-diagnostic-text d)))
                       reported)))))

(ert-deftest vulpea-schema-flymake-mode-registers-backend ()
  "Enabling the mode registers the backend buffer-locally; disabling removes it."
  (vulpea-flymake-test--with
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: T\n#+filetags: :wine:\n"
    (cl-letf (((symbol-function 'flymake-mode) #'ignore))
      (vulpea-schema-flymake-mode 1)
      (should (memq #'vulpea-schema-flymake flymake-diagnostic-functions))
      (vulpea-schema-flymake-mode -1)
      (should-not (memq #'vulpea-schema-flymake flymake-diagnostic-functions)))))

(provide 'vulpea-schema-flymake-test)
;;; vulpea-schema-flymake-test.el ends here
