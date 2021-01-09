;;; vulpea.el --- Vulpea is a fox -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Package-Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "1.2.3") (s "1.12"))
;;
;; Created: 08 Jan 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vulpea is a fox.
;;
;;; Code:

(require 'org-roam)
(require 'vulpea-meta)
(require 'vulpea-db)

(defun vulpea-select (prompt &optional initial-prompt completions filter-fn)
  "Select a note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

COMPLETIONS is a list of completions to be used instead of
`org-roam--get-title-path-completions`.

FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.  See
`vulpea--get-title-path-completions' for details."
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (or completions
                          (vulpea--get-title-path-completions)))
         (completions (if filter-fn
                          (seq-filter filter-fn completions)
                        completions))
         (title-with-tags (org-roam-completion--completing-read (concat prompt ": ") completions
                                                                :initial-input initial-prompt))
         (res (cdr (assoc title-with-tags completions)))
         (id (vulpea-db-get-id-by-file (plist-get res :path))))
    (if id
        (plist-put res :id id)
      (plist-put res :title title-with-tags))))

(defun vulpea--get-title-path-completions ()
  "Return an alist for completion.

The car is the displayed title for completion, and the cdr
contains all the funny stuff."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (org-roam--prepend-tag-string title tags))
              (v (list :path file-path :title title :tags tags)))
          (push (cons k v) completions))))))

(provide 'vulpea)
;;; vulpea.el ends here