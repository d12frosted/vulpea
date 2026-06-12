;;; tools/vulpea/config.el -*- lexical-binding: t; -*-

(use-package! vulpea
  ;; Load on first input after startup instead of blocking startup.
  ;; Vulpea derives its defaults from `org-directory' at load time, so
  ;; set `org-directory' (or `vulpea-db-sync-directories') in your
  ;; config.el if your notes live somewhere else.
  :defer t
  :after-call doom-first-input-hook
  :config
  (vulpea-db-autosync-mode +1))

(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("v" . "vulpea")
        :desc "Find note"              "f" #'vulpea-find
        :desc "Find backlink"          "b" #'vulpea-find-backlink
        :desc "Insert link"            "i" #'vulpea-insert
        :desc "Propagate title change" "p" #'vulpea-propagate-title-change
        :desc "Rename tag everywhere"  "r" #'vulpea-tags-batch-rename
        :desc "Full scan"              "s" #'vulpea-db-sync-full-scan
        :desc "Doctor"                 "d" #'vulpea-doctor)))
