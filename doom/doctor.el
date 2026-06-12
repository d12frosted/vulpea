;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/vulpea/doctor.el

(unless (executable-find "fswatch")
  (warn! "Couldn't find fswatch. Vulpea will detect external file changes (git, cloud sync) via polling, which is slower. If fswatch is installed, run 'doom env' to refresh Emacs's PATH."))

(unless (executable-find "fd")
  (warn! "Couldn't find fd. Vulpea's directory scans will fall back to find, which is much slower on large note collections. If fd is installed, run 'doom env' to refresh Emacs's PATH."))
