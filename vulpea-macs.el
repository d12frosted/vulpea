;;; vulpea-macs.el --- all things macros -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Dec 2020
;;
;; URL: https://github.com/d12frosted/vulpea
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

;;;###autoload
(defmacro vulpea-with-file (file &rest body)
  "Execute BODY in `org-mode' FILE."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

(provide 'vulpea-macs)
;;; vulpea-macs.el ends here
