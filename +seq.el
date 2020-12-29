;;; +seq.el --- Extra seq functions -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 29 Dec 2020
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun +seq-singleton (list)
  "Return the only element of the LIST.

Return nil, if the LIST contains more than one element."
  (cond ((listp list) (pcase list
                        (`(,l . nil) l)))
	      (t list)))

(provide '+seq)
;;; +seq.el ends here
