;;; vulpea-note.el --- Vulpea note definition  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2020-2021 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (org-roam "2.0.0") (s "1.12"))
;;
;; Created: 28 Feb 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Vulpea is a fox.
;;
;;; Code:

(require 'org-roam)

(cl-defstruct vulpea-note
  id
  path
  level
  title
  primary-title
  aliases
  tags)

(defun vulpea-note-from-node (node)
  "Convert NODE represented as `org-roam-node' to `vulpea-note'."
  (when (org-roam-node-title node)
    (make-vulpea-note
     :id (org-roam-node-id node)
     :path (org-roam-node-file node)
     :level (or (org-roam-node-level node) 0)
     :title (org-roam-node-title node)
     :aliases (org-roam-node-aliases node)
     :tags (org-roam-node-tags node))))

(provide 'vulpea-note)
;;; vulpea-note.el ends here
