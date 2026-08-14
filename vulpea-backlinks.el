;;; vulpea-backlinks.el --- Backlinks buffer -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Pavel Popov <pavel@vio.com>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
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
;; Created: 13 Aug 2026
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; A side buffer answering "what points here?" for the note at point:
;; the notes linking to it, and - since a mention without a link is the
;; same question one step earlier - the unlinked mentions of it.
;;
;; Each backlink is a section carrying the source note's title, the
;; outline path of the heading the link sits under, and a preview of
;; that heading's text; RET visits the source, `o' shows it without
;; leaving the buffer.  Unlinked references are grouped by file, and
;; visiting one runs `occur' over its mention lines, so a file
;; mentioning the note five times is one entry to review rather than
;; five to visit.  They arrive asynchronously (see
;; `vulpea-note-unlinked-mentions-async') and are cached per note, so
;; re-rendering does not re-run the search; `g' drops the note's cache
;; and searches again.
;;
;; The buffer is a `magit-section' one and inherits its bindings: TAB
;; and S-TAB cycle visibility, `g' reverts.
;;
;; Usage:
;;
;;   (require 'vulpea-backlinks)
;;   (global-set-key (kbd "C-c n l") #'vulpea-backlinks-toggle)
;;
;;   ;; optional: follow point between notes
;;   (vulpea-backlinks-follow-mode 1)
;;
;;   ;; optional: keep side-by-side splits sane while the sidebar is up
;;   (setq split-window-preferred-function
;;         #'vulpea-backlinks-split-window-sensibly)
;;
;; This is the only part of vulpea that needs `magit-section', which is
;; why it is a separate file: `require' it explicitly and nothing else
;; grows the dependency.
;;
;;; Code:

(require 'org)
(require 'subr-x)
(require 'magit-section)
(require 'vulpea)
(require 'vulpea-mentions)

(defgroup vulpea-backlinks nil
  "Backlinks buffer for Vulpea."
  :group 'vulpea)

(defcustom vulpea-backlinks-buffer-name "*vulpea-backlinks*"
  "Name of the backlinks buffer."
  :type 'string
  :group 'vulpea-backlinks)

(defcustom vulpea-backlinks-window-side 'right
  "Side of the frame the backlinks window is displayed on."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'vulpea-backlinks)

(defcustom vulpea-backlinks-window-width 0.33
  "Size of the backlinks window, as a fraction of the frame.

Its width when `vulpea-backlinks-window-side' is left or right,
its height when top or bottom."
  :type 'number
  :group 'vulpea-backlinks)

(defcustom vulpea-backlinks-solo-main-window nil
  "When non-nil, visiting a note clears the rest of the main area.

Opening the sidebar and visiting things from it then keep the
frame to two windows - the note and the sidebar.  When nil, the
window layout is left alone."
  :type 'boolean
  :group 'vulpea-backlinks)

(defcustom vulpea-backlinks-show-unlinked t
  "When non-nil, the buffer also lists unlinked mentions.

The mention search runs asynchronously over the whole collection
\(see `vulpea-note-unlinked-mentions-async'); set this to nil to
keep the buffer to backlinks alone."
  :type 'boolean
  :group 'vulpea-backlinks)

;;; State

(defvar-local vulpea-backlinks--id nil
  "Id of the note currently rendered in the buffer.")

(defvar-local vulpea-backlinks--mentions-cache nil
  "Alist of (ID . MENTIONS) holding finished mention searches.")

(defvar-local vulpea-backlinks--mentions-fetching nil
  "Ids whose mention search is still in flight.")

;;; Mode

(defun vulpea-backlinks-visit-thing ()
  "Visit the thing at point.

Bound in the buffer as a whole; every section remaps it to the
command that knows how to visit that kind of section."
  (interactive)
  (user-error "No thing at point to visit"))

(defun vulpea-backlinks-view-thing ()
  "Show the thing at point without leaving this buffer.

The counterpart of `vulpea-backlinks-visit-thing'; sections remap
it the same way."
  (interactive)
  (user-error "No thing at point to view"))

(defvar-keymap vulpea-backlinks-mode-map
  :doc "Parent keymap for `vulpea-backlinks-mode'.")

(define-derived-mode vulpea-backlinks-mode magit-section-mode "Vulpea-Backlinks"
  "Major mode for the Vulpea backlinks buffer."
  (setq-local font-lock-defaults nil))

;; Bindings live outside the mode definition so that reloading the
;; file re-applies them.
(set-keymap-parent vulpea-backlinks-mode-map magit-section-mode-map)
(keymap-set vulpea-backlinks-mode-map "C-<return>" #'vulpea-backlinks-visit-thing)
(keymap-set vulpea-backlinks-mode-map "C-m" #'vulpea-backlinks-visit-thing)
(keymap-set vulpea-backlinks-mode-map "<remap> <revert-buffer>" #'vulpea-backlinks-refresh)
(keymap-set vulpea-backlinks-mode-map "g" #'vulpea-backlinks-refresh)
(keymap-set vulpea-backlinks-mode-map "o" #'vulpea-backlinks-view-thing)

;;; Node section

(defvar-keymap vulpea-backlinks-node-map
  :doc "Keymap for backlink sections."
  :parent vulpea-backlinks-mode-map)

(keymap-set vulpea-backlinks-node-map
            "<remap> <vulpea-backlinks-visit-thing>" #'vulpea-backlinks-node-visit)
(keymap-set vulpea-backlinks-node-map
            "<remap> <vulpea-backlinks-view-thing>" #'vulpea-backlinks-node-view)

(defclass vulpea-backlinks-node-section (magit-section)
  ((keymap :initform 'vulpea-backlinks-node-map)
   (file :initform nil)
   (point :initform nil)))

(defun vulpea-backlinks-node-visit (&optional other-window)
  "Visit the note behind the backlink at point.

With OTHER-WINDOW, visit it in another window and leave the
window layout alone."
  (interactive "P")
  (let ((sec (magit-current-section)))
    (vulpea-backlinks--visit (oref sec file) (oref sec point) other-window)))

(defun vulpea-backlinks-node-view ()
  "Show the note behind the backlink at point, staying in this buffer."
  (interactive)
  (save-selected-window (vulpea-backlinks-node-visit)))

;;; Preview section

(defvar-keymap vulpea-backlinks-preview-map
  :doc "Keymap for preview sections."
  :parent vulpea-backlinks-mode-map)

(keymap-set vulpea-backlinks-preview-map
            "<remap> <vulpea-backlinks-visit-thing>" #'vulpea-backlinks-preview-visit)
(keymap-set vulpea-backlinks-preview-map
            "<remap> <vulpea-backlinks-view-thing>" #'vulpea-backlinks-preview-view)

(defclass vulpea-backlinks-preview-section (magit-section)
  ((keymap :initform 'vulpea-backlinks-preview-map)
   (file :initform nil)
   (point :initform nil)))

(defun vulpea-backlinks-preview-visit (&optional other-window)
  "Jump to the link the preview at point was taken from.

With OTHER-WINDOW, visit it in another window and leave the
window layout alone."
  (interactive "P")
  (let ((sec (magit-current-section)))
    (vulpea-backlinks--visit (oref sec file) (oref sec point) other-window)))

(defun vulpea-backlinks-preview-view ()
  "Show the link behind the preview at point, staying in this buffer."
  (interactive)
  (save-selected-window (vulpea-backlinks-preview-visit)))

;;; Mention section

(defvar-keymap vulpea-backlinks-mention-map
  :doc "Keymap for unlinked mention sections."
  :parent vulpea-backlinks-mode-map)

(keymap-set vulpea-backlinks-mention-map
            "<remap> <vulpea-backlinks-visit-thing>" #'vulpea-backlinks-mention-visit)
(keymap-set vulpea-backlinks-mention-map
            "<remap> <vulpea-backlinks-view-thing>" #'vulpea-backlinks-mention-view)

(defclass vulpea-backlinks-mention-section (magit-section)
  ((keymap :initform 'vulpea-backlinks-mention-map)
   (file :initform nil)
   (contexts :initform nil)))

(defun vulpea-backlinks--mention-lines-regexp (contexts)
  "Return a regexp matching exactly the mention lines CONTEXTS.

Searching for the note's terms again would also hit occurrences
the mention search itself excludes - inside Org links, on
metadata lines - and `occur' takes no per-line predicate, so the
known mention lines are matched by their full text instead."
  (concat "^[ \t]*\\(?:"
          (mapconcat #'regexp-quote
                     (delete-dups (copy-sequence contexts))
                     "\\|")
          "\\)[ \t]*$"))

(defun vulpea-backlinks-mention-visit (&optional keep-sidebar)
  "Visit the file mentioning the note, with an `occur' of its mentions.

A file usually mentions a note more than once, so the whole set is
presented at once rather than one position at a time: the file
opens with an `occur' buffer over its mention lines (clearing the
rest of the main area when `vulpea-backlinks-solo-main-window' is
non-nil).  With KEEP-SIDEBAR, the backlinks window stays where it
is."
  (interactive)
  (let* ((sec (magit-current-section))
         (file (oref sec file))
         (regexp (vulpea-backlinks--mention-lines-regexp (oref sec contexts))))
    (unless keep-sidebar
      (when-let* ((win (vulpea-backlinks--window)))
        (when (window-parent win) (delete-window win))))
    (pop-to-buffer (find-file-noselect file))
    (vulpea-backlinks--solo-main-window)
    (widen)
    (goto-char (point-min))
    (occur regexp 3)
    (when-let* ((win (get-buffer-window "*Occur*")))
      (select-window win))))

(defun vulpea-backlinks-mention-view ()
  "Show the mentioning file at point, staying in this buffer."
  (interactive)
  (save-selected-window (vulpea-backlinks-mention-visit :keep-sidebar)))

(defun vulpea-backlinks--visit (file point &optional other-window)
  "Visit FILE at POINT, widening and revealing the position.

When `vulpea-backlinks-solo-main-window' is non-nil the visited
buffer becomes the only window in the main area, so the sidebar
keeps its place instead of being pushed around by whatever else
was open.  With OTHER-WINDOW, use another window and leave the
layout alone.  Returns the buffer."
  (let ((buf (find-file-noselect file)))
    (if other-window
        (switch-to-buffer-other-window buf)
      (pop-to-buffer buf)
      (vulpea-backlinks--solo-main-window))
    (with-current-buffer buf
      (widen)
      (goto-char (or point (point-min)))
      (when (org-invisible-p) (vulpea--show-context)))
    buf))

;;; Preview contents

(defun vulpea-backlinks--context-at (file pt)
  "Return the context of position PT in FILE as (OLP . PREVIEW).

OLP is the outline path of the heading holding PT, that heading
included, whether or not it is a note of its own; nil when PT sits
at file level.  PREVIEW is the text under that heading, without
its planning line and property drawer.  Returns nil when FILE is
not readable - the database may still hold a row for a file that
is gone from disk."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (org-with-wide-buffer
       (goto-char pt)
       (let* ((olp (ignore-errors (org-get-outline-path t)))
              (beg (save-excursion
                     (org-back-to-heading-or-point-min t)
                     (when (org-at-heading-p) (forward-line))
                     (while (looking-at-p org-planning-line-re) (forward-line))
                     (when (looking-at-p org-property-drawer-re)
                       (re-search-forward org-property-drawer-re nil t)
                       (forward-line))
                     (point)))
              (end (save-excursion
                     (outline-next-heading)
                     (point))))
         (cons olp
               (string-trim (buffer-substring-no-properties beg end))))))))

(defun vulpea-backlinks--fontify (s)
  "Return S fontified as Org text."
  (if (fboundp 'org-fontify-like-in-org-mode)
      (org-fontify-like-in-org-mode s)
    s))

;;; Rendering

(defun vulpea-backlinks--olp-string (olp)
  "Format outline path OLP as \"a > b\", or \"Top\" when it is nil."
  (if olp
      (mapconcat #'org-link-display-format olp " > ")
    "Top"))

(defun vulpea-backlinks--insert-node (source pos)
  "Insert a backlink section for note SOURCE linking at POS.

When SOURCE's file is gone from disk - a stale database row - the
section says so instead of a preview."
  (let* ((file (vulpea-note-path source))
         (ctx (vulpea-backlinks--context-at file pos))
         (olp (car ctx))
         (preview (cdr ctx)))
    (magit-insert-section node (vulpea-backlinks-node-section (cons file pos))
      (magit-insert-heading
        (concat (propertize (vulpea-note-title source)
                            'font-lock-face 'org-link)
                (propertize (if ctx
                                (format " (%s)" (vulpea-backlinks--olp-string olp))
                              " (file is missing)")
                            'font-lock-face 'shadow)))
      (oset node file file)
      (oset node point pos)
      (when ctx
        (magit-insert-section pv (vulpea-backlinks-preview-section (cons file pos))
          (oset pv file file)
          (oset pv point pos)
          (insert (vulpea-backlinks--fontify preview) "\n\n"))))))

(defun vulpea-backlinks-ids (&optional note-or-id)
  "Return ids of the notes linking to NOTE-OR-ID.

NOTE-OR-ID is a `vulpea-note' or an id; when nil, the note at
point is used.  Only `id' links count.  Returns a list of unique
ids in unspecified order, nil when nothing links to the note or
no note could be determined."
  (when-let* ((id (cond ((vulpea-note-p note-or-id)
                         (vulpea-note-id note-or-id))
                        ((stringp note-or-id) note-or-id)
                        (t (org-entry-get nil "ID" t)))))
    (seq-uniq
     (mapcar (lambda (l) (plist-get l :source))
             (seq-filter (lambda (l) (string= (plist-get l :type) "id"))
                         (vulpea-db-query-links-to id))))))

(defun vulpea-backlinks--insert-backlinks (note)
  "Insert the backlinks section for NOTE, sorted by source title."
  (let* ((id (vulpea-note-id note))
         (links (seq-filter (lambda (l) (string= (plist-get l :type) "id"))
                            (vulpea-db-query-links-to id)))
         (backlinks
          (sort (delq nil
                      (mapcar (lambda (l)
                                (when-let* ((src (vulpea-db-get-by-id
                                                  (plist-get l :source))))
                                  (cons src (plist-get l :pos))))
                              links))
                (lambda (a b)
                  (string-lessp (downcase (vulpea-note-title (car a)))
                                (downcase (vulpea-note-title (car b))))))))
    (magit-insert-section (vulpea-backlinks-root)
      (magit-insert-heading
        (format "Backlinks (%d)" (length backlinks)))
      (pcase-dolist (`(,src . ,pos) backlinks)
        (vulpea-backlinks--insert-node src pos))
      (insert "\n"))))

(defun vulpea-backlinks--insert-mentions (note)
  "Insert the unlinked references section for NOTE.

One entry per mentioning file, carrying its occurrence count and
the mention lines themselves.  While the search is still running
the section says so and the buffer re-renders when it lands; a
failed search says so too, rather than posing as a note without
mentions."
  (let* ((id (vulpea-note-id note))
         (cell (assoc id vulpea-backlinks--mentions-cache)))
    (magit-insert-section (vulpea-backlinks-mentions)
      (cond
       ((stringp (cdr cell))
        (magit-insert-heading "Unlinked References (search failed)")
        (insert (propertize (format "  %s\n" (cdr cell))
                            'font-lock-face 'shadow))
        (insert "\n"))
       (cell
        (let ((mentions (cdr cell)))
          (magit-insert-heading (format "Unlinked References (%d)" (length mentions)))
          (pcase-dolist (`(,file . ,ms)
                         (seq-group-by (lambda (m) (plist-get m :path)) mentions))
            (let ((src (plist-get (car ms) :note)))
              (magit-insert-section sec (vulpea-backlinks-mention-section file)
                (magit-insert-heading
                  (concat (propertize (if src (vulpea-note-title src)
                                        (file-name-base file))
                                      'font-lock-face 'org-link)
                          (propertize (format " (%d)" (length ms))
                                      'font-lock-face 'shadow)))
                (oset sec file file)
                (oset sec contexts (mapcar (lambda (m) (plist-get m :context)) ms))
                (dolist (m ms)
                  (insert (propertize (format "  %s\n" (plist-get m :context))
                                      'font-lock-face 'shadow))))))
          (insert "\n")))
       (t
        (magit-insert-heading "Unlinked References (loading…)")
        (insert "\n")
        (vulpea-backlinks--fetch-mentions note))))))

(defun vulpea-backlinks--fetch-mentions (note)
  "Search unlinked mentions of NOTE and re-render once they arrive.

A search already in flight for the same note is not started
again.  A failed search caches the error string in place of the
mentions, so the buffer settles - and says the search failed -
instead of asking again on every render; a refresh drops the
cache and searches again.

Both outcomes are settled via a timer: the search may deliver its
result synchronously - no search terms, mention detection opted
out - and this function runs mid-render, inside an open
`magit-insert-section', where re-rendering must not start."
  (let ((id (vulpea-note-id note))
        (buf (current-buffer)))
    (unless (member id vulpea-backlinks--mentions-fetching)
      (push id vulpea-backlinks--mentions-fetching)
      (let ((settle
             (lambda (result)
               (run-at-time
                0 nil
                (lambda ()
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (setq vulpea-backlinks--mentions-fetching
                            (delete id vulpea-backlinks--mentions-fetching))
                      (push (cons id result) vulpea-backlinks--mentions-cache)
                      (when (equal vulpea-backlinks--id id)
                        (vulpea-backlinks--render id)))))))))
        (vulpea-note-unlinked-mentions-async
         note
         settle
         (lambda (err) (funcall settle (format "%s" err))))))))

(defun vulpea-backlinks--render (id)
  "Render the backlinks buffer for the note ID and return it.

Re-rendering the same note - a refresh, rather than a move to
another note - keeps point on the section it was on, which is why
every section carries a stable, unique value."
  (let ((buf (get-buffer-create vulpea-backlinks-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'vulpea-backlinks-mode)
        (vulpea-backlinks-mode))
      (let* ((same-note (equal vulpea-backlinks--id id))
             (old-ident (and same-note (magit-current-section)
                             (magit-section-ident (magit-current-section)))))
        (setq vulpea-backlinks--id id)
        (let ((inhibit-read-only t)
              (note (and id (vulpea-db-get-by-id id))))
          (erase-buffer)
          (if (not note)
              (insert (propertize "Point is not on a Vulpea note.\n"
                                  'font-lock-face 'shadow))
            (magit-insert-section (vulpea-backlinks-buffer)
              (magit-insert-heading
                (propertize (vulpea-note-title note)
                            'font-lock-face 'org-document-title))
              (insert "\n")
              (vulpea-backlinks--insert-backlinks note)
              (when vulpea-backlinks-show-unlinked
                (vulpea-backlinks--insert-mentions note)))))
        (when old-ident
          (if-let* ((section (magit-get-section old-ident)))
              (magit-section-goto section)
            (goto-char (point-min))))))
    buf))

;;; Commands

(defun vulpea-backlinks--id-at-point ()
  "Return the id of the note enclosing point, or nil."
  (when (derived-mode-p 'org-mode)
    (org-entry-get nil "ID" t)))

(defun vulpea-backlinks--window ()
  "Return the window showing the backlinks buffer, or nil."
  (get-buffer-window vulpea-backlinks-buffer-name))

(defun vulpea-backlinks-split-window-sensibly (&optional window)
  "Split WINDOW as `split-window-sensibly' would without a sidebar.

The sidebar narrows the main area, pushing WINDOW under
`split-width-threshold', and Emacs then stacks new windows
instead of placing them side by side.  Decide as if the side
windows were not there.

Not installed by anything here; set
`split-window-preferred-function' to it if you want that
behaviour."
  (let* ((side-width
          (apply #'+ (mapcar (lambda (w)
                               (if (window-parameter w 'window-side)
                                   (window-total-width w)
                                 0))
                             (window-list (window-frame window)))))
         (split-width-threshold
          (and split-width-threshold
               (max 0 (- split-width-threshold side-width)))))
    (split-window-sensibly window)))

(defun vulpea-backlinks--solo-main-window ()
  "Make the selected window the only one in the main area.

Does nothing unless `vulpea-backlinks-solo-main-window' is
non-nil.  `delete-other-windows' would take the side windows with
it, so only the other non-side windows are deleted."
  (when vulpea-backlinks-solo-main-window
    (dolist (w (window-list))
      (unless (or (eq w (selected-window))
                  (window-parameter w 'window-side))
        (delete-window w)))))

(defun vulpea-backlinks-refresh ()
  "Re-render the backlinks buffer.

Called from the backlinks buffer it keeps its current note;
anywhere else it takes the note at point.  The note's cached
mention search is dropped, so the refresh picks up mentions that
appeared since."
  (interactive)
  (let ((id (or (and (derived-mode-p 'vulpea-backlinks-mode)
                     vulpea-backlinks--id)
                (vulpea-backlinks--id-at-point))))
    (when-let* ((buf (and id (get-buffer vulpea-backlinks-buffer-name))))
      (with-current-buffer buf
        (setq vulpea-backlinks--mentions-cache
              (assoc-delete-all id vulpea-backlinks--mentions-cache))))
    (vulpea-backlinks--render id)))

;;;###autoload
(defun vulpea-backlinks-toggle ()
  "Toggle the backlinks window for the note at point.

When the window already shows that note it is closed; when it
shows another one it is retargeted."
  (interactive)
  (let ((id (vulpea-backlinks--id-at-point)))
    (if-let* ((win (vulpea-backlinks--window)))
        (if (equal id (buffer-local-value 'vulpea-backlinks--id
                                          (window-buffer win)))
            (delete-window win)
          (vulpea-backlinks--render id))
      (let ((buf (vulpea-backlinks--render id))
            (size (if (memq vulpea-backlinks-window-side '(top bottom))
                      'window-height
                    'window-width)))
        (vulpea-backlinks--solo-main-window)
        (display-buffer-in-side-window
         buf `((side . ,vulpea-backlinks-window-side)
               (,size . ,vulpea-backlinks-window-width)))))))

(defvar vulpea-backlinks--follow-last-id nil
  "Id the follow mode rendered last, so it re-renders only on a change.")

(defun vulpea-backlinks--maybe-follow ()
  "Re-render when point moved to another note and the buffer is visible."
  (when (and (vulpea-backlinks--window)
             (derived-mode-p 'org-mode))
    (let ((id (vulpea-backlinks--id-at-point)))
      (unless (equal id vulpea-backlinks--follow-last-id)
        (setq vulpea-backlinks--follow-last-id id)
        (vulpea-backlinks--render id)))))

;;;###autoload
(define-minor-mode vulpea-backlinks-follow-mode
  "Follow point: re-render the backlinks buffer as it moves between notes."
  :global t
  :group 'vulpea-backlinks
  (if vulpea-backlinks-follow-mode
      (add-hook 'post-command-hook #'vulpea-backlinks--maybe-follow)
    (remove-hook 'post-command-hook #'vulpea-backlinks--maybe-follow)))

(provide 'vulpea-backlinks)
;;; vulpea-backlinks.el ends here
