;;; vulpea-journal.el --- Daily note interface for vulpea -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
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
;; Created: 25 Nov 2025
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;;; Commentary:
;;
;; vulpea-journal (also known as vulpea-zilnic) provides a day-centric
;; interface for daily note workflows. It creates a focused workspace
;; with today's journal note and contextual widgets.
;;
;; Main features:
;; - Daily note identification and navigation
;; - Two-window layout: org buffer + widgets buffer
;; - Extensible widget system (see vulpea-journal-widget.el)
;; - Calendar integration
;;
;; Quick start:
;;
;;   (require 'vulpea-journal)
;;   (setq vulpea-journal-directory "journal/")
;;   (global-set-key (kbd "C-c j") #'vulpea-journal)
;;
;;; Code:

(require 'vulpea)
(require 'vulpea-db)
(require 'vulpea-db-query)
(require 'calendar)

(defvar vulpea-directory)

(declare-function vulpea-journal-widget--render-widget-list "vulpea-journal-widget")

;;; Customization

(defgroup vulpea-journal nil
  "Daily note interface for vulpea."
  :group 'vulpea)

(defcustom vulpea-journal-default-template
  '(:file-name "journal/%Y%m%d.org"
    :title "%Y-%m-%d %A"
    :tags ("journal"))
  "Default template for journal notes.

Can be a plist or a function taking DATE and returning a plist.

Required keys:
- `:file-name' - strftime format for file path (relative to
  `vulpea-directory')
- `:title' - strftime format for note title
- `:tags' - List of tags (first tag identifies journal notes)

Optional keys (same as `vulpea-create-default-template'):
- `:head' - Header content after #+title
- `:body' - Note body content
- `:properties' - Alist for property drawer
- `:meta' - Alist for metadata
- `:context' - Plist for template variables

Note on template syntax:

  `:file-name' and `:title' use strftime format (e.g., %Y%m%d)
  because they must be expanded for the TARGET DATE, not current
  time. When you open journal for Nov 25, the file should be
  20241125.org regardless of today's date.

  Other keys (`:head', `:body', etc.) use vulpea's %<format> syntax
  and are expanded by `vulpea-create' at note creation time.

Example:

  (setq vulpea-journal-default-template
        \\='(:file-name \"journal/%Y%m%d.org\"
          :title \"%Y-%m-%d %A\"
          :tags (\"journal\" \"daily\")
          :head \"#+created: %<[%Y-%m-%d]>\"
          :body \"* Morning\\n\\n* Evening\\n\"))

Or as a function for dynamic configuration:

  (setq vulpea-journal-default-template
        (lambda (date)
          (list :file-name (format-time-string \"journal/%Y%m%d.org\" date)
                :title (format-time-string \"%Y-%m-%d %A\" date)
                :tags \\='(\"journal\"))))"
  :type '(choice (plist :key-type symbol :value-type sexp)
                 function)
  :group 'vulpea-journal)

(defcustom vulpea-journal-window-ratio 0.5
  "Ratio of window width for the daily note buffer.
The widgets buffer takes the remaining space."
  :type 'float
  :group 'vulpea-journal)

(defcustom vulpea-journal-widgets-buffer-name "*vulpea-journal*"
  "Name for the journal widgets buffer."
  :type 'string
  :group 'vulpea-journal)

;;; Faces

(defface vulpea-journal-header-face
  '((t :inherit org-document-title :height 1.3))
  "Face for journal view header."
  :group 'vulpea-journal)

(defface vulpea-journal-date-face
  '((t :inherit org-date))
  "Face for dates in journal view."
  :group 'vulpea-journal)

(defface vulpea-journal-widget-title-face
  '((t :inherit org-level-2 :height 1.1))
  "Face for widget titles."
  :group 'vulpea-journal)

(defface vulpea-journal-meta-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for metadata."
  :group 'vulpea-journal)

;;; Template Resolution

(defun vulpea-journal--get-template (date)
  "Get resolved template plist for DATE."
  (if (functionp vulpea-journal-default-template)
      (funcall vulpea-journal-default-template date)
    vulpea-journal-default-template))

(defun vulpea-journal--get-tag ()
  "Get the primary journal tag from template.
Uses current time for template resolution."
  (car (plist-get (vulpea-journal--get-template (current-time)) :tags)))

;;; Variables

(defvar-local vulpea-journal--current-date nil
  "Current date displayed in journal view.")

(defvar-local vulpea-journal--note-buffer nil
  "Associated note buffer for widgets buffer.")

(defvar-local vulpea-journal--widgets-buffer nil
  "Associated widgets buffer for note buffer.")

;;; Journal Directory

(defun vulpea-journal--ensure-directory (file)
  "Ensure directory for FILE exists."
  (let ((dir (file-name-directory file)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;; Note Identification

(defun vulpea-journal-note-p (note)
  "Return non-nil if NOTE is a journal note."
  (and note
       (member (vulpea-journal--get-tag) (vulpea-note-tags note))))

(defun vulpea-journal--file-for-date (date)
  "Return file path for journal note on DATE."
  (let* ((tpl (vulpea-journal--get-template date))
         (file-fmt (plist-get tpl :file-name)))
    (expand-file-name
     (format-time-string file-fmt date)
     vulpea-directory)))

(defun vulpea-journal--title-for-date (date)
  "Return title for journal note on DATE."
  (let* ((tpl (vulpea-journal--get-template date))
         (title-fmt (plist-get tpl :title)))
    (format-time-string title-fmt date)))

(defun vulpea-journal--date-from-note (note)
  "Extract date from journal NOTE.
Returns time value or nil if not a journal note."
  (when (vulpea-journal-note-p note)
    (let* ((path (vulpea-note-path note))
           (filename (file-name-nondirectory path)))
      ;; Try to parse date from filename
      (when (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" filename)
        (let ((year (string-to-number (match-string 1 filename)))
              (month (string-to-number (match-string 2 filename)))
              (day (string-to-number (match-string 3 filename))))
          (encode-time 0 0 0 day month year))))))

;;; Note Lookup

(defun vulpea-journal-find-note (date)
  "Find existing journal note for DATE, or nil."
  (let ((file (vulpea-journal--file-for-date date)))
    (when (file-exists-p file)
      (car (vulpea-db-query
            (lambda (note)
              (and (string= (vulpea-note-path note) file)
                   (= (vulpea-note-level note) 0))))))))

(defun vulpea-journal-today-note ()
  "Get today's journal note, creating if needed."
  (vulpea-journal-note (current-time)))

(defun vulpea-journal-note (date)
  "Get journal note for DATE, creating if needed."
  (or (vulpea-journal-find-note date)
      (vulpea-journal--create-note date)))

(defun vulpea-journal--create-note (date)
  "Create a new journal note for DATE."
  (let* ((tpl (vulpea-journal--get-template date))
         (file (vulpea-journal--file-for-date date))
         (title (vulpea-journal--title-for-date date))
         (id (org-id-new)))
    (vulpea-journal--ensure-directory file)
    (vulpea-create
     title
     file
     :id id
     :tags (plist-get tpl :tags)
     :head (plist-get tpl :head)
     :body (plist-get tpl :body)
     :properties (plist-get tpl :properties)
     :meta (plist-get tpl :meta)
     :context (plist-get tpl :context))
    ;; Return the created note
    (vulpea-db-get-by-id id)))

;;; Date Queries

(defun vulpea-journal-dates-in-range (start end)
  "Return list of dates with journal entries between START and END."
  (let ((notes (vulpea-db-query
                (lambda (note)
                  (when-let ((date (vulpea-journal--date-from-note note)))
                    (and (time-less-p start date)
                         (time-less-p date end)))))))
    (delq nil (mapcar #'vulpea-journal--date-from-note notes))))

(defun vulpea-journal-dates-in-month (month year)
  "Return list of dates with journal entries in MONTH of YEAR."
  (let ((start (encode-time 0 0 0 1 month year))
        (end (encode-time 0 0 0 1 (if (= month 12) 1 (1+ month))
                          (if (= month 12) (1+ year) year))))
    (vulpea-journal-dates-in-range start end)))

(defun vulpea-journal-all-dates ()
  "Return list of all dates with journal entries."
  (let ((notes (vulpea-db-query #'vulpea-journal-note-p)))
    (sort (delq nil (mapcar #'vulpea-journal--date-from-note notes))
          #'time-less-p)))

;;; Two-Window Layout

(defun vulpea-journal--get-note-buffer (note)
  "Get or create buffer for NOTE."
  (find-file-noselect (vulpea-note-path note)))

(defun vulpea-journal--get-widgets-buffer (date)
  "Get or create widgets buffer for DATE."
  (let ((buffer (get-buffer-create vulpea-journal-widgets-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vulpea-journal-widgets-mode)
        (vulpea-journal-widgets-mode))
      (setq-local vulpea-journal--current-date date))
    buffer))

(defun vulpea-journal--link-buffers (note-buffer widgets-buffer date)
  "Link NOTE-BUFFER and WIDGETS-BUFFER for DATE."
  (with-current-buffer note-buffer
    (setq-local vulpea-journal--widgets-buffer widgets-buffer)
    (setq-local vulpea-journal--current-date date))
  (with-current-buffer widgets-buffer
    (setq-local vulpea-journal--note-buffer note-buffer)
    (setq-local vulpea-journal--current-date date)))

(defun vulpea-journal--setup-windows (note-buffer widgets-buffer)
  "Set up two-window layout with NOTE-BUFFER and WIDGETS-BUFFER."
  (delete-other-windows)
  (switch-to-buffer note-buffer)
  (let* ((total-width (window-total-width))
         (note-width (floor (* total-width vulpea-journal-window-ratio)))
         (widgets-window (split-window-right note-width)))
    (set-window-buffer widgets-window widgets-buffer)
    ;; Return to note buffer
    (select-window (get-buffer-window note-buffer))))

;;; Interactive Commands

;;;###autoload
(defun vulpea-journal (&optional date)
  "Open journal view for DATE (defaults to today)."
  (interactive)
  (let* ((date (or date (current-time)))
         (note (vulpea-journal-note date))
         (note-buffer (vulpea-journal--get-note-buffer note))
         (widgets-buffer (vulpea-journal--get-widgets-buffer date)))
    (vulpea-journal--link-buffers note-buffer widgets-buffer date)
    (vulpea-journal--setup-windows note-buffer widgets-buffer)
    (vulpea-journal-refresh-widgets)))

;;;###autoload
(defun vulpea-journal-today ()
  "Open journal view for today."
  (interactive)
  (vulpea-journal (current-time)))

;;;###autoload
(defun vulpea-journal-date (date)
  "Open journal view for DATE.
When called interactively, prompt for date."
  (interactive (list (vulpea-journal--read-date "Journal date: ")))
  (vulpea-journal date))

(defun vulpea-journal--read-date (prompt)
  "Read a date from user with PROMPT."
  (let* ((org-read-date-prefer-future nil)
         (date-string (org-read-date nil nil nil prompt)))
    (org-time-string-to-time date-string)))

;;;###autoload
(defun vulpea-journal-goto-date (date)
  "Navigate current journal view to DATE."
  (interactive (list (vulpea-journal--read-date "Go to date: ")))
  (when-let ((widgets-buffer (or vulpea-journal--widgets-buffer
                                 (get-buffer vulpea-journal-widgets-buffer-name))))
    (let* ((note (vulpea-journal-note date))
           (note-buffer (vulpea-journal--get-note-buffer note)))
      ;; Update note buffer
      (when-let ((note-window (get-buffer-window vulpea-journal--note-buffer)))
        (set-window-buffer note-window note-buffer))
      ;; Update state and refresh
      (vulpea-journal--link-buffers note-buffer widgets-buffer date)
      (vulpea-journal-refresh-widgets))))

(defun vulpea-journal-yesterday ()
  "Navigate to yesterday's journal."
  (interactive)
  (let ((yesterday (time-subtract (or vulpea-journal--current-date (current-time))
                                  (days-to-time 1))))
    (vulpea-journal-goto-date yesterday)))

(defun vulpea-journal-tomorrow ()
  "Navigate to tomorrow's journal."
  (interactive)
  (let ((tomorrow (time-add (or vulpea-journal--current-date (current-time))
                            (days-to-time 1))))
    (vulpea-journal-goto-date tomorrow)))

(defun vulpea-journal-goto-today ()
  "Navigate to today's journal."
  (interactive)
  (vulpea-journal-goto-date (current-time)))

;;; Widgets Mode

(defvar vulpea-journal-widgets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'vulpea-journal-refresh-widgets)
    (define-key map (kbd "r") #'vulpea-journal-refresh-widgets)
    (define-key map (kbd "q") #'vulpea-journal-quit)
    (define-key map (kbd "N") #'vulpea-journal-tomorrow)
    (define-key map (kbd "P") #'vulpea-journal-yesterday)
    (define-key map (kbd "d") #'vulpea-journal-goto-date)
    (define-key map (kbd ".") #'vulpea-journal-goto-today)
    (define-key map (kbd "n") #'vulpea-journal-next-widget)
    (define-key map (kbd "p") #'vulpea-journal-previous-widget)
    (define-key map (kbd "TAB") #'vulpea-journal-toggle-widget)
    (define-key map (kbd "RET") #'vulpea-journal-action-at-point)
    (define-key map (kbd "e") #'vulpea-journal-edit-note)
    map)
  "Keymap for `vulpea-journal-widgets-mode'.")

(define-derived-mode vulpea-journal-widgets-mode special-mode "Journal"
  "Major mode for vulpea journal widgets buffer.

\\{vulpea-journal-widgets-mode-map}"
  :group 'vulpea-journal
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t))

;;; Widget Rendering (stub - implemented in vulpea-journal-widget.el)

(defvar vulpea-journal-widgets nil
  "List of enabled widgets to display.
Each element is a symbol naming a widget.")

(defun vulpea-journal-refresh-widgets ()
  "Refresh all widgets in the current journal view."
  (interactive)
  (when-let ((buffer (or vulpea-journal--widgets-buffer
                         (and (derived-mode-p 'vulpea-journal-widgets-mode)
                              (current-buffer))
                         (get-buffer vulpea-journal-widgets-buffer-name))))
    (with-current-buffer buffer
      (vulpea-journal--render-widgets))))

(defun vulpea-journal--render-widgets (&optional use-cache)
  "Render all widgets in current buffer.
If USE-CACHE is non-nil, use cached query results."
  (let ((inhibit-read-only t)
        (date vulpea-journal--current-date))
    (erase-buffer)
    ;; Header
    (insert (propertize (format-time-string "Journal: %Y-%m-%d %A" date)
                        'face 'vulpea-journal-header-face))
    (insert "\n")
    (vulpea-journal--render-nav-bar date)
    (insert "\n\n")
    ;; Widgets (to be implemented by vulpea-journal-widget.el)
    (if (and (boundp 'vulpea-journal-widgets) vulpea-journal-widgets)
        (vulpea-journal--render-widget-list date use-cache)
      (insert (propertize "No widgets configured.\n"
                          'face 'vulpea-journal-meta-face)
              (propertize "See `vulpea-journal-widgets' to enable widgets.\n"
                          'face 'vulpea-journal-meta-face)))))

(defun vulpea-journal--render-nav-bar (_date)
  "Render navigation bar for DATE."
  (insert (propertize "[" 'face 'shadow))
  (insert-text-button "◂ Yesterday"
                      'action (lambda (_) (vulpea-journal-yesterday))
                      'follow-link t)
  (insert (propertize "]" 'face 'shadow))
  (insert "  ")
  (insert (propertize "[" 'face 'shadow))
  (insert-text-button "Today ⟳"
                      'action (lambda (_) (vulpea-journal-goto-today))
                      'follow-link t)
  (insert (propertize "]" 'face 'shadow))
  (insert "  ")
  (insert (propertize "[" 'face 'shadow))
  (insert-text-button "Tomorrow ▸"
                      'action (lambda (_) (vulpea-journal-tomorrow))
                      'follow-link t)
  (insert (propertize "]" 'face 'shadow))
  (insert "     ")
  (insert (propertize "[" 'face 'shadow))
  (insert-text-button "r"
                      'action (lambda (_) (vulpea-journal-refresh-widgets))
                      'follow-link t
                      'help-echo "Refresh widgets")
  (insert (propertize "] Refresh" 'face 'shadow))
  (insert "  ")
  (insert (propertize "[" 'face 'shadow))
  (insert-text-button "q"
                      'action (lambda (_) (vulpea-journal-quit))
                      'follow-link t
                      'help-echo "Quit journal view")
  (insert (propertize "] Quit" 'face 'shadow)))

(defun vulpea-journal--render-widget-list (date &optional use-cache)
  "Render enabled widgets for DATE.
If USE-CACHE is non-nil, use cached query results.
Calls the actual implementation from vulpea-journal-widget.el if loaded."
  (if (featurep 'vulpea-journal-widget)
      ;; Widget module is loaded, let it handle rendering
      (vulpea-journal-widget--render-widget-list date use-cache)
    ;; Stub when widget module not loaded
    (insert (propertize "Widgets will appear here.\n"
                        'face 'vulpea-journal-meta-face)
            (propertize "(Load vulpea-journal-widget for full functionality)\n"
                        'face 'vulpea-journal-meta-face))))

;;; Navigation Commands

(defun vulpea-journal-quit ()
  "Quit journal view and restore window configuration."
  (interactive)
  (when-let ((widgets-buffer (get-buffer vulpea-journal-widgets-buffer-name)))
    (when-let ((window (get-buffer-window widgets-buffer)))
      (delete-window window))
    (kill-buffer widgets-buffer)))

(defun vulpea-journal-edit-note ()
  "Switch to the note buffer for editing."
  (interactive)
  (when vulpea-journal--note-buffer
    (select-window (get-buffer-window vulpea-journal--note-buffer))))

(defun vulpea-journal-next-widget ()
  "Move to next widget."
  (interactive)
  ;; TODO: implement once widget system is ready
  (forward-paragraph))

(defun vulpea-journal-previous-widget ()
  "Move to previous widget."
  (interactive)
  ;; TODO: implement once widget system is ready
  (backward-paragraph))

(defun vulpea-journal-toggle-widget ()
  "Toggle expansion of widget at point."
  (interactive)
  ;; TODO: implement once widget system is ready
  (message "Toggle widget (not yet implemented)"))

(defun vulpea-journal-action-at-point ()
  "Execute action at point."
  (interactive)
  (if-let ((action (get-text-property (point) 'action)))
      (funcall action (get-text-property (point) 'action-data))
    (if-let ((button (button-at (point))))
        (button-activate button)
      (message "No action at point"))))

;;; Calendar Integration

(defface vulpea-journal-calendar-entry-face
  '((t :inherit bold :foreground "forest green"))
  "Face for calendar days that have journal entries."
  :group 'vulpea-journal)

(defun vulpea-journal-calendar-mark-entries ()
  "Mark days in calendar that have journal entries."
  (when (and (boundp 'displayed-month) (boundp 'displayed-year))
    (let ((entries (vulpea-journal-dates-in-month displayed-month displayed-year)))
      (dolist (date entries)
        (let* ((decoded (decode-time date))
               (day (decoded-time-day decoded))
               (month (decoded-time-month decoded))
               (year (decoded-time-year decoded)))
          (when (calendar-date-is-visible-p (list month day year))
            (calendar-mark-visible-date
             (list month day year)
             'vulpea-journal-calendar-entry-face)))))))

(defun vulpea-journal-calendar-open ()
  "Open journal for date at point in calendar."
  (interactive)
  (let ((date (calendar-cursor-to-date t)))
    (when date
      (vulpea-journal (encode-time 0 0 0
                                   (nth 1 date)   ; day
                                   (nth 0 date)   ; month
                                   (nth 2 date))) ; year
      )))

(defun vulpea-journal-calendar-open-view ()
  "Open journal view for date at point in calendar."
  (interactive)
  (let ((date (calendar-cursor-to-date t)))
    (when date
      (vulpea-journal (encode-time 0 0 0
                                   (nth 1 date)
                                   (nth 0 date)
                                   (nth 2 date))))))

;;;###autoload
(defun vulpea-journal-calendar-setup ()
  "Set up calendar integration for vulpea-journal.
Call this in your init file to enable calendar marks and keybindings."
  (add-hook 'calendar-today-visible-hook #'vulpea-journal-calendar-mark-entries)
  (add-hook 'calendar-today-invisible-hook #'vulpea-journal-calendar-mark-entries)
  ;; Add keybindings to calendar mode
  (with-eval-after-load 'calendar
    (define-key calendar-mode-map (kbd "j") #'vulpea-journal-calendar-open)
    (define-key calendar-mode-map (kbd "J") #'vulpea-journal-calendar-open-view)))

(provide 'vulpea-journal)
;;; vulpea-journal.el ends here
