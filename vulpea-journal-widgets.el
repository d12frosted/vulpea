;;; vulpea-journal-widgets.el --- Built-in widgets for vulpea-journal -*- lexical-binding: t; -*-
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
;; Built-in widgets for vulpea-journal:
;;
;; - `created-today' - Notes created on current date
;; - `links-to-today' - Notes linking to today's journal
;; - `previous-years' - Same date in previous years
;; - `calendar' - Compact month calendar
;;
;;; Code:

(require 'vulpea-journal)
(require 'vulpea-journal-widget)
(require 'vulpea-db-query)
(require 'calendar)

;;; Customization

(defgroup vulpea-journal-widgets nil
  "Built-in widgets for vulpea-journal."
  :group 'vulpea-journal)

;;; Widget: Created Today

(defcustom vulpea-journal-widget-created-today-exclude-journal t
  "If non-nil, exclude journal notes from Created Today widget."
  :type 'boolean
  :group 'vulpea-journal-widgets)

(defun vulpea-journal-widget-created-today-query (date)
  "Return notes created on DATE."
  (let ((date-str (format-time-string "%Y-%m-%d" date)))
    (vulpea-db-query
     (lambda (note)
       (and (= (vulpea-note-level note) 0)  ; file-level only
            (when-let ((created (cdr (assoc "CREATED" (vulpea-note-properties note)))))
              (string-prefix-p date-str created))
            (or (not vulpea-journal-widget-created-today-exclude-journal)
                (not (vulpea-journal-note-p note))))))))

(defun vulpea-journal-widget-created-today-render (note)
  "Render NOTE for created-today widget."
  (let* ((title (vulpea-note-title note))
         (tags (vulpea-note-tags note))
         (created (cdr (assoc "CREATED" (vulpea-note-properties note))))
         (time-str (if (and created (string-match "\\([0-9]+:[0-9]+\\)" created))
                       (match-string 1 created)
                     "     ")))
    (concat
     (propertize time-str 'face 'vulpea-journal-date-face)
     "  "
     (propertize title
                 'face 'link
                 'mouse-face 'highlight
                 'action #'vulpea-journal--visit-note
                 'action-data note
                 'help-echo "Open note")
     (when tags
       (concat "  "
               (propertize (string-join (mapcar (lambda (tag) (concat "#" tag)) tags) " ")
                           'face 'vulpea-journal-meta-face)))
     "\n")))

(defun vulpea-journal--visit-note (note)
  "Visit NOTE in other window."
  (when note
    (vulpea-visit note t)))

(vulpea-journal-define-widget created-today
  :title "Created Today"
  :order 20
  :query #'vulpea-journal-widget-created-today-query
  :render #'vulpea-journal-widget-created-today-render
  :empty-message "No notes created today")

;;; Widget: Links to Today

(defcustom vulpea-journal-widget-links-context-chars 80
  "Number of characters of context to show around links."
  :type 'integer
  :group 'vulpea-journal-widgets)

(defun vulpea-journal-widget-links-to-today-query (date)
  "Return notes linking to today's journal note for DATE."
  (when-let* ((today-note (vulpea-journal-find-note date))
              (today-id (vulpea-note-id today-note)))
    (vulpea-db-query-by-links-some (list today-id))))

(defun vulpea-journal-widget-links-to-today-render (note)
  "Render NOTE for links-to-today widget."
  (let* ((title (vulpea-note-title note))
         (tags (vulpea-note-tags note)))
    (concat
     (propertize title
                 'face 'link
                 'mouse-face 'highlight
                 'action #'vulpea-journal--visit-note
                 'action-data note
                 'help-echo "Open note")
     (when tags
       (concat "  "
               (propertize (string-join (mapcar (lambda (tag) (concat "#" tag)) tags) " ")
                           'face 'vulpea-journal-meta-face)))
     "\n")))

(vulpea-journal-define-widget links-to-today
  :title "Links to Today"
  :order 30
  :query #'vulpea-journal-widget-links-to-today-query
  :render #'vulpea-journal-widget-links-to-today-render
  :empty-message "No notes link to today")

;;; Widget: Previous Years

(defcustom vulpea-journal-widget-previous-years-count 5
  "Number of years to look back."
  :type 'integer
  :group 'vulpea-journal-widgets)

(defcustom vulpea-journal-widget-previous-years-preview-chars 200
  "Number of characters to show in preview."
  :type 'integer
  :group 'vulpea-journal-widgets)

(defcustom vulpea-journal-widget-previous-years-handle-leap-year t
  "If non-nil, handle leap year edge cases.
On Mar 1, include Feb 29 from leap years.
On Feb 29, include Feb 28 from non-leap years."
  :type 'boolean
  :group 'vulpea-journal-widgets)

(defcustom vulpea-journal-widget-previous-years-hide-drawers t
  "If non-nil, hide org drawers in preview."
  :type 'boolean
  :group 'vulpea-journal-widgets)

(defun vulpea-journal--dates-for-anniversary (month day year)
  "Get list of dates to check for anniversary on MONTH DAY in YEAR."
  (if (not vulpea-journal-widget-previous-years-handle-leap-year)
      (list (encode-time 0 0 0 day month year))
    (cond
     ;; March 1: include Feb 29 if leap year
     ((and (= month 3) (= day 1) (date-leap-year-p year))
      (list (encode-time 0 0 0 29 2 year)
            (encode-time 0 0 0 1 3 year)))
     ;; Feb 29: include Feb 28 if not leap year
     ((and (= month 2) (= day 29) (not (date-leap-year-p year)))
      (list (encode-time 0 0 0 28 2 year)))
     ;; Feb 28 in non-leap year viewing leap year
     ((and (= month 2) (= day 28)
           (not (date-leap-year-p (decoded-time-year (decode-time))))
           (date-leap-year-p year))
      (list (encode-time 0 0 0 28 2 year)
            (encode-time 0 0 0 29 2 year)))
     ;; Regular date
     (t (list (encode-time 0 0 0 day month year))))))

(defun vulpea-journal-widget-previous-years-query (date)
  "Return journal notes from same date in previous years for DATE."
  (let* ((decoded (decode-time date))
         (month (decoded-time-month decoded))
         (day (decoded-time-day decoded))
         (year (decoded-time-year decoded))
         (results nil))
    (dotimes (i vulpea-journal-widget-previous-years-count)
      (let* ((past-year (- year (1+ i)))
             (dates (vulpea-journal--dates-for-anniversary month day past-year)))
        (dolist (check-date dates)
          (when-let ((note (vulpea-journal-find-note check-date)))
            (push (list :date check-date
                        :years-ago (1+ i)
                        :note note)
                  results)))))
    (nreverse results)))

(defun vulpea-journal--strip-drawers (text)
  "Remove org drawers from TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*:[A-Z_]+:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" nil t)
      (replace-match ""))
    (buffer-string)))

(defun vulpea-journal--get-note-preview (note max-chars)
  "Get preview of NOTE content, up to MAX-CHARS."
  (when-let ((path (vulpea-note-path note)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path nil 0 (* max-chars 3))
        (goto-char (point-min))
        ;; Strip drawers if configured
        (when vulpea-journal-widget-previous-years-hide-drawers
          (let ((content (vulpea-journal--strip-drawers (buffer-string))))
            (erase-buffer)
            (insert content)
            (goto-char (point-min))))
        ;; Skip front matter (#+keywords)
        (while (and (not (eobp))
                    (looking-at "^\\(#\\+\\|$\\)"))
          (forward-line 1))
        (let ((start (point))
              (end (min (+ (point) max-chars) (point-max))))
          (when (< start end)
            (buffer-substring-no-properties start end)))))))

(defun vulpea-journal--indent-preview (text indent)
  "Indent each line of TEXT with INDENT string."
  (let ((lines (split-string text "\n" t)))
    (mapconcat (lambda (line)
                 (concat indent (string-trim-right line)))
               lines
               "\n")))

(defun vulpea-journal-widget-previous-years-render (entry)
  "Render ENTRY for previous-years widget."
  (let* ((date (plist-get entry :date))
         (years-ago (plist-get entry :years-ago))
         (note (plist-get entry :note))
         (date-str (format-time-string "%Y-%m-%d" date))
         (preview (vulpea-journal--get-note-preview
                   note
                   vulpea-journal-widget-previous-years-preview-chars)))
    (concat
     (propertize (format "▸ %s " date-str)
                 'face 'vulpea-journal-date-face
                 'action #'vulpea-journal--visit-note
                 'action-data note
                 'mouse-face 'highlight)
     (propertize (format "(%d year%s ago)"
                         years-ago
                         (if (= years-ago 1) "" "s"))
                 'face 'vulpea-journal-meta-face)
     "\n"
     (when preview
       (propertize (concat (vulpea-journal--indent-preview
                            (string-trim preview)
                            "    ")
                           "...\n")
                   'face 'font-lock-comment-face)))))

(vulpea-journal-define-widget previous-years
  :title "This Day in Previous Years"
  :order 40
  :default-collapsed nil
  :query #'vulpea-journal-widget-previous-years-query
  :render #'vulpea-journal-widget-previous-years-render
  :empty-message "No entries from previous years")

;;; Widget: Calendar

(defcustom vulpea-journal-widget-calendar-week-start 1
  "Day to start week on. 0 = Sunday, 1 = Monday."
  :type '(choice (const :tag "Sunday" 0)
                 (const :tag "Monday" 1))
  :group 'vulpea-journal-widgets)

(defface vulpea-journal-calendar-entry-face
  '((t :inherit bold))
  "Face for calendar days with journal entries."
  :group 'vulpea-journal-widgets)

(defface vulpea-journal-calendar-today-face
  '((t :inherit highlight :weight bold))
  "Face for today in calendar."
  :group 'vulpea-journal-widgets)

(defun vulpea-journal-widget-calendar-query (date)
  "Return calendar data for the month containing DATE.
Returns a single-item list for the widget system."
  (let* ((decoded (decode-time date))
         (month (decoded-time-month decoded))
         (year (decoded-time-year decoded))
         (entries (vulpea-journal-dates-in-month month year))
         (entry-days (mapcar (lambda (d)
                               (decoded-time-day (decode-time d)))
                             entries)))
    ;; Wrap in list - widget system iterates over items
    (list (list :month month
                :year year
                :entry-days entry-days
                :current-date date))))

(defun vulpea-journal-widget-calendar-render (data)
  "Render calendar DATA."
  (let* ((month (plist-get data :month))
         (year (plist-get data :year))
         (entry-days (plist-get data :entry-days))
         (current-date (plist-get data :current-date))
         (today (decode-time))
         (today-day (decoded-time-day today))
         (today-month (decoded-time-month today))
         (today-year (decoded-time-year today))
         (current-decoded (decode-time current-date))
         (current-day (decoded-time-day current-decoded))
         ;; Calendar calculation
         (first-day (encode-time 0 0 0 1 month year))
         (first-dow (decoded-time-weekday (decode-time first-day)))
         ;; Adjust for week start
         (first-dow-adjusted (mod (- first-dow vulpea-journal-widget-calendar-week-start) 7))
         (days-in-month (calendar-last-day-of-month month year))
         (lines nil)
         (month-name (calendar-month-name month)))
    ;; Month header
    (push (concat "       "
                  (propertize (format "%s %d" month-name year)
                              'face 'bold)
                  "\n")
          lines)
    ;; Day headers (4 chars per column to match day cells)
    (let ((day-names (if (= vulpea-journal-widget-calendar-week-start 1)
                         '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su")
                       '("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"))))
      (push (concat " " (mapconcat (lambda (d) (format " %s " d)) day-names "") "\n") lines))
    ;; Calendar grid
    (let ((day 1)
          (week-line " "))
      ;; Initial padding (4 chars per empty slot)
      (dotimes (_ first-dow-adjusted)
        (setq week-line (concat week-line "    ")))
      ;; Days
      (while (<= day days-in-month)
        (let* ((is-today (and (= day today-day)
                              (= month today-month)
                              (= year today-year)))
               (is-current (= day current-day))
               (has-entry (member day entry-days))
               (day-str (format "%2d" day))
               (face (cond
                      (is-today 'vulpea-journal-calendar-today-face)
                      (has-entry 'vulpea-journal-calendar-entry-face)
                      (t nil)))
               (cell (if is-current
                         (concat "[" (propertize day-str 'face face) "]")
                       (concat " " (propertize day-str 'face face) " "))))
          ;; Make day clickable
          (setq cell (propertize cell
                                 'action #'vulpea-journal--calendar-goto-day
                                 'action-data (encode-time 0 0 0 day month year)
                                 'mouse-face 'highlight
                                 'help-echo (format "Open %s-%02d-%02d" year month day)))
          (setq week-line (concat week-line cell)))
        ;; End of week?
        (when (= (mod (+ first-dow-adjusted day) 7) 0)
          (push (concat week-line "\n") lines)
          (setq week-line " "))
        (setq day (1+ day)))
      ;; Final partial week
      (unless (string= week-line " ")
        (push (concat week-line "\n") lines)))
    ;; Return combined string
    (apply #'concat (nreverse lines))))

(defun vulpea-journal--calendar-goto-day (date)
  "Navigate journal to DATE."
  (vulpea-journal-goto-date date))

(vulpea-journal-define-widget calendar
  :title "Calendar"
  :order 10
  :collapsible t
  :default-collapsed nil
  :query #'vulpea-journal-widget-calendar-query
  :render #'vulpea-journal-widget-calendar-render
  :empty-message "")

;;; Default Widget Configuration

(defcustom vulpea-journal-widgets
  '(calendar created-today links-to-today previous-years)
  "List of widgets to display in journal view.
Widgets are displayed in the order specified here, but their
internal :order property determines final sort order.

Available built-in widgets:
- `calendar' - Compact month calendar
- `created-today' - Notes created on current date
- `links-to-today' - Notes linking to today's journal
- `previous-years' - Same date in previous years"
  :type '(repeat symbol)
  :group 'vulpea-journal)

(provide 'vulpea-journal-widgets)
;;; vulpea-journal-widgets.el ends here
