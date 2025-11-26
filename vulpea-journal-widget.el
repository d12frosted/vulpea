;;; vulpea-journal-widget.el --- Widget system for vulpea-journal -*- lexical-binding: t; -*-
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
;; Widget system for vulpea-journal using ewoc.
;;
;; Widgets are display components that show information related to the
;; current journal date. Each widget has:
;; - A query function to fetch data
;; - A render function to display items
;; - Optional collapsible header
;; - Optional keymap for widget-specific actions
;;
;; Defining a widget:
;;
;;   (vulpea-journal-define-widget my-widget
;;     :title "My Widget"
;;     :query #'my-widget-query
;;     :render #'my-widget-render)
;;
;;; Code:

(require 'cl-lib)

;; Forward declaration - vulpea-journal.el loads this module
(declare-function vulpea-journal--render-widgets "vulpea-journal" (&optional use-cache))

;;; Widget Structure

(cl-defstruct (vulpea-journal-widget
               (:constructor vulpea-journal-widget-create)
               (:copier nil))
  "Definition of a journal widget."
  (name nil :documentation "Symbol: unique identifier")
  (title "" :documentation "String: display title")
  (order 50 :documentation "Number: display order (lower = earlier)")
  (collapsible t :documentation "Boolean: can be collapsed")
  (default-collapsed nil :documentation "Boolean: start collapsed")
  (query nil :documentation "Function: (date) -> data")
  (render nil :documentation "Function: (item) -> string")
  (empty-message "No items" :documentation "String: shown when query returns nil")
  (keymap nil :documentation "Keymap: widget-specific keybindings"))

;;; Widget Registry

(defvar vulpea-journal-widget-registry (make-hash-table :test 'eq)
  "Hash table of registered widgets, keyed by name symbol.")

(defun vulpea-journal-register-widget (widget)
  "Register WIDGET in the registry."
  (puthash (vulpea-journal-widget-name widget)
           widget
           vulpea-journal-widget-registry))

(defun vulpea-journal-get-widget (name)
  "Get widget by NAME from registry."
  (gethash name vulpea-journal-widget-registry))

;;; Widget Definition Macro

(defmacro vulpea-journal-define-widget (name &rest props)
  "Define a journal widget with NAME and PROPS.

PROPS is a plist with:
  :title        - Display title (string)
  :order        - Display order, lower = earlier (number, default 50)
  :collapsible  - Can be collapsed (boolean, default t)
  :default-collapsed - Start collapsed (boolean, default nil)
  :query        - Function (date) -> list of items
  :render       - Function (item) -> propertized string
  :empty-message - Message when no items (string)
  :keymap       - Widget-specific keymap"
  (declare (indent 1))
  (let ((title (plist-get props :title))
        (order (or (plist-get props :order) 50))
        (collapsible (if (plist-member props :collapsible)
                         (plist-get props :collapsible)
                       t))
        (default-collapsed (plist-get props :default-collapsed))
        (query (plist-get props :query))
        (render (plist-get props :render))
        (empty-message (or (plist-get props :empty-message) "No items"))
        (keymap (plist-get props :keymap)))
    `(vulpea-journal-register-widget
      (vulpea-journal-widget-create
       :name ',name
       :title ,title
       :order ,order
       :collapsible ,collapsible
       :default-collapsed ,default-collapsed
       :query ,query
       :render ,render
       :empty-message ,empty-message
       :keymap ,keymap))))

;;; Widget State

(defvar-local vulpea-journal--widget-states nil
  "Alist of (widget-name . collapsed-p) for current buffer.")

(defvar-local vulpea-journal--widget-positions nil
  "Alist of (widget-name . (start . end)) positions.")

(defvar-local vulpea-journal--widget-cache nil
  "Alist of (widget-name . query-result) for caching.
Populated on first render, reused on toggle, cleared on refresh.")

(defun vulpea-journal--widget-collapsed-p (name)
  "Return non-nil if widget NAME is collapsed."
  (if-let ((state (assq name vulpea-journal--widget-states)))
      (cdr state)
    (when-let ((widget (vulpea-journal-get-widget name)))
      (vulpea-journal-widget-default-collapsed widget))))

(defun vulpea-journal--set-widget-collapsed (name collapsed)
  "Set collapsed state of widget NAME to COLLAPSED."
  (if-let ((state (assq name vulpea-journal--widget-states)))
      (setcdr state collapsed)
    (push (cons name collapsed) vulpea-journal--widget-states)))

;;; Widget Rendering

(defun vulpea-journal--render-widget-header (widget count collapsed)
  "Render header for WIDGET with COUNT items.
COLLAPSED indicates current state."
  (let ((name (vulpea-journal-widget-name widget))
        (title (vulpea-journal-widget-title widget))
        (collapsible (vulpea-journal-widget-collapsible widget)))
    (concat
     ;; Collapse indicator
     (if collapsible
         (propertize (if collapsed "▸ " "▾ ")
                     'face 'bold
                     'widget-name name
                     'action #'vulpea-journal--toggle-widget-action
                     'mouse-face 'highlight
                     'help-echo "Click to toggle")
       "  ")
     ;; Title with count
     (propertize (format "%s" title)
                 'face 'vulpea-journal-widget-title-face
                 'widget-name name)
     (when (and count (> count 0))
       (propertize (format " (%d)" count)
                   'face 'vulpea-journal-meta-face))
     "\n")))

(defun vulpea-journal--toggle-widget-action (data)
  "Toggle widget specified by DATA."
  (when-let ((name (or data (get-text-property (point) 'widget-name))))
    (vulpea-journal-toggle-widget-by-name name)))

(defun vulpea-journal-toggle-widget-by-name (name)
  "Toggle collapsed state of widget NAME."
  (let ((collapsed (not (vulpea-journal--widget-collapsed-p name))))
    (vulpea-journal--set-widget-collapsed name collapsed)
    (vulpea-journal--rerender-widgets)))

(defun vulpea-journal--render-single-widget (widget date &optional use-cache)
  "Render WIDGET for DATE in current buffer.
If USE-CACHE is non-nil, use cached query results.
Returns (start . end) positions."
  (let* ((name (vulpea-journal-widget-name widget))
         (query-fn (vulpea-journal-widget-query widget))
         (render-fn (vulpea-journal-widget-render widget))
         (empty-msg (vulpea-journal-widget-empty-message widget))
         (collapsed (vulpea-journal--widget-collapsed-p name))
         (start (point))
         ;; Use cache or query
         (cached (assq name vulpea-journal--widget-cache))
         (data (cond
                ;; Use cached data if available and requested
                ((and use-cache cached) (cdr cached))
                ;; Otherwise query
                (query-fn
                 (let ((result (funcall query-fn date)))
                   ;; Store in cache
                   (if cached
                       (setcdr cached result)
                     (push (cons name result) vulpea-journal--widget-cache))
                   result))
                (t nil)))
         (count (length data)))
    ;; Header
    (insert (vulpea-journal--render-widget-header widget count collapsed))
    ;; Content (if not collapsed)
    (unless collapsed
      (if (null data)
          (insert (propertize (concat "  " empty-msg "\n")
                              'face 'vulpea-journal-meta-face))
        ;; Render items directly
        (dolist (item data)
          (let ((rendered (funcall render-fn item)))
            (insert "  " rendered)))))
    (insert "\n")
    ;; Store positions
    (let ((end (point)))
      (if-let ((existing (assq name vulpea-journal--widget-positions)))
          (setcdr existing (cons start end))
        (push (cons name (cons start end)) vulpea-journal--widget-positions))
      (cons start end))))

;;; Main Render Function

(defun vulpea-journal-widget--render-widget-list (date &optional use-cache)
  "Render all enabled widgets for DATE.
If USE-CACHE is non-nil, use cached query results instead of re-querying.
Called by vulpea-journal.el when widget module is loaded."
  ;; Clear widget positions for this render
  (setq vulpea-journal--widget-positions nil)
  ;; Clear cache if not using it (fresh query)
  (unless use-cache
    (setq vulpea-journal--widget-cache nil))
  ;; Get enabled widgets, sorted by order
  (let ((widgets (vulpea-journal--get-enabled-widgets)))
    (if (null widgets)
        (insert (propertize "No widgets enabled.\n"
                            'face 'vulpea-journal-meta-face))
      (dolist (widget widgets)
        (vulpea-journal--render-single-widget widget date use-cache)))))

(defun vulpea-journal--get-enabled-widgets ()
  "Return list of enabled widget structs, sorted by order."
  (let ((enabled vulpea-journal-widgets)
        (result nil))
    (dolist (name enabled)
      (when-let ((widget (vulpea-journal-get-widget name)))
        (push widget result)))
    (sort result (lambda (a b)
                   (< (vulpea-journal-widget-order a)
                      (vulpea-journal-widget-order b))))))

;; This function is called by vulpea-journal.el to render widgets.
;; The stub in vulpea-journal.el checks for this feature to know if
;; the widget system is loaded.

(defun vulpea-journal--rerender-widgets ()
  "Re-render widgets using cached data, preserving cursor position.
This is used for toggle operations where we don't need to re-query."
  (when-let ((buffer (or (bound-and-true-p vulpea-journal--widgets-buffer)
                         (and (derived-mode-p 'vulpea-journal-widgets-mode)
                              (current-buffer)))))
    (with-current-buffer buffer
      ;; Save position info
      (let ((saved-line (line-number-at-pos))
            (saved-col (current-column)))
        ;; Re-render with cache (vulpea-journal--render-widgets handles the rest)
        (vulpea-journal--render-widgets t)  ; use-cache = t
        ;; Restore position
        (goto-char (point-min))
        (forward-line (1- saved-line))
        (move-to-column saved-col)))))

;;; Widget Navigation

(defun vulpea-journal-next-widget ()
  "Move to next widget header."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (when (get-text-property (point) 'widget-name)
          (setq found (point)))
        (forward-line 1)))
    (when found
      (goto-char found))))

(defun vulpea-journal-previous-widget ()
  "Move to previous widget header."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (when (get-text-property (point) 'widget-name)
          (setq found (point)))
        (forward-line -1)))
    (when found
      (goto-char found))))

(defun vulpea-journal-toggle-widget ()
  "Toggle widget at point."
  (interactive)
  (if-let ((name (get-text-property (point) 'widget-name)))
      (vulpea-journal-toggle-widget-by-name name)
    (message "No widget at point")))

;; Override navigation stubs from vulpea-journal.el
(with-eval-after-load 'vulpea-journal
  (define-key vulpea-journal-widgets-mode-map (kbd "n") #'vulpea-journal-next-widget)
  (define-key vulpea-journal-widgets-mode-map (kbd "p") #'vulpea-journal-previous-widget)
  (define-key vulpea-journal-widgets-mode-map (kbd "TAB") #'vulpea-journal-toggle-widget))

(provide 'vulpea-journal-widget)
;;; vulpea-journal-widget.el ends here
