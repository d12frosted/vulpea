;;; vulpea-select.el --- Note selection with completion -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2026 Boris Buliga  <boris@d12frosted.io>
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
;; Created: 11 Jul 2021
;;
;; URL: https://github.com/d12frosted/vulpea
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Functionality to select a note.
;;
;;; Code:

(require 'vulpea-utils)
(require 'vulpea-note)
(require 'vulpea-db)
(require 'vulpea-db-query)

(defgroup vulpea-select nil
  "Note selection and completion."
  :group 'vulpea)

(defcustom vulpea-select-describe-fn #'vulpea-note-title
  "Function to describe a note for completion.

Accepts a `vulpea-note'. Returns a `string'."
  :type 'function
  :group 'vulpea-select)

(defcustom vulpea-select-annotate-fn #'vulpea-select-annotate
  "Function to annotate a note for completion.

Accepts a `vulpea-note'. Returns a `string'.

The annotation is rendered in `completions-annotations', applied
underneath whatever faces the string already carries, so a section
that styles itself keeps its own face.

When nil, candidates are not annotated at all."
  :type '(choice (const :tag "No annotation" nil) function)
  :group 'vulpea-select)

(defcustom vulpea-select-match-ids t
  "When non-nil, note ids are matchable in selection completion.

Each candidate built by `vulpea-select-describe' carries the note
id as an invisible suffix: it is part of the candidate string, so
typing or pasting an id (or part of one) narrows completion to
that note, but it is hidden from display so it never clutters the
list. The id is kept in the matchable string itself rather than
shown only as an annotation, the same way tags and aliases are.

This makes ids interactive handles alongside titles in
`vulpea-find' and `vulpea-insert', which matters when ids are
structured and meaningful (person:lectia) while titles are
incidental or absent (see `vulpea-note-titled-p'). How much of an
id you must type to narrow depends on your `completion-styles', the
same as for tags and aliases: interior styles (substring, flex,
orderless) match an id anywhere in the candidate, while a strict
prefix style only matches from the start.

Set to nil to drop ids from matching, e.g. if opaque ids produce
surprising matches."
  :type 'boolean
  :group 'vulpea-select)


(defcustom vulpea-select-annotate-matchable t
  "If t, annotations are added directly in the candidate string.

When this is t, annotations from `vulpea-select-annotate-fn' are
concatenated to the candidate string. This has the advantage that
annotations can be matched during search.

When this is nil, annotations from `vulpea-select-annotate-fn' are added
through the `:annotation-function' property, which is a standard way to
add annotations for minibuffer completion. This has the advantage that
annotations are not part of the candidate string, which can be useful
for some integrations, such as add multiple annotators that can be
cycled using the marginalia package."
  :type 'boolean
  :group 'vulpea-select)


(defcustom vulpea-select-dyncontext-fn nil
  "Function computing a shared context for the current selection.

When non-nil, it is called once per selection with the list of
`vulpea-note' values being presented and may return any value - the
\"dynamic context\". That value is then passed as the second argument to
`vulpea-select-describe-fn' and `vulpea-select-annotate-fn', but only to
functions that accept a second argument.

The point is to compute expensive shared data once - for example a table
of backlink counts built with a single query (see
`vulpea-db-query-backlink-counts') - and reuse it across every candidate,
instead of recomputing it per candidate or capturing it from a wrapper
command. Describe and annotate functions that take only a NOTE argument
are unaffected."
  :type '(choice (const :tag "No shared context" nil) function)
  :group 'vulpea-select)

(defun vulpea-select--accepts-context-p (fn)
  "Return non-nil when FN can be called with a NOTE and a context argument."
  (let ((arity (func-arity fn)))
    (and (<= (car arity) 2)
         (or (eq (cdr arity) 'many)
             (>= (cdr arity) 2)))))

(defun vulpea-select--funcall (fn note context)
  "Call FN with NOTE, also passing CONTEXT when FN accepts a second argument."
  (if (vulpea-select--accepts-context-p fn)
      (funcall fn note context)
    (funcall fn note)))

(defun vulpea-select--annotation-face (annotation)
  "Return ANNOTATION with the annotation face applied under its own faces.

`completions-annotations' is applied as a base rather than across the
whole string, so a section that styles itself keeps its face while plain
text still looks like an annotation.  ANNOTATION is not modified."
  (let ((result (copy-sequence annotation)))
    (add-face-text-property 0 (length result) 'completions-annotations t result)
    result))

(defun vulpea-select-describe (note &optional context)
  "Describe a NOTE for completion.

CONTEXT is the optional shared value produced by
`vulpea-select-dyncontext-fn'. It is forwarded as the second argument to
`vulpea-select-describe-fn' and `vulpea-select-annotate-fn' when they
accept one.

When `vulpea-select-match-ids' is non-nil, the note id is appended
as an invisible, matchable suffix so an id can be typed or pasted
to narrow completion. The suffix is added here, around any custom
`vulpea-select-describe-fn', so it is present regardless of how
candidates are displayed.

The returned string carries text properties linking it back to its
data: `vulpea-note-id' (the id), `vulpea-note' (the NOTE itself) and
`vulpea-select-context' (the CONTEXT). Read them through
`vulpea-select-candidate-note' and `vulpea-select-candidate-context'
instead of `get-text-property'. They are what makes a candidate
string self-describing for code that only ever sees strings - a
`display-sort-function' in `completion-category-overrides', an
`annotation-function' added by an integration, an embark action."
  (let* ((id (vulpea-note-id note))
         (description-part
          (vulpea-select--funcall
           vulpea-select-describe-fn note context))
         (annotation-part
          (if (and vulpea-select-annotate-matchable
                   vulpea-select-annotate-fn)
              (vulpea-select--annotation-face
               (vulpea-select--funcall
                vulpea-select-annotate-fn note context))
            ""))
         (invisible-id-part
          (when (and vulpea-select-match-ids id)
            (propertize (concat " " id) 'invisible t))))
    (propertize (concat
                 description-part annotation-part invisible-id-part)
                'vulpea-note-id
                id
                'vulpea-note
                note
                'vulpea-select-context
                context)))

(defun vulpea-select-candidate-note (candidate)
  "Return the `vulpea-note' carried by CANDIDATE, or nil.

CANDIDATE is a completion candidate string built by
`vulpea-select-describe'. A string that is not such a candidate -
for example the user's free-form input naming a new note - yields
nil.

This is the supported way to get from a candidate string back to
its note in code that only receives strings, such as a
`display-sort-function' set through `completion-category-overrides'
for the `vulpea-note' category:

  (defun my-sort-by-created (candidates)
    (seq-sort-by
     (lambda (c)
       (or (vulpea-note-created-at (vulpea-select-candidate-note c))
           \"\"))
     #\\='string>
     candidates))

Candidates served from the candidate cache (see
`vulpea-select-cache') carry only the note id, so their note is
read from the database on demand.  A function that asks for the
note of every candidate, like the sort function above, pays for one
full read of the notes table per selection."
  (or (get-text-property 0 'vulpea-note candidate)
      (when-let* ((id (get-text-property 0 'vulpea-note-id candidate)))
        (vulpea-select--cached-note
         id (get-text-property 0 'vulpea-select-alias candidate)))))

(defun vulpea-select-candidate-context (candidate)
  "Return the dynamic context carried by CANDIDATE, or nil.

CANDIDATE is a completion candidate string built by
`vulpea-select-describe'. The context is the value produced by
`vulpea-select-dyncontext-fn' for the selection the candidate
belongs to; nil when the hook is unset or CANDIDATE is not a
candidate string. See `vulpea-select-candidate-note'."
  (get-text-property 0 'vulpea-select-context candidate))

(defun vulpea-select-annotate (note)
  "Annotate a NOTE for completion."
  (let* ((alias-str
          (if (vulpea-note-primary-title note)
              (concat "("
                      (vulpea-note-primary-title note)
                      ")")
            ""))
         (tags-str (mapconcat
                    (lambda (x) (concat "#" x))
                    (vulpea-note-tags note)
                    " "))
         (sections (seq-remove #'string-empty-p
                               (list alias-str
                                     tags-str))))
    (if (null sections)
        ""
      (concat " " (string-join sections " ")))))

(defun vulpea-select--create-annotate-wrapper (annotation-fn)
  "Return a wrapper function for ANNOTATION-FN.

Return a wrapper function that receives a candidate string and then
calls ANNOTATION-FN with the note and context extracted from the
candidate. The wrapper function is suitable for use as a completion
annotation."
  (lambda (candidate)
    (let ((note (vulpea-select-candidate-note candidate))
          (context (vulpea-select-candidate-context candidate)))
      (if note
          (vulpea-select--annotation-face
           (vulpea-select--funcall annotation-fn note context))
        ""))))

;;; Describe Functions

(defun vulpea-select-describe-outline (note)
  "Describe NOTE with outline path prefix.

Shows the parent heading hierarchy before the note title.
For example, if a note titled \"Task\" is under \"Projects > Work\",
this displays: \"Projects → Work → Task\".

File-level notes are displayed without prefix.

To use this function, set `vulpea-select-describe-fn':

  (setq vulpea-select-describe-fn
        #\\='vulpea-select-describe-outline)"
  (let ((outline-path (vulpea-note-outline-path note))
        (title (vulpea-note-title note)))
    (if outline-path
        (concat
         (propertize
          (concat (string-join outline-path " → ") " → ")
          'face 'completions-annotations)
         title)
      title)))

(defun vulpea-select-describe-outline-full (note)
  "Describe NOTE with file title and outline path prefix.

Shows the file title and parent heading hierarchy before the note title.
For example, if a note titled \"Task\" is in file \"My Notes\" under
heading \"Projects\", this displays: \"My Notes → Projects → Task\".

File-level notes show just the title (no prefix needed since
file-title equals the title).

To use this function, set `vulpea-select-describe-fn':

  (setq vulpea-select-describe-fn
        #\\='vulpea-select-describe-outline-full)"
  (let ((file-title (vulpea-note-file-title note))
        (outline-path (vulpea-note-outline-path note))
        (title (vulpea-note-title note))
        (level (vulpea-note-level note)))
    (if (and level (> level 0) file-title)
        (let ((full-path (cons file-title (or outline-path '()))))
          (concat
           (propertize
            (concat (string-join full-path " → ") " → ")
            'face 'completions-annotations)
           title))
      title)))

(cl-defun vulpea-select (prompt
                         &key
                         require-match
                         initial-prompt
                         filter-fn
                         expand-aliases)
  "Select a note.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

FILTER-FN is the function to apply on the candidates, which takes
as its argument a `vulpea-note'.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'."
  (let ((notes (vulpea-db-query filter-fn)))
    (vulpea-select-from
     prompt notes
     :require-match require-match
     :initial-prompt initial-prompt
     :expand-aliases expand-aliases)))

(defun vulpea-select--completion-table (completions)
  "Build a completion table over COMPLETIONS exposing the `vulpea-note' category.

If `vulpea-select-annotate-matchable' is nil and
`vulpea-select-annotate-fn' is set, then `annotation-function' is also
included in the metadata.

COMPLETIONS is an alist of (description . note), or a plain list of
candidate strings as served by the candidate cache. The table completes
like COMPLETIONS and reports a completion category of `vulpea-note',
so that completion UIs and integrations (marginalia, embark, consult)
can recognize and act on the candidates, and so that users can target
the category from `completion-category-overrides' (e.g. to set a
`display-sort-function'). The candidate strings carry their note id,
the note itself and the dynamic context as text properties (see
`vulpea-select-describe'); read them with
`vulpea-select-candidate-note' and `vulpea-select-candidate-context'."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata
          (category . vulpea-note)
          ,@(when (and (not vulpea-select-annotate-matchable)
                       vulpea-select-annotate-fn)
              `((annotation-function
                 .
                 ,(vulpea-select--create-annotate-wrapper
                   vulpea-select-annotate-fn)))))

      (complete-with-action action completions string predicate))))

(defun vulpea-select--completions (notes expand-aliases)
  "Return the completion alist of (CANDIDATE . NOTE) for NOTES.

When EXPAND-ALIASES is non-nil, every alias of a note gets its own
entry (see `vulpea-note-expand-aliases').  The dynamic context from
`vulpea-select-dyncontext-fn' is computed once over the expanded
notes and passed to `vulpea-select-describe'."
  (let* ((expanded-notes (if expand-aliases
                             (seq-mapcat #'vulpea-note-expand-aliases notes)
                           notes))
         (context (when vulpea-select-dyncontext-fn
                    (funcall vulpea-select-dyncontext-fn expanded-notes))))
    (seq-map (lambda (n)
               (cons (vulpea-select-describe n context) n))
             expanded-notes)))

(cl-defun vulpea-select-from (prompt
                              notes
                              &key
                              require-match
                              initial-prompt
                              expand-aliases)
  "Select a note from the list of NOTES.

Returns a selected `vulpea-note'. If `vulpea-note-id' is nil, it
means that user selected non-existing note.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'.

Point and the current buffer are restored after the prompt: a
completion preview (consult and friends) that jumps to a candidate
living in the current buffer must not redirect whatever the caller
does at point next (vulpea#491)."
  (let ((completions (vulpea-select--completions notes expand-aliases)))
    (let* ((note (save-excursion
                   (completing-read
                    (concat prompt ": ")
                    (vulpea-select--completion-table completions)
                    nil require-match initial-prompt))))
      (or (cdr (assoc note completions))
          (make-vulpea-note
           :title (substring-no-properties note)
           :level 0)))))

(cl-defun vulpea-select-multiple-from (prompt
                                       notes
                                       &key
                                       require-match
                                       initial-prompt
                                       expand-aliases
                                       select-fn)
  "Collect multiple elements from list of NOTES.

When REQUIRE-MATCH is non-nil, use may select only existing note.

PROMPT is a message to present.

INITIAL-PROMPT is the initial title prompt.

When EXPAND-ALIASES is non-nil, each note with aliases will appear
multiple times in the completion list - once for the original title
and once for each alias. When an alias is selected, the returned
note will have that alias as `vulpea-note-title' and the original
title stored in `vulpea-note-primary-title'.

It behaves the same as the following code

  (vulpea-utils-collect-while
    #\\='vulpea-select-from nil prompt notes
    :require-match require-match
    :initial-prompt initial-prompt
    :expand-aliases expand-aliases)

The only difference, it allows to select a single note only once, i.e.
the next prompt iteration doesn't contain already selected notes.

Optionally, an interactive SELECT-FN can be provided to be used instead
of `vulpea-select-from'. Signatures must match."
  (let (result
        value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value
              (funcall-interactively
               (or select-fn #'vulpea-select-from)
               (concat prompt " (C-g to stop)")
               notes
               :require-match require-match
               :initial-prompt initial-prompt
               :expand-aliases expand-aliases))
        (setq notes (--remove (string-equal (vulpea-note-id it)
                                            (vulpea-note-id value))
                              notes))
        (setq result (cons value result))))
    (setq quit-flag nil)
    (reverse result)))

;;; Candidate Cache

(defcustom vulpea-select-cache t
  "When non-nil, keep note selection candidates in memory.

`vulpea-find' and `vulpea-insert' then open without reading every
note from the database and describing it again: the finished
candidate strings are kept between selections and only the one
picked note is read in full.  The cache is built on the first
selection (or in idle time, see `vulpea-select-cache-prewarm') and
kept current file by file through `vulpea-db-updated-functions'.

Only the default selection is served from the cache: no FILTER-FN
or CANDIDATES-FN argument, `vulpea-find-default-filter' and
`vulpea-insert-default-filter' nil, the default candidate sources,
alias expansion on and `vulpea-select-dyncontext-fn' nil.  Anything
else takes the uncached path, and so does every selection while
`vulpea-select-from' is advised (see `vulpea-select-cache-usable-p').

Changing `vulpea-select-describe-fn', `vulpea-select-annotate-fn',
`vulpea-select-annotate-matchable' or `vulpea-select-match-ids'
rebuilds the cache on the next selection.

Staleness contract: describe and annotate functions must depend on
the note alone.  A candidate is rebuilt only when the file of its
note changes, so anything else a function shows - backlink counts,
relative time, state of other notes - stays as it was when the
candidate was built.  Use `vulpea-select-cache-drop' to force a
rebuild, or set this to nil if your candidates need such data.

Each candidate costs memory: roughly 110MB for 165k candidates."
  :type 'boolean
  :group 'vulpea-select)

(defcustom vulpea-select-cache-prewarm t
  "When non-nil, build the candidate cache in idle time.

The build starts after `vulpea-db-autosync-mode' is enabled and
Emacs has been idle for a moment.  It reads notes in small chunks
and stops as soon as there is input, so the first `vulpea-find' is
fast without blocking the editor.  Has no effect when
`vulpea-select-cache' is nil."
  :type 'boolean
  :group 'vulpea-select)

(defvar vulpea-select-cache--chunk-size 500
  "Number of notes read per step while prewarming the cache.")

(defvar vulpea-select-cache--pending-limit 1000
  "Number of changed files after which the cache is dropped.

Changed files are refreshed on the next selection.  Past this
many - a full re-index, a parser upgrade - a rebuild from scratch
is cheaper than patching, so the cache is dropped instead.")

(defvar vulpea-select-cache--bulk-note-threshold 64
  "Note lookups per selection after which all notes are read at once.
See `vulpea-select--cached-note'.")

(defvar vulpea-select-cache--prewarm-delay 2
  "Idle seconds before the prewarm starts or resumes.")

(cl-defstruct (vulpea-select--cache-state
               (:constructor vulpea-select--cache-state-create)
               (:copier nil))
  "In-memory selection candidates of one database."
  db
  location
  fingerprint
  ;; id -> (PATH . CANDIDATES), in load order
  (by-id (make-hash-table :test #'equal))
  ;; path -> ids
  (by-path (make-hash-table :test #'equal))
  ;; paths changed since the last selection
  (pending (make-hash-table :test #'equal))
  ;; flat candidate list, valid unless STALE
  candidates
  (stale t)
  ;; last rowid read, nil once every note is loaded
  (cursor 0))

(defvar vulpea-select--cache nil
  "The candidate cache, a `vulpea-select--cache-state', or nil.")

(defvar vulpea-select--cache-timer nil
  "Idle timer of the running prewarm.")

(defvar vulpea-select--note-memo nil
  "Notes read for cached candidates during the current selection.
A hash table from id to note, bound by `vulpea-select-from-cache'.")

(defvar vulpea-buffer-alias-property)

(defun vulpea-select--cache-fingerprint ()
  "Return the settings the cached candidate strings depend on."
  (list vulpea-select-describe-fn
        vulpea-select-annotate-fn
        vulpea-select-annotate-matchable
        vulpea-select-match-ids
        (bound-and-true-p vulpea-buffer-alias-property)))

(defun vulpea-select--advised-p (symbol)
  "Return non-nil when the function of SYMBOL carries advice."
  (catch 'advised
    (advice-mapc (lambda (&rest _) (throw 'advised t)) symbol)
    nil))

(defun vulpea-select-cache-usable-p ()
  "Return non-nil when the default selection may use the candidate cache.

Besides `vulpea-select-cache' and `vulpea-select-dyncontext-fn', this
respects completion frontends: one that advises `vulpea-select-from',
as consult-vulpea does to add previews, keeps receiving every
selection, unless it advises `vulpea-select-from-cache' as well, which
is how a frontend opts into the cache."
  (and vulpea-select-cache
       (null vulpea-select-dyncontext-fn)
       (or (not (vulpea-select--advised-p 'vulpea-select-from))
           (vulpea-select--advised-p 'vulpea-select-from-cache))))

(defun vulpea-select-cache-drop ()
  "Drop the note selection candidate cache.

The next `vulpea-find' or `vulpea-insert' builds it again.  Use it
after redefining a describe or annotate function, or whenever the
candidates show something the cache does not track (see
`vulpea-select-cache')."
  (interactive)
  (when vulpea-select--cache-timer
    (cancel-timer vulpea-select--cache-timer)
    (setq vulpea-select--cache-timer nil))
  (setq vulpea-select--cache nil))

(defun vulpea-select--cache-valid-p (cache)
  "Return non-nil when CACHE belongs to the open database and settings."
  (and (eq (vulpea-select--cache-state-db cache) vulpea-db--connection)
       (equal (vulpea-select--cache-state-location cache) vulpea-db-location)
       (equal (vulpea-select--cache-state-fingerprint cache)
              (vulpea-select--cache-fingerprint))))

(defun vulpea-select--cache-current ()
  "Return the candidate cache of the open database, creating it if needed.
A cache left from another database or other settings is replaced by
an empty one."
  (vulpea-db)
  (let ((cache vulpea-select--cache))
    (unless (and cache (vulpea-select--cache-valid-p cache))
      (setq cache (vulpea-select--cache-state-create
                   :db vulpea-db--connection
                   :location vulpea-db-location
                   :fingerprint (vulpea-select--cache-fingerprint))
            vulpea-select--cache cache))
    cache))

(defun vulpea-select--cache-note-candidates (note)
  "Return the candidate strings of NOTE, one per title and alias.

They are what `vulpea-select-from' builds with alias expansion,
minus the note and context properties: holding every note in
memory is what the cache avoids.  Alias candidates remember their
alias in the `vulpea-select-alias' property."
  (mapcar (lambda (n)
            (let ((candidate (vulpea-select-describe n)))
              (remove-list-of-text-properties
               0 (length candidate)
               '(vulpea-note vulpea-select-context) candidate)
              (when (vulpea-note-primary-title n)
                (put-text-property 0 (length candidate)
                                   'vulpea-select-alias (vulpea-note-title n)
                                   candidate))
              candidate))
          (vulpea-note-expand-aliases note)))

(defun vulpea-select--cache-put (cache note)
  "Store the candidates of NOTE in CACHE, replacing older ones."
  (let* ((by-id (vulpea-select--cache-state-by-id cache))
         (by-path (vulpea-select--cache-state-by-path cache))
         (id (vulpea-note-id note))
         (path (vulpea-note-path note))
         (old-path (car (gethash id by-id))))
    (unless (equal old-path path)
      ;; the id moved here from another file
      (when old-path
        (puthash old-path (delete id (gethash old-path by-path)) by-path))
      (puthash path (cons id (gethash path by-path)) by-path))
    (puthash id (cons path (vulpea-select--cache-note-candidates note)) by-id)
    (setf (vulpea-select--cache-state-stale cache) t)))

(defun vulpea-select--cache-forget-path (cache path)
  "Remove the candidates of notes living in PATH from CACHE."
  (let ((by-id (vulpea-select--cache-state-by-id cache))
        (by-path (vulpea-select--cache-state-by-path cache)))
    (dolist (id (gethash path by-path))
      (when (equal (car (gethash id by-id)) path)
        (remhash id by-id)))
    (remhash path by-path)
    (setf (vulpea-select--cache-state-stale cache) t)))

(defun vulpea-select--cache-load (cache limit)
  "Read up to LIMIT more notes into CACHE, all remaining ones when nil.
Notes are read in rowid order from the cursor on; once none are
left the cursor becomes nil."
  (when-let* ((cursor (vulpea-select--cache-state-cursor cache)))
    (let ((rows (vulpea-db--select
                 "SELECT rowid, * FROM notes WHERE rowid > ? ORDER BY rowid LIMIT ?"
                 (list cursor (or limit -1)))))
      (dolist (row rows)
        (setq cursor (car row))
        (vulpea-select--cache-put cache (vulpea-db--row-to-note (cdr row))))
      (setf (vulpea-select--cache-state-cursor cache)
            (when (and limit (= (length rows) limit)) cursor)))))

(defun vulpea-select--cache-complete-p ()
  "Return non-nil when every note of the database is in the cache."
  (and vulpea-select--cache
       (null (vulpea-select--cache-state-cursor vulpea-select--cache))))

(defun vulpea-select--cache-flush (cache)
  "Refresh the candidates of files changed since the last selection in CACHE."
  (let ((pending (vulpea-select--cache-state-pending cache))
        paths)
    (when (> (hash-table-count pending) 0)
      (maphash (lambda (path _) (push path paths)) pending)
      (clrhash pending)
      (dolist (path paths)
        (vulpea-select--cache-forget-path cache path))
      (dolist (chunk (seq-partition paths 200))
        (dolist (note (vulpea-db-query-by-file-paths chunk))
          (vulpea-select--cache-put cache note))))))

(defun vulpea-select--cache-candidates ()
  "Return the cached candidate list, bringing the cache up to date first."
  (let ((cache (vulpea-select--cache-current)))
    (vulpea-select--cache-load cache nil)
    (vulpea-select--cache-flush cache)
    (when (vulpea-select--cache-state-stale cache)
      (let (candidates)
        (maphash (lambda (_id entry)
                   (dolist (candidate (cdr entry))
                     (push candidate candidates)))
                 (vulpea-select--cache-state-by-id cache))
        (setf (vulpea-select--cache-state-candidates cache) (nreverse candidates)
              (vulpea-select--cache-state-stale cache) nil)))
    (vulpea-select--cache-state-candidates cache)))

(defun vulpea-select--cache-file-updated (path _count)
  "Queue PATH for a candidate refresh on the next selection.

Runs on `vulpea-db-updated-functions'.  Past
`vulpea-select-cache--pending-limit' queued files the cache is
dropped: a bulk change is cheaper to rebuild than to patch."
  (when-let* ((cache vulpea-select--cache))
    (when (eq (vulpea-select--cache-state-db cache) vulpea-db--connection)
      (let ((pending (vulpea-select--cache-state-pending cache)))
        (puthash (vulpea-db-normalize-path path) t pending)
        (when (> (hash-table-count pending) vulpea-select-cache--pending-limit)
          (vulpea-select-cache-drop))))))

(add-hook 'vulpea-db-updated-functions #'vulpea-select--cache-file-updated)

(defun vulpea-select--cached-note (id &optional alias)
  "Return the note with ID for a cached candidate, as its ALIAS if given.

Within a selection, notes are remembered in
`vulpea-select--note-memo'.  Past
`vulpea-select-cache--bulk-note-threshold' lookups - a sort or
annotation function visiting every candidate - all notes are read
at once, which is far cheaper than a query per candidate."
  (let* ((memo vulpea-select--note-memo)
         (note (if (null memo)
                   (vulpea-db-get-by-id id)
                 (or (gethash id memo)
                     (progn
                       (when (= (hash-table-count memo)
                                vulpea-select-cache--bulk-note-threshold)
                         (dolist (n (vulpea-db-query))
                           (puthash (vulpea-note-id n) n memo)))
                       (or (gethash id memo)
                           (puthash id (vulpea-db-get-by-id id) memo)))))))
    (if (and note alias)
        (seq-find (lambda (n) (equal (vulpea-note-title n) alias))
                  (cdr (vulpea-note-expand-aliases note)))
      note)))

(cl-defun vulpea-select-from-cache (prompt &key require-match initial-prompt)
  "Select a note from the candidate cache.

Behaves like `vulpea-select' with alias expansion and no filter,
but the candidates come from the cache (see `vulpea-select-cache')
and only the picked note is read from the database.

Returns a selected `vulpea-note'.  If `vulpea-note-id' is nil, the
user selected a non-existing note.

PROMPT, REQUIRE-MATCH and INITIAL-PROMPT are as in
`vulpea-select-from'."
  (let* ((candidates (vulpea-select--cache-candidates))
         (vulpea-select--note-memo (make-hash-table :test #'equal))
         (choice (save-excursion
                   (completing-read
                    (concat prompt ": ")
                    (vulpea-select--completion-table candidates)
                    nil require-match initial-prompt)))
         (candidate (car (member choice candidates))))
    (cond
     ((null candidate)
      (make-vulpea-note :title (substring-no-properties choice) :level 0))
     ((vulpea-select-candidate-note candidate))
     (t (user-error "Note %s no longer exists"
                    (get-text-property 0 'vulpea-note-id candidate))))))

(defun vulpea-select-cache-prewarm ()
  "Build the candidate cache in idle time, one chunk at a time.

Reads `vulpea-select-cache--chunk-size' notes per step and keeps
going while there is no input; on input it yields and resumes the
next time Emacs is idle.  Once every note is read, the candidate
list is assembled as well."
  (interactive)
  (when vulpea-select--cache-timer
    (cancel-timer vulpea-select--cache-timer))
  (setq vulpea-select--cache-timer nil)
  (when (vulpea-select-cache-usable-p)
    (let ((cache (vulpea-select--cache-current)))
      (vulpea-select--cache-load cache vulpea-select-cache--chunk-size)
      (while (and (vulpea-select--cache-state-cursor cache)
                  (not (input-pending-p)))
        (vulpea-select--cache-load cache vulpea-select-cache--chunk-size))
      (if (vulpea-select--cache-state-cursor cache)
          (setq vulpea-select--cache-timer
                (run-with-idle-timer vulpea-select-cache--prewarm-delay nil
                                     #'vulpea-select-cache-prewarm))
        ;; assemble the candidate list too, so the first selection
        ;; does not pay for it
        (vulpea-select--cache-candidates)))))

(defun vulpea-select--cache-autosync-started ()
  "Schedule a prewarm of the candidate cache on autosync start."
  (when (and (bound-and-true-p vulpea-db-autosync-mode)
             vulpea-select-cache-prewarm
             (vulpea-select-cache-usable-p)
             (not vulpea-select--cache-timer)
             (not (vulpea-select--cache-complete-p)))
    (setq vulpea-select--cache-timer
          (run-with-idle-timer vulpea-select-cache--prewarm-delay nil
                               #'vulpea-select-cache-prewarm))))

(add-hook 'vulpea-db-autosync-mode-hook #'vulpea-select--cache-autosync-started)

(provide 'vulpea-select)
;;; vulpea-select.el ends here
