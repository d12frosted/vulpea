

# Vulpea

<p align="center">
  <img width="256px" src="https://barberry.io/images/vulpea-logo.png" alt="Banner">
</p>
<p align="center">
  <a href="https://melpa.org/#/vulpea"><img alt="MELPA" src="https://melpa.org/packages/vulpea-badge.svg"/></a>
  <a href="https://stable.melpa.org/#/vulpea"><img alt="MELPA Stable" src="https://stable.melpa.org/packages/vulpea-badge.svg"/></a>
</p>

A collection of functions for note taking based on `org` and `org-roam`. This
repository primary goal is to be a tested library for other applications and
utilities around note taking.

Users of this library:

-   [d12frosted/environment](https://github.com/d12frosted/environment) - Emacs configurations;
-   [d12frosted/vino](https://github.com/d12frosted/vino) - collection of functions for cellar tracking and wine rating.

**Table of Contents**

1.  [Reasons to use `vulpea`](#orgdb38042)
2.  [Install](#org123f0b4)
    1.  [`use-package`](#org8daee5f)
    2.  [`straight.el`](#orge8aaa7e)
3.  [`vulpea-note`](#orga065e79)
4.  [Metadata](#org0e3f9ff)
    1.  [Why not properties drawer](#org1d76a84)
5.  [Modules](#orgf8500a0)
    1.  [`vulpea`](#orgc101f3f)
    2.  [`vulpea-select`](#org8c7ba61)
    3.  [`vulpea-note`](#orgefc0108)
    4.  [`vulpea-db`](#org007637a)
    5.  [`vulpea-meta`](#orgb55416f)
    6.  [`vulpea-buffer`](#org4435383)
    7.  [`vulpea-utils`](#org6e827d3)
6.  [Performance](#orgb0b2734)
    1.  [Query from database](#org5c161d5)
7.  [Coding](#org8db04bb)
8.  [Building and testing](#org2bec714)
9.  [Acknowledgements](#org3086207)


<a id="orgdb38042"></a>

## Reasons to use `vulpea`

1.  If you are automating things around Org mode files and you want to have a
    tested library to build upon.
2.  While `org-roam` was greatly simplified and has adopted some ideas from
    `vulpea` starting from `v2`, it still lacks certain pieces for programmatic
    usage. Just to name few of them:
    1.  `vulpea-db-query` is a great interface for quick database search without
        knowing its scheme. In many cases you just want to have a fully
        materialized note instead of a structure that lacks, say tags or aliases.
        So you don't need to write any extra SQL. See  [Performance](#orgb0b2734) for more
        information.
    2.  `vulpea-select` is a configurable interface for selecting a note with an
        optional filter. See [vulpea-select](#org8c7ba61) for more information.
    3.  `vulpea-create` is a wrapper around `org-roam-capture-` allowing to easily
        provide extra properties, tags and context when creating a new note. And
        most importantly - it returns a newly created note. See [vulpea-create](#org0b4661b)
        for more information.
3.  Several interactive functions:
    1.  `vulpea-find` that acts like `org-roam-node-find`, but (a) uses
        `vulpea-select` for consistent experience and (b) allows to configure
        default and on-use filtering and candidates source. See [vulpea-find](#org8e04495) for
        more information.
    2.  `vulpea-find-backlink` is just a convenient function to find a note
        linking currently opened one. For those who don't want to use `org-roam`
        buffer.
    3.  `vulpea-insert` that acts like `org-roam-node-insert`, but (a) uses
        `vulpea-select` for consistent experience, (b) allows to set
        `vulpea-insert-default-filter` (see `vulpea-find` for examples) and (c)
        allows to provide an insertion handler via
        `vulpea-insert-handle-functions`, which is called with inserted note. See
        [vulpea-insert](#org32e6d52) for more information.
4.  `vulpea` is more than just a wrapper around `org-roam`, it contains more
    stuff:
    1.  Utilities for dealing with buffer properties (e.g. `#+KEY: VALUE`).
    2.  Utilities for dealing with [metadata](#orgf288964) (e.g. first description list in the
        buffer).


<a id="org123f0b4"></a>

## Install

`vulpea` is available via [MELPA](https://melpa.org/#/vulpea), but you can still install it manually, using
[straight](https://github.com/raxod502/straight), [quelpa](https://github.com/quelpa/quelpa), or any package management tool of alike.

In short, installation process is simple:

1.  Download `vulpea` package via any preferred way.
2.  Call `vulpea-db-autosync-enable` (either via adding a hook to
    `org-roam-db-autosync-mode` or directly).
3.  Before first usage you need to re-sync `org-roam-db` from scratch, e.g.
    `(org-roam-db-sync 'force)`. This is because `vulpea` has several custom
    tables in addition to what `org-roam` provides.


<a id="org8daee5f"></a>

### `use-package`

    (use-package vulpea
      :ensure t
      ;; hook into org-roam-db-autosync-mode you wish to enable
      ;; persistence of meta values (see respective section in README to
      ;; find out what meta means)
      :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))


<a id="orge8aaa7e"></a>

### `straight.el`

    (straight-use-package
     '(vulpea :type git :host github :repo "d12frosted/vulpea"))
    
    ;; hook into org-roam-db-autosync-mode you wish to enable persistence
    ;; of meta values (see respective section in README to find out what
    ;; meta means)
    (add-hook 'org-roam-db-autosync-mode-hook #'vulpea-db-autosync-enable)

In case you have [integration](https://github.com/raxod502/straight.el/#integration-with-use-package) with [use-package](https://github.com/jwiegley/use-package):

    (use-package vulpea
      :straight (vulpea
                 :type git
                 :host github
                 :repo "d12frosted/vulpea")
      ;; hook into org-roam-db-autosync-mode you wish to enable
      ;; persistence of meta values (see respective section in README to
      ;; find out what meta means)
      :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))


<a id="orga065e79"></a>

## `vulpea-note`

A note is represented as a `vulpea-note` structure with the following
slots/fields:

-   `vulpea-note-id` - `ID` property of note (file-level or heading level).
-   `vulpea-note-path` - absolute path to the note (even if the note is actually a heading);
-   `vulpea-note-level` - level of the note inside `:path`, with `0` being a
    file-level note and other numbers being a header;
-   `vulpea-note-title` - title of the note (it might be an alias);
-   `vulpea-note-primary-title` - primary title of the note (present only when
    `title` is an alias);
-   `vulpea-note-aliases` - aliases of the note;
-   `vulpea-note-links` - list of links, each link being represented as `cons` of
    type and link;
-   `vulpea-note-tags` - tags of the note;
-   `vulpea-note-meta` - associative list of [metadata](#orgf288964), where key is a string and
    the value is a list of strings. There are also two utilities to access/query
    metadata from note:
    -   `vulpea-note-meta-get-list` - to get all values of given `PROP` and `TYPE`.
    -   `vulpea-note-meta-get` - to get the first value of given `PROP` and `TYPE`.

If `ID` is not present in the note structure, this note is treated as
non-existent. For example, `vulpea-select` returns such a note, when
`require-match` is `nil` and the user selects non-existent note.

Example of a note:

    > (vulpea-db-get-by-id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
    #s(vulpea-note :id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c"
                   :path "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org"
                   :level 0
                   :title "Kitsune Book"
                   :primary-title nil
                   :aliases
                   ("vulpea" "Kitsune no Hon")
                   :tags
                   ("personal")
                   :links
                   (("https" . "https://github.com/d12frosted/vulpea")
                    ("https" . "https://github.com/d12frosted/environment")
                    ("https" . "https://github.com/d12frosted/vino"))
                   :properties
                   (("CATEGORY" . "20200407160812-kitsune_book")
                    ("ROAM_ALIASES" . "vulpea \"Kitsune no Hon\"")
                    ("ID" . "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
                    ("BLOCKED" . "")
                    ("FILE" . "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org")
                    ("PRIORITY" . "B"))
                   :meta
                   (("link" "[[https://github.com/d12frosted/vulpea][vulpea]]")
                    ("users" "[[https://github.com/d12frosted/environment][environment]]" "[[https://github.com/d12frosted/vino][vino]]")
                    ("status" "stable")))


<a id="org0e3f9ff"></a>

## Metadata

In general, metadata is a list of key value pairs that is represented by the
first description list in the note, e.g. list like:

    - key1 :: value1
    - key2 :: value21
    - key2 :: value22
    - key3 :: value3

It can be manipulated programatically by using functions from either
`vulpea-meta` module or from `vulpea-buffer` module (those prefixed by
`vulpea-buffer-meta`). This data is also persisted in Org roam database for your
convenience and is part of `vulpea-note` returned by `vulpea-db` module. See
respective module documentation to find out all available functions.

Currently metadata is limited to file-level only, e.g. description lists in
outlines are not handled by `vulpea`. Vote for [vulpea#75](https://github.com/d12frosted/vulpea/issues/75) to bring it faster.


<a id="org1d76a84"></a>

### Why not properties drawer

In many cases, properties are far better choice for storing technical
'metadata', like `ID`, `DATE`, `TAGS`, etc. - something that is not really part
of note content. After all, properties drawer is a drawer:

> Sometimes you want to keep information associated with an entry, but you
> normally do not want to see it. For this, Org mode has drawers.
> 
> [orgmode.org](https://orgmode.org/manual/Drawers.html#Drawers)

Of course you can use [properties drawer](https://orgmode.org/manual/Properties-and-Columns.html#Properties-and-Columns) to implement simple database
capabilities, but it has one important limitation - values are mere text, so you
can't have real Org mode links there, meaning that [Element API](https://orgmode.org/worg/dev/org-element-api.html), Org roam and
some other tools **do not** recognise them as links.

Metadata provided by library is just a part of your note content, meaning that
it incorporates well into existing tools. Sure enough it's not as rich as
properties and is not as battle tested as properties, but you can give them a
try.


<a id="orgf8500a0"></a>

## Modules


<a id="orgc101f3f"></a>

### `vulpea`

This one-stop module contains some generic functions that didn't find their
place in separate modules. It also imports every other module.

1.  `vulpea-find`

    A one stop function to select and find (visit) a note that can be used both
    interactively (e.g. `M-x vulpea-find`) and programatically. In the later case it
    provides multiple configuration bits.
    
    When `OTHER-WINDOW` argument is nil (default), the note is visited in the
    current window. In order to use the *other* window, you may use universal
    argument during interactive usage (e.g. `C-u M-x vulpea-find`) or pass a non-nil
    value as argument:
    
        (vulpea-find :other-window t)
    
    When `REQUIRE-MATCH` argument is nil (default), user may select a non-existent
    note and the capture process is started. In order to disallow selection of
    non-existent note, pass non-nil value:
    
        (vulpea-find :require-match t)
    
    `vulpea-find` allows to configure candidates for selection in two ways - by
    controlling source of candidates and by controlling filtering function.
    
    1.  Filter function
    
        Filtering is easy. It's just a function that takes one argument - `vulpea-note`
        that is being filtered. You can configure default filtering function called
        `vulpea-find-default-filter` (so it is applied to interactive usage) or pass an
        override for the default filtering function.
        
        For example, you wish to list only file-level notes during interactive usage of
        `vulpea-find` (to mimic how `org-roam-find` was behaving in v1). For that you
        just need to configure the value of `vulpea-find-default-filter` variable:
        
            (setq vulpea-find-default-filter
                  (lambda (note)
                    (= (vulpea-note-level note) 0)))
        
        But of course, it's possible to override this behaviour when `vulpea-find` is
        used programatically, just by passing filtering function as `FILTER-FN`
        argument:
        
            ;; by default `vulpea-find' lists aliases, imagine that we want to
            ;; list only primary titles
            (vulpea-find
             :filter-fn (lambda (note)
                          ;; primary-title is set only when title is one of the
                          ;; aliases
                          (null (vulpea-note-primary-title note))))
    
    2.  Candidates function
    
        As it was already mentioned, `vulpea-find` allows to configure the source of
        candidates. This may be needed for performance considerations (e.g. to avoid
        filtering EVERY existing note in your database) or for some 'esoteric' features
        (like ordering).
        
        By default `vulpea-db-query` is used as a source of candidates. Default source
        is controlled by `vulpea-find-default-candidates-source` variable. You should
        change it only when your intention is to configure behaviour of `vulpea-find`
        interactive usage. For example (an 'esoteric' one):
        
            (setq vulpea-find-default-candidates-source
                  (lambda (filter)
                    ;; sort notes by title, but keep in mind that your completion
                    ;; framework might override this sorting, it's just an example
                    (seq-sort-by
                     #'vulpea-note-title
                     #'string<
                     (vulpea-db-query filter))))
        
        But in most cases you should not touch the configuration variable and instead
        apply an override via `CANDIDATES-FN` argument. For example, if you wish to
        'find' a note linking to some specific note. Of course this can be achieved with
        a filtering function, but in this particular case performance can be drastically
        improved by overriding candidates source. You can achieve this by something
        along the lines:
        
            ;; Let's say we have a note in the context. First, we use a
            ;; specialized query to find what links to a given note.
            (let ((backlinks (vulpea-db-query-by-links-some
                              (list (cons "id" (vulpea-note-id note))))))
              ;; Secondly, we override default CANDIDATES-FN, so it simply
              ;; presents us a list of backlinks. We deliberately ignore filtering
              ;; function.
              (vulpea-find
               :candidates-fn (lambda (_) backlinks)
               :require-match t))
        
        Please don't rush into saving this function into your collection. It's already
        provided by `vulpea` as `vulpea-find-backlink`. Keep reading!

2.  `vulpea-find-backlink`

    An interactive function to select and find (visit) a note linking to the
    currently visited note. Keep in mind that outlines with assigned `ID` property
    are also treated as notes so you might want to go to beginning of buffer if you
    wish to select backlinks to current file.

3.  `vulpea-insert`

    An interactive function to select a note and insert a link to it. When user
    selects non-existent note, it is captured via `org-roam-capture` process (see
    `org-roam-capture-templates`). Once the link is inserted,
    `vulpea-insert-handle-functions` is called with inserted note as an argument, so
    you can easily perform any necessary post-insertion actions. Selection is
    controlled in a similar way to `vulpea-find` - via global
    `vulpea-insert-default-filter` or local filter.
    
    1.  Filter function
    
        This argument is just a function that takes one argument - `vulpea-note` that is
        being filtered. You can configure default filtering function called
        `vulpea-insert-default-filter` (so it is applied to interactive usage) or pass
        an override for the default filtering function.
        
        For example, you wish to list only file-level notes during interactive usage of
        `vulpea-insert` (to mimic how `org-roam-find` was behaving in v1). For that you
        just need to configure the value of `vulpea-insert-default-filter` variable:
        
            (setq vulpea-insert-default-filter
                  (lambda (note)
                    (= (vulpea-note-level note) 0)))
        
        But of course, it's possible to override this behaviour when `vulpea-insert` is
        used programatically, just by passing filtering function as `FILTER-FN`
        argument:
        
            ;; by default `vulpea-insert' lists aliases, imagine that we want to
            ;; list only primary titles
            (vulpea-insert
             (lambda (note)
               ;; primary-title is set only when title is one of the
               ;; aliases
               (null (vulpea-note-primary-title note))))
    
    2.  Insertion handler
    
        There are cases when you want to react somehow to link insertion. For this
        `vulpea` provides a configuration variable `vulpea-insert-handle-functions`,
        which is kind of a hook with argument - `vulpea-note` that is linked.
        
        For example, you want to tag an outline whenever a link to person is inserted
        (see some explanation of this use case in a dedicated [blog post](https://d12frosted.io/posts/2020-07-07-task-management-with-roam-vol4.html)). For that you
        need to define a handler function first:
        
            (defun my-vulpea-insert-handle (note)
              "Hook to be called on NOTE after `vulpea-insert'."
              (when-let* ((title (vulpea-note-title note))
                          (tags (vulpea-note-tags note)))
                (when (seq-contains-p tags "people")
                  (save-excursion
                    (ignore-errors
                      (org-back-to-heading)
                      (when (eq 'todo (org-element-property
                                       :todo-type
                                       (org-element-at-point)))
                        (org-set-tags
                         (seq-uniq
                          (cons
                           (vulpea--title-to-tag title)
                           (org-get-tags nil t))))))))))
        
        And then you just need to add it as a hook:
        
            (add-hook 'vulpea-insert-handle-functions
                      #'my-vulpea-insert-handle)

4.  `vulpea-create`

    This function enables programmatic creation of new notes without the need to
    configure `org-roam-capture-templaces`, but instead providing various bits to be
    inserted into new note. And yes, it returns you the created note. This function
    is heavily used in [vino](https://github.com/d12frosted/vino) and you can find several real world usage examples
    there.
    
    The minimal usage example:
    
        (vulpea-create
         "Title of new note"
         "relative/path/to/%<%Y%m%d%H%M%S>-${slug}.org")
    
    This will create a note file
    `relative/path/to/20211119082840-title-of-new-note.org` with the following
    content:
    
        :PROPERTIES:
        :ID:                     3dfd828f-fb73-41a6-9801-54bc17d41b57
        :END:
        #+title: Title of new note
    
    As you can see, thanks to `org-roam-capture` and `org-capture` system, this
    allows expansion of formatted text as long as expansion of variables from
    capture context. Read further to learn more.
    
    1.  Synchronous vs asynchronous
    
        By default capture process is 'asynchronous', meaning that it waits for user
        input and confirmation. In some cases, 'synchronous' creation is desired, so
        that note is created immediately and the created note is returned as result, so
        we can use it further. Example:
        
            > (vulpea-create
               "immediate note"
               "%<%Y%m%d%H%M%S>-${slug}.org"
               :immediate-finish t)
            #s(vulpea-note
               :id "5733ca9e-5b42-4b6b-ace9-2fef1091d421"
               :path "/Users/d12frosted/Dropbox/vulpea/20211119095443-immediate_note.org"
               :level 0
               :title "immediate note"
               :primary-title nil
               :aliases nil
               :tags nil
               :links nil
               :properties
               (("CATEGORY" . "20211119095443-immediate_note")
                ("ID" . "5733ca9e-5b42-4b6b-ace9-2fef1091d421")
                ("BLOCKED" . "")
                ("FILE" . "/Users/d12frosted/Dropbox/vulpea/20211119095443-immediate_note.org")
                ("PRIORITY" . "B"))
               :meta nil)
        
        And the content of created file is:
        
            :PROPERTIES:
            :ID:                     5733ca9e-5b42-4b6b-ace9-2fef1091d421
            :END:
            #+title: immediate note
        
        How cool is that? Pretty cool, I'd say.
    
    2.  Extra content
    
        Of course, in many cases we want to add much more than that into note file. In
        general, the file has the following format:
        
            :PROPERTIES:
            :ID: ID
            PROPERTIES if present
            :END:
            #+title: TITLE
            #+filetags: TAGS if present
            HEAD if present
            
            BODY if present
        
        So you can provide the following arguments controlling content:
        
        -   `properties` - a list consisting of `(key_str . val_str)` pairs added to
            properties block;
        -   `tags` is a list of strings inserted as `filetags` option (in a proper
            format);
        -   `head` - a string inserted after `title` and `filetags`;
        -   `body` - a string inserted after `title`, `filetags` and `head`;
        
        Simple example to illustrate:
        
            > (vulpea-create
               "Rich note"
               "%<%Y%m%d%H%M%S>-${slug}.org"
               :properties '(("COUNTER" . "1")
                             ("STATUS" . "ignore")
                             ("ROAM_ALIASES" . "\"Very rich note with an alias\""))
               :tags '("documentation" "showcase")
               :head "#+author: unknown\n#+date: today"
               :body "It was a very nice day.\n\nBut I didn't feel that."
               :immediate-finish t)
            #s(vulpea-note
               :id "568d4e29-76dd-4630-82f9-e1e2006bebdc"
               :path "/Users/d12frosted/Dropbox/vulpea/20211119095644-rich_note.org"
               :level 0
               :title "Rich note"
               :primary-title nil
               :aliases
               ("Very rich note with an alias")
               :tags
               ("documentation" "showcase")
               :links nil
               :properties
               (("CATEGORY" . "20211119095644-rich_note")
                ("ROAM_ALIASES" . "Very rich note with an alias")
                ("STATUS" . "ignore")
                ("COUNTER" . "1")
                ("ID" . "568d4e29-76dd-4630-82f9-e1e2006bebdc")
                ("BLOCKED" . "")
                ("FILE" . "/Users/d12frosted/Dropbox/vulpea/20211119095644-rich_note.org")
                ("PRIORITY" . "B"))
               :meta nil)
        
        This creates the following note:
        
            :PROPERTIES:
            :ID:                     568d4e29-76dd-4630-82f9-e1e2006bebdc
            :COUNTER:                1
            :STATUS:                 ignore
            :ROAM_ALIASES:           "Very rich note with an alias"
            :END:
            #+title: Rich note
            #+filetags: :documentation:showcase:
            #+author: unknown
            #+date: today
            
            It was a very nice day.
            
            But I didn't feel that.
    
    3.  Context variables
    
        Any content piece (except for title) may have arbitrary amount of context
        variables in form `${VAR}` that are expanded during note creation. By default
        there are 3 context variables - `slug`, `title` and `id`. But you may add extra
        variables to the context by passing `context` variable:
        
            > (vulpea-create
               "A Book"
               "${slug}.org"
               :context (list :name "Frodo")
               :immediate-finish t
               :properties '(("AUTHOR" . "${name}"))
               :tags '("@${name}")
               :head "#+author: ${name}"
               :body "This note was create by ${name}")
            #s(vulpea-note
               :id "1fecedf8-ccda-4d68-875e-111b8cc5992e"
               :path "/home/borysb/Dropbox/vulpea/a_book.org"
               :level 0
               :title "A Book"
               :primary-title nil
               :aliases nil
               :tags
               ("@Frodo")
               :links nil
               :properties
               (("CATEGORY" . "a_book")
                ("AUTHOR" . "Frodo")
                ("ID" . "1fecedf8-ccda-4d68-875e-111b8cc5992e")
                ("BLOCKED" . "")
                ("FILE" . "/home/borysb/Dropbox/vulpea/a_book.org")
                ("PRIORITY" . "B"))
               :meta nil)
        
        This creates the following note:
        
            :PROPERTIES:
            :ID:                     1fecedf8-ccda-4d68-875e-111b8cc5992e
            :AUTHOR:                 Frodo
            :END:
            #+title: A Book
            #+filetags: :@Frodo:
            #+author: Frodo
            
            This note was create by Frodo
        
        Please keep in mind that you cannot override the default context via `context`
        variable.
    
    4.  Mandatory ID
    
        By default `id` is being generated for you and you can not avoid it. This is
        what allows `vulpea-create` to return created note for you. In some cases you
        might want to provide `id` upfront instead of relying on generation. And
        `vulpea-create` has an argument for that.
        
            > (vulpea-create
             "Custom id"
             "${slug}.org"
             :id "xyz"
             :immediate-finish t)
            #s(vulpea-note
               :id "xyz"
               :path "/home/borysb/Dropbox/vulpea/custom_id.org"
               :level 0
               :title "Custom id"
               :primary-title nil
               :aliases nil
               :tags nil
               :links nil
               :properties
               (("CATEGORY" . "custom_id")
                ("ID" . "xyz")
                ("BLOCKED" . "")
                ("FILE" . "/home/borysb/Dropbox/vulpea/custom_id.org")
                ("PRIORITY" . "B"))
               :meta nil)
        
        This creates the following note:
        
            :PROPERTIES:
            :ID:                     xyz
            :END:
            #+title: Custom id


<a id="org8c7ba61"></a>

### `vulpea-select`

Common interface to select (e.g. `completing-read`) a note from the set of
notes. Used in functions like `vulpea-find`, `vulpea-find-backlink`,
`vulpea-insert`, etc.

<div class="html" id="org8c2bc6a">
<p>
&lt;div&gt;
  &lt;img src="images/vulpea-select.png" width="100%"/&gt;
  &lt;p align="center"&gt;&lt;em&gt;Narrowing by aliases and tags&lt;/em&gt;&lt;/p&gt;
&lt;/div&gt;
</p>

</div>

There are two variants of selection: `vulpea-select-from` and `vulpea-select`.
The difference between them is that the former accepts a list of notes to select
from and the latter accepts a filter function which is applied to all notes in
the database. Here are two examples to illustrate that:

    ;; Select a note from the list of passed notes
    (vulpea-select-from
     "Grape"
     ;; this function returns only notes that are tagged as 'wine' and
     ;; 'grape' at the same time (see `vulpea-db 'documentation for more
     ;; information on this function).
     (vulpea-db-query-by-tags-every '("wine" "grape"))
     :require-match t)
    
    ;; Select a note from all notes filtered by some predicate.
    (vulpea-select
     "Grape"
     :filter-fn
     ;; We just manually check that the note is tagged as 'wine' and
     ;; 'grape' at the same time.
     (lambda (note)
       (let ((tags (vulpea-note-tags note)))
         (and (seq-contains-p tags "wine")
              (seq-contains-p tags "grape")))))

Both of these examples achieve the same goal. The only practical difference here
is performance. Sometimes you either already have a list of notes that you want
to select from (so there is no need to filter all the database just to select
those notes you already have) or you have a way to fetch a list of notes in a
much faster way than by filtering whole database. See `vulpea-db` for more
information on performance.

1.  Visual configuration

    Each note is formatted using two functions - `vulpea-select-describe-fn` and
    `vulpea-select-annotate-fn`. Both of them are called by `vulpea-select`
    interface with a note as argument and their result is concatenated. The only
    difference between them is purely aesthetical - description has normal face and
    annotation has `completions-annotations` face.
    
    By default `vulpea-select-describe-fn` is defined as `vulpea-note-title`; and
    `vulpea-select-annotate-fn` returns aliases and tags if present. To illustrate
    how it works, let's use some fake notes.
    
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "note1.org" org-roam-directory)
         :title "Note without aliases and without tags")
        
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "note2.org" org-roam-directory)
         :title "Note with single tag"
         :tags '("tag1"))
        
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "note3.org" org-roam-directory)
         :title "Note with multiple tags"
         :tags '("tag1" "tag2"))
        
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "subdir/aliases.org" org-roam-directory)
         :title "Main title"
         :aliases '("Alias 1" "Alias 2"))
        
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "subdir/aliases.org" org-roam-directory)
         :title "Alias 1"
         :primary-title "Main title"
         :aliases '("Alias 1" "Alias 2"))
        
        (make-vulpea-note
         :id (org-id-new)
         :path (expand-file-name "subdir/aliases.org" org-roam-directory)
         :title "Alias 1"
         :primary-title "Main title"
         :aliases '("Alias 1" "Alias 2")
         :tags '("tag1" "tag2"))
    
    These notes are converted into the following lines:
    
        "Note without aliases and without tags"
        "Note with single tag #tag1"
        "Note with multiple tags #tag1 #tag2"
        "Main title"
        "Alias 1 (Main title)"
        "Alias 1 (Main title) #tag1 #tag2"
    
    <div class="html" id="org85a0374">
    <p>
    &lt;div&gt;
      &lt;img src="images/vulpea-select-example-1.png" width="50%"/&gt;
      &lt;p align="center"&gt;&lt;em&gt;Default describe behaviour&lt;/em&gt;&lt;/p&gt;
    &lt;/div&gt;
    </p>
    
    </div>
    
    Of course, you can configure this behaviour. For example:
    
        ;; relative path // title
        (setq vulpea-select-describe-fn
              (lambda (note)
                (concat
                 (string-remove-prefix
                  org-roam-directory
                  (vulpea-note-path note))
                 " // "
                 (vulpea-note-title note))))
        
        ;; display tags and ignore aliases
        (setq vulpea-select-annotate-fn
              (lambda (note)
                (let* ((tags-str (mapconcat
                                  (lambda (x) (concat "#" x))
                                  (vulpea-note-tags note)
                                  " ")))
                  (if (string-empty-p tags-str)
                      ""
                    (concat " " tags-str)))))
    
    This results in the following lines:
    
        "note1.org // Note without aliases and without tags"
        "note2.org // Note with single tag #tag1"
        "note3.org // Note with multiple tags #tag1 #tag2"
        "subdir/aliases.org // Main title"
        "subdir/aliases.org // Alias 1"
        "subdir/aliases.org // Alias 1 #tag1 #tag2"
    
    <div class="html" id="org931ab98">
    <p>
    &lt;div&gt;
      &lt;img src="images/vulpea-select-example-2.png" width="50%"/&gt;
      &lt;p align="center"&gt;&lt;em&gt;Custom describe behaviour&lt;/em&gt;&lt;/p&gt;
    &lt;/div&gt;
    </p>
    
    </div>


<a id="orgefc0108"></a>

### `vulpea-note`

This module contains `vulpea-note` definition, which is represented as a
structure with the following slots/fields:

-   `vulpea-note-id` - `ID` property of note (file-level or heading level).
-   `vulpea-note-path` - absolute path to the note (even if the note is actually a heading);
-   `vulpea-note-level` - level of the note inside `:path`, with `0` being a
    file-level note and other numbers being a header;
-   `vulpea-note-title` - title of the note (it might be an alias);
-   `vulpea-note-primary-title` - primary title of the note (present only when
    `title` is an alias);
-   `vulpea-note-aliases` - aliases of the note;
-   `vulpea-note-links` - list of links, each link being represented as `cons` of
    type and link;
-   `vulpea-note-tags` - tags of the note;
-   `vulpea-note-meta` - associative list of [metadata](#orgf288964), where key is a string and
    the value is a list of strings.

If `ID` is not present in the note structure, this note is treated as
non-existent. For example, `vulpea-select` returns such a note, when
`require-match` is `nil` and the user selects non-existent note.

Example of a note:

    > (vulpea-db-get-by-id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
    #s(vulpea-note :id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c"
                   :path "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org"
                   :level 0
                   :title "Kitsune Book"
                   :primary-title nil
                   :aliases
                   ("vulpea" "Kitsune no Hon")
                   :tags
                   ("personal")
                   :links
                   (("https" . "https://github.com/d12frosted/vulpea")
                    ("https" . "https://github.com/d12frosted/environment")
                    ("https" . "https://github.com/d12frosted/vino"))
                   :properties
                   (("CATEGORY" . "20200407160812-kitsune_book")
                    ("ROAM_ALIASES" . "vulpea \"Kitsune no Hon\"")
                    ("ID" . "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
                    ("BLOCKED" . "")
                    ("FILE" . "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org")
                    ("PRIORITY" . "B"))
                   :meta
                   (("link" "[[https://github.com/d12frosted/vulpea][vulpea]]")
                    ("users" "[[https://github.com/d12frosted/environment][environment]]" "[[https://github.com/d12frosted/vino][vino]]")
                    ("status" "stable")))

1.  Tags predicate

    In some cases you want to check if a note is tagged somehow. Vulpea provides two shortcuts for this:
    
    -   `vulpea-note-tagged-all-p` - return non-nil if a NOTE is tagged by all of the TAGS.
    -   `vulpea-note-tagged-any-p` - return non-nil if a NOTE is tagged by any of the TAGS.
    
        > (setq note (make-vulpea-note :tags '("tag-1" "tag-2" "tag-3")))
        #s(vulpea-note nil nil nil nil nil nil ("tag-1" "tag-2" "tag-3") nil nil nil)
        
        > (vulpea-note-tagged-all-p note "tag-2" "tag-3")
        t
        
        > (vulpea-note-tagged-all-p note "tag-2" "tag-3" "tag-4")
        nil
        
        > (vulpea-note-tagged-any-p note "tag-2" "tag-3")
        t
        
        > (vulpea-note-tagged-all-p note "tag-2" "tag-3" "tag-4")
        nil
        
        > (vulpea-note-tagged-all-p note "tag-4")
        nil

2.  Accessing meta

    In most cases you should not directly access `vulpea-note-meta`, but instead you
    should use one of the helpers - `vulpea-note-meta-get` and
    `vulpea-note-meta-get-list`. The only difference between these two functions is
    how they treat repeating keys. The former returns only the first occurrence of
    the key, while the latter returns a list.
    
    Let's take the following note as example:
    
        > (vulpea-db-get-by-id "05907606-f836-45bf-bd36-a8444308eddd")
        #s(vulpea-note :id "05907606-f836-45bf-bd36-a8444308eddd"
                       :path "..."
                       ...
                       :meta
                       (("name" "some name")
                        ("tags" "tag 1")
                        ("tags" "tag 2")
                        ("tags" "tag 3")
                        ("numbers" "12")
                        ("numbers" "18")
                        ("numbers" "24")
                        ("singleton" "only value")
                        ("symbol" "red")
                        ("url" "[[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]")
                        ("link" "[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]")
                        ("references" "[[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]")
                        ("references" "[[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]")
                        ("answer" "42")))
    
    As you can see, keys and values are strings. But that's not always useful,
    that's why `vulpea-note-meta-get` and `vulpea-note-meta-get-list` support string
    parsing of some common 'types': string (default), number, link (path of the link
    
    -   either ID of the linked note or raw link), note (queries note by id from db)
    
    and symbol.
    
        > (vulpea-note-meta-get note "name")
        "some name"
        
        > (vulpea-note-meta-get note "name" 'string)
        "some name"
        
        > (vulpea-note-meta-get-list note "name")
        ("some name")
        
        > (vulpea-note-meta-get note "tags")
        "tag 1"
        
        > (vulpea-note-meta-get-list note "tags")
        ("tag 1" "tag 2" "tag 3")
        
        > (vulpea-note-meta-get note "numbers" 'number)
        12
        
        > (vulpea-note-meta-get-list note "numbers" 'number)
        (12 18 24)
        
        > (vulpea-note-meta-get note "symbol")
        "red"
        
        > (vulpea-note-meta-get note "symbol" 'symbol)
        red
        
        > (vulpea-note-meta-get note "url" 'link)
        "https://en.wikipedia.org/wiki/Frappato"
        
        > (vulpea-note-meta-get note "link" 'link)
        "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
        
        > (vulpea-note-meta-get-list note "references" 'note)
        (#s(vulpea-note :id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                        :path "..."
                        :title "Note without META"
                        ...)
         #s(vulpea-note :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                        :path "..."
                        :title "Reference"
                        ...))


<a id="org007637a"></a>

### `vulpea-db`

This module contains functions to query notes from data base. In order for most
of these functions to operate, one needs to enable `vulpea-db-autosync-mode`
(see [Install](#org123f0b4) section), for example, by using `vulpea-db-autosync-enable`. This
hooks into `org-roam.db` by adding two extra tables:

-   `meta` - for storing [Metadata](#orgf288964);
-   `notes` - a view table of fully materialized note (see [Performance](#orgb0b2734)).

**Important!** You might need to perform a full re-sync of `org-roam.db`.

1.  `vulpea-db-get-by-id`

    The simplest function to get a note with some `ID`. Supports both file-level
    notes and outlines/headings. Returns `vulpea-note` if note with `ID` exists and
    nil otherwise.
    
        > (vulpea-db-get-by-id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
          #s(vulpea-note :id "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c"
                         :path "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org"
                         :level 0
                         :title "Kitsune Book"
                         :primary-title nil
                         :aliases
                         ("vulpea" "Kitsune no Hon")
                         :tags
                         ("personal")
                         :links
                         (("https" . "https://github.com/d12frosted/vulpea")
                          ("https" . "https://github.com/d12frosted/environment")
                          ("https" . "https://github.com/d12frosted/vino"))
                         :properties
                         (("CATEGORY" . "20200407160812-kitsune_book")
                          ("ROAM_ALIASES" . "vulpea \"Kitsune no Hon\"")
                          ("ID" . "7705e5e4-bcd4-4e16-9ba7-fda8acdefe8c")
                          ("BLOCKED" . "")
                          ("FILE" . "/Users/d12frosted/Dropbox/vulpea/20200407160812-kitsune_book.org")
                          ("PRIORITY" . "B"))
                         :meta
                         (("link" "[[https://github.com/d12frosted/vulpea][vulpea]]")
                          ("users" "[[https://github.com/d12frosted/environment][environment]]" "[[https://github.com/d12frosted/vino][vino]]")
                          ("status" "stable")))
        
        > (vulpea-db-get-by-id "xyz")
        nil

2.  `vulpea-db-query`

    Function to query notes from database with optional predicate. This function is
    very powerful as it allows to apply Emacs Lisp predicate on *every*
    `vulpea-note` in your database. This might be not very efficient on big set of
    notes, in such cases use specialized query functions.
    
    When predicate is not passed, `vulpea-db-query` returns ALL notes from your
    database.
    
        > (seq-length (vulpea-db-query))
        9554
    
    Since `vulpea-note` contains so much information, you can do many complex
    things, with `vulpea-db-query`.
    
        > (vulpea-db-query
           (lambda (note)
             (and
              (seq-contains-p (vulpea-note-links note) (cons "id" "8f62b3bd-2a36-4227-a0d3-4107cd8dac19"))
              (or
               (seq-contains-p (vulpea-note-tags note) "grape")
               (seq-contains-p (vulpea-note-tags note) "cellar")))))
        # 15 notes
    
    1.  Custom SQL
    
        As you can see, `vulpea-db-query` doesn't allow to pass any custom SQL for
        filtering or whatnot. For future-proof code you should avoid querying stuff
        manually from database, but in case you need to, just use `org-roam-db-query`:
        
            > (org-roam-db-query
               [:select title
                :from notes
                :limit 1])
            (("Arianna Occhipinti"))

3.  Specialized queries

    -   `vulpea-db-query-by-tags-some` - return all notes tagged with one of the
        provided `TAGS`.
    -   `vulpea-db-query-by-tags-every` - return all notes tagged by every tag from
        the list of provided `TAGS`.
    -   `vulpea-db-query-by-links-some` - return all notes linking at least one of the
        provided `DESTINATIONS`.
    -   `vulpea-db-query-by-links-every` - return all notes linking each and every
        provided `DESTINATIONS`.

4.  Other functions

    -   `vulpea-db-get-id-by-file` - function to get `ID` of a note represented by
        `FILE`.
    -   `vulpea-db-get-file-by-id` - function to get `FILE` of a note represented by
        `ID`. Supports headings of the note.
    -   `vulpea-db-search-by-title` - function to query notes with `TITLE`.


<a id="orgb55416f"></a>

### `vulpea-meta`

This module contains functions for manipulating note [metadata](#orgf288964) represented by the
first description list in the note, e.g. list like:

    - key1 :: value1
    - key2 :: value21
    - key2 :: value22
    - key3 :: value3

Functions of interest:

-   `vulpea-meta` - function to get metadata from `NOTE-OR-ID`. In most cases you
    should not use this function unless performance is important. In this case,
    take a look at bang functions, e.g. `vulpea-meta-get!`.
-   `vulpea-meta-get` - function to get a value of `PROP` for note with `ID`.
    Value is parsed based on the passed `TYPE` or as a string if omitted.
-   `vulpea-meta-get-list` - function to get all values of `PROP` for note with
    `ID`. Values are parsed based on the passed `TYPE` or as a string if omitted.
-   `vulpea-meta-set` - function to set `VALUE` of `PROP` for `NOTE-OR-ID`.
    Supports multi-value properties.
-   `vulpea-meta-add` - interactive version of `vulpea-meta-set`.
-   `vulpea-meta-add-list` - interactive version of `vulpea-meta-set` that
    operates on list values.
-   `vulpea-meta-remove` - interactive function to remove a `PROP` for
    `NOTE-OR-ID`.
-   `vulpea-meta-clean` - interactive function to remove all meta for
    `NOTE-OR-ID`.


<a id="org4435383"></a>

### `vulpea-buffer`

This module contains functions for prop and meta manipulations in current
buffer.

1.  Buffer properties

    Buffer properties are key-values defined as `#+KEY: VALUE` in the header of
    buffer.
    
    -   `vulpea-buffer-title-get` - function to get title of the current buffer.
    -   `vulpea-buffer-title-set` - function to set title of the current buffer.
    -   `vulpea-buffer-tags-get` - function to get list of tags.
    -   `vulpea-buffer-tags-set` - function to set/replace the value of `#+filetags`.
    -   `vulpea-buffer-tags-add` - function to add a tag to `#+filetags`.
    -   `vulpea-buffer-tags-remove` - function to remove a tag from `#+filetags`.
    -   `vulpea-buffer-prop-set` - function to set a `VALUE` of property with `NAME`
        in the current buffer, e.g. property in the buffer header using `#+NAME:
          value` format.
    -   `vulpea-buffer-prop-set-list` - function to set a value of property with
        `NAME` to the list of `VALUES` in the current buffer.
    -   `vulpea-buffer-prop-get` - function to get a value of property with `NAME`
        from the current buffer.
    -   `vulpea-buffer-prop-get-list` - function to get a value of property with
        `NAME` as a list separated by some `SEPARATORS`.
    -   `vulpea-buffer-prop-remove` - function to remove a property with `NAME` from
        the current buffer.

2.  Metadata

    Metadata is defined as the first description list in the buffer, e.g. list like:
    
        - key1 :: value1
        - key2 :: value21
        - key2 :: value22
        - key3 :: value3
    
    -   `vulpea-buffer-meta` - function to get metadata from current buffer. By it's
        own it has little value, use the following functions to manipulate it.
    -   `vulpea-buffer-meta-get!` - function to get a value of `PROP` from `META`
        (result of `vulpea-buffer-meta`). Value is parsed based on the passed `TYPE`
        or as a string if omitted. Use it performing multiple read operations in a
        row.
    -   `vulpea-buffer-meta-get-list!` - function to get all values of `PROP` from
        `META` (result of `vulpea-buffer-meta`). Values are parsed based on the passed
        `TYPE` or as a string if omitted. Use it performing multiple read operations
        in a row.
    -   `vulpea-buffer-meta-set` - function to set `VALUE` of `PROP` in current
        buffer. Supports multi-value properties.
    -   `vulpea-buffer-meta-remove` - function to remove a `PROP` from current buffer.
    -   `vulpea-buffer-meta-clean` - function to remove all meta from current buffer.
    -   `vulpea-buffer-meta-format` - function to format a `VALUE` based in its type
        (used for value serialization).
    
    1.  Example 1 - getting values
    
        Consider the following Org Mode file.
        
            :PROPERTIES:
            :ID:                     05907606-f836-45bf-bd36-a8444308eddd
            :END:
            #+title: Note with META
            
            - name :: some name
            - tags :: tag 1
            - tags :: tag 2
            - tags :: tag 3
            - numbers :: 12
            - numbers :: 18
            - numbers :: 24
            - singleton :: only value
            - symbol :: red
            - url :: [[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]
            - link :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]
            - references :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]
            - references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
            - answer :: 42
            
            Don't mind me. I am a content of this note.
        
        In order to get anything from meta, first you need to parse it:
        
            > (vulpea-buffer-meta)
            (:file "/path-to/with-meta.org" :buffer (org-data ...))
        
        And then you can retrieve values from parse meta:
        
            > (setq test-meta (vulpea-buffer-meta))
            
            > (vulpea-buffer-meta-get! test-meta "name")
            "some name"
            
            > (vulpea-buffer-meta-get! test-meta "tags")
            "tag 1"
            
            > (vulpea-buffer-meta-get-list! test-meta "tags")
            ("tag 1" "tag 2" "tag 3")
            
            > (vulpea-buffer-meta-get-list! test-meta "numbers" 'number)
            (12 18 24)
            
            > (vulpea-buffer-meta-get! test-meta "symbol" 'symbol)
            red
            
            > (vulpea-buffer-meta-get! test-meta "url" 'link)
            "https://en.wikipedia.org/wiki/Frappato"
            
            > (vulpea-buffer-meta-get! test-meta "link" 'link)
            "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
            
            > (vulpea-buffer-meta-get-list! test-meta "references" 'note)
            (#s(vulpea-note :id "444f94d7-61e0-4b7c-bb7e-100814c6b4bb"
                            ...)
             #s(vulpea-note :id "5093fc4e-8c63-4e60-a1da-83fc7ecd5db7"
                            ...))
    
    2.  Example 2 - setting values
    
        Consider the following Org Mode file.
        
            :PROPERTIES:
            :ID:                     05907606-f836-45bf-bd36-a8444308eddd
            :END:
            #+title: Note with META
            
            - name :: some name
            - tags :: tag 1
            - tags :: tag 2
            - tags :: tag 3
            - numbers :: 12
            - numbers :: 18
            - numbers :: 24
            - singleton :: only value
            - symbol :: red
            - url :: [[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]
            - link :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]
            - references :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]
            - references :: [[id:5093fc4e-8c63-4e60-a1da-83fc7ecd5db7][Reference]]
            - answer :: 42
            
            Don't mind me. I am a content of this note.
        
        Imagine that we evaluated the following code in this buffer.
        
            ;; put a value in the beginning of the list
            (vulpea-buffer-meta-set "date" "[2021-12-05]")
            
            ;;  replace existing name value
            (vulpea-buffer-meta-set "name" "new name")
            
            ;;replace list of references with new one
            (vulpea-buffer-meta-set "references" (list (vulpea-db-get-by-id "8f62b3bd-2a36-4227-a0d3-4107cd8dac19")))
            
            ;; append to the end of list
            (vulpea-buffer-meta-set "years" '(1993 1994) 'append)
            
            ;; remove numbers key
            (vulpea-buffer-meta-remove "numbers")
        
        The resulting buffer will look like this:
        
            :PROPERTIES:
            :ID:                     05907606-f836-45bf-bd36-a8444308eddd
            :END:
            #+title: Note with META
            
            - date :: [2021-12-05]
            - name :: new name
            - tags :: tag 1
            - tags :: tag 2
            - tags :: tag 3
            - singleton :: only value
            - symbol :: red
            - url :: [[https://en.wikipedia.org/wiki/Frappato][wikipedia.org]]
            - link :: [[id:444f94d7-61e0-4b7c-bb7e-100814c6b4bb][Note without META]]
            - references :: [[id:8f62b3bd-2a36-4227-a0d3-4107cd8dac19][Arianna Occhipinti]]
            - answer :: 42
            - years :: 1993
            - years :: 1994
            
            Don't mind me. I am a content of this note.


<a id="org6e827d3"></a>

### `vulpea-utils`

This module contains various utilities used by other modules. Functions of
interest:

-   `vulpea-utils-with-note` - function to execute `BODY` with point at `NOTE`.
    Supports file-level notes as well as heading notes.
-   `vulpea-utils-link-make-string` - make a bracket link to `NOTE`.
-   `vulpea-utils-note-hash` - function to calculate `sha1` of a given `NOTE`.
-   `vulpea-utils-collect-while` - utility to repeat some function and collect
    it's results until `C-g` is used or passed filter returns `nil`. Example of
    usage - you want to collect multiple values from user and be able to quit the
    process.
-   `vulpea-utils-repeat-while` - utility to repeat some function and return first
    unfiltered result. Example of usage - you want to enforce some validation on
    value and keep prompting user until valid value is typed.


<a id="orgb0b2734"></a>

## Performance


<a id="org5c161d5"></a>

### Query from database

This library provides multiple functions to query notes from the database.
Basically, there is one powerful `vulpea-db-query` allowing to filter notes by
any `vulpea-note` based predicate. The only downside of this power is
performance and memory penalty as all notes are loaded into memory. In cases
when performance is critical and the set of notes can be narrowed down, one can
use specialized queries:

-   `vulpea-db-query-by-tags-some` - return all notes tagged with one of the
    provided `TAGS`.
-   `vulpea-db-query-by-tags-every` - return all notes tagged by every tag from
    the list of provided `TAGS`.
-   `vulpea-db-query-by-links-some` - return all notes linking at least one of the
    provided `DESTINATIONS`.
-   `vulpea-db-query-by-links-every` - return all notes linking each and every
    provided `DESTINATIONS`.

The following table displays time required to query notes by using
`vulpea-db-query` vs specialized query on the database of 9554 [generated notes](https://github.com/d12frosted/vulpea-test-notes/).
The difference between various test cases is partially explained by the fact
that filtering functions result in different amount of notes. Since we need to
retrieve full note structure, the more notes we have, the more time it takes.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">test</th>
<th scope="col" class="org-left">result size</th>
<th scope="col" class="org-right">generic</th>
<th scope="col" class="org-right">specialized</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>tags-some</code></td>
<td class="org-left">30 notes</td>
<td class="org-right">1.0112478712</td>
<td class="org-right">0.0066033426</td>
</tr>


<tr>
<td class="org-left"><code>tags-every</code></td>
<td class="org-left">3168 notes</td>
<td class="org-right">1.0059819176</td>
<td class="org-right">0.5709392964999999</td>
</tr>


<tr>
<td class="org-left"><code>links-some</code></td>
<td class="org-left">1657 notes</td>
<td class="org-right">1.0462236128999999</td>
<td class="org-right">0.4248580532</td>
</tr>


<tr>
<td class="org-left"><code>links-every</code></td>
<td class="org-left">92 notes</td>
<td class="org-right">1.0204833089</td>
<td class="org-right">0.0545313596</td>
</tr>
</tbody>
</table>

See [this comment](https://github.com/d12frosted/vulpea/discussions/106#discussioncomment-1601429) for more background on why these functions where created.

In order to make these functions as fast as possible, `vulpea-db` module builds
and maintains a view table called `notes`. While it does drastically improve
query performance (see the table below), it adds a small footprint on
synchronisation time. See [vulpea#116](https://github.com/d12frosted/vulpea/pull/116) for more information on this feature and
measurements.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">test</th>
<th scope="col" class="org-left">result size</th>
<th scope="col" class="org-right"><a href="https://github.com/d12frosted/vulpea/blob/551495a59fb8c3bcd49a091b233e24e4cb8b584c/vulpea-db.el#L76-L187">regular</a></th>
<th scope="col" class="org-right">view table</th>
<th scope="col" class="org-right">ratio</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>tags-some</code></td>
<td class="org-left">30 notes</td>
<td class="org-right">4.6693460650999995</td>
<td class="org-right">1.0112478712</td>
<td class="org-right">4.6174100</td>
</tr>


<tr>
<td class="org-left"><code>tags-every</code></td>
<td class="org-left">3168 notes</td>
<td class="org-right">4.7333844436999996</td>
<td class="org-right">1.0059819176</td>
<td class="org-right">4.7052381</td>
</tr>


<tr>
<td class="org-left"><code>links-some</code></td>
<td class="org-left">1657 notes</td>
<td class="org-right">4.8095771283</td>
<td class="org-right">1.0462236128999999</td>
<td class="org-right">4.5970833</td>
</tr>


<tr>
<td class="org-left"><code>links-every</code></td>
<td class="org-left">92 notes</td>
<td class="org-right">4.5517473337999995</td>
<td class="org-right">1.0204833089</td>
<td class="org-right">4.4603839</td>
</tr>
</tbody>
</table>


<a id="org8db04bb"></a>

## Coding

Vulpea is developed using [eldev](https://github.com/doublep/eldev/). If you are using `flycheck`, it is advised to
also use [flycheck-eldev](https://github.com/flycheck/flycheck-eldev), as it makes dependencies and project files available
thus mitigating false negative results from default Emacs Lisp checker.


<a id="org2bec714"></a>

## Building and testing

Vulpea tests are written using [buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) testing framework. And [eldev](https://github.com/doublep/eldev/) is used to
run them both locally and on CI. In order to run the tests locally, first
[install](https://github.com/doublep/eldev/id:b946c716-e3b3-4c84-8229-dde59ddd55aeation) `eldev` and then run:

    $ make test

Please note, that the linter is used in this project, so you might want to run
it as well:

    $ make lint


<a id="org3086207"></a>

## Acknowledgements

[Logo](https://barberry.io/images/vulpea-logo.png) was created by [Iryna Rutylo](https://www.behance.net/irynarutylo).

