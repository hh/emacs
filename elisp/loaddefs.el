;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (color-theme-select) "color-theme" "color-theme.el"
;;;;;;  (19017 27006))
;;; Generated autoloads from color-theme.el

(autoload (quote color-theme-select) "color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (doc-view) "doc-view" "doc-view.el" (18861 25574))
;;; Generated autoloads from doc-view.el

(autoload (quote doc-view) "doc-view" "\
Convert FILE to png and start viewing it.
If no FILE is given, query for on.
If this FILE is still in the cache, don't convert and use the
existing page files.  With prefix arg NO-CACHE, don't use the
cached files and convert anew.

\(fn NO-CACHE &optional FILE)" t nil)

;;;***

;;;### (autoloads (inferior-erlang erlang-compile erlang-shell erlang-find-tag-other-window
;;;;;;  erlang-find-tag erlang-mode) "erlang" "erlang.el" (18861
;;;;;;  25552))
;;; Generated autoloads from erlang.el

(autoload (quote erlang-mode) "erlang" "\
Major mode for editing Erlang source files in Emacs.
It knows about syntax and comment, it can indent code, it is capable
of fontifying the source file, the TAGS commands are aware of Erlang
modules, and the Erlang man pages can be accessed.

Should this module, \"erlang.el\", be installed properly, Erlang mode
is activated whenever an Erlang source or header file is loaded into
Emacs.  To indicate this, the mode line should contain the word
\"Erlang\".

The main feature of Erlang mode is indentation, press TAB and the
current line will be indented correctly.

Comments starting with only one `%' are indented to the column stored
in the variable `comment-column'.  Comments starting with two `%':s
are indented with the same indentation as code.  Comments starting
with at least three `%':s are indented to the first column.

However, Erlang mode contains much more, this is a list of the most
useful commands:
     TAB     - Indent the line.
     C-c C-q - Indent current function.
     M-;     - Create a comment at the end of the line.
     M-q     - Fill a comment, i.e. wrap lines so that they (hopefully)
		 will look better.
     M-a     - Goto the beginning of an Erlang clause.
     M-C-a   - Ditto for function.
     M-e     - Goto the end of an Erlang clause.
     M-C-e   - Ditto for function.
     M-h     - Mark current Erlang clause.
     M-C-h   - Ditto for function.
     C-c C-z - Start, or switch to, an inferior Erlang shell.
     C-c C-k - Compile current file.
     C-x `   - Next error.
     ,       - Electric comma.
     ;       - Electric semicolon.

Erlang mode check the name of the file against the module name when
saving, whenever a mismatch occurs Erlang mode offers to modify the
source.

The variable `erlang-electric-commands' controls the electric
commands.  To deactivate all of them, set it to nil.

There exists a large number of commands and variables in the Erlang
module.  Please press `M-x apropos RET erlang RET' to see a complete
list.  Press `C-h f name-of-function RET' and `C-h v name-of-variable
RET'to see the full description of functions and variables,
respectively.

On entry to this mode the contents of the hook `erlang-mode-hook' is
executed.

Please see the beginning of the file `erlang.el' for more information
and examples of hooks.

Other commands:
\\{erlang-mode-map}

\(fn)" t nil)

(autoload (quote erlang-find-tag) "erlang" "\
Like `find-tag'.  Capable of retrieving Erlang modules.

Tags can be given on the forms `tag', `module:', `module:tag'.

\(fn MODTAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload (quote erlang-find-tag-other-window) "erlang" "\
Like `find-tag-other-window' but aware of Erlang modules.

\(fn TAGNAME &optional NEXT-P REGEXP-P)" t nil)

(autoload (quote erlang-shell) "erlang" "\
Start a new Erlang shell.

The variable `erlang-shell-function' decides which method to use,
default is to start a new Erlang host.  It is possible that, in the
future, a new shell on an already running host will be started.

\(fn)" t nil)
 (autoload 'run-erlang "erlang" "Start a new Erlang shell." t)

(autoload (quote erlang-compile) "erlang" "\
Compile Erlang module in current buffer.

\(fn)" t nil)

(autoload (quote inferior-erlang) "erlang" "\
Run an inferior Erlang.

This is just like running Erlang in a normal shell, except that
an Emacs buffer is used for input and output.
\\<comint-mode-map>
The command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (gnats-dbconfig-mode unlock-pr unlock-database
;;;;;;  query-pr view-pr edit-pr send-pr) "gnats" "gnats.el" (18861
;;;;;;  25577))
;;; Generated autoloads from gnats.el

(autoload (quote send-pr) "gnats" "\
Generate a new initial PR template for the user.
If a prefix argument is given, run `gnats-change-database' first.

\(fn &optional ARG)" t nil)

(autoload (quote edit-pr) "gnats" "\
Edit the specified PR; prompts for a PR number if one was not given.

\(fn PR)" t nil)

(autoload (quote view-pr) "gnats" "\
View the problem report PR.

\(fn PR)" t nil)

(autoload (quote query-pr) "gnats" "\
Create query buffer resulting from QUERY.
QUERY is a string representing a query in the gnatsd format.

\(fn QUERY)" t nil)

(autoload (quote unlock-database) "gnats" "\
Unlock the whole database.

\(fn)" t nil)

(autoload (quote unlock-pr) "gnats" "\
Unlock the problem report PR.

\(fn PR)" t nil)

(autoload (quote gnats-dbconfig-mode) "gnats" "\
Major mode for editing the `dbconfig' GNATS configuration file.

\(fn)" t nil)

;;;***

;;;### (autoloads (highlight-current-line-minor-mode) "highlight-current-line"
;;;;;;  "highlight-current-line.el" (18996 41766))
;;; Generated autoloads from highlight-current-line.el

(autoload (quote highlight-current-line-minor-mode) "highlight-current-line" "\
Toggle highlight-current-line minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-current-line.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (ibuffer) "ibuffer" "ibuffer.el" (18861 25536))
;;; Generated autoloads from ibuffer.el

(defsubst ibuffer-and-update (&optional other-window-p) "\
Like `ibuffer', but update the list of buffers too.
With optional prefix argument, use another window." (interactive "P") (ibuffer other-window-p nil nil t))

(defsubst ibuffer-and-update-other-window nil "\
Like `ibuffer-and-update', but use another window." (interactive) (ibuffer-and-update t))

(autoload (quote ibuffer) "ibuffer" "\
Begin using `ibuffer' to edit a list of buffers.
Type 'h' after entering ibuffer for more information.

Optional argument OTHER-WINDOW-P says to use another window.
Optional argument NAME specifies the name of the buffer; it defaults
to \"*Ibuffer*\".
Optional argument QUALIFIERS is an initial set of limiting qualifiers
to use; see `ibuffer-limiting-qualifiers'.

\(fn &optional OTHER-WINDOW-P NAME QUALIFIERS UPDATE)" t nil)

;;;***

;;;### (autoloads (ido-read-directory-name ido-read-file-name ido-dired
;;;;;;  ido-insert-file ido-write-file ido-find-file-other-frame
;;;;;;  ido-display-file ido-find-file-read-only-other-frame ido-find-file-read-only-other-window
;;;;;;  ido-find-file-read-only ido-find-alternate-file ido-find-file-other-window
;;;;;;  ido-find-file ido-find-file-in-dir ido-switch-buffer-other-frame
;;;;;;  ido-insert-buffer ido-kill-buffer ido-display-buffer ido-switch-buffer-other-window
;;;;;;  ido-switch-buffer ido-read-buffer ido-mode ido-enabled) "ido"
;;;;;;  "ido.el" (18861 25547))
;;; Generated autoloads from ido.el

(defvar ido-enabled nil "\
Determines for which functional group (buffer and files) ido behavior
should be enabled. The following values are possible:
- 'buffer: Turn only on ido buffer behavior (switching, killing,
  displaying...) 
- 'file: Turn only on ido file behavior (finding, writing, inserting...)
- 'both: Turn on ido buffer and file behavior.
- nil: Turn off any ido switching.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'.")

(custom-autoload (quote ido-enabled) "ido" nil)

(autoload (quote ido-mode) "ido" "\
Toggle ido speed-ups on or off.
With ARG, turn ido speed-up on if arg is positive, off otherwise.
If second argument NOBIND is non-nil, no keys are rebound; otherwise,
turning on ido-mode will modify the default keybindings for the 
find-file and switch-to-buffer families of commands to the ido
versions of these functions.
However, if second arg equals 'files, bind only for files, or if it 
equals 'buffers, bind only for buffers.
This function also adds a hook to the minibuffer.

\(fn &optional ARG NOBIND)" t nil)

(autoload (quote ido-read-buffer) "ido" "\
Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected.
If INITIAL is non-nil, it specifies the initial input string.

\(fn PROMPT &optional DEFAULT REQUIRE-MATCH INITIAL)" nil nil)

(autoload (quote ido-switch-buffer) "ido" "\
Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed if substring-matching is used (default). Look at
`ido-enable-prefix' and `ido-toggle-prefix'. When you have found the
buffer you want, it can then be selected. As you type, most keys have their
normal keybindings, except for the following: \\<ido-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-edit-input] Edit input string.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into ido-find-file.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list.
\\[ido-toggle-ignore] Toggle ignoring buffers listed in `ido-ignore-buffers'.

\(fn)" t nil)

(autoload (quote ido-switch-buffer-other-window) "ido" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'.

\(fn)" t nil)

(autoload (quote ido-display-buffer) "ido" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'.

\(fn)" t nil)

(autoload (quote ido-kill-buffer) "ido" "\
Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'.

\(fn)" t nil)

(autoload (quote ido-insert-buffer) "ido" "\
Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'.

\(fn)" t nil)

(autoload (quote ido-switch-buffer-other-frame) "ido" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'.

\(fn)" t nil)

(autoload (quote ido-find-file-in-dir) "ido" "\
Switch to another file starting from DIR.

\(fn DIR)" t nil)

(autoload (quote ido-find-file) "ido" "\
Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring. As you type
in a string, all of the filenames matching the string are displayed if
substring-matching is used (default). Look at `ido-enable-prefix' and
`ido-toggle-prefix'. When you have found the filename you want, it can
then be selected. As you type, most keys have their normal keybindings,
except for the following: \\<ido-mode-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-edit-input] Edit input string (including path).
\\[ido-prev-work-directory] or \\[ido-next-work-directory] go to previous/next directory in work directory history.
\\[ido-merge-work-directories] search for file in the work directory history.
\\[ido-forget-work-directory] removes current directory from the work directory history.
\\[ido-prev-work-file] or \\[ido-next-work-file] cycle through the work file history.
\\[ido-wide-find-file] and \\[ido-wide-find-dir] prompts and uses find to locate files or directories.
\\[ido-make-directory] prompts for a directory to create in current directory.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-vc] Toggle version control for this file.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-toggle-ignore] Toggle ignoring files listed in `ido-ignore-files'.

\(fn)" t nil)

(autoload (quote ido-find-file-other-window) "ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-find-alternate-file) "ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-find-file-read-only) "ido" "\
Edit file read-only with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-find-file-read-only-other-window) "ido" "\
Edit file read-only in other window with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-find-file-read-only-other-frame) "ido" "\
Edit file read-only in other frame with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-display-file) "ido" "\
Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-find-file-other-frame) "ido" "\
Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-write-file) "ido" "\
Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-insert-file) "ido" "\
Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-dired) "ido" "\
Call dired the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'.

\(fn)" t nil)

(autoload (quote ido-read-file-name) "ido" "\
Read file name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters.

\(fn PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL)" nil nil)

(autoload (quote ido-read-directory-name) "ido" "\
Read directory name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters.

\(fn PROMPT &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" nil nil)

;;;***

;;;### (autoloads (run-ruby inf-ruby-keys) "inf-ruby" "inf-ruby.el"
;;;;;;  (18861 25574))
;;; Generated autoloads from inf-ruby.el

(autoload (quote inf-ruby-keys) "inf-ruby" "\
Set local key defs for inf-ruby in `ruby-mode'.

\(fn)" nil nil)

(autoload (quote run-ruby) "inf-ruby" "\
Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD)" t nil)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;;;***

;;;### (autoloads (turn-off-line-numbers-display turn-on-line-numbers-display
;;;;;;  toggle-line-numbers-display display-line-numbers) "line-num"
;;;;;;  "line-num.el" (18781 55553))
;;; Generated autoloads from line-num.el

(autoload (quote display-line-numbers) "line-num" "\
Temporarily display line numbers in left margin of current buffer.

\(fn)" t nil)

(autoload (quote toggle-line-numbers-display) "line-num" "\
Display/clear line numbers in left margin of current buffer.
With prefix ARG, just number lines in current window, not all lines in
buffer.

\(fn ARG)" t nil)

(autoload (quote turn-on-line-numbers-display) "line-num" "\
Display line numbers in left margin of current buffer.
With prefix ARG, just number lines in current window, not all lines in
buffer.

\(fn ARG)" t nil)

(autoload (quote turn-off-line-numbers-display) "line-num" "\
Clear displayed line numbers from left margin of current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "lua-mode.el" (18861 25574))
;;; Generated autoloads from lua-mode.el

(autoload (quote lua-mode) "lua-mode" "\
Major mode for editing Lua code.
The following keys are bound:
\\{lua-mode-map}

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.lua$" . lua-mode)))

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "php-mode.el"
;;;;;;  (18954 58516))
;;; Generated autoloads from php-mode.el

(defvar php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")) "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload (quote php-file-patterns) "php-mode" nil)

(autoload (quote php-mode) "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (planner-w3m-annotation-from-w3m) "planner-w3m"
;;;;;;  "planner-w3m.el" (18878 29562))
;;; Generated autoloads from planner-w3m.el

(autoload (quote planner-w3m-annotation-from-w3m) "planner-w3m" "\
If called from a w3m page, return an annotation.
Suitable for use in `planner-annotation-functions'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (svn-status svn-checkout) "psvn" "psvn.el" (18861
;;;;;;  25536))
;;; Generated autoloads from psvn.el

(autoload (quote svn-checkout) "psvn" "\
Run svn checkout REPOS-URL PATH.

\(fn REPOS-URL PATH)" t nil)
 (defalias 'svn-examine 'svn-status)

(autoload (quote svn-status) "psvn" "\
Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'.

\(fn DIR &optional ARG)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "ruby-mode.el" (18861 25552))
;;; Generated autoloads from ruby-mode.el

(autoload (quote ruby-mode) "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (lisp-complete-symbol symbol-completion-try-complete
;;;;;;  symbol-complete) "sym-comp" "sym-comp.el" (18861 25570))
;;; Generated autoloads from sym-comp.el

(autoload (quote symbol-complete) "sym-comp" "\
Perform completion of the symbol preceding point.
This is done in a way appropriate to the current major mode,
perhaps by interrogating an inferior interpreter.  Compare
`complete-symbol'.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered.

This function requires `symbol-completion-completions-function'
to be set buffer-locally.  Variables `symbol-completion-symbol-function',
`symbol-completion-predicate-function' and
`symbol-completion-transform-function' are also consulted.

\(fn &optional PREDICATE)" t nil)

(autoload (quote symbol-completion-try-complete) "sym-comp" "\
Completion function for use with `hippie-expand'.
Uses `symbol-completion-symbol-function' and
`symbol-completion-completions-function'.  It is intended to be
used something like this in a major mode which provides symbol
completion:

  (if (featurep 'hippie-exp)
      (set (make-local-variable 'hippie-expand-try-functions-list)
	   (cons 'symbol-completion-try-complete
                 hippie-expand-try-functions-list)))

\(fn OLD)" nil nil)

(autoload (quote lisp-complete-symbol) "sym-comp" "\
Perform completion on Lisp symbol preceding point.
Compare that symbol against the known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

When called from a program, optional arg PREDICATE is a predicate
determining which symbols are considered, e.g. `commandp'.
If PREDICATE is nil, the context determines which symbols are
considered.  If the symbol starts just after an open-parenthesis, only
symbols with function definitions are considered.  Otherwise, all
symbols with function definitions, values or properties are
considered.

\(fn &optional PREDICATE)" t nil)

;;;***

;;;### (autoloads (tabbar-local-mode tabbar-mode tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar" "tabbar.el" (16094
;;;;;;  64565))
;;; Generated autoloads from tabbar.el

(autoload (quote tabbar-backward) "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload (quote tabbar-forward) "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'.

\(fn)" t nil)

(autoload (quote tabbar-backward-group) "tabbar" "\
Go to selected tab in the previous available group.

\(fn)" t nil)

(autoload (quote tabbar-forward-group) "tabbar" "\
Go to selected tab in the next available group.

\(fn)" t nil)

(autoload (quote tabbar-backward-tab) "tabbar" "\
Select the previous visible tab.

\(fn)" t nil)

(autoload (quote tabbar-forward-tab) "tabbar" "\
Select the next visible tab.

\(fn)" t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `tabbar-mode'.")

(custom-autoload (quote tabbar-mode) "tabbar" nil)

(autoload (quote tabbar-mode) "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\(fn &optional ARG)" t nil)

(autoload (quote tabbar-local-mode) "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar.

\(fn &optional ARG)" t nil)

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
