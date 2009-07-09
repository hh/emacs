;; planner tasklist extension
;; load this file in your .emacs using something like:

;; (load-file "/path/to/.planner-tasklist")

;; this file assumes that you have already loaded muse and set up a
;; planner project in your .emacs
;; please see below for a few customizations

(require 'muse-backlink)
(muse-backlink-install)

;; use a custom footer which provides a link to the index page
(setq planner-xhtml-footer "<hr/><p>Last changed: <lisp>(muse-publishing-directive \"date\")</lisp> <a href=\"index.html\">index</a></p>")

;; switch on task numbers and have them updated automatically
(setq planner-use-task-numbers t)
(setq planner-renumber-tasks-automatically t)

;; use a CSS file as stylesheet instead of hardcoded style information
(setq planner-html-style-sheet "<link href=\"planner-muse.css\" type=\"text/css\" rel=\"stylesheet\">")

;; this defines the range of tasks that should be displayed in the task list
(setq tasklist-startdate "2004-01-12")
(setq tasklist-enddate "2037-12-20")

;; this custom function runs the Perl script that actually creates the
;; task overview
(defun my-planner-tasklist (project)
  (let ((mode (muse-get-keyword :major-mode (cadr project) t)))
    (when (eq mode 'planner-mode)
      (interactive)
      (shell-command 
       (format
	;; customize the following items in the next line:
	;; the path to the script tasklist.pl
	;; set the -m option to f if you don't want a mind map
	;; the base url (defined with the -u option); used only if -m is used
	"/home/markus/prog/tasklist/tasklist/tasklist.pl -p %s -s %s -e %s -o %s/tasklist.html -m t -u http://yeti/~markus/plans";
	;; the input directory
	"/home/markus/Plans"
	tasklist-startdate
	tasklist-enddate
	;; the output directory
	"/home/markus/public_html/plans")
       ;; send script output to this buffer
       "*planner-tasklist-output*"
       nil))))

;; run our custom function after the project was published
(add-hook 'muse-after-project-publish-hook 'my-planner-tasklist)

