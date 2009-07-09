;;;
;;;
;;; zenicb.el --- Waste time on International Citizen's Band (ZenICB client)

;;; Copyright (C) 1994 Ben A. Mesander

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;; Maintainer: ben@gnu.ai.mit.edu
;;; Keywords: extensions
;;; Created: 1994/10/08

;;; $Id: zenicb.el,v 1.30 1995/04/12 01:21:15 ben Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; This code is derived from the code to the ZenIRC internet relay chat
;;; client, and the `icb.el' ICB client. The people who helped me write
;;; ZenIRC are:
;;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;;         Charles Hannum <mycroft@gnu.ai.mit.edu>
;;;         Richard Todd <rmtodd@essex.ecn.uoknor.edu>
;;;         Per Persson <pp@solace.mh.se>
;;;         Eric Prestemon <eric@american.edu>
;;;         Mark Bailen <msbailen@msbdcolka.cr.usgs.gov>
;;;         Nicolas Pioch <Nicolas.Pioch@enst.fr>
;;;
;;; The developers of the icb.el client are:
;;;         Matt Rhoten <mrhoten@cs.stanford.edu>
;;;         Greg Williams <greg.williams@gtri.gatech.edu>
;;;
;;; Bits of the ZenIRC client were also derived from the Kiwi and msa
;;; emacs clients; these have their own history and many contributors.
;;; Thanks to all.

;;; Code:

(defvar zenicb-initial-channel "Meditation"
  "*Initial channel to join.")

(defvar zenicb-nick (user-login-name)
  "*Nickname to use on ICB.")

(defvar zenicb-server "cvs.openbsd.org"
  "*Hostname of ICB server.")

(defvar zenicb-port 7326
  "*Integer port number that ICB server is listening on.")

(defvar zenicb-initial-status "pil"
  "*Initial status to set zenicb-channel to.")

(defvar zenicb-password ""
  "*Some sort of password.")

(defvar zenicb-mode-hook nil
  "Hook to run when setting zenicb mode, after everything else.")

(defvar zenicb-mode-map '()
  "Sparse keymap for zenicb-mode")
(cond 
 ((not zenicb-mode-map)
  (setq zenicb-mode-map (make-sparse-keymap))
  (define-key zenicb-mode-map "\n" 'zenicb-send-line)
  (define-key zenicb-mode-map "\C-m" 'zenicb-send-line)))

(defvar zenicb-partialline)
(defvar zenicb-process)
(defvar zenicb-debug-mainloop nil)
(defvar zenicb-debug-commands nil)
(defvar zenicb-active nil)

(defvar zenicb-server-b-hook 'zenicb-server-b) ; public message
(defvar zenicb-server-c-hook 'zenicb-server-c) ; private message
(defvar zenicb-server-d-hook 'zenicb-server-d) ; status
(defvar zenicb-server-e-hook 'zenicb-server-e) ; error
(defvar zenicb-server-g-hook 'zenicb-server-g) ; exit
;; is it not clear to me that the server ever sends an h packet to the client
(defvar zenicb-server-h-hook 'zenicb-server-h) ; command input
(defvar zenicb-server-i-hook 'zenicb-server-i) ; command output
(defvar zenicb-server-j-hook 'zenicb-server-j) ; protocol
(defvar zenicb-server-k-hook 'zenicb-server-k) ; beep
(defvar zenicb-server-l-hook 'zenicb-server-l) ; ping

(defvar zenicb-command-bcount-hook 'zenicb-command-bcount) ; byte count
(defvar zenicb-command-beep-hook 'zenicb-command-beep) ; beep someone
(defvar zenicb-command-boot-hook 'zenicb-command-boot) ; boot off group
(defvar zenicb-command-cancel-hook 'zenicb-command-cancel) ; cancel invite
(defvar zenicb-command-echo-hook 'zenicb-command-echo) ; echoback
(defvar zenicb-command-g-hook 'zenicb-command-g) ; change group
(defvar zenicb-command-group-hook 'zenicb-command-group) ; change group
(defvar zenicb-command-help-hook 'zenicb-command-help) ; help help
(defvar zenicb-command-invite-hook 'zenicb-command-invite) ; invite user
(defvar zenicb-command-join-hook 'zenicb-command-join) ; change group
(defvar zenicb-command-m-hook 'zenicb-command-m) ; message
(defvar zenicb-command-mess-hook 'zenicb-command-mess) ; no clue
(defvar zenicb-command-msg-hook 'zenicb-command-msg) ; message
(defvar zenicb-command-mode-hook 'zenicb-command-mode) ; help for the irc disabled.
(defvar zenicb-command-motd-hook 'zenicb-command-motd) ; message of the day
(defvar zenicb-command-nick-hook 'zenicb-command-nick) ; change nicknames
(defvar zenicb-command-nosecure-hook 'zenicb-command-nosecure) ; lame "security"
(defvar zenicb-command-pass-hook 'zenicb-command-pass) ; pass the mod
(defvar zenicb-command-quit-hook 'zenicb-command-quit) ; quit ICB
(defvar zenicb-command-read-hook 'zenicb-command-read) ; read a message
(defvar zenicb-command-secure-hook 'zenicb-command-secure) ; lame "security"
(defvar zenicb-command-shuttime-hook 'zenicb-command-shuttime) ; time to shut
(defvar zenicb-command-status-hook 'zenicb-command-status) ; group status
(defvar zenicb-command-topic-hook 'zenicb-command-topic) ; set topic of group
(defvar zenicb-command-version-hook 'zenicb-command-version) ; icb version
(defvar zenicb-command-w-hook 'zenicb-command-w) ; see who's on icb
(defvar zenicb-command-who-hook 'zenicb-command-who) ; see who's on icb
(defvar zenicb-command-whois-hook 'zenicb-command-whois) ; who is a user?
(defvar zenicb-command-write-hook 'zenicb-command-write) ; save message
; recieved packet format
; <one-byte-length><command-type><part1><^A><part2>...<^A><partN><NULL>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connect to the ICB server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb (&optional prefix)
  "Waste time on ICB"
  (interactive "P")
  (save-excursion
    (let ((zenicb-buffer (if prefix
			     (generate-new-buffer "*zenicb*")
			   (get-buffer-create "*zenicb*"))))
      (switch-to-buffer zenicb-buffer)
      (make-local-variable 'zenicb-partialline)
      (setq zenicb-partialline "")
      (make-local-variable 'zenicb-process)
      (make-local-variable 'zenicb-active)
      (if (not zenicb-active)
	  (unwind-protect
	      (setq zenicb-active t)
	      (setq zenicb-process 
		(open-network-stream "zenicb" zenicb-buffer zenicb-server 
				     zenicb-port))
	      (if (not zenicb-process)
		  ()
		(set-marker (process-mark zenicb-process) (point-max))
		(set-process-buffer zenicb-process zenicb-buffer)
		(set-process-filter zenicb-process 'zenicb-filter)
		(set-process-sentinel zenicb-process 'zenicb-sentinel)
		(zenicb-mode)
		(zenicb-logon zenicb-process)))))))

(defun zenicb-mode ()
  "Major mode for querying zenicb server."
  (interactive)
  (setq mode-name "zenicb")
  (setq major-mode 'zenicb-mode)
  (make-local-variable 'mode-line-process)
  (setq mode-line-process '(":%s"))
  (use-local-map zenicb-mode-map)
  (zenicb-run-hook 'zenicb-mode-hook))

(defun zenicb-logon (proc)
  (zenicb-send-string
   proc ?a
   (format "%s\C-a%s\C-a%s\C-a%s\C-a%s\C-a%s\C-a%d"
	   (user-login-name)
	   zenicb-nick ; nick
	   zenicb-initial-channel ; group
	   "login" ; command
	   zenicb-password
	   zenicb-initial-status
	   0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handle closing connection to server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-sentinel (proc sentinel)
  (save-excursion
    (set-buffer (process-buffer proc))
    (setq zenicb-active nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handle data arriving from the ICB server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-filter (proc string)
  (let ((data (match-data))
        (orig-buffer (current-buffer)))
    (unwind-protect
	(progn
          (set-buffer (process-buffer proc))
          (setq zenicb-partialline (zenicb-parselines
                                    proc (concat zenicb-partialline string))))
      (set-buffer orig-buffer)
      (store-match-data data))))

(defun zenicb-parselines (proc string)
  (while (let ((length (aref string 0)))
	   (and (> (length string) length)
		(let ((type (aref string 1))
		      (line (substring string 2 length)))
		  (setq string (substring string (1+ length)))
		  (zenicb-parseline proc type line)
		  (not (string-equal string ""))))))
  string)

(defun zenicb-parseline (proc type string)
  (if zenicb-debug-mainloop
      (zenicb-display-string
       proc (format "[debug] %s\n" (prin1-to-string string))))
  (let ((parsedmsg (zenicb-split-string string))
	(hook (intern (format "zenicb-server-%c-hook" type))))
    (if zenicb-debug-mainloop
	(progn
	  (zenicb-display-string
	   proc (format "[debug] %s\n" (prin1-to-string parsedmsg)))
	  (zenicb-display-string
	   proc (format "[debug] hook=%s\n" (prin1-to-string hook)))))
    (if (boundp hook)
	(zenicb-run-hook hook proc parsedmsg)
      (zenicb-display-string proc (format "[server] %s\n" string)))))

;; split a line along ^A's
(defun zenicb-split-string (line)
  (let ((posn (string-match "\C-a" line)))
    (if posn
	(cons (substring line 0 posn)
	      (zenicb-split-string (substring line (1+ posn))))
      (cons line nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send data to the ICB server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-send-string (proc type str)
  ;; The use of (concat) is to work around a bug in Emacs 18 that would
  ;; cause the NUL to be lost if it were in the (format) pattern.
  (let ((message (concat (format "%c%c%s" (+ 2 (length str)) type str) "\0")))
;   (zenicb-display-string proc
;			   (format "[debug] %s\n" (prin1-to-string message)))
    (process-send-string proc message)))

;;
;; parse a line into the first word and the rest.
;;
;; This returns ("word" . "rest"), where word means adjacent non-space 
;; characters. Any amount of whitespace is skipped after the first word, 
;; and "rest" is the rest of the line. If there is no "rest", a "rest"
;;  of "" is constructed.
;;
;;
(defun zenicb-parse-firstword (string)
  (if (string-match "\\( +\\)" string)
      (cons (substring string 0 (match-beginning 1))
	    (substring string (match-end 1)))
    (cons string "")))

(defun zenicb-send-line ()
  "Send current line to zenicb server."
  (interactive)
  (end-of-line)
  (insert "\n")
  (let* ((proc zenicb-process)
	 (proc-mark (process-mark proc))
	 (string (buffer-substring proc-mark (point))))
    (if (< proc-mark (point))
	(progn
	  (set-marker proc-mark (point))
 	  (if (not (string-match "\\`\\s-*\\'" string)) ; No blank strings
	      (progn
		;; Handle newlines in input.
 		(while (string-match "\\(\\`\\|\n\\)\\(\\s-*\n\\)" string)
 		  (setq string (concat (substring string 0 (match-beginning 2))
 				       (substring string (match-end 2)))))
		(while (string-match "[^\n]*\\(\n\\)." string)
		  (setq string (concat (substring string 0 (match-beginning 1))
				       " "
				       (substring string (match-end 1)))))
		(if (eq (aref string 0) ?/)
		    (zenicb-do-command proc (substring string 1 -1))
		  (zenicb-split (substring string 0 -1) 220 'zenicb-send-public
				proc)))))
      ;; if the user presses enter, jump to the bottom of the buffer
      (goto-char (point-max)))))
(defun zenicb-send-public (message proc)
  (zenicb-send-string proc ?b message))

;; Handle a zenicb / command typed by the user.  Check to see if there's a 
;; hook for the command and if so, execute the hook, otherwise just send the 
;; command line unaltered to the server.  
(defun zenicb-do-command (proc cmdline)
  (let* ((parsedcmd (zenicb-parse-firstword cmdline))
	 (cmdname (car parsedcmd))
	 (hook (intern (concat "zenicb-command-" cmdname "-hook"))))
    (if zenicb-debug-commands 
	(progn
	  (zenicb-display-string proc (format "[debug] cmdhook: %s\n"
					      (symbol-name hook)))
	  (zenicb-display-string proc (format "[debug] parsedcmd:%s\n"
					      (prin1-to-string parsedcmd)))))
    ;; Call the hook, if it's there.
    (if (boundp hook) (zenicb-run-hook hook proc parsedcmd)
      ;; Otherwise, signal error
      (zenicb-display-string 
       proc (format "[info] No such command: %s\n" cmdline)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Insert strings into the ZenICB buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenicb-display-string (proc string)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
	  (let* ((proc-mark (process-mark proc))
		 (region-begin (marker-position proc-mark))
		 (current-point-mark (set-marker (make-marker) (point)))
		 ;; If process mark is at window start,
		 ;; insert-before-markers will ;; insert text
		 ;; off-window since it's also inserting before the
		 ;; start ;; window mark.  Make sure we can see the
		 ;; most recent text.
		 (window (and (= proc-mark (window-start))
			      (get-buffer-window (current-buffer)))))
	    (goto-char proc-mark)
	    (insert-before-markers string)
	    (goto-char region-begin)
	    (while (search-forward "\C-m" proc-mark 'goto-end)
	      (delete-char -1))
	    (goto-char current-point-mark)
	    (and window
		 (>= (window-start window) region-begin)
		 (set-window-start window region-begin 'noforce))))
      (set-buffer orig-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ZenICB hook handling functions
;;;
;;; ZenICB uses a somewhat nonstandard hook mechanism. Hook sysmbols
;;; are manipulated with zenicb-add-hook and zenicb-delete hook, and
;;; are executed with zenicb-run-hook. A hook symbol is a list of
;;; symbols that are function names. When a hook is run with
;;; zenicb-run-hook, each symbol in the list is run in turn - unless
;;; one of the hooks sets the variable zenicb-run-next-hook to nil. In
;;; this case, zenicb-run-hook immediatelly returns to the caller.
;;; Unlike emacs 19 hooks, ZenICB hooks are called with arguments.
;;; ZenICB hooks return the value of the last hook run.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; run a hook
;;
(defun zenicb-run-hook (hooksym &rest args)
  "Take hook name HOOKSYM and run it, passing optional args ARGS.
HOOKSYM should be a symbol, a hook variable.
If the hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with args ARGS.
If it is a list, the elements are called, in order, with ARGS, if
zenicb-run-next-hook is t (the default). Otherwise, the hooks after
the one that set zenicb-run-next-hook are not called, and control is
returned to the caller. (zenicb-run-hook) returns the value returned 
from the last hook run."
      (let ((zenicb-run-next-hook t)
            (result))
        (and (boundp hooksym)
             (symbol-value hooksym)
             (let ((value (symbol-value hooksym)))
               (if (and (listp value) 
                        (not (eq (car value) 'lambda)))
                   (while (and value zenicb-run-next-hook)
                     (setq result (apply (car value) args))
                     (setq value (cdr value)))
                 (setq result (apply value args)))))
        result))
;;
;; add a function to a hook symbol
;;
(defun zenicb-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (or (if (consp function)
	  (member function (symbol-value hook))
	(memq function (symbol-value hook)))
      (set hook 
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))))))
;;
;; remove a function from a hook symbol
;;
(defun zenicb-delete-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `zenicb-add-hook'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (let ((hook-value (symbol-value hook)))
      (if (consp hook-value)
	  (setq hook-value (delete function hook-value))
	(if (equal hook-value function)
	    (setq hook-value nil)))
      (set hook hook-value))))

(fset 'zenicb-remove-hook 'zenicb-delete-hook)

;;
;; Convert a number of seconds since the epoch (in ASCII) into an
;; ASCII string representing the time.
;;
(defun zenicb-convert-date (seconds)
  (let (millions units high low)
    (if (string-match "\\(......\\)$" seconds)
	(setq millions (string-to-int (substring seconds 0 (match-beginning 1)))
	      units (string-to-int (substring seconds (match-beginning 1))))
      (setq millions 0
	    units (string-to-int seconds)))
    (setq high (+ (* millions 15) (/ (* millions 265) 1024) (/ units 65536))
	  low (+ (% (+ (* (% millions 4) 16384) (* millions 576)) 65536)
		 (% units 65536)))
    (if (> low 65535)
	(setq low (- low 65536)
	      high (1+ high)))
    (substring (current-time-string (cons high low)) 4 16)))
(if (string< emacs-version "19")
    ;; Emacs 18's (current-time-string) doesn't take an argument.
    (defun zenicb-convert-date (seconds)
      (format "%-12s" seconds)))
;;
;; Display the idle time in a nice format.
;;
(defun zenicb-convert-time (seconds)
  (let ((units (string-to-int seconds)))
    (cond
     ((< units 90) "-")
     (t (format "%dm" (/ (+ units 30) 60))))))
;;
;; Split MESSAGE into MAXLENGTH sized chunks, and call FUNCTION with each
;; piece, plus the additional ARGS.
;;
(defun zenicb-split (message maxlength function &rest args)
  (let ((length (length message)))
    (while (> length maxlength)
      (let ((string (substring message 0 maxlength)))
	(setq message (substring message maxlength))
	(setq length (- length maxlength))
	(apply function string args))))
  (apply function message args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server message hooks
;;;
;;; Server message hooks are called with two arguments. The first
;;; is the ZenICB process. The second is a list of the form:
;;;    (command arg1 arg2 ... argN)
;;; `command' is a character corresponding to the server message 
;;; letter (a-m). arg1 through argN are strings, the arguments to
;;; the command that the server sent, which were separated by C-a's
;;; in the unparsed message.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; b - public message
;;
(defun zenicb-server-b (proc parsedmsg)
  (zenicb-display-string proc (format "<%s> %s\n" (nth 0 parsedmsg) 
				      (nth 1 parsedmsg))))
;;
;; c - private message
;;
(defun zenicb-server-c (proc parsedmsg)
  (zenicb-display-string proc (format "*%s* %s\n" (nth 0 parsedmsg) 
				      (nth 1 parsedmsg))))
;;
;; d - status message
;;
(defun zenicb-server-d (proc parsedmsg)
  (zenicb-display-string proc (format "[info] %s: %s\n" (nth 0 parsedmsg)
				      (nth 1 parsedmsg))))
;;
;; e - error packet
;;
(defun zenicb-server-e (proc parsedmsg)
  (zenicb-display-string proc (format "[info] Error: %s\n" (nth 0 parsedmsg))))
;;
;; g - signoff
;;
(defun zenicb-server-g (proc parsedmsg)
  (zenicb-display-string 
   proc (format "[info] Server wants you to go away.\n")))
;;
;; i - command output
;;
(defun zenicb-server-i (proc parsedmsg)
  (let ((reply-type (nth 0 parsedmsg)))
    (cond 
     ((string= reply-type "wh") ; who reply header
      (zenicb-display-string
       proc (format "[info]  Nickname     Idle  Sign-On       Account\n")))
     ((string= reply-type "wl") ; who reply
      (zenicb-display-string 
       proc (format "[info] %s%-12s%5s  %s  %s@%s %s\n"
		    (if (string= (nth 1 parsedmsg) "m") "*" " ")
		    (nth 2 parsedmsg)
		    (zenicb-convert-time (nth 3 parsedmsg))
		    (zenicb-convert-date (nth 5 parsedmsg))
		    (nth 6 parsedmsg)
		    (nth 7 parsedmsg)
		    (nth 8 parsedmsg))))
     ((string= reply-type "co") ; comment
      (let ((message (nth 1 parsedmsg)))
	(if message
	    (let ((message
		   (if (string-match "\\( +\\)$" message)
		       (substring message 0 (match-beginning 1))
		     message)))
	      (zenicb-display-string proc (format "[info] %s\n" message)))
	  (zenicb-display-string proc "[info]\n"))))
     (t
      (zenicb-display-string
       proc (format "[debug] packet type i, subtype %s, data %s\n"
		    (prin1-to-string reply-type)
		    (prin1-to-string parsedmsg)))))))
(fset 'zenicb-server-h 'zenicb-server-i)
;;
;; j - protocol message
;;
(defun zenicb-server-j (proc parsedmsg)
  (zenicb-display-string proc "[info] You are wasting time.\n"))
;;
;; k - beep
;;
(defun zenicb-server-k (proc parsedmsg)
  (zenicb-display-string
   proc (format "[info] %s wants to annoy you.\n" (nth 0 parsedmsg))))
;;
;; l - ping
;;
(defun zenicb-server-l (proc parsedmsg)
;  (zenicb-display-string proc (format "[info] ping!\n"))
  (zenicb-send-string proc ?m ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; display byte count, time connected, etc.
;; /bcount [victim]
;;
(defun zenicb-command-bcount (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "m\C-aserver bcount " (cdr parsedcmd))))
;;
;; Reach out and beep someone
;;
(defun zenicb-command-beep (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "beep\C-a" (cdr parsedcmd))))
;;
;; Boot someone out of a group
;; /boot victim
;;
(defun zenicb-command-boot (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "boot\C-a" (cdr parsedcmd))))
;;
;; Cancel an invitation
;;
(defun zenicb-command-cancel (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "cancel\C-a" (cdr parsedcmd))))
;;
;; Change echoback status
;; /echo [on|off]
;;
(defun zenicb-command-echo (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "echoback\C-a" (cdr parsedcmd))))
;;
;; change current group
;; /group groupname (or /g, or /join)
;;
(defun zenicb-command-group (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "g\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-g 'zenicb-command-group)
(fset 'zenicb-command-join 'zenicb-command-group) ; for the irc-impaired
;;
;; get help
;; /help
;;
(defun zenicb-command-help (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver help")
  (zenicb-send-string proc ?h "m\C-aserver ?"))
;;
;; invite a victim, or show who is invited
;; /invite [victim]
;;
(defun zenicb-command-invite (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "invite\C-a" (cdr parsedcmd))))
;; 
;; private message
;; /m victim message or /msg victim message
;;
(defun zenicb-command-m (proc parsedcmd)
  (let* ((tmp (zenicb-parse-firstword (cdr parsedcmd)))
	 (victim (car tmp))
	 (message (cdr tmp)))
    (zenicb-split message 220 'zenicb-send-private proc victim)))
(defun zenicb-send-private (message proc victim)
  (zenicb-send-string proc ?h (concat "m\C-a" victim " " message)))
;; compatability for irc refugees
(fset 'zenicb-command-msg 'zenicb-command-m)
;;
;; Read some weird messages (I have no idea what this is)
;;
(defun zenicb-command-mess (proc parsecmd)
  (zenicb-send-string proc ?h "m\C-aserver mess"))
;;
;; Read the message-of-the-day
;; /motd
;;
(defun zenicb-command-motd (proc parsedcmd)
  (zenicb-send-string proc ?h "motd\C-a"))
;;
;; Change nicknames
;; /nick newnick
;;
(defun zenicb-command-nick (proc parsedcmd)
  ;; actually, we should parse return strings from the server to determine
  ;; what our nick is, and not keep state here, as this loses if someone
  ;; chooses an invalid nick, but there seems to be little standardization
  ;; between icb servers on the messages returned.
  (setq zenicb-nick (cdr parsedcmd))
  (zenicb-send-string proc ?h (concat "name\C-a" (cdr parsedcmd))))
;;
;; Set autoregister
;; /nosecure
;;
(defun zenicb-command-nosecure (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver nosecure"))
;;
;; Pass moderator status
;; /pass newmoderator
;;
(defun zenicb-command-pass (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "pass\C-a" (cdr parsedcmd))))
;;
;; quit icb
;; /quit
;;
(defun zenicb-command-quit (proc parsedcmd)
  (zenicb-display-string proc "[info] You are wasting time elsewhere.\n")
  (setq zenicb-active nil)
  (delete-process proc))
;;
;; Read stored message
;; /read
;;
(defun zenicb-command-read (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver read"))
;;
;; "secure" nick registration
;;
(defun zenicb-command-secure (proc parsedcmd)
  (zenicb-send-string proc ?h "m\C-aserver secure"))
;;
;; Display time to shutdown
;; /shuttime
;;
(defun zenicb-command-shuttime (proc parsedcmd)
  (zenicb-send-string proc ?h "M\C-aserver shuttime"))
;;
;; Change/view channel status
;; /status [modes]
;;
(defun zenicb-command-status (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "status\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-mode 'zenicb-command-status)
;;
;; set the topic for a group
;; /topic new-topic-string
;;
(defun zenicb-command-topic (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "topic\C-a" (cdr parsedcmd))))
;;
;; check version of icb
;; /version
;;
(defun zenicb-command-version (proc parsedcmd)
  (zenicb-send-string proc ?h "v\C-a"))
;;
;; See who's on ICB
;; /who [groupname] (groupname of . means current group)
;;
(defun zenicb-command-who (proc parsedcmd)
  (zenicb-send-string proc ?h (concat "w\C-a" (cdr parsedcmd))))
(fset 'zenicb-command-w 'zenicb-command-who)
;;
;; Get info about a user
;; /whois victim
;;
(defun zenicb-command-whois (proc parsedcmd)
  (zenicb-send-string
   proc ?h (concat "m\C-aserver whois " (cdr parsedcmd))))
;;
;; Save a message for a victim
;; /write victim message
;;
(defun zenicb-command-write (proc parsedcmd)
  (zenicb-send-string
   proc ?h (concat "m\C-aserver write " (cdr parsedcmd))))

(provide 'zenicb)
;;; End of zenicb.el
