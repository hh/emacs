<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: tempbuf.el</title><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/cgi-bin/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: tempbuf.el" href="http://www.emacswiki.org/cgi-bin/emacs?action=rss;rcidonly=tempbuf.el" /><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /></head><body class="http://www.emacswiki.org/cgi-bin/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/cgi-bin/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<br /><span class="specialdays">Niger, National Day</span><h1><a href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q=tempbuf.el">tempbuf.el</a></h1></div><div class="wrapper"><div class="content browse"><p><p><a href="http://www.emacswiki.org/cgi-bin/wiki/download/tempbuf.el">Download</a></p><pre class="source"><pre class="code"><span class="linecomment">;;; tempbuf.el --- kill unused buffers in the background</span>
<span class="linecomment">;; Copyright (c) 2001, 2002 Michele Bini</span>

<span class="linecomment">;; Author: Michele Bini &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;; Created: 11 Sep 2001</span>
<span class="linecomment">;; Version: 1.0</span>
<span class="linecomment">;; Keywords: convenience</span>

<span class="linecomment">;; This is free software; you can redistribute it and/or modify it</span>
<span class="linecomment">;; under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 2 of the License, or</span>
<span class="linecomment">;; (at your option) any later version.</span>

<span class="linecomment">;; This program is distributed in the hope that it will be</span>
<span class="linecomment">;; useful, but WITHOUT ANY WARRANTY; without even the implied</span>
<span class="linecomment">;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR</span>
<span class="linecomment">;; PURPOSE.  See the GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public</span>
<span class="linecomment">;; License along with this program; if not, write to the Free</span>
<span class="linecomment">;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,</span>
<span class="linecomment">;; MA 02111-1307 USA</span>

<span class="linecomment">;;; Commentary:</span>

<span class="linecomment">;; This package implements tempbuf-mode, a minor mode that enables</span>
<span class="linecomment">;; buffers to get automatically deleted in the background when it can</span>
<span class="linecomment">;; be deduced that they are no longer of any use.</span>

<span class="linecomment">;; It could be common for example to apply this mode to dired-mode</span>
<span class="linecomment">;; buffers or read-only buffers visiting files, relieving you from</span>
<span class="linecomment">;; having to delete each of them manually when the buffer list grows</span>
<span class="linecomment">;; too large.</span>

<span class="linecomment">;; The algorithm employed increases the life expectancy of the most</span>
<span class="linecomment">;; used buffers, even if their usage is intermittent.  A buffer is</span>
<span class="linecomment">;; considered to be in use if interactive commands are being executed</span>
<span class="linecomment">;; on it or if it is being displayed in some window.</span>
<span class="linecomment">;; Buffers visiting files with unsaved content or with active</span>
<span class="linecomment">;; processes will not get automatically deleted, but you can override</span>
<span class="linecomment">;; this behavior by customizing tempbuf-expire-hook.</span>

<span class="linecomment">;; To turn on this mode on dired buffers, put this line in your .emacs:</span>

<span class="linecomment">;;     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)</span>

<span class="linecomment">;; I find it also very convenient to turn on this mode in emacs</span>
<span class="linecomment">;; customization buffers, W3 (Emacs' Web Browser) buffers, UNIX 'man'</span>
<span class="linecomment">;; documentation buffers, and any buffer with view-mode activated.</span>

<span class="linecomment">;;     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)</span>
<span class="linecomment">;;     (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)</span>
<span class="linecomment">;;     (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)</span>
<span class="linecomment">;;     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)</span>

<span class="linecomment">;; It may also be reasonable to activate it by default on any visited</span>
<span class="linecomment">;; file buffer (buffers with unsaved content will not get automatically</span>
<span class="linecomment">;; deleted, anyway):</span>

<span class="linecomment">;;     (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)</span>

<span class="linecomment">;; You can set up things to make tempbuf-mode terminate idle</span>
<span class="linecomment">;; terminals:</span>

<span class="linecomment">;; (defun my-term-on-tempbuf-expire ()</span>
<span class="linecomment">;;   (when (get-buffer-process (current-buffer))</span>
<span class="linecomment">;;     (term-send-eof)))</span>
<span class="linecomment">;; (defun my-term-mode-patch ()</span>
<span class="linecomment">;;   (turn-on-tempbuf-mode)</span>
<span class="linecomment">;;   (add-hook 'tempbuf-expire-hook 'my-term-on-tempbuf-kill nil t))</span>
<span class="linecomment">;; (add-hook 'term-mode-hook 'my-term-mode-patch)</span>

<span class="linecomment">;; This way, a terminal emulator buffer will start receiving</span>
<span class="linecomment">;; periodic end-of-file signals if it does not seem to get any more</span>
<span class="linecomment">;; attention from the user, causing it to terminate if there is no</span>
<span class="linecomment">;; pending command (assuming that a well behaving shell is running on</span>
<span class="linecomment">;; that terminal).</span>

<span class="linecomment">;; I plan to release new versions in the following locations:</span>
<span class="linecomment">;; - http://www.emacswiki.org/cgi-bin/wiki.pl?TempbufMode</span>
<span class="linecomment">;; - the gnu.emacs.sources newsgroup</span>

<span class="linecomment">;;; History:</span>
<span class="linecomment">;; 2002-02-20  Michele Bini  &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 	* tempbuf.el (tempbuf-mode): Added "P" argument to</span>
<span class="linecomment">;; 	interactive, as it should be for a minor mode.</span>
<span class="linecomment">;; 	(turn-on-tempbuf-mode): removed call to make-local-hook; do</span>
<span class="linecomment">;; 	not alter tempbuf-minimum-timeout. Renamed from</span>
<span class="linecomment">;; 	tempbuf-mode-enable.</span>
<span class="linecomment">;; 	(turn-off-tempbuf-mode): Renamed from</span>
<span class="linecomment">;; 	tempbuf-mode-disable.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2002-02-17  Michele Bini  &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 	* tempbuf.el: Added customize definitions.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2001-12-21  Michele Bini  &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 	Release thru gnu.emacs.sources.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2001-12-16  Michele Bini  &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 	(tempbuf-buffers): Removed.</span>
<span class="linecomment">;; 	(tempbuf-expire-hook, tempbuf-expire): Added.</span>
<span class="linecomment">;; 	Buffers with active processes will no more be</span>
<span class="linecomment">;; 	automatically deleted.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2001-12-04  Michele Bini  &lt;mibin@libero.it&gt;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 	First public release on the Emacs Wiki</span>

<span class="linecomment">;;; Code:</span>

(defgroup tempbuf nil
  "<span class="quote">Kill unused buffers in the background.</span>"
  :group 'convenience :prefix 'tempbuf-)

(defcustom tempbuf-life-extension-ratio 2
  "<span class="quote">Ratio at which to extend the life expectancy of a used buffer.
This value should be greater than 1.</span>"
  :group 'tempbuf :type 'number)

(defcustom tempbuf-kill-message "<span class="quote">Killed inactive buffer: %s.</span>"
  "<span class="quote">Message used to signal the killing of a buffer.
If nil, do not show any message.  If a %s appears in the message, it
will get replaced with the name of the buffer being killed.</span>"
  :group 'tempbuf :type '(choice (const :tag "<span class="quote">No message.</span>" nil)
				 string))

(defcustom tempbuf-mode-hook nil
  "<span class="quote">Hook run after tempbuf mode is activated in a buffer.</span>"
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-expire-hook nil
  "<span class="quote">Hook run when a buffer expires to to inactivity.

The difference between this and `tempbuf-kill-hook' is that this hook
will be called even on buffers visiting files with unsaved content or
with active processes.  This hook can thus be used to kill, or perform
any other reasonable action on such buffers when they become
inactive.</span>"
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-kill-hook nil
  "<span class="quote">Hook run before a buffer gets killed due to inactivity.

It is possible for any function called by this hook to throw the tag
tempbuf-skip-kill.  When this happens the buffer killing will be
postponed.</span>"
  :group 'tempbuf :type 'hook)

(defcustom tempbuf-minimum-timeout 18
  "<span class="quote">Wait at least this many seconds before killing a buffer.
The actual timeout for killing a buffer increases with user activity
on it.  This value prevents a completely unused buffer from being
killed too early.</span>"
  :group 'tempbuf :type 'number)

(defcustom tempbuf-mode-line-string "<span class="quote"> TmpB</span>"
  "<span class="quote">String displayed on the mode line when tempbuf is active.</span>"
  :group 'tempbuf :type '(choice (const :tag "<span class="quote">No indicator.</span>" nil)
				 string))

(or (assq 'tempbuf-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(tempbuf-mode tempbuf-mode-line-string) minor-mode-alist)))

(defvar tempbuf-mode nil)
(make-variable-buffer-local 'tempbuf-mode)

<span class="linecomment">;;;###autoload</span>
(defun tempbuf-mode (&optional arg)
  "<span class="quote">Toggle tempbuf mode.

With prefix ARG, turn the mode on if ARG is positive.
After mode activation, `tempbuf-mode-hook' is run.</span>"
  (interactive "<span class="quote">P</span>")
  (cond
   ((null arg)
    (if tempbuf-mode (turn-off-tempbuf-mode) (turn-on-tempbuf-mode)))
   ((&gt; (prefix-numeric-value arg) 0) (turn-on-tempbuf-mode))
   (t (turn-off-tempbuf-mode))))

(defvar tempbuf-timer nil
  "<span class="quote">Timer used internally by tempbuf mode.</span>")

(defvar tempbuf-last-time nil
  "<span class="quote">Holds the last time a command was executed on the buffer.</span>")
(make-variable-buffer-local 'tempbuf-last-time)

(defvar tempbuf-timeout tempbuf-minimum-timeout
  "<span class="quote">Current timeout for buffer expiring.</span>")
(make-variable-buffer-local 'tempbuf-timeout)

(defvar tempbuf-activation-time nil
  "<span class="quote">Time at which tempbuf mode was activated in the current buffer.</span>")
(make-variable-buffer-local 'tempbuf-activation-time)

(defun tempbuf-time-diff (a b)
  "<span class="quote">Return the number of seconds between two timestamps A and B.</span>"
  (let ((diff 0.0))
    (setq diff (+ diff (- (car a) (car b))))
    (setq diff (* diff (* 256 256)))
    (setq diff (+ diff (- (cadr a) (cadr b))))
    (when (and (&lt; diff 4900.0) (cdr (cdr a)) (cdr (cdr b)))
      (setq diff
	    (+ diff (/ (float (- (cadr (cdr a)) (cadr (cdr b)))) 1000000))))
    diff))

(defun tempbuf-grace (&optional ct)
  "<span class="quote">Extend the life expectancy of the current buffer.

The optional argument CT specifies a pre-calculated \</span>"(current-time)\"<span class="quote">
value.</span>"
  (setq tempbuf-timeout
	(+ tempbuf-minimum-timeout
	   (* tempbuf-life-extension-ratio
	      (tempbuf-time-diff
	       (or ct (current-time))
	       tempbuf-activation-time)))))

(defun tempbuf-check-buffers ()
  "<span class="quote">Check all the buffers with tempbuf mode turned on.

Inactive buffers will expire and eventually get killed, active ones
will get additional life expectancy.</span>"
  (let ((ct (current-time)))
    (mapcar
     (lambda (buffer)
       (with-current-buffer buffer
	 (when tempbuf-mode
	   (if (get-buffer-window buffer t)
	       (progn
		 (tempbuf-post-command)
		 (tempbuf-grace ct))
	     (when (and (&gt; (tempbuf-time-diff ct tempbuf-last-time)
			   tempbuf-timeout))
	       (tempbuf-expire ct))))))
     (buffer-list))))

(defun tempbuf-expire (&optional ct)
  "<span class="quote">Expire the current buffer.

This function gets called after a certain period of inactivity in the
current buffer.
The hook `tempbuf-expire-hook' is run at first.
If the functions in that hook did not already take care about it,
the current buffer will be killed if it has no unsaved content and no
processes running.
The optional argument CT specifies a pre-calculated \</span>"(current-time)\"<span class="quote">
value.</span>"
  (let ((buffer (current-buffer)))
    (run-hooks 'tempbuf-expire-hook)
    (when (buffer-live-p buffer)
      (if (or buffer-offer-save
	      (and buffer-file-name (buffer-modified-p))
	      (get-buffer-process buffer))
	  (progn
	    (tempbuf-post-command)
	    (tempbuf-grace ct))
	(let ((name (buffer-name buffer)))
	  (catch 'tempbuf-skip-kill
	    (run-hooks 'tempbuf-kill-hook)
	    (kill-buffer buffer))
	  (when tempbuf-kill-message
	    (unless (buffer-live-p buffer)
	      (message tempbuf-kill-message
		       name))))))))

(defun tempbuf-post-command ()
  "<span class="quote">Update `tempbuf-last-time'.</span>"
  (setq tempbuf-last-time (current-time)))

<span class="linecomment">;;;###autoload</span>
(defun turn-on-tempbuf-mode ()
  "<span class="quote">Turn on tempbuf mode.

See also function `tempbuf-mode'.</span>"
  (when (not tempbuf-timer)
    (setq tempbuf-timer (run-at-time 15 15 'tempbuf-check-buffers)))
  (setq tempbuf-activation-time (current-time))
  (setq tempbuf-mode t)
  (tempbuf-grace)
  (add-hook 'post-command-hook 'tempbuf-post-command nil t)
  (tempbuf-post-command)
  (run-hooks 'tempbuf-mode-hook))

(defun turn-off-tempbuf-mode ()
  "<span class="quote">Turn off tempbuf mode.

See also function `tempbuf-mode'.</span>"
  (setq tempbuf-mode nil))

(provide 'tempbuf)
<span class="linecomment">;;; tempbuf.el ends here</span></span></pre></pre></p></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/cgi-bin/emacs/Suggestions">Suggestions</a> </span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/cgi-bin/emacs?action=edit;id=tempbuf.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/cgi-bin/emacs?action=history;id=tempbuf.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/cgi-bin/emacs?action=admin;id=tempbuf.el">Administration</a></span><span class="time"><br /> Last edited 2005-10-13 17:56 UTC by <a class="author" title="from 217-162-112-104.dclient.hispeed.ch" href="http://www.emacswiki.org/cgi-bin/emacs/AlexSchroeder">AlexSchroeder</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/cgi-bin/emacs?action=browse;diff=2;id=tempbuf.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p>
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>