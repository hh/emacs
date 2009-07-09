<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: tabbar-extension.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=tabbar-extension.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: tabbar-extension.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=tabbar-extension.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for tabbar-extension.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=tabbar-extension.el" /><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22tabbar-extension.el%22">tabbar-extension.el</a></h1></div><div class="wrapper"><div class="content browse"><p><a href="http://www.emacswiki.org/emacs/download/tabbar-extension.el">Download</a></p><pre class="code"><span class="linecomment">;;; tabbar-extension.el --- Some extension for tabbar.</span>

<span class="linecomment">;; Filename: tabbar-extension.el</span>
<span class="linecomment">;; Description: Some extension for tabbar.</span>
<span class="linecomment">;; Author: Andy Stewart lazycat.manatee@gmail.com</span>
<span class="linecomment">;; Maintainer: Andy Stewart lazycat.manatee@gmail.com</span>
<span class="linecomment">;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.</span>
<span class="linecomment">;; Created: 2008-11-04 10:39:03</span>
<span class="linecomment">;; Version: 0.1</span>
<span class="linecomment">;; Last-Updated: 2008-11-04 10:39:06</span>
<span class="linecomment">;;           By: Andy Stewart</span>
<span class="linecomment">;; URL:</span>
<span class="linecomment">;; Keywords: tabbar</span>
<span class="linecomment">;; Compatibility: GNU Emacs 23.0.60.1</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Features that might be required by this library:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; `tabbar'</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; This file is NOT part of GNU Emacs</span>

<span class="linecomment">;;; License</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This program is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 3, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; This program is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with this program; see the file COPYING.  If not, write to</span>
<span class="linecomment">;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth</span>
<span class="linecomment">;; Floor, Boston, MA 02110-1301, USA.</span>

<span class="linecomment">;;; Commentary:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Some extension for tabbar.</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Installation:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Put tabbar-extension.el to your load-path.</span>
<span class="linecomment">;; The load-path is usually ~/elisp/.</span>
<span class="linecomment">;; It's set in your ~/.emacs like this:</span>
<span class="linecomment">;; (add-to-list 'load-path (expand-file-name "~/elisp"))</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; And the following to your ~/.emacs startup file.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; (require 'tabbar-extension)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; No need more.</span>

<span class="linecomment">;;; Change log:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2008/11/04</span>
<span class="linecomment">;;      First released.</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Acknowledgements:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; TODO</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Require</span>
(require 'tabbar)

<span class="linecomment">;;; Code:</span>

(setq
 tabbar-scroll-left-help-function nil   <span class="linecomment">;don't show help information</span>
 tabbar-scroll-right-help-function nil
 tabbar-help-on-tab-function nil
 tabbar-home-help-function nil
 tabbar-buffer-home-button (quote (("<span class="quote"></span>") "<span class="quote"></span>")) <span class="linecomment">;don't show tabbar button</span>
 tabbar-scroll-left-button (quote (("<span class="quote"></span>") "<span class="quote"></span>"))
 tabbar-scroll-right-button (quote (("<span class="quote"></span>") "<span class="quote"></span>")))

(defun tabbar-select-end-tab ()
  "<span class="quote">Select end tab of current tabset.</span>"
  (interactive)
  (tabbar-select-beg-tab t))

(defun tabbar-select-beg-tab (&optional backward type)
  "<span class="quote">Select beginning tab of current tabs.</span>"
  (interactive)
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (setq tabset (tabbar-tabs tabset)
            tab (car (if backward (last tabset) tabset)))
      (tabbar-click-on-tab tab type))))

(defun tabbar-backward-tab-other-window (&optional reversed)
  "<span class="quote">Move to left tab in other window.
Optional argument REVERSED default is move backward, if reversed is non-nil move forward.</span>"
  (interactive)
  (other-window 1)
  (if reversed
      (tabbar-forward-tab)
    (tabbar-backward-tab))
  (other-window -1))

(defun tabbar-forward-tab-other-window ()
  "<span class="quote">Move to right tab in other window.</span>"
  (interactive)
  (tabbar-backward-tab-other-window t))

(provide 'tabbar-extension)

<span class="linecomment">;;; tabbar-extension.el ends here</span>

<span class="linecomment">;;; LocalWords:  tabset ttabset tabsets</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=tabbar-extension.el;missing=de_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=tabbar-extension.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=tabbar-extension.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=tabbar-extension.el">Administration</a></span><span class="time"><br /> Last edited 2009-01-10 17:34 UTC by <a class="author" title="from 13.142.212.222.broad.cd.sc.dynamic.163data.com.cn" href="http://www.emacswiki.org/emacs/Andy_Stewart">Andy Stewart</a></span><div style="float:right; margin-left:1ex;">
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

<p class="legal">
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
