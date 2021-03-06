;;; sb-welt-de.el --- shimbun backend for <http://www.welt.de>

;; Copyright (C) 2004 Andreas Seltenreich <seltenreich@gmx.de>

;; Author: Andreas Seltenreich <seltenreich@gmx.de>
;; Keywords: news, shimbun
;; Created: June 13, 2004

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; General national and international news in german. The items seem
;; directly forwarded from news agencies, so expect rather high
;; traffic.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-welt-de (shimbun-rss) ())

(defvar shimbun-welt-de-groups '("news"))

(defvar shimbun-welt-de-url "http://www.welt.de/z/newsticker/ticker_welt.xml")
(defvar shimbun-welt-de-content-start "</b></p>")
(defvar shimbun-welt-de-content-end "<noscript>")

(luna-define-method shimbun-groups ((shimbun shimbun-welt-de))
  shimbun-welt-de-groups)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-welt-de) url date)
  (unless (string-match "\\?nid=\\([0-9]+\\)" url)
    (error "Cannot find message-id"))
  (format "<%s@www.welt.de>" (match-string 1 url)))

;; Kill Javascript
(luna-define-method shimbun-clear-contents
  :before ((shimbun shimbun-welt-de) headers)

  (let ((case-fold-search t)
	javascript-image)
    (goto-char (point-min))
    (if (re-search-forward
	 "<img src=\"\\([^\"]+?\\)_thumbnail.jpg" nil t)
	(setq javascript-image (match-string-no-properties 1)))

    (shimbun-remove-tags "<table" "</table>")

    (when javascript-image
      (goto-char (point-min))
      (re-search-forward shimbun-welt-de-content-start)
      (insert (concat "<img src=\"" javascript-image
		      "_onlineBild.jpg\">")))))

(luna-define-method shimbun-groups ((shimbun shimbun-welt-de))
  shimbun-welt-de-groups)

(luna-define-method shimbun-index-url ((shimbun shimbun-welt-de))
  shimbun-welt-de-url)

(provide 'sb-welt-de)

;;; sb-welt-de.el ends here
