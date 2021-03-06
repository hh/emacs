;;; multi-term.el --- Multi term buffer. 
;; Author: Andy Stewart <lazycat.mana...@gmail.com> 
;; Maintainer: Andy Stewart <lazycat.mana...@gmail.com> 
;; Copyright (C) 2008, Andy Stewart, all rights reserved. 
;; Created: 2008-09-19 23:02:42 
;; Version: 0.2 
;; Last-Updated: 2008-10-22 00:36:26 
;; URL: not distributed yet 
;; Keywords: term, multiple buffer 
;; Compatibility: GNU Emacs 23.0.60.1 
;; This program is free software; you can redistribute it and/or modify 
;; it under the terms of the GNU General Public License as published by 
;; the Free Software Foundation; either version 2, or (at your option) 
;; any later version. 
;; This program is distributed in the hope that it will be useful, 
;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details. 
;; You should have received a copy of the GNU General Public License 
;; along with this program; see the file COPYING.  If not, write to 
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth 
;; Floor, Boston, MA 02110-1301, USA. 
;; Features that might be requried by this library: 
;; 
;;  `term' 
;; 
;;; Installation: 
;; 
;; Copy multi-term.el to your load-path and add to your ~/.emacs 
;; 
;;  (require 'multi-term) 
;; 
;;  And I think add below sentence is nice 
;; 
;;  (multi-term-keystroke-setup) 
;; 
;;  For unbind the default conflict keystroke. 
;;; Customize 
;; 
;; `multi-term-program' default is nil, so when create new term buffer, 
;; send environment variable of `SHELL' to `make-term'. 
;; 
;; And you can set it to your like, like me: ;-) 
;; 
;; (setq multi-term-program "/bin/zsh") 
;; 
;; `multi-term-default-dir' default is `~/', only use when current buffer 
;; is not in real directory. 
;; 
;; `multi-term-current-window-height' is window height when use function 
;; `multi-term-switch-to-current-directory' to open term buffer with 
;; current directory. 
;; 
;; `multi-term-buffer-name' is a name of term buffer. 
;; 
;; `term-rebind-key-alist' is a key alist that rebind some keystroke. 
;; If you don't like default, just setup it with (KEY . COMMAND) format. 
;; 
;; Example: 
;;      (setq term-rebind-key-alist 
;;        '((KEY1 . COMMAND1) 
;;          (KEY2 . COMMAND2) 
;;          (KEY3 . COMMAND3))) 
;; 
;; NOTE: 
;;    If you modified this variable, ensure make is front at 
;;    (multi-term-keystroke-setup) 
;; 
;;; Commentary: 
;; 
;; This package is for create and manager multiple term buffer. 
;; 
;; Default, `term.el' can create a terminal buffer with `term-mode'. 
;; But have some discommoded point: 
;; 
;; 1 -> 
;;      term-mode just create one terminal buffer with `term' command. 
;;      But not command for quick create and switch with terminal buffers. 
;; 
;;      Now, use command `multi-term' can quick create new terminal buffer. 
;;      And use command `multi-term-next' or `multi-term-prev' can switch 
;;      next or previous terminal buffer quickly, whatever current buffer. 
;; 
;; 2 -> 
;;      Default, when use *NIX command `exit' from term-mode, will left 
;;      an unused buffer. 
;; 
;;      Now `multi-term' can handled this, close buffer when use `exit'. 
;; 
;; 3 -> 
;;      If you use command `kill-this-buffer' to kill terminal buffer forcible. 
;;      term-mode can't interrupt sub-process before kill buffer. 
;; 
;;      And `multi-term' do this now. 
;; 
;; 4 -> 
;;      And this is most import, `term-mode' is great for use shell in emacs. 
;;      But it's default keystroke conflict with some global keystroke (example: C-x). 
;; 
;;      Now `multi-term' unbind those conflict keystroke with `term-char-mode'. 
;; 
;;; Change log: 
;; 
;; 2008/10/22 
;;      Add variable `multi-term-current-window-height'. 
;;      Add variable `multi-term-buffer-name'. 
;; 
;;      Move key setup and some extension from `term-extension.el'. 
;;      Create new function `multi-term-keystroke-setup'. 
;; 
;;      Fix doc. 
;; 
;; 2008/09/19 
;;      First released. 
;; 
;;; Acknowledgments: 
;; 
;;      Mark Triggs     <m...@dishevelled.net>   for multi-shell.el 
;; 
;;; TODO 
;; 
;; None 
;; 
;;; Require: 
(require 'term) 
;;; Code: 
;;; Customize 
(defvar multi-term-program nil 
  "The program of term. 
If this is nil, setup it equal the environment variable of `SHELL'.") 
(defvar multi-term-default-dir "~/" 
  "The default directory that term create. 
If current local directory is in-existence.") 
(defvar multi-term-buffer-name "terminal" 
  "The buffer name of term buffer.") 
(defvar multi-term-current-window-height -13 
  "This value is window height when call `multi-term-switch-to-current-directory'.") 
(defvar term-unbind-key-list 
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>") 
  "The key list that need to unbind.") 
(defvar term-rebind-key-alist 
  '( 
    ("C-c C-c" . term-interrupt-subjob) 
    ("M-f" . term-send-forward-word) 
    ("M-b" . term-send-backward-word) 
    ("M-o" . term-send-backspace) 
    ("C-p" . previous-line) 
    ("C-n" . next-line) 
    ("M-p" . term-send-up) 
    ("M-n" . term-send-down) 
    ("C-s" . isearch-forward) 
    ("C-r" . isearch-backward) 
    ("M-M" . term-send-forward-kill-word) 
    ("M-N" . term-send-backward-kill-word) 
    ("M-." . comint-dynamic-complete) 
    ("M-," . term-send-input)) 
  "The key alist that need to rebind. 
If you not like default setup, modified it, with (KEY . COMMAND) format.") 
;;;###autoload 
(defun multi-term () 
  "Create new term buffer." 
  (interactive) 
  (let* ((term-buffer (multi-term-get-buffer))) 
    (set-buffer term-buffer) 
    (term-mode) 
    (term-char-mode) 
    (multi-term-handle-close) 
    (setq term-scroll-show-maximum-output nil 
          term-scroll-to-bottom-on-output nil) 
    (switch-to-buffer term-buffer) 
    (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook))) 
(defun multi-term-get-buffer () 
  "Get term buffer." 
  (let* ((term-list-length (length (multi-term-list)))          ;get length of term list 
         (index (if term-list-length (1+ term-list-length) 1))) ;setup new term index 
    (with-temp-buffer 
      ;; switch to current local directory, 
      ;; if in-existence, switch to `multi-term-default-dir'. 
      (cd (or default-directory (expand-file-name multi-term-default-dir))) 
      ;; adjust value N when max index of term buffer is less than length of term list 
      (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index))) 
        (setq index (1+ index))) 
      ;; make term, details to see function `make-term' in `term.el'. 
      (make-term 
       (format "%s<%s>" multi-term-buffer-name index) 
       (or multi-term-program (getenv "SHELL")))))) 
(defun multi-term-handle-close () 
  "This function for close current term buffer. 
When `exit' from term buffer." 
  (when (ignore-errors (get-buffer-process (current-buffer))) 
    (set-process-sentinel (get-buffer-process (current-buffer)) 
                          (lambda (proc change) 
                            (when (string-match "\\(finished\\|exited\\)" change) 
                              (kill-buffer (process-buffer proc))))))) 
(defun multi-term-kill-buffer-hook () 
  "Function that hook `kill-buffer-hook'." 
  ;; Interrupt the current subjob 
  ;; when have alive process with current term buffer 
  (when (and (eq major-mode 'term-mode) 
             (term-check-proc (current-buffer))) 
    (term-interrupt-subjob))) 
(defun multi-term-list () 
  "The term buffers presently active." 
  (sort 
   (remove-if-not (lambda (b) 
                    (string-match 
                     (concat "^\*" multi-term-buffer-name) 
                     (buffer-name b))) 
                  (buffer-list)) 
   (lambda (a b) 
     (< (string-to-number 
         (cadr (split-string (buffer-name a) "[<>]"))) 
        (string-to-number 
         (cadr (split-string (buffer-name b)  "[<>]"))))))) 
(defun multi-term-next () 
  "Go to the next term." 
  (interactive) 
  (multi-term-switch 'NEXT)) 
(defun multi-term-prev () 
  "Go to the previous term." 
  (interactive) 
  (multi-term-switch 'PREVIOUS)) 
(defun multi-term-switch (direction) 
  "If DIRECTION is `NEXT', switch to the next term. 
If `PREVIOUS', switch to the previous term." 
  (let ((terms (multi-term-list))) 
    (setf (cdr (last terms)) terms) 
    (let ((this-buffer (position (current-buffer) (multi-term-list)))) 
      (if this-buffer 
          (if (eql direction 'NEXT) 
              (switch-to-buffer (nth (1+ this-buffer) terms)) 
            (switch-to-buffer (nth (+ (1- (length (multi-term-list))) 
                                      this-buffer) terms))) 
        (switch-to-buffer (car terms)))))) 
(defun multi-term-switch-to-current-directory () 
  "Open term that start at current directory." 
  (interactive) 
  (split-window-vertically multi-term-current-window-height) 
  (other-window 1) 
  (multi-term)) 
(defun multi-term-keystroke-setup () 
  "Keystroke setup of `term-char-mode'. 
Default, the key binds of `term-char-mode' conflict with user's keys-tokes. 
So this function unbind some keys with `term-raw-map'. 
And rebind some keys-tokes with `term-raw-map'." 
  (interactive) 
  (add-hook 'term-mode-hook 
            (lambda () 
              ;; Unbind base key that conflict with user's keys-tokes. 
              (dolist (unbind-key term-unbind-key-list) 
                (define-key term-raw-map (read-kbd-macro unbind-key) nil)) 
              ;; Add some i use keys. 
              ;; If you don't like my keystroke, 
              ;; just modified `term-rebind-key-alist' 
              (dolist (element term-rebind-key-alist) 
                (define-key term-raw-map (read-kbd-macro (car element)) (cdr element))) 
              ))) 
(defun term-send-backward-kill-word () 
  "Backward kill word in term mode." 
  (interactive) 
  (term-send-raw-string "\C-w")) 
(defun term-send-forward-kill-word () 
  "Kill word in term mode." 
  (interactive) 
  (term-send-raw-string "\ed")) 
(defun term-send-backward-word () 
  "Move backward word in term mode." 
  (interactive) 
  (term-send-raw-string "\eb")) 
(defun term-send-forward-word () 
  "Move forward word in term mode." 
  (interactive) 
  (term-send-raw-string "\ef")) 
(provide 'multi-term) 
;;; multi-term.el ends here