(add-to-list 'load-path "~/elisp/")

;; Use ir_black from TextMate
(load-file "~/elisp/color-theme.el")
(color-theme-ir_black)

;; Highlight Current Line
(load-file "~/elisp/highlight-current-line.el")
(highlight-current-line-set-bg-color "grey20")
(highlight-current-line-on t)

;; Backup file ~X~ Settings
(setq version-control t)
(setq vc-make-backup-files t)
(setq delete-old-versions 'yes)

;; Modeline
;(display-battery-mode)
;(display-time-mode)
(size-indication-mode)

;; Other Interesting Tidbits
(global-font-lock-mode t)
(setq inhibit-startup-screen t)		; Sensible for new users...
(setq disabled-function nil)	; Enable all disabled hooks.

(setq compilation-scroll-output t)

;; Sugar is bad for you.
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
;(menu-bar-mode -1)
;(tool-bar-mode nil)
;(menu-bar-mode nil)
;(scroll-bar-mode nil)

;(defun mynewframe ()
;(set-default-font "Purisa");)
;'(before-make-frame-hook mynewframe)
;'(after-make-frame-functions (mynewframe))

;; Load system specific settings.
(let ((site-file (concat "~/.emacs:" (system-name))))
  (when (file-exists-p site-file)
    (load-file site-file)))

;; Fire up so we can use `emacsclient'.
(server-start)

;; IPython rocks
;(load-file "~/.emacs.d/ipython.el")

;; trac-wiki
;(require 'trac-wiki)
;(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)
(load-file "~/.emacs.d/xml-rpc.el")
(load-file "~/.emacs.d/trac-wiki.el")
(trac-wiki-define-project "cc" "https://s.codecafe.com/trac/codecafe/" t)
(trac-wiki-define-project "cit" "https://trac.isgenesis.com/it/" t)

; This does SOMETHING interesting, but I'm not sure yet, it breaks because of parenthesis not matching
;; Generate and load our own autoload file.
;; (require 'autoload)
;; (when (file-directory-p "~/elisp")
;;    (add-to-list 'load-path "~/elisp")
;;    (let ((generated-autoload-file "~/elisp/loaddefs.el"))
;;      (when (fboundp 'update-directory-autoloads)
;;        (update-directory-autoloads "~/elisp"))
;;      (when (fboundp 'custom-autoload)
;;        (load-file generated-autoload-file))))

;; When mutt calls us for editing, use a nice mail mode....
(setq
      auto-mode-alist
     ( cons '("/tmp/mutt.*$" . mail-mode) auto-mode-alist )
   )
(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;;(setq tramp-default-method "ssh")
;;(add-to-list 'tramp-default-method-alist '("quadx.local" "mcclimans" "ssh"))

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)



;; ;; IMAP
;; (setq elmo-imap4-default-server "imap.gmail.com")
;; (setq elmo-imap4-default-user "chris@hippiehacker.org")
;; (setq elmo-imap4-default-authenticate-type 'clear)
;; (setq elmo-imap4-default-port '993)
;; (setq elmo-imap4-default-stream-type 'ssl)

;; (setq elmo-imap4-use-modified-utf7 t)

;; ;; SMTP
;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
;; (setq wl-smtp-authenticate-type "plain")
;; (setq wl-smtp-posting-user "chris@codecafe.com")
;; (setq wl-smtp-posting-server "smtp.gmail.com")
;; (setq wl-local-domain "codecafe.com")

;; (setq wl-default-folder "%inbox")
;; (setq wl-default-spec "%")
;; (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
;; (setq wl-trash-folder "%[Gmail]/Trash")

;; (setq wl-folder-check-async t)

;; ;; (setq elmo-imap4-use-modified-utf7 t)

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))

;; By default, `From:', `Subject:', `To:' and `Cc:' is available.  If you
;; want to decide destination by other header fields, set the variable
;; `elmo-msgdb-extra-fields' like following.

;;(setq elmo-msgdb-extra-fields
;;      '("x-ml-name"
        ;; "reply-to"
        ;; "sender"
;;        "mailing-list"))
;; ;;(require 'mailcrypt)
;; (add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

;; (defun mc-wl-verify-signature ()
;;   (interactive)
;;   (save-window-excursion
;;     (wl-summary-jump-to-current-message)
;;     (mc-verify)))

;; (defun mc-wl-decrypt-message ()
;;   (interactive)
;;   (save-window-excursion
;;     (wl-summary-jump-to-current-message)
;;     (let ((inhibit-read-only t))
;;       (mc-decrypt))))

;; (eval-after-load "mailcrypt"
;;   '(setq mc-modes-alist
;;        (append
;;         (quote
;;          ((wl-draft-mode (encrypt . mc-encrypt-message)
;;             (sign . mc-sign-message))
;;           (wl-summary-mode (decrypt . mc-wl-decrypt-message)
;;             (verify . mc-wl-verify-signature))))
;;         mc-modes-alist)))


;;(w3m-add-w3m-initial-frames x-dnd-init-frame)


;;'(set-face-font default -unknown-Purisa-light-normal-normal-*-*-*-*-*-*-0-iso10646-1)
;; '(custom-set-variable
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)set-variable
;;  '(display-battery-mode t)
;;  '(display-time-mode t)
;;  '(fringe-mode (quote (0)) nil (fringe))
;;  '(initial-scratch-message nil)
;;  '(setq jabber-account-list (quote (("chris@hippiehacker.org" (:password . "PASSWORD") (:network-server . "talk.google.com") (:port . 5222) (:connection-type . starttls)))))
;;  '(save-place t nil (saveplace))
;;  '(scroll-bar-mode nil)
;;  '(size-indication-mode t)
;;  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
;;  '(w3m-default-display-inline-images t)
;;  '(w3m-use-cookies t))
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(browse-url-browser-function (quote w3m-browse-url))
;;  '(ecb-options-version "2.40")
;;  '(jabber-account-list (quote (("chris@hippiehacker.org/emacs" (:password . "PASSWORD") (:network-server . "talk.google.com") (:connection-type . ssl)) ("chris@codecafe.com/emacs" (:password . "PASSWORD") (:network-server . "talk.google.com") (:connection-type . ssl)))))
;;  '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
;;  '(truncate-partial-width-windows nil)
;;  '(undo-outer-limit 20000000)
;;  '(w3m-default-display-inline-images t)
;;  '(w3m-use-cookies t)
;;  '(wl-demo nil)
;;  '(wl-draft-use-cache t))


     ;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
     ;; (if (boundp 'mail-user-agent)
     ;;     (setq mail-user-agent 'wl-user-agent))
     ;; (if (fboundp 'define-mail-user-agent)
     ;;     (define-mail-user-agent
     ;;       'wl-user-agent
     ;;       'wl-user-agent-compose
     ;;       'wl-draft-send
     ;;       'wl-draft-kill
     ;;       'mail-send-hook))

;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\." nil "/ssh:bird@bastion.your.domain:"))

;;; .emacs --- Emacs settings

;; (when (featurep 'xemacs)
;;   (error "Please don't use Lucid Emacs.  GNU Emacs is a better trip."))

;; ;; Load private stuff.
;; (when (file-exists-p "~/private/emacs-private.el")
;;   (unless (= (file-modes "~/private/emacs-private.el") 384)
;;     (error "People can read your ~/private/emacs-private.el!!!"))
;;   (load-file "~/private/emacs-private.el"))






;; Mail settings.
;; (setq mail-archive-file-name "~/RMAIL.outbox")
;; (setq mail-yank-ignored-headers "^.*:")

;; (setq rmail-displayed-headers "^From:\\|^To:\\|^Subject:\\|^Date:\\|^Cc:")
;; (setq rmail-confirm-expunge nil)


;; IRC settings.
;; (add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode 1)))


;;; .emacs ends here
;; (global-font-lock-mode t)
;; (setq compilation-scroll-output t)

;; (add-to-list 'load-path "~/elisp/ecb")
;; (add-to-list 'load-path "~/elisp/mtorus")
;; (add-to-list 'load-path "~/elisp/wiki")
;; (add-to-list 'load-path "~/elisp/etask")
;; (add-to-list 'load-path "~/elisp/emacs-w3m")

;; (load-file "~/elisp/cedet/common/cedet.el")

;; (load-file "~/elisp/zenicb.el")
;; (require 'color-theme)
;; (require 'ecb)
;; ;(global-set-key [(control o)] 'ido-find-file) ; use Ctrl-o to open a (new) file
;; ;(global-set-key [(control s)] 'save-buffer) ; save  with Ctrl-s
;; (load-file "~/elisp/xcscope.el")
;; (load-file "~/elisp/NetBSD.el")
;; ;(global-set-key [(meta q)]    'kill-this-buffer)
;; (setq semantic-load-turn-useful-things-on t)
;; (require 'xcscope)

;; ;;(load-file "~/elisp/tuareg/tuareg.el")
;; (load-file "~/elisp/tabbar.el")
;; ;; (autoload 'tuareg-mode "tuareg-mode"
;; (setq auto-mode-alist
;;       (append '(("\\.ml$" . tuareg-mode)) auto-mode-alist))
;; (setq interpreter-mode-alist (append '(("ocaml" . tuareg-mode))
;; 				     interpreter-mode-alist))


;; ;(global-set-key [(control f)] 'isearch-forward-regexp)
;; ;(global-set-key [(control g)] 'isearch-repeat-forward)

;; (setq ecb-tip-of-the-day nil)

;; (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))

;; (setq ecb-tip-of-the-day nil)
;; (setq ecb-tree-buffer-style (quote image))
;; (setq ecb-tree-indent 4)
;; (setq ecb-windows-height 0.5)
;; (setq ecb-windows-width 0.15)
;; (setq semantic-load-turn-useful-things-on t)

;; ;; Configuration variables here:
;; ;; Load the semantic mode
;; (require 'semantic-util)
;; (setq semantic-load-turn-everything-on t)
;; (require 'semantic-load)


;; ;; Show column number at bottom of screen..
;; (column-number-mode 1)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(browse-url-browser-function (quote w3m-browse-url))
;;  '(ecb-options-version "2.40")
;;  '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
;;  '(truncate-partial-width-windows nil)
;;  '(undo-outer-limit 20000000))
;; ;;'(vc-handled-backends (quote (SVN))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(background "blue")
;;  '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
;;  '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
;;  '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
;;  '(font-lock-doc-string-face ((t (:foreground "green2"))))
;;  '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
;;  '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
;;  '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
;;  '(font-lock-reference-face ((t (:foreground "DodgerBlue"))) t)
;;  '(font-lock-string-face ((t (:foreground "LimeGreen"))))
;;  '(font-lock-type-face ((t (:foreground "#9290ff"))))
;;  '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
;;  '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
;;  '(highlight ((t (:background "CornflowerBlue"))))
;;  '(highlight-current-line-face ((t (:background "#333333"))))
;;  '(list-mode-item-selected ((t (:background "gold"))))
;;  '(makefile-space-face ((t (:background "wheat"))) t)
;;  '(mode-line ((t (:background "Navy"))))
;;  '(paren-match ((t (:background "darkseagreen4"))))
;;  '(region ((t (:background "DarkSlateBlue"))))
;;  '(show-paren-match-face ((t (:foreground "black" :background "wheat"))))
;;  '(show-paren-mismatch-face ((((class color)) (:foreground "white" :background "red"))))
;;  '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
;;  '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
;;  '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
;;  '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
;;  '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:underline t :foreground "Blue"))))
;;  '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:foreground "darkGreen"))))
;;  '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:underline t :foreground "darkGreen"))))
;;  '(vhdl-speedbar-package-face ((((class color) (background dark)) (:foreground "black"))))
;;  '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:underline t :foreground "black"))))
;;  '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "Yellow"))))
;;  '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue")))))
;; ;; Xrefactory configuration part ;;
;; ;; some Xrefactory defaults can be set here
;; ;; 					(defvar xref-current-project nil) ;; can be also "my_project_name"
;; ;; 					(defvar xref-key-binding 'global) ;; can be also 'local or 'none
;; ;; 					(setq load-path (cons "~/elisp/xref/emacs" load-path))
;; ;; 					(setq exec-path (cons "~/elsip/xref" exec-path))
;; ;; 					(load "xrefactory")
;; ;; ;; end of Xrefactory configuration part ;;
;; ;; 					(message "xrefactory loaded")

;; ;;(setq compile-command "/src/bb/src/build.sh -m bebox -j2 kernel=GENERIC")
;; (setq compile-command "cd ~/trunk/soap/Luci/ && rake")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.

;; (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; (if (and (boundp 'window-system) window-system)
;;     (when (string-match "XEmacs" emacs-version)
;;       (if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
;; 	  (require 'sym-lock))
;;       (require 'font-lock)))
;; ;;(ansi-term "/bin/ksh")
;; (setq inhibit-startup-message t)
;; (setq tramp-default-method "ssh")

;; 					; Planner stuff

;; (add-to-list 'load-path "~/elisp/wiki")
;; (load-file "~/elisp/wiki/emacs-wiki.el")
;; (load-file "~/elisp/etask/etask.el")

;; (add-to-list 'load-path "~/elisp/muse/lisp")
;; (add-to-list 'load-path "~/elisp/planner")
;; 					;(add-to-list 'load-path "~/elisp/planner/contrib")
;; (add-to-list 'load-path "~/elisp/remember")
;; (add-to-list 'load-path "~/elisp/elib")
;; (add-to-list 'load-path "~/elisp/jde/lisp")
;; 					;(require 'emacs-wiki)
;; (require 'planner)

;; (require 'remember-planner)
;; (require 'planner-publish)

;; (setq planner-project "WikiPlanner")
;; (setq muse-project-alist
;;       '(("WikiPlanner"
;; 	 ("~/plans" ;; Or wherever you want your planner files to be
;; 	  :default "TaskPool"
;; 	  :major-mode planner-mode
;; 	  :visit-link planner-visit-link)
;; 	 (:base "planner-xhtml"
;; 	  :path "~/plans_html"))))

;; (setq remember-handler-functions '(remember-planner-append))
;; (setq remember-annotation-functions planner-annotation-functions)
;; (global-set-key (kbd "C-c r") 'remember)
;; (global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)
;; (global-set-key (kbd "<f9> p") 'muse-publish-this-file)
;; (global-set-key (kbd "<f9> n") 'planner-goto-today)


;; 					;(setq debug-on-error t)


;; (require 'muse-wiki)
;; (setq muse-wiki-allow-nonexistent-wikiword t)
;; (setq calendar-mark-diary-entries-flag t)
;; (require 'timeclock)
;; (defun my-ansi-term () (interactive)(ansi-term "/bin/bash")) (global-set-key "\C-t" 'my-ansi-term)
;; (require 'ido)
;; (ido-mode t)
;; (load-file "~/elisp/elib/elib-node.el")

;; ;;(load-file "~/elisp/jde/lisp/jde.elc")
;; ;;(require 'elib-mode)
;; ;;(require 'jde)

;; ;; (defun start-zsh (&optional new)
;; ;;   "Switch to the zsh buffer or start one if none exists."
;; ;;   (interactive "P")
;; ;;   (if new
;; ;;       (ansi-term "/bin/zsh" "zsh")
;; ;;     (if (get-buffer "*zsh*")
;; ;; 	(switch-to-buffer "*zsh*")
;; ;;       (ansi-term "/usr/pkg/bin/zsh" "zsh"))))
;; ;; Global binding to access a zsh shell
;; ;;(global-set-key [(control c) (control v)] 'start-zsh)


;; (setq iswitch-mode t)
;; (require 'uniquify)

;; (setq uniquify-buffer-name-style 'forward)
;; ;; Moving cursor down at bottom scrolls only a
;; ;; single line, not half page
;; (setq scroll-step 1)
;; (setq scroll-conservatively 5)
;; (fset 'yes-or-no-p 'y-or-n-p)
;; ;;{{{ VI style paren matching.
;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;; 	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;; 	(t (self-insert-command (or arg 1)))))
;; ;;}}}
;; (global-set-key "%" 'match-paren)	; vi style paren matching.
;; (defun autocompile ()
;;   "compile itself if ~/.emacs"
;;   (interactive)
;;   (if (string= (buffer-file-name) (concat default-directory ".emacs"))
;;       (byte-compile-file (buffer-file-name))))
;; (add-hook 'after-save-hook 'autocompile)
;; (setq mouse-autoselect-window t)

;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ;;(setq tramp-default-method "ssh")
;; ;;(setq default-cursor-type 'bar)

;; (setq font-lock-maximum-decoration t)
;; (setq display-time-24hr-format t)	; time in mil style.
;; (setq display-time-day-and-date t)
;; (setq column-number-mode t)		; show current column

;; ;;{{{ grep visited buffers

;; ;;(defun visited-files (&optional predicate)
;; ;;  "Return a list of all the files being visited in any buffer.
;; ;;  If the optional PREDICATE is specified, only file names satisfying it
;; ;;  are included in the list."
;; ;;  (apply `nconc
;; ;;    (mapcar (lambda (buffer)
;; ;;         (let ((file (buffer-file-name buffer)))
;; ;;           (if (and file
;; ;;               (or (null predicate)
;; ;;              (funcall predicate file)))
;; ;;          (list file))))
;; ;;       (buffer-list))))

;; 					;
;; ;;(defun local-visited-files ()
;; ;;  "Return a list of the local files being visited in any buffer."
;; ;;  (visited-files (cond ((featurep `ange-ftp)
;; ;;         (lambda (file)
;; ;;           (not (ange-ftp-ftp-name file))))
;; 					; ;            ((featurep `efs)
;; ;;         (lambda (file)
;; 					; ;          (not (efs-ftp-path file))))
;; ;;             (t nil))))

;; ;;(defun grep-visited-files (command)
;; ;;  "*Run `grep` COMMAND on all visited files."
;; 					; ; (interactive
;; ;;   (let* ((directory-abbrev-alist
;; ;;      (cons (cons (expand-file-name default-directory)
;; ;;             "./")
;; ;;       directory-abbrev-alist))
;; ;;     (local-visited-files
;; ;;      (mapcar `abbreviate-file-name
;; ;;         (local-visited-files))))
;; ;;     (list (read-from-minibuffer "Run grep (like this): "
;; ;;             (cons (mapconcat `identity
;; ;;                    (cons grep-command
;; ;;                     local-visited-files)
;; ;;                    " ")
;; ;;                   (1+ (length grep-command)))
;; ;;             nil nil `grep-history))))
;; ;;  (grep command))
;; ;;}}}

;; (setq special-display-buffer-names (nconc '("*Backtrace*"
;; 					    "*VC-log*"
;; 					    "*cscope*"
;; 					    "*compile*"
;; 					    "*grep*")
;; 					  special-display-buffer-names) )

;; (load-file "~/elisp/psvn.el")
;; ;;(setq compile-command "export QTDIR=/usr/pkg/qt4; export PATH=/usr/pkg/qt4/bin:$PATH;qmake -project && qmake && make")
;; (setq compile-command "/root/luci/trunk/soap/soap_unittest.rb")

;; (display-time)
;; ;(server-start)
;; (require 'doc-view)
;; (fset 'yes-or-no-p 'y-or-n-p)
;; ;;(plan)
;; (setq transient-mark-mode t)

;; (windmove-default-keybindings 'meta)
;; (setq ido-enable-flex-matching t)
;; ;(global-set-key [(f6)] 'recompile)
;; (global-set-key [(f12)] 'comment-region)
;; (global-set-key [(f11)] 'uncomment-region)

;; (require 'w3m-load)
;; (setq mail-user-agent "mh-e-user-agent")
;; (setq read-mail-command "mh-rmail")
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; (setq mp3play-dirlist '("~/mp3"))
;; (autoload 'mp3play "mp3play" nil t)

;; (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
;; (setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; (defun my-getmail () (interactive)(shell-command "/stick/gm &")) (global-set-key [(f10)] 'my-getmail)

;; (add-hook 'write-file-hooks
;; 'delete-trailing-whitespace)

;; (load-file "~/elisp/snippet.el")
;; (load-file "~/elisp/find-recursive.el")

;; (load-file "~/elisp/php-mode.el")

;; ;; Angry Fruit Salid here
;;  (defun egoge-wash-out-colour (colour &optional degree)
;;   "Return a colour string specifying a washed-out version of COLOUR."
;;   (let ((basec (color-values
;; 		(face-attribute 'default :foreground)))
;; 	(col (color-values colour))
;; 	(list nil))
;;     (unless degree (setq degree 2))
;;     (while col
;;       (push (/ (/ (+ (pop col)
;; 		     (* degree (pop basec)))
;; 		  (1+ degree))
;; 	       256)
;; 	    list))
;;     (apply 'format "#%02x%02x%02x" (nreverse list))))

;;  (defun egoge-wash-out-face (face &optional degree)
;;    "Make the foreground colour of FACE appear a bit more pale."
;;    (let ((colour (face-attribute face :foreground)))
;;      (unless (eq colour 'unspecified)
;;        (set-face-attribute face nil
;;  			  :foreground (egoge-wash-out-colour colour degree)))))

;;  (defun egoge-find-faces (regexp)
;;    "Return a list of all faces whose names match REGEXP."
;;    (delq nil
;;  	(mapcar (lambda (face)
;;  		  (and (string-match regexp
;;  				     (symbol-name face))
;;  		       face))
;;  		(face-list))))

;;  (defun egoge-wash-out-fontlock-faces (&optional degree)
;;    (mapc (lambda (elt)
;;  	  (egoge-wash-out-face elt degree))
;;  	(delq 'font-lock-warning-face
;;  	      (egoge-find-faces "^font-lock"))))

;;  (when (> (length (defined-colors)) 16)
;;    (egoge-wash-out-fontlock-faces 1))

;; (global-set-key (kbd "C-=") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)


;; ;; (if (not (eq (string-match "23" (emacs-version)) nil))
;; ;;      (push '(font . "Pragmata-12:Medium") default-frame-alist)
;; ;;      (push '(font . "Monaco-12:bold") default-frame-alist)
;; ;; ;;     (push '(font . "7x13") default-frame-alist)
;; ;; )

;; ;;     (push '(font . "Liberation Mono-12:bold") default-frame-alist)
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion


;; ;;raise hell

;; ;; (global-set-key "^X!" 'shell)
;; ;; (global-set-key "^Xx" 'send-to-buffer) ;;copies from top buffer to bottom
;; ;; (fset 'send-to-buffer "\C-@\C-[\C-f\C-e\C-[w\C-xo\C-y\C-m\C-xo\C-e\C-[OC")


;; ;; ;;
;; ;; ;; In addition, you can also add this line too.
;; ;; ;;
;; ;; ;; ;; do overlay

;; ;;(load-file "~/elisp/pabbrev.el")
;; ;;(require 'pabbrev)


;(load-file "~/elisp/tabbar.el")
;(require 'tabbar-extension)
;(tabbar-mode t)
;; (setq tabbar-buffer-groups-function
;;          (lambda ()
;;             (list "All")))

;; (set-face-attribute
;;  'tabbar-default-face nil
;;  :background "gray60")
;; (set-face-attribute
;;  'tabbar-unselected-face nil
;;  :background "gray85"
;;  :foreground "gray30"
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-selected-face nil
;;  :background "#f2f2f6"
;;  :foreground "black"
;;  :box nil)
;; (set-face-attribute
;;  'tabbar-button-face nil
;;  :box '(:line-width 1 :color "gray72" :style released-button))
;; (set-face-attribute
;;  'tabbar-separator-face nil
;;  :height 0.7)
;; (tabbar-mode 1)
;; (define-key global-map [(alt j)] 'tabbar-backward)
;; (define-key global-map [(alt k)] 'tabbar-forward)



;; ;; ############################################################################################################################
;; ;; ### Ruby Stuff
;; ;; (autoload 'run-ruby "inf-ruby"
;; ;;   "Run an inferior Ruby process")
;; ;; (autoload 'inf-ruby-keys "inf-ruby"
;; ;;   "Set local key defs for inf-ruby in ruby-mode")
;; ;; (add-hook 'ruby-mode-hook
;; ;; 	  '(lambda ()
;; ;; 	     (inf-ruby-keys)))
;; ;; (add-to-list 'load-path "~/elisp/emacs-rails")
;; ;; (require 'rails)
;; ;;(require 'ruby-mode)
;; ;; (autoload 'ruby-mode "ruby-mode"
;; ;;   "Mode for editing ruby source files" t)
;; ;; (setq auto-mode-alist
;; ;;       (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
;; ;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;; ;; 				     interpreter-mode-alist))
;; ;; (setq auto-mode-alist
;; ;;       (append '(("\\.rhtml$" . ruby-mode)) auto-mode-alist))
;; ;(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
;; ;(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; ;;(defvar ruby-tag-face (make-face 'ruby-tag-face))
;; ;;(defvar ruby-variable-face (make-face 'ruby-variable-face))
;; ;;(set-face-background 'ruby-tag-face "Aquamarine")
;; ;;(set-face-foreground 'ruby-tag-face "Black")
;; ;;(set-face-background 'ruby-variable-face "Plum")
;; ;;(set-face-foreground  'ruby-variable-face "Black")
;; ;;(font-lock-add-keywords
;; ;;'ruby-mode

;; ;; (setq ruby-block-highlight-toggle 'overlay)
;; ;; (setq ruby-block-highlight-toggle 'minibuffer)
;; ;; (setq ruby-block-highlight-toggle t)
;; ;; (defun ruby-highlight-var-parens ()
;; ;;   (interactive "p")
;; ;;   (highlight-regexp "{%.*?%}" 'hi-orange))
;; ;; (add-hook 'ruby-mode-hook 'ruby-highlight-var-parens)
;; ;; (load-file "~/elisp/inf-ruby.el")
;; ;; ;;(setq ri-ruby-script "~/elisp/ri-emacs.rb")
;; ;; ;; (autoload 'ri "~/elisp/ri-ruby.el" nil t)
;; ;; ;;/usr/pkg/lib/ruby/gems/1.8/gems/fastri-0.3.1.1/bin
;; ;; (setq ri-ruby-script
;; ;;       "/usr/pkg/lib/ruby/gems/1.8/gems/fastri-0.3.1.1/bin/ri-emacs")
;; ;; (autoload 'ri
;; ;;   "~/elisp/ri-ruby.el" nil t)
;; ;;(load-file "~/elisp/ruby-electric.el")
;; ;; (eval-after-load 'ruby-mode
;; ;;   '(progn
;; ;;      (require 'ruby-compilation)
;; ;;      (add-hook 'ruby-mode-hook 'inf-ruby-keys)
;; ;;      (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
;; ;;      (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
;; ;;      (define-key ruby-mode-map (kbd "C-c l") "lambda")))

;; (global-set-key (kbd "C-h r") 'ri)
;; (require 'ruby-block)
;; (ruby-block-mode t)
;; (require 'hideshow-org)
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
;; (dolist (hook (list 'emacs-lisp-mode-hook
;; 		    'ruby-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))
;; (defun ruby-custom-setup ()
;;   (add-to-list 'hs-special-modes-alist
;; 	       '(ruby-mode
;; 		 "\\(def\\|do\\)"
;; 		 "end"
;; 		 "#"
;; 		 (lambda (arg) (ruby-end-of-block))
;; 		 nil
;; 		 ))
;;   (hs-minor-mode t)
;; )

;; (add-hook 'ruby-mode-hook 'ruby-custom-setup)
;; ;;(add-to-list 'load-path "~/elisp/yasnippet")
;; (require 'yasnippet-bundle)
;; (setq linum-mode t)

;; ;; (progn
;; ;;   (imenu--cleanup)
;; ;;   (setq imen)
;; ;;(add-hook 'ruby-mode-hook
;;  ;; (lambda()
;; (global-set-key (kbd "C-c <right>") 'hs-show-block)
;; (global-set-key (kbd "C-c <left>")  'hs-hide-block)
;; (global-set-key (kbd "C-c <up>")    'hs-hide-all)
;; (global-set-key (kbd "C-c <down>")  'hs-show-all)
;; ;(hs-minor-mode t)))
;; (add-to-list 'which-func-modes 'ruby-mode)
;; (which-function-mode t)
;; ;;(global-linum-mode)
;; (global-set-key [(end)] 'tabbar-backward)
;; (global-set-key [(home)]       'tabbar-forward)
;; (global-set-key [(control c)(control d)]       'rubydb)
;; (require 'tramp)

;; (setq tramp-default-method "ssh")
;; ;(load-file "~/elisp/tramps.el")

;; ;; (setq tramp-default-user "hh"
;; ;;       tramp-default-host "air")
;; ;; (tramp-set-completion-function "ssh"
;; ;;  '((tramp-parse-sconfig "/etc/ssh_config")
;; ;;   (tramp-parse-sconfig "~/.ssh/config")))
;; ;; ((tramp-parse-sconfig "/etc/ssh_config")
;; ;;  (tramp-parse-sconfig "~/.ssh/config"))
;; (require 'git-emacs)
