;; Start Mutt in Emacs

;; Call with: emacs -l emacs-mutt.el <arguments for Mutt>

(when (not (fboundp 'gnuserv-start))
  (locate-library "gnuserv-compat")
  (load-library "gnuserv-compat"))
(gnuserv-start)

(defadvice term-handle-exit (after close-emacs)
  (smart-close))
(ad-activate 'term-handle-exit)

(set-buffer (apply 'make-term "Mutt" "mutt" nil command-line-args-left))
(setq command-line-args-left nil)
(term-mode)
(term-char-mode)
(switch-to-buffer "*Mutt*")

;; See thread:
;;   From: Vincent Lefevre <vincent+news@vinc17.org>
;;   Newsgroups: fr.comp.applications.emacs
;;   Subject: lancer Emacs avec un Mutt dedans en mode terminal
;;   Date: Thu, 9 Feb 2006 15:54:21 +0000 (UTC)
;;   Message-ID: <20060209154329$7828@prunille.vinc17.org>

;; $Id: emacs-mutt.el 14550 2006-10-23 11:48:47Z lefevre $
