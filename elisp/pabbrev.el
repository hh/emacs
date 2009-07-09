;;; pabbrev.el --- Predictive abbreviation expansion

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer (XEmacs): Martin Kuehl (martin.kuehl@gmail.com)
;; Website: http://www.russet.org.uk

;;; Commentary:
;;
;; The code provides a abbreviation expansion for Emacs.  Its fairly
;; similar to "dabbrev" expansion, which works based on the contents
;; of the current buffer (or other buffers).
;;
;; Predictive abbreviation expansion works based on the previously
;; written text.  Unlike dynamic abbreviation, the text is analysed
;; during idle time, while Emacs is doing nothing else.  `pabbrev-mode'
;; tells you when this is happening.  If this irritates you unset
;; `pabbrev-idle-timer-verbose'.  The advantage of this is that its
;; very quick to look up potential abbreviations, which means that the
;; can be constantly displayed, without interfering with the user as
;; they type.  Certainly it works for me, on an old laptop, typing as
;; fast as I can (which is fast, since I learnt to type with four
;; fingers).
;;
;; pabbrev's main entry point is through the minor mode
;; `pabbrev-mode'.  There is also a global minor mode, called
;; `global-pabbrev-mode', which does the same in all appropriate
;; buffers.

;; The current user interface looks like so...
;;
;; p[oint]
;; pr[ogn]
;; pre[-command-hook]
;; pred[ictive]
;;
;; As the user types the system narrows down the possibilities.  The
;; narrowing is based on how many times the words have been used
;; previously.  By hitting [tab] at any point the user can complete the
;; word.  The [tab] key is normally bound to `indent-line'.
;; `pabbrev-mode' preserves access to this command (or whatever else
;; [tab] was bound to), if there is not current expansion.
;;
;; Sometimes you do not want to select the most commonly occurring
;; word, but a less frequently occurring word.  You can access this
;; functionality by hitting [tab] for a second time.  This takes you
;; into a special suggestions buffer, from where you can select
;; secondary selections.  See `pabbrev-select-mode' for more
;; details. There is also an option `pabbrev-minimal-expansion-p'
;; which results in the shortest substring option being offered as the
;; first replacement.
;;
;; But is this actually of any use? Well having use the system for a
;; while now, I can say that it is sometimes.  I originally thought
;; that it would be good for text, but in general its not so
;; useful.  By the time you have realised that you have an expansion
;; that you can use, hit tab, and checked that its done the right
;; thing, you could have just typed the word directly in.  It's much
;; nicer in code containing buffers, where there tend to be lots of
;; long words, which is obviously where an abbreviation expansion
;; mechanism is most useful.
;;
;; Currently pabbrev builds up a dictionary on a per major-mode basis.
;; While pabbrev builds up this dictionary automatically, you can also
;; explicitly add a buffer, or a region to the dictionary with
;; `pabbrev-scavenge-buffer', or `pabbrev-scavenge-region'.  There is
;; also a command `pabbrev-scavenge-some' which adds some words from
;; around point.  pabbrev remembers the word that it has seen already,
;; so run these commands as many times as you wish.


Customizations

Normally hitting TAB a second time after a completion will take you to a suggestions buffer where you can select from alternate completions. This bit of elisp hackery will instead use a pop-up list via auto-competion mode. Make sure this is executed after pabbrev is loaded.

(require 'auto-complete)

(defun pabbrevx-ac-on-pre-command ()
  (if (or (eq this-command 'self-insert-command)
          (and (not (ac-trigger-command-p))
               (or (not (symbolp this-command))
                   (not (string-match "^ac-" (symbol-name this-command))))))
      (progn
        (remove-hook 'post-command-hook 'pabbrevx-ac-on-post-command t)
        (remove-hook 'pre-command-hook 'pabbrevx-ac-on-pre-command t)
        (ac-abort))))

(defun pabbrevx-ac-on-post-command ()
  (if (and (not isearch-mode)
           (ac-trigger-command-p))
      (pabbrevx-ac-start)))

(defun pabbrevx-ac-start ()
  (let ((candidates (mapcar 'car pabbrev-expansion-suggestions)))
    (add-hook 'pre-command-hook 'pabbrevx-ac-on-pre-command nil t)
    (add-hook 'post-command-hook 'pabbrevx-ac-on-post-command nil t)
    (let* ((point (save-excursion (funcall ac-prefix-function)))
           (reposition (not (equal ac-point point))))
      (if (null point)
          (ac-abort)
        (setq ac-point point)
        (when (not (equal ac-point ac-old-point))
          (setq ac-old-point point))
        (setq ac-prefix (buffer-substring-no-properties point (point)))
        (setq ac-limit ac-candidate-max)
        (if (or reposition (null ac-menu))
            (save-excursion
              (funcall ac-init-function)))
        (let* ((current-width (if ac-menu (ac-menu-width ac-menu) 0))
               (width (let ((w '(0)) s)
                        (dotimes (i ac-candidate-menu-height)
                          (setq s (nth i candidates))
                          (if (stringp s) (push (string-width s) w)))
                        (apply 'max w))))
          (if (or reposition
                  (null ac-menu)
                  (> width current-width)
                  (< width (- current-width 20)))
              (ac-setup point (* (ceiling (/ width 20.0)) 20)))
          (ac-update-candidates candidates))))))

(defun pabbrevx-suggestions-goto-buffer (suggestions)
  (pabbrevx-ac-start))

(fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)

