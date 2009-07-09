;; -*-Emacs-Lisp-*-
;;
;; $Id: mp3play.el,v 1.20 2006-03-04 19:04:40 fleuret Exp $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or         ;;
;; modify it under the terms of the GNU General Public License           ;;
;; version 2 as published by the Free Software Foundation.               ;;
;;                                                                       ;;
;; This program is distributed in the hope that it will be useful, but   ;;
;; WITHOUT ANY WARRANTY; without even the implied warranty of            ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     ;;
;; General Public License for more details.                              ;;
;;                                                                       ;;
;; Written and (C) by FranÅÁois Fleuret                                   ;;
;; Contact <francois@fleuret.name> for comments & bug reports            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A simple front-end major mode for mpg321 in remote-control
;; mode. Tested with emacs versions 21.3.1 and 21.3.5 on GNU/Linux
;; Debian, with mpg321 version 0.2.10 and aumix version 2.8
;;
;; One should use mpg321 instead of mpg321, first because the former
;; is GPL while the later is not, and also because the
;; --skip-printing-frames option of mpg321 allows to control the
;; frequency at which mpg321 sends data to the front-end, to reduce
;; the CPU usage.
;;
;; Version 0.9.5 and later can use the remote gain control feature
;; provided by a small patch I wrote for mpg321-0.2.10.3 and
;; later. With it, you just set mp3play-use-remote-gain to 't' to
;; control the volume of the playing mp3 without changing the global
;; wolume with aumix.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use this mode, put the .el file where emacs can find it (which
;; means, in a directory appearing in load-path), load it, and set
;; your list of mp3 directories. For instance, to do that, just add in
;; your .emacs:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (setq mp3play-dirlist '("~/mp3")
;; ;; I want to see the remaining time
;;       mp3play-show-timing 1
;; ;; This because I use a patched mpg321 to control the gain
;;       mp3play-use-remote-gain t
;; ;; Initially, set the gain to 50%
;;       mp3play-initial-gain 50
;; ;; Where to store the info about the song playing to resume later
;;       mp3play-resume-file "~/.mp3play-resume")
;;
;; ;; So that mp3play.el is loaded only when mp3play is called
;; (autoload 'mp3play "mp3play" nil t)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; M-x mp3play builds a *mp3play* buffer. In this one, type 'h' for
;; help, <return> to play a song and 'q' to burry the buffer.
;;
;; I personally also use the following shortcuts:
;;
;; ;; M-\ go to the mp3play buffer
;; (define-key global-map [(meta \\)] 'mp3play)
;; ;; C-x C-m C-s stops the current mp3
;; (define-key global-map [(control x) (control m) (control s)]
;;   'mp3play-stop)
;; ;; C-x C-m C-p pauses it
;; (define-key global-map [(control x) (control m) (control p)]
;;   'mp3play-pause)
;; ;; C-x C-m C-i shows mp3 informations
;; (define-key global-map [(control x) (control m) (control i)]
;;  'mp3play-show-current-information)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The global idea is that every line contains an invisible part
;; between the beginning of the line and a ^_. Those parts are hidden
;; with the invisible property (cf. mp3play-put-properties).
;;
;; There are three types of lines. COMMENT ones are useless, except
;; the ones at the beginning of the buffer, used to switch to the mode
;; automatically when loading a saved buffer (it contains
;; -*-mp3play-*-). The FILE lines each contains the real name of the
;; file, and a cleaned up version shown to the user. The DIR lines are
;; similar. One could easily add fields in the hidden part, to store
;; MD5 and various statistics about the file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup mp3play ()
  "Major mode to control mpg321"
  :version "0.9.10")

(defconst mp3play-cvs-id "$Id: mp3play.el,v 1.20 2006-03-04 19:04:40 fleuret Exp $")

(provide 'mp3play)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The customizable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mp3play-dirlist nil
  "List of the directories containing the mp3s for mp3play"
  :type '(list string)
  :group 'mp3play)

;; Thanks Joe Drew for the info about "--skip-printing-frames". (Note:
;; Next time, I'll read the man page before requesting new features to
;; the author ...)

(defcustom mp3play-player '("mpg321"
			    "--skip-printing-frames=50" "-R" "dummy")
  "Program to use to play mp3s, with options"
  :type '(list string)
  :group 'mp3play)

(defcustom mp3play-playlist-file nil
  "File in which to store the filenames and information about songs"
  :type 'string
  :group 'mp3play)

(defcustom mp3play-cleanup-filename 'mp3play-native-cleanup-filename
  "The function to clean up the filenames before displaying them"
  :type 'function
  :group 'mp3play)

(defcustom mp3play-file-filter "mp3$"
  "Regexp to select which files to show"
  :type 'string
  :group 'mp3play)

(defcustom mp3play-multi-mode nil
  "*Non-nil means tunes are played in sequence automatically"
  :type 'boolean
  :group 'mp3play)

(defcustom mp3play-show-timing 0
  "0 means do not show timing, 1 is remaining time, 2 current percentage"
  :type 'integer
  :group 'mp3play)

(defcustom mp3play-use-remote-gain nil
  "*Nil means we use aumix instead of the remote gain control for mpg321
(which requires a patched mpg321)"
  :type 'boolean
  :group 'mp3play)

(defcustom mp3play-initial-gain 100
  "Initial value for either aumix or mpg321's gain"
  :type 'integer
  :group 'mp3play)

(defcustom mp3play-resume-file nil
  "Filename containing the interrupted mp3 filename and timing to resume
when restarting mp3play. nil means do not resume."
  :type 'string
  :group 'mp3play)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Face definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface mp3play-dir-face
  '((((background light)) (:foreground "royal blue" :bold t))
    (((background dark)) (:foreground "azure2" :bold t)))
  "The face for directory names.")

(defface mp3play-nonexisting-face
  '((((background light)) (:foreground "red3" :bold t))
    (((background dark)) (:foreground "red" :bold t)))
  "The face for files which do not exist anymore.")

(defface mp3play-playlist-face
  '((((background light)) (:foreground "black" :background "MediumPurple1" :bold t))
    (((background dark)) (:foreground "white" :background "gray50" :bold t)))
  "The face for playlist names.")

(defface mp3play-current-tune-face
  '((((background light)) (:underline t :bold t))
    (((background dark)) (:underline t :bold t)))
  "The face for the currently playing tune.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The other variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ;; this is the mpg321 process
      mp3play-process nil
      ;; this is the main buffer to which the player is attached (only
      ;; one, we need to know it cf. mp3play-filter-subfunctions)
      mp3play-main-buffer nil
      ;; the overlay for the highlighted line (current playing tune)
      mp3play-current-overlay nil
      ;; the marker indicating where to copy tunes when building a playlist
      mp3play-marker nil
      ;; the name of the last tune we got a message from mpg321 about
      mp3play-on-air nil
      ;; the current state 'idle 'paused 'play
      mp3play-state 'idle
      ;; the keymap
      mp3play-mode-map nil
      ;; the information for timing
      mp3play-current-timing nil
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-help ()
  (interactive)
  (with-output-to-temp-buffer "*mp3play help*"
    (set-buffer "*mp3play help*")

;;     ;; All that mess to display an ugly box!
;;     (mapc (lambda (string)
;;             (insert (propertize
;;                      (if (string= string "")
;;                          (concat "+" (make-string 73 ?- ) "+\n")
;;                        (concat "| " string (make-string (- 71 (length string)) ?\ ) " |\n"))
;;                      'face 'mp3play-dir-face)))
;;           `(""
;;             "MP3Play"
;;             ,mp3play-cvs-id
;;             "Written and (C) by FranÅÁois Fleuret, 2003, 2004"
;;             "Contact <francois.fleuret@epfl.ch> for comments"
;;             ""))

    ;; All that mess to display an ugly box!
    (insert (propertize
             (concat "
  MP3Play
  "
                     mp3play-cvs-id
"
  Written and (C) by FranÅÁois Fleuret, 2003, 2005
  Contact <francois.fleuret@epfl.ch> for comments

")
             'face 'mp3play-dir-face))

    ;; the Shows key shortcut in bold and the explanation in normal face
    (mapc (lambda (item)
            (if (stringp item) (insert item)
              (insert (propertize (car item) 'face 'bold) " " (cdr item))))

          '("  " ("<RET>" . "play")  "  " ("N" . "next tune")
            "  " ("P" . "previous tune")
            "  " ("<SPC>" . "go to current")
            "\n"
            "  " ("p" . "pause") "  " ("s" . "stop") "  " ("S" . "force stop in multi-mode")
            "\n"
            "  " ("<" . "fast backward") "  " (">" . "fast forward")
            "  " ("ctrl <" . "faster backward") "  " ("ctrl >" . "faster forward")
            "\n"
            "  " ("j" . "jump to given % (accepts universal argument)")
            "\n"
            "  " ("k" . "kill buffer") "  " ("q" . "hide")
            "\n"
            "  " ("m" . "multi-mode") "  " ("t" . "timing")
            "\n"
            "  " ("n" . "set current as next")
            "  " ("shift <up>" . "carry up")
            "  " ("shift <down>" . "carry down")
            "\n"
            "  " ("?,h" . "help") "\n"
            "  " ("i" . "informations")
            "  " ("f" . "show filename and ID3 tags")
            "\n"
            "  " ("r" . "rename tune")
            "  " ("R" . "rename tune according to the ID3 tags")
            "\n"
            "  " ("G" . "build list") "  " ("g" . "refresh list")
            "\n"
            "  " ("=" . "reset volume (accepts universal argument)") "  " ("+" . "volume up")
            "  " ("-" . "volume down")
            "\n"
            "  " ("C" . "set marker")
            "  " ("c" . "copy tune to marker") "\n"))))

(defun mp3play-yank (&optional arg)
  "Yanks and puts back the properties in a mp3play buffer"
  (interactive "p")
  (goto-char (point-at-bol))
  (save-restriction
    (let ((inhibit-read-only t)
          (start (point)))
      (yank arg)
      (narrow-to-region start (point))
      (mp3play-put-properties)))
  (goto-char (point-at-bol)))

(defun mp3play-kill-line (&optional arg)
  "Kill the lines in a mp3play buffer"
  (interactive "p")

  ;; Dirty hack, I don't get why kill-line behaves so strangely on emacs
  ;; 21.3.1 (works fine on 21.3.5 though)

  ;;   (goto-char (point-at-bol))
  ;;   (let ((inhibit-read-only t)) (kill-line arg))
  (let ((inhibit-read-only t))
    (while (> arg 0) (kill-region (point-at-bol) (1+ (point-at-eol)))
           (setq arg (- arg 1)))))

(defun mp3play-goto-next-existing-tune (n)
  (next-line n)
  (if (> n 0)
      (while (and (re-search-forward "^FILE:\\([^]+\\)" nil t)
                  (not (file-exists-p (match-string-no-properties 1)))))
    (while (and (re-search-backward "^FILE:\\([^]+\\)" nil t)
                (not (file-exists-p (match-string-no-properties 1))))))
  (goto-char (point-at-bol))
  )

(defun mp3play-goto-next-directory () (interactive)
  (next-line 1)
  (re-search-forward "^DIR:" nil t)
  (goto-char (point-at-bol))
  )

(defun mp3play-goto-current (&optional silent)
  "Move the cursor to the currently playing song"
  (interactive)
  (if (equal (overlay-start mp3play-current-overlay)
             (overlay-end mp3play-current-overlay))
      (unless silent (message "No current!"))
    (goto-char (overlay-start mp3play-current-overlay))
    (goto-char (point-at-bol))))

(defun mp3play-set-point-as-next ()
  "Moves the song from the cursor line to the line after the one currently on air"
  (interactive)
  (if (equal (overlay-start mp3play-current-overlay)
             (overlay-end mp3play-current-overlay))
      (message "No current!")
    (save-excursion
      (goto-char (point-at-bol))
      (mp3play-kill-line 1)
      (goto-char (overlay-start mp3play-current-overlay))
      (next-line 1)
      (goto-char (point-at-bol))
      (mp3play-yank)))
  )

(setq mp3play-genre-table
      [ "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk"
        "Grunge" "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies"
        "Other" "Pop" "R&B" "Rap" "Reggae" "Rock"
        "Techno" "Industrial" "Alternative" "Ska" "Death Metal" "Pranks"
        "Soundtrack" "Euro-Techno" "Ambient" "Trip-Hop" "Vocal" "Jazz+Funk"
        "Fusion" "Trance" "Classical" "Instrumental" "Acid" "House"
        "Game" "Sound Clip" "Gospel" "Noise" "Alt. Rock" "Bass"
        "Soul" "Punk" "Space" "Meditative" "Instrumental Pop"
        "Instrumental Rock" "Ethnic" "Gothic" "Darkwave" "Techno-Industrial"
        "Electronic" "Pop-Folk" "Eurodance" "Dream" "Southern Rock" "Comedy"
        "Cult" "Gangsta" "Top 40" "Christian Rap" "Pop/Funk" "Jungle"
        "Native US" "Cabaret" "New Wave" "Psychadelic" "Rave" "Showtunes"
        "Trailer" "Lo-Fi" "Tribal" "Acid Punk" "Acid Jazz" "Polka" "Retro"
        "Musical" "Rock & Roll" "Hard Rock" "Folk" "Folk-Rock" "National Folk"
        "Swing" "Fast Fusion" "Bebob" "Latin" "Revival" "Celtic" "Bluegrass"
        "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
        "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
        "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
        "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club" "Tango"
        "Samba" "Folklore" "Ballad" "Power Ballad" "Rythmic Soul" "Freestyle"
        "Duet" "Punk Rock" "Drum Solo" "Acapella" "Euro-House" "Dance Hall"
        "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie" "BritPop"
        "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap" "Heavy Metal"
        "Black Metal" "Crossover" "Contemporary Christian" "Christian Rock"
        "Merengue" "Salsa" "Trash Metal" ])

(defun mp3play-get-id3-tags (file)
  "Returns the id3 tags in a list (SONG ARTIST ALBUM YEAR NOTE GENRE),
returns nil if no id3 tags could be found."
  (let ((size (elt (file-attributes file) 7)))
    (unless (integerp size) (error "Can not read the file ID3 information (file probably too big)"))
    (with-temp-buffer
      (when (and (> size 128)
                 (insert-file-contents-literally file nil (- size 128) size t)
                 (string= (buffer-substring 1 4) "TAG"))
        ;; Here we have the 128 last bytes of the file in a temporary
        ;; buffer, and the three first characters are "TAG"
        (append
         ;; We get the 5 first id3s
         (mapcar (lambda (pos)
                   (replace-regexp-in-string
                    "[  ]*$" ""
                    (buffer-substring (car pos) (cdr pos))))
                 '((4 . 34) (34 . 64) (64 . 94) (94 . 98) (98 . 127)))
         ;; And we decode the last one with the genre table
         (list
          (condition-case nil
              (elt mp3play-genre-table (string-to-char
                                        (buffer-substring 128 129)))
            (error "<Error>"))))))))

(defun mp3play-show-id3-at-point ()
  "Shows in the minibuffer the filename with complete path and the ID3
tags of the song on the cursor line"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (re-search-forward "^FILE:\\([^]+\\)" (point-at-eol) t))
        (message "No file on this line!")
      (if (file-exists-p (match-string-no-properties 1))
          (let* ((filename (match-string-no-properties 1))
                 (id3tags (mp3play-get-id3-tags filename)))
            (if id3tags
                (message
                 "[%s] Song [%s] Artist [%s] Album [%s] Year [%s] Note [%s] Genre [%s]"
                 filename
                 (elt id3tags 0)
                 (elt id3tags 1)
                 (elt id3tags 2)
                 (elt id3tags 3)
                 (elt id3tags 4)
                 (elt id3tags 5))
              (message "[%s] (no id3 tags) " filename)))
        (message "No such file!")
        ))))

(defun mp3play-edit-id3-at-point ()
  "Open a new buffer with the ID3 fields of the file on line editable."

  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (re-search-forward "^FILE:\\([^]+\\)" (point-at-eol) t))
        (message "No file on this line!")
      (if (file-exists-p (match-string-no-properties 1))
          (let* ((filename (match-string-no-properties 1))
                 (id3tags (or (mp3play-get-id3-tags filename) '("-" "-" "-" "-" "-" "-"))))

              (let ((map (make-sparse-keymap)))

                (switch-to-buffer (get-buffer-create (generate-new-buffer-name "*mp3play ID3 editor*")))

                (text-mode)
                (auto-fill-mode)

                (insert (apply 'concat
                               (mapcar (lambda (s)
                                         (if (numberp s) (elt id3tags s)
                                           (propertize s 'read-only t)))

                                       '("SONG:   " 0 "\n"
                                         "ARTIST: " 1 "\n"
                                         "ALBUM:  " 2 "\n"
                                         "YEAR:   " 3 "\n"
                                         "NOTE:   " 4 "\n"
                                         "GENRE:  " 5 "\n"))))

                (goto-char (point-min))
                (re-search-forward "SONG:   ")

                (define-key map (kbd "TAB")
                  (lambda () (interactive)
                    (unless (re-search-forward ": +" nil t)
                      (goto-char (point-min))
                      (re-search-forward ": +" nil t))))

                (define-key map [(control c) (control c)]
                  (lambda () (interactive)
                    ;;                   (enotes-store-for-undo)
                    ;;                   (enotes-set-info enotes-edited-note
                    ;;                                    (buffer-substring-no-properties (point-min)
                    ;;                                                                    (point-max)))
                    (kill-this-buffer)
                    ;;                   (enotes-do-it)
                    )
                  )

                (define-key map [(control c) (control q)]
                  (lambda () (interactive)
                    (kill-this-buffer)
                    ;;                   (enotes-do-it)
                    (message "Cancel")
                    ))

;;                 (set (make-local-variable 'enotes-edited-note) note)
;;                 (set (make-local-variable 'fill-column) 60)

                (use-local-map map)
;;                 (when (enotes-get-info note) (insert (enotes-get-info note)))
                (message "C-c C-c to save the information, C-c C-q to cancel")
                )
              )
            )
        )
    )
  )

(defun mp3play-rename-point () "Renames the file located at point"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (re-search-forward "^FILE:\\([^]+\\)" (point-at-eol) t))
        (message "No file on this line!")
      (let* ((original (match-string-no-properties 1))
             (new (read-from-minibuffer "New name: " original)))
        (if (string= original new)
            (message "Cancel")
          (message "Renaming [%s] into [%s]" original new)
          (rename-file original new)
          (mp3play-refresh-list)
          )))))

(defun mp3play-rename-point-from-id3 () "Renames the file located at
point, according to the ID3 tags"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (re-search-forward "^FILE:\\([^]+\\)" (point-at-eol) t))
        (message "No file on this line!")
      (if (file-exists-p (match-string-no-properties 1))
          (let* ((filename (match-string-no-properties 1))
                 (id3tags (mp3play-get-id3-tags filename)))
            (if id3tags
                (let* ((original (match-string-no-properties 1))
                       (new (read-from-minibuffer "New name: "
                                                  (replace-regexp-in-string
                                                   " " "_"
                                                   (concat (replace-regexp-in-string "[^/]+$" "" (match-string-no-properties 1))
                                                           (elt id3tags 1)
                                                           "_-_"
                                                           (elt id3tags 0)
                                                           ".mp3")))))
                  (if (string= original new)
                      (message "Cancel")
                    (message "Renaming [%s] into [%s]" original new)
                    (rename-file original new)
                    (mp3play-refresh-list)
                    ))
              (message "[%s] (no id3 tags) " filename)))
        (message "No such file!")))))

(defun mp3play-move-point-to-tmp () "Move the file at point to /tmp
(this is my gentle delete)"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (re-search-forward "^FILE:\\([^]+\\)" (point-at-eol) t))
        (message "No file on this line!")
      (let* ((original (match-string-no-properties 1))
             (new (concat "/tmp/" (replace-regexp-in-string "^.*/" "" original))))
        (if (string= original new)
            (message "Cancel")
          (message "Renaming [%s] into [%s]" original new)
          (rename-file original new)
          (mp3play-refresh-list)
          )))))

(defun mp3play-copy-line-to-marker (&optional n)
  "Copies the song on the cursor line to the marker line. Sets the
marker position with `mp3play-move-marker'"
  (interactive "p")
  (if (not (marker-position mp3play-marker))
      (message "No marker!")
    (let ((inhibit-read-only t))
      (while (> n 0)
        (setq n (1- n))
        (copy-region-as-kill (point-at-bol 1) (point-at-bol 2))
        (save-excursion
          (goto-char (marker-position mp3play-marker))
          (mp3play-yank)
          (move-marker mp3play-marker (point-at-bol 2))
          )
        (mp3play-goto-next-existing-tune 1)))))

(defun mp3play-move-marker ()
  "Moves the marker used to copy songs to the current position. Move
songs with `mp3play-copy-line-to-marker'"
  (interactive)
  (move-marker mp3play-marker (point-at-bol 1))
  (message "Marker set"))

(defun mp3play-play-point ()
  "Starts mpg321 with the file on the current line and goes down"
  (interactive)

  (if (not (eq mp3play-main-buffer (current-buffer)))
      (message "This is a deaf mp3play buffer!")

    (goto-char (point-at-bol))

    (if (not (re-search-forward "^FILE:\\([^]+\\).*-- \\([^]*\\)$"
                                (point-at-eol) t))
        (message "No file on this line!")

      (if (mp3play-play (match-string-no-properties 1))
          (move-overlay mp3play-current-overlay
                        (match-beginning 2) (match-end 2)))

      (goto-char (point-at-bol))
      (mp3play-goto-next-existing-tune 1)))
  )

(defun mp3play-play-next ()
  "Plays the tune after the current one. Recenters on the cursor if the \
playing tune is not visible in the window"
  (interactive)
  (let ((p (save-excursion (mp3play-goto-current)
                           (mp3play-goto-next-existing-tune 1)
                           (prog1 (point)
                             (mp3play-play-point)))))
    (unless (pos-visible-in-window-p p) (goto-char p))))

(defun mp3play-play-prev ()
  "Plays the tune before the current one. Recenters on the cursor if the \
playing tune is not visible in the window"
  (interactive)
  (let ((p (save-excursion (mp3play-goto-current)
                           (mp3play-goto-next-existing-tune 0)
                           (prog1 (point)
                             (mp3play-play-point)))))
    (unless (pos-visible-in-window-p p) (goto-char p))))

(defun mp3play-play (filename)
  "Starts playing a tune"
  (if (not (file-exists-p filename)) (and (message "Non-existing file!") nil)
    (mp3play-send-to-mpg321 (format "LOAD %s\n" filename))
    (setq mp3play-on-air filename
          mp3play-current-timing nil)
    t))

(defun mp3play-multi ()
  "Commutes between the one-tune only and the continue mode"
  (interactive)
  (setq mp3play-multi-mode (not mp3play-multi-mode))
  (force-mode-line-update)
  )

(defun mp3play-switch-timing ()
  "Commutes the display of the timing"
  (interactive)
  (when (= (setq mp3play-show-timing (mod (1+ mp3play-show-timing) 3)) 0)
    (setq mp3play-current-timing nil))
  (force-mode-line-update)
  )

(defun mp3play-stop (&optional arg)
  (interactive "P")
  "Stops the currently playing tune. If argument is t, stops even in continue mode"
  (interactive)
  (when arg
    (setq mp3play-multi-mode nil)
    (force-mode-line-update))
  (mp3play-send-to-mpg321 "STOP\n")
  )

(defun mp3play-pause ()
  "Pauses the currently playing tune"
  (interactive)
  (mp3play-send-to-mpg321 "PAUSE\n")
  )

(defun mp3play-jump-at-percent (&optional perc)
  "Goes to a certain % of the song, either given as a universal
argument, or interactively in the minibuffer."
  (interactive "P")
  (if (not mp3play-current-timing)
      (message "Song length not known yet. Please retry.")
    (setq perc (or perc (string-to-number (read-from-minibuffer "Percentage: "))))
    (let ((a (elt mp3play-current-timing 0))
          (b (elt mp3play-current-timing 1))
          (p  (min 100 (max 0 perc))))
      (mp3play-jump-at-frame (/ (* (+ a b) p) 100)))))

(defun mp3play-jump-at-frame (frame)
  "Goes to a given frame in the current song"
  (interactive)
  (mp3play-send-to-mpg321 (format "JUMP %s\n" frame))
  )

(defun mp3play-move-item-up ()
  "Moves the current item one line up"
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-regions (point-at-bol 1) (point-at-bol 2)
                       (point-at-bol 0) (point-at-bol 1))))

(defun mp3play-move-item-down ()
  "Moves the current item one line down"
  (interactive)
  (let ((inhibit-read-only t))
    (transpose-regions (point-at-bol 1) (point-at-bol 2)
                       (point-at-bol 2) (point-at-bol 3))))

(defun mp3play-show-current-information ()
  "Displays informations (bitrate, etc.) about the tune playing"
  (interactive)
  (if (not mp3play-on-air) (message "No song playing!")
    (message "Now playing: %s - %dkb/s %gKhz %s"
             (file-name-nondirectory mp3play-on-air)
             (string-to-number (elt mp3play-current-information 10))
             (/ (string-to-number (elt mp3play-current-information 2)) 1000.0)
             (elt mp3play-current-information 3))))

(defun mp3play-volume-up ()
  "Increase the volume"
  (interactive)
  (if mp3play-use-remote-gain
      (mp3play-send-to-mpg321 (concat "GAIN +2\n"))
    (call-process "aumix" nil nil nil "-v" "+2" "-w" "+2")
    (message "Volume up"))
  )

(defun mp3play-volume-down ()
  "Decrease the volume"
  (interactive)
  (if mp3play-use-remote-gain
      (mp3play-send-to-mpg321 (concat "GAIN -2\n"))
    (call-process "aumix" nil nil nil "-v" "-2" "-w" "-2")
    (message "Volume down"))
  )

(defun mp3play-reset-volume (&optional arg)
  "Depending on the value of `mp3play-use-remote-gain', either resets
the volume with aumix or the internal gain of mpg321. If a universal
argument is provided, it is used as a reset value. If no argument is
provided, `mp3play-initial-gain' is used."
  (interactive "P")
  (let ((vol (or arg mp3play-initial-gain)))
    (if mp3play-use-remote-gain
        (mp3play-send-to-mpg321 (format "GAIN %s\n" vol))
      (call-process "aumix" nil nil nil
                    "-v" (number-to-string vol)
                    "-w" (number-to-string vol))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The displaying functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-multi-replace (s l)
  (if l
      (mp3play-multi-replace
       (replace-regexp-in-string (elt (car l) 0) (elt (car l) 1)
                                 s
                                 nil nil (elt (car l) 2))
       (cdr l))
    s))

(defun mp3play-native-cleanup-filename (str)
  "Clean up the filename by replacing the \"_\" by \" \", removing the \
extension and changing a few other things"
  (mp3play-multi-replace str '(("_" " " 0)
                               (" +" " " 0)
                               (" *.\\(mp3\\|og[gm]\\)$" "" 0)
                               ("[^ ]\\(-\\)" " -" 1)
                               ("\\(-\\)[^ ]" "- " 1)
                               )))

(defun mp3play-add-file (filename size shift already-here)

  (defun string-from-size (size)
    (if (< size 1024) (format "%5db" size)
      (if (< size 1048576) (format "%5dk" (ash size -10))
        ;;         (format "%4dM" (ash size -20))
        (format "%5.01fM" (/ size 1048576.0))
        )))

  (when (and (file-regular-p filename)
             (string-match mp3play-file-filter filename)
             (not (gethash filename already-here nil)))

    (insert (format
             "FILE:%s%s%s -- %s\n"
             filename
             (make-string (* 3 shift) ?\ )
             (string-from-size size)
             (eval `(,mp3play-cleanup-filename
                     (file-name-nondirectory filename))))))

  (when (file-directory-p filename)
    (mp3play-add-directory filename (1+ shift) already-here))

  )

(defun mp3play-add-directory (dir shift already-here)

  ;; If the directory has not already a line, we insert one. If it is
  ;; already here, we jump to the next line

  (if (not (gethash dir already-here nil))
      (insert (format "DIR:%s%s%s\n"
                      dir
                      (make-string (* 3 shift) ?\ )
                      (abbreviate-file-name dir)
                      ))
    (goto-char (point-min))
    (re-search-forward (format "^DIR:%s" dir) nil t)
    (goto-char (point-at-bol))
    (next-line 1)
    )

  (condition-case nil ;; This condition-case prevents the script from
                      ;; crashing if dirs are read-protected

      (mapc (lambda (file)
              (unless (string-match "^\\." (car file))
                (mp3play-add-file (command-line-normalize-file-name
                                   (concat dir "/" (car file)))
                                  (nth 8 file)
                                  shift
                                  already-here)))

            ;; First files, then directories. And everybody sorted by
            ;; alphabetical order

            (sort (directory-files-and-attributes dir)
                  (lambda (x y)
                    (or (and (not (cadr x)) (cadr y))
                        (string< (car x) (car y)))))
            )
    (error nil))
  )

(defun mp3play-import-list ()
  "Add all new files from mp3play-dirlist in the current buffer"

  (unless (not (and (boundp 'mp3play-dirlist) mp3play-dirlist))

    (let ((already-here (make-hash-table :test 'equal))
          (inhibit-point-motion-hooks t))

      (goto-char (point-min))

      (while (re-search-forward "^\\(FILE\\|DIR\\):\\([^]+\\)" nil t)
        (puthash (match-string 2) t already-here)
        (goto-char (match-end 0)))

      (message "Importing tune list ... ")

      (goto-char (point-max))

      (mapc (lambda (dir)
              (mp3play-add-directory (expand-file-name dir)
                                     0
                                     already-here))
            mp3play-dirlist)

      (message "done!")

      )))

(defun mp3play-refresh-list (&optional erase)
  "Adds the list of new tunes in the buffer and highlights non-existing files"
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (when erase
        ;; We remove only the FILE and DIR, to keep the playlists
        (goto-char (point-min))
        (while (re-search-forward "^\\(FILE\\|DIR\\):.*$" nil t)
          (delete-region (match-beginning 0) (1+ (match-end 0)))))
      (goto-char (point-max))
      (mp3play-import-list)
      (mp3play-put-properties)
      )))

(defun mp3play-reset-list ()
  "Rebuild the list from scratch, according to `mp3play-dirlist'"
  (interactive)
  (mp3play-refresh-list t)
  (mp3play-goto-current t))

(defun mp3play-create-playlist (title) (interactive "STitle: ")
  (goto-char (point-min))
  (let ((inhibit-read-only t))
    (insert (format "PLAYLIST:%s\n" title))
    (mp3play-put-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The functions to deal with the process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-second-to-time (total)
  (if (>= total 1)
      (let ((sec (mod total 60))
            (min (/ total 60)))
        (concat (if (>= min 1) (format "%dmin" min))
                (if (>= sec 1) (format "%ds" sec))))
    "finished"))

;; If you want to parse a new command, dont forget to add the header
;; character (P, S, I, etc.) to the parsing regular expression in
;; mp3play-filter

(setq mp3play-filter-subfunctions

      '(("P 0" . (with-current-buffer mp3play-main-buffer
                   (if mp3play-multi-mode
                       (save-excursion
                         (mp3play-goto-current)
                         (mp3play-goto-next-existing-tune 1)
                         (mp3play-play-point))
                     ;; dirty way to hide it ...
                     (move-overlay mp3play-current-overlay 0 0)
                     (setq mp3play-on-air nil
                           mp3play-current-timing nil
                           mp3play-state 'idle)
                     (force-mode-line-update)
                     )))

        ("P 1" . (when mp3play-on-air
                   (message "Song [%s] paused"
                            (file-name-nondirectory mp3play-on-air))
                   (setq mp3play-state 'paused)
                   (force-mode-line-update)))

        ("P 2" . (progn
                   (message "Song [%s] continues"
                            (file-name-nondirectory mp3play-on-air))
                   (setq mp3play-state 'playing)
                   (force-mode-line-update)))

        ("S" . (progn
                 (setq mp3play-current-information
                       (vconcat (split-string (match-string 2 str))))
                 (mp3play-show-current-information)))

        ("I" . (progn
                 (setq mp3play-currently-loaded (match-string 2 str)
                       mp3play-state 'playing)
                 (force-mode-line-update)))

        ("F" . (when (> mp3play-show-timing 0)
                 (setq mp3play-current-timing
                       (mapcar 'string-to-number (split-string
                                                  (match-string 2 str))))
                 (force-mode-line-update)))

        ("G" . (progn
                 (message "Gain set to %s%%" (match-string 2 str))
                 (setq mp3play-current-gain
                       (string-to-number (match-string 2 str)))))

        ("E" . (message "Error from %s: \"%s\""
                        (car mp3play-player) (match-string 2 str)))

        )
      )

(defun mp3play-filter (process str)
;;   (message ">>> [%s]" str)
  (let ((start 0))
    (while (and (< start (length str))
                (string-match "^@\\(P 0\\|P 1\\|P 2\\|S\\|I\\|F\\|G\\|E\\) *\\(.*\\)$"
                              str
                              start))
      (setq start (1+ (match-end 0)))
      (eval (cdr (assoc (match-string 1 str) mp3play-filter-subfunctions))))))

(defun mp3play-sentinel (process str) ()
  (unless (eq (process-status mp3play-process) 'run)

    (setq mp3play-process nil))
  (message "Mp3play process got %s" (replace-regexp-in-string "\n" "" str)))

(defun mp3play-send-to-mpg321 (string)
  (if (not mp3play-main-buffer) (error "No mp3play buffer!")

    ;; Set up the process for mpg321
    (unless mp3play-process
      (setq mp3play-process
            (apply 'start-process
                   (append '("mp3player" nil) mp3play-player)))

      (set-process-filter mp3play-process 'mp3play-filter)
      (set-process-sentinel mp3play-process 'mp3play-sentinel)
      (process-kill-without-query mp3play-process)

      (unless (boundp 'mp3play-current-gain)
        (setq mp3play-current-gain mp3play-initial-gain))

      (if mp3play-use-remote-gain
          (process-send-string mp3play-process
                               (format "GAIN %d\n" mp3play-current-gain)))

      )

;;     (message "Sending [%s] to mpg321" string)

    (process-send-string mp3play-process string)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The resuming part. When the buffer or emacs are killed, the current
;; tune (and frame, gain and multi-playing mode) is saved into a file
;; so that when mp3play starts later it can resume the song.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-save-current ()
  (when mp3play-resume-file
    ;; Note that this will erase the resume-file if nothing is playing
    (with-temp-file mp3play-resume-file
      (when mp3play-on-air
        (insert (format "CURRENT_FILE:%sFRAME:%dMULTI:%sGAIN:%dSTATE:%s\n"
                        mp3play-on-air
                        (or (elt mp3play-current-timing 0) 0)
                        (if mp3play-multi-mode "ON" "OFF")
                        mp3play-current-gain
                        (cdr (assoc mp3play-state '((playing . "playing") (paused . "paused") (idle . "idle"))))
                        )))
      )))

(defun mp3play-resume ()
  (when (and mp3play-resume-file
             (file-exists-p mp3play-resume-file))

    (with-temp-buffer
      (insert-file-contents-literally mp3play-resume-file)
      (when (re-search-forward
             "CURRENT_FILE:\\([^]+\\)FRAME:\\([0-9]+\\)MULTI:\\([^]+\\)GAIN:\\([0-9]+\\)STATE:\\([^]+\\)$"
             nil t)
        (when (message "Resuming [%s] at frame %s"
                       (file-name-nondirectory
                        (match-string-no-properties 1))
                       (match-string-no-properties 2))

          (setq mp3play-current-gain
                (string-to-number (match-string-no-properties 4)))

          (mp3play-play (match-string-no-properties 1))

          (mp3play-jump-at-frame (match-string-no-properties 2))

          (when (string= "paused" (match-string-no-properties 5))
            ;; This is ugly, but mpg321 ignores a play/jump/pause
            ;; sequence if sent in a row
            (sleep-for 0.1)
            (mp3play-pause)
            )

          (setq mp3play-multi-mode (string= "ON" (match-string-no-properties 3)))
          (force-mode-line-update))
        )
      )

    (when mp3play-on-air
      (let ((inhibit-read-only t)) (mp3play-put-properties))
      (mp3play-goto-current))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The settings of the major mode itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-kill-buffer-cleanup () (interactive)

  (mp3play-save-current)

  (if mp3play-process (delete-process mp3play-process))
  (when (eq mp3play-main-buffer (current-buffer))
    (remove-hook 'kill-emacs-hook 'mp3play-save-current)
    (setq global-mode-string (remove '(:eval (mp3play-mode-string)) global-mode-string)
          mp3play-process nil
          mp3play-main-buffer nil
          mp3play-current-overlay nil
          mp3play-on-air nil)))

;; One mp3play buffer only can have a player attached at a given
;; instant

(defun mp3play-mode ()
  "Major mode to play songs with mpg321"
  (interactive)

  (if mp3play-main-buffer
      (error "There is already a mp3play buffer"))

  (kill-all-local-variables)

  (unless mp3play-mode-map
    (setq mp3play-mode-map (make-sparse-keymap))
    (suppress-keymap mp3play-mode-map)

    (mapc (lambda (x) (define-key mp3play-mode-map (car x) (cdr x)))
          `(("p" . mp3play-pause)
            (,(kbd "RET") . mp3play-play-point)
            (,(kbd "TAB") . mp3play-goto-next-directory)
            (" " . mp3play-goto-current)
            ("n" . mp3play-set-point-as-next)
            ("f" . mp3play-show-id3-at-point)
            ("F" . mp3play-edit-id3-at-point)
            ("r" . mp3play-rename-point)
            ("R" . mp3play-rename-point-from-id3)
            ("N" . mp3play-play-next)
            ("P" . mp3play-play-prev)
            ("q" . bury-buffer)
            ("k" . kill-this-buffer)
            ("s" . mp3play-stop)
            ("S" . (lambda () (interactive) (mp3play-stop t)))
            ("m" . mp3play-multi)
            ("M" . mp3play-move-point-to-tmp)
            ("t" . mp3play-switch-timing)
            ("g" . mp3play-refresh-list)
            ("G" . mp3play-reset-list)
            ("h" . mp3play-help)
            ("?" . mp3play-help)
            ("c" . mp3play-copy-line-to-marker)
            ("C" . mp3play-move-marker)
            ("l" . mp3play-create-playlist)
            ("i" . mp3play-show-current-information)
            ([(shift up)] . mp3play-move-item-up)
            ([(shift down)] . mp3play-move-item-down)
            ("<" . (lambda () (interactive) (mp3play-jump-at-frame "-250")))
            (">" . (lambda () (interactive) (mp3play-jump-at-frame "+250")))
            ("j" . mp3play-jump-at-percent)
            ([(control >)] . (lambda () (interactive) (mp3play-jump-at-frame "+2500")))
            ([(control <)] . (lambda () (interactive) (mp3play-jump-at-frame "-2500")))
            ("=" . mp3play-reset-volume)
            ("+" . mp3play-volume-up)
            ("-" . mp3play-volume-down)
            ([(control y)] . mp3play-yank)
            ([(control k)] . mp3play-kill-line)
            )))

  (use-local-map mp3play-mode-map)

  ;; If we do not have already a mp3play buffer, set up all the
  ;; variables for the player and the interface
  (add-hook 'kill-buffer-hook 'mp3play-kill-buffer-cleanup nil t)
  (add-hook 'kill-emacs-hook 'mp3play-save-current)

  ;; dirty way to make an invisible overlay ...
  (setq mp3play-current-overlay (make-overlay 0 0))
  (overlay-put mp3play-current-overlay 'face 'mp3play-current-tune-face)
  (setq mp3play-marker (make-marker))

  (setq global-mode-string (append global-mode-string '((:eval (mp3play-mode-string))))
        mp3play-main-buffer (current-buffer)
        major-mode 'mp3play-mode)

  ;; First, we narrow so that the COMMENTs at the beginning can not
  ;; be altered

  (widen)
  (goto-char (point-min))
  (while (re-search-forward "^COMMENT.*\n" nil t) (goto-char (match-end 0)))
  (narrow-to-region (point) (point-max))

  ;; Then we hide what has to be hidden and set faces

  (mp3play-put-properties)

  ;; We set the mode name, make it read-only and deal with long lines

  (setq mode-name "Mp3play"
        buffer-read-only t
        truncate-lines t)

  ;; Try to resume the interrupted song
  (mp3play-resume)
)

;; ;; One day this code will be clean and all the styles will be specified
;; ;; by this unique function

;; (defun mp3play-set-on-air (filename)
;;   (when (and mp3play-set-on-air (re-search-forward (format "^FILE:%s.*$" mp3play-on-air)))
;;     (goto-char (match-beginning 0))
;;     (delete-region (match-beginning 0) (match-end 0))
;;     (mp3play-insert-line (shift mp3play-on-air))))

;;
;; (defun mp3play-insert-line (shift filename)
;;   (let ((explicit-name (eval `(,mp3play-cleanup-filename
;;                                (file-name-nondirectory filename)))))
;;     (insert (format "FILE:%s%s%s -- %s\n"
;;                     filename
;;                     (make-string (* 3 shift) ?\ )
;;                     (string-from-size size)
;;                     (if (file-exists-p filename)
;;                         (if (string= filename mp3play-on-air)
;;                             (propertize filename 'face 'mp3play-current-tune-face)
;;                           filename)
;;                       (propertize filename 'face 'mp3play-nonexisting-face))
;;                     explicit-name))))

;; Hides what has to be hidden, put the titles in bold, put back the
;; overlay on the current mp3's line, and put the cursor on it

(defun mp3play-put-properties ()

  (let ((m (buffer-modified-p)))

    ;; Hide the beginnings of lines (before the ^_)
    (goto-char (point-min))
    (while (re-search-forward "^\\(.*\\)[^]*$" nil t)
      (add-text-properties (match-beginning 1) (match-end 1)
                           '(invisible t))
      (add-text-properties (match-beginning 1) (1+ (match-end 1))
                           '(intangible t))
      (goto-char (match-end 0)))

    ;; Change the dir face
    (goto-char (point-min))
    (while (re-search-forward "^DIR:\\([^]*\\) *\\([^]+\\)$" nil t)
      (add-text-properties (match-beginning 2) (match-end 2)
                           '(face mp3play-dir-face))
      (goto-char (match-end 0)))

    ;; Change the playlist face
    (goto-char (point-min))
    (while (re-search-forward "^PLAYLIST:\\([^]*\\) *\\([^]+\\)$" nil t)
      (add-text-properties (match-beginning 2) (1+ (match-end 2))
                           '(face mp3play-playlist-face))
      (goto-char (match-end 0)))

    ;; Highlight the non-existing files
    (goto-char (point-min))
    (while (re-search-forward "^FILE:\\([^]+\\).*-- \\([^]+\\)$" nil t)
      (if (file-exists-p (match-string 1))
          (set-text-properties (match-beginning 2) (match-end 2) nil)
        (add-text-properties (match-beginning 2) (match-end 2)
                             '(face mp3play-nonexisting-face)))
      (goto-char (match-end 0)))

    (when (eq mp3play-main-buffer (current-buffer))
      ;; If this is the main buffer and not a deaf one, look for the
      ;; current file, put the overlay there
      (goto-char (point-min))
      (when (and mp3play-on-air
                 (or ;; First we look for the exact match (path + file)
                  (re-search-forward
                   (format "^FILE:%s.*-- \\([^]+\\)$" mp3play-on-air)
                   nil t)
                  ;; If this fail, look for a match of the filename alone (whatever the path)
                  (re-search-forward
                   (format "^FILE:.*/%s.*-- \\([^]+\\)$" (replace-regexp-in-string ".*/" ""  mp3play-on-air))
                   nil t))
                 )
        (move-overlay mp3play-current-overlay
                      (match-beginning 1) (match-end 1))))

    (set-buffer-modified-p m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The modeline string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play-timing-string ()
  (eval (nth mp3play-show-timing
             '(
               nil
               (if mp3play-current-timing (mp3play-second-to-time (elt mp3play-current-timing 3))
                 "---")
               (if mp3play-current-timing
                   (let ((a (elt mp3play-current-timing 0))
                         (b (elt mp3play-current-timing 1)))
                     ;; To display a '%', format goes from %%%% to %%
                     ;; and the modeline displaying goes from %% to %.
                     (format "%g%%%%" (/ (* 100 a ) (+ a b))))
                 "--%%")
               ))
        )
  )


(defun mp3play-mode-string ()
  (concat
   " <"
   (cdr (assoc mp3play-state '((playing . "Play")
                               (paused . "Pause")
                               (idle . "Idle"))))
   (if mp3play-multi-mode "-cont")
   (if (and mp3play-on-air (> mp3play-show-timing 0))
       (concat " " (mp3play-timing-string)))
   ">"
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mp3play ()

  "Switches to the mp3play buffer, creates it if necessary. This
buffer contains the list of the mp3s from the directories listed in
`mp3play-dirlist'. You can play a given mp3 by typing <return> on the
mp3 in this buffer. Use 'q' to burry the buffer, 'k' to kill it and
'?' for the help. The command-line software used to play the songs is
specified in `mp3play-player'"

  (interactive)

  ;; If there already is a mp3play buffer switch to it, if not, set
  ;; one up from scratch

  (if mp3play-main-buffer (switch-to-buffer mp3play-main-buffer)

    ;; Set up the buffer, and switch to it
    (if (and mp3play-playlist-file (file-exists-p mp3play-playlist-file))
        (find-file mp3play-playlist-file)
      (switch-to-buffer (get-buffer-create "*mp3play*"))
      (insert "COMMENT: -*-mp3play-*- This was saved by mp3play.el\n")
      (mp3play-import-list)
      (mp3play-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
