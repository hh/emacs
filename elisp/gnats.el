;;; gnats.el --- Emacs interface for GNATS.

;; Copyright (C) 2000, 2001, 2002 Milan Zamazal
;; Copyright (C) 2000 Bob Manson
;;
;; Contributed by Bob Manson (manson@juniper.net)
;;  entirely written from scratch.
;; Further improved by Milan Zamazal <pdm@zamazal.org>.
;;
;; This file is part of GNU GNATS.
;;
;; GNU GNATS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU GNATS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU GNATS; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111, USA.

;;; Commentary:
;; 
;; This file is still woefully incomplete; it seems to work for me,
;; however.

;;; Code:


(defgroup gnats nil
  "The GNU GNATS bug tracking system frontend.")

(defcustom gnats-server nil
  "*Server name or address to connect to.
If it is nil, the value from the GNATSDB environment variable is used.
If the environment variable is unset, \"localhost\" is used."
  :group 'gnats
  :type '(choice (const nil) string))

(defcustom gnats-port nil
  "*Server port to connect to as an integer or nil.
If it is nil, the value from the GNATSDB environment variable is used.
If the environment variable is unset, 1529 is used."
  :group 'gnats
  :type '(choice (const nil) integer))

(defcustom gnats-database nil
  "*Name of the database to work with.
If it is nil, the value from the GNATSDB environment variable is used.
If the environment variable is unset, \"default\" is used."
  :group 'gnats
  :type '(choice (const nil) string))

(defcustom gnats-user nil
  "*User name to use when connecting to the server.
If it is nil, the value from the GNATSDB environment variable is used.
If the environment variable is unset, (user-login-name) is used."
  :group 'gnats
  :type '(choice (const nil) string))

(defcustom gnats-password nil
  "*Password to use when connecting to the server.
If it is nil, the value from the GNATSDB environment variable is used.
If the environment variable is unset, \"*\" is used."
  :group 'gnats
  :type '(choice (const nil) string))

(defcustom gnats-default-organization ""
  "*Default value of the `Organization' field when sending new PRs."
  :group 'gnats
  :type 'string)

(defcustom gnats-default-submitter ""
  "*Default value of the `Submitter-Id' field when sending new PRs."
  :group 'gnats
  :type 'string)

(defcustom gnats-query-reverse-listing nil
  "*If non-nil, list query results from the highest PR number to the lowest."
  :group 'gnats
  :type 'boolean)



(defvar gnats-server-conn nil
  "The per-buffer connection descriptor for gnats edit mode.")
(make-variable-buffer-local 'gnats-server-conn)
(put 'gnats-server-conn 'permanent-local t)

(defvar gnats-edit-mode-map '()
  "Keymap for GNATS edit mode.
Most keys are rebound; the buffer is read-only so that only gnats-specific;
functions can modify it.")
(unless gnats-edit-mode-map
  (setq gnats-edit-mode-map (make-keymap))
  (substitute-key-definition 'newline 'gnats-end-line gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'newline-and-indent
			     'gnats-end-line gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'delete-backward-char
			     'gnats-delete-backward-char gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'delete-char
			     'gnats-delete-char gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'open-line
			     'gnats-open-line gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'kill-line
			     'gnats-kill-line gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'advertised-undo
			     'gnats-advertised-undo gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'indent-for-tab-command
			     'gnats-insert-char gnats-edit-mode-map
			     global-map)
  (substitute-key-definition 'self-insert-command
			     'gnats-insert-char gnats-edit-mode-map
			     global-map)
  (define-key gnats-edit-mode-map "\C-c\C-c" 'gnats-apply-or-submit)
  (define-key gnats-edit-mode-map "\t" 'gnats-next-field)
  (define-key gnats-edit-mode-map "\e\t" 'gnats-previous-field))

(easy-menu-define
 gnats-edit-mode-menu gnats-edit-mode-map
 "Menu for editing GNATS problem reports."
 '("Gnats"
   ["Submit changes" gnats-apply-or-submit
    :active (let ((field-list (get gnats-server-conn 'field-list))
		  (foundp nil))
	      (while (and (not foundp) field-list)
		(when (gnats-field-edited-p (car field-list))
		  (setq foundp t))
		(setq field-list (cdr field-list)))
	      foundp)
    :help "Submit changes to edited fields"]
   ["Next field" gnats-next-field
    :help "Go to the next field"]
   ["Previous field" gnats-previous-field
    :help "Go to the previous field"]))

(defvar gnats-edit-button-map '()
  "Keymap for enumerated field editting buttons.")
(unless gnats-edit-button-map
  (setq gnats-edit-button-map (copy-keymap gnats-edit-mode-map))
  (define-key gnats-edit-button-map [down-mouse-2] 'gnats-mouse-request-enum)
  (define-key gnats-edit-button-map [mouse-2] (lambda () (interactive))))

(defun gnats-delete-process ()
  (let ((process (get gnats-server-conn 'process))
	(buffer (get gnats-server-conn 'server-buffer)))
    (when process
      (delete-process process))
    (when buffer
      (kill-buffer buffer))))
(add-hook 'kill-buffer-hook 'gnats-delete-process)

(defun gnats-gen-completion-list (field)
  "Generate a completion collection from the legal values for FIELD."
  (mapcar '(lambda (x) (list x x)) (get field 'valid-values)))

(defun gnats-get-field-value (field)
  "Return the current value of FIELD from the buffer."
  (let* ((curr-point (gnats-find-field-text field))
	 (field-type (get field 'type))
	 (start (gnats-find-field-start field curr-point))
	 (end (next-single-property-change curr-point 'gnats-field-name)))
    ;; Don't want the trailing newline.
    (setq end (if end
		  (- end 1)
		(point-max)))
    ;; Don't include the leading newline on a multitext field.
    (if (eq field-type 'multitext)
	(setq start (+ start 1)))
    (buffer-substring-no-properties start end)))

(defun gnats-advertised-undo (&optional count)
  "Undo, but disabling motion-hooks and read-only attributes.
Do the undo COUNT times."
  (interactive "P")
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (advertised-undo count)))

(defun gnats-mark-field-edited (field)
  "Mark FIELD as having been edited.
Insert a Reason-Changed-Why field as needed."
  (put field 'was-edited t)
  (if (and (not (get field 'has-changed-why))
	   (get field 'requires-changed-why))
      (let ((field-type (get field 'type))
	    (end-of-curr-field))
	(if (eq (get-text-property (point) 'gnats-field-name) field)
	    (setq end-of-curr-field
		  (next-single-property-change (point) 'gnats-field-name))
	  (setq end-of-curr-field (point)))
	(gnats-run-modify-command
	 (let ((field-changed (gnats-gen-changed-why field)))
	   (goto-char end-of-curr-field)
	   (gnats-insert-label field-changed nil nil)
	   (gnats-insert-text field-changed "" nil)
	   (put field 'has-changed-why t)
	   (undo-boundary)
	   ;; Wanna end up at the start of the to-be-entered text for
	   ;; the foo-Changed-Why field.
	   (goto-char (- (point) 1)))))))

(defun gnats-find-field-start (field curr-point)
  "Find the start of the field contents for FIELD.
CURR-POINT must be a point within the field contents."
  (or (if (eq (get-text-property (- curr-point 1) 'gnats-field-name) field)
	  (previous-single-property-change curr-point 'gnats-field-name))
      curr-point))

(defun gnats-replace-curr-text (field new-text)
  "Replace FIELD's text in the edit buffer with NEW-TEXT."
  (let* ((curr-point (point))
	 (start (gnats-find-field-start field curr-point))
	 (end (next-single-property-change curr-point 'gnats-field-name))
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t))
    (delete-region start end)
    ;; Make the undo atomic.
    (if (and (listp buffer-undo-list) (null (car buffer-undo-list)))
	(setq buffer-undo-list (cdr buffer-undo-list)))
    (goto-char start)
    (gnats-insert-text field new-text t
		       (list 'read-only t 'mouse-face 'highlight
			     'local-map gnats-edit-button-map))))

(defun gnats-delete-field (field)
  "Remove FIELD entirely from the PR.
Point is assumed to be somewhere before the start of the field."
  (let ((field-data-start (gnats-find-field-text field)))
    ;; Did we find the field contents?
    (if field-data-start
	(let* ((start (or (previous-single-property-change
			   (- field-data-start 1)
			   'gnats-field-name)
			  (point-min)))
	       (end (or (next-single-property-change field-data-start
						     'gnats-field-name)
			(point-max))))
	  (gnats-run-modify-command (delete-region start end))
	  (gnats-clear-field-contents field)))))

(defun gnats-reset-value (field)
  "Reset FIELD to its original value.
Also deletes an associated -Changed-Why field, if one was added."
  (when (get field 'has-changed-why)
    (gnats-delete-field (gnats-gen-changed-why field))
    (put field 'has-changed-why nil))
  (gnats-replace-curr-text field (get field 'value))
  (put field 'was-edited nil))

(defun gnats-cycle-enum-value (field)
  "Replace the current enum FIELD's value in the buffer with the next value."
  (let* ((old-value (gnats-get-field-value field))
	 (valid-values (get field 'valid-values))
	 (values valid-values)
	 v
	 new-value)
    (while values
      (when (and (string= (car values) old-value)
		 (cdr values))
	(setq new-value (cadr values))
	(setq values nil))
      (setq values (cdr values)))
    (unless new-value
      (setq new-value (car valid-values)))
    (unless (string= new-value old-value)
      (undo-boundary)
      (if (and (get field 'has-changed-why)
	       (string= (get field 'value) old-value))
	  (put field 'has-changed-why nil))
      (save-excursion
	(if (string= (get field 'value) new-value)
	    (gnats-reset-value field)
	  (gnats-replace-curr-text field new-value)
	  (gnats-mark-field-edited field))))))

(defun gnats-mouse-request-enum (e)
  "Ask the user for an enumerated value for FIELD and change the field value.
Use an X menu for the value selection."
  (interactive "e")
  (mouse-set-point e)
  (sit-for 0)
  (gnats-request-enum (get-text-property (point) 'gnats-field-name) e))

(defun gnats-request-enum (field &optional menup no-default)
  "Ask the user for an enumerated value for FIELD and change the field value.
If MENUP is non-nil, use an X menu for the selection.
If NO-DEFAULT is non-nil, do not insert a suggested default value into
minibuffer."
  (let* ((completions (gnats-gen-completion-list field))
	 (old-value (gnats-get-field-value field))
	 (new-value
	  (if menup
	      (x-popup-menu
	       t
	       (list (concat "New value for " (get field 'field-name) ":")
		     (cons "" (mapcar (lambda (x) (cons (car x) (cadr x)))
				      completions))))
	    (completing-read
	     (concat "New value for " (get field 'field-name) ": ")
	     completions nil t
	     (cond
	      (no-default
	       "")
	      ((integerp last-nonmenu-event)
	       (let ((c completions)
		     (default nil))
		 (while (and (not default) c)
		   (if (eq (string-to-char (caar c)) last-nonmenu-event)
		       (setq default (caar c))
		     (setq c (cdr c))))
		 (or default old-value)))
	      (t old-value))))))
    (when (and new-value (not (string= new-value old-value)))
      (undo-boundary)
      (if (and (get field 'has-changed-why)
	       (string= (get field 'value) old-value))
	  (put field 'has-changed-why nil))
      (if (string= (get field 'value) new-value)
	  (gnats-reset-value field)
	(gnats-replace-curr-text field new-value)
	(or (gnats-mark-field-edited field)
	    (goto-char (gnats-find-next-field)))))))

(defun gnats-find-next-field ()
  "Return the position of the next field, or (point-max)."
  (let ((pos (text-property-any (point) (point-max) 'gnats-field-name nil)))
    (if pos
	(setq pos (next-single-property-change pos 'gnats-field-name)))
    (or (gnats-find-field-adjust pos) (point-max))))

(defun gnats-find-previous-field ()
  "Return the position of the previous field, or (point-min)."
  (let ((pos (let ((field-name (get-text-property (point) 'gnats-field-name)))
	       (if field-name
		   (previous-single-property-change
		    (gnats-find-field-start field-name (point))
		    'gnats-field-name)
		 (point)))))
    (if pos
	(setq pos (previous-single-property-change pos 'gnats-field-name)))
    (or (gnats-find-field-adjust pos) (point-min))))

(defun gnats-find-field-adjust (pos)
  "Return the first editing position of the field at POS.
If POS is nil, return nil."
  (when pos
    (save-excursion
      (goto-char pos)
      (let ((field-name (get-text-property (point) 'gnats-field-name)))
	  (and field-name
	       (eq (get field-name 'type) 'multitext)
	       (eolp)
	       (beginning-of-line 2)))
      (point))))

(defmacro gnats-run-modify-command (&rest body)
  "Execute BODY after inhibiting read-only attributes.
The read-only attributes are restored to their previous state afterwards."
  `(let ((inhibit-read-only t))
     ,@body))

(defun gnats-delete-char (count)
  "Delete COUNT chars under the cursor from the current buffer."
  (interactive "p")
  (let* ((char-attrs (text-properties-at (point)))
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get field-name 'type))))
    (cond
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (gnats-request-enum field-name))
     ((and (eolp)
	   (not (eq field-type 'multitext)))
      (error "Read-only text may not be edited"))
     ((and (eolp)
	   (not (and
		 (eq count 1)
		 (bolp)
		 (not (and
		       (get-text-property (+ (point) 1) 'read-only)
		       (get-text-property (- (point) 1) 'read-only)))))
	   (or
	    (get-text-property (+ (point) 1) 'read-only)
	    (get-text-property (- (point) 1) 'read-only)))
      (error "Read-only text may not be edited"))
     ((or
       (null field-name)
       (text-property-any (point) (+ (point) count) 'read-only t))
      (error "Read-only text may not be edited"))
     (t (gnats-run-modify-command (delete-char count))
	(gnats-mark-field-edited field-name)))))

(defun gnats-delete-backward-char (count)
  "Delete COUNT chars under the cursor from the edit buffer."
  (interactive "p")
  (let* ((edit-point (if (< 1 (point)) (- (point) 1) 1))
	 (char-attrs (text-properties-at edit-point))
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get field-name 'type))))
    (cond
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (gnats-request-enum field-name))
     ((and (bolp)
	   (not (eq field-type 'multitext)))
      (error "Read-only text may not be edited"))
     ((and (bolp)
	   (or
	    (get-text-property (- edit-point 1) 'read-only)
	    (get-text-property (+ edit-point 1) 'read-only)))
      (error "Read-only text may not be edited"))
     ((or
       (null field-name)
       (text-property-any (- (point) count) edit-point 'read-only t))
      (error "Read-only text may not be edited"))
     (t (gnats-run-modify-command (delete-backward-char count))
	(gnats-mark-field-edited field-name)))))

(defun gnats-find-field-text (field)
  "Return the location of the field text in the buffer for FIELD.
If the text is not found, nil is returned."
  ;; We assume that (point) is within the field contents, tho this
  ;; will work even if it isn't.
  (or (text-property-any (point) (point-max) 'gnats-field-name field)
      (text-property-any (point-min) (point) 'gnats-field-name field)))

(defun gnats-server-response-ok (resp)
  "Examine the server response line RESP.
Return either the error message from the server, or nil if the response
indicates that the operation was successful."
  (if (and (>= (car (car resp)) 200)
	   (< (car (car resp)) 300))
      nil
    (concat (nth 1 (car resp)) "\n")))

(defun gnats-field-edited-p (field)
  "Return T if the FIELD has been edited."
  (and (get field 'was-edited)
       (not (string= (gnats-get-field-value field)
		     (get field 'value)))))

(defun gnats-validate-field (field)
  "Verify that the contents of FIELD are valid.
Return either nil, indicating that the contents are valid, or the server error
message."
  (if (gnats-field-edited-p field)
      (let* ((field-text (gnats-get-field-value field))
	     (server-resp
	      (gnats-send-command-and-text (concat "VFLD "
						   (get field 'field-name))
					   field-text)))
	(gnats-server-response-ok server-resp))))

(defun gnats-apply-edit (field)
  "Send the contents of FIELD to the server as an edit.
The result is either nil (indicating that the edit was successful) or a string
containing the error message from the server."
  (let* ((curr-value (gnats-get-field-value field))
	 (changed-why-field (gnats-gen-changed-why field))
	 (change-reason (and (get field 'has-changed-why)
			     (not (string= (get field 'value) curr-value))
			     (gnats-get-field-value changed-why-field)))
	 (server-resp
	  (gnats-server-response-ok
	   (gnats-send-command-and-text
	    (concat "REPL "
		    (get gnats-server-conn 'pr-number)
		    " "
		    (get field 'field-name))
	    curr-value change-reason))))
    server-resp))

(defun gnats-apply-edits ()
  "Verify the current set of edits with the server.
If all edits are acceptable, the outstanding edits are applied and any
resulting error messages are displayed.  If there are no errors after the edits
are applied, the PR contents are reloaded."
  (let ((field-list (get gnats-server-conn 'field-list))
	(validate-res (mapconcat 'gnats-validate-field
				 (get gnats-server-conn 'field-list)
				 "")))
    (if (not (string= validate-res ""))
	(error validate-res))
    (while field-list
      (let ((field (car field-list)))
	(if (gnats-field-edited-p field)
	    (let ((field-res (gnats-apply-edit field)))
	      (if field-res
		  (error field-res)
		(put field 'was-edited nil)
		(put field 'value (gnats-get-field-value field)))))
	(setq field-list (cdr field-list))))
    (gnats-clear-edit-buffer)
    (gnats-get-pr (get gnats-server-conn 'pr-number))
    (gnats-edit-mode t)))

(defun gnats-apply-or-submit ()
  "If in edit mode apply the current set of changes, or submit the new PR."
  (interactive)
  (save-excursion
    (if (eq (get gnats-server-conn 'mode) 'submit)
	(gnats-submit-pr)
      (gnats-apply-edits))))

(defun gnats-get-pr-contents ()
  "Collect the current PR contents as a standard-formatted PR."
  (save-excursion
    (goto-char (point-min))
    
    (let ((field-list (get gnats-server-conn 'input-fields))
	  (res ""))
      (while field-list
	(let* ((field (car field-list))
	       (field-value (gnats-get-field-value field))
	       (field-type (get field 'type))
	       (field-name (concat ">" (get field 'field-name) ":")))
	  (setq res
		(concat res
			(if (eq 'multitext field-type)
			    (concat field-name "\n" field-value)
			  (concat field-name " " field-value))
			"\n")))
	(setq field-list (cdr field-list)))
      res)))

(defun gnats-submit-pr ()
  "Submit the current PR; if successful, the buffer is killed."
  (let* ((pr-contents (gnats-get-pr-contents))
	 (server-resp
	  (gnats-server-response-ok
	   (gnats-send-command-and-text "SUBM" pr-contents))))
    (if server-resp
	(error server-resp)
      ;(kill-buffer (get gnats-server-conn 'server-buffer))
      (message "PR successfully submitted."))))

(defun gnats-insert-char (count)
  "Insert COUNT chars of the `last-command-char' into the current buffer."
  (interactive "p")
  (let* ((char-attrs (text-properties-at (point)))
	 (ch last-command-char)
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get field-name 'type))))
    (cond
     ((or (eq ch ?\r)
	  (eq ch ?\n))
      (gnats-end-line))
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (if (eq ch ?\ )
	  (gnats-cycle-enum-value field-name)
	(gnats-request-enum field-name)))
     ((and (eq 'multitext field-type)
	   (eolp)
	   (get-text-property (- (point) 1) 'read-only))
      (error "Read-only text may not be edited"))
     ((or (plist-get char-attrs 'read-only)
	  (null field-name))
      (error "Read-only text may not be edited"))
     (t
      (let ((curr-point (point)))
	(gnats-run-modify-command (self-insert-command count))
	(add-text-properties curr-point (point)
			     (list 'gnats-field-name field-name))
	(gnats-mark-field-edited field-name))))))

(defun gnats-end-line ()
  "Handle an EOL character from the user."
  (interactive)
  (let* ((char-attrs (text-properties-at (point)))
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get field-name 'type))))
    (cond
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (gnats-request-enum field-name nil t))
     ((or
       (plist-get char-attrs 'read-only)
       (null field-name))
      (error "Read-only text may not be edited"))
     ((gnats-multitext-field-p field-name)
      (gnats-run-modify-command (newline))
      (gnats-mark-field-edited field-name))
     (t (error "Newlines not permitted in this field")))))

(defun gnats-open-line (count)
  "Open a line.
Argument COUNT specifies how many lines are opened."
  (interactive "p")
  (let* ((char-attrs (text-properties-at (point)))
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get field-name 'type))))
    (cond
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (gnats-request-enum field-name))
     ((or
       (plist-get char-attrs 'read-only)
       (null field-name))
      (error "Read-only text may not be edited"))
     ((gnats-multitext-field-p field-name)
      (gnats-run-modify-command (open-line count))
      (gnats-mark-field-edited field-name))
     (t (error "This field may only contain one line of text")))))

(defun gnats-kill-line (&optional count)
  "Delete to the end of the line.
If the optional argument COUNT is given, delete that many lines."
  (interactive "P")
  (let* ((edit-point (point))
	 (end-point
	  (save-excursion
	    (if count
		(forward-line count)
	      (end-of-line))
	    (point)))
	 (start-point (min edit-point end-point))
	 (char-attrs (text-properties-at edit-point))
	 (field-name (plist-get char-attrs 'gnats-field-name))
	 (field-type (and field-name (get
				      field-name 'type))))
    (setq end-point (1+ (max end-point edit-point)))
    (if (and (null count) (eolp))
	(setq end-point (1+ end-point)))
    (cond
     ((null field-name)
      (error "Read-only text may not be edited"))
     ((or (eq 'enum field-type)
	  (eq 'enum-in-file field-type))
      (gnats-request-enum field-name))
     ((and (text-property-any start-point end-point 'read-only t)
	   (or (not (bolp))
	       (not (eq count nil))
	       (get-text-property (- start-point 2) 'read-only)))
      (error "Read-only text may not be edited"))
     (t (gnats-run-modify-command (kill-line count))
	(gnats-mark-field-edited field-name)))))

(defun gnats-next-field ()
  "Move to the next input field."
  (interactive)
  (goto-char (gnats-find-next-field))
  (when (eobp)
    (goto-char (point-min))
    (goto-char (gnats-find-next-field))))

(defun gnats-previous-field ()
  "Move to the previous input field."
  (interactive)
  (goto-char (gnats-find-previous-field))
  (when (bobp)
    (goto-char (point-max))
    (goto-char (gnats-find-previous-field))))

(defvar gnats-edit-font-lock-keywords
  '(("^>\\S-+:" . font-lock-keyword-face)
    ("^>Synopsis:\\s-*\\(.*\\)$" 1 font-lock-variable-name-face)))

(defun gnats-update-font-lock-keywords ()
  "Update the value of `gnats-edit-font-lock-keywords-1'.
The value is set to fontify the values of the enum fields."
  (let ((fields (get gnats-server-conn 'field-list))
	(highlight-fields nil))
    (while fields
      (when (eq (get (car fields) 'type) 'enum)
	(setq highlight-fields (cons (regexp-quote
				      (get (car fields) 'field-name))
				     highlight-fields)))
      (setq fields (cdr fields)))
    (when highlight-fields
      (setq gnats-edit-font-lock-keywords
	    (cons (let ((regexp (regexp-opt highlight-fields)))
		    (list (concat "^>\\(" regexp "\\):\\s-*\\(.*\\)$")
			  (+ (regexp-opt-depth regexp) 2)
			  'font-lock-builtin-face))
		  gnats-edit-font-lock-keywords)))))

(defvar gnats-edit-mode-hook nil
  "Hook run by `gnats-edit-mode'.")

(defun gnats-edit-mode (&optional quietp)
  "Major mode for editing PRs.
Press \\[gnats-apply-or-submit] to submit your changes."
  ;; Should we be doing this here?  By the time the mode is specified
  ;; we've already diddled with several local variables.
  (kill-all-local-variables)
  (setq major-mode 'gnats-edit-mode)
  (setq mode-name "gnats-edit")
  (use-local-map gnats-edit-mode-map)
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-separate ">\\|[ \t\f]*$"
	paragraph-start paragraph-separate)
  (make-local-variable 'gnats-edit-font-lock-keywords)
  (gnats-update-font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gnats-edit-font-lock-keywords t t))
  (make-local-variable 'after-change-functions)
  (setq after-change-functions
	(cons 'gnats-extend-properties after-change-functions))
  (unless quietp
    (message "Press `C-c C-c' to submit your changes."))
  (run-hooks 'gnats-edit-mode-hook))
(put 'gnats-edit-mode 'mode-class 'special)

(defun gnats-edit-server (&optional pr)
  "Connect to the GNATS server and return its connection as a process.
Optional argument PR specifies the problem report from which the server
information should be retrieved."
  (setq gnats-server-conn (intern (buffer-name)))
  (gnats-init-response)
  (put gnats-server-conn 'server-buffer (generate-new-buffer " *gnats-net*"))
  (gnats-get-addr-info pr)
  (let ((first-char (string-to-char (get gnats-server-conn 'server-host)))
	(success nil))
    (unwind-protect
	(progn
	  (if (or (eq first-char ?/)
		  (eq first-char ?.))
	      (progn
		(put gnats-server-conn 'process
		     (start-process "gnatsd"
				    (get gnats-server-conn 'server-buffer)
				    (get gnats-server-conn 'server-host)
				    "--database" "default"
				    "--not-inetd"))
		(put gnats-server-conn 'eol-chars "\n"))
	    (put gnats-server-conn 'process
		 (open-network-stream "gnats-edit-server"
				      (get gnats-server-conn 'server-buffer)
				      (get gnats-server-conn 'server-host)
				      (get gnats-server-conn 'server-port)))
	    (put gnats-server-conn 'eol-chars "\r\n"))
	  (setq success t))
      (if success
	  (progn
	    (set-process-filter (get gnats-server-conn 'process)
				'gnats-process-filter)
	    (gnats-get-response))
	(kill-buffer (get gnats-server-conn 'server-buffer))))))

(defun gnats-debug-string (string)
  "Display STRING in the associated server buffer."
  (let ((old-buffer (current-buffer))
	(process-buffer (process-buffer (get gnats-server-conn 'process))))
    (unwind-protect
	(when process-buffer
	  (set-buffer process-buffer)
	  (goto-char (point-max))
	  (insert string))
      (set-buffer old-buffer))))

(defun gnats-insert-label (field single-line first-char-attr)
  "Add a label for FIELD at point.
If SINGLE-LINE is true, the field is assumed to only allow one line of text.
The first character of the label text will have FIRST-CHAR-ATTR set."
  (let* ((curr-pos (point))
	 (field-name (get field 'field-name))
	 (field-spacing
	  (1+ (- (get gnats-server-conn 'max-field-name-width)
		 (length field-name)))))
    (insert ">" field-name ":")
    (if single-line
	(insert (make-string field-spacing ?\ ))
      (insert "\n"))
    (if first-char-attr
	(add-text-properties curr-pos (1+ curr-pos)
			     (append first-char-attr '(intangible t))))
    (if single-line
	(set-text-properties (- (point) 1) (point)
			     '(intangible t read-only t rear-nonsticky t))
      (add-text-properties (- (point) 2) (- (point) 1) '(rear-nonsticky t)))
    (add-text-properties curr-pos (- (point) 1)
			 '(intangible t read-only t))))

(defun gnats-insert-text (field text single-line &optional props)
  "Insert FIELD TEXT into the edit buffer.
If SINGLE-LINE is set, then the field is assumed to only hold one line of text.
The optional property list PROPS will be set on the inserted text, if it is
supplied."
  (let ((curr-pos (point)))
    (insert text "\n")
    (if (not single-line)
	(setq curr-pos (- curr-pos 1)))
    (add-text-properties curr-pos (1+ curr-pos) '(front-sticky t))
    (add-text-properties (- (point) 1)  (point) '(rear-nonsticky t))
    (add-text-properties curr-pos (point)
			 (append (list 'gnats-field-name field) props))))

(defun gnats-extend-properties (beg end len)
  "Set GNATS mode properties of the insert text between BEG and END.
Moreover, mark the current field as edited.
Intended to be used in the `insert-behind-hooks' property."
  (let ((field-name (get-text-property end 'gnats-field-name)))
    (when field-name
      (set-text-properties beg end
			   `(gnats-field-name ,field-name front-sticky t))
      (gnats-mark-field-edited field-name))))

(defun gnats-format-field (prev-field field &optional input)
  ;; checkdoc-order: nil
  "If FIELD is a valid edit field, format it into the edit buffer.
PREV-FIELD is the previously-inserted field, or nil if this is the first field.
FIELD is returned if it was formatted, PREV-FIELD otherwise.
If the optional argument INPUT is nil and the field is read only, don't format
the field."
  (let* ((field-type (get field 'type))
	 (field-flags (downcase (get field 'flags)))
	 (field-value (or (get field 'value) ""))
	 (curr-pos (point))
	 (prev-field-type
	  (and prev-field
	       (get prev-field 'type)))
	 (first-label-attr nil))
    ;; skip readonly fields
    (if (or
	 input
	 (not (string-match "readonly" field-flags)))
	(progn
	  (if (or (null prev-field-type)
		  (not (eq prev-field-type 'multitext)))
	      (setq first-label-attr '(front-sticky t)))
	  (cond
	   ((eq field-type 'multitext)
	    (gnats-insert-label field nil first-label-attr)
	    (gnats-insert-text field field-value nil))

	   ;; enum fields can't be directly edited
	   ((or (eq field-type 'enum)
		(eq field-type 'enum-in-file))
	    (gnats-insert-label field t first-label-attr)
	    (gnats-insert-text
	     field field-value t
	     (append '(read-only t)
		     (unless input
		       (list 'mouse-face 'highlight
			     'local-map gnats-edit-button-map)))))
	   
	   (t (gnats-insert-label field t first-label-attr)
	      (gnats-insert-text field field-value t)))
	  field)
      prev-field)))

(defun gnats-format-pr (&optional input)
  "Format the currently-loaded PR into the edit buffer.
If the optional argument INPUT is nil, don't format read-only fields."
  (let ((prev-field nil))
    (mapcar '(lambda (field)
	       (setq prev-field (gnats-format-field prev-field field input)))
	    (get gnats-server-conn 'field-list))))

(defun gnats-gen-input-template ()
  "Generate an initial input template."
  (let ((prev-field nil))
    (mapcar '(lambda (field)
	       (setq prev-field (gnats-format-field prev-field field)))
	    (get gnats-server-conn 'input-fields))))

(defun gnats-gen-field-symbol (field)
  "Return the symbol for FIELD."
  (intern (concat "gnats-field-name-" (downcase field))))

(defun gnats-gen-changed-why (field)
  "Return the symbol of the change-reason field associated with FIELD."
  (make-variable-buffer-local
   (gnats-gen-field-symbol (concat (get field 'field-name) "-Changed-Why"))))

(defun gnats-add-field-name (element)
  "Add ELEMENT as a valid field name in the current database.
ELEMENT must be a string."
  (if (eq (car element) 0)
      (let ((namelen (length (nth 1 element)))
	    (field-sym (make-variable-buffer-local
			(gnats-gen-field-symbol (nth 1 element)))))
	(if (> namelen (get gnats-server-conn 'max-field-name-width))
	    (put gnats-server-conn 'max-field-name-width namelen))
	(put gnats-server-conn 'field-list
	     (append (get gnats-server-conn 'field-list) (list field-sym)))
	(put field-sym 'field-name (nth 1 element)))))

(defun gnats-field-list-as-string ()
  "Return the list of field names as a space-separated string."
  (mapconcat '(lambda (x) (get x 'field-name))
	     (get gnats-server-conn 'field-list)
	     " "))

(defun gnats-iterate-info (fields result-list func)
  ;; checkdoc-order: nil
  "Iterate the set of server responses in RESULT-LIST.
FIELDS is the list of corresponding field names.  For each valid server
response, FUNC is invoked with the field name and server response text."
  (while result-list
    (let ((elem (car result-list))
	  (field (car fields)))
      (if (or (eq (car elem) 0) (eq (car elem) 350))
	  (funcall func field (nth 1 elem))))
    (setq result-list (cdr result-list))
    (setq fields (cdr fields))))

(defun gnats-get-field-values (field)
  "Get the set of legal field values for FIELD from the server.
Store them as the valid-values property in FIELD."
  (let ((values (gnats-send-command "FVLD" (get field 'field-name)))
	(res nil))
    (while values
      (let ((elem (car values)))
	(if (eq (car elem) 0)
	    (setq res (append res (list (nth 1 elem)))))
	(setq values (cdr values))))
    (put field 'valid-values res)))

(defun gnats-add-changed-why (field)
  "Add a change-reason field for FIELD, if FIELD requires a change reason."
  (if (string-match "requirechangereason"
		    (downcase (get field 'flags)))
      (let ((newf (gnats-gen-changed-why field)))
	;; Add a property for it, to make things a bit easier.
	(put field 'requires-changed-why t)
	(put newf 'field-name (concat (get field 'field-name) "-Changed-Why"))
	(put newf 'type 'multitext))))

(defun gnats-get-field-info ()
  "Get the field metadata from the server.
Write it into the appropriate field properties."
  (put gnats-server-conn 'field-list nil)
  (put gnats-server-conn 'max-field-name-width 0)
  (put gnats-server-conn 'input-fields nil)
  (let ((our-field-list (gnats-send-command "list" "fieldnames")))
    (mapcar 'gnats-add-field-name our-field-list)
    (setq our-field-list (gnats-send-command "FTYP"
					     (gnats-field-list-as-string)))

    (gnats-iterate-info (get gnats-server-conn 'field-list) our-field-list
			'(lambda (field val)
			   (put field 'type
				(intern (downcase val)))))

    (setq our-field-list (gnats-send-command "FIELDFLAGS"
					     (gnats-field-list-as-string)))
    (gnats-iterate-info (get gnats-server-conn 'field-list) our-field-list
			'(lambda (field val)
			   (put field 'flags val)))

    (setq our-field-list (gnats-send-command "INPUTDEFAULT"
					     (gnats-field-list-as-string)))
    (gnats-iterate-info (get gnats-server-conn 'field-list) our-field-list
			'(lambda (field val)
			   (when val
			     (while (string-match "\\\\n" val)
			       (setq val (replace-match "\n" nil nil val)))
			     (when (and (gnats-multitext-field-p field)
					(string-match "\\`\n" val))
			       (setq val (replace-match "" nil nil val))))
			   (put field 'input-default val)))

    (setq our-field-list (gnats-send-command "LIST INITIALINPUTFIELDS"))

    (mapcar '(lambda (field)
	       (if (eq (car field) 0)
		   (put gnats-server-conn 'input-fields
			(append (get gnats-server-conn
				     'input-fields)
				(list
				 (gnats-gen-field-symbol (nth 1 field)))))))
	    our-field-list)

    (mapcar 'gnats-get-field-values (get gnats-server-conn 'field-list))
    (mapcar 'gnats-add-changed-why (get gnats-server-conn 'field-list))))

(defun gnats-field-name (string)
  "Return a list composed of the field name and text is returned.
STRING is a single line from a PR retrieved from the server.  If STRING does
not begin with a GNATS field header, the field name will be nil instead, and
the text will be the entire value of STRING."
  (let ((data (match-data)))
    (unwind-protect
	(if (string-match "^>\\([^:]+\\):[ \t]*" string)
	    (list (gnats-gen-field-symbol (match-string 1 string))
		  (substring string (match-end 0)))
	  (list nil string))
      (set-match-data data))))

(defun gnats-multitext-field-p (field)
  "Return t if FIELD is a multitext field, nil otherwise."
  (eq 'multitext (get field 'type)))

(defun gnats-parse-pr (state server-response)
  ;; checkdoc-order: nil
  "Parse one line of a PR retrieved from the server in SERVER-RESPONSE.
STATE is the current parse state.  The PR text is stored in the appropriate
field's value property, and the next parse state is returned."
  (if (and server-response (eq 0 (nth 0 server-response)))
      (let* ((field-info (gnats-field-name (nth 1 server-response)))
	     (field-name (nth 0 field-info))
	     (text (nth 1 field-info))
	     (field-type))
	(if (null field-name)
	    (setq field-name (nth 1 state))
	  ;; Clear the current field value; we're just seeing this field for
	  ;; the first time.
	  (put field-name 'value nil))
	(when field-name
	  (setq state (list (nth 0 state) field-name))
	  (setq field-type (get field-name 'type))
	  ;; If the current line has a field header, then we go back to
	  ;; state 0.
	  (if (nth 0 field-info)
	      (setq state '(0 nil)))
	  (if (eq 0 (nth 0 state))
	      (if (eq field-type 'multitext)
		  (setq state (list 1 field-name))
		(put field-name 'value text))
	    (let ((oldtext (get field-name 'value)))
	      (put field-name 'value
		   (concat oldtext (cond (oldtext "\n")) text)))))))
  state)

(defun gnats-fetch-pr (pr &optional input)
  "Lock and parse PR from the server.
If the optional argument INPUT is non-nil, get the problem report without
locking the database."
  (let ((our-pr-text (if input
			 (progn
			   ;; TODO: Handle gnatsd errors.
			   (gnats-send-command "QFMT" "full")
			   (gnats-send-command "RSET")
			   (gnats-send-command "QUER" pr))
		       (gnats-send-command "LOCK" (prin1-to-string pr t)
					   "emacs")))
	(parse-pr-state '(0 nil)))
    ;; We unlock it (for now).
    (unless input
      (unlock-pr pr))
    (put gnats-server-conn 'pr-number pr)
    ;; We must reset all input fields here, otherwise an input field from the
    ;; previous PR not present in this PR might survive.
    (mapc (lambda (field-name) (put field-name 'value nil))
	  (get gnats-server-conn 'field-list))
    (if (> (car (car our-pr-text)) 399)
	(progn
	  (error (nth 1 (car our-pr-text)))
	  nil)
      (mapcar '(lambda (x)
		 (setq  parse-pr-state (gnats-parse-pr parse-pr-state x)))
	      our-pr-text)
      t)))

(defun gnats-add-dot (string)
  "If STRING begins with a period (.), prepend a second period to it.
The resulting string is returned."
  (if (eq (string-to-char string) ?.)
      (concat "." string)
    string))

(defun gnats-nl-to-crnl (string)
  "Convert the linefeeds in STRING to CRLF pairs.
Additionally a single CRLF is appended if the text does not end in one.
The resulting string is returned."
  (let ((res "")
	(eol-chars (get gnats-server-conn 'eol-chars)))
    (while (string-match "\n" string)
      (setq res (concat res
			(gnats-add-dot (substring string
						  0
						  (match-beginning 0)))
			eol-chars)
	    string (substring string (match-end 0))))
    (if string
	(concat res (gnats-add-dot string) eol-chars)
      res)))

(defun gnats-send-command (&rest comm)
  "Send the command COMM to the server, and wait for the server's response."
  (let ((cstr (concat (mapconcat 'concat comm " ")
		    (get gnats-server-conn 'eol-chars))))
    (gnats-init-response)
    (gnats-debug-string cstr)
    (process-send-string (get gnats-server-conn 'process) cstr)
    (gnats-get-response)))

(defun gnats-send-command-and-text (comm first-text &optional second-text)
  "Send the command COMM, followed by FIRST-TEXT.
If the optional argument SECOND-TEXT is given and the server requests it, it
will be sent.  If COMM is successful nil is returned; otherwise, the server
error message is returned."
  (let* ((server-resp (gnats-send-command comm))
	 (server-code (car (car server-resp))))
    (if (and (> server-code 210)
	     (< server-code 300))
	(progn
	  (setq server-resp
		(gnats-send-command
		 (concat (gnats-nl-to-crnl first-text) ".")))
	  (setq server-code (car (car server-resp)))
	  (if (and second-text
		   (> server-code 210)
		   (< server-code 300))
	      (setq server-resp
		    (gnats-send-command
		     (concat (gnats-nl-to-crnl second-text) "."))))))
    server-resp))

(defun gnats-parse-server-line (string)
  "Parse the server response line in STRING and return it as a list of lists.
Each sublist consists of two elements; a response code and the associated text.
PR text has a response code of 0.  The `gnats-server-conn' property
`output-complete' will be set when the final server response line has been
reached."
  (let ((data (match-data))
	(status 0)
	(cont t)
	(res nil))
    (unwind-protect
	(progn
	  (while (string-match "^\\([^\r\n]*\\)\r?\n" string)
	    (let ((text (match-string 1 string)))
	      (setq string (substring string (match-end 0)))
	      ;; If in mode 1, the server's sending
	      ;; us the standard response lines.
	      ;; Mode 2 has output ending with a
	      ;; single . at the end.
	      (if (eq (get gnats-server-conn 'read-mode) 1)
		  (progn
		    ;; Should check result of string-match to see if it worked.
		    (string-match "^\\([0-9][0-9][0-9]\\)\\(.\\)\\(.*\\)$"
				  text)
		    (setq status (string-to-number (match-string 1 text)))
		    (setq cont (string= "-" (match-string 2 text)))
		    (setq text (match-string 3 text)))
					; Mode 2.
		(progn
		  (setq status 0)
		  ;; If the line contains just a period, then we're done.
		  (setq cont (not (string= "." text)))
		  ;; Strip leading periods.
		  (string-match "^[.]?\\(.*\\)$" text)
		  ;; Don't add a line for the final .
		  (if cont
		      (setq text (match-string 1 text))
		    (setq text nil))))
	      (if text
		  (setq res (append res (list (list status text)))))
	      (if (and (eq (get gnats-server-conn 'read-mode) 1)
		       (>= status 300)
		       (< status 350))
		  (progn
		    (put gnats-server-conn 'read-mode 2)
		    (setq cont t)))))
	  (put gnats-server-conn 'output-complete (not cont))
	  (put gnats-server-conn 'parsed-output
		(append (get gnats-server-conn 'parsed-output) res)))
      (set-match-data data))))

(defun gnats-init-response ()
  "Prepare to parse a server response.
This function should be invoked just before a server command is sent."
  (put gnats-server-conn 'curr-output "")
  (put gnats-server-conn 'parsed-output nil)
  (put gnats-server-conn 'read-mode 1)
  (put gnats-server-conn 'output-complete nil))

(defun gnats-get-response ()
  "Wait up to 10 seconds for a complete response from the server.
The response is a value returned from `gnats-parse-server-line'."
  ;;; TODO: Make the timeout configurable.
  (let ((no-timeout t))
    (while (and
	    no-timeout
	    (eq (get gnats-server-conn 'output-complete) nil))
      (setq no-timeout
	    (accept-process-output (get gnats-server-conn 'process) 10 0)))
    (if no-timeout
	(let ((res (get gnats-server-conn 'parsed-output)))
	  (put gnats-server-conn 'parsed-output nil)
	  res)
      (error "Input timeout from the server"))))

(defun gnats-process-filter (process output)
  "Process a response from the GNATS server connection in PROCESS.
OUTPUT is the text from the server.  The resulting parsed response is placed in
gnats-server-conn's `parsed-output' property.  If a complete response has been
parsed, the `output-complete' property is set."
  (put gnats-server-conn 'curr-output
       (concat (get gnats-server-conn 'curr-output) output))
  (if (eq 10 (string-to-char (substring output -1)))
      (progn
	(gnats-debug-string (get gnats-server-conn 'curr-output))
	(let ((our-output (get gnats-server-conn 'curr-output)))
	  (put gnats-server-conn 'curr-output "")
	  (gnats-parse-server-line our-output)))))
 
(defun gnats-server-login ()
  "Login to the server."
  (gnats-send-command "USER"
		      (get gnats-server-conn 'server-username)
		      (get gnats-server-conn 'server-password)))

(defun gnats-edit-chdb ()
  "Do a CHDB command as needed."
  (gnats-send-command "CHDB" (get gnats-server-conn 'server-database)))

(defun gnats-get-mail-alias ()
  "Return the mail alias for the current user.
The alias is taken from the database's responsible adm file.  If it cannot be
retrieved `user-mail-address' is used.
The alias is then set as the editting e-mail address."
  (let* ((resp (gnats-send-command "ADMV" "builtinfield:responsible"
				   (user-login-name) "alias"))
	 (alias (if (and (>= (car (car resp)) 350)
			 (< (car (car resp)) 400))
		    (nth 1 (car resp))
		  user-mail-address)))
    (put gnats-server-conn 'mail-alias alias)
    (when alias
      (gnats-send-command "EDITADDR" alias))
    alias))

(defun gnats-get-pr (pr &optional input)
  "Read in PR from the server and display it.
If the optional argument INPUT is non-nil, get the problem report without
locking the database."
    (when (gnats-fetch-pr pr input)
      (gnats-format-pr input)
      t))

(defun gnats-clear-field-contents (field)
  "Clear the field value properties for FIELD."
  (put field 'value nil)
  (put field 'was-edited nil)
  (put field 'has-chagned-why nil))

(defun gnats-set-initial-values ()
  "Set the initial values for PR fields."
  (mapcar '(lambda (field)
	     (put field 'value (get field 'input-default)))
	  (get gnats-server-conn 'field-list))
  (if gnats-default-organization
      (put 'gnats-field-name-organization 'value gnats-default-organization))
  (if gnats-default-submitter
      (put 'gnats-field-name-submitter-id 'value gnats-default-submitter))
  (put 'gnats-field-name-originator 'value
       (concat (get gnats-server-conn 'mail-alias) " (" (user-full-name) ")")))

(defun gnats-clear-all-field-contents ()
  "Clear all the field contents."
  (mapcar 'gnats-clear-field-contents (get gnats-server-conn 'field-list)))

(defun gnats-clear-edit-buffer ()
  "Clear the contents of the field buffer, and any associated data structures."
  (toggle-read-only -1)
  (gnats-run-modify-command (erase-buffer))
  (gnats-clear-all-field-contents))

(defun gnats-get-addr-info (pr)
  "Determine the username, passwd, database server, etc. to use for PR."
  (save-excursion
    (let* ((database (or (and
			  pr
			  (string-match "^\\([^:]+\\):" (prin1-to-string pr t))
			  (match-string 1 (prin1-to-string pr t)))
			 gnats-database
			 (and (not (getenv "GNATSDB"))
			      "default")))
	   (server (or gnats-server
		       (and (not (getenv "GNATSDB"))
			    "localhost")))
	   (port (or (and gnats-port
			  (prin1-to-string gnats-port t))
		     (and (not (getenv "GNATSDB"))
			  "1529")))
	   (user (or gnats-user
		     (and (not (getenv "GNATSDB"))
			  (user-login-name))))
	   (password (or gnats-password
			 (unless (getenv "GNATSDB")
			   (setq gnats-password
				 (read-passwd
				  (format "Password for %s@%s:%s/%s: "
					  user server port database)
				  nil "*")))))
	   (query-args (append (when database (list "--database" database))
			       (when database (list "--host" server))
			       (when database (list "--port" port))
			       (when database (list "--user" user))
			       (when database (list "--passwd" password))))
	   (query-pr-buffer
	    (generate-new-buffer " *gnats-query-pr*"))
	   (query-pr-res
	    (if (and server (not (string-match "^/" server)))
		(apply 'call-process
		       "query-pr" nil query-pr-buffer nil "--print-server-addr"
		       query-args)
	      (getenv "GNATSDB"))))
      (if query-pr-res
	  (let ((buffer-contents (save-excursion
				   (set-buffer query-pr-buffer)
				   (buffer-string))))
	    (if (string-match "^\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^:]+\\):\\([^\r\n]+\\)" buffer-contents)
		(progn
		  (put gnats-server-conn 'server-host
		       (match-string 1 buffer-contents))
		  (put gnats-server-conn 'server-port
		       (let ((x (string-to-int
				 (match-string 2 buffer-contents))))
			 (if (< 0 x)
			     x
			   (match-string 2 buffer-contents))))
		  (put gnats-server-conn 'server-database
		       (match-string 3 buffer-contents))
		  (put gnats-server-conn 'server-username
		       (match-string 4 buffer-contents))
		  (put gnats-server-conn 'server-password
		       (match-string 5 buffer-contents)))
	      (switch-to-buffer query-pr-buffer)
	      (error "Couldn't retrieve server info")))
	(error "Couldn't retrieve server info from query-pr"))
      (kill-buffer query-pr-buffer))))

;;;###autoload
(defun send-pr (&optional arg)
  "Generate a new initial PR template for the user.
If a prefix argument is given, run `gnats-change-database' first."
  (interactive "P")
  (when arg
    (call-interactively 'gnats-change-database))
  (let ((edit-buffer (generate-new-buffer "*gnats-send*"))
	(success nil))
    (unwind-protect
	(save-excursion
	  (set-buffer edit-buffer)
	  (gnats-clear-edit-buffer)
	  (gnats-edit-server)
	  (gnats-server-login)
	  (gnats-edit-chdb)
	  (gnats-get-mail-alias)
	  (gnats-get-field-info)
	  (gnats-set-initial-values)
	  (gnats-gen-input-template)
	  (put gnats-server-conn 'mode 'submit)
	  (gnats-edit-mode)
	  (setq success t))
      (if (not success)
	  (progn
	    (kill-buffer edit-buffer)
	    (kill-buffer (get gnats-server-conn 'server-buffer)))
	(switch-to-buffer edit-buffer)
	(goto-char (point-min))))))

;;;###autoload
(defun edit-pr (pr)
  "Edit the specified PR; prompts for a PR number if one was not given."
  (interactive "nPR Number: ")
  ;; Should whine if a gnats-edit-PR buffer for the PR already exists.
  (setq pr (prin1-to-string pr t))
  (let ((edit-buffer (generate-new-buffer
		      (concat "*gnats-edit-" pr "*")))
	(success nil))
    (unwind-protect
	(save-excursion
	  (set-buffer edit-buffer)
	  (gnats-clear-edit-buffer)
	  (gnats-edit-server pr)
	  (gnats-server-login)
	  (gnats-edit-chdb)
	  (gnats-get-mail-alias)
	  (gnats-get-field-info)
	  (if (gnats-get-pr pr)
	      (progn
		(put gnats-server-conn 'mode 'edit)
		(gnats-edit-mode)
		(setq success t))))
      (if (not success)
	  (progn
	    (kill-buffer edit-buffer)
	    (kill-buffer (get gnats-server-conn 'server-buffer)))
	(switch-to-buffer edit-buffer)
	(goto-char (point-min))))))

(defvar gnats-query-buffer-name "*gnats-query*"
  "Name of the buffer presenting the summary of PRs returned by a query.")

(defvar gnats-query-mode-map '()
  "Keymap for GNATS query mode.")
(when (null gnats-query-mode-map)
  (setq gnats-query-mode-map (make-keymap))
  (define-key gnats-query-mode-map "\r" 'gnats-query-view-pr)
  (define-key gnats-query-mode-map "v" 'gnats-query-view-pr)
  (define-key gnats-query-mode-map [mouse-2] 'gnats-query-view-pr)
  (define-key gnats-query-mode-map "D" 'gnats-change-database)
  (define-key gnats-query-mode-map "e" 'gnats-query-edit-pr)
  (define-key gnats-query-mode-map "g" 'gnats-query-reread)
  (define-key gnats-query-mode-map "G" 'query-pr)
  (define-key gnats-query-mode-map "q" 'bury-buffer)
  (define-key gnats-query-mode-map "s" 'send-pr))

(defvar gnats-query-mode-hook nil
  "Hook run by `gnats-query-mode'.")

(defvar gnats-query-font-lock-keywords
  '(("^\\s-*[0-9]+\\s-+\\S-+\\s-+\\S-+\\s-+\\(feedback\\|closed\\).*$"
     . font-lock-comment-face)
    ("^.*\\<high\\>.*$"
     . font-lock-warning-face)))

(defun gnats-query-mode ()
  "Major mode for browsing GNATS reports.
Use \\[gnats-query-view-pr] to view the problem report under the cursor and
\\[gnats-query-edit-pr] to edit it.

\\{gnats-query-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gnats-query-mode)
  (setq mode-name "gnats-query")
  (use-local-map gnats-query-mode-map)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (make-local-variable 'truncate-lines)
  (setq truncate-lines t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gnats-query-font-lock-keywords t))
  (view-mode)
  (setq minor-mode-overriding-map-alist
	(list (cons 'view-mode (let ((keymap (copy-keymap view-mode-map)))
				 (define-key keymap "e" nil)
				 (define-key keymap "g" nil)
				 (define-key keymap "s" nil)
				 (define-key keymap "\r" nil)
				 keymap))))
  (run-hooks 'gnats-query-mode-hook))
(put 'gnats-query-mode 'mode-class 'special)

(defun gnats-query-current-pr ()
  "Return the id of the problem report on the current line, as a string.
If there is no problem report on the current line, signal an error.
The function works in the query buffers."
  (let ((pr (save-excursion
	      (beginning-of-line)
	      (when (re-search-forward "\\=[ 	]*\\([0-9]+\\)" nil t)
		(match-string-no-properties 1)))))
    (or pr
	(error "No problem on the current line"))))

(defvar gnats-view-mode-map nil
  "Keymap for GNATS view mode.")
(when (null gnats-view-mode-map)
  (setq gnats-view-mode-map (make-keymap))
  (define-key gnats-view-mode-map "e" 'gnats-view-edit-pr))

(defvar gnats-view-mode-hook nil
  "Hook run by `gnats-view-mode'.")

(defun gnats-view-edit-pr ()
  "Turn the current viewer of a problem report into the edit mode."
  (interactive)
  (unless (eq major-mode 'gnats-view-mode)
    (error "Not in the gnats-view mode"))
  (rename-buffer (generate-new-buffer-name
		  (concat "*gnats-edit-"
			  (prin1-to-string (get gnats-server-conn 'pr-number)
					   t)
			  "*")))
  ;; We have to reformat the buffer to discard read-only fields
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (gnats-format-pr))
  (put gnats-server-conn 'mode 'edit)
  (gnats-edit-mode)
  (goto-char (point-min)))

(defun gnats-view-mode ()
  "Major mode for viewing GNATS problem reports.

\\{gnats-view-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'gnats-view-mode)
  (setq mode-name "gnats-view")
  (use-local-map gnats-view-mode-map)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (make-local-variable 'gnats-edit-font-lock-keywords)
  (gnats-update-font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gnats-edit-font-lock-keywords t t))
  (view-mode)
  (setq minor-mode-overriding-map-alist
	(list (cons 'view-mode (let ((keymap (copy-keymap view-mode-map)))
				 (define-key keymap "e" nil)
				 keymap))))
  (run-hooks 'gnats-view-mode-hook))
(put 'gnats-view-mode 'mode-class 'special)

(defun gnats-query-view-pr ()
  "View the problem report listed on the current line."
  (interactive)
  (when (memq 'down (event-modifiers last-command-event))
    (mouse-set-point last-command-event))
  (view-pr (string-to-int (gnats-query-current-pr))))

;;;###autoload
(defun view-pr (pr)
  "View the problem report PR."
  (interactive "nPR Number: ")
  (let* ((edit-buffer (generate-new-buffer
		       (concat "*gnats-view-" (prin1-to-string pr t) "*")))
	 (success nil))
    (unwind-protect
	(save-excursion
	  (set-buffer edit-buffer)
	  (gnats-clear-edit-buffer)
	  (gnats-edit-server)
	  (gnats-server-login)
	  (gnats-edit-chdb)
	  (gnats-get-mail-alias)
	  (gnats-get-field-info)
	  (if (gnats-get-pr (format "%d" pr) t)
	      (progn
		(put gnats-server-conn 'mode 'view)
		(gnats-view-mode)
		(setq success t))))
      (if (not success)
	  (progn
	    (kill-buffer edit-buffer)
	    (kill-buffer (get gnats-server-conn 'server-buffer)))
	(switch-to-buffer edit-buffer)
	(goto-char (point-min))))))

(defun gnats-query-edit-pr ()
  "Edit the problem report listed on the current line."
  (interactive)
  (edit-pr (string-to-int (gnats-query-current-pr))))

(defun gnats-do-query (query)
  "Perform QUERY and insert the result to the current buffer."
  ;; TODO: Handle gnatsd errors.
  (gnats-send-command "QFMT" "summary")
  (gnats-send-command "RSET")
  (unless (string= query "")
    (gnats-send-command "EXPR" query))
  (let ((result (gnats-send-command "QUER")))
    (unless (= (caar result) 300)
      (error "Query error (%d %s)" (car (car result)) (cadr (car result))))
    (setq result (cdr result))
    (while result
      (let ((line (car result)))
	(unless (= (car line) 0)
	  (error "Error during receiving the answer"))
	(let ((point (point)))
	  (insert (cadr line) "\n")
	  (put-text-property point (1- (point)) 'mouse-face 'highlight)))
      (setq result (cdr result)))))

(defvar query-pr-history nil
  "History of the `query-pr' command.")

(defun gnats-query-reread ()
  "Redo last query."
  (interactive)
  (if query-pr-history
      (query-pr (car query-pr-history))
    (call-interactively 'query-pr)))

;;;###autoload
(defun query-pr (query)
  "Create query buffer resulting from QUERY.
QUERY is a string representing a query in the gnatsd format."
  ;; TODO: Maybe the description of the format should be here?
  (interactive (list (read-string "Query: " nil 'query-pr-history)))
  (let ((query-buffer (get-buffer-create gnats-query-buffer-name))
	(success nil))
    (unwind-protect
	(save-excursion
	  (set-buffer query-buffer)
	  (gnats-clear-edit-buffer)
	  (gnats-delete-process)
	  (gnats-edit-server)
	  (gnats-server-login)
	  (gnats-edit-chdb)
	  (gnats-get-mail-alias)
	  (gnats-get-field-info)
	  (gnats-do-query query)
	  (sort-lines gnats-query-reverse-listing (point-min) (point-max))
	  (put gnats-server-conn 'mode 'query)
	  (gnats-query-mode)
	  (setq success t))
      (if (not success)
	  (progn
	    (kill-buffer query-buffer)
	    (kill-buffer (get gnats-server-conn 'server-buffer)))
	(switch-to-buffer query-buffer)
	(message "%d matching PRs." (count-lines (point-min) (point-max)))
	(goto-char (point-min))))))

;;;###autoload
(defun unlock-database ()
  "Unlock the whole database."
  (interactive)
  (gnats-send-command "UNDB"))

;;;###autoload
(defun unlock-pr (pr)
  "Unlock the problem report PR."
  (interactive "sPR Number: ")
  (gnats-send-command "UNLK" pr))

(defun gnats-change-database (database host port user)
  "Set new database parameters."
  (interactive
   (list (read-from-minibuffer "Database: " gnats-database)
	 (read-from-minibuffer "Host: " (or gnats-server "localhost"))
	 (string-to-number
	  (read-from-minibuffer "Port: " (if gnats-port
					     (format "%d" gnats-port)
					   1529)))
	 (read-from-minibuffer "User: " (or gnats-user (user-login-name)))))
  (setq gnats-database database
	gnats-server host
	gnats-port port
	gnats-user user
	gnats-password nil)
  (setq-default gnats-database database
		gnats-server host
		gnats-port port
		gnats-user user))

(defun gnats-show-connection ()
  "Show the server connection buffer associated with the current buffer.
You can use this function to view the communication with gnatsd in case of
problems."
  (interactive)
  (let ((buffer (get gnats-server-conn 'server-buffer)))
    (if buffer
	(switch-to-buffer buffer)
      (error "No connection associated with this buffer"))))

  


(defvar gnats-dbconfig-mode-map nil
  "Keymap for `gnats-dbconfig-mode'.")
(unless gnats-dbconfig-mode-map
  (setq gnats-dbconfig-mode-map (make-sparse-keymap)))

(defvar gnats-dbconfig-mode-syntax-table nil
  "Syntax table for `gnats-dbconfig-mode'.")
(unless gnats-dbconfig-mode-syntax-table
  (setq gnats-dbconfig-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\(  "(" gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?\)  ")" gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?{  "("  gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?}  ")"  gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?$  "_"  gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?#  "<"  gnats-dbconfig-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  gnats-dbconfig-mode-syntax-table))

(defvar gnats-dbconfig-mode-abbrev-table nil
  "Abbrev table for `gnats-dbconfig-mode'.")

(defvar gnats-dbconfig-font-lock-keywords
  `(,(regexp-opt
      '("add-audit-trail" "all" "allow-any-value" "append-to-field"
	"audit-trail-format" "aux-flags" "binary-index" "body" "builtin-name"
	"business-day-hours" "business-week-days" "category-dir-perms"
	"create-category-dirs" "database-info" "debug-mode" "default"
	"description" "edit-only" "exact-regexp" "field" "fields"
	"fixed-address" "format" "from-address" "header" "index"
	"inexact-regexp" "initial-entry" "input-default"
	"keep-all-received-headers" "key" "libexecdir" "mail-format"
	"matching" "max-prs-in-field" "nospaces-in-index"
	"notify-about-expired-prs" "on-change" "path" "pr-list" "query"
	"query-default" "raw" "read-only" "reply-to" "require"
	"require-change-reason" "restricted" "send-submitter-ack" "separator"
	"separators" "set-field" "textsearch" "to-addresses" "values"
	"virtual-format")
      'words)
    (,(regexp-opt
       '("date" "enum" "enumerated-in-file" "integer"
	 "multi-enumerated-in-file" "multienum" "multitext" "text")
       'words)
     . font-lock-type-face)
    ("false\\|true" . font-lock-constant-face)
    (,(concat "\"\\(\\$"
	      (regexp-opt
	       '("ChangeReason" "CurrentDate" "EditUserEmailAddr" "FieldName"
		 "MailCC" "MailFrom" "MailSubject" "MailTo" "NewAuditTrail"
		 "NewValue" "OldResponsible" "OldValue" "PRNum")
	       'words)
	      "\\)\"") . (1 font-lock-variable-name-face t nil)))
  "Font lock keywords for `gnats-dbconfig-mode'.")

(defvar gnats-dbconfig-mode-hook nil
  "Hook run `gnats-dbconfig-mode'.")

;;;###autoload
(defun gnats-dbconfig-mode ()
  "Major mode for editing the `dbconfig' GNATS configuration file."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gnats-dbconfig-mode)
  (setq mode-name "dbconfig")
  (use-local-map gnats-dbconfig-mode-map)
  (set-syntax-table gnats-dbconfig-mode-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "# "
	comment-start-skip "# *")
  (setq local-abbrev-table gnats-dbconfig-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gnats-dbconfig-font-lock-keywords nil t))
  (setq imenu-generic-expression
	'((nil "^\\(database-info\\|on-change\\|index\\|initial-entry\\)" 1)
	  ("Mail formats" "^mail-format +\"\\([^\"\n]+\\)\"" 1)
	  ("Queries" "^query +\"\\([^\"\n]+\\)\"" 1)
	  ("Fields" "^field +\"\\([^\"\n]+\\)\"" 1)))
  (run-hooks 'gnats-dbconfig-mode-hook))
(add-to-list 'auto-mode-alist '("\\<dbconfig$" . gnats-dbconfig-mode))

(provide 'gnats)

;;; gnats.el ends here
