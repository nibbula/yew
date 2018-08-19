;;
;; rl.lisp - An input line editor.
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defkeymap *normal-keymap*
  `(
    ;; Movement
    (,(ctrl #\B)		. backward-char)
    (,(ctrl #\F)		. forward-char)
    (,(ctrl #\A)		. beginning-of-line)
    (,(ctrl #\E)		. end-of-line)
    (,(ctrl #\P)		. previous-history)
    (,(ctrl #\N)		. next-history)
    (,(meta-char #\b)		. backward-word)
    (,(meta-char #\f)		. forward-word)
    (,(meta-char #\<)		. beginning-of-history)
    (,(meta-char #\>)		. end-of-history)

    ;; Editing
    (#\return			. accept-line)
    (#\newline			. accept-line)
    (#\backspace		. delete-backward-char)
    (#\rubout			. delete-backward-char)
    (,(ctrl #\D)		. delete-char-or-exit)
    (,(ctrl #\W)		. backward-kill-word)
    (,(ctrl #\K)		. kill-line)
    (,(ctrl #\@)		. set-mark)
    (,(ctrl #\Y)		. yank)
    (,(ctrl #\U)		. backward-kill-line)
    (,(ctrl #\O)		. undo-command)
    (,(meta-char #\d)		. kill-word)
    (,(meta-char #\rubout)	. backward-kill-word)
    (,(meta-char #\u)		. upcase-word)
    (,(meta-char #\l)		. downcase-word)
    (,(meta-char #\c)		. capitalize-word)
    (,(meta-char #\w)		. copy-region)
    (,(meta-char #\\)		. delete-horizontal-space)

    ;; Completion
    (#\tab			. complete)
    (#\?			. show-completions)
    (,(meta-char #\/)		. complete-filename-command)
    (,(meta-char #\?)		. show-filename-completions-command)

    ;; Misc
    (,(ctrl #\L)		. redraw-command)
    (,(ctrl #\G)		. abort-command)
    (,(ctrl #\S)		. isearch-forward)
    (,(ctrl #\R)		. isearch-backward)
    (,(ctrl #\T)		. toggle-debugging) ; @@@ temporary?
    (,(ctrl #\Q)		. quoted-insert)

    ;; key binding
    (,(meta-char #\=)		. describe-key-briefly)
    (,(meta-char #\+)		. set-key-command)

    ;; dorky temporary
    (,(meta-char #\t)		. un-studly-cap)
    
    ;; Other keymaps
    (#\escape			. *escape-keymap*)
    (,(ctrl #\X)		. *ctlx-keymap*)
    )
  :default-binding 'self-insert
)

;; These ^X commands are quite impoverished.
(defkeymap *ctlx-keymap*
  `(
    ;; (,(ctrl #\F) (edit-function))
    ;; (,(ctrl #\Q) (toggle-read-only))
    ;; (,(ctrl #\S) (save-history))
    ;; (,(ctrl #\Z) (suspend))
    ;; (#\h (hyperspec-lookup))
    ;; (#\i (insert-file))
    ;; (#\= (describe-cursor-position))
    ;; (#\! (shell-command))
    ;; (#\( (start-macro))
    ;; (#\) (end-macro))
    (#\9		. unipose-command)
    (#\7		. char-picker-command)
    (#\l		. pop-to-lish)
    (#\=		. what-cursor-position)
    (,(ctrl #\C)	. exit-editor)
    (,(ctrl #\X)	. exchange-point-and-mark)))
;  :default-binding #| (beep e "C-x ~a is unbound." cmd |#

#|
(defkeymap *app-key-keymap*
  `((#\A . previous-history)		; :up
    (#\B . next-history)		; :down
    (#\C . forward-char) 		; :right
    (#\D . backward-char) 		; :left
    ;; Movement keys
    (#\H . beginning-of-line) 		; :home
    (#\F . end-of-line) 		; :end
    ;; Function keys
;    (#\P . ) 				; :f1
;    (#\Q . ) 				; :f2
;    (#\R . ) 				; :f3
;    (#\S . ) 				; :f4
    ))

(defkeymap *escape-raw-keymap*
  `(
    ;;(#\O	. do-app-key)
    (#\O	. *app-key-keymap*)
    (#\[	. do-function-key)
    (#\newline  . finish-line)
   ))
|#

(defkeymap *special-keymap*
  `(
    (:left            . backward-char)
    (:right           . forward-char)
    (:up              . previous-history)
    (:down            . next-history)
    (:backspace       . delete-backward-char)
    (:home            . beginning-of-line)
    (:end             . end-of-line)
    (:delete	      . delete-char)
    (:bracketed-paste . bracketed-paste)
    (:return	      . accept-line)
    ;; XXX @@@ this shouldn't be here. It should be in the repl or lish
    (:f9              . pop-to-lish)
    ))

;; Normal mode commands prefaced by escape.
(defparameter *escape-keymap*
  ;;(add-keymap (build-escape-map *normal-keymap*) *escape-raw-keymap*)
  (build-escape-map *normal-keymap*))

;; Make the stuff in the special keymap appear in the normal keymap too.
(add-keymap *special-keymap* *normal-keymap*)

(defkeymap *vi-insert-mode-keymap*
  `(
    ;; Editing
    (#\return			. accept-line)
    (#\newline			. accept-line)
    (#\backspace		. delete-backward-char)
    (#\rubout			. delete-backward-char)
    (,(ctrl #\W)		. backward-kill-word)
    (,(ctrl #\U)		. backward-kill-line)

    ;; Completion
    (#\tab			. complete)
    (,(ctrl #\D)		. show-completions)

    ;; Misc
    (,(ctrl #\L)		. redraw-command)
    ;; (,(ctrl #\R)		. @@@ redisplay? @@@)
    (,(ctrl #\Q)		. quoted-insert)

    ;; Other keymaps
    (#\escape			. set-vi-command-mode)
    (,(ctrl #\C)		. set-vi-command-mode)
    (,(ctrl #\O)		. vi-do-command)
    (,(ctrl #\X)		. *ctlx-keymap*)
    )
  :default-binding 'self-insert
)

;; Make the stuff in the special keymap appear in the vi insert keymap too.
(add-keymap *special-keymap* *vi-insert-mode-keymap*)

(defkeymap *vi-command-mode-keymap*
  `(
    ;; Movement
    (,(ctrl #\N)		. next-history)		;
    (,(ctrl #\P)		. previous-history)	;
    (#\j			. next-history)		;
    (#\k			. previous-history)	;
    (#\h			. backward-char)	;
    (#\backspace		. backward-char)	;
    (#\rubout			. backward-char)	;
    (#\l			. forward-char)		;
    (#\space			. forward-char)		;
    (#\0			. beginning-of-line)	;
    (#\^			. first-nonblank)	; *
    (#\$			. end-of-line)		;
    (#\g			. vi-goto)		; *
    (#\b			. backward-word)	;
    (#\W			. backward-word)	;
    (#\w			. forward-word)		;
    (#\|			. vi-goto-column)	; *
    (#\f			. vi-find-right)	; *
    (#\F			. vi-find-left)		; *
    (#\:			. vi-repeat-find)	; *
    (#\,			. vi-repeat-find-reverse) ; *
    (#\(			. forward-sexp)		; *
    (#\)			. backward-sexp)	; *
    (#\[			. vi-forward-thing)	; *
    (#\]			. vi-backward-thing)	; *
    (#\'			. vi-goto-mark)		; *
    (#\`			. vi-goto-mark-any)	; *
    (#\H			. vi-goto-line)		; *

    ;; Editing
    (#\return			. accept-line)		;
    (#\newline			. accept-line)		;
    (,(ctrl #\R)		. undo)			; @@@ should be redo??
    (,(ctrl #\A)		. vi-incrment)		;
    (,(ctrl #\X)		. vi-decrement)		;
    (#\a			. vi-append)		; *
    (#\A			. vi-append-eol)	; *
    (#\c			. vi-change)		; *
    (#\d			. vi-delete-move)	; *
    (#\D			. vi-delete-eol)	; *
    (#\m			. vi-mark)		; *
    (#\'			. vi-set-buffer)	; *
    (#\i			. vi-insert)		; *
    (#\I			. vi-insert-bot)	; *
    (#\J			. vi-join-lines)	; *
    (#\o			. vi-open-above)	; *
    (#\O			. vi-open-below)	; *
    (#\P			. vi-put-before)	; *
    (#\p			. vi-put-after)		; *
    (#\s			. vi-substitute)	; *
    (#\u			. undo)			;
    (#\x			. vi-delete-char)	; *
    (#\rubout			. delete-backward-char)	; *
    (#\X			. delete-backward-char)	; *
    (#\y			. vi-yank)		; *
    (#\Y			. vi-yank-lines)	; *
    (#\~			. vi-toggle-case)	; *

    ;; Completion
    (#\tab			. complete)		;
    (,(ctrl #\D)		. show-completions)	;

    ;; Misc
    (#\0			. vi-digit)		; *
    (#\1			. vi-digit)		; *
    (#\2			. vi-digit)		; *
    (#\3			. vi-digit)		; *
    (#\4			. vi-digit)		; *
    (#\5			. vi-digit)		; *
    (#\6			. vi-digit)		; *
    (#\7			. vi-digit)		; *
    (#\8			. vi-digit)		; *
    (#\9			. vi-digit)		; *
    (,(ctrl #\L)		. redraw-command)	;
    ;; (,(ctrl #\R)		. @@@ redisplay? @@@)
    (,(ctrl #\G)		. what-cursor-position)	; *
    (#\n			. vi-next-match)	; *
    (#\N			. vi-previous-match)	; *
    (#\/			. vi-re-search-forward)	; *
    (#\?			. vi-re-search-backward) ; *
    (#\.			. vi-repeat-change)

    ;; Other keymaps
    (#\escape			. beep-command)
    (,(ctrl #\X)		. *ctlx-keymap*)
    )
  :default-binding 'self-insert
)

(add-keymap *special-keymap* *vi-command-mode-keymap*)

;; @@@ do we really need this?
;; (defun bad-special-key (e)
;;   (beep e "Bad special key ~s." key))

#|
(defun do-app-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-app-key e)))))

(defun do-function-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-function-key e)))))

(defun do-special-key (e key)
  (perform-key e key *special-keymap*))
|#

;; Key bindings can be a list to apply, or a symbol bound to function to call,
;; or a keymap in which to look up further key presses.
(defun perform-key (e key keymap)
  "Perform the action for the key in the keymap."
  (let* ((action (key-definition key keymap)))
    (cond
      ((not action)
       (beep e "Key ~a is not bound in keymap ~w." (nice-char key) keymap)
       (return-from perform-key))
      ;; a list to apply
      ((consp action)
       ;;(dbug "action is a cons~%")
       (if (fboundp (car action))
	   (apply (car action) (cdr action))
	   (beep e "(~S) is not defined." (car action))))
      ;; something represted by a symbol
      ((symbolp action)
       (cond
	 ((fboundp action)		; a function
	  (funcall action e))
	 ((keymap-p (symbol-value action)) ; a keymap
	  (setf (cmd e) (get-a-char e))
	  (log-message e "keymap cmd ~s" (cmd e))
	  (perform-key e (cmd e) (symbol-value action)))
	 (t				; anything else
	  (beep e "Key binding ~S is not a function or a keymap." action))))
      ;; a function object
      ((functionp action)
       ;;(dbug "action is a function~%")
       (funcall action e))
      (t					; anything else is an error
       (error "Weird thing in keymap: ~s." action)))))

(defvar *terminal-name* nil
  "Device name of the terminal to use for input.")

;; The main entry point

(defun rl (&key (input-stream *standard-input*)
	     (eof-error-p t)
	     (eof-value nil)
	     (quit-value nil)
	     (recursive-p nil)
	     (prompt *default-prompt*)
	     (output-prompt-func nil)
	     (completion-func #'complete-symbol)
	     (string nil)
	     (input-callback nil)
	     (output-callback nil)
	     (debug nil)
	     (editor nil)
	     (local-keymap nil)
	     (keymap nil)
	     (terminal-name *terminal-name*)
	     (terminal-class (find-terminal-class-for-type
			      (pick-a-terminal-type)))
			      ;; *default-terminal-type*))
	     (accept-does-newline t)
	     (context :tiny))		; remnant
  "Read a line from the terminal, with line editing and completion.
Return the string read and the line-editor instance created.
Keyword arguments: 
  EOF-ERROR-P (T)                 
    True to signal an error on end of file.
  EOF-VALUE (nil)
    Value to return on end of file. 
  QUIT-VALUE (nil)
    Value to return if the user quit.
  PROMPT (*default-prompt*)
    String to prompt with.
  OUTPUT-PROMPT-FUNC (nil)
    Function to print out a prompt. Called with the LINE-EDITOR instance and a
    prompt string.
  COMPLETION-FUNC (#'complete-symbol)
    Completion function to use. See the completion package for details.
  EDITOR (nil)
    LINE-EDITOR instance to use.
  LOCAL-KEYMAP (nil)
    A LOCAL-KEYMAP to use. For when you want to add your own customized key
    bindings.
  KEYMAP (nil)
    A KEYMAP to use. If you want to completely replace all the key bindings
    by your own. This defaults to a list of (LOCAL-KEYMAP *NORMAL-KEYMAP*).
  TERMINAL-NAME (*terminal-name*)
    Name of a terminal device to use. If NIL 
  ACCEPT-DOES-NEWLINE (t)
    True if accept-line outputs a newline.
  CONTEXT (nil)
    Symbol or string which defines the context for keeping history.
"			    ; There must be a better way to format docstrings.
  (declare (ignore recursive-p))
  (history-init context)

  ;; Initialize the buffer
  (let* ((e (or editor (make-instance
			'line-editor
			:point		    	0
			:prompt		    	prompt
			:prompt-func	    	output-prompt-func
			:completion-func    	completion-func
			:context	    	context
			:input-callback	    	input-callback
			:output-callback    	output-callback
			:debugging	    	debug
			:local-keymap	    	local-keymap
			:keymap		    	keymap
			:accept-does-newline	accept-does-newline
			:terminal-device-name	terminal-name
			:terminal-class	    	terminal-class)))
	 (*terminal* (line-editor-terminal e))
	 ;;(*standard-output* *terminal*)
	 ;;(*standard-input* *terminal*)
	 (*completion-count* 0)
	 (*history-context* context)
	 terminal-state)
    (when editor
      (freshen editor))
    (setf (fill-pointer (buf e)) (point e))
    #+ccl (setf ccl::*auto-flush-streams* nil)
    ;; (when (typep (line-editor-terminal e)
    ;; 		 (find-terminal-class-for-type :crunch))
    ;;   (setf (oelt (line-editor-terminal e) :start-line)
    ;; 	    (terminal-get-cursor-position
    ;; 	     (oelt (line-editor-terminal e) :wrapped-terminal))))
    (setf terminal-state (terminal-start (line-editor-terminal e)))

    ;; Add the new line we're working on.
    (history-add nil)
    (history-next)

    ;; Output the prompt
    (setf (prompt e) prompt (prompt-func e) output-prompt-func)
    (do-prompt e (prompt e) (prompt-func e))
    (when string
      (without-undo (e)
	(buffer-insert e 0 string)
	(setf (point e) (length string))
	(display-buf e 0)))

    ;; If the terminal is in line mode, our whole thing is kind of moot, so just
    ;; fall back to reading from the terminal driver, so we work on dumb
    ;; terminals.
    (when (eq (tt-input-mode) :line)
      (finish-output)
      (return-from rl (values (read-line *terminal*) e)))

    ;; Command loop
    (with-slots (quit-flag exit-flag cmd buf point last-input terminal
		 debugging) e
      (let ((result nil))
	(unwind-protect
	     (loop :do
		(finish-output)
		(when debugging
		  (message e "~d ~d [~d x ~d] ~a ~w"
			   (screen-col e) (screen-row e)
			   (terminal-window-columns terminal)
			   (terminal-window-rows terminal)
			   ;; (when (typep *terminal*
			   ;; 		'terminal-crunch:terminal-crunch)
			   ;;   (terminal-crunch::start-line *terminal*))
			   point cmd)
		  (show-message-log e))
		;; @@ Is this really where I want it?
		(when (line-editor-output-callback e)
		  (tt-save-cursor)
		  (tt-cursor-off)
		  (unwind-protect
		       (funcall (line-editor-output-callback e) e)
		    (tt-cursor-on)
		    (tt-restore-cursor)))
		(setf cmd (get-a-char e))
		(log-message e "cmd ~s" cmd)
		(when (need-to-redraw e)
		  (redraw e))
		(if (equal cmd '(nil))
		    (if eof-error-p
			(error (make-condition 'end-of-file
					       :stream input-stream))
			(setf result eof-value))
		    (progn
		      (setf (did-complete e) nil)
		      (perform-key e cmd (line-editor-keymap e))
		      (setf (last-command-was-completion e) (did-complete e))
		      (when (not (last-command-was-completion e))
			(set-completion-count e 0))
		      (when exit-flag (setf result quit-value))
		      ;; @@@ perhaps this should be done by a hook?
		      (if (eq *paren-match-style* :highlight)
			  (cond
			    ((is-open-char (aref buf point))
			     (highlight-paren e point))
			    ((and (plusp point)
				  (is-close-char (aref buf (1- point))))
			     (highlight-paren e (1- point)))))))
		(setf last-input cmd)
		:while (not quit-flag))
	  (block nil
	    (tt-finish-output)
	    (terminal-end terminal terminal-state)))
	(values (if result result (fatchar-string-to-string buf))
		e)))))

;; This is for compatability with read-line.
(defun rl-read-line (&optional (input-stream *standard-input*)
		       (eof-error-p t)
		       (eof-value nil)
		       (recursive-p nil)
		       (prompt ""))
  "Replacement for read-line, with line editing."
  (rl :input-stream input-stream
      :eof-error-p eof-error-p
      :eof-value eof-value
      :recursive-p recursive-p
      :prompt prompt))

(defun read-filename (&key (prompt *default-prompt*) allow-nonexistent string)
  "Read a file name."
  (let (filename editor)
    (loop :do
       (tt-erase-to-eol)		; umm
       (setf (values filename editor)
	     (rl :prompt prompt
		 :completion-func #'complete-filename
		 :context :read-filename
		 :accept-does-newline nil
		 :string string
		 :editor editor))
       (tt-erase-to-eol)		; umm
       :until (or allow-nonexistent (probe-file filename))
       :do (tmp-message editor "File not found."))
    filename))

(defun read-choice (list &key (prompt *default-prompt*))
  "Read a choice from a list."
  (let (item editor)
    (loop :do
       (setf (values item editor)
	     (rl :prompt prompt
		 :completion-func (list-completion-function list)
		 :context :read-choice
		 :accept-does-newline nil
		 :editor editor))
       :until (position item list :key #'princ-to-string :test #'equal)
       :do (tmp-message editor "~a is not a valid choice." item))
    item))

(defmacro edit-value (place &key prompt (value nil value-supplied-p))
  "Edit the value of something."
  `(setf ,place (read-from-string
		 (rl :prompt (or ,prompt (s+ (princ-to-string ',place) " = "))
		     :string (write-to-string
			      (if ,value-supplied-p
				  ,value
				  ,place)
			      :escape t :readably t)))))

;; EOF
