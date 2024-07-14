;;;
;;; rl.lisp - An input line editor.
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

(defkeymap *normal-keymap* (:default-binding 'self-insert-command)
  `(
    ;; Movement
    (,(ctrl #\B)		. backward-char)
    (,(ctrl #\F)		. forward-char-or-accept-suggestion)
    (,(ctrl #\A)		. beginning-of-line)
    (,(ctrl #\E)		. end-of-line)
    (,(meta-char #\a)		. beginning-of-buffer)
    (,(meta-char #\e)		. end-of-buffer)
    (,(ctrl #\P)		. previous-line-or-history)
    (,(ctrl #\N)		. next-line-or-history)
    (,(meta-char #\p)		. previous-history)
    (,(meta-char #\n)		. next-history)
    (,(meta-char #\b)		. backward-word)
    (,(meta-char #\f)		. forward-word-or-accept-suggestion)
    (:c-left			. backward-word)
    (:c-right			. forward-word-or-accept-suggestion)
    (:s-left			. mark-backward-char)
    (:s-right			. mark-forward-char)
    (:s-c-left			. mark-backward-word)
    (:s-c-right			. mark-forward-word)
    (,(meta-char #\<)		. beginning-of-history)
    (,(meta-char #\>)		. end-of-history)
    (,(meta-char #\v)		. previous-page)
    (,(ctrl #\v)		. next-page)
    (:page-up			. previous-page)
    (:page-down			. next-page)
    (:c-home			. message-home)
    (:c-end			. message-end)

    ;; Editing
    (#\return			. accept-line)
    (#\newline			. newline)
    (,(meta-char #\m)		. newline)
    (#\backspace		. delete-backward-char)
    (#\rubout			. delete-backward-char)
    (:s-delete			. kill-region)
    (,(meta-char (ctrl #\w))    . kill-region)
    (,(meta-char (ctrl #\q))    . re-indent)
    (,(ctrl #\D)		. delete-char-or-exit)
    (,(ctrl #\W)		. backward-kill-word)
    (,(ctrl #\K)		. kill-line)
    (,(ctrl #\@)		. set-mark)
    (,(ctrl #\Y)		. yank)
    (,(ctrl #\U)		. backward-kill-line)
    (,(ctrl #\O)		. undo-command)
    (,(ctrl #\_)		. undo-command)
    (,(ctrl #\T)		. transpose-characters)
    (,(meta-char #\d)		. kill-word)
    (,(meta-char #\rubout)	. backward-kill-word)
    (,(meta-char #\backspace)	. backward-kill-word)
    (,(meta-char (ctrl #\h))	. backward-kill-word)
    (,(meta-char #\u)		. upcase-word)
    (,(meta-char #\l)		. downcase-word)
    (,(meta-char #\c)		. capitalize-word)
    (,(meta-char #\w)		. copy-region)
    (,(meta-char #\\)		. delete-horizontal-space)
    (,(meta-char #\/)		. add-cursor-on-next-line)
    (,(meta-char #\.)		. insert-last-argument)
    ;; (,(meta-char #\.)		. just-one-context)

    ;; Completion
    (#\tab			. complete)
    (#\?			. show-completions)
    ;; (,(meta-char #\/)		. complete-filename-command)
     (,(meta-char #\')		. complete-filename-command)
    (,(meta-char #\?)		. show-filename-completions-command)
    (,(meta-char #\I)		. dictionary-complete)
    (,(meta-char #\$)		. dictionary-complete)
    (,(meta-char #\i)		. complete-history-command)

    ;; Misc
    (,(ctrl #\L)		. redraw)
    ;;(,(ctrl #\G)		. abort-command)
    (,(ctrl #\G)		. reset-stuff)
    (,(ctrl #\S)		. isearch-forward)
    (,(ctrl #\R)		. isearch-backward)
    (,(ctrl #\Q)		. quoted-insert)
    (,(meta-char #\q)		. park-it)
    (,(meta-char #\escape)	. eval-expression)
    (,(meta-char #\])		. insert-expression)

    ;; key binding
    (,(meta-char #\=)		. describe-key-briefly)
    (,(meta-char #\+)		. set-key-command)

    ;; dorky temporary
    (,(meta-char #\t)		. un-studly-cap)
    (,(meta-char #\")		. quote-region)
    
    ;; Other keymaps
    (#\escape			. *escape-keymap*)
    (,(ctrl #\X)		. *ctlx-keymap*)
    ))

;; These ^X commands are quite impoverished.
(defkeymap *ctlx-keymap* ()
  `(
    ;; (,(ctrl #\F) (edit-function))
    ;; (,(ctrl #\Q) (toggle-read-only))
    ;; (,(ctrl #\S) (save-history))
    ;; (,(ctrl #\Z) (suspend))
    ;; (#\h (hyperspec-lookup))
    ;; (#\! (shell-command))
    ;; (#\( (start-macro))
    ;; (#\) (end-macro))
    (#\i		. insert-file)
    (#\w		. save-line-command)
    (#\9		. unipose-command)
    (#\7		. char-picker-command)
    (#\l		. pop-to-lish)
    (#\=		. what-cursor-position)
    (#\T	        . toggle-debugging) ; @@@ temporary?
    (#\m	        . toggle-mode-line)
    (#\(		. start-recording)
    (#\)		. stop-recording)
    (#\e		. replay-recording)
    (,(ctrl #\C)	. exit-editor)
    (,(ctrl #\X)	. exchange-point-and-mark)))
;  :default-binding #| (beep e "C-x ~a is unbound." command |#

(defkeymap *special-keymap* ()
  `(
    (:left            . backward-char)
    (:right           . forward-char-or-accept-suggestion)
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

(defkeymap *vi-insert-mode-keymap* (:default-binding 'self-insert-command)
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
    (,(ctrl #\L)		. redraw)
    ;; (,(ctrl #\R)		. @@@ redisplay? @@@)
    (,(ctrl #\Q)		. quoted-insert)

    ;; Other keymaps
    (#\escape			. set-vi-command-mode)
    (,(ctrl #\C)		. set-vi-command-mode)
    (,(ctrl #\O)		. vi-do-command)
    (,(ctrl #\X)		. *ctlx-keymap*)
    ))

;; Make the stuff in the special keymap appear in the vi insert keymap too.
(add-keymap *special-keymap* *vi-insert-mode-keymap*)

(defkeymap *vi-command-mode-keymap* (:default-binding 'self-insert-command)
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
    (#\B			. backward-syntax-word)	;
    (#\W			. forward-syntax-word)	;
    (#\w			. forward-word-or-accept-suggestion) ;
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
    (#\r			. vi-replace)		; *
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
    (,(ctrl #\L)		. redraw)		;
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
    ))

(add-keymap *special-keymap* *vi-command-mode-keymap*)

(defvar *terminal-name* nil
  "Device name of the terminal to use for input.")

(defvar *entry-hook* nil
  "Functions to run before reading a line.")

(defvar *exit-hook* nil
  "Functions to run after reading a line.")

(defvar *post-command-hook* nil
  "Functions to run after a command has been processed.")

(defvar *default-terminal-type* :crunch)

;; The main entry point

(defun rl (&key
	     (input-stream *standard-input*)
	     (eof-error-p t)
	     eof-value
	     quit-value
	     recursive-p
	     (prompt *default-prompt*)
	     output-prompt-func
	     right-prompt
	     (completion-func #'complete-symbol)
	     string
	     input-callback
	     output-callback
	     debug
	     editor
	     local-keymap
	     keymap
	     terminal
	     (terminal-name *terminal-name*)
	     ;; (terminal-class (find-terminal-class-for-type
	     ;; 		      *default-terminal-type*)) ; (pick-a-terminal-type)
	     (terminal-class (find-terminal-class-for-type
			      (pick-a-terminal-type)))
	     (accept-does-newline t)
	     partial-line-indicator
	     re-edit
	     (history-context :tiny))		; remnant
  "Read a line from the terminal, with line editing and completion.
Return the string read and the line-editor instance created.
Keyword arguments: 
  eof-error-p          True to signal an error on end of file. [t]
  eof-value            Value to return on end of file. 
  quit-value           Value to return if the user quit.
  prompt                String to prompt with. [*default-prompt*]
  output-prompt-func   Function to print out a prompt. Called with the
                       ‘line-editor’ instance and a prompt string.
  right-prompt         String to output on the right side of the input line.
  completion-func      Completion function to use. See the completion package
                       for details. [#'complete-symbol]
  string               A string to set as the initial contents of the buffer.
  input-callback       A function to call on input. Called with the editor
                       object and the input.
  output-callback      A function to call after the display is updated, but
                       before input. Called with the editor object.
  editor               ‘line-editor’ instance to use.
  local-keymap         A ‘local-keymap’ to use. For when you want to add your
                       own customized key bindings.
  keymap               A ‘keymap’ to use. If you want to completely replace all
                       the key bindings by your own. This defaults to a list of
                       [(local-keymap *normal-keymap*)].
  terminal             An already started terminal to use.
  terminal-name        Name of a terminal device to use. [*terminal-name*]
  accept-does-newline  True if accept-line outputs a newline. [t]
  partial-line-indicator
                       A character to output if starting on a partial line.
  re-edit              True to re-edit the previous line as if accept was a
                       newline.
  hisory-context       Symbol or string which defines the context for keeping
                       history.
" ; There must be a better way to format docstrings.
  (declare (ignore recursive-p))
  (history-init history-context)

  ;; Initialize the buffer
  (let* ((e (or editor (make-instance
			'line-editor
			;;:point		    	#(0)
			:prompt-string	    	prompt
			:prompt-func	    	output-prompt-func
			:right-prompt		right-prompt
			:completion-func    	completion-func
			:history-context        history-context
			:input-callback	    	input-callback
			:output-callback    	output-callback
			:debugging	    	debug
			:quit-value		quit-value
			:local-keymap	    	local-keymap
			:keymap			keymap
			:accept-does-newline	accept-does-newline
			:partial-line-indicator	partial-line-indicator
			:terminal		terminal
			:terminal-device-name	terminal-name
			:terminal-class	    	terminal-class)))
	 (*terminal* (line-editor-terminal e))
	 ;;(*standard-output* *terminal*)
	 ;;(*standard-input* *terminal*)
	 (*completion-count* 0)
	 (*history-context* history-context)
	 terminal-state)
    (when quit-value
      (setf (slot-value e 'quit-value) quit-value))

    #+ccl (setf ccl::*auto-flush-streams* nil)
    #+ccl (ccl::%remove-periodic-task 'ccl::auto-flush-interactive-streams)
    (setf terminal-state (terminal-start (line-editor-terminal e)))

    ;; We have to do this horrible thing before the terminal potentially queries
    ;; the row.
    (when (not re-edit)
      (pre-read e))

    (setf (terminal-input-mode (line-editor-terminal e)) :char)

    (cond
      (re-edit
       (setf (inator-quit-flag e) nil)
       (use-first-context (e)
	 (newline e)))
      (t
       (when editor
	 (freshen editor))

       ;;(setf (fill-pointer (buf e)) (inator-point e))
       (setf (fill-pointer (buf e)) 0)

       ;; Restore a pushed buffer.
       (when (pushed-buffers e)
	 (pop-buffer e))

       ;; Set the start line for crunch terminals. We use oelt here so we
       ;; don't even have to depend on terminal-crunch being loaded.
       ;; (when (typep (line-editor-terminal e)
       ;; 		    (find-terminal-class-for-type :crunch))
       ;; 	 (setf (start-row e)
       ;; 	       (terminal-get-cursor-position
       ;; 		(oelt (line-editor-terminal e) :wrapped-terminal))
       ;; 	       (oelt (line-editor-terminal e) :start-line) (start-row e)))

       ;; Add the new line we're working on.
       ;; (history-add nil)		; @@@@@@@ This is bad.
       ;; (history-next)

       (run-hooks *entry-hook* e)

       ;; Set the prompt
       (setf (prompt-string e) prompt
	     (prompt-func e) output-prompt-func
	     (right-prompt e) right-prompt)
       (when string
	 ;; @@@ It might be nice to get a better error if string is not
	 ;; insert-able, but that might limit how we can extend the buffer.
	 (without-undo (e)
	   (let ((to-insert
		   (typecase string
		     (fat-string string)
		     (t (make-fat-string :string string)))))
	     (buffer-insert e 0 to-insert 0)
	     (set-all-points e (olength to-insert)))))))

    ;; If the terminal is in line mode even after we set it to :char mode,
    ;; our whole thing is kind of moot, so just fall back to reading from the
    ;; terminal driver, so we work on dumb terminals.
    (when #-windows (eq (tt-input-mode) :line) #+windows nil ;; @@@ WORKAROUND
      (update-display e)
      (finish-output *terminal*)
      (let ((line (read-line *terminal*)))
	(unwind-protect
	     (progn
	       (history-line-open)
	       (accept-line e :string line))
	  (history-line-close))
	(return-from rl (values line e))))

    ;; Command loop
    (with-slots (quit-flag exit-flag quit-value command buf point last-command
		 terminal screen-relative-row screen-col debugging
		 temporary-message keep-message filter-hook region-active
		 buf-str keep-region-active auto-suggest-p) e
      ;; (multiple-value-setq (screen-relative-row screen-col)
      ;; 	(terminal-get-cursor-position *terminal*))
      (let ((result nil))
	(unwind-protect
	     (progn
	       ;; Make sure the terminal has accurate size and cursor position.
	       (terminal-reinitialize *terminal*)
	       ;; (tt-fresh-line)
	       ;; (when (not re-edit)
	       ;; 	 (pre-read e))
	       (history-line-open)
	       (loop :do
		  (finish-output)
		  ;;(describe buf *debug-io*)
		  (when auto-suggest-p
		    (auto-suggest-command e))
		  (when debugging
		    (message-prepend e "~d ~d [~d x ~d] ~a ~w~%"
			     (screen-col e) (screen-relative-row e)
			     (terminal-window-columns terminal)
			     (terminal-window-rows terminal)
			     ;; (when (typep *terminal*
			     ;; 	     'terminal-crunch:terminal-crunch)
			     ;;   (terminal-crunch::start-line *terminal*))
			     ;;point command)
			     command (inator-contexts e))
		    ;; (setf keep-message t)
		    )
		  (update-display e)
		  (when debugging
		    (show-message-log e))
		  (tt-finish-output)
		  (unhighlight-matching-parentheses e)
		  ;; @@ Is this really where I want it?
		  (when (line-editor-output-callback e)
		    (tt-save-cursor)
		    (tt-cursor-off)
		    (unwind-protect
			 (funcall (line-editor-output-callback e) e)
		      (tt-cursor-on)
		      (tt-restore-cursor)))
		  ;;(setf command (await-event e))
		  (setf (last-event e) (if (queued-input e)
					   (pop (queued-input e))
					   (await-event e)))
		  (log-message e "command ~s" command)
		  ;; Erase the temporary message.
		  (when (and temporary-message (not keep-message))
		    ;; (setf temporary-message nil)
		    (clear-completions e))
		  (setf keep-region-active nil)
		  (if (equal command '(nil))
		      (if eof-error-p
			  (error (make-condition 'end-of-file
						 :stream input-stream))
			  (setf result eof-value))
		      (progn
			(setf (did-complete e) nil)
			;;(perform-key e command (inator-keymap e))
			(process-event e (last-event e) (inator-keymap e))
			(setf (last-command-was-completion e) (did-complete e))
			(when (not (last-command-was-completion e))
			  (set-completion-count e 0))
			(when exit-flag
			  (setf result quit-value))
			;; @@@ perhaps this should be done by a hook?
			(run-hooks *post-command-hook* e)
			(when (not quit-flag)
			  (highlight-matching-parentheses e))))
		  (save-lossage (inator-event-sequence e) command)
		  (setf last-command command)
		  (run-hooks filter-hook e)
		  ;; (when (need-to-recolor e)
		  ;;   (recolor-line e))
		  ;; Turn off the region if it's not flagged to be kept.
		  ;;(dbugf :rl "region-active = ~s~%keep-region-active = ~s~%")
		  (when (and region-active (not keep-region-active))
		    (setf region-active nil))
		  :while (not quit-flag)))
	  (block nil
	    (clear-completions e)
	    (setf (first-point e) (fill-pointer buf))
	    (update-display e)
	    (when accept-does-newline
	      ;; (tt-write-string buf-str :start point)
	      (tt-write-char #\newline)
	      ;;(tt-fresh-line)
	      ;; (tt-write-char #\return)
	      ;; (tt-finish-output)
	      ;; (write-char #\newline)
	      )
	    (tt-finish-output)
	    (run-hooks *exit-hook* e)
	    (terminal-end terminal terminal-state)
	    ;; Make sure the NIL history item is gone.
	    (history-line-close)))
	(values (if (or result exit-flag)
		    result
		    (fatchar-string-to-string buf))
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

(defun list-acceptance-function (list test &key message)
  "Return a function which takes an editor accepts an item from ‘list’
where ‘test’ is true. If ‘list’ is function, call it with the item so far to
generate the list. ‘message’ will be printed when the item fails the test."
  (lambda (e)
    (let ((item (buffer-string (buf e))))
      (if (funcall test item list)
	  (accept-line e)
	  (let ((output-string
		  (with-output-to-fat-string (str)
		    (print-columns
		     (typecase list
		       (function (funcall list item))
		       (t list))
		     :columns (terminal-window-columns *terminal*)
		     :smush t
		     :format-char "/fatchar-io:print-string/"
		     :stream str))))
	    (message e "~s ~a. Please pick one of:~%~a"
		     (if (zerop (olength item))
			 "Nothing"
			 item)
		     (or message "is not a valid choice")
		     output-string))))))

(defsingle abort-editor (e)
  "Stop editing and return NIL."
  (with-slots (quit-flag exit-flag quit-value) e
    (setf quit-flag t
	  exit-flag t
	  quit-value nil)))

(defun make-read-local-keymap ()
  (make-instance 'keymap :map `((,(ctrl #\g) . abort-editor))))

(defun read-filename (&key (prompt *default-prompt*) allow-nonexistent string)
  "Read a file name."
  (let ((keymap
	  (substitute-key-definition
	   (list-acceptance-function
	    (lambda (item)
	      (glob:glob (s+ item "*")))
	    (lambda (item list)
	      (declare (ignore list))
	      (or allow-nonexistent (nos:file-exists (glob:expand-tilde item))))
	    :message "isn't an existing file")
	   'accept-line
	   (make-read-local-keymap)
	   :old-keymap *normal-keymap*)))
    (values
     (rl :prompt prompt
	 :completion-func #'complete-filename
	 :history-context :read-filename
	 :accept-does-newline nil
	 :string string
	 :local-keymap keymap))))

(defun read-choice (list &key (prompt *default-prompt*) (test #'equal))
  "Read a choice from a list."
  (let ((keymap
	  (substitute-key-definition
	   (list-acceptance-function list
	     (lambda (item list)
	       (position item list :key #'princ-to-string :test test)))
	   'accept-line
	   (make-read-local-keymap)
	   :old-keymap *normal-keymap*)))
    (values
     (rl :prompt prompt
	 :completion-func (list-completion-function list)
	 :history-context :read-choice
	 :accept-does-newline nil
	 :local-keymap keymap))))

(defmacro edit-value (place &key prompt (value nil value-supplied-p))
  "Edit the value of something."
  `(setf ,place (read-from-string
		 (rl :prompt (or ,prompt (s+ (princ-to-string ',place) " = "))
		     :string (write-to-string
			      (if ,value-supplied-p
				  ,value
				  ,place)
			      :escape t :readably t)))))

;; End
