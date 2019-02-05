;;;
;;; tiny-repl.lisp - A poor little REPL that works with RL.
;;;

;;; TODO:
;;;   - mutli-line
;;;     - make multi-line statements 1 history entry
;;;     - make enter just go to the next line in RL
;;;       - Meta-M for really accept-line

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defpackage "TINY-REPL"
  (:use :common-lisp :terminal :rl :keymap :dlib :dlib-misc)
  (:documentation "A tiny REPL replacement that works with RL.")
  (:export
   #:tiny-repl
   #:*repl-level*
   #:snarky-interceptor
   #:read-arg
  ))
(in-package "TINY-REPL")

(defparameter *real-eof-symbol* 'repl-real-eof)
(defparameter *continue-symbol* 'repl-continue)
(defparameter *empty-symbol* 'repl-empty)
(defparameter *error-symbol* 'repl-error)
(defparameter *quit-symbol* 'repl-quit)
(defparameter *exit-symbol* 'repl-exit)

(defvar *repl-level* -1
  "How many recursive command loops are active.")

(defun repl-output-prompt (e &optional (p nil prompt-supplied))
  "Output the prompt. The prompt will look something like:
(lisp-implementation-type) \":\"
*package* (if not :cl-user) \":\"
*repl-*level* \">\""
   ;; ^^ docstring really fucks over emacs (still...) ^^
   ;; Emacs can finally handle it now (as of 24.4.1)
  (declare (ignore e))
  (let ((pkg (if (not (eq *package* (find-package :cl-user)))
		 (shortest-package-nick)
		 nil)))
    (format t "~a~@[:~a~]~:[~*~;:~d~]~a"
	       #+(and ccl 32-bit-target) "CCL-32"
	       #+(and ccl 64-bit-target) "CCL-64"
	       #+(and ccl (not (or 32-bit-target 64-bit-target))) "CCL"
	       #-ccl *lisp-implementation-nickname*
	       pkg
	       (> *repl-level* 0) *repl-level*
	       (if (and prompt-supplied p) p *default-prompt*))))
    ;; (rl::editor-write-string
    ;;  e (format nil "~a~@[:~a~]~:[~*~;:~d~]~a"
    ;; 	       #+(and ccl 32-bit-target) "CCL-32"
    ;; 	       #+(and ccl 64-bit-target) "CCL-64"
    ;; 	       #+(and ccl (not (or 32-bit-target 64-bit-target))) "CCL"
    ;; 	       #-ccl *lisp-implementation-nickname*
    ;; 	       pkg
    ;; 	       (> *repl-level* 0) *repl-level*
    ;; 	       (if prompt-supplied p *default-prompt*)))))

(defstruct repl-state
  "Internal state of the REPL. Slots are:
  editor	  The current RL editor. An instance of RL:LINE-EDITOR.
  interceptor	  A function of two arguments, a value to be intercepted and
                  copy of this REPL-STATE.
  prompt-func     A propmpt function for RL.
  more		  If non-nil, a string of more input.
  terminal        A terminal to use.
  terminal-name   Name of a terminal device or nil.
  terminal-class  A terminal class symbol.
  keymap          Custom keymap for RL or nil.
  output          Stream to print output on or nil for the default.
  got-error	  A boolean which is true if we got an error.
  error-count     A fixnum which keeps the count of errors we have gotten.
  debug           A boolean which is true to enter the debugger on errors.
"
  editor
  interceptor
  prompt-func
  prompt-string
  more
  terminal
  terminal-name
  terminal-class
  keymap
  output
  (got-error	nil	:type boolean)
  (error-count	0	:type fixnum)
  (debug	nil	:type boolean))

(defvar +newline-string+ (string #\newline)
  "So we don't have to keep making one.")

(defun read-arg (state)
  "For interceptors to get arguments. First value is the argument.
 Second value is true if we got an EOF."
  (with-slots (more) state
    (multiple-value-bind (val pos)
	(ignore-errors (read-from-string more nil 'repl-continue))
      (if (or (eql val 'repl-continue)
	      (not (and more (stringp more))))
	(progn
	  (setf more nil)
	  (values nil t))
	(progn
	  (setf more (subseq more pos))
	  (values val nil))))))

;; @@@ better interface for custom interceptors: make-interceptor ?

(defun snarky-interceptor (value state)
  (flet ((matches (s string)
	   (and (symbolp s)
		(equal 0 (search (symbol-name s) string :test #'equalp)))))
    (with-slots (output) state
      (cond
	;; Parenless function call!
	((and (symbolp value) (fboundp value))
	 (let ((args (loop :with a :and eof
			:do
			(setf (values a eof) (read-arg state))
			:while (not eof)
			:collect a)))
;;;	   (format t "(eval ~s)~%" `(,value ,@args))
	   (format output "~&~s~%" (eval `(,value ,@args)))
	   (finish-output output)
	   t))
	((matches value "Help")
	 (let ((cols (terminal-window-columns
		      (rl:line-editor-terminal
		       (repl-state-editor state)))))
	   (dlib-misc:justify-text (format nil "~
Hi. ~@? If you're weren't expecting a Lisp REPL, just type \"quit\" now. ~
Otherwise, you might be interested to know that you are using Nibby's ~
TINY-REPL. If you want to get back to the normal REPL you can probably type ~
\".\" (a period by itself). You can use some Emacs-like commands to edit the ~
command line.~%"
#+clisp "You are probably using CLisp and typed :h expecting some help."
#-clisp "You probably typed :h by accident, or may even be expecting some help."
) :cols cols :stream output)
	   (format output "
Some notable keys are:
 <Tab>        Complete lisp symbol.
 ?            List lisp symbol completions.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line.
 <Control-N>  Next history line.
 <Control-Q>  Quote next character, like if you want to really type a \"?\".
 ~@?
" (if (find-package :lish) "~
 <F9>         Switch back and forth to LISH, which is a lispy/posixy shell."
		    ""))
	   (format output "
The REPL also has a few commands:
  History      - list the command history.
  IP <package> - Abbreviation for (in-package <package>)
  UP           - Abbreviation for (in-package :cl-user)
  Help         - You are looking at it.~%"))
	 t)
	((matches value "History")
	 (rl:show-history :repl)
	 t)
	((or (matches value "IP") (matches value "IN"))
	 ;; Since this doesn't work: (in-package (read-arg state))
	 ;; let's just hope this does enough of the same thing.
	 (setf *package* (find-package (read-arg state)))
	 t)
	((matches value "UP")
	 (in-package :cl-user)
	 t)))))

(defvar *default-interceptor*
  #'snarky-interceptor
  "Help.")

(defun confirm-quit (state level)
  (with-slots (output) state
    (or (>= level 1)
	(confirm "quit the REPL"))))

(define-condition repl-read-continue (simple-condition)
  ()
  (:report "I hope you never see this."))

(defun repl-read (state)
  (with-slots (editor debug prompt-func prompt-string got-error more
	       terminal terminal-name terminal-class keymap) state
    (let ((result nil)
	  (pre-str nil)
	  (str nil))
      (flet ((call-rl (prompt)
	       (rl :eof-value 'repl-real-eof
		   :quit-value 'repl-quit
		   :editor editor
		   :keymap keymap
		   :terminal terminal
		   :terminal-name terminal-name
		   :terminal-class terminal-class
		   :context :repl
		   :prompt (or (and (stringp prompt) prompt) *default-prompt*)
		   :output-prompt-func (and (or (symbolp prompt)
						(functionp prompt))
					    prompt))))
      (loop
	 :do
	 (dbugf :repl "editor before = ~a~%" editor)
	 (cond
	   (more
	    (setf str more)
	    (setf more nil)
	    (dbugf :repl "Using MORE~%"))
	   (pre-str
	    (setf (values str editor) (call-rl "")))
	   (t
	    (setf (values str editor)
		  (call-rl (or prompt-string
			       prompt-func
			       'repl-output-prompt)))))
	 (dbugf :repl "str = ~s~%editor after = ~a~%" str editor)
	 (setf result
	       (cond
		 ((and (stringp str) (zerop (length str))) 'repl-empty)
		 ((and (stringp str) (equal "." str)) 'repl-exit)
		 ((equal str 'repl-real-eof)
		  (dbugf :repl "You got a 'repl-real-eof~%")
		  'repl-real-eof)
		 ((equal str 'repl-quit) 'repl-quit)
		 (t
		  (dbugf :repl "Before read: pre-str = ~w str = ~w~%"
			 pre-str str)
		  (let ((cat (or (and pre-str (s+ pre-str str)) str)))
		    (multiple-value-bind (obj pos)
			(read-from-string cat nil 'repl-continue)
		      ;; Make MORE be the rest of the string, if any.
		      (setf more (if (and (not (eql obj 'repl-continue))
					  (< pos (length cat)))
				     (subseq cat pos)
				     nil))
		      obj)))))
	 :while (eq result 'repl-continue)
	 :do
	 (setf pre-str
	       (if (stringp pre-str)
		   (s+ pre-str str #\newline)
		   (s+ str #\newline)))
	 (dbugf :repl "set pre-str = ~w~%" pre-str)
	 (dbugf :repl "Do CONTIUE~%"))
      result))))

(defun repple-stepper (c)
  (when (find-package :deblarg)
    (funcall (intern "STEPPER" (find-package :deblarg)) c)))

;; Eval and print
(defun repl-eval (form state)
  (with-slots (got-error error-count interceptor debug output) state
    (when (or (eq form 'repl-empty) (eq form 'repl-error))
      (dbugf :repl "Do Nothing~%")
      (return-from repl-eval nil))
    (dbugf :repl "Do Something~%")
    ;; If there is an interceptor, let it have a crack at it.
    ;; If interceptor returns nil, it didn't intercept and we should go on.
    ;; There might be some more arguments for the interceptor which it
    ;; can read out of the more string, if it wants to.
    (when (or (not interceptor)
	      (and interceptor
		   (and (not (funcall interceptor form state))
			(not (and (dbugf
				   :repl "Interceptor returned NIL.~%") nil)))))
      (progn
	(setf - form)
	(dbugf :repl "About to eval ~s~%" form)
	(unwind-protect
	     (let* ((vals (multiple-value-list (eval form)))
		    (*print-circle* t))
	       ;; These ***shmoozy-vars*** are in the standard,
	       ;; so we'd better support them.
	       (setf /// //
		     // /
		     / vals
		     *** **
		     ** *
		     * (car vals))
	       (loop :with len = (length vals) :and i = 0
		  :for v :in vals
		  :do
		  ;; This is the "P" in REPL
		  (format output "~&~s" v)
		  (when (and (> len 1) (< i (- len 1)))
		    ;; It's kind of a convention for values.
		    (format output " ;~%"))
		  (incf i))
	       (terpri output))
	  (setf +++ ++
		++ +
		+ -))))))

(defmacro with-error-handling ((state) &body body)
  (with-unique-names (thunk error-handler)
    `(flet ((,thunk () ,@body)
	    (,error-handler (c)
	      (dbugf :repl "Handler bind~%")
	      (setf (repl-state-got-error ,state) t)
	      (if (repl-state-debug ,state)
		  (invoke-debugger c)
		  (format (repl-state-output ,state)
			  "Condition: ~a~%" c))))
       (if (repl-state-debug ,state)
	   (handler-bind
	       (#+sbcl (sb-ext::step-condition 'repple-stepper)
		(serious-condition #',error-handler))
	     (,thunk))
	   (handler-bind
	       ((serious-condition #',error-handler))
	     (,thunk))))))

(defmacro with-repl-terminal ((terminal) &body body)
  "If terminal isn't set Evaluate BODY with a *terminal* set up.
TERMINAL-NAME and TERMINAL-TYPE should be in the environment."
  (with-unique-names (thunk)
    `(flet ((,thunk () ,@body))
       (if ,terminal
	   (,thunk)
	   (with-terminal (terminal-type *terminal*
					 :device-name terminal-name
					 :start-at-current-line t)
	     (,thunk))))))

(defvar *default-terminal-type* :crunch)

(defun tiny-repl (&key prompt-func prompt-string no-announce keymap
		    terminal terminal-name
		    (terminal-type (pick-a-terminal-type))
		    ;;(terminal-type *default-terminal-type*)
		    (output *standard-output*)
		    (interceptor *default-interceptor*) (debug t))
  "Keep reading and evaluating lisp, with line editing. Return true if we want
to quit everything. Arguments are:
 PROMPT-FUNC    -- A RL prompt function, which is called with a with
                   an instance of RL:LINE-EDITOR and a prompt string.
 PROMPT-STRING  -- 
 NO-ANNOUNCE    -- True to supress the announcement on starting.
 TERMINAL       -- An already created terminal to use.
 TERMINAL-NAME  -- Name of a system terminal device to read from.
 TERMINAL-TYPE  -- Type of terminal to read from. Defaults from
                   pick-a-terminal-type and so from *default-terminal-type*.
 KEYMAP         -- A custom keymap to use for RL.
 OUTPUT         -- Stream to print output on.
 INTERCEPTOR    -- Function that's called with an object to be evaluated and a
                   TINY-REPL:REPL-STATE. Allows interception of sepcial objects
                   before they're evaluated, usually used for commands. The
                   interceptor should return true if does not want evaluation
                   to happen. Defaults to *DEFAULT-INTERCEPTOR*.
 DEBUG          -- True to install DeBLARG as the debugger. Default is T.
"
  ;; Annouce the implemtation and version on systems that don't always do it.
  #-sbcl (when (not no-announce)
	   (format output "~a ~a~%"
		   (lisp-implementation-type)
		   (lisp-implementation-version)))
  #+sbcl (declare (ignore no-announce))
  (let ((state (make-repl-state
		:debug          debug
		:interceptor    interceptor
		:prompt-func    prompt-func
		:prompt-string  prompt-string
		:keymap         keymap
		:terminal       terminal
		:terminal-class (find-terminal-class-for-type terminal-type)
		:output         output))
	(result nil)
	(want-to-quit nil)
	(old-debugger-hook *debugger-hook*)
	(start-level (incf *repl-level*))
	(*history-context* :repl))
    (when (not theme:*theme*)
      (setf theme:*theme* (theme:default-theme)))

    (with-repl-terminal (terminal)
      (setf (tt-input-mode) :line)
      ;; Activate the debugger if it's loaded.
      (when (and debug (find-package :deblarg))
	(funcall (intern "ACTIVATE" (find-package :deblarg))))
      (unwind-protect
	   (tagbody
	    TOP
	      (restart-case
		  (with-error-handling (state)
		    (loop :do
		       (setf (repl-state-got-error state) nil)
		       (setf result (repl-read state)) ;;; READ
		       :until (or (equal result 'repl-real-eof)
				  (equal result 'repl-exit))
		       :do
		       (dbugf :repl "~s (~a) ~s~%"
			      result (type-of result)
			      (eq result 'repl-empty))
		       (if (equal result 'repl-quit)
			   (when (confirm-quit state *repl-level*)
			     (return result))
			   (repl-eval result state)) ;;; EVAL
		       :finally (return result)))
		(abort ()
		  :report
		  (lambda (stream)
		    (if (= start-level 0)
			(format stream "Return to TOP command loop.")
			(format stream "Return to command loop ~d."
				start-level)))
		  (setf result nil)))
	      (dbugf :repl "main result = ~s~%" result)
	      (when (not result) (go TOP)))

	;; Let's hope that this will clear an EOF on *standard-input*
	(clear-input *standard-input*)

	(when (> (repl-state-error-count state) 8)
	  (format output "Quit due to too many errors.~%"))

	(setf want-to-quit
	      (cond
		((eq result 'repl-real-eof) (format output "*EOF*~%") t)
		((eq result 'repl-quit) (format output "*Quit*~%")    t)
		((eq result 'repl-exit) (format output "*Exit*~%")    nil)
		(t (dbugf :repl "~s ~a~%" result (type-of result))    t)))
	(setf *debugger-hook* old-debugger-hook)
	(decf *repl-level*))
      want-to-quit)))

;; EOF
