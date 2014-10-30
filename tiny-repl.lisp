;;;
;;; tiny-repl.lisp - A poor little REPL that works with tiny-rl.
;;;

;;; This used to be part of tiny-rl, but it's split out now.
;;; It's actually quite broken and incomplete.
;;; It includes a debugger which is very very shabby.
;;; If you like self deprecating software, this is for you.

;;; TODO:
;;;   - pasteability?
;;;     - make multi-line statements 1 history entry
;;;     - some way to turn off completion (tab & ?) for pasting

;;; OR you could just say:
;;; (loop (print (eval (read-from-string (tiny-rl:tiny-rl) nil nil))))

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; Is this the proper way/place to do this?
;#+sbcl (require 'sb-introspect)

(defpackage "TINY-REPL"
  (:use :common-lisp :tiny-rl :dlib :termios)
  (:documentation
   "A tiny REPL replacement to be used with tiny-rl.")
  (:export
   #:tiny-repl
   #:*repl-level*
   #:snarky-interceptor
   #:read-arg
  )
)
(in-package "TINY-REPL")

;(defvar real-eof-symbol (gensym "reof"))
;(defvar continue-symbol (gensym "reof"))
;(defvar empty-symbol (gensym "reof"))
(defparameter *real-eof-symbol* :Z-REAL-EOF)
(defparameter *continue-symbol* :Z-CONTINUE)
(defparameter *empty-symbol* :Z-EMPTY)
(defparameter *error-symbol* :Z-ERROR)
(defparameter *quit-symbol* :Z-QUIT)
(defparameter *exit-symbol* :Z-EXIT)

(defvar *repl-level* -1
  "How many recursive command loops are active.")

(defun shortest-package-nick ()
  "Find the shortest nickname."
  (if (package-nicknames *package*)
      (let ((p nil))
	(loop with r = most-positive-fixnum
	      for n in (package-nicknames *package*)
	      do (if (< (length n) r)
		     (setf p n)))
	p)
      (package-name *package*)))

(defun repl-output-prompt (e &optional (p nil prompt-supplied))
  "Output the prompt. The prompt will look something like:
(lisp-implementation-type) \":\"
*package* (if not :cl-user) \":\"
*repl-*level* \">\""
   ;; ^^ docstring really fucks over emacs (still...) ^^
  (let ((pkg (if (not (eq *package* (find-package :cl-user)))
		 (shortest-package-nick)
		 nil)))
    (tiny-rl::editor-write-string
     e (format nil "~a~@[:~a~]~:[~*~;:~d~]~a"
	       #+(and ccl 32-bit-target) "CCL-32"
	       #+(and ccl 64-bit-target) "CCL-64"
	       #+(and ccl (not (or 32-bit-target 64-bit-target))) "CCL"
	       #+cmu "CMU"
	       #+lispworks "LW"
	       #-(or ccl cmu lispworks) (lisp-implementation-type)
	       pkg
	       (> *repl-level* 0) *repl-level*
	       (if prompt-supplied p *default-prompt*)))))

(defstruct repl-state
  "Internal state of the REPL. Slots are:
  editor	  The current TINY-RL editor. An instance of the class
                  TINY-RL:LINE-EDITOR.
  interceptor	  A function of two arguments, a value to be intercepted and
                  copy of this REPL-STATE.
  prompt-func   
  more		  If non-nil, a string of more input.
  terminal-name   Name of a terminal device or nil.
  got-error	  A boolean which is true if we got an error.
  error-count     A fixnum which keeps the count of errors we have gotten.
  debug           A boolean which is true to enter the debugger on errors.
"
  editor
  interceptor
  prompt-func
  more
  terminal-name
  (got-error	nil	:type boolean)
  (error-count	0	:type fixnum)
  (debug	nil	:type boolean))

;(defvar *repl-debug-messages* nil)
; This should be a macro when things are working well enough.
;(defun dbg (fmt &rest args)
;  (when *repl-debug-messages*
;    (apply #'format t fmt args)))

;; (define-constant +newline-string+ (string #\newline) ;#.(string #\newline)
;;   "So we don't have to keep making one." #'equal)

(defvar +newline-string+ (string #\newline)
  "So we don't have to keep making one.")

(defun read-arg (state)
  "For interceptors to get arguments. First value is the argument.
 Second value is true if we got an EOF."
  (with-slots (more) state
    (multiple-value-bind (val pos)
	(ignore-errors (read-from-string more nil *continue-symbol*))
      (if (or (eql val *continue-symbol*)
	      (not (and more (stringp more))))
	(progn
	  (setf more nil)
	  (values nil t))
	(progn
	  (setf more (subseq more pos))
	  (values val nil))))))
#|
(defun make-interceptor (commands)
  (lambda (value state)
    (flet ((matches (s string)
	     (and (symbolp s)
		  (equal 0 (search (symbol-name s) string :test #'equalp)))))
      (if (and (symbolp value) (fboundp value))
	  ;; Parenless function call!
	  (let ((args (loop :with a :and eof
			 :do
			 (setf (values a eof) (read-arg state))
			 :while (not eof)
			 :collect a)))
	    #| (format t "(eval ~s)~%" `(,value ,@args)) |#
	    (format t "~&~s~%" (eval `(,value ,@args))) (finish-output)
	    t)
	  (loop :for c :in commands
	     :do
	     (when (matches value "Help")
     (format t "~
Hi. ~@?
If you're weren't expecting a Lisp REPL, just type \"quit\" now. Otherwise,
you might be interested to know that you are using Dan's TINY-REPL. If you want
to get back to the normal REPL you can probably type \".\" (a period by
itself). You can use some Emacs-like commands to edit the command line.

Some notable keys are:
 <Tab>        Complete lisp symbol.
 ?            List lisp symbol completions.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line.
 <Control-N>  Next history line.
 <Control-Q>  Quote next character, like if you want to really type a \"?\".
 ~@?
"
#+clisp "You are probably using CLisp and typed :h expecting some help."
#-clisp "You probably typed :h by accident, or may even be expecting some help."
		(if (find-package :lish) "~
 <F9>         Switch back and forth to LISH, which is a lispy/posixy shell."
		    ""))
     t)
    ((matches value "History")
     (tiny-rl:show-history (tiny-rl::context (repl-state-editor state)))
     t))))
|#

(defun snarky-interceptor (value state)
  (flet ((matches (s string)
	   (and (symbolp s)
		(equal 0 (search (symbol-name s) string :test #'equalp)))))
  (cond
    ;; Parenless function call!
    ((and (symbolp value) (fboundp value))
     (let ((args (loop :with a :and eof
		    :do
		    (setf (values a eof) (read-arg state))
		    :while (not eof)
		    :collect a)))
;       (format t "(eval ~s)~%" `(,value ,@args))
       (format t "~&~s~%" (eval `(,value ,@args))) (finish-output)
       t))
    ((matches value "Help")
     (format t "~
Hi. ~@?
If you're weren't expecting a Lisp REPL, just type \"quit\" now. Otherwise,
you might be interested to know that you are using Dan's TINY-REPL. If you want
to get back to the normal REPL you can probably type \".\" (a period by
itself). You can use some Emacs-like commands to edit the command line.

Some notable keys are:
 <Tab>        Complete lisp symbol.
 ?            List lisp symbol completions.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line.
 <Control-N>  Next history line.
 <Control-Q>  Quote next character, like if you want to really type a \"?\".
 ~@?
"
#+clisp "You are probably using CLisp and typed :h expecting some help."
#-clisp "You probably typed :h by accident, or may even be expecting some help."
		(if (find-package :lish) "~
 <F9>         Switch back and forth to LISH, which is a lispy/posixy shell."
		    ""))
     t)
    ((matches value "History")
     (tiny-rl:show-history (tiny-rl::context (repl-state-editor state)))
     t))))

(defvar *default-interceptor*
  #'snarky-interceptor
  "Help.")

(defun confirm (state level)
  (declare (ignore state))
  (or (>= level 1)
      (progn
	(format t "~%Do you really want to quit the REPL? ")
	(finish-output)
	(let ((l (read-line *standard-input* nil nil)))
	  (or (not l) 			; EOF = confirm (i.e. hit ^D)
	      (and (stringp l) (> (length l) 0) (equalp (aref l 0) #\y)))))))

(define-condition repl-read-continue (simple-condition)
  ()
  (:report "I hope you never see this."))

(defun repl-read (state)
  (with-slots (editor debug prompt-func got-error more terminal-name) state
    (let ((result nil)
	  (pre-str nil)
	  (str nil))
      (loop :while (eq *continue-symbol* (setf result
        (handler-case
         (handler-bind
	     ((end-of-file #'(lambda (c)
			       (declare (ignore c))
			       (dbug "GOT read EOF - Continuing~%")
			       (signal (make-condition 'repl-read-continue))))
; 	      (condition #'(lambda (c)
; 			     (dbug "GOT something else - debug or signal~%")
; 			     ;; ??? Why can't we just use "debug" here?
; 			     (if (repl-state-debug state)
; 				 (invoke-debugger c)
; 				 (signal c))))
	      )
	   (progn
	    (dbug "editor before = ~a~%" editor)
	    (if more
		(progn
		  (setf str more)
		  (setf more nil)
		  (dbug "Using MORE!~%"))
		(if pre-str
		    (setf (values str editor)
			  (tiny-rl :eof-value *real-eof-symbol*
				   :quit-value *quit-symbol*
				   :editor editor
				   :terminal-name terminal-name
				   :context :repl
				   :prompt ""))
		    (progn
		      ;; (try-to-reset-curses)
		      (setf (values str editor)
			    (tiny-rl :eof-value *real-eof-symbol*
				     :quit-value *quit-symbol*
				     :editor editor
				     :terminal-name terminal-name
				     :context :repl
				     :output-prompt-func
				     (if prompt-func
					 prompt-func
					 #'repl-output-prompt))))))
	    (dbug "str = ~a~%editor after = ~a~%" str editor)
	    (cond
	      ((and (stringp str) (equal 0 (length str)))
	       *empty-symbol*)
	      ((and (stringp str) (equal "." str))
	       *exit-symbol*)
	      ((equal str *real-eof-symbol*)
	       (dbug "You got a *real-eof-symbol* !")
	       *real-eof-symbol*)
	      ((equal str *quit-symbol*)
	       *quit-symbol*)
;	      ((eq form *continue-symbol*))
	      (t
	       (dbug "Before read: pre-str = ~w str = ~w~%" pre-str str)
	       (let ((cat (if pre-str
			      (format nil "~a~%~a" pre-str str)
			      str)))
		 (multiple-value-bind (obj pos)
		     (read-from-string cat nil *continue-symbol*)
		   ;; Make MORE be the rest of the string, if any.
		   (setf more
			 (if (and (not (eql obj *continue-symbol*))
				  (< pos (length cat)))
			     (subseq cat pos)
			     nil))
		   obj))))))
	  (repl-read-continue () *continue-symbol*)
;	  (condition (c)
	  (error (c)
	    (setf got-error t)
	    (if debug
		(invoke-debugger c)
		(format t "~&~a" c))
	    *error-symbol*)
	  )))
    :do
    (if (stringp pre-str)
	(setf pre-str (concatenate 'string pre-str str +newline-string+))
	(setf pre-str (concatenate 'string str +newline-string+)))
    (dbug "set pre-str = ~w~%" pre-str)
    (dbug "DO CONTIUE!!~%"))
      result)))

;; Eval and print
(defun repl-eval (form state)
  (with-slots (got-error error-count interceptor debug) state
    (cond
      ((or (eq form *empty-symbol*) (eq form *error-symbol*))
       ;; do nothing
       (dbug "DO NOTHING!!~%"))
      (t
       (dbug "Do Something!!~%")
       ;; If there is an interceptor, let it have a crack at it.
       ;; If interceptor returns nil, it didn't intercept and we should go on.
       ;; There might be some more arguments for the interceptor which it
       ;; can read out of the more string, if it wants to.
       (when (or (not interceptor)
		 (and interceptor
		      (not (funcall interceptor form state))))
	 (handler-case
	     (handler-bind
		 ;; @@@ Probably a bad idea but, ignore warnings in the REPL
		 (
; 		  (warning #'(lambda (c)
; 			       (format t "~&WARNING: ~a~%" c)
; 			       (muffle-warning)))
; 		  #+excl (excl::compiler-note
; 			  #'(lambda (c)
; 			      (format t "Note: ~a~%" c)
; 			      (continue)))
		  (serious-condition
		   #'(lambda (c)
		       (dbug "Handler bind~%")
		       (if debug
			   (invoke-debugger c)
			   (format t "Condition: ~a~%" c))))
		  )
	       (progn
		 (setf - form)
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
			      (format t "~&~s" v) ; This is the "P" in the REPL
			      (when (and (> len 1) (< i (- len 1)))
				(format t " ;~%")) ; It's kind of a convention
			      (incf i))
			(terpri))
		   (setf +++ ++
			 ++ +
			 + -))))
; 	     #+excl (excl::compiler-note (c)
;                       (format t "WTF Note: ~a~%" c)
; 		      (continue))
	   (serious-condition
	    (c)
	     (dbug "Handler case, serious condition~%")
	     (if debug
		 (invoke-debugger c)
		 (format t "~a~%" c)))
	   (error (c)
	     (dbug "Handler case, error~%")
	     (setf got-error t)
	     (if debug
		 (invoke-debugger c)
		 (format t "~a~%" c)))))))
    (if got-error
	(incf error-count)
	(setf error-count 0))))

(defun tiny-repl (&key (debug t)
		       (interceptor *default-interceptor*)
		       prompt-func no-announce terminal-name)
  "Keep reading and evaluating lisp, with line editing."
  ;; Annouce the implemtation and version on systems that don't always do it.
  #-sbcl (when (not no-announce)
	   (format t "~a ~a~%"
		   (lisp-implementation-type)
		   (lisp-implementation-version)))
  #+sbcl (declare (ignore no-announce))
  (let ((state (make-repl-state
		:debug debug
		:interceptor interceptor
		:prompt-func prompt-func
		:terminal-name terminal-name))
	(result nil)
;	(restart-result t)
	(pass-back t)
	(old-debugger-hook *debugger-hook*))
    (when (and debug (find-package :tiny-debug))
      ;; @@@ On SBCL we could also set sb-ext:*invoke-debugger-hook*, to catch
      ;; break and such, but let's not for now.
      (setf *debugger-hook* (find-symbol (symbol-name '#:tiny-debug)
					 (find-package :tiny-debug))))

  (unwind-protect
    (progn
      (incf *repl-level*)
      (loop :while (not			; Yes, the indentation is weird
; (setq restart-result
        (let ((lvl *repl-level*))
	  (restart-case
	   (progn
	     (loop :do
	       (setf (repl-state-got-error state) nil)
	       (setf result (repl-read state))			;;; READ
	       :until (or (equal result *real-eof-symbol*)
			  (equal result *exit-symbol*))
	       :do
		(dbug "~s (~a) ~s~%"
		     result (type-of result)
		     (eq result *empty-symbol*))
		(if (equal result *quit-symbol*)
		  (when (confirm state *repl-level*)
		    (return result))
		  (repl-eval result state))			;;; EVAL
	       :finally (return result)))
	   (abort ()
	     :report
	     (lambda (stream)
	       (if (= lvl 0)
		   (format stream "Return to TOP command loop.")
		   (format stream "Return to command loop ~d." lvl)))
	     nil))))
; )
;      do (dbug "rr = ~a result = ~a~%" restart-result result)
      ))

    ;; Well, let's hope that this will clear the EOF on *standard-input*
    (clear-input *standard-input*)

    (when (> (repl-state-error-count state) 8)
      (format t "Quit due to too many errors.~%"))

    (setq pass-back
	  (cond
	    ((eq result *real-eof-symbol*)
	     (format t "*EOF*~%")
	     t)
	    ((eq result *quit-symbol*)
	     (format t "*Quit*~%")
	     t)
	    ((eq result *exit-symbol*)
	     (format t "*Exit*~%")
	     nil)
	    (t
	     (dbug "~s ~a~%" result (type-of result))
	     t)))
    (setf *debugger-hook* old-debugger-hook)
    (decf *repl-level*))
  pass-back))

;; EOF
