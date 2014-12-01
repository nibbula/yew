;;
;; lish.lisp - Unix Shell & Lisp somehow smushed together
;;

;; $Revision: 1.15 $

;; Todo:
;;  - BUGS:
;;    - parsing < in first word?
;;    - glob expansion of filenames with quoted spaces?
;;    - cd to dir with spaces?
;;    - tiny-rl display bugs
;;    - newly defined commands don't get recognized in completion
;;  - argument parsing for commands
;;    - fix edge cases, more testing for posix-to-lisp-args
;;  - work out error handling
;;    - compilation?
;;    - other?
;;  - have shell-eval return a value: for external commands return the
;;    old shell exit status (from wait), otherwise the command function
;;    return value, or lisp return value
;;  - at least handle ^Z of subprocess!
;;  - fix completion bugs
;;    - // or absolute paths
;;  - fix bug: twiddle expansion when spaces (or quoted chars?) in filename?
;;  - process stuff:
;;    - chains: || &&
;;    - background jobs: & fg bg jobs %n ^Z SIGTSTP etc
;;    - jobs command
;;  - redirections: < > << <()
;;  - smarter completion, specifically:
;;    - completion should use proper completion for command line argument types
;;    - come up with a way to add argument specs for external programs
;;    - just basically do the "right thing" in any circumstance!!! completion
;;      should know what you can type in any circumstance and provide help.
;;  - start/end comment syntax? #| |#, use for tops20 style completion prompts?
;;    Like: alias #|name|# ls #|expansion|# ls -CF
;;    Maybe it's better to use the first lines of of ‘?’ output as a
;;    description.
;;  - add more features to globbing (all the ignored arguments of glob)
;;  - more built-in commands (bash-like):
;;    - "command" command?
;;    - finish bind
;;    - ulimit
;;    - umask
;;    - wait

;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defpackage :lish
  (:documentation "Unix Shell & Lisp somehow smushed together.")
  (:use :cl :dlib :dlib-misc :opsys :stretchy :glob :completion :tiny-rl
	:cl-ppcre)
  (:export
   ;; Main entry point(s)
   #:lish
   #:shell-toplevel
   ;; variables
   #:*lish-level*
   #:*shell*
   #:*old-pwd*
   #:*dir-list*
   #:*shell-path*
   ;; (installation)
   #:make-standalone
   ;; shell object
   #:shell
   #:lish-sub-prompt
   #:lish-prompt-char
   #:lish-prompt-function
   #:lish-aliases
   ;; arguments
   #:argument
   #:arg-name #:arg-type #:arg-value #:arg-default #:arg-repeating
   #:arg-optional #:arg-hidden #:arg-prompt #:arg-help #:arg-short-arg
   #:arg-long-arg
   ;; argument types
   #:arg-boolean #:arg-number #:arg-integer #:arg-float #:arg-string
   #:arg-keyword #:arg-date #:arg-pathname
   #:arg-choice #:arg-choices #:arg-choice-labels
   ;; argument generics
   #:convert-arg
   ;; commands
   #:command #:command-name #:command-function #:command-arglist
   #:command-built-in-p #:command-loaded-from
   #:defcommand
   #:!cd #:!pwd #:!pushd #:!popd #:!dirs #:!suspend #:!history #:!echo
   #:!help #:!alias #:!unalias #:!type #:!exit #:!source #:!debug #:!bind
   #:!times #:!time #:!ulimit #:!wait #:!export #:!format
   #:!read #:!kill #:!umask #:!jobs #:!exec #:|!:| #:!hash
   ;; convenience / scripting
   #:command-pathname
   #:command-paths
   #:input-line-words
   #:command-output-words
   #:command-output-list
   ;; magical punctuation
   #:! #:!? #:!! #:!$ #:!_
   #:!and #:!or #:!bg
   #:!> #:!>> #:!>! #:!>>!
   #:!< #:!!<
   ))
(in-package :lish)

;; We should almost always provide backwards compatibility as an option, but
;; releases with the same major version number should always be compatible
;; in the default configuration.
(defparameter *major-version* 0
  "Major version number. Releases with the same major version number should be
compatible in the default configuration.")
(defparameter *revision* "$Revision: 1.15 $"
  "Minor version number. This should change at least for every release,
probably for every commit to the master.")
(defparameter *version*
  (format nil "~d.~a" *major-version*
	  (subseq *revision* (1+ (position #\space *revision*))
		  (position #\space *revision* :from-end t))))
(defparameter *shell-name* "Lish"
  "The somewhat superfluous name of the shell.")

;; Like on windows this is #\; right? But not cygwin?
(defvar *path-separator*
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

(defvar *shell* nil
  "The current shell instance.")

(defclass shell ()
  ((debug
    :initarg :debug
    :accessor lish-debug
    :documentation "True to enter the debugger on errors in lish.")
   (exit-flag
    :initarg :exit-flag
    :accessor lish-exit-flag
    :documentation "Set to true to exit the shell.")
   (exit-values
    :initarg :exit-values
    :accessor lish-exit-values
    :documentation "List of values to return to the caller.")
   (sub-prompt
    :initarg :sub-prompt
    :accessor lish-sub-prompt
    :documentation "Prompt for continuation lines.")
   (prompt
    :initarg :prompt-char
    :accessor lish-prompt-char
    :documentation "Normal prompt character.")
   (prompt-function
    :initarg :prompt-function
    :accessor lish-prompt-function
    :documentation "Function returning the prompt string.")
   (aliases
    :accessor lish-aliases
    :documentation "Hash table of aliases.")
   (editor
    :accessor lish-editor
    :documentation "Line editor instance.")
   )
  (:documentation "A lispy system command shell.")
  (:default-initargs
   :prompt-char #\@
   :prompt-function #'make-prompt
   :sub-prompt "- "	; @@@ maybe we need sub-prompt-char & sub-prompt-func?
   :debug nil
   :exit-flag nil
   :exit-values '()
  ))

(defvar *shell-path* '()
  "List of directories to autoload commands from.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command arguments

(defclass argument ()
  ((name	:documentation "Name"
		:initarg :name
		:accessor arg-name)
   (type	:documentation "Declared type"
		:initarg :type
		:accessor arg-type)
   (value	:documentation "Value"
		:initarg :value
		:accessor arg-value)
   (default	:documentation "Default value, if optional."
		:initarg :default
		:initform nil
		:accessor arg-default)
   (repeating	:type boolean
		:documentation "True if value can repeat."
		:initarg :repeating
		:initform nil
		:accessor arg-repeating)
   (optional	:type boolean
		:documentation "True if a value is not required."
		:initarg :optional
		:initform t
		:accessor arg-optional)
   (hidden	:type boolean
		:documentation "If true, don't show in help."
		:initarg :hidden
		:initform nil
		:accessor arg-hidden)
   (prompt	:type string
		:documentation "Show when asking user for value."
		:initarg :propmt
		:accessor arg-propmt)
   (help	:type string
		:documentation "Description for the user."
		:initarg :help
		:accessor arg-help)
   (short-arg	:type (or character null)
		:documentation "Command line argument, short form."
		:initarg :short-arg
		:initform nil
		:accessor arg-short-arg)
   (long-arg	:type (or string null)
		:documentation "Command line argument, long form."
		:initarg :long-arg
		:initform nil
		:accessor arg-long-arg))
  (:documentation "Generic command parameter."))

(defmethod initialize-instance :after ((o argument) &rest initargs)
  (declare (ignore initargs))
  ;; Make the long-arg default to the name if the short-arg is set.
  (when (slot-value o 'short-arg)
    (setf (slot-value o 'long-arg) (slot-value o 'name))))

(defmethod print-object ((o argument) stream)
  "Print a lish command argument in an unreadable way."
  (print-unreadable-object (o stream :identity nil :type t)
    (format stream
	    "~a ~s~:[~; repeating~]~:[~; optional~]~:[~; hidden~]~
~@[ -~a~]~@[ --~a~]"
	    (arg-name o) (arg-type o)
	    (arg-repeating o)
	    (arg-optional o)
	    (arg-hidden o)
	    (arg-short-arg o)
	    (arg-long-arg o))))

(defgeneric convert-arg (arg value)
  (:documentation "Convert an argument value from one type to another."))

(defmethod convert-arg ((arg argument) value)
  "The base default conversion just returns the value."
  value)

(defclass arg-boolean (argument) () (:documentation "A true or false value."))
(define-constant +true-strings+ '("T" "TRUE" "YES" "ON" "1"))
(define-constant +false-strings+ '("NIL" "FALSE" "NO" "OFF" "0"))
(defmethod convert-arg ((arg arg-boolean) (value string))
  (cond
    ((position value +true-strings+ :test #'equalp) t)
    ((position value +false-strings+ :test #'equalp) nil)
    (t (error "Can't convert ~w to a boolean." value))))

(defclass arg-number (argument) () (:documentation "A number."))
(defmethod convert-arg ((arg arg-number) (value string))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (numberp num))
	num
	(error "Can't convert ~w to a number." value))))

(defclass arg-integer (arg-number) () (:documentation "An integer."))
(defmethod convert-arg ((arg arg-integer) (value string))
  (let ((int (parse-integer value :junk-allowed nil)))
    (if (and int (integerp int))
	int
	(error "Can't convert ~w to an integer." value))))

(defclass arg-float (arg-number) ()
  (:documentation "An floating point number."))
(defmethod convert-arg ((arg arg-float) (value string))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (floatp num))
	num
	(error "Can't convert ~w to a float." value))))

(defclass arg-string (argument) () (:documentation "A string."))
(defmethod convert-arg ((arg arg-string) (value string))
  (declare (ignore arg))
  value)

(defclass arg-keyword (argument) () (:documentation "A Lisp keyword."))
(defmethod convert-arg ((arg arg-keyword) (value string))
   (if (char/= (char arg 0) #\:)
       (intern (string-upcase (subseq value 1)) (find-package :keyword))
       value))

(defclass arg-date (argument) () (:documentation "A date."))
(defmethod convert-arg ((arg arg-date) (value string))
  (declare (ignore arg))
  ;; @@@ This could be better.
  value)

(defclass arg-pathname (arg-string) () (:documentation "A file system path."))
(defmethod convert-arg ((arg arg-pathname) (value string))
  (declare (ignore arg))
  value)

(defclass arg-choice (argument)
  ((choices	:type list
		:documentation "A list of choices for value."
		:initarg :choices
		:accessor arg-choices)
   (choice-labels :type list
		:documentation "A list of string names for choices."
		:initarg :choice-labels
		:accessor arg-choice-labels)
   (choice-func :type function
		:documentation "A function to call to get the list of choices."
		:initarg :choice-func
		:accessor arg-choice-func))
  (:documentation "An argument whose value must be one of a list of choices."))

(defmethod convert-arg ((arg arg-choice) (value string))
  (let (choice)
    (if (setf choice (find value (arg-choices arg)
			   :test #'(lambda (a b)
				     (equalp a (format nil "~a" b)))))
	choice
	(error "Argument ~w is not one of ~a." value (arg-choices arg)))))

#| Actually I think these should just be in the base class
(defclass arg-command-line (argument)
  ((short-arg	:type character
		:documentation "Command line argument, short form."
		:initarg :short-arg
		:accessor arg-short-arg)
   (long-arg	:type string
		:documentation "Command line argument, long form."
		:initarg :long-arg
		:accessor arg-long-arg))
  (:documentation "A parameter from a command line."))

(defclass arg-cmd-boolean (arg-boolean arg-command-line) ()
  (:documentation "A true or false value from the command line."))
|#

(defun argument-type-class (type)
  "Return the argument class for a given type. If the type is not a defined
ARG-* class, it defaults to the generic ARGUMENT class."
  (let* (;(pkg (symbol-package type))
	 class-symbol arg-class)
    (cond
      ((listp type)
       (if (not (eq (car type) 'or))
	 (error "Only (or ...) compound types are supported.")
	 'argument))
      ((or (symbolp type) (stringp type))
       (when (setf class-symbol
		   (intern (s+ "ARG-" (string type)) :lish #|pkg|#))
	 (setf arg-class (find-class class-symbol nil)))
       (or arg-class 'argument))
      ((eql type t)
       'argument)
      (t
       (error "Argument type is not a symbol, string or T.")))))

(defun arglist-value (arglist key)
  "Return a value from a DEFCOMMAND arglist argument."
  (let ((p (position key arglist)))
    (and p (elt arglist (1+ p)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-argument-list (arglist)
  "Take an ARGLIST from DEFCOMMAND and turn in into a list of argument objects,
like in the command object."
;;;  (declare (type list arglist))
  (when (not (listp arglist))
    (error "Command argument list must be a list."))
  (loop :with name :and type
     :for a :in arglist :do
;     (assert (listp a))
     (when (not (listp a))
       (error "Command argument list element must be a list."))
     #|   (setf name (arglist-value a :name)
     type (arglist-value a :type)) |#
     (setf name (first a)
	   type (second a))
     (when (not name)
       (error "Arguments must have a name."))
     (when (not type)
       (error "Arguments must have a type."))
     (setf a (append (list :name name :type type) (cddr a)))
     :collect (apply #'make-instance (argument-type-class type) a))))

(defun arg-has-flag (arg)
  (or (arg-short-arg arg) (arg-long-arg arg)))

;; They must be keyworded if there are any flagged arguments.
(defun args-keyworded (args)
  "Check if an argument must be keyworded. "
  (loop :for a :in args :do
     (when (arg-has-flag a)
       (return-from args-keyworded t)))
  nil)

;; Thankfully this is nowhere near as hairy as posix-to-lisp-args.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun shell-to-lisp-args (shell-args)
  "Return a Lisp argument list for the given lish argument list."
  (let ((mandatories
	 (loop :for a :in shell-args
	    :if (not (arg-optional a))
	    :collect a))
	(optionals
	 (loop :for a :in shell-args
	    :if (arg-optional a)
	    :collect a))
	(repeating
	 (loop :for a :in shell-args
	    :if (arg-repeating a)
	    :collect a))
	(keyworded (args-keyworded shell-args))
	(new-list '()))
    ;; Mandatory arguments
    (loop :for a :in mandatories :do
       (push (symbolify (arg-name a)) new-list))
    ;; This is augmented here to allow paralellism in the let above.
    (setf keyworded (or keyworded
			(and optionals repeating
			     (and (not (equal optionals repeating))
				  (= (length optionals) 1)))
			(> (length repeating) 1)))
    (if keyworded
	(progn
	  (push '&key new-list)
	  (loop :for a :in optionals :do
	     (push
	      (if (arg-default a)
		  (list (symbolify (arg-name a)) (arg-default a))
		  (symbolify (arg-name a)))
	      new-list)))
	(cond
	  ;; If both optional and repeating, do repeating (i.e. &rest)
	  (repeating
	   (push '&rest new-list)
	   ;; Must be only one repeating, else it would be keyworded.
	   (push (symbolify (arg-name (first repeating))) new-list))
	  (optionals
	   (push '&optional new-list)
	   (loop :for a :in optionals :do
	      (push
	       (if (arg-default a)
		   (list (symbolify (arg-name a)) (arg-default a))
		   (symbolify (arg-name a)))
	       new-list)))))
    (nreverse new-list))))

(defun vuvu (str l-args)
  (let ((aa (shell-to-lisp-args (command-arglist (get-command str)))))
    (format t "~w ~{~w ~}~%~w~%~%" str (command-arglist (get-command str)) aa)
    (assert (equalp aa l-args))))

;; You probably have to say: (with-package :lish (test-stla))
;; Unfortunately this may fail if not updated to reflect builtin changes.
(defun test-stla ()
  (vuvu "cd"      '(&optional directory))
  (vuvu "pwd"     '())
  (vuvu "pushd"   '(&optional directory))
  (vuvu "popd"    '(&optional number))
  (vuvu "dirs"    '())
  (vuvu "suspend" '())
  (vuvu "history" '(&key clear write read append read-not-read filename
		    show-times delete))
  (vuvu ":"       '(&rest args))
  (vuvu "echo" 	  '(&key no-newline args))
  (vuvu "help" 	  '(&optional subject))
  (vuvu "alias"   '(&optional name expansion))
  (vuvu "unalias" '(name))
  (vuvu "exit" 	  '(&rest values))
  (vuvu "source"  '(filename))
  (vuvu "debug"   '(&optional (state :toggle)))
  (vuvu "export"  '(&optional name value))
  (vuvu "jobs" 	  '(&key long))
  (vuvu "kill" 	  '(&key list-signals (signal 15) pids))
  (vuvu "format"  '(format-string &rest args))
  (vuvu "read" 	  '(&key name prompt timeout editing))
  (vuvu "time" 	  '(&rest command))
  (vuvu "times"   '())
  (vuvu "umask"   '(&key print-command symbolic mask))
  (vuvu "ulimit"  '())
  (vuvu "wait" 	  '())
  (vuvu "exec" 	  '(&rest command-words))
  (vuvu "bind" 	  '(&key print-bindings print-readable-bindings query
		    remove-function-bindings	remove-key-binding key-sequence
		    function-name))
  (vuvu "hash" 	  '(&key rehash commands))
  (vuvu "type" 	  '(&key type-only path-only all names))
  (format t "SUCCEED!~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defparameter *initial-commands* nil
  "List of initial commands.")

(defparameter *command-list* nil
  "List of command names")

(defvar *lish-commands* nil
  "Hash table of commands. This means that commands are shared by all shell
instances.")

(defun lish-commands ()
  (when (not *lish-commands*)
    (setf *lish-commands* (make-hash-table :test #'equal)))
  *lish-commands*)

(defun set-command (name obj)
  (setf (gethash name (lish-commands)) obj))

(defun get-command (name)
  (gethash name (lish-commands)))

(defun init-commands ()
  "Set up the *LISH-COMMANDS* hash table, and load it with the commands from
*INITIAL-COMMANDS*, which is likely whatever commands were defined with
DEFBUILTIN."
  (loop :for (k v) :in *initial-commands*
     :do (set-command k v)))

(defmethod initialize-instance :after ((sh shell) &rest initargs)
  (declare (ignore initargs))
;  (setf (slot-value sh 'commands) (make-hash-table :test #'equal))
  (setf (slot-value sh 'aliases) (make-hash-table :test #'equal))
  (init-commands))

(eval-when (:compile-toplevel :load-toplevel) ; needed by defcommand macro
  (defun command-function-name (n)
    "Return the normal command function symbol for the given command name."
    (intern (concatenate 'string "!" (string-upcase n)))))

#|
  (defmacro squirrel-def (package &body body)
(let ((squirrel-package (s+ package "-DEFS"))
(defs-sym (gensym "SQUIRREL-DEF")))
`(if (find-package ,package)
(progn ,@body)
(progn
(when (not (find-package ,squirrel-package))
(make-package ,squirrel-package)
(let ((,defs-sym (intern "*DEFS*"
(find-package ,squirrel-package))))
(eval `(defvar ,,defs-sym '()))))
(let ((,defs-sym (intern "*DEFS*" (find-package ,squirrel-package))))
(set ,defs-sym (cons ',body (symbol-value ,defs-sym))))))))
  |#

;; There should be little distinction between a user defined command and
;; "built-in" command, except perhaps for a warning if you redefine a
;; pre-defined command, and the fact that things defined in here are
;; considered "built-in" and listed in help.

(defmacro defbuiltin (name (&rest arglist) &body body)
  "This is like defcommand, but for things that are considered built in to the
shell."
  (let ((func-name (command-function-name name))
	(command-name (intern (string name) :lish))
	(name-string (string-downcase name))
;;;	(name-string (concatenate 'string "\"" (string-downcase name) "\""))
	(params (shell-to-lisp-args (make-argument-list arglist))))
    `(progn
       (defun ,func-name ,params
	 ,@body)
;       (export (quote ,func-name))
       (push (quote ,command-name) *command-list*)
       (push (list ,name-string (make-instance
				 'command :name ,name-string
				 :arglist (make-argument-list ',arglist)
				 :loaded-from *load-pathname*
				 :built-in-p t))
	     *initial-commands*))))

;; This is differs from DEFBUILTIN in that:
;;   - It doesn't push to *initial-commands*
;;   - It doesn't set BUILT-IN-P
;;   - It doesn't automatically export
;;
;; I would wish that defcommand could provide enough documentation to make a
;; man page, or an info page, or a CLHS like page.
;;
;; We should make it easy to indicate man page sections and info links.
;; We should accept some kind of markdown for doc strings.
;; We should automatically generate any sections possible.
;;
;; Man pages have:
;;   NAME		name - Less than one line summary
;;   SYNOPSIS		(should be automatically generated, as in help)
;;   DESCRIPTION	Pretty much the normal docstring.
;;   OPTIONS		List options and describe in detail. Generate from
;;			option docstrings?
;;   EXAMPLES		very useful
;;   FILES		<perhaps not so useful / optional>
;;   ENVIRONMENT	<perhaps not so useful / optional>
;;   DIAGNOSTICS	populated by gleaning errors?
;;   BUGS		Of course we don't need this ;)
;;   AUTHOR		Gleaned from package author
;;   SEE ALSO		this is useful in CLHS, so...
;;
;; Info pages have:
;;   Up, Prev, Next links
;;   Menu sub links
;; 
;; CLHS pages have:
;;   Summary  (type, name, args, and return values)
;;   Arguments and Values
;;   Description
;;   Examples
;;   Exceptional Situations
;;   Side Effects
;;   Affected By
;;   See Also
;;   Notes

(defmacro defcommand (name (&rest arglist) &body body)
  "Define a command for the shell. NAME is the name it is invoked by. ARGLIST
is a shell argument list. The BODY is the body of the function it calls."
  (let ((func-name (command-function-name name))
	(command-name (intern (string name)))
	(name-string (string-downcase name))
	(params (shell-to-lisp-args (make-argument-list arglist))))
    `(progn
       (defun ,func-name ,params
	 ,@body)
       ;; Don't export stuff because it causes package variance on reloading.
       ;; @@@ Perhaps we should define commands in the LISH-USER package
       ;; instead, so they can be exported and used by other packages?
       (push (quote ,command-name) lish::*command-list*)
;;;       (set (find-symbol "*COMMAND-LIST*" :lish) (quote ,command-name))
       (set-command ,name-string
		    (make-instance (find-symbol "COMMAND" :lish)
				   :name ,name-string
				   :loaded-from *load-pathname*
				   :arglist (make-argument-list ',arglist))))))

(defclass command ()
  ((name
    :accessor command-name        :initarg :name
   :documentation "The string word that invokes the command.")
   (function
    :accessor command-function    :initarg :function
    :documentation "The function that performs the command.")
   (arglist
    :accessor command-arglist     :initarg :arglist
    :documentation "A list of arguments.")
   (built-in-p
    :accessor command-built-in-p  :initarg :built-in-p :initform nil
    :documentation "True if the command is considered ‘built in’.")
   (loaded-from
    :accessor command-loaded-from :initarg :loaded-from :initform nil
    :documentation "Where the command was loaded from."))
  (:documentation "A command defined internally in the shell."))

(defmethod initialize-instance :after ((b command) &rest initargs)
  (declare (ignore initargs))
  ;; Make the default function binding from the name
  (if (not (slot-boundp b 'function))
      (setf (slot-value b 'function)
	    (command-function-name (slot-value b 'name)))))

(defmethod print-object ((o command) stream)
  "Print a lish command in an unreadable way."
  (print-unreadable-object (o stream :identity nil :type t)
    ;; (if (slot-boundp o 'synopsis)
    ;; 	(format stream "~s" (command-synopsis o))
    ;; 	(format stream "~s" (if (slot-boundp o 'name)
    ;; 				(command-name o)
    ;; 				(format stream "<unnamed>"))))))
    (format stream "~a" (posix-synopsis o))))

;; Yet another defclass wrapper.
;; (defmacro defargtype (name (&rest superclasses) &body body)
;;   "This defines a command argument type. Mostly for the purposes "
;;   (let ((slots '()) (body
;; 	 (loop :for form :in body :do
;; 	    (cond
;; 	      ((listp form) ; a slot specifier
;; 	       )
;;     `(defclass ,name ,superclasses
;;        ,rock-the-body)))


#|
  (defclass arg-list ()
(arg-list-list)
(:documentation "A list of arguments."))

  (defgeneric get-args ()
(:documentation "Return a string to prompt with."))
  (defmethod get-args (arg-list))

  (defgeneric args-help ()
(:documentation "Return a string to prompt with."))
  (defmethod args-help (arg-list))
  |#

#|
;;					;
;; The rules for converting POSIX arguments to lambda lists are fairly ;
;; complicated.				;
;;					;
;; m=manditory o=optional r=repeating f=flagged ;
;;					;
;; When only manditory and optional, we don't need keywords. ;
;; Order of manditories vs optionals doesn't matter to lambda lists, but does ;
;; to posix.				;
;; Non-flagged optionals must come after manditories. ;
  m1 m2		(m1 m2)				m1 m2
  m1 m2 o1 o2	(m1 m2 &optional o1 o2)		m1 m2 [o1] [o2]
  o1 o2 m1 m2 	(m1 m2 &optional o1 o2)		[o1] [o2] m1 m2  problematic???
  o1 o2		(&optional o1 o1)		[o1] [o2]

;; We can't have more than one non-flagged repeating ;
  ro1		(&rest r1)			[ro1...]
  rm1 rm2		ERROR
  ro1 ro2		ERROR
  m1 m2 ro1	(m1 m2 &rest r1)		m1 m2 [ro2...]
  m1 m2 r1 r2	ERROR
  o1 o2 r1	ERROR (o1 and o2 must be flagged)

  rm1		(&rest r1)			r1[...]
  r1 r2		ERROR
  m1 m2 rm1	(m1 m2 &rest r1)		m1 m2 rm1[...]
  m1 m2 ro1	(m1 m2 &rest r1)		m1 m2 [ro1...]
  m1 m2 r1 r2	ERROR
  of1 of2 rm1	(&key of1 of2 r1)		[-12] r1[...]
  of1 of2 ro1	(&key of1 of2 r1)		[-12] [r1...]

;; Flagged optional must be done as keywords ;
  m1 m2 of1 o2	(m1 m2 &key of1 o2)		[-1] m1 m2 [o2]
  m1 m2 o1 of2	(m1 m2 &key o1 of2)		[-2] m1 m2 [o1]
  m1 m2 of1 of2	(m1 m2 &key o1 of2)		[-2] m1 m2 [o1]
  of1 o2 m1 m2 	(m1 m2 &key of1 o2)		[-1] m1 m2 [o2]
  of1 of2 m1 m2 	(m1 m2 &key of1 of2)		[-12] m1 m2
  of1 of2		(&key of1 of2)			[-12]
  o1 of2		(&key of1 of2)			[-2] [o1]
  of1 o2		(&key of1 of2)			[-1] [o2]

;; Flagged manditory must be done as keywords, DOES'T make other manditories ;
;; keywords.				;
;; Manditory flagged treated as optional flagged, except error afterward if ;
;; not present.				;
  mf1 m2 of1 o2	(m2 &key mf1 of1 o2)		[-of1] [-mf1] [o2] m2
  m1 mf2 o1 of2	(m1 &key mf2 o1 of2)		[-mf2] [-of2] [o1] m1
  mf1 mf2 of1 of2	(&key mf1 mf2 o1 of2)		[-mf1] [-mf2] [-of2] [o1]
  of1 o2 mf1 m2 	(m2 &key of1 o2 mf1)		[-of1] [-mf1] [o2] m2
  of1 of2 m1 mf2 	(m1 &key of1 of2 mf2)		[-of1] [-of2] [-mf2] m1
  mf1 mf2		(&key mf1 mf2)			[-mf1] [-mf2]

;; Repeating flagged: can have more than one, but values can't start with ;
;; dashes!				;
;; Repeating flagged manditory and optional are treated the same. ;
  rf1		(&rest rf1)			[-rf1 foo] [...]
  (*stupid but legal)
  rf1 rf2		(&key rf1 rf2)			[-rf1 foo...] [-rf2 bar...]
  (*can be given in any order, e.g.: -rf2 foo bar -rf1 foo bar baz)

;; Flagged arguments can appear in POSIX in multiple ways: ;
  (:short-arg x :type boolean)		[-x]
  ((:short-arg x :type boolean)   	[-xy]
(:short-arg y :type boolean))
  (:short-arg x :type (not boolean))	[-x arg]
  ((:short-arg x :type (not boolean))	[-x arg] [-y arg]
(:short-arg y :type (not boolean)))
  (:long-arg foo :type boolean)   	[--foo]
  (:long-arg foo :type (not boolean))    	[--foo bar]

  |#

;;
;; If there are any optional non-positional args (i.e. optional args with
;; short-arg or long-arg specified, then all the lisp args must be keyworded.
;;
;; @@@ Make argument type classes work, with specific type
;; validation and completion methods.

#|
  (defun lish-to-lisp-args (args)
"Convert a Lish argument list into a Lisp lambda list."
(cond
((null args)
(list))
    ;; (x y z...)			;
(every #(lambda (a) (not ())) args)
    ;; (&optional x y z...)		;
()
    ;; (&rest x)			;
()
    ;; (x y &rest z)			;
()
    ;; (&key x y z)			;
()))
  |#

(defmacro move-arg (old new i arg)
  "Move the I'th item from the OLD to the NEW list, and return both."
  `(progn
     (setf ,new (push (convert-arg ,arg (nth ,i ,old)) ,new)
	   ,old (delete-nth ,i ,old))))

(defun arg-key (arg)
  (intern (string-upcase (arg-name arg)) :keyword))

(defmacro move-key (old new i arg keyworded)
  "Move the I'th item from the OLD to the NEW list, and return both."
  `(progn
     (when ,keyworded
       (setf ,new (push (arg-key ,arg) ,new)))
     (setf ,new (push (convert-arg ,arg (nth ,i ,old)) ,new))
     (setf ,old (delete-nth ,i ,old))))

(defmacro move-flag (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
;;;     (format t "(nth (1+ ~s) ~s) = ~s~%" ,i ,old (nth ,i ,old))
;;;     (format t "(convert-arg ~s ~s) = ~s~%" ,arg (nth ,i ,old)
;;;	     (convert-arg ,arg (nth ,i ,old)))
;;;     (setf ,new (push (convert-arg ,arg (nth (1+ ,i) ,old)) ,new))
     (setf ,new (push (convert-arg ,arg (nth (1+ ,i) ,old)) ,new))
     (setf ,old (delete-nth ,i ,old))  ; flag
     (setf ,old (delete-nth ,i ,old)))) ; arg

(defmacro move-boolean-2 (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push t ,new))
     (setf ,old (delete-nth ,i ,old))  ; keyword
     (setf ,old (delete-nth ,i ,old)))) ; arg

(defmacro move-boolean (old new i arg)
  (declare (ignore old i))
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push t ,new))))

(defmacro move-repeating (old new start arg keyworded &optional until)
  (let ((e (gensym "e")))
  `(progn
     (if ,until
	 (error "can't do until yet") ;; @@@
	 (progn
	   (if ,keyworded
	       (progn
		 (setf ,new (push (arg-key ,arg) ,new))
		 (setf ,new (push (nthcdr ,start ,old) ,new)))
	       (loop :for ,e :in (nthcdr ,start ,old) :do
		  (setf ,new (push ,e ,new))))
	   (setf ,old (subseq ,old 0 ,start)))))))

(defun posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t'
become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  (let ((i 0)
;	(new-list        '())
	(old-list        (copy-list p-args)) ; so we don't modify it
	(new-flags       '())
	(new-mandatories '())
	(new-optionals   '())
	(new-repeating   '())
	(keyworded (args-keyworded (command-arglist command)))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (loop :for a :in p-args :do
       (if (and (stringp a) (> (length a) 0)
		(char= (char a 0) #\-)) ; arg starts with dash
	   (if (eql (char a 1) #\-)		    ; two dash arg
	       ;; --long-arg
	       (loop :for arg :in (command-arglist command) :do
		  ;; @@@ have to deal with repeating?
		  (if (equalp (subseq a 2) (arg-long-arg arg))
		      (move-flag old-list new-flags i arg)
		      (incf i)))
	       ;; -abcxyz (short args)
	       (prog (flag-taken boolean-taken)
		  (loop :for cc :from 1 :below (length a) :do
		     (setf flag-taken nil)
		     (loop :for arg :in (command-arglist command) :do
		       (when (eql (arg-short-arg arg) (char a cc))
			 (setf flag-taken t)
			 ;; @@@ have to deal with repeating?
			 (if (eq (arg-type arg) 'boolean)
			     (progn
			       (move-boolean old-list new-flags i arg)
			       (setf boolean-taken t))
			     (if (/= cc (1- (length a)))
				 (error "Unrecognized flag ~a." a)
				 (move-flag old-list new-flags i arg)))))
		     (when (not flag-taken)
		       (warn "Unrecognized option ~a" (char a cc))))
		  (when boolean-taken
		    (setf old-list (delete-nth i old-list)))))
	   (incf i)))
    (setf new-flags (nreverse new-flags))
    ;; Non-flagged mandatories.
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (move-arg old-list new-mandatories 0 arg)
	       (error "Missing mandatory argument ~a." (arg-name arg)))
	   (incf i)))
    (setf new-mandatories (nreverse new-mandatories))
    ;; Non-flagged optionals
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)) (> (length old-list) 0))
	   (move-key old-list new-optionals 0 arg keyworded)
	   (incf i)))
    (setf new-optionals (nreverse new-optionals))
    ;; Repeating
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-repeating 0 arg keyworded)))))
    (setf new-repeating (nreverse new-repeating))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (concatenate
     'list new-mandatories new-optionals new-repeating new-flags)))

(defun old2-posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t' become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  (dbug "(length p-args) = ~w~%" (length p-args))
  (dbug "command = ~w~%" command)
  (let ((i 0)
	(new-list '())
	(old-list (copy-list p-args))	; so we don't modify it
	(keyworded (args-keyworded (command-arglist command)))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (dbug "Flags:~%")
    (loop :for a :in p-args :do
       (dbug "old-list = (")
       (loop :for ii :from 0 :below (length old-list) :do
	  (dbug "~w~a " (nth ii old-list) (if (= ii i) "*" "")))
       (dbug ")~%")
       (if (and (stringp a) (> (length a) 0)
		(char= (char a 0) #\-)) ; arg starts with dash
	   (if (eql (char a 1) #\-)		    ; two dash arg
	       ;; --long-arg
	       (loop :for arg :in (command-arglist command) :do
		  ;; @@@ have to deal with repeating?
		  (if (equalp (subseq a 2) (arg-long-arg arg))
		      (progn
			(move-flag old-list new-list i arg)
			(dbug "long-arg ~w ~%" a))
		      (incf i)))
	       ;; -abcxyz (short args)
	       (prog (flag-taken)
		  (loop :for i :from 1 :below (length a) :do
		     (setf flag-taken nil)
		     (loop :for arg :in (command-arglist command) :do
		       (when (eql (arg-short-arg arg) (char a i))
			 (dbug "short-arg ~w~%" (char a i))
			 (setf flag-taken t)
			 ;; @@@ have to deal with repeating?
			 (if (eq (arg-type arg) 'boolean)
			     (move-boolean old-list new-list i arg)
			     (if (/= i (1- (length a)))
				 (error "Unrecognized flag ~a." a)
				 (move-flag old-list new-list i arg)))))
		     (when (not flag-taken)
		       (warn "Unrecognized option ~a" (char a i))))
		  (setf old-list (delete-nth i old-list))))
	   (incf i)))
    ;; Non-flagged mandatories.
    (dbug "Mandatory: ~a ~w~%" i old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (progn
		 (move-arg old-list new-list 0 arg)
		 (dbug "mandatory ~w~%" arg))
	       (error "Missing mandatory argument ~a." (arg-name arg)))
	   (incf i)))
    ;; Non-flagged optionals
    (dbug "Optional: ~a ~w~%" i old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)) (> (length old-list) 0))
	   (progn
	     (move-key old-list new-list 0 arg keyworded)
	     (dbug "optional ~w~%" arg))
	   (incf i)))
    ;; Repeating
    (dbug "Repeating: ~a ~w~%" i old-list)
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-list 0 arg keyworded)))))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (nreverse new-list)))

(defun o-vivi (str &rest args)
  (format t "~w ~{~w ~}~%~w~%~%" str args
	  (posix-to-lisp-args (get-command str) args)))

(defun vivi (str p-args l-args)
  (let ((aa (posix-to-lisp-args (get-command str) p-args)))
    (format t "~w ~{~w ~}~%~w~%~%" str p-args aa)
    (assert (equalp aa l-args))))

(defcommand tata  ;; @@@ just for testing
    (("one" boolean :short-arg #\1)
     ("two" string  :short-arg #\2))
  "Test argument conversion."
  (format t "one = ~s two = ~s~%" one two))

(defcommand gurp ;; @@@ just for testing
    (("pattern" string   :optional nil)
     ("files"   pathname :repeating t)
     ("invert"  boolean  :short-arg #\i))
  "Test argument conversion."
  (format t "pattern = ~s files = ~s invert = ~s~%" pattern files invert))

(defcommand zurp
    (("section" string :short-arg #\s)
     ("entry"   string :optional t))
  "Test argument conversion."
  (format t "entry = ~s section = ~s~%" entry section))

(defun test-ptla ()
  (vivi ":" '() '())
  (vivi ":"
	'("(format t \"egg~a~%\" (lisp-implementation-type))")
	'("(format t \"egg~a~%\" (lisp-implementation-type))"))
  (vivi ":"
	'("blah" "blah" "blah" "etc" "...")
	'("blah" "blah" "blah" "etc" "..."))
  (vivi "alias" '() '())
  (vivi "alias" '("name") '("name"))
  (vivi "alias" '("name" "expansion") '("name" "expansion"))
  (vivi "alias"
	'("name" "expansion" "extra" "junk")
	'("name" "expansion"))
  (vivi "bind" '() '())
  (vivi "bind" '("-p") '(:print-bindings t))
  (vivi "bind" '("-P") '(:print-readable-bindings t))
  (vivi "bind" '("-r" "foo") '(:remove-key-binding "foo"))
  (vivi "cd" '() '())
  (vivi "cd" '("dir") '("dir"))
  (vivi "debug" '() '())
  (vivi "debug" '("on") '(t))
  (vivi "debug" '("off") '(nil))
  ;; This is supposed to fail, since pecan isn't a boolean
;  (vivi "debug" '("pecan") '()) 
  (vivi "gurp"  '("-i" "foo" "bar" "baz" "lemon")
	'("foo" :files ("bar" "baz" "lemon") :invert t))
  (vivi "zurp"  '("-s" "3" "chflags") '(:entry "chflags" :section "3"))
)

;(with-dbug (lish::posix-to-lisp-args (lish::get-command "bind") '("-r" "foo")))

(defun posix-synopsis (command)
  "Return a string with the POSIX style argument synopsis."
  (with-output-to-string (str)
    (format str "~a" (command-name command))
    ;; boolean flag options
    (loop :with first-time = t
       :for a :in (command-arglist command) :do
       (when (and (eql (arg-type a) 'boolean)
		  (arg-short-arg a))
	 (when first-time
	   (setf first-time nil)
	   (format str " [-"))
	 (format str "~c" (arg-short-arg a)))
       :finally (when (not first-time) (format str "]")))
    ;; non-boolean
    (loop :for a :in (command-arglist command) :do
       (when (not (and (eql (arg-type a) 'boolean)
		       (arg-short-arg a)))
	 (if (arg-optional a)
	     (format str " [")
	     (format str " "))
	 (if (arg-short-arg a)
	     (format str "-~a " (arg-short-arg a))
	     (when (arg-long-arg a)
	       (format str "--~a " (arg-long-arg a))))
	 (format str "~a" (arg-name a))
	 (when (arg-repeating a)
	   (format str "..."))
	 (when (arg-optional a)
	   (format str "]"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command definitions

(defparameter *old-pwd* nil		; @@@ should be per shell
  "The last wording directory.")

(defbuiltin cd (("directory" pathname))
  "Usage: cd [directory]
Change the current directory to DIRECTORY."
  (setf *old-pwd* (nos:current-directory))
  (nos:change-directory directory))

(defbuiltin pwd ()
  "Usage: pwd
Print the current working directory."
  (format t "~a~%" (nos:current-directory)))

(defvar *dir-list* nil			; @@@ should be per shell
  "Directory list for pushd and popd.")

(defbuiltin pushd (("directory" pathname))
  "Usage: pushd [dir]
Change the current directory to DIR and push it on the the front of the directory stack."
  (when (not directory)
    (setf directory (pop *dir-list*)))
  (push (nos:current-directory) *dir-list*)
  (!cd directory))

(defbuiltin popd (("number" number))
  "Usage: popd [n]
Change the current directory to the top of the directory stack and remove it from stack."
  (declare (ignore number))
  (let ((dir (pop *dir-list*)))
    (!cd dir)
    dir))

(defbuiltin dirs ()
  "Usage: dirs
Show the directory stack."
  (format t "~a~%" *dir-list*))

(defbuiltin suspend ()
  "Usage: suspend
Suspend the shell."
;  (opsys:kill (opsys:getpid) opsys:sigstop))
  (opsys:kill (opsys:getpid) 17))	; SIGSTOP

(defbuiltin history
    (("clear"	      boolean  :short-arg #\c)
     ("write"	      boolean  :short-arg #\w)
     ("read"	      boolean  :short-arg #\r)
     ("append"	      boolean  :short-arg #\a)
     ("read-not-read" boolean  :short-arg #\n)
     ("filename"      filename :short-arg #\f)
     ("show-times"    boolean  :short-arg #\t)
     ("delete"	      integer  :short-arg #\d))
  "Show a list of the previously entered commands."
  ;; Check argument conflicts
  (cond ;; @@@ Could this kind of thing be done automatically?
    ((and clear (or write read append read-not-read filename show-times delete))
     (error "CLEAR should not be given with any other arguments."))
    ((and delete (or write read append read-not-read filename show-times clear))
     (error "DELETE should not be given with any other arguments."))
    ((> (count t `(,write ,read ,append ,read-not-read)) 1)
     (error
      "Only one of WRITE, READ, APPEND, or READ-NOT-READ should be given."))
    ((and filename (not (or read write append read-not-read)))
     (error
      "FILENAME is only useful with READ, WRITE, APPEND, or READ-NOT-READ.")))
  (cond
    (clear
     (tiny-rl:history-clear :lish))
    ;; @@@ TODO: finish this when history saving in tiny-rl is done.
    (t
     (tiny-rl:show-history :lish))))

(defbuiltin #:|:| (("args" t :repeating t))
  "Usage: : [args]
Arguments are evaluated for side effects."
  (declare (ignore args))
  (values))

(defbuiltin echo
    (("no-newline" boolean :short-arg #\n)
     ("args" t :repeating t))
  "Usage: echo [-n] ...
Output the arguments. If -n is given, then don't output a newline a the end."
  (format t "~{~a~#[~:; ~]~}" args)
  (when (not no-newline)
    (format t "~%")))

(defparameter *help-subjects*
  '("commands" "builtins" "editor" "keys")
  "Subjects we have help about.")

(defun help-choices ()
  "Return a list of choices for a help subject."
  (append *help-subjects* *command-list*))

(defclass help-subject (arg-choice)
  ()
  (:default-initargs
   :choice-func #'help-choices))

(defbuiltin help (("subject" help-subject))
  "help [subject]         Show help on the subject.
Without a subject show some subjects that are available."
  (if (not subject)
      (progn
	(format t "~
Lish version ~a help:
  command [arg*...]   Run a program in your path with the given ARGs.
  ([expressions]...)  Evaluate Lisp expressions.
  help [subject]      Show help on the subject.
  exit                Exit the shell.
Subjects:
  help commands       Show help on built-in commands.
  help editor         Show help on the line editor.
  help keys           Show help on key bindings.
" *version*))
      ;; topics
      (cond
	((or (equalp subject "commands") (equalp subject "builtins"))
;	 (format t "  ~c[4mName~14t~c[0m  ~c[4mSynopsis~80t~c[0m~%"
;		 #\escape #\escape #\escape #\escape)
	 (let ((commands
		(sort
		 (loop :for k :being :the :hash-keys :of (lish-commands)
		    :collect k)
		 #'string-lessp)))
	   (format t "Built-in commands:~%")
	   (loop :for k :in commands :do
	      (let ((b (get-command k)))
		  (when (and b (command-built-in-p b))
		    (format t "  ~a~%" (posix-synopsis b)))))
	   (format t "Added commands:~%")
	   (loop :for k :in commands :do
	      (let ((b (get-command k)))
		  (when (and b (not (command-built-in-p b)))
		    (format t "  ~a~%" (posix-synopsis b)))))))
	((or (equalp subject "editor"))
	 (format t "You can use some Emacs-like commands to edit the command line.

Some notable keys are:
 <Tab>        Try to complete the word in front of the cursor.
 ?            Show what input is expected. List possibilities.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line. Also the <Up Arrow> key.
 <Control-N>  Next history line. Also the <Down Arrow> key.
 <Control-B>  Move the cursor back one character. Also the <Left Arrow> key.
 <Control-F>  Move the cursor forward one character. Also the <Right Arrow> key.
 <Control-Q>  Quote next character, like if you want to really type a \"?\".
 <F9>         Switch back and forth between LISH and the lisp REPL."))
	((or (equalp subject "keys"))
	 (format t "Here are the keys active in the editor:~%")
	 (!bind :print-bindings t))
	(t ;; Try a specific command
	 (let* ((b    (get-command subject))
		(symb (intern (string-upcase subject) :lish))
		(doc  (when b (documentation b 'function)))
		(fdoc (when (fboundp symb)
			(documentation (symbol-function symb) 'function))))
	   (cond
	     (doc  (format t "~a~%" doc))
	     (fdoc (format t "Lisp function:~%~a~%" fdoc))
	     (b    (format t "Sorry, there's no help for \"~a\".~%" subject))
	     (t    (format t "I don't know about the subject \"~a\"~%"
			   subject))))))))

(defmethod documentation ((b command) (doctype (eql 'function)))
  "Return the documentation string for the given shell command."
  (with-output-to-string (str)
    (format str "~a" (posix-synopsis b))
    (when (documentation (command-function b) 'function)
      (format str "~%~a" (documentation (command-function b) 'function)))
    (when (command-loaded-from b)
      (format str "~%Loaded from ~a" (command-loaded-from b)))))

(defun set-alias (sh name expansion)
  "Define NAME to be an alias for EXPANSION.
NAME is replaced by EXPANSION before any other evaluation."
  (setf (gethash name (lish-aliases sh)) expansion))

(defun unset-alias (sh name)
  "Remove the definition of NAME as an alias."
  (remhash name (lish-aliases sh)))

(defun get-alias (sh name)
  (gethash name (lish-aliases sh)))

(defbuiltin alias
    (("name" string)
     ("expansion" string))
  "Define NAME to expand to EXPANSION when starting a line."
  (if (not name)
      (loop :for a :being :the :hash-keys :of (lish-aliases *shell*)
	    :do
	    (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		    a (get-alias *shell* a)))
      (if (not expansion)
	  (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		  name (get-alias *shell* name))
	  (set-alias *shell* name expansion))))

(defbuiltin unalias (("name" string :optional nil))
  "Remove the definition of NAME as an alias."
  (unset-alias *shell* name))

(defbuiltin exit (("values" string :repeating t))
  "Exit from the shell. Optionally return values."
  (when values
    (setf (lish-exit-values *shell*) (loop :for v :in values :collect v)))
  (setf (lish-exit-flag *shell*) t))

(defbuiltin source (("filename" pathname :optional nil))
  "Evalute lish commands in the given file."
  (without-warning (load-file *shell* filename)))

;; This is so if it's not provided, it can toggle.
(defclass arg-boolean-toggle (arg-boolean)
  ()
  (:default-initargs
   :default :toggle)
  (:documentation "A true or false value, that can be toggled."))
;; (defmethod convert-arg ((arg arg-boolean-toggle) (value string))
;;   (cond
;;     ((position value +true-strings+ :test #'equalp) t)
;;     ((position value +false-strings+ :test #'equalp) nil)
;;     (t (error "Can't convert ~w to a boolean." value))))

;; @@@ state arg doesn't work right: make designator: 0 off nil
(defbuiltin debug (("state" boolean-toggle))
  "Toggle shell debugging."
  (setf (lish-debug *shell*)
	(if (eql state :toggle)
	    (not (lish-debug *shell*))
	    state))
  (format t "Debugging is ~:[OFF~;ON~].~%" (lish-debug *shell*)))

#|
;; Just use the version from dlib-misc	;
;; @@@ Or maybe the version from there should live here, since it's shellish?? ;
  (defun printenv (&optional original-order) ; copied from dlib-misc ;
"Like the unix command."
(let ((mv (reduce #'max (nos:environ)
:key #'(lambda (x) (length (symbol-name (car x))))))
(sorted-list (if original-order
(nos:environ)
(sort (nos:environ) #'string-lessp
:key #'(lambda (x) (symbol-name (car x)))))))
(loop :for v :in sorted-list
:do (format t "~va ~30a~%" mv (car v) (cdr v)))))
  |#

(defbuiltin export (("name" string) ("value" string))
  "Set environment variable NAME to be VALUE. Omitting VALUE, just makes sure the current value of NAME is exported. Omitting both, prints all the exported environment variables."
  (if name
      (if value
	  (nos:setenv name value)
	  (nos:getenv name))		; actually does nothing
      (printenv)))

(defbuiltin jobs (("long" boolean :short-arg #\l))
  "Lists spawned processes that are active."
  ;; @@@ totally faked & not working
  (loop :for p :in '(fake old junk)
     :do (format t "~a ~a~%" p long)))

(defun get-cols ()
  (let ((tty (tiny-rl::line-editor-terminal (lish::lish-editor *shell*))))
    (ansiterm:terminal-get-size tty)
    (ansiterm:terminal-window-columns tty)))

(defbuiltin kill
    (("list-signals" boolean :short-arg #\l)
     ("signal" 	     signal  :default   15)
     ("pids" 	     integer :repeating t))
  ;; @@@ pid should be job # type to support %job
  "Sends SIGNAL to PID."
  ;; @@@ totally faked & not working
  (if list-signals
      (format t (s+ "~{~<~%~1," (get-cols) ":;~a~> ~}~%") ; bogus, but v fails
	      (loop :for i :from 1 :below nos:*signal-count*
		 :collect (format nil "~2d) ~:@(~8a~)" i (nos:signal-name i))))
      (when pids
	(mapcar #'(lambda (x) (nos:kill signal x)) pids))))

;; Actually I think that "format" and "read" are a bad idea / useless, because
;; they're for shell scripting which you should do in Lisp.

;;; make printf an alias
(defbuiltin format
    (("format-string" string :optional nil)
     ("args" t :repeating t))
  "Formatted output."
  ;; @@@ totally faked & not working
  (apply #'format t format-string args))

;; Since this is for scripting in other shells, I think we don't need to worry
;; about it, since the user can just call READ-LINE-like functions directly.
(defbuiltin read
    (("name"    string)
     ("prompt"  string  :short-arg #\p)
     ("timeout" integer :short-arg #\t)
     ("editing" boolean :short-arg #\e))
  "Read a line of input."
  ;; @@@ totally faked & not working
  (declare (ignore timeout name))
  (if editing (tiny-rl:tiny-rl :prompt prompt)
      (read-line nil nil)))

(defbuiltin time (("command" string :repeating t))
  "Usage: time command ...
Shows some time statistics resulting from the execution of COMMNAD."
  (time (shell-eval *shell* (make-shell-expr :words command))))

(defun print-timeval (tv &optional (stream t))
  (let* ((secs  (+ (timeval-seconds tv)
		   (/ (timeval-micro-seconds tv) 1000000)))
	 days hours mins)
    (setf days  (/ secs (* 60 60 24))
	  secs  (mod secs (* 60 60 24))
	  hours (/ secs (* 60 60))
	  secs  (mod secs (* 60 60))
	  mins  (/ secs 60)
	  secs  (mod secs 60))
    ;; (format t "days ~a hours ~a min ~a sec ~a~%"
    ;; 	    (floor days) (floor hours) (floor mins) secs)
    (format stream
	    "~@[~dd ~]~@[~dh ~]~@[~dm ~]~5,3,,,'0fs"
            (when (>= days 1) (floor days))
            (when (>= hours 1) (floor hours))
            (when (>= mins 1) (floor mins))
            secs)))

(defbuiltin times ()
  "Usage: times
Show accumulated times for the shell."
  (let ((self (getrusage :SELF))
	(children (getrusage :CHILDREN)))
    (format t "Self     User: ~a~32tSys: ~a~%"
	    (print-timeval (rusage-user self) nil)
	    (print-timeval (rusage-system self) nil))
    (format t "Children User: ~a~32tSys: ~a~%"
	    (print-timeval (rusage-user children) nil)
	    (print-timeval (rusage-system children) nil))))

(defbuiltin umask
    (("print-command" boolean :short-arg #\p)
     ("symbolic"      boolean :short-arg #\S)
     ("mask"	     string))
  "Set or print the default file creation mode mask (a.k.a. permission mask). If mode is not given, print the current mode. If PRINT-COMMAND is true, print the mode as a command that can be executed. If SYMBOLIC is true, output in symbolic format, otherwise output in octal."
  (declare (ignore symbolic)) ;; @@@
  (if (not mask)
      ;; printing
      (let ((current-mask (nos:umask 0)))
	(nos:umask current-mask)
	(when print-command
	  (format t "umask "))
	;; (if symbolic
	;;     (format t "~a~%" (symbolic-mode-offset current-mask))
	;;     (format t "~o~%" current-mode)))
	(format t "~o~%" current-mask))
      ;; setting
      (progn
	(multiple-value-bind (real-mask err)
	    (ignore-errors (parse-integer mask :radix 8))
	  (when (typep err 'error)
	    (error err))
	  (nos:umask real-mask)))))

(defbuiltin ulimit ())
(defbuiltin wait ())

(defbuiltin exec (("command-words" t :repeating t))
  "Replace the whole Lisp system with another program. This seems like a rather drastic thing to do to a running Lisp system."
  (when command-words
    (let ((path (command-pathname (first command-words))))
      (format t "path = ~w~%command-words = ~w~%" path command-words)
      (nos:exec path command-words))))

(defbuiltin bind
    (("print-bindings"		 boolean      :short-arg #\p)
     ("print-readable-bindings"	 boolean      :short-arg #\P)
     ("query"			 function     :short-arg #\q)
     ("remove-function-bindings" function     :short-arg #\u)
     ("remove-key-binding"	 key-sequence :short-arg #\r)
     ("key-sequence"		 key-sequence)
     ("function-name"		 function))
  "Manipulate key bindings."
  (when (> (count t (list print-bindings print-readable-bindings query
			  remove-function-bindings remove-key-binding)) 1)
    (error "Mutually exclusive arguments provided."))
  (cond
    (print-bindings
     (keymap:dump-keymap tiny-rl:*normal-keymap*))
    (print-readable-bindings
     (keymap:map-keymap
      #'(lambda (key val)
	  (format t "(keymap:define-key tiny-rl:*normal-keymap* ~w '~a)~%"
		  key val))
      tiny-rl:*normal-keymap*))
    ;; @@@ todo: query remove-function-bindings remove-key-binding
    ((and key-sequence (not function-name))
     (format t "~w: ~(~a~)~%" key-sequence
	     (keymap:key-sequence-binding
	      key-sequence tiny-rl:*normal-keymap*)))
    (query
     (if (not function-name)
	 (error "Missing function name.")
	 (keymap:map-keymap
	  #'(lambda (key val)
	      (when (equal val function-name)
		(format t "~w: ~a~%" key val)))
	  tiny-rl:*normal-keymap*)))
    ((and key-sequence function-name)
     (keymap:set-key key-sequence function-name tiny-rl:*normal-keymap*))))

#| Actually I think this is ill advised.

;; This is really just for simple things. You should probably use the ;
;; Lisp version instead.		;

;; @@@ This is what I would like to be able to say: ;
  @ defcommand tf ((file filename :optional nil)) (! "file" ($$ "type -p" file)) 

  (defbuiltin defcommand
(("name"     string :optional nil)
("function" string :optional nil))
"Defines a command which calls a function."
(let (;(func-name (command-function-name name)) ;
(cmd-name (string-downcase name))
(func-symbol (let ((*read-eval* nil))
(read-from-string (string-upcase function))))
(cmd-symbol (intern (string name))))
(if (fboundp func-symbol)
(progn
(push cmd-symbol *command-list*)
(set-command cmd-name
(make-instance 'command
:name cmd-name
:function func-symbol
:arglist '())))
(format t "~a is not a function" func-symbol))))
  |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *lish-user-package*
  (make-package "LISH-USER" :use '(:cl :lish :cl-ppcre :glob)
		:nicknames '("LU"))
  "Package for lish to hang out in. Auto-updates from :cl-user.")

;; The "result" argument is not for the caller, but rather so we can detect
;; cycles in the package inheritance graph.
(defun flattened-package-use-list (package &optional result)
  (loop :for p :in (package-use-list package) :do
     (when (not (position p result))
       (push p result)
       (loop :for ip :in (flattened-package-use-list p result) :do
	  (pushnew ip result))))
  result)

;; This tries to keep :lish-user up to date with respect to :cl-user.
(defun update-user-package ()
  ;; Update uses
  (loop :with isym :and isymbol-type :and esym :and esymbol-type
     :for p :in (package-use-list :cl-user) :do
     (when (not (position p (flattened-package-use-list *lish-user-package*)))
       (dbug "Package ~w~%" p)
       ;; Things directly in lish-user are uninterned in favor of one
       ;; in cl-user.
       (unintern-conflicts *lish-user-package* p)
       ;; Conflicts in inherited symbols are resolved by having the "explicitly"
       ;; used package symbol (i.e. things used by :lish-user such as :lish)
       ;; interned and made shadowing.
       (do-symbols (sym p)
	 (setf (values esym esymbol-type)
	       (find-symbol (symbol-name sym) p)
	       (values isym isymbol-type)
	       (find-symbol (symbol-name sym) *lish-user-package*))
	 (when (not (equal esym isym))
	   (case isymbol-type
	     ((:internal :external)
	      (dbug "CONFLICT ~w ~w ~w~%" p (symbol-name sym) isymbol-type)
	      (shadow isym *lish-user-package*))
	     (:inherited
	      (when (not (eq (symbol-package esym) (symbol-package isym)))
		(dbug "CONFLICT ~w ~w ~w~%" p (symbol-name sym) isymbol-type)
		(shadowing-import isym *lish-user-package*))))))
       (use-package p *lish-user-package*)))
  ;; Update all symbols
  (do-symbols (sym :cl-user)
    ;; @@@ deal with conflicts between imported symbols from different packages
    ;; @@@ keep symbols from packages used directly by :lish-user
    (when (not (find-symbol (symbol-name sym) *lish-user-package*))
      (import sym *lish-user-package*)))
  ;; Export exported symbols
  (do-external-symbols (sym :cl-user)
    (export sym *lish-user-package*)))

(defvar *lish-level* nil
  "Number indicating the depth of lish recursion. Corresponds to the ~
LISH_LEVEL environment variable.")
(declaim (special *lish-level*))

(defun fixed-homedir ()
  "(user-homedir-pathname) with a trailing slash removed if there was one."
  (let ((h (namestring (user-homedir-pathname))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (namestring (fixed-homedir)) "~"
		  (namestring name) :count 1))

;; I personally favor the prompt function, so this is basically a ploy to get
;; people that just want their bash prompt to work, to use my shell.

(defun format-prompt (sh prompt &optional (escape-char #\%))
  "Return the prompt string with bash-like formatting character replacements.
So far we support:
%%	A percent.
%a	#\bell
%e	#\escape
%n	#\newline
%r	#\return
%NNN	The character whose ASCII code is the octal value NNN.
%s	The name of the shell, which is usually “Lish”.
%v	Shell version.
%V	Even more shell version.
%u	User name.
%h	Host name truncated at the first dot.
%H	Host name.
%w	Working directory, tildified.
%W	The basename of `$PWD', tildified.
%$	If the effective UID is 0, `#', otherwise `$'.
%d      <3 char weekday> <3 char month name> <date>.
%t	24 hour HH:MM:SS
%T	12 hour HH:MM:SS
%@	The time, in 12-hour am/pm format.
%A	The time, in 24-hour HH:MM format.
Not implemented yet:
%!	The history number of this command.
%#	The command number of this command.
%[	Start of non-printing characters.
%]	End of non-printing characters.
%l	The basename of the shell's terminal device name.
%D{FORMAT}
	Some date formated by Unix strftime. Without the FORMAT just put some
	locale-specific date.
%j	The number of jobs currently managed by the shell.
"
  (declare (ignore sh))
  (with-output-to-string (str)
    (loop :with c :for i :from 0 :below (length prompt) :do
       (setf c (aref prompt i))
       (if (equal c escape-char)
         (progn
	   (incf i)
	   (when (< i (length prompt))
	     (setf c (aref prompt i))
	     (case c
	       (#\% (write-char escape-char str))
	       (#\a (write-char #\bell str))
	       (#\e (write-char #\escape str))
	       (#\n (write-char #\newline str))
	       (#\r (write-char #\return str))
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		(write-char (code-char
			     (parse-integer (subseq prompt i (+ i 3)) :radix 8))
			    str))
	       (#\s (write-string *shell-name* str))
	       (#\v (write-string *major-version* str))
	       (#\V (write-string *version* str))
	       (#\u (write-string (nos:getlogin) str))
	       (#\h (write-string dlib:*host* str))
	       (#\H (write-string (machine-instance) str))
	       (#\w (write-string (twiddlify (nos:current-directory)) str))
	       (#\W (write-string
		     (twiddlify (basename (nos:current-directory))) str))
	       (#\$ (write-char (if (= (nos:geteuid) 0) #\# #\$) str))
	       (#\d (write-string
		     (format-date "~3a ~3a ~2d"
				  (:day-abbrev :month-abbrev :date)) str))
	       (#\t (write-string
		     (format-date "~2,'0d:~2,'0d:~2,'0d"
				  (:hours :minutes :seconds)) str))
	       (#\T (write-string
		     (format-date "~2,'0d:~2,'0d:~2,'0d"
				  (:12-hours :minutes :seconds)) str))
	       (#\@ (write-string
		     (format-date "~2,'0d:~2,'0d ~2a"
				  (:12-hours :minutes :am)) str))
	       (#\A (write-string
		     (format-date "~2,'0d:~2,'0d" (:hours :minutes)) str))
	       )))
	 (write-char c str)))))

(defgeneric make-prompt (shell)
  (:documentation "Return a string to prompt with."))
(defmethod make-prompt ((sh shell))
  "Return a string to prompt with."
  (format nil "~a " (make-string (+ 1 *lish-level*)
				 :initial-element (lish-prompt-char sh))))

(defparameter *real-eof-symbol* :Z-REAL-EOF)
(defparameter *continue-symbol* :Z-CONTINUE)
(defparameter *empty-symbol* :Z-EMPTY)
(defparameter *error-symbol* :Z-ERROR)
(defparameter *quit-symbol* :Z-QUIT)

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words
  word-start
  word-end
  word-quoted
  line)

(defstruct lisp-expression
  "Nothing fancy. Just a wrapper for a lisp value for now."
  object)

(defun in-shell-word (exp word-num position)
  (declare (type shell-expr exp)
	   (type number word-num position))
  "Return true if the POSITION is in the shell word numbered WORD-NUM."
  (and (>= position (elt (shell-expr-word-start exp) word-num))
       (<= position (elt (shell-expr-word-end   exp) word-num))))

(defun shell-word-number (exp pos
			  &key (exp-len (length (shell-expr-words exp))))
  (declare (type shell-expr exp))
  "Return the shell expression's word number that position POS is in."
;  (with-slots (word-start word-end) exp
  (loop :for w :from 0 :below exp-len
#|
  :when (and (>= pos (elt (shell-expr-word-start exp) w))
     (<= pos (elt (shell-expr-word-end exp) w))) 
  |#
     :when (in-shell-word exp w pos)
     :return w))

(defun read-string (s)
  "Read a lish string. It has similar syntax to a lisp string. ~
Assumes the opening double quote has already been read. ~
Read until a double quote. Backslash escapes the special meaning of ~
the following character. Return the string and how long it is. If we got to ~
the end and didn't get a close quote the third value is true.~
"
  (let ((v (make-stretchy-string 10))
	(i 0)
	(end-quote nil)
	(do-quote nil))
    (loop :for c :across s :do
       (setf end-quote (and (eql c #\") (not do-quote)))
       :while (not end-quote)
       :do
       (if (and (eql c #\\) (not do-quote))
	   (setf do-quote t)
	   (progn
	     (setf do-quote nil)
	     (vector-push-extend c v)))
       (incf i))
    (values v i (not end-quote))))

;; I'm not so old fashioned that I think ^L should be in here, but are there
;; any other unicode things that should?
(defparameter *whitespace* #(#\space #\newline #\tab #\return)
  "Word separators for lish.")

(defun contains-whitespace-p (s)
  (position-if #'(lambda (x) (position x *whitespace*)) s))

;; Previously this was a method so you could make a speicalized one for
;; different shell reader syntax, but since it doesn't use anything in the
;; shell object and you might want to call it without having that, I made
;; it into a function. If we really need to, we can make a wrapper method.

(defun shell-read (line &key partial (package *lish-user-package*))
  "Read objects in shell syntax and return them. If PARTIAL is true, don't 
signal an error if we can't read a full expression.
The syntax is vaguely like:
  ; comment
  command [arg...]
  command \"string\" !*lisp-object* (lisp-code)
  command word\ with\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])"
  (let (word-start word-end word-quoted words
	(c nil)				; current char
	(i 0)				; index in line
	(len (length line))
	(args '())
	(w (make-stretchy-string 12))	; temp word
	(in-word nil)			; t if in word
	(do-quote nil)			;
	(did-quote nil))		;
    (labels ((finish-word ()
	       "Finish the current word."
	       (when in-word
		 (push (copy-seq w) args)
		 (push i word-end)
		 (push did-quote word-quoted)
		 (setf (fill-pointer w) 0
		       in-word nil
		       did-quote nil)))
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (progn
		     (push i word-start)
		     (push (subseq line i) args)
		     (push (length line) word-end)
		     (push nil word-quoted)
		     (return-from shell-read
		       (make-shell-expr
			:line line
			:words (nreverse args)
			:word-start (reverse word-start)
			:word-end (nreverse word-end) 
			:word-end (nreverse word-quoted))))
		   (return-from shell-read *continue-symbol*)))
	     (reverse-things ()
	       "Reverse the things we've been consing, so they're in order."
	       (setf word-start  (reverse word-start)
		     word-end    (nreverse word-end)
		     word-quoted (nreverse word-quoted)
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy the lists."
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted))))
      (loop
	 :named tralfaz
	 :while (< i len)
	 :do
	 (setf c (aref line i))
	 (cond
	   ;; quoted char
	   (do-quote
	       (vector-push-extend c w)
	     (when (not in-word)
	       (push (1- i) word-start))
	     (setf in-word t)
	     (setf do-quote nil)
	     (setf did-quote t)
	     (incf i))
	   ;; a string
	   ((eql c #\")
	    (finish-word)
	    ;; read a string as a separate word
	    (multiple-value-bind (str ink cont)
		(read-string (subseq line (1+ i)))
	      (when (and cont (not partial))
		(return-from shell-read *continue-symbol*))
	      (push i word-start)
	      (push str args)
	      (incf i (+ 2 ink))
	      (push i word-end)
	      (push t word-quoted)))
	   ;; a lisp function application
	   ((eql c #\()
	    (finish-word)
	    (handler-case
		;; read a form as a separate word
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil *continue-symbol* :start i))
		  (push i word-start)
		  (setf i pos)
		  (push obj args)
		  (push i word-end)
		  (push nil word-quoted))
;;	      (end-of-file ()
	      (error () (do-continue))
	      (condition (c) (signal c))))
	   ;; a lisp expr
	   ((eql c #\!)
	    (finish-word)
	    ;; read a form as a separate word
	    (handler-case
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil *continue-symbol*
					:start (+ i 1)))
		  (push i word-start)
		  (setf i pos)
		  (push i word-end)
		  (push nil word-quoted)
		  (push obj args))
	      (error () (do-continue))
;	      (end-of-file () (do-continue))
	      (condition (c) (signal c))))
	   ;; quote char
	   ((eql c #\\)
	    (setf do-quote t)
	    (incf i))
	   ;; whitespace
	   ((position c *whitespace*)
	    (finish-word)
	    (incf i))
	   ;; comment
	   ((eql c #\;)
	    (finish-word)
	    (loop :for j :from i :below len
	       :while (not (eql (aref line j) #\newline))
	       :do (incf i)))
	   ;; pipe
	   ((eql c #\|)
	    (finish-word)
	    (reverse-things)
	    (let ((e (list :pipe (make-the-expr))))
	      (setf args (list e)))
	    (setf word-start (list i))
	    (incf i)
	    (setf word-end (list i)
		  word-quoted (list nil)))
	   ;; redirect
	   ((or (eql c #\<) (eql c #\>))
	    (finish-word)
	    (reverse-things)
	    ;; @@@ need to get the file name as a word
	    (let ((e (list :redirect (make-the-expr))))
	      (setf args (list e)))
	    (setf word-start (list i))
	    (incf i)
	    (setf word-end (list i)
		  word-quoted (list nil)))
	   ;; any other character: add to word
	   (t
	    (when (not in-word)
	      (push i word-start))
	    (setf in-word t)
	    (vector-push-extend c w)
	    (incf i)))
        :finally
	(progn
	  (when in-word
	    (push (copy-seq w) args)
	    (push i word-end)
	    (push did-quote word-quoted))
	  (reverse-things)))
      (if (and (= (length words) 1) (consp (first words)))
	  ;; just a lisp expression to be evaluated
	  (first words)
	  ;; a normal shell expression
	  (make-the-expr)))))

(defparameter *command-cache* nil
  "A hashtable which caches the of full names of commands.")

(defun is-executable (s)
  (logand (file-status-mode s) S_IXUSR))

(defun is-regular (s)
  (logand (file-status-mode s) S_IXUSR))

(defun is-regular-executable (p)
  (let ((st (stat p)))
    (and st (is-executable st) (is-regular st))))

(defun has-directory-p (p)
  (position *directory-separator* p))

(defun command-pathname (cmd)
  "Return the full pathname of the first executable file in the PATH or nil
if there isn't one."
  (when (has-directory-p cmd)
    (return-from command-pathname cmd))
  (loop :for dir :in (split-sequence *path-separator* (getenv "PATH")) :do
	(when (probe-directory dir)
	  (loop :with full = nil
		:for f :in (read-directory :dir dir) :do
		(when (and (equal f cmd)
			   (is-regular-executable
			    (setf full
				  (format nil "~a~c~a"
					  dir *directory-separator* cmd))))
		  (return-from command-pathname full)))))
  nil)

(defun command-paths (cmd)
  "Return all possible command paths. Don't cache the results."
  (loop :with r = nil
    :for dir :in (split-sequence *path-separator* (getenv "PATH"))
    :do
    (setf r (when (probe-directory dir)
	      (loop :with full = nil
		    :for f :in (read-directory :dir dir)
		    :when (and (equal f cmd)
			       (is-regular-executable
				(setf full
				      (format nil "~a~c~a"
					      dir *directory-separator* cmd))))
		    :return full)))
    :if r
    :collect r))


(defun get-command-path (cmd)
  "Return the possibly cached command path."
  (when (not *command-cache*)
    (setf *command-cache* (make-hash-table :test #'equal)))
  (let ((result (gethash cmd *command-cache*)))
    (when (not result)
      (let ((path (command-pathname cmd)))
	(when path
	  (setf (gethash cmd *command-cache*) path
		result path))))
    result))

(defbuiltin hash
    (("rehash" boolean :short-arg #\r)
     ("commands" t :repeating t))
  "Usage: hash [-r] [commands...]
Show remembered full pathnames of commands. If -r is given, forget them all."
  (labels ((pr-cmd (c) (format t "~a~%" c)))
    (if rehash
	(if commands
	    (loop :for c :in commands :do
	       (remhash c *command-cache*))
	    (setf *command-cache* nil))
	(when *command-cache*
	  (if commands
	      (loop :for c :in commands :do
		 (pr-cmd (gethash c *command-cache*)))
	      (maphash #'(lambda (c p) (declare (ignore c)) (pr-cmd p))
		       *command-cache*))))))

;; Since this is based on phonetics, we would need a phonetic English
;; dictionary to do this right.
(defun indefinite (str)
  (declare (type string str))
  "Return an approximately appropriate indefinite article for the given ~
string. Sometimes gets it wrong for words startings with 'U', 'O', or 'H'."
  (when (> (length str) 0)
    (let ((c (aref str 0)))
      (if (position c "aeiouAEIOU") "an" "a"))))

(defun command-type (sh command)
  "Return a string representing the command type of command."
  (cond
    ((gethash command (lish-commands))   "command")
    ((gethash command (lish-aliases sh)) "alias")
    ((get-command-path command)          "file")
    (t "")))

(defun describe-command (cmd)
  (let (x)
    (cond
      ((setf x (gethash cmd (lish-aliases *shell*)))
       (when x
	 (format t "~a is aliased to ~a~%" cmd x)))
      ((setf x (gethash cmd (lish-commands)))
       (when x
	 (format t "~a is the command ~a~%" cmd x)))
      ((setf x (get-command-path cmd))
       (when x
	 (format t "~a is ~a~%" cmd x)))
      ((setf x (read-from-string cmd))
       (when (and (symbolp x) (fboundp x))
	 (format t "~a is the function ~s~%" cmd (symbol-function x)))))))

(defbuiltin type
    (("type-only" boolean :short-arg #\t)
     ("path-only" boolean :short-arg #\p)
     ("all" 	  boolean :short-arg #\a)
     ("names" 	  string  :repeating t))
  "Describe what kind of command the name is."
  (when names
    (loop :with args = names :and n = nil
       :while args :do
       (setf n (car args))
       (cond
	 (path-only
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t "~a~%" (first paths)))))
	 (all
	  (let ((x (gethash n (lish-aliases *shell*))))
	    (when x
	      (format t "~a is aliased to ~a~%" n x)))
	  (let ((x (gethash n (lish-commands))))
	    (when x
	      (format t "~a is the command ~a~%" n x)))
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t (format nil "~~{~a is ~~a~~%~~}" n)
		      paths)))
	  (let* ((obj (read-from-string n)))
	    (when (and (symbolp obj) (fboundp obj))
	      (format t "~a is the function ~s~%" n (symbol-function obj)))))
	 (t
	  (let ((tt (command-type *shell* n)))
	    (when tt
	      (if type-only
		  (format t "~a~%" tt)
		  (describe-command n))))))
	 (setf args (cdr args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job control

(defun start-job-control ()
  (setf (signal-action nos::SIGTSTP) :ignore
	(signal-action nos::SIGTTIN) :ignore
	(signal-action nos::SIGTTOU) :ignore))

(defun stop-job-control (saved-sigs)
  (let ((tstp (first saved-sigs))
	(ttin (second saved-sigs))
	(ttou (third saved-sigs)))
    (setf (signal-action nos::SIGTSTP) (if (keywordp tstp) tstp :default)
	  (signal-action nos::SIGTTIN) (if (keywordp tstp) ttin :default)
	  (signal-action nos::SIGTTOU) (if (keywordp tstp) ttou :default))))

(defun set-default-job-sigs ()
  (setf (signal-action nos::SIGTSTP) :default
	(signal-action nos::SIGTTIN) :default
	(signal-action nos::SIGTTOU) :default))

;(defun run (cmd args)
  ; block sigchld & sigint
  ; give terminal to child if not running it bg?
  ; fork
  ; in the child:
  ;   unblock sigchld & sigint
  ;   set default action tty signals (TSTP, TIN, TOU)
  ;     or ignore them if not going to be in the foreground
  ;   set the process group setpgid to it's own pid (or group of the pipeline)
  ;   give terminal to child's process group
  ;   exec
  ; in the parent:
  ;   just to be sure:
  ;     set the child's process group (setpgid) to it's own pid
  ;     (or group of the pipeline)
  ;   unblock sigchld & sigint
  ;   wait for the child
;  )

; (defun set-terminal-group (tty group)
;   "Make the terminal TTY be controled by process group GROUP."
;   ;block TTOU TTIN TSTP & CHLD while we do this:
;   (tcsetpgrp tty group))

; (defun init-job-control (sh)
;   (let ((our-process-group (getpgid 0))
; 	(tty-process-group (tcgetpgrp tty))
; 	(our-pid (getpid)))
;     (loop :while (/= our-process-group tty-process-group)
; 	  :do 
; 	  ;; If we're not the foreground process
; 	  ;; Signal the process group that we want input, which will likely
; 	  ;; stop us. Keep demanding the tty until we get it or die.
; 	  (kill SIGTTIN)
; 	  (setf tty-process-group (tcgetpgrp tty)))
;     ;; If for some reason we're not the process group leader,
;     ;; then become it, and take control of the terminal.
;     (when (/= our-process-group our-pid)
;       (setpgid 0 our-pid)
;       (set-terminal-group our-pid))))

(defun in-lisp-path (command)
  "Return true if a command is in the lisp path."
  ;; (loop :with path
  ;;    :for dir :in *lisp-path* :do
  ;;    (when (setf path (probe-file (s+ dir command)))
  ;;      (asdf::resolve-symlinks path))))	; XXX I know, this is cheating.
  (ignore-errors (asdf:find-component nil command)))
;  (asdf:find-component nil command))

(defun load-lisp-command (command)
  "Load a command in the lisp path."
  (let* ((pkg (intern (string-upcase command) :keyword)))
    (if (ignore-errors (asdf:oos 'asdf:load-op pkg :verbose nil))
	;; succeeded
	(progn 
	  ;; (init-commands sh)
	  (get-command command))
	;; failed
	nil)))

(defun do-system-command (command-line &optional in-pipe out-pipe)
  "Run a system command. IN-PIPE is an input stream to read from, if non-nil.
OUT-PIPE is T to return a input stream which the output of the command can be
read from."
  ;; Since run-program can't throw an error when the program is not found, we
  ;; try to do it here.
  (let* ((program (car command-line))
	 (args    (cdr command-line))
	 (path    (get-command-path program))
	 result result-stream)
    (if (not path)
	(error "~a not found." program)
	(progn
	  (set-default-job-sigs)
	  (cond
	    (out-pipe
	     (setf result-stream
		   (if in-pipe
		       (nos:popen path args :in-stream in-pipe)
		       (nos:popen path args))))
	    (in-pipe
	     (nos:popen path args :in-stream in-pipe :out-stream t))
	    (t
	     (setf result
		   #+(or clisp ecl cmu lispworks) (fork-and-exec path args)
		   #+sbcl (nos:run-program path args)	;@@@ until fork fixed
		   #+ccl (nos:system-command path args)	;@@@ until fork fixed
		   )))))
    (values (or result '(0)) result-stream)))

(defun unquoted-string-p (w expr i)
  "True if W in EXPR with index I is _not_ quoted."
  (and (stringp w) (> (length w) 0)
       (and (shell-expr-word-quoted expr)
	    (not (elt (shell-expr-word-quoted expr) i)))))

(defun do-expansions (expr pos)
  "Perform shell syntax expansions / subsitutions on the expression."
  (let ((new-words '()))
    (loop
       :for w :in (shell-expr-words expr)
       :for i = 0 :then (1+ i)
       :do
       (if (unquoted-string-p w expr i)
	 (cond
	   ;; $ environment variable expansion
	   ((eql #\$ (aref w 0))
	    (let ((v (nos:getenv (subseq w 1))))
	      (push (or v "") new-words)))
	   ;; filename globbing, with ~ expansion on
	   ((glob:pattern-p w nil t)
	    (let ((g (glob:glob w :tilde t)))
	      (if g
		(dolist (x g) (push x new-words))
		;; If there's no existing file expansions, but try just twiddle,
		;; and also keep the glob expression if no matches.
		(push (glob:expand-tilde w) new-words))))
	   (t (push w new-words)))
	 ;; Quoted, so just push it verbatim
	 (push w new-words)))
    (setf (shell-expr-words expr) (nreverse new-words)))
  pos)

(defun lisp-exp-eval (words)
  "Evaluate lisp expr in words."
;  (format t "Evaling ~w~%" words)
  (loop :with results
     :for e :in words
     :if (or (consp e) (symbolp e))
       :do (setf results (eval e))
       :and :if (listp results)
         :append results	      ; Spread list results into separate args
       :else
         :collect results
     :else
        :collect e))

(defun expand-alias (sh alias words in-pipe out-pipe)
  (let* ((expr (shell-read alias))
	 (new-expr
	  ;; XXX This trashes the rest of the things in the expr, like
	  ;; the quoted, etc. which could cause problems.
	  (make-shell-expr
	   :words (append (shell-expr-words expr) (cdr words))
	   :line (format nil "~s ~{~s ~}" (shell-expr-line expr) words))))
    (shell-eval sh new-expr :no-alias t :in-pipe in-pipe :out-pipe out-pipe)))

(defun do-command (command args &optional in-pipe out-pipe)
  "Call a command with the given POSIX style arguments."
  (labels ((runky (command args)
	     (let ((lisp-args (posix-to-lisp-args command args))
		   (cmd-func (symbol-function (command-function command))))
	       (if (> (length lisp-args) 0)
		   (apply cmd-func lisp-args)
		   (funcall cmd-func)))))
    (if out-pipe
	(let ((out-str (make-stretchy-string 20)))
	  (values
	   (list (with-output-to-string (*standard-output* out-str)
		   (if in-pipe
		       (let ((*standard-input* in-pipe))
			 (runky command args))
		       (runky command args))))
	   (make-string-input-stream out-str)
	   nil))
	(if in-pipe
	    (let ((*standard-input* in-pipe))
	      (runky command args))
	    (runky command args)))))

(defun read-parenless-args (string)
  "Read and shell-eval all the expressions possible from a string and return
them as a list."
  (loop :with start = 0 :and expr
     :while (setf (values expr start)
		  (read-from-string string nil nil :start start))
     :collect (eval expr)))

(defun shell-eval-command (sh expr &key no-alias in-pipe out-pipe)
  "Evaluate an expression that is a command."
  (let* ((cmd (elt (shell-expr-words expr) 0))
	 #| (args (subseq (shell-expr-words expr) 1)) |#
	 (command (gethash cmd (lish-commands)))
	 (alias (gethash cmd (lish-aliases sh)))
	 (expanded-words (lisp-exp-eval (shell-expr-words expr)))
	 result result-stream)
    (dbug "words = ~w~%" (shell-expr-words expr))
    (dbug "expanded words = ~w~%" expanded-words)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (cond
      ;; Alias
      ((and alias (not no-alias))
       ;; re-read and re-eval the line with the alias expanded
       (expand-alias sh alias expanded-words in-pipe out-pipe))
      ;; Autoload
      ((and (in-lisp-path cmd)	
	    (setf command (load-lisp-command cmd)))
       ;; now try it as a command
       (do-command command (subseq expanded-words 1) in-pipe out-pipe))
      ;; Lish command
      (command			
       (do-command command (subseq expanded-words 1) in-pipe out-pipe))
      (t
       (flet ((sys-cmd ()
		"Do a system command."
		(setf (values result result-stream)
		      (do-system-command expanded-words in-pipe out-pipe))
		(dbug "result = ~w~%" result)
		(when (not result)
		  (format t "Command failed.~%"))
		(force-output)	   ; @@@ is this really a good place for this?
		(values result result-stream nil)))
	 ;; If we can find a command in the path, try it first.
	 (if (get-command-path (first expanded-words))
	     (sys-cmd)
	     ;; Otherwise try a parenless Lisp line.
	     (multiple-value-bind (symb pos)
		 (read-from-string (shell-expr-line expr) nil nil)
	       (if (and (symbolp symb) (fboundp symb))
		   (values
		    (multiple-value-list
		     (apply (symbol-function symb)
			    (read-parenless-args
			     (subseq (shell-expr-line expr) pos))))
		    nil ;; stream
		    t)	;; show the values
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd)))))))))

(defun shell-eval (sh expr &key no-alias in-pipe out-pipe)
  "Evaluate a shell expression. If NO-ALIAS is true, don't expand aliases.
Return a list of the result values, a stream or NIL, and a boolean which is T to show the values."
  (typecase expr
    (shell-expr
;     (format t "(shell-expr-words expr) = ~w~%" (shell-expr-words expr))
     (when (= (length (shell-expr-words expr)) 0)
       (return-from shell-eval (values nil nil nil)))
     (let ((w0 (elt (shell-expr-words expr) 0)))
       (do-expansions expr 0)
       (dbug "~w~%" expr)
       (if (listp w0)
	 (case (first w0)
	   (:pipe
	    (multiple-value-bind (vals out-stream show-vals)
		(shell-eval sh (second w0) :in-pipe in-pipe :out-pipe t)
	      (declare (ignore show-vals))
	      (when (and vals (> (length vals) 0))
		(with-package *lish-user-package*
		  (shell-eval-command
		   sh
		   (make-shell-expr
		    :words (cdr (shell-expr-words expr))
		    :line (format nil "~{~s ~}"
				  (cdr (shell-expr-words expr))))
		   :no-alias no-alias
		   :in-pipe out-stream
		   :out-pipe out-pipe)))))
	   (:and
	    )
	   (:or
	    )
	   (:sequence
	    ))
	 ;; Not a list
	 (with-package *lish-user-package*
	   (shell-eval-command sh expr
			       :no-alias no-alias
			       :in-pipe in-pipe :out-pipe out-pipe)))))
    (t ;; A full Lisp expression all by itself
     (with-package *lish-user-package*
       (values (multiple-value-list (eval expr)) nil t)))))

(defun load-rc-file (sh)
  "Load the users start up (a.k.a. run commands) file."
;  (without-warning
    (load-file sh (merge-pathnames
		   (user-homedir-pathname)
		   (make-pathname :name ".lishrc"))))

(defun load-file (sh file)
  (if (probe-file file)
      (with-open-file (streamy file :direction :input)
	(with-package *lish-user-package*
	  (loop :with line = nil :and newy-line = t :and expr = nil
	     :while (and (setf line (read-line streamy nil))
			 newy-line)
	     :do
;	     (format t "  rc> ~s~%" line)
	     (loop :while (and (eql (setf expr (shell-read line))
				    *continue-symbol*)
			       (setf newy-line (read-line streamy nil)))
		:do
		(setf line (format nil "~a~%~a" line newy-line))
;		(format t "cont> ~s~%" line) (force-output)
		)
;	     (format t "expr> ~s~%" expr)
	     (shell-eval sh expr))))))

;(defvar *shell-non-word-chars* " ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;

; (defun quoted-start (str pos)
;   "Check if we are inside a shell quoted string and return it's starting
;  position."
;   (

(defun complete-env-var (str all)
  ;; (complete-string-sequence
  ;;  str all (mapcar #'(lambda (x) (string (car x))) (nos:environ))))
  (complete-list str (length str) all
		 (mapcar #'(lambda (x) (string (car x))) (nos:environ))))

(defun complete-user-name (str all)
  (prog2
      (nos:setpwent)
      (complete-list str (length str) all
		     (loop :with p = nil
			:while (setf p (nos:getpwent))
			:collect (nos:passwd-name p)))
    (nos:endpwent)))

;; @@@ Consider caching this.
;; @@@ In fact we should probably require a "rehash", like other shells.
(defparameter *verb-list* nil
  "List of current lish commands. Includes aliases, built-in commands, and ~
exectuables in the path. Use the \"rehash\" command to update after new ~
commands are added.")

(defun probe-file-or-dir (p)
  (or (probe-directory p) (probe-file p)))

(defun verb-list (shell)
  (declare (type shell shell))
  "Return the command list for the current shell: *shell*."
  (if (not *verb-list*)
      (setf *verb-list*
	    (remove-duplicates
	     (append
	      (loop :for k :being :the :hash-keys :of (lish-aliases shell)
		 :collect k)
	      (loop :for k :being :the :hash-keys :of (lish-commands)
		 :collect k)
	      (loop :for dir :in (split-sequence #\: (nos:getenv "PATH"))
		 :if (probe-directory dir)
		 :append (loop :for f :in (nos:read-directory :dir dir :full t)
			    :if (eql (nos:dir-entry-type f) :regular)
			    :collect (nos:dir-entry-name f))))
	     :test #'equal))
      *verb-list*))

(defun complete-command (str all)
;  (complete-string-sequence str all (verb-list *shell*)
  (complete-list str (length str) all (verb-list *shell*)))

;; This is mostly like complete-symbol but it handles the ! at the beginning.
;; XXX Uses completion internals.
(defun complete-bang-symbol (context pos all)
  "Completion function for symbols (preceded by ! in the shell)."
  (let* ((word-start (completion::scan-over-str
		      context pos :backward
		      :not-in completion::*lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil))
;    (format t "Howdy: word-start ~s word ~s~%" word-start word)
    (when (eql #\! (aref word 0))
      (setf word (subseq word 1)
	    word-start (1+ word-start)))
    (multiple-value-setq (pack external)
      (completion::find-back-pack context word-start))
    (if all
	(completion::symbol-completion-list
	 word :package pack :external external)
	(values (completion::symbol-completion
		 word :package pack :external external) word-start))))

(defun quotify (string)
  "Put a backslash in front of any character that might not be intrepreted
literally in shell syntax."
  (let ((result string))
    (flet ((possibly-quote (c)
	     (when (position c result)
	       (setf result (join (split-sequence c result) (s+ #\\ c))))))
      (loop :for c :across " !$|;[]*?()" :do ;
  (possibly-quote c))
      result)))

(defvar *junk-package*
  (progn
    (when (find-package :lish-junk)
      (delete-package :lish-junk))
    (make-package :lish-junk)))

;; Remember, a completion functions returns:
;;   One completion: completion and replacement starting position
;;   List:           sequence and sequence length

;; If we can't do at least a good as Tops-20 COMND% JSYS, then we suck.
;;
;; I dream and aspire to be as good as or better than CP, "The Command
;; Processor Reader" on Symbolics Genera, but since I never really used it, I
;; won't know, will I. Anyway, we're ostensibly driving Unix underneath, which
;; is a bit of a different beasty.
;;
;; I designed this thing without really knowing about the Symbolics stuff but
;; it turns out I made something quite similar. It seems like having something
;; like "defcommand" and a reader which does the right thing based on it, and
;; which can basicly prompt for lambda expressions is an inherently Lispy way
;; to do it. But the part with translating to Unix style is really quite
;; crufty.

(defun shell-complete (context pos all)
  (declare (type string context))
  "Analyze the context and try figure out what kind of thing we want to ~
complete, and call the appropriate completion function."
  (let ((exp (ignore-errors (shell-read context :partial t
					:package *junk-package*))))
    (typecase exp
      (shell-expr
       (let* ((word-num (shell-word-number exp pos))
	      (word     (if word-num
			    (elt (shell-expr-words exp) word-num))))
	 (dbug "~%word-num = ~w word = ~w~%exp ~w~%" word-num word exp)
	 (flet ((simple-complete (func word wpos)
		  (if all
		      (let ((list (funcall func word all)))
			(values list (length list)))
		      (values (funcall func word all) wpos))))
	   (cond
	     ((not word-num)		; no words
	      (simple-complete #'complete-command "" 0))
	     ((not word)		; probably ()
	      (complete-symbol context pos all))
	     ((symbolp word)
	      (complete-bang-symbol context pos all))
	     ((consp word)		; (foo)
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\()	; (foo
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\!)	; !foo
	      (complete-bang-symbol context pos all))
	     ((eql (aref word 0) #\$)	; $foo
	      (simple-complete #'complete-env-var
			       (subseq word 1)
			       (1+ (elt (shell-expr-word-start exp)
					word-num))))
	     ((and (eql (aref word 0) #\~) ; ~foo
		   (valid-user-name (subseq word 1)))
;;;	      (format t "CHING! ~a~%" (valid-user-name word))
	      (simple-complete #'complete-user-name
			       (subseq word 1)
			       (1+ (elt (shell-expr-word-start exp)
					word-num))))
	     ;; first word, when not starting with directory chars
	     ((and (= word-num 0) (not (position (aref word 0) "/.~")))
	      ;; try commands
	      (multiple-value-bind (v1 v2)
		  (simple-complete #'complete-command context
				   (elt (shell-expr-word-start exp) 0))
;		(format t "CMD v1=~s v2=~s~%~%" v1 v2)
		;; then symbols
		(when (not v1)
		  (setf (values v1 v2)
			(complete-symbol context pos all))
;		  (format t "SYM v1=~s v2=~s~%~%" v1 v2)
		  )
		(values v1 v2)))
	     (t
	      (let ((from-end (- (length context) pos)))
		(multiple-value-bind (result new-pos)
		    (complete-filename word (- (length word) from-end) all)
		  (declare (ignore new-pos))
		  (values (if (not all) (quotify result) result)
			  (elt (shell-expr-word-start exp) word-num)))))))))
      (cons
       (complete-symbol context pos all)))))

(defvar *shell-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\/ #\" #\' #\\ #\# #\, #\` #\| #\.
    #\- #\$ #\~ #\! #\&)
  "Characters that are not considered to be part of a word in the shell.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; Just the state of the REPL-y area to make it easy to pass around.
(defstruct read-state
  "The line we've read and the previous line."
  string
  prefix-string)

(defun lish-read (sh state)
  "Read a string with the line editor and convert it shell expressions, handling errors."
  (with-slots ((str string) (pre-str prefix-string)) state
    (handler-case
	(handler-bind
	    (#+sbcl
	     (sb-sys:interactive-interrupt
	      #'(lambda (c)
		  (declare (ignore c))
		  (format t "~%") (finish-output)
		  (invoke-restart (find-restart 'abort))))
	     (condition #'(lambda (c)
			    (if (lish-debug sh)
				(invoke-debugger c)
				(format t "~&~a" c))))
	     (error #'(lambda (c)
			(if (lish-debug sh)
			    (invoke-debugger c)
			    (progn
			      #| (format t "~&~a" c) |#
			      (signal c))))))
	  (progn
	    (setf str (tiny-rl
		       :eof-value *real-eof-symbol*
		       :quit-value *quit-symbol*
		       :context :lish
		       :editor (lish-editor sh)
		       :prompt
		       (if pre-str
			   (lish-sub-prompt sh)
			   (funcall (lish-prompt-function sh) sh)))))
	  (cond
	    ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
	    ((equal str *real-eof-symbol*)		*real-eof-symbol*)
	    ((equal str *quit-symbol*)	  		*quit-symbol*)
	    (t (shell-read (if pre-str
			       (format nil "~a~%~a" pre-str str)
			       str)))))
      #+sbcl
      (sb-sys:interactive-interrupt ()
	(format t "~%") (finish-output)
	(invoke-restart (find-restart 'abort)))
      (end-of-file () *continue-symbol*)
      #| (condition (c) |#
      (error (c)
	(if (lish-debug sh)
	    (invoke-debugger c)
	    (format t "~&~a" c)
	    )
	*error-symbol*))))

(defun lish-eval (sh result state)
  "Evaluate the shell expressions in RESULT."
  (dbug "~s (~a) ~s~%" result (type-of result) (eq result *empty-symbol*))
  (with-slots ((str string) (pre-str prefix-string)) state
    (cond
      ((eq result *continue-symbol*)
       (if (stringp pre-str)
	   (setf pre-str (format nil "~a~%~a" pre-str str))
	   (setf pre-str (format nil "~a" str)))
       (dbug "DO CONTIUE!!~%"))
      ((or (eq result *empty-symbol*) (eq result *error-symbol*))
       ;; do nothing
       (dbug "DO NOTHING!!~%"))
      (t
       (dbug "Do Something!!~%")
       (setf pre-str nil)
       (handler-case
	   (handler-bind
	       (#+sbcl
		(sb-sys:interactive-interrupt
		 #'(lambda (c)
		     (declare (ignore c))
		     (format t "~%") (finish-output)
		     (invoke-restart (find-restart 'abort))))
		#| (warning
		 #'(lambda (c)
		     (format t "Warning: ~a~%" c)
		     (muffle-warning))) |#
		#| #+excl (excl::compiler-note
		    #'(lambda (c)
		(format t "Note: ~a~%" c))) |#
		(serious-condition
		 #'(lambda (c)
		     (if (lish-debug sh)
			 (invoke-debugger c)))))
	     (force-output)
	     (multiple-value-bind (vals stream show-vals)
		 (shell-eval sh result)
	       (declare (ignore stream))
	       (when show-vals
		 (loop :with len = (length vals) :and i = 0
		    :for v :in vals
		    :do
		    (format t "~s" v)
		    (if (and (> len 1) (< i (- len 1)))
			(format t " ;~%"))
		    (incf i)
		    :finally (format t "~&")))))
	 ;; (condition (c)
	 ;; 	 (if (lish-debug sh)
	 ;; 	     (invoke-debugger c)
	 ;; 	     (format t "GOO ~a~%" c)))
	 (error (c)
	   (if (lish-debug sh)
	       (invoke-debugger c)
	       (format t "~a~%" c))))))))

(defun lish (&key debug terminal-name)
  "Unix Shell & Lisp somehow smushed together."
  (let* ((*shell* (make-instance 'shell :debug debug))
	 (sh *shell*)		; shorthand
	 (state (make-read-state))
	 (*lish-level* (if *lish-level*
			   (funcall #'1+ (symbol-value '*lish-level*))
			   0))
	 (saved-sigs (list (signal-action nos::SIGTSTP)
			   (signal-action nos::SIGTTIN)
			   (signal-action nos::SIGTTOU))))
    (declare (special *shell*))	; XXX it's probably already special from defvar
    (update-user-package)
    (nos:setenv "LISH_LEVEL" (format nil "~d" lish::*lish-level*))
    (load-rc-file sh)
    ;; Make a customized line editor
    (setf (lish-editor sh)
	  (make-instance 'tiny-rl:line-editor
			 :non-word-chars *shell-non-word-chars*
			 :completion-func #'shell-complete
			 :context :lish
			 :terminal-device-name terminal-name
			 :prompt-func nil))
    (unwind-protect
	 (progn
	   (start-job-control)
	   (when (not (eq :lish-quick-exit (catch :lish-quick-exit
             (loop
		:named pippy
		:with result = nil :and lvl = *lish-level*
		:if (lish-exit-flag sh)
		  :return (values-list (lish-exit-values sh))
		:end
		:do
		(restart-case
		    (progn
		      (setf result (lish-read sh state))
		      (when (or (eq result *real-eof-symbol*)
				(eq result *quit-symbol*))
			(return-from pippy result))
		      (lish-eval sh result state))
		  (abort ()
		    :report
		    (lambda (stream)
		      (format stream
			      "Return to Lish ~:[~;TOP ~]level~:[~; ~d~]."
			      (= lvl 0) (/= lvl 0) lvl))
		    nil))))))))
      (stop-job-control saved-sigs))
    (when (lish-exit-flag sh)
      (return-from lish (when (lish-exit-values sh)
			  (values-list (lish-exit-values sh)))))
    (format t "*EOF*~%")
    ;; Well, let's hope that this will clear the EOF on *standard-input*
    (clear-input *standard-input*)))

(defvar *standalone* nil
  "True if we are nearly just a shell.") ; [sic]

(defun flash-msg (msg)
  "Temporarity flash a message, nearly subliminally."
  (format t msg) (finish-output) (sleep .2)
  (format t "~v,,,va" (length msg) #\backspace #\backspace)
  (finish-output) (sleep .1)
  (format t "~v,,,va" (length msg) #\space #\space)
  (format t "~v,,,va" (length msg) #\backspace #\backspace))

(defun lishity-split ()
  "Get out real quick."
  (if *standalone*
      (flash-msg "You the man now dog."))
      (throw :lish-quick-exit :lish-quick-exit))

(defun shell-toplevel (&key debug)
  "For being invoked as a standalone shell."
  (setf *standalone* t)
  (let* ((level-string (nos:getenv "LISH_LEVEL")))
    (when level-string
      (setf *lish-level* (parse-integer level-string)))
    (lish :debug debug))
  (nos:exit-lisp))

;; So, like, to do it cleanly, for me:
;;   $LISP -- -norl
;;   (l :tiny-repl)
;;   (l :lish)
;;   (lish:make-standalone)
;; where LISP can be either sbcl or clisp.
;; @@@ what about ccl?

; (defun make-lish ()
;   (

(defun make-standalone (&optional (name "lish"))
  "FUFKFUFUFUFUFF"
  #+sbcl (sb-ext:save-lisp-and-die name :executable t
				   :toplevel #'lish:shell-toplevel)
  #+clisp (ext:saveinitmem name :executable t :quiet t :norc t
			   :init-function #'lish:shell-toplevel)
  #-(or sbcl clisp) (declare (ignore name))
  #-(or sbcl clisp) (missing-implementation 'make-standalone)
  )

;;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;|| Piping
;;||
;;|| Piping, I/O redirection, and I/O functions that are useful for using in
;;|| a lish command line or script.

(defun lisp-args-to-command (args &key (auto-space nil))
  "Turn the arguments into a string of arguments for a system command. String
arguements are concatenated together. Symbols are downcased and turned into
strings. Keywords are like symbols but prefixed with '--'. Everything else is
just turned into a string as printed with PRINC. If AUTO-SPACE is true, put
spaces between every argument."
  (with-output-to-string (str)
    (loop :with first-time = t
       :for a :in args :do
       (when auto-space
	 (if first-time
	     (setf first-time nil)
	     (princ " " str)))
       (typecase a
	 (keyword			; this is sort of goofy
	  (princ "--" str)
	  (princ (string-downcase (symbol-name a)) str))
	 (symbol
	  (princ (string-downcase (symbol-name a)) str))
	 (t
	  (princ a str))))))

(defvar *buffer-size* (nos:getpagesize)
  "General buffer size for file or stream operations.")

;; I suppose we could make this generic so that streams can do a special
;; things with it, but that might be sort of edging into the stream protocol,
;; which simple-streams and 
(defun copy-stream (source destination)
  "Copy data from reading from SOURCE and writing to DESTINATION, until we get
an EOF on SOURCE."
  ;; ^^^ We could try to make *buffer-size* be the minimum of the file size
  ;; (if it's a file) and the page size, but I'm pretty sure that the stat
  ;; call and possible file I/O is way more inefficient than wasting less than
  ;; 4k of memory to momentarily. Of course we could mmap it, but it should
  ;; end up doing approximately that anyway and the system should have a
  ;; better idea of how big is too big, window sizing and all that. Also,
  ;; that's way more complicated. Even this comment is too much. Let's just
  ;; imagine that a future IDE will collapse or footnotify comments tagged
  ;; with "^^^".
  (let ((buf (make-array *buffer-size*
			 :element-type (stream-element-type source)))
	pos)
    (loop :do
       (setf pos (read-sequence buf source))
       (when (> pos 0)
	 (write-sequence buf destination :end pos))
       :while (= pos *buffer-size*))))

(defun run-with-output-to (file-or-stream commands &key supersede)
  "Run commands with output to a file or stream."
  (let ((result nil))
    (multiple-value-bind (vals in-stream show-vals)
	(shell-eval
	 *shell* (shell-read (lisp-args-to-command commands)) :out-pipe t)
      (declare (ignore show-vals))
      (unwind-protect
	   (when (and vals (> (length vals) 0))
	     (with-open-file-or-stream (out-stream file-or-stream
						   :direction :output
						   :if-exists
						   (if supersede
						       :supersede :error)
						   :if-does-not-exist :create)
	       (copy-stream in-stream out-stream))
	     (setf result vals))
	(close in-stream)))
    result))

(defun input-line-words ()
  "Return lines from *standard-input* as a string of words."
  (with-output-to-string (s)
    (loop :with l = nil :and first = t
       :while (setf l (read-line *standard-input* nil nil))
       :do
       (if first
	   (progn (format s "~a" l)
		  (setf first nil))
	   (format s " ~a" l)))))

(defun map-output-lines (func command)
  "Return a list of the results of calling the function FUNC with each output
line of COMMAND. COMMAND should probably be a string, and FUNC should take one
string as an argument."
  (multiple-value-bind (vals stream show-vals)
      (shell-eval *shell* (shell-read command) :out-pipe t)
    (declare (ignore show-vals))
    (when (and vals (> (length vals) 0))
      (loop :with l = nil
	 :while (setf l (read-line stream nil nil))
	 :collect (funcall func l)))))

;; This is basically backticks #\` or $() in bash.
(defun command-output-words (command)
  "Return lines output from command as a string of words."
  (labels ((convert-to-words (in-stream out-stream)
	     (loop :with l = nil :and first-time = t
		:while (setf l (read-line in-stream nil nil))
		:do
		(format out-stream "~:[~; ~]~a" (not first-time) l)
		(setf first-time nil))))
    (with-output-to-string (s)
      (let* ((expr (shell-read command))
;	     (seq (shell-expr-words expr))
;	     (cmd (first seq))
;	     (args (cdr seq))
	     )
	;; (nos:with-process-output (proc cmd args)
	(multiple-value-bind (vals stream show-vals)
	    (shell-eval *shell* expr :out-pipe t)
	  (declare (ignore show-vals))
	  (when (and vals (> (length vals) 0))
	    (convert-to-words stream s)))))))

(defun command-output-list (command)
  "Return lines output from command as a list."
  (map-output-lines #'identity command))

;; (defvar *files-to-delete* '()
;;   "A list of files to delete at the end of a command.")
;;
;; ;; This has a lot of potential problems / security issues.
;; (defun != (&rest commands)
;;   "Temporary file name output substitution."
;;   (multiple-value-bind (vals stream show-vals)
;;       (shell-eval *shell* (shell-read (lisp-args-to-command commands))
;;                   :out-pipe t)
;;     (declare (ignore show-vals))
;;     (if (and vals (> (length vals) 0))
;; 	(let ((fn (nos:mktemp "lish")))
;; 	  (push fn *files-to-delete*)
;; 	  (with-posix-file (fd fn (logior O_WRONLY O_CREAT O_EXCL) #o600)
;; 	    (let ((buf (make-string (buffer-size))))
;; 	      (loop :while (read-sequence buf stream)
;; 	(progn
;; 	  (close stream)
;; 	  nil))))

;;; The problem with these shelly symbols is: even I can't remember them all.
;;; I think I can remember all the Tetris™ pieces or even all the Blokus™
;;; pieces, so it's not really not a capacity thing. The issue seems to be
;;; memorability versus succinctness. Perhaps if I could come up with one
;;; memorable word for each one of these and then these could be used as
;;; replacements (even automatcially / abbrev-like). Or maybe once they're
;;; working and I'm using them regularly, I will figure it out.

;;; I call these the Snormnambulous™®© Shellbilitous Frobmogrifiers.
;;; [Too. Much. Coffee.]

(defun ! (&rest args)
  "Evaluate the shell command."
  (shell-eval *shell* (shell-read (lisp-args-to-command args))))

(defun !? (&rest args)
  "Evaluate the shell command, converting Unix shell result code into boolean. This means the 0 is T and anything else is NIL."
  (let ((result (shell-eval *shell* (shell-read (lisp-args-to-command args)))))
    (and (numberp result) (zerop result))))

(defun !$ (command)
  "Return lines output from command as a string of words. This is basically like $(command) in bash."
  (command-output-words command))

(defun !_ (command)
  "Return a list of the lines of output from the command."
  (command-output-list command))

(defun !and (&rest commands)
  "Run commands until one fails."
  (declare (ignore commands))
  )

(defun !or (&rest commands)
  "Run commands if previous command succeeded."
  (declare (ignore commands))
  )

(defun !bg (&rest commands)
  "Run commands in the background."
  (declare (ignore commands))
  )

(defun !! (&rest commands)
  "Pipe output of commands. Return a stream of the output."
  (multiple-value-bind (vals stream show-vals)
      (shell-eval *shell* (shell-read (lisp-args-to-command commands))
		  :out-pipe t)
    (declare (ignore show-vals))
    (if (and vals (> (length vals) 0))
	stream
	(progn
	  (close stream)
	  nil))))

(defun !> (file-or-stream &rest commands)
  "Run commands with output to a file or stream."
  (run-with-output-to file-or-stream commands))

(defun !>> (file-or-stream &rest commands)
  "Run commands with output appending to a file or stream."
  (declare (ignore file-or-stream commands))
  )

(defun !<> (file-or-stream &rest commands)
  "Run commands with input and output to a file or stream."
  (declare (ignore file-or-stream commands))
  )

(defun !>! (file-or-stream &rest commands)
  "Run commands with output to a file or stream, superseding it."
  (run-with-output-to file-or-stream commands :supersede t))

(defun !>>! (file-or-stream &rest commands)
  "Run commands with output appending to a file or stream, overwritting it."
  (declare (ignore file-or-stream commands))
  )

(defun !< (file-or-stream &rest commands)
  "Run commands with input from a file or stream."
  (with-open-file-or-stream (in-stream file-or-stream)
    (multiple-value-bind (vals stream show-vals)
	(shell-eval *shell* (shell-read (lisp-args-to-command commands))
		    :in-pipe in-stream)
      (declare (ignore stream show-vals))
      (values-list vals))))

(defun !!< (file-or-stream &rest commands)
  "Run commands with input from a file or stream and return a stream of output."
  (with-open-file-or-stream (in-stream file-or-stream)
    (multiple-value-bind (vals stream show-vals)
	(shell-eval *shell* (shell-read (lisp-args-to-command commands))
		    :out-pipe t
		    :in-pipe in-stream)
      (declare (ignore show-vals))
      (if (and vals (> (length vals) 0))
	  stream
	  (progn
	    (close stream)
	    nil)))))

;; @@@ consider features in inferior-shell?

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
