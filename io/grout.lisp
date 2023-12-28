;;;
;;; grout.lisp - Generic output.
;;;

(defpackage :grout
  (:documentation
   "Backronym: [G]eneric [R]ectilinear [OU]tput And [T]ext
This is so we can make old fashioned command line utilities that can use a
few output features when they're available, but fall back to plain text
when not. This is not for making “fancy” interactive applications. It's just
for relatively simple output.")
  (:use :cl :dlib :dlib-misc :char-util :opsys :terminal :terminal-ansi
	:terminal-dumb-color :table-print :terminal-table :fatchar)
  (:export
   #:grout
   #:grout-stream
   #:*grout*
   #:grout-dumb #:grout-ansi #:grout-generic-term #:grout-ansi-stream
   #:grout-slime
   #:grout-supports-attributes
   #:grout-width
   #:grout-height
   #:grout-bold
   #:grout-set-bold
   #:grout-underline
   #:grout-set-underline
   #:grout-set-normal
   #:grout-color
   #:grout-set-color
   #:grout-clear
   #:grout-beep
   #:grout-object
   #:grout-write
   #:grout-princ
   #:grout-prin1
   #:grout-print
   #:grout-format
   #:grout-span
   #:grout-print-table
   #:grout-finish
   #:grout-done
   #:make-grout
   #:with-grout
   ))
(in-package :grout)

(declaim (optimize (debug 2)))

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defclass grout ()
  ((stream
    :initarg :stream :accessor grout-stream  
    :documentation "The stream for output."))
  (:documentation "Generic output destination."))

(defvar *grout* nil
  "The current dynamic grout.")

;; Mostly miraculous macro-defining macros make me mirthful!
(defmacro defgrout (name (&rest args) doc-string)
  "Macro that defines a grout generic function along with a macro that calls
that generic function with *GROUT* as it's first arg, just for API prettyness."
  (let ((grout-name (symbolify (s+ "GROUT-" name)))
	(grout-generic (symbolify (s+ "%GROUT-" name)))
	(whole-arg (gensym "DEFGROUT-WHOLE-ARG"))
	;;(ignorables (remove-if (_ (char= #\& (char (string _) 0))) args)))
	(ignorables (lambda-list-vars args :all-p t)))
    `(progn
       (defgeneric ,grout-generic (grout ,@args)
         (:documentation ,doc-string)
	 ;; Make an error catching method.
	 (:method ((grout null) ,@args)
	   (declare (ignorable ,@ignorables))
	   (error "*grout* is NIL. Not inside a (with-grout () ...)?")))
       (defmacro ,grout-name (&whole ,whole-arg ,@args)
	 (declare (ignorable ,@ignorables))
	 ,doc-string
	 (append (list ',grout-generic '*grout*) (cdr ,whole-arg))))))

(defgrout supports-attributes ()
  "Return T if the *GROUT* supports character attributes.")

(defgrout width ()
  "Return the width of the output, or NIL for infinite or
unknown.")

(defgrout height ()
  "Return the width of the output, or NIL for infinite or
unknown.")

(defgrout bold (string)
  "Output the string boldly.")

(defgrout set-bold (flag)
  "Turn bold on or off.")

(defgrout underline (string)
  "Output the string underlined.")

(defgrout set-underline (flag)
  "Turn underlining on or off.")

(defgrout set-normal ()
  "Return output to normal. No attributes. No color.")

(defgrout color (foreground background string)
  "Output the string with the colors set.")

(defgrout set-color (foreground background)
  "Set the color.")

(defgrout clear ()
  "Clear the screen.")
    
(defgrout beep ()
  "Do something annoying.")

(defgrout object (object)
  "Output the object in a way that it might be accesible.")

(defgrout write (object
		 &key
		 array base case circle escape gensym length level
		 lines miser-width pprint-dispatch pretty radix
		 readably right-margin
		 &allow-other-keys)
  "Write an object to the grout.")

(defgrout format (format-string &rest format-args)
  "Formatted output to the grout.")

;; These are implemented in terms of grout-write.

(declaim (inline grout-princ))
(defun grout-princ (object)
  "Like PRINC but using GROUT-WRITE."
  (grout-write object :escape nil :readably nil))

(declaim (inline grout-prin1))
(defun grout-prin1 (object)
  "Like PRIN1 but using GROUT-WRITE."
  (grout-write object :escape t))

(declaim (inline grout-print))
(defun grout-print (object)
  "Like PRINT but ostensibly using GROUT-WRITE."
  (grout-write #\newline :escape nil)
  (grout-write object)
  (grout-write #\space :escape nil))

(defgrout print-table (table &key print-titles long-titles max-width
			     trailing-spaces renderer-type renderer
			     &allow-other-keys)
  "Print the table in some kind of nice way, probably using
TABLE-PRINT:OUTPUT-TABLE.")

(defgrout span (span)
  "Output SPAN. A span is attributed text as a list, as is handled by
fatchar:span-to-fat-string.")

(defgrout finish ()
  "Make any pending output be sent to the grout.")

(defgrout done ()
  "Be done with the grout.")

#|
;; We want this to work even if Lish is not loaded.
(defun shell-output-accepts-grotty ()
  "Return true if the LISH output accepts terminal decoration."
  (let (pkg sym val)
    ;;(format t "==--//==--//==--//==--//==~%")
    (and (setf pkg (find-package :lish))
	 (setf sym (intern "*ACCEPTS*" pkg))
	 (boundp sym)
	 (setf val (symbol-value sym))
	 (progn
	   (format t "Grottyness = ~s~%" val)
	   (or (and (keywordp val)
		    (eq :grotty-stream val))
	       (and (typep val 'sequence)
		    (find :grotty-stream val)))))))
|#

(defun shell-output-accepts-grotty ()
  (when (find-package :lish)
    (symbol-call :lish :accepts :grotty-stream)))

;; @@@ Consider a way to support "NO_COLOR" environment variable.
;; Perhaps make a terminal subclass that supresses color? Or just switch
;; to a non-color theme?

;; If you need a specific one, just make it yourself.
(defun make-grout (&optional (stream *standard-output* stream-provided))
  "Return an appropriate grout instance. Try to figure out what kind to make
from the STREAM. STREAM defaults to *STANDARD-OUTPUT*."
  (cond
    ((shell-output-accepts-grotty)
     (make-instance 'grout-ansi-stream :stream stream))
    ;; Use the current *terminal*?
    ((and (not stream-provided) *terminal*
	  ;; crunch seems a bad choice for now
	  (and (not (string= (type-of *terminal*) :terminal-crunch))))
     (make-instance 'grout-generic-term :stream *terminal*))
    ((and stream-provided (typep stream 'terminal))
     (make-instance 'grout-generic-term :stream stream))
    ((has-terminal-attributes stream)
     (make-instance 'grout-generic-term :stream stream))
    ((and (nos:environment-variable "EMACS")
	  (find-package :slime))
     ;; @@@ should really test the stream
     (make-instance 'grout-slime :stream stream))
    (t
     (make-instance 'grout-dumb :stream stream))))

(defmacro with-grout ((&optional (var '*grout*) (stream nil stream-provided))
		      &body body)
  "Evaluate the body with a GROUT bound to ‘var’. Doesn't do anything if ‘var’
is already bound, so multiple wrappings will use the same object. ‘var’ defaults
to ‘*grout*’. Note that if you supply your own ‘var’, you will have to use the
generic functions (i.e. %GROUT-*) directly."
  (with-names (thunk var-value)
    `(flet ((,thunk () ,@body))
       (let* (#|(,var-sym ,var) |#
	      (,var-value ,var))
	 ;; (dbugf :grout "~s ~s ~s ~s ~s ~s~%" (boundp ',var) ',var ,var-value
	 ;; 	,stream-provided
	 ;; 	,stream
	 ;; 	(and ,stream (eq ,stream (grout-stream ,var-value))))
	 (if (and (boundp ',var) ,var-value
		  ;; If the stream was given and it's not the same, we have to
		  ;; make a new grout.
		  (or (not ,stream-provided)
		      (and ,stream-provided
			   (eq ,stream (grout-stream ,var-value)))))
	   (,thunk)
	   (let (,var)
	     (declare (special ,var))
	     (unwind-protect
		  (progn
		    ;; (setf ,var (make-grout (or ,stream *standard-output*)))
		    (setf ,var (if ,stream (make-grout ,stream) (make-grout)))
		    (typecase ,var
		      (grout-generic-term
		       (let ((*terminal* (generic-term ,var))
			     (*standard-output* (generic-term ,var)))
			 (,thunk)))
		      (grout-ansi
		       (let ((*terminal* (ansi-term ,var))
			     (*standard-output* (ansi-term ,var)))
			 (,thunk)))
		      (grout-ansi-stream
		       (let ((*terminal* (ansi-stream ,var))
			     (*standard-output* (ansi-stream ,var)))
			 (,thunk)))
		      (t
		       (,thunk))))
	       (when ,var (%grout-done ,var)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dumb all over. A little ugly on the side.

(defclass grout-dumb (grout)
  ()
  (:documentation "Can't do nothing special."))

(defmethod %grout-supports-attributes ((g grout-dumb))
  "Return T if the *GROUT* supports character attributes."
  nil)

(defmethod %grout-width ((g grout-dumb))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((col (nos:environment-variable "COLUMNS")))
    (or (and col (parse-integer col)) 80)))

(defmethod %grout-height ((g grout-dumb))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((rows (nos:environment-variable "ROWS")))
    (or (and rows (parse-integer rows)) 24)))

(defmethod %grout-bold ((g grout-dumb) string)
  "Output the string boldly."
  (write-string string (grout-stream g)))

(defmethod %grout-set-bold ((g grout-dumb) flag)
  "Turn bold on or off."
  (declare (ignore g flag)))

(defmethod %grout-underline ((g grout-dumb) string)
  "Output the string underlined."
  (write-string string (grout-stream g)))

(defmethod %grout-set-underline ((g grout-dumb) flag)
  "Turn underlining on or off."
  (declare (ignore g flag)))

(defmethod %grout-set-normal ((g grout-dumb))
  "Return output to normal. No attributes. No color."
  (declare (ignore g)))

(defmethod %grout-color ((g grout-dumb) foreground background string)
  "Set the color."
  (declare (ignore foreground background))
  (write-string string (grout-stream g)))

(defmethod %grout-set-color ((g grout-dumb) foreground background)
  "Set the color."
  (declare (ignore g foreground background)))

(defmethod %grout-clear ((g grout-dumb))
  "Clear the screen."
  (dotimes (n (%grout-height g))
    (write-char #\newline (grout-stream g))))
    
(defmethod %grout-beep ((g grout-dumb))
  "Do something annoying."
  (write-char (ctrl #\G) (grout-stream g))
  (finish-output (grout-stream g)))

(defmethod %grout-object ((g grout-dumb) object)
  "Output the object in a way that it might be accesible."
  (write-string (princ-to-string object) (grout-stream g)))

(defmethod %grout-write ((g grout-dumb) object &rest args 
			 &key
			   (array            *print-array*)
			   (base             *print-base*)
			   (case             *print-case*)
			   (circle           *print-circle*)
			   (escape           *print-escape*)
			   (gensym           *print-gensym*)
			   (length           *print-length*)
			   (level            *print-level*)
			   (lines            *print-lines*)
			   (miser-width      *print-miser-width*)
			   (pprint-dispatch  *print-pprint-dispatch*)
			   (pretty           *print-pretty*)
			   (radix            *print-radix*)
			   (readably         *print-readably*)
			   (right-margin     *print-right-margin*)
			   &allow-other-keys)
  (declare (ignorable array base case circle escape gensym length level
		      lines miser-width pprint-dispatch pretty radix
		      readably right-margin))
  (apply #'write object :stream (grout-stream g) args))

(defmethod %grout-format ((g grout-dumb) format-string &rest format-args)
  (apply #'format (grout-stream g) format-string format-args))

(defmethod %grout-span ((g grout-dumb) span)
  (write-string (fatchar-string-to-string (span-to-fatchar-string span))
		(grout-stream g)))

(defmethod %grout-print-table ((g grout-dumb) table
			       &key (print-titles t) long-titles
				 ;;(max-width (grout-width))
				 max-width
				 (trailing-spaces t)
				 (renderer-type 'text-table-renderer)
			         renderer
				 &allow-other-keys)
  (output-table table (or renderer (make-instance renderer-type))
		(grout-stream g)
		:print-titles print-titles :long-titles long-titles
		:max-width max-width
		:trailing-spaces trailing-spaces))

(defmethod %grout-finish ((g grout-dumb))
  (finish-output (grout-stream g)))

(defmethod %grout-done ((g grout-dumb))
  "Be done with the grout."
  (declare (ignore g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI stream, mostly for decorative purposes.

(defclass grout-ansi-stream (grout)
  ((term-stream
    :initarg :term :accessor ansi-stream
    :documentation "The terminal stream."))
  (:documentation "Can do a few standard things."))

(defmethod initialize-instance
    :after ((o grout-ansi-stream) &rest initargs &key &allow-other-keys)
  "Initialize a ANSI stream."
  (declare (ignore initargs))
  (setf (slot-value o 'term-stream)
	(if (typep (slot-value o 'stream) 'terminal-stream)
	    (progn
	      ;; Re-use the steam
	      (slot-value o 'stream))
	    (progn
	      ;; Make a new stream
	      (make-instance ;'terminal-ansi-stream
	                     'terminal-dumb-color
			     :output-stream (slot-value o 'stream))))))

(defmethod %grout-supports-attributes ((g grout-ansi-stream))
  "Return T if the *GROUT* supports character attributes."
  t)

;; Unfortunately we have to do the same the as the dumb driver.
(defmethod %grout-width ((g grout-ansi-stream))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((col (nos:environment-variable "COLUMNS")))
    (or (and col (parse-integer col))
	(and *terminal*
	     (typep *terminal* 'terminal-ansi)
	     (terminal-window-columns *terminal*))
	80)))

;; Unfortunately we have to do the same the as the dumb driver.
(defmethod %grout-height ((g grout-ansi-stream))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((rows (nos:environment-variable "ROWS")))
    (or (and rows (parse-integer rows))
	(and *terminal*
	     (typep *terminal* 'terminal-ansi)
	     (terminal-window-rows *terminal*))
	24)))

(defmethod %grout-bold ((g grout-ansi-stream) string)
  "Output the string boldly."
  (with-slots (term-stream) g
    (terminal-bold term-stream t)
    (terminal-write-string term-stream string)
    (terminal-bold term-stream nil)))

(defmethod %grout-set-bold ((g grout-ansi-stream) flag)
  "Turn bold on or off."
  (terminal-bold (ansi-stream g) flag))

(defmethod %grout-underline ((g grout-ansi-stream) string)
  "Output the string underlined."
  (with-slots (term-stream) g
    (terminal-underline term-stream t)
    (terminal-write-string term-stream string)
    (terminal-underline term-stream nil)
    ;; (terminal-finish-output term-stream)
    ))

(defmethod %grout-set-underline ((g grout-ansi-stream) flag)
  "Turn underlining on or off."
  (terminal-underline (ansi-stream g) flag))

(defmethod %grout-set-normal ((g grout-ansi-stream))
  "Return output to normal. No attributes. No color."
  (terminal-normal (ansi-stream g)))

(defmethod %grout-color ((g grout-ansi-stream) foreground background string)
  "Set the color."
  (with-slots (term-stream) g
    (terminal-color term-stream foreground background)
    (terminal-write-string term-stream string)
    (terminal-color term-stream :default :default)))

(defmethod %grout-set-color ((g grout-ansi-stream) foreground background)
  "Set the color."
  (terminal-color (ansi-stream g) foreground background))

(defmethod %grout-clear ((g grout-ansi-stream))
  "Clear the screen."
  (terminal-clear (ansi-stream g)))

(defmethod %grout-beep ((g grout-ansi-stream))
  "Do something annoying."
  (terminal-beep (ansi-stream g)))

(defmethod %grout-object ((g grout-ansi-stream) object)
  "Output the object in a way that it might be accesible."
  (terminal-write-string (ansi-stream g) (princ-to-string object)))

(defmethod %grout-write ((g grout-ansi-stream) object &rest args 
			 &key
			   (array            *print-array*)
			   (base             *print-base*)
			   (case             *print-case*)
			   (circle           *print-circle*)
			   (escape           *print-escape*)
			   (gensym           *print-gensym*)
			   (length           *print-length*)
			   (level            *print-level*)
			   (lines            *print-lines*)
			   (miser-width      *print-miser-width*)
			   (pprint-dispatch  *print-pprint-dispatch*)
			   (pretty           *print-pretty*)
			   (radix            *print-radix*)
			   (readably         *print-readably*)
			   (right-margin     *print-right-margin*)
			   &allow-other-keys)
  (declare (ignorable array base case circle escape gensym length level
		      lines miser-width pprint-dispatch pretty radix
		      readably right-margin))
  ;; (terminal-write-string (ansi-stream g)
  ;; 			 (with-output-to-string (str)
  ;; 			   (apply #'write object :stream str args)))
  (apply #'write object :stream (ansi-stream g) args)
  )

(defmethod %grout-format ((g grout-ansi-stream) format-string &rest format-args)
  (apply #'terminal-format (ansi-stream g) format-string format-args))

(defmethod %grout-span ((g grout-ansi-stream) span)
  (apply #'terminal-write-span (ansi-stream g) (list span)))

(defmethod %grout-print-table ((g grout-ansi-stream) table
			       &key (print-titles t) long-titles
				 max-width
				 (trailing-spaces t)
				 (renderer-type 'terminal-table-renderer)
			         renderer
				 &allow-other-keys)
  (output-table table (or renderer (make-instance renderer-type))
		(ansi-stream g)
		:print-titles print-titles :long-titles long-titles
		:max-width max-width :trailing-spaces trailing-spaces))

(defmethod %grout-finish ((g grout-ansi-stream))
  (terminal-finish-output (ansi-stream g)))

(defmethod %grout-done ((g grout-ansi-stream))
  "Be done with the grout."
  (terminal-finish-output (ansi-stream g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We expect a real genuine fake "ANSI" 'terminal'.

(defclass grout-ansi (grout)
  ((term
    :initarg :term :accessor ansi-term
    :documentation "The terminal.")
   (own-term
    :initarg :own-term :accessor own-term :initform nil :type boolean
    :documentation "True if we made our own terminal instance."))
  (:documentation "Can do a few standard things."))

(defmethod initialize-instance
    :after ((o grout-ansi) &rest initargs &key &allow-other-keys)
  "Initialize a grout-ansi."
  (declare (ignore initargs))
  (setf (slot-value o 'term)
	(if (typep (slot-value o 'stream) 'terminal-stream)
	    (progn
	      (slot-value o 'stream))
	    (progn
	      (setf (slot-value o 'own-term) t)
	      (make-instance 'terminal-ansi)
	      )))
  (when (slot-value o 'own-term)
    (terminal-start (slot-value o 'term))))

(defmethod %grout-supports-attributes ((g grout-ansi))
  "Return T if the *GROUT* supports character attributes."
  t)

(defmethod %grout-width ((g grout-ansi))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-columns (ansi-term g)))

(defmethod %grout-height ((g grout-ansi))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-rows (ansi-term g)))

(defmethod %grout-bold ((g grout-ansi) string)
  "Output the string boldly."
  (with-slots (term) g
    (terminal-bold term t)
    (terminal-write-string term string)
    (terminal-bold term nil)))

(defmethod %grout-set-bold ((g grout-ansi) flag)
  "Turn bold on or off."
  (terminal-bold (ansi-term g) flag))

(defmethod %grout-underline ((g grout-ansi) string)
  "Output the string underlined."
  (with-slots (term) g
    (terminal-underline term t)
    (terminal-write-string term string)
    (terminal-underline term nil)
    ;; (terminal-finish-output term)
    ))

(defmethod %grout-set-underline ((g grout-ansi) flag)
  "Turn underlining on or off."
  (terminal-underline (ansi-term g) flag))

(defmethod %grout-set-normal ((g grout-ansi))
  "Return output to normal. No attributes. No color."
  (terminal-normal (ansi-term g)))

(defmethod %grout-color ((g grout-ansi) foreground background string)
  "Set the color."
  (with-slots (term) g
    (terminal-color term foreground background)
    (terminal-write-string term string)
    (terminal-color term :default :default)))

(defmethod %grout-set-color ((g grout-ansi) foreground background)
  "Set the color."
  (terminal-color (ansi-term g) foreground background))

(defmethod %grout-clear ((g grout-ansi))
  "Clear the screen."
  (terminal-clear (ansi-term g)))

(defmethod %grout-beep ((g grout-ansi))
  "Do something annoying."
  (terminal-beep (ansi-term g)))

(defmethod %grout-object ((g grout-ansi) object)
  "Output the object in a way that it might be accesible."
  (terminal-write-string (ansi-term g) (princ-to-string object)))

(defmethod %grout-write ((g grout-ansi) object &rest args 
			 &key
			   (array            *print-array*)
			   (base             *print-base*)
			   (case             *print-case*)
			   (circle           *print-circle*)
			   (escape           *print-escape*)
			   (gensym           *print-gensym*)
			   (length           *print-length*)
			   (level            *print-level*)
			   (lines            *print-lines*)
			   (miser-width      *print-miser-width*)
			   (pprint-dispatch  *print-pprint-dispatch*)
			   (pretty           *print-pretty*)
			   (radix            *print-radix*)
			   (readably         *print-readably*)
			   (right-margin     *print-right-margin*)
			   &allow-other-keys)
  (declare (ignorable array base case circle escape gensym length level
		      lines miser-width pprint-dispatch pretty radix
		      readably right-margin))
  ;; (terminal-write-string (ansi-term g)
  ;; 		   (with-output-to-string (str)
  ;; 		     (apply #'write object :stream str args)))
  (apply #'write object :stream (ansi-term g) args)
  )

(defmethod %grout-format ((g grout-ansi) format-string &rest format-args)
  (apply #'terminal-format (ansi-term g) format-string format-args))

(defmethod %grout-span ((g grout-ansi) span)
  (apply #'terminal-write-span (ansi-term g) (list span)))

(defmethod %grout-print-table ((g grout-ansi) table
			       &key (print-titles t) long-titles
				 (max-width (grout-width))
				 (trailing-spaces t)
				 (renderer-type 'terminal-table-renderer)
			         renderer
				 &allow-other-keys)
  (output-table table (or renderer (make-instance renderer-type))
		(ansi-term g)
		:print-titles print-titles :long-titles long-titles
		:max-width max-width :trailing-spaces trailing-spaces))

(defmethod %grout-finish ((g grout-ansi))
  (terminal-finish-output (ansi-term g)))

(defmethod %grout-done ((g grout-ansi))
  "Be done with the grout."
  (terminal-finish-output (ansi-term g))
  (when (own-term g)
    (terminal-done (ansi-term g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic terminal

(defclass grout-generic-term (grout)
  ((term
    :initarg :term :accessor generic-term
    :documentation "The terminal.")
   (own-term
    :initarg :own-term :accessor own-term :initform nil :type boolean
    :documentation "True if we made our own terminal instance."))
  (:documentation "Can do a few standard things."))

(defmethod initialize-instance
    :after ((o grout-generic-term) &rest initargs &key &allow-other-keys)
  "Initialize a grout generic-term."
  (declare (ignore initargs))
  (setf (slot-value o 'term)
	(if (typep (slot-value o 'stream) 'terminal-stream)
	    (progn
	      (slot-value o 'stream))
	    (progn
	      (setf (slot-value o 'own-term) t)
	      (make-instance
	       (find-terminal-class-for-type (platform-default-terminal-type))
	       :start-at-current-line t))))
  (when (slot-value o 'own-term)
    (terminal-start (slot-value o 'term))))

(defmethod %grout-supports-attributes ((g grout-generic-term))
  "Return T if the *GROUT* supports character attributes."
  t)

(defmethod %grout-width ((g grout-generic-term))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-columns (generic-term g)))

(defmethod %grout-height ((g grout-generic-term))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-rows (generic-term g)))

(defmethod %grout-bold ((g grout-generic-term) string)
  "Output the string boldly."
  (with-slots (term) g
    (terminal-bold term t)
    (terminal-write-string term string)
    (terminal-bold term nil)))

(defmethod %grout-set-bold ((g grout-generic-term) flag)
  "Turn bold on or off."
  (terminal-bold (generic-term g) flag))

(defmethod %grout-underline ((g grout-generic-term) string)
  "Output the string underlined."
  (with-slots (term) g
    (terminal-underline term t)
    (terminal-write-string term string)
    (terminal-underline term nil)
    ;; (terminal-finish-output term)
    ))

(defmethod %grout-set-underline ((g grout-generic-term) flag)
  "Turn underlining on or off."
  (terminal-underline (generic-term g) flag))

(defmethod %grout-set-normal ((g grout-generic-term))
  "Return output to normal. No attributes. No color."
  (terminal-normal (generic-term g)))

(defmethod %grout-color ((g grout-generic-term) foreground background string)
  "Set the color."
  (with-slots (term) g
    (terminal-color term foreground background)
    (terminal-write-string term string)
    (terminal-color term :default :default)))

(defmethod %grout-set-color ((g grout-generic-term) foreground background)
  "Set the color."
  (terminal-color (generic-term g) foreground background))

(defmethod %grout-clear ((g grout-generic-term))
  "Clear the screen."
  (terminal-clear (generic-term g)))

(defmethod %grout-beep ((g grout-generic-term))
  "Do something annoying."
  (terminal-beep (generic-term g)))

(defmethod %grout-object ((g grout-generic-term) object)
  "Output the object in a way that it might be accesible."
  (terminal-write-string (generic-term g) (princ-to-string object)))

(defmethod %grout-write ((g grout-generic-term) object &rest args 
			 &key
			   (array            *print-array*)
			   (base             *print-base*)
	#| see that? -> |# (case             *print-case*)
			   (circle           *print-circle*)
			   (escape           *print-escape*)
			   (gensym           *print-gensym*)
			   (length           *print-length*)
			   (level            *print-level*)
			   (lines            *print-lines*)
			   (miser-width      *print-miser-width*)
			   (pprint-dispatch  *print-pprint-dispatch*)
			   (pretty           *print-pretty*)
			   (radix            *print-radix*)
			   (readably         *print-readably*)
			   (right-margin     *print-right-margin*)
			   &allow-other-keys)
  (declare (ignorable array base case circle escape gensym length level
		      lines miser-width pprint-dispatch pretty radix
		      readably right-margin))
  ;; (terminal-write-string (generic-term g)
  ;; 		   (with-output-to-string (str)
  ;; 		     (apply #'write object :stream str args)))
  (apply #'write object :stream (generic-term g) args)
  )

(defmethod %grout-format ((g grout-generic-term) format-string &rest format-args)
  (apply #'terminal-format (generic-term g) format-string format-args))

(defmethod %grout-span ((g grout-generic-term) span)
  (apply #'terminal-write-span (generic-term g) (list span)))

(defmethod %grout-print-table ((g grout-generic-term) table
			       &key (print-titles t) long-titles
				 (max-width (grout-width))
				 (trailing-spaces t)
				 (renderer-type 'terminal-table-renderer)
			         renderer
				 &allow-other-keys)
  (output-table table (or renderer (make-instance renderer-type))
		(generic-term g)
		:print-titles print-titles :long-titles long-titles
		:max-width max-width :trailing-spaces trailing-spaces)
  ;; One doesn't always want output-table to do a newline, although probably
  ;; with print-table you do.
  (terminal-fresh-line (generic-term g)))

(defmethod %grout-finish ((g grout-generic-term))
  (terminal-finish-output (generic-term g)))

(defmethod %grout-done ((g grout-generic-term))
  "Be done with the grout."
  (terminal-finish-output (generic-term g))
  (when (own-term g)
    (terminal-done (generic-term g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime, with worms.

;; There must be a way to do these things with:
;;
;; (swank:eval-in-emacs '(save-excursion (set-buffer ".emacs") (piggy)))
;;
;; First we have to make the output go, then we have to set an overlay.
;;  (overlay-put (make-overlay beg end buffer) 'face 'underline)
;;  (overlay-put (make-overlay beg end buffer) 'face 'bold)

(defmacro swank (func &rest args)
  "So we don't have to depend on swank."
  `(if (find-package :swank)
       (funcall (intern ,(symbol-name func) (find-package :swank)) ,@args)
       (warn "You should probably load Swank.")))

(defclass grout-slime (grout)
  ()
  (:documentation "Can just tell emacs to do something."))

(defmethod %grout-supports-attributes ((g grout-slime))
  "Return T if the *GROUT* supports character attributes."
  ;; @@@ We want to make it so this can be T.
  nil)

(defmethod %grout-width ((g grout-slime))
  "Return the width of the output, or NIL for infinite or unknown."
  (swank eval-in-emacs '(window-width)))

(defmethod %grout-height ((g grout-slime))
  "Return the width of the output, or NIL for infinite or unknown."
  (swank eval-in-emacs '(window-height)))

(defmethod %grout-bold ((g grout-slime) string)
  "Output the string boldly."
  (write-string string (grout-stream g)))

(defmethod %grout-set-bold ((g grout-slime) flag)
  "Turn bold on or off."
  (declare (ignore g flag)))

(defmethod %grout-underline ((g grout-slime) string)
  "Output the string underlined."
  (write-string string (grout-stream g)))

(defmethod %grout-set-underline ((g grout-slime) flag)
  "Turn underlining on or off."
  (declare (ignore g flag)))

(defmethod %grout-set-normal ((g grout-slime))
  "Return output to normal. No attributes. No color."
  (declare (ignore g)))

(defmethod %grout-color ((g grout-slime) foreground background string)
  "Output the string with the colors set."
  (declare (ignore foreground background))
  (write-string string (grout-stream g)))

(defmethod %grout-set-color ((g grout-slime) foreground background)
  "Set the color."
  (declare (ignore g foreground background)))

(defmethod %grout-clear ((g grout-slime))
  "Clear the screen."
  (declare (ignore g)))
    
(defmethod %grout-beep ((g grout-slime))
  "Do something annoying."
  (swank eval-in-emacs '(ding t)))

(defmethod %grout-object ((g grout-slime) object)
  "Output the object in a way that it might be accesible."
  (swank present-repl-results (list object)))

(defmethod %grout-write ((g grout-slime) object &rest args
			 &key
			   (array            *print-array*)
			   (base             *print-base*)
			   (case             *print-case*)
			   (circle           *print-circle*)
			   (escape           *print-escape*)
			   (gensym           *print-gensym*)
			   (length           *print-length*)
			   (level            *print-level*)
			   (lines            *print-lines*)
			   (miser-width      *print-miser-width*)
			   (pprint-dispatch  *print-pprint-dispatch*)
			   (pretty           *print-pretty*)
			   (radix            *print-radix*)
			   (readably         *print-readably*)
			   (right-margin     *print-right-margin*)
			   &allow-other-keys)
  (declare (ignorable array base case circle escape gensym length level
		      lines miser-width pprint-dispatch pretty radix
		      readably right-margin))
  (apply #'write object :stream (grout-stream g) args))

(defmethod %grout-format ((g grout-slime) format-string &rest format-args)
  (apply #'format (grout-stream g) format-string format-args))

(defmethod %grout-span ((g grout-slime) span)
  (write-string (fatchar-string-to-string (span-to-fatchar-string span))
		(grout-stream g)))

;; Just use the plain text renderer for now, but perhaps 'twould be interesting
;; to use table-insert or org-table-*?
(defmethod %grout-print-table ((g grout-slime) table
			       &key (print-titles t) long-titles
				 (max-width (grout-width))
				 (trailing-spaces t)
				 (renderer-type 'text-table-renderer)
			         renderer
				 &allow-other-keys)
  (output-table table (or renderer (make-instance renderer-type))
		(grout-stream g)
		:print-titles print-titles :long-titles long-titles
		:max-width max-width :trailing-spaces trailing-spaces))

(defmethod %grout-finish ((g grout-slime))
  (finish-output (grout-stream g)))

(defmethod %grout-done ((g grout-slime))
  "Be done with the grout."
  (declare (ignore g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Future-past Lisp term, which can do presentations, even better than emacs,
;; which was/will be wonderful.

;; ... @@@

;; EOF
