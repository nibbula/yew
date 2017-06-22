;;
;; grout.lisp - Generic output.
;;

(defpackage :grout
  (:documentation "Backronym: [G]eneric [R]ectilinear [OU]tput And [T]ext
This is so we can make old fashioned command line utilities that can use a
few output features when they're available, but fall back to plain text
when not. This is not for making “fancy” interactive applications. It's just
for relatively simple output.")
  (:use :cl :dlib :dlib-misc :char-util :opsys :terminal :terminal-ansi)
  (:export
   #:grout
   #:grout-stream
   #:*grout*
   #:dumb #:ansi #:ansi-stream #:slime
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
   #:grout-finish
   #:grout-done
   #:make-grout
   #:with-grout
   ))
(in-package :grout)

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
       (defgeneric ,grout-generic (grout ,@args) (:documentation ,doc-string))
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
  ;;(dbugf :accepts "*accepts* = ~s~%" lish:*accepts*)
  (dbugf :accepts "Grotty yo = ~s~%"
	 (symbol-call :lish :accepts :grotty-stream))
  (symbol-call :lish :accepts :grotty-stream))

;; If you need a specific one, just make it yourself.
(defun make-grout (&optional (stream *standard-output*))
  "Return an appropriate grout instance. Try to figure out what kind to make
from the STREAM. STREAM defaults to *STANDARD-OUTPUT*."
  (cond
    ((has-terminal-attributes stream)
     (make-instance 'ansi :stream stream))
    ((shell-output-accepts-grotty)
     (make-instance 'ansi-stream :stream stream))
    ((and (nos:environment-variable "EMACS")
	  (find-package :slime))
     ;; @@@ should really test the stream
     (make-instance 'slime :stream stream))
    (t
     (make-instance 'dumb :stream stream))))

(defmacro with-grout ((&optional (var '*grout*) stream) &body body)
  "Evaluate the body with a GROUT bound to VAR. Doesn't do anything if VAR is
already bound, so multiple wrappings will use the same object. VAR defaults to
*GROUT*. Note that if you supply your own VAR, you will have to use the
generic functions (i.e. %GROUT-*) directly."
  `(if (and (boundp ',var) ,var)
       (progn
	 ,@body)
       (let (,var)
	 (declare (special ,var))
	 (unwind-protect
	   (progn
	     (setf ,var (make-grout (or ,stream *standard-output*)))
	     ,@body)
	   (when ,var (%grout-done ,var))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dumb all over. A little ugly on the side.

(defclass dumb (grout)
  ()
  (:documentation "Can't do nothing special."))

(defmethod %grout-supports-attributes ((g dumb))
  "Return T if the *GROUT* supports character attributes."
  nil)

(defmethod %grout-width ((g dumb))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((col (nos:environment-variable "COLUMNS")))
    (or (and col (parse-integer col)) 80)))

(defmethod %grout-height ((g dumb))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((rows (nos:environment-variable "ROWS")))
    (or (and rows (parse-integer rows)) 24)))

(defmethod %grout-bold ((g dumb) string)
  "Output the string boldly."
  (write-string string (grout-stream g)))

(defmethod %grout-set-bold ((g dumb) flag)
  "Turn bold on or off."
  (declare (ignore g flag)))

(defmethod %grout-underline ((g dumb) string)
  "Output the string underlined."
  (write-string string (grout-stream g)))

(defmethod %grout-set-underline ((g dumb) flag)
  "Turn underlining on or off."
  (declare (ignore g flag)))

(defmethod %grout-set-normal ((g dumb))
  "Return output to normal. No attributes. No color."
  (declare (ignore g)))

(defmethod %grout-color ((g dumb) foreground background string)
  "Set the color."
  (declare (ignore foreground background))
  (write-string string (grout-stream g)))

(defmethod %grout-set-color ((g dumb) foreground background)
  "Set the color."
  (declare (ignore g foreground background)))

(defmethod %grout-clear ((g dumb))
  "Clear the screen."
  (dotimes (n (%grout-height g))
    (write-char #\newline (grout-stream g))))
    
(defmethod %grout-beep ((g dumb))
  "Do something annoying."
  (write-char (ctrl #\G) (grout-stream g))
  (finish-output (grout-stream g)))

(defmethod %grout-object ((g dumb) object)
  "Output the object in a way that it might be accesible."
  (write-string (princ-to-string object) (grout-stream g)))

(defmethod %grout-write ((g dumb) object &rest args 
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

(defmethod %grout-format ((g dumb) format-string &rest format-args)
  (apply #'format (grout-stream g) format-string format-args))

(defmethod %grout-finish ((g dumb))
  (finish-output (grout-stream g)))

(defmethod %grout-done ((g dumb))
  "Be done with the grout."
  (declare (ignore g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI stream, mostly for decorative purposes.

(defclass ansi-stream (grout)
  ((term-stream
    :initarg :term :accessor ansi-stream
    :documentation "The terminal stream."))
  (:documentation "Can do a few standard things."))

(defmethod initialize-instance
    :after ((o ansi-stream) &rest initargs &key &allow-other-keys)
  "Initialize a ANSI stream."
  (declare (ignore initargs))
  (setf (slot-value o 'term-stream)
	(make-instance 'terminal-ansi-stream
		       :output-stream (slot-value o 'stream))))

(defmethod %grout-supports-attributes ((g ansi-stream))
  "Return T if the *GROUT* supports character attributes."
  t)

;; Unfortunately we have to do the same the as the dumb driver.
(defmethod %grout-width ((g ansi-stream))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((col (nos:environment-variable "COLUMNS")))
    (or (and col (parse-integer col))
	(and *terminal* (terminal-window-columns *terminal*))
	80)))

;; Unfortunately we have to do the same the as the dumb driver.
(defmethod %grout-height ((g ansi-stream))
  "Return the width of the output, or NIL for infinite or unknown."
  (declare (ignore g))
  (let ((rows (nos:environment-variable "ROWS")))
    (or (and rows (parse-integer rows)) 24)))

(defmethod %grout-bold ((g ansi-stream) string)
  "Output the string boldly."
  (with-slots (term-stream) g
    (terminal-bold term-stream t)
    (terminal-write-string term-stream string)
    (terminal-bold term-stream nil)))

(defmethod %grout-set-bold ((g ansi-stream) flag)
  "Turn bold on or off."
  (terminal-bold (ansi-stream g) flag))

(defmethod %grout-underline ((g ansi-stream) string)
  "Output the string underlined."
  (with-slots (term-stream) g
    (terminal-underline term-stream t)
    (terminal-write-string term-stream string)
    (terminal-underline term-stream nil)
    (terminal-finish-output term-stream)))

(defmethod %grout-set-underline ((g ansi-stream) flag)
  "Turn underlining on or off."
  (terminal-underline (ansi-stream g) flag))

(defmethod %grout-set-normal ((g ansi-stream))
  "Return output to normal. No attributes. No color."
  (terminal-normal (ansi-stream g)))

(defmethod %grout-color ((g ansi-stream) foreground background string)
  "Set the color."
  (with-slots (term-stream) g
    (terminal-color term-stream foreground background)
    (terminal-write-string term-stream string)
    (terminal-color term-stream :default :default)))

(defmethod %grout-set-color ((g ansi-stream) foreground background)
  "Set the color."
  (terminal-color (ansi-stream g) foreground background))

(defmethod %grout-clear ((g ansi-stream))
  "Clear the screen."
  (terminal-clear (ansi-stream g)))

(defmethod %grout-beep ((g ansi-stream))
  "Do something annoying."
  (terminal-beep (ansi-stream g)))

(defmethod %grout-object ((g ansi-stream) object)
  "Output the object in a way that it might be accesible."
  (terminal-write-string (ansi-stream g) (princ-to-string object)))

(defmethod %grout-write ((g ansi-stream) object &rest args 
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
  (terminal-write-string (ansi-stream g)
		   (with-output-to-string (str)
		     (apply #'write object :stream str args))))

(defmethod %grout-format ((g ansi-stream) format-string &rest format-args)
  (apply #'terminal-format (ansi-stream g) format-string format-args))

(defmethod %grout-finish ((g ansi-stream))
  (terminal-finish-output (ansi-stream g)))

(defmethod %grout-done ((g ansi-stream))
  "Be done with the grout."
  (terminal-finish-output (ansi-stream g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI is a bad word. So is ‘terminal’ in this sense.
;; We expect a real genuine fake.

(defclass ansi (grout)
  ((term
    :initarg :term :accessor ansi-term
    :documentation "The terminal."))
  (:documentation "Can do a few standard things."))

(defmethod initialize-instance
    :after ((o ansi) &rest initargs &key &allow-other-keys)
  "Initialize a ansi."
  (declare (ignore initargs))
  (setf (slot-value o 'term) (make-instance 'terminal-ansi))
  (terminal-start (slot-value o 'term)))

(defmethod %grout-supports-attributes ((g ansi))
  "Return T if the *GROUT* supports character attributes."
  t)

(defmethod %grout-width ((g ansi))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-columns (ansi-term g)))

(defmethod %grout-height ((g ansi))
  "Return the width of the output, or NIL for infinite or unknown."
  (terminal-window-rows (ansi-term g)))

(defmethod %grout-bold ((g ansi) string)
  "Output the string boldly."
  (with-slots (term) g
    (terminal-bold term t)
    (terminal-write-string term string)
    (terminal-bold term nil)))

(defmethod %grout-set-bold ((g ansi) flag)
  "Turn bold on or off."
  (terminal-bold (ansi-term g) flag))

(defmethod %grout-underline ((g ansi) string)
  "Output the string underlined."
  (with-slots (term) g
    (terminal-underline term t)
    (terminal-write-string term string)
    (terminal-underline term nil)
    (terminal-finish-output term)))

(defmethod %grout-set-underline ((g ansi) flag)
  "Turn underlining on or off."
  (terminal-underline (ansi-term g) flag))

(defmethod %grout-set-normal ((g ansi))
  "Return output to normal. No attributes. No color."
  (terminal-normal (ansi-term g)))

(defmethod %grout-color ((g ansi) foreground background string)
  "Set the color."
  (with-slots (term) g
    (terminal-color term foreground background)
    (terminal-write-string term string)
    (terminal-color term :default :default)))

(defmethod %grout-set-color ((g ansi) foreground background)
  "Set the color."
  (terminal-color (ansi-term g) foreground background))

(defmethod %grout-clear ((g ansi))
  "Clear the screen."
  (terminal-clear (ansi-term g)))

(defmethod %grout-beep ((g ansi))
  "Do something annoying."
  (terminal-beep (ansi-term g)))

(defmethod %grout-object ((g ansi) object)
  "Output the object in a way that it might be accesible."
  (terminal-write-string (ansi-term g) (princ-to-string object)))

(defmethod %grout-write ((g ansi) object &rest args 
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
  (terminal-write-string (ansi-term g)
		   (with-output-to-string (str)
		     (apply #'write object :stream str args))))

(defmethod %grout-format ((g ansi) format-string &rest format-args)
  (apply #'terminal-format (ansi-term g) format-string format-args))

(defmethod %grout-finish ((g ansi))
  (terminal-finish-output (ansi-term g)))

(defmethod %grout-done ((g ansi))
  "Be done with the grout."
  (terminal-finish-output (ansi-term g))
  (terminal-done (ansi-term g)))

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

(defclass slime (grout)
  ()
  (:documentation "Can just tell emacs to do something."))

(defmethod %grout-supports-attributes ((g slime))
  "Return T if the *GROUT* supports character attributes."
  ;; @@@ We want to make it so this can be T.
  nil)

(defmethod %grout-width ((g slime))
  "Return the width of the output, or NIL for infinite or unknown."
  (swank eval-in-emacs '(window-width)))

(defmethod %grout-height ((g slime))
  "Return the width of the output, or NIL for infinite or unknown."
  (swank eval-in-emacs '(window-height)))

(defmethod %grout-bold ((g slime) string)
  "Output the string boldly."
  (write-string string (grout-stream g)))

(defmethod %grout-set-bold ((g slime) flag)
  "Turn bold on or off."
  (declare (ignore g flag)))

(defmethod %grout-underline ((g slime) string)
  "Output the string underlined."
  (write-string string (grout-stream g)))

(defmethod %grout-set-underline ((g slime) flag)
  "Turn underlining on or off."
  (declare (ignore g flag)))

(defmethod %grout-set-normal ((g slime))
  "Return output to normal. No attributes. No color."
  (declare (ignore g)))

(defmethod %grout-color ((g slime) foreground background string)
  "Output the string with the colors set."
  (declare (ignore foreground background))
  (write-string string (grout-stream g)))

(defmethod %grout-set-color ((g slime) foreground background)
  "Set the color."
  (declare (ignore g foreground background)))

(defmethod %grout-clear ((g slime))
  "Clear the screen."
  (declare (ignore g)))
    
(defmethod %grout-beep ((g slime))
  "Do something annoying."
  (swank eval-in-emacs '(ding t)))

(defmethod %grout-object ((g slime) object)
  "Output the object in a way that it might be accesible."
  (swank present-repl-results (list object)))

(defmethod %grout-write ((g slime) object &rest args
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

(defmethod %grout-format ((g slime) format-string &rest format-args)
  (apply #'format (grout-stream g) format-string format-args))

(defmethod %grout-finish ((g slime))
  (finish-output (grout-stream g)))

(defmethod %grout-done ((g slime))
  "Be done with the grout."
  (declare (ignore g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Future-past Lisp term, which can do representations, even better than emacs,
;; which was/will be wonderful.

;; ... @@@

;; EOF
