;;;
;;; parse-util.lisp - Parsing utilities.
;;;

(defpackage :parse-util
  (:documentation "Parsing utilities.

This is a small utility for constructing simple recursive descent parsers.
It's probably most applicable to simple grammars and short inputs. Perhaps
things that you might consider parsing with a regular expression.

Parsing is done with a set of macros inside a ‘with-parsing’ macro, which
returns the parse results. Results are created with ‘note’ macro.

Quantifiers macros are:
   (optional ...)      If the body doesn't evaluate to true, restore the point.
   (must-be ...)       The body must evaluate true.
   (sequence-of ...)   Every form of the body must be true.
   (one-or-more ...)   Do the body once and until it is false.
   (zero-or-more ...)  Do the body, until it returns false. Always true.
   (one-of ...)        Return after the first optional expression of the body
                       is true.

Result macro:
   (note () ...)       Push the of value of the body onto the results.
   (with-substring (name) ...)
                       Make name be a function that returns the current
                       substring, which the macro encloses.

Character functions are:
   (peek)              Return the next character.
   (next-char)         Return and consume the next character.
   (char-in C STRING)  Return true if C is in STRING.
   (is-char C)         Return true if C is the next character.
   (is--not-char C)    Return true if C is not the next character.
   (is-string STRING)  Return true if the next characters are STRING.

For example:
   (with-parsing ()
    )
")
  (:use :cl :dlib)
  (:nicknames :pu)
  (:export
   ;; state object
   #:state
   #:state-string
   #:state-i
   #:state-results
   #:*state*
   ;; macros
   #:optional
   #:must-be
   #:sequence-of
   #:one-or-more
   #:zero-or-more
   #:one-of
   #:note
   #:with-substring
   #:with-parsing
   ;; functions
   #:peek
   #:next-char
   #:char-in
   #:is-char
   #:is-not-char
   #:is-string
   ))
(in-package :parse-util)

(defstruct state
  "Parsing state."
  (string "" :type simple-string)
  (i 0 :type fixnum)
  (results nil :type list))

(defparameter *state* nil
  "Parsing state.")

(defmacro optional (&body body)
  "If BODY doesn't evaluate to true, restore the point."
  (with-names (start result)
    `(let ((,start (state-i *state*)) ,result)
       (when (not (setf ,result (progn ,@body)))
	 (setf (state-i *state*) ,start))
       t)))

(defmacro must-be (&body body)
  "BODY must evaluate true."
  (with-names (start result)
    `(let ((,start (state-i *state*)) ,result)
       (when (not (setf ,result (progn ,@body)))
	 (setf (state-i *state*) ,start))
       ,result)))

(defmacro sequence-of (&body body)
  "Every form of BODY must be true."
  (with-names (start result)
    `(let ((,start (state-i *state*)) ,result)
       (when (not (setf ,result (and ,@body)))
	 (setf (state-i *state*) ,start))
       ,result)))

(defmacro one-or-more (&body body)
  "Do the body once and until it is false."
  (with-names (thunk)
    `(flet ((,thunk () ,@body))
       (and (,thunk)
	    (or (loop :while (,thunk)) t)))))

(defmacro zero-or-more (&body body)
  "Do the body, until it returns false. Always true."
  (with-names (thunk)
    `(flet ((,thunk () ,@body))
       (loop :while (,thunk))
       t)))

(defmacro one-of (&body body)
  "Return after the first optional expression of BODY is true."
  `(or ,@(loop :for expr :in body
	    :collect `(must-be ,expr))))

(defmacro note ((x) &body body)
  "Push the of value of the body onto the results."
  (with-names (r)
    `(let ((,r (progn ,@body)))
       (when ,r
	 (push ,x (state-results *state*)))
       ,r)))

(defun peek ()
  "Return the next character."
  (with-slots (i string) *state*
    (declare (type simple-string string)
	     (type fixnum i))
    (when (< i (length string))
      (char string i))))

(defun next-char ()
  "Return and consume the next character."
  (with-slots (i string) *state*
    (declare (type simple-string string)
	     (type fixnum i))
    (if (< i (length string))
	(prog1 (char string i)
	  (incf i))
	(throw 'eof nil))))

(defun char-in (c string)
  "True if C is in STRING."
  (declare (type simple-string string)
	   (type character c))
  (find c string :test #'char=))

;; @@@ should be a function?
(defun is-char (char)
  "Return true if C is the next character."
  (and (peek)
       (char= (peek) char)
       (next-char)))

(defun is-not-char (char)
  "Return true if C is NOT the next character."
  (and (peek)
       (char/= (peek) char)
       (next-char)))

;; @@@ should be a function?
(defun is-string (string)
  "Return true if the next characters are STRING."
  (declare (type simple-string string))
  (let ((str string)
	(start (state-i *state*))
	(i 0))
    (loop
       :while (and (peek) (char= (peek) (char str i)))
       :do (next-char))
    (cond
      ((/= i (length string)) t)
      (t (setf (state-i *state*) start)
	 nil))))

(defmacro with-substring ((name) &body body)
  "Evaluate the BODY with a function called NAME that returns the substring
from the start of the macro to the current point."
  (with-names (start)
    `(let ((,start (state-i *state*)))
       (flet ((,name ()
		(subseq (state-string *state*) ,start (state-i *state*))))
	 ,@body))))

(defmacro with-parsing ((string &key junk-allowed) &body body)
  "Use the parsing utilites on STRING and return the results."
  (with-names (str)
    `(let* ((,str ,string)
	    (*state* (make-state :string ,str)))
       (declare (type simple-string ,str))
       (values (and (catch 'eof (progn ,@body))
		    (and (not ,junk-allowed)
			 (>= (state-i *state*) (length ,str))))
	       (nreverse (state-results *state*))))))

;; An example.
(defun parse-path (path)
  "Parse a unix path."
  (with-parsing (path)
    ;; This is so we can distinguish between relative and absolute paths.
    ;; Absolute paths will have a "/" as the first element.
    (optional
     (note ("/") (is-char #\/)))
    (one-or-more
     (one-of
      (one-or-more (is-char #\/))
      (with-substring (element)
	(note ((element))
	      (one-or-more
	       (is-not-char #\/))))))))

;; End
