;;;
;;; parse-util.lisp - Parsing utilities.
;;;

(defpackage :parse-util
  (:documentation "Parsing utilities.

This is a small set of macros for constructing simple recursive descent
parsers. It's probably most applicable to simple grammars and short inputs.
A set of macros inside a ‘with-parsing’ macro, which returns the parse
results. Results are created with ‘note’ macro.

The source is sequence of elements. In the common case of parsing a string, the
sequence is a string and the elements are characters.

Quantifiers macros are:
   (sequence-of ...)   Every form of the body must be true.
   (one-of ...)        Return after the first optional expression of the body
                       is true.
   (optional ...)      If the body doesn't evaluate to true, restore the point.
   (zero-or-more ...)  Do the body, until it returns false. Always true.
   (one-or-more ...)   Do the body once and until it is false.
   (must-be ...)       The body must evaluate true.

Result macros:
   (note (x) ...)      Push ‘x’ onto the results if the ‘body’ is true.
   (with-sub-sequence (name) ...)
                       Make name be a function that returns the current
                       sub-sequence, which the macro encloses.
   (note-part (x) ...) Combination of note and with-sub-sequence, like:
                       (with-sub-sequence (x)
                          (note ((x)) ...))

Element functions are:
   (peek)                        Return the next element.
   (next-element)                Return and consume the next element.
   (element-in E SEQ &key test)  Return true if E is in SEQ.
   (is-element E &key test)      Return true if E is the next element.
   (is-not-element E &key test)  Return true if E is not the next element.
   (in-sequence SEQ &key test)   Return true if the next element is in SEQ.
   (is-sequence SEQ &key test)   Return true if the next elementss are SEQ.

See parse-util-test for examples.
")
  (:use :cl :dlib :collections)
  (:nicknames :pu) ;; @@@
  (:export
   ;; state object
   #:state
   #:state-sequence
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
   #:with-sub-sequence
   #:note-part
   #:with-parsing
   ;; functions
   #:peek
   #:next-element
   #:element-in
   #:is-element
   #:is-not-element
   #:in-sequence
   #:is-sequence
   ))
(in-package :parse-util)

(defstruct state
  "Parsing state."
  sequence
  (i          0   :type fixnum)
  (next       nil :type list)
  (track-next nil :type boolean)
  (results    nil :type list))

(defparameter *state* nil
  "Parsing state.")

(defun at-end ()
  (>= (state-i *state*) (olength (state-sequence *state*))))

#+(or)
(progn
  (defparameter *do-derp* nil)
  (defun derp ()
    "Visual debugging of parsers."
    (when *do-derp*
      (terminal:with-immediate ()
	(terminal:tt-move-to 0 0)
	(terminal:tt-clear)
	(terminal:tt-erase-below)
	(terminal:tt-write-string (state-sequence *state*))
	(terminal:tt-newline)
	(dotimes (i (state-i *state*)) (terminal:tt-write-char #\space))
	(terminal:tt-format "^~%")
	(terminal:tt-format "~(~a~)~%~a~%" (state-next *state*)
			    (state-results *state*))
	(terminal:tt-format "~s~%" *state*)
	(when (eql #\q (terminal:tt-get-key))
	  (throw 'eof nil))))))

(defmacro with-state ((&optional (result-form nil result-provided-p))
		      &body body)
  "Wrapper to handle backtracking and next tracking."
  (with-names (start result)
    `(let ((,start (state-i *state*)) ,result)
       (when (state-track-next *state*)
	 (setf (state-next *state*) ',body))
       ;; (derp)
       (when (not (setf ,result (progn ,@body)))
	 (setf (state-i *state*) ,start))
       ;; (derp)
       ,(if (not result-provided-p) result result-form))))

(defmacro optional (&body body)
  "If BODY doesn't evaluate to true, restore the point."
  `(with-state (t)
     ,@body))

(defmacro must-be (&body body)
  "BODY must evaluate true."
  `(with-state ()
     ,@body))

(defmacro sequence-of (&body body)
  "Every form of BODY must be true."
  `(with-state ()
     (and ,@body)))

(defmacro one-or-more (&body body)
  "Do the body once and until it is false."
  (with-names (thunk)
    `(flet ((,thunk () ,@body))
       (when (state-track-next *state*)
	 (setf (state-next *state*) ',body))
       (when (must-be (,thunk))
	 (loop :while (must-be (and (not (at-end)) (,thunk))))
	 t))))

(defmacro zero-or-more (&body body)
  "Do the body, until it returns false. Always true."
  `(progn
     (when (state-track-next *state*)
       (setf (state-next *state*) ',body))
     (loop :while (must-be (and (not (at-end)) (progn ,@body))))
     t))

(defmacro one-of (&body body)
  "Return after the first optional expression of BODY is true."
  (let ((choices (loop :for expr :in body :collect `(must-be ,expr))))
    `(progn
       (when (state-track-next *state*)
	 (setf (state-next *state*) ',body))
       (or ,@choices))))

(defmacro note ((x) &body body)
  "Push ‘x’ onto the results if the ‘body’ is true."
  (with-names (r)
    `(progn
       ;; (setf (state-next *state*) ',body)
       (let ((,r (progn ,@body)))
	 (when ,r
	   (push ,x (state-results *state*)))
	 ,r))))

(defun peek ()
  "Return the next character."
  (with-slots (i sequence) *state*
    (declare ;; (type simple-string string)
	     (type fixnum i))
    (when (< i (olength sequence))
      (oelt sequence i))))

(defun next-element ()
  "Return and consume the next character."
  (with-slots (i sequence) *state*
    (declare ;; (type simple-string string)
	     (type fixnum i))
    (if (< i (olength sequence))
	(prog1 (oelt sequence i)
	  (incf i))
        (progn
	  (throw 'eof nil)))))

(defun element-in (e sequence &key (test #'equal))
  "True if C is in STRING."
  ;; (declare (type simple-string string)
  ;; 	   (type character c))
  (ofind e sequence :test test))

(defun is-element (element &key (test #'equal))
  "Return true if ELEMENT is the next element."
  (and (peek)
       (funcall test (next-element) element)))

(defun is-not-element (element &key (test #'equal))
  "Return true if ELEMENT is NOT the next element."
  (and (peek)
       (not (funcall test (next-element) element))))

;; (defun in-string (string)
;;   "Return true if the next character is in STRING."
;;   (and (peek)
;;        (find (peek) string)
;;        (next-char)))

(defun in-sequence (sequence &key (test #'equal))
  "Return true if the next character is in STRING."
  (and (peek)
       (ofind (peek) sequence :test test)
       (next-element)))

#+(or)
(defun is-string (string)
  "Return true if the next characters are STRING."
  (declare (type simple-string string))
  (let ((start (state-i *state*))
	(len (length string))
	(i 0))
    (loop
       :while (and (< i len) (peek) (char= (peek) (char string i)))
       :do (next-char) (incf i))
    (cond
      ((= i (length string)) t)
      (t (setf (state-i *state*) start)
	 nil))))

(defun is-sequence (sequence &key (test #'equal))
  "Return true if the next elements are SEQUENCE."
  ;;(declare (type simple-string string))
  (let ((start (state-i *state*))
	(len (olength sequence))
	(i 0))
    (loop
       :while (and (< i len) (peek) (funcall test (peek) (oelt sequence i)))
       :do (next-element) (incf i))
    (cond
      ((= i (olength sequence)) t)
      (t (setf (state-i *state*) start)
	 nil))))

(defmacro with-sub-sequence ((name) &body body)
  "Evaluate the BODY with a function called NAME that returns the substring
from the start of the macro to the current point."
  (with-names (start)
    `(let ((,start (state-i *state*)))
       (declare (ignorable ,start))
       (symbol-macrolet ((,name (osubseq (state-sequence *state*)
					 ,start (state-i *state*))))
	 ,@body))))

(defmacro note-part ((&optional (name (gensym "NAME"))) &body body)
  "Combination of note and with-sub-sequence."
  `(with-sub-sequence (,name)
     (note ((,name)) ,@body)))

(defmacro with-parsing ((sequence &key junk-allowed track-next) &body body)
  "Use the parsing utilites on STRING and return the results."
  (with-names (seq)
    `(let* ((,seq ,sequence)
	    (*state* (make-state :sequence ,seq :track-next ,track-next)))
       ;; (declare (type simple-string ,str))
       (values (and (catch 'eof (progn ,@body))
		    (and (not ,junk-allowed)
			 (>= (state-i *state*) (olength ,seq))))
	       (nreverse (state-results *state*))
	       (state-next *state*)))))

;; End
