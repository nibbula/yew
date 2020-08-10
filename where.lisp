;;;
;;; where.lisp - Where command.
;;;

(defpackage :where
  (:documentation "Where command.")
  (:use :cl :dlib :collections :lish #| :where-is |#)
  (:export
   #:where
   ))
(in-package :where)

(defun where (predicate sequence)
  "Return the items from sequence for which predicate returns true.
Predicate can be a function of one argument, or a list which which is the body
of a function of the argument ‘_’. If sequence is NIL, do where like a verb
invoking where-is:what-where."
  (flet ((where-what (p s)
	   (when (find-package :where-is)
	     (symbol-call :where-is :where-what p s))))
    (if sequence
	(etypecase predicate
	  (cons
	   (dbugf :where "List predicate ~s~%" predicate)
	   (opick (eval `(lambda (_)
			   (declare (ignorable _))
			   ,predicate))
		  sequence))
	  (function
	   (dbugf :where "function predicate ~s~%" predicate)
	   (opick predicate sequence))
	  (symbol
	   (dbugf :where "symbol predicate ~s~%" predicate)
	   (case (intern (string predicate) :where)
	     ((is am)
	      (dbugf :where "-> ~s ~s~%" (make-symbol (symbol-name predicate))
		     sequence)
	      (where-what predicate sequence))
	     (otherwise
	      (opick predicate sequence)))))
	(where-what predicate sequence))))

#+lish
(lish:defcommand where
  ((predicate object
    :required t
    :help
    "A function designator which returns true for items to keep from sequence.")
   (print boolean :short-arg #\p :help "Print the result.")
   (sequence object :rest t
    :help "A sequence to use instead of *input*."))
  "A word which does more than one thing.
As an adverb, act like remove-if-not or opick.
As a verb, find where something is.
The default is to act like an adverb is a sequence-like thing is provided as
*input* or with the seqence argument, and as a verb if a sequence is not
provided."
  :accepts 'collection
  (dbugf :where "predicate ~s ~s~%sequence ~s ~s~%"
	 (type-of predicate) predicate
	 (type-of sequence) sequence)
  (prog1 (setf *output*
	       (where predicate (or *input* sequence)))
    (when print
      (write *output*)
      (terpri))))

;; End
