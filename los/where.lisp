;;;
;;; where.lisp - Where command.
;;;

(defpackage :where
  (:documentation "Where command.")
  (:use :cl :dlib :collections  #| :where-is |#)
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

;; End
