;;
;; doc.lisp - Dig up documentation
;;

(defpackage :doc
  (:documentation
"Doc tries to find documentation for something. It knows how to find
documentation for a Lisp symbol or object, regardless of what package it's in,
if it has one of the standard documentation types of:
  FUNCTION
  VARIABLE
  STRUCTURE
  TYPE
  SETF
  COMPILER-MACRO
  METHOD-COMBINATION
It also can find the documenation for shell commands.

The DOC package provides:
  - DOC macro, which is convenient from a REPL so you don't have to quote
    things.
  - %DOC function 
    If you want to find documentation for a value, or direct the output to
    somewhere other than *STANDARD-OUTPUT*
  - \"doc\" command
    Useful from the shell. The \"doc\" command also just calls describe if it
    can't find anything else.")
  (:use :cl :dlib :dlib-misc :grout :completion :syntax)
  (:export
   #:doc
   ))
(in-package :doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *doc-types* '(compiler-macro function method-combination setf
			    structure type variable)
  "Types of documentation to try for the DOC function.")

;; Paragraphs delimited by two #\newline
;; Format based on first character
;;   alpha - accumulate line in paragraph to be justified
;;   anything else - leave alone

#|
(defun output-text (s)
  (let (par new-paragraph)
    (labels ((print-it ()
	       (when new-paragraph
		 (grout-princ (s+ #+nil "•" #\newline)))
	       (grout-color :white :default
			    (justify-text (join-by-string (nreverse par) #\space)
					  :stream nil
					  :cols (grout-width)))
	       (grout-princ (s+ #+nil "‼" #\newline))
	       (setf new-paragraph t)))
      (loop :for l :in (split-sequence #\newline s)
	 :do
	 (cond
	   ((zerop (length l))
	    (when par
	      (print-it)
	      (setf par nil))
	    ;;(grout-princ (s+ #+nil "•" #\newline))
	    )
	   ((alpha-char-p (char l 0))
	    (push l par))
	   (t
	    (when par
	      (print-it)
	      ;;(setf new-paragraph nil)
	      ;;(grout-princ (s+ "˚" #\newline))
	      (setf par nil))
	    (grout-color :white :default l)
	    (grout-princ (s+ #+nil "¶" #\newline)))))
      (when par
	(print-it)))))
|#

(defun output-text (s)
  (format-comment-text (make-instance 'syntax-lisp:lisp-token :object s)
		       (grout-stream *grout*)))

(defun %doc (x &key (stream *standard-output*) (all t))
  "Show the documentation for something."
  (when (stringp x)
    (setf x (make-symbol x)))
  (let ((did-one nil) (did-structure nil)
	(p (ignore-conditions (type-error) (find-package x))))
    (with-grout (*grout* stream)
      (labels ((maybe-doc (obj type)
		 (without-warning (documentation obj type)))
	       (print-doc (sym pkg doc-type &optional fake-type)
		 (when (maybe-doc sym doc-type)
		   (when did-one
		     (grout-princ #\newline))
		   (if (and (eq doc-type 'function) (maybe-doc sym doc-type))
		       (progn
			 (when (and pkg (not (eq pkg (find-package :cl))))
			   (grout-color :green :default
					(format nil "~a " (package-name pkg)))
			   (grout-color :green :default
					(format nil "~:(~a~):~%"
						(or fake-type doc-type))))
			 (grout-format "~a~%" (function-help sym 0)))
		       (progn
			 (grout-color :green :default
				      (format nil "~:(~a~): "
					      (or fake-type doc-type)))
			 (when pkg
			   (grout-color :green :default
					(format nil "~a:" (package-name pkg))))
			 (grout-color :green :default (format nil "~a~%" sym))))
		   (if (eq doc-type :command)
		       (grout-color :white :default
				    (format nil "~a"
					    (maybe-doc sym doc-type)))
		       (progn
			 (output-text (maybe-doc sym doc-type))
			 ;;(grout-princ #\newline)
			 ))
		   (setf did-one t)))
	       (do-docs (sym pkg)
		 (loop :for d :in *doc-types*
		    :do
		    ;; Don't print duplicate type documentation for structures,
		    (when (and (eq d 'structure) (maybe-doc sym d))
		      (setf did-structure t))
		    (when (not (and (eq d 'type) did-structure))
		      (print-doc sym pkg d)))))
	;;(do-docs x nil)
	(when (and (or (not did-one) all)
		   #+sbcl (not (symbolp x)) ; sbcl warns about this
		   (maybe-doc x t))
	  ;;(grout-color :white :default
	  ;;       (format nil "~a~%" (maybe-doc x t)))
	  (when did-one
	    (grout-princ #\newline))
	  (output-text (maybe-doc x t))
	  (setf did-one t))
	;; We have to check packages separately
	(when (and p (maybe-doc p t))
	  (when did-one
	    (grout-princ #\newline))
	  (grout-color :green :default "Package:")
	  (grout-princ #\newline)
	  ;;(grout-color :white :default
	  ;;	       (format nil "~%~a~%" (maybe-doc p t)))
	  (output-text (maybe-doc p t))
	  (setf did-one t))
	;; Check for the symbol name in other packages
	(when (and (symbolp x) (or (not did-one) all))
	  (loop :for s :in (find-all-symbols (symbol-name x))
	     :do
	     ;(when (not (eq (symbol-package s) (find-package :cl)))
	     (do-docs s (symbol-package s))))
	;; Check for the command symbol in other packages
	;; (when (or (not did-one) all)
	;;   (loop :for s :in (find-all-symbols (s+ "!" (symbol-name x)))
	;;      :do
	;;      ;;(do-docs s (symbol-package s))
	;;      (print-doc s (symbol-package s) 'function 'command)))
	(when (or (not did-one) all)
	  (print-doc x nil :command))
	;;(when (not did-one)
	;; (grout-format "Nothing.~%"))
	))
    did-one))

;; This is a macro just so we don't have to quote the argument.
(defmacro doc (x)
  "Show the documentation for something."
  (cond
    ((or (listp x)
	 (stringp x))
     `(%doc ,x))
    ((symbolp x)			; Unbound symbol, so:
     `(%doc ',x))			; quote it.
    (t
     `(progn (format t "Nothing.~%") nil))))

#+lish
(lish:defcommand doc (("thing" object :required nil))
  "Show documentation for something named by THING."
  (if (not thing)
      (format t "~
This shows documentation strings for something.
Try typing \"doc doc\".
")
      (progn
	;;(format t "Thing is a ~s with the value ~s.~%" (type-of thing) thing)
	(when (not (%doc thing))
	  (describe thing)))))

;; EOF
