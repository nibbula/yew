;;;
;;; sort.lisp - Sort or something.
;;;

(defpackage :sort
  (:documentation "Sort things.")
  (:use :cl :dlib)
  (:export
   #:sort-lines
   #:!sort
   ))
(in-package :sort)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))
;; (declaim (optimize (speed 3) (safety 0) (debug 3) (space 1)
;; 		   (compilation-speed 0)))

(defun sort-lines (lines &key
			   (numeric-compare #'<)
			   (string-compare #'string<))
  (declare (type function numeric-compare string-compare))
  (when (not (typep lines 'sequence))
    (error "LINES must be a sequence."))
  (when (first lines)
    (sort-muffled lines
	  (etypecase (first lines)
	    (string string-compare)
	    (number numeric-compare)
	    (pathname
	     #'(lambda (a b)
		 (funcall string-compare (namestring a) (namestring b))))))))

#|
(mapcan #'(lambda (x) (format t "~5d ~a~%" (first x) (second x)))
        (sort (loop for f in (glob "*.lisp")
                  collect (list (wc:count-words f) f))
               #'< :key #'car))
|#

;; EOF
