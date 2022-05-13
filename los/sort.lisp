;;;
;;; sort.lisp - Sort or something.
;;;

(defpackage :sort
  (:documentation "Sort things.")
  (:use :cl :dlib)
  (:export
   #:sort-lines
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

#+lish
(lish:defcommand sort
  ((numeric boolean :short-arg #\n
    :help "True to treat the line as starting with a number.")
   (reverse boolean :short-arg #\r
    :help "True to reverse the sort order.")
   (ignore-case boolean :short-arg #\i
    :help "True to ignore the case of strings.")
   (sequence boolean :short-arg #\c
    :default (lish:accepts 'sequence)
    :help "True to return a sequence instead of printing.")
   (files string :repeating t
    :help "Files to sort."))
  :accepts (:stream :sequence)
  "Sort input lines."
  (let ((lines '())
	(numeric-compare (if reverse #'> #'<))
	(string-compare
	 (if reverse
	     (if ignore-case #'string-greaterp #'string>)
	     (if ignore-case #'string-lessp #'string<))))
    (flet ((get-the-lines (stream)
	     (setf lines
		   (nconc
		    (loop :with line
		       :while (setf line (read-line stream nil nil))
		       :collect line)
		    lines)))
	   (numeric-sort ()
	     (let ((ll (loop :for l :in lines :collect
			  (list (or (ignore-errors
				      (parse-integer l :junk-allowed t)) 0)
				l))))
	       (declare (type list ll))
	       (sort-muffled ll numeric-compare :key #'car)))
	   (string-sort ()
	     (sort-lines lines
			 :string-compare string-compare
			 :numeric-compare numeric-compare)))
      (if files
	  (loop :for f :in files :do
	     (with-open-file (stream f :direction :input)
	       (get-the-lines stream)))
	  (if (and lish:*input* (typep lish:*input* 'sequence))
	      (progn
		(dbugf :accepts "sort getting input from *input*~%")
		(setf lines lish:*input*))
	      (progn
		(dbugf :accepts "sort getting input from *standard-input*~%")
		(get-the-lines *standard-input*))))
      (if numeric
	  (if sequence
	      (progn
		(setf lish:*output* (numeric-sort)))
	      (mapcan #'(lambda (x) (princ (second x)) (terpri))
		      (numeric-sort)))
	  (if sequence
	      (progn
		(dbugf :accepts "output to *output*~%accepts is ~a~%"
			lish::*accepts*)
		(setf lish:*output* (string-sort)))
	      (mapcan #'(lambda (x) (princ x) (terpri))
		      (string-sort)))))))

#|
(mapcan #'(lambda (x) (format t "~5d ~a~%" (first x) (second x)))
        (sort (loop for f in (glob "*.lisp")
                  collect (list (wc:count-words f) f))
               #'< :key #'car))
|#

;; EOF
