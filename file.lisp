;;
;; file.lisp - WTF is this file?
;;

(defpackage :file
  (:documentation "I can't believe you.")
  (:use :cl :dlib :dlib-misc :magic :grout :table)
  (:export
   #:describe-content-type
   #:!file
   ))
(in-package :file)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun print-content-type (thing type &key full (stream t))
  "Print the content type to STREAM. TYPE should be magic:content-type
structure. If FULL is true print all the data, otherwise just print the
description."
  (if full
      (progn
	(print-properties
	 (append `((file . ,thing))
		 (mapcar (_ (cons _ (funcall
				     (symbolify (s+ "content-type-" _)
						:package :magic) type)))
			 '(name category description file-name-match encoding
			   properties)))
	 :stream stream)
	(terpri))
      (format stream "~a~%" (content-type-description type))))

(defun describe-content-type (thing &key full (stream t))
  "Describe the content type of THING to STREAM. THING can be a pathname
designator, or a vector of (unsigned-byte 8). If FULL is true print all the
data, otherwise just print the description."
  (let ((type
	 (typecase thing
	   ((or pathname string)
	    (guess-file-type thing))
	   ((or stream (vector (unsigned-byte 8)))
	    (guess-content-type thing)))))
    (print-content-type thing type :full full :stream stream)
    type))

#+lish
(lish:defcommand file
  ;; BUG: we should be able to specify :optional nil and not get FILES twice
  ;; in the arg list.
  ((full boolean :short-arg #\f
    :help "True to show more information about the file.")
   (collect boolean :short-arg #\c
    :help "True to set *output* to a sequence of content-type structures.")
   (files pathname #|:optional nil XXX |# :repeating t
    :help "Files to identify."))
  :accepts (or sequence pathname string)
  "Try to guess what a file is."
  (with-grout ()
    (let (results)
      (flet ((do-file-list (file-list)
	       (if full
		   (loop :for f :in file-list :do
		      (push (list f (describe-content-type f :full t)) results))
		   (grout-print-table
		    (make-table-from
		     (loop :for f :in file-list
			:collect
			(list (s+ f ":")
			      (progn
				(push (list f (guess-file-type f)) results)
				(content-type-description
				 (second (car results)))))))
		    #| :trailing-spaces nil |#
		    :print-titles nil))))
	(when lish:*input*
	  (do-file-list
	      (if (listp lish:*input*) lish:*input* (list lish:*input*))))
	(if files
	    (do-file-list files)
	    ;; (when (not lish:*input*)
	    ;;   (grout-format "You should probably supply a file name to guess ~
	    ;; 		   the content of.~%"))
	    (when (not lish:*input*)
	      ;; @@@ This might only work if *standard-input* is bi-valent?!
	      (let ((content
		     (slurp *standard-input* :element-type '(unsigned-byte 8))))
		(push (list "*standard-input*"
			    (describe-content-type content :full full))
		      results)))
	    )
	(when collect
	  (setf lish:*output* (nreverse results)))))))

;; EOF
