;;;
;;; file.lisp - WTF is this file?
;;;

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
  "Print the content type to ‘stream’. ‘type’ should be magic:content-type
structure. If ‘full’ is true print all the data, otherwise just print the
description."
  (if full
      (with-grout ()
	(grout-print-table
	 (make-table-from
	  (append `(("File" . ,(s+ ": " thing)))
		  (mapcar (_ (cons (string-capitalize _)
				   (s+ ": "
				       (funcall
					(symbolify (s+ "content-type-" _)
						   :package :magic) type))))
			  '(name category description file-name-match encoding
			    properties)))
	  :columns '((:name "Name") (:name "Value" :align :wrap)))
	 :stream stream
	 :print-titles nil)
	(grout-princ #\newline))
      (format stream "~a~%" (content-type-description type))))

(defparameter *signal-errors* nil
  "True to signal file errors instead of printing them.")

(defun safer-guess-file-type (file &key device-contents-p)
  (if (not *signal-errors*)
      (handler-case
	  (guess-file-type file :device-contents-p device-contents-p)
	((or stream-error file-error opsys:opsys-error) (c)
	  (finish-output)
	  (let ((*print-pretty* nil))
	    (format *error-output*
		    ;; "~a: ~a ~a~%" file (type-of c) c))
		    "~a ~a~%" (type-of c) c))
	  (invoke-restart 'continue)))
      (guess-file-type file :device-contents-p device-contents-p)))

(defun describe-content-type (thing &key full (stream t) device-contents-p)
  "Describe the content type of THING to STREAM. THING can be a pathname
designator, or a vector of (unsigned-byte 8). If FULL is true print all the
data, otherwise just print the description. If DEVICE-CONTENTS-P is true, try
to read the contents of the device, otherwise just return the device type based
on metadata."
  (let ((type
	 (typecase thing
	   ((or pathname string)
	    (safer-guess-file-type thing :device-contents-p device-contents-p))
	   ((or stream (vector (unsigned-byte 8)))
	    (guess-content-type thing)))))
    (when type
      (print-content-type thing type :full full :stream stream))
    type))

;; End
