;;;
;;; cat.lisp - Concatenate files
;;;

(defpackage :cat
  (:documentation "Concatenate files. Copy streams.")
  (:use :cl :dlib :utf8b-stream :opsys :dlib-misc)
  (:export
   #:cat
   #:!cat
   #:!slurp
   ))
(in-package :cat)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@
;; This whole thing has so many problems!
;; The problems center around Lisp stream element types and character encoding.
;; Don't use it yet!

;; Perhaps we could treat streams something like the "numerical tower", i.e.
;; object stream
;;   character stream (with an external format)
;;     byte stream
;;       8 bit byte stream
;;
;; Byte streams are an exact representation. Character streams are possibly
;; inexact, although like floats they could be exact depending on content.
;; The point is we don't want to lose information.

#|
(defun cat-copy-stream (source destination)
  "Like copy-stream, but we want to use flexi-streams to handle element type
conversion."
  (if (eql (stream-element-type source)
	   (stream-element-type destination))
      ;; If the source element type and destination 
      (copy-stream *standard-input* *standard-output*)
      (let ((buf (make-array *buffer-size*
			     :element-type (stream-element-type source)))
	    (out (make-flexi-stream
		  destination
		  :element-type (stream-element-type destination)))
	    pos)
	(when (stream-external-format destination)
	  (setf (flexi-stream-external-format out)
		(stream-external-format destination)))
	(loop :do
	   (setf pos (read-sequence buf source))
	   (when (> pos 0)
	     (write-sequence buf out :end pos))
	   :while (= pos *buffer-size*)))))

(defun cat-copy-stream-2 (source destination)
  "Like copy-stream, but we want to use flexi-streams to handle element type
conversion."
  (if (eql (stream-element-type source)
	   (stream-element-type destination))
      (copy-stream *standard-input* *standard-output*)
      (let* ((buf (make-array *buffer-size*
			      :element-type (stream-element-type source)))
	     (source-format (stream-external-format source))
	     (destination-format (stream-external-format destination))
	     out
	     pos)
	;; (when (stream-external-format destination)
	;;   (setf (flexi-stream-external-format out)
	;; 	(stream-external-format destination)))
	(format t "source-format ~a~%" source-format)
	(format t "destination-format ~a~%" destination-format)
	(finish-output)
	(setf out (make-flexi-stream
		   destination
		   :external-format source-format
		   :element-type (stream-element-type source)))
	(loop :do
	   (setf pos (read-sequence buf source))
	   (when (> pos 0)
	     (write-string
	      (flexi-streams:octets-to-string
	       buf :external-format destination-format)
	      out :end pos))
	     ;;(write-sequence buf out :end pos))
	   :while (= pos *buffer-size*)))))

(defun bivalent-copy-stream (source destination)
  "Like copy-stream, but it's bi-valent, so good."
  (if (eql (stream-element-type source)
	   (stream-element-type destination))
      (copy-stream source destination)
      (let ((buf (make-array *buffer-size*
			     :element-type (stream-element-type source)))
	    (out (make-flexi-stream
		  destination
		  :element-type (stream-element-type destination)))
	    pos)
	(when (stream-external-format destination)
	  (setf (flexi-stream-external-format out)
		(stream-external-format destination)))
	(loop :do
	   (setf pos (read-sequence buf source))
	   (when (> pos 0)
	     (write-sequence buf out :end pos))
	   :while (= pos *buffer-size*)))))
|#

(defun slow-byte-copy-stream (source destination)
  "This seems like a slow thing that will only work on bivalent streams?"
  (loop :with b
     :while (setf b (read-byte source nil nil))
     :do (write-byte b destination)))

#|

      | Output type
Input | 
type  | byte| char|other
------+-----+-----+-----
byte  |  x  |  x  |  
      |-----+-----+-----
char  |     |  x  |  
      |-----+-----+-----
other |     |     |  x

x = supported

So, copying char streams to binary streams isn't supported yet, but character
streams are usually converted to the right bits by the external-format.
Also streams with 'other' element types aren't part of the standard, but do
seem like they might be a sensible thing to support.
|#

(defun lossy-copy-stream (source destination)
  (cond
    ((equal (stream-element-type source) (stream-element-type destination))
     (copy-stream source destination))
    ((and (equal (stream-element-type source) '(unsigned-byte 8))
	  (equal (stream-element-type destination) 'character))
     (let ((in-stream (make-instance 'utf8b-input-stream
				     :input-stream source)))
       (copy-stream in-stream destination)))
    (t
     (error "Sorry. I don't know how to copy a ~s stream to ~s stream."
	    (stream-element-type source)
	    (stream-element-type destination)))))

(defparameter *copy-stream-function*
  #|
  #+sbcl #'slow-byte-copy-stream
  #+ccl #'copy-stream
  #-(or sbcl ccl) #'copy-stream
  |#
  #'lossy-copy-stream
  )

;; @@@ Unfortunately, this doesn't do what Unix cat does, in a number of ways.
;; Since there's at least one buffer between it and the underlying read, it
;; doesn't replicate the system buffering. For example, when you read from a tty
;; it doesn't return after one line. Probably other devices/file types have
;; similar issues. Like sockets?

(defun cat (&rest files)
 "Concatenate FILES. If a file name is \"-\" then treat it as *standard-input*.
This allows interspersing standard input between other things."
  (loop :for f :in (or files (list "-")) :do
     (if (equal f "-")
	 (funcall *copy-stream-function* *standard-input* *standard-output*)
	 (with-open-file-or-stream (stream (quote-filename f)
					   :element-type '(unsigned-byte 8))
	   (funcall *copy-stream-function* stream *standard-output*))))
  (finish-output *standard-output*))

;; EOF
