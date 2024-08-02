;;;
;;; snip.lisp - Snip the output
;;;

(defpackage :snip
  (:documentation "Snip the output.")
  (:use :cl :dlib :opsys :stretchy :utf8b-stream #+lish :lish)
  (:export
   #:!snip
   #:snip-bytes
   #:take-lines
   #:!head
   ))
(in-package :snip)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; @@@ The performance of this is horrible!
(defun snip-bytes (file-or-stream count direction &key collect quiet)
  (declare (type integer count))
  (when (not (or (eq direction :before) (eq direction :after)))
    (error "Direction must be :BEFORE or :AFTER."))
  (let ((result nil) (n 0))
    (declare (type integer n))
    (flet ((put-byte (b)
	     (unless quiet
	       (write-byte b *standard-output*))
	     (when collect
	       (setf (aref result n) b))))
      (with-open-file-or-stream (stream file-or-stream
					:direction :input
					:element-type '(unsigned-byte 8))
	(when collect
	  (setf result (make-array count :element-type '(unsigned-byte 8))))
	(case direction
	  (:before
	   (loop :with b
		 :while (and (< n count)
			     (setf b (read-byte stream nil nil)))
		 :do
		    (put-byte b)
		    (incf n)))
	  (:after
	   (let ((seekable (file-position stream 0)))
	     (if seekable
		 (progn
		   (when (not (file-position stream count))
		     (error "Can't position stream to ~a." count))
		   (loop :with b
			 :while (setf b (read-byte stream nil nil))
			 :do (put-byte b) (incf n)))
		 (progn
		   ;; Read the non-output part
		   (loop :while (and (< n count)
				     (read-byte stream nil nil))
			 :do (incf n))
		   (setf n 0)
		   ;; Read & output the rest
		   (loop :with b
			 :while (setf b (read-byte stream nil nil))
			 :do (put-byte b) (incf n)))))))
	(finish-output *standard-output*)))
    result))

(defun snip-lines-after (stream count)
  "Write the first ‘count’ lines of ‘stream’ to *standard-output*."
  (let ((n 0))
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (write-line line)
	    (incf n))
	  (return)))))

(defun snip-and-collect (stream count)
  "Write the first ‘count’ lines of ‘stream’ to *standard-output*, and return
them."
  (let ((n 0) result)
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (write-line line)
	    (push line result)
	    (incf n))
	  (return)))
    (nreverse result)))

(defun take-lines (stream count)
  "Return the first ‘count’ lines of ‘stream’."
  (let ((n 0) result)
    (with-lines (line stream)
      (if (< n count)
	  (progn
	    (push line result)
	    (incf n))
	  (return)))
    (nreverse result)))

;; EOF
