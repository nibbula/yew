;;;
;;; tee.lisp - Copy input to multiple outputs.
;;;

(defpackage :tee
  (:documentation "Copy input to multiple outputs.")
  (:use :cl :dlib :opsys)
  (:export
   #:tee
   #:!tee
   ))
(in-package :tee)

(defun tee (&rest files)
  (let* (to-close
	 (source *standard-input*)
	 (input
	   (if (equal (stream-element-type source) '(unsigned-byte 8))
	       (make-instance 'utf8b-input-stream :input-stream source)
	       source))
	 (buffer-size dlib:*buffer-size*)
	 (outputs)
	 (buf (make-array buffer-size
			  :element-type (stream-element-type source)))
	 (pos 0))
    (unwind-protect
	 (progn
	   (setf outputs
		 (cons *standard-output*
		       (loop
			 :with stream
			 :for f :in files
			 :if (and (streamp f) (output-stream-p f))
			   :collect f
			 :else
			   :if (and (or (stringp f) (pathnamep f)))
			     :do
				(setf stream
				      (if (uos:isatty f)
					  (open f :direction :output
						  :if-does-not-exist :create
						  :if-exists :append)
					  (open f :direction :output
						  :if-does-not-exist :create)))
				(push stream to-close)
			     :and
			       :collect stream
			 :else
			   :do
			      (error
			       "~s doesn't look like something I can output to."
			       f))))
	   (loop :do
	     (setf pos (read-sequence buf input))
	     (when (> pos 0)
	       (loop
		 :for out :in outputs
		 :do
		    (write-sequence buf out :end pos)))
	     :while (= pos *buffer-size*)))
      (finish-output)
      (loop :for s :in to-close :do
	(close s)))))

#+lish
(lish:defcommand tee
  ((outputs pathname :repeating t :help "Files or streams to write output to."))
  "Copy input to multiple outputs."
  (apply #'tee outputs)
  (setf lish:*output* lish:*input*))

;; End
