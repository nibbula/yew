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

(defun is-binary (stream)
  "Return true if ‘stream’ has an element-type of (unsigned-byte 8)."
  (equal (stream-element-type stream) '(unsigned-byte 8)))

(defun all-outputs-are-binary (files-or-streams)
  "Return true if all elements of the list ‘files-or-streams’ are strings or
pathnames, or satisfy ‘is-binary.’."
  (every (_ (or (pathnamep _) (stringp _) (and (streamp _) (is-binary _))))
	 files-or-streams))

(defun tee (&rest files)
  (let* (to-close
	 (source *standard-input*)
	 (all-binary-out (all-outputs-are-binary (append (list source) files)))
	 ;; @@@ The default is 'character not :default, so we shouldn't use
	 ;; :default here right?
	 (output-element-type (if all-binary-out '(unsigned-byte 8) 'character))
	 (input
	   ;; If the source in binary, and all the outputs aren't, we convert to
	   ;; utf8b.
	   (if (and (is-binary source) (not all-binary-out))
	       (make-instance 'utf8b-input-stream :input-stream source)
	       source))
	 (buffer-size dlib:*buffer-size*)
	 (outputs)
	 (buf (make-array buffer-size
			  :element-type (stream-element-type input)))
	 (pos 0))
    ;; (format *debug-io* "all-binary-out ~s~%output-element-type ~s~%"
    ;; 	    all-binary-out output-element-type)
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
						  :if-exists :append
						  :element-type
						  output-element-type)
					  (open f :direction :output
						  :if-does-not-exist :create
						  :element-type
						  output-element-type)))
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
