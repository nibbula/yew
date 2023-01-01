;;;
;;; deblarg-ccl.lisp - Clozure Common Lisp specific parts of the debugger.
;;;

(in-package :deblarg)

(defclass ccl-deblargger (deblargger)
  ()
  (:documentation "Deblarger for CCL."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *deblargger-implementation-class* 'ccl-deblargger))

#|
(defun debugger-backtrace-lines (n)
  "Our own backtrace for CCL."
  (let ((frames '()) (*print-readably* nil))
    (ccl:map-call-frames
     #'(lambda (frame-ptr context)
	 (push (list frame-ptr context) frames))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (setf frames (nreverse frames))
    (loop :with i = 0
       :for (f context) :in frames
       :collect
       (format nil "~3a (~a~{ ~s~})~%"
	       i (ccl:function-name (ccl:frame-function f context))
	       (ccl:frame-supplied-arguments f context))
       :do
       (incf i)
       :while (< i n))))
|#

(defmethod debugger-backtrace-lines ((d ccl-deblargger) n)
  (let ((current-frame
	  (or (and d (debugger-current-frame d))
	      (debugger-internal-frame))))
    (loop :with i = 0
       :for b :in (ignore-errors (ccl::backtrace-as-list))
       :if (>= i current-frame)
       ;; :collect (format nil "~(~3d ~a~)" i b)
       :collect (cons i (string-downcase (ignore-errors (princ-to-string b))))
       :end
       :do
       (incf i)
       :while (< i (+ current-frame n)))))

(defmethod debugger-backtrace ((d ccl-deblargger) n)
  "Our own backtrace for CCL."
  (declare (ignore n))			; @@@
  (let ((frames '()) (*print-readably* nil))
    (ignore-errors
     (ccl:map-call-frames
      #'(lambda (frame-ptr context)
	  (push (list frame-ptr context) frames))
      :origin ccl:*top-error-frame*
      :start-frame-number 0
      :count most-positive-fixnum))
    (setf frames (nreverse frames))
    (loop :with i = 0
       :for (f context) :in frames
       :do
       (print-span `((:fg-yellow ,(format nil "~3a" i))
		     ,(or
		       (ignore-errors
			(format nil " (~(~a~{ ~s~}~))~%"
				(or (ignore-errors
				     (ccl:function-name
				      (ccl:frame-function f context))) "")
				(or (ignore-errors
				     (ccl:frame-supplied-arguments
				      f context)))))
		       `(:red
			 ,(format
			   nil " Error printing frame ~a~%" f)))))
       (incf i))))

(defun OLD-get-frame (n)
  (let ((f 0) frame-ptr context)
    (ccl:map-call-frames
     #'(lambda (p c)
	 (when (= f n)
	   (setf frame-ptr p
		 context c)
	   (incf f)))
     :origin ccl:*top-error-frame*
     :start-frame-number 0
     :count most-positive-fixnum)
    (values frame-ptr context)))

(defun get-frame (n)
  (let ((f 0) frame-ptr context)
    (ccl:map-call-frames
     #'(lambda (p c)
	 (when (= f (1+ n))
	   (setf frame-ptr p
		 context c))
	 (incf f)))
    (values frame-ptr context)))

;; (ccl:map-call-frames
;;  #'(lambda (p c)
;;      (multiple-value-bind (func pc) (ccl:frame-function p c)
;;        (format t "~s ~s ~4d ~s~%~s~%" p c pc func
;; 	       (ccl:source-note-filename (ccl:function-source-note func))))))

(defun debugger-source-note (n)
  (multiple-value-bind (pointer context) (get-frame n)
    (multiple-value-bind (func pc) (ccl:frame-function pointer context)
      (if pc
	  (or (ccl:find-source-note-at-pc func pc)
	      (ccl:function-source-note func))
	  (ccl:function-source-note func)))))

(defmethod debugger-show-source ((d ccl-deblargger) n)
  (when (not n)
    (setf n 0))
  (let ((*print-readably* nil))
    (multiple-value-bind (pointer context) (get-frame n)
      (multiple-value-bind (func pc) (ccl:frame-function pointer context)
	(let ((note (if pc
			(or (ccl:find-source-note-at-pc func pc)
			    (ccl:function-source-note func))
			(ccl:function-source-note func))))
	  (when note
	    (cond
	      ((ccl:source-note-text note)
	       (format *debug-io* "~a~%" (ccl:source-note-text note)))
	      ((ccl:source-note-filename note)
	       (with-open-file (stream (ccl:source-note-filename note))
		 (file-position stream (ccl:source-note-start-pos note))
		 (let* ((len (- (ccl:source-note-end-pos note)
				(ccl:source-note-start-pos note)))
			(str (make-array (list len) :element-type 'character)))
		   (read-sequence str stream)
		   (format *debug-io* "~a~%" str)))))))))))

(defmethod debugger-source-path ((d ccl-deblargger) frame
				 #| &optional (window-size 10) |#)
  ;; (declare (ignore window-size)) ; @@@
  (let ((note (debugger-source-note frame)))
    (cond
      ((not note)
       ":Unknown")
      ((ccl:source-note-filename note)
       (ccl:source-note-filename note))
      (t
       ":Internal"))))

(defmethod debugger-source ((d ccl-deblargger) frame
			    &optional (source-height 10))
  (let ((note (debugger-source-note frame)))
    (if (not note)
	(list "Sorry, I can't figure it out.")
	(cond
	  ((ccl:source-note-text note)
	   (list (format nil "~a~%" (ccl:source-note-text note))))
	  ((ccl:source-note-filename note)
	   (with-open-file (stream (ccl:source-note-filename note))
	     (file-position stream (ccl:source-note-start-pos note))
	     #|(let* ((len (- (ccl:source-note-end-pos note)
			    (ccl:source-note-start-pos note)))
		    (str (make-array (list len) :element-type 'character)))
	       (read-sequence str stream)
	     (format nil "~a~%" str)))))))) |#
	     (loop :with line
		:for i :from 1 :to source-height
		:while (setf line (read-line stream nil nil))
		:collect line)))))))

(defmethod debugger-show-locals ((d ccl-deblargger) n)
  (let ((*print-readably* nil))
    (multiple-value-bind (pointer context) (get-frame n)
      (loop :for (name . value) :in (ccl:frame-named-variables pointer context)
	 :do
	 (print-span
	  `((:magenta ,(princ-to-string name)) " = "
	    (:green ,(prin1-to-string value)) #\newline))
	 ;; (format *debug-io* "~a = ~a~%" name value)
	 ))))

(defmethod debugger-old-backtrace ((d ccl-deblargger) n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  (loop :with i = 0
     :for b :in (ccl::backtrace-as-list)
     :do
     (print-span `((:fg-yellow ,(format nil "~3d" i))
		   ,(format nil " ~(~a~)~%" b)))
     ;;(format *debug-io* "~(~3d ~a~)~%" i b)
     (incf i)
     :while (or (null n) (and (numberp n) (< i n)))))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  ;;  #+ccl ccl:*top-error-frame*
  0)

(defmethod debugger-up-frame ((d ccl-deblargger) &optional (count 1))
  (with-slots (current-frame) d
    (when (> current-frame 0)
      (decf current-frame (or count 1)))))

(defmethod debugger-down-frame ((d ccl-deblargger) &optional (count 1))
  (with-slots (current-frame) d
    (incf current-frame (or count 1))))

(defmethod debugger-set-frame ((d ccl-deblargger) frame)
  (with-slots (current-frame) d
    (setf current-frame frame)))

(defmethod debugger-top-frame ((d ccl-deblargger) count)
  (declare (ignore count))
  (with-slots (current-frame) d
    (setf current-frame 0))) ;; XXX wrong?

(defmethod debugger-eval-in-frame ((d ccl-deblargger) form n)
  (multiple-value-bind (pointer context) (get-frame n)
    (let ((vars (ccl:frame-named-variables pointer context)))
      (eval `(let ,(loop :for (var . val) :in vars :collect `(,var ',val))
               (declare (ignorable ,@(mapcar #'car vars)))
               ,form)))))

(defmethod debugger-hook ((d (eql 'ccl-deblargger)))
  *debugger-hook*)

(defmethod debugger-set-hook ((d (eql 'ccl-deblargger)) function)
  (setf *debugger-hook* function
	ccl:*break-hook* function))

;; EOF
