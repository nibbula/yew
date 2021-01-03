;;
;; deblarg-ccl.lisp - Clozure Common Lisp specific parts of the debugger.
;;

(in-package :deblarg)

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

(defun debugger-backtrace-lines (n)
  (with-slots (current-frame) *deblarg*
    (loop :with i = 0
       :for b :in (ccl::backtrace-as-list)
       :if (>= i current-frame)
       ;; :collect (format nil "~(~3d ~a~)" i b)
       :collect (cons i (string-downcase (princ-to-string b)))
       :end
       :do
       (incf i)
       :while (< i (+ current-frame n)))))

(defun debugger-wacktrace (n)
  "Our own backtrace for CCL."
  (declare (ignore n))			; @@@
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
       :do
       ;; (format *debug-io* "~3a (~(~a~{ ~s~}~))~%" i
       ;; 	       (or (ignore-errors
       ;; 		     (ccl:function-name (ccl:frame-function f context))) "")
       ;; 	       (or (ignore-errors
       ;; 		     (ccl:frame-supplied-arguments f context)) '("")))
       (print-span `((:fg-yellow ,(format nil "~3a" i))
		     ,(format nil " (~(~a~{ ~s~}~))~%"
			      (or (ignore-errors
				    (ccl:function-name
				     (ccl:frame-function f context))) "")
			      (or (ignore-errors
				    (ccl:frame-supplied-arguments f context))))))
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

(defun debugger-show-source (n)
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

(defun debugger-source-path (frame &optional (window-size 10))
  (declare (ignore window-size)) ; @@@
  (let ((note (debugger-source-note frame)))
    (cond
      ((not note)
       ":Unknown")
      ((ccl:source-note-filename note)
       (ccl:source-note-filename note))
      (t
       ":Internal"))))

(defun debugger-source (frame &optional (window-size 10))
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
		:for i :from 1 :to window-size
		:while (setf line (read-line stream nil nil))
		:collect line)))))))

(defun debugger-show-locals (n)
  (let ((*print-readably* nil))
    (multiple-value-bind (pointer context) (get-frame n)
      (loop :for (name . value) :in (ccl:frame-named-variables pointer context)
	 :do
	 (print-span
	  `((:magenta ,(princ-to-string name)) " = "
	    (:green ,(prin1-to-string value)) #\newline))
	 ;; (format *debug-io* "~a = ~a~%" name value)
	 ))))

(defun debugger-backtrace (n)
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

(defun debugger-up-frame (&optional (count 1))
  (with-slots (current-frame) *deblarg*
    (when (> current-frame 0)
      (decf current-frame (or count 1)))))

(defun debugger-down-frame (&optional (count 1))
  (with-slots (current-frame) *deblarg*
    (incf current-frame (or count 1))))

(defun debugger-set-frame (frame)
  (with-slots (current-frame) *deblarg*
    (setf current-frame frame)))

(defun debugger-top-frame (count)
  (declare (ignore count))
  (with-slots (current-frame) *deblarg*
    (setf current-frame 0))) ;; XXX wrong?

(defun debugger-eval-in-frame (form n)
  (multiple-value-bind (pointer context) (get-frame n)
    (let ((vars (ccl:frame-named-variables pointer context)))
      (eval `(let ,(loop :for (var . val) :in vars :collect `(,var ',val))
               (declare (ignorable ,@(mapcar #'car vars)))
               ,form)))))

(defun activate-stepper (&key quietly)
  "Activate the setpper."
  (declare (ignore quietly))
  (values))

(defun debugger-hook ()
  *debugger-hook*)

(defun set-debugger-hook (function)
  (setf *debugger-hook* function
	ccl:*break-hook* function))

;; EOF
