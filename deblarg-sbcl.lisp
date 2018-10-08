;;
;; deblarg-sbcl.lisp - SBCL specific debugger pieces.
;;

(in-package :deblarg)

(declaim
 (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; Temporarily set the feature if the implementation supports breakpoints.
#+sbcl (eval-when (:compile-toplevel)
	 (d-add-feature :tdb-has-breakpoints))

(defun print-frame (f &optional (stream *debug-io*))
  "Print a frame."
  (labels ((print-lambda-list (f arg-list)
	     (loop :for v :in arg-list
		:do
		(cond
		  ((sb-di::debug-var-p v)
		   ;;(sb-di::debug-var-symbol v)
		   (let ((vv (handler-case
				 (sb-di::debug-var-value v f)
			       (condition () '#:|<Unavailable>|))))
		     (when (not (or (eql vv '#:|<Unavailable>|)
				    (typep vv 'condition)))
		       (write-char #\space stream)
		       (display-value vv stream))))
		  ((and (listp v) (eql (car v) :rest))
		   (format stream " &rest")
		   (print-lambda-list f (cdr v)))))))
    (let* ((loc     (sb-di:frame-code-location f))
	   (dbg-fun (sb-di:code-location-debug-fun loc))
	   (name    (sb-di:debug-fun-name dbg-fun)))
      (format stream "(")
      (if (symbolp name)
	  (let ((pkg (symbol-package name)))
	    (if (not (eql (find-package :cl) pkg))
		(format stream "~(~a~)::" (package-name pkg)))))
      (format stream "~(~a~)" name)
      (if (sb-di::debug-fun-%lambda-list dbg-fun)
	  (print-lambda-list f (sb-di::debug-fun-lambda-list dbg-fun))
	  (princ " <unavailable>" stream))
      (format stream ")"))))

(defun sbcl-start-frame ()
  ;; introduced sometime after 1.0.57
  (let ((sym (find-symbol "BACKTRACE-START-FRAME" :sb-debug)))
    (if sym
     (funcall sym :debugger-frame)
     (sb-di:top-frame))))

(defun debugger-wacktrace (n)
  "Our own backtrace for SBCL."
  ;; (or *saved-frame* (sb-di:top-frame))
  (loop
     :with f = (sbcl-start-frame)
     :and i = 0
     :do
     ;; (format *debug-io* "~3d " (sb-di:frame-number f))
     ;; (print-frame f)
     ;; (terpri *debug-io*)
     (print-span `((:fg-yellow ,(format nil "~3d" (sb-di:frame-number f))) " "))
     (print-frame f *terminal*)
     (terpri *terminal*)
     (setf f (sb-di:frame-down f))
     (incf i)
     :until (or (not f) (and n (>= i n)))))

(defun debugger-backtrace-lines (n)
  (loop 
;;;     :with f = *saved-frame* #| (sbcl-start-frame) |#
     :with f = *current-frame*
     :for i :from 1 :to n
     :collect
     (with-output-to-string (str)
       (format str "~3d " (sb-di:frame-number f))
       (print-frame f str))
     :do
     (setf f (sb-di:frame-down f))
     :while f))

(defun debugger-eval-in-frame (form n)
  (let ((frame (frame-number-or-current n)))
    (funcall (the function
		  (sb-di:preprocess-for-eval form
					     (sb-di:frame-code-location frame)))
             frame)))

#|#+sbcl
(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
         (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
         (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

#+sbcl
(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun fallback-source-location (code-location)
  )

(defun source-file-source-location (code-location)
  )
|#

#|
#+sbcl
(defun debugger-source (frame-number)
  (when (not frame-number)
    (setf frame-number 0))
  (let ((frame (frame-number frame-number))
	(loc  (sb-di::frame-code-location frame))
	(src  (sb-di::code-location-debug-source loc))
	(filename (sb-c::debug-source-namestring src)))
    (if filename
	(if (code-location-has-debug-block-info-p code-location)
	    (source-file-source-location code-location)
	    (fallback-source-location code-location))
	(prin1-to-string
	 (sb-debug::code-location-source-form loc 100)))))
|#

#|
(with-open-file (str filename)
      (file-position str pos)
      (loop
	 :for i :from 1 :to 10
	 :collect
	 (read-line str nil nil)
	 ;;(format nil "~s~%" (sb-di:debug-fun-start-location dbg-fun))
	 ))))
|#

(defun debugger-source-path (frame)
#|  (let* ((fun (sb-di:code-location-debug-fun
	       (sb-di:frame-code-location frame)))
	 (src (and fun (sb-introspect:find-definition-source
			(symbol-function
			 (sb-di:debug-fun-name fun))))))
    (if src
	(definition-source-pathname src)
	":Unknown"))) |#
  (let* ((loc (sb-di:frame-code-location frame))
	 (src (sb-di::code-location-debug-source loc)))
    (sb-c::debug-source-namestring src)))

;;; Return the number of the form corresponding to CODE-LOCATION. The
;;; form number is derived by a walking the subforms of a top level
;;; form in depth-first order.
;;; (defun code-location-form-number (code-location)

(defun get-loc-subform-pos (loc)
  "Return the file position of the subform."
  (declare (ignore loc))
  )

(defmacro compiled-debug-function-form-number (fun)
  (let ((sym
	 (or (find-symbol "COMPILED-DEBUG-FUN-TLF-NUMBER" :sb-c)    ; older name
	     (find-symbol "COMPILED-DEBUG-FUN-FORM-NUMBER" :sb-c))))
    `(,sym ,fun)))

(defun get-loc-form-offset (loc)
  (if (sb-di::code-location-unknown-p loc)
      ;; on some version before 1.4.2 it was:
      ;;(sb-c::compiled-debug-fun-tlf-number
      (compiled-debug-function-form-number
       (sb-di::compiled-debug-fun-compiler-debug-fun
	(sb-di::compiled-code-location-debug-fun loc)))
      (sb-di:code-location-toplevel-form-offset loc)))

(defun get-snippet-pos (stream loc)
  (let ((form-offset (get-loc-form-offset loc))
	start-pos form)
    (let ((*read-suppress* t)
	  (eof (cons nil nil)))
      (loop :with i = 0
	 :while (and (<= i form-offset)
		     (not (eq eof
			      (setf start-pos (file-position stream)
				    form (read stream nil eof)))))
	 :do
	 (incf i)))
    (values start-pos form)))

(defun debugger-source (frame &optional (window-size 10))
  (let* ((loc (sb-di:frame-code-location frame))
	 ;;(fun (and loc (sb-di:code-location-debug-fun loc)))
	 ;;(src (and fun (sb-introspect:find-definition-source
	 ;;		(symbol-function
	 ;;		 (sb-di:debug-fun-name fun)))))
	 ;;(path (definition-source-pathname src))
	 ;;(offset (definition-source-character-offset src))
	 ;;
	 (src2        (sb-di::code-location-debug-source loc))
	 ;;(form-num    (sb-di::code-location-form-number loc))
	 ;;(form-offset (sb-di::code-location-toplevel-form-offset loc))
	 ;; (sb-di::code-location-source-form loc context??)
	 ;; (sb-di::get-toplevel-form loc)
	 (path2       (sb-c::debug-source-namestring src2))
	 offset)
;;;    (if src2
    (with-open-file (stream path2)
      (setf offset (get-snippet-pos stream loc))
      (file-position stream offset)
      (loop :with line
	 :for i :from 1 :to window-size
	 :while (setf line (read-line stream nil nil))
	 :collect line))))

(defun debugger-show-source (n)
  (let ((frame
	 (cond
	   ((numberp n) (frame-number n))
	   (t *current-frame*))))
    ;;(format t "~s~%" (debugger-source frame))))
    (loop :for s :in (debugger-source frame)
       :do (format t "~a~%" s))))

(defun frame-number (n)
  "Return the internal frame object given a frame number."
  (let ((result 
	 (loop :with f = (sb-di:top-frame)
	    :for fn :from 0 :below n
	    :do (setf f (sb-di:frame-down f))
	    :finally (return f))))
    (assert (= (sb-di:frame-number result) n))
    result))

(defun frame-number-or-current (&optional (n *current-frame*))
  "Return the frame numbered N, or the *current-frame*, or the top frame."
  (cond
    ((numberp n) (frame-number n))
    ((sb-di:frame-p n) n)
    (t (frame-number 0))))

(defun debugger-show-locals (n)
  (if n
      (format *debug-io* "Locals for frame ~s:~%" n)
      (format *debug-io* "Locals for current frame:~%"))
  (let* ((cur (frame-number-or-current n))
	 (fun (sb-di:frame-debug-fun cur)))
    (if (sb-di:debug-var-info-available fun)
	(let* ((*print-readably* nil)
	       (loc (sb-di:frame-code-location cur)))
	  (loop :for v :in (sb-di:ambiguous-debug-vars fun "")
	     :do
	     (when (eq (sb-di:debug-var-validity v loc) :valid)
              (format *debug-io* "~S~:[#~W~;~*~] = ~S~%"
                      (sb-di:debug-var-symbol v)
                      (zerop (sb-di:debug-var-id v))
                      (sb-di:debug-var-id v)
                      (sb-di:debug-var-value v cur))))))))

(defun debugger-backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  #+sbcl
  (let ((bt-func (if (< *lisp-version-number* 10300)
		     (intern "BACKTRACE" :sb-debug)
		     (intern "PRINT-BACKTRACE" :sb-debug))))
    (if (< *lisp-version-number* 10300)
	(if n (funcall bt-func n) (funcall bt-func)))
	(if n (funcall bt-func :count n) (funcall bt-func)))
  ;;  #+sbcl (sbcl-wacktrace)
  )

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  (or sb-debug::*stack-top-hint* (sb-di::top-frame)))

(defun debugger-up-frame (&optional (count 1))
  (declare (ignore count))
  (let ((next (sb-di:frame-up *current-frame*)))
    (if next
	(setf *current-frame* next))))

(defun debugger-down-frame (&optional (count 1))
  (declare (ignore count))
  (let ((next (sb-di:frame-down *current-frame*)))
    (if next
	(setf *current-frame* next))))

(defun debugger-set-frame (frame)
  (cond
    ((or (not frame) (and (numberp frame) (= frame 0)))
     (sb-di:top-frame))
    ((numberp frame)
     (setf *current-frame* (frame-number frame)))
    ((sb-di:frame-p frame)
     (setf *current-frame* frame))
    (t
     (format *debug-io* "No such frame ~s~%" frame))))

(defun debugger-top-frame (count)
  (declare (ignore count))
  (setf *current-frame* *saved-frame*))

;; Stepping

(progn
  ;; (defvar *step-form* nil)
  ;; (defvar *step-args* nil)
  (defun stepper (c)
    "Thing to set the stepper hook to."
    ;; (setf *step-form* (sb-ext::step-condition-form c)
    ;;  	  *step-args* (sb-ext::step-condition-args c))
    (format *debug-io* "-- SteppeR --~%")
    ;; Handle special stepping conditions:
    (typecase c
      (sb-ext:step-values-condition
       (format *debug-io* "Form: ~s~%Result: ~s~%"
	       (slot-value c 'sb-kernel::form)
	       (slot-value c 'sb-kernel::result))
       (finish-output *debug-io*)
       (return-from stepper)))
    (finish-output *debug-io*)
    (let ((sb-debug::*stack-top-hint* (sb-di::find-stepped-frame))
	  (sb-ext::*stepper-hook* nil))
      (invoke-debugger c))))

(defun activate-stepper ()
  "Activate the setpper."
  (setf sb-ext::*stepper-hook* 'stepper)
  (format *debug-io* "Activating the DEBLARG stepper.~%"))

;; Breakpoints

(defvar *breakpoints* '()
  "List of known breakpoints.")

(defun breaker (frame obj)
  "Function called when a breakpoint is hit."
  (declare (ignore obj))
  (format *debug-io* "You gots breaked!~%")
  ;; (invoke-debugger (make-condition
  ;; 		      'simple-condition
  ;; 		      :format-control "Breakpoint"))
  (deblarg (make-condition
	    'simple-condition
	    :format-control "Breakpoint") nil frame))

(defun set-func-breakpoint (fun)
  (if (sb-di:fun-debug-fun fun)
      (let ((bp (sb-di:make-breakpoint
		 #'breaker
		 (sb-di:fun-debug-fun fun) :kind :fun-start)))
	(if bp
	    (progn
	      (push bp *breakpoints*)
	      (sb-di:activate-breakpoint bp))
	    (format *debug-io* "Can't make no breakpoint fer ~s~%" fun)))
      (format *debug-io* "Ain't no debug fun fer ~s~%" fun)))

(defun find-breakpoint (n)
  (nth (1- n) *breakpoints*))
  
(defun activate-breakpoint (n)
  (let ((bp (find-breakpoint n)))
    (if bp
	(sb-di:activate-breakpoint bp)
	(format *debug-io* "No such breakpoint ~a~%" n))))

(defun deactivate-breakpoint (n)
  (let ((bp (find-breakpoint n)))
    (if bp
	(sb-di:deactivate-breakpoint bp)
	(format *debug-io* "No such breakpoint ~a~%" n))))

(defun toggle-breakpoint (n)
  (let ((bp (find-breakpoint n)))
    (if bp
	(if (sb-di:breakpoint-active-p bp)
	    (sb-di:deactivate-breakpoint bp)
	    (sb-di:activate-breakpoint bp))
	(format *debug-io* "No such breakpoint ~a~%" n))))

(defun delete-breakpoint (n)
  (let ((bp (find-breakpoint n)))
    (if bp
	(sb-di:deactivate-breakpoint bp)
	(format *debug-io* "No such breakpoint ~a~%" n))))

(defun list-breakpoints ()
  (let ((rows
	 (loop :for b :in *breakpoints* :and i = 1 :then (1+ i)
	    :collect (list i
			   (sb-di:breakpoint-active-p b)
			   (sb-di:breakpoint-kind b)
			   (sb-di:breakpoint-what b)
			   (sb-di:breakpoint-info b)))))
    (nice-print-table rows '("#" "Act" "Kind" "What" "Info")
		      :stream *debug-io*)))

;; EOF
