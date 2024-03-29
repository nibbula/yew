;;;
;;; deblarg-clisp.lisp - CLisp specific parts of the debugger.
;;;

(in-package :deblarg)

(defconstant +all-stack-elements+ 1)
(defconstant +all-frames+ 2)
(defconstant +only-lexical-frames+ 3)
(defconstant +only-eval-and-apply-frames+ 4)
(defconstant +only-apply-frames+ 5)

(defclass clisp-deblargger (deblargger)
  ()
  (:documentation "Deblargger for CLisp."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *deblargger-implementation-class* 'clisp-deblargger))

(defun frame-description (frame)
  (let* ((*print-length* 4)
	 (result (with-output-to-string (str)
		   (sys::describe-frame str frame))))
    (subseq result 0 (or (position #\newline result) (length result)))))

(defvar *frame-prefixes*
  '(("\\[[0-9]\\+\\] frame binding variables" bind-var)
    ("<1> #<compiled-function" compiled-fun)
    ("<1> #<system-function" sys-fun)
    ("<1> #<special-operator" special-op)
    ("EVAL frame" eval)
    ("APPLY frame" apply)
    ("\\[[0-9]\\+\\] compiled tagbody frame" compiled-tagbody)
    ("\\[[0-9]\\+\\] compiled block frame" compiled-block)
    ("block frame" block)
    ("nested block frame" block)
    ("tagbody frame" tagbody)
    ("nested tagbody frame" tagbody)
    ("catch frame" catch)
    ("handler frame" handler)
    ("unwind-protect frame" unwind-protect)
    ("driver frame" driver)
    ("\\[[0-9]\\+\\] frame binding environments" bind-env)
    ("CALLBACK frame" callback)
    ("- " stack-value)
    ("<1> " fun)
    ("<2> " 2nd-frame)
    ))

(defun frame-type (frame desc)
  (declare (ignore frame)) ;; @@@
  (loop
     :for prefix :in *frame-prefixes*
     :when (ppcre:scan (s+ "(?i)" (car prefix)) desc)
     :do (return (cadr prefix))))

(defun totally-fucking-boring-frame-p (frame desc)
  (member (frame-type frame desc)
	  '(stack-value bind-var bind-env compiled-tagbody compiled-block)))

(defmethod debugger-backtrace ((d clisp-deblargger) n)
  (loop
    :with frame = (sys::the-frame)
    :and i = 0 :and desc :and last
    :unless (totally-fucking-boring-frame-p
	     frame (setf desc (frame-description frame)))
      :do
      (incf i)
      (print-span `((:fg-yellow ,(format nil "~3a" i))
		    ,(format nil "~(~a~)~%" desc)))
    :while (and (not (eq last (setf frame (sys::frame-up
					   1 frame
					   ;; +all-stack-elements+
					   +only-eval-and-apply-frames+
					   ))))
		 (or (null n) (< i n)))
     :do (setf last frame)))

;; Or perhaps
;; (system::print-backtrace :mode 4)  ; @@@ pick different modes?

(defmethod debugger-old-backtrace ((d clisp-deblargger) n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  (declare (ignore n))
  (catch 'debug (system::debug-backtrace "4")))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  ;; We don't want to be sorry here, so just be wrong.
  nil)

(defmethod debugger-hook ((d (eql 'clisp-deblargger)))
  *debugger-hook*
  ;;sys::*break-driver*
  )

(defmethod debugger-set-hook ((d (eql 'clisp-deblargger)) function)
  (setf *debugger-hook* function
	sys::*break-driver* function))

;; End
