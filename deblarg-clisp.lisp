;;
;; deblarg-clisp.lisp - CLisp specific parts of the debugger.
;;

(in-package :deblarg)

(defconstant +all-stack-elements+ 1)
(defconstant +all-frames+ 2)
(defconstant +only-lexical-frames+ 3)
(defconstant +only-eval-and-apply-frames+ 4)
(defconstant +only-apply-frames+ 5)

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
  (loop
     :for prefix :in *frame-prefixes*
     :when (ppcre:scan (s+ "(?i)" (car prefix)) desc)
     :do (return (cadr prefix))))

(defun totally-fucking-boring-frame-p (frame desc)
  (member (frame-type frame desc)
	  '(stack-value bind-var bind-env compiled-tagbody compiled-block)))

(defun debugger-wacktrace (n)
  (loop
     :with frame = (sys::the-frame) :and i = 0 :and desc :and last
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

(defun debugger-show-source (n)
  (declare (ignore n)) (debugger-sorry "show source"))

(defun debugger-show-locals (n)
  (declare (ignore n)) (debugger-sorry "show locals"))

(defun debugger-backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  (catch 'debug (system::debug-backtrace "4")))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  ;; We don't want to be sorry here, so just be wrong.
  nil)

(defun activate-stepper (&key quietly)
  "Activate the setpper."
  (declare (ignore quietly))
  (values))
