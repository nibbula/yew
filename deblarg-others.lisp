;;
;; deblarg-others.lisp - Implementation specific parts for all other
;;                       implementations that aren't in a separate file.
;;

(in-package :deblarg)

(defun debugger-wacktrace (n)
  (declare (ignore n)) (debugger-sorry "wacktrace"))

(defun debugger-show-source (n)
  (declare (ignore n)) (debugger-sorry "show source"))

(defun debugger-show-locals (n)
  (declare (ignore n)) (debugger-sorry "show locals"))

;; As you may know, this is quite implementation specific.
(defun debugger-backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  #+cmu (if n (debug:backtrace n) (debug:backtrace))
  #+lispworks (dbg:output-backtrace :full)
  #+abcl
  (loop :with i = 0
     :for f :in (sys:backtrace)
     :do (format *debug-io* "~(~3d ~a~)~%" i (sys:frame-to-string f)) (incf i)
     :while (or (null n) (and (numberp n) (< i n))))
  #-(or cmu clisp lispworks ecl abcl)
  (debugger-sorry "backtrace"))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  ;; We don't want to be sorry here, so just be wrong.
  #-(or sbcl ccl) nil)

(defun activate-stepper (&key quietly)
  "Activate the setpper."
  (declare (ignore quietly))
  (values))

(defun debugger-hook ()
  *debugger-hook*)

(defun set-debugger-hook (function)
  (setf *debugger-hook* function))

;; EOF
