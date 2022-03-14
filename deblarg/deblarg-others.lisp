;;;
;;; deblarg-others.lisp - Implementation specific parts for all other
;;;                       implementations that aren't in a separate file.
;;;

(in-package :deblarg)

(defclass other-deblargger (deblargger)
  ()
  (:documentation "Deblargger for other implementations."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *deblargger-implementation-class* 'other-deblargger))

;; As you may know, this is quite implementation specific.
(defmethod debugger-backtrace ((d other-deblargger) n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  #+cmu (if n (debug:backtrace n) (debug:backtrace))
  #+lispworks (dbg:output-backtrace :full)
  #+abcl
  (loop :with i = 0
     :for f :in (sys:backtrace)
     :do (format *debug-io* "~(~3d ~a~)~%" i (sys:frame-to-string f)) (incf i)
     :while (or (null n) (and (numberp n) (< i n))))
  #+excl
  (top-level.debug:zoom *debug-io* :count n)
  #-(or cmu clisp lispworks ecl abcl excl)
  (debugger-sorry "backtrace"))

(declaim (inline debugger-internal-frame))
(defun debugger-internal-frame ()
  ;; We don't want to be sorry here, so just be wrong.
  #-(or sbcl ccl) nil)

(defmethod activate-stepper ((d other-deblargger) &key quietly)
  "Activate the setpper."
  (declare (ignore quietly))
  (values))

(defmethod debugger-hook ((d (eql 'other-deblargger)))
  *debugger-hook*)

(defmethod debugger-set-hook ((d (eql 'other-deblargger)) function)
  (setf *debugger-hook* function))

;; EOF
