;;
;; deblarg-others.lisp - Implementation specific parts for all other
;;                     implementations that aren't in a separate file.
;;

(in-package :deblarg)

(defun debugger-wacktrace (n)
  (declare (ignore n)) (debugger-sorry "wacktrace"))

(defun debugger-show-source (n)
  (declare (ignore n)) (debugger-sorry "show source"))

(defun debugger-show-locals (n)
  (declare (ignore n)) (debugger-sorry "show locals"))

#|
#+ecl (defvar *stack-base* 0)

#+ecl
(defun ecl-args (frame)
  (let ((base (or (si::sch-frs-base si::*frs-top* *stack-base*)
		  (1+ (si::frs-top)))))
    (loop :with i :and name
       :for f :from base :until (si::frs-top)
       :do
       (setf i (- (si::frs-ihs f) *stack-base* 1))
       :if (and (plusp i) (= i frame) (not (si::fixnump (si::frs-tag f))))
       :collect (si::frs-tag f))))

(si::ihs-env i)
|#

#+ecl
(defun ecl-backtrace (n)
  ;; (loop :for i :from 0 :below (min (si::ihs-top) n)
  ;;    :do (format *debug-io* "~a ~a~%" (si::ihs-fun i) #|(ecl-args i)|#))
  (let* ((top (if n (min n (si::ihs-top)) (si::ihs-top)))
	 (stack (reverse (loop :for i :from 1 :below top
			    :collect (si::ihs-fun i)))))
    (loop :for s :in stack :and i = 0 :then (1+ i)
       :do (format *debug-io* "~3d: ~w~%" i s))))

;; As you may know, this is quite implementation specific.
(defun debugger-backtrace (n)
  "Output a list of execution stack contexts. Try to limit it to the
innermost N contexts, if we can."
  #+cmu (if n (debug:backtrace n) (debug:backtrace))
;  #+clisp (system::print-backtrace :mode 4)  ; @@@ pick different modes?
  ;; Or perhaps
  #+clisp (catch 'debug (system::debug-backtrace "4"))
  #+lispworks (dbg:output-backtrace :full)
  #+ecl (ecl-backtrace n)
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

;; EOF
