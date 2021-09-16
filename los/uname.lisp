;;;
;;; uname.lisp - Print system type information.
;;;

(defpackage :uname
  (:documentation "Print system type information.")
  (:use :cl :dlib :opsys :lish)
  (:export
   #:!uname
   ))
(in-package :uname)

(defcommand uname
  ((system boolean :short-arg #\s :help "Print the operating system name.")
   (network boolean :short-arg #\n :help "Print the network name.")
   (release boolean :short-arg #\r :help "Print the kernel release.")
   (version boolean :short-arg #\v :help "Print the kernel version.")
   (machine boolean :short-arg #\m :help "Print the machine name.")
   (all boolean :short-arg #\a :help "Print all the infomation."))
  "Print system type information."
  (when (not (or system network version release machine))
    (setf system t))
  (let ((first t) (results '()))
    (macrolet ((print-item (name func)
		 `(when (or ,name all)
		    (let ((value (,func)))
		      (if first
			  (setf first nil)
			  (write-char #\space))
		      (princ value)
		      (push (cons (keywordify ',name) value) results)))))
      (print-item system  os-software-type)
      (print-item network os-machine-instance)
      (print-item release os-software-release)
      (print-item version os-software-version)
      (print-item machine os-machine-type)
      (setf results (nreverse results)))
    (terpri)
    (setf *output*
	  (if (> (length results) 1)
	      results
	      (cdar results)))))

;; End
