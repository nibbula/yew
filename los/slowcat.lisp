;;;
;;; slowcat.lisp - Make things go by slowly, like the old days.
;;;

(defpackage :slowcat
  (:documentation "Make things go by slowly, like the old days.")
  (:use :cl :dlib :opsys)
  (:export
   ;; Main entry point
   #:slowcat
   #:!slowcat
   ))
(in-package :slowcat)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun slowcat (stream-or-file &key (delay .02) (unit :char))
  (with-open-file-or-stream (s stream-or-file :direction :input)
    (ecase unit
      (:line
       (loop :with line
	  :while (setf line (read-line s nil))
	  :do
	  (write-line line)
	  (finish-output)
	  (sleep delay)))
      (:char
       (loop :with line
	  :while (setf line (read-char s nil))
	  :do
	  (write-char line)
	  (finish-output)
	  (sleep delay))))))

#+lish
(lish:defcommand slowcat
  ((delay number :short-arg #\d :default .001 :help "Delay in seconds.")
   (unit choice :short-arg #\u :default :char :choices '(:line :char)
    :help "Whether ‘dalay’ applies to characters or lines.")
   (files pathname :repeating t
    :help "Files or stream-like things to use as input."))
  "Make things go by slowly, like the old days."
  (if files
      (loop :for f :in files :do
	(lish:with-streamlike-input (f)
	  (slowcat f :delay delay :unit unit)))
      (progn
	(slowcat *standard-input* :delay delay :unit unit))))

;; End
