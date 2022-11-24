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

;; End
