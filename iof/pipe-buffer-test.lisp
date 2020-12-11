;;
;; pipe-buffer-test.lisp - Test pipe buffers
;;

(defpackage :pipe-buffer-test
  (:documentation "Test pipe buffers")
  (:use :cl :test :pipe-buffer)
  (:export
   #:run-tests
   #:*buf*
   ))
(in-package :pipe-buffer-test)

(defun small-input ()
  (let ((pp (make-pipe-buffer :initial-size 10)))
    (write-data pp "Hello")
    (write-data pp " Again")
    (write-data pp " I am just fine. How are you?")
    (write-data pp " Things could be better. My monkey is loose.")
    pp))

(defun small-output-test ()
  (pipe-buffer::whole (small-input)))

(defparameter *small-test-output*
  "Hello Again I am just fine. How are you? Things could be better. My monkey is loose.")

(defun file-test ()
  (let ((pp (make-pipe-buffer)))
    (with-open-file (str (first (glob:glob "~/funkazo.ps")) :direction :input)
      (loop :with line
	 :while (setf line (read-line str nil nil))
	 :do (write-data pp line) (write-data pp (string #\newline))))
    (loop :with b
       :while (setf b (read-data pp nil nil nil))
       :do (write-string b))))

(deftests (reread-test)
  "Test emptying out a reading more."
  (let ((pp (small-input)) output read-output)
    (and (progn
	   (setf read-output (read-data pp nil nil nil))
	   (format t "~s~%~s~%"
		   (coerce *small-test-output* '(vector integer *))
		   (coerce read-output '(vector integer *)))
	   (format t "~s~%~s~%" (length *small-test-output*)
		   (length read-output))
	   (format t "Small ~:[FAIL~;OK~]~%"
		   (equal read-output *small-test-output*))
	   (equal read-output *small-test-output*))
	 (progn
	   (setf output
		 (with-output-to-string (str)
		   (loop :for i :from 1 :to 10
		      :do
		      (write-data pp "0123456789")
		      (princ "0123456789" str)))
		 read-output (read-data pp nil nil nil))
	   (format t "~a~%~a~%" output read-output)
	   (equal output read-output)))))

(defvar *buf* nil
  "Pipe buffer for testing.")

(defun simple-setup ()
  (setf *buf* (make-pipe-buffer)))

(deftests (simple :setup simple-setup)
  (equal (small-output-test) *small-test-output*)
  (and (write-data *buf* "Hello.")
       (equal (read-data *buf*) "Hello."))
  (and (write-data *buf* " Again")
       (equal (read-data *buf*) " Again"))
  (null (read-data *buf* 5 nil nil))
  )

(defun run-tests ()
  (run-all-tests))

;; EOF
