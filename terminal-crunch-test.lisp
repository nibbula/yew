;;
;; terminal-crunch-test.lisp - Tests for terminal-crunch.
;;

(defpackage :terminal-crunch-test
  (:documentation "Tests for terminal-crunch.")
  (:use :cl :dlib :terminal :terminal-crunch :terminal-ansi :fatchar)
  (:export
   #:test-1
   #:test-2
   ))
(in-package :terminal-crunch-test)

(defvar tta)
(defvar tt)

(defun dump-hashes (tty)
  (let ((old-hashes (terminal-crunch::screen-hashes
		     (terminal-crunch::old-screen tty)))
	(new-hashes (terminal-crunch::screen-hashes
		     (terminal-crunch::new-screen tty))))
    (format *debug-io* "Old -> New~%")
    (loop :for i :from 0 :below (length old-hashes) :do
       (format *debug-io* "~s ~s~%" (aref old-hashes i) (aref new-hashes i)))))

(defun dump-screen (tty)
  (let ((old-lines (terminal-crunch::screen-lines
		     (terminal-crunch::old-screen tty)))
	(new-lines (terminal-crunch::screen-lines
		     (terminal-crunch::new-screen tty))))
    (format *debug-io* "Old -> New~%")
    (loop :for i :from 0 :below (length old-lines) :do
       (format *terminal* "[~a] [~a]~%"
	       (make-fat-string :string (aref old-lines i))
	       (make-fat-string :string (aref new-lines i))))))

(defun test-1 (device-name)
  (setf tta (make-instance 'terminal-ansi :device-name device-name))
  (setf tt (make-instance 'terminal-crunch :wrapped-terminal tta))
  (terminal-start tt)
  ;; make sure the wrapped terminal is functioning?
  ;; (terminal-home tta)
  ;; (terminal-clear tta)
  ;; (terminal-write-string tta "Hello there.")
  ;; (terminal-finish-output tta)
  ;; (tt-write-string "Okay?")
  (tt-get-key)
  (dotimes (i 20)
    (terminal-write-string tt (format nil "~r~%" (+ 100000 i))))
  ;; (dump-screen tt)
  (terminal-finish-output tt)
  ;; on a different terminal:
  ;; (dump-hashes tt)
  ;; (dump-screen tt)
  (tt-write-string "How bout it?")
  (tt-get-key)
  (terminal-done tt))

(defun test-2 (device-name)
  ;; (with-new-terminal (:crunch *terminal*
  ;; 			      :wrapped-terminal
  ;; 			      (make-instance 'terminal-ansi
  ;; 					     :device-name device-name))
  (let ((tty (make-instance 'terminal-crunch
			    :wrapped-terminal
			    (make-instance 'terminal-ansi
					   :device-name device-name))))
    (let ((state (terminal-start tty)))
      (flet ((blit (c)
	     (loop :for y :from 0 :below 10 :do
		(loop :for x :from 0 :below 40 :do
		   (terminal-move-to tty y x)
		   (terminal-write-char tty c)))))
	(dotimes (i 5001)
	  (blit (if (evenp i) #\space #\X)))
	(terminal-finish-output tty)
	(tt-write-string "-->")
	(tt-get-key)
	(dotimes (i 5000)
	  (blit (if (evenp i) #\- #\X)))
	(terminal-finish-output tty)
	(tt-write-string "-->")
	(tt-get-key))
      (terminal-end tty state))))

;; EOF
