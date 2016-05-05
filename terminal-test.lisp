;;
;; terminal-test.lisp - Test the generic terminal library.
;;

(defpackage :terminal-test
  (:documentation "Test the generic terminal library.")
  (:use :cl :terminal :terminal-ansi :terminal-curses)
  (:export
   #:test
   ))
(in-package :terminal-test)

(defvar *tty* nil)

(defun ask-class ()
  (format t "Which sub-class ?~%  1. ANSI~%  2. Curses~% ? ")
  (finish-output)
  (let ((choice (ignore-errors (parse-integer (read-line)))))
    (case choice
      (1 'terminal-ansi)
      (2 'terminal-curses)
      (otherwise (format t "Whaaa?~%")))))

(defun prompt-next ()
  (tt-move-to *tty* (1- (terminal-window-rows *tty*)) 0)
  (tt-format *tty* "Press Q to quit, anything else to continue.")
  (tt-get-key *tty*))

(defun test ()
  (let ((class (ask-class)))
    (when class
      (setf *tty* (make-instance class))
      (unwind-protect
	   (progn
	     (terminal-start *tty*)
	     (terminal-get-size *tty*)
	     (tt-clear *tty*)
	     (tt-format *tty* "The screen should have cleared.~%")
	     (prompt-next))
	(terminal-end *tty*)))))

;; EOF
