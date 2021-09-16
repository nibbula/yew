;;;
;;; tty.lisp - Print the name of the terminal.
;;;

(defpackage :tty
  (:documentation "Print the name of the terminal.")
  (:use :cl :terminal :opsys :lish)
  (:export
   #:!tty
   ))
(in-package :tty)

(defcommand tty
  ((lisp boolean :short-arg #\l :help "Use Lisp *standard-input*.")
   (type boolean :short-arg #\t :help "Print the name and type of *terminal*."))
  "Print the file name of the terminal on standard input."
  (let (fd name)
    (cond
      ((and type *terminal*)
       (let ((real-term
	       (or (terminal-wrapped-terminal *terminal*) *terminal*)))
	 (format t "*terminal* is a ~a on ~a.~%"
		 (type-of *terminal*)
		 (setf name (terminal-device-name real-term)))
	 (setf fd (terminal-file-descriptor real-term)
	       *output* *terminal*)))
      (type
       (setf name "unknown")
       (format t "*terminal* is not set.~%"))
      (t
       (when (not fd)
	 (setf fd (or (and lisp (stream-system-handle *standard-input*))
		      0)))
       (setf name
	     #+unix (if (uos:isatty fd) (uos:ttyname fd) "not a tty")
	     #-unix "unknown")
       (format t "~a~%" name)
       (setf *output* name)))))

;; End
