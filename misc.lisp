;;
;; misc.lisp - Miscellaneous small commands.
;;

(defpackage :misc
  (:documentation "Miscellaneous small commands.")
  (:use :cl :dlib :lish :opsys :terminal)
  (:export
   #:!tty
   ))
(in-package :misc)

(defcommand tty
  ((lisp boolean :short-arg #\l :help "Use Lisp *standard-input*.")
   (type boolean :short-arg #\t :help "Print the name and type of *terminal*."))
  "Print the file name of the terminal on standard input."
  (let (fd name)
    (if type
	(if *terminal*
	    (progn
	      (let ((real-term
		     (or (terminal-wrapped-terminal *terminal*) *terminal*)))
		(format t "*terminal* is a ~a on ~a.~%"
			(type-of *terminal*)
			(setf name (terminal-device-name real-term)))
		(setf fd (terminal-file-descriptor real-term))))
	    (progn
	      (setf name "unknown")
	      (format t "*terminal* is not set.~%")))
	(progn
	  (when (not fd)
	    (setf fd (or (and lisp (stream-system-handle *standard-input*))
			 0)))
	  (setf name
		#+unix (if (zerop (uos:isatty fd)) "not a tty" (uos:ttyname fd))
		#-unix "unknown")
	  (format t "~a~%" name)
	  (setf *output* name)))))

;; EOF
