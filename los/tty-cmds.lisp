;;;
;;; tty-cmds.lisp - Commands for tty.
;;;

(in-package :tty)

(defcommand tty
  ((lisp boolean :short-arg #\l :help "Use Lisp *standard-input*.")
   (type boolean :short-arg #\t :help "Print the name and type of *terminal*."))
  "Print the file name of the terminal on standard input."
  (cond
    (type
     (write-line (type-description))
     (setf *output* *terminal*))
    (t
     (let ((name (device-name
		  :fd (and lisp (stream-system-handle *standard-input*)))))
       (format t "~a~%" name)
       (setf *output* name)))))

;; End
