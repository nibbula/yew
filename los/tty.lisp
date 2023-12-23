;;;
;;; tty.lisp - Print the name of the terminal.
;;;

(defpackage :tty
  (:documentation "Print the name of the terminal.")
  (:use :cl :terminal :opsys :lish)
  (:export
   #:device-name
   #:type-description
   #:!tty
   ))
(in-package :tty)

(defun device-name (&key fd)
  "Return the terminal device name."
  (when (null fd)
    (setf fd 0))
  ;; #+unix (if (uos:isatty fd) (uos:ttyname fd) "not a tty")
  #+unix
  (if (file-handle-terminal-p fd)
      (file-handle-terminal-name fd)
      "not a tty")
  #-unix "unknown")

(defun type-description ()
  "Return a description of *terminal* and it's file descriptor."
  (cond
    (*terminal*
     (let ((real-term
	     (or (terminal-wrapped-terminal *terminal*) *terminal*)))
       (values
	(format nil "*terminal* is a ~a on ~a."
		(type-of *terminal*)
		(terminal-device-name real-term))
	 (terminal-file-descriptor real-term))))
    (t
     (values "unknown" nil))))

;; End
