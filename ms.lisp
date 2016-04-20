;;
;; ms.lisp - Interface to Microsoft systems
;;

(defpackage :ms
  (:documentation "Interface to Microsoft systems.")
  (:use :cl)
  (:export
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   ))
(in-package :ms)

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (GetConsoleMode fd))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
  (GetFileInformationByHandleEx fd))

;; EOF
