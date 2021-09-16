;;;
;;; groups.lisp - Print a list of groups that the current user is in.
;;;

(defpackage :groups
  (:documentation "Print a list of groups that the current user is in.")
  (:use :cl :opsys :lish)
  (:export
   #:!groups
   ))
(in-package :groups)

(defcommand groups ()
  "Print a list of groups that the current user is in."
  #+unix
  (let ((result (map 'list #'uos:group-name (uos:get-groups))))
    (format t "~{~a~^ ~}~%" result)
    (setf *output* result))
  #-unix
  (format t "I don't know how to show your groups on ~s.~%" *os*))

;; End
