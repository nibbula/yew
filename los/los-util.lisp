;;;
;;; los-util.lisp - Common utilities for LOS programs.
;;;

(defpackage :los-util
  (:documentation "Common utilities for LOS programs.")
  (:use :cl :opsys :lish)
  (:export
   #:user-name-list
   ))
(in-package :los-util)

(defun user-name-list ()
  (mapcar #'nos:user-info-name (nos:user-list)))

(defclass lish-user::arg-user (arg-lenient-choice)
  ()
  (:default-initargs
   :choice-func #'user-name-list)
  (:documentation "User name."))

;; End
