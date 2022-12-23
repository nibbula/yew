;;;
;;; result.lisp - Generic command results.
;;;

(defpackage :result
  (:documentation "Generic command results.")
  (:use :cl )
  (:export
   #:file-result
   #:file-result-os-pathname
   #:file-line-result
   #:file-result-line
   ))
(in-package :result)

(defclass file-result ()
  ()
  (:documentation "A generic file result."))

(defgeneric file-result-os-pathname (result)
  (:documentation "Return an os-pathname from a file-result."))

(defclass file-line-result (file-result)
  ()
  (:documentation "A generic result of a line of a file."))

(defgeneric file-result-line (result)
  (:documentation "Return the line number from a file-line-result."))

;; End
