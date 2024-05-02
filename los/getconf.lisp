;;;
;;; getconf.lisp - Print system configuration.
;;;

(defpackage :getconf
  (:documentation "Print system configuration.")
  (:use :cl :opsys :grout)
  (:export
   #:describe-sysconf
   #:!getconf
   ))
(in-package :getconf)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defun describe-sysconf (&key verbose)
  "Print out the sysconf values."
  (with-grout ()
    (let ((table
	   (if verbose
	       (table:make-table-from
		(loop :for v :across (system-info-names)
		   :collect `(,v ,(ignore-errors
				    (get-system-info (symbol-value v)))
				 ,(system-info-description v)))
		:columns
		'((:name "Name")
		  (:name "Value")
		  (:name "Description" :align :wrap)))
	       (table:make-table-from
		(loop :for v :across (system-info-names)
		   :collect `(,v ,(ignore-errors
				    (get-system-info (symbol-value v)))))
		:column-names '("Name" "Value")))))
      (grout-print-table table)
      table)))

(defun system-info-names-list ()
  (coerce (system-info-names) 'list))

;; End
