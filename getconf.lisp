;;;
;;; getconf.lisp - Print system configuration.
;;;

(defpackage :getconf
  (:documentation "Print system configuration.")
  (:use :cl :opsys :lish :grout)
  (:export
   #:getconf
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
		:column-names '("Name" "Value" "Description"))
	       (table:make-table-from
		(loop :for v :across (system-info-names)
		   :collect `(,v ,(ignore-errors
				    (get-system-info (symbol-value v)))))
		:column-names '("Name" "Value")))))
      (grout-print-table table)
      table)))

(defun system-info-names-list ()
  (coerce (system-info-names) 'list))

(defcommand getconf
  ((name choice
    :choice-func 'system-info-names-list :optional t
    :test #'arg-choice-compare-ignore-case
    :help "Name of the value to print.")
   (all boolean :short-arg #\a :help "Show all values.")
   (list boolean :short-arg #\l :help "List value names.")
   (verbose boolean :short-arg #\v :help "List descriptions too.")
   (collect boolean :short-arg #\c :help "Collect results."))
  "Print system configuration."
  (cond
    (all
     (let ((table (describe-sysconf :verbose verbose)))
       (when collect
	 (setf *output* table))))
    (list
     (if verbose
	 (progn
	   (let ((table
		  (table:make-table-from
		   (loop :for v :across (system-info-names)
		      :collect `(,v ,(system-info-description v)))
		   :column-names '("Name" "Description"))))
	     (with-grout ()
	       (grout-print-table table)
	       (when collect
		 (setf *output* table)))))
	 (progn
	   (map nil (lambda (x) (format t "~a~%" x)) (system-info-names))
	   (when collect
	     (setf *output* (system-info-names))))))
    (name
     (let ((value (get-system-info name)))
       (format t "~a~%" value)
       (setf *output* value)))
    (t
     (format t "Please give me a value to get. ~%"))))

;; End
