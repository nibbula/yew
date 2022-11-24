;;;
;;; getconf-cmds.lisp - Commands for getconf.
;;;

(in-package :getconf)

(lish:defcommand getconf
  ((name choice
    :choice-func 'system-info-names-list :optional t
    :test #'lish:arg-choice-compare-ignore-case
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
	 (setf lish:*output* table))))
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
		 (setf lish:*output* table)))))
	 (progn
	   (map nil (lambda (x) (format t "~a~%" x)) (system-info-names))
	   (when collect
	     (setf lish:*output* (system-info-names))))))
    (name
     (let ((value (get-system-info name)))
       (format t "~a~%" value)
       (setf lish:*output* value)))
    (t
     (format t "Please give me a value to get. ~%"))))

;; End
