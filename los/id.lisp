;;;
;;; id.lisp - Print user information.
;;;

(defpackage :id
  (:documentation "Print user information.")
  (:use :cl :dlib :opsys :collections :table :grout :lish)
  (:export
   #:!id
   ))
(in-package :id)

(defun user-name-list ()
  (mapcar #'nos:user-info-name (nos:user-list)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defargtype user (arg-lenient-choice)
    "User name."
    ()
    (:default-initargs
     :choice-func #'user-name-list)
    :convert string
      ;; Convert a string into a user number.
      (multiple-value-bind (i pos) (parse-integer lish::value :junk-allowed t)
	(if (and i (plusp i) (plusp pos))
	  (setf lish::value i)
	  lish::value)))
  ;; (defclass arg-user (arg-lenient-choice)
  ;;   ()
  ;;   (:default-initargs
  ;;    :choice-func #'user-name-list)
  ;;   (:documentation "User name."))
  )

(defcommand id
  ((user user :default '(nos:user-id) :optional t
    :help "The user to print information about.")
   (format choice :short-arg #\f :default :los :choices '("los" "unix")
    :help "Format for output."))
  "Print user information."
  (let ((id (if (and user (numberp user)) user (nos:user-id :name user))))
    (flet ((los-format ()
	     (with-grout ()
	       (let ((table
		      (make-table-from
		       (let (l)
			 (omapk (_ (push
				    (list
				     (name-to-title
				      (princ-to-string (okey _)))
				     (princ-to-string (ovalue _))) l))
				(nos:get-user-info :id id))
			 (setf l (nreverse l))
			 #+unix
			 (progn
			   (setf l
			     (append l
			       (list
			         (list
				  "Groups"
				  (format nil "~{~{~d(~(~a~))~}~^ ~}"
					  (map 'list
					       (_ (list _ (uos:group-name _)))
					       (uos:get-groups))))))))
			 l)
		       :columns '((:name "Name" :type string)
				  (:name "Value" :align :wrap)))))
		 (grout-print-table table)
		 (setf *output* table)))))
      (case (keywordify format)
	(:unix
	 (format t "uid=~d(~a) gid=~d(~a)" id (nos:user-name id)
		 (nos:group-id) (nos:group-name (nos:group-id)))
	 #+unix
	 (format t " groups=~{~{~d(~(~a~))~}~^,~}"
		 (map 'list (_ (list _ (uos:group-name _))) (uos:get-groups)))
	 (terpri))
	(:los (los-format))
	(otherwise (los-format))))))

;; End
