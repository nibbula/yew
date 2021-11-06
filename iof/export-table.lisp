;;;
;;; export-table.lisp - Export tables to other data formats.
;;;

(defpackage :export-table
  (:documentation "Export tables to other data formats.")
  (:use :cl :table :table-print :table-html-renderer)
  (:export
   #:export-table
   ))
(in-package :export-table)

(defgeneric export-table (table format destination
			  &rest args &key &allow-other-keys)
  (:documentation "Export ‘table’ as ‘format’ to ‘destination’."))

(defvar *formats* '()
  "Registered export formats.")

(defun register-export-format (format)
  "Add ‘format’ to the list of known export formats."
  (pushnew format *formats*))

(defmethod export-table ((table table) (format (eql :html)) (destination stream)
			 &rest args &key whole-file (title "A table")
			 &allow-other-keys)
  (declare (ignore args))
  (if whole-file
      (cl-who:with-html-output
	  (s destination :prologue "<!doctype html>" :indent t)
	(:html
	 (:head
	  (:meta :http-equiv "content-type" :content "text/html; charset=UTF-8")
	  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
	  (:meta :name "description" :content (cl-who:str title))
	  ;; (:link :rel "shortcut icon" (str icon))
	  ;; (:meta :itemprop "image" :content "")
	  (:title (cl-who:str title)))
	 (:body
	  (print-table table :renderer (make-instance 'html-table-renderer)
			     :stream s))))
      (print-table table :renderer (make-instance 'html-table-renderer))))
  
(defmethod export-table ((table table) (format (eql :html))
			 (destination string)
			 &rest args &key whole-file &allow-other-keys)
  "Export a table to an HTML file. Create the file if it doesn't exist."
  ;; (declare (ignore args))
  (declare (ignorable whole-file))
  (with-open-file (stream destination
			  :direction :output :if-does-not-exist :create)
    (apply #'call-next-method table :html stream args)))

(defmethod export-table ((table table) (format (eql :html))
			 (destination pathname)
			 &rest args &key whole-file &allow-other-keys)
  "Export a table to an HTML file. Create the file if it doesn't exist."
  ;; (declare (ignore args))
  (declare (ignorable whole-file))
  (with-open-file (stream destination
			  :direction :output :if-does-not-exist :create)
    (apply #'call-next-method table :html stream args)))

(register-export-format :html)

#+lish
(lish:defcommand export-table
   ((table object :optional t :help "The table to export.")
    (format choice :short-arg #\f :default :html
     :choices *formats*
     :help "Format to export to.")
    (whole-file boolean :short-arg #\h
     :help "True to output a complete file, not just the table.")
    (title string :short-arg #\t :help "Title for the table.")
    (output pathname :short-arg #\o :default *standard-output*
     :help "File or stream to output to."))
  "Export a table to an external format."
  :accepts 'table
  (when (and (not table) lish:*input* (typep lish:*input* 'table))
    (setf table lish:*input*))
  (export-table table format output :whole-file whole-file :title title))

;; End
