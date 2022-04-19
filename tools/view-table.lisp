;;;
;;; view-table.lisp - Command to view a table using the table-viewer.
;;;

(defpackage :view-table
  (:documentation "Command to view a table using the table-viewer.")
  (:use :cl :dlib :view-generic :table :table-print :table-viewer :dtt
        :grout :lish)
  (:export
   #:with-coerced-table
   #:!print-table
   #:view-table-file
   #:view-table-thing
   #:!view-table
   ))
(in-package :view-table)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-coerced-table ((var thing &key column-names) &body body)
    "Evalute BODY with VAR bound to THING coerced into a table. Don't do anything
if THING or lish:*input* NIL."
    (with-unique-names (thunk)
      `(let ((,var (or ,thing lish:*input*)))
	 (flet ((,thunk () ,@body))
	   (when ,var
	     (typecase ,var
	       (table)
	       ((or symbol)
		;; This is semi-bogus interpreting a symbol as a filename,
		;; but this makes the command work.
		(setf ,var (read-table (symbol-name ,var)
				       :column-names ,column-names)))
	       ((or string pathname stream)
		(setf ,var (read-table ,var :column-names ,column-names)))
	       ((or list array hash-table structure-object)
		(setf ,var (make-table-from ,var :column-names ,column-names)))
	       (t
		;; @@@ check with find-method?
		(setf ,var (make-table-from ,var :column-names ,column-names))))
	     (,thunk)))))))

(lish:defcommand print-table
  ((long-titles boolean :short-arg #\l
    :help "True to show full column titles.")
   (column-names list :short-arg #\c
    :help "List of column titles for the table.")
   (renderer object :short-arg #\r :help "Table renderer to use.")
   (table object :help "Table to print."))
  :accepts '(table sequence hash-table structure-object)
  "Print a table to the terminal."
  (with-coerced-table (tab table :column-names column-names)
    (with-grout ()
      (if renderer
	  (grout-print-table tab :long-titles long-titles :renderer renderer)
	  (grout-print-table tab :long-titles long-titles)))
    (setf lish:*output* tab)))

(defun view-table-file (file-name &key long-titles guess-types)
  "View the contents of FILE-NAME as a table."
  (let ((table (read-table file-name)))
    (when guess-types
      (dtt:guess-types table))

    (view-table table :long-titles long-titles)))

(defun view-table-thing (thing &key long-titles guess-types)
  "View the THING as a table."
  (with-coerced-table (table thing)
    (when guess-types
      (dtt:guess-types table))

    (view-table table :long-titles long-titles)))

(defmethod view ((thing table))
  (view-table thing))

(lish:defcommand view-table
  ((long-titles boolean :short-arg #\l :default nil
    :help "True to show full column titles.")
   (table case-preserving-object :optional t :help "Table to view.")
   (guess-types boolean :short-arg #\g :default t
    :help "Guess table data types."))
  :accepts '(or table sequence hash-table structure-object)
  "View a table. Pass or pipe it either a table:table object, or something which
has a table:make-table-from method, which by default are lists, hash-tables,
arrays, and structures. If it's a string or pathname, try to read a table from
the file."
  :accepts '(table sequence hash-table structure-object)
  (view-table-thing table :long-titles long-titles :guess-types guess-types))

;; End
