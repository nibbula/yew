;;;
;;; print-table.lisp - Command to print a table using the table printer.
;;;

(defpackage :print-table
  (:documentation "Command to print a table using the table printer.")
  (:use :cl :lish :grout :view-table)
  (:export
   #:!print-table
   ))
(in-package :print-table)

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

;; End
