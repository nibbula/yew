;;
;; terminal-table.lisp - Table renderer for terminals.
;;

(defpackage :terminal-table
  (:documentation "Table renderer for terminals.")
  (:use :cl :terminal :table :table-print)
  (:export
   #:terminal-table-renderer
   ))
(in-package :terminal-table)

(defclass terminal-table-renderer (text-table-renderer)
  ()
  (:documentation "Render a table to a terminal."))

;; @@@ This is kludgey hack of the text-table-renderer.
(defmethod table-output-column-titles ((renderer terminal-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (declare (ignore table))
  (with-accessors ((separator text-table-renderer-separator)
		   (stream text-table-renderer-stream)) renderer
    (let ((has-underline (terminal-has-attribute stream :underline)))
      (loop :with str
	 :and fmt = "~va"
	 :and len = (length titles)
	 :and size :and just
	 :for col :in titles
	 :and i :from 0 :below (length sizes)
	 :do
	 (setf size (car (aref sizes i))
	       just (cadr (aref sizes i)))
	 (if (listp col)
	     (setf str (first col)
		   fmt (if (eql just :right) "~v@a" "~va"))
	     (setf str col
		   fmt "~va"))
	 (when has-underline
	   (terminal-underline stream t))
	 (terminal-format stream fmt size
			  (subseq (string-capitalize
				   (substitute #\space #\_ str))
				  0 (min (length str) size)))
	 (when has-underline
	   (terminal-underline stream nil))
	 (when (< i (1- len))
	   (terminal-write-string stream separator)))
      (terminal-write-char stream #\newline)

      ;; Lines
      (when (not has-underline)
	(loop :with len = (length sizes)
	   :for i :from 0 :below len
	   :do
	   (terminal-format stream "~v,,,va" (car (aref sizes i)) #\- #\-)
	   (when (< i (1- len))
	     (terminal-write-string stream separator)))
	(terminal-write-char stream #\newline)))))

;; EOF
