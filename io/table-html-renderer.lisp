;;;
;;; table-html-renderer.lisp - Output a table to HTML.
;;;

(defpackage :table-html-renderer
  (:documentation "Output a table to HTML.")
  (:use :cl :dlib :collections :table :table-print)
  (:export
   #:html-table-renderer
   ))
(in-package :table-html-renderer)

(defclass html-table-renderer (table-renderer)
  ((indent
    :initarg :indent :accessor html-table-renderer-indent
    :initform 0 :type integer
    :documentation "How many spaces to indent.")
   (offset
    :initarg :indent :accessor html-table-renderer-offset
    :initform 2 :type integer
    :documentation "How many spaces to indent per item.")
   (embed-style
    :initarg :embed-style :accessor html-table-renderer-embed-style
    :initform t :type boolean
    :documentation "True to embed style in the table."))
  (:documentation "Render a table as HTML."))

(defun indent (n)
  (dotimes (i n) (write-char #\space *destination*)))

(defun table-line (renderer offset-direction format &rest args)
  (with-slots (indent offset) renderer
    (when (minusp offset-direction)
      (incf indent (* offset-direction offset)))
    (indent indent)
    (apply #'format *destination* format args)
    (terpri *destination*)
    (when (plusp offset-direction)
      (incf indent (* offset-direction offset)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-embedded-style ((stream) &body body)
    (with-names (did-style)
      `(let (,did-style)
	 (flet ((add-style (prop)
		  (when (not ,did-style)
		    (write-string " style=\"" ,stream)
		    (setf ,did-style t))
		  (write-string prop ,stream))
		(style-done ()
		  (when ,did-style
		    (write-char #\" ,stream))))
	   ,@body)))))
       
(defmethod table-output-header ((renderer html-table-renderer) table
				 &key width sizes)
  "Output something before the column titles."
  (declare (ignore #|table|# width sizes))
  (table-line renderer 1 "<table>")
  (table-line renderer 1 "<thead>")
  (table-line renderer 1 "<tr>")

  (with-slots (embed-style) renderer
    (omap (lambda (col)
	    (table-line renderer 0 "<th~@[~a~]>~a</th>"
			(when embed-style
			  (with-output-to-string (str)
			    (with-embedded-style (str)
			      (when (column-align col)
				(add-style (format nil "text-align: ~a;"
						   (case (column-align col)
						     (:left "start")
						     (:right "end")
						     (:center "center")
						     (:wrap "justify")))))
			      (when (plusp (column-width col))
				(add-style (format nil "width: ~sem"
						   (column-width col))))
			      (style-done))))
			(column-name col)))
	  (table-columns table)))
  
  (table-line renderer -1 "</tr>")
  (table-line renderer -1 "</thead>")
  (table-line renderer 1 "<tbody>"))

(defmethod table-output-column-titles ((renderer html-table-renderer)
					table titles
					&key sizes &allow-other-keys)
  "Output all the column titles."
  ;; (declare (ignore sizes))
  (declare (ignore renderer table titles sizes)))

#|
(defmethod table-output-column-title ((renderer html-table-renderer)
				      table titles
				      &key sizes &allow-other-keys)
  "Output all the column titles."
  )
|#

(defmethod table-output-sizes ((renderer html-table-renderer) table)
  ;; All zeros, since it doesn't matter.
  (make-array (length (table-columns table))
	      :element-type 'fixnum :initial-element 0))

(defmethod table-output-start-row ((renderer html-table-renderer) table)
  "Start a row of table output."
  (declare (ignore table))
  (table-line renderer 1 "<tr>"))

(defmethod table-output-end-row ((renderer html-table-renderer) table n)
  "Start a row of table output."
  (declare (ignore table n))
  (table-line renderer 1 "<tr>"))

(defmethod table-output-cell ((renderer html-table-renderer)
			      table cell width justification row column)
  "Output a table cell."
  (declare (ignore table row column))
  (with-slots (embed-style) renderer
    (table-line renderer 0 "<td~@[~a~]>~a</td>"
		(when embed-style
		  (with-output-to-string (str)
		    (with-embedded-style (str)
		      (when justification
			(add-style (format nil "text-align: ~a;"
					   (case justification
					     (:left "start")
					     (:right "end")
					     (:center "center")
					     (:wrap "justify")))))
		      (when (plusp width)
			(add-style (format nil "width: ~sem" width)))
		      (style-done))))
		cell)))

(defmethod table-output-column-separator ((renderer html-table-renderer) table
					  &key width)
  "Output a separator between columns."
  (declare (ignore renderer table width))
  )

(defmethod table-output-end-row ((renderer html-table-renderer) table n)
  "End a row of table output."
  (declare (ignore table n))
  (table-line renderer -1 "</tr>"))

(defmethod table-output-row-separator ((renderer html-table-renderer) table n
				       &key width sizes)
  "Output separator between rows."
  (declare (ignore renderer table width sizes)))

(defmethod table-output-footer ((renderer html-table-renderer) table
				&key width sizes)
  "Output the bottom of the table."
  (declare (ignore width sizes))
  (table-line renderer -1 "</tbody>")
  (table-line renderer -1 "</table>"))

;; End
