;;
;; table-print.lisp - Print tables
;;

(defpackage :table-print
  (:documentation
   "Once, tables were crafted with wood and saws. Now we can simply print them
from the computer. This package aims to make printing a sturdy table of any
length, a relatively painless and risk free procedure. This does not, of course,
make the table in the first place. For that you want the TABLE package.")
  (:use :cl :dlib :table :dlib-misc)
  (:export
   #:output-table
   #:print-table
   #:nice-print-table
   ))
(in-package :table-print)

(declaim (optimize (speed 0) (safety 3) (debug 3)
		   (space 0) (compilation-speed 0)))

(defgeneric output-table (table destination &key long-titles column-names)
  (:documentation "Output a table."))

;; This is quite inefficient since it gets the whole data set in
;; one shot and then goes thru every datum twice. But it's nice for
;; small queries since it displays in a somewhat compact form.
(defun nice-print-table (rows column-names
			 &key (long-titles t) (stream *standard-output*)
			   (trailing-spaces t))
  "Print results nicely in horizontal table. ROWS is a list of row, which are
lists of values. COLUMN-NAMES is a list of column names to printed on top.
Each column-name can be a string or a list of (\"name\" :<justification>),
where <justification> is one of the keywords :LEFT or :RIGHT. If LONG-TITLES
is true, make the columns at least as wide as the column names. COLUMN-NAMES
can be NIL to print without column names."
  (let ((sizes				; Initial column sizes from labels
	 (if column-names
	     (loop :for f :in column-names
		:collect (if long-titles (length f) 0))
	     (make-list (length (first rows)) :initial-element 0))))
    ;; Adjust column sizes by field data
    (loop :for r :in rows
       :do
       (setf sizes
	     (loop :for f :in r :and s :in sizes
		:collect (max s (typecase f
				  (string (length f))
				  (otherwise
				   (length (format nil "~a" f))))))))
    ;; Get justification
    (setf sizes
	  (loop :for s :in sizes
	     :for col = column-names :then (cdr col)
	     :collect
	     (list s (if (and (listp (car col)) (eql (second (car col)) :right))
			 :right :left))))
    ;; Print titles
    (when column-names
      (loop :with str :and fmt = "~va "
	 :for col :in column-names
	 :and (size just) :in sizes
	 :do
	 (if (listp col)
	     (setf str (first col)
		   fmt (if (eql just :right) "~v@a " "~va "))
	     (setf str col
		   fmt "~va "))
	 (format stream fmt size
		 (subseq (string-capitalize (substitute #\space #\_ str))
			 0 (min (length str) size))))
      (terpri stream)
      ;; Lines
      (loop :for				;f in field-names and
	 (size) :in sizes
	 :do
	 (format stream "~v,,,va " size #\- #\-))
      (terpri stream))
    ;; Values
    (loop :with fmt :and cell-lines :and cell-col :and cell-width
       :for r :in rows :do
       (loop :with row-len = (length r) :and col = 0 :and cell
	  :for f :in r :and (size just) :in sizes :and i = 0 :then (1+ i)
	  :do
	  (when (eq just :wrap)
	    (setf cell-lines (split-sequence
			      #\newline (justify-text f :cols size
						      :stream nil))
		  cell-col col
		  cell-width size
		  f (car cell-lines)))
	  (setf fmt
		(cond
		  ((and (= i (1- row-len)) (not trailing-spaces)) "~*~a ")
		  ((eql just :right) "~v@a ")
		  ((typep f 'number) "~v@a ")
		  (t "~va ")))
	  (setf cell (format nil fmt size f))
	  (incf col (length cell))	; of course this isn't right
	  (write-string cell stream))
       (when cell-lines
	 (loop :for l :in (cdr cell-lines) :do
	    (format t "~%~v,,,va~va" cell-col #\space #\space cell-width l))
	 (setf cell-lines nil))
       (terpri stream)))
    (length rows))

;; Version which doesn't assume lists.
(defun oprint-table (table &key (long-titles t) (stream *standard-output*))
  "Print results nicely in horizontal table. ROWS is a list of row, which are
lists of values. COLUMN-NAMES is a list of column names to printed on top.
Each column-name can be a string or a list of (\"name\" :<justification>),
where <justification> is one of the keywords :LEFT or :RIGHT. If LONG-TITLES
is true, make the columns at least as wide as the column names."
  (let* ((column-names
	  (let ((n 0))
	    (mapcar #'(lambda (x)
			(prog1 (or (column-name x)
				   (format nil "Column~a" n))
			  (incf n)))
		    (table-columns table))))
	 (sizes				; Initial column sizes from labels
	  (loop :for f :in column-names
	     :collect (if long-titles (length f) 0))))
    ;; Make fake zero sizes if we have to
    (when (not sizes)
      (if (and (> (olength table) 0)
	       (element table 0)
	       (olength (element table 0)))
	  (setf sizes (make-list
		       (olength (element table 0)) :initial-element 0))
	  (list 0)))
    ;; Adjust column sizes by field data
    (omap (lambda (r)
	    (setf sizes
		  (let ((i sizes) s result)
		    (omap
		     (lambda (f)
		       (when i
			 (setf s (car i))
			 (push (max s (typecase f
					(string (length f))
					(otherwise
					 (length (format nil "~a" f)))))
			       result)
			 (setf i (cdr i))))
		     r)
		    (setf result (nreverse result)))))
	  table)
    ;; Get justification
    (setf sizes
	  (loop :for col :in column-names :and s :in sizes
	     :collect
	     (list s (if (and (listp col) (eql (second col) :right))
			 :right :left))))
    ;; Print titles
    (loop :with str :and fmt = "~va "
       :for col :in column-names
       :and (size just) :in sizes
       :do
       (if (listp col)
	   (setf str (first col)
		 fmt (if (eql just :right) "~v@a " "~va "))
	   (setf str col
		 fmt "~va "))
       (format stream fmt size
	       (subseq (string-capitalize (substitute #\space #\_ str))
		       0 (min (length str) size))))
    (terpri stream)
    ;; Lines
    (loop :for				;f in field-names and
       (size) :in sizes
       :do
       (format stream "~v,,,va " size #\- #\-))
    (terpri stream)
    ;; Values
    (let (fmt)
      (omap (lambda (r)
	      (let* ((i sizes) size just)
		(omap (lambda (f)
			(setf size (caar i)
			      just (cadar i)
			      i (cdr i))
			(setf fmt
			      (cond ((eql just :right) "~v@a ")
				    ((typep f 'number) "~v@a ")
				    (t "~va ")))
			(format stream fmt size f))
		      r)
		(terpri stream)))
	    table))
    (olength table)))

(defclass table-renderer ()
  ()
  (:documentation "Something that displays tables."))

(defgeneric table-output-cell (renderer cell width justification)
  (:documentation "Output a table cell."))

(defgeneric table-output-column-title (renderer title width justification)
  (:documentation "Output a table column title."))

(defgeneric table-output-start-row (renderer n)
  (:documentation "Start a row of table output."))

(defgeneric table-output-end-row (renderer n)
  (:documentation "End a row of table output."))

(defgeneric table-output-header-separator (renderer width)
  (:documentation "End a row of table output."))

;; Version which doesn't assume anything.
(defun ooprint-table (table renderer &key (long-titles t))
;  "Print results nicely in horizontal table. COLUMN-NAMES is a list of column
;names to printed on top. Each column-name can be a string or a list of
;(\"name\" :<justification>), where <justification> is one of the keywords
;:LEFT or :RIGHT. If LONG-TITLES is true, make the columns at least as wide as
;the column names."
  (let* ((column-names
	  (let ((n 0))
	    (mapcar #'(lambda (x)
			(prog1 (or (column-name x)
				   (format nil "Column~a" n))
			  (incf n)))
		    (table-columns table))))
	 (sizes				; Initial column sizes from labels
	  (loop :for f :in column-names
	     :collect (if long-titles (length f) 0))))
    ;; Make fake zero sizes if we have to
    (when (not sizes)
      (if (and (> (olength table) 0)
	       (element table 0)
	       (olength (element table 0)))
	  (setf sizes (make-list
		       (olength (element table 0)) :initial-element 0))
	  (list 0)))
    ;; Adjust column sizes by field data
    (omap (lambda (r)
	    (setf sizes
		  (let ((i sizes) s result)
		    (omap
		     (lambda (f)
		       (when i
			 (setf s (car i))
			 (push (max s (typecase f
					(string (length f))
					(otherwise
					 (length (format nil "~a" f)))))
			       result)
			 (setf i (cdr i))))
		     r)
		    (setf result (nreverse result)))))
	  table)
    ;; Get justification
    (setf sizes
	  (loop :for col :in column-names :and s :in sizes
	     :collect
	     (list s (if (and (listp col) (eql (second col) :right))
			 :right :left))))
    ;; Print titles
    (loop :with str
       :for col :in column-names
       :and (size just) :in sizes
       :do
       (if (listp col)
	   (setf str (first col))
	   (setf str col))
       (table-output-cell
	renderer size just 
	(subseq (string-capitalize (substitute #\space #\_ str))
		0 (min (length str) size))))
    (table-output-end-row renderer -2)
    ;; Lines
    (loop :for				;f in field-names and
       (size) :in sizes
       :do
       (table-output-header-separator renderer size))
    (table-output-end-row renderer -1)
    ;; Values
    (let ((n 0))
      (omap (lambda (r)
	      (let* ((i sizes) size (just :left))
		(omap (lambda (f)
			(setf size (caar i)
			      just (cadar i)
			      i (cdr i))
			(table-output-cell renderer f size just))
		      r)
		(table-output-end-row renderer n)
		(incf n)))
	    table))
    (olength table)))

;; Plain text

(defclass table-plain-text-renderer (table-renderer)
  ((stream
    :initarg :stream :accessor table-plain-text-renderer-stream
    :documentation "Output stream to print on."))
  (:documentation "Render a table with plain text."))

(defmethod table-output-cell ((r table-plain-text-renderer) cell width justification)
  "Output a table cell."
  (let ((fmt (cond
	       ((eql justification :right) "~v@a ")
	       ((typep cell 'number) "~v@a ")
	       (t "~va "))))
    (format (table-plain-text-renderer-stream r) fmt width cell)))

(defmethod table-output-column-title ((r table-plain-text-renderer) title width justification)
  "Output a table column title."
  (let ((fmt (if (eql justification :right) "~v@a " "~va ")))
    (format (table-plain-text-renderer-stream r) fmt width title)))

(defmethod table-output-start-row ((r table-plain-text-renderer) n)
  "Start a row of plain text table output."
  (declare (ignore r n)))

(defmethod table-output-end-row ((r table-plain-text-renderer) n)
  "End a row of plain text table output."
  (declare (ignore n))
  (terpri (table-plain-text-renderer-stream r)))

(defmethod table-output-header-separator ((r table-plain-text-renderer) width)
  "Output a plain text header separator."
  (format (table-plain-text-renderer-stream r) "~v,,,va " width #\- #\-))

#|
(defun generic-print-table (rows column-names
			    &key (long-titles t)
			      (stream *standard-output*)
			      output-col
			      output-line
			      output-title
			      output-newline)
  "Print results nicely in horizontal table. ROWS is a list of row, which are
lists of values. COLUMN-NAMES is a list of column names to printed on top.
Each column-name can be a string or a list of (\"name\" :<justification>),
where <justification> is one of the keywords :LEFT or :RIGHT. If LONG-TITLES
is true, make the columns at least as wide as the column names."
  (flet ((nl
  (let ((sizes				; Initial column sizes from labels
	 (loop :for f :in column-names
	    :collect (if long-titles (length f) 0))))
    ;; Adjust column sizes by field data
    (loop :for r :in rows
       :do
       (setf sizes
	     (loop :for f :in r :and s :in sizes
		:collect (max s (typecase f
				  (string (length f))
				  (otherwise
				   (length (format nil "~a" f))))))))
    ;; Get justification
    (setf sizes
	  (loop :for col :in column-names :and s :in sizes
	     :collect
	     (list s (if (and (listp col) (eql (second col) :right))
			 :right :left))))
    ;; Print titles
    (loop :with str :and fmt = "~va "
       :for col :in column-names
       :and (size just) :in sizes
       :do
       (if (listp col)
	   (setf str (first col)
		 fmt (if (eql just :right) "~v@a " "~va "))
	   (setf str col
		 fmt "~va "))
       (if output-title
	   (funcall output-title
		    (format nil fmt size
			    (subseq (string-capitalize
				     (substitute #\space #\_ str))
				    0 (min (length str) size))))
	   (format t fmt size
		   (subseq (string-capitalize (substitute #\space #\_ str))
			   0 (min (length str) size)))))
    (terpri stream)
    ;; Lines
    (loop :for				;f in field-names and
       (size) :in sizes
       :do
       (format stream "~v,,,va " size #\- #\-))
    (terpri stream)
    ;; Values
    (loop :with fmt
       :for r :in rows :do
       (loop :for f :in r :and (size just) :in sizes
	  :do
	  (setf fmt
		(cond ((eql just :right) "~v@a ")
		      ((typep f 'number) "~v@a ")
		      (t "~va ")))
	  (format stream fmt size f))
       (terpri stream)))
    (length rows))
|#

(defun print-table (table &key (long-titles t) (stream *standard-output*))
  "Print results nicely in horizontal table."
  (let ((rows (collection-data table))
	(column-names (mapcar #'column-name (table-columns table))))
    (nice-print-table rows column-names
		      :long-titles long-titles :stream stream)))

(defmethod output-table ((table table) (destination stream)
			 &key long-titles column-names)
  (declare (ignore column-names))
  (oprint-table table :long-titles long-titles :stream destination))

;; EOF
