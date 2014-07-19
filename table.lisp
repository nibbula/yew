;;
;; table.lisp - Generic table data types
;;

;; $Revision: 1.2 $

(defpackage :table
  (:documentation "Generic table data types")
  (:use :cl)
  (:export
   ;; column struct
   #:column #:make-column #:column-name #:column-type #:column-width
   ;; collection
   #:collection #:collection-data
   ;; table
   #:table #:table-columns
   #:mem-table
   #:db-table #:table-name #:table-indexes
   ;; generic functions
   #:element
   #:olength
   ;; table funcs
   #:table-add-column
   #:table-update-column-width
   #:table-set-column-type
   #:print-table
   #:nice-print-table
   ))
(in-package :table)

(declaim (optimize (speed 0) (safety 3) (debug 3)
		   (space 0) (compilation-speed 0)))

(defstruct column "Description of a database column."
  (name ""      :type string)
  (type :string :type keyword)
  (width 0      :type number))

(defclass collection ()
  ((data :initarg :data
	 :accessor collection-data
	 :documentation "Collection of data."))
  (:documentation "A pile of stuff."))

(defclass table ()
  ((columns :initarg :columns
	    :accessor table-columns
	    :documentation "List of column descriptions."))
  (:documentation "Table with rows and columns / or a collection of objects with uniform attributes."))

(defclass mem-table (table collection)
  ()
  (:documentation "Table stored in volatile memory."))

(defclass db-table (table)
  ((name    :initarg :name
	    :accessor table-name
	    :documentation "Name of the table.")
   (indexes :initarg :indexes
	    :accessor table-indexes
	    :documentation "List of column names that are indexed."))
  (:documentation "A table that is stored in a database."))

(defgeneric element (thing index)
  (:documentation "Return the element of THING specified by INDEX.")
  (:method ((thing list) index) 	 (nth index thing))
  (:method ((thing vector) index)        (aref thing index))
  (:method ((thing sequence) index)      (elt thing index))
  (:method ((thing hash-table) index)    (gethash index thing))
  (:method ((thing collection) index)    (element (collection-data thing) index)))

;; Palpable.
(defgeneric olength (thing)
  (:documentation "Return the length of a THING.")
  (:method ((thing list)) 	 	(length thing))
  (:method ((thing vector)) 	        (length thing))
  (:method ((thing sequence))      	(length thing))
  (:method ((thing hash-table))		(hash-table-count thing))
  (:method ((thing collection))		(olength (collection-data thing))))

(defun table-add-column (table name &key type width)
  "Shorthand for adding a column to a table."
  (nconc (table-columns table)
	 (list (make-column :name name :type type :width width))))

;; I wish I could specialize on unsigned-byte.

(defgeneric table-update-column-width (table col width)
  (:documentation "Update the column width, growing not shrinking.")
  (:method (table (col string) width)
    (let ((e (find col (table-columns table)
		   :test #'string= :key #'column-name)))
      (when (> width (column-width e))
	(setf (column-width e) width))))
  (:method (table (col integer) width)
    (let ((e (nth col (table-columns table))))
      (when (> width (column-width e))
	(setf (column-width e) width)))))

;; Could be done with defsetf table-column-type
(defgeneric table-set-column-type (table col type)
  (:documentation "Update the column type.")
  (:method (table (col string) type)
    (setf (column-type
	   (find col (table-columns table) :test #'string= :key #'column-name))
	   type))
  (:method (table (col integer) type)
    (setf (column-type (nth col (table-columns table))) type)))

(defun print-table (table &key (long-titles t))
  "Print results nicely in horizontal table."
  (let ((rows (collection-data table))
	(column-names (mapcar #'column-name (table-columns table))))
    (nice-print-table rows column-names :long-titles long-titles)))

;; This is quite inefficient since it gets the whole data set in
;; one shot and then goes thru every datum twice. But it's nice for
;; small queries since it displays in a somewhat compact form.
(defun nice-print-table (rows column-names &key (long-titles t))
  "Print results nicely in horizontal table. ROWS is a list of row, which are lists of values. COLUMN-NAMES is a list of column names to printed on top. Each column-name can be a string or a list of (\"name\" :<justification>), where <justification> is one of the keywords :LEFT or :RIGHT. If LONG-TITLES is true, make the columns at least as wide as the column names."
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
       (format t fmt size
	       (subseq (string-capitalize (substitute #\space #\_ str))
		       0 (min (length str) size))))
    (terpri)
    ;; Lines
    (loop :for				;f in field-names and
       (size) :in sizes
       :do
       (format t "~v,,,va " size #\- #\-))
    (terpri)
    ;; Values
    (loop :with fmt
       :for r :in rows :do
       (loop :for f :in r :and (size just) :in sizes
	  :do
	  (setf fmt
		(cond ((eql just :right) "~v@a ")
		      ((typep f 'number) "~v@a ")
		      (t "~va ")))
	  (format t fmt size f))
       (terpri)))
    (length rows))

;; EOF
