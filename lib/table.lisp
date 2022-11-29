;;;
;;; table.lisp - Generic table data types
;;;

;; The idea is to provide a generic interface to tables, regardless of the
;; storage of the data. We would like to be able to operate on tables in
;; memory, for example stored in sequences like arrays, lists, or hash tables,
;; with the same interface as tables stored in databases, files, networks,
;; etc. Tables of any underlying data representation or storage method should
;; be able to be accessed using the same interface.
;;
;; See table-print for printing tables.
;; See table-viewer for examining tables.

(defpackage :table
  (:documentation "Generic table data types.")
  (:use :cl :collections :dlib)
  (:export
   ;; column struct
   #:column #:make-column #:column-name #:column-type #:column-width
   #:column-format #:column-align
   ;; table
   #:table #:table-columns
   #:mem-table
   #:db-table #:table-name #:table-indexes
   ;; table funcs
   #:table-add-column
   #:table-update-column-width
   #:table-set-column-type
   #:table-set-column-format
   #:table-column-number
   #:copy-table
   #:replace-cells
   #:map-cells
   #:map-column
   #:map-column-as
   #:map-into-column
   #:table-column
   #:table-column-as
   #:sub-table
   #:pick-columns
   #:insert-column
   #:make-table-from
   ;; #:join
   ))
(in-package :table)

;; (declaim (optimize (speed 0) (safety 3) (debug 3)
;; 		   (space 0) (compilation-speed 0)))

#|
(defstruct column
  "Description of a table column.
  name   - A printable name.
  type   - A Lisp type.
  width  - An integer specifying the desired display width.
  format - A format string for `format', or a format function.
  align  - An alignment such as :left or :right. If NIL, it means the printer
           can make the decision. Format can override this.

Table cell formmating functions should accept CELL and WIDTH, and should be
able accept WIDTH as NIL, to indicate no width limitation. Also, formmating
strings should be alble to be prepended with ~* to ignore the width argument."
  (name nil)
  (type t)
  (width 0)
  (format nil)
  (align nil))
|#

(defclass column ()
  ((name :initarg :name :accessor column-name :initform nil
    :documentation "A printable name.")
   (type :initarg :type :accessor column-type :initform t
    :documentation "A Lisp type.")
   (width :initarg :width :accessor column-width :initform 0
    :documentation "An integer specifying the desired display width.")
   (format :initarg :format :accessor column-format :initform nil
    :documentation "A format string for `format', or a format function.
Table cell formmating functions should accept ‘cell’ and ‘width’, and should be
able accept ‘width’ as NIL, to indicate no width limitation. Formmating strings
should be alble to be prepended with \"~*\" to ignore the width argument.
See print-table:table-format-cell.")
   (align :initarg :align :accessor column-align :initform nil
    :documentation
    "An alignment such as :left or :right. If NIL, it means the printer can make
the decision. Format can override this."))
  (:documentation "A description of a table column."))

(defun make-column (&rest initargs)
  "Make a column instance."
  (apply #'make-instance 'column initargs))

(defun column-p (object)
  "True if ‘object’ is a column."
  (typep object 'column))

(defun copy-column (object)
  "Return a shallow copy of ‘object’ as a column."
  (make-column :name (column-name object)
	       :type (column-type object)
	       :width (column-width object)
	       :format (column-format object)
	       :align (column-align object)))

(defclass table ()
  ((columns :initarg :columns
	    :accessor table-columns
	    :initform nil
	    :documentation "List of column descriptions."))
  (:documentation
   "Table with rows and columns / or a collection of objects with uniform
attributes."))

(defclass mem-table (table container)
  ()
  (:documentation "Table stored in ‘volatile’ memory."))

(defclass db-table (table)
  ((name    :initarg :name
	    :accessor table-name
	    :documentation "Name of the table.")
   (indexes :initarg :indexes
	    :accessor table-indexes
	    :documentation "List of column names that are indexed."))
  (:documentation "A table that is stored in a database."))

(defun table-add-column (table name &key type width format align)
  "Shorthand for adding a column to a table. Name can be just the name or a
column structure. Note that this just adds to the metadata in table-columns.
To add data, use insert-column or collection functions."
  (let ((col (or (and (column-p name) name)
		 (make-column :name (princ-to-string name)))))
    (when type   (setf (column-type   col) type))
    (when width  (setf (column-width  col) width))
    (when format (setf (column-format col) format))
    (when align  (setf (column-align  col) align))
    (setf (table-columns table) (nconc (table-columns table) (list col)))
    table))

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

;; Could be done with defsetf table-column-format
(defgeneric table-set-column-format (table col format)
  (:documentation "Update the column format.")
  (:method (table (col string) format)
    (setf (column-format
	   (find col (table-columns table) :test #'string= :key #'column-name))
	   format))
  (:method (table (col integer) format)
    (setf (column-format (nth col (table-columns table))) format)))

;; Could be done with defsetf table-column-align
(defgeneric table-set-column-align (table col align)
  (:documentation "Update the column alignment.")
  (:method (table (col string) align)
    (setf (column-align
	   (find col (table-columns table) :test #'string= :key #'column-name))
	   align))
  (:method (table (col integer) align)
    (setf (column-align (nth col (table-columns table))) align)))

(defun table-column-number (name table &key (test #'equal))
  "Return the ordinal number of column named NAME from from TABLE, comparing
by the TEST function, which defaults to EQUAL. Return NIL if the column is not
found."
  (position name (table-columns table) :test test :key #'column-name))

;; @@@ Actually TYPE should be able to be any sub-type of TABLE, but we have yet
;; to implement DB-TABLES, etc. so I'm not sure how I want it to work.
;; @@@ We should allow less column names than columns in the object, and make
;; up the rest of the names. Maybe we should consider signaling a correctable
;; error if there's more column names than data.
(defgeneric make-table-from (object &key column-names columns type)
  (:documentation
   "Make a table from another object type.

 OBJECT        An alist or a list of mappable things, or a hash-table, or a
               uniform vector of other mappable types, or a 2d array.
 TYPE          The class of the instance to create, which should be a
               sub-class of MEM-TABLE.
 COLUMN-NAMES  A list of column names, which should match the number of
               columns in OBJECT.
 COLUMNS       A list of plists for initializing table-columns, which should
               match the number of columns in OBJECT. E.g.:
               '((:name \"foo\" :type 'number :format \"[~d]\"))"))

(defmethod make-table-from ((object table) &key column-names columns type)
  "Make a table from a table, which just returns the table."
  (declare (ignore column-names columns type))
  object)

(defun uniform-classes (sequence)
  "Return true if every class in SEQUENCE is a subtype of the first element."
  (let ((first-type (class-of (elt sequence 0))))
    (every (_ (subtypep (type-of _) first-type)) sequence)))

(defun set-columns-names-from-class (table obj)
  (loop :for slot :in (mop:class-slots (class-of obj)) :do
     (table-add-column table
		       (format nil "~:(~a~)" (mop:slot-definition-name slot))
		       :type (mop:slot-definition-type slot))))

(defun sequence-of-classes-p (obj)
  (let ((first-obj (elt obj 0)))
    (and first-obj
	 (or (typep first-obj 'structure-object)
	     (typep first-obj 'standard-object))
	 (uniform-classes obj))))

(defmethod omap (function (table mem-table))
  (if (keyed-collection-p (container-data table))
      (omapk function (container-data table))
      (omap function (container-data table))))

(defmethod oaref ((table mem-table) &rest subscripts)
    (flet ((fail ()
	     (error 'simple-type-error
		    :format-control
		    "Array dimemsions mismatch: given ~s, but should be ~s."
		    :format-arguments
		    (list (length subscripts)
			  (length (array-dimensions (container-data table)))))))
     (case (length subscripts)
       (1 (oelt (container-data table) (first subscripts)))
       (2
	(typecase (container-data table)
	  (array
	   (case (length (array-dimensions (container-data table)))
	     (1 ;; Pretend a nested thing is a 2d array.
	      (oelt (aref (container-data table) (first subscripts))
		    (second subscripts)))
	     (2 ;; Probably really 2d array
	      (apply #'oaref (container-data table) subscripts))
	     (otherwise (fail))))
	  (t
	   (oelt (oelt (container-data table) (first subscripts))
		 (second subscripts)))))
       ;; hope for the best
       (otherwise (apply #'oaref (container-data table) subscripts)))))

(defmethod (setf oaref) (value (table mem-table) &rest subscripts)
  (flet ((fail ()
	   (error 'simple-type-error
		  :format-control
		  "Array dimemsions mismatch: given ~s, but should be ~s."
		  :format-arguments
		  (list (length subscripts)
			(length (array-dimensions (container-data table)))))))
    (case (length subscripts)
      (1 (setf (oelt (container-data table) (first subscripts)) value))
      (2
       (typecase (container-data table)
	 (array
	  (case (length (array-dimensions (container-data table)))
	    (1 ;; Pretend a nested thing is a 2d array.
	     (setf (oelt (aref (container-data table) (first subscripts))
			 (second subscripts)) value))
	    (2 ;; Probably really 2d array
	     (apply #'(setf oaref) value (container-data table) subscripts))
	    (otherwise (fail))))
	 (t
	  (setf (oelt (oelt (container-data table) (first subscripts))
		      (second subscripts)) value))))
      ;; hope for the best
      (otherwise
       (apply #'(setf oaref) value (container-data table) subscripts)))))

(defgeneric copy-table (table)
  (:documentation "Make a copy of TABLE. This defaults to a shallow copy."))

(defmethod copy-table ((table mem-table))
  (let* ((class (class-of table))
	 (result (allocate-instance class)))
    (loop :for slot :in (mapcar #'mop:slot-definition-name
				(mop:class-slots class))
       :when (slot-boundp table slot)
       :do (setf (slot-value result slot)
		 (slot-value table slot)))
    (setf (container-data result)
	  (omap (lambda (row)
		  (copy-seq row))
		(container-data table)))
    result))

(defgeneric replace-cells (function table)
  (:documentation "Replace every cell in TABLE by the result of calling FUNCTION
on it's value. This destructively modifies TABLE."))

(defmethod replace-cells (function (table table))
  (omapn
   (lambda (row)
     (omap-into row function row))
   (container-data table))
  table)

(defgeneric map-cells (function table)
  (:documentation "Return a new table which is the result of FUNCTION applied
to every cell in TABLE. FUNCTION is called with the value of cell."))

(defmethod map-cells (function (table table))
  (let ((result (copy-table table)))
    ;; (omapn
    ;;  (lambda (row)
    ;;    (omapn
    ;; 	(lambda (col)
    ;; 	  (funcall function col))
    ;; 	row))
    ;;  (container-data result))
    (replace-cells function result)
    result))

(defgeneric map-column (column function table)
  (:documentation
   "Call FUNCTION with the value for row of COLUMN in TABLE. COLUMN can be a
number or a column name. Return the results."))

(defmethod map-column (column function (table mem-table))
  (let ((col (or (and (integerp column) column)
		 (table-column-number column table))))
    (when (not col)
      (error "Can't find a column designated by ~s." column))
    (omap (_ (funcall function (oelt _ col))) table)
    ;; @@@ We should return the results, right?
    ;; table
    ))

(defgeneric map-column-as (type column function table)
  (:documentation
   "Call ‘function’ with the value for row of ‘column’ in ‘table’. ‘column’ can
be a number or a column name. Return the results as a ‘type’."))

(defmethod map-column-as (type column function (table mem-table))
  (let ((col (or (and (integerp column) column)
		 (table-column-number column table))))
    (when (not col)
      (error "Can't find a column designated by ~s." column))
    (omap-as type (_ (funcall function (oelt _ col))) table)))

(defgeneric map-into-column (column function table)
  (:documentation
  "Set each value of COLUMN in TABLE, to the result of calling FUNCTION with
the value. COLUMN can be a number or a column name. Return the TABLE."))

(defmethod map-into-column (column function (table mem-table))
  "Set each value of COLUMN in TABLE, to the result of calling FUNCTION with
the value. COLUMN can be a number or a column name. Return the TABLE."
  (let ((col (or (and (integerp column) column)
		 (table-column-number column table))))
    (when (not col)
      (error "Can't find a column designated by ~s." column))
    (omap (_ (setf (oelt _ col) (funcall function (oelt _ col))))
	  table)
    table))

;; @@@ or should it be called table-column-data ?
(defgeneric table-column (column table)
  (:documentation "Return the data from ‘column’ of ‘table’."))

(defmethod table-column (column (table mem-table))
  "Return the data from ‘column’ of ‘table’."
  (map-column column #'identity table))

(defgeneric table-column-as (type column table)
  (:documentation "Return the data from ‘column’ of ‘table’ as ‘type’."))

(defmethod table-column-as (type column (table mem-table))
  "Return the data from ‘column’ of ‘table’ as ‘type’."
  (map-column-as type column #'identity table))

(defgeneric insert-column (name at table &key value)
  (:documentation
   "Insert a column named ‘name’ at column number ‘at’ in ‘table’. ‘name’ can
be a name or a column structure. Not all tables support inserting columns."))

(defmethod insert-column (at name (table mem-table) &key value)
  "Insert a column named ‘name’ before column ‘at’ in ‘table’. Not a"
  (let ((col (or (and (column-p name) name) (make-column :name name))))
    (setf (table-columns table)
	  (insert-at at col (table-columns table))))
  (omap-into
    (container-data table)
    (lambda (row)
      ;; This is stupid. We should have a specialized inserts.
      (let ((glom-type (etypecase row
			 (list 'list)
			 (string 'string)
			 (vector 'vector)
			 (hash-table 'hash-table))))
	(if (zerop at)
	    (oconcatenate
	     (collections::make-collection glom-type :size 1
					   :initial-element value
					   :element-type (type-of row))
	     (osubseq row at))
	  (oconcatenate
	   (osubseq row 0 at)
	   (collections::make-collection glom-type :size 1
					 :initial-element value
					 :element-type (type-of row))
	   (osubseq row at)))))
    (container-data table))
  table)

(defun copy-columns (table)
  "Return a copy of TABLE's columns."
  (omap (_ (copy-column _)) (table-columns table)))

(defmacro call-with-start-and-end (func args)
  "Call func with args and START and END keywords, assume that an environemnt
that has START and START-P and END and END-P."
  `(progn
     (if start-p
	 (if end-p
	     (,func ,@args :start start :end end)
	     (,func ,@args :start start))
	 (if end-p
	     (,func ,@args ::end end)
	     (,func ,@args)))))

(defmethod opick (predicate (collection mem-table)
		  &key from-end key (start nil start-p) (end nil end-p) count)
  (declare (ignorable start start-p end end-p))
  (make-instance
   'mem-table
   :columns (copy-columns collection)
   :data (call-with-start-and-end opick
				  (predicate
				   (container-data collection)
				   :from-end from-end
				   :count count
				   :key key))))

(defun column-ref (table column-designator &optional (n 0))
  "Return the column number for ‘column-designator’, which can be an integer
or a column name as a symbol or string. For convenience ‘n’ is added to the
result."
  (+ n (etypecase column-designator
	 (integer column-designator)
	 (symbol (table-column-number (string column-designator) table))
	 (string (table-column-number column-designator table)))))

(defgeneric sub-table (table &key start-row end-row start-col end-col)
  (:documentation "Return a new table as sub-table of ‘table’, containing rows
from ‘start-row’ to ‘end-row’, and columns from ‘start-col’ to ‘end-col’.
Columns can be numbers or column labels. If the ‘end-col’ is a label, it's an
inclusive limit, otherwise it's an exclusive limit."))

(defmethod sub-table ((table mem-table)
		      &key (start-row 0) end-row (start-col 0) end-col)
  (flet ((col-num (col &optional (n 0))
	   (if (null col) n (column-ref table col n))))
    (let* ((start (col-num start-col))
	   (end (col-num end-col 1))
	   (result
	     (make-instance
	      'mem-table
	      :columns
	      (if end-col
		  (osubseq (table-columns table) start end)
		  (osubseq (table-columns table) start))
	      :data
	      (omap
	       (lambda (row)
		 (if end-col
		     (osubseq row start end)
		     (osubseq row start)))
	       (if end-row
		   (osubseq table start-row end-row)
		   (osubseq table start-row))))))
      result)))

#|
(defmacro do-elements ((elements sequence) &body body)
  `(typecase sequence
     (list)
     (t

(defgeneric select-from (table columns &key where)
  (:documentation ""))

;; More complicated features should probably be done elsewhere and by macros.
(defmethod select-from ((table mem-table) columns &key where order-by limit)
  (flet ((col-num (col &optional (n 0))
	   (if (null col) n (column-ref table col n))))
    (let ((cols (if (eq columns t)
		    (loop :for i :from 0 :below (olength (table-columns table))
			  :collect i)
		    (mapcar #'col-num columns)))
	  (ncols (length cols))
	  (rows nil) result)
      (omapn
       (lambda (row)
	 (let ((new-row (make-array ncols)))
	   (push
	    (
	    (omap (lambda (col)
		    (oelt row col))
		  cols)
	    rows)
       (container-data table))

	   (result
	     (make-instance
	      'mem-table
	      :columns
	      (if (eq columns t)
		  (copy-seq (table-columns table))
		  (loop :for c :in cols
			:collect
			(copy-structure (oelt (table-columns table) c))))
	      :data
      result)))
|#

(defun make-columns (columns-list)
  (loop :for c :in columns-list
	:collect
	   (if (column-p c)
	       c
	       (apply #'make-column c))))

(defun possibly-convert-from-alist (thing)
  "If THING is a list, and the first element looks like an alist, try to convert
the list from an alist to a list of proper lists. If THING is not a list, just
return it. If ‘thing’ looks like a list of non-collections, convert it to a list
of one element lists, so we can make a 1 column table."
  (if (consp thing)
      (cond
	((consp (first thing))
	 (flet ((alist-element-p (x) (not (null (cdr (last x))))))
	   (if (alist-element-p (first thing))
	       (loop :for x :in thing
	         :if (alist-element-p x)
		   :collect (list (car x) (cdr x))
		  :else
		    :collect x)
	       thing)))
	((and (not (stringp (first thing))) (collection-p (first thing)))
	 thing)
	(t
	 ;; Pretend it's 1 column table.
	 (mapcar (_ (list _)) thing)))))

(defmethod make-table-from ((object list) &key column-names columns type)
  "Make a table from an alist or a list of things."
  (let* ((data (possibly-convert-from-alist object))
	 (tt (make-instance (or type 'mem-table) :data data))
	 (first-obj (first data)))
    (cond
      (columns
       (setf (table-columns tt) (make-columns columns)))
      (column-names
       ;; @@@ When there's less column-names than potential columns in object,
       ;; we should make the missing column names.
       (loop :for c :in column-names :do
	    (table-add-column tt c)))
      (t
       (when first-obj
	 (if (sequence-of-classes-p object)
	     (set-columns-names-from-class tt first-obj)
	     (loop :for i :from 0 :below (olength first-obj)
		   :do (table-add-column tt (format nil "Column~d" i)
					 :type t))))))
    tt))

(defmethod make-table-from ((object hash-table) &key column-names columns type)
  "Make a table from a hash table."
  (when (not column-names)
    (setf column-names '("Key" "Value")))
  (when (or (> (length column-names) 2) (> (length columns) 2))
    (error "Hash tables can only have 2 columns."))
  (let ((tt (make-instance (or type 'mem-table) :data object)))
    (cond
      (columns (setf (table-columns tt) (make-columns columns)))
      (column-names
       (loop :for c :in column-names :do
	    (table-add-column tt c))))
    tt))

(defmethod make-table-from ((object array) &key column-names columns type)
  "Make a table from a hash table."
  (when (not (< 0 (length (array-dimensions object)) 3))
    (error "I don't know how to make a table from an array that isn't 1 or ~
            2 dimensions."))
  (let ((tt (make-instance (or type 'mem-table) :data object)))
    (case (length (array-dimensions object))
      (2
       (cond
	 (columns (setf (table-columns tt) (make-columns columns)))
	 (column-names
	  (loop :for c :in column-names :do
	       (table-add-column tt c)))
	 (t
	  (loop :for i :from 0 :to (array-dimension object 0)
	     :do (table-add-column tt (format nil "Column~d" i))))))
      (1
       (cond
	 (columns (setf (table-columns tt) (make-columns columns)))
	 (column-names
	   ;; @@@ When there's less column-names than potential columns in
	   ;; object, we should make the missing column names.
	   (loop :for c :in column-names :do
		(table-add-column tt c)))
	 (t
	  (when (not (zerop (length object)))
	    (let ((first-obj (aref object 0)))
	      (when first-obj
		(if (sequence-of-classes-p object)
		    (set-columns-names-from-class tt first-obj)
		    (loop :for i :from 0 :below (olength first-obj)
		       :do (table-add-column
			    tt (format nil "Column~d" i)))))))))))
    tt))

(defmethod make-table-from ((object structure-object)
			    &key (column-names '("Slot" "Value")) columns type)
  "Make a table from an structure."
  (when (or (> (length column-names) 2) (> (length columns) 2))
    (error "Structures can only have 2 column names."))
  (let ((tt (make-instance (or type 'mem-table) :data object)))
    (cond
      (columns (setf (table-columns tt) (make-columns columns)))
      (column-names
       (loop :for c :in column-names :do
	    (table-add-column tt c))))
    tt))

;; @@@ Is there any downside to this? You can always make a more specific
;; method, if you have a more interesting table object.

(defmethod make-table-from ((object standard-object)
			    &key (column-names '("Slot" "Value")) columns type)
  "Make a table from an structure."
  (when (or (> (length column-names) 2) (> (length columns) 2))
    (error "Standard objects can only have 2 column names."))
  (let ((tt (make-instance (or type 'mem-table) :data object)))
    (cond
      (columns (setf (table-columns tt) (make-columns columns)))
      (column-names
       (loop :for c :in column-names :do
	    (table-add-column tt c))))
    tt))

;; We have two kinds of generic table joins, since the specialized method of
;; joining a sequence of tables is very likely more efficient than performing a
;; series of joins of two tables, but the latter allows us to do joins on
;; sequences of tables of different types.
;;
;; @@@ Or use generic iterators to gather the rows?

#|

(defgeneric join-tables (tables type result-type)
  (:documentation
   "Join a sequence tables of type, with a result of RESULT-TYPE."))

;; @@@ Maybe this would be better called join-two-tables?
(defgeneric join-table (table-1 table-2 result-type)
  (:documentation
   "Join two tables, with a result of RESULT-TYPE."))

(defmethod join-tables (tables (type (eql 'mem-table))
			(result-type (eql 'mem-table)))
  "Join a sequence of mem-tables."
  (let* ((new-cols
	  (omap #'copy-columns tables))
	 (new-row-count
	  (let ((rc 0))
	    (omapn
	     (_ (setf rc (+ rc (olength (table-data _)))))
	     tables)
	    rc))
	 (result
	  (make-instance
	   'mem-table
	   :columns
	   (apply #'oconcatenate new-cols))
	   :data
	   (omap
	    @@@ How about we need to finish iterators?
	    (_ )
	    tables)))
    result))

(defmethod join-table ((table-1 mem-table)
		       (table-2 mem-table)
		       (result-type (eql 'mem-table)))
  "Join individual mem-tables."
  ;; Just use the sequence joiner.
  (join-tables (list table-1 table-2) 'mem-table))

(defun join (tables &key by result-type)
  "Join tables. The result table has rows with all the columns
from the tables. If the tables different numbers of row, the columns from the
shorter tables will be empty. By default, the resulting table is of the same
type as the first table."
  (let ((tables-type
	 (reduce (lambda (a b) (and (eql a b) a)) tables :key #'type-of)))
    (cond
      (table-type
       (join-tables tables tables-type result-type))
      ;; @@@ We could check the whole chain of types works out,
      ;; but, for now, let's just say there has to be a uniform result type.
      #|
      ((loop :with prev-type :and prev-result
	  :for table :in tables
	  :do
	  (when prev-type
	    (find-method 'join-table nil (list prev-type (type-of table)
					       prev-result)
			 nil))))
      |#
      (t
       (reduce (lambda (a b) (join-table a b result-type)) tables)))))

|#

;; EOF
