;;
;; table.lisp - Generic table data types
;;

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
   #:column-format
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
   #:make-table-from
   ))
(in-package :table)

(declaim (optimize (speed 0) (safety 3) (debug 3)
		   (space 0) (compilation-speed 0)))

(defstruct column
  "Description of a table column.
  name   - A printable name.
  type   - A Lisp type.
  width  - An integer specifying the desired display width.
  format - A format string for `format', or a format function.

Table cell formmating functions should accept CELL and WIDTH, and should be
able accept WIDTH as NIL, to indicate no width limitation. Also, formmating
strings should be alble to be prepended with ~* to ignore the width argument."
  (name nil)
  (type t)
  (width 0)
  (format nil))

;; (defclass collection ()
;;   ((data :initarg :data
;; 	 :accessor collection-data
;; 	 :documentation "Collection of data."))
;;   (:documentation "A pile of stuff."))

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

(defun table-add-column (table name &key type width)
  "Shorthand for adding a column to a table."
  (let ((col (make-column :name name)))
    (when type  (setf (column-type  col) type))
    (when width (setf (column-width col) width))
    (setf (table-columns table) (nconc (table-columns table) (list col)))))

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

;; Could be done with defsetf table-column-format
(defgeneric table-set-column-format (table col format)
  (:documentation "Update the column format.")
  (:method (table (col string) type)
    (setf (column-format
	   (find col (table-columns table) :test #'string= :key #'column-name))
	   type))
  (:method (table (col integer) type)
    (setf (column-format (nth col (table-columns table))) type)))

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
  OBJECT       - An alist or a list of mappable things, or a hash-table, or a
                 uniform vector of other mappable types, or a 2d array.
  TYPE         - The class of the instance to create, which should be a
                 sub-class of MEM-TABLE.
  COLUMN-NAME  - A list of column names, which should match the number of
                 columns in OBJECT.
  COLUMNS      - A list of plists for initializing table-columns, which should
                 match the number of columns in OBJECT. E.g.:
                 '((:name \"foo\" :type 'number :format \"[~d]\"))
"))

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

(defun make-columns (columns-list)
  (loop :for c :in columns-list
     :collect
       (apply #'make-column c)))

(defmethod make-table-from ((object list) &key column-names columns type)
  "Make a table from an alist or a list of things."
  (let ((tt (make-instance (or type 'mem-table) :data object))
	(first-obj (first object)))
    (cond
      (columns (setf (table-columns tt) (make-columns columns)))
      (column-names
       ;; @@@ When there's less column-names than potential columns in object,
       ;; we should make the missing column names.
       (loop :for c :in column-names :do
	    (table-add-column tt c)))
      (t
	(when first-obj
	  (if (sequence-of-classes-p object)
	      (set-columns-names-from-class tt first-obj)
	      ;; @@@ This will fail if first-object is not a propper list.
	      ;; But we should be able to take an alist.
	      (loop :for i :from 0 :below (olength first-obj)
		 :do (table-add-column tt (format nil "Column~d" i)
				       :type t))))))
    tt))

(defmethod make-table-from ((object hash-table)
			    &key (column-names '("Key" "Value")) columns type)
  "Make a table from a hash table."
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
		    (loop :for i :from 0 :to (olength first-obj)
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

;; EOF
