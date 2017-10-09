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
   #:table-renderer
   #:table-output-header
   #:table-output-column-type-justification
   #:table-output-column-titles
   #:table-output-column-title
   #:table-output-start-row
   #:table-output-cell
   #:table-output-cell-display-width
   #:table-output-sizes
   #:table-output-column-separator
   #:table-output-end-row
   #:table-output-row-separator
   #:table-output-footer

   #:text-table-renderer
   #:text-table-renderer-stream
   #:text-table-renderer-prefix
   #:text-table-renderer-suffix
   #:text-table-renderer-separator
   
   #:output-table
   #:print-table
   #:nice-print-table
   ))
(in-package :table-print)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defclass table-renderer ()
  ()
  (:documentation "Something that displays tables."))

(defgeneric table-output-header (renderer table &key width sizes)
  (:documentation "Output something before the column titles."))

(defgeneric table-output-column-type-justification (renderer table type)
  (:documentation "Return the justification for TYPE."))

(defgeneric table-output-column-titles (renderer table titles
					&key sizes &allow-other-keys)
  (:documentation "Output all the column titles."))

(defgeneric table-output-column-title (renderer table title width justification)
  (:documentation "Output a column title."))

(defgeneric table-output-start-row (renderer table)
  (:documentation "Start a row of table output."))

(defgeneric table-output-cell (renderer table cell width justification)
  (:documentation "Output a table cell."))

(defgeneric table-output-cell-display-width (renderer table cell)
  (:documentation "Return the display width for a table cell."))

(defgeneric table-output-sizes (renderer table)
  (:documentation "Return an array of column sizes."))

(defgeneric table-output-column-separator (renderer table &key width)
  (:documentation "Output a separator between columns."))

(defgeneric table-output-end-row (renderer table n)
  (:documentation "End a row of table output."))

(defgeneric table-output-row-separator (renderer table n &key width sizes)
  (:documentation "Output separator between rows."))

(defgeneric table-output-footer (renderer table &key width sizes)
  (:documentation "Output the bottom of the table."))

;; @@@ or should it be called table-output, to be orthogonal?
(defgeneric output-table (table renderer destination
			  &key long-titles print-titles max-width
			  &allow-other-keys)
  (:documentation "Output a table."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default methods
;;

;; Default method which does nothing.
(defmethod table-output-header (renderer table &key width sizes)
  (declare (ignore renderer table width sizes)))

;; Numbers right justified and everything else, left.
(defmethod table-output-column-type-justification (renderer table type)
  (declare (ignore renderer table))
  (cond
    ((subtypep type 'number) :right)
    (t :left)))

;; Call output-column-title for each title.
(defmethod table-output-column-titles (renderer table titles &key sizes)
  "Output all the column titles."
  (if sizes
    (loop :with width :and justification :and size
       :for title :in titles
       :and i = 0 :then (1+ i)
       :and col :in (table-columns table)
       :do
       (setf size (elt sizes i)
	     width (if (listp size) (car size) size)
	     justification (if (listp size)
			       (cadr size)
			       (table-output-column-type-justification
				renderer table (column-type col))))
       (table-output-column-title renderer table title width justification))
    (loop
       :for title :in titles
       :and col :in (table-columns table)
       :do
       (table-output-column-title renderer table title
				  (column-width col)
				  (table-output-column-type-justification
				   renderer table (column-type col))))))

;; This is just a simple character count.
(defmethod table-output-cell-display-width (renderer table cell)
  "Return the display width for a table cell."
  (declare (ignore renderer table))
  (typecase cell
    (string (length cell))
    (otherwise
     (length (princ-to-string cell)))))

;; This just figures the maximum of all sizes.
(defmethod table-output-sizes (renderer table)
  (let ((sizes (make-array (max (olength
				 (element (collection-data table) 0))
				(olength (table-columns table)))))
	(col-num 0))
    ;; First set by pre-defined widths and name lengths.
    (omapn
     (lambda (col)
       (setf (aref sizes col-num)
	     (max (column-width col) (length (column-name col))))
       (incf col-num))
     (table-columns table))
    ;; Then set by actual data.
    (omapn
     (lambda (row)
       (setf col-num 0)
       (omapn
	(lambda (col)
	  (setf (aref sizes col-num)
		(max
		 (aref sizes col-num)
		 (table-output-cell-display-width renderer table col)))
	  (incf col-num))
	row))
     (collection-data table))
    sizes))

;; Default method which does nothing.
(defmethod table-output-footer (renderer table &key width sizes)
  (declare (ignore renderer table width sizes)))

;; This is just one possible way of many.
(defmethod output-table (table renderer destination
			 &key long-titles print-titles max-width
			   &allow-other-keys)
  "Output a table."
  (declare (ignore destination long-titles print-titles max-width)) ; @@@
  (let ((row-num 0) (col-num 0)
	(sizes (table-output-sizes renderer table)))
    (table-output-header renderer table :sizes sizes)
    (table-output-column-titles renderer table
				(mapcar #'column-name (table-columns table))
				:sizes sizes)
    (omapn (lambda (row)
	     (table-output-start-row renderer table)
	     (setf col-num 0)
	     (omapn (lambda (cell)
		      (when (not (zerop col-num))
			(table-output-column-separator renderer table))
		      (table-output-cell
		       renderer table cell
		       ;;(table-output-cell-display-width renderer table cell)
		       (aref sizes col-num)
		       (table-output-column-type-justification
			renderer table
			(column-type
			 (elt (table-columns table) col-num))))
		      (incf col-num))
		    row)
	     (table-output-end-row renderer table row-num)
	     (table-output-row-separator renderer table row-num :sizes sizes)
	     (incf row-num))
	   table)
    (table-output-footer renderer table :sizes sizes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; derpy test table which uses the default methods

(defclass derp-table-renderer (table-renderer)
  ()
  (:documentation "Test generic table rendering."))

(defmethod table-output-header ((renderer derp-table-renderer) table
				 &key width sizes)
  "Output something before the column titles."
  (table-output-row-separator renderer table nil :width width :sizes sizes))

(defmethod table-output-column-titles ((renderer derp-table-renderer)
					table titles
					&key sizes &allow-other-keys)
  "Output all the column titles."
  (princ "| ")
  (loop :with str :and fmt :and size :and just
     :for title :in titles :and i = 0 :then (1+ i) :do
     (if (listp (aref sizes i))
	 (setf size (car (aref sizes i))
	       just (cadr (aref sizes i)))
	 (setf size (aref sizes i)
	       just :left))
     (if (listp title)
	 (setf str (first title)
	       fmt (if (eql just :right) "~v@a" "~va"))
	 (setf str title
	       fmt "~va"))
     (format t (s+ fmt " | ")
      	     size
      	     (subseq str 0 (min (length str) size))))
  (terpri)
  (table-output-row-separator renderer table nil :sizes sizes))

(defmethod table-output-start-row ((renderer derp-table-renderer) table)
  "Start a row of table output."
  (declare (ignore renderer table))
  (princ "| "))

(defmethod table-output-cell ((renderer derp-table-renderer)
			      table cell width justification)
  "Output a table cell."
  (declare (ignore renderer table))
  (let ((*print-pretty* nil))
    (format t (if (eq justification :right) "~v@a" "~va") width cell)))

;; (defmethod table-output-cell-display-width ((renderer derp-table-renderer)
;; 					    table cell)
;;   "Return the display width for a table cell."
;;   (length cell))

(defmethod table-output-column-separator ((renderer derp-table-renderer) table
					  &key width)
  "Output a separator between columns."
  (declare (ignore renderer table width))
  (princ " | "))

(defmethod table-output-end-row ((renderer derp-table-renderer) table n)
  "End a row of table output."
  (declare (ignore renderer table n))
  (format t " |~%"))

(defmethod table-output-row-separator ((renderer derp-table-renderer) table n
				       &key width sizes)
  "Output separator between rows."
  (declare (ignore renderer table width))
  (when (not n)
    (princ "+")
    (loop
       :for s :across sizes :do
       (format t "~v,,,va+" (+ 2 #| one space of padding on either side |#
			       (if (listp s) (car s) s))
	       #\- #\-))
    (terpri)))

(defmethod table-output-footer ((renderer derp-table-renderer) table
				&key width sizes)
  "Output the bottom of the table."
  (table-output-row-separator renderer table nil :width width :sizes sizes))

;; @@@ or should it be called table-output, to be orthogonal?
;; (defmethod output-table (table renderer destination
;; 			  &key long-titles print-titles max-width
;; 			  &allow-other-keys)
;;   "Output a table."
;;   (declare (ignore long-titles print-titles max-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain text table

(defclass text-table-renderer (table-renderer)
  ((stream
    :initarg :stream :accessor text-table-renderer-stream
    :documentation "The stream to output to.")
   (prefix
    :initarg :prefix :accessor text-table-renderer-prefix
    :initform "" :type string
    :documentation "Prefix for rows.")
   (suffix
    :initarg :suffix :accessor text-table-renderer-suffix
    :initform "" :type string
    :documentation "Suffix for rows.")
   (separator
    :initarg :separator :accessor text-table-renderer-separator
    :initform " " :type string
    :documentation "Separator between columns.")
   )
  (:documentation "Render a text table."))

(defmethod table-output-column-titles ((renderer text-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (declare (ignore table))
  (with-slots (stream separator) renderer
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
       (format stream fmt size
	       (subseq (string-capitalize (substitute #\space #\_ str))
		       0 (min (length str) size)))
       (when (< i (1- len))
	 (write-string separator stream)))
    (terpri stream)

    ;; Lines
    (loop :with len = (length sizes)
       :for i :from 0 :below len
       :do
       (format stream "~v,,,va" (car (aref sizes i)) #\- #\-)
       (when (< i (1- len))
	 (write-string separator stream)))
    (terpri stream)))

(defun column-name-list (table &optional (template "Column~d"))
  "Return a list of column names for TABLE or a TEMPLATE filled in with the
column number."
  (let ((n 0))
    (mapcar #'(lambda (x)
		(prog1 (or (column-name x)
			   (format nil template n))
		  (incf n)))
	    (table-columns table))))

(defun column-name-sizes (column-names long-titles)
  ;; Initial column sizes from labels
  (loop :for field :in column-names
     :collect (typecase field
		(string
		 (if long-titles (length field) nil))
		(list
		 (let ((width (find-if #'numberp field)))
		   (if long-titles
		       (if width
			   (- (max (length (car field)) width 0))
			   (length (car field)))
		       (and width (- width)))))
		(t nil))))

(defun adjust-sizes-for-data (table renderer sizes max-width)
  "Adjust the sizes in the vector SIZES, by the data in TABLE."
  (let ((sep-len (length (text-table-renderer-separator renderer))))
    (omap
     #'(lambda (row)
	 (let ((len (olength row)) (i 0) (new-size nil) (col 0) size)
	   (omap
	    #'(lambda (field)
		(setf size (aref sizes i)
		      new-size
		      (if (and size (minusp size))
			  size		; Don't mess with preset size
			  (max (or size 0)
			       (typecase field
				 (string (length field))
				 (otherwise
				  (length (princ-to-string field)))))))
		;;(incf col (abs new-size))
		(when (and max-width (> (+ col (abs new-size)) max-width))
		  (setf new-size (max 0
				      (- new-size
					 (- (+ col (abs new-size))
					    max-width)))))
		(incf col (abs new-size))
		(when (< i (1- len))
		  (incf col sep-len))
		(setf (aref sizes i) new-size)
		(incf i))
	    row)))
     (collection-data table))))

(defmethod table-output-start-row ((renderer text-table-renderer) table)
  "Start a row of table output."
  (declare (ignore table))
  (with-slots (stream prefix) renderer
    (write-string prefix stream)))

(defmethod table-output-column-separator ((renderer text-table-renderer)
					  table &key width)
  "Output a separator between columns."
  (declare (ignore table width))
  (with-slots (stream separator) renderer
    (write-string separator stream)))

(defmethod table-output-end-row ((renderer text-table-renderer) table n)
  "End a row of table output."
  (declare (ignore table n))
  (with-slots (stream suffix) renderer
    (write-string suffix stream)))

;; (defmethod table-column-sizes ((table table) (renderer text-table-renderer))
;;   )

(defmethod output-table ((table table) (renderer text-table-renderer)
			 destination
			 &key
			   (long-titles t) (print-titles t) max-width
			   (trailing-spaces t) (separator " "))
  "Output a table to a destination.

LONG-TITLES can be true, to make the columns at least as wide as the names.
PRINT-TITLES can be nil, to make the columns headings not print.

MAX-WIDTH is the maximum width of the table. If necessary, the last column is
resized to fit in this, and the whole row is trimmed to this."
  ;; @@@ Setting the stream here is somewhat wrongish.
  (setf (text-table-renderer-stream renderer) destination)
  (let* ((column-names (and print-titles (column-name-list table)))
	 (sizes (if column-names
		    (coerce (column-name-sizes column-names long-titles)
			    'vector)
		    (make-array `(,(olength (element table 0)))
				:initial-element nil)))
	 all-zero
	 (stream destination))

    ;;(format t "sizes = ~s~%" sizes) (finish-output)

    ;; Adjust column sizes by field data
    (adjust-sizes-for-data table renderer sizes max-width)

    ;; Flip pre-set sizes, and force unset sizes to zero.
    ;; (format t "sizes = ~s~%" sizes) (finish-output)
    (map-into sizes (_ (if (and _ (numberp _)) (abs _) 0)) sizes)

    ;; As a special case, if all sizes are zero, set them all to the column name
    ;; sizes, as if we forced long-titles on.
    (when (every #'zerop sizes)
      (setf all-zero t))

    ;; Set up the column justification.
    ;; This makes the sizes elements be: (width :justification).
    (loop :with name :and justification
       :for i :from 0 :below (length sizes)
       :for col = column-names :then (cdr col)
       :do
       (if (listp (car col))
	   (progn
	     (setf justification 
		   (if (member (second (car col))
			       '(:right :wrap :overflow))
		       (second (car col))
		       ;; default to left justification
		       :left)
		   name (first (car col))))
	   (setf justification :left
		 name (car col)))
       (setf (aref sizes i)
	     (list (if all-zero
		       (length name)
		       (aref sizes i))
		   justification)))

    (table-output-header renderer table)

    ;; Print titles
    (when (and column-names print-titles)
      (table-output-column-titles renderer table column-names :sizes sizes))

    ;; Values
    (let (fmt cell-lines cell-col cell-width)
      (omap
       #'(lambda (row)
	   (let ((row-len (olength row))
		 (col 0)
		 (i 0)
		 cell size just)
	     (table-output-start-row renderer table)
	     (omap
	      #'(lambda (field)
		  (setf size (car (aref sizes i))
			just (cadr (aref sizes i)))
		  (when (eq just :wrap)
		    (setf cell-lines
			  (split-sequence
			   #\newline (justify-text field :cols (1+ size)
						   :stream nil))
			  cell-col col
			  cell-width size
			  field (car cell-lines)))
		  (setf fmt
			(cond
			  ((and (= i (1- row-len)) (not trailing-spaces))
			   "~*~a")
			  ((eql just :right)
			   "~v@a")
			  ((typep field 'number)
			   "~v@a")
			  (t
			   "~va")))
		  (let ((*print-pretty* nil))
		    (setf cell (format nil fmt size field)))
		  (incf col (length cell))	; of course this isn't right
		  (if (and (eq just :overflow)
			   (> (length cell) size))
		      (progn
			(write-string cell stream)
			(format stream "~%~v,,,va" size #\space #\space))
		      (write-string (subseq cell 0 (min size (length cell)))
				    stream))
		  (when (< i (1- row-len))
		    (write-string separator stream)
		    (incf col (length separator)))
		  (incf i))
	      row)
	     (when cell-lines
	       (loop :for l :in (cdr cell-lines) :do
		  (format stream "~%~v,,,va~va"
			  cell-col #\space #\space cell-width l))
	       (setf cell-lines nil))
	     (terpri stream)))
       (collection-data table)))
    (length (collection-data table)))) ;; @@@ should actually be rows output?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is quite inefficient since it gets the whole data set in
;; one shot and then goes thru every datum twice. But it's nice for
;; small queries since it displays in a somewhat compact form.
;;
;; @@@ Of course, *as usual*, This logic fails for unicode, since it uses
;; things like LENGTH to determine how many columns a character occupies.
;;
(defun nice-print-table (rows column-names
			 &key (long-titles t) (print-titles t)
			   (stream *standard-output*)
			   (trailing-spaces t) max-width
			   (separator " "))
  "Print results nicely in horizontal table.
ROWS is a list of row, which are lists of values.

COLUMN-NAMES is a list of column names to printed on top, which can be
NIL to print without column names. Each column-name can be a string or a list
of the form:
  (\"name\" [:<attribute>...] [width])
where <attribute> is one of the keywords :LEFT, :RIGHT, :WRAP, :OVERFLOW, and
width is the desired width of the column. If the width isn't given, the width of
the columns is determined by the maximum width required to print the data.

LONG-TITLES can be true, to make the columns at least as wide as the names.
PRINT-TITLES can be nil, to make the columns headings not print, even though
you have provided COLUMN-NAMES.

MAX-WIDTH is the maximum width of the table. If necessary, the last column is
resized to fit in this, and the whole row is trimmed to this."
  (let ((sizes
	 (if column-names
	     ;; Initial column sizes from labels
	     (loop :for field :in column-names
		:collect (typecase field
			   (string
			    (if long-titles (length field) nil))
			   (list
			    (let ((width (find-if #'numberp field)))
			      (if long-titles
				  (if width
				      (- (max (length (car field)) width 0))
				      (length (car field)))
				  (and width (- width)))))
			   (t nil)))
	     (make-list (length (first rows)) :initial-element nil)))
	(sep-len (length separator)))
    ;;(format t "sizes = ~s~%" sizes)

    ;; Adjust column sizes by field data
    (loop :for row :in rows
       :do
       (setf sizes
	     (loop :with col = 0 :and len = (length row) :and new-size
		:for field :in row
		:and size :in sizes
		:and i = 0 :then (1+ i)
		:do
		(setf new-size
		      (if (and size (minusp size))
			  size	; Don't mess with preset size
			  (max (or size 0)
			       (typecase field
				 (string (length field))
				 (otherwise
				  (length (princ-to-string field)))))))
		;;(incf col (abs new-size))
		(when (and max-width (> (+ col (abs new-size)) max-width))
		  ;;(format t "Chow ~s ~s~%" new-size col)
		  (setf new-size (max 0
				      (- new-size
					 (- (+ col (abs new-size))
					    max-width)))))
		(incf col (abs new-size))
		(when (< i (1- len))
		  (incf col sep-len))
		:collect new-size
		)))
    ;; Flip pre-set sizes.
    ;;(format t "sizes = ~s~%" sizes)
    (setf sizes (mapcar #'abs sizes))

    ;; Get justification
    (setf sizes
	  (loop :for size :in sizes
	     :for col = column-names :then (cdr col)
	     :collect
	     (list size (if (and (listp (car col))
				 (member (second (car col))
					 '(:right :wrap :overflow)))
			    (second (car col))
			    ;; default to left justification
			    :left))))
    ;; Print titles
    (when (and column-names print-titles)
      (loop :with str :and fmt = "~va" :and len = (length column-names)
	 :for col :in column-names
	 :and (size just) :in sizes
	 :and i = 0 :then (1+ i)
	 :do
	 (if (listp col)
	     (setf str (first col)
		   fmt (if (eql just :right) "~v@a" "~va"))
	     (setf str col
		   fmt "~va"))
	 (format stream fmt size
		 (subseq (string-capitalize (substitute #\space #\_ str))
			 0 (min (length str) size)))
	 (when (< i (1- len))
	   (write-string separator stream)))
      (terpri stream)
      ;; Lines
      (loop :with len = (length sizes)
	 :for (size) :in sizes
	 :and i = 0 :then (1+ i)
	 :do
	 (when (not (zerop size))
	   (format stream "~v,,,va" size #\- #\-)
	   (when (< i (1- len))
	     (write-string separator stream))))
      (terpri stream))

    ;; (dbug "sizes = ~a~%" sizes)
    
    ;; Values
    (loop :with fmt :and cell-lines :and cell-col :and cell-width
       :for row :in rows :do
       (loop :with row-len = (length row) :and col = 0 :and cell
	  :for field :in row
	  :and (size just) :in sizes
	  :and i = 0 :then (1+ i)
	  :do
	  (when (eq just :wrap)
	    (setf cell-lines (split-sequence
			      #\newline (justify-text field :cols (1+ size)
						      :stream nil))
		  cell-col col
		  cell-width size
		  field (car cell-lines)))
	  (setf fmt
		(cond
		  ((and (= i (1- row-len)) (not trailing-spaces)) "~*~a")
		  ((eql just :right) "~v@a")
		  ((typep field 'number) "~v@a")
		  (t "~va")))
	  (setf cell (format nil fmt size field))
	  (incf col (length cell))	; of course this isn't right
	  (if (and (eq just :overflow)
		   (> (length cell) size))
	      (progn
		(write-string cell stream)
		(format stream "~%~v,,,va" size #\space #\space))
	      (write-string (subseq cell 0 (min size (length cell))) stream))
	  (when (< i (1- row-len))
	    (write-string separator stream)
	    (incf col (length separator))))
       (when cell-lines
	 (loop :for l :in (cdr cell-lines) :do
	    (format stream "~%~v,,,va~va"
		    cell-col #\space #\space cell-width l))
	 (setf cell-lines nil))
       (terpri stream)))
    (length rows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-table (table &key (stream *standard-output*)
			    (long-titles t) (print-titles t) max-width)
  "Print results nicely in horizontal table."
  (output-table table (make-instance 'text-table-renderer) stream
		:long-titles long-titles
		:print-titles print-titles
		:max-width max-width))
;; EOF
