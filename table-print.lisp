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

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defclass table-renderer ()
  ()
  (:documentation "Something that displays tables."))

(defgeneric table-output-header (renderer table)
  (:documentation "Output something before the column titles."))

(defgeneric table-output-column-titles (renderer table titles
					&key &allow-other-keys)
  (:documentation "Output all the column titles."))

(defgeneric table-output-column-title (renderer table title width justification)
  (:documentation "Output a column title."))

(defgeneric table-output-start-row (renderer table)
  (:documentation "Start a row of table output."))

(defgeneric table-output-cell (renderer table cell width justification)
  (:documentation "Output a table cell."))

(defgeneric table-output-column-separator (renderer table width)
  (:documentation "Output a separator between columns."))

(defgeneric table-output-end-row (renderer table n)
  (:documentation "End a row of table output."))

(defgeneric table-output-row-separator (renderer table n)
  (:documentation "Output separator between rows."))

(defgeneric table-output-footer (renderer table)
  (:documentation "Output the bottom of the table."))

(defgeneric output-table (table renderer destination
			  &key long-titles print-titles max-width
			  &allow-other-keys)
  (:documentation "Output a table."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod table-output-header (renderer table)
  "Output something before the column titles."
  (declare (ignore renderer table)))

(defmethod table-output-column-titles (renderer table titles &key sizes)
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

(defmethod table-output-column-title (renderer table title width justification)
  "Output a column title."
  (declare (ignore renderer table title width justification)))

(defmethod table-output-start-row (renderer table)
  "Start a row of table output."
  (declare (ignore table))
  (with-slots (stream prefix) renderer
    (write-string prefix stream)))

(defmethod table-output-cell (renderer table cell width justification)
  "Output a table cell."
  (declare (ignore renderer table cell width justification)))

(defmethod table-output-column-separator (renderer table width)
  "Output a separator between columns."
  (declare (ignore table width))
  (with-slots (stream separator) renderer
    (write-string separator stream)))

(defmethod table-output-end-row (renderer table n)
  "End a row of table output."
  (declare (ignore table n))
  (with-slots (stream suffix) renderer
    (write-string suffix stream)))

(defmethod table-output-row-separator (renderer table n)
  "Output separator between rows."
  (declare (ignore renderer table n)))

(defmethod table-output-footer (renderer table)
  "Output the bottom of the table."
  (declare (ignore renderer table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	 (let ((len (olength row)) (i 0) (new-size nil) col size)
	   (omap
	    #'(lambda (field)
		(setf col 0
		      size (aref sizes i)
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
		  ;;(format t "Chow ~s ~s~%" new-size col)
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

(defmethod output-table ((table table) (renderer text-table-renderer)
			 destination
			 &key
			   (long-titles t) (print-titles t) max-width
			   (trailing-spaces t) (separator " "))
  "Output a table to a destination."
  ;; @@@ Setting the stream here is somewhat wrongish.
  (setf (text-table-renderer-stream renderer) destination)
  (let* ((column-names (and print-titles (column-name-list table)))
	 (sizes (if column-names
		    (coerce (column-name-sizes column-names long-titles)
			    'vector)
		    (make-array `(,(olength (element table 0)))
				:initial-element nil)))
	 (stream destination))

    ;;(format t "sizes = ~s~%" sizes)

    ;; Adjust column sizes by field data
    (adjust-sizes-for-data table renderer sizes max-width)

    ;; Flip pre-set sizes.
    ;;(format t "sizes = ~s~%" sizes)
    (map-into sizes #'abs sizes)

    ;; Get justification
    (loop
       :for i :from 0 :below (length sizes)
       :for col = column-names :then (cdr col)
       :do
       (setf (aref sizes i)
	     (list (aref sizes i)
		   (if (and (listp (car col))
			    (member (second (car col))
				    '(:right :wrap :overflow)))
		       (second (car col))
		       ;; default to left justification
		       :left))))

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
		  (setf cell (format nil fmt size field))
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
	 (format stream "~v,,,va" size #\- #\-)
	 (when (< i (1- len))
	   (write-string separator stream)))
      (terpri stream))
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

#|
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

(defmethod output-table ((table table) (destination stream)
			 &key long-titles column-names)
  (declare (ignore column-names))
  (oprint-table table :long-titles long-titles :stream destination))

|#

(defun print-table (table &key (long-titles t) (stream *standard-output*))
  "Print results nicely in horizontal table."
  ;; (let ((rows (collection-data table))
  ;; 	(column-names (mapcar #'column-name (table-columns table))))
  ;;   (nice-print-table rows column-names
  ;; 		      :long-titles long-titles :stream stream)))
  (output-table table (make-instance 'text-table-renderer) stream
		:long-titles long-titles))
;; EOF
