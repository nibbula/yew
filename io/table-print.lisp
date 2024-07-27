;;;
;;; table-print.lisp - Print tables
;;;

(defpackage :table-print
  (:documentation
   "Once, tables were crafted with wood and saws. Now we can simply print them
from the computer. This package aims to make printing a sturdy table of any
length, a relatively painless and risk free procedure. This does not, of course,
make the table in the first place. For that you want the TABLE package.")
  (:use :cl :dlib :collections :table :char-util :stretchy :ostring
        :fatchar :fatchar-io)
  (:import-from :dlib-misc #:justify-text)
  (:export
   #:table-renderer
   #:table-output-header
   #:table-output-column-type-justification
   #:table-cell-type-formatter
   #:table-format-cell
   #:table-output-column-titles
   #:table-output-column-title
   #:table-output-start-row
   #:table-output-row
   #:table-output-cell
   #:table-output-cell-display-width
   #:table-output-sizes
   #:default-table-output-sizes
   #:table-output-column-separator
   #:table-output-end-row
   #:table-output-row-separator
   #:table-output-footer

   #:text-table-renderer
   ;;#:text-table-renderer-stream
   #:text-table-renderer-prefix
   #:text-table-renderer-suffix
   #:text-table-renderer-separator
   #:text-table-renderer-cursor
   #:text-table-renderer-horizontal-line-char
   #:text-table-adjust-sizes
   #:text-table-cell-lines
   #:*trailing-spaces*

   #:text-box-table-renderer
   #:write-box-line
   #:box-line #:make-box-line
   #:box-style #:make-box-style
   #:*fancy-box* #:*ascii-box* #:*thick-box*
   
   #:output-table
   #:print-table
   #:print-as-table
   #:*long-titles*
   #:*print-titles*
   #:*max-width*
   #:*destination*
   #:with-column-title
   #:set-column-titles
   #:with-row
   #:with-cell
   #:with-table-output

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

(defgeneric table-format-cell (renderer table cell row-number column-number
			       &key width justification use-given-format)
  (:documentation
   "Return ‘cell’ with any formatting applied. If ‘use-given-format’ is NIL,
don't use the formatting given in the table. ‘use-given-format’ should default
to T. If ‘width’ is given, use that as the field width. If ‘justification’
is given, try to use that as the justification.

Table cell formmating functions should accept ‘cell’ and ‘width’, and should be
able accept ‘width’ as NIL, to indicate no width limitation. Also, formmating
strings should be alble to be prepended with ~* to ignore the width argument.
These are given in the ‘format’ slot of a ‘table:column’."))

(defgeneric table-output-column-titles (renderer table titles
					&key sizes &allow-other-keys)
  (:documentation "Output all the column titles."))

(defgeneric table-output-column-title (renderer table title width justification
				       column-number)
  (:documentation "Output a column title."))

(defgeneric table-output-start-row (renderer table)
  (:documentation "Start a row of table output."))

(defgeneric table-output-row (renderer table row row-number &key sizes)
  (:documentation "Output a whole table row."))

(defgeneric table-output-cell (renderer table cell width justification
			       row-number column-number)
  (:documentation "Output a table cell."))

(defgeneric table-output-cell-display-width (renderer table cell column-number
					     &key use-given-format)
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

(defvar *long-titles* nil "True to print long column titles.")
(defvar *print-titles* t "True to print titles.")
(defvar *max-width* nil "Maximum output width of the table.")
(defvar *destination* nil "The current table output destination.")
(defvar *trailing-spaces* nil
  "True to output trailing spaces after the data in the last column.")

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

;; @@@ I suppose we could make a formatter defining macro, but is that
;; really useful/necessary?
(defun table-cell-type-formatter (type func)
  "Return a function that only does special cell formatting for type, but
normal processing for other types. This useful for creating a cell formatting
function."
  (lambda (cell width)
    (if (typep cell type)
	(if width
	    (format nil "~v@a" width (funcall func cell))
	    (funcall func cell))
	(if width
	    (format nil "~v@a" width cell)
	    (format nil "~@a" cell)))))

(defmethod table-format-cell (renderer table cell row-number column-number
			      &key width justification (use-given-format t))
  (declare (ignore renderer row-number))
  (let ((given-format (when (< column-number (length (table-columns table)))
			(column-format (oelt (table-columns table)
					     column-number))))
	op fmt field)
    (flet ((format-it (fmt)
	     (let ((*print-pretty* nil))
	       (with-output-to-fat-string (str)
		 ;; (dbugf :tv "normal format ~s ~s ~s~%" fmt width cell)
		 (format str fmt (or width 0) cell)))))
      (cond
	((and use-given-format (likely-callable given-format))
	 (setf field (funcall given-format cell width)))
	((and use-given-format (ostringp given-format))
	 (setf field (format-it given-format)))
	(t
	 (setf op (typecase cell
		    ((or string ostring) "/fatchar-io:print-string/")
		    (t "a"))
	       fmt (cond
		     ((and (= column-number (1- (olength (table-columns table))))
			   (not *trailing-spaces*))
		      (setf width nil)
		      (s+ "~v" op))
		     ((eql justification :right)
		      (s+ "~v@" op))
		     ((and (not justification) (typep cell 'number))
		      (s+ "~v@" op))
		     (t
		      (s+ "~v" op)))
	       field (format-it fmt))))
      field)))

;; Call output-column-title for each title.
(defmethod table-output-column-titles (renderer table titles &key sizes)
  "Output all the column titles."
  (if sizes
      (loop :with width :and justification :and size
        :for title :in titles
	:and i = 0 :then (1+ i)
        :and col :in (table-columns table)
        :do
        (setf size (elt sizes i))
        (assert (not (consp size)) () "Size shouldn't be a list anymore.")
        (setf width size
	      justification (or (column-align col)
				(table-output-column-type-justification
				 renderer table (column-type col))))
	(table-output-column-title renderer table title width justification i))
      (loop
	:for title :in titles
	:and col :in (table-columns table)
	:and i = 0 :then (1+ i)
	:do
	(table-output-column-title renderer table title
				   (column-width col)
				   (table-output-column-type-justification
				    renderer table (column-type col)) i))))

(defun multi-line-display-length (string)
  "Return the width in character cells of a possibly multi-line ‘string’,
assuming wrapping at ‘*max-width*’."
  (let ((endings (dlib-misc:calculate-line-endings string
						   0 *max-width* nil nil nil)))
    (if endings
	(1+ (loop :for (nil . e) :in endings
		  :maximize e))
	(display-length string))))

(defmethod table-output-cell-display-width (renderer table cell column-number
					    &key (use-given-format t))
  "Return the display width for a table cell."
  ;; @@@ The list check was for when we would have the align in the name, but
  ;; probably shouldn't be used anymore, and can be removed eventually.
  ;; (assert (not (consp cell)) () "Cell shouldn't be a list anymore.")
  ;; (display-length (table-format-cell renderer table
  ;; 				     cell
  ;; 				     nil column-number
  ;; 				     :use-given-format use-given-format))
  ;; This is pretty fugly.
  (multi-line-display-length
   (table-format-cell renderer table
		      cell
		      nil column-number
		      :use-given-format use-given-format)))

;; This is separate from the table-output-sizes default method, so we can easily
;; call it separately without having to find-method or whatever.
(defun default-table-output-sizes (renderer table)
  "Calculate the column sizes for ‘renderer’ and ‘table’ and return an array of
sizes."
  (block nil
    (let ((col-num 0)
	  sizes)
      ;; First set by pre-defined widths and name lengths.
      (omapn
       (lambda (col)
	 (push (max (column-width col)
		    (if *long-titles*
			(table-output-cell-display-width
			 renderer table (column-name col) col-num
			 :use-given-format nil)
			0))
	       sizes)
	 (incf col-num))
       (table-columns table))
      (setf sizes (nreverse sizes))
      (when (zerop col-num)
	(return #()))

      ;; Then set by actual data. We make an ajustable array in case the table
      ;; has variable length rows, we can handle it.
      (setf sizes (make-array col-num
			      :fill-pointer (length sizes)
			      :adjustable t
			      :initial-contents sizes))
      (omapn
       (lambda (row)
	 (setf col-num 0)
	 (when (mappable-p row)
	   (omapn
	    (lambda (col)
	      ;; Handle rows with different amounts of columns.
	      (stretchy-set sizes col-num
			    (max
			     (if (< col-num (length sizes))
				 (aref sizes col-num)
				 0)
			     (table-output-cell-display-width renderer table col
							      col-num)))
	      (incf col-num))
	    row)))
       table)
      sizes)))

(defmethod table-output-sizes (renderer table)
  (default-table-output-sizes renderer table))

;; Default method which does nothing.
(defmethod table-output-footer (renderer table &key width sizes)
  (declare (ignore renderer table width sizes)))

(defmethod table-output-row (renderer table row row-number &key sizes)
  "Output a whole table row."
  (table-output-start-row renderer table)
  (let ((col-num 0))
    (omapn (lambda (cell)
	     (when (not (zerop col-num))
	       (table-output-column-separator renderer table))
	     (table-output-cell
	      renderer table cell
	      ;;(table-output-cell-display-width renderer table cell)
	      ;; Just in case the row size changed.
	      (if (< col-num (length sizes))
		  (aref sizes col-num)
		  (olength cell))
	      (if (< col-num (length (table-columns table)))
		  (table-output-column-type-justification
		   renderer table
		   (column-type
		    (elt (table-columns table) col-num)))
		  :left)
	      row-number col-num)
	     (incf col-num))
	   row)
    (table-output-end-row renderer table row-number)
    (table-output-row-separator renderer table row-number :sizes sizes)))

;; This is just one possible way of many.
(defmethod output-table (table renderer destination
			 &key long-titles print-titles max-width
			 &allow-other-keys)
  "Output a table."
  (let* ((*long-titles* long-titles)
	 (*print-titles* print-titles)
	 (*max-width* max-width)
	 (*destination* destination)
	 (row-num 0) #| (col-num 0) |#
	 (sizes (table-output-sizes renderer table)))
    (table-output-header renderer table :sizes sizes)
    (table-output-column-titles renderer table
				(mapcar #'column-name (table-columns table))
				:sizes sizes)
    (omapn (lambda (row)
	     (table-output-row renderer table row row-num :sizes sizes)
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
     :for title :in titles
     :and i = 0 :then (1+ i)
     :do
     (setf size (aref sizes i)
	   just (or (column-align (oelt (table-columns table) i)) :left))
     ;; @@@ Someday we can get rid of these asserts here and the rest
     (assert (not (consp size)) () "Size shouldn't be a list anymore.")
     (assert (not (consp title)) () "Title shouldn't be a list anymore.")
     (setf str title
	   fmt (if (eql just :right) "~v@a" "~va"))
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
			      table cell width justification
			      row-number column-number)
  "Output a table cell."
  (declare (ignore renderer table row-number column-number))
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
       (assert (not (consp s)) () "Size shouldn't be a list anymore.")
       (format t "~v,,,va+" (+ 2 s) ;; One space of padding on either side
	       #\- #\-))
    (terpri)))

(defmethod table-output-footer ((renderer derp-table-renderer) table
				&key width sizes)
  "Output the bottom of the table."
  (table-output-row-separator renderer table nil :width width :sizes sizes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain text table

(defclass text-table-renderer (table-renderer)
  ((prefix
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
   (horizontal-line-char
    :initarg :horizontal-line-char
    :accessor text-table-renderer-horizontal-line-char
    #| :initform #\- |# :type character
    :documentation "Character for horizontal lines.")
   (cursor
    :initarg :cursor :accessor text-table-renderer-cursor
    :initform 0 :type fixnum
    :documentation
    "Width of output already in this row, i.e. where the cursor would be."))
  (:default-initargs
   :horizontal-line-char #\-)
  (:documentation "Render a text table."))

(defmethod table-output-column-titles ((renderer text-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (with-slots (separator cursor horizontal-line-char) renderer
    (setf cursor 0)
    (let ((sep-len (display-length separator))
	  (stream *destination*))
      (loop :with str
	 :and fmt = "~va"
	 :and len = (length titles)
	 :and size :and just
	 :for col :in titles
	 :and i :from 0 :below (length sizes)
	 :do
	 (assert (not (consp (aref sizes i))) ()
		 "Size shouldn't be a list anymore")
	 (setf size (aref sizes i)
	       just (column-align (oelt (table-columns table) i)))
	 (assert (not (consp col)) () "Column title shouldn't be a list anymore")
	 (setf str col
	       fmt (if (eql just :right) "~v@a" "~va"))
	 (format stream fmt size
		 (subseq (string-capitalize (substitute #\space #\_ str))
			 0 (min (length str) size)))
	 (incf cursor size)
	 (when (< i (1- len))
	   (write-string separator stream)
	   (incf cursor sep-len)))
      (when (or (not *max-width*) (/= cursor *max-width*))
	(terpri stream))

      ;; Lines
      (setf cursor 0)
      (loop :with len = (length sizes) :and size
	 :for i :from 0 :below len
	 :do
	 (setf size (aref sizes i))
	 (format stream "~v,,,va" size horizontal-line-char
		 horizontal-line-char)
	 (incf cursor size)
	 (when (< i (1- len))
	   (write-string separator stream)
	   (incf cursor sep-len)))
      (when (or (not *max-width*) (/= cursor *max-width*))
	(terpri stream)))))

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
  ;; @@@ shouldn't this use display-width instead of length?
  (loop :for field :in column-names
     :collect (typecase field
		(string
		 (if long-titles (length field) nil))
		;; @@@ We shouldn't hit this case anymore, so eventually get rid
		;; of it.
		(list
		 (assert nil () "We shouldn't have hit this case."))
		(t nil))))

;; This is just for text-table-renderer based things.
(defgeneric text-table-adjust-sizes (table renderer sizes max-width)
  (:documentation "Adjust column sizes in the vector SIZES, by the data in
TABLE, limited to MAX-WIDTH."))

(defmethod text-table-adjust-sizes (table
				    (renderer text-table-renderer)
				    sizes max-width)
  "Adjust the sizes in the vector ‘sizes’, by the data in ‘table’, limited
to ‘max-width’. Return the adjusted ‘sizes’ vector, which may be different than
the given one, if the sizes vector is too small."
  (let ((sep-len (length (text-table-renderer-separator renderer)))
	(result sizes))
    (omap
     #'(lambda (row)
	 (let ((len (olength row)) (i 0) (new-size nil) (col 0) size)
	   (omap
	    #'(lambda (field)
		(setf size (progn
			     (when (>= i (length result))
			       (setf result (let ((a (make-array (1+ i))))
					      (replace a result)
					      a)))
			     (aref result i))
		      new-size
		      (if (and size (minusp size))
			  size		; Don't mess with preset size
			  (max (or size 0)
			       (table-output-cell-display-width
				renderer table field i)
			       )))
		;;(incf col (abs new-size))
		(when (and max-width (> (+ col (abs new-size)) max-width))
		  (setf new-size (max 0
				      (- new-size
					 (- (+ col (abs new-size))
					    max-width)))))
		(incf col (abs new-size))
		(when (< i (1- len))
		  (incf col sep-len))
		(setf (aref result i) new-size)
		(incf i))
	    row)))
     table)
    result))

;; This is just for text-table-renderer based things.
(defgeneric text-table-cell-lines (table renderer cell width column-number)
  (:documentation "Return the lines of CELL fitting in WIDTH.")
  (:method (table (renderer text-table-renderer) cell width column-number)
    ;; (declare (ignore table renderer))
    (osplit #\newline
	    (with-output-to-string (str)
	      (justify-text
	       ;; @@@ we should figure out how to avoid formatting the cell
	       ;; multiple times
	       (table-format-cell renderer table
				  cell
				  nil column-number
				  :use-given-format t)
	       :cols width :stream str)))))

(defmethod table-output-start-row ((renderer text-table-renderer) table)
  "Start a row of table output."
  (declare (ignore table))
  (with-slots (prefix) renderer
    (write-string prefix *destination*)))

(defmethod table-output-column-separator ((renderer text-table-renderer)
					  table &key width)
  "Output a separator between columns."
  (declare (ignore table width))
  (with-slots (separator) renderer
    (write-string separator *destination*)))

(defmethod table-output-end-row ((renderer text-table-renderer) table n)
  "End a row of table output."
  (declare (ignore table n))
  (with-slots (suffix) renderer
    (write-string suffix *destination*)))

;; (defmethod table-column-sizes ((table table) (renderer text-table-renderer))
;;   )

#|
(defmethod table-output-cell ((renderer text-table-renderer)
			      table cell width justification
                              row-number column-number)
  "Output a table cell."
  (declare (ignore row))
  (with-slots (cursor) renderer
    (let ((len (display-length thing)))
      (ecase justification
	(:right
	 (loop :repeat (max 0 (min (- width len) *max-width*))
	    (write-char #\space *destination*))
	 (loop :repeat (max 0 (min (- width len) *max-width*))
	    (write-char (oelt thing *destination*)))
	(:left
       (loop :while (
       (when (or (/= column (1- (length (table-columns table))))
		 trailing-spaces)
	 (loop :repeat (min (- width len)

	 
       )
      (:none thing))))
|#

(defmethod table-output-cell ((renderer text-table-renderer)
			      table cell width justification
			      row-number column-number)
  "Output a table cell."
  (with-slots (cursor) renderer
    (let* ((field (table-format-cell renderer table cell row-number column-number
				     :width width
				     :justification justification))
	   (len (display-length field))
	   (stream *destination*))
      ;;(incf cursor len)
      (if (and (eq justification :overflow)
	       (> len width))
	  (progn
	    (write-string field stream)
	    (format stream "~%~v,,,va" width #\space #\space)
	    (setf cursor width))
	  (typecase field
	    (standard-object
	     (write (osubseq field 0 (min width (olength field)))
		    :stream stream :escape nil :readably nil :pretty nil)
	     (incf cursor (min width (olength field))))
	    (t
	     (write (osubseq field 0 (min width (olength field)))
		    :stream stream :escape nil :readably nil :pretty nil)
	     (incf cursor (min width (olength field)))))))))

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
  (let* ((column-names (and print-titles (column-name-list table)))
	 (sizes (if column-names
		    (coerce (column-name-sizes column-names long-titles)
			    'vector)
		    (make-array `(,(olength (oelt table 0)))
				:initial-element nil)))
	 all-zero
	 (*destination* destination)
	 (stream destination)
	 (*long-titles* long-titles)
	 (*print-titles* print-titles)
	 (*max-width* max-width)
	 (*trailing-spaces* trailing-spaces))

    ;; Adjust column sizes by field data
    (setf sizes (text-table-adjust-sizes table renderer sizes max-width))

    ;; Flip pre-set sizes, and force unset sizes to zero.
    ;; (format t "sizes = ~s~%" sizes) (finish-output)
    (map-into sizes (_ (if (and _ (numberp _)) (abs _) 0)) sizes)

    ;; As a special case, if all sizes are zero, set them all to the column name
    ;; sizes, as if we forced long-titles on.
    (when (every #'zerop sizes)
      (setf all-zero t))

    ;; Set up the column justification.
    ;; This makes the sizes elements be: (width :justification).
    (loop :with name #| :and justification |#
       :for i :from 0 :below (length sizes)
       :for col = (column-name-list table) :then (cdr col)
       :do
       (assert (not (consp (car col))) ()
	       "Column names shouldn't be lists anymore")
       (setf name (car col))
       (setf (aref sizes i)
	     (if all-zero (display-length name) (aref sizes i))))

    (table-output-header renderer table)

    ;; Print titles
    (when (and column-names print-titles)
      (table-output-column-titles renderer table column-names :sizes sizes))

    ;; Values
    (with-slots (cursor) renderer
      (let (cell-lines cell-col cell-width (row-num 0))
	(omap
	 #'(lambda (row)
	     (let ((row-len (olength row))
		   (column-num 0)
		   size just column)
	       (setf cursor 0)
	       (table-output-start-row renderer table)
	       (omap
		#'(lambda (field)
		    (setf size (aref sizes column-num)
			  column (oelt (table-columns table) column-num)
			  just (or (and column (column-align column)) :left))
		    (when (eq just :wrap)
		      (setf cell-lines
			    (text-table-cell-lines table renderer field
						   (1+ size) column-num)
			    cell-col cursor
			    cell-width size
			    field (car cell-lines)))
		    (table-output-cell renderer table field size just
				       row-num column-num)
		    (when (and (< column-num (1- row-len))
			       (or (not max-width)
				   (< cursor (1- max-width))))
		      (write-string separator stream)
		      (incf cursor (display-length separator)))
		    (incf column-num))
		row)
	       (when cell-lines
		 (loop :for l :in (cdr cell-lines) :do
		    (when (or (not max-width) (<= cursor (1- max-width)))
		      (terpri stream))
		    (format stream "~v,,,va"
			    cell-col #\space #\space)
		    (setf cursor cell-col)
		    ;; (write-string separator stream)
		    ;; (incf cursor (display-length separator))
		    ;; (dbugf :tp "cell-col n ~s~%" cell-col)
		    (table-output-cell renderer table l cell-width just
				       row-num column-num)
		    (incf cursor (table-output-cell-display-width
				  renderer table l column-num))
		    (when (or (not max-width) (< cursor (1- max-width)))
		      (terpri stream)))
		 (setf cell-lines nil))
	       ;; (when (or (not max-width) (< cursor (1- max-width)))
	       (when (or (not max-width) (< cursor max-width))
		 (terpri stream)))
	     (incf row-num))
	 table)))
    (olength (container-data table)))) ;; @@@ should actually be rows output?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Box tables

(defstruct box-line
  "A line style for a text box."
  left
  horizontal
  joint
  vertical
  right)

(defstruct box-style
  "Style of a text box."
  top
  under-titles
  between-rows
  data
  bottom)

(defclass text-box-table-renderer (table-renderer)
  ((box-style
    :initarg :box-style :accessor text-box-table-renderer-box-style
    :documentation "The style of the box."))
  (:default-initargs
   ;; :separator "|"
   ;; :horizontal-line-char #\-
   )
  (:documentation "A table rendered in text box."))

(defparameter *fancy-box*
  (make-box-style
   :top          (make-box-line :left "╒" :horizontal "═" :joint "╤" :right "╕")
   :under-titles (make-box-line :left "╞" :horizontal "═" :joint "╪" :right "╡")
   :between-rows (make-box-line :left "├" :horizontal "─" :joint "┼" :right "┤")
   :bottom       (make-box-line :left "╘" :horizontal "═" :joint "╧" :right "╛")
   :data         (make-box-line :left "│" :vertical "│" :right "│")
   ))

(defparameter *ascii-box*
  (make-box-style
   :top          (make-box-line :left "," :horizontal "_" :joint "_" :right ".")
   :under-titles (make-box-line :left "|" :horizontal "-" :joint "+" :right "|")
   :between-rows (make-box-line :left "|" :horizontal "-" :joint "+" :right "|")
   :bottom       (make-box-line :left "`" :horizontal "-" :joint "^" :right "'")
   :data         (make-box-line :left "|" :vertical "|" :right "|")
   ))

(defparameter *thick-box*
  (make-box-style
   :top          (make-box-line :left "▗" :horizontal "▄" :joint "▄" :right "▖")
   :under-titles (make-box-line :left "▐" :horizontal "▀" :joint "█" :right "▌")
   :between-rows (make-box-line :left "▐" :horizontal "▀" :joint "█" :right "▌")
   :bottom       (make-box-line :left "▝" :horizontal "▀" :joint "▀" :right "▘")
   :data         (make-box-line :left "▐" :vertical "█" :right "▌")
   ))

(defmethod write-box-line ((renderer text-box-table-renderer) style sizes)
  (write-string (box-line-left style) *destination*)
  (let ((first t))
    (omapn
     (lambda (s)
       (when (not first)
	  (write-string (box-line-joint style) *destination*))
       (loop :repeat s ;; @@@ assumes size is a number
	  :do (write-string (box-line-horizontal style) *destination*))
	(when first (setf first nil)))
     sizes))
  (write-string (box-line-right style) *destination*)
  (write-char #\newline *destination*))

(defmethod table-output-header ((renderer text-box-table-renderer) table
				&key width sizes)
  (declare (ignore table width))
  (write-box-line renderer
   (box-style-top (text-box-table-renderer-box-style renderer)) sizes))

(defmethod table-output-column-titles ((renderer text-box-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (table-output-start-row renderer table)
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
     (when (plusp i)
       (table-output-column-separator renderer table))
     (table-output-column-title renderer table title width justification i))
  (table-output-end-row renderer table 0)
  (write-box-line renderer
   (box-style-under-titles (text-box-table-renderer-box-style renderer)) sizes))

(defmethod table-output-column-title ((renderer text-box-table-renderer)
				      table title width justification
				      column-number)
  (table-output-cell renderer table title
		     width justification nil column-number))

;; @@@ resolve overlap between this and the text-table-renderer version
(defmethod table-output-cell ((renderer text-box-table-renderer)
			      table cell width justification
			      row-number column-number)
  "Output a table cell."
  (let* ((*trailing-spaces* t)
	 (field (table-format-cell renderer table cell row-number column-number
				   :width width
				   :justification justification))
	 (len (display-length field))
	 (stream *destination*))
    (if (and (eq justification :overflow)
	     (> len width))
	(progn
	  (write-string field stream)
	  (format stream "~%~v,,,va" width #\space #\space))
	(typecase field
	  (standard-object
	   (write (osubseq field 0 (min width (olength field)))
		  :stream stream :escape nil :readably nil :pretty nil))
	  (t
	   (write (osubseq field 0 (min width (olength field)))
		  :stream stream :escape nil :readably nil :pretty nil))))))

;; (defmethod table-output-cell ((renderer text-box-table-renderer)
;; 			      table cell width justification
;;                            row-number column-number)
;;   "Output a table cell."
;;   (declare (ignore renderer table row-number column-number))
;;   (let ((*print-pretty* nil)
;; 	(field (table-format-cell renderer table cell row-number column-number
;; 				  :width width :justification justification)))
;;     (write-string field *destination*
;; 	    (if (eq justification :right) "~v@a" "~va") width cell)

(defmethod table-output-start-row ((renderer text-box-table-renderer) table)
  (declare (ignore table))
  (with-slots (box-style) renderer
    (format *destination* "~a" (box-line-left (box-style-data box-style)))))

(defmethod table-output-column-separator ((renderer text-box-table-renderer)
					  table &key width)
  "Output a separator between columns."
  (declare (ignore table width))
  (with-slots (box-style) renderer
    (write-string (box-line-vertical (box-style-data box-style)) *destination*)))

(defmethod table-output-end-row ((renderer text-box-table-renderer) table n)
  (declare (ignore table n))
  (with-slots (box-style) renderer
    (format *destination* "~a~%" (box-line-right (box-style-data box-style)))))

(defmethod table-output-row-separator ((renderer text-box-table-renderer) table
				       n &key width sizes)
  (declare (ignore width))
  (when (< n (1- (olength table)))
    (with-slots (box-style) renderer
      (write-box-line renderer (box-style-between-rows box-style) sizes))))

(defmethod table-output-footer ((renderer text-box-table-renderer) table
				&key width sizes)
  (declare (ignore table width))
  (with-slots (box-style) renderer
    (write-box-line renderer (box-style-bottom box-style) sizes)))

#|
(defmethod output-table ((table table) (renderer text-box-table-renderer)
			 destination
			 &key
			   (long-titles t) (print-titles t) max-width
			   (trailing-spaces t) separator)
  (let ((sizes (table-output-sizes renderer table))
	(*max-width* max-width)
	(*destination* destination))
    (table-output-header renderer table :sizes sizes #|:width |#)
    (call-next-method)
    (table-output-footer renderer table :sizes sizes #|:width |#)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *table-output* nil
  "The dynamic table being constructed.")

(defmacro with-column-title ((&optional title) &body body)
  "Add TITLE as a title, or if TITLE isn't given, the output of the body."
  `(table-add-column *table-output*
		     (or ,title
			 (with-output-to-string (*standard-output*)
			   ,@body))))

(defun set-column-titles (titles)
  "Add TITLE as a title, or if TITLE isn't given, the output of the body."
  (loop :for title :in titles :do
     (table-add-column *table-output* title)))

(defmacro with-row (() &body body)
  "Add a row to the current table. Cells added in the body should go in
this row."
  `(progn
     (when (not *table-output*)
       (error "WITH-TABLE-ROW is probably not inside a WITH-TABLE-OUTPUT."))
     ;; Make a new empty row. 
     (push '() (container-data *table-output*))
     ,@body
     (setf (car (container-data *table-output*))
	   (nreverse (car (container-data *table-output*))))))

(defmacro with-cell (() &body body)
  "Collect the output of the BODY into a table cell."
  `(progn
     (when (not *table-output*)
       (error "WITH-TABLE-CELL is probably not inside a WITH-TABLE-OUTPUT."))
     ;; Make an new row if there isn't one.
     (when (not (container-data *table-output*))
       (push '() (container-data *table-output*)))
     (push (with-output-to-string (*standard-output*)
	     ,@body)
	   (car (container-data *table-output*)))))

(defvar *default-table-renderer-type* 'text-table-renderer
  "Type of the default table renderer. Passed to make-instance.")

(defvar *table-renderer* nil
  "Dynamic current table renderer when not otherwise provided.")

(defun table-renderer ()
  (or *table-renderer*
      (setf *table-renderer*
	    (make-instance *default-table-renderer-type*))))

(defmacro with-table-output ((&key (renderer (table-renderer))
				   (destination *standard-output*)
				   (long-titles t) (print-titles t) max-width)
			     &body body)
  "Output a table collected with WITH-ROW and WITH-CELL in the BODY."
  `(let ((*table-output* (make-instance 'mem-table :data '())))
     ,@body
     (setf (container-data *table-output*)
	   (nreverse (container-data *table-output*)))
     (output-table *table-output* ,renderer ,destination
		   :long-titles ,long-titles
		   :print-titles ,print-titles
		   :max-width ,max-width)))

#| Test:
(with-table-output ()
  (set-column-titles '("Number" "Name" "Numeral" "Ordinal"))
  (loop :for i :from 1 :to 10 :do
     (with-row ()
       (with-cell () (format t "~d" i))
       (with-cell () (format t "~r" i))
       (with-cell () (format t "~@r" i))
       (with-cell () (format t "~:r" i)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-table (table &key (stream *standard-output*)
			    (renderer (table-renderer))
			    (long-titles t) (print-titles t) max-width)
  "Print the table, defaulting to the text-table-renderer on *standard-output*."
  (output-table table renderer stream
		:long-titles long-titles
		:print-titles print-titles
		:max-width max-width)
  (fresh-line stream) ;; @@@ Is this right?
  table)

(defun print-as-table (thing &key column-names table-type
			       (stream *standard-output*)
			       (renderer (table-renderer))
			       (long-titles t) (print-titles t) max-width)
  "Make a table from thing and print it like print-table."
  (output-table (apply #'make-table-from thing
		       `(,@(when column-names `(:column-names ,column-names))
			 ,@(when table-type `(:type ,table-type))))
		renderer stream
		:long-titles long-titles
		:print-titles print-titles
		:max-width max-width)
  thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @@@ This is deprecated and we should eventually get rid of it when there's
;; no more old code that uses it.

(defun nice-print-table (rows column-names
			 &key (long-titles t) (print-titles t)
			   (stream *standard-output*)
			   (trailing-spaces t) max-width
			   (separator " "))
  (declare (ignore trailing-spaces separator))
  (print-as-table rows :column-names column-names
		       :long-titles long-titles
		       :print-titles print-titles
		       :stream stream
		       :max-width max-width))

#|
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
|#

;; EOF
