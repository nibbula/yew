;;;
;;; table-viewer.lisp - View tables.
;;;

(defpackage :table-viewer
  (:documentation "View tables.")
  (:use :cl :dlib :collections :table :table-print :keymap :inator :terminal
	:terminal-inator :char-util :fui :fatchar :fatchar-io :grout
	:terminal-table :ostring :view-generic)
  (:export
   #:viewer-table-renderer
   #:table-viewer
   #:table-viewer-table
   #:table-viewer-renderer
   #:table-viewer-recalculate-sizes
   #:table-point
   #:table-point-row
   #:table-point-col
   #:hide-column
   #:set-column-hidden-state
   #:view-cell
   #:current-cell
   #:cell-box
   #:view-table
   ))
(in-package :table-viewer)

;; (declaim (optimize (speed 0) (safety 0) (debug 3) (space 0)))

(defkeymap *table-viewer-keymap* ()
  `((#\q		. quit)
    (#\?		. help)
    (#\<		. move-to-top)
    (#\>		. move-to-bottom)
    (#\i		. record-info)
    (#\a		. add-row)
    (#\e		. edit-cell)
    (#\v		. view-cell)
    (:home		. move-to-top)
    (:end		. move-to-bottom)
    (:up		. previous)
    (:down		. next)
    (:right		. scroll-right)
    (:left		. scroll-left)
    (:scroll-up		. scroll-up)
    (:scroll-down	. scroll-down)
    ;;(,(meta-char #\i)	. table-info)
    (#\escape		. *table-viewer-escape-keymap*)
    ))

(defkeymap *table-viewer-escape-keymap* ()
  `((,(ctrl #\s)	. sort-descending-command)
    (#\S		. sort-multiple-command)
    (#\i		. table-info)
    (#\h		. hide-column)
    (#\H		. show-all-columns)
    ))

(add-keymap *default-inator-escape-keymap* *table-viewer-escape-keymap*)

;; @@@ maybe there should be a point class in inator?
(defstruct table-point
  "A character grid location of the cursor in a table. This is usually at
the start of a cell."
  row
  col)

(deftype table-selection ()
  "Type of a table selection. NIL for no selection, a cons of start-row end-row
for a range of rows, or a table-point for a specific item,"
  '(or null cons table-point))

(defvar *table-viewer* nil
  "The current table viewer.")

(defclass table-viewer (terminal-inator)
  ((table
    :initarg :table :accessor table-viewer-table
    :documentation "The table to view.")
   (cached-table-length
    :initarg :cached-table-length :accessor cached-table-length
    :initform -1 :type fixnum
    :documentation "The cached number of rows of the table data.")
   (last-sort-direction
    :initarg :last-sort-direction :accessor last-sort-direction
    :initform nil :type (or null (member :ascending :descending))
    :documentation
    "The direction of the last sort, or NIL if there wasn't one.")
   (renderer
    :initarg :renderer :accessor table-viewer-renderer
    :initform (make-instance 'viewer-table-renderer) :type table-renderer
    :documentation "The table renderer.")
   (long-titles
    :initarg :long-titles :accessor table-viewer-long-titles
    :initform nil :type boolean
    :documentation "True to show long column titles.")
   (message
    :initarg :message :accessor table-viewer-message :initform nil
    :documentation "Temporary message.")
   (search-string
    :initarg :search-string :accessor table-viewer-search-string :initform nil
    :documentation "Last search string or NIL."))
  (:default-initargs
   :point (make-table-point :row 0)
   :keymap `(,*table-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "View a table."))

(defgeneric table-length (table-viewer)
  (:documentation "Return the number of rows of the table data.")
  (:method ((o table-viewer))
    (or (and (plusp (cached-table-length o)) (cached-table-length o))
	(olength (container-data (table-viewer-table o))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View columns

(defclass view-column (column)
  ((hidden
    :initarg :hidden :accessor column-hidden :initform nil :type boolean
    :documentation "True to omit displaying the column.")
   ;; @@@ This isn't used yet. We have to figure out how to handle all the
   ;; print-table code that expects a sizes array.
   (size
    :initarg :size :accessor column-size :initform 0 :type fixnum
    :documentation "Displayed Width of the column in character cells."))
  (:documentation "Column type for viewing."))

;; This is mostly so we can treat the view columns as the size array for
;; the print-table code.
(defclass view-columns (container)
  ()
  (:documentation "A container for the view columns."))

(defun make-view-columns (table)
  "Make the initial view columns from ‘table’. Note that this doesn't calculate
this sizes."
  (let ((result (make-array (length (table-columns table)))))
    (loop :for c :in (table-columns table)
	  :for i = 0 :then (1+ i)
	  :do
	  (setf (aref result i)
		(funcall #'make-instance 'view-column
			 :name (column-name c)
			 :type (column-type c)
			 :width (column-width c)
			 :format (column-format c)
			 :align (column-align c))))
    (make-instance 'container :data result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized table renderer

(defclass viewer-table-renderer (table-renderer)
  ((x ;; @@@ fake bbox
    :initarg :x :accessor viewer-table-renderer-x
    :initform 0 :type fixnum
    :documentation "Horizontal position.")
   (y
    :initarg :y :accessor viewer-table-renderer-y
    :initform 0 :type fixnum
    :documentation "Vertical position.")
   (width
    :initarg :width :accessor viewer-table-renderer-width
    :initform (tt-width) :type fixnum
    :documentation "Horizontal size.")
   (height
    :initarg :height :accessor viewer-table-renderer-height
    :initform (tt-height) :type fixnum
    :documentation "Vertical size.")
   (output-x
    :initarg :output-x :accessor output-x
    :documentation "Current window X coordinate in character cells.")
   (output-y
    :initarg :output-y :accessor output-y
    :documentation "Current window Y coordinate in character cells.")
   (sizes
    :initarg :sizes :accessor sizes :initform nil
    :documentation "Pre-calculated sizes.")
   (columns
    :initarg :columns :accessor view-columns :initform nil
    :documentation "A container array of view columns.")
   (incremental-sizes-p
    :initarg :incremental-sizes :accessor incremental-sizes-p
    :initform nil :type boolean
    :documentation "True to calculate sizes incrementally as they are viewed.
This can be quicker for large tables.")
   (separator
    :initarg :separator :accessor separator :initform " "
    :documentation "Separator between columns.")
   (start
    :initarg :start :accessor start
    :documentation "Point to view from.")
   (rows
    :initarg :rows :accessor rows :type integer
    :documentation "Number of rows in the view.")
   (current-position
    :initarg :current-position :accessor current-position :initform nil
    :documentation "Position to display as current in the table.")
   (selection
    :initarg :selection :accessor table-viewer-selection :initform nil
    :type table-selection
    :documentation "Part of the table selected.")
   (cursor
    :initarg :cursor :accessor table-viewer-cursor
    :documentation "Where the actual cursor is or should be.")
   (last-displayed-col
    :initarg :last-displayed-col :accessor table-viewer-last-displayed-col
    :initform nil
    :documentation "The last column that was displayed."))
  (:default-initargs
   :start (make-table-point :row 0 :col 0)
   :cursor (make-table-point))
  (:documentation "An interactive table renderer."))

(defvar *incremental-view-threshold* 100000
  "How many rows before we switch to incrementally updating the sizes.")

(defgeneric update-view-sizes (renderer table)
  (:documentation "Update the sizes for the current view."))

(defmethod update-view-sizes ((renderer viewer-table-renderer) table)
  (with-slots (start rows sizes) renderer
    (let* ((row-num (table-point-row start))
	   (sub-table (table-subseq table row-num (+ row-num rows)))
	   (new-sizes (default-table-output-sizes renderer sub-table)))
      (if sizes
	  (loop :for i :from 0 :below (olength sizes)
	    :when (> (oelt new-sizes i) (oelt sizes i))
	    :do (setf (oelt sizes i) (oelt new-sizes i)))
	  (setf sizes new-sizes)))))

(defmethod table-output-sizes ((renderer viewer-table-renderer) table)
  (declare (ignorable table))
  (when (not (view-columns renderer))
    (setf (view-columns renderer) (make-view-columns table)))
  (cond
    ((incremental-sizes-p renderer)
     ;; Update only the sizes that we can see.
     (update-view-sizes renderer table)
     (sizes renderer))
    (t
     ;; Cache the sizes so we don't have to recompute them every time.
     ;; Then use the default method.
     (or (sizes renderer)
	 (setf (sizes renderer) (call-next-method))))))

(defun in-view (renderer #| &optional row-in |#)
  ;; (with-slots (start rows) *table-viewer*
  ;;   (let ((row (or row-in (output-y renderer))))
  ;;     (and (>= row (table-point-row start))
  ;; 	   (< row (+ (table-point-row start) rows))
  ;; 	   (< (output-x renderer) (1- *max-width*))))))
  (with-slots (width) renderer
    (< (output-x renderer) (1- width))))

;; @@@ There should only be one of tv-output-cell or tv-cell-output ???

(defun tv-output-cell (renderer table cell width justification row column
		       given-format)
  (declare (ignore row))
  (with-slots (output-x) renderer
    (let (op fmt field len)
      (flet ((format-it (fmt)
	       (let ((*print-pretty* nil))
		 (with-output-to-fat-string (str)
		   (format str fmt (or width 0) cell)))))
	(cond
	  ((likely-callable given-format)
	   (setf field (funcall given-format cell width)))
	  ((ostringp given-format)
	   (setf field (format-it given-format)))
	  (t
	   (setf op (typecase cell
		      ((or string fat-string) "/fatchar-io:print-string/")
		      (t "a"))
		 fmt (cond
		       ((and (= column (1- (olength (table-columns table))))
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
	(setf len (min width (olength field)))
	(typecase cell
	  (standard-object
	   (princ (osubseq field 0 len) *terminal*))
	  (t
	   (write (osubseq field 0 len)
		  :stream *terminal* :escape nil :readably nil :pretty nil)))))))

(defun tv-cell-output (renderer table cell width justification row column
		       given-format)
  (declare (ignore row))
  (with-slots (output-x) renderer
    (let (op fmt field len)
      (flet ((format-it (fmt)
	       (let ((*print-pretty* nil))
		 (with-output-to-fat-string (str)
		   (format str fmt (or width 0) cell)))))
	(cond
	  ((likely-callable given-format)
	   (setf field (funcall given-format cell width)))
	  ((ostringp given-format)
	   (setf field (format-it given-format)))
	  (t
	   (setf op (typecase cell
		      ((or string fat-string) "/fatchar-io:print-string/")
		      (t "a"))
		 fmt (cond
		       ((and (= column (1- (olength (table-columns table))))
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
	(setf len (min width (olength field)))

	(with-output-to-fat-string (str)
	  (typecase cell
	    (standard-object
	     (princ (osubseq field 0 len) str))
	    (t
	     (write (osubseq field 0 len)
		    :stream str :escape nil :readably nil :pretty nil))))))))

(defun de-dork (name)
  (typecase name
    (list
     (values (first name) (or (second name) nil)))
    (t
     (values name nil))))

(defmethod table-output-column-titles ((renderer viewer-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (declare (ignorable titles))
  (with-slots (start last-displayed-col separator output-x output-y x y width
	       columns) renderer
    (let ((has-underline (tt-has-attribute :underline)))
      (tt-move-to y (+ x 2))
      (setf output-x 2)
      (loop
	:with col-num  = (table-point-col start)
	:and max-col   = (length (table-columns table))
	:and sep-len   = (display-length separator)
	:and first-col = t
	:and cell-width
	:and clipped-width
	:and name
	:and justification
	:while (and (< col-num max-col)
		    (< output-x (- *max-width* 1 sep-len)))
	:do
	;; Write a separator if we have room and we're not at the first col
	(unless (column-hidden (oelt columns col-num))
	  (when (and (not first-col)
		     (< (+ output-x sep-len) *max-width*)
		     (/= col-num (table-point-col start)))
	    (tt-write-string separator)
	    (incf output-x sep-len))

	  (when has-underline
	    (tt-underline t))

	  (setf cell-width (aref sizes col-num)
		clipped-width (max 0 (min (- *max-width* output-x)
					  cell-width)))
	  ;; (dbugf :tv "title cell-width ~s clipped-width ~s output-x ~s ~
          ;;             *max-width* ~s~%"
	  ;; 	  cell-width clipped-width output-x *max-width*)
	  (setf (values name justification)
		(de-dork
		 (column-name (oelt (table-columns table) col-num))))
	  (tv-output-cell renderer table
			  name clipped-width justification 0 col-num nil)
	  (incf output-x clipped-width)

	  (when has-underline
	    (tt-underline nil))

	  (setf last-displayed-col col-num
		first-col nil))
	(incf col-num))
      (when (<= output-x *max-width*)
	;; (tt-write-char #\newline)
	(tt-move-to (+ y (1+ output-y)) x))
      (incf output-y))))

(defvar *cell-box-recording* nil
  "Cell box coordinates to record.")

(defun record-cell-box (row col x y width height)
  "Record the box for cell at ‘row’ and ‘col’, if it's asked to be recorded."
  (when *cell-box-recording*
    (dbugf :qq "cell-box [~s ~s] [~s ~s ~s ~s] ~s~%" row col x y width height
	   *cell-box-recording*)
    (let ((cell (assoc (cons row col) *cell-box-recording* :test #'equal)))
      (when cell
	(dbugf :qq "FOUND IT ~s~%" cell)
	(setf (cdr cell) `((,x ,y ,width ,height)))))))

(defmethod table-output-cell ((renderer viewer-table-renderer)
			      table cell width justification row column)
  (declare (ignore width justification))
  "Output a table cell."
  (with-slots (start rows cursor selection last-displayed-col current-position
	       output-x output-y sizes x columns) renderer
    ;; (dbugf :tv "Howdy ~s ~s~%" cell start)
    (when (and (>= column (table-point-col start))
	       (not (column-hidden (oelt columns column))))
      ;; (dbugf :tv "well now ~s ~s ~s~%" sizes column cursor)
      (let* ((cell-width (aref sizes column))
	     (clipped-width (max 0 (min (- *max-width* output-x)
					cell-width)))
	     (hilite (and current-position
			  (= row (table-point-row current-position))))
	     (cell (tv-cell-output renderer table cell clipped-width
				   nil ;; bogus justification
				   row column
				   (column-format
				    (oelt (table-columns table) column)))))
	;; @@@ Bogusly pretending the height is 1
	(record-cell-box row column output-x output-y clipped-width 1)
	(if hilite
	    (progn
	      ;; Save the cursor y position
	      (when (not (table-point-row cursor))
		(setf (table-point-row cursor) output-y))
	      (if (and (table-point-col current-position)
		       (= column (table-point-col current-position)))
		  (progn
		    ;; Save the cursor x position
		    (setf (table-point-col cursor) output-x)
		    (setf cell (ochar:osimplify cell))
		    (tt-write-string
		     (style:themed-string
		      '(:program :table :current-cell :style) cell)
		     ;; (style:styled-string '(:fg-black :bg-yellow) cell)
		     ))
		  (progn
		    (tt-write-string
		     (style:themed-string '(:program :table :current-row :style)
					  cell)))))
	    (tt-write-string cell))

	(incf (output-x renderer) clipped-width)
	(tt-normal)))))

;; (defmethod text-table-adjust-sizes (table
;; 				    (renderer viewer-table-renderer)
;; 				    sizes max-width)
;;   ;; Cache the sizes so we don't have to recompute them every time.
;;   (if (sizes renderer)
;;       (progn
;; 	(map-into sizes #'identity (sizes renderer))
;;       	(dbugf :tv "didn't update sizes~%"))
;;       (progn
;; 	(call-next-method)
;; 	(dbugf :tv "updated sizes~%")
;; 	(setf (sizes renderer) (copy-seq sizes))))
;;   sizes)

;; This is so we can hopefully handle anyting that make-table-from can throw
;; at us. It's not as if it's a good idea.
;; @@@ maybe we could just use table:sub-table, but it doesn't handle
;; hash/struct?
(defun table-subseq (table start &optional end)
  "Return an object made from TABLE that we can do SUBSEQ on."
  (typecase (container-data table)
    ((or list array)
     (if end
	 (osubseq table start end)
	 (osubseq table start)))
    ;; @@@ Maybe this could be keyed-collection-p ?
    ((or structure-object hash-table)
     (let ((i 0) result)
       (block nil
	 ;; @@@ Should we make a collection function to iterate a range of
	 ;; a keyed collection? Or should we make a subseq that works on
	 ;; these keyed things? This is sub-good.
	 (omapk (_ (cond
		     ((and end (= i end)) (return nil))
		     ((>= i start) (push _ result))))
		(container-data table)))
       result))))

(defmethod output-table (table (renderer viewer-table-renderer) destination
			 &key long-titles print-titles max-width
			   &allow-other-keys)
  "Output a table."
  (declare (ignore print-titles max-width))
  (with-slots (output-x output-y separator width x y start rows
	       last-displayed-col columns) renderer
    (let* ((*long-titles* long-titles)
	   (row-num (table-point-row start))
	   (col-num (table-point-col start))
	   (sizes (table-output-sizes renderer table))
	   (sep-len (display-length separator))
	   (first-col t)
	   ;; @@@ Hopefully this is not too slow. We should probably cache
	   ;; it if the view start and size hasn't changed.
	   (sub-table (table-subseq table row-num (min (+ row-num rows)
						       (olength table))))
	   (*destination* (or destination *terminal*))
	   (*max-width* width)
	   (*trailing-spaces* t)) ;; @@@ bogus?
      ;; (dbugf :tv "sizes ~s~%" sizes)
      (setf output-x 0
	    output-y 0
	    last-displayed-col nil)

      (table-output-column-titles renderer table
				  (mapcar #'column-name (table-columns table))
				  :sizes sizes)
      (omapn
       (lambda (row)
	 (setf output-x 0
	       col-num 0
	       first-col t)
	 (omapn
	  (lambda (cell)
	    (when (not (column-hidden (oelt columns col-num)))
	      (when first-col
		;; Move to the start of the row, for the first column.
		(tt-move-to-col (+ x 2))
		(setf output-x 2))

	      ;; Output the column separator
	      (when (not first-col)
		(when (in-view renderer)
		  (incf (output-x renderer) sep-len)
		  (tt-write-string separator)))

	      ;; Output the cell
	      (when (and (>= col-num (table-point-col start))
			 (< output-x (- *max-width* 1 sep-len)))
		(table-output-cell renderer table cell
				   nil ;; bogus width
				   nil ;; bogus justification
				   row-num col-num))
	      (setf first-col nil))
	    (incf col-num))
	  row)

	 ;; End of the row.
	 ;; (when (in-view renderer)
	 ;;   (tt-write-char #\newline))
	 ;; (tt-write-char #\newline)
	 (tt-move-to (+ y (1+ output-y)) x)
	 (incf output-y)
	 (incf row-num))
       sub-table))))

(defgeneric cell-box (table-viewer row col)
  (:documentation
   "Return the coordinates and size of the cell at ‘row’ and ‘col’ in
‘table-viewer’, as the values ‘x’ ‘y’ ‘width’ ‘height’."))

(defmethod cell-box ((o table-viewer) row col)
  (with-slots (table renderer long-titles) o
    (let ((*cell-box-recording* `(((,row . ,col) ()))))
      (with-terminal-output-to-string (:null)
	(tt-home)
	(output-table table renderer *terminal* :long-titles long-titles))
      (values-list (cadar *cell-box-recording*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inator methods

;; Navigation
(defun forward-row (o &optional (n 1))
  (with-slots ((point inator::point)) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) 0
	     (table-point-col point) 0))
      ((table-point-row point)
       (if (< (+ (table-point-row point) n) (table-length o))
	   (incf (table-point-row point) n)
	   (setf (table-point-row point) (1- (table-length o))))))))

(defun backward-row (o &optional (n 1))
  (with-slots ((point inator::point)) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) (1- (table-length o))))
      ((table-point-row point)
       (if (> (- (table-point-row point) n) 0)
	   (decf (table-point-row point) n)
	   (setf (table-point-row point) 0))))))

(defmethod next ((o table-viewer))
  "Move to the next line."
  (forward-row o))

(defmethod previous ((o table-viewer))
  "Move to the previous line."
  (backward-row o))

;; @@@ But something else doesn't get the value right.
;; But at least it doesn't crash.
(defun fake-o-row-len (x row)
  "An olength that does the right thing for us for keyed-collections."
  (if (keyed-collection-p x)
      2
      (olength (oelt x row))))

(defun forward-col (o &optional (n 1))
  (with-slots ((point inator::point) table renderer) o
    (with-slots (start last-displayed-col columns) renderer
      (cond
	;;((or (not (table-point-row point)) (not (table-point-col point)))
	((not (table-point-col point))
	 (setf (table-point-col point) 0))
	((and (table-point-row point) (table-point-col point))
	 (loop :with i = 0
	       :while (and (< i n)
			   (< (table-point-col point)
			      (1- (fake-o-row-len
				   table (table-point-row point)))))
	       :do
	       (incf (table-point-col point))
	       (when (not (column-hidden (oelt columns (table-point-col point))))
		 (incf i))))))))

(defun backward-col (o &optional (n 1))
  (with-slots ((point inator::point) table renderer) o
    (with-slots (start columns) renderer
      (cond
	;;((or (not (table-point-row point)) (not (table-point-col point)))
	((not (table-point-col point))
	 (setf (table-point-col point)
	       (1- (fake-o-row-len table (table-point-row point)))))
	((and (table-point-row point) (table-point-col point))
	 (loop :with i = 0
	       :while (and (< i n)
			   (> (table-point-col point) 0))
	       :do
	       (decf (table-point-col point))
	       (when (not (column-hidden (oelt columns (table-point-col point))))
		 (incf i))))))))

(defmethod forward-unit ((o table-viewer))
  "Move forward a column."
  (forward-col o))

(defmethod backward-unit ((o table-viewer))
  "Move backward a column."
  (backward-col o))

(defmethod forward-multiple ((o table-viewer))
  (forward-row o (round (rows (table-viewer-renderer o)) 2)))

(defmethod backward-multiple ((o table-viewer))
  (backward-row o (round (rows (table-viewer-renderer o)) 2)))

(defmethod next-page ((o table-viewer))
  "Move to the next page."
  (forward-row o (rows (table-viewer-renderer o))))

(defmethod previous-page ((o table-viewer))
  "Move to the previous page."
  (backward-row o (rows (table-viewer-renderer o))))

(defmethod move-to-beginning ((o table-viewer))
  "Move to the first column."
  (with-slots ((point inator::point) renderer) o
    (with-slots (start columns) renderer
      (if (not (or point (table-point-row point)))
	  (move-to-top o)
	  (progn
	    (setf (table-point-col point) -1)
	    (forward-col o)
	    (when (< (table-point-col point) (table-point-col start))
	      (setf (table-point-col start) (table-point-col point))))))))

(defmethod move-to-end ((o table-viewer))
  "Move to the last column."
  (with-slots ((point inator::point) table renderer) o
    (with-slots (start last-displayed-col) renderer
      (when (not (or point (table-point-row point)))
	(move-to-top o))
      (setf (table-point-col point)
	    (olength (oelt table (table-point-row point))))
      (backward-col o)
      (when (> (table-point-col point) last-displayed-col)
	(setf (table-point-col start) (table-point-col point))))))

(defmethod move-to-top ((o table-viewer))
  "Move to the first row."
  (with-slots ((point inator::point)) o
    (setf (table-point-row point) 0)))

(defmethod move-to-bottom ((o table-viewer))
  "Move to the last row."
  (with-slots ((point inator::point) table) o
    (setf (table-point-row point) (1- (table-length o)))))

(defun scroll-left (o &optional (n 1))
  "Move the view left."
  (with-slots (renderer table) o
    (with-slots (start columns) renderer
      (loop :with i = 0
	    :while (and (< i n)
			(< (table-point-col start)
			   (1- (olength (table-columns table)))))
	    :do
	    (when (not (column-hidden (oelt columns (table-point-col start))))
	      (incf i))
	    (incf (table-point-col start))))))

(defun scroll-right (o &optional (n 1))
  "Move the view right."
  (with-slots (table renderer) o
    (with-slots (start columns) renderer
      (loop :with i = 0
	    :while (and (< i n)
			(> (table-point-col start) 0))
	    :do
	    (when (not (column-hidden (oelt columns (table-point-col start))))
	      (incf i))
	    (decf (table-point-col start))))))

(defgeneric scroll-down (table-viewer)
  (:documentation "Scroll the table back some lines.")
  (:method ((o table-viewer))
    (with-slots ((point inator::point) renderer cached-table-length) o
      (with-slots (start rows) renderer
	(incf (table-point-row start) 5)
	(when (>= (table-point-row start) (- (table-length o) rows))
	  (setf (table-point-row start) (- (table-length o) rows)))
	(clampf (table-point-row point) (table-point-row start)
		(+ (table-point-row start) rows))))))

(defgeneric scroll-up (table-viewer)
  (:documentation "Scroll the table forward some lines.")
  (:method ((o table-viewer))
    (with-slots ((point inator::point) renderer) o
      (with-slots (start rows) renderer
	(decf (table-point-row start) 5)
	(when (< (table-point-row start) 0)
	  (setf (table-point-row start) 0))
	(clampf (table-point-row point) (table-point-row start)
		(+ (table-point-row start) (1- rows)))))))

(defun ensure-point (o)
  "Make sure point is set to a specific row and column, which both default to 0."
  (with-slots ((point inator::point) table) o
    (when (not (table-point-row point))
      (setf (table-point-row point) 0))
    (when (not (table-point-col point))
      (setf (table-point-col point) 0))))

(defun search-table (o string &optional (direction :forward))
  "Search the table starting at point for the string and return the point
at which it's found or NIL if it's not found."
  (declare (ignore direction)) ; @@@
  (with-slots ((point inator::point) table) o
    (ensure-point o)
    (let* ((result (make-table-point :col 0
				     :row
				     (min (1+ (table-point-row point))
					  (1- (table-length o)))))
	   (sub-table (table-subseq table (table-point-row result))))
      (omapn
       (lambda (row)
	 (setf (table-point-col result) 0)
	 (omapn
	  (lambda (col)
	    (when (search string
			  (typecase col
			    (string col)
			    (t (princ-to-string col))))
	      (return-from search-table result))
	    (incf (table-point-col result)))
	  row)
	 (incf (table-point-row result)))
       sub-table)
      nil)))

(defun search-table-command (o &optional (direction :forward))
  "Search for a string."
  (with-slots ((point inator::point) search-string) o
    (tt-move-to (1- (tt-height)) 0) (tt-erase-to-eol)
    (tt-finish-output)
    (let* ((string (rl:rl :prompt
			  (format nil " Search for~@[ [~a]~]: " search-string)
			  :history-context :table-viewer-search))
	   (search (if (and search-string
			    (zerop (length string)))
		       search-string
		       string))
	   result)
      (tt-finish-output)
      (setf search-string search)
      (when search
	(if (setf result (search-table o search direction))
	    (progn
	      (setf (table-point-row point) (table-point-row result)
		    (table-point-col point) (table-point-col result))
	      ;; (message o "found at ~s" point)
	      )
	    (message o "~s not found" search))
	result))))

(defmethod search-command ((o table-viewer))
  "Search forward for a string."
  (search-table-command o :forward))

(defun search-backward-command (o)
  "Search backward for a string."
  (search-table-command o :backward))

(defun sort-table-in-direction (viewer direction)
  (let (string-op number-op)
    (ecase direction
      (:ascending  (setf string-op #'string-lessp    number-op #'<))
      (:descending (setf string-op #'string-greaterp number-op #'>)))
    (with-slots ((point inator::point) table last-sort-direction) viewer
      (when (table-point-col point)
	(let ((col (table-point-col point)))
	  (setf table
		(let ((t1 (column-type (oelt (table-columns table) col))))
		  (cond
		    ((subtypep t1 'string)
		     (osort table string-op :key (_ (oelt _ col))))
		    ((subtypep t1 'number)
		     (osort table number-op :key (_ (oelt _ col))))
		    (t
		     ;; sort the printed version like a string
		     (osort table string-op
			    :key (_ (princ-to-string (oelt _ col)))))))
		last-sort-direction direction))))))

(defmethod sort-command ((o table-viewer))
  "Sort the rows."
  (with-slots (last-sort-direction) o
    (case last-sort-direction
      (:ascending (sort-table-in-direction o :descending))
      (:descending (sort-table-in-direction o :ascending))
      (otherwise
       (sort-table-in-direction o :ascending)))))

(defmethod sort-descending-command ((o table-viewer))
  "Sort the rows in descending order."
  (sort-table-in-direction o :descending))

(defmethod sort-multiple-command ((o table-viewer))
  "Sort by multiple columns."
  (declare (ignore o))
  ;; @@@
  )

;; (defmethod jump-command ((o table-viewer))
;;   )

(defgeneric table-viewer-recalculate-sizes (viewer)
  (:documentation "Recalculate the column sizes for the table."))

(defmethod table-viewer-recalculate-sizes ((o table-viewer))
  "Recalculate the column sizes for the table."
  (with-slots (table renderer) o
    (unless (incremental-sizes-p renderer)
      (setf (sizes renderer) nil))
    (table-output-sizes renderer table)))

(defun resize-viewer (viewer)
  "Resize the viewr from the current terminal."
  (with-slots (width height start rows current-position cursor sizes)
      (table-viewer-renderer viewer)
    (setf width (tt-width)
	  height (tt-height)
	  ;; rows (min (olength (container-data (table-viewer-table viewer)))
	  rows (min (table-length viewer) (- (tt-height) 2)))
    (when start
      (clampf (table-point-row start) 0 (1- rows)))
    (when current-position
      (clampf (table-point-row current-position) 0 (1- rows)))
    (when cursor
      (when (table-point-row cursor)
	(clampf (table-point-row cursor) 0 (1- rows)))
      (when (table-point-col cursor)
	(clampf (table-point-col cursor) 0 (1- (olength sizes)))))))

(defmethod resize ((o table-viewer))
  (resize-viewer o)
  (call-next-method))

(defun table-info-table (table)
  (make-table-from
   (mapcar (_ (list (if (listp (column-name _))
			(first (column-name _))
			(column-name _))
		    (string-capitalize (column-type _))
		    (if (listp (column-name _))
			(string-capitalize (or (second (column-name _)) "Left"))
			"Left")
		    (column-width _)))
	   (table-columns table))
   :column-names '("Name" "Type" "Just" "Width")))

(defun table-info (o)
  (with-slots (table) o
    (display-text
     "Table Info"
     (append
      (osplit #\newline
	      (make-fat-string
	       :string
	       (process-ansi-colors
		(make-fatchar-string
		 (with-terminal-output-to-string () ;; :ansi
		   (table-print:print-table
		    (table-info-table table)
		    :renderer (make-instance
			       'terminal-table:terminal-table-renderer)
		    :stream *terminal*)))))
	      :omit-empty t)
      `("" ,(format nil "Rows: ~d Columns: ~d" (table-length o)
		 (or (and (table-columns table)
			  (olength (table-columns table)))
		     (and (oelt table 0) (olength (oelt table 0)))
		     0))))
     :justify nil)))

(defun record-info-table (table rec)
  (make-table-from
   (let ((i 0))
     (mapcar (_ (prog1 (list (if (listp (column-name _))
				 (first (column-name _))
				 (column-name _))
			     (princ-to-string (oelt rec i))
			     (princ-to-string (type-of (oelt rec i))))
		  (incf i)))
	   (table-columns table)))
   :column-names '("Name" "Value" "Actual Type")))

(defun record-info (o)
  (with-slots (table (point inator::point)) o
    ;; (dbugf :tv "table ~s row ~s~%" table
    ;; 	   (oelt table (table-point-row point)))
    (let ((outer-width (- (tt-width) 4)))
      (display-text
       "Record Info"
       (append
	(osplit #\newline
		(make-fat-string
		 :string
		 (process-ansi-colors
		  (make-fatchar-string
		   (with-terminal-output-to-string () ;; :ansi
		     (table-print:print-table
		      (record-info-table table
					 (oelt table (table-point-row point)))
		      :renderer (make-instance
				 'terminal-table:terminal-table-renderer)
		      :max-width outer-width
		      :stream *terminal*)))))
		:omit-empty t)
	`("" ,(format nil "Record ~d of ~d"
		      (1+ (table-point-row point)) (table-length o))))
       :justify nil))))

(defun set-column-hidden-state (table-viewer column state)
  "Set the visibility for ‘column’ in ‘table-viewer’ to the boolean ‘state’.
‘column’ can be a column number or column name."
  (with-slots (renderer table) table-viewer
    (with-slots (columns) renderer
      (setf column
	    (ctypecase column
	      (string
	       (oelt columns (table-column-number column table)))
	      (integer
	       (oelt columns column)))
	    (column-hidden column) state))))

(defun hide-column (o &key column)
  "Hide the column the cursor is in."
  (with-slots (renderer (point inator::point) table) o
    (with-slots (columns) renderer
      (when (not column)
	(setf column (table-point-col point)))
      (when (not column)
	;; We still don't know the column, so just retrun.
	(return-from hide-column nil))
      (set-column-hidden-state o column t)

      ;; Don't hide the last column.
      (if (oevery (_ (column-hidden _)) columns)
	  ;; Set it back
	  ;; (setf (column-hidden column) nil)
	  (set-column-hidden-state o column nil)
	  ;; Adjust the point so it's in visible column.
	  (cond
	    ((< (table-point-col point)
		(1- (fake-o-row-len table (table-point-row point))))
	     (forward-col o)
	     (when (column-hidden (oelt columns (table-point-col point)))
	       (backward-col o)))
	    ((plusp (table-point-col point))
	     (backward-col o)
	     (when (column-hidden (oelt columns (table-point-col point)))
	       (forward-col o))))))))

(defun show-all-columns (o)
  "Show all the hidden columns."
  (with-slots (renderer) o
    (with-slots (columns) renderer
      (omapn (_ (setf (column-hidden _) nil)) columns))))

(defun edit-cell (o)
  "Edit the current cell."
  ;; (with-slots ((point inator::point) table) o
  ;;   (ensure-point o)
  ;;   (rl:
  (declare (ignore o))
  )

(defmethod cell-at (o position)
  (with-slots (table) o
    (oaref table
	   (table-point-row position)
	   (table-point-col position))))

(defgeneric current-cell (viewer)
  (:documentation "Return the value in the current cell of VIEWER."))

(defmethod current-cell ((viewer table-viewer))
  (with-slots (table renderer) viewer
    (with-slots (current-position) renderer
      (oaref table
	     (table-point-row current-position)
	     (table-point-col current-position)))))

(defmethod (setf current-cell) (value (viewer table-viewer))
  (with-slots (table renderer) viewer
    (with-slots (current-position) renderer
      (setf (oaref table
		   (table-point-row current-position)
		   (table-point-col current-position))
	    value))))

(defgeneric view-cell (inator)
  (:documentation "View the current cell."))

(defmethod view-cell ((o table-viewer))
  (with-simple-restart (flarb "Back to the table view.")
    (handler-case
	(view (current-cell o))
      (error (c)
	(if (fui:popup-y-or-n-p
	     (span-to-fat-string
	      `((:red "Error: ") ,(apply #'format nil "~a" (list c))
		#\newline #\newline "Enter the debugger?"))
	     :default #\N)
	    (invoke-debugger c)
	    ;; (continue)
	    (invoke-restart (find-restart 'flarb)))))))

(defun add-row (o)
  "Add a row to the table."
  (declare (ignore o))
  )

(defmethod message ((o table-viewer) format-string &rest args)
  (with-slots (message) o
    (setf message (apply #'format nil format-string args))
    (when (next-method-p)
      (call-next-method))))

(defmethod update-display ((o table-viewer))
  (with-slots (table renderer (point inator::point) message long-titles) o
    (with-slots (x y start rows cursor selection last-displayed-col) renderer
      (tt-home)
      (tt-erase-below)

      ;; Adjust the view to the point
      (when point
	(when (>= (table-point-row point) (+ (table-point-row start) rows))
	  (setf (table-point-row start) (1+ (- (table-point-row point) rows))))
	(when (< (table-point-row point) (table-point-row start))
	  (setf (table-point-row start) (table-point-row point)))

	;; Unset the cursor
	(setf (table-point-row cursor) nil))

      ;; Tell the renderer where point is.
      (setf (current-position renderer) point)

      ;; Output the table rows
      ;; (dbugf :tv "Before output table ~s~%" long-titles)
      (output-table table renderer *terminal* :long-titles long-titles)

      ;; Show the message temporarily.
      (when message
	(message o (quote-format message))
	(setf message nil))

      ;; Make the cursor show up in the right spot.
      (when (table-point-row cursor) ;; @@@ XXX workaround
	;;(tt-move-to (table-point-row cursor) 0)
	(tt-move-to (+ y (table-point-row cursor)) x)))))

(defmethod finish-inator ((o table-viewer))
  ;; Redraw without the current row highlighted.
  (let ((saved-point (inator-point o)))
    (unwind-protect
	 (progn
	   (setf (inator-point o) nil)
	   (update-display o))
      (setf (inator-point o) saved-point)))
  (call-next-method))

(defun view-table (table &key long-titles (type 'table-viewer))
  "View a table."
  (with-terminal-inator (*table-viewer* type
			 :table table
			 ;; :point (make-table-point)
			 :long-titles long-titles)
    (let ((renderer (table-viewer-renderer *table-viewer*))
	  (*long-titles* long-titles))
      (resize-viewer *table-viewer*)
      (when (>= (olength table) *incremental-view-threshold*)
	(setf (incremental-sizes-p renderer) t))
      ;; Calculate the sizes of the table for side effect.
      (table-output-sizes renderer table)
      (event-loop *table-viewer*)
      (tt-move-to (1- (tt-height)) 0)
      (table-viewer-selection renderer))))

;; EOF
