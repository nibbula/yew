;;
;; table-viewer.lisp - View tables.
;;

(defpackage :table-viewer
  (:documentation "View tables.")
  (:use :cl :dlib :collections :table :table-print :keymap :inator :terminal
	:terminal-inator :dtt :char-util :fui :fatchar :fatchar-io :grout
	:terminal-table :ostring :view-generic)
  (:export
   #:viewer-table-renderer
   #:table-viewer
   #:view-table-file
   #:view-table-thing
   #:view-table
   #:!view-table
   #:!print-table
   ))
(in-package :table-viewer)

(declaim (optimize (speed 0) (safety 0) (debug 3) (space 0)))

(defkeymap *table-viewer-keymap*
  `((#\q		. quit)
    (#\?		. help)
    (#\<		. move-to-top)
    (#\>		. move-to-bottom)
    (#\i		. record-info)
    (:home		. move-to-top)
    (:end		. move-to-bottom)
    (:up		. previous)
    (:down		. next)
    (:right		. scroll-right)
    (:left		. scroll-left)
    ;;(,(meta-char #\i)	. table-info)
    (#\escape		. *table-viewer-escape-keymap*)
    ))

(defkeymap *table-viewer-escape-keymap*
  `((,(ctrl #\s)	. sort-descending-command)
    (#\i		. table-info)))

(add-keymap *default-inator-escape-keymap* *table-viewer-escape-keymap*)

(defstruct table-point
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
    :documentation "Temporary message."))
  (:default-initargs
   :point (make-table-point :row 0)
   :keymap `(,*table-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "View a table."))

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

(defmethod table-output-sizes ((renderer viewer-table-renderer) table)
  ;; Cache the sizes so we don't have to recompute them every time.
  ;; Then use the default method.
  (declare (ignorable table))
  (or (sizes renderer)
      (setf (sizes renderer) (call-next-method))))

(defun in-view (renderer #| &optional row-in |#)
  ;; (with-slots (start rows) *table-viewer*
  ;;   (let ((row (or row-in (output-y renderer))))
  ;;     (and (>= row (table-point-row start))
  ;; 	   (< row (+ (table-point-row start) rows))
  ;; 	   (< (output-x renderer) (1- *max-width*))))))
  (with-slots (width) renderer
    (< (output-x renderer) (1- width))))

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
  (with-slots (start last-displayed-col separator output-x output-y x y width)
      renderer
    (let ((has-underline (tt-has-attribute :underline)))
      ;;(tt-move-to-col 2)
      (tt-move-to y (+ x 2))
      (setf output-x 2)
      (loop
	 :with col-num = (table-point-col start)
	 :and max-col  = (length (table-columns table))
	 :and sep-len  = (display-length separator)
	 :and cell-width
	 :and clipped-width
	 :and name
	 :and justification
	 :while (and (< col-num max-col)
		     (< output-x (- *max-width* 1 sep-len)))
	 :do
	 ;; Write a separator if we have room and we're not at the first col
	   (when (and (< (+ output-x sep-len) *max-width*)
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

	   (setf last-displayed-col col-num)
	   (incf col-num))
      (when (<= output-x *max-width*)
	;; (tt-write-char #\newline)
	(tt-move-to (+ y (1+ output-y)) x)
	)
      (incf output-y))))

(defmethod table-output-cell ((renderer viewer-table-renderer)
			      table cell width justification row column)
  (declare (ignore width justification))
  "Output a table cell."
  (with-slots (start rows cursor selection last-displayed-col current-position
	       output-x output-y sizes x) renderer
    ;; (dbugf :tv "Howdy ~s ~s~%" cell start)
    (when (>= column (table-point-col start))
      ;; Move to the start of the row, for the first column.
      (when (or (zerop column)
		(and (table-point-col start)
		     (= column (table-point-col start))))
	;; (tt-move-to-col 2)
	(tt-move-to-col (+ x 2))
	(setf output-x 2))

      ;; (dbugf :tv "well now ~s ~s ~s~%" sizes column cursor)
      (let* ((cell-width (aref sizes column))
	     (clipped-width (max 0 (min (- *max-width* output-x)
					cell-width)))
	     (hilite (= row (table-point-row current-position))))

	(when hilite
	  (tt-standout t)
	  (when (not (table-point-row cursor))
	    (setf (table-point-row cursor) output-y))
	  (if (and (table-point-col current-position)
		   (= column (table-point-col current-position)))
	      (progn
		(setf (table-point-col cursor) output-x)
		(tt-color :yellow :default)) ; @@@ should get from theme
	      (tt-color :default :default)))

	;; (dbugf :tv "is it? ~s~%" (table-columns table))
	(tv-output-cell renderer table cell clipped-width
			nil ;; bogus justification
			row column
			(column-format (oelt (table-columns table) column)))
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
(defun table-subseq (table start end)
  "Return an object made from TABLE that we can do SUBSEQ on."
  (typecase (container-data table)
    ((or list array) (osubseq table start end))
    ;; @@@ Maybe this could be keyed-collection-p ?
    ((or structure-object hash-table)
     (let ((i 0) result)
       (block nil
	 ;; @@@ Should we make a collection function to iterate a range of
	 ;; a keyed collection? Or should we make a subseq that works on
	 ;; these keyed things? This is sub-good.
	 (omapk (_ (cond
		     ((= i end) (return nil))
		     ((>= i start) (push _ result))))
		(container-data table)))
       result))))

(defmethod output-table (table (renderer viewer-table-renderer) destination
			 &key long-titles print-titles max-width
			   &allow-other-keys)
  "Output a table."
  (declare (ignore print-titles max-width))
  (with-slots (output-x output-y separator width x y start rows
	       last-displayed-col) renderer
    (let* ((*long-titles* long-titles)
	   (row-num (table-point-row start))
	   (col-num (table-point-col start))
	   (sizes (table-output-sizes renderer table))
	   (sep-len (display-length separator))
	   ;; @@@ Hopefully this is not too slow. We should probably cache
	   ;; it if the view start and size hasn't changed.
	   (sub-table (table-subseq table row-num (+ row-num rows)))
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
	       col-num 0)
	 (omapn
	  (lambda (cell)
	    ;; Output the column separator
	    (when (not (zerop col-num))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inator methods

;; Navigation
(defun forward-row (o &optional (n 1))
  (with-slots ((point inator::point) table) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) 0
	     (table-point-col point) 0))
      ((table-point-row point)
       (if (< (+ (table-point-row point) n) (olength table))
	   (incf (table-point-row point) n)
	   (setf (table-point-row point) (1- (olength table))))))))

(defun backward-row (o &optional (n 1))
  (with-slots ((point inator::point) table) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) (1- (olength table))))
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
    (with-slots (start last-displayed-col) renderer
      (cond
	;;((or (not (table-point-row point)) (not (table-point-col point)))
	((not (table-point-col point))
	 (setf (table-point-col point) 0))
	((and (table-point-row point) (table-point-col point))
	 (when (< (+ (table-point-col point) n)
		  (fake-o-row-len table (table-point-row point)))
	   (incf (table-point-col point) n)
	   (when (> (table-point-col point) last-displayed-col)
	     (setf (table-point-col start) (table-point-col point)))))))))

(defun backward-col (o &optional (n 1))
  (with-slots ((point inator::point) table renderer) o
    (with-slots (start) renderer
      (cond
	;;((or (not (table-point-row point)) (not (table-point-col point)))
	((not (table-point-col point))
	 (setf (table-point-col point)
	       (1- (fake-o-row-len table (table-point-row point)))))
	((and (table-point-row point) (table-point-col point))
	 (when (>= (- (table-point-col point) n) 0)
	   (decf (table-point-col point) n)
	   (when (< (table-point-col point) (table-point-col start))
	     (setf (table-point-col start) (table-point-col point)))))))))

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
    (with-slots (start) renderer
      (if (not (or point (table-point-row point)))
	  (move-to-top o)
	  (progn
	    (setf (table-point-col point) 0)
	    (when (< (table-point-col point) (table-point-col start))
	      (setf (table-point-col start) (table-point-col point))))))))

(defmethod move-to-end ((o table-viewer))
  "Move to the last column."
  (with-slots ((point inator::point) table renderer) o
    (with-slots (start last-displayed-col) renderer
      (when (not (or point (table-point-row point)))
	(move-to-top o))
      (setf (table-point-col point)
	    (1- (olength (oelt table (table-point-row point)))))
      (when (> (table-point-col point) last-displayed-col)
	(setf (table-point-col start) (table-point-col point))))))

(defmethod move-to-top ((o table-viewer))
  "Move to the first row."
  (with-slots ((point inator::point)) o
    (setf (table-point-row point) 0
	  ;;(table-point-col point) 0
	  )))

(defmethod move-to-bottom ((o table-viewer))
  "Move to the last row."
  (with-slots ((point inator::point) table) o
    (setf (table-point-row point) (1- (olength table))
	  ;; (table-point-col point) 0
	  )))

(defun scroll-left (o &optional (n 1))
  "Move the view left."
  (with-slots (renderer table) o
    (with-slots (start) renderer
      (when (< (+ (table-point-col start) n) (length (table-columns table)))
	(incf (table-point-col start) n)))))

(defun scroll-right (o &optional (n 1))
  "Move the view right."
  (with-slots (table renderer) o
    (with-slots (start) renderer
      (when (>= (- (table-point-col start) n) 0)
	(decf (table-point-col start) n)))))

;; (defmethod search-command ((o table-viewer))
;;   )

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

;; (defmethod jump-command ((o table-viewer))
;;   )

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
		 (with-terminal-output-to-string (:ansi)
		   (table-print:print-table
		    (table-info-table table)
		    :renderer (make-instance
			       'terminal-table:terminal-table-renderer)
		    :stream *terminal*)))))
	      :omit-empty t)
      `("" ,(format nil "Rows: ~d Columns: ~d" (olength table)
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
		   (with-terminal-output-to-string (:ansi)
		     (table-print:print-table
		      (record-info-table table
					 (oelt table (table-point-row point)))
		      :renderer (make-instance
				 'terminal-table:terminal-table-renderer)
		      :max-width outer-width
		      :stream *terminal*)))))
		:omit-empty t)
	`("" ,(format nil "Record ~d of ~d"
		      (1+ (table-point-row point)) (olength table))))
       :justify nil))))

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
      (when (>= (table-point-row point) (+ (table-point-row start) rows))
	(setf (table-point-row start) (1+ (- (table-point-row point) rows))))
      (when (< (table-point-row point) (table-point-row start))
	(setf (table-point-row start) (table-point-row point)))

      ;; Unset the cursor
      (setf (table-point-row cursor) nil)

      ;; Tell the renderer where point is.
      (setf (current-position renderer) point)

      ;; Output the table rows
      ;; (dbugf :tv "Before output table ~s~%" long-titles)
      (output-table table renderer *terminal* :long-titles long-titles)

      ;; Show the message temporarily.
      (when message
	(message o message)
	(setf message nil))

      ;; Make the cursor show up in the right spot.
      (when (table-point-row cursor) ;; @@@ XXX workaround
	;;(tt-move-to (table-point-row cursor) 0)
	(tt-move-to (+ y (table-point-row cursor)) x)
	)
      )))

;; (defgeneric start-inator ((o table-viewer))
;;   )

;; (defgeneric finish-inator ((o table-viewer))
;;   )

;; @@@@@@@
;; (defgeneric table-viewer-for (

(defun view-table (table &key long-titles)
  "View a table."
  (with-terminal ()
    (let* ((*table-viewer*
	    (make-instance 'table-viewer
			   :table table
			   ;; :point (make-table-point)
			   :long-titles long-titles
			   ))
	   (renderer (table-viewer-renderer *table-viewer*))
	   (*long-titles* long-titles))
      (setf (rows renderer)
	    (min (olength (container-data table)) (- (tt-height) 2)))
      ;; Calculate the sizes of the whole table for side effect.
      (table-output-sizes renderer table)
      (event-loop *table-viewer*)
      (tt-move-to (1- (tt-height)) 0)
      (table-viewer-selection renderer))))

(defmacro with-coerced-table ((var thing &key column-names) &body body)
  "Evalute BODY with VAR bound to THING coerced into a table. Don't do anything
if THING or lish:*input* NIL."
  (with-unique-names (thunk)
    `(let ((,var (or ,thing lish:*input*)))
       (flet ((,thunk () ,@body))
	 (when ,var
	   (typecase ,var
	     (table)
	     ((or string pathname stream)
	      (setf ,var (read-table ,var :column-names ,column-names)))
	     ((or list array hash-table structure-object)
	      (setf ,var (make-table-from ,var :column-names ,column-names)))
	     (t
	      ;; @@@ check with find-method?
	      (setf ,var (make-table-from ,var :column-names ,column-names))))
	   (,thunk))))))

#+lish
(lish:defcommand print-table
  ((long-titles boolean :short-arg #\l
    :help "True to show full column titles.")
   (column-names list :short-arg #\c
    :help "List of column titles for the table.")
   (table object :help "Table to print."))
  :accepts '(table sequence hash-table structure-object)
  "Print a table to the terminal."
  (with-coerced-table (tab table :column-names column-names)
    (with-grout ()
      (grout-print-table tab :long-titles long-titles))))

(defun view-table-file (file-name)
  "View the contents of FILE-NAME as a table."
  (view-table (read-table file-name)))

(defun view-table-thing (thing &key long-titles)
  "View the THING as a table."
  (with-coerced-table (tab thing)
    (view-table tab :long-titles long-titles)))

(defmethod view ((thing table))
  (view-table thing))

#+lish
(lish:defcommand view-table
  ((long-titles boolean :short-arg #\l :default nil
    :help "True to show full column titles.")
   (table object :optional t :help "Table to view."))
  :accepts '(table sequence hash-table structure-object)
  "View a table. Pass or pipe it either a table:table object, or something which
has a table:make-table-from method, which by default are lists, hash-tables,
arrays, and structures. If it's a string or pathname, try to read a table from
the file."
  :accepts '(table sequence hash-table structure-object)
  (view-table-thing table :long-titles long-titles))

;; EOF
