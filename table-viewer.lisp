;;
;; table-viewer.lisp - View tables.
;;

(defpackage :table-viewer
  (:documentation "View tables.")
  (:use :cl :collections :table :table-print :keymap :inator :terminal
	:terminal-inator)
  (:export
   #:view-table
   ))
(in-package :table-viewer)

(defkeymap *table-viewer-keymap*
  `((#\q		. quit)
    (#\?		. help)
    (#\<		. move-to-top)
    (#\>		. move-to-bottom)
    (:up		. previous)
    (:down		. next)
    (:right		. scroll-right)
    (:left		. scroll-left)
    ))

(defparameter *table-viewer-escape-keymap*
  (build-escape-map *table-viewer-keymap*))

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
   (start
    :initarg :start :accessor table-viewer-start
    :documentation "Point to view from.")
   (rows
    :initarg :rows :accessor table-viewer-rows :type integer
    :documentation "Number of rows in the view.")
   (selection
    :initarg :selection :accessor table-viewer-selection :initform nil
    :type table-selection
    :documentation "Part of the table selected.")
   (cursor
    :initarg :cursor :accessor table-viewer-cursor
    :documentation "Where the cursor is.")
   (renderer
    :initarg :renderer :accessor table-viewer-renderer
    :initform (make-instance 'viewer-table-renderer) :type table-renderer
    :documentation "The table renderer."))
  (:default-initargs
   :start (make-table-point :row 0 :col 0)
   :point (make-table-point :row 0)
   :cursor (make-table-point)
   :keymap `(,*table-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "View a table."))

;; Customized table renderer

(defclass viewer-table-renderer (table-renderer)
  ((output-column
    :initarg :output-column :accessor output-column
    :documentation "Column being output.")
   (output-line
    :initarg :output-line :accessor output-line
    :documentation "Line being output."))
  (:documentation "An interactive table renderer."))

(defmethod table-output-header ((renderer viewer-table-renderer)
				table &key width sizes)
  (declare (ignore table width sizes))
  (setf (output-column renderer) 0
	(output-line renderer) 0))

(defmethod table-output-start-row ((renderer viewer-table-renderer) table)
  (declare (ignore table))
  (setf (output-column renderer) 0))

(defun in-view (renderer &optional row-in)
  (with-slots (start rows) *table-viewer*
    (let ((row (or row-in (output-line renderer))))
      (and (>= row (table-point-row start))
	   (< row (+ (table-point-row start) rows))
	   (< (output-column renderer) (1- *max-width*))))))

(defmethod table-output-column-separator ((renderer viewer-table-renderer)
					  table &key width)
  (declare (ignore table width))
  (when (in-view renderer)
    (incf (output-column renderer))
    (tt-write-char #\space)))

(defmethod table-output-end-row ((renderer viewer-table-renderer) table n)
  (declare (ignore table n))
  (when (in-view renderer)
    (tt-write-char #\newline))
  (setf (output-column renderer) 0)
  (incf (output-line renderer)))

(defmethod table-output-row-separator ((renderer viewer-table-renderer)
				       table n &key width sizes)
  (declare (ignore renderer table n width sizes))
  ;; (tt-write-char #\newline)
  )

(defmethod table-output-column-title ((renderer viewer-table-renderer)
				      table title width justification column)
  ;;(declare (ignore table))
  (when (< (output-column renderer) *max-width*)
    (if (zerop column)
	(progn
	  (tt-move-to-col 2)
	  (setf (output-column renderer) 3))
	(progn
	  (tt-write-char #\space)
	  (incf (output-column renderer))))

    (let* ((clipped-width (min (- *max-width* (output-column renderer))
			       width))
	   (str (if (consp title) (car title) title))
	   (len (min (olength str) clipped-width)))
      (tt-underline t)
      (tt-format (if (eq justification :right) "~v@a" "~va")
		 ;;width
		 clipped-width
		 (osubseq str 0 len))
      (tt-underline nil)
      (incf (output-column renderer) clipped-width)))

  (when (and (= column (1- (olength (table-columns table))))
	     (<= (output-column renderer) *max-width*)
	     )
    (tt-write-char #\newline)))

;; (defmethod table-output-cell-display-width ((renderer viewer-table-renderer)
;; 					    table cell column)
;;   (declare (ignore renderer table))
;;   (case column
;;     (0 0)
;;     (1 5)
;;     (2 (display-length (dlib-misc:date-string :format :relative
;; 					      :time cell)))
;;     (3 (display-length cell))))

(defmethod table-output-cell ((renderer viewer-table-renderer)
			      table cell width justification row column)
  "Output a table cell."
  (declare (ignore table))
  (with-slots ((point inator::point) start rows cursor) *table-viewer*
    (when (and (>= row (table-point-row start))
	       (< row (+ (table-point-row start) rows))
	       (>= column (table-point-col start)))
      (when (zerop column)
	(tt-move-to-col 2))
      (let* ((*print-pretty* nil)
	     (clipped-width (max 0 (min (- *max-width*
					   (1+ (output-column renderer)))
					width)))
	     (string (princ-to-string cell))
	     (len (min clipped-width (olength string)))
	     (hilite (= row (table-point-row point))))
	(when hilite
	  (tt-standout t)
	  ;; (tt-bold t)
	  (multiple-value-bind (r c)
	      (terminal-get-cursor-position *terminal*)
	    (setf (table-point-row cursor) r)
	    (if (and (table-point-col point)
		     (= column (table-point-col point)))
		(progn
		  (setf (table-point-row cursor) c)
		  (tt-color :yellow :default))
		(tt-color :default :default))))
	(tt-format (if (eq justification :right) "~v@a" "~va")
		   ;; width
		   clipped-width
		   ;;(subseq string 0 (min width (length string)))
		   (osubseq string 0 len))
	(incf (output-column renderer) clipped-width)
	(tt-normal)))))

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

(defun forward-col (o &optional (n 1))
  (with-slots ((point inator::point) table) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) 0
	     (table-point-col point) 0))
      ((and (table-point-row point) (table-point-col point))
       (when (< (+ (table-point-col point) n)
		(olength (oelt table (table-point-row point))))
	 (incf (table-point-col point) n))))))

(defun backward-col (o &optional (n 1))
  (with-slots ((point inator::point) table) o
    (cond
      ((not (table-point-row point))
       (setf (table-point-row point) 0
	     (table-point-col point)
	     (1- (olength (oelt table (table-point-row point))))))
      ((and (table-point-row point) (table-point-col point))
       (when (>= (- (table-point-col point) n) 0)
	 (decf (table-point-col point) n))))))

(defmethod forward-unit ((o table-viewer))
  "Move forward a column."
  (forward-col o))

(defmethod backward-unit ((o table-viewer))
  "Move backward a column."
  (backward-col o))

(defmethod forward-multiple ((o table-viewer))
  (forward-row o (round (table-viewer-rows o) 2)))

(defmethod backward-multiple ((o table-viewer))
  (backward-row o (round (table-viewer-rows o) 2)))

(defmethod next-page ((o table-viewer))
  "Move to the next page."
  (forward-row o (table-viewer-rows o)))

(defmethod previous-page ((o table-viewer))
  "Move to the previous page."
  (backward-row o (table-viewer-rows o)))

(defmethod move-to-beginning ((o table-viewer))
  "Move to the first column."
  (with-slots ((point inator::point)) o
    (if (not (or point (table-point-row point)))
	(move-to-top o)
	(setf (table-point-col point) 0))))

(defmethod move-to-end ((o table-viewer))
  "Move to the last column."
  (with-slots ((point inator::point) table) o
    (when (not (or point (table-point-row point)))
      (move-to-top o))
    (setf (table-point-col point)
	  (1- (olength (oelt table (table-point-row point)))))))

(defmethod move-to-top ((o table-viewer))
  "Move to the first row."
  (with-slots ((point inator::point)) o
    (setf (table-point-row point) 0
	  (table-point-col point) 0)))

(defmethod move-to-bottom ((o table-viewer))
  "Move to the last row."
  (with-slots ((point inator::point) table) o
    (setf (table-point-row point) (1- (olength table))
	  (table-point-col point) 0)))

(defun scroll-left (o &optional (n 1))
  "Move the view left."
  (with-slots (start table) o
    (when (< (+ (table-point-col start) n) (1- (length (table-columns table))))
      (incf (table-point-col start) n))))

(defun scroll-right (o &optional (n 1))
  "Move the view right."
  (with-slots (start table) o
    (when (>= (- (table-point-col start) n) 0)
      (decf (table-point-col start) n))))

;; (defmethod search-command ((o table-viewer))
;;   )

;; (defmethod sort-command ((o table-viewer))
;;   (with-slots ((point inator::point)) o
;;     (when (table-point-col point)
;;       (
;;    )

;; (defmethod jump-command ((o table-viewer))
;;   )

(defmethod update-display ((o table-viewer))
  (with-slots (table renderer (point inator::point) start rows cursor
	       selection) o
    (tt-home)
    (tt-erase-below)
    (when (>= (table-point-row point) (+ (table-point-row start) rows))
      (setf (table-point-row start) (1+ (- (table-point-row point) rows))))
    (when (< (table-point-row point) (table-point-row start))
      (setf (table-point-row start) (table-point-row point)))
    (output-table table renderer *terminal* :max-width (1- (tt-width)))
    (tt-move-to (table-point-row cursor) 0)))

;; (defgeneric start-inator ((o table-viewer))
;;   )

;; (defgeneric finish-inator ((o table-viewer))
;;   )

(defun view-table (table)
  "View a table."
  (with-terminal ()
    (let* ((*table-viewer*
	    (make-instance 'table-viewer
			   :table table
			   :rows (min (olength (container-data table))
				      (- (tt-height) 2))
			   ;; :point (make-table-point)
			   )))
      (event-loop *table-viewer*)
      (tt-move-to (1- (tt-height)) 0)
      (table-viewer-selection *table-viewer*))))

#+lish
(lish:defcommand view-table
  ((table object :optional t :help "Table to view."))
  "View a table."
  (let ((tab (or table lish:*input*)))
    (when tab
      (view-table tab))))

;; EOF
