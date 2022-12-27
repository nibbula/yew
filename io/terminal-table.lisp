;;;
;;; terminal-table.lisp - Table renderer for terminals.
;;;

(defpackage :terminal-table
  (:documentation "Table renderer for terminals.")
  (:use :cl :dlib :terminal :table :table-print :fatchar :fatchar-io
	:collections :char-util :dlib-misc)
  (:export
   #:terminal-table-renderer
   #:terminal-box-table-renderer
   ))
(in-package :terminal-table)

(defclass terminal-table-renderer (text-table-renderer)
  ((x
    :initarg :x :accessor terminal-table-renderer-x
    :initform nil :type (or null fixnum)
    :documentation "Horizontal coordinate to render at."))
  (:default-initargs
   :horizontal-line-char (code-char #x2500))
  (:documentation "Render a table to a terminal."))

;; @@@ This whole thing is crap becuase it copy & pastes most of the
;; text-table-renderer method. We should make it not do that, and just
;; call to them appropriately.

(defmethod table-output-column-titles ((renderer terminal-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  ;; (declare (ignore table))
  (with-accessors ((separator text-table-renderer-separator)
		   (cursor text-table-renderer-cursor)
		   (horizontal-line-char
		    text-table-renderer-horizontal-line-char)) renderer
    (let* ((stream *destination*)
	   (has-underline (terminal-has-attribute stream :underline)))
      (setf cursor 0)
      (loop :with str
	 :and fmt = "~va"
	 :and len = (length titles)
	 :and sep-len = (display-length separator)
	 :and size :and just :and out-str
	 :for col :in titles
	 :and i :from 0 :below (length sizes)
	 :do
	 ;; (setf size (car (aref sizes i))
	 ;;       just (cadr (aref sizes i)))
	 (assert (not (listp (aref sizes i))) ()
		 "Size shouldn't be a list anymore")
	 (setf size (aref sizes i)
	       just (column-align (oelt (table-columns table) i)))
	 (assert (not (listp col)) () "Column title shouldn't be a list anymore")
	 ;; (if (listp col)
	 ;;     (setf str (first col)
	 ;; 	   fmt (if (eql just :right) "~v@a" "~va"))
	 ;;     (setf str col
	 ;; 	   fmt "~va"))
	 (setf str col
	       fmt (if (eql just :right) "~v@a" "~va"))
	 (when has-underline
	   (terminal-underline stream t))
	 (setf out-str (subseq (string-capitalize
				(substitute #\space #\_ str))
			       0 (min (length str) size)))
	 (terminal-format stream fmt size out-str)
	 (incf cursor size)
	 (when has-underline
	   (terminal-underline stream nil))
	   (when (and (< i (1- len))
		      (or (not *max-width*) (< cursor *max-width*)))
	     (terminal-write-string stream separator)
	     (incf cursor sep-len)))
      ;; (when (or (not *max-width*) (< cursor (1- *max-width*)))
      (when (or (not *max-width*) (< cursor *max-width*))
	;;(dbugf :termtab "Fuckler--- ~s ~s~%" cursor *max-width*)
	(terminal-write-char stream #\newline))

      ;; Lines
      (when (not has-underline)
	(setf cursor 0)
	(loop :with len = (length sizes) :and size
	   :for i :from 0 :below len
	   :do
	   ;; (setf size (car (aref sizes i)))
	   (setf size (aref sizes i))
	   ;; (terminal-format stream "~v,,,va" size #\- #\-)
	   (terminal-format stream "~v,,,va" size horizontal-line-char
			    horizontal-line-char)
	   (incf cursor size)
	   (when (< i (1- len))
	     (terminal-write-string stream separator)))
	;; (when (or (not *max-width*) (< cursor (1- *max-width*)))
	(when (or (not *max-width*) (< cursor *max-width*))
	  (terminal-write-char stream #\newline))))))

(defmethod text-table-cell-lines (table (renderer terminal-table-renderer)
				  cell width)
  "Return the lines of CELL fitting in WIDTH."
  (declare (ignore table renderer))
  (osplit #\newline (with-output-to-fat-string (str)
		      (justify-text cell :cols width :stream str))))

;; @@@ to implement x
;; (defmethod table-output-header ((renderer terminal-table-renderer) table
;; 				&key width sizes)
;;   )
;; (defmethod table-output-column-titles ((renderer terminal-table-renderer)
;; 				       table titles &key sizes)
;;   )
;;
;; (defmethod table-output-start-row ((renderer terminal-table-renderer) table)
;;   (declare (ignore table))
;;   (with-slots (box-color x) renderer
;;     (terminal-move-to-col *destination* (or x 0))
;;     (terminal-color *destination* box-color nil)
;;     (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Box tables

(defclass terminal-box-table-renderer (text-box-table-renderer)
  ((box-color
    :initarg :box-color :accessor terminal-box-table-renderer-box-color
    :initform :default #| :type |#
    :documentation "Color for the box.")
   (title-color
    :initarg :title-color :accessor terminal-box-table-renderer-title-color
    :initform :default #| :type |#
    :documentation "Color for the column titles.")
   (alternate-row-bg
    :initarg :alternate-row-bg
    :accessor terminal-box-table-renderer-alternate-row-bg
    ;; :initform :default
    :initform nil
    :documentation "Background color for alternate rows.")
   (x
    :initarg :x :accessor terminal-box-table-renderer-x
    :initform nil :type (or null fixnum)
    :documentation "Horizontal coordinate to render at.")
   (y
    :initarg :y :accessor terminal-box-table-renderer-y
    :initform nil :type (or null fixnum)
    :documentation "Vertical coordinate to render at."))
  (:default-initargs
   ;;:horizontal-line-char (code-char #x2500)
  )
  (:documentation "Render a box table to a terminal."))

(defmethod write-box-line ((renderer terminal-box-table-renderer) style sizes)
  (declare (ignore style sizes))
  (with-slots (box-color x y) renderer
    (cond
      (y (terminal-move-to *destination* y (or x 0)))
      (x (terminal-move-to-col *destination* x)))
    (terminal-color *destination* box-color nil)
    (call-next-method)))

(defmethod table-output-cell ((renderer terminal-box-table-renderer)
			      table cell width justification row column)
  (declare (ignorable table cell width justification column))
  (with-slots (alternate-row-bg) renderer
    (terminal-color *destination*
		    :default ;; nil
		    (if (and row (zerop (mod row 2)))
			nil
			alternate-row-bg))
    (call-next-method)))

(defmethod table-output-start-row ((renderer terminal-box-table-renderer) table)
  (declare (ignore table))
  (with-slots (box-color x) renderer
    (terminal-move-to-col *destination* (or x 0))
    (terminal-color *destination* box-color nil)
    (call-next-method)))

(defmethod table-output-column-separator ((renderer terminal-box-table-renderer)
					  table &key width)
  "Output a separator between columns."
  (declare (ignore table width))
  (with-slots (box-color) renderer
    (terminal-color *destination* box-color nil)
    (call-next-method)
    ;;(write-string (box-style-separator box-style) *destination*)
    ))

(defmethod table-output-end-row ((renderer terminal-box-table-renderer) table n)
  (declare (ignore table n))
  (with-slots (box-color) renderer
    (terminal-color *destination* box-color nil)
    (call-next-method)))

(defmethod table-output-column-title ((renderer terminal-box-table-renderer)
				      table title width justification column)
  ;;(format *destination* "~va" width title)
  ;; (table-format-cell renderer table title nil column
  ;; 		     :width width :justification justification)
  (with-slots (title-color) renderer
    ;; (call-next-method)
    #|
    (table-output-cell renderer table
		       ;; (span-to-fat-string
		       ;; 	`(:fg :color ,title-color ,(oelt title 0)))
		       (oelt title 0)
		       width justification nil column)
    |#
    (let* ((*trailing-spaces* t)
	   (field (table-format-cell renderer table title nil column
				     :width width
				     :justification justification))
	   ;; (len (display-length field))
	   )
      (terminal-color *destination* title-color nil)
      (write (char-util:simplify-string
	      (osubseq field 0 (min width (olength field))))
	     :stream *destination* :escape nil :readably nil :pretty nil))))

;; EOF
