;;
;; terminal-table.lisp - Table renderer for terminals.
;;

(defpackage :terminal-table
  (:documentation "Table renderer for terminals.")
  (:use :cl :dlib :terminal :table :table-print :fatchar :fatchar-io
	:collections :char-util :dlib-misc)
  (:export
   #:terminal-table-renderer
   ))
(in-package :terminal-table)

(defclass terminal-table-renderer (text-table-renderer)
  ()
  (:documentation "Render a table to a terminal."))

;; @@@ This whole thing is crap becuase it copy & pastes most of the
;; text-table-renderer methods. We should make it not do that, and just
;; call to them appropriately.

(defmethod table-output-column-titles ((renderer terminal-table-renderer)
				       table titles &key sizes)
  "Output all the column titles."
  (declare (ignore table))
  (with-accessors ((separator text-table-renderer-separator)
		   (stream text-table-renderer-stream)
		   (cursor text-table-renderer-cursor)) renderer
    (let ((has-underline (terminal-has-attribute stream :underline)))
      (setf cursor 0)
      (loop :with str
	 :and fmt = "~va"
	 :and len = (length titles)
	 :and sep-len = (display-length separator)
	 :and size :and just :and out-str
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
	 (when has-underline
	   (terminal-underline stream t))
	 (setf out-str (subseq (string-capitalize
				(substitute #\space #\_ str))
			       0 (min (length str) size)))
	 (terminal-format stream fmt size out-str)
	 (incf cursor size)
	 (when has-underline
	   (terminal-underline stream nil))
	 (when (< i (1- len))
	   (terminal-write-string stream separator)
	   (incf cursor sep-len)))
      (when (or (not *max-width*) (/= cursor *max-width*))
	;;(dbugf :termtab "Fuckler--- ~s ~s~%" cursor *max-width*)
	(terminal-write-char stream #\newline))

      ;; Lines
      (when (not has-underline)
	(setf cursor 0)
	(loop :with len = (length sizes)
	   :for i :from 0 :below len
	   :do
	   (terminal-format stream "~v,,,va" (car (aref sizes i)) #\- #\-)
	   (incf cursor (aref sizes i))
	   (when (< i (1- len))
	     (terminal-write-string stream separator)))
	(when (or (not *max-width*) (/= cursor *max-width*))
	  (terminal-write-char stream #\newline))))))

(defmethod text-table-adjust-sizes (table (renderer terminal-table-renderer)
				    sizes max-width)
  "Adjust the sizes in the vector SIZES, by the data in TABLE, limited
to MAX-WIDTH.."
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
				 ((or string fat-string)
				  (display-length field))
				 (otherwise
				  (display-length
				   (princ-to-string field)))))))
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
     table)))

(defmethod text-table-cell-lines (table (renderer terminal-table-renderer)
				  cell width)
  "Return the lines of CELL fitting in WIDTH."
  (osplit #\newline (with-output-to-fat-string (str)
		      (justify-text cell :cols width :stream str))))

(defmethod table-output-cell ((renderer terminal-table-renderer)
			      table cell width justification row column)
  "Output a table cell."
  ;;(declare (ignore row))
  (with-slots ((cursor table-print::cursor)) renderer
    (let* ((op (typecase cell
		 ((or string fat-string) "/fatchar-io:print-string/")
		 (t "a")))
	   (fmt (cond
		  ((and (= column (1- (olength (table-columns table))))
			(not *trailing-spaces*))
		   (s+ "~*~" op))
		  ((eql justification :right)
		   (s+ "~v@" op))
		  ((and (not justification) (typep cell 'number))
		   (s+ "~v@" op))
		  (t
		   (s+ "~v" op))))
	   (field (let ((*print-pretty* nil))
		    (with-output-to-fat-string (str)
		      (format str fmt width cell))))
	   (len (display-length field)))
      (if (and (eq justification :overflow)
	       (> len width))
	  (progn
	    (write field :stream *destination* :escape nil :readably nil
		   :pretty nil)
	    (format *destination* "~%~v,,,va" width #\space #\space)
	    (setf cursor width))
	  (typecase cell
	    (standard-object
	     (dbugf :termtab "POOTY ~s ~s ~s~%" row column (type-of cell))
	     (princ (osubseq field 0 (min width (olength field)))
		    *destination*)
	     (incf cursor (min width (olength field))))
	    (t
	     (write (osubseq field 0 (min width (olength field)))
		    :stream *destination* :escape nil :readably nil :pretty nil)
	     (incf cursor (min width (olength field)))))))))

;; EOF
