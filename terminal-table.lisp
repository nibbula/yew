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
	   (when (and (< i (1- len))
		      (or (not *max-width*) (< cursor *max-width*)))
	     (terminal-write-string stream separator)
	     (incf cursor sep-len)))
      (when (or (not *max-width*) (< cursor (1- *max-width*)))
	;;(dbugf :termtab "Fuckler--- ~s ~s~%" cursor *max-width*)
	(terminal-write-char stream #\newline))

      ;; Lines
      (when (not has-underline)
	(setf cursor 0)
	(loop :with len = (length sizes) :and size
	   :for i :from 0 :below len
	   :do
	   (setf size (car (aref sizes i)))
	   (terminal-format stream "~v,,,va" size #\- #\-)
	   (incf cursor size)
	   (when (< i (1- len))
	     (terminal-write-string stream separator)))
	(when (or (not *max-width*) (< cursor (1- *max-width*)))
	  (terminal-write-char stream #\newline))))))

(defmethod text-table-cell-lines (table (renderer terminal-table-renderer)
				  cell width)
  "Return the lines of CELL fitting in WIDTH."
  (declare (ignore table renderer))
  (osplit #\newline (with-output-to-fat-string (str)
		      (justify-text cell :cols width :stream str))))

;; EOF
