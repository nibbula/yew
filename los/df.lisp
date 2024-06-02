;;;
;;; df.lisp - Show how much disk is free.
;;;

(defpackage :df
  (:documentation "Show how much disk is free.")
  (:use :cl :opsys :dlib :dlib-misc :table :grout :fatchar :fatchar-io
	:theme)
  (:export
   #:df
   #:!df
   #:bogus-filesystem-p
   ))
(in-package :df)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; Custom short size abbreviations
(defparameter *size-abbrevs*
  #(nil "K" "M" "G" "T" "P" "E" "Z" "Y" "*"))

(defparameter *block-size* 1024)

(defun size-out (n)
  (remove #\space
	  (print-size n :abbrevs *size-abbrevs* :stream nil :unit "")))
(defun bytes-out (n) (princ-to-string n))
(defun blocks-out (n) (princ-to-string (ceiling n *block-size*)))

(defun size-out-func (func)
  "Return a format function to print the size ‘n’ in field of ‘width’, in our
 prefered number formatting style, defined by ‘func’."
  (lambda (n width)
    (if (numberp n)
	(if width
	    (format nil "~v@a" width (funcall func n))
	    (funcall func n))
	(if width
	    (format nil "~v@a" width n)
	    (format nil "~@a" n)))))

(defun column-data (show-type visual sizes-as-bytes sizes-as-blocks)
  (let ((func
	  (cond
	    (sizes-as-bytes #'bytes-out)
	    (sizes-as-blocks #'blocks-out)
	    (t #'size-out))))
    `((:name "Filesystem")
      ,@(if show-type '((:name "Type")) nil)
      (:name "Size"  :align :right :type number :format ,(size-out-func func))
      (:name "Used"  :align :right :type number :format ,(size-out-func func))
      (:name "Avail" :align :right :type number :format ,(size-out-func func))
      (:name "Use%"  :align :right :type number :width 4 :format "~*~3d%")
      ,@(if visual '((:name "Free Space")) nil)
      (:name "Mounted on"))))

(defvar *cols* nil "Current column data.")

(defun bogus-filesystem-p (f)
  "Return true if ‘f’ is a bogus file system that we don't usually care to see."
  (or (zerop (filesystem-info-total-bytes f))
      #+(or linux netbsd)
      (not (or (begins-with "/" (filesystem-info-device-name f))
	       (begins-with "fuse" (filesystem-info-type f))))
      #+windows (not (filesystem-info-mount-point f))))

(defun visual-percent (pct &key (width 10))
  "Return a fat-string representing ‘pct’ percent in a field of ‘width’. Uses
the (:program :meter :character) from the theme."
  (with-output-to-fat-string (str)
    (loop :for i :from 1 :to (/ (* width pct) 100)
       :do
       (write (make-fatchar
	       :c (or (theme:value '(:program :meter :character))
		      #\space) ;; (code-char #x2592)
	       :bg (if (< i (* width .8)) :green :red))
	      :stream str :escape nil :readably nil))))

(defun generic-info (&key file-systems include-dummies show-type visual)
  "Return a collection of file system info."
  (loop
     :with size :and free :and avail :and used :and pct :and type
     :for f :in (or file-systems (mounted-filesystems))
     :do
     (setf size  (filesystem-info-total-bytes f)
	   free  (filesystem-info-bytes-free f)
	   avail (filesystem-info-bytes-available f)
	   type  (filesystem-info-type f)
	   used  (- size free)
	   pct   (if (zerop size)
		     0
		     (ceiling (* (/ used size) 100))))
     :when (or (not (bogus-filesystem-p f)) include-dummies file-systems)
     :collect
     (apply #'vector
	    `(,(filesystem-info-device-name f)
	       ,@(if show-type (list type) nil)
	       ,size
	       ,used
	       ,avail
	       ,pct
	       ,@(if visual (list (visual-percent pct)) nil)
	       ,(filesystem-info-mount-point f)
	       ))))

;; Absence of evidence is not evidence of absence.
(defun df (&key files include-dummies show-type omit-header (print t) visual
	     sizes-as-bytes sizes-as-blocks)
  "Show how much disk is free."
  (let ((*cols* (copy-tree (column-data show-type visual sizes-as-bytes
					sizes-as-blocks)))
	file-systems data table)
    (with-grout ()
      (setf file-systems (and files
			      (loop :for f :in files
				 :collect (get-filesystem-info f)))
	    data (generic-info
		  :file-systems file-systems
		  :include-dummies include-dummies
		  :show-type show-type
		  :visual visual)
	    table (make-table-from data :columns *cols*))
      (when print
	(grout-print-table table
			   :print-titles (not omit-header)
			   :trailing-spaces nil
			   :long-titles t)))
    table))

;; End
