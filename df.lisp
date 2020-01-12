;;
;; df.lisp - Show how much disk is free.
;;

(defpackage :df
  (:documentation "Show how much disk is free.")
  (:use :cl :opsys :dlib :dlib-misc :table :grout :fatchar :fatchar-io)
  (:export
   #:df
   #:!df
   ))
(in-package :df)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; Custom short size abbreviations
(defparameter *size-abbrevs*
  #(nil "K" "M" "G" "T" "P" "E" "Z" "Y" "*"))

(defun size-out (n)
  (remove #\space
	  (print-size n :abbrevs *size-abbrevs* :stream nil :unit "")))

(defun size-out-with-width (n width)
  "Print the size in our prefered style."
  (if (numberp n)
      (if width
	  (format nil "~v@a" width (size-out n))
	  (size-out n))
      (if width
	  (format nil "~v@a" width n)
	  (format nil "~@a" n))))

#|
(defparameter *default-cols*
  `((:name ("Filesystem"))
    (:name ("Size"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Used"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Avail" :right) :type number :format ,#'size-out-with-width)
    (:name ("Use%"  :right) :type number :width 4 :format "~*~3d%")
    (:name ("Mounted on"))))

(defparameter *type-cols*
  `((:name ("Filesystem"))
    (:name ("Type"))
    (:name ("Size"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Used"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Avail" :right) :type number :format ,#'size-out-with-width)
    (:name ("Use%"  :right) :type number :width 4 :format "~*~3d%")
    (:name ("Mounted on"))))
|#

(defun column-data (show-type visual)
  `((:name ("Filesystem"))
    ,@(if show-type '((:name ("Type"))) nil)
    (:name ("Size"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Used"  :right) :type number :format ,#'size-out-with-width)
    (:name ("Avail" :right) :type number :format ,#'size-out-with-width)
    (:name ("Use%"  :right) :type number :width 4 :format "~*~3d%")
    ,@(if visual '((:name "Free Space")) nil)
    (:name ("Mounted on"))))

(defvar *cols* nil "Current column data.")

#|

(defun print-blocks-as-size (blocks f)
  (print-size (* blocks (statfs-bsize f))
	      :abbrevs *size-abbrevs* :stream nil :unit ""))

(defun bsd-unix-info ()
  (loop :for f :in (getmntinfo)
     :collect
     (let* ((size      (statfs-blocks f))
	    (free      (statfs-bfree f))
	    (avail     (statfs-bavail f))
	    (used      (- size free))
	    (pct       (if (zerop size)
			   0
			   (ceiling (* (/ used size) 100))))
	    (from-name (statfs-mntfromname f))
	    (to-name   (statfs-mntonname f))
	    (dev	   (elt (statfs-fsid f) 0)))
       (vector from-name
	       (print-blocks-as-size size f)
	       (print-blocks-as-size used f)
	       (print-blocks-as-size avail f)
	       (format nil "~d%" pct)
	       to-name
	       dev))))
|#

(defun bogus-filesystem-p (f)
  (or (zerop (filesystem-info-total-bytes f))
      #+linux (not (begins-with "/" (filesystem-info-device-name f)))
      #+windows (not (filesystem-info-mount-point f))
      ))

(defun visual-percent (pct &key (width 10))
  (with-output-to-fat-string (str)
    (loop :for i :from 1 :to (/ (* width pct) 100)
       :do
       (write (make-fatchar
	       :c (code-char #x2592)
	       :bg (if (< i (* width .8)) :green :red))
	      :stream str :escape nil :readably nil))))

(defun generic-info (&key file-systems include-dummies show-type visual)
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
(defun df (&key files include-dummies show-type omit-header (print t) visual)
  "Show how much disk is free."
  (let ((*cols* (copy-tree (column-data show-type visual)))
		 ;;(if show-type *type-cols* *default-cols*)))
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

#+lish
(lish:defcommand df
  ((include-dummies boolean :short-arg #\a
    :help "True to include dummy file systems.")
   (show-type boolean :short-arg #\t
    :help "True to show filesystem types.")
   (omit-header boolean :short-arg #\h
    :help "True to omit the header.")
   (visual boolean :short-arg #\v :default t
    :help "True to show free space visually.")
   (files pathname :repeating t
    :help "File systems to report on."))
  "Show how much disk is free. Lists mounted filesystems and shows usage
statisics for each one."
  (setf lish:*output*
	(df :files files :include-dummies include-dummies :show-type show-type
	    :omit-header omit-header :visual visual)))

;; EOF
