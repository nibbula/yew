;;
;; df.lisp - Show how much disk is free.
;;

(defpackage :df
  (:documentation "Show how much disk is free.")
  (:use :cl :opsys :dlib :dlib-misc :table :grout)
  (:export
   #:df
   #:!df
   ))
(in-package :df)

;; Custom short size abbreviations
(defparameter *size-abbrevs*
  #(nil "K" "M" "G" "T" "P" "E" "Z" "Y" "*"))

#|
(defparameter *default-cols*
  '(("Filesystem" 20 :left)
    ("Size"	  7)
    ("Used"	  7)
    ("Avail"	  7)
    ("Use%"	  4)
    ("Mounted on" nil :left)))

(defparameter *type-cols*
  '(("Filesystem" 20 :left)
    ("Type"	  5)
    ("Size"	  7)
    ("Used"	  7)
    ("Avail"	  7)
    ("Use%"	  4)
    ("Mounted on" nil :left)))
|#

(defparameter *default-cols*
  '(("Filesystem")
    ("Size"	  :right)
    ("Used"	  :right)
    ("Avail"	  :right)
    ("Use%"	  :right)
    ("Mounted on")))

(defparameter *type-cols*
  '(("Filesystem")
    ("Type")
    ("Size"	  :right)
    ("Used"	  :right)
    ("Avail"	  :right)
    ("Use%"	  :right)
    ("Mounted on")))

(defvar *cols* nil "Current column data.")

#|
(defun print-col (n v &key no-space)
  "Print column number N with value V."
  (with-output-to-string (str)
    (let* ((col   (elt *cols* n))
	   (width (second col))
	   (left  (eql (third col) :left))
	   (fmt   (if width (if left "~va" "~v@a") "~a")))
      (if width
	  (format str fmt width (subseq v 0 (min width (length v))))
	  (format str fmt v))
      (if (= n (1- (length *cols*)))
	  (write-char #\newline str)
	  (when (not no-space)
	    (write-char #\space str))))))

(defun print-title (n)
  (grout-underline (print-col n (first (elt *cols* n)) :no-space t))
  (when (< n (1- (length *cols*)))
    (grout-write #\space :escape nil))
  (grout-finish))
|#

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

(defun size-out (n)
  (remove #\space
	  (print-size n :abbrevs *size-abbrevs* :stream nil :unit "")))

(defun bogus-filesystem-p (f)
  (or (zerop (filesystem-info-total-bytes f))
      #+linux (not (begins-with "/" (filesystem-info-device-name f)))))

(defun generic-info (&optional (dummies nil) show-type)
  (loop
     :with size :and free :and avail :and used :and pct :and type
     :for f :in (mounted-filesystems)
     :do
     (setf size  (filesystem-info-total-bytes f)
	   free  (filesystem-info-bytes-free f)
	   avail (filesystem-info-bytes-available f)
	   type  (filesystem-info-type f)
	   used  (- size free)
	   pct   (if (zerop size)
		     0
		     (ceiling (* (/ used size) 100))))
     :when (or (not (bogus-filesystem-p f)) dummies)
     :collect
     (apply #'vector
	    `(,(filesystem-info-device-name f)
	       ,@(if show-type (list type) nil)
	       ,(size-out size)
	       ,(size-out used)
	       ,(size-out avail)
	       ,(format nil "~d%" pct)
	       ,(filesystem-info-mount-point f)
	       ))))

;; Absence of evidence is not evidence of absence.
(defun df (&key files include-dummies show-type omit-header (print t))
  "Show how much disk is free."
  (let ((*cols* (copy-tree (if show-type *type-cols* *default-cols*)))
	data #| max-length wanted-extra extra |# devs table)
    (with-grout ()
      (dbug "files = ~w~%" files)
      (setf devs
	    (and files
		 (loop :for f :in files
		    :collect (mount-point-of-file f)))
	    data (generic-info include-dummies show-type)
	    ;; Calculate maximum line length, and extra needed.
	    ;; max-length
	    ;; (1- 
	    ;;  (loop :for d :in data :maximize
	    ;; 	(loop :for i :from 0 :below (1- (length d))
	    ;; 	   :sum (1+ (or (second (elt *cols* i))
	    ;; 			(length (aref d i)))))))
	    ;; wanted-extra
	    ;; (loop :with small-len = (second (elt *cols* 0))
	    ;;    :for d :in data
	    ;;    :maximize (max 0 (- (length (aref d 0)) small-len)))
	    ;; extra
	    ;; (max 0 (- (grout-width) max-length))
	    )
      (dbug "devs = ~w~%" devs)
      #|
      (when (> extra wanted-extra)
	(incf (second (elt *cols* 0)) wanted-extra))
      (when (not omit-header)
	(loop :for i :from 0 :below (length *cols*)
	   :do (print-title i)))
      (loop :for d :in data :do
	 (dbug "~s ~s~%" (elt d 5) (position (elt d 5) devs))
	 (when (or (not devs)
	  	   (position (elt d 5) devs :key #'car :test #'equal))
	   (loop :for i :from 0 :below (1- (length d)) :do
	      (grout-write (print-col i (aref d i)) :escape nil)))))))
      |#
      (setf table (make-table-from
		   data
		   ;;:column-names (loop :for c :in *cols* :collect (car c))))
		   :column-names *cols*))
      (when print
	(grout-print-table table
			   :print-titles (not omit-header)
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
   (files pathname :repeating t
    :help "File systems to report on."))
  "Show how much disk is free. Lists mounted filesystems and shows usage
statisics for each one."
  (df :files files :include-dummies include-dummies :show-type show-type
      :omit-header omit-header))

;; EOF
