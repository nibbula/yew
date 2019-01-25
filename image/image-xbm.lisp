;;
;; image-xbm.lisp - X bitmap images
;;

(defpackage :image-xbm
  (:documentation "XBM images")
  (:use :cl :dlib :dlib-misc :image :ppcre)
  (:export
   #:image-format-xbm
   #:x-hot-spot
   #:y-hot-spot
   #:bitmap-name
   #:read-xbm
   ))
(in-package :image-xbm)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 2) (space 0) (compilation-speed 0)))

(defclass xbm-image (image)
  ((bitmap-name
    :initarg :bitmap-name :accessor bitmap-name
    :documentation "Name in the file.")
   (x-hot-spot
    :initarg :x-hot-spot :accessor x-hot-spot
    :initform nil :type (or null fixnum)
    :documentation "X coordinate of cursor hot spot.")
   (y-hot-spot
    :initarg :y-hot-spot :accessor y-hot-spot
    :initform nil :type (or null fixnum)
    :documentation "X coordinate of cursor hot spot."))
  (:documentation "An X Bitmap image."))

(defun fail (noun &optional (verb "find"))
  (error "Can't ~s ~s. Probably not an X bitmap file." verb noun))

(defun find-define (name string &optional (error-p t))
  "Find a thing like a ‘C’ define. If error-p is NIL, don't signal an error
if we can't find it."
  (let (result)
    (multiple-value-bind (start end starts ends)
	;;(scan (s+ "\\s*#\\s*define\\s+(\\w+)_" name "\\s+([0-9]+)") string)
	;; @@@ some other junk could appear in names besides "-A-Za-z0-9_."
	(scan (s+ "\\s*#\\s*define\\s+([-A-Za-z0-9_.]+)_" name
		  "\\s+([0-9]+)") string)
      (when (not start)
	(if error-p
	    (fail name)
	    (return-from find-define nil)))
      (when (and (not (setf result
			    (parse-integer string
					   :start (aref starts 1)
					   :end (aref ends 1)
					   :junk-allowed t)))
		 error-p)
	(fail name "parse"))
      (values result end))))

(defparameter *bits-decl-regexp*
  ;; "static\\s+.*char\\s+([-A-Za-z0-9_.]+)_bits\\[\\]\\s*=\\s*{"
  ;; some bitmaps don't even say "static"
  "\\s+.*char\\s+([-A-Za-z0-9_.]+)_bits\\[\\]\\s*=\\s*{"
  "Regular expression for the bits declaration.")

(defun guess-xbm (file-or-stream)
  (let ((string (slurp file-or-stream)))
    (and (find-define "width" string nil)
	 (find-define "height" string nil)
	 (scan *bits-decl-regexp* string)
	 t)))

(defun read-hex (string start)
  "Read a C-like 2 digit hex surrounded by optional white space.
Eat following punctuation. Return the number and the new position."
  ;; This should be faster than:
  ;;  (scan "\\s*0x([0-9A-Fa-f][0-9A-Fa-f])\\s*." string :start start)
  ;; and a parse-integer
  (block nil
    (let ((pos start)
	  (len (length string))
	  x1 x2)
      (flet ((eat-whitespace (exit-p)
	       (loop :while (and (< pos len)
				 (find (char string pos) *whitespace*))
		  :do (incf pos))
	       (when (and exit-p (>= pos len))
		 (return nil)))
	     (eat-punctuation (exit-p)
	       (loop :while (and (< pos len)
				 (find (char string pos) ",};'"))
		  :do (incf pos))
	       (when (and exit-p (>= pos len))
		 (return nil))))
	(eat-whitespace t)
	(eat-punctuation t)
	(when (not (or (char= (char string pos) #\0)
		       (char= (char string pos) #\\))) ; wak old format '\x00'
       	  (return nil))
	(incf pos)
	(when (char-not-equal (char string pos) #\x)
	  (return nil))
	(incf pos)
	(when (not (setf x1 (digit-char-p (char string pos) 16)))
	  (return nil))
	(incf pos)
	(when (not (setf x2 (digit-char-p (char string pos) 16)))
	  (return nil))
	(incf pos)
	(eat-whitespace nil)
	(eat-punctuation nil)
	(values (logior (ash x1 4) x2)
		pos)))))

(defun read-xbm (file-or-stream)
  "Read an X Bitmap image from FILE-OR-STREAM."
  (let* ((string (slurp file-or-stream))
	 (width (find-define "width" string))
	 (height (find-define "height" string))
	 x-hot y-hot
	 name array
	 start end starts ends)
    (declare (type fixnum width height))
    (setf x-hot (find-define "x_hot" string nil)
	  y-hot (find-define "y_hot" string nil)
	  array (make-image-array width height))
    (multiple-value-setq (start end starts ends)
      (scan *bits-decl-regexp* string))
    (when (not start)
      (fail "bits"))
    (setf name (subseq string (aref starts 0) (aref ends 0))
	  start (1+ end))
    (loop
       :with num :and x fixnum = 0 :and y fixnum = 0
       :while (and (multiple-value-setq (num start)
		     (read-hex string start))
		   (< y height))
       :do
	 (loop
	    :for i :from 0 :to 7
	    :do
	      (when (logbitp i num)
		(setf (aref array y x) #xffffffff))
	      (incf x)
	    (when (= x width)
	      (setf x 0)
	      (incf y)
	      (return nil))))
    (make-instance 'xbm-image
		   :bitmap-name name
		   :name file-or-stream
		   :width width
		   :height height
		   :x-hot-spot x-hot
		   :y-hot-spot y-hot
		   :subimages
		   (vector
		    (make-sub-image :x 0 :y 0
				    :width width
				    :height height
				    :data array)))))

(defclass xbm-image-format (image-format)
  ()
  (:default-initargs
   :name "XBM"
   :description "X Bitmap"
   :mime-types '("image/x-xbitmap")
   :extensions '("xbm")
   :lossy nil
   :multiple nil
   :transparency nil
   :depths '(1)
   :animation nil)
  (:documentation "An X bitmap."))

(register-image-format :xbm (make-instance 'xbm-image-format))

(defmethod read-image-format (file (format (eql :xbm)))
  (read-xbm file))

(defmethod read-image-format (file (format xbm-image-format))
  (read-xbm file))

(defmethod guess-image-format (file (format (eql :xbm)))
  (guess-xbm file))

(defmethod guess-image-format (file (format xbm-image-format))
  (guess-xbm file))

;; EOF
