;;;
;;; image-gif.lisp - GIF images
;;;

(defpackage :image-gif
  (:documentation "GIF images")
  (:use :cl :dlib :image)
  (:export
   #:image-format-gif
   #:read-gif
   ))
(in-package :image-gif)

(defun get-gif-image (gif image-number)
  (let* ((image (elt (skippy:images gif) image-number))
	 (array (make-image-array (skippy:width image) (skippy:height image)))
	 (color-table (or (skippy:color-table image)
			  (skippy:color-table gif)))
	 (delay (* (skippy:delay-time image) 10)) ; convert 1/100s to ms
	 (r 0) (g 0) (b 0) (a 0)
	 (i 0) color-index transparent)
    (declare (type fixnum i)
	     (type (unsigned-byte 8) r g b))
    (loop :for y fixnum :from 0 :below (skippy:height image) :do
       (loop :for x fixnum :from 0 :below (skippy:width image) :do
	  (setf color-index (aref (skippy:image-data image) i))
	  (if (or (not (skippy:transparency-index image))
		  (/= color-index (skippy:transparency-index image)))
	      (setf (values r g b) (skippy:color-rgb
				    (skippy:color-table-entry
				     color-table color-index))
		    a +max-alpha+)
	      (setf r 0 g 0 b 0 a 0
		    transparent t))
	  ;; (cond
	  ;;   ((= (length colors) 1)
	  ;;    ;; Assume it's grayscale.
	  ;;    (setf (aref array x y 0) r
	  ;; 	   (aref array x y 1) r
	  ;; 	   (aref array x y 2) r))
	  ;;   ((= (length colors) 3)
	  ;;     (setf (aref array x y 0) r
	  ;; 	    (aref array x y 1) g
	  ;; 	    (aref array x y 2) b))
	  ;;   (t
	  ;;    (error "Unknown color format in GIF pixel.")))
	  ;; (setf (aref array y x 0) r
	  ;; 	(aref array y x 1) g
	  ;; 	(aref array y x 2) b
	  ;; 	(aref array y x 3) a)
	  (set-pixel array y x r g b a)
	  (incf i)))
    (make-sub-image :x           (skippy:left-position image)
		    :y           (skippy:top-position image)
		    :width       (skippy:width image)
		    :height      (skippy:height image)
                    :delay       (cond
                                   ((zerop delay) 100) ; default 100 ms
                                   ((< delay 20) 20)   ; quickest is 20 ms
                                   (t delay))
                    :disposal    (skippy:disposal-method image)
		    :transparent transparent
		    :data        array)))

(defun read-gif (file-or-stream)
  (let* ((gif (if (streamp file-or-stream)
		  (skippy:read-data-stream file-or-stream)
		  (skippy:load-data-stream file-or-stream)))
	 (image-count (length (skippy:images gif)))
	 (sub (make-array image-count
			  :element-type 'sub-image
			  :initial-element (make-sub-image))))
    (declare (type fixnum image-count))
    (loop :for i fixnum :from 0 :below image-count :do
       (setf (aref sub i) (get-gif-image gif i)))
    (make-image :width (skippy:width gif) :height (skippy:height gif)
		:name file-or-stream
		:subimages sub)))

(defun read-gif-synopsis (file-or-stream)
  (let ((gif (if (streamp file-or-stream)
		 (skippy:read-data-stream file-or-stream :synopsis t)
		 (skippy:load-data-stream file-or-stream :synopsis t))))
    (make-image :width (skippy:width gif)
		:height (skippy:height gif)
		:name file-or-stream)))

(defun guess-gif (file-or-stream)
  (with-open-file-or-stream (str file-or-stream
				 :element-type '(unsigned-byte 8))
    (handler-case
	(skippy::check-gif-signature str) ; @@@ export?
      (skippy:skippy-error (c)
	(declare (ignore c))
	(return-from guess-gif nil)))
    t))

(defclass gif-image-format (image-format)
  ()
  (:default-initargs
   :name "GIF"
   :description "Graphics Interchange Format"
   :mime-types '("image/gif")
   :extensions '("gif")
   :lossy nil
   :multiple t
   :transparency 1
   :depths '(8)
   :animation t)
  (:documentation ""))

(register-image-format :gif (make-instance 'gif-image-format))

(defmethod read-image-format (file (format (eql :gif)))
  (read-gif file))

(defmethod read-image-format (file (format gif-image-format))
  (read-gif file))

(defmethod guess-image-format (file (format (eql :gif)))
  (guess-gif file))

(defmethod guess-image-format (file (format gif-image-format))
  (guess-gif file))

(defmethod read-image-synopsis-format (file (format (eql :gif)))
  (read-gif-synopsis file))

(defmethod read-image-synopsis-format (file (format gif-image-format))
  (read-gif-synopsis file))

;; EOF
