;;
;; image-tiff.lisp - TIFF images
;;

(defpackage :image-tiff
  (:documentation
   "Read Tagged Image File Format images. Uses the Retrospectiff package.
It doesn't handle all TIFF files.")
  (:use :cl :dlib :image :retrospectiff :color)
  (:export
   #:read-tiff
   #:tiff-image-format
   ))
(in-package :image-tiff)

(defun read-tiff (file-or-stream)
  (let* ((image (read-tiff-file file-or-stream))
	 (array (make-image-array (tiff-image-width image)
				  (tiff-image-length image)))
    	 (data (tiff-image-data image)))
    (typecase (tiff-image-bits-per-sample image)
      (sequence
       (when (some (_ (> _ 32)) (tiff-image-bits-per-sample image))
	 (error "Can't handle more than 32 bits per sample: ~s."
      		(tiff-image-bits-per-sample image))))
      (number
       (when (> (tiff-image-bits-per-sample image) 32)
	 (error "Can't handle more than 32 bits per sample: ~s."
      		(tiff-image-bits-per-sample image))))
      (t
       (error "I don't understand a bits-per-sample of type ~s: ~s"
	      (type-of (tiff-image-bits-per-sample image))
	      (tiff-image-bits-per-sample image))))
    (case (tiff-image-samples-per-pixel image)
      (1
       (if (tiff-image-color-map image)
	 (loop :with i = 0
	   :and len = (length data)
	   :and map = (tiff-image-color-map image)
	   :and pixel
	   :for y :from 0 :below (truncate len (tiff-image-width image))
	   :while (< i len) :do
             (loop :for x :from 0 :below (tiff-image-width image)
		:while (< i len) :do
		(setf pixel (aref map (aref data i)))
		(set-pixel array y x
			   (component-to-8bit (/ (first  pixel) #xffff))
			   (component-to-8bit (/ (second pixel) #xffff))
			   (component-to-8bit (/ (third  pixel) #xffff))
			   +max-alpha+)
		(incf i)))
	 (loop :with i = 0 :and len = (length data)
	  :for y :from 0 :below (truncate len (tiff-image-width image))
	  ;;(tiff-image-length image)
	  :while (< i len)
	  :do
            (loop :for x :from 0 :below (tiff-image-width image)
	      :while (< i len)
	      :do 
	      (set-pixel array y x (aref data i) (aref data i) (aref data i)
			 +max-alpha+)
	      (incf i)))))
      (3
       (loop :with i = 0 :and len = (length data)
	  :for y :from 0 :below (truncate len (tiff-image-width image))
	  :while (< i len)
	  :do
	    #|(tiff-image-length image) |#
            (loop :for x :from 0 :below (tiff-image-width image)
	       :while (< i len) :do
	       (set-pixel array y x
			  (aref data i) (aref data (+ i 1)) (aref data (+ i 2))
			  +max-alpha+)
	       (incf i 3))))
      (otherwise
       (error "Sorry. I can't handle ~s samples per pixel."
	      (tiff-image-samples-per-pixel image))))
    (make-image :name file-or-stream
		:width (tiff-image-width image)
		:height (tiff-image-length image)
		:subimages
		(vector
		 (make-sub-image :x 0 :y 0
				 :width (tiff-image-width image)
				 :height (tiff-image-length image)
				 :transparent nil
				 :data array)))))

(defclass tiff-image-format (image-format)
  ()
  (:default-initargs
   :name "TIFF"
   :description "Tagged Image File Format"
   :mime-types '("image/tiff")
   :extensions '("tiff" "tif")
   :lossy t
   :lossless t
   :multiple t
   :transparency t
   :depths t
   :animation nil)
  (:documentation "Format for TIFF images."))

(register-image-format :tiff (make-instance 'tiff-image-format))

(defmethod read-image-format (file (format (eql :tiff)))
  (read-tiff file))

(defmethod read-image-format (file (format tiff-image-format))
  (read-tiff file))

;; EOF
