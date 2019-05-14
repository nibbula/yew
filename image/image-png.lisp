;;
;; image-png.lisp - PNG images
;;

(defpackage :image-png
  (:documentation "PNG images")
  (:use :cl :image :dlib)
  (:export
   #:image-format-png
   #:read-png
   ))
(in-package :image-png)

(defun read-png (file-or-stream &key synopsis)
  (let ((png (if (streamp file-or-stream)
		 (pngload:load-stream file-or-stream :decode (not synopsis))
		 (pngload:load-file file-or-stream :decode (not synopsis))))
	 array dims use-alpha transparent)
    (when (not synopsis)
      (setf array (make-image-array (pngload:width png) (pngload:height png))
	    dims (array-dimensions (pngload:data png)))

      ;; We only really have to use our own array because of alpha.
      (cond
	((= 2 (length dims))
	 ;; Grayscale
	 (case (pngload:bit-depth png)
	   (8
	    (loop :with pixel
	       :for y :from 0 :below (pngload:height png) :do
	       (loop :for x :from 0 :below (pngload:width png) :do
		  (setf pixel (aref (pngload:data png) y x))
		  (set-pixel array y x pixel pixel pixel +max-alpha+)
		  )))
	   (16
	    (loop :with pixel
	       :for y :from 0 :below (pngload:height png) :do
	       (loop :for x :from 0 :below (pngload:width png) :do
		  (setf pixel
			(ash (logand #xff00 (aref (pngload:data png) y x))
			     -8))
		  (set-pixel array y x pixel pixel pixel +max-alpha+)
		  )))))
	((= 3 (length dims))
	 ;; Color, probably RGB or RGBA
	 (case (pngload:color-type png)
	   (:greyscale-alpha
	    (loop :with pixel
	       :for y :from 0 :below (pngload:height png) :do
	       (loop :for x :from 0 :below (pngload:width png) :do
		  (setf pixel (aref (pngload:data png) y x 0))
		  (set-pixel array y x pixel pixel pixel
			     (aref (pngload:data png) y x 1))
		  )))
	   ((:truecolour :truecolor :truecolour-alpha :truecolor-alpha
	     :indexed-colour)
	    (setf use-alpha (= 4 (array-dimension (pngload:data png) 2)))
	    (loop :for y :from 0 :below (pngload:height png) :do
	       (loop :for x :from 0 :below (pngload:width png) :do
		  (set-pixel array y x
			     (aref (pngload:data png) y x 0)
			     (aref (pngload:data png) y x 1)
			     (aref (pngload:data png) y x 2)
			     (if use-alpha
				 (prog1 (aref (pngload:data png) y x 3)
				   (setf transparent t))
				 +max-alpha+))
		  )))))
	(t
	 (error "I don't know how to handle ~d dimensions in a PNG."
		(length dims)))))
    (make-image :name file-or-stream
		:width (pngload:width png)
		:height (pngload:height png)
		:subimages
		(when (not synopsis)
		  (vector
		   (make-sub-image :x 0 :y 0
				   :width (pngload:width png)
				   :height (pngload:height png)
				   :transparent transparent
				   :data array))))))
(defun write-png (image file)
  (when (> (length (image-subimages image)) 1)
    (error "PNG files don't support multiple images."))
  (with-slots ((width image::width)
	       (height image::height)
	       (data image::data)) (svref (image-subimages image) 0)
    (let ((png (make-instance 'zpng:pixel-streamed-png
			      :color-type :truecolor-alpha
			      :width width
			      :height height))
	  (pixel (make-array 4 :element-type 'fixnum)))
    (with-open-file-or-stream (stream file :direction :output
				      :if-does-not-exist :create
				      :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below width :do
	    (setf (aref pixel 0) (pixel-r data y x)
		  (aref pixel 1) (pixel-g data y x)
		  (aref pixel 2) (pixel-b data y x)
		  (aref pixel 3) (pixel-a data y x))
	    (zpng:write-pixel pixel png)))
      (zpng:finish-png png)))))

(defparameter *png-signature* '(137 80 78 71 13 10 26 10))

(defun guess-png (file-or-stream)
  (block nil
    (with-open-file-or-stream (stream file-or-stream
				      :element-type '(unsigned-byte 8))
      (let ((buf (make-array (length *png-signature*) ;; @@@
			     :element-type '(unsigned-byte 8))))
	(handler-case
	    (progn
	      (read-sequence buf stream)
	      (when (not (equalp buf *png-signature*)) ;; @@@
		(return nil)))
	  (stream-error (c)
	    (declare (ignore c))
	    (return nil)))))
    t))

(defclass png-image-format (image-format)
  ()
  (:default-initargs
   :name "PNG"
   :description "Portable Network Graphics"
   :mime-types '("image/png")
   :extensions '("png")
   :lossy nil
   :multiple nil
   :transparency 16
   :depths '(1 2 4 8 16 24 32 64)
   :animation nil)
  (:documentation ""))

(register-image-format :png (make-instance 'png-image-format))

(defmethod read-image-format (file (format (eql :png)))
  (read-png file))

(defmethod read-image-format (file (format png-image-format))
  (read-png file))

(defmethod write-image-format (image file (format (eql :png)))
  (write-png image file))

(defmethod write-image-format (image file (format png-image-format))
  (write-png image file))

(defmethod guess-image-format (file (format (eql :png)))
  (guess-png file))

(defmethod guess-image-format (file (format png-image-format))
  (guess-png file))

(defmethod read-image-synopsis-format (file (format (eql :png)))
  (read-png file :synopsis t))

(defmethod read-image-synopsis-format (file (format png-image-format))
  (read-png file :synopsis t))

;; EOF
