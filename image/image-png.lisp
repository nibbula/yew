;;;
;;; image-png.lisp - PNG images
;;;

(defpackage :image-png
  (:documentation "PNG images")
  (:use :cl :image :dlib)
  (:export
   #:image-format-png
   #:read-png
   ))
(in-package :image-png)

;; Pick whether to use png-read or pngload based on which one is already loaded.
;; This is a horrible situation. The Lisp ecosystem mostly sucks donkey ass.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :png-read)
    (add-feature :t-png-read)))

#+t-png-read
(defun png-load-stream (stream &key decode)
  (declare (ignore decode))
  (png-read:read-png-datastream stream))

#-t-png-read
(defun png-load-stream (stream &key decode)
  (pngload:load-stream file-or-stream :decode decode))

#+t-png-read
(defun png-load-file (file &key decode)
  (declare (ignore decode))
  (png-read:read-png-file file))

#-t-png-read
(defun png-load-file (file &key decode)
  (pngload:load-file file-or-stream :decode decode))

(defun png-width (image)
  #+t-png-read (png-read:width image)
  #-t-png-read (pngload:width image))

(defun png-height (image)
  #+t-png-read (png-read:height image)
  #-t-png-read (pngload:height image))

(defun png-data (image)
  #+t-png-read (png-read:image-data image)
  #-t-png-read (pngload:data image))

(defun png-data-ref (image y x &optional z)
  #+t-png-read
  (if z
      (aref (png-read:image-data image) x y z)
      (aref (png-read:image-data image) x y))
  #-t-png-read
  (if z
      (aref (pngload:data image) y x z)
      (aref (pngload:data image) y x)))
;; @@@ defsetf?

(defun png-bit-depth (image)
  #+t-png-read (png-read:bit-depth image)
  #-t-png-read (pngload:bit-depth image))

(defun png-color-type (image)
  #+t-png-read (png-read:colour-type image)
  #-t-png-read (pngload:color-type image))

(defun read-png (file-or-stream &key synopsis)
  (let ((png (if (streamp file-or-stream)
		 (png-load-stream file-or-stream :decode (not synopsis))
		 (png-load-file file-or-stream :decode (not synopsis))))
	 array dims use-alpha transparent)
    (when (not synopsis)
      (setf array (make-image-array (png-width png) (png-height png))
	    dims (array-dimensions (png-data png)))

      ;; We only really have to use our own array because of alpha.
      (cond
	((= 2 (length dims))
	 ;; Grayscale
	 (case (png-bit-depth png)
	   (8
	    (loop :with pixel
	       :for y :from 0 :below (png-height png) :do
	       (loop :for x :from 0 :below (png-width png) :do
		  (setf pixel (png-data-ref png y x))
		  (set-pixel array y x pixel pixel pixel +max-alpha+)
		  )))
	   (16
	    (loop :with pixel
	       :for y :from 0 :below (png-height png) :do
	       (loop :for x :from 0 :below (png-width png) :do
		  (setf pixel
			(ash (logand #xff00 (png-data-ref png y x))
			     -8))
		  (set-pixel array y x pixel pixel pixel +max-alpha+)
		  )))))
	((= 3 (length dims))
	 ;; Color, probably RGB or RGBA
	 (case (png-color-type png)
	   (:greyscale-alpha
	    (loop :with pixel
	       :for y :from 0 :below (png-height png) :do
	       (loop :for x :from 0 :below (png-width png) :do
		  (setf pixel (png-data-ref png y x 0))
		  (set-pixel array y x pixel pixel pixel
			     (png-data-ref png y x 1))
		  )))
	   ((:truecolour :truecolor :truecolour-alpha :truecolor-alpha
	     :indexed-colour)
	    (setf use-alpha (= 4 (array-dimension (png-data png) 2)))
	    (loop :for y :from 0 :below (png-height png) :do
	       (loop :for x :from 0 :below (png-width png) :do
		  (set-pixel array y x
			     (png-data-ref png y x 0)
			     (png-data-ref png y x 1)
			     (png-data-ref png y x 2)
			     (if use-alpha
				 (prog1 (png-data-ref png y x 3)
				   (setf transparent t))
				 +max-alpha+))
		  )))))
	(t
	 (error "I don't know how to handle ~d dimensions in a PNG."
		(length dims)))))
    (make-image :name file-or-stream
		:width (png-width png)
		:height (png-height png)
		:subimages
		(when (not synopsis)
		  (vector
		   (make-sub-image :x 0 :y 0
				   :width (png-width png)
				   :height (png-height png)
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

#+t-png-read (remove-feature :t-png-read)

;; EOF
