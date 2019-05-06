;;
;; image-ops.lisp - Image operations.
;;

(defpackage :image-ops
  (:documentation "Operations on images.")
  (:use :cl :dlib :image)
  (:import-from :image #:width #:height #:data)
  (:export
   #:map-pixels
   #:reflect-horizontal
   #:reflect-vertical
   #:rotate-1/4
   #:rotate-half
   #:rotate-3/4
   #:scale
   #:scale-horizontal
   #:scale-vertical
   #:scale-to))

(in-package :image-ops)

(defun map-pixels (image function)
  "Calls the FUNCTION for every pixel of image. FUNCTION is called with
with arguments: (IMAGE X Y PIXEL), where X and Y are positive integers, and
PIXEL is a thing that can be accessed by the pixel macros.
"
  (declare (ignore function))
  image)

(defmacro do-image-or-subimage ((image-or-subimage) &body body)
  "Evaluate body for the sub-image or all the sub-images. SUB-IMAGE
is bound to the in the body. The body should return the new SUB-IMAGE."
  (with-unique-names (do-it i)
    `(flet ((,do-it (sub-image) ,@body))
       (etypecase ,image-or-subimage
	 (image
	  (loop :for ,i :from 0
	     :below (length (image-subimages ,image-or-subimage))
	     :do
	     (setf (aref (image-subimages ,image-or-subimage) ,i)
		   (,do-it (aref (image-subimages ,image-or-subimage) ,i)))))
	 (sub-image (,do-it ,image-or-subimage))))))

(defun reflect-horizontal (image)
  "Flip the image horizontally, so the pixels are mirrored. Return the image."
  (do-image-or-subimage (image)
    (with-slots (width height data) sub-image
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below (floor width 2) :do
	    (rotatef (whole-pixel data y x)
		     (whole-pixel data y (- width x 1))))))
    sub-image)
  image)

(defun reflect-vertical (image)
  "Flip the image vertically, so the pixels are mirrored. Return the image."
  (do-image-or-subimage (image)
    (with-slots (width height data) sub-image
      (loop :for y :from 0 :below (floor height 2) :do
	 (loop :for x :from 0 :below width :do
	    (rotatef (whole-pixel data y x)
		     (whole-pixel data (- height y 1) x)))))
    sub-image)
  image)

(defun rotate-1/4 (image)
  "Rotate the image 1/4 turn. Return the image."
  (do-image-or-subimage (image)
    (with-slots (width height data) sub-image
      (let ((new-image (copy-sub-image sub-image))
	    new-data)
	;; Make a new image with swapped width & height.
	(setf (sub-image-width new-image) (sub-image-height sub-image)
	      (sub-image-height new-image) (sub-image-width sub-image)
	      new-data (make-image-array height width)
	      (sub-image-data new-image) new-data)
	(loop :for y :from 0 :below height :do
	   (loop :for x :from 0 :below width :do
	      (setf (whole-pixel new-data x (- height 1 y))
		    (whole-pixel data y x))))
	new-image)))
  image)

(defun rotate-3/4 (image)
  "Rotate the image 3/4 turn. Return the image."
  (do-image-or-subimage (image)
    (with-slots (width height data) sub-image
      (let ((new-image (copy-sub-image sub-image))
	    new-data)
	;; Make a new image with swapped width & height.
	(setf (sub-image-width new-image) (sub-image-height sub-image)
	      (sub-image-height new-image) (sub-image-width sub-image)
	      new-data (make-image-array height width)
	      (sub-image-data new-image) new-data)
	(loop :for y :from 0 :below height :do
	   (loop :for x :from 0 :below width :do
	      (setf (whole-pixel new-data (- width x 1) y)
		    (whole-pixel data y x))))
	new-image)))
  image)

(defun rotate-half (image)
  "Rotate the image a half turn. Return the image."
  image)

(defun scale (image factor)
  "Scale the IMAGE by FACTOR."
  (declare (ignore factor))
  image)

(defun scale-horizontal (image factor)
  "Scale the IMAGE horizontally FACTOR. The vertical dimension is ajusted
proportionately."
  (declare (ignore factor))
  image)

(defun scale-vertical (image factor)
  "Scale the IMAGE vertically FACTOR. The horizontal dimension is ajusted
proportionately."
  (declare (ignore factor))
  image)

(defun scale-to (image width height)
  "Scale the IMAGE to a specific WIDTH and HEIGHT. This can distort the image."
  (declare (ignore width height))
  image)

;; EOF
