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

(defmacro do-image-or-subimage ((from-image to-image) &body body)
  "Evaluate body for the sub-image or all the sub-images. FROM-SUB-IMAGE and
TO-SUB-IMAGE are bound in the body. The body should return the new SUB-IMAGE.
The layout of the 'FROM' image and the 'TO' image must be identical.
They can be EQ, if that's what you want."
  (with-unique-names (do-it i)
    `(flet ((,do-it (from-sub-image to-sub-image)
	      (declare (ignorable from-sub-image to-sub-image))
	      ,@body))
       (etypecase ,from-image
	 (image
	  (loop :for ,i :from 0
	     :below (length (image-subimages ,from-image))
	     :do
	     (setf (aref (image-subimages ,to-image) ,i)
		   (,do-it (aref (image-subimages ,from-image) ,i)
		           (aref (image-subimages ,to-image) ,i)))))
	 (sub-image (,do-it ,from-image ,to-image))))))

(defun map-pixels (from-image to-image function)
  "Calls the FUNCTION for every pixel of FROM-IMAGE. FUNCTION is called with
with arguments: (SUB-IMAGE Y X PIXEL), where X and Y are positive integers, and
PIXEL is a thing that can be accessed by the pixel macros."
  (do-image-or-subimage (from-image to-image)
    (with-slots (width height data) from-sub-image
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below width :do
	    (setf (whole-pixel (sub-image-data to-sub-image) y x)
		  (funcall function from-sub-image y x
			   (whole-pixel data y x))))))
    to-sub-image)
  to-image)

(defun reflect-horizontal (image)
  "Flip the image horizontally, so the pixels are mirrored. Return the image."
  (do-image-or-subimage (image image)
    (with-slots (width height data) from-sub-image
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below (floor width 2) :do
	    (rotatef (whole-pixel data y x)
		     (whole-pixel data y (- width x 1))))))
    to-sub-image)
  image)

(defun reflect-vertical (image)
  "Flip the image vertically, so the pixels are mirrored. Return the image."
  (do-image-or-subimage (image image)
    (with-slots (width height data) from-sub-image
      (loop :for y :from 0 :below (floor height 2) :do
	 (loop :for x :from 0 :below width :do
	    (rotatef (whole-pixel data y x)
		     (whole-pixel data (- height y 1) x)))))
    to-sub-image)
  image)

;; @@@ rename these. maybe rotate-left & rotate-right ?

(defun rotate-1/4 (image)
  "Rotate the image 1/4 turn. Return the image."
  (do-image-or-subimage (image image)
    (with-slots (width height data) from-sub-image
      (let ((new-image (copy-sub-image from-sub-image))
	    new-data)
	;; Make a new image with swapped width & height.
	(setf (sub-image-width new-image) (sub-image-height from-sub-image)
	      (sub-image-height new-image) (sub-image-width from-sub-image)
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
  (do-image-or-subimage (image image)
    (with-slots (width height data) from-sub-image
      (let ((new-image (copy-sub-image from-sub-image))
	    new-data)
	;; Make a new image with swapped width & height.
	(setf (sub-image-width new-image) (sub-image-height from-sub-image)
	      (sub-image-height new-image) (sub-image-width from-sub-image)
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
