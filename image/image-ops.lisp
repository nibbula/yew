;;
;; image-ops.lisp - Image operations.
;;

(defpackage :image-ops
  (:documentation "Operations on images.")
  (:use :cl :dlib :image :lparallel)
  (:import-from :image #:width #:height #:data)
  (:export
   #:map-pixels
   #:pmap-pixels
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

(define-condition pixel-function-error (simple-error)
  ((conditions
    :initarg :conditions :accessor pixel-function-error-conditions
    :initform nil :type list
    :documentation "A list of conditions occured during the pixel function."))
  (:documentation "Some condition occured in a pixel function."))

(defmacro with-condition-gathering ((conditions-var) &body body)
  "Handle any condition that occurs in BODY and gather them in ERRORS-VAR.
Sets ERRORS-VAR to NIL beforehand, so it's only non-NIL if you got errors.
Return CONDITIONS-VAR."
  `(progn
     (setf ,conditions-var nil)
     (handler-case
	 (progn ,@body)
       (condition (c)
	 (push c ,conditions-var)))
     ,conditions-var))

(defun map-pixels (from-image to-image function)
  "Calls the FUNCTION for every pixel of FROM-IMAGE. FUNCTION is called with
with arguments: (SUB-IMAGE Y X PIXEL), where X and Y are positive integers, and
PIXEL is a thing that can be accessed by the pixel macros."
  (let ((conditions nil))
    (with-condition-gathering (conditions)
      (do-image-or-subimage (from-image to-image)
	(with-slots ((width image::width)
		     (height image::height)
		     (data image::data)) from-sub-image
	  (loop :for y :from 0 :below height :do
	     (loop :for x :from 0 :below width :do
		(setf (whole-pixel (sub-image-data to-sub-image) y x)
		      (funcall function from-sub-image y x
			       (whole-pixel data y x))))))
	to-sub-image)
      to-image)
    (when conditions
      (error 'pixel-function-error
	     :format-control "Pixel errors: ~s"
	     :format-arguments conditions
	     :conditions conditions))))

(defun ensure-parallel-kernel ()
  (when (not lparallel:*kernel*)
    (setf lparallel:*kernel*
	  (lparallel:make-kernel (nos:processor-count)))))

(defun map-piece (from-image to-image function start end width)
  (loop :for y :from start :below end :do
     (loop :for x :from 0 :below width :do
	(setf (whole-pixel (sub-image-data to-image) y x)
	      (funcall function from-image y x
		       (whole-pixel (sub-image-data from-image) y x))))))

(defun pmap-pixels (from-image to-image function)
  "Calls the FUNCTION for every pixel of FROM-IMAGE. FUNCTION is called with
with arguments: (SUB-IMAGE Y X PIXEL), where X and Y are positive integers, and
PIXEL is a thing that can be accessed by the pixel macros."
  (ensure-parallel-kernel)
  (let ((n (nos:processor-count))
	(chan (make-channel))
	(conditions nil))
    (do-image-or-subimage (from-image to-image)
      (let* ((height (sub-image-height from-sub-image))
	     (piece-size (floor height n))
	     (left-over (rem height n))
	     (width (sub-image-width from-sub-image))
	     (piece 0)
	     (y 0)
	     (done-count 0))
	(loop :while (< piece (1- n)) :do
	   ;; (submit-task chan #'map-piece
	   ;; 	      from-sub-image to-sub-image function
	   ;; 	      y (+ y piece-size) width)
	   (submit-task chan
			(lambda (s e)
			  (with-condition-gathering (conditions)
			    (map-piece
			     from-sub-image to-sub-image function s e width)))
			y (+ y piece-size))
	   ;; (map-piece from-sub-image to-sub-image function
	   ;; 	      y (+ y piece-size) width)
	   (incf piece)
	   (setf y (* piece piece-size)))
	;; last piece
	;; (submit-task chan #'map-piece from-sub-image to-sub-image function
	;; 		   y (+ y piece-size left-over) width)
	(submit-task chan
		     (lambda (s e)
		       (with-condition-gathering (conditions)
			 (map-piece from-sub-image to-sub-image function
				    s e width)))
		     y (+ y piece-size left-over))
	;; wait for every piece
	;; (loop :with junk :and not-done
	;;    :do (setf (values junk not-done)
	;; 	     (try-receive-result chan :timeout .0000001))
	;;    (format t "~s ~s~%" junk not-done)
	;;    (incf done-count)
	;;    :while not-done)
	(loop :with junk
	   :for i :from 1 :to n
	   :do (setf junk (receive-result chan))
	   (incf done-count)
	   :while (not conditions))
	(when conditions
	  (error 'pixel-function-error
		 :format-control "Pixel errors: ~s"
		 :format-arguments conditions
		 :conditions conditions))
	(when (< done-count n)
	  (format t "Only ~d done!~%" done-count))
	)
      to-sub-image)
    to-image))

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
