;;
;; image-png.lisp - PNG images
;;

(defpackage :image-png
  (:documentation "PNG images")
  (:use :cl :image)
  (:export
   #:image-format-png
   #:read-png
   ))
(in-package :image-png)

(defun read-png (file-or-stream)
  (let* ((png (if (streamp file-or-stream)
		 (png-read:read-png-datastream file-or-stream)
		 (png-read:read-png-file file-or-stream)))
	 (array (make-image-array (png-read:width png) (png-read:height png)))
	 (dims (array-dimensions (png-read:image-data png)))
	 use-alpha transparent)
    ;; We only really have to use our own array because of alpha.
    (cond
      ((= 2 (length dims))
       ;; Grayscale
       (case (png-read:bit-depth png)
	 (8
	  (loop :with pixel
	     :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		;; (setf (aref array y x 0) (aref (png-read:image-data png) x y)
		;;       (aref array y x 1) (aref (png-read:image-data png) x y)
		;;       (aref array y x 2) (aref (png-read:image-data png) x y)
		;;       (aref array y x 3) +max-alpha+)
		(setf pixel (aref (png-read:image-data png) x y))
		(set-pixel array y x pixel pixel pixel +max-alpha+)
		)))
	 (16
	  (loop :with pixel
	     :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf pixel
		      (ash (logand #xff00 (aref (png-read:image-data png) x y))
			   -8))
		;; (setf (aref array y x 0) pixel
		;;       (aref array y x 1) pixel
		;;       (aref array y x 2) pixel
		;;       (aref array y x 3) +max-alpha+)
		(set-pixel array y x pixel pixel pixel +max-alpha+)
		)))))
      ((= 3 (length dims))
       ;; Color, probably RGB or RGBA
       (case (png-read:colour-type png)
	 (:greyscale-alpha
	  (loop :with pixel
	     :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf pixel (aref (png-read:image-data png) x y 0))
		;; (setf (aref array y x 0) pixel
		;;       (aref array y x 1) pixel
		;;       (aref array y x 2) pixel
		;;       (aref array y x 3)
		;;       (aref (png-read:image-data png) x y 1))
		(set-pixel array y x pixel pixel pixel
			   (aref (png-read:image-data png) x y 1))
		)))
	 ((:truecolor :truecolor-alpha :indexed-colour)
	  (setf use-alpha (= 4 (array-dimension (png-read:image-data png) 2)))
	  (loop :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		;;(setf (aref array y x 0) (aref (png-read:image-data png) x y 0)
		;;      (aref array y x 1) (aref (png-read:image-data png) x y 1)
		;;      (aref array y x 2) (aref (png-read:image-data png) x y 2)
		;;      (aref array y x 3)
		;;      (if use-alpha
		;;	  (prog1 (aref (png-read:image-data png) x y 3)
		;;	    (setf transparent t))
		;;	  +max-alpha+))
		(set-pixel array y x
			   (aref (png-read:image-data png) x y 0)
			   (aref (png-read:image-data png) x y 1)
			   (aref (png-read:image-data png) x y 2)
			   (if use-alpha
			       (prog1 (aref (png-read:image-data png) x y 3)
				 (setf transparent t))
			       +max-alpha+))
		)))))
      (t
       (error "I don't know how to handle ~d dimensions in a PNG."
	      (length dims))))
    (make-image :name file-or-stream
		:width (png-read:width png)
		:height (png-read:height png)
		:subimages
		(vector
		 (make-sub-image :x 0 :y 0
				 :width (png-read:width png)
				 :height (png-read:height png)
				 :transparent transparent
				 :data array)))))

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

;; EOF
