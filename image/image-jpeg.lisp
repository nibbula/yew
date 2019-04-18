;;
;; image-jpeg.lisp - JPEG images
;;

(defpackage :image-jpeg
  (:documentation "JPEG images.")
  (:use :cl :image :dlib :dlib-misc)
  (:export
   #:jpeg-image-format
   #:read-jpeg
   ))
(in-package :image-jpeg)

(defun read-jpeg (file-or-stream)
  (multiple-value-bind (data height width colors)
      (if (streamp file-or-stream)
	  (cl-jpeg:decode-stream file-or-stream)
	  (cl-jpeg:decode-image (nos:quote-filename file-or-stream)))
    (when (not (member colors '(1 3)))
      (error "I don't know how to handle a ~d color JPEG." colors))
    ;; convert to multi-dimensional array
    (let ((array (make-image-array width height))
	  (i 0))
      ;;(pause "JPEG ~a x ~a ~d" width height (length data))
      (with-spin ()
	(loop :for y :from 0 :below height :do
	   (when *show-progress* (spin))
	   (loop :for x :from 0 :below width :do
	      (case colors
		(3
		 ;; (setf (aref array y x 3) +max-alpha+
		 ;;       (aref array y x 2) (aref data i)
		 ;;       (aref array y x 1) (aref data (+ i 1))
		 ;;       (aref array y x 0) (aref data (+ i 2)))
		 (set-pixel array y x
			    (aref data (+ i 2))
			    (aref data (+ i 1))
			    (aref data i)
			    +max-alpha+)
		 (incf i 3))
		(1
		 ;; (setf (aref array y x 3) +max-alpha+
		 ;;       (aref array y x 2) (aref data i)
		 ;;       (aref array y x 1) (aref data i)
		 ;;       (aref array y x 0) (aref data i))
		 (set-pixel array y x
			    (aref data i) (aref data i) (aref data i)
			    +max-alpha+)
		 (incf i))))))
      (make-image :name file-or-stream
		  :width width :height height
		  :subimages
		  (vector
		   (make-sub-image :x 0 :y 0
				   :width width
				   :height height
				   :data array))))))

(defun read-jpeg-synopsis (file-or-stream)
  ;; @@@ fix not to read the whole image
  (multiple-value-bind (data height width colors)
      (if (streamp file-or-stream)
	  (cl-jpeg:decode-stream file-or-stream)
	  (cl-jpeg:decode-image (nos:quote-filename file-or-stream)))
    (declare (ignore data colors))
    (make-image :name file-or-stream
		:width width :height height)))

(defun guess-jpeg (file-or-stream)
  (block nil
    (with-open-file-or-stream (stream file-or-stream
				 :element-type '(unsigned-byte 8))
      (let ((buf (make-array 10 :element-type '(unsigned-byte 8))))
	(handler-case
	    (progn
	      (read-sequence buf stream)
	      (when (not (equalp (subseq buf 0 2) #(#xff #xd8)))
		(return nil)))
	  (stream-error (c)
	    (declare (ignore c))
	    (return nil)))))
    t))

(defclass jpeg-image-format (image-format)
  ()
  (:default-initargs
   :name "JPEG"
   :description "Joint Photographic Experts Group image"
   :mime-types '("image/jpeg")
   :extensions '("jpg" "jpeg")
   :lossy t
   :multiple nil
   :transparency nil
   :depths '(12 24)
   :animation nil)
  (:documentation ""))

(register-image-format :jpeg (make-instance 'jpeg-image-format))

(defmethod read-image-format (file (format (eql :jpeg)))
  (read-jpeg file))

(defmethod read-image-format (file (format jpeg-image-format))
  (read-jpeg file))

(defmethod guess-image-format (file (format (eql :jpeg)))
  (guess-jpeg file))

(defmethod guess-image-format (file (format jpeg-image-format))
  (guess-jpeg file))

(defmethod read-image-synopsis-format (file (format (eql :jpeg)))
  (read-jpeg-synopsis file))

(defmethod read-image-synopsis-format (file (format jpeg-image-format))
  (read-jpeg-synopsis file))

;; EOF
