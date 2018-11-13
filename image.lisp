;;
;; image.lisp - Image objects
;;

(defpackage :image
  (:documentation "Image objects.")
  (:use :cl :dlib :magic)
  (:export
   #:+max-alpha+
   #:*show-progress*
   #:image
   #:image-name
   #:image-width
   #:image-height
   #:image-color-model
   #:image-pixel-format
   #:image-subimages
   #:make-image
   #:make-sub-image
   #:sub-image
   #:sub-image-x
   #:sub-image-y
   #:sub-image-width
   #:sub-image-height
   #:sub-image-delay
   #:sub-image-disposal
   #:sub-image-transparent
   #:sub-image-data
   #:make-image-array
   #:set-pixel
   #:set-whole-pixel
   #:set-row
   #:get-pixel-r
   #:get-pixel-g
   #:get-pixel-b
   #:get-pixel-a
   #:get-whole-pixel
   #:image-format
   #:image-format-description
   #:image-format-extensions
   #:image-format-mime-types
   #:image-format-lossy
   #:image-format-multiple
   #:image-format-transparency
   #:image-format-depths
   #:image-format-animation
   #:*image-formats*
   #:find-image-format
   #:register-image-format
   #:read-image-format
   #:unknown-image-type
   #:read-image
   ))
(in-package :image)

;; (declaim (optimize (speed 3) (safety 0) (debug 1) (space 0)
;; 		   (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
 		   (compilation-speed 0)))

(defconstant +max-alpha+ 255
  "Maximum alpha value, i.e. no transparency.")

(defparameter *show-progress* t
  "True to show image loading progress indicators.")

(defparameter *disposal-methods*
  ;; No disposal specified. The decoder is not required to take any action.
  '((:unspecified . 0)
    ;; Do not dispose. The graphic is to be left in place.
    (:none . 1)
    ;; Restore to background color. The area used by the graphic must be
    ;; restored to the background color.
    (:restore-background . 2)
    ;; Restore to previous. The decoder is required to restore the area
    ;; overwritten by the graphic with what was there prior to rendering the
    ;; graphic.
    (:restore-previous . 3)))

(defstruct sub-image
  (x           0 :type fixnum)
  (y           0 :type fixnum)
  (width       0 :type fixnum)		; width in pixels
  (height      0 :type fixnum)		; height in pixels
  (delay       0 :type fixnum)		; ms to delay
  (disposal    :unspecified) 		; disposal method
  (transparent nil :type boolean) 	; true if there's any transparancy
  data)					; image data

(defclass image ()
  ((name
    :initarg :name :accessor image-name  
    :documentation "Image name, usually a file name.")
   (width
    :initarg :width :accessor image-width :initform 0 :type fixnum
    :documentation "Width in pixels.")
   (height
    :initarg :height :accessor image-height :initform 0 :type fixnum
    :documentation "Height in pixels.")
   (color-model ;; @@@ not used yet
    :initarg :color-model :accessor image-color-model
    :initform *default-color-model* :type symbol
    :documentation "Color model.")
   (pixel-format ;; @@@ not used yet
    :initarg :pixel-format :accessor image-pixel-format
    :initform :uint8 :type symbol
    :documentation "Format of pixels in data.")
   (subimages
    :initarg :subimages :accessor image-subimages
    :initform nil :type (or null (simple-array sub-image *))
    :documentation "Array of sub images, NIL if none."))
  (:documentation "2d pixel arrays."))

(defun make-image (&rest args)
  (apply #'make-instance 'image args))

(defun make-image-array (width height)
  "Make an array for image data of WIDTH x HEIGHT."
  (make-array `(,height ,width)
	      :initial-element (coerce 0 `(unsigned-byte 32))
	      :element-type `(unsigned-byte 32)))

(defun set-pixel (image y x r g b a)
  "Set pixel at X Y of IMAGE to red R, green G, and blue B."
  (setf (aref image y x) (logior (ash r 24) (ash g 16) (ash b 8) a)))

(defun set-whole-pixel (image y x pixel)
  "Set pixel at X Y of IMAGE to PIXEL. The pixel had better be the right type
and format, since there is no checking."
  (setf (aref image y x) pixel))

(defun set-row (image y x row)
  "Set row Y of the IMAGE to ROW, starting at X."
  (replace
   (make-array (- (array-dimension image 1) x)
	       :element-type (array-element-type image)
	       :displaced-to image
	       :displaced-index-offset (array-row-major-index image y x))
   row))

(defun get-pixel-r (image y x)
  "Return the red component of the pixel at X Y of IMAGE."
  (ash (aref image y x) -24))

(defun get-pixel-g (image y x)
  "Return the green component of the pixel at X Y of IMAGE."
  (logand #xff (ash (aref image y x) -16)))

(defun get-pixel-b (image y x)
  "Return the blue component of the pixel at X Y of IMAGE."
  (logand #xff (ash (aref image y x) -8)))

(defun get-pixel-a (image y x)
  "Return the alpha (or transparency) component of the pixel at X Y of IMAGE."
  (logand #xff (aref image y x)))

(defun get-whole-pixel (image y x)
  "Return the whole pixel at X Y of IMAGE."
  (aref image y x))

(defclass image-format ()
  ((name
    :initarg :name :accessor image-format-name  
    :documentation "Name of the image format.")
   (description
    :initarg :description :accessor image-format-description  
    :documentation "Description of the format.")
   (extensions
    :initarg :extensions :accessor image-format-extensions  
    :documentation "List of filename extensions used for the format.")
   (mime-types
    :initarg :mime-types :accessor image-format-mime-types
    :documentation "List of MIME types.")
   (lossy
    :initarg :lossy :accessor image-format-lossy
    :initform t :type boolean
    :documentation "True if the format can lose data.")
   (multiple
    :initarg :multiple :accessor image-format-multiple
    :initform nil :type boolean
    :documentation "True if the format can store multiple images.")
   (transparency
    :initarg :transparency :accessor image-format-transparency :initform nil
    :documentation "Bits of transparency the format can store, or NIL for none,
or T for arbitrary.")
   ;; @@@ We proably should come up with a better way to specify supported
   ;; pixel formats.
   (depths
    :initarg :depths :accessor image-format-depths  
    :documentation "List of bit depths supported by the format. T for any.")
   (animation
    :initarg :animation :accessor image-format-animation
    :initform nil :type boolean
    :documentation "True if the format supports animation."))
  (:documentation "Storage format of an image."))

(defparameter *image-formats* nil
  "List of image formats. Populated by register-image-format.")

(defun find-image-format (type &key by)
  "Find an image format. BY is one of :tag :mime :ext. If BY isn't specified, we
try to figure it out."
  (ecase by
    (:tag  (cdr (find type *image-formats* :key #'car)))
    (:mime (cdr (find type *image-formats*
		      :test (lambda (a b) (find a b :test #'equalp))
		      :key (_ (image-format-mime-types (cdr _))))))
    (:ext  (cdr (find type *image-formats*
		      :test (lambda (a b) (find a b :test #'equalp))
		      :key (_ (image-format-extensions (cdr _))))))
    ((nil)
     (typecase type
       (keyword (or (cdr (find type *image-formats* :key #'car))
		    (find-image-format (string type) :by :ext)
		    (find-image-format (string type) :by :mime)))
       (string
	(or (find-image-format (string type) :by :ext)
	    (find-image-format (string type) :by :mime)))
       (t (find-image-format (string type) :by :tag))))))

(defun register-image-format (tag object)
  (pushnew (cons tag object) *image-formats* :key #'car))

(defgeneric read-image-format (file format)
  (:documentation "Read an image file or stream with specific format."))

(define-condition unknown-image-type (simple-error) ()
  (:default-initargs
   ;;:format-control "~s is an image type ~a, which I don't know how to handle."
   :format-control "I don't know how to handle an image type of ~*~a."
    ))

(defun read-image (file-or-stream)
  (let* (slurped-stream
	 (thing (typecase file-or-stream
		  (string (pathname file-or-stream))
		  (pathname file-or-stream)
		  (stream
		   ;;(pushnew "gomer" *modules* :test #'equal)
		   (setf slurped-stream t)
		   (flexi-streams:with-output-to-sequence
		       (str :element-type '(unsigned-byte 8))
		     ;; (slow-byte-copy-stream file-or-stream str)
		     (copy-stream file-or-stream str :errorp nil
				  :element-type '(unsigned-byte 8))
		     ))
		  (t file-or-stream))) 	; hope it's okay?
	 (type
	  (progn
	    ;;(pushnew (s+ "blalp" (type-of thing)) *modules* :test #'equal)
	    (guess-content-type thing))))
    (if (equal (content-type-category type) "image")
	(let ((format (find-image-format (s+ (content-type-category type) "/"
					     (content-type-name type))
					 :by :mime)))
	  (when (not format)
	    ;; Try to autoload
	    (asdf:load-system (s+ "image-" (content-type-name type)))
	    ;; And look for it registered again.
	    (setf format (find-image-format
			  (s+ (content-type-category type) "/"
			      (content-type-name type))
			  :by :mime)))
	  (if format
	      (if slurped-stream
		  (flexi-streams:with-input-from-sequence (str thing)
		    (read-image-format str format))
		  (read-image-format thing format))
	      (cerror "Skip the image."
		      'unknown-image-type
		      :format-arguments `(,file-or-stream
					  ,(content-type-name type)))))
	(cerror "Skip the file"
		'non-image-file :format-arguments `(,file-or-stream)))))

;; EOF
