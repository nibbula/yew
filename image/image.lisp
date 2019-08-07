;;
;; image.lisp - Image objects
;;

(defpackage :image
  (:documentation "Image objects.

To use:
  read-image file -> image
  write-image image file

  make-instance 'image

  Accessors:
    image-*
    sub-image-*

  Functions:
    set-pixel image y x r g b a
    set-whole-pixel image y x pixel
    set-row image y x row
    get-pixel-* image y x
    get-whole-pixel image y x

To make a new format:
  - Follow an example in image-*.{lisp,asd}:
    - Make an :image-ZZ system and package.
    - Subclass image-format, as ZZ-image-format.
    - Make methods for:
       guess-image-format file X
       read-image-format file X
       write-image-format file X
   - Optionally add a tag to *known-image-types*. It's optional because most
     image types can be identified by the `magic` package, but if yours can't,
     then it's best to put it in *known-image-types* so it can use the guesser.
     This auto loading makes it so we don't always have to wastefully load every
     archaic image type.
")
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
   #:make-blank-image
   #:make-blank-copy
   #:make-sub-image
   #:copy-sub-image
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
   #:make-pixel
   #:whole-pixel
   #:get-whole-pixel
   #:set-whole-pixel
   #:set-row
   #:pixel-r
   #:pixel-g
   #:pixel-b
   #:pixel-a
   #:image-format
   #:image-format-name
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
   #:guess-image-format
   #:guess-registered-image-type
   #:load-known-formats
   #:read-image-format
   #:read-image-synopsis-format
   #:write-image-format
   #:unknown-image-type
   #:non-image-file
   #:read-image
   #:read-image-synopsis
   #:write-image
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
   ;; (color-model ;; @@@ not used yet
   ;;  :initarg :color-model :accessor image-color-model
   ;;  :initform *default-color-model* :type symbol
   ;;  :documentation "Color model.")
   ;; (pixel-format ;; @@@ not used yet
   ;;  :initarg :pixel-format :accessor image-pixel-format
   ;;  :initform :uint8 :type symbol
   ;;  :documentation "Format of pixels in data.")
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

(defun make-blank-image (&rest args)
  (unless (and (getf args :width)
	       (getf args :height))
    (error "You need to specify a width and height to make a blank image."))
  (when (getf args :subimages)
    (error "Don't specify subimages when making a blank image."))
  (let ((result (apply #'make-instance 'image args))
	(width (getf args :width))
	(height (getf args :height)))
    (setf (image-subimages result)
	  (make-array
	   1 :element-type 'sub-image
	   :initial-contents
	   (list (make-sub-image :x 0 :y 0
				 :width width
				 :height height
				 :data (make-image-array width height)))))
    result))

(defun make-blank-copy (image)
  "Make a new blank version of image."
  (let ((new-image
	 (make-image
	  :name (image-name image)
	  :width (image-width image)
	  :height (image-height image)
	  :subimages
	  (make-array
	   (length (image-subimages image))
	   :element-type 'sub-image
	   :initial-contents ;; What a Horselover Fat of a function.
	   (loop :with img
	      :for i :from 0
	      :below (length (image-subimages image))
	      :collect
	      (make-sub-image
	       :x (sub-image-x
		   (setf img (aref (image-subimages image) i)))
	       :y           (sub-image-y           img)
	       :width       (sub-image-width       img)
	       :height      (sub-image-height      img)
	       :delay       (sub-image-delay       img)
	       :disposal    (sub-image-disposal    img)
	       :transparent (sub-image-transparent img)
	       :data (make-image-array
		      (sub-image-width img)
		      (sub-image-height img))))))))
    new-image))

(defun make-pixel (r g b &optional (a #xff))
  (logior (ash r 24) (ash g 16) (ash b 8) a))

(defun set-pixel (image y x r g b a)
  "Set pixel at X Y of IMAGE to red R, green G, and blue B."
  (setf (aref image y x) (logior (ash r 24) (ash g 16) (ash b 8) a)))

(defun set-whole-pixel (image y x pixel)
  "Set pixel at X Y of IMAGE to PIXEL. The pixel had better be the right type
and format, since there is no checking."
  (setf (aref image y x) pixel))

;; @@@ I think the get-* forms should be deprecated.

(defun get-whole-pixel (image y x)
  "Return the whole pixel at X Y of IMAGE."
  (aref image y x))

(defun whole-pixel (image y x)
  "Return the whole pixel at the X and Y coorindates of IMAGE."
  (aref image y x))

(defsetf whole-pixel set-whole-pixel
  "Set pixel at X Y of IMAGE to PIXEL. The pixel had better be the right type
and format, since there is no checking.")

(defun set-row (image y x row)
  "Set row Y of the IMAGE to ROW, starting at X."
  (replace
   (make-array (- (array-dimension image 1) x)
	       :element-type (array-element-type image)
	       :displaced-to image
	       :displaced-index-offset (array-row-major-index image y x))
   row))

(defun pixel-r (image y x)
  "Return the red component of the pixel at X Y of IMAGE."
  (ash (aref image y x) -24))

(defun set-pixel-r (image y x r)
  "Set the red component of pixel at X Y of IMAGE to R."
  (setf (aref image y x) (logior (logand (ash r 24) #xff000000)
				 (logand #x00ffffff (aref image y x)))))

(defsetf pixel-r set-pixel-r
  "Set the red component of pixel at X Y of IMAGE.")

(defun pixel-g (image y x)
  "Return the green component of the pixel at X Y of IMAGE."
  (logand #xff (ash (aref image y x) -16)))

(defun set-pixel-g (image y x g)
  "Set the green component of pixel at X Y of IMAGE to G."
  (setf (aref image y x) (logior (logand (ash g 16) #x00ff0000)
				 (logand #xff00ffff (aref image y x)))))

(defsetf pixel-g set-pixel-g
  "Set the green component of pixel at X Y of IMAGE.")

(defun pixel-b (image y x)
  "Return the blue component of the pixel at X Y of IMAGE."
  (logand #xff (ash (aref image y x) -8)))

(defun set-pixel-b (image y x b)
  "Set the blue component of pixel at X Y of IMAGE to B."
  (setf (aref image y x) (logior (logand (ash b 8) #x0000ff00)
				 (logand #xffff00ff (aref image y x)))))

(defsetf pixel-b set-pixel-b
  "Set the green component of pixel at X Y of IMAGE.")

(defun pixel-a (image y x)
  "Return the alpha (or transparency) component of the pixel at X Y of IMAGE."
  (logand #xff (aref image y x)))

(defun set-pixel-a (image y x a)
  "Set the alpha component of pixel at X Y of IMAGE to A."
  (setf (aref image y x) (logior (logand a #xff)
				 (logand #xffffff00 (aref image y x)))))

(defsetf pixel-a set-pixel-a
  "Set the alpha component of pixel at X Y of IMAGE.")

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
   (lossless
    :initarg :lossless :accessor image-format-lossless
    :initform t :type boolean
    :documentation "True if the format can preserve all data.")
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
       (image-format type)
       (keyword (or (cdr (find type *image-formats* :key #'car))
		    (find-image-format (string type) :by :ext)
		    (find-image-format (string type) :by :mime)))
       (string
	(or (find-image-format (string type) :by :ext)
	    (find-image-format (string type) :by :mime)))
       (t (find-image-format (string type) :by :tag))))))

(defun register-image-format (tag object)
  (pushnew (cons tag object) *image-formats* :key #'car))

(defgeneric guess-image-format (thing format)
  (:documentation "Return true if we guess that THING is a FORMAT image."))

(defun coerce-to-stream-or-filename (thing)
  (typecase thing
    ((vector (unsigned-byte 8))
     (flexi-streams:make-flexi-stream
      (flexi-streams:make-in-memory-input-stream thing)
      ;; :element-type '(unsigned-byte 8)
      ))
    (t thing)))

(defun guess-registered-image-type (thing)
  "Return the guessed format of THING, from the registered image formats."
  (block nil
    (loop :for (nil . obj) :in *image-formats* ;; actually (tag . obj)
       :do
       (when (guess-image-format (coerce-to-stream-or-filename thing) obj)
	 (return obj)))))

(defparameter *known-image-types* '(:jpeg :png :gif :tiff :xbm)
  "List of known image format tags.")

(defun load-image-format (format-tag)
  (asdf:load-system (s+ "image-" (string-downcase format-tag))))

(defun load-known-formats ()
  "Load all the image formats we know about. This is useful to make sure
read-image knows about all the formats."
  (loop :for tag :in *known-image-types*
     :do (load-image-format tag)))

(defgeneric read-image-format (file format)
  (:documentation "Read an image file or stream with specific format."))

(define-condition unknown-image-type (simple-error) ()
  (:default-initargs
   ;;:format-control "~s is an image type ~a, which I don't know how to handle."
   :format-control "I don't know how to handle an image type of ~*~a."
    ))

(define-condition non-image-file (simple-error) ()
  (:default-initargs
   :format-control "~s doesn't seem to be an image"))

(defun read-image-with-function (file-or-stream function)
  "Try to read FILE-OR-STREAM as an image. Return an IMAGE object if we're
 successful. It tries to guess the format, but it might help if the format
is loaded already. Otherwise it tries to load a guessed format.
It signals unknown-image-type or non-image-file, if it can't figure it out."
  (let* (array
	 (thing (typecase file-or-stream
		  (string (pathname file-or-stream))
		  (pathname file-or-stream)
		  (stream
		   (coerce-to-stream-or-filename
		    (setf array
			  (flexi-streams:with-output-to-sequence
			      (str :element-type '(unsigned-byte 8))
			    (copy-stream file-or-stream str :errorp nil
					 :element-type '(unsigned-byte 8))))))
		  (t file-or-stream))) 	; hope it's okay?
	 type format)
    (flet ((read-it ()
	     (funcall function thing format)))
      (cond
	;; First let the registered formats try guessing.
	((setf format (guess-registered-image-type (or array thing)))
	 (values (read-it) format))
	;; Then try the system guesser, and autoloading the format.
	((equal (content-type-category
		 (setf type (cond
			      (array (guess-content-type array))
			      (thing (guess-file-type thing))
			      (t (error "neither thing or array")))))
		"image")
	 (when (not (setf format (find-image-format
				  (s+ (content-type-category type) "/"
				      (content-type-name type))
				  :by :mime)))
	   ;; Try to autoload
	   (asdf:load-system (s+ "image-" (content-type-name type)))
	   ;; And look for it registered again.
	   (setf format (find-image-format
			 (s+ (content-type-category type) "/"
			     (content-type-name type))
			 :by :mime)))
	 (if format
	     (values (read-it) format)
	     (cerror "Skip the image."
		     'unknown-image-type
		     :format-arguments `(,thing #| file-or-stream |#
					 ,(content-type-name type)))))
	(t
	 (cerror "Skip the file"
		 'non-image-file
		 :format-arguments `(,thing #|file-or-stream|#)))))))

(defun read-image (file-or-stream)
  "Try to read FILE-OR-STREAM as an image. Return an IMAGE object if we're
 successful. It tries to guess the format, but it might help if the format
is loaded already. Otherwise it tries to load a guessed format.
It signals unknown-image-type or non-image-file, if it can't figure it out.
The format type is returned as the second value."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (read-image-with-function file-or-stream #'read-image-format))

(defgeneric read-image-synopsis-format (file format)
  (:documentation
   "Read a synopsis of an image from file or stream with a specific format."))

(defun read-image-synopsis (file-or-stream)
  "Try to read FILE-OR-STREAM as an image. Return an IMAGE object if we're
 successful, but without the image data. It tries to guess the format, but it
might help if the format is loaded already. Otherwise it tries to load a
guessed format. It signals unknown-image-type or non-image-file, if it can't
figure it out. The format type is returned as the second value."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (read-image-with-function file-or-stream #'read-image-synopsis-format))

(defgeneric write-image-format (image stream format)
  (:documentation "Read an image file or stream with specific format."))

(defun write-image (image file-or-stream format)
  "Try to write FILE-OR-STREAM as an image. Signal an error if we fail,
which, among other possiblities, will be unknown-image-type if format isn't a
known type, or a file-error if we can't open it."
  (let ((real-format (find-image-format format)))
    (if (not real-format)
	(progn
	  ;; Try to autoloading
	  (asdf:load-system (s+ "image-" (string-downcase format)))
	  (when (not (setf real-format (find-image-format format)))
	    (cerror "Skip the image."
		    'unknown-image-type
		    :format-arguments `(,file-or-stream ,format))))
	(typecase file-or-stream
	  ((or pathname string)
	   (with-open-file (stream file-or-stream
				   :direction :output
				   :if-does-not-exist :create
				   :element-type '(unsigned-byte 8))
	     (write-image-format image stream format)))
	  (stream
	   ;; @@@ make sure it's a binary stream or convert it with flexistreams
	   ;; like above?
	   (write-image-format image file-or-stream format))))))

;; EOF
