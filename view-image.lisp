;;
;; view-image.lisp - Image viewer
;;

(defpackage :view-image
  (:documentation "Image viewer")
  (:use :cl :dlib :keymap :char-util :terminal :terminal-ansi :inator
	:terminal-inator :magic)
  (:export
   #:view-image
   ))
(in-package :view-image)

;; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)
;; 		   (compilation-speed 0)))

(defstruct sub-image
  (x      0 :type fixnum)
  (y      0 :type fixnum)
  (width  0 :type fixnum)		; width in pixels
  (height 0 :type fixnum)		; height in pixels
  (delay  0 :type fixnum)		; ms to delay
  data)					; image data

(defstruct image
  name					; image name, usually a file name
  (width  0 :type fixnum)		; width in pixels
  (height 0 :type fixnum)		; height in pixels
  subimages)				; array of subimages, nil if none

(defkeymap *image-viewer-keymap*
  `((#\escape		  . *image-viewer-escape-keymap*)
    (#\q	  	  . quit)
    (#\h		  . backward-unit)
    (#\j		  . next)
    (#\k		  . previous)
    (#\l		  . forward-unit)
    (#\H		  . left-by-increment)
    (#\J		  . down-by-increment)
    (#\K		  . up-by-increment)
    (#\L		  . right-by-increment)
    (#\n		  . next-sub-image)
    (#\p		  . previous-sub-image)
    (:down		  . next)
    (:up		  . previous)
    (:left		  . backward-unit)
    (:right		  . forard-unit)
    (,(ctrl #\F)	  . next-page)
    (#\space	  	  . next-page)
    (,(ctrl #\V)	  . next-page)
    (:npage		  . next-page)
    (,(ctrl #\B)	  . previous-page)
    (#\b	          . previous-page)
    (,(meta-char #\v)	  . previous-page)
    (:ppage		  . previous-page)
    (#\>		  . move-to-bottom)
    (,(meta-char #\>)     . move-to-bottom)
    (:end		  . move-to-bottom)
    (#\<		  . move-to-top)
    (,(meta-char #\<)     . move-to-top)
    (:home		  . move-to-top)
    (,(ctrl #\a)	  . beginning-of-line)
    (,(ctrl #\e)	  . end-of-line)
    (,(meta-char #\n)     . next-file)
    (,(meta-char #\p)     . previous-file)
    (,(meta-char #\l)     . toggle-looping)
    (#\t     		  . toggle-looping)
    (,(meta-char #\=)	  . binding-of-key)
    (,(meta-char #\escape) . eval-expression)
    (,(meta-char #\m)     . toggle-modeline)
    (#\?		  . help)
    (,(ctrl #\@)	  . set-mark)))

(defparameter *image-viewer-escape-keymap*
  (build-escape-map *image-viewer-keymap*))
  
(defclass image-inator (inator)
  ((image
    :initarg :image :accessor image-inator-image
    :documentation "The image to viewer.")
   (x
    :initarg :x :accessor image-inator-x :initform 0 :type fixnum
    :documentation "Horizontal coordinate.")
   (y
    :initarg :x :accessor image-inator-y :initform 0 :type fixnum
    :documentation "Vertical coordinate.")
   (zoom
    :initarg :zoom :accessor image-inator-zoom :initform 1.0 :type float
    :documentation "Magnification factor.")
   (subimage
    :initarg :subimage :accessor image-inator-subimage :initform 0 :type fixnum
    :documentation "The sub-image index number.")
   (looping
    :initarg :looping :accessor image-inator-looping :initform nil :type boolean
    :documentation "True if we are looping.")
   (increment
    :initarg :increment :accessor image-inator-increment
    :initform 20 :type fixnum
    :documentation "Unit for operations.")
   (file-list
    :initarg :file-list :accessor image-inator-file-list
    :initform nil :type list
    :documentation "List of files to view.")
   (file-index
    :initarg :file-index :accessor image-inator-file-index
    :initform 0 :type fixnum
    :documentation "Position in FILE-LIST that we are viewing.")
   (show-modeline
    :initarg :show-modeline :accessor image-inator-show-modeline
    :initform t :type boolean
    :documentation "True to show the mode line.")
   (message
    :initarg :message :accessor image-inator-message :initform nil
    :documentation "Message to show.")
   )
  (:default-initargs
   :keymap	`(,*image-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "An image viewer."))

(defvar *image-viewer* nil
  "The current image viewer.")

(define-condition unknown-image-type (simple-error) ()
  (:default-initargs
   :format-control "~s is an image type ~a, which I don't know how to handle."))

(define-condition non-image-file (simple-error) ()
  (:default-initargs
   :format-control "~s doesn't seem to be an image"))

(defun left (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (x) o
    (declare (type fixnum x))
    (decf x n)
    (when (<= x 0)
      (setf x 0))))

(defun down (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (y image) o
    (declare (type fixnum y))
    (incf y n)
    (when (>= y (image-height image))
      (setf y (1- (image-height image))))))
  
(defun up (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (y) o
    (declare (type fixnum y))
    (decf y n)
    (when (<= y 0)
      (setf y 0))))

(defun right (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (x image) o
    (declare (type fixnum x))
    (incf x n)
    (when (>= x (image-width image))
      (setf x (1- (image-width image))))))

(defmethod forward-unit	 ((o image-inator)) (right o))
(defmethod backward-unit ((o image-inator)) (left o))
(defmethod next		 ((o image-inator)) (down o))
(defmethod previous	 ((o image-inator)) (up o))

(defun left-by-increment  (o) (left  o (image-inator-increment o)))
(defun down-by-increment  (o) (down  o (image-inator-increment o)))
(defun up-by-increment 	  (o) (up    o (image-inator-increment o)))
(defun right-by-increment (o) (right o (image-inator-increment o)))

(defmethod move-to-top ((o image-inator))
  (setf (image-inator-x o) 0
	(image-inator-y o) 0))

(defmethod move-to-bottom ((o image-inator))
  (with-slots (x y image) o
    (declare (type fixnum x y))
    (setf x (max 0 (- (image-width image) (tt-width)))
	  y (max 0 (- (image-height image) (tt-height))))))

(defmethod next-page ((o image-inator))
  (with-slots (y image) o
    (declare (type fixnum y))
    (setf y (min (+ y (tt-height))
		 (max 0 (- (image-height image) (tt-height)))))))

(defmethod previous-page ((o image-inator))
  (with-slots (y) o
    (declare (type fixnum y))
    (setf y (max (- y (tt-height)) 0))))

(defun beginning-of-line (o)
  (setf (image-inator-x o) 0))

(defun end-of-line (o)
  (with-slots (x image) o
    (declare (type fixnum x))
    (setf x (max 0 (- (image-width image) (tt-width))))))

(defun report-and-continue (c)
  (apply #'pause (simple-condition-format-control c)
	 (simple-condition-format-arguments c))
  (continue))

(defun reset-image (o)
  "Reset some viewer parameters when we switch images."
  (with-slots (subimage looping) o
    (setf subimage 0
	  looping nil)))

(defun toggle-looping (o)
  (with-slots (looping) o
    (setf looping (not looping))))

(defun next-file (o)
  (with-slots (file-list file-index image) o
    (flet ((next ()
	     (if (< file-index (1- (length file-list)))
		 (incf file-index)
		 (return-from next-file nil))))
      (let (img)
	(handler-bind
	    ((unknown-image-type #'report-and-continue)
	     (non-image-file #'report-and-continue))
	  (loop
	     :while (not img) :do
	     (next)
	     (setf img (read-image (nth file-index file-list)))))
	(setf image img)
	(reset-image o)))))

(defun previous-file (o)
  (with-slots (file-list file-index image) o
    (flet ((prev ()
	     (if (> file-index 0)
		 (decf file-index)
		 (return-from previous-file nil))))
      (let (img)
	(handler-bind
	    ((unknown-image-type #'report-and-continue)
	     (non-image-file #'report-and-continue))
	  (loop
	     :while (not img) :do
	     (prev)
	     (setf img (read-image (nth file-index file-list)))))
	(setf image img)
	(reset-image o)))))

(defun next-sub-image (o)
  (with-slots (image subimage) o
    (with-slots (subimages) image
      (when (and subimages (< subimage (1- (length subimages))))
	(incf subimage)))))

(defun previous-sub-image (o)
  (with-slots (image subimage) o
    (with-slots (subimages) image
      (when (and subimages (> subimage 0))
	(decf subimage)))))

(defmethod redraw ((o image-inator))
  (tt-clear))

(defmethod message ((o image-inator) format-string &rest args)
  (setf (image-inator-message o) 
	(apply #'format nil format-string args)))

(defun say (format-string &rest args)
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-write-string (apply #'format nil format-string args)))

(defun pause (format-string &rest args)
  (apply #'say format-string args)
  (tt-write-string " --More--")
  (tt-get-key))

(defun binding-of-key (o)
  (say "Press a key: ")
  (let* ((key (tt-get-key))
	 (action (key-definition key (inator-keymap o))))
    (if action
	(message o "~a is bound to ~a" (nice-char key) action)
	(message o "~a is not defined" (nice-char key)))))

(defun eval-expression (o)
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-cursor-on)
  (tt-finish-output)
  (let (result)
    (handler-case
	(setf result (eval (read-from-string (rl:rl :prompt "Eval: ") nil nil)))
      (condition (c)
	(say "~w" c)
	(continue)))
    (message o "~s" result))
  (tt-cursor-off))

(defun toggle-modeline (o)
  (with-slots (show-modeline) o
    (setf show-modeline (not show-modeline))))

(defmethod await-event ((o image-inator))
  "Image viewer event."
  (with-slots (looping subimage image) o
    (with-slots (subimages) image
      (if (and looping subimages)
	  (let ((t-o (truncate (sub-image-delay (aref subimages subimage)) 10))
		result)
	    (setf result
		  (terminal-ansi::get-char *terminal* :timeout t-o)) ;; XXX
	    (when (not result)
	      (if (= subimage (1- (length subimages)))
		  (setf subimage 0)
		  (next-sub-image o)))
	    result)
	  (tt-get-key)))))

(defun show-status (o)
  "Display the status/message line."
  (with-slots (image message file-index file-list subimage zoom x y) o
    (with-slots (name width height subimages) image
      (if message
	  (progn
	    (tt-write-string message)
	    (setf message nil))
	  (let ((position (if (and (plusp x) (plusp y))
			      (format nil "+~d+~d " x y) ""))
		(file-count (if (> (length file-list) 1)
				(format nil "(file ~d of ~d) "
					file-index (length file-list)) ""))
		(frame-count (if (> (length subimages) 1)
				 (format nil "[frame ~d of ~d] " subimage
					 (length subimages)) "")))
	    (multiple-value-bind (start-x end-x start-y end-y) (clip o)
	      (tt-format "~a ~dx~d ~a~a~a~f% <~d-~d ~d-~d>"
			 name width height position file-count frame-count
			 zoom start-x end-x start-y end-y)))))))

#|

        x
        +-----------------+
     sx |    sx + w       |
     +---w----+           |
     |  |     |           |
     +--------+           |
        |                 |
        |                 |
        +-----------------+

0-------------------------------------w

        x
        +-----------------+
        |           sx    |   sx + w
        |           +---w----+
        |           |     |  |
        |           +--------+
        |                 |
        |                 |
        +-----------------+

0-------------------------------------w

        x                 x+tw
     30 +-------60--------+
     sx |                 |    sx + w
     +-----------w-------------+
     |  |                 |    |
     +-------------------------+
     0  |                 |    110
        |                 |
        +-----------------+
        40                100

(min (- w (abs (- x sx)))
     tt-w

|#

(defun clip (o)
  (with-slots (x y zoom image subimage) o
    (with-slots (subimages) image
      (with-slots ((si-x x) (si-y y) width height) (aref subimages subimage)
	(let* ((start-x (if (> x si-x) (- x si-x) 0))
	       (start-y (if (> y si-y) (- y si-y) 0))
	       (end-x (min width (- width (- (+ si-x width)
					     (+ x (1- (tt-width)))))))
	       (end-y (min height (- height (- (+ si-y height)
					       (+ y (- (tt-height) 2)))))))
	  (values start-x end-x start-y end-y))))))

(defun show-image (inator)
  (with-slots (x y zoom message file-index file-list image subimage looping
	       show-modeline) inator
    (declare (type fixnum x y) (type float zoom))
    (with-slots (name subimages) image
      (with-slots ((si-x x) (si-y y) width height data)
	  (aref subimages subimage)
	(declare (type fixnum si-x si-y width height))
	(tt-home)
	(when (not looping)
	  (tt-clear))
	(multiple-value-bind (start-x end-x start-y end-y) (clip inator)
	  (when (> si-y y)
	    (tt-move-to (max y (- si-y y)) 0))
	  (loop :for iy fixnum :from start-y :below end-y :do
	     (when (> si-x x)
	       (tt-move-to-col (max x (- si-x x))))
	     (loop :with r :and g :and b
		:for ix fixnum :from start-x :below end-x :do
		(setf r (aref data ix iy 0)
		      g (aref data ix iy 1)
		      b (aref data ix iy 2))
		(tt-color nil (vector r g b))
		(tt-write-char #\space))
	     (tt-write-char #\newline)))
	(tt-color nil nil)
	(tt-move-to (1- (tt-height)) 0)
	(when show-modeline
	  (show-status inator))))))

(defmethod update-display ((o image-inator))
  "Update the image viewer display."
  ;;(call-next-method)
  (show-image o))

(defun make-image-array (width height)
  (make-array `(,width ,height 3)
	      :element-type '(unsigned-byte 8)))

(defun read-png (file-or-stream)
  (let ((png (if (streamp file-or-stream)
		 (png-read:read-png-datastream file-or-stream)
		 (png-read:read-png-file file-or-stream))))
    (make-image :name file-or-stream
		:width (png-read:width png)
		:height (png-read:height png)
		:subimages
		(vector
		 (make-sub-image :x 0 :y 0
				 :width (png-read:width png)
				 :height (png-read:height png)
				 :data (png-read:image-data png))))))

(defun read-jpeg (file-or-stream)
  (multiple-value-bind (data height width)
      (cl-jpeg:decode-image file-or-stream)
    ;; convert to multi-dimensional array
    (let ((array (make-image-array width height))
	  (i 0))
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below width :do
	    (setf (aref array x y 2) (aref data i)
		  (aref array x y 1) (aref data (+ i 1))
		  (aref array x y 0) (aref data (+ i 2)))
	    (incf i 3)))
      (make-image :name file-or-stream
		  :width width :height height
		  :subimages
		  (vector
		   (make-sub-image :x 0 :y 0
				   :width width
				   :height height
				   :data array))))))

(defun get-gif-image (gif image-number)
  (let* ((image (elt (skippy:images gif) image-number))
	 (array (make-image-array (skippy:width image) (skippy:height image)))
	 (color-table (or (skippy:color-table image)
			  (skippy:color-table gif)))
	 (r 0) (g 0) (b 0) #| (a 0) |#
	 (i 0))
    (declare (type fixnum i)
	     (type (unsigned-byte 8) r g b))
    (loop :for y fixnum :from 0 :below (skippy:height image) :do
       (loop :for x fixnum :from 0 :below (skippy:width image) :do
	  (setf (values r g b)
		(skippy:color-rgb
		 (skippy:color-table-entry
		  color-table
		  (aref (skippy:image-data image) i))))
	  ;; (cond
	  ;;   ((= (length colors) 1)
	  ;;    ;; Assume it's grayscale.
	  ;;    (setf (aref array x y 0) r
	  ;; 	   (aref array x y 1) r
	  ;; 	   (aref array x y 2) r))
	  ;;   ((= (length colors) 3)
	  ;;     (setf (aref array x y 0) r
	  ;; 	    (aref array x y 1) g
	  ;; 	    (aref array x y 2) b))
	  ;;   (t
	  ;;    (error "Unknown color format in GIF pixel.")))
	  (setf (aref array x y 0) r
		(aref array x y 1) g
		(aref array x y 2) b)
	  (incf i)))
    (make-sub-image :x      (skippy:left-position image)
		    :y      (skippy:top-position image)
		    :width  (skippy:width image)
		    :height (skippy:height image)
		    :delay  (skippy:delay-time image)
		    :data   array)))

(defun read-gif (file-or-stream)
  (let* ((gif (if (streamp file-or-stream)
		  (skippy:read-data-stream file-or-stream)
		  (skippy:load-data-stream file-or-stream)))
	 (image-count (length (skippy:images gif)))
	 (sub (make-array image-count)))
    (declare (type fixnum image-count))
    (loop :for i fixnum :from 0 :below image-count :do
       (setf (aref sub i) (get-gif-image gif i)))
    (make-image :width (skippy:width gif) :height (skippy:height gif)
		:name file-or-stream
		:subimages sub)))

(defparameter *image-reader-alist*
  `(("png"  . read-png)
    ("jpeg" . read-jpeg)
    ("gif"  . read-gif)))

(defun read-image (file-or-stream)
  (let* ((thing (if (stringp file-or-stream)
		    (pathname file-or-stream) file-or-stream))
	 (type (guess-content-type thing)))
    (if (equal (content-type-category type) "image")
	(let ((func (cdr (assoc (content-type-name type) *image-reader-alist*
				:test #'equal))))
	  (if (and func (fboundp func))
	      (funcall (symbol-function func) file-or-stream)
	      (cerror "Skip the image."
		      'unknown-image-type
		      :format-arguments `(,file-or-stream
					  ,(content-type-name type)))))
	(cerror "Skip the file"
		'non-image-file :format-arguments `(,file-or-stream)))))

(defun view-image (file-or-stream &key file-list)
  (with-terminal (:ansi)
    (let* (image *image-viewer*)
      (loop :with file = file-or-stream :and list = file-list
	 :while (and (or file list) (not (setf image (read-image file))))
	 :do (setf list (cdr list)
		   file (car list)))
      (when (not image)
	(error "No more files to try."))
      (setf *image-viewer* (make-instance 'image-inator
					  :image image
					  :file-list file-list))
      (unwind-protect
        (progn
	  (tt-cursor-off)
	  (event-loop *image-viewer*))
	(tt-cursor-on))
      (inator-quit-flag *image-viewer*))))

(defun view-images (files)
  (if (not files)
      (view-image *standard-input*)
      (view-image (first files) :file-list files)))

#+lish
(lish:defcommand view-image
  ((images pathname :repeating t :help "Image to view."))
  "View an image."
  (view-images (or images *standard-input*)))

;; EOF
