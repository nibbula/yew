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

(defconstant +max-alpha+ 255
  "Maximum alpha value, i.e. no transparency.")

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

(defkeymap *image-viewer-keymap*)
(defkeymap *image-viewer-escape-keymap*)
  
(defclass image-inator (terminal-inator)
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
  (with-slots (x zoom) o
    (declare (type fixnum x))
    (decf x (* n (truncate 1 zoom)))
    (when (<= x 0)
      (setf x 0))))

(defun down (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (y image zoom) o
    (declare (type fixnum y))
    (incf y (* n (truncate 1 zoom)))
    (when (>= y (image-height image))
      (setf y (1- (image-height image))))))
  
(defun up (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (y zoom) o
    (declare (type fixnum y))
    (decf y (* n (truncate 1 zoom)))
    (when (<= y 0)
      (setf y 0))))

(defun right (o &optional (n 1))
  (declare (type fixnum n))
  (with-slots (x image zoom) o
    (declare (type fixnum x))
    (incf x (* n (truncate 1 zoom)))
    (when (>= x (image-width image))
      (setf x (1- (image-width image))))))

(defmethod forward-unit	 ((o image-inator)) (right o))
(defmethod backward-unit ((o image-inator)) (left o))
(defmethod next		 ((o image-inator)) (down o))
(defmethod previous	 ((o image-inator)) (up o))

(defun left-by-increment  (o) (left  o (image-inator-increment o)))
(defun down-by-increment  (o) (down  o (image-inator-increment o)))
(defun up-by-increment    (o) (up    o (image-inator-increment o)))
(defun right-by-increment (o) (right o (image-inator-increment o)))

(defmethod move-to-top ((o image-inator))
  (setf (image-inator-x o) 0
	(image-inator-y o) 0))

(defmethod move-to-bottom ((o image-inator))
  (with-slots (x y image zoom) o
    (declare (type fixnum x y))
    (setf x (max 0 (- (image-width image) (* (tt-width) (truncate 1 zoom))))
	  y (max 0 (- (image-height image) (* (tt-height) (truncate 1 zoom)))))))

(defmethod next-page ((o image-inator))
  (with-slots (y image zoom) o
    (declare (type fixnum y))
    (let ((step (truncate 1 zoom)))
      (setf y (min (+ y (* (tt-height) step))
		   (max 0 (- (image-height image) (* (tt-height) step))))))))

(defmethod previous-page ((o image-inator))
  (with-slots (y zoom) o
    (declare (type fixnum y))
    (let ((step (truncate 1 zoom)))
      (setf y (max (- y (* (tt-height) step)) 0)))))

(defun beginning-of-line (o)
  (setf (image-inator-x o) 0))

(defun end-of-line (o)
  (with-slots (x image zoom) o
    (declare (type fixnum x))
    (setf x (max 0 (- (image-width image) (* (tt-width) (truncate 1 zoom)))))))

(defun zoom-in (o)
  (with-slots (zoom) o
    (setf zoom (* zoom 1.10))))

(defun zoom-out (o)
  (with-slots (zoom) o
    (setf zoom (* zoom .90))))

(defun zoom-reset (o)
  (with-slots (zoom) o
    (setf zoom 1.0)))

(defun fit-width-to-window (o)
  (with-slots (zoom image) o
    (with-slots (width height) image
      ;; width * zoom = tt-width
      ;; zoom = tt-width / width
      (setf zoom
	    (min 1.0 ;; teporarily @@@
		 (float (/ (tt-width) width))))
      )))

(defun fit-height-to-window (o)
  (with-slots (zoom image) o
    (with-slots (width height) image
      (setf zoom
	    (min 1.0 ;; teporarily @@@
		 (float (/ (tt-height) height))))
      )))

(defun reset-image (o)
  "Reset some viewer parameters when we switch images."
  (with-slots (subimage looping) o
    (setf subimage 0
	  looping nil)))

(defun toggle-looping (o)
  (with-slots (looping) o
    (setf looping (not looping))))

(defun report-and-continue (c)
  (typecase c
    (simple-condition
     (apply #'pause (simple-condition-format-control c)
	    (simple-condition-format-arguments c))
     (continue))
    (t
     (pause "Error: ~s" c))))

(defmacro with-image-error-handling ((file-name) &body body)
  `(handler-bind
       ((unknown-image-type #'report-and-continue)
	(non-image-file #'report-and-continue))
     (handler-case
	 (progn
	   ,@body)
       (cl-jpeg:jpeg-error (c)
	 (pause "Error: ~a ~a" ,file-name c))
       (simple-error (c)
	 (pause "Error: ~a ~a" ,file-name c)))))

(defmethod next-file ((o image-inator))
  (with-slots (file-list file-index image) o
    (flet ((next ()
	     (if (< file-index (1- (length file-list)))
		 (incf file-index)
		 (return-from next-file nil))))
      (let (img)
	(loop :with file-name
	   :while (not img) :do
	   (next)
	   (setf file-name (nth file-index file-list))
	   (with-image-error-handling (file-name)
	     (setf img (read-image file-name))))
	(setf image img)
	(reset-image o)))))

(defmethod previous-file ((o image-inator))
  (with-slots (file-list file-index image) o
    (flet ((prev ()
	     (if (> file-index 0)
		 (decf file-index)
		 (return-from previous-file nil))))
      (let (img)
	(loop :with file-name
	   :while (not img) :do
	   (prev)
	   (setf file-name (nth file-index file-list))
	   (with-image-error-handling (file-name)
	     (setf img (read-image file-name))))
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
  (let* ((key-seq (read-key-sequence o)) ;;(tt-get-key))
	 (action (key-sequence-binding key-seq (inator-keymap o))))
    (if action
	(message o "~a is bound to ~a" (key-sequence-string key-seq) action)
	(message o "~a is not defined" (key-sequence-string key-seq)))))

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

(set-keymap *image-viewer-keymap*
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
    (#\+		  . zoom-in)
    (#\-		  . zoom-out)
    (#\=		  . zoom-reset)
    (#\f		  . fit-width-to-window)
    (#\F		  . fit-height-to-window)
    (,(meta-char #\n)     . next-file)
    (,(meta-char #\p)     . previous-file)
    (,(meta-char #\l)     . toggle-looping)
    (#\t     		  . toggle-looping)
    (,(meta-char #\=)	  . binding-of-key)
    (,(meta-char #\escape) . eval-expression)
    (,(meta-char #\m)     . toggle-modeline)
    (#\?		  . help)
    (,(ctrl #\@)	  . set-mark)))

(setf *image-viewer-escape-keymap* (build-escape-map *image-viewer-keymap*))

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
	      (let ((line
		     (format nil "~a ~dx~d ~a~a~a~f% <~d-~d ~d-~d>"
			     name width height position file-count frame-count
			     zoom start-x end-x start-y end-y)))
		(tt-write-string (subseq line 0 (min (length line)
						     (1- (tt-width))))))))))))

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
  (with-slots (x y image subimage zoom) o
    (with-slots (subimages) image
      (with-slots ((si-x x) (si-y y) width height) (aref subimages subimage)
	(let* ((step (truncate 1 zoom))
	       (start-x (if (> x si-x) (- x si-x) 0))
	       (start-y (if (> y si-y) (- y si-y) 0))
	       (end-x (min width (- width
				    (- (+ si-x width)
				       (+ x (* step (1- (tt-width))))))))
	       (end-y (min height (- height
				     (- (+ si-y height)
					(+ y (1- (* step (1- (tt-height))))))))))
	  (values start-x end-x start-y end-y))))))

#| 
Could double vertical resolution with #\upper_half_block
setting forground & background
pixels more square
But also greatly increasing # of chars output.

(defparameter *double-buf* nil)

(defun show-image-double (inator)
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
	  (let ((step (max 1 (truncate 1 zoom))))
	    ;;(pause "zoom = ~s step = ~s" zoom step)
	    (when (> si-y y)
	      ;;(tt-move-to (max y (truncate (- si-y y) step)) 0))
	      (tt-move-to (max y (truncate (- (+ si-y start-y) y) step)) 0))
	    (loop :with r = 0 :and g = 0 :and b = 0 :and a = 0
	       :for iy fixnum :from start-y :below end-y :by step :do
	       (when (> si-x x)
		 ;;(tt-move-to-col (max x (truncate (- si-x x) step))))
		 (tt-move-to-col (max x (truncate (- (+ si-x start-x) x) step))))
	       (loop 
		  :for ix fixnum :from start-x :below end-x :by step :do
		  (setf r (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 0))))
		  (setf g (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 1))))
		  (setf b (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 2))))
		  (setf a (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 3))))
		  (if (not (zerop a))
		      (progn
			(tt-color nil (vector
				       (truncate r (* step step))
				       (truncate g (* step step))
				       (truncate b (* step step))))
			(tt-write-char #\space)
			(tt-color nil nil))
		      (progn
			(tt-forward 1)))
		  (setf r 0 g 0 b 0 a 0))
	       (tt-color nil nil)
	       (tt-write-char #\newline))))
	(tt-color nil nil)
	(tt-move-to (1- (tt-height)) 0)
	(when show-modeline
	  (show-status inator))))))
|#

#|
(defun term-mover (x &optional y)
  (declare (type fixnum x y))
  (if y
      (tt-move-to y x)
      (tt-move-to-col x)))

(defun print-mover (x &optional y)
  (declare (type fixnum x y))
  (if y
      (tt-move-to y x)
      (tt-move-to-col x)))

(defun print-image (x y zoom image subimage mover)
  (declare (type fixnum x y) (type float zoom))
  (flet ((move-to (y x) (funcall mover x y))
	 (move-to-col (col) (funcall mover x)))
    (with-slots (name subimages) image
      (with-slots ((si-x x) (si-y y) width height data)
	  (aref subimages subimage)
	(declare (type fixnum si-x si-y width height))
	(multiple-value-bind (start-x end-x start-y end-y) (clip inator)
	  (let ((step (max 1 (truncate 1 zoom))))
	    ;;(pause "zoom = ~s step = ~s" zoom step)
	    (when (> si-y y)
	      ;;(tt-move-to (max y (truncate (- si-y y) step)) 0))
	      (move-to (max y (truncate (- (+ si-y start-y) y) step)) 0))
	    (loop :with r = 0 :and g = 0 :and b = 0 :and a = 0
	       :for iy fixnum :from start-y :below end-y :by step :do
	       (when (> si-x x)
		 ;;(tt-move-to-col (max x (truncate (- si-x x) step))))
		 (tt-move-to-col (max x (truncate (- (+ si-x start-x) x) step))))
	       (loop 
		  :for ix fixnum :from start-x :below end-x :by step :do
		  (setf r (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 0))))
		  (setf g (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 1))))
		  (setf b (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 2))))
		  (setf a (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 3))))
		  (if (not (zerop a))
		      (progn
			(tt-color nil (vector
				       (truncate r (* step step))
				       (truncate g (* step step))
				       (truncate b (* step step))))
			(tt-write-char #\space)
			(tt-color nil nil))
		      (progn
			(tt-forward 1)))
		  (setf r 0 g 0 b 0 a 0))
	       (tt-color nil nil)
	       (tt-write-char #\newline))))
	(tt-color nil nil)
	(tt-move-to (1- (tt-height)) 0)
	(when show-modeline
	  (show-status inator))))))
|#

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
	  (let ((step (max 1 (truncate 1 zoom))))
	    ;;(pause "zoom = ~s step = ~s" zoom step)
	    (when (> si-y y)
	      ;;(tt-move-to (max y (truncate (- si-y y) step)) 0))
	      (tt-move-to (max y (truncate (- (+ si-y start-y) y) step)) 0))
	    (loop :with r = 0 :and g = 0 :and b = 0 :and a = 0
	       :for iy fixnum :from start-y :below end-y :by step :do
	       (when (> si-x x)
		 ;;(tt-move-to-col (max x (truncate (- si-x x) step))))
		 (tt-move-to-col (max x (truncate (- (+ si-x start-x) x) step))))
	       (loop 
		  :for ix fixnum :from start-x :below end-x :by step :do
		  (setf r (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 0))))
		  (setf g (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 1))))
		  (setf b (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 2))))
		  (setf a (loop :for av-y :from 0 :below step :sum
			     (loop :for av-x :from 0 :below step
				:sum (aref data
					   (min (1- width) (+ ix av-x))
					   (min (1- height) (+ iy av-y)) 3))))
		  (if (not (zerop a))
		      (progn
			(tt-color nil (vector
				       (truncate r (* step step))
				       (truncate g (* step step))
				       (truncate b (* step step))))
			(tt-write-char #\space)
			(tt-color nil nil))
		      (progn
			(tt-forward 1)))
		  (setf r 0 g 0 b 0 a 0))
	       (tt-color nil nil)
	       (tt-write-char #\newline))))
	(tt-color nil nil)
	(tt-move-to (1- (tt-height)) 0)
	(when show-modeline
	  (show-status inator))))))

(defmethod update-display ((o image-inator))
  "Update the image viewer display."
  ;;(call-next-method)
  (show-image o))

(defun make-image-array (width height)
  (make-array `(,width ,height 4)	; R G B A
	      :element-type '(unsigned-byte 8)))

(defun read-png (file-or-stream)
  (let* ((png (if (streamp file-or-stream)
		 (png-read:read-png-datastream file-or-stream)
		 (png-read:read-png-file file-or-stream)))
	 (array (make-image-array (png-read:width png) (png-read:height png)))
	 (dims (array-dimensions (png-read:image-data png)))
	 use-alpha)
    ;; We only really have to use our own array because of alpha.
    (cond
      ((= 2 (length dims))
       ;; Grayscale
       (case (png-read:bit-depth png)
	 (8
	  (loop :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf (aref array x y 0) (aref (png-read:image-data png) x y)
		      (aref array x y 1) (aref (png-read:image-data png) x y)
		      (aref array x y 2) (aref (png-read:image-data png) x y)
		      (aref array x y 3) +max-alpha+))))
	 (16
	  (loop :with pixel
	     :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf pixel
		      (ash (logand #xff00 (aref (png-read:image-data png) x y))
			   -8)
		      (aref array x y 0) pixel
		      (aref array x y 1) pixel
		      (aref array x y 2) pixel
		      (aref array x y 3) +max-alpha+))))))
      ((= 3 (length dims))
       ;; Color, probably RGB or RGBA
       (case (png-read:colour-type png)
	 (:greyscale-alpha
	  (loop :with pixel
	     :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf pixel (aref (png-read:image-data png) x y 0)
		      (aref array x y 0) pixel
		      (aref array x y 1) pixel
		      (aref array x y 2) pixel
		      (aref array x y 3)
		      (aref (png-read:image-data png) x y 1)))))
	 ((:truecolor :truecolor-alpha :indexed-colour)
	  (setf use-alpha (= 4 (array-dimension (png-read:image-data png) 2)))
	  (loop :for y :from 0 :below (png-read:height png) :do
	     (loop :for x :from 0 :below (png-read:width png) :do
		(setf (aref array x y 0) (aref (png-read:image-data png) x y 0)
		      (aref array x y 1) (aref (png-read:image-data png) x y 1)
		      (aref array x y 2) (aref (png-read:image-data png) x y 2)
		      (aref array x y 3) (if use-alpha
					     (aref (png-read:image-data png)
						   x y 3)
					     +max-alpha+)))))))
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
				 :data array)))))

(defun read-jpeg (file-or-stream)
  (multiple-value-bind (data height width colors)
      (cl-jpeg:decode-image file-or-stream)
    (when (not (member colors '(1 3)))
      (error "I don't know how to handle a ~d color JPEG." colors))
    ;; convert to multi-dimensional array
    (let ((array (make-image-array width height))
	  (i 0))
      ;;(pause "JPEG ~a x ~a ~d" width height (length data))
      (loop :for y :from 0 :below height :do
	 (loop :for x :from 0 :below width :do
	    (case colors
	      (3
	       (setf (aref array x y 3) +max-alpha+
		     (aref array x y 2) (aref data i)
		     (aref array x y 1) (aref data (+ i 1))
		     (aref array x y 0) (aref data (+ i 2)))
	       (incf i 3))
	      (1
	       (setf (aref array x y 3) +max-alpha+
		     (aref array x y 2) (aref data i)
		     (aref array x y 1) (aref data i)
		     (aref array x y 0) (aref data i))
	       (incf i)))))
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
	 (r 0) (g 0) (b 0) (a 0)
	 (i 0) color-index)
    (declare (type fixnum i)
	     (type (unsigned-byte 8) r g b))
    (loop :for y fixnum :from 0 :below (skippy:height image) :do
       (loop :for x fixnum :from 0 :below (skippy:width image) :do
	  (setf color-index (aref (skippy:image-data image) i))
	  (if (or (not (skippy:transparency-index image))
		  (/= color-index (skippy:transparency-index image)))
	      (setf (values r g b) (skippy:color-rgb
				  (skippy:color-table-entry
				   color-table color-index))
		    a +max-alpha+)
	      (setf r 0 g 0 b 0 a 0))
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
		(aref array x y 2) b
		(aref array x y 3) a)
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
  :accepts (:sequence :stream)
  "View an image."
  (view-images (or images lish:*input* *standard-input*)))

#|
(defun cat-images (files)
  (if (not files)
      (print-image *standard-input*)
      (map nil #'print-image files)))

#+lish
(lish:defcommand imgcat
  ((images pathname :repeating t :help "Image to cat."))
  :accepts (:sequence :stream)
  "Print an image as terminal talk."
  (cat-images (or images lish:*input* *standard-input*)))
|#

;; EOF
