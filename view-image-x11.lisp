;;
;; view-image-x11.lisp - Image viewer X11 driver.
;;

;; This whole package is just a temporary hack for my enjoyment/suffering.

(defpackage :view-image-x11
  (:documentation "Image viewer X11 driver.")
  (:use :cl :dlib :dlib-misc :char-util :inator :view-image
	;; :image
	:xlib
	:terminal)
  (:shadowing-import-from :xlib #:process-event)
  (:export
   #:x11-image-inator
   ))
(in-package :view-image-x11)

(defclass image-x11-inator (image-inator)
  ((display
    :initarg :display :accessor image-x11-inator-display
    :initform nil
    :documentation "The X display.")
   (window
    :initarg :window :accessor image-x11-inator-window
    :initform nil
    :documentation "The window.")
   (depth
    :initarg :depth :accessor image-x11-inator-depth :initform nil
    :documentation "The depth of the display.")
   (window-width
    :initarg :window-width :accessor image-x11-inator-window-width
    :initform 0 :type fixnum
    :documentation "The width of the window in pixels.")
   (window-height
    :initarg :window-height :accessor image-x11-inator-window-height
    :initform 0 :type fixnum
    :documentation "The height of window in pixels.")
   (own-window
    :initarg :own-window :accessor own-window
    :initform nil :type boolean
    :documentation "True to use our own window.")
   (draw-gc
    :initarg :draw-gc :accessor draw-gc :initform nil
    :documentation "Graphics context for drawing.")
   (erase-gc
    :initarg :erase-gc :accessor erase-gc :initform nil
    :documentation "Graphics context for erasing.")
   (overlay-gc
    :initarg :overlay-gc :accessor overlay-gc :initform nil
    :documentation "Graphics context for erasing.")
   (font
    :initarg :font :accessor inator-font :initform nil
    :documentation "The font for text.")
   (need-to-redraw
    :initarg :need-to-redraw :accessor need-to-redraw
    :initform nil :type boolean
    :documentation "True if we need to redraw the screen.")
   (eraser-window
    :initarg :eraser-window :accessor eraser-window :initform nil 
    :documentation "Window to erase with.")
   (ximages
    :initarg :ximages :accessor ximages
    :initform nil
    :documentation "Array of image handles paralelling the image-subimages.")
   (ximages-mask
    :initarg :ximages-mask :accessor ximages-mask
    :initform nil
    :documentation "Array of image masks paralelling the image-subimages.")
   (ximage-data
    :initarg :ximage-data :accessor ximage-data
    :type (simple-array (unsigned-byte 32) (* *))
    :initform (the (simple-array (unsigned-byte 32) (* *))
		   (make-array '(1 1)
			       :initial-element (coerce 0 '(unsigned-byte 32))
			       :element-type '(unsigned-byte 32)))
    :documentation "Data in X format.")
   (ximage-mask-data
    :initarg :ximage-mask-data :accessor ximage-mask-data
    ;; :type (simple-array (unsigned-byte 32) (* *))
    ;; :initform (the (simple-array (unsigned-byte 32) (* *))
    ;; 		   (make-array '(1 1)
    ;; 			       :initial-element (coerce 0 '(unsigned-byte 32))
    ;; 			       :element-type '(unsigned-byte 32)))
    :documentation "Mask data in X format.")
   )
  (:default-initargs
   :move-object-mode t)
  (:documentation "Image viwer for X11."))

(defmethod initialize-instance
    :after ((o image-x11-inator) &rest initargs &key &allow-other-keys)
  "Initialize a image-x11-inator."
  (declare (ignore initargs))
  )

(defun get-display-from-environment ()
  "Return the display host and the the display number from the environment."
  (let ((display (nos:environment-variable "DISPLAY")) s)
    (and display
	 (setf s (split-sequence #\: display))
	 (values (first s) (parse-integer (second s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (d-add-feature :clx-use-classes))

(defun make-window-from (&key display id)
  #-clx-use-classes
  (xlib::make-window :id id :display display)
  #+clx-use-classes
  (make-instance 'xlib:window :id id :display display))

(defmethod start-inator ((o image-x11-inator))
  "Start the inator."
  (with-slots ((image view-image::image)
	       display window window-width window-height own-window font
	       draw-gc erase-gc overlay-gc depth) o
    (when (not display)
      (multiple-value-bind (host number) (get-display-from-environment)
	(when (not host)
	  (error "Can't get X display from the environment."))
	(let (screen black white)
	  (setf display (open-display host :display number)
		screen (display-default-screen display)
		;;depth (screen-root-depth screen)
		black (screen-black-pixel screen)
		white (screen-white-pixel screen)
		window
		(if own-window
		    (create-window
		     :parent (screen-root (display-default-screen display))
		     :x 0 :y 0
		     :width (view-image::image-width image)
		     :height (view-image::image-height image)
		     :background black
		     :event-mask 
		     (make-event-mask
		      :key-press :button-press :button-release
		      :exposure :visibility-change :structure-notify))
		    (make-window-from
		     :id (parse-integer (nos:environment-variable "WINDOWID"))
		     :display display))
		depth (drawable-depth window)
		font (open-font display "fixed")
		draw-gc (create-gcontext
			 :drawable window
			 :background black
			 :foreground white
			 :function boole-1
			 :subwindow-mode :include-inferiors
			 :font font)
		overlay-gc (create-gcontext
			    :drawable window
			    :background black
			    :foreground white
			    :function boole-1
			    :subwindow-mode :include-inferiors
			    :font font)
		erase-gc (create-gcontext
			  :drawable window
			  :background black
			  :foreground white
			  :function boole-clr
			  :subwindow-mode :include-inferiors
			  :font font))
	  ;;(format t "depth = ~s~%" depth)
	  ;;(format t "~s~%" (get-absolute-coords window))
	  (when own-window
	    (setf (wm-name window) "Image Viewer: "
		  (wm-icon-name window) "Image Viewer")
	    (let ((wmh (make-wm-hints :input :on)))
	      (setf (wm-hints window) wmh))
	    (map-window window))
	  (call-next-method))))))

(defun get-window-width (inator)
  (with-slots (window window-width) inator
    (or (and (zerop window-width)
	     (setf window-width (drawable-width window)))
	window-width)))

(defun get-window-height (inator)
  (with-slots (window window-height) inator
    (or (and (zerop window-height)
	     (setf window-height (drawable-height window)))
	window-height)))

(defmethod view-image::width ((inator image-x11-inator))
  "Return the width of the window in pixels."
  (get-window-width inator))

(defmethod view-image::height ((inator image-x11-inator))
  "Return the height of the window in pixels."
  (get-window-height inator))

(defun free-pixmaps (pixmap-array)
  (when pixmap-array
    (loop :for i :across pixmap-array :do
       (when i
	 (xlib:free-pixmap i)))))

(defmethod finish-inator ((o image-x11-inator))
  "Finish with the inator."
  (with-slots (display window own-window draw-gc erase-gc ximages-mask) o
    ;;(when ximage (destroy-image ximage) No such call?
    (when window
      (if own-window
	  (destroy-window window)
	  (progn
	    ;; (draw-rectangle window erase-gc 0 0
	    ;; 		    (drawable-width window) (drawable-height window)
	    ;; 		    t)
	    ;; (clear-area window
	    ;; 		:x 0 :y 0
	    ;; 		:width (drawable-width window)
	    ;; 		:height (drawable-height window)
	    ;; 		:exposures-p t
	    ;; 		)
	    (display-finish-output display)))
      (free-pixmaps ximages-mask))
    ;; (when *derp*
    ;;   (free-pixmap *derp*)
    ;;   (setf *derp* nil))
    (when draw-gc (free-gcontext draw-gc))
    (when erase-gc (free-gcontext erase-gc))
    (when display (close-display display))))

(defmethod view-image::show-message ((o image-x11-inator) string)
  (with-slots (window draw-gc font) o
    (draw-image-glyphs window draw-gc 0 (- (drawable-height window)
					   (font-descent font))
		       (string-to-utf8-bytes string))))

(defun get-absolute-coords (win)
  (let (children parent root (xoff 0) (yoff 0))
    (declare (ignorable root children))
    (loop :with w = win
       :do
       (multiple-value-setq (children parent root) (query-tree w))
       :while parent
       :do
       (incf xoff (drawable-x parent))
       (incf yoff (drawable-y parent))
       ;;(format t "~s ~d ~d~%" w (drawable-x w) (drawable-y w))
       (setf w parent))
    (values xoff yoff)))

(defun window-eraser (inator x y width height)
  (with-slots (display window eraser-window) inator
    (multiple-value-bind (abs-x abs-y) (get-absolute-coords window)
      (if (not eraser-window)
	  (setf eraser-window
		(create-window
		 :parent (screen-root (display-default-screen display))
		 :x (+ abs-x x)
		 :y (+ abs-y y)
		 :width width :height height
		 :override-redirect :on))
	  (progn
	    (setf (drawable-x eraser-window) (+ abs-x x)
		  (drawable-y eraser-window) (+ abs-y y)
		  (drawable-width eraser-window) width
		  (drawable-height eraser-window) height)))
      (map-window eraser-window)
      (unmap-window eraser-window)
      (display-finish-output display)
      (sleep .01)
      ;; (format t "x ~s ~s y ~s ~s~%"
      ;; 	 x (drawable-x window)
      ;; 	 y (drawable-y window))
      )))

(defun erase-area (o x y width height)
  (with-slots (display window own-window) o
    (clear-area window :x x :y y
		:width width :height height :exposures-p t)
    (display-finish-output display)
    (when (not own-window)
      (window-eraser o x y width height))))

(defun erase-image (o)
  (with-slots ((image    view-image::image)
	       (subimage view-image::subimage)
	       (x        view-image::x)
	       (y        view-image::y)
	       display window erase-gc overlay-gc ximages-mask) o
    (with-accessors ((width image:image-width)
		     (height image:image-height)) image
      (with-accessors ((si-x image:sub-image-x)
		       (si-y image:sub-image-y)
		       (si-width image:sub-image-width)
		       (si-height image:sub-image-height)
		       ;;(disposal image:sub-image-disposal)
		       )
	  (aref (image:image-subimages image) subimage)
	;;(setf (gcontext-clip-mask erase-gc) (aref ximages-mask subimage))
	;;(draw-rectangle window erase-gc x y width height t)
	;; (put-image window erase-gc (aref ximages subimage)
	;; 	   :x x-pos :y y-pos
	;; 	   :src-x source-x :src-y source-y
	;; 	   :width w :height h)
	;;(display-finish-output display)
	;;(setf (gcontext-clip-mask erase-gc) :none)
	(erase-area o x y width height)
	))))

(defun update-pos (o)
  (with-slots ((image    view-image::image)
	       (subimage view-image::subimage)
	       (x        view-image::x)
	       (y        view-image::y)
	       display window erase-gc overlay-gc) o
      (with-accessors ((si-x      image:sub-image-x)
		       (si-y      image:sub-image-y)
		       (si-width  image:sub-image-width)
		       (si-height image:sub-image-height)
		       ;;(disposal image:sub-image-disposal)
		       )
	  (aref (image:image-subimages image) subimage)
	(let* ((start-x (+ x si-x))
	       (start-y (+ y si-y))
	       (x-pos (max start-x 0))
	       (y-pos (max start-y 0)))
	       ;;(x-pos start-x)
	       ;;(y-pos start-y))
	  (setf (gcontext-clip-x overlay-gc) x-pos
		(gcontext-clip-y overlay-gc) y-pos)))))

(defmethod view-image::left ((o image-x11-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defmethod view-image::down ((o image-x11-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )
  
(defmethod view-image::up ((o image-x11-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defmethod view-image::right ((o image-x11-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defmethod move-to-top ((o image-x11-inator))
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defmethod move-to-bottom ((o image-x11-inator))
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defmethod view-image::center ((o image-x11-inator))
  (erase-image o)
  (call-next-method)
  (update-pos o)
  )

(defun our-get-key (inator timeout)
  (with-slots ((our-window window)
	       (frame-start-time view-image::frame-start-time)
	       display own-window window-width window-height)
      inator
    ;; If the timeout has already elapsed, don't use
    (let ((time-left (and timeout
			  (dtime- (dtime+ frame-start-time
					  (make-dtime-as timeout :ms))
				  (get-dtime)))))
      (if own-window
	 (event-case (display :force-output-p t
			      :timeout (and time-left
					    (dtime-plusp time-left)
					    (dtime-to time-left :seconds)))
	   ;; (:client-message ()
	   ;;   t)
	   ;; (:button-press (code)
	   ;;   t)
	   ;; (:exposure (x y width height window count)
	   ;;   (when (eq win window)
	   ;;     (draw-thingy win xgc w h))
	   ;;   nil)
	   (:configure-notify (#| x y |# xlib:window xlib::width xlib::height)
	     (when (eq xlib:window our-window)
	       (when (or (locally (declare (optimize (speed 0)))
			   (/= xlib::width window-width)
			   (/= xlib::height window-height)))
		 (setf window-width xlib::width window-height xlib::height)
		 ;;(clear-area win :x x :y y :width w :height h)
		 ;;(display-finish-output display)
		 ))
	     nil)
	   (:key-press (code state)
	     (let* ((sym (keycode->keysym display code 0))
		    (chr (keysym->character display sym state)))
	       chr)))
	 (progn
	   (dbug "tty get key~%")
	   (event-listen display)
	   (dbug "timeout = ~s time-left ~s ~s~%" timeout time-left
		 (when time-left (dtime-to time-left :seconds)))
	   (if timeout
	       ;; (let ((ds (dtime-to time-left :deciseconds)))
	       ;; 	 (if (< ds 1)
	       ;; 	     (progn
	       ;; 	       (tt-listen-for (dtime-to time-left :seconds))
	       ;; 	       (terminal-ansi::get-char ; XXX
	       ;; 		*terminal* :timeout 0))
	       ;; 	     (terminal-ansi::get-char
	       ;; 	      *terminal* :timeout (truncate ds))))
	       (when (tt-listen-for (if (dtime-plusp time-left)
					(dtime-to time-left :seconds)
					0))
		 (tt-get-char))
	       (tt-get-char)))))))

(defmethod await-event ((inator image-x11-inator))
  "Wait for an event."
  (with-slots ((looping view-image::looping)
	       (subimage view-image::subimage)
	       (image view-image::image)
	       display window own-window) inator
    (with-accessors ((subimages image:image-subimages)) image
      (cond
	((and looping subimages)
	 (let* ((timeout (view-image::sub-image-delay
			  (aref subimages subimage)))
		(result (our-get-key inator timeout)))
	   (when (not result) ;; timed out
	     (if (= subimage (1- (length subimages)))
		 (setf subimage 0)
		 (view-image::next-sub-image inator)))
	   result))
	(t
	 (our-get-key inator nil))))))

(defun calculate-rowsize (width depth)
  (cond
    ((= depth 1) ;; a bitmap
     (ceiling (/ width 8)))
    (t
     (let ((bytes-per-pixel (/ depth 8))
	   ;; currently the reading code only produces rgb color images
	   (word-pad t))
       (cond
	 (word-pad
	  ;; Assumes a pixel takes a multiple of bytes in the image format
	  ;; (i.e. not packed). This will probably only work for depths
	  ;; that are a multiple of 8. For depths > 8, rounds up to a
	  ;; multiple of 4 (i.e. an even number of 32 bit words).
	  (logand (+ (* bytes-per-pixel width) 3) (lognot 3)))
	 ;; (t
	 ;;  (* bytes-per-pixel width))
	 )))))

(defmethod view-image::clear-image ((inator image-x11-inator))
  "Clear the current image. Called before switching files or ending."
  (erase-image inator)
  (call-next-method)
  )

(defmethod view-image::reset-image ((inator image-x11-inator))
  "Reset things for a new image. Called after switching files."
  (with-slots ((file-list view-image::file-list)
	       (file-index view-image::file-index)
	       ximages ximages-mask need-to-redraw own-window window) inator
    (free-pixmaps ximages-mask)
    (setf ximages nil
	  ximages-mask nil
	  ;;need-to-redraw t
	  )
    (when own-window
      (setf (wm-name window) (format nil "Image Viewer: ~a"
				     (elt file-list file-index)))))
  (call-next-method))

(defun make-mask (inator)
  "Create the mask pixmap, from XIMAGE-MASK-DATA, and save it in the
XIMAGES-MASK array."
  (with-slots ((image view-image::image)
	       (subimage view-image::subimage)
	       display window ximage-mask-data ximages-mask) inator
    (with-accessors ((si-x image::sub-image-x)
		     (si-y image::sub-image-y)
		     (si-width image::sub-image-width)
		     (si-height image::sub-image-height)
		     (disposal image::sub-image-disposal))
	(aref (image:image-subimages image) subimage)
    (let* ((mask-image (create-image
		       :width si-width
		       :height si-height
		       ;; :plist plist
		       :data ximage-mask-data))
	   (pixmap (create-pixmap :width si-width
				  :height si-height
				  :depth 1
				  :drawable window))
	   (pixmap-gc (create-gcontext
		       :drawable pixmap
		       :foreground (screen-white-pixel
				    (display-default-screen display))
		       :background (screen-black-pixel
				    (display-default-screen display)))))
      (put-image pixmap pixmap-gc mask-image
		 :x 0 :y 0 :width si-width :height si-height)
      (setf (aref ximages-mask subimage) pixmap)))))

(defun use-mask-p (inator)
  (with-slots ((image view-image::image)
	       (subimage view-image::subimage)) inator
    (with-accessors ((subimages image:image-subimages)) image
      (with-accessors ((disposal image:sub-image-disposal)
		       (transparent image:sub-image-transparent))
	  (aref subimages subimage)
	(or transparent (eq disposal :none))))))
  
(defun make-image (inator)
  (with-slots ((image view-image::image)
	       (zoom view-image::zoom)
	       (subimage view-image::subimage)
	       ;;(subimages view-image::subimages)
	       depth ximages ximages-mask ximage-data ximage-mask-data) inator
    (with-accessors ((width image:image-width)
		     (height image:image-height)
		     (subimages image:image-subimages)) image
      (with-accessors ((disposal image:sub-image-disposal)
		       (transparent image:sub-image-transparent))
	  (aref subimages subimage)
	(when (not (aref ximages subimage))
	  (dbug "make image~%")
	  (setf (aref ximages subimage)
		(create-image
		 :width (truncate (* width zoom))
		 :height (truncate (* height zoom))
		 :depth depth
		 ;;:depth 8
		 :data ximage-data
		 ;;:bits-per-pixel 8
		 :bits-per-pixel 32
		 :format :z-pixmap
		 ;;:bytes-per-line (calculate-rowsize width depth)
		 ;;:bytes-per-line (calculate-rowsize width depth)
		 ))
	  (when (use-mask-p inator)
	    (make-mask inator)))))))

(defun copy-image-to-ximage-data (inator)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)
  ;;  		     (compilation-speed 0)))
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       ximage-data ximage-mask-data depth) inator
    (declare (type fixnum x y) (type float zoom)
	     (type (simple-array (unsigned-byte 32) (* *)) ximage-data)
	     ;;(type (simple-array (* *)) ximage-mask-data)
	     )
    (with-accessors ((name image:image-name)
		     (subimages image:image-subimages)) image
      ;;(format t "subimage = ~s ~s~%" subimage subimages)
      (with-accessors ((si-x        image:sub-image-x)
		       (si-y        image:sub-image-y)
		       (width       image:sub-image-width)
		       (height      image:sub-image-height)
		       (disposal    image:sub-image-disposal)
		       (transparent image:sub-image-transparent)
		       (data        image:sub-image-data))
	  (aref subimages subimage)
	(declare (type fixnum si-x si-y width height)
		 ;;(type (simple-array (unsigned-byte 8) (* * *)) data)
		 ;;(type (simple-array (unsigned-byte 32) (* *)) data)
		 )
	;;(format t "ximage-data = ~s~%" (type-of ximage-data))
	;;(when (not (equal (array-dimensions ximage-data) `(,height ,width)))
	(setf ximage-data
	      (make-array `(,height ,width)
			  :initial-element (coerce 0 `(unsigned-byte 32))
			  :element-type `(unsigned-byte 32)))
	;;:initial-element (coerce 0 '(unsigned-byte *))
	;;:element-type `(unsigned-byte 8))))
	(when (or transparent (eq disposal :none))
	  (setf ximage-mask-data
		(make-array `(,height ,width)
			    ;;:initial-element (coerce 0 `(unsigned-byte 32))
			    ;;:element-type '(unsigned-byte 32))))
			    :initial-element (coerce 0 `bit)
			    :element-type 'bit)))
	;;(format t "ximage-data = ~s~%" (type-of ximage-data))
	(dbug "width = ~s height = ~s ~s~%" width height
	      (array-dimensions ximage-data))
	(let* ((step (max 1 (truncate 1 zoom)))
	       (step-squared (* step step))
	       (mask-p (use-mask-p inator)))
	  (declare (type fixnum step step-squared))
	  (loop
	     :with r fixnum = 0
	     :and g fixnum = 0
	     :and b fixnum = 0
	     :and a fixnum = 0
	     :for iy fixnum :from 0 :below height :by step :do
	     (loop
		:for ix fixnum :from 0 :below width :by step :do
		(setf r (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 0)
			      (get-pixel-r data
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(setf g (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 1)
			      (get-pixel-g data
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(setf b (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 2)
			      (get-pixel-b data
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(setf a (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 3)
			      (get-pixel-a data
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(cond
		  ((zerop a)
		   (setf (aref ximage-data iy ix) #x000000)
		   (when mask-p
		     (setf (aref ximage-mask-data iy ix) 0)))
		  (t
		   (setf (aref ximage-data iy ix)
			 (logior
			  (ash (truncate r step-squared) (* 2 8))
			  (ash (truncate g step-squared) (* 1 8))
			  (truncate b step-squared))
			 r 0 g 0 b 0 a 0)
		   (when mask-p
		     (setf (aref ximage-mask-data iy ix) 1)))))))))))

;;(defparameter *derp* nil)

(defun show-image (inator)
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       (file-index view-image::file-index)
	       (file-list view-image::file-list)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       (show-modeline view-image::show-modeline)
	       display window ximages ximages-mask draw-gc erase-gc overlay-gc)
      inator
    (declare (type fixnum x y) (type float zoom))
    (with-accessors ((name image:image-name)
		     (subimages image:image-subimages)) image
      (with-accessors ((si-x     image:sub-image-x)
		       (si-y     image:sub-image-y)
		       (width    image:sub-image-width)
		       (height   image:sub-image-height)
		       (disposal image:sub-image-disposal)
		       (data     image:sub-image-data)) (aref subimages subimage)
	(declare (type fixnum si-x si-y width height))
	;;(format t "width ~s height ~s~%" width height)
	(dbug "put image~%")
	(let* ((start-x (+ x si-x))
	       (start-y (+ y si-y))
	       (x-pos (max start-x 0))
	       (y-pos (max start-y 0))
	       ;;(x-pos start-x)
	       ;;(y-pos start-y)
	       (source-x (if (minusp start-x) (abs start-x) 0))
	       (source-y (if (minusp start-y) (abs start-y) 0))
	       (w (max (min (- width source-x) (get-window-width inator)) 0))
	       (h (max (min (- height source-y) (get-window-height inator)) 0))
	       (mask-p (use-mask-p inator)))
	  (dbug "mask-p = ~s~%" mask-p)
	  (when (not ximages)
	    (setf ximages (make-array (length subimages) :initial-element nil)))
	  (when (and (not ximages-mask) mask-p)
	    (setf ximages-mask
		  (make-array (length subimages) :initial-element nil)))
	  (when (not (aref ximages subimage))
	    (dbug "blit~%")
	    (copy-image-to-ximage-data inator))
	  (make-image inator)
	  (when (and (not (zerop w)) (not (zerop h)))
	    (if mask-p
		(progn
		  ;;(draw-rectangle window draw-gc 0 0 200 200 t)
		  ;;(when (not *derp*) (setf *derp* (get-derp inator)))
		  (setf (gcontext-clip-mask overlay-gc)
		    	(aref ximages-mask subimage)
			(gcontext-clip-x overlay-gc) x-pos
			(gcontext-clip-y overlay-gc) y-pos
			;;*derp*
			)
		  ;;(dump-it)
		  ;;(invoke-debugger (make-condition 'error))
		  ;; (display-finish-output display)
		  ;; (tt-get-char)
		  ;; (put-image window overlay-gc (aref ximages subimage)
		  ;;  	     :x x-pos :y y-pos
		  ;;  	     :src-x source-x :src-y source-y
		  ;;  	     :width w :height h)
		  (put-image window overlay-gc (aref ximages subimage)
		   	     :x x-pos :y y-pos
		   	     :src-x source-x :src-y source-y
		   	     :width w :height h)
		  )
		(put-image window draw-gc (aref ximages subimage)
			   :x x-pos :y y-pos
			   :src-x source-x :src-y source-y
			   :width w :height h))))
	(dbug "put image done~%")
	(when show-modeline
	  (dbug "modeline~%")
	  (view-image::show-status inator))))))

(defmethod update-display ((o image-x11-inator))
  "Update the display."
  (dbug "update~%")
  (with-slots ((frame-start-time view-image::frame-start-time)
	       display window erase-gc need-to-redraw) o
    (setf frame-start-time (get-dtime))
    (when need-to-redraw
      (dbug "redraw~%")
      ;; (draw-rectangle window erase-gc 0 0
      ;; 		      (drawable-width window) (drawable-height window)
      ;; 		      t)
      (erase-area o 0 0 (drawable-width window) (drawable-height window))
      (setf need-to-redraw nil))
    (show-image o)
    (display-finish-output display)))

(defmethod redraw ((o image-x11-inator))
  (with-slots (need-to-redraw) o
    (setf need-to-redraw t)
    ;;(update-display o)
    ))

(defmethod image-inator-usable-p ((type (eql 'image-x11-inator)))
  (get-display-from-environment))

(register-image-inator 'image-x11-inator 2)

(defun dump-it ()
  (let* ((a (view-image-x11::ximage-mask-data view-image::*image-viewer*))
	 (w (array-dimension a 0))
	 (h (array-dimension a 1)))
    (with-open-file (out "fizifoo" :direction :output :if-exists :supersede)
      (loop :for y :from 0 :below h :do
	 (loop :for x :from 0 :below w :do
	    (princ (aref a y x) out))
	 (terpri out)))))

;; EOF
