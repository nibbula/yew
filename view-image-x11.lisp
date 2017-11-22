;;
;; view-image-x11.lisp - Image viewr X11 driver.
;;

;; This whole package is just a temporary hack for my enjoyment/suffering.

(defpackage :view-image-x11
  (:documentation "Image viewer X11 driver.")
  (:use :cl :dlib :dlib-misc :inator :view-image :xlib :terminal)
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
    :initarg :own-window :accessor image-x11-inator-own-window
    :initform nil :type boolean
    :documentation "True to use our own window.")
   (draw-gc
    :initarg :draw-gc :accessor image-x11-inator-draw-gc :initform nil
    :documentation "Graphics context for drawing.")
   (erase-gc
    :initarg :erase-gc :accessor image-x11-inator-erase-gc :initform nil
    :documentation "Graphics context for erasing.")
   (overlay-gc
    :initarg :overlay-gc :accessor image-x11-inator-overlay-gc :initform nil
    :documentation "Graphics context for erasing.")
   (font
    :initarg :font :accessor image-x11-inator-font :initform nil
    :documentation "The font for text.")
   (need-to-redraw
    :initarg :need-to-redraw :accessor image-x11-inator-need-to-redraw
    :initform nil :type boolean
    :documentation "True if we need to redraw the screen.")
   (frame-start-time
    :initarg :frame-start-time :accessor image-x11-inator-frame-start-time
    :documentation "The time we start rendering a frame.")
   (ximages
    :initarg :ximage :accessor image-x11-inator-ximages
    :initform nil
    :documentation "Array of image handles paralelling the image-subimages.")
   (ximage-data
    :initarg :ximage-data :accessor image-x11-inator-ximage-data
    :type (simple-array (unsigned-byte 32) (* *))
    :initform (the (simple-array (unsigned-byte 32) (* *))
		   (make-array '(1 1)
			       :initial-element (coerce 0 '(unsigned-byte 32))
			       :element-type '(unsigned-byte 32)))
    :documentation "Data in X format.")
   )
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

(defmethod start-inator ((o image-x11-inator))
  "Start the inator."
  (with-slots (display window window-width window-height own-window font
	       draw-gc erase-gc overlay-gc depth) o
    (when (not display)
      (multiple-value-bind (host number) (get-display-from-environment)
	(when (not host)
	  (error "Can't get X display from the environment."))
	(let (screen black white)
	  (setf display (open-display host :display number)
		screen (display-default-screen display)
		depth (screen-root-depth screen)
		black (screen-black-pixel screen)
		white (screen-white-pixel screen)
		window
		(if own-window
		    (create-window
		     :parent (screen-root (display-default-screen display))
		     :x 0 :y 0 :width 400 :height 400 
		     :background black
		     :event-mask 
		     (make-event-mask
		      :key-press :button-press :button-release
		      :exposure :visibility-change :structure-notify))
		    (xlib::make-window
		     :id (parse-integer (nos:environment-variable "WINDOWID"))
		     :display display))
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
			    :function boole-ior
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
	  (when own-window
	    (setf (wm-name window) "Image Viewer: "
		  (wm-icon-name window) "Image Viewer")
	    (let ((wmh (make-wm-hints :input :on)))
	      (setf (wm-hints window) wmh))
	    (map-window window)))))))

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

(defmethod finish-inator ((o image-x11-inator))
  "Finish with the inator."
  (with-slots (display window own-window draw-gc erase-gc) o
    (when draw-gc (free-gcontext draw-gc))
    (when erase-gc (free-gcontext erase-gc))
    ;;(when ximage (destroy-image ximage) No such call?
    (when window
      (if own-window
	  (destroy-window window)
	  (progn
	    (clear-area window
			:x 0 :y 0
			:width (drawable-width window)
			:height (drawable-height window)
			:exposures-p t
			)
	    (display-finish-output display))))
    (when display (close-display display))))

(defmethod message ((o image-x11-inator) format-string &rest args)
  (let ((str (apply #'format nil format-string args)))
    (with-slots (window draw-gc font) o
      (draw-image-glyphs window draw-gc 0 (- (drawable-height window)
					     (font-descent font))
			 str))))

(defun our-get-key (inator timeout)
  (with-slots ((our-window window)
	       display own-window window-width window-height frame-start-time)
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
	       (progn
		 (when (dtime-plusp time-left)
		   (tt-listen-for (dtime-to time-left :seconds)))
		 ;; XXX
		 (terminal-ansi::get-char *terminal* :timeout 0))
	       (tt-get-char)))))))

(defmethod await-event ((inator image-x11-inator))
  "Wait for an event."
  (with-slots ((looping view-image::looping)
	       (subimage view-image::subimage)
	       (image view-image::image)
	       display window own-window) inator
    (with-slots ((subimages view-image::subimages)) image
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

(defmethod view-image::reset-image ((inator image-x11-inator))
  (call-next-method)
  (with-slots (ximages need-to-redraw) inator
    (setf ximages nil
	  need-to-redraw t)))

(defun ensure-image (inator)
  (with-slots ((image view-image::image)
	       (zoom view-image::zoom)
	       (subimage view-image::subimage)
	       ;;(subimages view-image::subimages)
	       depth ximages ximage-data) inator
    (with-slots ((width view-image::width)
		 (height view-image::height)) image
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
	       ))))))

(defun copy-image-to-ximage-data (inator)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)
   		     (compilation-speed 0)))
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       ximage-data depth) inator
    (declare (type fixnum x y) (type float zoom)
	     (type (simple-array (unsigned-byte 32) (* *)) ximage-data))
    (with-slots ((name view-image::name)
		 (subimages view-image::subimages)) image
      ;;(format t "subimage = ~s ~s~%" subimage subimages)
      (with-slots ((si-x view-image::x)
		   (si-y view-image::y)
		   (width view-image::width)
		   (height view-image::height)
		   (data view-image::data))
	  (aref subimages subimage)
	(declare (type fixnum si-x si-y width height)
		 (type (simple-array (unsigned-byte 8) (* * *)) data))
	;;(format t "ximage-data = ~s~%" (type-of ximage-data))
	;;(when (not (equal (array-dimensions ximage-data) `(,height ,width)))
	(setf ximage-data
	      (make-array `(,height ,width)
			  :initial-element (coerce 0 `(unsigned-byte 32))
			  :element-type `(unsigned-byte 32)))
			    ;;:initial-element (coerce 0 '(unsigned-byte *))
			    ;;:element-type `(unsigned-byte 8))))
	;;(format t "ximage-data = ~s~%" (type-of ximage-data))
	(dbug "width = ~s height = ~s ~s~%" width height
	      (array-dimensions ximage-data))
	(let* ((step (max 1 (truncate 1 zoom)))
	       (step-squared (* step step)))
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
			      :sum (aref data
					 (min (1- width) (+ ix av-x))
					 (min (1- height) (+ iy av-y)) 0))))
		(setf g (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum (aref data
					 (min (1- width) (+ ix av-x))
					 (min (1- height) (+ iy av-y)) 1))))
		(setf b (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum (aref data
					 (min (1- width) (+ ix av-x))
					 (min (1- height) (+ iy av-y)) 2))))
		(setf a (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum (aref data
					 (min (1- width) (+ ix av-x))
					 (min (1- height) (+ iy av-y)) 3))))
		(setf (aref ximage-data iy ix)
		      (if (not (zerop a))
			  (logior
			   (ash (truncate r step-squared) (* 2 8))
			   (ash (truncate g step-squared) (* 1 8))
			   (truncate b step-squared))
			  #x000000)
		      r 0 g 0 b 0 a 0))))))))

(defun show-image (inator)
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       ;;(message view-image::message)
	       (file-index view-image::file-index)
	       (file-list view-image::file-list)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       (show-modeline view-image::show-modeline)
	       window ximages draw-gc erase-gc overlay-gc) inator
    (declare (type fixnum x y) (type float zoom))
    (with-slots ((name view-image::name)
		 (subimages view-image::subimages)) image
      (with-slots ((si-x view-image::x)
		   (si-y view-image::y)
		   (width view-image::width)
		   (height view-image::height)
		   (data view-image::data)) (aref subimages subimage)
	(declare (type fixnum si-x si-y width height))
	;; (when (not looping)
	;;   (tt-clear))
	;;(format t "width ~s height ~s~%" width height)
	(when (not ximages)
	  (setf ximages (make-array (length subimages) :initial-element nil)))
	(when (not (aref ximages subimage))
	  (dbug "blit~%")
	  (copy-image-to-ximage-data inator))
	(ensure-image inator)
	(dbug "put image~%")
	(let* ((start-x (+ x si-x))
	       (start-y (+ y si-y))
	       (x-pos (max start-x 0))
	       (y-pos (max start-y 0))
	       (source-x (if (minusp start-x) (abs start-x) 0))
	       (source-y (if (minusp start-y) (abs start-y) 0))
	       (w (max (min (- width source-x) (get-window-width inator)) 0))
	       (h (max (min (- height source-y) (get-window-height inator)) 0)))
	  (if looping
	      (progn
		(put-image window draw-gc (aref ximages subimage)
			   :x x-pos :y y-pos
			   :src-x source-x :src-y source-y
			   :width w :height h)
		(put-image window draw-gc (aref ximages subimage)
			   :x x-pos :y y-pos
			   :src-x source-x :src-y source-y
			   :width w :height h))
	      (put-image window draw-gc (aref ximages subimage)
			   :x x-pos :y y-pos
			   :src-x source-x :src-y source-y
			   :width w :height h)))
	(dbug "put image done~%")
	(when show-modeline
	  (dbug "modeline~%")
	  (view-image::show-status inator))))))

(defmethod update-display ((o image-x11-inator))
  "Update the display."
  (dbug "update~%")
  (with-slots (display window erase-gc need-to-redraw frame-start-time) o
    (setf frame-start-time (get-dtime))
    (when need-to-redraw
      (dbug "redraw~%")
      (draw-rectangle window erase-gc 0 0
		      (drawable-width window) (drawable-height window)
		      t)
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

;; EOF
