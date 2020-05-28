;;;
;;; view-image-fb.lisp - Image viewer FB driver.
;;;

;; This whole package is just a temporary hack for my enjoyment/suffering.

(defpackage :view-image-fb
  (:documentation "Image viewer FB driver.")
  (:use :cl :dlib :dlib-misc :char-util :inator :view-image :terminal)
  (:export
   #:fb-image-inator
   ))
(in-package :view-image-fb)

;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;;(declaim (optimize (speed 3) (safety 0) (debug 2) (space 0) (compilation-speed 0)))

;; (declaim #.`(optimize ,.view-image::*optimize-settings*))

(defclass image-fb-inator (image-inator)
  ((framebuffer
    :initarg :framebuffer :accessor framebuffer :initform nil
    :documentation "The linux-fb framebuffer.")
   (fb-context
    :initarg :fb-context :accessor fb-context :initform nil 
    :documentation "The linux-fb graphics context.")
   (need-to-redraw
    :initarg :need-to-redraw :accessor need-to-redraw :initform nil :type boolean
    :documentation "True if we need to redraw."))
  (:default-initargs
   :move-object-mode nil)
  (:documentation "Image viwer for FB."))

(defmethod initialize-instance
    :after ((o image-fb-inator) &rest initargs &key &allow-other-keys)
  "Initialize a image-fb-inator."
  (declare (ignore initargs))
  (setf (slot-value o 'view-image::use-half-block) nil))

(defmethod start-inator ((o image-fb-inator))
  "Start the inator."
  (with-slots ((image view-image::image)
	       framebuffer fb-context) o
    (when (not fb-context)
      (setf framebuffer (linux-fb:start)
	    fb-context (linux-fb:new-gc framebuffer)))
      ;; (call-next-method)
    (call-next-method)
    (when (view-image::image-inator-image o)
      (view-image::center o)
      (view-image::fit-image-to-window o))))

(defun get-window-width (inator)
  (linux-fb:framebuffer-width (framebuffer inator)))

(defun get-window-height (inator)
  (linux-fb:framebuffer-height (framebuffer inator)))

(defmethod view-image::width ((inator image-fb-inator))
  "Return the width of the window in pixels."
  (get-window-width inator))

(defmethod view-image::height ((inator image-fb-inator))
  "Return the height of the window in pixels."
  (get-window-height inator))

(defmethod finish-inator ((o image-fb-inator))
  "Finish with the inator."
  (linux-fb:done (framebuffer o)))

(defmethod view-image::show-message ((o image-fb-inator) string)
  ;; (with-slots (fb-context) o
  ;;   (draw-image-glyphs window draw-gc 0 (if message
  ;; 					  (+ (font-descent font)
  ;; 					     (font-ascent font))
  ;; 					  (- (drawable-height window)
  ;; 					     (font-descent font)))
  ;; 		       (string-to-utf8-bytes string)))
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-write-string string)
  )

#|
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
|#

(defun erase-area (o x y width height)
  (declare (ignore o))
  ;; (with-slots (display window own-window) o
  ;;   (clear-area window :x x :y y
  ;; 		:width width :height height :exposures-p t)
  ;;   (display-finish-output display)
  ;;   (when (not own-window)
  ;;     (window-eraser o x y width height)))
  (multiple-value-bind (xx yy ww hh) (linux-fb::clip-rect x y width height)
    (linux-fb:rectangle-fill xx yy ww hh #x00000000)))

(defun erase-image (o)
  (with-slots ((image    view-image::image)
	       (subimage view-image::subimage)
	       (x        view-image::x)
	       (y        view-image::y)) o
    (when image
      (with-accessors ((width image:image-width)
		       (height image:image-height)) image
	(with-accessors ((si-x image:sub-image-x)
			 (si-y image:sub-image-y)
			 (si-width image:sub-image-width)
			 (si-height image:sub-image-height)
			 ;;(disposal image:sub-image-disposal)
			 )
	    (aref (image:image-subimages image) subimage)
	  (erase-area o x y width height))))))

#|
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

|#

(defmethod view-image::left ((o image-fb-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

(defmethod view-image::down ((o image-fb-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )
  
(defmethod view-image::up ((o image-fb-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

(defmethod view-image::right ((o image-fb-inator) &optional (n 1))
  (declare (ignore n))
  ;;(setf (need-to-redraw o) t)
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

(defmethod move-to-top ((o image-fb-inator))
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

(defmethod move-to-bottom ((o image-fb-inator))
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

(defmethod view-image::center ((o image-fb-inator))
  (erase-image o)
  (call-next-method)
  ;; (update-pos o)
  )

#|
(defmethod view-image::fit-image-to-window ((o image-fb-inator))
  (when (image-fb-inator-display o)
    (call-next-method)))

(defmethod view-image::image-listen ((inator image-fb-inator))
  "Return true if there is input ready."
  (with-slots (display own-window) inator
    (if own-window
	(xlib:event-listen display)
	(tt-listen-for 0))))

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
		 (tt-get-key))
	       (tt-get-key)))))))

(defmethod await-event ((inator image-fb-inator))
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

(defmethod view-image::clear-image ((inator image-fb-inator))
  "Clear the current image. Called before switching files or ending."
  (erase-image inator)
  (call-next-method))

(defmethod view-image::invalidate-cache ((inator image-fb-inator))
  "Invalidate the image cache. Re-generate the Ximages from the image data."
  (setf (cache-valid inator) nil))

(defmethod view-image::reset-image ((inator image-fb-inator))
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

(defun copy-image-to-ximage-data (inator &key keep)
  ;; (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)
  ;;  		     (compilation-speed 0)))
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       ximage-data ximage-mask-data depth
	       cache-valid) inator
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
	(when (not keep)
	  (setf ximage-data
		(make-array `(,height ,width)
			    :initial-element (coerce 0 `(unsigned-byte 32))
			    :element-type `(unsigned-byte 32))))
	;;:initial-element (coerce 0 '(unsigned-byte *))
	;;:element-type `(unsigned-byte 8))))
	(when (and (not keep) (or transparent (eq disposal :none)))
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
			      (image:pixel-r data
				       (min (1- height) (+ iy av-y))
				       (min (1- width) (+ ix av-x)))
			      )))
		(setf g (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 1)
			      (image:pixel-g data
				       (min (1- height) (+ iy av-y))
				       (min (1- width) (+ ix av-x)))
			      )))
		(setf b (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 2)
			      (image:pixel-b data
				       (min (1- height) (+ iy av-y))
				       (min (1- width) (+ ix av-x)))
			      )))
		(setf a (loop :for av-y fixnum :from 0 :below step :sum
			   (loop :for av-x fixnum :from 0 :below step
			      :sum
			      ;; (aref data
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 3)
			      (image:pixel-a data
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
|#

(defun show-image (inator)
  (with-slots ((x view-image::x)
	       (y view-image::y)
	       (zoom view-image::zoom)
	       (file-index view-image::file-index)
	       (file-list view-image::file-list)
	       (image view-image::image)
	       (subimage view-image::subimage)
	       (looping view-image::looping)
	       (show-modeline view-image::show-modeline))
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
	(let* (
	       ;; (start-x (+ x si-x))
	       ;; (start-y (+ y si-y))
	       ;; (x-pos (max start-x 0))
	       ;; (y-pos (max start-y 0))
	       ;;(x-pos start-x)
	       ;;(y-pos start-y)
	       ;;(source-x (if (minusp start-x) (abs start-x) 0))
	       ;;(source-y (if (minusp start-y) (abs start-y) 0))
	       ;;(w (max (min (- width source-x) (get-window-width inator)) 0))
	       ;;(h (max (min (- height source-y) (get-window-height inator)) 0))
	       ;;(mask-p (use-mask-p inator))
	       )
	  ;; (linux-fb:put-image image x-pos y-pos :subimage subimage)
	  (linux-fb:put-image image x y :subimage subimage :scale zoom)
	  )))
    (dbug "put image done~%")
    (when show-modeline
      (dbug "modeline~%")
      (view-image::show-status inator))))

(defmethod update-display ((o image-fb-inator))
  "Update the display."
  (with-slots ((frame-start-time view-image::frame-start-time)
	       framebuffer
	       #|display window erase-gc|# need-to-redraw) o
    (setf frame-start-time (get-dtime))
    (when need-to-redraw
      (dbug "redraw~%")
      ;; (draw-rectangle window erase-gc 0 0
      ;; 		      (drawable-width window) (drawable-height window)
      ;; 		      t)
      (erase-area o 0 0
		  (linux-fb:framebuffer-width framebuffer)
		  (linux-fb:framebuffer-height framebuffer))
      (setf need-to-redraw nil))
    (show-image o)
    ;; (display-finish-output display)
    ))

(defmethod redraw ((o image-fb-inator))
  (with-slots (need-to-redraw) o
    (setf need-to-redraw t)
    ;;(update-display o)
    ))

(defmethod image-inator-usable-p ((type (eql 'image-fb-inator)))
  (and (nos:file-accessible-p "/dev/fb0" '(:read :write)) t))

(register-image-inator 'image-fb-inator 2)

#+lish
(lish:defcommand view-image-fb
  ((images pathname :repeating t :help "Image to view."))
  :accepts (:sequence :stream)
  "View an image."
  (view-image::view-images (or images
			       (and lish:*input* (list lish:*input*))
			       (list *standard-input*))
			   :type 'image-fb-inator))

;; EOF
