;;;
;;; view-image.lisp - Image viewer
;;;

(defpackage :view-image
  (:documentation "Image viewer")
  (:use :cl :dlib :dtime :keymap :char-util :terminal :terminal-ansi
	:terminal-crunch :inator :terminal-inator :magic :grout :image
	:image-ops :dcolor)
  (:export
   #:view-image
   #:!view-image
   #:cat-images
   #:!imgcat
   #:image-inator
   #:register-image-inator
   #:image-inator-usable-p
   ;; #:get-pixel-r #:get-pixel-g #:get-pixel-b #:get-pixel-a
   ))
(in-package :view-image)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defparameter *fast*
;;     '((speed 3) (safety 0) (debug 1) (space 0) (compilation-speed 0)))
;;   (defparameter *fastest*
;;     '((speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
;;   (defparameter *dev*
;;     '((speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;;   (defparameter *optimize-settings* *dev*))

;; (declaim #.`(optimize ,.*optimize-settings*))

(defkeymap *image-viewer-keymap* ())
(defkeymap *image-viewer-escape-keymap* ())

(defclass image-inator (terminal-inator)
  ((image
    :initarg :image :accessor image-inator-image :initform nil
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
   (buffer
    :initarg :buffer :accessor image-inator-buffer :initform nil
    :documentation "Buffer for compositing images.")
   (mod-buffer
    :initarg :mod-buffer :accessor image-inator-mod-buffer :initform nil
    :documentation "Buffer for modifying images.")
   (last-func
    :initarg :last-func :accessor image-inator-last-func :initform nil
    :documentation "The last pixel function.")
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
   (initial-command
    :initarg :initial-command :accessor image-inator-initial-command
    :initform nil
    :documentation "Command to perform when starting.")
   (show-mode-line
    :initarg :show-mode-line :accessor image-inator-show-mode-line
    :initform nil :type boolean
    :documentation "True to show the mode line.")
   (message
    :initarg :message :accessor image-inator-message :initform nil
    :documentation "Message to show.")
   (debug
    :initarg :debug :accessor image-inator-debug :initform nil :type boolean
    :documentation "True to enter the debugger on error.")
   (frame-start-time
    :initarg :frame-start-time :accessor frame-start-time
    :documentation "The time we start rendering a frame.")
   (delay-factor
    :initarg :delay-factor :accessor image-inator-delay-factor :initform 1
    :documentation "Amount to adjust delay.")
   (move-object-mode
    :initarg :move-object-mode :accessor image-inator-move-object-mode
    :initform nil :type boolean
    :documentation
    "True if movement commands move the object instead of the view.")
   (use-half-block
    :initarg :use-half-block :accessor image-inator-use-half-block
    :initform t :type boolean
    :documentation
    "True to use unicode half block to get double vertical resolution.")
   (use-serial-map
    :initarg :use-serial-map :accessor use-serial-map
    :initform nil :type boolean
    :documentation
    "True to use the non-parallel pixel mapping. Useful for debugging.")
   (bg-color
    :initarg :bg-color :accessor image-inator-bg-color
    :initform (lookup-color :black)
    :documentation "Background color of the window.")
   )
  (:default-initargs
   :keymap	`(,*image-viewer-keymap* ,*default-inator-keymap*))
  (:documentation "An image viewer."))

(defvar *image-viewer* nil
  "The current image viewer.")

(defgeneric image-inator-usable-p (type)
  (:documentation "Return true if an image-inator of TYPE can be used.")
  (:method ((type (eql 'image-inator)))
    (or (equal (symbol-name (type-of *terminal*)) "TERMINAL-ANSI")
	(equal (symbol-name (type-of *terminal*)) "TERMINAL-CRUNCH")
	(equal (symbol-name (type-of *terminal*)) "TERMINAL-CURSES")
	(equal (symbol-name (type-of *terminal*)) "TERMINAL-X11"))))

(defgeneric width (inator)
  (:documentation "Return the width of the image-inator in pixels.")
  (:method ((inator image-inator))
    (tt-width)))

(defgeneric height (inator)
  (:documentation "Return the height of the image-inator in pixels.")
  (:method ((inator image-inator))
    (tt-height)))

(defgeneric show-message (inator string)
  (:documentation "Actually show the message.")
  (:method ((inator image-inator) string)
    (tt-move-to (1- (height inator)) 0)
    (tt-write-string string)))

(defgeneric left (o &optional n)
  (:method ((o image-inator) &optional (n 1))
    (declare (type fixnum n))
    (with-slots (x zoom) o
      (declare (type fixnum x))
      (decf x (* n (truncate 1 zoom)))
      ;; (when (<= x 0)
      ;;   (setf x 0))
      )))

(defgeneric down (o &optional n)
  (:method ((o image-inator) &optional (n 1))
    (declare (type fixnum n))
    (with-slots (y image zoom) o
      (declare (type fixnum y))
      (incf y (* n (truncate 1 zoom)))
      ;; (when (>= y (image-height image))
      ;;   (setf y (1- (image-height image))))
      )))
  
(defgeneric up (o &optional n)
  (:method ((o image-inator) &optional (n 1))
    (declare (type fixnum n))
    (with-slots (y zoom) o
      (declare (type fixnum y))
      (decf y (* n (truncate 1 zoom)))
      ;; (when (<= y 0)
      ;;   (setf y 0))
      )))

(defgeneric right (o &optional n)
  (:method ((o image-inator) &optional (n 1))
    (declare (type fixnum n))
    (with-slots (x image zoom) o
      (declare (type fixnum x))
      (incf x (* n (truncate 1 zoom)))
      ;; (when (>= x (image-width image))
      ;;   (setf x (1- (image-width image))))
      )))

(defmethod forward-unit	 ((o image-inator)) (right o))
(defmethod backward-unit ((o image-inator)) (left o))
(defmethod next		 ((o image-inator)) (down o))
(defmethod previous	 ((o image-inator)) (up o))

(defun left-by-increment  (o) (left  o (image-inator-increment o)))
(defun down-by-increment  (o) (down  o (image-inator-increment o)))
(defun up-by-increment    (o) (up    o (image-inator-increment o)))
(defun right-by-increment (o) (right o (image-inator-increment o)))

(defmethod move-to-top ((o image-inator))
  "Move to the top left of the image."
  (setf (image-inator-x o) 0
	(image-inator-y o) 0))

(defmethod move-to-bottom ((o image-inator))
  "Move to the bottom right of the image, or "
  (with-slots (x y image zoom move-object-mode) o
    (declare (type fixnum x y))
    (let ((effective-image-width (truncate (* (image-width image) zoom)))
	  (effective-image-height (truncate (* (image-height image) zoom))))
      (if move-object-mode
	  (setf x (max 0 (- (width o) effective-image-width))
		y (max 0 (- (height o) effective-image-height)))
	  (setf x (max 0 (- effective-image-width (width o)))
		y (max 0 (- effective-image-height (height o)))))
      ;; (message o "Zing! ~d ~d ~dx~d [~dx~d]" x y (width o) (height o)
      ;; 	       effective-image-width effective-image-height)
      )))

(defgeneric center (inator)
  (:documentation "Center the image in the window.")
  (:method ((inator image-inator))
    (with-slots (x y image zoom move-object-mode use-half-block) inator
      (declare (type fixnum x y))
      (let ((effective-image-width (* (image-width image) zoom))
	    (effective-image-height (* (image-height image) zoom)))
	(when use-half-block
	  (setf effective-image-height (/ effective-image-height 2)))
	(if move-object-mode
	    (progn
	      (setf effective-image-width (truncate effective-image-width)
		    effective-image-height (truncate effective-image-height)
		    x (truncate (max 0 (- (/ (width inator) 2)
					  (/ effective-image-width 2))))
		    y (truncate (max 0 (- (/ (height inator) 2)
					  (/ effective-image-height 2))))))
	    (setf x (truncate (* (- (- (/ (width inator) 2)
				       (/ effective-image-width 2)))
				 (/ 1 zoom)))
		  y (truncate (* (- (- (/ (height inator) 2)
				       (/ effective-image-height 2)))
				 (/ 1 zoom))))
	    )))))

(defmethod next-page ((o image-inator))
  (with-slots (y image zoom) o
    (declare (type fixnum y))
    (let ((step (truncate 1 zoom)))
      (setf y (min (+ y (* (height o) step))
		   (max 0 (- (image-height image) (* (height o) step))))))))

(defmethod previous-page ((o image-inator))
  (with-slots (y zoom) o
    (declare (type fixnum y))
    (let ((step (truncate 1 zoom)))
      (setf y (max (- y (* (height o) step)) 0)))))

(defun beginning-of-line (o)
  (setf (image-inator-x o) 0))

(defun end-of-line (o)
  (with-slots (x image zoom) o
    (declare (type fixnum x))
    (setf x (max 0 (- (image-width image) (* (width o) (truncate 1 zoom)))))))

(defun zoom-in (o)
  (with-slots (zoom) o
    (setf zoom (* zoom 1.10))
    (center o)))

(defun zoom-out (o)
  (with-slots (zoom) o
    (setf zoom (* zoom .90))
    (center o)))

(defun zoom-reset (o)
  (with-slots (zoom) o
    (setf zoom 1.0)
    (center o)))

(defun fit-width-to-window (o)
  (with-slots (zoom image) o
    (with-accessors ((width image-width)
		     (height image-height)) image
      ;; width * zoom = tt-width
      ;; zoom = tt-width / width
      (setf zoom
	    (min 1.0 ;; teporarily @@@
		 (float (/ (width o) width))))
      (center o)
      )))

(defun fit-height-to-window (o)
  (with-slots (zoom image use-half-block) o
    (with-accessors ((width image-width)
		     (height image-height)) image
      (let ((hh height))
	(when use-half-block
	  (setf hh (/ height 2)))
	(setf zoom
	      (min 1.0 ;; teporarily @@@
		   (float (/ (height o) hh))))
	(center o)
	))))

(defgeneric fit-image-to-window (o)
  (:documentation "Resize the image to fit in the window.")
  (:method ((o image-inator))
    (with-slots ((width image::width)
		 (height image::height)) (image-inator-image o)
      (cond
	((> width height)
	 (fit-width-to-window o))
	(t
	 (fit-height-to-window o))))))

(defun flip-image-vertical (o)
  (with-slots (image) o
    (reflect-vertical image)
    (invalidate-cache o)))

(defun flip-image-horizontal (o)
  (with-slots (image) o
    (reflect-horizontal image)
    (invalidate-cache o)))

(defun rotate-image-right (o)
  (with-slots (image) o
    (rotate-1/4 image)
    (invalidate-cache o)))

(defun rotate-image-left (o)
  (with-slots (image) o
    (rotate-3/4 image)
    (invalidate-cache o)))

(defgeneric clear-image (inator)
  (:documentation
   "Clear the current image. Called before switching files or ending.")
  (:method ((inator image-inator))
    (declare (ignore inator))))

(defgeneric reset-image (inator)
  (:documentation "Reset things for a new image. Called after switching files.")
  (:method ((inator image-inator))
    (with-slots (subimage looping image mod-buffer) inator
      (when image
	(setf subimage 0)
	;; too damn slow
	;; Make a frame buffer for the whole image if some sub-image needs it.
	;; (when (notevery (_ (eq (sub-image-disposal _) :unspecified))
	;; 		      (image-subimages image))
	;; 	(setf (image-inator-buffer inator)
	;; 	      (make-image-array (image-width image) (image-height image))))
	(set-auto-looping image)
	(setf mod-buffer nil)
	(fit-image-to-window inator)
	(center inator)))))

(defgeneric invalidate-cache (inator)
  (:documentation "Invalidate the image cache, if any.")
  (:method ((inator image-inator))
    (declare (ignore inator))))

(defun increase-delay (o)
  "Slow down the looping rate."
  (with-slots (delay-factor) o
    (if (< delay-factor 1)
	(incf delay-factor .05)
	(incf delay-factor))))

(defun decrease-delay (o)
  "Speed up the looping rate."
  (with-slots (delay-factor) o
    (if (< delay-factor 1)
	(decf delay-factor .05)
	(decf delay-factor))
    (when (minusp delay-factor)
      (setf delay-factor 0))))

(defun toggle-looping (o)
  (with-slots (looping) o
    (setf looping (not looping))))

(defun report-and-continue (c)
  (typecase c
    (simple-condition
     (apply #'show-error (simple-condition-format-control c)
	    (simple-condition-format-arguments c))
     (continue))
    (t
     (show-error "~s" c))))

(defmacro with-image-error-handling ((file-name) &body body)
  `(handler-bind
       ((unknown-image-type #'report-and-continue)
	(non-image-file #'report-and-continue))
     (handler-case
	 (progn
	   ,@body)
       ;; (cl-jpeg:jpeg-error (c)
       ;; 	 (formatted-window "Error: ~a ~a" ,file-name c))
       (error (c)
	 (if (image-inator-debug *image-viewer*)
	     (invoke-debugger c)
	     (show-error "~a ~a" ,file-name c)))
       (simple-error (c)
	 (if (image-inator-debug *image-viewer*)
	     (invoke-debugger c)
	     (show-error "~a ~a" ,file-name c))))))

(defun perserverant-read-image (file)
  "Try to read an image, but try to load image format stragglers after
the first time it fails to identify the image."
  (or (catch 'pequod
	(handler-case
	    (progn
	      (read-image file))
	  (non-image-file (c)
	    (declare (ignore c))
	    (throw 'pequod nil))))
      (progn
	(load-known-formats)
	(read-image file))))

(defun reload-file (o)
  (with-slots (image) o
    (let ((file-name (image-name image))
	  img)
      (with-image-error-handling (file-name)
	(setf img (perserverant-read-image file-name)))
      (clear-image o)
      (setf image img)
      (invalidate-cache o)
      ;; (reset-image o)
      )))

(defmethod next-file ((o image-inator))
  (with-slots (file-list file-index image) o
    (let (img (first-time t) (was-first-time nil))
      (flet ((next ()
	       (cond
		 ((and (not image) first-time)
		  ;; Probably when we're used as an initial-command.
		  (setf file-index 0
			first-time nil
			was-first-time t))
		 ((< file-index (1- (length file-list)))
		  (incf file-index))
		 (t
		  (return-from next-file nil)))))
	(loop :with file-name
	   :while (not img) :do
	   (next)
	   (setf file-name (nth file-index file-list))
	   (with-image-error-handling (file-name)
	     (setf img (perserverant-read-image file-name))))
	(clear-image o)
	(setf image img)
	(when (not was-first-time)
	  (reset-image o))))))

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
	     (setf img (perserverant-read-image file-name))))
	(clear-image o)
	(setf image img)
	(reset-image o)))))

(defun next-sub-image (o)
  (with-slots (image subimage) o
    (with-slots ((subimages image::subimages)) image
      (when (and subimages (< subimage (1- (length subimages))))
	(incf subimage)))))

(defun previous-sub-image (o)
  (with-slots (image subimage) o
    (with-slots ((subimages image::subimages)) image
      (when (and subimages (> subimage 0))
	(decf subimage)))))

(defmethod redraw ((o image-inator))
  (tt-clear))

(defmethod message ((o image-inator) format-string &rest args)
  ;; We just save message for later.
  (setf (image-inator-message o) 
	(apply #'format nil format-string args)))

(defun say (format-string &rest args)
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-write-string (apply #'format nil format-string args)))

(defun formatted-window (format-string &rest args)
  ;; (apply #'say format-string args)
  ;; (tt-write-string " --More--")
  (fui:show-text (apply #'format nil format-string args) :justify t)
  ;; (tt-get-key)
  )

(defun show-error (format-string &rest args)
  ;; (apply #'say format-string args)
  ;; (tt-write-string " --More--")
  (fui:show-text (apply #'format nil format-string args)
		 :title "Error" :justify t)
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

(defparameter *popi-help*
  "Enter a pixel expression.
Some useful variables are:
  p        - The current pixel
  w        - Width of the image in pixels
  h        - Height of the image in pixels
  i        - Sub image number
  n        - An integer that increments for every frame
Some useful functions or macros are:
  (cvt number)                    - Convert a number to a pixel.
  (px r g b &optional (a #xff)))  - Make a pixel from R G B A color components.
  (gray value)                    - Make a gray pixel.
  (pr pixel)                      - Pixel red component
  (pg pixel)                      - Pixel green component
  (pb pixel)                      - Pixel blue component
  (pa pixel)                      - Pixel alpha component
  (f  pixel)                      - Convert pixel component to float
  (a x y)                         - Image pixel at coordinates X and Y.")

(defun popi-help ()
  (fui:show-text *popi-help*))

(defun apply-pixel-expr (o)
  "Apply an expression to every pixel in the image."
  (tt-move-to 0 0)
  (tt-cursor-on)
  (tt-finish-output)
  (let (form real-form raw-form func warns fails
	(*package* (find-package :popi)))
    (declare (ignorable warns fails))
    (catch 'flanky
      (handler-case
	  (progn
	    ;;(setf form (list (read-from-string (rl:rl :prompt ": ") nil nil))
	    (setf raw-form (read-from-string
			      (rl-widget:widget-read
			       :width 60 :height 2
			       :flex-height t
			       :completion-func #'completion:complete-symbol)
			      nil nil)
		  form (list raw-form))
	    (when (and (typep raw-form '(or string symbol))
		       (string-equal raw-form "help"))
	      (popi-help)
	      (throw 'flanky nil))
	    (setf real-form (popi:popi-form form)
		  popi:w (width o)
		  popi:h (height o)
		  popi:n 0
		  (values func warns fails) (compile nil real-form))
	    ;; (format t "Warnings ~s Errors ~s~%" warns fails)
	    ;; (format t "Real form ~s~%" (macroexpand real-form))
	    (with-slots (image mod-buffer last-func use-serial-map) o
	      (when (not mod-buffer)
		(setf mod-buffer (make-blank-copy image)))
	      (if use-serial-map
		  (map-pixels image mod-buffer func)
		  (pmap-pixels image mod-buffer func))
	      (setf last-func func)
	      (rotatef image mod-buffer)))
	(error (c)
	  (formatted-window "Form: ~w~%~a" form c)
	  ;; (sb-debug:print-backtrace)
	  (throw 'flanky nil)
	  #| (continue) |#
	  )
	)))
  (tt-cursor-off)
  (invalidate-cache o)
  (redraw o))

(defun apply-last-pixel-expr (o)
  "Apply the last entered pixel expression again."
  (let ((*package* (find-package :popi)))
    (catch 'flanky
      (handler-case
	  (with-slots (image mod-buffer last-func use-serial-map) o
	    (when (not mod-buffer)
	      (setf mod-buffer (make-blank-copy image)))
	    (if use-serial-map
		(map-pixels image mod-buffer last-func)
		(pmap-pixels image mod-buffer last-func))
	    (incf popi:n)
	    (rotatef image mod-buffer))
	(condition (c)
	  (formatted-window "~a~%" c)
	  (throw 'flanky nil)
	  #| (continue) |#
	  )))
    (invalidate-cache o)))

(defmethod image-listen ((inator image-inator))
  "Return true if there is input ready."
  (tt-listen-for 0))

(defun pixel-expr-loop (o)
  "Apply the last entered pixel expression until a key is pressed."
  (let ((*package* (find-package :popi)))
    (catch 'flanky
      (handler-case
	  (with-slots (image mod-buffer last-func use-serial-map) o
	    (when (not mod-buffer)
	      (setf mod-buffer (make-blank-copy image)))
	    (loop :while (not (image-listen o))
	       :do
	       (if use-serial-map
		   (map-pixels image mod-buffer last-func)
		   (pmap-pixels image mod-buffer last-func))
	       (incf popi:n)
	       (rotatef image mod-buffer)
	       (invalidate-cache o)
	       (update-display o)))
	(condition (c)
	  (formatted-window "~a~%" c)
	  (throw 'flanky nil)
	  #| (continue) |#
	  )))
    (invalidate-cache o)))

(defun toggle-mode-line (o)
  (with-slots (show-mode-line) o
    (setf show-mode-line (not show-mode-line))))

(defun open-file (o)
  "Open a file."
  (with-slots (image) o
    (tt-move-to 0 0)
    (tt-erase-to-eol)
    (tt-cursor-on)
    (tt-finish-output)
    (let ((file-name (rl:read-filename :prompt "Open file: "))
	img)
      (with-image-error-handling (file-name)
	(setf img (perserverant-read-image (glob:expand-tilde file-name)))
	(clear-image o)
	(setf image img)
	(reset-image o)
	;; (invalidate-cache o)
	;; (redraw o)
	)
      (tt-cursor-off))))

(defun toggle-use-serial-map (o)
  "Toggle the value of use-serial-map."
  (with-slots (use-serial-map) o
    (setf use-serial-map (not use-serial-map))
    (message "Serial mapping is ~:[OFF~;ON~]" use-serial-map)))

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
    (#\f		  . fit-width-to-window)
    (#\F		  . fit-height-to-window)
    (#\|		  . flip-image-vertical)
    (#\\		  . flip-image-horizontal)
    (#\]		  . rotate-image-right)
    (#\[		  . rotate-image-left)
    (#\d		  . decrease-delay)
    (#\D		  . increase-delay)
    (:down		  . next)
    (:up		  . previous)
    (:left		  . backward-unit)
    (:right		  . forward-unit)
    (,(ctrl #\F)	  . next-page)
    (#\space	  	  . next-file)
    (,(ctrl #\V)	  . next-page)
    (:npage		  . next-page)
    (,(ctrl #\B)	  . previous-page)
    (#\b	          . previous-file)
    (,(meta-char #\v)	  . previous-page)
    (:ppage		  . previous-page)
    (#\>		  . move-to-bottom)
    (,(meta-char #\>)     . move-to-bottom)
    (:end		  . move-to-bottom)
    (#\<		  . move-to-top)
    (,(meta-char #\<)     . move-to-top)
    (:home		  . move-to-top)
    (#\c		  . center)
    (#\C		  . invalidate-cache)
    (,(ctrl #\a)	  . beginning-of-line)
    (,(ctrl #\e)	  . end-of-line)
    (#\+		  . zoom-in)
    (#\-		  . zoom-out)
    (#\=		  . zoom-reset)
    (,(meta-char #\n)     . next-file)
    (,(meta-char #\p)     . previous-file)
    (,(ctrl #\R)	  . reload-file)
    (,(meta-char #\l)     . toggle-looping)
    (#\t     		  . toggle-looping)
    (,(meta-char #\=)	  . binding-of-key)
    (,(meta-char #\escape) . eval-expression)
    (#\return		  . apply-pixel-expr)
    (:F12		  . apply-last-pixel-expr)
    (,(meta-char #\a)     . pixel-expr-loop)
    (:F11		  . pixel-expr-loop)
    (,(meta-char #\m)     . toggle-mode-line)
    (#\?		  . help)
    (,(ctrl #\@)	  . set-mark)
    (,(ctrl #\X)	  . *ctlx-keymap*)
    ))

(setf *image-viewer-escape-keymap* (build-escape-map *image-viewer-keymap*))

(defkeymap *ctlx-keymap* ()
  `((,(ctrl #\C)	. quit)
    (,(ctrl #\F)	. open-file)
    (,(ctrl #\P)	. toggle-use-serial-map)))

(defmethod await-event ((o image-inator))
  "Image viewer event."
  (with-slots (looping subimage image frame-start-time delay-factor
	       #| initial-command |#) o
    (with-slots ((subimages image::subimages)) image
      (if (and looping subimages)
	  (let* ((t-o (truncate (* delay-factor
				   (sub-image-delay (aref subimages subimage)))))
		 (time-left (and t-o
				 (dtime- (dtime+ frame-start-time
						 (make-dtime-as t-o :ms))
					 (get-dtime))))
		 ready result)
	    (when (dtime-plusp time-left)
	      (setf ready (tt-listen-for (dtime-to time-left :seconds))))
	    ;; (setf result
	    ;; 	  (terminal-ansi::get-char *terminal* :timeout 0))
	    (when (not ready)
	      (setf ready (tt-listen-for 0)))
	    (when ready
	      (setf result (tt-get-key)))
	    (when (not result)
	      (if (= subimage (1- (length subimages)))
		  (setf subimage 0)
		  (next-sub-image o)))
	    result)
	  (tt-get-key)))))

(defun show-status (o)
  "Display the status/message line."
  (with-slots (image message file-index file-list subimage zoom x y
	       move-object-mode delay-factor buffer bg-color) o
    (if (not image)
	(show-message o "--NO IMAGE--")
	(with-accessors ((name image-name)
			 (width image-width)
			 (height image-height)
			 (subimages image-subimages)) image
	  (if message
	      (progn
		(show-message o message)
		(setf message nil))
	      (let ((position (format nil "+~d+~d " x y))
		    (file-count (if (> (length file-list) 1)
				    (format nil "(file ~d of ~d) "
					    file-index (length file-list)) ""))
		    (frame-count (if (> (length subimages) 1)
				     (format nil "[frame ~d of ~d] " subimage
					     (length subimages)) ""))
		    (disposal (sub-image-disposal (aref subimages subimage))))
		(multiple-value-bind (start-x end-x start-y end-y) (old-clip o)
		  (let ((line
			 (format nil
				 "~a ~dx~d ~a~a~a~f% ~s <~d-~d ~d-~d> o:~s ~s ~
                                  ~:[b~;~]"
				 (typecase name
				   ((or string pathname)
				    (nos:path-file-name name))
				   (t name))
				 width height position file-count frame-count
				 zoom delay-factor
				 start-x end-x start-y end-y
				 move-object-mode
				 disposal
				 buffer
				 )))
		    (show-message o (subseq line 0 (min (length line)
							(1- (width o)))))))))))))

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

|#

(defun old-clip (o)
  (with-slots (x y image subimage zoom) o
    (with-slots ((subimages image::subimages)) image
      (with-accessors ((si-x sub-image-x) (si-y sub-image-y)
		       (width sub-image-width) (height sub-image-height))
	  (aref subimages subimage)
	(clip x y width height (width o) (height o) si-x si-y zoom)))))

(defun clip (x y width height view-width view-height si-x si-y zoom)
  (let* ((step (round 1 zoom))
	 (start-x (if (> x si-x) (- x si-x) 0))
	 (start-y (if (> y si-y) (- y si-y) 0))
	 (end-x (min width (- width
			      (- (+ si-x width)
				 (+ x (* step (1- view-width)))))))
	 (end-y (min height (- height
			       (- (+ si-y height)
				  (+ y (1- (* step (1- view-height)))))))))
    (values start-x end-x start-y end-y)))

(defun term-mover (dir n)
  (declare (type fixnum n) (type (member :down :right :forward) dir))
  (case dir
    (:down    (tt-move-to n 0))
    (:right   (tt-move-to-col n))
    (:forward (tt-forward n))))

(defun print-mover (dir n)
  (declare (type fixnum n) (type (member :down :right :forward) dir))
  (case dir
    (:down  (dotimes (i n) (tt-write-char #\newline)))
    ((:right :forward) (dotimes (i n) (tt-write-char #\space)))))

(declaim (inline set-pixel-fg))
(defun set-pixel-fg (r g b step)
  (tt-color (vector :rgb8
		    (truncate r (* step step))
		    (truncate g (* step step))
		    (truncate b (* step step)))
	    nil)
  (tt-write-char (code-char #x2588))) ; full_block

(declaim (inline set-pixel-bg))
(defun set-pixel-bg (r g b step)
  (tt-color nil (vector :rgb8
			(truncate r (* step step))
			(truncate g (* step step))
			(truncate b (* step step))))
  (tt-write-char #\space))

(defun output-image (x y zoom image subimage view-width view-height
		     mover setter buffer bg-color)
  (declare (type fixnum x y) (type float zoom)
	   (ignore bg-color)) ;; @@@
  (with-slots (name (subimages image::subimages)) image
    (with-accessors ((si-x     sub-image-x)
		     (si-y     sub-image-y)
		     (width    sub-image-width)
		     (height   sub-image-height)
		     (data     sub-image-data)
		     (disposal sub-image-disposal))
	(aref subimages subimage)
      (declare (type fixnum si-x si-y width height))
      (multiple-value-bind (start-x end-x start-y end-y)
	  (clip x y width height view-width view-height si-x si-y zoom)
	(let ((step (max 1 (round 1 zoom)))
	      (r 0) (g 0) (b 0) (a 0) source)
	  (flet ((mover-down (n) (funcall mover :down n))
		 (mover-right (n) (funcall mover :right n))
		 (mover-forward (n) (funcall mover :forward n))
		 (pixel-setter () (funcall setter r g b step)))
	  ;;(declare (type fixnum r g b a))
	  ;;(formatted-window "zoom = ~s step = ~s" zoom step)
	  (when (> si-y y)
	    ;;(tt-move-to (max y (truncate (- si-y y) step)) 0))
	    (mover-down (max y (truncate (- (+ si-y start-y) y) step))))
	  (loop ;; :with r = 0 :and g = 0 :and b = 0 :and a = 0
	     :for iy fixnum :from start-y :below end-y :by step :do
	     (when (> si-x x)
	       ;;(tt-move-to-col (max x (truncate (- si-x x) step))))
	       (mover-right (max x (truncate (- (+ si-x start-x) x) step))))
	     (loop 
		:for ix fixnum :from start-x :below end-x :by step :do
		(setf source (or buffer data))
		(setf r (loop :for av-y :from 0 :below step :sum
			   (loop :for av-x :from 0 :below step
			      :sum
			      ;; (aref source (min (1- height) (+ iy av-y))
			      ;; 	      (min (1- width) (+ ix av-x)) 0)
			      (pixel-r source
				       (min (1- height) (+ iy av-y))
				       (min (1- width) (+ ix av-x)))
			      )))
		(setf g (loop :for av-y :from 0 :below step :sum
			   (loop :for av-x :from 0 :below step
			      :sum
			      ;; (aref source (min (1- height) (+ iy av-y))
			      ;; 	      (min (1- width) (+ ix av-x)) 1)
			      (pixel-g source
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(setf b (loop :for av-y :from 0 :below step :sum
			   (loop :for av-x :from 0 :below step
			      :sum
			      ;; (aref source
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 2)
			      (pixel-b source
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(setf a (loop :for av-y :from 0 :below step :sum
			   (loop :for av-x :from 0 :below step
			      :sum
			      ;; (aref source
			      ;; 	    (min (1- height) (+ iy av-y))
			      ;; 	    (min (1- width) (+ ix av-x)) 3)
			      (pixel-a source
					   (min (1- height) (+ iy av-y))
					   (min (1- width) (+ ix av-x)))
			      )))
		(if (not (zerop a))
		    (progn
		      (pixel-setter)
		      #|
		      (tt-color nil (vector :rgb8
					    (truncate r (* step step))
					    (truncate g (* step step))
					    (truncate b (* step step))))
		      (tt-write-char #\space)
		      (tt-color (vector :rgb8
					(truncate r (* step step))
					(truncate g (* step step))
					(truncate b (* step step)))
				nil)
		      (tt-write-char (code-char #x2588)) ; full_block
		      |#
		      (tt-color :default :default))
		    (progn
		      ;; (tt-forward 1)
		      ;;(mover-right 1)
		      (mover-forward 1)))
		(setf r 0 g 0 b 0 a 0))
	     (when buffer
	       ;; Copy the whole subimage into to buffer.
	       (loop
		  :for iy fixnum :from si-y :below (+ si-y height)
		  :for source-y :from 0 :do
		  ;; (loop :for ix fixnum :from si-x :below (+ si-x width)
		  ;;    :for source-x :from 0 :do
		     ;; (set-pixel buffer iy ix
		     ;; 		(pixel-r data iy ix)
		     ;; 		(pixel-g data iy ix)
		     ;; 		(pixel-b data iy ix)
		     ;; 		(pixel-a data iy ix))
		     ;; (aref buffer iy ix 0) (aref data source-y source-x 0)
		     ;; (aref buffer iy ix 1) (aref data source-y source-x 1)
		     ;; (aref buffer iy ix 2) (aref data source-y source-x 2)
		     ;; (aref buffer iy ix 3) (aref data source-y source-x 3)
		     ;; (replace (row-major-aref buffer iy)
		     ;; 	   (row-major-aref data source-y) :start1 si-x)))
		  (set-row buffer y si-x
			   (make-array width
				       :element-type (array-element-type data)
				       :displaced-to data
				       :displaced-index-offset
				       (array-row-major-index data iy 0)))))
	     (tt-color :default :default)
	     (tt-write-char #\newline))))
	(tt-color :default :default)))))

(declaim (inline set-pixel-half))
(defun set-pixel-half (r1 g1 b1 r2 g2 b2)
  (tt-color (vector :rgb8 r1 g1 b1)
	    (vector :rgb8 r2 g2 b2))
  (tt-write-char (code-char #x2580))) ; #\upper_half_block

(defun output-image-half (x y zoom image subimage view-width view-height
			  mover setter buffer bg-color)
  (declare (type fixnum x y) (type float zoom)
	   (ignore setter))
  (with-slots (name (subimages image::subimages)) image
    (with-accessors ((si-x     sub-image-x)
		     (si-y     sub-image-y)
		     (width    sub-image-width)
		     (height   sub-image-height)
		     (data     sub-image-data)
		     (disposal sub-image-disposal))
	(aref subimages subimage)
      (declare (type fixnum si-x si-y width height))
      (multiple-value-bind (start-x end-x start-y end-y)
	  (clip x y width height view-width (* view-height 2) si-x si-y zoom)
	(let* ((step (max 1 (round 1 zoom)))
	       (ss (* step step))
	       (bg-r (color-component bg-color :red))
	       (bg-g (color-component bg-color :green))
	       (bg-b (color-component bg-color :blue))
	       (r1 bg-r) (g1 bg-g) (b1 bg-b) ;; (a1 0)
	       (r2 bg-r) (g2 bg-g) (b2 bg-b) ;; (a2 0)
	       source alpha-is-zero alpha (xx 0) (yy 0))
	  (macrolet
	      ((mover-down (n) `(funcall mover :down ,n))
	       (mover-right (n) `(funcall mover :right ,n))
	       (mover-forward (n) `(funcall mover :forward ,n))
	       ;; (pixel-setter () (funcall setter r g b step))
	       (pixi (ix iy comp pixer plus)
		 `(loop :for av-y :from 0 :below step :sum
		     (loop :for av-x :from 0 :below step :sum
			(progn
			  (setf xx (min (1- width) (+ ,ix av-x))
				yy (min (1- height) (+ ,iy ,plus av-y))
				alpha (pixel-a source yy xx))
			  (when (not (zerop alpha))
			    (setf alpha-is-zero nil))
			  (+ (* (/ alpha #xff) (,pixer source yy xx))
			     (* ,comp (/ (- #xff alpha) #xff))))))))
	  ;;(declare (type fixnum r g b a))
	  (declare (type fixnum step ss xx yy))
	  ;;(formatted-window "zoom = ~s step = ~s" zoom step)
	  (when (> si-y y)
	    ;;(tt-move-to (max y (truncate (- si-y y) step)) 0))
	    (mover-down (max y (truncate (- (+ si-y start-y) y) step))))
	  (loop ; vertical
	     :with iy fixnum = start-y
	     :while (< iy end-y) :do
	     (when (> si-x x)
	       ;;(tt-move-to-col (max x (truncate (- si-x x) step))))
	       (mover-right (max x (truncate (- (+ si-x start-x) x) step))))
	     (loop ; horizontal
		:for ix fixnum :from start-x :below end-x :by step :do
		(setf source (or buffer data)
		      alpha-is-zero t)
		(setf r1 (pixi ix iy bg-r pixel-r 0))
		(setf g1 (pixi ix iy bg-g pixel-g 0))
		(setf b1 (pixi ix iy bg-b pixel-b 0))
		(setf r1 (truncate r1 ss)
		      g1 (truncate g1 ss)
		      b1 (truncate b1 ss))
		(when (< (+ iy step) end-y)
		  (setf r2 (pixi ix iy bg-r pixel-r step))
		  (setf g2 (pixi ix iy bg-g pixel-g step))
		  (setf b2 (pixi ix iy bg-b pixel-b step))
		  (setf r2 (truncate r2 ss)
			g2 (truncate g2 ss)
			b2 (truncate b2 ss))
		  ;;(incf iy)
		  )
		;; @@@ The alpha multiply should really use whatever the
		;; background color is.
		(if alpha-is-zero
		    (progn
		      (mover-forward 1))
		    (progn
		      (set-pixel-half r1 g1 b1 r2 g2 b2)
		      (tt-color :default :default)))
		(setf r1 bg-r g1 bg-g b1 bg-b #| a1 0 |#
		      r2 bg-r g2 bg-g b2 bg-b #| a2 0 |#))
	     (incf iy (* 2 step))
	     (when buffer
	       ;; Copy the whole subimage into to buffer.
	       (loop
		  :for iy fixnum :from si-y :below (+ si-y height)
		  :for source-y :from 0 :do
		  ;; (loop :for ix fixnum :from si-x :below (+ si-x width)
		  ;;    :for source-x :from 0 :do
		     ;; (set-pixel buffer iy ix
		     ;; 		(pixel-r data iy ix)
		     ;; 		(pixel-g data iy ix)
		     ;; 		(pixel-b data iy ix)
		     ;; 		(pixel-a data iy ix))
		     ;; (aref buffer iy ix 0) (aref data source-y source-x 0)
		     ;; (aref buffer iy ix 1) (aref data source-y source-x 1)
		     ;; (aref buffer iy ix 2) (aref data source-y source-x 2)
		     ;; (aref buffer iy ix 3) (aref data source-y source-x 3)
		     ;; (replace (row-major-aref buffer iy)
		     ;; 	   (row-major-aref data source-y) :start1 si-x)))
		  (set-row buffer y si-x
			   (make-array width
				       :element-type (array-element-type data)
				       :displaced-to data
				       :displaced-index-offset
				       (array-row-major-index data iy 0)))))
	     (tt-color :default :default)
	     (tt-write-char #\newline))))
	(tt-color :default :default)))))

(defun print-image (file &key zoom width height errorp use-full)
  (let ((t-width (with-terminal () (tt-width)))
	;;(t-height (tt-height))
	(bg-color (lookup-color :black)))
    (with-terminal (:ansi-stream *terminal* :output-stream *standard-output*)
      ;; (format t "file = ~s~%" file)
      (catch 'git-out
	(handler-bind
	    (
	     ;; (cl-jpeg:jpeg-error
	     ;;  (lambda (c)
	     ;; 	(when (not errorp)
	     ;; 	  (format *error-output* "Error: ~a ~a~%" file c)
	     ;; 	  (throw 'git-out nil))))
	     (simple-error
	      (lambda (c)
		(when (not errorp)
		  (format *error-output* "Error: ~a ~a~%" file c)
		  (throw 'git-out nil)))))
	  (let* ((*show-progress* nil)
		 (image
		  (if (typep file 'image)
		      file
		      (perserverant-read-image file)))
		 (view-width (or width t-width))
		 (view-height (or height
				  (if use-full
				      (image-height image)
				      (/ (image-height image) 2))
				  ))
		 (our-zoom (or (and zoom (coerce zoom 'float))
			       (min 1.0 ;; teporarily @@@
				    (if height
					(float (/ height
						  (image-height image)))
					(float (/ view-width
						  (image-width image)))
					)))))
	    (declare (ignorable *show-progress*))
	    (if use-full
		(output-image 0 0 our-zoom image 0
			      view-width
			      view-height
			      #'print-mover
			      #'set-pixel-fg nil bg-color)
		(output-image-half 0 0 our-zoom image 0
			      view-width
			      view-height
			      #'print-mover
			      #'set-pixel-half nil bg-color))
	    ))))))

(defun show-image (inator)
  (with-slots (x y zoom message file-index file-list image subimage looping
	       show-mode-line use-half-block buffer bg-color) inator
    (declare (type fixnum x y) (type float zoom))
    (tt-home)
    ;; (when (not looping)
    ;;   (tt-clear))
    (when (not looping)
      (tt-erase-below))
    (when image
      (if use-half-block
	  (output-image-half
	   x y zoom image subimage (width inator) (height inator)
	   #'term-mover #'set-pixel-half buffer bg-color)
	  (output-image
	   x y zoom image subimage (width inator) (height inator)
	   #'term-mover #'set-pixel-bg buffer bg-color)))
    (tt-move-to (1- (height inator)) 0)
    (when show-mode-line
      (show-status inator))))

(defmethod update-display ((o image-inator))
  "Update the image viewer display."
  ;;(call-next-method)
  (setf (frame-start-time o) (get-dtime))
  (show-image o))

(defmethod start-inator ((o image-inator))
  (call-next-method)
  (with-slots (initial-command bg-color) o
    (when (or (typep *terminal* 'terminal-ansi:terminal-ansi)
	      (and (typep *terminal* 'terminal-wrapper)
		   (terminal-wrapped-terminal *terminal*)
		   (typep (terminal-wrapped-terminal *terminal*)
			  'terminal-ansi:terminal-ansi)))
      (setf bg-color
	    (convert-color-to
	     (or (tt-window-background)
		 #(:rgb8 0 0 0)) :rgb8)))
    (cond
      (initial-command
       (call-command o initial-command nil)
       (setf initial-command nil)
       (when (image-inator-image o)
	 (center *image-viewer*)
	 (fit-image-to-window *image-viewer*)))
      ((image-inator-image o)
       (center *image-viewer*)
       (fit-image-to-window *image-viewer*)))))

(defmethod resize ((o image-inator))
  (center *image-viewer*)
  (fit-image-to-window *image-viewer*)
  (call-next-method))

(defvar *image-inator-types* nil
  "A list of image inator types that are defined.")

(defun register-image-inator (type priority)
  (pushnew (cons priority type) *image-inator-types* :test #'equal))

(defun pick-image-inator ()
  (setf *image-inator-types*
	(sort-muffled *image-inator-types* #'> :key #'car))
  (loop :for i :in *image-inator-types*
     :when (image-inator-usable-p (cdr i))
     :return (cdr i)))

(defun set-auto-looping (image)
  "Turn on looping for multi-frame images."
  (when image
    (setf (image-inator-looping *image-viewer*)
	  (and (image-subimages image)
	       (> (length (image-subimages image)) 1)))))

(defun fix-scrolling (value)
  (when (eq :crunch (find-terminal-type-for-class
		     (class-name (class-of *terminal*))))
    (funcall (fdefinition `(setf ,(intern (string :allow-scrolling)
					  :terminal-crunch)))
	     value *terminal*)))

(defun view-image (image-designator &key file-list type own-window use-full
				      debug)
  "View an image. The IMAGE-DESIGNATOR can be a file name, a stream, an
object of class IMAGE:IMAGE, or an array of (unsigned-byte 8).
Key arguments:
  FILE-LIST   - A list of file names to view
  TYPE        - The type of image viewer to use
  OWN-WINDOW  - True to view the image in a new window, otherwise try to view
                it in the current window.
  USE-FULL    - True to use full block characters in the character based viewer."
  (with-terminal ()
    (let* ((inator-type (or type (pick-image-inator)))
	   image *image-viewer*)
      (when (not inator-type)
	(error "Can't find a usable image viewer."))
      (etypecase image-designator
	(image
	 (setf *image-viewer*
		(make-instance inator-type
			       :image image-designator
			       :file-list file-list
			       :own-window own-window
			       :debug debug
			       :use-half-block (not use-full)))
	 ;; (clear-image *image-viewer*)
	 ;; (reset-image *image-viewer*)
	 )
	(stream
	 (setf *image-viewer*
		(make-instance inator-type
			       :image (perserverant-read-image image-designator)
			       :file-list file-list
			       :own-window own-window
			       :debug debug
			       :use-half-block (not use-full)))
	 ;; (clear-image *image-viewer*)
	 ;; (reset-image *image-viewer*)
	 )
	((or string pathname)
	 (setf *image-viewer*
	       (make-instance inator-type
			      :initial-command 'next-file
			      :file-list (or file-list
					     (list image-designator))
			      :own-window own-window
			      :debug debug
			      :use-half-block (not use-full))))
	((array (unsigned-byte 8) *) ;; presumably encoded
	 (setf *image-viewer*
	       (make-instance inator-type
			      :image
			      ;; (perserverant-read-image image-designator)
			      (flexi-streams:with-input-from-sequence
				  (str image-designator)
				(perserverant-read-image str))
			      :file-list file-list
			      :own-window own-window
			      :debug debug
			      :use-half-block (not use-full))))
	(null
	 (when (not file-list)
	   (error "Must provide an image or a file-list."))
	  (setf *image-viewer*
		(make-instance inator-type
			       :initial-command 'next-file
			       :file-list file-list
			       :own-window own-window
			       :debug debug
			       :use-half-block (not use-full)))))
      (set-auto-looping image)
      (unwind-protect
        (progn
	  (tt-cursor-off)
	  (fix-scrolling nil)
	  (event-loop *image-viewer*))
	(tt-cursor-on)
	(fix-scrolling t))
      (inator-quit-flag *image-viewer*))))

(register-image-inator 'image-inator 1)

(defun view-images (files &rest args &key type own-window use-full debug)
  (declare (ignorable type own-window use-full debug))
  ;; (format t "type = ~s~%" type)
  (if (not files)
      (apply #'view-image *standard-input* args)
      (apply #'view-image (first files) :file-list files args)))

(defun image-inator-types ()
  (mapcar (_ (string (cdr _))) *image-inator-types*))

#+lish
(lish:defcommand view-image
  ((images pathname :repeating t :help "Image to view.")
   ;; @@@ take this out until args are fixed
   (type choice :short-arg #\t :optional t
    :choice-func 'image-inator-types
    :choice-test 'equalp
    :help "Type of image viewer to use.")
   (own-window boolean :short-arg #\o
    :help "True to use it's own window if the backend supports it.")
   (use-full boolean :short-arg #\f
    :help "True to use full blocks.")
   (debug boolean :short-arg #\d :help "Invoke the debugger on error.")
   ;;(images pathname :repeating t :help "Image to view.")
   )
  :accepts (:sequence :stream image:image)
  "View an image."
  ;;(let ((type nil))
  (view-images (or images
		   (and lish:*input* (if (not (listp lish:*input*))
					 (list lish:*input*)
					 lish:*input*))
		   (list *standard-input*))
	       :type (cdr (find type *image-inator-types*
				:key (_ (symbol-name (cdr _)))))
	       :own-window own-window
	       :use-full use-full
	       :debug debug))

#+lish
(lish:defcommand view-image-tty
  ((images pathname :repeating t :help "Image to view.")
   ;; @@@ take this out until args are fixed
   (use-full boolean :short-arg #\f
    :help "True to use full blocks.")
   ;;(images pathname :repeating t :help "Image to view.")
   )
  :accepts (:sequence :stream image:image)
  "View an image."
  ;;(let ((type nil))
  (view-images (or images
		   (and lish:*input* (list lish:*input*))
		   (list *standard-input*))
	       :type 'image-inator
	       :use-full use-full))

(defun cat-images (files &rest args &key zoom width height errorp use-full)
  (declare (ignorable zoom width height errorp use-full))
  (flet ((print-it (f)
	   (apply #'print-image f args)))
    (typecase files
      (null (print-it *standard-input*))
      ((or stream string) (print-it files))
      (list (map nil
		 (_ (with-simple-restart (continue "Continue to the next image.")
		      (print-it _)))
		 files)))))

#+lish
(lish:defcommand imgcat
  ((images pathname :repeating t :help "Images to cat.")
   (zoom number :short-arg #\z :help "Percent to zoom images.")
   (width number :short-arg #\w
    :help "Width to scale image to. Defaults to the terminal width.")
   (height number :short-arg #\h
    :help "Height to scale image to. Defaults to the appropriate height for ~
           the width.")
   (use-full boolean :short-arg #\f
    :help "True to use full blocks."))
  :accepts (:sequence :stream)
  "Print an image as terminal talk."
  (cat-images
   (or images lish:*input* *standard-input*)
   :width width
   :height height
   :zoom (when zoom (/ zoom 100))
   :use-full use-full))

;; EOF
