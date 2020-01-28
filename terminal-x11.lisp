;;
;; terminal-x11.lisp - X11 window as a terminal.
;;

;; I know this is temporary and relatively pointless, and maybe I should wait
;; util I can make a terminal-graf, but the idea is enticing. Of course this
;; is only part of having a Lispy terminal. It will work okay for Lisp only
;; programs, but to have it work for the shell, we'll have to make the stupid
;; old terminal emulator part that makes a pty and can fork process attached
;; to the slave end.

(defpackage :terminal-x11
  (:documentation "X11 window as a terminal.")
  (:use :cl :dlib :dlib-misc :terminal :trivial-gray-streams :xlib :collections
	:terminal-grid :ochar :fatchar :stretchy :keysyms)
  (:export
   #:terminal-x11
   ;; extensions:
   #:*selection-codes*
   #:selection
   #:set-selection
   #:foreground-color
   #:background-color
   #:set-foreground-color
   #:set-background-color
   ))
(in-package :terminal-x11)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

(defun get-display-from-environment ()
  "Return the display host and the the display number from the environment."
  (let ((display (nos:environment-variable "DISPLAY")) s)
    (and display
	 (setf s (split-sequence #\: display))
	 (values (first s) (parse-integer (second s))))))

(defun make-window-from (&key display id)
  #-clx-use-classes
  (xlib::make-window :id id :display display)
  #+clx-use-classes
  (make-instance 'xlib:window :id id :display display))

(defparameter *default-window-size* '(80 . 24)
  "The default size of a window in characters.")

(defparameter *default-font-name*
  "-misc-fixed-medium-r-*-*-20-*-iso10646-*"
  ;; "9x15" "fixed"
  "Name of the font to use if not specified.")

(defparameter *attributes*
  '(:normal
    :bold
    :faint
    :dim
    :italic
    :underline
    :blink
    :inverse
    :reverse
    :standout
    :invisible
    :crossed-out
    :double-underline))

(defparameter *attributes-off*
  '(:all
    :bold
    :faint
    :dim
    :italic
    :underline
    :blink
    :inverse
    :reverse
    :standout
    :invisible
    :crossed-out
    :double-underline))

(defstruct out-buf
  "Buffered output."
  row col string moved)

(defclass terminal-x11 (terminal #| terminal-crunch |#)
  ((display
    :initarg :display :accessor display
    :initform nil
    :documentation "The X display, the connection to the server.")
   (window
    :initarg :window :accessor window
    :initform nil
    :documentation "The window.")
   (depth
    :initarg :depth :accessor depth :initform nil
    :documentation "The depth of the display.")
   (window-width
    :initarg :window-width :accessor window-width
    :initform 0 :type fixnum
    :documentation "The width of the window in pixels.")
   (window-height
    :initarg :window-height :accessor window-height
    :initform 0 :type fixnum
    :documentation "The height of window in pixels.")
   (cell-width
    :initarg :cell-width :accessor cell-width :initform 0 :type fixnum
    :documentation "The width of cells in the character grid, in pixels.")
   (cell-height
    :initarg :cell-height :accessor cell-height :initform 0 :type fixnum
    :documentation "The height of the cells in character grid in pixels.")
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
   (cursor-gc
    :initarg :cursor-gc :accessor cursor-gc :initform nil
    :documentation "Graphics context for drawing the cursor.")
   (font-name
    :initarg :font-name
    ;; No accessor since it conflicts with xlib:font-name
    ;; :accessor font-name
    :initform ""
    :type string
    :documentation "Name of the font to use.")
   (bold-font-name
    :initarg :bold-font-name
    ;; No accessor since it conflicts with xlib:font-name
    ;; :accessor font-name
    :initform ""
    :type string
    :documentation "Name of the bold font to use.")
   (italic-font-name
    :initarg :italic-font-name :accessor italic-font-name
    :initform "" :type string
    :documentation "Name of the italic font to use.")
   (font
    :initarg :font :accessor font :initform nil
    :documentation "The font for text.")
   (bold-font
    :initarg :bold-font :accessor bold-font :initform nil
    :documentation "The font for bold text.")
   (bold-color
    :initarg :bold-color :accessor bold-color :initform nil
    :documentation "The color for bold text.")
   (italic-font
    :initarg :italic-font :accessor italic-font :initform nil
    :documentation "The font for italic text.")
   (title
    :initarg :title :accessor title :initform "Lisp Terminal" :type string
    :documentation "The title of the window.")
   (cursor-row
    :initarg :cursor-row :accessor cursor-row :initform 0 :type fixnum
    :documentation "The grid row the cursor is at.")
   (cursor-column
    :initarg :cursor-column :accessor cursor-column :initform 0 :type fixnum
    :documentation "The grid column the cursor is at.")
   (saved-cursor-position
    :initarg :saved-cursor-position :accessor saved-cursor-position
    :initform nil :type list
    :documentation "For saving and restoring the cursor position. NIL or a cons
of (column . row)")
   (cursor-state
    :initarg :cursor-state :accessor cursor-state :initform :visible
    :type (member :visible :invisible)
    :documentation "Display state of the cursor.")
   (cursor-rendition
    :initarg :cursor-rendition :accessor cursor-rendition
    :initform (make-fatchar) :type fatchar
    :documentation "Attributes for drawing the cursor.")
   (rendition
    :initarg :rendition :accessor rendition
    :initform (make-fatchar) :type fatchar
    :documentation "The current character attributes.")
   (foreground
    :initarg :foreground :accessor foreground :initform :white
    :documentation "The default foreground color for text. Note that this is
different from the current foreground color which in rendition.")
   (background
    :initarg :background :accessor background :initform :black
    :documentation "The background of the window.")
   (pixel-format
    :initarg :pixel-format :accessor pixel-format :initform nil
    :documentation "Format of pixels for this thing.")
   (lines
    :initarg :lines :accessor lines
    #| :initform (make-array) |# :type (or null (vector grid-string))
    :documentation "The character grid rows of the screen.")
   (scollback
    :initarg :scollback :accessor scollback :initform #() :type array
    :documentation "Lines of history.")
   (scrolling-region
    :initarg :scrolling-region :accessor scrolling-region
    :initform nil :type list
    :documentation "The lines definiing a region to scroll. A pair of
 (start-line . end-line) or NIL.")
   ;; @@@ I really don't want to implement line mode.
   (input-mode
    :initarg :input-mode :accessor input-mode :initform :line
    :type (member :line :char)
    :documentation "Fake input mode.")
   (saved-mode
    :initarg :saved-mode :accessor saved-mode
    :documentation "Saved terminal modes for restoring on exit.")
   (line-buffered-p
    :initarg :line-buffered-p :accessor line-buffered-p
    :initform nil :type boolean
    :documentation "True if we always flush after outputting a newline.")
   (pushback
    :initarg :pushback :accessor pushback :initform nil :type list
    :documentation "List of fake things to be read. For unread-char.")
   (modifiers
    :initarg :modifiers :accessor modifiers :initform nil :type list
    :documentation "List of keycodes that are modifiers.")
   (allow-send-events
    :initarg :allow-send-events :accessor allow-send-events
    :initform nil :type boolean
    :documentation "True to allow synthetic events sent by other programs.")
   (interrupt-key
    :initarg :interrupt-key :accessor interrupt-key
    :initform (char-util:ctrl #\c)
    :documentation "Key or character to throw an interrupt, or NIL for none.")
;;    (threads
;;     :initarg :threads :accessor threads :initform nil :type 
;;     :documentation "Threads running in the terminal, so we can kill them when
;; we close the window.")
   (output-buffer
    :initarg :output-buffer :accessor output-buffer :initform nil
    :documentation "Buffer for output.")
   (delay-scroll
    :initarg :delay-scroll :accessor delay-scroll :initform nil :type boolean
    :documentation "True to delay scrolling until the next character is output.")
   )
  (:default-initargs
    :file-descriptor		nil
    :device-name		(get-display-from-environment)
    :output-stream		nil
    :font-name			*default-font-name*
    :window-columns		80
    :window-rows		24
  )
  (:documentation "What we need to know about terminal device."))

(defmethod terminal-default-device-name ((type (eql 'terminal-x11)))
  "Return the default device name for a TERMINAL-X11."
  (get-display-from-environment))

(defun set-cell-size (tty)
  "Set the cell-width and cell-height in TTY from the font."
  (with-slots (font cell-width cell-height) tty
    (setf cell-width (xlib:char-width font (xlib:font-default-char font))
	  cell-height (+ (font-descent font) (font-ascent font)))))

(defparameter *crap* #| 4 |# 0)

(defun window-size (tty width height)
  "Return the window size in pixels for WIDTH and HEIGHT in characers in the
font. If the cell size isn't set, it will be set from the font. Don't assume
the window size is an exact multiple of the cell size."
  (with-slots (font cell-width cell-height) tty
    (when (or (zerop cell-width) (zerop cell-height))
      (set-cell-size tty))
    (values (+ (* cell-width width) *crap*)
	    (+ (* cell-height height) *crap*))))

(defvar *already-flushing* nil)

(defun flush-buffer (tty)
  (with-slots (output-buffer) tty
    (when (and output-buffer (not *already-flushing*))
      (let ((*already-flushing* t))
	(render-unit-string tty
			    (nreverse (out-buf-string output-buffer))
			    :row (out-buf-row output-buffer)
			    :col (out-buf-col output-buffer))
	(setf output-buffer nil)))))

(defun set-moved (tty)
  (with-slots (output-buffer) tty
    (when output-buffer
      (setf (out-buf-moved output-buffer) t))))

(defun make-new-grid (tty)
  "Make the character grid arrays for TTY assuming that the rows and columns
are already set."
  (with-slots (lines (window-rows terminal::window-rows)
		     (window-columns terminal::window-columns)) tty
    (setf lines (make-array window-rows :element-type 'grid-string))
    (dotimes (i window-rows)
      (setf (aref lines i)
	    (make-array window-columns
			:element-type 'grid-char
			:initial-element (make-grid-char))
	    ;; (aref index i) i
	    )
      (dotimes (j window-columns)
	(setf (aref (aref lines i) j) (blank-char))))))

(defun resize-grid (tty)
  (with-slots (lines (window-rows terminal::window-rows)
		     (window-columns terminal::window-columns)) tty
    (flush-buffer tty)
    (let ((old-lines lines))
      (setf lines (make-array window-rows :element-type 'grid-string))
      (dotimes (i window-rows)
	(setf (aref lines i)
	      (make-array window-columns
			  :element-type 'grid-char
			  :initial-element (make-grid-char))
	      ;; (aref index i) i
	      )
	(dotimes (j window-columns)
	  (setf (aref (aref lines i) j) (blank-char))))
      ;; @@@ Copy old screen
      (loop :with min-width
	 :for i :from 0 :below (min (length old-lines) (length lines))
	 :do
	 (setf min-width (min (length (aref old-lines i))
			      (length (aref lines i))))
	 (setf (osubseq (aref lines i) 0 min-width)
	       (osubseq (aref old-lines i) 0 min-width))))))

(defun dump-grid (tty)
  (with-slots (lines (window-rows terminal::window-rows)
		     (window-columns terminal::window-columns)) tty
    (loop :for y :from 0 :below (length lines) :do
       (loop :for x :from 0 :below (length (aref lines y)) :do
	  (format *trace-output* "~a" (grid-char-c (aref (aref lines y) x))))
       (terpri *trace-output*))
    (terpri *trace-output*)
    (finish-output *trace-output*)))

(defmethod initialize-instance
    :after ((o terminal-x11) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-x11."
  (with-slots (display window window-width window-height font title pixel-format
	       draw-gc erase-gc cursor-gc modifiers cell-width cell-height
	       foreground background rendition cursor-rendition) o
    (multiple-value-bind (host number) (get-display-from-environment)
      (when (not host)
	(error "Can't get X display from the environment."))
      (setf display (open-display host :display number))
      (when (not display)
	(error "Can't open the display ~a:~a." host number)))
    (let* ((screen (display-default-screen display))
	   (root (screen-root screen))
	   (black (screen-black-pixel screen))
	   (white (screen-white-pixel screen)))
      (setf
       font (open-font display (slot-value o 'font-name))
       (terminal-window-columns o) (car *default-window-size*)
       (terminal-window-rows o) (cdr *default-window-size*)
       (values window-width window-height)
       (window-size o (terminal-window-columns o) (terminal-window-rows o))
       window (create-window
	       :parent root
	       :x 0 :y 0
	       :width window-width
	       :height window-height
	       :background black
	       :event-mask 
	       (make-event-mask
		:key-press :button-press :button-release :button-motion
		:exposure :visibility-change :structure-notify))
       draw-gc (create-gcontext
		:drawable window
		:background black
		:foreground white
		:function boole-1
		;;:subwindow-mode :include-inferiors
		:font font)
       cursor-gc (create-gcontext
		  :drawable window
		  :background white
		  :foreground black
		  :function boole-1
		  ;;:subwindow-mode :include-inferiors
		  :font font)
       erase-gc (create-gcontext
		 :drawable window
		 :background black
		 :foreground white
		 :function boole-clr
		 ;;:subwindow-mode :include-inferiors
		 :font font)
       (wm-name window) title
       (wm-icon-name window) title)

      ;; Set up the window manager hints and stuff.
      (let ((wmh (make-wm-hints :input :on :initial-state :normal)))
	(setf (wm-hints window) wmh))
      (set-cell-size o)
      (let ((wmh (make-wm-size-hints
		  :width window-width :height window-height
		  ;; :base-width 2 :base-height 2
		  :base-width 0 :base-height 0
		  :min-width cell-width :min-height cell-height
		  :width-inc cell-width :height-inc cell-height)))
	(setf (wm-normal-hints window) wmh))
      (setf (wm-protocols window)
	    (list "WM_DELETE_WINDOW")
	    ;; (map 'vector #'char-code (s+ "WM_DELETE_WINDOW" #\nul))
	    rendition (make-fatchar :fg foreground :bg background)
	    cursor-rendition (or cursor-rendition
				 (getf initargs :cursor-rendition)
				 (make-fatchar :fg background :bg foreground)))
      (map-window window))
    (terminal-get-size o)
    (make-new-grid o)
    (setf pixel-format (get-pixel-format o)
	  modifiers (flatten (multiple-value-list (modifier-mapping display)))
	  cursor-gc (create-gcontext
		     :drawable window
		     :foreground (color-pixel o
				   (color-from-rendition o
				     (fatchar-fg cursor-rendition) :fg))
		     :background (color-pixel o
				   (color-from-rendition o
				     (fatchar-bg cursor-rendition) :bg))
		     :function boole-1
		     :font font))))

(defun set-grid-size-from-pixel-size (tty width height)
  (with-slots (cell-width cell-height
	       (window-columns terminal::window-columns)
               (window-rows terminal::window-rows)) tty
    (setf window-rows    (truncate height cell-height)
	  window-columns (truncate width cell-width))))

(defmethod terminal-get-size ((tty terminal-x11))
  "Get the window size from the server and store it in tty."
  (with-slots (window cell-wdith cell-height) tty
    (set-cell-size tty)
    (set-grid-size-from-pixel-size
     tty (drawable-width window) (drawable-height window))))

(defmethod terminal-get-cursor-position ((tty terminal-x11))
  "Get the row of the screen the cursor is on. Returns the two values ROW and
COLUMN."
  (values (cursor-row tty) (cursor-column tty)))

(defmethod terminal-start ((tty terminal-x11))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots (#| (file-descriptor	   terminal::file-descriptor)
	       (device-name   	   terminal::device-name)
	       (output-stream 	   terminal::output-stream) |#
	       saved-mode input-mode) tty
    (setf saved-mode input-mode
	  input-mode :char)
    ;; We don't need a file-descriptor or output-stream or a device-name right?
    (terminal-get-size tty)
    saved-mode))

(defmethod terminal-end ((tty terminal-x11) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  (terminal-finish-output tty)
  (when (or state (and (slot-boundp tty 'saved-mode)
		       (slot-value tty 'saved-mode)))
    (setf (input-mode tty) (or state (saved-mode tty)))))

(defmethod terminal-done ((tty terminal-x11) &optional state)
  "Forget about the whole terminal thing and stuff."
  (with-slots (window display) tty
    (terminal-end tty state)
    (display-finish-output display)
    (destroy-window window)
    (close-display display))
  (values))

(defmethod terminal-reinitialize ((tty terminal-x11))
  "Do any re-initialization necessary, and return the saved state."
  (with-slots (input-mode) tty
    (setf input-mode :char)
    (terminal-get-size tty)
    input-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @@@
;; Consider what or if we can share with terminal-crunch.
;; I considered making this a sub-class, but but it has problems with the
;; wrapped terminal being used in non-generic code.
;;
;; Perhaps there's some way I could just share the resulting grid from crunch
;; to x11 as the wrapped terminal? Otherwise we seem to be doubling up on the
;; grid maintainance.
;;
;; I already pulled the grid-char stuff out to grid.lisp, but I don't dare
;; replace it's use in terminal-crunch yet.

(defun line-blanker (x)
  "Blank out screen lines X."
  (loop :for line :across x :do
       (fill-by line #'blank-char)))

;; (defun xline-blanker (x)
;;   "Blank out screen lines X."
;;   (loop :for line :across x :do
;;        (fill-by line (lambda () (make-grid-char :c #\x)))))

(defun scroll-copy (n height array blanker)
  "Copy ARRAY for scrolling N lines in a HEIGHT window. BLANKER is a function
to blank with."
  (cond
    ((plusp n)
     (let ((new-blanks (subseq array 0 n))) ; save overwritten lines
       ;; Copy the retained lines up
       (setf (subseq array 0 (- height n))
	     (subseq array n height))
       ;; (dbugf :crunch "scroll-copy ~d ~d -> 0 ~d~%" n height (- height n))
       ;; Move the new blank lines in place.
       (setf (subseq array (- height n)) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))
    (t ;; minusp
     (let ((offset (abs n))
	   (new-blanks (subseq array (+ height n))))
       ;; Copy the retained lines down
       ;;(setf (subseq array (1+ offset))
       (setf (subseq array offset)
	     (subseq array 0 (+ height n)))
       ;; Move the new blank lines in place.
       (setf (subseq array 0 offset) new-blanks)
       ;; Blank out the newly blank lines
       (funcall blanker new-blanks)))))

;; @@@ It would be best to batch scrolling too. e.g. turn successive scrolls 1
;; into a scroll n.

(defun scroll (tty n)
  "Scroll by N lines."
  ;; (dbugf :crunch "(scroll ~s)~%" n)
  (with-slots ((window-rows terminal::window-rows)
	       (window-columns terminal::window-columns)
	       lines cell-width cell-height window draw-gc) tty
    ;; (when (not (zerop n))
    ;;   (no-hints tty))
    ;; (if-dbugf (:crunch) (dump-screen tty))
    (let ((abs-n (abs n)))
      (if (< abs-n window-rows)
	  (progn
	    (scroll-copy n window-rows lines #'line-blanker)
	    ;; (scroll-copy n window-rows index #'index-blanker)
	    (cond
	      ((plusp n)
	       (copy-area window draw-gc 0 (* cell-height n)
			  (* window-columns cell-height)
			  (* (- window-rows n) cell-height)
			  window
			  0 0)
	       (clear-area window
			   :x 0
			   :y (* (- window-rows n) cell-height)
			   :width (* window-columns cell-height)
			   :height (* n cell-height)))
	      (t ;; minusp
	       (copy-area window draw-gc 0 0
			  (* window-columns cell-height)
			  (* (- window-rows abs-n) cell-height)
			  window
			  0 (* cell-height abs-n))
	       (clear-area window :x 0 :y 0
			   :width (* window-columns cell-height)
			   :height (* abs-n cell-height))
	       )))
	  (progn
	    ;; Just erase everything
	    (line-blanker lines)
	    ;; (index-blanker index)
	    (clear-area window :x 0 :y 0
			:width (* window-columns cell-height)
			:height (* window-rows cell-height)))))))

(defun char-char (c)
  "Return the Lisp character from whatever other type of thing."
  (etypecase c
    (grid-char (grid-char-c c))
    (fatchar (fatchar-c c))
    (fatchar-string (if (= (length c) 1) (elt c 0) c))
    (string (if (= (length c) 1) (char c 0) c))
    (character c)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-color ((tty fg bg) &body body)
    "Evaluate the BODY with the current colors temporarily set to FG and BG."
    (with-unique-names (saved-fg saved-bg)
      `(let ((,saved-fg (fatchar-fg (rendition ,tty)))
	     (,saved-bg (fatchar-bg (rendition ,tty))))
	 (unwind-protect
	      (progn
		(%terminal-color ,tty ,fg ,bg)
		,@body)
	   (%terminal-color ,tty ,saved-fg ,saved-bg)))))

  (defmacro with-position ((tty row col) &body body)
    "Evaluate the BODY with the cursor temporarily at ROW COL."
    (with-unique-names (saved-row saved-col)
      `(let ((,saved-row ,row) (,saved-col ,col))
	 (setf (cursor-row ,tty) ,row
	       (cursor-column ,tty) ,col)
	 ,@body
	 (setf (cursor-row ,tty) ,saved-row
	       (cursor-column ,tty) ,saved-col))))

  (defmacro with-cursor-movement ((tty) &body body)
    "Wrap around something that moves the cursor to make the cursor be drawn."
    (with-unique-names (our-state tty-val)
      `(let ((,tty-val ,tty))
	 (with-slots ((,our-state cursor-state)) ,tty-val
	   (set-moved ,tty-val)
	   (when (eq ,our-state :visible)
	     (draw-cursor ,tty-val :state :invisible))
	   ,@body
	   (when (eq ,our-state :visible)
	     (draw-cursor ,tty-val)))))))

(defgeneric draw-thing (tty thing &key x y gc)
  (:documentation
   "Draw a string or character at a position with the graphics context."))

(defmethod draw-thing (tty (thing character) &key x y gc overstrike)
  (with-slots (window draw-gc cursor-column cursor-row font
	       cell-width cell-height) tty
    (when (graphic-char-p thing)
      (let ((cc (char-code (char-char thing)))
	    (func (if overstrike #'draw-glyph #'draw-image-glyph)))
	(funcall func window (or gc draw-gc)
		 (+ (* (or x cursor-column) cell-width)
		    (if overstrike 1 0))
		 (+ (* (or y cursor-row) cell-height)
		    (font-ascent font))
		 (if (> cc #xffff) #xfffd cc)
		 :size 16)))))

(defmethod draw-thing (tty (thing string) &key x y gc overstrike)
  (with-slots (window draw-gc cursor-column cursor-row font
	       cell-width cell-height) tty
    (let ((str (remove-if-not #'graphic-char-p thing))
	  (func (if overstrike #'draw-glyphs #'draw-image-glyphs)))
      (funcall func  window (or gc draw-gc)
	       (+ (* (or x cursor-column) cell-width)
		  (if overstrike 1 0))
	       (+ (* (or y cursor-row) cell-height)
		  (font-ascent font))
	       ;; (char-util:string-to-utf8-bytes str)
	       (map 'vector (_ (let ((cc (char-code _)))
				 (if (> cc #xffff) #xfffd cc))) str)
	       :size 16))))

;; A "unit" is what we are calling a character or a string with uniform
;; attributes that should be drawn at one position.

(defun render-unit-string (tty string &key row col)
  "Render a string with a uniform rendition on a single line."
  (with-slots (display window draw-gc font bold-font italic-font
	       cell-width cell-height cursor-column cursor-row) tty
    ;; (format t "type-of string ~s~%" (type-of string))
    ;; (format t "type-of gc ~s~%" (type-of (oelt string 0)))
    (let* ((gc (oelt string 0))
	   ;; (cc (any-char-c gc))
	   (fg (any-char-fg gc))
	   (bg (any-char-bg gc))
	   (attrs (any-char-attrs gc))
	   (plain-string
	    (with-output-to-string (str)
	      (omapn (_
		      ;; (format t "type-of _ ~s~%" (type-of _))
		      (princ (or (any-char-c _) #\space) str))
		     string))))
      ;; (let ((*print-length* 3))
      ;;  	(format t "render-unit-string ~s ~s ~s ~s ~s~%"
      ;;  		row col plain-string (type-of string) string))
      (when (not row)
	(setf row cursor-row))
      (when (not col)
	(setf col cursor-column))
      (cond
	;; ((not (or fg bg attrs))
	;;  ;; No effects
	;;  (draw-thing tty plain-string :x col :y row))
	;; ((and (or fg bg) (not attrs))
	;;  ;; Only colors
	;;  (with-color (tty fg bg)
	;;    (draw-thing tty plain-string :x col :y row)))
	((not attrs)
	 ;; Only colors
	 (with-color (tty fg bg)
	   (draw-thing tty plain-string :x col :y row)))
	(t ;; Attrs and maybe colors too
	 (let (bold faint dim italic underline blink inverse reverse standout
		    invisible crossed-out double-underline inverted dimmed
		    (use-font font))
	   (loop :for a :in attrs :do
	      (case a
		(:bold             (setf bold t))
		(:faint            (setf faint t))
		(:dim              (setf dim t))
		(:italic           (setf italic t))
		(:underline        (setf underline t))
		(:blink            (setf blink t))
		(:inverse          (setf inverse t))
		(:reverse          (setf reverse t))
		(:standout         (setf standout t))
		(:invisible        (setf invisible t))
		(:crossed-out      (setf crossed-out t))
		(:double-underline (setf double-underline t))))
	   (setf inverted (or inverse reverse standout)
		 dimmed (or faint dim))
	   (when (not invisible)
	     (with-color (tty fg bg)
	       (cond
		 ((or (and bold (not bold-font)) dimmed inverted)
		  ;; color change
		  (when (or dim faint)
		    (let* ((ffg (color:convert-color-to
				 (color-from-rendition tty fg :fg) :hsl))
			   (l (color:color-component ffg :lightness)))
		      (setf l (max 0.2 (- l .5))
			    (color:color-component ffg :lightness) l
			    fg ffg)))
		  (when (and bold (not bold-font))
		    ;; use bold color or increase value
		    (let* ((bfg (color:convert-color-to
				 (color-from-rendition tty fg :fg) :hsl))
			   (l (color:color-component bfg :lightness))
			   (s (color:color-component bfg :saturation)))
		      (setf l (min 1.0 (+ l .3))
			    s (if (= l 1.0) (+ s 0.3) s)
			    (color:color-component bfg :lightness) l
			    (color:color-component bfg :saturation) s
			    fg bfg)))
		  (when inverted ;; switch fg & bg
		    (rotatef fg bg))
		  ;; (setf fg (color:convert-color-to fg :rgb8)
		  ;; 	bg (color:convert-color-to bg :rgb8))
		  ;; (format t "fg ~s bg ~s~%" fg bg)
		  ;; (finish-output *standard-output*)
		  (%terminal-color tty fg bg))
		 ((and italic italic-font)
		  (setf use-font italic-font))
		 ((and bold bold-font)
		  (setf use-font bold-font)))
	       (draw-thing tty plain-string :x col :y row)
	       (when (and bold (not bold-font))
		 ;; Overstrike for bold
		 (draw-thing tty plain-string :x col :y row
			     :overstrike t))
	       (when (or underline crossed-out double-underline)
		 ;; Draw lines over characters
		 (let* ((start-x (* col cell-width))
			(start-y (+ (* row cell-height)
				    (font-ascent font)))
			(end-x (+ start-x
				  (* cell-width
				     (char-util:display-length plain-string))))
			(half-y (- start-y
				   (round (* (font-ascent font) 3) 8))))
		   (cond
		     ((and underline (not double-underline))
		      (draw-line window draw-gc start-x (1+ start-y)
				 end-x (1+ start-y)))
		     (double-underline
		      (draw-line window draw-gc start-x start-y end-x start-y)
		      (draw-line window draw-gc start-x (+ start-y 2)
				 end-x (+ start-y 2))))
		   (when crossed-out
		     (draw-line window
				draw-gc start-x half-y end-x half-y))))))))))))

(defun copy-char-to-grid (tty char)
  "Put the CHAR at the current screen postion."
  (with-slots (rendition delay-scroll cursor-row cursor-column output-buffer
	       (window-rows terminal::window-rows)
	       (window-columns terminal::window-columns) lines) tty
    (let (changed (last-char (and output-buffer
				  (first (out-buf-string output-buffer)))))
      (labels ((regularize (c)
		 (etypecase c
		   ((or fatchar character) c)
		   (string (case (length c)
			     (0 nil)
			     (1 (char c 0))
			     (otherwise c)))))
	       (set-char (gc char)
		 (etypecase char
		   ((or character string)
		    (let* ((rc (regularize char))
			   (new-gc
			    (make-grid-char
			     :c rc
			     :fg (fatchar-fg rendition)
			     :bg (fatchar-bg rendition)
			     :attrs (fatchar-attrs rendition)
			     :line 0))) ;; unless it's a line char??
		      (when (not (grid-char= gc new-gc))
			(setf (grid-char-c gc) rc
			      (grid-char-fg gc) (fatchar-fg rendition)
			      (grid-char-bg gc) (fatchar-bg rendition)
			      (grid-char-attrs gc) (fatchar-attrs rendition)
			      (grid-char-line gc) 0
			      changed t))))
		   (fatchar
		    (when (not (grid-char= gc char))
		      (setf (grid-char-c gc)     (fatchar-c char)
			    (grid-char-fg gc)    (or (fatchar-fg char)
						     (fatchar-fg rendition))
			    (grid-char-bg gc)    (or (fatchar-bg char)
						     (fatchar-fg rendition))
			    (grid-char-attrs gc) (intersection
						  (fatchar-attrs char)
						  (fatchar-fg rendition))
			    (grid-char-line gc)  (fatchar-line char)
			    changed t)))
		   (grid-char
		    (when (not (grid-char= gc char))
		      (set-grid-char gc char) ;; @@@ merge rendition??
		      (setf changed t)))))
	       (push-buf (c)
		 ;; (format t "push ~s~%" c)
		 (if output-buffer
		     (push (copy-grid-char c) (out-buf-string output-buffer))
		     (setf output-buffer
			   (make-out-buf :row cursor-row :col cursor-column
					 :string (list c)))))
	       (moved ()
		 (if output-buffer
		     (out-buf-moved output-buffer)
		     nil))
	       (put-char (gc char)
		 "Put a char in the grid and output with buffering."
		 (set-char gc char)
		 (cond
		   ((not output-buffer)
		    (push-buf gc))
		   ((and (grid-char-same-effects gc last-char) (not (moved)))
		    (push-buf gc))
		   (t ; different effects than last or moved
		    (when output-buffer
		      (flush-buffer tty))
		    (set-moved tty)
		    (push-buf gc)))
		 (setf last-char gc))
	       (scroll-one-line ()
		 ;; (no-hints tty)
		 (when output-buffer
		   (flush-buffer tty))
		 (scroll tty 1)
		 (setf cursor-column 0 changed t)
		 ;; (set-moved tty)
		 )
	       (delayed-scroll ()
		 (when delay-scroll
		   (setf delay-scroll nil)
		   ;; Actually scroll when in the bottom right corner.
		   (when (and (= cursor-row (1- window-rows))
			      (= cursor-column (1- window-columns)))
		     ;; (dbugf :crunch "Delayed scroll~%")
		     (scroll-one-line))))
	       (next-line ()
		 (if (< cursor-row (1- window-rows))
		     (progn
		       (incf cursor-row)
		       (setf cursor-column 0)
		       (set-moved tty))
		     (when t #| (allow-scrolling tty) @@@ |#
		       (if (= cursor-column (1- window-columns))
			   (progn
			     ;; @@@ horrible
			     ;; (dbugf :crunch "Delaying scroll @ ~d ~d~%" x y)
			     (setf delay-scroll t))
			   (progn
			     ;; (dbugf :crunch "next-line scroll-one-line~%")
			     (scroll-one-line)))))))
	(case (char-char char)
	  (#\newline
	   (delayed-scroll)
	   ;; (terminal-erase-to-eol tty)
	   (setf cursor-column 0 changed t)
	   (set-moved tty)
	   (next-line))
	  (#\return
	   (setf cursor-column 0)
	   (set-moved tty))
	  (#\backspace
	   (setf cursor-column (max 0 (1- cursor-column)))
	   (set-moved tty))
	  (#\tab
	   (let ((new-x
		  (+ cursor-column
		     (- (1+ (logior 7 cursor-column)) cursor-column))))
	     ;; @@@ should tabs actually wrap?
	     (setf cursor-column (min new-x (1- window-columns)))
	     (set-moved tty)))
	  (t
	   (delayed-scroll)
	   (put-char (aref (aref lines cursor-row) cursor-column) char)
	   ;; (format *trace-output* "put-char ~s ~s~%" cursor-column cursor-row)
	   ;; (finish-output *trace-output*)
	   ;; (when changed
	   ;;   (note-single-line tty))
	   (let* ((len (char-util:display-length char))
		  (new-x (+ cursor-column len)))
	     (if (< new-x window-columns)
		 (progn
		   (when (> len 1)
		     ;; "Underchar removal"
		     (unset-grid-char (aref (aref lines cursor-row)
					    (1+ cursor-column))))
		   (setf cursor-column new-x)
		   (set-moved tty))
		 (next-line)))
	   ))
	)
      changed)))

(defun copy-string-to-grid (tty string &key start end)
  "Copy the STRING from START to END to the screen. Return true if we actually
changed the screen contents."
  (with-slots (rendition) tty
    (loop
       :with changed :and gchar
       ;; :and str = (if (or (and start (not (zerop start))) end)
       ;; 		      (if end
       ;; 			  (displaced-subseq string (or start 0) end)
       ;; 			  (displaced-subseq string start))
       ;; 		      string)
       ;; :with len = (or end (length string))
       ;; :while (< i len)
       :for c :in (char-util:graphemes
		   (cond ;; @@@ What's better? this or splicing?
		     ((and start end) (osubseq string start end))
		     (start (osubseq string start))
		     (end (osubseq string 0 end))
		     (t string)))
       :do
       ;; Make sure the color is set properly in the upgraded char
       (setf gchar (grapheme-to-grid-char c))
       (when (typep c '(or string character))
	 (setf (grid-char-fg gchar) (fatchar-fg rendition)
	       (grid-char-bg gchar) (fatchar-bg rendition)
	       (grid-char-attrs gchar) (fatchar-attrs rendition)))
       (with-cursor-movement (tty)
	 (when (copy-char-to-grid tty gchar)
	   (setf changed t)))
       ;;(incf i)
       :finally
       (return changed))))

(defun copy-text-to-grid (tty text)
  (etypecase text
    ((or character fatchar grid-char)
     (with-cursor-movement (tty)
       (copy-char-to-grid tty text)))
    ((or string fat-string) (copy-string-to-grid tty text))))

#|
(defun update-column-for-char (tty char)
  (with-slots (cursor-column window-columns) tty
    (cond
      ((graphic-char-p char)
       (incf cursor-column (char-util:display-length char))
       (when (> cursor-column window-columns)
	 (setf cursor-column 0))
      (t
       (case char
	 (#\return
	  (setf fake-column 0))
	 (#\tab
	  (incf fake-column (- (1+ (logior 7 fake-column)) fake-column)))
	 (otherwise
	  0 ;; some non-graphic control char?
	  ))))))

(defun update-column (tty thing &key start end)
  (etypecase thing
    (character (update-column-for-char tty thing))
    (string
     (loop
	:with the-end = (or end (length thing))
	:and the-start = (or start 0)
	:for i :from the-start :below the-end
	:do (update-column-for-char tty (char thing i))))))
|#

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defun color-from-rendition (tty color type)
  (assert (member type '(:fg :bg)))
  (cond
    ((or (not color) (eq color :default))
     (color:lookup-color (case type
			   (:fg (foreground tty))
			   (:bg (background tty)))))
    ((color:structured-color-p color) color)
    (t (color:lookup-color color))))

(defun %terminal-color (tty fg bg)
  ;; (format t "%terminal-color fg ~s bg ~s~%" fg bg)
  (with-slots (draw-gc rendition) tty
    (flush-buffer tty)
    (let ((fg-pos (and (keywordp fg) (position fg *colors*)))
	  (bg-pos (and (keywordp bg) (position bg *colors*))))
      (when (and (keywordp fg) (not fg-pos))
	(error "Forground ~a is not a known color." fg))
      (when (and (keywordp bg) (not bg-pos))
	(error "Background ~a is not a known color." bg))
      (setf (gcontext-foreground draw-gc)
	    (color-pixel tty (color-from-rendition tty fg :fg)))
      (setf (gcontext-background draw-gc)
	    (color-pixel tty (color-from-rendition tty bg :bg)))
      (setf (fatchar-fg rendition) fg
	    (fatchar-bg rendition) bg))))

#|
(defun %draw-fat-unit (tty unit &key copy x y)
  "Output a unit to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (with-slots (display window rendition draw-gc font bold-font italic-font
	       cell-width cell-height cursor-column cursor-row) tty
    (let ((style (or (typecase unit
		       (fatchar unit)
		       (fat-string (oelt unit 0)) ;; assuming len > 1
		       ((or character string)
			rendition))))
	  (plain-unit (osimplify unit))
	  (col (or x cursor-column))
	  (row (or y cursor-row)))
      (with-slots ((cc fatchar::c)
		   (fg fatchar::fg)
		   (bg fatchar::bg)
		   (line fatchar::line)
		   (attrs fatchar::attrs)) style
	;; We still do this dumb replacing, just in case.
	;; (when (and (translate-alternate-characters tty)
	;; 		 (characterp cc))
	;; 	(let ((replacement (gethash cc *acs-table*)))
	;; 	  (when replacement
	;; 	    (setf cc replacement))))
	(cond
	  ((not (or fg bg attrs))
	   ;; No effects
	   (etypecase plain-unit
	     (character
	      (draw-thing tty _ :x x :y y)
	      (copy-char-to-grid tty _))
	     (string
	      (with-position (y x)
		(map nil (_ (draw-thing tty _)
			    (copy-char-to-grid tty _)) plain-unit))))
	   )
	  ((and (or fg bg) (not attrs))
	   ;; Only colors
	   (with-color (tty fg bg)
	     (draw-thing tty plain-unit :x col :y row)
	     (when copy
	       (copy-text-to-grid tty plain-unit))))
	  (t ;; Attrs and maybe colors too
	   (let (bold faint dim italic underline blink inverse reverse standout
		 invisible crossed-out double-underline inverted dimmed
		 (use-font font))
	     (with-color (tty fg bg)
	       (loop :for a :in attrs :do
		  (case a
		    (:bold             (setf bold t))
		    (:faint            (setf faint t))
		    (:dim              (setf dim t))
		    (:italic           (setf italic t))
		    (:underline        (setf underline t))
		    (:blink            (setf blink t))
		    (:inverse          (setf inverse t))
		    (:reverse          (setf reverse t))
		    (:standout         (setf standout t))
		    (:invisible        (setf invisible t))
		    (:crossed-out      (setf crossed-out t))
		    (:double-underline (setf double-underline t))))
	       (setf inverted (or inverse reverse standout)
		     dimmed (or faint dim))
	       (cond
		 ((or (and bold (not bold-font))
		      dimmed inverted)
		  ;; color change
		  (when dim ;; lower value
		    )
		  (when faint ;; even lower value
		    )
		  (when inverted ;; switch fg & bg
		    (rotatef fg bg))
		  (when (and bold (not bold-font))
		    ;; use bold color or increase value
		    )
		  ;; (%terminal-color tty fg bg)
		  )
		 ((and italic italic-font)
		  (setf use-font italic-font))
		 ((and bold bold-font)
		  (setf use-font bold-font)))
	       (when (not invisible)
		 (with-color (tty fg bg)
		   (draw-thing tty plain-unit :x col :y row)
		   (when (or underline crossed-out double-underline)
		     (let* ((start-x (* col cell-width))
			    (start-y (+ (* row cell-height)
					(font-ascent font)))
			    (end-x (+ start-x
				      (* cell-width
					 (char-util:display-length unit))))
			    (half-y (- start-y
				       (truncate (font-ascent font) 2))))
		       (when (or underline double-underline)
			 (draw-line window draw-gc
				    start-x start-y end-x start-y))
		       (when crossed-out
			 (draw-line window draw-gc start-x half-y end-x half-y))
		       (when double-underline
			 (draw-line window draw-gc
				    start-x (+ start-y 2)
				    end-x (+ start-y 2)))))
		   (when copy
		     (copy-text-to-grid tty unit))))
	       ;; (if (zerop line)
	       ;;     (write-char cc stream)
	       ;;     (write-char (line-char line) stream))
	       ;; (copy-char-to-grid tty cc)
	       (when (and (line-buffered-p tty) (eql cc #\newline))
		 (display-finish-output display))))))))))
|#

(defun %write-text (tty unit)
  ;; (%draw-fat-unit tty unit :copy t)
  (copy-text-to-grid tty unit)
  )

(defun %draw-fat-string (tty str &key start end x y)
  "Draw a fat string STR on TTY from START to END at X Y, without copying it
to the grid. It must be on a single line and have no motion characters."
  (when (and (not (and (and start (zerop start)) (and end (zerop end))))
	     (fat-string-string str))
    (let ((fs (fat-string-string str))
	  (substr (make-fat-string))
	  ;;(translate (translate-alternate-characters tty))
	  had-newline #|replacement|#)
      ;; (format t ">> draw-fat-string ~s ~s~%" str (olength str))
      (loop
	 :with i = (or start 0)
	 :and our-end = (or (and end (min end (length fs))) (length fs))
	 :and c :and last-c
	 :and unit-start = 0
	 :while (< i our-end)
	 :do
	 (setf c (aref fs i))
	 (with-slots ((cc fatchar::c)
		      #| (line fatchar::line) |#
		      ) c
	   (when (and last-c (not (same-effects c last-c)))
	     ;; (if (zerop line)
	     ;; 	   (if (and translate
	     ;; 		    (setf replacement (gethash cc *acs-table*)))
	     ;; 	       (write-char replacement stream)
	     ;; 	       (write-char cc stream))
	     ;; 	   (write-char (line-char line) stream))
	     (when (> (- i 1 unit-start) 0)
	       ;; (setf (fat-string-string substr) (osubseq fs unit-start (1- i)))
	       (setf (fat-string-string substr) (osubseq fs unit-start i))
	       (render-unit-string tty substr :row y :col (+ unit-start x)))
	     (setf unit-start i))
	   (setf last-c c)
	   (when (char= cc #\newline)
	     (setf had-newline t)))
	 (incf i)
	 :finally
	 (when (> (- i 1 unit-start) 0)
	   (setf (fat-string-string substr)
		 (osubseq fs unit-start (min (length fs) i)))
	   ;; (format t "draw-fat-string ~s ~s~%" substr (olength substr))
	   (render-unit-string tty substr :row y :col (+ x unit-start))))
      had-newline)))

;; (defun %write-fat-string (tty str start end)
;;   (%draw-fat-string tty str :start start :end end :copy t))

(defun %write-string (tty str start end)
  (when (not (and (and start (zerop start)) (and end (zerop end))))
    (%write-text tty (if (or start end)
			 (osubseq str (or start 0) end)
			 str))))
#|
    (etypecase str
      (string
       (%draw-fat-unit tty (if (or start end)
			       (subseq str (or start 0) end)
			       str) :copy t))
      (fat-string
       (%write-fat-string tty str start end)
       ;; (apply #'%write-fat-string `(,tty ,str
       ;; 					 ,@(and start `(:start ,start))
       ;; 					 ,@(and end `(:end ,end))))
       ))))
|#

(defparameter *line-table-unicode*
  `#(,#\space
     ,(code-char #x2577) ;; #\box_drawings_light_down)                     ╷
     ,(code-char #x2576) ;; #\box_drawings_light_right)                    ╶
     ,(code-char #x250c) ;; #\box_drawings_light_down_and_right)           ┌
     ,(code-char #x2575) ;; #\box_drawings_light_up)                       ╵
     ,(code-char #x2502) ;; #\box_drawings_light_vertical)                 │
     ,(code-char #x2514) ;; #\box_drawings_light_up_and_right)             └
     ,(code-char #x251c) ;; #\box_drawings_light_vertical_and_right)       ├
     ,(code-char #x2574) ;; #\box_drawings_light_left)                     ╴
     ,(code-char #x2510) ;; #\box_drawings_light_down_and_left)            ┐
     ,(code-char #x2500) ;; #\box_drawings_light_horizontal)               ─
     ,(code-char #x252c) ;; #\box_drawings_light_down_and_horizontal)      ┬
     ,(code-char #x2518) ;; #\box_drawings_light_up_and_left)              ┘
     ,(code-char #x2524) ;; #\box_drawings_light_vertical_and_left)        ┤
     ,(code-char #x2534) ;; #\box_drawings_light_up_and_horizontal)        ┴
     ,(code-char #x253c) ;; #\box_drawings_light_vertical_and_horizontal)  ┼
     )
  "Line drawing characters from Unicode.")

(defparameter *line-table-vt100*
  `#(#\space ;;            0 - 0000 - blank
     #\x     ;; VLINE      1 - 0001 - bottom
     #\q     ;; HLINE      2 - 0010 - right
     #\l     ;; ULCORNER   3 - 0011 - bottom + right
     #\x     ;; VLINE      4 - 0100 - top
     #\x     ;; VLINE      5 - 0101 - top + bottom
     #\m     ;; LLCORNER   6 - 0110 - top + right
     #\t     ;; LTEE       7 - 0111 - bottom + right + top
     #\q     ;; HLINE      8 - 1000 - left
     #\k     ;; URCORNER   9 - 1001 - left + bottom
     #\q     ;; HLINE     10 - 1010 - left + right
     #\w     ;; TTEE      11 - 1011 - left + right + bottom
     #\j     ;; LRCORNER  12 - 1100 - left + top
     #\t     ;; RTEE      13 - 1101 - left + top + bottom
     #\v     ;; BTEE      14 - 1110 - left + top + right
     #\n     ;; PLUS      15 - 1111 - left + top + right + bottom
     ))

(defparameter *line-table* *line-table-unicode* ;; *line-table-vt100*
  "The table to use for looking up line drawing characters.")

(defun line-char (line)
  "Convert line bits into line drawing characters."
  (aref *line-table* line))

;; @@@ It's possible we might need to get depth out of out xlib:screen-depths
(defstruct pixel-format
  name
  class
  bits-per-rgb
  red-mask
  green-mask
  blue-mask)

(defparameter *pixel-formats*
  `(,(make-pixel-format
      :name :RGB8
      :class :true-color
      :bits-per-rgb 8
      :red-mask   #xff0000
      :green-mask #x00ff00
      :blue-mask  #x0000ff)
     ,(make-pixel-format
       :name :RGB565
       :class :true-color		; correct ?
       :red-mask   #xf800
       :green-mask #x07e0
       :blue-mask  #x001f))
  "List of known pixel formats.")

(defun get-pixel-format (tty)
  "Return the pixel format for TTY."
  (loop :with v = (xlib:window-visual-info (window tty))
     :for f :in *pixel-formats*
     :do
     (when (and
	    (eq (pixel-format-class        f) (visual-info-class        v))
	    (=  (pixel-format-bits-per-rgb f) (visual-info-bits-per-rgb v))
	    (=  (pixel-format-red-mask     f) (visual-info-red-mask     v))
	    (=  (pixel-format-green-mask   f) (visual-info-green-mask   v))
	    (=  (pixel-format-blue-mask    f) (visual-info-blue-mask    v)))
       (return (pixel-format-name f)))))

(defun color-pixel (tty color)
  "Return a pixel value for a color."
  ;; In the olden days we might have to allocate a color, but takes a lot
  ;; of work and server traffic, so we just assume we can make the color
  ;; ourselves given the data in the visual.
  (let ((c (color:convert-color-to color :rgb8)))
    (with-slots (pixel-format) tty
      (case pixel-format
	(:rgb8
	 (logior (ash (color:color-component c :red)   16)
		 (ash (color:color-component c :green) 8)
		      (color:color-component c :blue)))
	(:rgb565
	 ;; This is mostly just an example. I don't think it will happen,
	 ;; unless you're using an ancient graphics card. It's more likely
	 ;; someone will have to add 30, 36, or 48 bit color.
	 (logior (logand (ash (ash (color:color-component c :red  ) -3) 11) #x1f)
		 (logand (ash (ash (color:color-component c :green) -2) 5)  #x3f)
		 (logand      (ash (color:color-component c :blue ) -3)     #x1f)))
	(otherwise
	 ;; This probably means someone should write a new case here.
	 (error "Unknown pixel format."))))))

(defun char-at (tty row column)
  "Return the character at the ROW and COLUMN"
  (with-slots (lines) tty
    (aref (aref lines row) column)))

(defun draw-cursor (tty &key state)
  "Draw the cursor."
  (with-slots (cursor-gc draw-gc cursor-row cursor-column cursor-state) tty
    ;; (format t "cursor char = ~s ~a~%" (char-at tty cursor-row cursor-column)
    ;; 	    (type-of (char-at tty cursor-row cursor-column)))
    (let* ((gchar (char-at tty cursor-row cursor-column))
	   (c (or (osimplify gchar) #\space))
	   (%state (or state cursor-state)))
      (if (eq %state :visible)
	  (draw-thing tty c :gc cursor-gc)
	  (let ((str (vector (or gchar
				 (make-fatchar
				  :c #\space
				  :bg (background tty)
				  :fg (foreground tty))))))
	    (render-unit-string tty str :row cursor-row :col cursor-column))))))

(defun erase-grid (tty start-x start-y end-x end-y)
  "Erase an area of the character grid."
  (with-slots (lines) tty
    (loop :for y :from start-y :to end-y :do
       (loop :for x :from start-x :to end-x :do
	  (unset-grid-char (aref (aref lines y) x))))))

(defun clear-text (tty start-x start-y end-x end-y &key erase)
  "Clear an area of the window, with coordinates in character cells."
  (with-slots (window font cell-width cell-height) tty
    (when erase
      (erase-grid tty start-x start-y end-x end-y))
    (clear-area window
		:x (* start-x cell-width)
		:y (* start-y cell-height)
		:width (* (1+ (- end-x start-x)) cell-width)
		:height (* (1+ (- end-y start-y)) cell-height)
			   ;; (+ (font-ascent font) (font-descent font))
			   )))

(defun redraw-area (tty start-x start-y width height)
  "Redraw a rectangular area."
  (with-slots (lines cell-width cell-height window draw-gc
	       (window-rows terminal::window-rows)
	       (window-columns terminal::window-columns)) tty
    (let ((c-start-x (1- (ceiling start-x cell-width)))
	  (c-start-y (1- (ceiling start-y cell-height)))
	  (c-width   (1+ (ceiling width cell-width)))
	  (c-height  (1+ (ceiling height cell-height))))
      (setf c-start-x (max 0 c-start-x)
	    c-start-y (max 0 c-start-y))
      (dbugf :tx11 "redraw-area ~s ~s ~s ~s~%"
	     c-start-x c-start-y c-width c-height)
      ;; (draw-rectangle window draw-gc start-x start-y width height)
      (loop
	 :with actual-end :and str :and output-start
	 :for y :from (max 0 c-start-y)
	 :to (clamp (+ c-start-y c-height) 0 (1- window-rows))
	 :do
	 (setf actual-end
	       (clamp (+ c-start-x c-width) 0 (min (length (aref lines y))
						   (1- window-columns)))
	       (values str output-start) (grid-to-fat-string
					  (aref lines y)
					  :start c-start-x
					  :end actual-end
					  :null-as #\space))
	 ;;(format t "str ~s ~s output-start ~s~%" (olength str) str output-start)
	 (when (and (plusp (- actual-end c-start-x))
		    (not (zerop (olength str))))
	   (%draw-fat-string
	    tty str :start c-start-x :end actual-end
	    :x (+ c-start-x output-start) :y y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminal methods

(defmethod terminal-format ((tty terminal-x11) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'format nil fmt args)))
    (apply #'format tty fmt args)
    ;; (copy-string-to-grid tty string)
    (when (and (line-buffered-p tty) (position #\newline string))
      (terminal-finish-output tty))))

(defmethod terminal-alternate-characters ((tty terminal-x11) state)
  (declare (ignore tty state))
  #|
  (setf (translate-alternate-characters tty) state)
  (when (and state (not *acs-table*))
    (make-acs-table))
  (if state
      (terminal-escape-sequence tty "(0")
      (terminal-escape-sequence tty "(B")
      )
  |#
  )

(defmethod terminal-write-string ((tty terminal-x11) str
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (%write-string tty str start end)
  (when (and (line-buffered-p tty)
	     (apply #'position `(#\newline ,str
					   ,@(and start `(:start ,start))
					   ,@(and end `(:end ,end)))))
    (terminal-finish-output tty)))

(defmethod terminal-write-line ((tty terminal-x11) str
				&key start end)
  "Output a string to the terminal, followed by a newline."
  (%write-string tty str start end)
  (%write-text tty #\newline)
  (when (line-buffered-p tty)
    (terminal-finish-output tty)))

(defmethod terminal-write-char ((tty terminal-x11) (char fatchar))
  (%write-text tty char)
  (when (and (line-buffered-p tty) (eql (fatchar-c char) #\newline))
    (terminal-finish-output tty)))

(defmethod terminal-write-char ((tty terminal-x11) (char character))
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  #|
  (let ((stream (terminal-output-stream tty)))
    (when (and (translate-alternate-characters tty)
	       (characterp char))
      (let ((replacement (gethash char *acs-table*)))
	(when replacement
	  (setf char replacement))))
  |#
  ;; (format *trace-output* "terminal-write-char ~s~%" char)
  (%write-text tty char)
  (when (and (line-buffered-p tty) (eql char #\newline))
    (terminal-finish-output tty)))

(defmethod terminal-write-string ((tty terminal-x11) (str fat-string)
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (when (and (%write-string tty str start end) (line-buffered-p tty))
    (terminal-finish-output tty)))

(defmethod terminal-write-line ((tty terminal-x11) (str fat-string)
				&key start end)
  "Output a string to the terminal, followed by a newline."
  (%write-string tty str start end)
  (%write-text tty #\newline)
  (when (line-buffered-p tty)
    (terminal-finish-output tty)))

(defmethod terminal-newline ((tty terminal-x11))
  (terminal-write-char tty #\newline))

(defmethod terminal-fresh-line ((tty terminal-x11))
  (when (not (zerop (cursor-column tty)))
    (terminal-write-char tty #\newline)
    t))

(defmethod terminal-move-to ((tty terminal-x11) row col)
  (with-slots ((window-rows terminal::window-rows)
	       (window-columns terminal::window-columns)) tty
    ;; (assert (<= 0 row (1- window-rows)))
    ;; (assert (<= 0 col (1- window-columns)))
    (with-cursor-movement (tty)
      (setf (cursor-row tty) (clamp row 0 (1- window-rows))
	    (cursor-column tty) (clamp col 0 (1- window-columns))))))

(defmethod terminal-move-to-col ((tty terminal-x11) col)
  (with-cursor-movement (tty)
    (setf (cursor-column tty) col)))

(defmethod terminal-beginning-of-line ((tty terminal-x11))
  (with-cursor-movement (tty)
    (setf (cursor-column tty) 0)))

(defmethod terminal-delete-char ((tty terminal-x11) n)
  (with-slots (lines cursor-row cursor-column) tty
    (flush-buffer tty)
    (let* ((line (aref lines cursor-row))
	   (line-len (length line)))
      (setf (osubseq line cursor-column (- line-len (+ n 2)))
	    (osubseq line (+ cursor-column 1 n) (1- line-len)))
      (%draw-fat-string
       tty (grid-to-fat-string
	    (osubseq line cursor-column (- line-len (+ n 2)))
	    :null-as #\space :keep-nulls t)
       :x cursor-column :y cursor-row)
      (clear-text tty cursor-column cursor-row
		      (- line-len n 1) cursor-row))))

(defmethod terminal-insert-char ((tty terminal-x11) n)
  (with-slots (lines cursor-row cursor-column) tty
    (flush-buffer tty)
    (let* ((line (aref lines cursor-row))
	   (line-len (length line)))
      (setf (osubseq line (+ cursor-column n) (- line-len 1))
	    (osubseq line cursor-column (- line-len (+ n 2))))
      (%draw-fat-string
       tty (grid-to-fat-string
	    (osubseq line (+ cursor-column n) (- line-len 1))
	    :null-as #\space :keep-nulls t)
       :x cursor-column :y cursor-row)
      (clear-text tty cursor-column cursor-row
		      (+ cursor-column n) cursor-row))))

(defmethod terminal-backward ((tty terminal-x11) &optional (n 1))
  (with-slots (cursor-column (window-columns terminal::window-columns)) tty
    (with-cursor-movement (tty)
      (setf cursor-column
	    (clamp (- cursor-column n) 0 (1- window-columns))))))

(defmethod terminal-forward ((tty terminal-x11) &optional (n 1))
  (with-slots (cursor-column (window-columns terminal::window-columns)) tty
    (with-cursor-movement (tty)
      (setf cursor-column
	    (clamp (+ cursor-column n) 0 (1- window-columns))))))

(defmethod terminal-up ((tty terminal-x11) &optional (n 1))
  (with-slots (cursor-row (window-rows terminal::window-rows)) tty
    (with-cursor-movement (tty)
      (setf cursor-row (clamp (- cursor-row n) 0 (1- window-rows))))))

(defmethod terminal-down ((tty terminal-x11) &optional (n 1))
  (with-slots (cursor-row (window-rows terminal::window-rows)) tty
    (with-cursor-movement (tty)
      (setf cursor-row (clamp (+ cursor-row n) 0 (1- window-rows))))))

(defmethod terminal-scroll-down ((tty terminal-x11) n)
  (when (> n 0)
    (scroll tty n)))

(defmethod terminal-scroll-up ((tty terminal-x11) n)
  (when (> n 0)
    (scroll tty (- n))))

(defmethod terminal-erase-to-eol ((tty terminal-x11))
  (with-slots (cursor-column cursor-row
               (window-columns terminal::window-columns)) tty
    (flush-buffer tty)
    (clear-text tty cursor-column cursor-row
		(1- window-columns) cursor-row :erase t)
    (draw-cursor tty)))

(defmethod terminal-erase-line ((tty terminal-x11))
  (with-slots (cursor-column cursor-row
               (window-columns terminal::window-columns)) tty
    (flush-buffer tty)
    (clear-text tty 0 cursor-row (1- window-columns) cursor-row :erase t)
    (draw-cursor tty)))

(defmethod terminal-erase-above ((tty terminal-x11))
  (with-slots (cursor-column cursor-row
               (window-columns terminal::window-columns)) tty
    (flush-buffer tty)
    (clear-text tty 0 cursor-row (1- cursor-column) cursor-row :erase t)
    (clear-text tty 0 0 (1- window-columns) (1- cursor-row) :erase t)
    (draw-cursor tty)))

(defmethod terminal-erase-below ((tty terminal-x11))
  (with-slots (cursor-column cursor-row 
               (window-columns terminal::window-columns)
               (window-rows terminal::window-rows)) tty
    (flush-buffer tty)
    (terminal-erase-to-eol tty)
    (clear-text tty 0 (1+ cursor-row)
		(1- window-columns) (1- window-rows) :erase t)
    (draw-cursor tty)))

(defmethod terminal-clear ((tty terminal-x11))
  (with-slots (cursor-column cursor-row
	       (window-columns terminal::window-columns)
	       (window-rows terminal::window-rows)) tty
    (flush-buffer tty)
    (clear-text tty 0 0 (1- window-columns) (1- window-rows) :erase t)
    (draw-cursor tty)))

(defmethod terminal-home ((tty terminal-x11))
  (with-cursor-movement (tty)
    (setf (cursor-column tty) 0
	  (cursor-row tty) 0)))

(defmethod terminal-cursor-off ((tty terminal-x11))
  (setf (cursor-state tty) :invisible)
  (draw-cursor tty))

(defmethod terminal-cursor-on ((tty terminal-x11))
  (setf (cursor-state tty) :visible)
  (draw-cursor tty))

(defun set-attr (tty attr state)
  "Set the attribute in the rendition on or off according to STATE."
  (flush-buffer tty)
  (if state
      (pushnew attr (fatchar-attrs (rendition tty)))
      (setf (fatchar-attrs (rendition tty))
	    (delete attr (fatchar-attrs (rendition tty))))))

(defmethod terminal-standout ((tty terminal-x11) state)
  (set-attr tty :standout state))

(defmethod terminal-normal ((tty terminal-x11))
  (with-slots (rendition foreground background) tty
    ;; (setf (fatchar-attrs rendition) '()
    ;; 	  (fatchar-fg rendition) foreground
    ;; 	  (fatchar-bg rendition) background)
    (setf rendition (make-fatchar :fg foreground :bg background))
    (%terminal-color tty (fatchar-fg rendition) (fatchar-bg rendition))
    ))

(defmethod terminal-underline ((tty terminal-x11) state)
  (set-attr tty :underline state))

(defmethod terminal-bold ((tty terminal-x11) state)
  (set-attr tty :bold state))

(defmethod terminal-inverse ((tty terminal-x11) state)
  (set-attr tty :inverse state))

(defun foreground-color (tty)
  "Get the default foreground color for text."
  (foreground tty))

(defun background-color (tty)
  "Get the default background color for text."
  (background tty))

(defun set-foreground-color (tty color)
  "Set the default forground color for text."
  (when (not (color:known-color-p color))
    (error "Unknown color ~s." color))
  (setf (foreground tty) (color:lookup-color color)))

(defun set-background-color (tty color)
  "Set the default background color for the terminal."
  (when (not (color:known-color-p color))
    (error "Unknown color ~s." color))
  (setf (background tty) (color:lookup-color color)))

(defmethod terminal-color ((tty terminal-x11) fg bg)
  (%terminal-color tty fg bg))

(defmethod terminal-colors ((tty terminal-x11))
  (cond
    ((let ((fc (nos:env "FORCE_COLOR")))
       (or (equal fc "0")
	   (string-equal fc "false")))
     0)
    (t ;; Assume the most, because so what.
     ;; (* 256 256 256)
     (with-slots (window) tty
       (drawable-depth window)))))

(defmethod terminal-beep ((tty terminal-x11))
  (with-slots (display) tty
    (bell display)
    (display-finish-output display)))

(defmethod terminal-set-scrolling-region ((tty terminal-x11) start end)
  (with-slots (scrolling-region) tty
    (if (and (not start) (not end))
	(setf scrolling-region nil)
	(if (or (< start 0) (> end (terminal-window-rows tty)))
	    (cerror "Just try it anyway."
		    "The scrolling region doesn't fit in the screen.")
	    (setf scrolling-region (cons start end))))))

(defmethod terminal-set-attributes ((tty terminal-x11) attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords."
  (with-slots (rendition) tty
    (flush-buffer tty)
    (etypecase attributes
      (list
       (let ((ng (find-if (_ (not (member _  *attributes*))) attributes)))
	 (when ng
	   (warn "Unsupported attribute ~a" ng))
	 (setf (fatchar-attrs rendition) (remove-duplicates attributes))))
       (keyword
	(when (not (member attributes *attributes*))
	  (warn "Unsupported attribute ~a" attributes))
	(setf (fatchar-attrs rendition) (list attributes))))))

(defmethod terminal-finish-output ((tty terminal-x11))
  (flush-buffer tty)
  (display-finish-output (display tty)))

(defconstant +shift-mask+   (make-state-mask :shift))
(defconstant +lock-mask+    (make-state-mask :lock))
(defconstant +control-mask+ (make-state-mask :control))
(defconstant +mod-1-mask+   (make-state-mask :mod-1))
(defconstant +mod-2-mask+   (make-state-mask :mod-2))

(defparameter *event-tty* nil
  "For passing the TTY to event handlers.")

(defvar *input-available* nil
  "Indicator that there's mouse or keyboard input.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro event-slots ((slots &rest vars) &body body)
    "Bind VARS from the SLOTS property list, and evaluate BODY. For use in event
handler cases."
    (with-unique-names (slot-var)
      `(let* ((,slot-var ,slots)
	      ,@(loop
		   :for v :in vars
		   :collect `(,v (getf ,slot-var ,(keywordify v)))))
	 ,@body))))

(defun window-handler (&rest slots
		       &key display event-key send-event-p &allow-other-keys)
  "Handle normal window maintenance events that aren't input."
  (declare (ignore send-event-p))
  (let ((tty *event-tty*) result)
    (with-slots ((our-window window) window-width window-height threads) tty
      (case event-key
	(:exposure
	 (event-slots (slots x y width height xlib:window #|count|#)
	   (when (eq xlib:window our-window)
	     ;; (format t "exposure ~s ~s ~s ~s~%" x y width height)
	     (redraw-area tty x y width height)
	     (display-finish-output display)
	     ;; (dump-grid tty)
	     )
	   t))
	(:configure-notify
	 (event-slots (slots #| x y |# xlib:window xlib::width xlib::height)
	   (when (eq xlib:window our-window)
	     (when (or (/= xlib::width window-width)
		       (/= xlib::height window-height))
	       (setf window-width xlib::width window-height xlib::height)
	       (set-grid-size-from-pixel-size tty window-width window-height)
	       (format *trace-output*
		       "resize to ~s ~s~%" window-width window-height)
	       (finish-output *trace-output*)
	       (resize-grid tty)
	       ;;(clear-area win :x x :y y :width w :height h)
	       ;;(display-finish-output display)
	       (when (find :resize (terminal-events-enabled tty))
		 (setf result :resize))))
	   (or result (and (discard-current-event display) nil))))
	(:visibility-notify
	 t)
	(:client-message ()
	  (event-slots (slots type format data)
	    (when (and (eq type :WM_PROTOCOLS)
		       (= format 32)
		       (eq (atom-name display (aref data 0)) :WM_DELETE_WINDOW))
	      ;; (format *trace-output*
	      ;; 	      "client message type ~s format ~s data ~s~%"
	      ;; 	      type format data)
	      ;; (finish-output *trace-output*)
	      ;; Suppossedly:
	      ;; • The atom that names their protocol in the data[0] field
	      ;; • A timestamp in their data[1] field
	      (throw :lish-quick-exit :lish-quick-exit)))
	  t)
	;; :enter-notify fill the cursor
	;; :leave-notify hollow the cursor
	;; probably :keymap-notify
	;; so we know what keys are down
	;; :mapping-notify
	;;   update the map, calling xlib:mapping-notify
	;; :map-notify
	;; :unmap-notify
	;;   maybe turn off outputting? icon window or something?
	;; :property-notify :selection-clear :selection-notify :selection-request
	;;   complicated selection crap
	;; what else? extensions?
	(otherwise t)))))

(defun modifier-mask-to-list (mask)
  "Return a list of modifier keywords from an event state."
  (remove-if (_ (begins-with "BUTTON" (symbol-name _)))
	     (substitute :meta :mod-1 (make-state-keys mask))))

(defun modifier-mask-buttons (mask)
  (remove-if-not (_ (begins-with "BUTTON" (symbol-name _)))
		 (make-state-keys mask)))

(defun modifier-prefixed (symbol mask)
  "Return a keyword of SYMBOL prefixed by modifiers mask MASK."
  (intern (format nil "~:[~;S-~]~:[~;A-~]~:[~;C-~]~:[~;M-~]~@:(~a~)"
		  (logtest +shift-mask+   mask)
		  (logtest +mod-2-mask+   mask)
		  (logtest +control-mask+ mask)
		  (logtest +mod-1-mask+   mask)
		  (symbol-name symbol)) :keyword))

(defun key-handler (&rest slots
		    &key display event-key send-event-p &allow-other-keys)
  "Handle key or mouse input."
  (with-slots (cell-width cell-height modifiers interrupt-key) *event-tty*
    (let ((tty *event-tty*) result)
      (when (and send-event-p (member event-key '(:button-press :button-release
						  :key-press :key-release))
		 (not (allow-send-events tty)))
	(warn "Synthetic button press!")
	(return-from key-handler t))
      (case event-key
	((:exposure :configure-notify :visibility-notify :client-message)
	 (apply #'window-handler slots))
	(:button-press
	 (event-slots (slots code x y state)
           (let ((cx (round x cell-width))
		 (cy (round y cell-height)))
	     (when (find :mouse-buttons (terminal-events-enabled tty))
	       (setf result
		     (make-instance 'tt-mouse-button-event
				    :terminal tty
				    :x cx :y cy
				    :button (keywordify (s+ "BUTTON-" code))
				    :modifiers (modifier-mask-to-list state))
		     *input-available* t))
	     (or result t))))
	(:button-release
	 (event-slots (slots code x y state)
	   (let ((cx (round x cell-width))
		 (cy (round y cell-height)))
	     (when (find :mouse-buttons (terminal-events-enabled tty))
	       (setf result
		     (make-instance 'tt-mouse-button-release
				    :terminal tty
				    :x cx :y cy
				    :button (keywordify (s+ "BUTTON-" code))
				    ;;:button (list code :release)
				    ;;:button :release
				    :modifiers (modifier-mask-to-list state))
		     *input-available* t))
	     (or result t))))
	(:motion-notify
	 (event-slots (slots #|code|# x y state)
	   (let ((cx (round x cell-width))
		 (cy (round y cell-height)))
	     (when (find :mouse-buttons (terminal-events-enabled tty))
	       (setf result
		     (make-instance 'tt-mouse-button-motion
				    :terminal tty
				    :x cx :y cy
				    ;;:button (keywordify (s+ "BUTTON-" code))
				    :button (first ;; @@@ what about others?
					     (modifier-mask-buttons state))
				    ;;:button (list code :release)
				    ;;:button :release
				    :modifiers (modifier-mask-to-list state))
		     *input-available* t))
	     (or result t))))
	(:key-press
	 (event-slots (slots code state)
	   (let* ((sym (keycode->keysym display code 0))
		  (sym-name (gethash sym *keysym-names*))
		  (chr (keycode->character display code state)))
	     ;; @@@ the keyword is pretty bogus, maybe we should drop it?
	     ;; (format t "code ~s state ~x sym ~s ~s chr ~s~%" code state
	     ;; 	       sym sym-name chr)
	     (cond
	       ;; @@@ Ignore modifier presses by themselves for now
	       ((find code modifiers)
		(setf result nil))
	       ;; A character was found
	       (chr
		(setf result chr)
		(when (or (logtest state +shift-mask+)
			  (logtest state +lock-mask+))
		  (setf result (char-upcase chr)))
		(when (logtest state +control-mask+)
		  (setf result
			(if (char-util:control-char-p (char-util:ctrl chr))
			    (char-util:ctrl chr)
			    ;; For control combos which aren't traditional
			    ;; control character, do a prefixed symbol.
			    ;; But with more special cases!
			    (case sym-name
			      (:space #\nul)
			      (t
			       (modifier-prefixed sym-name state))))))
		(when (logtest state +mod-1-mask+)
		  ;; @@@ this isn't really good
		  ;; perhaps we should stuff Escape?
		  (setf result (char-util:meta-char chr))))
	       ;; A key symbol was found
	       (sym-name
		(setf result
		      (if (not (zerop state))
			  (modifier-prefixed sym-name state)
			  ;; special translations
			  (case sym-name
			    (:escape #\escape)
			    (t sym-name)))))
	       (t
		;; Try to return something at least
		(setf result (or chr sym-name sym code))))
	     (when result (setf *input-available* t))
	     (when (eq result :f11) ;; @@@ debugging
	       (dump-grid tty)
	       (discard-current-event display)
	       (return-from key-handler nil))
	     (when (and interrupt-key (eq result interrupt-key))
	       (discard-current-event display)
	       (throw 'interactive-interrupt nil))
	     (or result t))))
	(otherwise t)))))

(defun get-key (tty &key timeout)
  (with-slots (display pushback modifiers) tty
    (when pushback
      (return-from get-key
	(pop pushback)))

    (when (not modifiers)
      (setf modifiers
	    (flatten (multiple-value-list (modifier-mapping display)))))

    ;; If the timeout has already elapsed, don't use
    (let* ((start-time (get-dtime))
	   (real-timeout (and timeout (make-dtime-as timeout :seconds)))
	   (*event-tty* tty)
	   (*input-available* nil)
	   (time-left real-timeout)
	   result)
      (loop :do
	 (dbugf :tx11 "get-key BEFORE ~s ~s ~s ~s ~s~%"
		timeout time-left
		(and time-left (dtime-plusp time-left))
		(and time-left (dtime-to time-left :seconds))
		(and timeout
		     time-left
		     (dtime-plusp time-left)
		     (dtime-to time-left :seconds)))
	 (setf result
	       (process-event display
			      :handler #'key-handler
			      :force-output-p t
			      :timeout (and timeout
					    time-left
					    (dtime-plusp time-left)
					    (dtime-to time-left :seconds))))
	 (dbugf :tx11 "get-key ~s~%" result)
	 (when timeout
	   (setf time-left (dtime- (dtime+ start-time real-timeout)
				   (get-dtime))))
	 :while (and (not *input-available*)
		     (or (not timeout) (and time-left (dtime-plusp time-left)))))
      result)))

(defmethod terminal-get-char ((tty terminal-x11))
  "Read a character from the terminal."
  (terminal-finish-output tty)
  (let (result)
    (loop
       :do (setf result (get-key tty))
       :while (not (characterp result)))
    result))

(defmethod terminal-get-key ((tty terminal-x11))
  (terminal-finish-output tty)
  (get-key tty))

(defun listen-handler (&rest slots
		       &key display event-key send-event-p &allow-other-keys)
  (declare (ignore send-event-p))
  (case event-key
    ((:exposure :configure-notify :visibility-notify :client-message)
     (apply #'window-handler slots)
     (discard-current-event display)
     t)
    ((:button-press :button-release :key-press)
     (dbugf :tx11 "something pressed~%")
     (dbugf :tx11 "WTF input? ~s ~s~%" event-key slots) (finish-output)
     (apply #'queue-event display event-key slots)
     (setf *input-available* t)
     t)
    (otherwise
     ;; (discard-current-event display)
     t)))

(defun useless-handler (&rest slots
			&key display event-key send-event-p &allow-other-keys)
  (declare (ignore display send-event-p))
  (format t "~s slots ~s~%" event-key slots)
  t)

(defun fuxor (tty)
  "Clear out the event queue."
  (with-slots (display) tty
    (let ((result))
      (loop :while (and (setf result (event-listen display))
			(not (zerop result)))
	 :do
	 (format t "eating event ~s~%" result)
	 (process-event display :handler #'useless-handler)
	 (discard-current-event display)))))

(defmethod terminal-listen-for ((tty terminal-x11) timeout)
  (with-slots (display) tty
    (terminal-finish-output tty)
    (let* ((start-time (get-dtime))
	   (real-timeout (and timeout (make-dtime-as timeout :seconds)))
	   (time-left real-timeout)
	   (*event-tty* tty)
	   (*input-available* nil)
	   result)
      (flet ((timeout ()
	       (and timeout (dtime-to time-left :seconds))))
	(loop
	   :do
	   (dbugf :tx11 "time-left ~s ~s~%" time-left (timeout))
	   (setf result
		 (process-event display
				;;:peek-p t ;; <<--
				;; :handler #'window-handler
				:handler #'listen-handler
				:force-output-p t
				:timeout (timeout)))
	   (dbugf :tx11 "result ~s *input-available* ~s~%" result
		  *input-available*)
	   (when timeout
	     (setf time-left (dtime- (dtime+ start-time real-timeout)
				     (get-dtime))))
	   :while (and (not *input-available*)
		       (or (not timeout) (dtime-plusp time-left))))
	(dbugf :tx11 "CHANG ~s~%" *input-available*)
	*input-available*))))

(defmethod terminal-input-mode ((tty terminal-x11))
  (input-mode tty))

(defmethod (setf terminal-input-mode) (mode (tty terminal-x11))
  (when (not (member mode '(:line :char)))
    (error "Unknown terminal input mode ~s" mode))
  (setf (input-mode tty) mode))

(defmethod terminal-reset ((tty terminal-x11))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (flush-buffer tty)
  (setf (rendition tty) (make-fatchar :fg (foreground tty) :bg (background tty))
	(cursor-rendition tty) (make-fatchar
				;; Maybe faster then :inverse ?
				:fg (background tty) :bg (foreground tty))
	;; (cursor-row tty) 0
	;; (cursor-column tty) 0
	(saved-cursor-position tty) nil
	(cursor-state tty) :visible
	(scrolling-region tty) nil
	(input-mode tty) :char
	(delay-scroll tty) nil
	(output-buffer tty) nil)
  (with-cursor-movement (tty)
    (setf (cursor-row tty) 0
	  (cursor-column tty) 0))
  (terminal-finish-output tty))

(defmethod terminal-save-cursor ((tty terminal-x11))
  "Save the cursor position."
  (setf (saved-cursor-position tty) (cons (cursor-column tty)
					  (cursor-row tty))))

(defmethod terminal-restore-cursor ((tty terminal-x11))
  "Restore the cursor position, from the last saved postion."
  (with-slots (saved-cursor-position cursor-column cursor-row) tty
    (when saved-cursor-position
      (with-cursor-movement (tty)
	(setf cursor-column (car saved-cursor-position)
	      cursor-row (cdr saved-cursor-position))))))

#|
(defun describe-terminal ()
  "Interrogate the terminal properties and report the results."
  (let (a props)
    (push `("Cursor position" ,(format nil "~a ~a" cursor-column cursor-row))
	  props)
    ;; Window state
    (setf a (query-parameters "11t" :offset 2))
    (push `("Window state"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (case (first a)
		   (1 "Open")
		   (2 "Iconified")
		   (t "Unknown"))))
	  props)
    ;; Window position
    (setf a (query-parameters "13t" :offset 2))
    (push `("Window position"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Window size
    (setf a (query-parameters "14t" :offset 2))
    (push `("Window size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text size
    (setf a (query-parameters "18t" :offset 2))
    (push `("Text size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text screen size
    (push `("Text screen size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" window-columns window-rows)))
	  props)
    ;; Icon label
    (setf a (wm-icon-name window))
    (push `("Icon label"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;; Title
    (setf a (wm-name window))
    (push `("Title"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;;
    (setf props (nreverse props))
    (print-properties props)))
|#

#| I think there no reason to do this

(defgeneric set-bracketed-paste-mode (tty &optional state)
  (:documentation "Set bracketed paste mode to STATE.")
  (:method ((tty terminal-x11) &optional (state t))
    (terminal-raw-format tty "~a?2004~c" +csi+ (if state #\h #\l))))

(defvar *bracketed-read-timeout* 4
  "Maximum time in seconds before bailing out of reading one buffer full of a
bracketed read.")

(defgeneric read-bracketed-paste (tty)
  (:documentation "Read a bracketed paste and return it as a string.")
  (:method ((tty terminal-x11))
    (let ((end-string (s+ +csi+ "201~"))
	  ;; (buf (make-string *buffer-size*))
	  (fd (terminal-file-descriptor tty)))
      (with-output-to-string (str)
	(with-raw (fd)
	  (loop :with done :and i = 0 :and len = (length end-string) :and s
	     :while (not done)
	     :if (listen-for *bracketed-read-timeout* fd) :do
	     (with-interrupts-handled (tty)
	       (setf s (read-until fd (char end-string i)
				   ;; :timeout (* *bracketed-read-timeout* 10)
				   :timeout *bracketed-read-timeout*
				   :octets-p t)))
	     ;; (dbugf :bp "got dingus ~s ~s~%length ~s~%fill-pointer ~s"
	     ;; 	    s (type-of s) (length s)
	     ;; 	    (when s (fill-pointer s))
	     ;; 	    )
	     (if s
		 (progn
		   ;; (princ s str)
		   (let ((uu (char-util:utf8-bytes-to-string s)))
		     ;; (dbugf :bp "why? ~s ~s~%" uu (type-of uu))
		     (princ uu str))
		   (setf i 1))
		 (progn
		   (incf i)))
	     (when (= i len)
	       (setf done t))
	     :else :do
	     (cerror "Return what we got so far."
		     "Bracketed paste timed out.")
	     (setf done t)))))))

|#

(defparameter *selection-codes*
  #((:clipboard    . #\c)
    (:primary      . #\p)
    (:select       . #\s)
    (:cut-buffer-0 . #\0)
    (:cut-buffer-1 . #\1)
    (:cut-buffer-2 . #\2)
    (:cut-buffer-3 . #\3)
    (:cut-buffer-4 . #\4)
    (:cut-buffer-5 . #\5)
    (:cut-buffer-6 . #\6)
    (:cut-buffer-7 . #\7))
  "Selection type codes.")

(defun selection-type-code (type)
  "Return the terminal selection type code given a the keyword TYPE. If TYPE is
already a character or string just return it."
  (etypecase type
    (string type)
    (character (cdr (find type *selection-codes* :key #'cdr)))
    (symbol (cdr (find type *selection-codes* :key #'car)))))

#| This is way too complicated for me to do now.
   Have to implement the whole complicated selection event processing in
   ~/doc/software/icccm.pdf

;; the configurable primary/clipboard selection and cut buffer 0
(defun selection (&key (type "s0") (tty *terminal*))
  "Return the selection of TYPE, which defaults to the :select or :cut-buffer-0.
Otherwise TYPE should be one of :clipboard :primary :select or :cut-buffer-<N>, 
wheren <N> is a number 1-7."
  (let* ((type-code (selection-type-code type))
	 result)
    (when (not type-code)
      (error "Unknown selection type ~s" type))
    (setf result (query-string (s+ "52;" type-code ";?" #\bel)
			       :lead-in +osc+ :ending 1
			       :tty (terminal-file-descriptor tty)))
    (when result
      (cl-base64:base64-string-to-string
       (remove-prefix result (s+ "2;" type-code ";"))))))

(defun set-selection (selection &key (type "s0"))
  "Set the selection to the string SELECTION. TYPE should be one of :clipboard,
:primary, :select, or :cut-buffer-<N>, wheren <N> is a number 1-7.
TYPE  defaults to :select or :cut-buffer-0."
  (tt-format "~a52;~a;~a~a" +osc+
	     (selection-type-code type)
	     (cl-base64:string-to-base64-string selection)
	     +st+)
  selection)

(defsetf selection (&rest keys &key type) (val)
  "Set the selection of TYPE, which defaults to the :select or :cut-buffer-0.
Otherwise TYPE should be one of :clipboard :primary :select or :cut-buffer-<N>, 
wheren <N> is a number 1-7."
  (declare (ignorable type))
  `(apply #'set-selection (list ,val ,@keys)))

(defun set-utf8-title-mode (tty state)
  (terminal-raw-format tty "~c[>2;3~c" #\escape (if state #\t #\T))
  (terminal-finish-output tty))

|#

(defun set-title (tty title &optional (which :window))
  "Set the window title of TTY to TITLE. WHICH should be one of :window, :icon,
or :both. WHICH defaults to :window."
  (with-slots (window) tty
    (case which
      (:window
       (xlib:set-wm-properties window :name title))
      (:icon
       (xlib:set-wm-properties window :icon-name title))
      (:both
       (xlib:set-wm-properties window :name title :icon-name title)))
    (terminal-finish-output tty)))

(defun get-title (tty &optional (which :window))
  "Return the window title of TTY. WHICH should be one of :window, :icon,
or :both. WHICH defaults to :window"
  (with-slots (window) tty
    (ecase which
      (:icon
       (map 'string #'code-char (xlib:get-property window :wm_icon_name)))
      (otherwise #| :window |#
       (map 'string #'code-char (or
				 (xlib:get-property window :_net_wm_name)
				 (xlib:get-property window :wm_name)))))))

(defmethod terminal-title ((tty terminal-x11))
  "Return the window title."
  (get-title tty))

(defmethod (setf terminal-title) (title (tty terminal-x11))
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (set-title tty title))

(defmethod terminal-has-attribute ((tty terminal-x11) attribute)
  "Return true if the terminal can display the character attribute."
  ;; (case attribute
  ;;   ;; @@@ Consider what else we might want to support.
  ;;   ((:standout :underline :bold :inverse :color) t)))
  (and (find attribute *attributes*) t))

(defmethod terminal-has-autowrap-delay ((tty terminal-x11))
  "Return true if the terminal delays automatic wrapping at the end of a line."
  nil ;; @@@ what do I want?
  )

;; @@@ I think this is a misfeature. Should I even include it?
#|
(defun text-link (text to &optional (params ""))
  "Make a “hyperlink” linking TEXT to TO. TO should probably be a URI.
According to iTerm2, params can be \"id=something\", to make adjacent
links highlight differently?"
  (tt-format "~a8;~a;~a~c~a~c]8;;~c"
	     +osc+ params to #\bel text #\esc #\bel))
|#

;; Konsole and maybe VTE cursor shape ? : @@@ need to verify
;;    #\esc #\[ p #\q
;;              0  reset to default
;;              1  block blink
;;              2  block no-blink
;;              3  underline blink
;;              4  underline no-blink
;;              5  ibeam blink
;;              6  ibeam no-blink

;; @@@ These probably shouldn't be in here as they're specific to iTerm2. But I
;; don't care enough to make a terminal-iterm2 currently.

#|
(defun set-cursor-shape (shape)
  (setf shape (etypecase shape
		(integer
		 (when (not (<= 0 shape 2))
		   (error "Unknown cursor shape number ~d." shape)))
		(symbol
		 (case shape
		   (:block 0)
		   (:vertical-bar 1)
		   (:underline 2)))))
  (tt-format "~a1337;CursorShape=~d~c" +osc+ shape #\bel))

(defun notification-message (message)
  "Display some kind of out-of-band notification."
  (tt-format "~a9;~a~c" +osc+ message #\bel))

(defun change-profile (profile-name)
  "Change the terminal's profile."
  (tt-format "~a1337;SetProfile=~s~c" +osc+ profile-name #\bel))

(defun copy-to-clipboard (text &key clipboard-name)
  "Copy to TEXT to the clipboard"
  (let ((name
	 (etypecase clipboard-name
	   (null "") ;; general pasteboard
	   ((or string symbol)
	    (or (and (stringp clipboard-name)
		     (zerop (length clipboard-name)))
		(case (keywordify clipboard-name)
		  (:rule "rule")
		  (:find "find")
		  (:font "font")
		  (otherwise
		   (error "Unknown clipboard name ~s." clipboard-name))))))))
    (tt-format "~a1337;CopyToClipboard=~a~c~a~a1377;EndCopy~c"
	       +osc+ name #\bel text +osc+ #\bel)))
|#

(defun set-mouse-event (tty event state)
  (with-slots (window) tty
    ;; @@@ But actually we probably don't want to really change the
    ;; window event masks, only change what we report from tt-get-key
    (case event
      (:mouse-buttons
       (setf (window-event-mask window)
	     (if state
		 (logior (window-event-mask window)
			 (make-event-mask :button-press :button-release
					  :button-motion))
		 (logand (window-event-mask window)
			 (lognot
			  (make-event-mask :button-press :button-release
					   :button-motion))))))
      (:mouse-motion
       (setf (window-event-mask window)
	     (if state
		 (logior (window-event-mask window)
			 (make-event-mask :pointer-motion))
		 (logand (window-event-mask window)
			 (lognot (make-event-mask :pointer-motion)))))))))

(defmethod terminal-enable-event ((tty terminal-x11) event)
  "Enable event and return true if the terminal can enable event."
  (let (result)
    (when (member event '(:resize :mouse-buttons :mouse-motion))
      (pushnew event (terminal-events-enabled tty))
      (setf result t))
    (case event
      (:mouse-buttons (set-mouse-event tty :mouse-buttons t))
      (:mouse-motion (set-mouse-event tty :mouse-motion t)))
    result))

(defmethod terminal-disable-event ((tty terminal-x11) event)
  "Enable event and return true if the terminal can disable event."
  (let (result)
    (when (member event '(:resize :mouse-buttons :mouse-motion))
       (setf (terminal-events-enabled tty)
	     (remove event (terminal-events-enabled tty)))
      (setf result t))
    (case event
      (:mouse-buttons (set-mouse-event tty :mouse-buttons nil))
      (:mouse-motion (set-mouse-event tty :mouse-motion nil)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

;; common methods

(defmethod-quiet close ((stream terminal-x11) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-x11))
  ;; (clear-output (terminal-output-stream stream))
  ;; @@@ I don't know if we can really do this without creating our
  ;; own complex buffer structure which encodes every tt-* output change.
  ;; But also, I don't know if it matters.
  (setf (pushback stream) nil)) ;; At least we can do that.

(defmethod stream-finish-output ((stream terminal-x11))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-x11))
  (terminal-finish-output stream)
  (display-force-output (display stream)))

(defmethod stream-write-sequence ((stream terminal-x11) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string stream seq :start start :end end))
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	    (when (>= i start)
	      (terminal-write-char output-stream (car l)))
	    (setf l (cdr l))
	    (incf i))))))

;; character output stream methods

(defmethod stream-line-column ((stream terminal-x11))
  (cursor-column stream))

(defmethod stream-start-line-p ((stream terminal-x11))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-x11) column)
  (with-slots (cursor-column cursor-row) stream
    ;; :-D This is so magically easy, it must be right.
    (when (> column cursor-column)
      (clear-text stream cursor-column cursor-row column cursor-row :erase t)
      (with-cursor-movement (stream)
	(setf cursor-column column))))
  t)

;;(defmethod stream-fresh-line ((stream terminal-x11-stream))

;; #+sbcl (defmethod sb-gray:stream-line-length ((stream terminal-x11-stream))
;;   )

(defmethod stream-write-char ((stream terminal-x11) char
			     #| &optional start end |#)
  (terminal-write-char stream char))

(defmethod stream-write-string ((stream terminal-x11) string
			       &optional start end)
  (terminal-write-string stream string :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-x11, which is also an input stream.

(defmethod stream-clear-input ((stream terminal-x11))
  (declare (ignore stream))
  #|(with-slots (display) stream
    ;; we want to
    ;; (discard-current-event display)
    ;; if it's an input event
    ;; otherwise process events normally
    ;; until the input queue is empty
    ;;
    ;; (loop :while (not (zerop (event-listen display)))
    ;;    :do
    ;;    (event-case (display :discard-p t :force-output-p t)
    ;;      (:button-
  ) |#
  )

#|
(defmethod stream-read-sequence ((stream terminal-x11) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore stream seq start end))
  ;; @@@ we could actually do this?
  nil)
|#

;;(defgeneric stream-peek-char ((stream terminal-x11))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-x11))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (terminal-finish-output stream)
  (get-key stream :timeout 0))

(defmethod stream-read-char ((stream terminal-x11))
  (terminal-get-char stream))

(defun fake-read-line (tty)
  (with-slots (cursor-column) tty
    (let ((result (make-stretchy-string 33))
	  got-eof)
      (loop :with c :and done
	 :do
	 (setf c (get-key tty))
	 (case c
	   (#\newline (setf done t))
	   (#.(char-util:ctrl #\c) (throw 'interactive-interrupt t))
	   (#.(char-util:ctrl #\d) (setf done t got-eof t))
	   (#.(char-util:ctrl #\u)
	      (with-cursor-movement (tty)
		(setf cursor-column 0))
	      (stretchy-truncate result 0))
	   (#\backspace
	    (when (not (zerop (length result)))
	      (when (not (zerop cursor-column))
		(with-cursor-movement (tty)
		  (decf cursor-column)))
	      (stretchy-truncate result (1- (length result)))))
	   (otherwise
	    (stretchy-append result c)))
	 :while (not done))
      (values (or result "")
	      got-eof))))

(defmethod stream-read-line ((stream terminal-x11))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (fake-read-line stream))

(defmethod stream-listen ((stream terminal-x11))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (or (pushback stream)
      (terminal-listen-for stream 0)))

(defmethod stream-unread-char ((stream terminal-x11) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (push character (pushback stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Costs

;; @@@ Could some of these benefit from memo-ization?

(defmethod output-cost ((tty terminal-x11) (op (eql :move-to)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :move-to-col)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :up)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :down)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :backward)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :forward)) &rest params)
  (declare (ignore params))
  1)

(defmethod output-cost ((tty terminal-x11) (op (eql :color)) &rest params)
  (declare (ignore params))
  5)

(defmethod output-cost ((tty terminal-x11) (op (eql :write-fatchar))
			&rest params)
  (declare (ignore params))
  3)

(defmethod output-cost ((tty terminal-x11) (op (eql :write-fatchar-string))
			&rest params)
  ;; Not very accurate. Costs depend on how many effect switches, etc.
  (* 3 (length (first params))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :x11 'terminal-x11)

;; EOF
