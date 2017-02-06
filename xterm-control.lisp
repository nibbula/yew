;;
;; xterm-control.lisp - Control an XTerm compatible terminal.
;;

(defpackage :xterm-control
  (:documentation "Control an XTerm compatible terminal.")
  (:use :cl :dlib-misc :char-util :terminal :terminal-ansi :terminal-curses
	:keymap :inator :curses :fui :tiny-rl)
  (:export
   #:control-xterm
   ))
(in-package :xterm-control)

(defun iconify (state)
  (tt-format "~c[~at" #\escape (if state 2 1))
  (tt-finish-output))

(defun move-to (x y)
  (tt-format "~c[~a;~a;~at" #\escape 3 x y)
  (tt-finish-output))

(defun resize-pixels (width height)
  (tt-format "~c[~a;~a;~at" #\escape 4 width height)
  (tt-finish-output))

(defun resize-characters (width height)
  (tt-format "~c[~a;~a;~at" #\escape 8 height width)
  (tt-finish-output))

(defun raise ()
  (tt-format "~c[~at" #\escape 5)
  (tt-finish-output))

(defun lower ()
  (tt-format "~c[~at" #\escape 6)
  (tt-finish-output))

(defun refresh-term ()
  (tt-format "~c[~at" #\escape 7)
  (tt-finish-output))

;; I would like to use this to toggle, but it doesn't seem to work quite right.
;; (tt-format "~c[10;2t" #\escape)
(defun set-fullscreen (state)
  (tt-format "~c[10;~dt" #\escape (if state 1 0))
  (tt-finish-output))

(defun set-utf8-title-mode (state)
  (tt-format "~c[>2;3~c" #\escape (if state #\t #\T))
  (tt-finish-output))

(defun set-title (title)
  (tt-format "~c]0;~a~c" #\escape title (char-util:ctrl #\G))
  (tt-finish-output))

(defun get-title ()
  (set-utf8-title-mode t)
  (terminal-ansi::query-string "21t"))

(defun edit-title (i)
  (declare (ignore i))
  (let ((title (get-title)) result)
    (move 10 0)
    (refresh)
    (reset-shell-mode)
    (setf result
	  (tiny-rl:tiny-rl :prompt "Title: " :string (or title "")
			   :terminal-class 'terminal-curses:terminal-curses
			   :accept-does-newline nil))
    (when result
      (set-title result))
    (reset-prog-mode)
    (erase)
    (refresh)))

(defkeymap *xterminator-keymap*
  `((#\escape		  . *xterminator-escape-keymap*)
    (,(ctrl #\G)	  . quit)
    (#\q	  	  . quit)
    (#\return		  . edit-title)
    (#\newline		  . edit-title)
    (#\f		  . toggle-fullscreen)
    (#\F		  . fullscreen-on)
    (,(ctrl #\F)	  . fullscreen-off)

    (:down		  . down-multiple)
    (:up		  . up-multiple)
    (:left		  . backward-multiple)
    (:right		  . forward-multiple)

    (#\j		  . down-multiple)
    (#\k		  . up-multiple)
    (#\h		  . backward-multiple)
    (#\l		  . forward-multiple)

    (#\J		  . next)
    (#\K		  . previous)
    (#\H		  . backward-unit)
    (#\L		  . forward-unit)

    (#\]		  . move-to-bottom)
    (,(meta-char #\>)     . move-to-bottom)
    (:end		  . move-to-bottom)
    (#\[		  . move-to-top)
    (,(meta-char #\<)     . move-to-top)
    (:home		  . move-to-top)

    (,(ctrl #\V)	  . expand-height-chars)
    (:npage		  . expand-height-chars)
    (,(meta-char #\v)	  . shrink-height-chars)
    (:ppage		  . shrink-height-chars)

    (#\+		  . expand-height-chars)
    (#\-	  	  . shrink-height-chars)
    (#\>		  . expand-width-chars)
    (#\<	  	  . shrink-width-chars)
    (#\,		  . shrink-height-chars)
    (#\.	  	  . expand-height-chars)

    ;;(,(meta-char #\=)	  . pick-list-binding-of-key)
    (#\?		  . help)
    ))

(defparameter *xterminator-escape-keymap*
  (build-escape-map *xterminator-keymap*))

(defclass xterminator (fui-inator)
  ((x
    :initarg :x :accessor xterminator-x  :type integer
    :documentation "X coordinate in pixels.")
   (y
    :initarg :y :accessor xterminator-y  :type integer
    :documentation "Y coordinate in pixels.")
   (char-width
    :initarg :width :accessor xterminator-char-width  :type integer
    :documentation "Width in character cells.")
   (char-height
    :initarg :char-height :accessor xterminator-char-height  :type integer
    :documentation "Height in character cells.")
   (pixel-width
    :initarg :pixel-width :accessor xterminator-pixel-width  :type integer
    :documentation "Width in pixels.")
   (pixel-height
    :initarg :pixel-height :accessor xterminator-pixel-height  :type integer
    :documentation "Height in pixels.")
   (screen-char-width
    :initarg :screen-char-width :accessor xterminator-screen-char-width
    :type integer
    :documentation "Width of the screen in characters.")
   (screen-char-height
    :initarg :screen-char-height :accessor xterminator-screen-char-height
    :type integer
    :documentation "Height of the screen in characters.")
   (fullscreen
    :initarg :fullscreen :accessor xterminator-fullscreen :initform nil
    :type boolean
    :documentation "True if the window is in fullscreen mode.")
   (increment
    :initarg :increment :accessor xterminator-increment :initform 20
    :type integer
    :documentation "Size to increment."))
  (:documentation "XTerm compatible terminal manipulator."))

(defun get-xterm-paramaters (o)
  (let (result)
    ;; location
    (if (setf result (terminal-ansi::query-parameters "13t"))
	(setf (slot-value o 'x) (elt result 1)
	      (slot-value o 'y) (elt result 2))
	(setf (slot-value o 'x) 0
	      (slot-value o 'y) 0))
    ;; pixel size
    (if (setf result (terminal-ansi::query-parameters "14t"))
	(setf (slot-value o 'pixel-width) (elt result 2)
	      (slot-value o 'pixel-height) (elt result 1))
	(setf (slot-value o 'pixel-width) 0
	      (slot-value o 'pixel-height) 0))
    ;; character size
    (if (setf result (terminal-ansi::query-parameters "18t"))
	(setf (slot-value o 'char-width) (elt result 2)
	      (slot-value o 'char-height) (elt result 1))
	(setf (slot-value o 'char-width) 0
	      (slot-value o 'char-height) 0))
    ;; screen size
    (if (setf result (terminal-ansi::query-parameters "19t"))
	(setf (slot-value o 'screen-char-width) (elt result 2)
	      (slot-value o 'screen-char-height) (elt result 1))
	(setf (slot-value o 'screen-char-width) 0
	      (slot-value o 'screen-char-height) 0))
    ;; Try to figure out fullscreen
    (setf (slot-value o 'fullscreen)
	  (and (not (zerop (slot-value o 'screen-char-width)))
	       (= (slot-value o 'screen-char-width)
		  (slot-value o 'char-width))
	       (= (slot-value o 'screen-char-height)
		  (slot-value o 'char-height))))))

(defmethod initialize-instance
    :after ((o xterminator) &rest initargs &key &allow-other-keys)
  "Initialize a xterminator."
  (declare (ignore initargs))
  (get-xterm-paramaters o))

(defvar *xterminator* nil
  "Dynamic xterminator instance.")

(defmethod next ((i xterminator))
  "Move the window down."
  (incf (xterminator-y i))
  (move-to (xterminator-x i) (xterminator-y i)))

(defmethod previous ((i xterminator))
  "Move the window up."
  (when (> (xterminator-y i) 0)
    (decf (xterminator-y i))
    (move-to (xterminator-x i) (xterminator-y i))))

(defmethod forward-unit ((i xterminator))
  "Move the window right."
  (incf (xterminator-x i))
  (move-to (xterminator-x i) (xterminator-y i)))

(defmethod backward-unit ((i xterminator))
  "Move the window left."
  (when (> (xterminator-x i) 0)
    (decf (xterminator-x i))
    (move-to (xterminator-x i) (xterminator-y i))))

(defmethod forward-multiple ((i xterminator))
  "Move the window right by some."
  (incf (xterminator-x i) (xterminator-increment i))
  (move-to (xterminator-x i) (xterminator-y i)))

(defmethod backward-multiple ((i xterminator))
  "Move the window left by some."
  (with-slots (x y increment) i
    (if (> (- x increment) 0)
	(decf x increment)
	(setf x 0))
    (move-to x y)))

(defun up-multiple (i)
  "Move the window up by some."
  (with-slots (x y increment) i
    (if (> (- y increment) 0)
	(decf y increment)
	(setf y 0))
    (move-to x y)))

(defun down-multiple (i)
  "Move the window down by some."
  (incf (xterminator-y i) (xterminator-increment i))
  (move-to (xterminator-x i) (xterminator-y i)))

(defun shrink-width-pixels (i)
  "Shrink the window width by pixels."
  (with-slots (pixel-width pixel-height increment) i
    (decf pixel-width increment)
    (resize-pixels pixel-width pixel-height)))

(defun shrink-height-pixels (i)
  "Shrink the window height by pixels."
  (with-slots (pixel-width pixel-height increment) i
    (decf pixel-height increment)
    (resize-pixels pixel-width pixel-height)))

(defun expand-width-pixels (i)
  "Expand the window width by pixels."
  (with-slots (pixel-width pixel-height increment) i
    (incf pixel-width increment)
    (resize-pixels pixel-width pixel-height)))

(defun expand-height-pixels (i)
  "Expand the window height by pixels."
  (with-slots (pixel-width pixel-height increment) i
    (incf pixel-height increment)
    (resize-pixels pixel-width pixel-height)))

(defun shrink-width-chars (i)
  (with-slots (char-width char-height increment) i
    (decf char-width)
    (resize-characters char-width char-height)))

(defun shrink-height-chars (i)
  (with-slots (char-width char-height) i
    (decf char-height)
    (resize-characters char-width char-height)))

(defun expand-width-chars (i)
  "Expand the window width by characters."
  (with-slots (char-width char-height) i
    (incf char-width)
    (resize-characters char-width char-height)))

(defun expand-height-chars (i)
  "Expand the window height by characters."
  (with-slots (char-width char-height) i
    (incf char-height)
    (resize-characters char-width char-height)))

(defmethod move-to-top ((i xterminator))
  "Raise the window."
  (raise))

(defmethod move-to-bottom ((i xterminator))
  "Lower the window."
  (lower))

(defmethod redraw ((i xterminator))
  "Lower the window."
  (refresh-term))

(defun toggle-fullscreen (i)
  (with-slots (fullscreen) i
    (setf fullscreen (not fullscreen))
    (set-fullscreen fullscreen)))

(defun fullscreen-on (i)
  (with-slots (fullscreen) i
    (set-fullscreen (setf fullscreen t))))

(defun fullscreen-off (i)
  (with-slots (fullscreen) i
    (set-fullscreen (setf fullscreen nil))))

(defmethod update-display ((i xterminator))
  (with-slots (x y char-width char-height pixel-width pixel-height fullscreen)
      *xterminator*
    (erase)
    (move 0 0)
    ;; (addstr (format nil "X: ~d~%Y: ~d~%Width: ~d ~d~%Height: ~d ~d~%"
    ;; 		    x y char-width pixel-width char-height pixel-height))))
    (addstr (with-output-to-string (str)
	      (print-values*
	       (x y char-width char-height pixel-width pixel-height fullscreen)
	       str)))
    (addch (char-code #\newline))
    (addstr (format nil "hjkl    - Move window (HJKL by pixel)~%~
                         <>,.    - Resize window~%~
                         f       - Toggle fullscreen~%~
                         [ ]     - Raise / Lower~%~
                         [Enter] - Edit title~%~
                         q       - Quit~%"))))

(defun control-xterm ()
  (with-terminal (:ansi)
    (let ((*xterminator*
	   (make-instance 'xterminator
			  :keymap (list *xterminator-keymap*
					*default-inator-keymap*))))
      (event-loop *xterminator*))))

#+lish
(lish:defcommand xterm-control
  ((iconify boolean :short-arg #\i :help "Iconify the terminal.")
   (x integer :short-arg #\x
    :help "Set the window's horizontal position.")
   (y integer :short-arg #\y
    :help "Set the window's vertical position.")
   (width integer :short-arg #\w
    :help "Set the window's horizontal size.")
   (height integer :short-arg #\h
    :help "Set the window's vertical size.")
   (raise boolean :short-arg #\r
    :help "Raise the window in the stacking order.")
   (lower boolean :short-arg #\l
    :help "Lower the window in the stacking order.")
   (toggle-fullscreen boolean :short-arg #\f
    :help "True to toggle the window's fullscreen state.")
   (fullscreen boolean :short-arg #\F
    :help "Set the window's fullscreen state.")
   (title string :short-arg #\t :help "Set the window's title.")
   (get-title boolean :short-arg #\g :help "Get the window's title.")
   )
  "Control an XTerm comaptible terminal. If no arguments are given, go into an
interactive control mode."
  (if (or iconify x y width height raise lower toggle-fullscreen fullscreen
	  title get-title)
      (let (xt)
	(when (and raise lower)
	  (error "I can't both raise and lower the window."))
	(when (and toggle-fullscreen fullscreen)
	  (error "I can't both set and toggle fullscreen mode."))
	(when (or x y)
	  (move-to (or x "") (or y "")))
	(when (or width height)
	  (setf xt (or xt (make-instance 'xterminator)))
	  (resize-characters (or width (xterminator-char-width xt))
			     (or height (xterminator-char-height xt))))
	(when title (set-title title))
	(when toggle-fullscreen
	  (setf xt (or xt (make-instance 'xterminator)))
	  (set-fullscreen (not (xterminator-fullscreen xt))))
	(when fullscreen (set-fullscreen fullscreen))
	(when raise (raise))
	(when lower (lower))
	(when iconify (iconify t))
	(when get-title (format t "~a~%" (get-title))))
      (control-xterm)))

;; EOF
