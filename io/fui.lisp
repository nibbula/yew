;;;
;;; fui.lisp - Fake UI
;;;

(defpackage :fui
  (:documentation "Fake UI")
  (:use :cl :dlib :dlib-misc :stretchy :char-util :keymap :terminal :inator
	:collections :fatchar :fatchar-io)
  (:export
   #:*interactive*
   #:non-interactively
   #:interactively
   #:pause
   #:fui-window
   #:fui-window-x #:fui-window-y #:fui-window-y #:fui-window-width
   #:fui-window-height #:fui-window-border
   #:fui-window-text-x #:fui-window-text-y
   #:fui-window-terminal
   #:fui-window-draw-content-function
   #:draw-window
   #:erase-window
   #:make-window
   #:delete-window

   #:*box-chars-unicode*
   #:*box-chars-ascii*
   #:*box-chars-heavy-unicode*
   #:*box-chars-double-unicode*
   #:*box-chars-round-unicode*
   #:*box-chars*
   #:*box-char-names*
   #:+upper-left+ #:+upper-right+ #:+lower-left+ #:+lower-right+
   #:+vertical+ #:+horizontal+
   #:draw-box

   #:window-move-to
   #:window-text
   #:window-centered-text
   #:display-text
   #:show-text
   #:popup-y-or-n-p
   #:with-typeout
   #:help-list
   ))
(in-package :fui)

;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; @@@ Is *interactive* really a useful facility?

(defvar *interactive* t
  "True when we can expect user interaction.")

(defmacro non-interactively (&body body)
  "Evaluate body without pausing for PAUSE."
  `(let ((*interactive* nil))
     ,@body))

(defmacro interactively (&body body)
  "Evaluate body with pausing for PAUSE."
  `(let ((*interactive* t))
     ,@body))

(defun pause (&optional (prompt "[Press Enter]") &rest args)
  "Print a message and wait for Enter to be pressed. Does nothing if not
*interactive*."
  (when *interactive*
    (apply #'format *standard-output* prompt args)
    (finish-output *standard-output*)
    (read-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

;;(defparameter *plain-acs-table* nil)

(defun offscreen-p (x y width height)
  "Return true if the box is totally off screen."
  (or (< (+ x width) 0)  (> x (1- (tt-width)))
      (< (+ y height) 0) (> y (1- (tt-height)))))

(defun clip-box (x y width height &key (to-x 0) (to-y 0)
				    (to-width (tt-width))
				    (to-height (tt-height)))
  "Clip a rectangle to another rectangle. The region to clip to defaults to the
terminal. Returns four values of the clipped (x y width height). Parameters are:
 ‘x’‘y’ ‘width’ ‘height’    Dimensions of the source rectangle.
 ‘to-x’ ‘to-y’        Top left. Default to 0.
 ‘to-width’           Horizontal dimension of clip. Defaults to (tt-width).
 ‘to-height’          Vertical dimension of clip. Defaults to (tt-height)."
  ;; Offscreen check should be done before calling this.
  ;; (when (offscreen-p x y width height)
  ;;   (values nil nil nil nil))
  (let* ((nx      (clamp x to-x (+ to-x to-width)))
	 (ny      (clamp y to-y (+ to-y to-height)))
	 (right   (clamp (+ x width)  to-x (+ to-x to-width)))
	 (bottom  (clamp (+ y height) to-y (+ to-y to-height)))
	 (nwidth  (- right nx))
	 (nheight (- bottom ny)))
    (values nx ny nwidth nheight)))

;; @@@ consider factoring this with the stuff in table-print.lisp

(defparameter *box-chars-unicode* "┌┐└┘│─"
  "Light box drawing characters in Unicode.")

(defparameter *box-chars-ascii* ".,`'|-"
  "Box drawing characters in ASCII.")

(defparameter *box-chars-heavy-unicode* "┏┓┗┛┃━"
  "Heavy box drawing characters in Unicode.")

(defparameter *box-chars-double-unicode* "╔╗╚╝║═"
  "Double box drawing characters in Unicode.")

(defparameter *box-chars-round-unicode* "╭╮╰╯│─"
  "Rounded 'arc' box drawing characters in Unicode.")

(defparameter *box-chars* *box-chars-unicode*
  "A string or vector of characters for drawing a box. The order is:
upper left, upper right, lower left, lower right, vertical, horizontal.")

(defparameter *box-char-names* nil
  "Names of box drawing character indexs into a *box-chars* array.")

(nos:define-enum-list *box-char-names*
  #(#(+upper-left+ 
      "Upper left corner, unicode: BOX_DRAWINGS_*_DOWN_AND_RIGHT.")
    #(+upper-right+
      "Upper right corner, unicode BOX_DRAWINGS_*_DOWN_AND_LEFT.")
    #(+lower-left+
      "Lower left corner, unicode BOX_DRAWINGS_*_UP_AND_RIGHT.")
    #(+lower-right+
      "Lower right corner, unicode BOX_DRAWINGS_*_UP_AND_LEFT.")
    #(+vertical+
      "Vertical line, unicode BOX_DRAWINGS_*_VERTICAL.")
    #(+horizontal+
      "Horizontal line, unicode BOX_DRAWINGS_*_HORIZONTAL")))

(defun draw-box (x y width height &key string (chars *box-chars*))
  "Draw a box at ‘x’ ‘y’ of ‘width’ and ‘height’. ‘string’ a string of ‘width’
to use for drawing, which will get overwritten. ‘chars’ is an array of box
drawing characters to use, which defaults to *box-chars*."
  (declare (ignore string))
  (let* (#| str-len str |# xx yy ww hh
	 (right  (1- (+ x width)))
	 (bottom (1- (+ y height))))
    (unless (offscreen-p x y width height)
      (multiple-value-setq (xx yy ww hh) (clip-box x y width height))

      ;; (when string
      ;; 	(setf str-len (- ww 2)
      ;; 	      str (fill string (code-char #x2500) :end str-len))) ; ─

      (loop :for i :from yy :below (+ yy hh) :do
        (loop :for j :from xx :below (+ xx ww) :do
          (cond
	    ((and (= i y) (= j x))
	     (tt-write-char-at i j (aref chars +upper-left+)))       ; ┌
	    ((and (= i y) (= j right))
	     (tt-write-char-at i j  (aref chars +upper-right+)))     ; ┐
	    ((and (= i bottom) (= j x))
	     (tt-write-char-at i j (aref chars +lower-left+)))       ; └
	    ((and (= i bottom) (= j right))
	     (tt-write-char-at i j (aref chars +lower-right+)))      ; ┘
	    ((or (= j x) (= j right))
	     (tt-write-char-at i j (aref chars +vertical+)))         ; │
	    ((or (= i y) (= i bottom))
	     (tt-write-char-at i j (aref chars +horizontal+))))))))) ; ─

;; @@@ maybe windows should be implemented as a sub-terminal?

(defclass fui-window ()
  ((x
    :initarg :x :accessor fui-window-x :initform 1 :type fixnum
    :documentation "Top left horizontal coordinate.")
   (y
    :initarg :y :accessor fui-window-y :initform 1 :type fixnum
    :documentation "Top left vertical coordinate.")
   (width
    :initarg :width :accessor fui-window-width :initform 40 :type fixnum
    :documentation "Width in character cells.")
   (height
    :initarg :height :accessor fui-window-height :initform 10 :type fixnum
    :documentation "Height in character cells.")
   (border
    :initarg :border :accessor fui-window-border :initform t :type boolean
    :documentation "True to draw a border around the window.")
   (background-color
    :initarg :background-color
    :accessor fui-window-background-color :initform nil
    :documentation "Window background color.")
   (text-x
    :initarg :text-x :accessor fui-window-text-x :initform 0 :type fixnum
    :documentation "Column to write text at.")
   (text-y
    :initarg :text-y :accessor fui-window-text-y :initform 0 :type fixnum
    :documentation "Row to write text at.")
   (terminal
    :initarg :terminal :accessor fui-window-terminal :initform nil
    :documentation "The terminal the window is on.")
   (draw-content-function
    :initarg :draw-content-function :accessor fui-window-draw-content-function
    :initform nil
    :documentation "Function to draw the window's content."))
  (:documentation "A stupid text window."))

(defgeneric erase-area (window x y width height &key string)
  (:method ((window fui-window) x y width height &key string)
    (declare (ignore string))
    (with-slots (background-color terminal) window
      (let* ((*terminal* terminal))
	;; 	   (str (if string
	;; 		    (fill string #\space :end width)
	;; 		    (make-string width :initial-element #\space))))
	;; (loop :for iy :from y :below (+ y height) :do
	;; 	 (tt-move-to iy x)
	;; 	 (when background-color
	;; 	   (tt-color :default background-color))
	;; 	 (tt-write-string str)))))
	(unless (offscreen-p x y width height)
	  (clip-box x y width height)
	  (tt-color :default background-color)
	  (multiple-value-bind (xx yy ww hh) (clip-box x y width height)
	    (loop :for i :from yy :below (+ yy hh) :do
	      (loop :for j :from xx :below (+ xx ww) :do
	        (tt-write-char-at i j #\space)))))))))

(defgeneric erase-window (window)
  (:method ((window fui-window))
    (with-slots (x y width height border background-color) window
      (if border
	  (erase-area window (1- x) (1- y) (+ width 2) (+ height 2))
	  (erase-area window x y width height)))))

(defgeneric draw-window (window)
  (:method ((window fui-window))
    (with-slots (width height x y border terminal draw-content-function) window
      (let ((*terminal* terminal))
	(erase-window window)
	;;(let ((str (make-string width :initial-element #\space)))
	;; The border is outside the window.
	(when border
	  (draw-box (1- x) (1- y) (+ width 2) (+ height 2)
		    #| :string str |#))
	(when draw-content-function
	  (funcall draw-content-function window))))))

(defmethod initialize-instance
    :after ((o fui-window) &rest initargs &key &allow-other-keys)
  "Initialize a fui-window."
  (declare (ignore initargs))
  (when (or (not (slot-boundp o 'terminal))
	    (not (slot-value o 'terminal)))
    (setf (slot-value o 'terminal) *terminal*))
  (with-slots (x y width height) o
    ;; (erase-area o x y width height #| :string str |#)
    (draw-window o)))

(defun make-window (#| &rest keys |#
		    &key (width 40) (height 10) (x 1) (y 1) (border t)
		      background-color (terminal *terminal*)
		      draw-content-function)
  (declare (ignorable border background-color terminal))
  (let ((window (make-instance 'fui-window
			       :width width :height height
			       :x x :y y :border border
			       :background-color background-color
			       :terminal terminal
			       :draw-content-function draw-content-function))
	;; (str (make-string width :initial-element #\space))
	)
    ;; erase the window
    ;; (erase-area window x y width height :string str)
    ;; (draw-window window)
    window))

(defgeneric delete-window (window)
  (:method ((window fui-window))
    (erase-window window)))

(defgeneric window-move-to (window row column)
  (:documentation "Move the cursor to ‘row’ and ‘column’ in ‘window’.")
  (:method ((window fui-window) row column)
    (with-slots (x y width height text-x text-y terminal) window
      (let ((*terminal* terminal)
	    (start-x (if (< column 0) x (min (+ x column) (+ x (1- width)))))
	    (start-y (if (< row 0)    y (min (+ y row)    (+ y (1- height))))))
	(setf text-x column
	      text-y row)
	(tt-move-to start-y start-x)))))

(defun re-fangle (string)
  "Turn FATCHAR-STRINGS back to FAT-STRINGS, but leave other things."
  (typecase string
    (fatchar-string (make-fat-string :string string))
    (t string)))

(defgeneric window-text (window text &key row column)
  (:documentation
  "Draw ‘text’ in ‘window’. Draw it at ‘row’ and ‘column’ if provided, otherwise
draw it a the current window text position.")
  (:method ((window fui-window) text &key row column)
    (with-slots (x y width height text-x text-y terminal) window
      (when (not row)
	(setf row text-y))
      (when (not column)
	(setf column text-x))

      (let* ((*terminal* terminal)
	     (lines
	       (map 'vector #'re-fangle
		    (split-sequence #\newline
				    (string-vector text)
				    :key #'simplify-char)))
	     (xx 0) (yy 0) (ww 0) (hh 0)
	     (tx 0) (ty 0) (tw 0) (th 0)
	     (start-line 0) (end-line 0)
	     (start-col 0))
	(dbugf :fui "lines = ~s~%" lines)

	;; The dimensions of the raw text.
	(setf tx (+ x column)
	      ty (+ y row)
	      tw (loop :for l :across lines :maximize (display-length l))
	      th (olength lines))

	;; Clip the text to the window.
	(multiple-value-setq (xx yy ww hh)
	  (clip-box tx ty tw th
		    :to-x x :to-y y :to-width width :to-height height))

	;; Clip to the screen.
	(multiple-value-setq (xx yy ww hh)
	  (clip-box xx yy ww hh))

	(setf start-line (if (< ty 0)
			     (- ty)
			     (max 0 (min (- ty y) (1- (length lines)))))
	      end-line   (min (+ start-line hh) (length lines))
	      start-col  (- xx (+ x column)))

	#+(or)
	(dbugf :fui "row ~s column ~s~%~
                     x ~s y ~s width ~s height ~s~%~
                     tx ~s ty ~s tw ~s th ~s~%~
                     xx ~s yy ~s ww ~s hh ~s~%~
                     start-line ~s end-line ~s~%~
                     start-col ~s~%"
	       row column
	       x y width height
	       tx ty tw th
               xx yy ww hh
	       start-line end-line
	       start-col)

	(unless (or (zerop hh) (zerop ww))
	  (loop
	    :for line :from start-line :below end-line
            :for i :from yy :below (+ yy hh)
	    :do
	       (tt-move-to i xx)
	       (dbugf :fui "line ~s i = ~s~%" line i)
               (loop
		 :with len = (olength (oelt lines line))
		 :with end-col = (min (+ start-col ww) len)
		 :for c :from start-col :below end-col
		 :for j :from xx :below (+ xx ww)
		 :do
		    (when (< c len)
		      (tt-write-char (oelt (elt lines line) c))))))

	;; @@@ This will work horribly for terminal-ansi.
	;; We should be able to get rid of this.
	(multiple-value-bind (new-y new-x)
	    (terminal-get-cursor-position *terminal*)
	  ;;(dbugf :fui "new-x=~s new-y=~s~%" new-x new-y)
	  (setf text-x (- new-x xx)
		text-y (- new-y yy)))))))

(defgeneric window-centered-text (window row text)
  (:documentation "Put a centered ‘string’ in ‘window’ at ‘row’.")
  (:method ((window fui-window) row text)
    (with-slots (width) window
      (window-text window text
		   :row row
		   :column (round (- (/ width 2)
				     (/ (display-length text) 2)))))))

;; This interface is somewhat deprecated. I recommend using show-text.
(defun display-text (title text-lines &key input-func (justify t)
					x y width height
					min-width min-height)
  "Display text in a pop up window. Optionally calls INPUT-FUNC with the
window as an argument to get input. If no INPUT-FUNC is provided it just
waits for a key press and then returns."
;;  (with-terminal ()
  (let* ((mid    (truncate (/ (tt-width) 2)))
	 (width  (or width (min (- (tt-width) 6)
				(max (or min-width 0)
				     (+ 4 (loop :for l :in text-lines
					     :maximize (display-length l)))))))
	 (justified-lines (loop :for l :in text-lines
			     :nconc
			     (split-sequence
			      #\newline
			      (if justify
				  (fat-string-string
				   (with-output-to-fat-string (str)
				     (justify-text l :cols (- width 2)
						   :stream str)))
				  (string-vector l))
			      :key #'simplify-char)))
	 (margin (if title 4 0))
	 (height (or height (min
			     (max (or min-height 0)
				  (+ margin (length justified-lines)))
			     (- (tt-height) margin))))
	 (xpos   (truncate (- mid (/ width 2))))
	 (w      (make-window :width width :height height
			      :y (or y 1)
			      :x (or x xpos)
			      :border t))
	 result)
    (when title
      (window-centered-text w 0 title))
    (loop :with i = (if title 2 0)
       :for l :in justified-lines
       :do
       (window-text w (re-fangle l) :row i :column 2)
       (incf i))
    (tt-finish-output)
    (setf result (if input-func
		     (funcall input-func w)
		     (tt-get-key)))
    (delete-window w)
    ;; (tt-clear) ;; ?? really
    (tt-finish-output)
    result))

;; This is kind of like a old-timey typeout window.
;; @@@ Eventually display-text should be a wrapper to this?
;; Differences from display-text:
;;   - don't require title arg
;;   - don't justify by default
;;   - don't require lines to be pre-split
;;   - In general, just take a string and display it as-is.
(defun show-text (text &key input-func title justify
			 x y width height min-width min-height)
  "Display ‘text’ in a pop up window. Optionally calls ‘input-func’ with the
window as an argument to get input. If no ‘input-func’ is provided it just
waits for a key press and then returns.
Keyword arguments are:
 ‘title’                   A title for the window.
 ‘justify’                 True to justify the text in the window.
 ‘x’ ‘y’                   Position of the window.
 ‘width’ ‘height’          Size of the window.
 ‘min-width’ ‘min-height’  Minimum size of the window."
  (with-immediate ()
    (display-text title (if justify
			    (list text)
			    (osplit #\newline text :omit-empty nil))
		  :input-func input-func
		  :justify justify
		  :x x :y y :width width :height height
		  :min-width min-width :min-height min-height)))

(defun popup-y-or-n-p (question &rest args
		       &key title x y width height min-width min-height default
			 justify)
  "A popup window version of Y-OR-N-P. Display the ‘question’.
Keyword arguments are:
 ‘default’                 If given, allow other characters to act as if the
                           default was entered. Must be #\\Y be #\\N.
 ‘title’                   A title for the window.
 ‘justify’                 True to justify the text in the window.
 ‘x’ ‘y’                   Position of the window.
 ‘width’ ‘height’          Size of the window.
 ‘min-width’ ‘min-height’  Minimum size of the window."
  (declare (ignorable title x y width height min-width min-height))
  (labels ((valid-answer (c)
	     (and (characterp c)
		  (find c "YN" :test #'equalp)))
	   (yornp (w)
	     (let (c)
	       (loop :with key = (tt-get-key)
		  :while (or (not (characterp key))
			     (and (not (setf c (valid-answer key)))
				  (not default)))
		  :do
		    (window-text w (ß `(:red " Please type Y or N."))
				 :row (1- (fui-window-height w)) :column 0)
		    (setf key (tt-get-key)))
	       (or c default))))
    (when (and default (not (valid-answer default)))
      (error "The default must be #\\Y or #\\N."))
    (remf args :default)
    (with-immediate ()
      (char= #\Y (apply 'display-text
			nil
			(list "" question "" "Y or N ?")
			:input-func #'yornp
			:justify justify
			args)))))

;; @@@ make justify work? maybe it's the re-fangling?
(defmacro with-typeout ((stream-var &key title input-func #|(justify t)|# x y)
			&body body)
  "Evaluate BODY with STREAM-VAR bound to an output-stream, which will be
displayed as text in a pop up window.

TITLE       Displayed at the top of the window.
INPUT-FUNC  A function called with the window as an argument to get input.
            If no INPUT-FUNC is provided it just waits for a key press and then
            returns.
X Y         Top left coordinates of the window."
;; JUSTIFY     If true, wrap words in the output.
  `(display-text
    ,title
    (split-sequence
     #\newline
     (fat-string-string
      (with-output-to-fat-string (,stream-var)
	,@body)))
    :input-func ,input-func
    :justify nil
    :x ,x :y ,y))

(defun make-reverse-keymap (keymap)
  "Make table of actions and all the keys that invoke them from a keymap."
  ;; work around a compiler bug in sbcl 2.1.8?
  #+sbcl (declare (optimize (speed 0) (safety 0) (debug 2)))
  (let ((rev-hash (make-hash-table)) prefix)
    (declare (special prefix))
    (labels ((action-is-keymap (action)
	       (and (symbolp action) (boundp action)
		    (typep (symbol-value action) 'keymap)))
	     (key-to-add (key)
	       (cond
		 (prefix
		  (append
		   (if (atom prefix) (list prefix) prefix)
		   (list key)))
		  (t
		   key)))
	     (add-key (key def)
	       ;; (format t "add-key ~s ~s~%" key action)
	       (let ((action (key-definition-action def)))
		 (if (action-is-keymap action)
		     (let ((prefix (key-to-add key)))
		       (declare (special prefix))
		       (map-keymap #'add-key (symbol-value action)))
		     (push (key-to-add key) (gethash action rev-hash))))))
      (map-keymap #'add-key keymap))
    rev-hash))

(defun help-list (keymap &optional special-doc-finder prefix)
  "Return a list of key binding help lines, suitable for the HELP function.
The optional SPECIAL-DOC-FINDER is a function which looks up documentation for
keymap bindings."
  ;; Make a reverse hash of functions to keys, so we can put all the bindings
  ;; for a function on one line.
  (let* ((rev-hash (make-reverse-keymap keymap))
	 (key-col-len 0)
	 (table
	  ;; Get the maxiumum size of the keys section
	  (loop :with str
	     :for func :being :the :hash-keys :of rev-hash
	     :collect
	       (cons
		(setf str
		      (format nil "~{~a~^, ~}"
			      (loop :for k :in (gethash func rev-hash)
				 :if (listp k)
				 :collect
				   (format nil "~{~a~^ ~}"
					   (mapcar (_ (nice-char _ :caret t))
						   k))
				 :else
				 :collect (nice-char k :caret t))))
		func)
	     :do (setf key-col-len (max key-col-len (length str))))))
    ;; Actually collect the strings
    #|
    (loop :with doc
       :for func :being :the :hash-keys :of rev-hash
       ;; Look up the documentation for the function.
       :if (setf doc (or (and (or (functionp func)
				  (and (symbolp func) (fboundp func)))
			      (documentation func 'function))
			 (and special-doc-finder
			      (funcall special-doc-finder func))
			 (and (symbolp func)
			      (boundp func)
			      (keymap-p (symbol-value func))
			      (string-downcase func))
			 (and (keymap-p func)
			      (princ-to-string func))
			 nil))
       :collect
       (with-output-to-string (str)
	 (format str "~va - ~a" key-col-len
		 (format nil "~{~a~^, ~}"
			 (loop :for k :in (gethash func rev-hash)
			    :collect (nice-char k :caret t)))
		 doc)))))
    |#
    (loop :with doc
       :for (keys . func) :in table
       ;; Look up the documentation for the function.
       :if (setf doc (or (and (or (functionp func)
				  (and (symbolp func) (fboundp func)))
			      (documentation func 'function))
			 (and special-doc-finder
			      (funcall special-doc-finder func))
			 (and (symbolp func)
			      (boundp func)
			      (keymap-p (symbol-value func))
			      (string-downcase func))
			 nil))
	  :collect
	     (format nil "~@[~a ~]~va - ~a" prefix key-col-len keys doc)
	:else :if (and (not doc) (keymap-p func))
	  :append (help-list func special-doc-finder keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defclass progress-bar
  win)

(defun progress-bar (title &key message)
  "Display a progress bar in a pop up window."
  (let* ((mid (truncate (/ (tt-width) 2)))
	 (width (min (- (tt-width) 6)
		     (+ 4 (max 50 (display-length message)))))
	 (height (min (+ 4 (+ 5))
		      (- (tt-height) 4)))
	 (xpos   (truncate (- mid (/ width 2))))
	 (win    (make-window :height height :width width :y 3 :x pos))
	 (bar    (make-progress-bar :win win)))
    (wcentered win width 0 title)
    (window-text win message :row 1 :column 2)
    ;; (window-text win (format nil "") :row 2 :column 2)
    bar))

(defun progress-bar-update (bar percent &optional status)
  (with-slots (win) bar
    (window-text win message :row 1 :column 2)
    ;; (window-text win (format nil "") :row 2 :column 2)
    (tt-finish-output)))

(defun progress-bar-done (bar)
  (with-slots (win) bar
    (delete-window win)))

(defmacro with-progress-bar ((title &key message) &body body)
  (with-names (bar)
    `(with-terminal ()
       (let (,bar)
	 (unwind-protect
	      (progn
		(setf ,bar (progress-bar ,title :message ,message))
		,@body)
	   (progress-bar-done ,bar))))))
|#

;; End
