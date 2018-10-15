;;
;; fui.lisp - Fake UI
;;

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
   #:draw-window
   #:make-window
   #:delete-window
   #:draw-box
   #:window-move-to
   #:window-text
   #:window-centered-text
   #:display-text
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

(defun draw-box (x y width height &key string)
  "Draw a box at X Y of WIDTH and HEIGHT. STRING a string of WIDTH to use for
drawing, which will get overwritten."
  (let* ((str-len (- width 2))
	 (str (if string
		  (fill string (code-char #x2500) :end str-len)
		  (make-string str-len
			      :initial-element (code-char #x2500))))) ; ─
    (tt-move-to y x)
    (tt-write-char (code-char #x250c))	; ┌
    (tt-write-string str :end str-len)
    (tt-write-char (code-char #x2510))	; ┐
    (loop :for iy :from (1+ y) :below (+ y (1- height)) :do
       (tt-move-to iy x)
       (tt-write-char (code-char #x2502)) ; │
       (tt-move-to iy (+ x (1- width)))
       (tt-write-char (code-char #x2502))) ; │
    (tt-move-to (+ y (1- height)) x)
    (tt-write-char (code-char #x2514))	; └
    (tt-write-string str :end str-len)
    (tt-write-char (code-char #x2518)))) ; ┘

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
    :initarg :border :accessor fui-window-border :initform nil :type boolean
    :documentation "True to draw a border around the window.")
   (text-x
    :initarg :text-x :accessor fui-window-text-x :initform 0 :type fixnum
    :documentation "Column to write text at.")
   (text-y
    :initarg :text-y :accessor fui-window-text-y :initform 0 :type fixnum
    :documentation "Row to write text at.")
   )
  (:documentation "A stupid text window."))

(defun erase-area (x y width height &key string)
  (let ((str (if string
		 (fill string #\space :end width)
		 (make-string width :initial-element #\space))))
    (loop :for iy :from y :below (+ y height) :do
       (tt-move-to iy x)
       (tt-write-string str))))

(defun erase-window (window)
  (with-slots (x y width height border) window
    (if border
	(erase-area (1- x) (1- y) (+ width 2) (+ height 2))
	(erase-area x y width height))))

(defun draw-window (window)
  (with-slots (width height x y border) window
    (erase-window window)
    (let ((str (make-string width :initial-element #\space)))
      ;; The border is outside the window.
      (when border
	(draw-box (1- x) (1- y) (+ width 2) (+ height 2) :string str)))))

(defun make-window (&rest keys
		    &key (width 40) (height 10) (x 1) (y 1) (border t))
  (declare (ignorable border))
  (let ((win (apply #'make-instance 'fui-window keys))
	(str (make-string width :initial-element #\space)))
    ;;(dbugf :fui "make-window ~s x ~s @ [~s ~s]~%" width height x y)
    ;; erase the window
    (erase-area x y width height :string str)
    (draw-window win)
    win))

(defun delete-window (window)
  (erase-window window))

(defun window-move-to (window row column)
  (with-slots (x y width height text-x text-y) window
    (let ((start-x (if (< column 0) x (min (+ x column) (+ x (1- width)))))
	  (start-y (if (< row 0)    y (min (+ y row)    (+ y (1- height))))))
      (setf text-x column
	    text-y row)
      (tt-move-to start-y start-x))))

(defun refangle (string)
  "Turn FATCHAR-STRINGS back to FAT-STRINGS, but leave other things."
  (typecase string
    (fatchar-string (make-fat-string :string string))
    (t string)))

(defun window-text (window text &key row column)
  (with-slots (x y width height text-x text-y) window
    (when (not row)
      (setf row text-y))
    (when (not column)
      (setf column text-x))
    ;;(dbugf :fui "text-x=~s text-y=~s~%" text-x text-y)
    (let* (;; (len (display-length l))
	   ;; (start-x (if (< column 0) x (min (+ x column) (+ x (1- width)))))
	   ;; (start-y (if (< row 0)    y (min (+ y row)    (+ y (1- height)))))
	   (output-y 0)
	   ;; (start-pos (if (< column 0) (min len (- column)) 0))
	   ;; (end-pos   (min len (max 0 (- width column))))
	   (lines
	    (mapcar #'refangle
		    (split-sequence #\newline
				    (string-vector text)
				    :key #'simplify-char))))
      ;; (dbugf :fui "start-x ~d start-y ~d~%start-pos ~d end-pos ~d~%"
      ;; 	      start-x start-y start-pos end-pos)
      (dbugf :fui "lines = ~s~%" lines)
      ;;(tt-move-to start-y start-x)
      (loop :with len :and start-pos :and end-pos
	 :for l :in lines
	 :do
	 (setf len (display-length l)
	       start-pos (if (< column 0) (min len (- column)) 0)
	       end-pos   (min len (max 0 (- width column))))
	 (when (and (>= row 0) (< row height)
		    (< column width) (< start-pos len))
	   (window-move-to window (+ row output-y) column)
	   (tt-write-string l
			    :start (min (max 0 start-pos) len)
			    :end   (min (max start-pos end-pos) len))
	   (incf output-y)))
      (window-move-to window (+ row output-y 1) column)
      ;; (tt-write-string text
      ;; 		 :start (min (max 0 start-pos) len)
      ;; 		 :end   (min (max start-pos end-pos) len))
      ;; @@@ This will work horribly for terminal-ansi. But what's our choice?
      (multiple-value-bind (new-y new-x)
	  (terminal-get-cursor-position *terminal*)
	;;(dbugf :fui "new-x=~s new-y=~s~%" new-x new-y)
	;; @@@ This wrong since it doesn't even clip.
	(setf text-x (- new-x x)
	      text-y (- new-y y))))))

;; Hello this is some long text

(defun window-centered-text (window row text)
  "Put a centered STRING in WINDOW at ROW."
  (with-slots (width) window
    (window-text window text
		 :row row
		 :column (round (- (/ width 2) (/ (length text) 2))))))

;; This is kind of like a old-timey typeout window.
(defun display-text (title text-lines &key input-func (justify t))
  "Display text in a pop up window. Optionally calls INPUT-FUNC with the
window as an argument to get input. If no INPUT-FUNC is provided it just
waits for a key press and then returns."
;;  (with-terminal ()
  (let* ((mid    (truncate (/ (tt-width) 2)))
	 (width  (min (- (tt-width) 6)
		      (+ 4 (loop :for l :in text-lines
			      :maximize (display-length l)))))
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
	 (height (min (+ 4 (length justified-lines))
		      (- (tt-height) 4)))
	 (xpos   (truncate (- mid (/ width 2))))
	 (w      (make-window :width width :height height :y 3 :x xpos
			      :border t))
	 result)
    (window-centered-text w 0 title)
    (loop :with i = 2
       :for l :in justified-lines
       :do
       (window-text w (refangle l) :row i :column 2)
       (incf i))
    (tt-finish-output)
    (setf result (if input-func
		     (funcall input-func w)
		     (tt-get-char)))
    (delete-window w)
    ;; (tt-clear) ;; ?? really
    (tt-finish-output)
    result))

(defun help-list (keymap &optional special-doc-finder)
  "Return a list of key binding help lines, suitable for the HELP function.
The optional SPECIAL-DOC-FINDER is a function which looks up documentation for
keymap bindings."
  ;; Make a reverse hash of functions to keys, so we can put all the bindings
  ;; for a function on one line.
  (let ((rev-hash (make-hash-table)) key-col-len)
    (flet ((add-key (k v) (push k (gethash v rev-hash))))
      (map-keymap #'add-key keymap))
    ;; Get the maxiumum size of the keys section
    (setf key-col-len
	  (loop :for func :being :the :hash-keys :of rev-hash
	     :maximize
	     (length (format nil "~{~a~^, ~}"
			     (loop :for k :in (gethash func rev-hash)
				:collect (nice-char k :caret t))))))
    ;; Actually collect the strings
    (loop :with doc
       :for func :being :the :hash-keys :of rev-hash
       ;; Look up the documentation for the function.
       :if (setf doc (or (and (or (functionp func)
				  (and (symbolp func) (fboundp func)))
			      (documentation func 'function))
			 (and special-doc-finder
			      (funcall special-doc-finder func))
			 (and (keymap-p func)
			      (string-downcase func))
			 nil))
       :collect
       (with-output-to-string (str)
	 (format str "~va - ~a" key-col-len
		 (format nil "~{~a~^, ~}"
			 (loop :for k :in (gethash func rev-hash)
			    :collect (nice-char k :caret t)))
		 doc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defstruct progress-bar
  win
  width
  height)

(defun progress-bar (title &key message)
  "Display a progress bar in a pop up window."
  (with-curses
    (let* ((mid (truncate (/ *cols* 2)))
	   (width (min (- *cols* 6)
		       (+ 4 (max 50 (length message)))))
	   (height (min (+ 4 (+ 5))
			(- *lines* 4)))
	   (xpos   (truncate (- mid (/ width 2))))
	   (win    (newwin height width 3 xpos))
	   (bar    (make-progress-bar :win win :width width :height height)))
      (box win 0 0)
      (wcentered win width 0 title)
      (mvwaddstr win 1 2 message)
      (mvwaddstr win 2 2 (format nil ""))
      (refresh)
      (wrefresh w)
      result)))

(defun progress-bar-update (bar percent &optional status)
  (with-slots (win) bar
    (mvwaddstr win 1 2 message)
    (mvwaddstr win 2 2 (format nil ""))
    (refresh)
    (wrefresh win)))

(defun progress-bar-done (bar)
  (with-slots (win) bar
    (delwin w)
  (delwin w)
  (clear)
  (refresh))
|#

;; EOF
