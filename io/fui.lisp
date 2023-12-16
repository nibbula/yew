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
   #:draw-window
   #:erase-window
   #:make-window
   #:delete-window
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

(defun draw-box (x y width height &key string #| alt-chars plain |#)
  "Draw a box at X Y of WIDTH and HEIGHT. STRING a string of WIDTH to use for
drawing, which will get overwritten."
  (let* (str-len str)
    (when string
      (setf str-len (- width 2)
	    str (fill string (code-char #x2500) :end str-len))) ; ─

    (tt-move-to y x)
    (tt-write-char (code-char #x250c))	; ┌
    (if string
	(tt-write-string str :end str-len)
	(loop :repeat (- width 2) :do (tt-write-char (code-char #x2500)))) ; ─
    (tt-write-char (code-char #x2510))	; ┐
    (loop :for iy :from (1+ y) :below (+ y (1- height)) :do
       (tt-move-to iy x)
       (tt-write-char (code-char #x2502)) ; │
       (tt-move-to iy (+ x (1- width)))
       (tt-write-char (code-char #x2502))) ; │
    (tt-move-to (+ y (1- height)) x)
    (tt-write-char (code-char #x2514))	; └
    (if string
	(tt-write-string str :end str-len)
	(loop :repeat (- width 2) :do (tt-write-char (code-char #x2500)))) ; ─
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
    :documentation "The terminal the window is on."))
  (:documentation "A stupid text window."))

(defun erase-area (window x y width height &key string)
  (with-slots (background-color terminal) window
    (let* ((*terminal* terminal)
	   (str (if string
		    (fill string #\space :end width)
		    (make-string width :initial-element #\space))))
      (loop :for iy :from y :below (+ y height) :do
	 (tt-move-to iy x)
	 (when background-color
	   (tt-color :default background-color))
	 (tt-write-string str)))))

(defun erase-window (window)
  (with-slots (x y width height border background-color) window
    (if border
	(erase-area window (1- x) (1- y) (+ width 2) (+ height 2))
	(erase-area window x y width height))))

(defun draw-window (window)
  (with-slots (width height x y border terminal) window
    (let ((*terminal* terminal))
      (erase-window window)
      (let ((str (make-string width :initial-element #\space)))
	;; The border is outside the window.
	(when border
	  (draw-box (1- x) (1- y) (+ width 2) (+ height 2) :string str))))))

(defun make-window (#| &rest keys |#
		    &key (width 40) (height 10) (x 1) (y 1) (border t)
		      background-color (terminal *terminal*))
  (declare (ignorable border background-color terminal))
  (let ((window (make-instance 'fui-window
			       :width width :height height
			       :x x :y y :border border
			       :background-color background-color
			       :terminal terminal))
	(str (make-string width :initial-element #\space)))
    ;;(dbugf :fui "make-window ~s x ~s @ [~s ~s]~%" width height x y)
    ;; erase the window
    (erase-area window x y width height :string str)
    (draw-window window)
    window))

(defun delete-window (window)
  (erase-window window))

(defun window-move-to (window row column)
  (with-slots (x y width height text-x text-y terminal) window
    (let ((*terminal* terminal)
	  (start-x (if (< column 0) x (min (+ x column) (+ x (1- width)))))
	  (start-y (if (< row 0)    y (min (+ y row)    (+ y (1- height))))))
      (setf text-x column
	    text-y row)
      (tt-move-to start-y start-x))))

(defun re-fangle (string)
  "Turn FATCHAR-STRINGS back to FAT-STRINGS, but leave other things."
  (typecase string
    (fatchar-string (make-fat-string :string string))
    (t string)))

(defun window-text (window text &key row column)
  (with-slots (x y width height text-x text-y terminal) window
    (when (not row)
      (setf row text-y))
    (when (not column)
      (setf column text-x))
    ;;(dbugf :fui "text-x=~s text-y=~s~%" text-x text-y)
    (let* ((*terminal* terminal)
	   ;; (len (display-length l))
	   ;; (start-x (if (< column 0) x (min (+ x column) (+ x (1- width)))))
	   ;; (start-y (if (< row 0)    y (min (+ y row)    (+ y (1- height)))))
	   (output-y 0)
	   ;; (start-pos (if (< column 0) (min len (- column)) 0))
	   ;; (end-pos   (min len (max 0 (- width column))))
	   (lines
	    (mapcar #'re-fangle
		    (split-sequence #\newline
				    (string-vector text)
				    :key #'simplify-char))))
      ;; (dbugf :fui "start-x ~d start-y ~d~%start-pos ~d end-pos ~d~%"
      ;; 	      start-x start-y start-pos end-pos)
      (dbugf :fui "lines = ~s~%" lines)
      ;;(tt-move-to start-y start-x)
      (loop :with len :and start-pos :and end-pos :and str-len
	 :for l :in lines
	 :do
	 (setf len (display-length l)
	       str-len (olength l)
	       start-pos (if (< column 0) (min len (- column)) 0)
	       end-pos   (min len (max 0 (- width column))))
	 (when (and (>= row 0) (< row height)
		    (< column width) (< start-pos len))
	   (window-move-to window (+ row output-y) column)
	   (tt-write-string l
			    :start (min (max 0 start-pos) len)
			    ;; @@@ This is wrong. The problem is no knowing what
			    ;; character length corresponds to what display
			    ;; length.
			    ;; @@@ make char-util:display-to-char-index work!
			    :end   (min (max start-pos end-pos) str-len))
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
		 :column (round (- (/ width 2) (/ (display-length text) 2))))))

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
