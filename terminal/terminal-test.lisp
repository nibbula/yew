;;;
;;; terminal-test.lisp - Test the generic terminal library.
;;;

(defpackage :terminal-test
  (:documentation "Test the generic terminal library.")
  (:use :cl :dlib :terminal :dcolor :char-util :fatchar)
  (:export
   #:run
   #:menu
   #:test
   #:churn
   ))
(in-package :terminal-test)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

(defparameter *basic-colors*
  '(:black :red :green :yellow :blue :magenta :cyan :white))

(defparameter *contrast-colors*
  '(:white :black :black :black :white :black :black :black)
  "Color which will contrast with *basic-colors*")

(defun z-width () (or (tt-width) 80))
(defun z-height () (or (tt-height) 24))

(defun ask-class ()
  (format t "Which sub-class ?~%")
  (let (types)
    (loop
       :for i = *terminal-types* :then (cddr i)
       :for n = 0 :then (1+ n)
       :while i
       :do (format t "~d. ~:(~a~)~%" n (car i))
       (pushnew (car i) types))
    (format t "? ")
    (finish-output)
    (setf types (nreverse types))
    (prog1 (let ((choice (ignore-errors (parse-integer (read-line)))))
	     (if choice
		 (nth choice types)
		 (prog1 nil (format t "Whaaa?~%"))))
      (finish-output)
      (clear-input))))

(defun center (text offset)
  (tt-move-to (+ (truncate (z-height) 2)
		 offset)
	      (- (truncate (z-width) 2)
		 (truncate (display-length text) 2)))
  (let ((str (format nil text)))
    (tt-write-string text :end (min (tt-width) (length str)
				    (display-length str)))))

(defun prompt-next (&key
		      (message "Press Q to quit, anything else to continue.")
		      (position :bottom)
		      redraw-func input-func)
  "Display ‘message’ and wait for a key press. The message is displayed
at ‘position’, which can be one of :bottom, :top, or :center. If ‘redraw-func’
is provided, it will be called to re-draw the scrren, and pressing ^L will
work. Pressing #\Q will invoke a QUIT restart."
  (with-immediate ()
    (labels
	((show-message ()
	   (when message
	     (ecase position
	       (:bottom
		;; (tt-write-string-at (1- (terminal-window-rows *terminal*)) 0
		(tt-write-string-at (1- (z-height)) 0
				    message))
	       (:top
		(tt-write-string-at 0 0 message))
	       (:center
		(center message 0)))))
	 (handle-input ()
	   (case (tt-get-key)
	     ((#\Q #\q)
	      (invoke-restart (find-restart 'quit)))
	     (#.(ctrl #\L)
	      redraw-func)
	     (:resize t))))
      (when (not input-func)
	(setf input-func #'handle-input))
      (show-message)
      (tt-finish-output)
      (unwind-protect
        (progn
	  (tt-enable-events :resize)
	  (loop
	    :while (funcall input-func)
	    :do
	    (when (and redraw-func
		       (or (functionp redraw-func)
			   (and (symbolp redraw-func)
				(fboundp redraw-func))))
	      (funcall redraw-func)
	      (show-message))))
	(tt-enable-events :none)))))

(defmacro blurp ((&key (position :bottom)) &body body)
  `(with-immediate ()
    (tt-clear) (tt-home)
    ,@body
    (prompt-next :position ,position)))

(defparameter *top-left*
  #("X <---"
    "^"
    "|"
    "|"))
(defparameter *top-right*
  #("---> X"
    "     ^"
    "     |"
    "     |"))
(defparameter *bottom-left*
  #("|"
    "|"
    "v"
    "X <---"))
(defparameter *bottom-right*
  #("     |"
    "     |"
    "     v"
    "---> X"))

(defun put-at (row col thing &key step)
  "Put a ‘thing’ at ‘row’ and ‘col’. Don't ouput spaces. Clip to the screen.
‘thing’ should be an array of strings."
  (tt-move-to row col)
  (loop
    :for s :across thing
    :for y := row :then (1+ y)
    :do
    (loop
      :for c :across s
      :for x := col :then (1+ x)
      :do
      (when (and (char/= #\space c)
		 (>= y 0) (< y (tt-height))
		 (>= x 0) (< x (tt-width)))
	(tt-write-char-at y x c)
	(when step
	  (tt-get-key))))))

(defun test-screen-size-draw ()
  (tt-clear) (tt-home)
  (put-at 0 0 *top-left*)
  (put-at 0 (- (tt-width) 6) *top-right*)
  (put-at (- (tt-height) 4) 0 *bottom-left*)
  (put-at (- (tt-height) 4) (- (tt-width) 6) *bottom-right* #| :step t |#)

  ;; message
  (center "There should be an 'X' in every corner of the screen.~%" -1)
  (center "Press Q to quit, anything else to continue." 0)
  (center "Also try resizing the window." 1)
  (center "hjkl or arrows key might resize the window." 2)
  (tt-finish-output))

(defun test-screen-size-input ()
  (case (tt-get-key)
    ((#\Q #\q)
     (invoke-restart (find-restart 'quit)))
    (#.(ctrl #\L) (test-screen-size-draw) t)
    (:resize t)
    ((#\h #\H :left)  (decf (tt-width)))
    ((#\j #\J :down)  (incf (tt-height)))
    ((#\k #\K :up)    (decf (tt-height)))
    ((#\l #\L :right) (incf (tt-width)))))

(defun test-screen-size ()
  (test-screen-size-draw)
  (prompt-next :redraw-func #'test-screen-size-draw :message nil
	       :input-func #'test-screen-size-input)
  ;; (unwind-protect
  ;;      (progn
  ;; 	 (tt-enable-events :resize)
  ;; 	 (loop :while (case (tt-get-key)
  ;; 			((#\Q #\q)
  ;; 			 (invoke-restart (find-restart 'quit)))
  ;; 			(:resize t)
  ;; 			(otherwise nil))
  ;; 	    :do (test-screen-size-draw)))
  ;;   (tt-enable-events :none))
  )

(defun test-move-to-col ()
  (blurp ()
   (loop :for i :from -4 :to 4 :do
      (loop :for j :from 1 :below (1- (z-width)) :do
	 (let ((s (* 3 (cos (/ j 4.2)))))
	   (if (<= (round s) i)
	       (progn
		 (tt-beginning-of-line)
		 (tt-move-to-col j)
		 (tt-color :magenta :black)
		 (tt-write-char #\*))
	       (progn
		 (tt-beginning-of-line)
		 (tt-move-to-col j)
		 (tt-color :cyan :black)
		 (tt-write-char #\.)))))
      (tt-down 1))
   (tt-move-to 10 0)
   (tt-color :default :default)
   (tt-write-string "You should see an approximately smooth sine wave.")
   (tt-write-char #\newline)
   (tt-write-string "The top should be cyan and the bottom should be magenta.")
   ))

(defun test-ins-del ()
  (blurp ()
   (let* ( #| (half (/ (terminal-window-columns *terminal*) 2)) |#
	  (full (z-width))
	  (height 8)
	  (thing "|----->")
	  (junk "#%_.")
	  (thing-width (length thing))
	  (sleep .01))

     (tt-move-to (+ height 2) 0)
     (tt-write-string "You should see stuff pushed off the screen.")
     (loop :for i :from 0 :below height :do
	(tt-move-to i 8)
	(loop :for i :from thing-width :below (1- full)
	   :do (tt-write-char (elt junk (random (length junk))))))

     (loop :for i :from 0 :below height :do
	(tt-move-to i 0)
	(tt-write-string thing))

     (loop :for col :from 0 :below (- full thing-width) :do
	(loop :for i :from 0 :below height :do
	   (tt-move-to i 0)
	   (tt-insert-char 1))
	(tt-finish-output)
	(sleep sleep))

     (loop :for col :from 0 :below (- full thing-width) :do
	(loop :for i :from 0 :below height :do
	   (tt-move-to i 0)
	   (tt-delete-char 1))
	(tt-finish-output)
	(sleep sleep))
     )))

(defparameter *all-attrs*
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

(defun test-attrs ()
  (blurp ()
   (labels ((zerp (attr state)
	      (let ((sym (intern (s+ "TERMINAL-" (string-upcase attr))
				 (find-package :terminal))))
		(funcall sym *terminal* state)))
	    ;; (donk (attr str)
	    ;;   (let ((sym (intern (s+ "TERMINAL-" (string-upcase attr)))))
	    ;; 	(funcall sym *terminal* t)
	    ;; 	(tt-write-string str)
	    ;; 	(funcall sym *terminal* nil)))
	    (zink (attr)
	      (format nil "~10a" (string-capitalize attr)))
	    ;; (derp (attr)
	    ;;   (donk attr (zink attr)))
	    )
     (let ((attrs '(standout underline bold inverse)))
       (tt-write-string "           ")
       (loop :for a :in attrs :do
	  (tt-write-string "| ")
	  (tt-write-string (zink a))
	  (tt-write-char #\space))
       (tt-write-char #\newline)
       (tt-write-string "-----------")
       (loop :for i :from 1 :to (length attrs) :do
	  (tt-write-string "|------------"))
       (tt-write-char #\newline)
       (loop :for a :in attrs :do
	  (tt-write-string (zink a))
	  (loop :for b :in attrs :do
	     (tt-write-string " | ")
	     (zerp a t)
	     (zerp b t)
	     (tt-write-string "[Blargity]")
	     (zerp b nil)
	     (zerp a nil)
	     )
	  (tt-write-char #\newline)))
     (tt-format "~%You should see a table of combined attributes.")))

  (blurp ()
    ;; Test all exotic attributes
    (tt-format "All attributes:~%")
    (loop :for a :in *all-attrs*
       :do
       (tt-set-attributes (list a))
       (tt-write-string (string-capitalize a))
       (tt-normal)
       (when (eq a :invisible)
	 (tt-format " <-- ~a" (string-capitalize a)))
       (tt-newline))
    (tt-normal)
    (tt-newline)

    ;; Test set-rendition with attrs and colors
    (tt-format "Attributes mixed with colors:~%")
    (flet ((attr-name (a)
	     (string-capitalize
	      ;; the full name is too long
	      (if (eq a :double-underline) :double-u a))))
      (loop :with attrs
        :for fg :in *basic-colors* :do
        (tt-set-rendition (make-fatchar :attrs attrs :fg fg))
        (tt-format "Normal")
        (loop
          :for a :in '(:bold :underline :double-underline :reverse) :do
          (tt-write-char #\space)
          (tt-set-rendition (make-fatchar :attrs (push a attrs) :fg fg))
          (tt-format "~a" (attr-name a)))
        (loop #| :with a |#
          :while (pop attrs) :do
          (tt-write-char #\space)
          (tt-set-rendition (make-fatchar :attrs attrs :fg fg))
          (tt-format "~a" (attr-name (or (car attrs) "Normal"))))
        (tt-newline)))
    (tt-normal)
    (tt-color :default :default))

  #| Too ugly
  (blurp ()
    (let (span)
      (loop :for a :in (remove :invisible *all-attrs*)
	 :for i = 0 :then (+ i 2)
	 :do
	 (setf span `(,a
		      ,(make-string i :initial-element #\space)
		      ,(string-capitalize a) #\newline ,@span )))
      (tt-write-span span)
      (setf span nil)
      (loop :for a :in (reverse (remove :invisible *all-attrs*))
	 :for i = 0 :then (+ i 2)
	 :do
	 (setf span `(,a
		      ,(make-string i :initial-element #\space)
		      ,(string-capitalize a) #\newline ,@span )))
      (tt-write-span span)))
  |#
  )

(defun test-colors ()
  (blurp ()
   (let ((colors (cons :default *basic-colors*))
	 (i 0))
     (flet ((squares (x y title func)
	      (setf i 0)
	      (tt-move-to y x)
	      (tt-format "~a" title)
	      (when func
		(funcall func *terminal* t))
	      (loop :for fg :in colors
		 :for row :from (1+ y) :below (+ y 1 (length colors))
		 :do
		 (tt-move-to row x)
		 (loop :for bg :in colors :do
		    (tt-color fg bg)
		    (tt-format "~2,,'0d " i)
		    (tt-color :default :default)
		    (incf i)))))
       ;; Show the color pairs
       (squares 0 1 "Color pairs" nil)
       (squares 0 12 "Bold colors" #'terminal-bold)
       (squares 30 1 "Bold & inverse colors" #'terminal-inverse)

       ;; Try writing colored strings in various ways.
       (let ((line 13))
	 (tt-normal)
	 (tt-move-to line 30)
	 (tt-write-span '((:red "Red") " " (:green "Green") " " (:blue "Blue")
			  " Normal"))

	 (tt-move-to (incf line) 30)
	 (tt-color :red nil) (tt-write-string "Red") (tt-write-char #\space)
	 (tt-color :green nil) (tt-write-string "Green") (tt-write-char #\space)
	 (tt-color :blue nil) (tt-write-string "Blue") (tt-write-char #\space)
	 (tt-color :default :default) (tt-write-string "Normal")

	 (tt-move-to (incf line) 30)
	 (tt-write-span '((:red "Red") " "))
	 (tt-color :green nil) (tt-write-string "Green ")
	 (tt-color :default nil)
	 (tt-write-span '((:blue "Blue") " "))
	 (tt-write-string "Normal")

	 (tt-move-to (incf line) 30)
	 (tt-write-span '((:cyan "Cyan") " " (:magenta "Magenta") " "
			  (:yellow "Yellow") " Normal"))

	 (tt-move-to (incf line) 30)
	 (tt-color :cyan nil) (tt-write-string "Cyan") (tt-write-char #\space)
	 (tt-color :magenta nil) (tt-write-string "Magenta") (tt-write-char #\ )
	 (tt-color :yellow nil) (tt-write-string "Yellow") (tt-write-char #\ )
	 (tt-color :default :default) (tt-write-string "Normal")

	 (tt-move-to (incf line) 30)
	 (tt-write-span '((:cyan "Cyan") " "))
	 (tt-color :magenta nil) (tt-write-string "Magenta ")
	 (tt-color :default nil)
	 (tt-write-span '((:yellow "Yellow") " "))
	 (tt-write-string "Normal")

	 (incf line)
	 (tt-move-to (incf line) 30)
	 (tt-write-string "Normal")
	 (tt-forward)
	 (tt-color :red nil)
	 (tt-write-string "Red FG")
	 (tt-forward)
	 (tt-color nil :blue)
	 (tt-write-string "Red FG Blue BG")
	 (tt-forward)
	 (tt-color nil nil)
	 (tt-write-string "Both NIL")
	 (tt-forward)
	 (tt-color :default :default)
	 (tt-write-string "Both default")))))

  (blurp ()
    (let ((original-bg
	   (or (tt-window-background)
	       (prog1 :black
		 (tt-write-span
		  '((:red "WARNING") ": Can't figure out the terminal "
		    "background. Assuming it's black." #\newline
		    (:standout "--Press a key--")))
		 (tt-get-key)))))
      (unwind-protect
	   (loop
	      :for c :in *basic-colors*
	      :for tc :in *contrast-colors*
	      :do
	      (setf (tt-window-background) c)
	      (tt-home) (tt-erase-below)
	      (tt-color tc :default)
	      (center (format nil "The screen should be ~a" c) 0)
	      (tt-color :default :default)
	      (prompt-next))
	(setf (tt-window-background) original-bg)
	(tt-home) (tt-erase-below)))))

(defun test-alternate-characters ()
  (blurp ()
   (flet ((test-char (number name)
	    (tt-format "~30a: \"" name)
	    (tt-alternate-characters t)
	    (tt-write-char (code-char number))
	    (tt-alternate-characters nil)
	    (tt-write-char #\")
	    (tt-write-char #\newline)))
    (test-char #x250c "upper left corner")        ; ┌
    (test-char #x2514 "lower left corner")        ; └
    (test-char #x2510 "upper right corner")       ; ┐
    (test-char #x2518 "lower right corner")       ; ┘
    (test-char #x251c "tee pointing right")       ; ├
    (test-char #x2524 "tee pointing left")        ; ┤
    (test-char #x2534 "tee pointing up")          ; ┴
    (test-char #x252c "tee pointing down")        ; ┬
    (test-char #x2500 "horizontal line")          ; ─
    (test-char #x2502 "vertical line")            ; │
    (test-char #x253c "large plus or crossover")  ; ┼
    (test-char #x23ba "scan line 1")              ; ⎺
    (test-char #x23bd "scan line 9")              ; ⎽
    (test-char #x25c6 "diamond")                  ; ◆
    (test-char #x2592 "checker board (stipple)")  ; ▒
    (test-char #x00b0 "degree symbol")            ; °
    (test-char #x00b1 "plus/minus")               ; ±
    (test-char #x00b7 "bullet")                   ; ·
    (test-char #x2190 "arrow pointing left")      ; ←
    (test-char #x2192 "arrow pointing right")     ; →
    (test-char #x2193 "arrow pointing down")      ; ↓
    (test-char #x2191 "arrow pointing up")        ; ↑
    (test-char #x2591 "board of squares")         ; ▒
    (test-char #x240b "lantern symbol")           ; ␋
    (test-char #x2588 "solid square block")       ; █
    (test-char #x23bb "scan line 3")              ; ⎻
    (test-char #x23bc "scan line 7")              ; ⎼
    (test-char #x2264 "less/equal")               ; ≤
    (test-char #x2265 "greater/equal")            ; ≥
    (test-char #x03c0 "Pi")                       ; π
    (test-char #x2260 "not equal")                ; ≠
    (test-char #x00a3 "UK pound sign"))))	  ; £

(defun draw-box (x y width height &key string)
  "Draw a box at X Y of WIDTH and HEIGHT. STRING a string of WIDTH to use for
drawing, which will get overwritten."
  (let* ((str-len (- width 2))
	 (str (if string
		  (fill string (code-char #x2500) :end str-len)
		  (make-string str-len
			      :initial-element (code-char #x2500))))) ; ─
    (tt-alternate-characters t)
    (tt-move-to y x)
    (tt-write-char (code-char #x250c))	   ; ┌
    (tt-write-string str :end str-len)
    (tt-write-char (code-char #x2510))	   ; ┐
    (loop :for iy :from (1+ y) :below (+ y (1- height)) :do
       (tt-move-to iy x)
       (tt-write-char (code-char #x2502))  ; │
       (tt-move-to iy (+ x (1- width)))
       (tt-write-char (code-char #x2502))) ; │
    (tt-move-to (+ y (1- height)) x)
    (tt-write-char (code-char #x2514))	   ; └
    (tt-write-string str :end str-len)
    (tt-write-char (code-char #x2518))     ; ┘
    (tt-alternate-characters nil)))

(defun test-boxes ()
  "Test drawing boxes."
  (flet ((draw-boxes (#|delay|#)
	   (tt-clear) (tt-home)
	   (let ((limit (truncate (z-height) 2)))
	     (loop :for i :from 1 :below limit :by 1 :do
		(draw-box i i (- (z-width) (* 2 i)) (- (z-height) (* 2 i)))
		(tt-finish-output)
		;; (when delay
		;;   (sleep delay))
		)
	     (tt-write-string-at
	      limit limit
	      "This should be surrounded by concentric boxes."))))
    (draw-boxes #|nil .1|#)
    (prompt-next :redraw-func (lambda () (draw-boxes #|nil|#)))))

(defun test-cursor-visibility ()
  (blurp ()
   (let ((half-width (truncate (/ (z-width) 2)))
	 (half-height (truncate (/ (z-height) 2))))
     (tt-cursor-on)
     (tt-move-to (1- half-height) 0)
     (tt-format "The cursor should be visible. Press a key.")
     (tt-move-to half-height (- half-width 3))
     (tt-write-string "-->")
     (tt-move-to half-height (+ half-width 1))
     (tt-write-string "<--")
     (tt-move-to half-height half-width)
     (tt-get-key)

     (tt-move-to (1- half-height) 0)
     (tt-format "The cursor should be INVISIBLE. Press a key.")
     (tt-move-to half-height half-width)
     (tt-cursor-off)
     (tt-get-key)

     (tt-cursor-on)
     (tt-move-to (1- half-height) 0)
     (tt-format "The cursor should be visible again. Press a key.")
     (tt-move-to half-height half-width)
     (tt-get-key)
     )))

(defun test-save-and-restore-cursor ()
  (blurp ()
   (let ((half-width (truncate (/ (z-width) 2)))
	 (half-height (truncate (/ (z-height) 2)))
	 (opposite #(2 3 0 1))
	 (offs #((0 . -1) (1 . 0) (0 . 1) (-1 . 0)))
	 ;;end-row end-col
	 )
     (tt-move-to 0 0)
     (tt-write-string "At the starting point. Press a key.")
     (tt-move-to half-height half-width)
     (tt-up 2) (tt-write-char #\|)
     (tt-down) (tt-backward 2) (tt-write-string "\\|/")
     (tt-down) (tt-backward 5) (tt-write-string "--> <--")
     (tt-down) (tt-backward 5) (tt-write-string "/|\\")
     (tt-down) (tt-backward 2) (tt-write-string "|")
     (tt-move-to half-height half-width)
     (tt-save-cursor)
     (tt-get-key)
     (loop :with last-dir = 2 :and new-dir :and len
	:for i :from 1 :to 100 :do
	(loop :do (setf new-dir (random 4))
	   :while (= new-dir (aref opposite last-dir)))
	(setf last-dir new-dir
	      len 1 #|(random 5) |#)
	(tt-forward (* len (car (aref offs new-dir))))
	(tt-down    (* len (cdr (aref offs new-dir))))
	(tt-finish-output)
	(sleep .01))
     (tt-get-key)
     (tt-move-to 0 0)
     (tt-write-string "The cursor should be back to the starting point.")
     (tt-restore-cursor)
     (tt-get-key)
   )))

;; (defun test-autowrap-delay ()
;;   (blurp ()
;;    (let ((width (tt-width)))

(defun test-scrolling-basic ()
  (blurp ()
   (tt-clear) (tt-finish-output)
   (dotimes (i 5)
     (tt-format "This line should disappear ~d.~%" (1+ i)))
   (dotimes (i 5)
     (tt-format "This line should stay ~d.~%" (1+ i)))
   (tt-move-to (1- (z-height)) 0)
   (tt-scroll-down 5)
   (tt-write-string
    "You should see 5 lines that say 'stay' and none that say 'disappear'.")
   (tt-finish-output)
   (tt-get-key)
   (tt-clear)
   (tt-move-to (- (z-height) 11) 0)
   (dotimes (i 5)
     (tt-format "~%This line should stay ~d." (1+ i)))
   (dotimes (i 5)
     (tt-format "~%This line should disappear ~d." (1+ i)))
   (tt-move-to 0 0)
   (tt-scroll-up 5)
   (tt-write-string 
    "You should see 5 lines that say 'stay' and none that say 'disappear'.")
   (tt-finish-output)
   (tt-get-key)))

(defun test-scrolling-with-fixed-line ()
  (blurp (:position :center)
   (labels ((bottom-line (offset)
	      (tt-move-to (- (z-height) offset) 0)
	      (loop
		 :with blob = "{--The-Bottom-Line--}"
		 :with len = (length blob)
		 :for x :from 0 :below (z-width) :by len
		 :do
		 (tt-write-string
		  (subseq blob 0 (min len
				      (+ len (- (z-width) (+ x len))))))))
	    (show-lines (n)
	      (tt-home)
	      (tt-move-to 0 0)
	      (tt-erase-below)
	      (loop :for i :from n :downto 1 :do
		 (tt-move-to (- (z-height) i 1) 0)
		 (tt-format "-=-=- Line ~d -=-=-" (- n i)))
	      (bottom-line 1)
	      ;; (tt-get-key)
	      (tt-finish-output)
	      (sleep .2)
	      ))
     (let ((test-lines 15))
       (tt-clear)
       (tt-home)
       (tt-format "Scrolling ~d lines at the bottom.~%~
                   The bottom line should stay in place." test-lines)
       (prompt-next :message "Press any key to continue.")
       (loop :for i :from 1 :to test-lines :do
	  (show-lines i))))))

(defun test-scroll-by (n &key (reps 3))
  (with-immediate ()
    (tt-home) (tt-erase-below)
    (loop :for i :from 0 :below (z-height)
       :do (tt-format-at i 0 "-- Line ~d ~r" i i))
    (tt-get-key)
    (dotimes (j reps)
      (dotimes (i n)
	(tt-move-to (1- (z-height)) 0)
	(tt-write-char #\newline)
	(cond
	  ((= n 1)
	   (tt-write-string "< "))
	  ((= i 0)
	   (tt-write-string "/ "))
	  ((= i (1- n))
	   (tt-write-string "\\ "))
	  (t
	   (tt-write-string "| ")))
	(tt-format "~d: Scroll by ~d" (1+ i) n))
      (tt-get-key))))

(defun test-scrolling-n ()
  (blurp (:position :center)
    (tt-format
     "This tests scrolling by a number of lines at a time.~%~
      Then screen will be filled with numbered lines, then wait for you~%~
      to press a key, after which there should be lines N lines at the~%~
      bottom which say \"I: Scroll by N\" where N is the amount scrolled~%~
      and I is the number of the scrolled line.~%~%~
      Press a key to start the test.")
    (tt-get-key)
    (tt-home) (tt-erase-below)
    (loop :for n :from 1 :to 10 :do
       (test-scroll-by n))))

(defun test-scrolling-saved-clear ()
  (blurp ()
    ;; Try to put some stuff in the saved scrolled lines.
    (loop
       :with height = (or (tt-height) 24)
       :and width = (or (tt-width) 80)
       :for y :from 0 :below (* height 3)
       :do
       (tt-format "--->> You shouldn't see this line ~d" y)
       (let ((right-side (format nil " page ~d <<---" (mod y height))))
	 (tt-move-to-col (- width (length right-side) 1))
	 (tt-write-string right-side)
	 (tt-newline)))
    ;; And try to clear it.
    (tt-clear :saved-p t)
    (tt-finish-output)
    (center "The screen and the saved scrolled lines should have cleared," 0)
    (center "if the terminal supports it." 1)))

;; (defun test-scrolling-no-newline ()
;;   (blurp
;;     (loop :for i :from 0 :below (z-height)
;;        :do (tt-format-at i 0 "-- Line ~d ~r" i i))))

(defun test-scrolling-region ()
  (blurp ()
    (let ((width  (z-width))
	  (height (z-height))
	  (junk "%@#-."))
      (tt-format
       "We will now test setting the scrolling region.~@
        This should remain visible and unaffected by scrolling below.~@
        There should be some similar text at the bottom.~%~@
        Press any key to begin.")
      (tt-move-to 8 0)
      (tt-format "~v,,,'-a" width "-")
      (tt-get-key)
      (tt-move-to (- height 3) 0)
      (tt-format "~v,,,'-a~%" width "-")
      (tt-get-key)
      (tt-format "This should remain visible and unaffected.~%")
      (tt-format "There should be some similar text at the top.")
      (tt-get-key)
      (tt-set-scrolling-region 9 (- height 4))
      (tt-move-to 10 0)
      (loop :for i :from 1 :to #|200|# 80 :do
        (loop :for j :from 0 :below (1- width) :do
	  (tt-write-char (elt junk (random (length junk)))))
	(tt-write-char #\newline)
	(tt-finish-output)
	(sleep .03)
	;; (sleep .003)
	;; (tt-get-key)
	)
      (tt-set-scrolling-region nil nil)
      (tt-move-to 4 0)
      (tt-erase-line)
      (tt-write-span '("Press any key to " (:standout "continue.")))
      (tt-get-key)
      (tt-erase-line) (tt-format "We will now un-set the scrolling region.~%")
      (tt-erase-line) (tt-format
		       "This should soon disappear by scrolling off the top.~%")
      (tt-erase-line) (tt-format "~%")
      (tt-erase-line) (tt-format "~%")
      ;; (tt-move-to 8 0)
      ;; (tt-format "~v,,,'-a" width "-")
      (tt-move-to (- height 3) 0)
      ;; (tt-format "~v,,,'-a~%" width "-")
      (tt-erase-line)
      (tt-format "This should soon disappear by scrolling off the top.")
      (tt-erase-line)
      (tt-format-at (1- height) 0
		    "You should not see this when scrolling is done!.~%")
      (tt-move-to 10 0)
      (loop :with str
        :for i :from 1 :to 400 :do
	(setf str (format nil "~d" (expt 2 i)))
	(tt-write-string (subseq str 0 (min (1- width) (length str))))
	(tt-write-char #\newline)
	(tt-finish-output))
      (tt-format "The screen should have only numbers above.~%")))
  t)

(defparameter *big-down-arrow*
  #("  ||  " 
    "  ||  " 
    "\\ || /" 
    " \\||/ "
    "  \\/  "
    ))

(defparameter *big-up-arrow*
  #("  /\\  "
    " /||\\ "
    "/ ||  \\"
    "  ||  "
    "  ||  "
    ))

(defun test-insert-delete-line ()
  (blurp (:position :top)
   (let* ((half-height (round (z-height) 2))
	  ;; (half-width (round (z-width) 2))
	  (arrow-height (length *big-down-arrow*))
	  (arrow-width (display-length (elt *big-down-arrow* 0)))
	  (arrow-x (- (round (z-width) 2) arrow-width))
	  (arrow-y (- half-height arrow-height)))
     (center "The lines should be pushed off the bottom, then pulled back up."
	     (- (+ arrow-height 2)))
     ;; going down
     (tt-move-to half-height 0)
     (junk-block half-height :relative t)
     (put-at arrow-y arrow-x *big-down-arrow*)
     (tt-get-key)
     (loop :repeat (- (z-height) half-height) :do
       (tt-insert-line)
       (tt-move-to (+ arrow-y arrow-height) (+ arrow-x arrow-width))
       (tt-erase-above)
       (incf arrow-y)
       (put-at arrow-y arrow-x *big-down-arrow*)
       (tt-finish-output)
       ;; (sleep .02)
       (tt-get-key)
       )
     (tt-get-key)
     ;; coming up
     (tt-move-to (+ arrow-y arrow-height) (+ arrow-x arrow-width))
     (tt-erase-above)
     (put-at arrow-y arrow-x *big-up-arrow*)
     (loop :repeat (- (z-height) half-height) :do
       ;; (tt-move-to half-height half-width)
       (tt-move-to (- half-height arrow-height) 0)
       (tt-delete-line)
       (decf arrow-y)
       ;; (tt-move-to (+ arrow-y arrow-height) (+ arrow-x arrow-width))
       ;; (put-at arrow-y arrow-x *big-up-arrow*)
       (tt-move-to-row (1- (z-height)))
       (junk-block 1 :relative t)
       (tt-finish-output)
       ;; (sleep .02)
       (tt-get-key)
       ))))

(defun junk-block (height &key relative)
  (let ((junk "#%_.")
	#| (half (/ (terminal-window-columns *terminal*) 2)) |#
	(full (z-width)))
    (loop :for i :from 0 :below height :do
       (when (and relative (not (zerop i)))
	 (tt-newline))
       (loop :for j :from 0 :below (1- full) :do
	  (cond
	    (relative
	     (tt-move-to-col j)
	     (tt-write-char (elt junk (random (length junk)))))
	    (t
	     (tt-move-to i j)
	     (tt-write-char (elt junk (random (length junk))))))))))

(defun test-basics ()
  (blurp ()
   (tt-format "The screen should have cleared.~%"))

  (blurp ()
   (tt-write-string "You should see this sentence.")
   (tt-write-char #\newline)
   (tt-write-string "This should be on another line."))

  (blurp ()
   (let ((size 10))
     (loop :for i :from 0 :below size
	:do
	(tt-move-to i i)
	(tt-write-char #\X))
     (loop :for i :from 0 :below size
	:do
	(tt-move-to i (- size i 1))
	(tt-write-char #\X))
     (tt-move-to size 0)
     (tt-write-string "You should see an X of X's.")))

  (blurp ()
   (let ((height 8)
	 (half (truncate (z-width) 2)))
     (junk-block height)
     (loop :for i :from 0 :below height :do
	(tt-move-to i half)
	(tt-erase-to-eol))
     (tt-move-to height half)
     (tt-write-string "Above here should be blank.")))

  (blurp ()
   (let ((height 8)
	 (half (truncate (z-width) 2)))
     (junk-block height)
     (loop :for i :from 0 :below height :do
	(tt-move-to i half)
	(tt-erase-to-bol))
     (tt-move-to height 0)
     (tt-write-string "Above here should be blank.")))

  (blurp ()
   (let ((height 8)
	 (half (truncate (z-width) 2)))
     (junk-block height)
     (loop :for i :from 0 :below height :do
	(tt-move-to i half)
	(tt-erase-line))
     (tt-move-to height 0)
     (tt-write-string "Everything above here should be blank.")))

  (blurp ()
   (let ((height 8)
	 (half (truncate (z-width) 2)))
     (junk-block height)
     (tt-move-to (truncate height 2) half)
     (tt-erase-above)
     (tt-write-string "Above and left of here should be blank.")))

  (blurp ()
   (let ((height 8))
     (junk-block height)
     (tt-move-to (truncate height 2) 0)
     (tt-write-string "Below and right of here should be blank.")
     (tt-erase-below)))

  (with-immediate ()
    (tt-clear) (tt-home)
    (tt-move-to 10 0)
    (tt-write-string "The cursor should be at the start of the next line.")
    (tt-newline)
    (tt-get-key))

  (with-immediate ()
    (tt-clear) (tt-home)
    (tt-move-to 10 0)
    (tt-write-string "The cursor should STILL be at the start of the next line.")
    (tt-fresh-line)
    (tt-get-key))

  (with-immediate ()
    (tt-clear) (tt-home)
    (tt-move-to 10 0)
    (tt-write-string "This is on one line.") (tt-newline)
    (tt-write-string "This is on another line.") (tt-fresh-line)
    (tt-write-string "The cursor should be at the start of the next line.")
    (tt-fresh-line)
    (tt-fresh-line)
    (tt-get-key))

  (blurp ()
   (tt-format "I am very sorry, but we will now try to BEEP.~%")
   (tt-format "You should hear a sound or see some visual indication.~%")
   (tt-beep))
  )

(defparameter *block*
  '("░░░░░░░░"
    "░▒▒▒▒▒▒░"
    "░▒▓▓▓▓▒░"
    "░▒▓██▓▒░"
    "░▒▓▓▓▓▒░"
    "░▒▒▒▒▒▒░"
    "░░░░░░░░"))

(defun draw-block (y x)
  (tt-move-to y x)
  (loop :with i = y
     :for line :in *block* :do
     (tt-write-string line)
     (tt-move-to (incf i) x)))

(defstruct blook
  color
  x y
  xinc yinc)

(defun show-blook (b)
  (tt-color (blook-color b) :default)
  (draw-block (blook-y b) (blook-x b))
  (tt-color :default :default))

;; I know this isn't really a very good test, but at least it's a little fun.
(defun test-wide-characters ()
  (tt-clear)
  (let ((blocks
	 (list
	  (make-blook :color :red    :y 10 :x 20 :xinc  1 :yinc 1)
	  (make-blook :color :blue   :y 10 :x 30 :xinc  1 :yinc 0)
	  (make-blook :color :green  :y 20 :x 20 :xinc -1 :yinc 0)
	  (make-blook :color :yellow :y 20 :x 40 :xinc -1 :yinc -1)))
	(t-o .05)
	xi yi c)
    (labels
	((random-color ()
	   (elt *simple-colors*
		(random (length *simple-colors*))))
	 (new-blook ()
	   (loop :do (setf xi (- (random 3) 1) yi (- (random 3) 1))
	      :while (and (= xi 0) (= yi 0)))
	   (push (make-blook :color (random-color)
			     :y (random (z-height)) :xinc xi
			     :x (random (z-width))  :yinc yi) blocks)))
      (loop ;; :for i :from 1 :to 2000 :do
	 (tt-move-to 0 0)
	 (tt-erase-below)
	 (center "Ｙｅｓ， ｙｏｕ ｈａｖｅ ＷＩＤＥ ｃｈａｒｓ！" 0)
	 (center "You should see rectangles bouncing around." 1)
	 (center "Press 'n' to add more blocks, 'q' to quit." 2)
	 (loop :for b :in blocks :do
	    (show-blook b)
	    (incf (blook-x b) (blook-xinc b))
	    (incf (blook-y b) (blook-yinc b))
	    (when (<= (blook-x b) 0)		    (setf (blook-xinc b) 1))
	    (when (>= (blook-x b) (- (z-width) 8))  (setf (blook-xinc b) -1))
	    (when (<= (blook-y b) 0)	    	    (setf (blook-yinc b) 1))
	    (when (>= (blook-y b) (- (z-height) 7)) (setf (blook-yinc b) -1)))
	 (tt-finish-output)

	 (when (setf c (if (plusp t-o)
			   (when (tt-listen-for t-o)
			     (tt-get-key))
			   (tt-get-key)))
	   (case c
	     (#\q (return))
	     (#\n (new-blook))
	     (#\p (setf t-o (- t-o)))
	     (#\- (decf t-o .01))
	     (#\+ (incf t-o .01))))))))

(defun show-real-pallet ()
  "Show the terminal's real pallet directly."
  (loop :for i :from 0 :below 256 :do
    (write-string (format nil "~c[48;5;~dm  " #\esc i))
    (case i
      ((7 15 231) (write-char #\newline)))
    (cond
      ((zerop (mod (- 15 i) 36))
       (write-char #\newline))))
  (write-char #\newline)
  (write-string (format nil "~c[0m " #\esc))
  (finish-output))

(defun dump-colors ()
  "Dump the colors from the *xterm-256-color-table*."
  (loop :for i :from 0 :below 256 :do
     (tt-color :default (aref fatchar::*xterm-256-color-table* i))
     (tt-write-string "  ")
     (case i
       ((7 15 231) (tt-write-char #\newline)))
     (cond
       ((zerop (mod (- 15 i) 36))
	(tt-write-char #\newline))))
  (tt-write-char #\newline)
  (tt-normal)
  (tt-finish-output))

(defun base-colors ()
  "Output the base 16 system colors."
  (loop :for c :in *basic-colors*
     :do
     (tt-color :black c)
     (tt-write-string "  "))
  (tt-write-char #\newline)
  (tt-bold t)
  (tt-inverse t)
  (loop :for c :in *basic-colors*
     :do
     (tt-color c :black)
     (tt-write-string "  "))
  (tt-normal))

(defun color-cube (n func)
  "Output an N * N * N color cube, defined by FUNC."
  (let (r-val g-val b-val)
    (tt-format "~%~%~d * ~:*~d * ~:*~d cube:~%" n)
    (loop :for r :from 0 :below n :do
       (setf r-val (funcall func r))
       (loop :for g :from 0 :below n :do
	  (setf g-val (funcall func g))
	  (loop :for b :from 0 :below n :do
	     (setf b-val (funcall func b))
	     (tt-color :default (make-color :rgb8
					    :red   r-val
					    :green g-val
					    :blue  b-val))
	     (tt-write-string "  ")))
       (tt-write-char #\newline))
    (tt-normal)))

(defun gray-ramp (n func)
  "Output a gray ramp of length N, defined by FUNC."
  (tt-format "~%~d gray ramp:~%" n)
  (loop :with value
     :for g :from 0 :below n :do
     (setf value (funcall func g))
     (tt-color :default (make-color :rgb8
				    :red   value
				    :green value
				    :blue  value))
     (tt-write-string "  "))
  (tt-write-char #\newline))

(defun test-pallet-colors-88 ()
  "Test to see if the terminal can handle 88 pallet colors."
  (tt-format "88 colors~%~%16 base colors:~%")
  (base-colors)
  (color-cube 4 (_ (aref #(0 139 205 255) _)))
  (gray-ramp 8 (_ (+ (* _ 23) 46)))
  (tt-normal)
  (tt-write-char #\newline))

(defun test-pallet-colors-256 ()
  "Test to see if the terminal can handle 256 pallet colors."
  (tt-format "256 colors~%~%16 base colors:~%")
  (base-colors)
  (color-cube 6 (_ (if (zerop _) 0 (+ 55 (* 40 _)))))
  (gray-ramp 24 (_ (+ (* _ 10) 8)))
  (tt-normal)
  (tt-write-char #\newline))

(defun test-pallet-colors ()
  (blurp () (test-pallet-colors-88))
  (blurp () (test-pallet-colors-256)))

(defun draw-rgb-colors ()
  "Test to see if the terminal can handle a lot of RGB colors."
  (let* ((rows (- (z-height) 1))
	 (cols (z-width))
	 (blue-step (/ 255.0 rows))
	 (red-green-step (/ 255.0 cols))
	 (r 0.0) (g 0.0) (b 0.0))
    (tt-clear)
    (tt-home)
    (loop :for row :from 0 :below rows
       :do
       (setf r 0 g 255)
       (loop :for col :from 0 :below cols :do
	  (tt-color :default (make-color :rgb8
					 :red (truncate r)
					 :blue (truncate b)
					 :green (truncate g)))
	  (tt-write-char #\space)
	  (incf r red-green-step)
	  (decf g red-green-step))
       (incf b blue-step)))
  (tt-normal)
  (center "You should see colors smoothly blended from the screen corners." 0)
  (center "Upper left: green, Upper right: red" 1)
  (center "Lower left: cyan, Lower right: magenta" 2))

(defun test-rgb-colors ()
  (draw-rgb-colors)
  (prompt-next :message nil :redraw-func #'draw-rgb-colors))

(defun test-random-colors ()
  (blurp ()
    (loop
      :with i = 0
      :while (and (or (not (tt-listen-for 0))
		      (not (equalp #\q (tt-get-key))))
		  (< i 100000))
      :do
      (tt-move-to (random (tt-height)) (random (tt-width)))
      (tt-write-span `(:bg :color
                           #(:rgb8 ,(random 255) ,(random 255) ,(random 255))
                           #\space))
      (center "Press Q to quit." 1)
      (incf i))))

(defun test-mouse ()
  (tt-home)
  (tt-clear)
  (tt-write-line "Try clicking, dragging, and scrolling with the mouse.")
  (tt-write-line "'m' - Toggle motion events")
  (tt-write-line "'c' - Clear the screen")
  (tt-write-line "'q' - Quit")
  (flet ((clear-modeline ()
	   (tt-move-to (1- (z-height)) 0)
	   (tt-color :default :default)
	   (tt-erase-to-eol))
	 (write-modeline (e)
	   (tt-move-to (1- (z-height)) 0)
	   (tt-color :default :default)
	   (tt-erase-to-eol)
	   (tt-format "~s" e)))
    (let ((events-type :mouse-buttons))
    (unwind-protect
	 (with-immediate ()
	   (tt-enable-events events-type)
	   (loop :with e :and color = :default :and quit-flag
	      :while (not quit-flag)
	      :do
	      (setf e (tt-get-key))
	      (clear-modeline)
	      (typecase e
		(tt-mouse-button-event
		 (let ((x (tt-mouse-event-x e))
		       (y (tt-mouse-event-y e))
		       (button (tt-mouse-button e)))
		   (setf color (case button
				 (:button-1 :red)
				 (:button-2 :green)
				 (:button-3 :blue)
				 (:release :default)))
		   (case button
		     (:button-5
		      (tt-move-to (1- (z-height)) 0)
		      (tt-scroll-down 2)
		      (write-modeline e))
		     (:button-4
		      (tt-move-to 0 0)
		      (tt-scroll-up 2)
		      (write-modeline e))
		     (otherwise
		      (write-modeline e)
		      (tt-move-to y x)
		      (tt-color color :default)
		      (tt-write-char #\X)))))
		(tt-mouse-motion
		 (let ((x (tt-mouse-event-x e))
		       (y (tt-mouse-event-y e)))
		   (write-modeline e)
		   (tt-move-to y x)
		   (tt-color :white :default)
		   (tt-write-char #\.)))
		(character
		 (cond
		   ((or (equal e #\q) (equal e #\Q))
		    (setf quit-flag t))
		   ((eql e #\m)
		    (tt-disable-events events-type)
		    (setf events-type
			  (if (eq events-type :mouse-buttons)
			      :mouse-motion
			      :mouse-buttons))
		    (tt-enable-events events-type))
		   ((eql e #\c)
		    (tt-clear)))))
	      (tt-finish-output)))
      ;; disable both just in case
      (tt-disable-events :mouse-motion)
      (tt-disable-events :mouse-buttons)))))

(defun test-input ()
  (tt-home)
  (tt-clear)
  (tt-write-line "Try pressing keys. They should be printed.")
  (tt-write-line "Press the <ESC> key four times in a row to exit.")
  (let ((saved-events (terminal-events-enabled *terminal*)))
    (unwind-protect
	 (with-immediate ()
	   (tt-enable-events '(:mouse-buttons #|:mouse-motion|# :resize))
	   (loop :with escape-count = 0
	      :and c
	      :do
	      (setf c (tt-get-key))
	      (typecase c
		(character
		 (tt-format "~a ~s ~a ~x ~a~%" (char-util:displayable-char c)
			    c (char-code c) (char-code c) (char-name c)))
		(tt-event
		 (tt-format "~s~%" c))
		(t
		 (tt-format "~a ~s ~a~%" c c (type-of c))))
	      (if (or (eql c #\escape) (eq c :escape))
		  (incf escape-count)
		  (setf escape-count 0))
	      :while (< escape-count 4)))
      (tt-disable-events '(:mouse-buttons :mouse-motion :resize))
      (tt-enable-events saved-events))))

(defun test-events ()
  (format t "~&before: ~s" (terminal-events-enabled *terminal*))
  (with-immediate ()
    (with-enabled-events ('(:mouse-buttons))
      (format t "~&during: ~s" (terminal-events-enabled *terminal*))
      (print (tt-get-key))))
  (format t "~&after: ~s" (terminal-events-enabled *terminal*)))

(defun test-autowrap ()
  (blurp ()
    (let ((width (z-width))
	  before-row before-col after-row after-col)
      (tt-move-to (round (z-height) 2) 0)
      (loop :repeat width :do
	 (tt-write-char #\-))
      (tt-finish-output)
      (setf (values before-row before-col)
	    (terminal-get-cursor-position *terminal*))
      (tt-write-char #\X)
      (tt-finish-output)
      (setf (values after-row after-col)
	    (terminal-get-cursor-position *terminal*))
      (tt-fresh-line)
      (tt-format "before ~s ~s~%after  ~s ~s~%"
		 before-row before-col after-row after-col))))

(defun test-autowrap-2 ()
  (with-immediate ()
    (format t "~a" #\░)         (finish-output) (tt-get-key)
    (format t "~v,,,'xa" 79 "") (finish-output) (tt-get-key)
    (format t "~c" #\return)    (finish-output) (tt-get-key)
    (format t "x")              (finish-output) (tt-get-key)
    (format t "~c" #\return)    (finish-output) (tt-get-key)))

;; @@@ how about a test of mouse motion only events?

(defstruct menu
  name
  items)

(defparameter *menu*
  (make-menu
   :items
   `(("Screen size"                   . test-screen-size)
     ("Basic functionality"           . test-basics)
     ("Box drawing"                   . test-boxes)
     ("Cursor visibility"             . test-cursor-visibility)
     ("Cursor save and restore"       . test-save-and-restore-cursor)
     ("Column movement"               . test-move-to-col)
     ("Insert & delete characters"    . test-ins-del)
     ("Text attributes"               . test-attrs)
     ("Text colors"                   . test-colors)
     ("Alternate characters"          . test-alternate-characters)
     ("Wide characters"               . test-wide-characters)
     ("Pallet colors"                 . test-pallet-colors)
     ("RGB 24-bit colors"             . test-rgb-colors)
     ("Random Colors"                 . test-random-colors)
     ("Mouse events"                  . test-mouse)
     ("Input and Keys"                . test-input)
     ("Autowrap"		      . test-autowrap)
     ("Scrolling menu"		      . *scrolling-menu*)
     )))

(defparameter *scrolling-menu*
  (make-menu :name "Scrolling"
   :items
   `(("Basic scrolling"		      . test-scrolling-basic)
     ("Scrolling with a fixed line"   . test-scrolling-with-fixed-line)
     ("Scrolling by various amounts"  . test-scrolling-n)
;;     ("Scrolling without newlines"    . test-scrolling-no-newline)
     ("Saved scrolled lines clear"    . test-scrolling-saved-clear)
     ("Scrolling region"              . test-scrolling-region)
     ("Insert/Delete line"            . test-insert-delete-line)
     )))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-maybe-new-terminal ((class no-new) &body body)
    "Evaluate the BODY but try not to make a new terminal if the class is the
same as the current and NO-NEW is true."
    (with-unique-names (thunk class-value)
      `(let ((,class-value ,class))
	 (flet ((,thunk () ,@body))
	   (when ,class-value
	     (if (and (typep *terminal*
			     (find-terminal-class-for-type ,class-value))
		      ,no-new)
		 (with-new-terminal (,class-value)
		   (,thunk))
		 (with-terminal (,class-value)
		   (,thunk)))))))))

(defun run-menu (menu)
  "Run every test in the menu, recursing into sub-menus."
  (loop :with action
     :for item :in (menu-items menu) :do
     (setf action (cdr item))
     (cond
       ((fboundp action) (funcall action))
       (t (run-menu (symbol-value action))))))

(defun run (&key type no-new)
  "Run all the tests."
  (let ((class (or type (ask-class))))
    (with-maybe-new-terminal (class no-new)
      (with-simple-restart (quit "Quit testing the terminal")
	(terminal-get-size *terminal*)
	(run-menu *menu*))
      (format t "~%All done.~%"))))

(defun show-menu (menu)
  "Display the MENU with keys for invoking the items."
  (with-slots (name items) menu
    (tt-home)
    (tt-erase-below)
    (tt-format " Terminal Tests (~a)~@[ ~a~]~%~%"
	       (class-name (class-of *terminal*)) name)
    (loop
       :for i :from 1
       :for (item-name . nil) :in items :do
       (tt-format "  [~c]  ~a~%" (char-downcase
				  (digit-char i (1+ (length items))))
		  item-name))
    (tt-format "~%")
    (when (find-restart 'back)
      (tt-format "  [,]  Back to previous menu~%"))
    (tt-format "  [^L] Redraw the screen~%")
    (tt-format "  [q]  Quit~%")))

(defun do-menu-item (item)
  "Either call the function or enter the menu bound to the symbol ITEM."
  (check-type item symbol)
  (cond
    ((fboundp item) (funcall item))
    (t
     (with-simple-restart (back "Back to previous menu")
       (menu-loop (symbol-value item))))))

(defun menu-loop (menu)
  "Menu event loop."
  (loop :with c :and num
     :do
     (terminal-get-size *terminal*)
     (show-menu menu)
     (tt-finish-output)
     (setf c (tt-get-key))
     (cond
       ((and (characterp c)
	     (setf num (digit-char-p c (1+ (length (menu-items menu)))))
	     (not (zerop num)))
	(do-menu-item (cdr (nth (1- num) (menu-items menu)))))
       ((eql #\q c)
	(invoke-restart (find-restart 'quit)))
       ((eql #\, c)
	(let ((r (find-restart 'back)))
	  (when r
	    (invoke-restart r))))
       ((eql #\page c)
	(tt-clear)))))

(defun menu (&key type no-new)
  "Allow the user to pick which tests to run from menus."
  (let ((class (or type (ask-class))))
    (with-maybe-new-terminal (class no-new)
      (with-simple-restart (quit "Quit testing the terminal")
	(terminal-get-size *terminal*)
	(when (or (< (z-width) 80)
		  (< (z-height) 24))
	  (tt-format "Your terminal is less than 80x24 characters.~%~
                        You might have trouble.")
	  (prompt-next))
	(menu-loop *menu*)))))

(defun churn (&optional (class :ansi))
  (when (or class #| (ask-class) |#)
    (with-simple-restart (quit "Quit testing the terminal")
      (with-new-terminal (class)
	(terminal-get-size *terminal*)
	;;(test-basics)
	(loop
	  :do
	     (asdf:load-system :terminal-test)
	  :while (test-scrolling-region)))
      ;; (format t "~%All done.~%")
      )))

#+lish
(lish:defcommand terminal-test
  ((all boolean :short-arg #\a :help "True to run all tests.")
   (no-new boolean :short-arg #\n
    :help "True not to make a new terminal if the type is the same.")
   (type lenient-choice
    :short-arg #\t
    :default '(find-terminal-type-for-class (class-name (class-of *terminal*)))
    :choice-func #'terminal-types
    :choice-labels `(loop :with x = terminal:*terminal-types*
		       :while x :collect (string-downcase (car x))
		       :do (setf x (cddr x)))
    :help "Terminal type to test."))
  "Test terminal functionality."
  ;; (when (not type)
  ;;   (setf type (find-terminal-type-for-class (class-of *terminal*))))
  (if all
      (run :type type :no-new no-new)
      (menu :type type :no-new no-new)))

;; EOF
