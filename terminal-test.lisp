;;
;; terminal-test.lisp - Test the generic terminal library.
;;

(defpackage :terminal-test
  (:documentation "Test the generic terminal library.")
  (:use :cl :dlib :terminal :color :char-util)
  (:export
   #:run
   #:menu
   #:test
   #:churn
   ))
(in-package :terminal-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

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

(defun prompt-next ()
  (tt-move-to (1- (terminal-window-rows *terminal*)) 0)
  (tt-format "Press Q to quit, anything else to continue.")
  (tt-finish-output)
  (case (tt-get-key)
    ((#\Q #\q)
     (throw 'quit nil))))

(defmacro blurp (&body body)
  `(progn
    (tt-clear) (tt-home)
    ,@body
    (prompt-next)))

(defun center (text offset)
  (tt-move-to (+ (truncate (terminal-window-rows *terminal*) 2)
		 offset)
	      (- (truncate (terminal-window-columns *terminal*) 2)
		 (truncate (display-length text) 2)))
  (tt-format text))

(defun test-screen-size-draw ()
  (tt-clear) (tt-home)
  ;; upper left
  (tt-format "X <---~%^~%|~%|")
  ;;(case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; lower left
  (tt-move-to (- (terminal-window-rows *terminal*) 4) 0)
  (tt-format "|~%|~%v~%X <---")
  ;;(case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; upper right
  (tt-move-to 0 (- (terminal-window-columns *terminal*) 6))
  (tt-format "---> X")
  (tt-move-to 1 (- (terminal-window-columns *terminal*) 1)) (tt-format "^")
  (tt-move-to 2 (- (terminal-window-columns *terminal*) 1)) (tt-format "|")
  (tt-move-to 3 (- (terminal-window-columns *terminal*) 1)) (tt-format "|")
  ;;(case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; lower right
  (tt-move-to (- (terminal-window-rows *terminal*) 4)
	      (- (terminal-window-columns *terminal*) 1))
  (tt-format "|")
  (tt-move-to (- (terminal-window-rows *terminal*) 3)
	      (- (terminal-window-columns *terminal*) 1))
  (tt-format "|")
  (tt-move-to (- (terminal-window-rows *terminal*) 2)
	      (- (terminal-window-columns *terminal*) 1))
  (tt-format "V")
  (tt-move-to (- (terminal-window-rows *terminal*) 1)
	      (- (terminal-window-columns *terminal*) 6))
  (tt-format "---> X")
  ;;(case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; message
  (center "There should be an 'X' in every corner of the screen.~%" 0)
  (center "Press Q to quit, anything else to continue." 1)
  (center "Also try resizing the window." 2)
  (tt-finish-output))

(defun test-screen-size ()
  (test-screen-size-draw)
  (unwind-protect
       (progn
	 (tt-enable-events :resize)
	 (loop :while (case (tt-get-key)
			((#\Q #\q)
			 (throw 'quit nil))
			(:resize t)
			(otherwise nil))
	    :do (test-screen-size-draw)))
    (tt-enable-events :none)))

(defun test-move-to-col ()
  (blurp
   (loop :for i :from -4 :to 4 :do
      (loop :for j :from 1 :below (1- (terminal-window-columns *terminal*)) :do
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
   (tt-write-string "You should see a waveform.")
   (tt-write-char #\newline)
   (tt-write-string "The top should be cyan and the bottom should be magenta.")
   ))

(defun test-ins-del ()
  (blurp
   (let* ( #| (half (/ (terminal-window-columns *terminal*) 2)) |#
	  (full (terminal-window-columns *terminal*))
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

(defun test-attrs ()
  (blurp
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
	     (tt-write-string "Blargity  ")
	     (zerp b nil)
	     (zerp a nil)
	     )
	  (tt-write-char #\newline)))
     (tt-format "~%You should see a table of combined attributes."))))

(defun test-colors ()
  (blurp
   (let ((colors '(:default :black :red :green :yellow :blue :magenta :cyan
		   :white))
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
       (tt-normal)))))

(defun test-alternate-characters ()
  (blurp
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

(defun test-boxes ()
  "Test drawing boxes."
  (blurp
   (let ((limit (truncate (tt-height) 2)))
     (loop :for i :from 1 :below limit :by 1 :do
	(draw-box i i (- (tt-width) (* 2 i)) (- (tt-height) (* 2 i)))
	(tt-finish-output)
	(sleep .1))
     (tt-write-string-at limit limit
			 "This should be surrounded by concentric boxes."))))

(defun test-cursor-visibility ()
  (blurp
   (let ((half-width (truncate (/ (terminal-window-columns *terminal*) 2)))
	 (half-height (truncate (/ (terminal-window-rows *terminal*) 2))))
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
  (blurp
   (let ((half-width (truncate (/ (terminal-window-columns *terminal*) 2)))
	 (half-height (truncate (/ (terminal-window-rows *terminal*) 2)))
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
;;   (blurp
;;    (let ((width (tt-width)))

(defun test-scrolling ()
  (blurp
   (tt-clear) (tt-finish-output)
   (dotimes (i 5)
     (tt-format "This line should disappear ~d.~%" (1+ i)))
   (dotimes (i 5)
     (tt-format "This line should stay ~d.~%" (1+ i)))
   (tt-move-to (1- (tt-height)) 0)
   (tt-scroll-down 5)
   (tt-write-string
    "You should see 5 lines that say 'stay' and none that say 'disappear'.")
   (tt-finish-output)
   (tt-get-key)
   (tt-clear)
   (tt-move-to (- (tt-height) 11) 0)
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

(defun test-scrolling-region ()
  (blurp
   (let ((width  (terminal-window-columns *terminal*))
	 (height (terminal-window-rows *terminal*))
	 (junk "%@#-."))
     (tt-format "We will now test setting the scrolling region.~%")
     (tt-format "This should be visible and unaffected by scrolling below.~%")
     (tt-format "There should be some similar text at the bottom.~%")
     (tt-move-to 8 0)
     (tt-format "~v,,,'-a" width "-")
     (tt-move-to (- height 3) 0)
     (tt-format "~v,,,'-a~%" width "-")
     (tt-format "This should be visible and unaffected.~%")
     (tt-format "There should be some similar text at the top.~%")
     (tt-set-scrolling-region 9 (- height 4))
     (tt-move-to 10 0)
     (loop :for i :from 1 :to 200 :do
       (loop :for j :from 0 :below (1- width) :do
	  (tt-write-char (elt junk (random (length junk)))))
	(tt-write-char #\newline)
	(tt-finish-output)
	(sleep .03))
     (tt-set-scrolling-region nil nil)
     (tt-move-to 0 0)
     (tt-format "We will now un-set the scrolling region.~%")
     (tt-format "This should disappear by scrolling off the top.~%")
     (tt-move-to 8 0)
     (tt-format "~v,,,'-a" width "-")
     (tt-move-to (- height 3) 0)
     (tt-format "~v,,,'-a~%" width "-")
     (tt-format "This should disappear by scrolling off the top.~%")
     (tt-format "You should not see this when scrolling is done!.~%")
     (tt-move-to 10 0)
     (loop :with str
	:for i :from 1 :to 400 :do
	(setf str (format nil "~d" (expt 2 i)))
	(tt-write-string (subseq str 0 (min (1- width) (length str))))
	(tt-write-char #\newline)
	(tt-finish-output))
     (tt-format "The screen should have only numbers above.~%"))))

(defun junk-block (height)
  (let ((junk "#%_.")
	#| (half (/ (terminal-window-columns *terminal*) 2)) |#
	(full (terminal-window-columns *terminal*)))
    (loop :for i :from 0 :below height :do
       (loop :for j :from 0 :below (1- full) :do
	  (tt-move-to i j)
	  (tt-write-char (elt junk (random (length junk))))))))

(defun test-basics ()
  (blurp
   (tt-format "The screen should have cleared.~%"))


  (blurp
   (tt-write-string "You should see this sentence.")
   (tt-write-char #\newline)
   (tt-write-string "This should be on another line."))

  (blurp
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

  (blurp
   (let ((height 8)
	 (half (truncate (terminal-window-columns *terminal*) 2)))
     (junk-block height)
     (loop :for i :from 0 :below height :do
	(tt-move-to i half)
	(tt-erase-to-eol))
     (tt-move-to height half)
     (tt-write-string "Above here should be blank.")))

  (blurp
   (let ((height 8)
	 (half (truncate (terminal-window-columns *terminal*) 2)))
     (junk-block height)
     (loop :for i :from 0 :below height :do
	(tt-move-to i half)
	(tt-erase-line))
     (tt-move-to height 0)
     (tt-write-string "Everything above here should be blank.")))

  (blurp
   (let ((height 8)
	 (half (truncate (terminal-window-columns *terminal*) 2)))
     (junk-block height)
     (tt-move-to (truncate height 2) half)
     (tt-erase-above)
     (tt-write-string "Above and left of here should be blank.")))

  (blurp
   (let ((height 8))
     (junk-block height)
     (tt-move-to (truncate height 2) 0)
     (tt-write-string "Below and right of here should be blank.")
     (tt-erase-below)))

  (blurp
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
			     :y (random (tt-height)) :xinc xi
			     :x (random (tt-width))  :yinc yi) blocks)))
      (loop ;; :for i :from 1 :to 2000 :do
	 (tt-move-to 0 0)
	 (tt-erase-below)
	 (center "Ｙｅｓ， ｙｏｕ ｈａｖｅ ＷＩＤＥ ｃｈａｒｓ！" 0)
	 (center "You should see rectangles bouncing around." 1)
	 ;; (tt-write-string "Ｙｅｓ， ｙｏｕ ｈａｖｅ ＷＩＤＥ ｃｈａｒｓ！")
	 ;; (tt-write-string-at 1 0 "You should see rectangles bouncing around.")
	 (loop :for b :in blocks :do
	    (show-blook b)
	    (incf (blook-x b) (blook-xinc b))
	    (incf (blook-y b) (blook-yinc b))
	    (when (<= (blook-x b) 0)		     (setf (blook-xinc b) 1))
	    (when (>= (blook-x b) (- (tt-width) 8))  (setf (blook-xinc b) -1))
	    (when (<= (blook-y b) 0)	    	     (setf (blook-yinc b) 1))
	    (when (>= (blook-y b) (- (tt-height) 7)) (setf (blook-yinc b) -1)))
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

(defun test-rgb-colors ()
  "Test to see if the terminal can handle a lot of RGB colors."
  (let* ((rows (- (tt-height) 1))
	 (cols (tt-width))
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
  (tt-finish-output)
  (tt-get-key))

(defun test-mouse ()
  (tt-home)
  (tt-clear)
  (tt-write-line "Try clicking, dragging, and scrolling with the mouse.")
  (tt-write-line "Press 'q' to quit.")
  (flet ((clear-modeline ()
	   (tt-move-to (1- (tt-height)) 0)
	   (tt-color :default :default)
	   (tt-erase-to-eol))
	 (write-modeline (e)
	   (tt-move-to (1- (tt-height)) 0)
	   (tt-color :default :default)
	   (tt-erase-to-eol)
	   (tt-format "~s" e)))
    (unwind-protect
	 (with-immediate ()
	   (tt-enable-events :mouse-buttons)
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
		      (tt-move-to (1- (tt-height)) 0)
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
		(character
		 (cond
		   ((or (equal e #\q) (equal e #\Q))
		    (setf quit-flag t))
		   ((eql e #\c)
		    (tt-clear)))))
	      (tt-finish-output)))
      (tt-disable-events :mouse-buttons))))

;; @@@ how about a test of mouse motion only events?

(defparameter *menu*
  `(("Screen size"                 . test-screen-size)
    ("Basic functionality"         . test-basics)
    ("Box drawing"                 . test-boxes)
    ("Cursor visibility"           . test-cursor-visibility)
    ("Cursor save and restore"     . test-save-and-restore-cursor)
    ("Column movement"             . test-move-to-col)
    ("Insert & delete characters"  . test-ins-del)
    ("Text attributes"             . test-attrs)
    ("Text colors"                 . test-colors)
    ("Alternate characters"        . test-alternate-characters)
    ("Wide characters"             . test-wide-characters)
    ("RGB 24-bit colors"           . test-rgb-colors)
    ("Mouse events"                . test-mouse)
    ("Scrolling"		   . test-scrolling)
    ("Scrolling region"            . test-scrolling-region)
    ))

(defun run (&optional terminal-type)
  (let ((class (or terminal-type (ask-class))))
    (when class
      (with-new-terminal (class)
	(catch 'quit
	  (terminal-get-size *terminal*)
	  (loop :for item :in *menu*
	     :do
	     (funcall (cdr item)))))
      (format t "~%All done.~%"))))

(defun show-menu ()
  (tt-home)
  (tt-erase-below)
  (tt-format " Terminal Tests (~a)~%~%" (class-name (class-of *terminal*)))
  (loop
     :for i :from 1
     :for (name . nil) :in *menu* :do
     (tt-format "  [~c]  ~a~%" (char-downcase
				(digit-char i (1+ (length *menu*)))) name))
  (tt-format "~%  [q]  Quit~%"))

(defun menu (&optional terminal-type)
  (let ((class (or terminal-type (ask-class))))
    (when class
      (with-new-terminal (class)
	(catch 'quit
	  (terminal-get-size *terminal*)
	  (loop :with c :and quit-flag :and num
	     :while (not quit-flag)
	     :do
	     (show-menu)
	     (tt-finish-output)
	     (setf c (tt-get-key))
	     (cond
	       ((and (setf num (digit-char-p c (1+ (length *menu*))))
		     (not (zerop num)))
		(funcall (cdr (nth (1- num) *menu*))))
	       ((eql #\q c)
		(setf quit-flag t)))))))))

(defun churn ()
  (let ((class :ansi #| (ask-class) |# ))
    (when class
      (with-new-terminal (class)
	(terminal-get-size *terminal*)
	;;(test-basics)
	(loop
	   :do
	   (asdf:load-system :terminal-test)
	   :while (test-scrolling-region)))
      (format t "~%All done.~%"))))

#+lish
(lish:defcommand terminal-test
  ((all boolean :short-arg #\a :help "True to run all tests.")
   (type lenient-choice
    :short-arg #\t
    :default '(find-terminal-type-for-class (class-name (class-of *terminal*)))
    :choices (loop :with x = terminal:*terminal-types*
		:while x :collect (car x) :do (setf x (cddr x)))
    :choice-labels `(loop :with x = terminal:*terminal-types*
		       :while x :collect (string-downcase (car x))
		       :do (setf x (cddr x)))
    :help "Terminal type to test."))
  "Test terminal functionality."
  ;; (when (not type)
  ;;   (setf type (find-terminal-type-for-class (class-of *terminal*))))
  (if all
      (run type)
      (menu type)))

;; EOF
