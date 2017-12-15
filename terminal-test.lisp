;;
;; terminal-test.lisp - Test the generic terminal library.
;;

(defpackage :terminal-test
  (:documentation "Test the generic terminal library.")
  (:use :cl :dlib :terminal #| :terminal-ansi :terminal-curses |#)
  (:export
   #:run
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
    (let ((choice (ignore-errors (parse-integer (read-line)))))
      (if choice
	  (nth choice types)
	  (prog1 nil (format t "Whaaa?~%"))))))

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

(defun test-screen-size ()
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
  (let ((l1 "There should be an 'X' in every corner of the screen.~%")
	(l2 "Press Q to quit, anything else to continue."))
    (tt-move-to (truncate (terminal-window-rows *terminal*) 2)
		(- (truncate (terminal-window-columns *terminal*) 2)
		   (truncate (length l1) 2)))
    (tt-format l1)
    (tt-move-to (+ (truncate (terminal-window-rows *terminal*) 2) 1)
		(- (truncate (terminal-window-columns *terminal*) 2)
		   (truncate (length l2) 2)))
    (tt-format l2))
  (tt-finish-output)

  (case (tt-get-key)
    ((#\Q #\q)
     (throw 'quit nil))))

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
	   (tt-ins-char 1))
	(tt-finish-output)
	(sleep sleep))

     (loop :for col :from 0 :below (- full thing-width) :do
	(loop :for i :from 0 :below height :do
	   (tt-move-to i 0)
	   (tt-del-char 1))
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

(defun run ()
  (let ((class #| :ansi |# (ask-class)  ))
    (when class
      (with-terminal (class)
	(catch 'quit
	  (terminal-get-size *terminal*)
	  (test-screen-size)
	  (test-basics)
	  (test-cursor-visibility)
	  (test-save-and-restore-cursor)
	  (test-move-to-col)
	  (test-ins-del)
	  (test-attrs)
	  (test-colors)
	  (test-scrolling-region)
	  ))
      (format t "~%All done.~%"))))

(defun churn ()
  (let ((class :ansi #| (ask-class) |# ))
    (when class
      (with-terminal (class)
	(terminal-get-size *terminal*)
	;;(test-basics)
	(loop
	   :do
	   (asdf:load-system :terminal-test)
	   :while (test-scrolling-region))
	)
      (format t "~%All done.~%"))))

;; EOF
