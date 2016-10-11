;;
;; curses-test.lisp - Tests for the curses interface.
;;

(defpackage :curses-test
  (:documentation "Tests for the curses interface.")
  (:use :cl :curses :cffi)
  (:export
   #:test
   #:test-menu
   ))
(in-package :curses-test)

(defun init-colors ()
  ;; Initialize all the color pairs
  (start-color)
  (let ((ncolors 8))
    (if (= (has-colors) 1)
    	(prog ((pair 0))
	   (loop :for fg :from (- ncolors 1) :downto 0 :by 1 :do
	      (loop :for bg :from 0 :below ncolors :by 1 :do
		 (if (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		     (init-pair pair fg bg))
		 (setq pair (+ pair 1)))))))
  (bkgd (color-pair 0)))

(defun test-colors ()
  (clear)
  (let ((has-color (= (has-colors) 1))
	(normal-color-pairs (min *COLOR-PAIRS* 64)))

    ;; Describe the color setup
    (if (not has-color)
	(addstr "!"))
    (addstr (format nil "has_colors~%"))
    (if (= (can-change-color) 0)
	(addstr "!"))
    (addstr (format nil "can_change_color~%"))
    (addstr (format nil "COLOR_PAIRS = ~d~%" *COLOR-PAIRS*))

    ;; Initialize all the normal color pairs
    (init-colors)
    ;; (if has-color
    ;; 	(prog ((pair 0))
    ;; 	   (loop :for fg :from (- NCOLORS 1) :downto 0 :by 1 :do
    ;; 	      (loop :for bg :from 0 :below NCOLORS :by 1 :do
    ;; 		 (if (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
    ;; 		     (init-pair pair fg bg))
    ;; 		 (setq pair (+ pair 1))))))

    ;; Show the color pairs
    (addstr (format nil "Color pairs~%"))
    (loop :for i :from 0 :below normal-color-pairs :by 1 :do
       (if (= (mod i 8) 0)
	   (addch (char-code #\newline)))
       (attron (COLOR-PAIR i))
       (addstr (format nil "~2d " i))
       (attroff (COLOR-PAIR i)))

    (addch (char-code #\newline))
    (refresh)

    ;; Show the color pairs with bold on
    (addstr (format nil "Bold color pairs~%"))
    (loop :for i :from 0 :below normal-color-pairs :by 1 :do
       (if (= (mod i 8) 0)
	   (addch (char-code #\newline)))
       (attron (COLOR-PAIR i))
       (attron +a-bold+)
       (addstr (format nil "~2d " i))
       (attroff (COLOR-PAIR i)))
    (attroff +a-bold+)
    (refresh)

    ;; Show the color pairs with bold and reverse on
    (let ((y 3))
      (move y 30)
      (addstr (format nil "Bold and reverse color pairs~%"))
      (loop :for i :from 0 :below normal-color-pairs :by 1 :do
	 (when (= (mod i 8) 0)
	   (move (incf y) 30))
	 (attron (COLOR-PAIR i))
	 (attron +a-bold+)
	 (attron +a-reverse+)
	 (addstr (format nil "~2d " i))
	 (attroff (COLOR-PAIR i))))
    (attroff +a-bold+)
    (attroff +a-reverse+)
    (refresh)
    (getch)

;; Nobody does dim
      ;; Show the color pairs with dim
;       (let ((y 13))
; 	(move y 30)
; 	(addstr (format nil "Dim color pairs~%"))
; 	(loop :for i :from 0 :below *COLOR-PAIRS* :by 1 :do
; 	      (when (= (mod i 8) 0)
; 		(move (incf y) 30))
; 	      (attron (COLOR-PAIR i))
; 	      (attron A-DIM)
; 	      (addstr (format nil "~2d " i))
; 	      (attroff (COLOR-PAIR i))))
;       (attroff A-DIM)
;       (refresh)
;       (getch)

    (when (> *color-pairs* 64)
      ;; @@@@@
      )))

(defun test-acs ()
  "Test the alternate character set."
  (clear)
  (let ((alt-chars
	 `((ulcorner      . ,(acs-ulcorner))
	   (llcorner      . ,(acs-llcorner))
	   (urcorner      . ,(acs-urcorner))
	   (lrcorner      . ,(acs-lrcorner))
	   (ltee	  . ,(acs-ltee))
	   (rtee	  . ,(acs-rtee))
	   (btee	  . ,(acs-btee))
	   (ttee	  . ,(acs-ttee))
	   (hline	  . ,(acs-hline))
	   (vline	  . ,(acs-vline))
	   (plus	  . ,(acs-plus))
	   (s1	          . ,(acs-s1))
	   (s9	          . ,(acs-s9))
	   (diamond	  . ,(acs-diamond))
	   (ckboard	  . ,(acs-ckboard))
	   (degree	  . ,(acs-degree))
	   (plminus	  . ,(acs-plminus))
	   (bullet	  . ,(acs-bullet))
	   (larrow	  . ,(acs-larrow))
	   (rarrow	  . ,(acs-rarrow))
	   (darrow	  . ,(acs-darrow))
	   (uarrow	  . ,(acs-uarrow))
	   (board	  . ,(acs-board))
	   (lantern	  . ,(acs-lantern))
	   (block	  . ,(acs-block))
	   (s3	          . ,(acs-s3))
	   (s7	          . ,(acs-s7))
	   (lequal	  . ,(acs-lequal))
	   (gequal	  . ,(acs-gequal))
	   (pi	          . ,(acs-pi))
	   (nequal	  . ,(acs-nequal))
	   (sterling      . ,(acs-sterling)))))
    (loop :with y = 0 :and x = 0
       :for (name . c) :in alt-chars :do
       (mvaddstr y x (string-downcase (princ-to-string name)))
       (attron +a-altcharset+)
       (mvaddch y (+ x 11) c)
       (attroff +a-altcharset+)
       (incf y)
       (when (>= y (1- *LINES*))
	 (setf y 0)
	 (incf x 20))))
      (attroff +a-altcharset+)
      (refresh)
      (getch))

(defun test-borders ()
  "Test drawing borders and windows."
  (border 0 0 0 0 0 0 0 0)
  (refresh)

  (loop :for i :from 1 :below (/ *LINES* 2) :by 1 :do
     (let ((w (newwin (- *LINES* (* 2 i))
		      (- *COLS* (* 2 i)) i i)))
       (wborder w 0 0 0 0 0 0 0 0)
       (wrefresh w)
       (refresh)
       ;; (when (= (getch) (char-code #\q))
       ;; (return))
       (sleep .1)
       (delwin w)))
  (refresh)
  (getch)

  ;; Clear stuff out
  (let ((w (newwin (- *LINES*  2) (- *COLS* 2) 1 1)))
    (wclear w)
    (wrefresh w)))

(defun test-random ()
  "Random color blast"
  (clear)
  (border 0 0 0 0 0 0 0 0)
  (let* ((y (+ 1 (random (- *LINES* 2))))
	 (x (+ 1 (random (- *COLS*  2))))
	 (c (if (= 0 (random 2)) #\* #\space))
	 (has-color (= (has-colors) 1))
	 (color (if has-color (random *COLOR-PAIRS*) 0))
	 (start-time (get-universal-time)))
    ;;(loop :for i :from 0 :to 4000 :by 1 :do
    (loop :with i = 0 :and now-ish = start-time
       :while (< (- now-ish start-time) 3)
       :do
       (setq y (+ 1 (random (- *LINES* 2))))
       (setq x (+ 1 (random (- *COLS* 2))))
       (setq c (if (= 0 (logand (random #x9000) #x1000)) #\* #\space))
       (when has-color
	 (setq color (random *COLOR-PAIRS*))
	 (attron (COLOR-PAIR color)))
       (mvaddch y x (char-code c))
       (refresh)
       (when (zerop (mod i 500))
	 (setf now-ish (get-universal-time)))
       (when has-color
	 (attroff (COLOR-PAIR color))))
    (move (- *LINES* 1) 0)
    (getch)))

(defun call-test (test-func &key device term-type)
  (declare (type (or string null) device term-type))
  (let (screen)
    (unwind-protect
      (progn
	(if device
	    (progn
	      (let (fin fout)
		(if (null-pointer-p (setf fin (nos:fopen device "r")))
		    (error "Can't open curses input device"))
		(if (null-pointer-p (setf fout (nos:fopen device "w")))
		    (error "Can't open curses output device"))
		(if (null-pointer-p (setf screen (newterm term-type fout fin)))
		    (error "Can't initialize curses terminal"))
		(set-term screen)))
	    (progn
	      (initscr)))
	(start-color)
	(cbreak)
	(noecho)
	(nonl)
	(typeahead -1)
	(clear)

	(funcall test-func))
      (reset-shell-mode)
      (endwin)
      (when screen
	(delscreen screen)))
    (princ "Done.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+curses-use-wide
(defun test-wchar-1 ()
  (clear)
  (let ((chars
	 #+(or sbcl clisp cmu)
	 `(#\black_left-pointing_triangle
	   #\black_smiling_face
	   #\black_right-pointing_triangle
	   ,(code-char 0))
	 #+(or ecl ccl)
	 `(#\u25C0
	   #\u263B
	   #\u25B6
	   ,(code-char 0))
	 ))
    (with-foreign-object (str :int (length chars))
      (loop :with i = 0 :for c :in chars :do
	 (setf (mem-aref str :int i) (char-code c))
	 (incf i))
      (setf (mem-aref str :int (1- (length chars))) 0)
      (move 10 10)
      (addnwstr str (1- (length chars)))))
  (refresh)
  (getch))

#+curses-use-wide
(defun wa (wide-string)
  (with-foreign-object (fstr :int (1+ (length wide-string)))
      (loop :with i = 0 :for c :across wide-string :do
	 (setf (mem-aref fstr :int i) (char-code c))
	 (incf i))
      (setf (mem-aref fstr :int (length wide-string)) 0)
      (addnwstr fstr (length wide-string))))

#+curses-use-wide
(defparameter *block*
  '("░░░░░░░░"
    "░▒▒▒▒▒▒░"
    "░▒▓▓▓▓▒░"
    "░▒▓██▓▒░"
    "░▒▓▓▓▓▒░"
    "░▒▒▒▒▒▒░"
    "░░░░░░░░"))

#+curses-use-wide
(defun draw-block (y x)
  (move y x)
  (loop :with i = y
     :for line :in *block* :do
     (wa line)
     (move (incf i) x))
  (refresh))

#+curses-use-wide
(defstruct blook
  color
  x y
  xinc yinc)

#+curses-use-wide
(defun show-blook (b)
  (attron (color-pair (blook-color b)))
  (draw-block (blook-y b) (blook-x b))
  (attroff (color-pair (blook-color b))))

#+curses-use-wide
(defun test-wchar-2 ()
  (clear)
  (let ((blocks
	 (list
	  (make-blook :color 40 :y 10 :x 20 :xinc  1 :yinc 1)
	  (make-blook :color 16 :y 10 :x 30 :xinc  1 :yinc 0)
	  (make-blook :color 24 :y 20 :x 20 :xinc -1 :yinc 0)
	  (make-blook :color 48 :y 20 :x 40 :xinc -1 :yinc -1)))
	(t-o 50) xi yi)
    (flet ((new-blook ()
	     (loop :do (setf xi (- (random 3) 1) yi (- (random 3) 1))
		:while (and (= xi 0) (= yi 0)))
	     (push (make-blook :color (* (random 7) 8)
			       :y (random *lines*) :xinc xi
			       :x (random *cols*)  :yinc yi) blocks)))
      (curses::timeout t-o)
      (loop :for i :from 1 :to 2000 :do
	 (erase)
	 (move 0 0) (wa "Ｙｅｓ， ｙｏｕ ｈａｖｅ ＷＩＤＥ ｃｈａｒｓ！")
	 (mvaddstr 1 0 "You should see rectangles bouncing around.")
	 (loop :for b :in blocks :do
	    (show-blook b)
	    (incf (blook-x b) (blook-xinc b))
	    (incf (blook-y b) (blook-yinc b))
	    (when (<= (blook-x b) 0)		 (setf (blook-xinc b) 1))
	    (when (>= (blook-x b) (- *cols* 8))	 (setf (blook-xinc b) -1))
	    (when (<= (blook-y b) 0)	    	 (setf (blook-yinc b) 1))
	    (when (>= (blook-y b) (- *lines* 7)) (setf (blook-yinc b) -1)))
	 (let* ((c (getch)) (cc (if (>= c 0) (code-char c) nil)))
	   (case cc
	     (#\q (return))
	     (#\n (new-blook))
	     (#\- (curses::timeout (decf t-o 1)))
	     (#\+ (curses::timeout (incf t-o 1))))))))
  (curses::timeout -1))

;; On some implementations, such as CCL and ECL you need to call setlocale
;; before this will work.
#+curses-use-wide
(defun test-wchar ()
  ;; (initscr)
  ;; (cbreak)
  ;; (noecho)
  ;; (nonl)
  ;; (typeahead -1)
  ;; (init-colors)
  (test-wchar-1)
  (test-wchar-2)
  ;; (endwin)
  )

(defun test-keys ()
  "See what curses thinks a key is."
  (cbreak)
  (noecho)
  (nonl)
  (keypad *stdscr* 1)
  (clear)
  (move 0 0)
  (addstr (format nil "Type keys to see what curses returns.~%"))
  (addstr (format nil "Press escape four time in a row to exit~%"))
  (addstr (format nil "-------~%"))
  (let ((escape 0) c (line 3))
    (loop :while (< escape 4)
       :do
       (setf c (getch))
       (addstr (format nil "~d #x~x #o~o " c c c))
       (if (and (>= c 0) (<= c 255))
	   (addstr (format nil "~s ~a" (code-char c)
			   (char-name (code-char c))))
	   (addstr (format nil "~s" (function-key c))))
       (addch (char-code #\newline))
       (incf line)
       (when (> line (1- *lines*))
	 (setf line 3)
	 (move 3 0))
       (if (eql (code-char c) #\escape)
	   (incf escape)
	   (setf escape 0)))))

(defparameter *test-menu*
  '(("Colors"                   . test-colors)
    ("Alternate character set"  . test-acs)
    ("Window borders"           . test-borders)
    ("Random cursor movement"   . test-random)
    #+curses-use-wide
    ("Wide characters"          . test-wchar)
    ("Keys"			. test-keys)
    ))

(defun show-menu ()
  (clear)
  (addstr (format nil " Curses Tests ~%~%"))
  (loop :with i = 1
     :for (name . nil) :in *test-menu* :do
     (addstr (format nil "  [~d]  ~a~%" i name))
     (incf i))
  (addstr (format nil "~%  [q]  Quit~%")))

(defun menu-loop ()
  (loop :with c :and quit-flag
     :while (not quit-flag)
     :do
     (show-menu)
     (setf c (getch))
     (cond
       ((digit-char-p (code-char c))
	(funcall (cdr (nth (1- (- c (char-code #\0))) *test-menu*))))
       ((eql #\q (code-char c))
	(setf quit-flag t)))))

(defun test-all ()
  (test-colors)
  (test-acs)
  (test-borders)
  (test-random)
  #+curses-use-wide (test-wchar))

(defun test-menu ()
  (call-test #'menu-loop)
  (values))

(defun test ()
  (call-test #'test-all)
  (values))

;; EOF
