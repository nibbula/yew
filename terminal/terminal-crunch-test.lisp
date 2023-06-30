;;
;; terminal-crunch-test.lisp - Tests for terminal-crunch.
;;

(defpackage :terminal-crunch-test
  (:documentation "Tests for terminal-crunch.")
  (:use :cl :dlib :dlib-misc :terminal :terminal-crunch :terminal-ansi :fatchar
	:table :table-print :rl :lish :dtime)
  (:import-from :terminal-crunch #:dump-hashes #:dump-screen)
  (:export
   #:test-1 #:test-2 #:test-3 #:test-4 #:test-5 #:test-6 #:test-7 #:test-8
   #:test-hashing #:test-hash-1
   #:test-zerg-1 #:test-zerg-2
   #:run
   ))
(in-package :terminal-crunch-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; These are really just things I used for getting it working initially.
;; When it's working better, we should probably get rid of these and make
;; tests that fully exercise the crunching aspects and efficiency.

(defvar tta)
(defvar tt)

(defun test-1 (device-name)
  (setf tta (make-instance 'terminal-ansi :device-name device-name))
  (setf tt (make-instance 'terminal-crunch :wrapped-terminal tta))
  (terminal-start tt)
  ;; make sure the wrapped terminal is functioning?
  ;; (terminal-home tta)
  ;; (terminal-clear tta)
  ;; (terminal-write-string tta "Hello there.")
  ;; (terminal-finish-output tta)
  (tt-write-string "Okay?")
  (tt-get-key)
  (dotimes (i 20)
    (terminal-write-string tt (format nil "~r~%" (+ 100000 i))))
  ;; (dump-screen tt)
  (terminal-finish-output tt)
  ;; on a different terminal:
  ;; (dump-hashes tt)
  ;; (dump-screen tt)
  (tt-write-string "How bout it?")
  (tt-get-key)
  (terminal-done tt))

(defun test-2 (device-name)
  ;; (with-new-terminal (:crunch *terminal*
  ;; 			      :wrapped-terminal
  ;; 			      (make-instance 'terminal-ansi
  ;; 					     :device-name device-name))
  (let ((tty (make-instance 'terminal-crunch
			    :wrapped-terminal
			    (make-instance 'terminal-ansi
					   :device-name device-name))))
    (let ((state (terminal-start tty)))
      (flet ((blit (c)
	     (loop :for y :from 0 :below 10 :do
		(loop :for x :from 0 :below 40 :do
		   (terminal-move-to tty y x)
		   (terminal-write-char tty c)))))
	(dotimes (i 5001)
	  (blit (if (evenp i) #\space #\X)))
	(terminal-finish-output tty)
	(tt-write-string "-->")
	(tt-get-key)
	(dotimes (i 5000)
	  (blit (if (evenp i) #\- #\X)))
	(terminal-finish-output tty)
	(tt-write-string "-->")
	(tt-get-key))
      (terminal-end tty state))))

(defmacro with-crunch ((device-name &rest args) &body body)
  (with-unique-names (tty state)
    `(let ((,tty (setf tt (make-instance
			   'terminal-crunch
			   :wrapped-terminal
			   (setf tta
				 (make-instance 'terminal-ansi
						:device-name ,device-name))
			   ,@args))))
       (let ((,state (terminal-start ,tty))
	     (*terminal* ,tty))
	 (catch 'quit
	   ,@body)
	 (terminal-end ,tty ,state)))))

(defun test-screen-size ()
  (tt-clear) (tt-home)
  ;; upper left
  (tt-format "X <---~%^~%|~%|")
  (dump-screen *terminal*)
  (dump-hashes *terminal*)
  (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; lower left
  (tt-move-to (- (terminal-window-rows *terminal*) 4) 0)
  (tt-format "|~%|~%v~%X <---")
  (dump-screen *terminal*)
  (dump-hashes *terminal*)
  (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

  ;; upper right
  (tt-move-to 0 (- (terminal-window-columns *terminal*) 6))
  (tt-format "---> X")
  (tt-move-to 1 (- (terminal-window-columns *terminal*) 1)) (tt-format "^")
  (tt-move-to 2 (- (terminal-window-columns *terminal*) 1)) (tt-format "|")
  (tt-move-to 3 (- (terminal-window-columns *terminal*) 1)) (tt-format "|")
  (dump-screen *terminal*)
  (dump-hashes *terminal*)
  (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

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
  (dump-screen *terminal*)
  (dump-hashes *terminal*)
  (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))

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
  (dump-screen *terminal*)
  (dump-hashes *terminal*)
  (tt-finish-output)

  (case (tt-get-key)
    ((#\Q #\q)
     (throw 'quit nil))))

(defun test-3 (device-name)
  (with-crunch (device-name)
    (test-screen-size)))

(defun test-4 (device-name)
  ;; (with-terminal (:crunch)
  (with-crunch (device-name)
    (tt-clear)
    (tt-home)
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
	 ;; (sleep sleep)
	 (dump-screen *terminal*)
	 (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))
	 )

      (loop :for col :from 0 :below (- full thing-width) :do
	 (loop :for i :from 0 :below height :do
	    (tt-move-to i 0)
	    (tt-delete-char 1))
	 (tt-finish-output)
	 (sleep sleep)
	 (dump-screen *terminal*)
	 (dump-hashes *terminal*)
	 (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))
	 ))))

(defun test-5 (device-name)
  (with-crunch (device-name)
    (tt-clear)
    (tt-home)
    (let ((width  (terminal-window-columns *terminal*))
	  ;;(height (terminal-window-rows *terminal*))
	  ;;(junk "%@#-.")
	  )
      (loop :with str
	 :for i :from 1 :to 200 :do
	 (setf str (format nil "~d" (expt 2 i)))
	 (tt-write-string (subseq str 0 (min (1- width) (length str))))
	 (tt-write-char #\newline)
	 (tt-finish-output)
	 (dump-screen *terminal*)
	 (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))
	 ))))

(defun test-6 (device-name)
  "Test RL with a start-line."
  (let ((line
	 (with-new-terminal (:ansi *terminal* :device-name device-name)
	   (terminal:terminal-get-cursor-position *terminal*))))
    (labels ((show-state (e)
	       (declare (ignore e))
	       (dump-screen *terminal*)
	       (dump-hashes *terminal*)))
      (with-crunch (device-name :start-line line)
	(rl:rl :output-callback #'show-state)))))

(defun test-7 (device-name)
  "Test RL with a start-line."
  (let ((line
	 (with-new-terminal (:ansi *terminal* :device-name device-name)
	   (terminal:terminal-get-cursor-position *terminal*))))
    ;; (labels ((show-state (e)
    ;; 	       (declare (ignore e))
    ;; 	       (dump-screen *terminal*)
    ;; 	       (dump-hashes *terminal*)))
    (with-crunch (device-name :start-line line)
      (tiny-repl:tiny-repl :terminal-type :crunch))))

(defun test-hashing ()
  (flet ((hash (x) (terminal-crunch::hash-thing x)))
    (let ((things
	   `(("char"         #\x #\z)
	     ("unicode char" #\λ #\🆑)
	     ("integers 1"   0 1)
	     ;; I suppose this one is excusable, because we cut it off and the
	     ;; bit pattern of negative integers is indistinguishable.
	     ("integers 2"   -1 #xffffffffffffffffffff)
	     ("integers 3"   #xdeadbeefcafebabe #x1010101010101010)
	     ("numbers 1"    0.0 0.0000001)
	     ;; Did I ever mention I hate floating point?
	     ;; This would be expected to fail 
	     ;; ("numbers 2"    123213123.0 123213123.00001)
	     ("numbers 2"    213123.0 213123.01)
	     ("numbers 3"    123213123.0d0 123213123.00001d0)
	     ("numbers 4"    ,pi ,(sqrt 2))
	     ("numbers 5"    ,(+ 22 (sqrt -1203211.123)) ,(sqrt -1))
	     ("keywords 1"   :23 :eleven)
	     ("keywords 2"   :blue :red)
	     ("strings 1"    "" " ")
	     ("strings 2"    "x" "y")
	     ("strings 3"    "as;dlfkjas;dlfkj" "100000000")
	     ("array 1"      #(1 2 3) #(3 2 1))
	     ("array 1"      #(:rgb .4 1.0 3.0) #(3 2 1))
	     ("array 2"      #() #(0))
	     ("fatchar 1"    ,(make-fatchar) ,(make-fatchar :line 1))
	     ("fatchar 2"    ,(make-fatchar :c #\x) ,(make-fatchar :c #\X))
	     ("fatchar 3"    ,(make-fatchar :c #\x :bg :blue)
			     ,(make-fatchar :c #\x :bg :blue-ish)))))
      (table-print:print-table
       (table:make-table-from
	(loop :with r1 :and r2
	   :for thing :in things
	   :collect (vector (first thing)
			    (write-to-string (second thing)
					     :readably t :pretty nil)
			    (setf r1 (hash (second thing)))
			    (write-to-string (third thing)
					     :readably t :pretty nil)
			    (setf r2 (hash (third thing)))
			    (if (= r1 r2) "BAD!" ""))
	   ;; We repeat it to appease our paranoia that it could be
	   ;; non-deterministic.
	   :collect (vector (first thing)
			    (write-to-string (second thing)
					     :readably t :pretty nil)
			    (setf r1 (hash (second thing)))
			    (write-to-string (third thing)
					     :readably t :pretty nil)
			    (setf r2 (hash (third thing)))
			    (if (= r1 r2) "BAD!" "")))
	:column-names '("Type" "Value 1" "Hash 1" 
			"Value 2" "Hash 2" "Bad?"))))))

(defun test-hash-1 (&optional (n 50))
  (let* ((colors (length dcolor:*simple-colors*))
	 (attrs (length fatchar::*known-attrs*))
	 (start-time (get-dtime))
	 (elapsed)
	 (hash-size (* (tt-width) (tt-height)))
	 (hash-count (* hash-size n))
	 (pre-done))
    (labels ((random-color ()
	       (elt dcolor:*simple-colors* (random colors)))
	     (random-attr ()
	       (elt fatchar::*known-attrs* (random attrs)))
	     (random-char ()
	       (terminal-crunch::make-grid-char
		:c (code-char (random #xff))
		:fg (random-color)
		:bg (random-color)
		:attrs (when (zerop (random 10))
			 (list (random-attr))))))
      (setf pre-done (make-array hash-size))
      (loop :for i :below hash-size
	    :do (setf (aref pre-done i) (random-char)))
      (loop
	:repeat n
	:do
	   (loop
	     :for c :across pre-done
	     :do (terminal-crunch::hash-thing c)))
      (setf elapsed (dtime- (dtime:get-dtime) start-time))
      (describe-duration elapsed)
      (format t "Hashes per second ~s~%"
	      (coerce (/ hash-count (dtime-to elapsed :seconds)) 'float)))))

(defun test-8 (device-name)
  "Test RL in Lish."
  (with-open-file (oo device-name :direction :io :if-exists :append)
    (labels ((show-state (e)
	       (declare (ignore e))
	       (dump-screen *terminal*)
	       (dump-hashes *terminal*)
	       )
	     (set-output-callback ()
	       (setf (rl:line-editor-output-callback rl:*line-editor*)
		     #'show-state)))
      (let ((*debug-io* oo)
	    lish:*enter-shell-hook*)
	(add-hook lish:*enter-shell-hook* #'set-output-callback)
	(with-dbugf '(:crunch :terminal)
	  (lish:lish :terminal-type :crunch))))))

(defun test-zerg-1 (&key (type :crunch) (n 50))
  (with-terminal (type)
    (with-immediate ()
      (loop :repeat n :do
	(tt-home)
	;; (tt-erase-below)
	(loop :for y :from 0 :below (tt-height) :do
	  (loop :for x :from 0 :below (tt-width) :do
	    ;; (tt-write-char-at y x (code-char c))))
	    (tt-write-char-at y x
	      (code-char (+ (char-code #\A)
			    (random (- (char-code #\Z) (char-code #\A))))))))
	(tt-finish-output)))))

(defun test-zerg-2 (&key (type :crunch) (n 50))
  (with-terminal (type)
    (with-immediate ()
      (loop :repeat n :do
	(loop :for c :from (char-code #\A) :to (char-code #\Z) :do
	  (tt-home)
	  ;; (tt-erase-below)
	  (loop :for y :from 0 :below (tt-height) :do
	    (loop :for x :from 0 :below (tt-width) :do
	      (tt-write-char-at y x (code-char c))))
	  (tt-finish-output))))))

(defun run ()
  (format t "These have to be run interactively in a terminal. So, no.~%")
  t)

;; End
