;;
;; terminal-crunch-test.lisp - Tests for terminal-crunch.
;;

(defpackage :terminal-crunch-test
  (:documentation "Tests for terminal-crunch.")
  (:use :cl :dlib :terminal :terminal-crunch :terminal-ansi :fatchar)
  (:export
   #:test-1
   #:test-2
   #:test-3
   #:test-4
   ))
(in-package :terminal-crunch-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

;; These are really just things I used for getting it working initially.
;; When it's working better, we should probably get rid of these and make
;; tests that fully exercise the crunching aspects and efficiency.

(defvar tta)
(defvar tt)

(defun dump-hashes (tty)
  (let ((old-hashes (terminal-crunch::screen-hashes
		     (terminal-crunch::old-screen tty)))
	(new-hashes (terminal-crunch::screen-hashes
		     (terminal-crunch::new-screen tty))))
    (format *debug-io* "Old -> New~%")
    (loop :for i :from 0 :below (length old-hashes) :do
       (format *debug-io* "~s ~s~%" (aref old-hashes i) (aref new-hashes i)))))

(defun dump-screen (tty)
  (let ((old-lines (terminal-crunch::screen-lines
		     (terminal-crunch::old-screen tty)))
	(new-lines (terminal-crunch::screen-lines
		     (terminal-crunch::new-screen tty))))
    (format *debug-io* "Old -> New~%")
    (loop :for i :from 0 :below (length old-lines) :do
       (format *debug-io* "[~a] [~a]~%"
	       (make-fat-string :string (aref old-lines i))
	       (make-fat-string :string (aref new-lines i))))))

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

(defmacro with-crunch ((device-name) &body body)
  (with-unique-names (tty state)
    `(let ((,tty (make-instance 'terminal-crunch
				:wrapped-terminal
				(make-instance 'terminal-ansi
					       :device-name ,device-name))))
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
	    (tt-ins-char 1))
	 (tt-finish-output)
	 ;; (sleep sleep)
	 (dump-screen *terminal*)
	 (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))
	 )

      (loop :for col :from 0 :below (- full thing-width) :do
	 (loop :for i :from 0 :below height :do
	    (tt-move-to i 0)
	    (tt-del-char 1))
	 (tt-finish-output)
	 (sleep sleep)
	 (dump-screen *terminal*)
	 (dump-hashes *terminal*)
	 (case (tt-get-key) ((#\Q #\q) (throw 'quit nil)))
	 ))))

;; EOF
