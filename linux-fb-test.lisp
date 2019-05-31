;;
;; linux-fb-test.lisp - Tests for Linux framebuffer
;;

(defpackage :linux-fb-test
  (:documentation "Tests for Linux framebuffer")
  (:use :cl :dlib :linux-fb :terminal)
  (:export
   #:run
   ))
(in-package :linux-fb-test)

(defun test-rect ()
  "Test drawing boxes."
  (let ((limit (truncate (framebuffer-height (context-fb *context*)) 2))
	(height (framebuffer-height (context-fb *context*)))
	(width  (framebuffer-width (context-fb *context*)))
	(n 20))
    (loop :for i :from 1 :below limit :by n
       :do (rectangle i i
		      (+ i (- width  (* 2 i)))
		      (+ i (- height (* 2 i))) #x0000ff00))))

(defun test-line ()
  (let* ((height (framebuffer-height (context-fb *context*)))
	 (width  (framebuffer-width (context-fb *context*)))
	 (center-x (truncate width 2))
	 (center-y (truncate height 2))
	 (pixel #x0000ff00)
	 (step 10))
    (loop :for y :from center-y :below height :by step
       :do (line center-x center-y (1- width) y pixel))

    (loop :for x :from center-x :below width :by step
       :do (line center-x center-y x (1- height) pixel))

    (loop :for x :from 0 :below center-x :by step
       :do (line center-x center-y x 0 pixel))

    (loop :for y :from 0 :below center-y :by step
       :do (line center-x center-y 0 y pixel))

    (loop :for x :from center-x :below width :by step
       :do (line center-x center-y x 0 pixel))

    (loop :for y :from 0 :below center-y :by step
       :do (line center-x center-y (1- width) y pixel))

    (loop :for y :from center-y :below height :by step
       :do (line center-x center-y 0 y pixel))

    (loop :for x :from 0 :below center-x :by step
       :do (line center-x center-y x (1- height) pixel))))

(defun test-pixel ()
  (let ((height (framebuffer-height (context-fb *context*)))
	(width  (framebuffer-width (context-fb *context*))))
    (loop :for i :from 1 :to 100000 :do
	 (set-pixel (random width) (random height)
		    (color-pixel (random #xff) (random #xff) (random #xff))))))

(defun test-colors ()
  (let* ((height (framebuffer-height (context-fb *context*)))
	 (width  (framebuffer-width (context-fb *context*)))
	 (rows (- height 2))
	 (cols (- width 1))
	 (x-div (/ width 255.0))
	 (y-div (/ height 255.0)))
    (loop :for y :of-type fixnum :from 0 :below rows
       :do
       (loop :for x :of-type fixnum :from 0 :below cols :do
	  (set-pixel x y (color-pixel (round x x-div)
				      (round y y-div)
				      128))))))
(defun munch (n)
  (let* ((height (framebuffer-height (context-fb *context*)))
	 (width  (framebuffer-width (context-fb *context*))))
    (loop :with i :of-type fixnum
       :for y :of-type fixnum :from 0 :below height :do
       (loop :for x :of-type fixnum :from 0 :below width :do
         (setf i (+ (logxor x y) n))
	 (set-pixel x y (linux-fb:color-pixel i i i))))))

(defun munching ()
  (loop
     (loop :for i :from 0 :to 255
	:do (munch i)
	(when (tt-listen-for .01)
	  (return-from munching nil)))
     (sleep .1)))

(defun clear ()
  (let* ((height (framebuffer-height (context-fb *context*)))
	 (width  (framebuffer-width (context-fb *context*))))
    (rectangle-fill 0 0 width height 0)))

(defparameter *tests*
  '(test-rect test-line test-pixel test-colors munching))

(defun show-off ()
  (with-immediate ()
    (loop :for test :in *tests* :do
	 (clear)
	 (funcall test)
	 (tt-get-key))))

(defmacro p (r g b) `(color-pixel (round ,r) (round ,g) (round ,b)))
(defmacro g (i)
  (with-unique-names (ii)
    `(let ((,ii (round ,i)))
       (color-pixel ,ii ,ii ,ii))))

(defun plunk (func)
  (let* ((height (framebuffer-height (context-fb *context*)))
	 (width  (framebuffer-width (context-fb *context*))))
    (clear)
    (loop :for i :of-type fixnum = 0
       :for y :of-type fixnum :from 0 :below height :do
       (loop :for x :of-type fixnum :from 0 :below width :do
	  (set-pixel x y (funcall func x y width height i))
	  (incf i)))))

(defun blerg (body)
  (plunk (compile nil `(lambda (x y w h i)
			 (declare
			  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
			  (optimize (speed 3) (safety 0) (debug 0)
				    (space 0) (compilation-speed 0))
			  (ignorable x y w h i))
			 (progn ,@body)))))

(defun burr ()
  (loop :while
     (catch 'boo
       (handler-case
	   (progn
	     (loop :with line
		:while (and (setf line (rl:rl)) (not (zerop (length line))))
		:do (blerg (list (read-from-string line))))
	     nil)
	 (error (c)
	   (format t "~a~%" c)
	   (throw 'boo t))))))

;; (if (> y 100) (g (/ (* (- x (/ w 2)) w) (* y 1.2))) 0)
;; (g (+ (* (* 20 (sin (/ y 200))) (* 20 (cos (/ x 200))) 2) x y pi))
;; (let ((r (+ (* (* pi (sin (/ y 50))) (* 20 (cos (/ x 50))) 2) y x pi))) (p (round r 6.8) (round r pi) (round r 4.1)))

(defun run ()
  (let (fb gcontext)
    (unwind-protect
	 (progn
	   (setf fb (start)
		 gcontext (new-gc fb))
	   (tt-cursor-off)
	   (show-off))
      (when fb
	(tt-cursor-on)
	(done fb)))))

;; EOF
