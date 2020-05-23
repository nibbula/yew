;;;
;;; renderp.lisp - stopgap graphics rendering
;;;

(defpackage :renderp
  (:documentation "shit")
  (:use :cl )
  (:export
   #:define-draw-line
   #:draw-rectangle
   #:define-draw-circle
   ))
(in-package :renderp)

;; This is some bullshit software rendering until I get something really
;; functioning. Keep in mind this may be moved/adapted into a software renderer
;; there. I'm not sure it's worth it that these are macros. Maybe compare using
;; functions?

;; @@@ This assumes coordinates must be integers. It should probably let the
;; drawing function decide.
(defmacro define-draw-line (name draw-function)
  "Define a line drawing function called NAME, which draws pixels with
DRAW-FUNCTION. If DRAW-FUNCTION is T, the function is defined with a
DRAW-FUNCTION keyword, which can be passed to the function and is called instead
of the providied DRAW-FUNCTION.

The function NAME has the arguments:
  (start-x start-y end-x end-y stroke)

DRAW-FUNCTION takes:
  (x y stroke)
"
  (let ((draw-it (if (eq draw-function t)
		     '(funcall draw-function) `(,draw-function))))
    `(defun ,name (start-x start-y end-x end-y
		   &key stroke
		     ,@(when (eq draw-function t) '(draw-function)))
       (let* ((x-inc   (signum (- end-x start-x)))
	      (y-inc   (signum (- end-y start-y)))
	      (slope   (when (not (zerop (- end-x start-x)))
			 (/ (- end-y start-y) (- end-x start-x)))))
	 (cond
	   ((not slope)
	    ;; vertical line
	    (when (minusp y-inc)
	      (rotatef start-y end-y))

	    (loop :for y :from start-y :to end-y :do
	       (,@draw-it start-x y stroke)))

	   ((zerop slope) ;; horizontal
	    (when (minusp x-inc)
	      (rotatef start-x end-x))

	    (loop :for x :from start-x :to end-x :do
	       (,@draw-it x start-y stroke)))

	   (t ;; sloped line
	    (when (< x-inc 0)
	      (rotatef start-x end-x)
	      (rotatef start-y end-y))

	    (if (> (abs slope) 1)
		(if (> slope 0)
		    (loop
		       :for x :from start-x :to end-x
		       :for y = start-y :then (+ y slope)
		       :do
		       (loop :for yy :from y :to (min (+ y slope) end-y)
			  :do
			  (,@draw-it (truncate x) (truncate yy) stroke)))
		    (loop
		       :for x :from start-x :to end-x
		       :for y = start-y :then (+ y slope)
		       :do
		       (loop :for yy :from y :downto (max (+ y slope) end-y)
			  :do
			  (,@draw-it (truncate x) (truncate yy) stroke))))
		(loop
		   :for x :from start-x :to end-x
		   :for y = start-y :then (+ y slope)
		   :do
		   (,@draw-it (truncate x) (truncate y) stroke)))))))))

(defun draw-rectangle (start-x start-y end-x end-y &key stroke fill)
  (let* ((top    (max 0 (min end-y start-y)))
	 ;; (bottom (min (max end-yy start-y) (aa-buffer-height buf)))
	 (bottom (max end-y start-y))
	 (left   (max 0 (min end-x start-x)))
	 ;; (right  (min (max end-x start-x) (aa-buffer-width buf)))
	 (right  (max end-x start-x))
	 (width  (- right left))
	 (height (- bottom top)))
    (cond
      (fill
       (loop :with i fixnum = 0
	  :for y :from top :below bottom :do
	  (loop :with j fixnum = 0
	     :for x :from left :below right :do
	     (if (or (= i 0) (= j 0) (= (1+ j) width) (= (1+ i) height))
		 (when stroke
		   (funcall stroke x y))
		 (funcall fill x y))
	     (incf j))
	  (incf i)))
      (stroke
       (loop :for x :from left :below right :do
	  (funcall stroke x 0))
       (loop :for y :from top :below bottom :do
	  (funcall stroke 0 y)
	  (funcall stroke (1- right) y))
       (loop :for x :from left :below right :do
	  (funcall stroke x (1- height)))))))

;; General formula for unit circle at zero of radius r:
;; r² = x² + y²
;;
;; With the center at cx,cy:
;; r² = (x - cx)² + (y - cy)²

;; Solve for y:
;; y = sqrt(r² - (x - cx)²) + cy

;; - plot each octant
;; - fill in the blanks

(defun circle-points (function ix iy cx cy pix)
  (flet ((sett (x y)
	   (funcall function (+ cx x) (+ cy y)) pix))
    (let* ((x  (- ix cx))
	   (y  (- iy cy))
	   (nx (- x))
	   (ny (- y)))
      (sett  x  y)
      (sett  y  x)
      (sett  x ny)
      (sett  y nx)
      (sett nx  y)
      (sett ny  x)
      (sett nx ny)
      (sett ny nx))))

(defun circle-points-fill (function ix iy cx cy r pix fill over)
  (flet ((seto (x y)
	   (funcall function (+ cx x) (+ cy y)) pix)
	 (seti (x y)
	   (funcall function (+ cx x) (+ cy y)) fill))
    (let* ((x  (- ix cx))
	   (y  (- iy cy))
	   (nx (- x))
	   (ny (- y)))
      (when (and (< y r) (not over))
	(loop :for lx :from (1+ nx) :below x
	   :do
	   (seti lx y)
	   (seti lx ny)))
      (loop :for lx :from (1+ ny) :below y
	 :do
	 (seti lx x)
	 (seti lx nx))
      (seto  x  y)
      (seto  y  x)
      (seto  x ny)
      (seto  y nx)
      (seto nx  y)
      (seto ny  x)
      (seto nx ny)
      (seto ny nx))))

(defmacro define-draw-circle (name draw-function)
  "Define a circle drawing function called NAME, which draws pixels with
DRAW-FUNCTION. If DRAW-FUNCTION is T, the function is defined with a
DRAW-FUNCTION keyword, which can be passed to the function and is called instead
of the providied DRAW-FUNCTION.

The function NAME has the arguments:
  (start-x start-y end-x end-y &key stroke fill)

STROKE is a pixel value to draw with.
FILL is a pixel value for the interior of the circle.

DRAW-FUNCTION takes:
  (y x pixel)
"
  (let ((draw-func (if (eq draw-function t)
		       'draw-function draw-function)))
    `(defun ,name (start-x start-y end-x end-y
		   &key stroke fill
		     ,@(when (eq draw-function t) '(draw-function)))
	(let* ((top    (max 1 (min end-y start-y)))
	       ;; (bottom (min (max end-y start-y) (aa-buffer-height buf)))
	       (bottom (max end-y start-y))
	       (left   (max 1 (min end-x start-x)))
	       ;; (right  (min (max end-x start-x) (aa-buffer-width buf)))
	       (right  (max end-x start-x))
	       (width  (- right left))
	       (height (- bottom top))
	       (cx     (+ left (truncate width 2)))
	       (cy     (+ top (truncate height 2)))
	       (x-r    (- right cx))
	       (y-r    (- bottom cy))
	       (r      (min x-r y-r)))
	  (if fill
	      (circle-points-fill ,draw-func cx (+ cy r) cx cy r
				  stroke fill nil)
	      (circle-points ,draw-func cx (+ cy r) cx cy
			     stroke))
	  (loop
	     :with x = cx
	     :and y = (+ cy r)
	     :and d = (- 1 r)
	     :and last-y
	     :while (> (- y cy) (- x cx))
	     :do
	     (setf last-y y)
	     (if (<= d 0)
		 (incf d (+ (* (- x cx) 2) 3))
		 (progn
		   (incf d (+ (* (- (- x cx) (- y cy)) 2) 5))
		   (decf y)))
	     (incf x)
	     (if fill
		 (circle-points-fill ,draw-func x y cx cy r
				     stroke fill (equal last-y y))
		 (circle-points ,draw-func x y cx cy stroke)))))))

;; End
