;;
;; view-image-popi.lisp - Something like a pixel shader.
;;

(defpackage :view-image-popi
  (:documentation "Something like a pixel shader, for the image viewer.
Really, just some very terse macros for doing pixel manipulation, designed
for use inside the pixel expression.")
  (:nicknames :popi)
  (:use :cl :dlib :dlib-misc :image :image-ops :dcolor)
  (:export
   #:popi-form
   #:popi-result
   #:w
   #:h
   #:n
   ))
(in-package :view-image-popi)

(declaim (inline cvt))
(defun cvt (num)
  "Convert a pixel number."
  (typecase num
    (integer (mod num #x100))
    (number (component-to-8bit num))
    (t 0)))

(declaim (inline px))
(defun px (r g b &optional (a #xff))
  "Make a color pixel."
  (make-pixel (cvt r) (cvt g) (cvt b) (cvt a)))

(declaim (inline gray))
(defun gray (v)
  "Make a gray pixel."
  (let ((g (cvt v)))
    (make-pixel g g g)))

;; Invariants
(defvar w 0 "Image width.")
(defvar h 0 "Image height.")
(defvar n 0 "A convenient variable for you. Reset each new exprssion, but left
alone otherwise, like when repeating the last expression.")

(defun popi-form (form)
  `(lambda (i y x p)
     (declare (ignorable i y x p)
	      ;; (optimize (speed 3) (safety 0) (debug 0)
	      ;; 		(space 0) (compilation-speed 0))
	      (optimize (speed 0) (safety 3) (debug 3)
			(space 0) (compilation-speed 0))
	      #+sbcl (sb-ext:muffle-conditions
		      sb-ext:compiler-note))
     (popi-result p ,@form)))

(declaim (inline popi-result))
(defun popi-result (pixel result)
  (typecase result
    (null pixel)
    (integer
     (logand #xffffffff result))
    (float
     ;; Grayscale
     (let ((v (component-to-8bit result)))
       (make-pixel v v v #xff)))
    (number
     (logand #xffffffff (truncate result)))
    (list
     (let ((r (or (car    result) 0))
	   (g (or (cadr   result) 0))
	   (b (or (caddr  result) 0))
	   (a (or (cadddr result) 1.0)))
       (make-pixel (cvt r) (cvt g) (cvt b) (cvt a))))
    (t pixel)))

(defmacro pr (p) "Pixel red component."     `(ash ,p -24))
(defmacro pg (p) "Pixel green component."   `(logand #xff (ash ,p -16)))
(defmacro pb (p) "Pixel blue component."    `(logand #xff (ash ,p -8)))
(defmacro pa (p) "Pixel alpha component."   `(logand #xff ,p))
(defmacro f  (p) "Pixel component to float." `(/ ,p #xff))
(defmacro a  (x y)
  "Image pixel ref."
  `(aref (sub-image-data i)
	 (mod (truncate ,y) (sub-image-height i))
	 (mod (truncate ,x) (sub-image-width i))))

;; End
