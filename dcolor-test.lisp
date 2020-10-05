;;;
;;; dcolor-test.lisp - Tests for DCOLOR packge.
;;;

(defpackage :dcolor-test
  (:documentation "Tests for DCOLOR packge.")
  (:use :cl :test :dcolor)
  (:export
   #:run
   ))
(in-package :dcolor-test)

(deftests (dcolor-1 :doc "Basic functionality")
  "color-model-name"
  (eq (color-model-name #(:rgb 1 0 1)) :rgb)
  (eq (color-model-name #(:gray 1 0 1)) :gray)
  (equal (color-model-name #(1 0 1)) *default-color-model*)
  "structured-color-p"
  (not (structured-color-p nil))
  (not (structured-color-p #(:fake-color-model 9 0 9 0 9 .5)))
  (not (structured-color-p #(1 2 problem?)))
  (structured-color-p '(22 11 55))
  (structured-color-p '(:gray 2/3))
  (structured-color-p '(:hsv 2/3 .505 #x1))
  (structured-color-p '(:rgba 2/3 .505 #x1 0.8432323))
  (structured-color-p #(:rgb what is the problem?)) ;; @@@ Maybe no good.
  "copy-color"
  (equalp (copy-color #(:rgb 0 0 1)) (vector :rgb 0 0 1))
  (equal (copy-color :blue) :blue)
  "known-color-p"
  (known-color-p :cyan)
  (known-color-p :black)
  (known-color-p #(:HSV 195.10d0 1 139/255))
  (known-color-p :aquamarine)
  (known-color-p :purple)
  "lookup-color"
  (equalp (lookup-color :black) #(:rgb 0 0 0))
  (equalp (lookup-color :green) #(:rgb 0 1 0))
  (structured-color-p (lookup-color :orange))
  "color-component"
  (= (color-component #(:rgb 0 8/12 0) :green) 8/12)
  (= (color-component #(:hsv 270 1 .56) :hue) 270)
  (= (color-component #(:gray8 127) :value) 127)
  "setf color-component"
  (= (let ((c (vector :rgb 0 0 0)))
       (setf (color-component c :green) 8/12)
       (color-component c :green))
     8/12)
  (= (let ((c (vector :hsv 0 0 0)))
       (setf (color-component c :hue) 270)
       (color-component c :hue))
     270)
  (= (let ((c (vector :gray8 55)))
       (setf (color-component c :value) 99)
       (color-component c :value))
     99)
  "conversions"
  (equalp (convert-color-to #(:rgb 33/85 0 88/255) :hsv)
	  #(:hsv 920/3 1 33/85))
  (equalp (convert-color-to #(:rgb 33/85 0 88/255) :hsl)
	  #(:hsl 920/3 1 33/170))
  (equalp (convert-color-to #(:rgb 1 0 1) :gray)
	  #(:gray 2/3))
  )

(deftests (dcolor-xcolor :doc "xcolor conversion")
  "color-to-xcolor"
  (equal (color-to-xcolor #(:rgb 1 1 1)) "rgb:FF/FF/FF")
  (equal (color-to-xcolor #(:rgb 0 0 0)) "rgb:00/00/00")
  (equal (color-to-xcolor #(:rgb 0.1 0.2 0.3)) "rgbi:0.1/0.2/0.3")
  (equal (color-to-xcolor #(:rgb 0.1 0.2 0.3) :integer-p t) "rgb:19/33/4C")
  (equal (color-to-xcolor #(:rgb 0.1111 0.2222 0.3333) :integer-p t :bits 16)
	 "rgb:1C70/38E1/5552")
  "xcolor-to-color"
  (equalp (xcolor-to-color "#fff") #(:rgb 1 1 1))
  (equalp (xcolor-to-color "#ffffff") #(:rgb 1 1 1))
  (equalp (xcolor-to-color "#f01f02f03") #(:RGB 3841/4095 3842/4095 61/65))
  (equalp (xcolor-to-color "#10f20f30f") #(:RGB 271/4095 527/4095 87/455))
  (equalp (xcolor-to-color "#10ef20ef30ef")
	  #(:RGB 17/257 8431/65535 12527/65535))
  (equalp (xcolor-to-color "#f0e1f0e2f0e3")
	  #(:RGB 4111/4369 61666/65535 61667/65535))
  (equalp (xcolor-to-color "#ABC") #(:RGB 2/3 11/15 4/5))
  (equalp (xcolor-to-color "#ABCDEF") #(:RGB 57/85 41/51 239/255))
  (equalp (xcolor-to-color "#abcdef") #(:RGB 57/85 41/51 239/255))
  (equalp (xcolor-to-color "rgb:a/b/c") #(:RGB 2/3 11/15 4/5))
  (equalp (xcolor-to-color "rgb:ab/bc/cd") #(:RGB 57/85 188/255 41/51))
  (equalp (xcolor-to-color "rgb:abc/bcd/cde")
	  #(:RGB 916/1365 1007/1365 366/455))
  (equalp (xcolor-to-color "rgb:abcd/bcde/cdef")
	  #(:RGB 43981/65535 9670/13107 17573/21845))
  )

(deftests (dcolor-all :doc "All test for dcolor.")
  dcolor-1
  dcolor-xcolor)

(defun run ()
  (run-group-name 'dcolor-all :verbose t))

;; End
