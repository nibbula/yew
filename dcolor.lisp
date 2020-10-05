;;;
;;; dcolor.lisp - Color representations
;;;

(defpackage :dcolor
  (:documentation
   "Provide color models and functions to manipulate color values, color names,
and conversions between color models.

Colors are stored as:

  (color-model number [number...])
  OR
  (number [number...])

where color-model is a name defining the color model, and the numbers define
values in that model. Alternatively a color can be stored without a color
model which assumes the default color model in *default-color-model*, which is
initially :rgb.

Some standard color names are be defined as keywords, in *simple-colors*.
See the color-names package for a bunch of color names.

Note that a color model is not a color space. A color model just says
what color components we store, not a precise definition of how to render,
or what exactly that means in terms of light and/or pigments and/or a
specific device, e.g. an ICC color profile or something. Although, the color
model can affect the color space we can represent.

We can consider the color model as being the color of light, not the
properties of a material which affects what happens to the light after it
hits the material, which probably are best represented by statisical
functions that approximate light modification or some other physical model.
For example, what color is your opalescent ink with sparkles in it?

Note also that a color model isn't a color format, since it only specifies
what information we store, not exactly how, for example, a pixel should
be laid out in bits.

# How to use

## Making colors

The easiest way to make a color is to just use a vector like:

  #(:rgb 0.82 0.0 0.62)

or with a fresh one, like:

  (vector :rgb 0.82 0.0 0.62)

Another way to make a color is ‘make-color’. For example:

  (make-color :rgb :red 0.82 :blue 0.0 :green 0.62)

We also support named colors. There are a small number of primary color names
built in. For more color names you can load the ‘color-names’ package. You can
make a color from a name with ‘lookup-color’:

  (lookup-color :red)
   ⇒ #(:RGB 1 0 0)

  (dcolor:lookup-color :lightsteelblue)  ; with color-names
   ⇒ #(:RGB 176/255 196/255 74/85)

So you don't have to depend on the representation of colors, you can use
‘copy-color’ to make a copy of an exsiting color. This is important if you want
to modifiy a component, but don't want to change the places it's already used.

## Testing for colors

   #:structured-color-p
   #:known-color-p

## Color components

You can examine or modifiy the components of a color with the access function
‘color-component’. For example:

  (color-component #(:rgb 1 0 2/3) :blue)
   ⇒ 2/3
  (defvar *my-color* (vector :rgb 1 0 2/3))
  (setf (color-component *my-color* :green) .321)
   ⇒ .321
  *my-color*
   ⇒ #(:rgb 1 .321 2/3)

## Miscellaneous functions

   Theses functions can be used convert back and forth between some color
   representations used by XParseColor.

   #:color-to-xcolor
   #:xcolor-to-color

## Color models implemented

  ╒═════════╤═══════════════════════╤═══════════════════════════╕
  │ Keyword │Ccmponents             │Examples                   │
  ╞═════════╪═══════════════════════╪═══════════════════════════╡
  │ :RGB    │(number 0 1) Red       │#(:rgb 0.388 0 0.345)      │
  │         │(number 0 1) Green     │#(:rgb 33/85 0 88/255)     │
  │         │(number 0 1) Blue      │                           │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :RGB8   │(integer 0 255) Red    │#(:rgb8 99 0 88)           │
  │         │(integer 0 255) Green  │                           │
  │         │(integer 0 255) Blue   │                           │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :RGBA   │(number 0 1) Red       │#(:rgba 0.388 0 0.345 0.5) │
  │         │(number 0 1) Green     │#(:rgba 33/85 0 88/255 1/2)│
  │         │(number 0 1) Blue      │                           │
  │         │(number 0 1) Alpha     │                           │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :GRAY   │(number 0 1) Value     │#(:gray .15)               │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :GRAY8  │(integer 0 255) Value  │#(:gray8 38)               │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :HSV    │(number 0 1) Hue       │#(:hsv 920/3 1 33/85)      │
  │         │(number 0 1) Saturation│#(:hsv 306.666 1 0.388)    │
  │         │(number 0 1) Value     │                           │
  ├─────────┼───────────────────────┼───────────────────────────┤
  │ :HSL    │(number 0 1) Hue       │#(:hsl 920/3 1 33/170)     │
  │         │(number 0 1) Saturation│#(:HSL 306.666 1 0.194)    │
  │         │(number 0 1) Lightness │                           │
  ╘═════════╧═══════════════════════╧═══════════════════════════╛

## Converting between color models

To convert colors between color models you can use ‘convert-color’:

  (convert-color-to (lookup-color :deepskyblue4) :hsv)
   ⇒ #(:HSV 27120/139 1 139/255)

## To add a new color model:

Make methods for:
 ‘color-model-components’    : Returns a sequence of color component keywords.
 ‘color-model-component’     : Returns the value of color component.
 ‘set-color-model-component’ : How to set a component.
 ‘make-color’                : Guess what this does.

Make methods for ‘convert-color’ of the color conversion you would like to
support. And finally:

  (register-color-model :<your-color-model-name>)
")
  (:use :cl :dlib :cl-ppcre)
  (:export
   #:*simple-colors*
   #:*default-color-model*
   #:*color-models*
   #:register-color-model
   #:color-model-name
   #:structured-color-p
   #:copy-color
   #:known-color-p
   #:lookup-color
   #:color-model-components
   #:color-model-component
   #:color-component
   #:set-color-component
   #:convert-color
   #:convert-color-to
   #:make-color
   #:component-to-8bit
   #:color-to-xcolor
   #:xcolor-to-color
   ))
(in-package :dcolor)

(declaim (optimize (debug 2)))

(defparameter *simple-colors*
  '(:black :red :green :yellow :blue :magenta :cyan :white)
  "Simple color names.")

;; In case you don't want to load color-names, but need to use lookup-color.
(defparameter *simple-color-values*
  '((:black   . #(:rgb 0 0 0))
    (:red     . #(:rgb 1 0 0))
    (:green   . #(:rgb 0 1 0))
    (:yellow  . #(:rgb 1 1 0))
    (:blue    . #(:rgb 0 0 1))
    (:magenta . #(:rgb 1 0 1))
    (:cyan    . #(:rgb 0 1 1))
    (:white   . #(:rgb 1 1 1)))
  "Simple color values.")

;; @@@ perhaps I should use a defstruct rigamarole for this?
;; like defstruct with :type vector?

;; We should probably support:
;; gray, RGB, RGBA, HSV, HSL, CMYK, and perhaps YUV, and YCbCr
;; I know there's many other color models.

(defvar *default-color-model* :rgb)

(defvar *color-models* nil
  "A list of known color models.")

(defun register-color-model (model-name)
  (pushnew model-name *color-models*))

(defun color-model-name (color)
  "Return the color model name of a color."
  (assert (vector color))
  (if (symbolp (aref color 0))
      (aref color 0)
      *default-color-model*))

(defun structured-color-p (x)
  "True if x maybe could be a structured color."
  (and (not (null x))
       (or (consp x) (arrayp x))
       (or (find (elt x 0) *color-models*)
	   (every #'numberp x))))

(defun copy-color (color)
  "Return a copy of COLOR."
  (typecase color
    (sequence (copy-seq color))
    (t color)))

(defun known-color-p (x)
  "True if x is a color or kind of color that we know about."
  (or (structured-color-p x)
      (find x *simple-colors*)
      (and (find-package :color-names)
	   (find-symbol (symbol-name x) :color-names))))

(defun lookup-color (color)
  "Find the color values for a color name, or just return it."
  (if (or (keywordp color) (symbolp color))
      (if (find-package :color-names)
	  (let ((sym (find-symbol (symbol-name color) :color-names)))
	    (if sym (symbol-value sym) color))
	  (or (and *simple-color-values*
		   (cdr (assoc color *simple-color-values*)))
	      color))
      color))

(defgeneric color-model-component (color-model color component-name)
  (:documentation
   "Return the component COMPONENT-NAME of the color of the COLOR-MODEL."))

(defgeneric color-model-components (color-model)
  (:documentation
   "Return the sequence of COMPONENT-NAMEs for the COLOR-MODEL."))

(defun color-component (color component-name)
  "Return the named color component."
  (color-model-component
   (color-model-name color) color component-name))

(defgeneric set-color-model-component (color-model color component-name value)
  (:documentation
   "Set the component COMPONENT-NAME of the color of the COLOR-MODEL to
the VALUE."))

(defun set-color-component (color component-name value)
  (set-color-model-component
   (color-model-name color) color component-name value))

(defsetf color-component set-color-component)

;; Of course color conversion can be quite lossy.

;; The from-color-model is separated out so we can specialize on it.
(defgeneric convert-color (color from-color-model to-color-model)
  (:documentation "Return the COLOR converted to the TO-COLOR-MODEL."))

(defun convert-color-to (color to-color-model)
  "Return COLOR converted to color model TO-COLOR-MODEL."
  (let ((from-model (color-model-name color)))
    (if (eq from-model to-color-model)
	color
	(convert-color color from-model to-color-model))))

(defgeneric make-color (color-model &key &allow-other-keys)
  (:documentation
   "Make a color of the COLOR-MODEL with the given components."))

(defun component-to-8bit (component)
  "Convert a [0-1] value to a [0-255] value."
  ;;(round (* component #xff)))
  (logand #xff (truncate (* component #xff))))

(defun component-to-bits (component n-bits)
  "Convert a [0-1] value to a [0-<n-bits>] value."
  ;;(round (* component #xff)))
  (let ((all (1- (ash 1 n-bits))))
    (logand all (truncate (* component all)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RGB model - Red, green, and blue, components stored as Lisp numbers.
;;

(defmethod color-model-components ((color-model (eql :rgb)))
  '(:red :green :blue))

(defmethod color-model-component ((color-model (eql :rgb)) color component-name)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      (:red   (svref color (+ 0 i)))
      (:green (svref color (+ 1 i)))
      (:blue  (svref color (+ 2 i)))
      (t
       (error "There's no color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod set-color-model-component ((color-model (eql :rgb))
				      color component-name value)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      (:red   (setf (svref color (+ 0 i)) value))
      (:green (setf (svref color (+ 1 i)) value))
      (:blue  (setf (svref color (+ 2 i)) value))
      (t
       (error "There's No color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod make-color ((color-model (eql :rgb)) &key red green blue)
  (vector color-model red green blue))

(defmethod convert-color (color (from-color-model (eql :gray))
			          (to-color-model (eql :rgb)))
  (make-color :rgb
	      :red   (color-component color :value)
	      :green (color-component color :value)
	      :blue  (color-component color :value)))

(register-color-model :rgb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The RGB-8 color model: 8-bit integers
;; Mostly the same as RGB.
;; We don't do any range checking.

(defmethod color-model-components ((color-model (eql :rgb8)))
  '(:red :green :blue))

(defmethod color-model-component ((color-model (eql :rgb8)) color component-name)
  (color-model-component :rgb color component-name))

(defmethod set-color-model-component ((color-model (eql :rgb8))
				      color component-name value)
  (set-color-model-component :rgb color component-name value))

(defmethod make-color ((color-model (eql :rgb8)) &key red green blue)
  (vector color-model red green blue))

(defmethod convert-color (color
			  (from-color-model (eql :rgb))
			  (to-color-model (eql :rgb8)))
  (make-color :rgb8
	      :red   (component-to-8bit (color-component color :red))
	      :green (component-to-8bit (color-component color :green))
	      :blue  (component-to-8bit (color-component color :blue))))

(defmethod convert-color (color
			  (from-color-model (eql :rgb8))
			  (to-color-model (eql :rgb)))
  (make-color :rgb8
	      :red   (/ (color-component color :red)   #xff)
	      :green (/ (color-component color :green) #xff)
	      :blue  (/ (color-component color :blue)  #xff)))

(defmethod convert-color (color (from-color-model (eql :gray8))
			          (to-color-model (eql :rgb8)))
  (make-color :rgb8
	      :red   (color-component color :value)
	      :green (color-component color :value)
	      :blue  (color-component color :value)))

(register-color-model :rgb8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Gray model - Value component stored as a Lisp number.
;;

(defmethod color-model-components ((color-model (eql :gray)))
  '(:value))

(defmethod color-model-component ((color-model (eql :gray)) color component-name)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      (:value (svref color (+ 0 i)))
      (t
       (error "There's no color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod set-color-model-component ((color-model (eql :gray))
				      color component-name value)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      (:value  (setf (svref color (+ 0 i)) value))
      (t
       (error "There's No color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod make-color ((color-model (eql :gray)) &key value)
  (vector color-model value))

(defmethod convert-color (color (from-color-model (eql :rgb))
			          (to-color-model (eql :gray)))
  (make-color :gray :value
	      (/ (+ (color-component color :red)
		    (color-component color :green)
		    (color-component color :blue))
		 3)))

(defmethod convert-color (color (from-color-model (eql :gray8))
			          (to-color-model (eql :gray)))
  (make-color :gray :value
	       (/ (color-component color :value) #xff)))

(register-color-model :gray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The GRAY-8 color model: 8-bit integers
;; Mostly the same as GRAY.
;; We don't do any range checking.

(defmethod color-model-components ((color-model (eql :gray8)))
  '(:value))

(defmethod color-model-component ((color-model (eql :gray8))
				  color component-name)
  (color-model-component :gray color component-name))

(defmethod set-color-model-component ((color-model (eql :gray8))
				      color component-name value)
  (set-color-model-component :gray color component-name value))

(defmethod make-color ((color-model (eql :gray8)) &key value)
  (vector color-model value))

(defmethod convert-color (color
			  (from-color-model (eql :rgb))
			  (to-color-model (eql :gray8)))
  (make-color :gray8
	      :value
	      (/ (+ (component-to-8bit (color-component color :red))
		    (component-to-8bit (color-component color :green))
		    (component-to-8bit (color-component color :blue))) 3)))

(defmethod convert-color (color
			  (from-color-model (eql :rgb8))
			  (to-color-model (eql :gray8)))
  (make-color :gray8 :value
	      (/ (+ (color-component color :red)
		    (color-component color :green)
		    (color-component color :blue))
		 3)))

(defmethod convert-color (color
			  (from-color-model (eql :gray8))
			  (to-color-model (eql :rgb)))
  (make-color :rgb
	      :red   (/ (color-component color :value) #xff)
	      :green (/ (color-component color :value) #xff)
	      :blue  (/ (color-component color :value) #xff)))

(defmethod convert-color (color (from-color-model (eql :gray))
			          (to-color-model (eql :gray8)))
  (make-color :gray8 :value
	      (component-to-8bit (color-component color :value))))

(register-color-model :gray8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The RGBA color model:
;;

(defmethod color-model-components ((color-model (eql :rgba)))
  '(:red :green :blue :alpha))

(defmethod color-model-component ((color-model (eql :rgba)) color component-name)
  (case component-name
    (:red   (svref color 1))
    (:green (svref color 2))
    (:blue  (svref color 3))
    (:alpha (svref color 4))
    (t
     (error "There's no color component ~s in the ~s color-model."
	    component-name color-model))))

(defmethod set-color-model-component ((color-model (eql :rgba))
				      color component-name value)
  (case component-name
    (:red   (setf (svref color 1) value))
    (:green (setf (svref color 2) value))
    (:blue  (setf (svref color 3) value))
    (t
     (error "There's no color component ~s in ~s color-model."
	    component-name color-model))))

(defmethod make-color ((color-model (eql :rgba)) &key red green blue alpha)
  (vector color-model red green blue alpha))

(defmethod convert-color (color (from-color-model (eql :rgba))
			          (to-color-model (eql :gray)))
  (make-color :gray :value
	      (/ (+ (color-component color :red)
		    (color-component color :green)
		    (color-component color :blue))
		 3)
	      :alpha (color-component color :alpha)))

(defmethod convert-color (color (from-color-model (eql :gray))
			          (to-color-model (eql :rgba)))
  (make-color :rgba
	      :red   (color-component color :value)
	      :green (color-component color :value)
	      :blue  (color-component color :value)
	      :alpha (color-component color :alpha)))

(register-color-model :rgba)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HSV model - Hue, saturation, and value, components stored as Lisp numbers.
;;

(defmethod color-model-components ((color-model (eql :hsv)))
  '(:hue :saturation :value))

(defmethod color-model-component ((color-model (eql :hsv)) color component-name)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      ((:hue :h)        (svref color (+ 0 i)))
      ((:saturation :s) (svref color (+ 1 i)))
      ((:value :v)      (svref color (+ 2 i)))
      (t
       (error "There's no color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod set-color-model-component ((color-model (eql :hsv))
				      color component-name value)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      ((:hue :h)        (setf (svref color (+ 0 i)) value))
      ((:saturation :s) (setf (svref color (+ 1 i)) value))
      ((:value :v)      (setf (svref color (+ 2 i)) value))
      (t
       (error "There's No color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod make-color ((color-model (eql :hsv)) &key hue saturation value)
  (vector color-model hue saturation value))

;; I translated this and HSL from the annoyingly mathy description on:
;; https://en.wikipedia.org/wiki/HSL_and_HSV

(defmethod convert-color (color (from-color-model (eql :rgb))
			          (to-color-model (eql :hsv)))
  ;; Assuming that the rgb components are in the range [0…1]
  (let* ((r (color-component color :red))
	 (g (color-component color :green))
	 (b (color-component color :blue))
	 (c-max (max r g b))
	 (c-min (min r g b))
	 (c (- c-max c-min))
	 (hue (cond
		((= c-max c-min) 0)
		((= c-max r) (* 60 (/ (- g b) c)))
		((= c-max g) (* 60 (+ 2 (/ (- b r) c))))
		((= c-max b) (* 60 (+ 4 (/ (- r g) c)))))))
    (when (< hue 0)
      (setf hue (+ hue 360)))
    (make-color
     :hsv
     :hue hue
     :saturation
     (cond
       ((zerop c-max) 0)
       (t (/ c c-max)))
     :value c-max)))

(defmethod convert-color (color (from-color-model (eql :hsv))
			          (to-color-model (eql :rgb)))
  ;; Assuming that hue is [0…360] and v & s are [0…1]
  (let ((h (color-component color :hue))
	(s (color-component color :saturation))
	(v (color-component color :value)))
    (flet ((f (n)
	     (let ((k (mod (+ n (/ h 60)) 6)))
	       (- v (* v s (max (min k (- 4 k) 1) 0))))))
      (make-color :rgb
		  :red   (f 5)
		  :green (f 3)
		  :blue  (f 1)))))

(defmethod convert-color (color (from-color-model (eql :hsv))
			          (to-color-model (eql :rgb8)))
  (convert-color (convert-color color :hsv :rgb) :rgb :rgb8))

(defmethod convert-color (color (from-color-model (eql :rgb8))
			          (to-color-model (eql :hsv)))
  (convert-color (convert-color color :rgb8 :rgb) :rgb :hsv))

;; Also, fuck color model patents.

(register-color-model :hsv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HSL model - Hue, saturation, and lightness, components stored as Lisp numbers.
;;

(defmethod color-model-components ((color-model (eql :hsl)))
  '(:hue :saturation :lightness))

(defmethod color-model-component ((color-model (eql :hsl)) color component-name)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      ((:hue :h)        (svref color (+ 0 i)))
      ((:saturation :s) (svref color (+ 1 i)))
      ((:lightness :l)  (svref color (+ 2 i)))
      (t
       (error "There's no color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod set-color-model-component ((color-model (eql :hsl))
				      color component-name value)
  (let ((i (if (symbolp (svref color 0)) 1 0)))
    (case component-name
      ((:hue :h)        (setf (svref color (+ 0 i)) value))
      ((:saturation :s) (setf (svref color (+ 1 i)) value))
      ((:lightness :l)  (setf (svref color (+ 2 i)) value))
      (t
       (error "There's No color component ~a in the ~a color-model."
	      component-name color-model)))))

(defmethod make-color ((color-model (eql :hsl)) &key hue saturation lightness)
  (vector color-model hue saturation lightness))

(defmethod convert-color (color (from-color-model (eql :rgb))
			          (to-color-model (eql :hsl)))
  ;; Assuming that the rgb components are in the range [0…1]
  (let* ((r (color-component color :red))
	 (g (color-component color :green))
	 (b (color-component color :blue))
	 (c-max (max r g b))
	 (c-min (min r g b))
	 (c (- c-max c-min))
	 (hue (cond
		((= c-max c-min) 0)
		((= c-max r) (* 60 (/ (- g b) c)))
		((= c-max g) (* 60 (+ 2 (/ (- b r) c))))
		((= c-max b) (* 60 (+ 4 (/ (- r g) c)))))))
    (when (< hue 0)
      (setf hue (+ hue 360)))
    (make-color
     :hsl
     :hue hue
     :saturation
     (cond
       ((zerop c-max) 0)
       ((= c-min 1) 0)
       (t
	(/ c (- 1 (abs (1- (+ c-max c-min)))))))
     :lightness (/ (+ c-max c-min) 2))))

(defmethod convert-color (color (from-color-model (eql :hsl))
			          (to-color-model (eql :rgb)))
  ;; Assuming that hue is [0…360] and v & s are [0…1]
  (let ((h (color-component color :hue))
	(s (color-component color :saturation))
	(l (color-component color :lightness)))
    (flet ((f (n)
	     (let ((k (mod (+ n (/ h 30)) 12))
		   (a (* s (min l (- 1 l)))))
	       (- l (* a (max (min (- k 3) (- 9 k) 1) -1))))))
      (make-color :rgb
		  :red	  (f 0)
		  :green  (f 8)
		  :blue   (f 4)))))

(defmethod convert-color (color (from-color-model (eql :hsl))
			          (to-color-model (eql :rgb8)))
  (convert-color (convert-color color :hsl :rgb) :rgb :rgb8))

(defmethod convert-color (color (from-color-model (eql :rgb8))
			          (to-color-model (eql :hsl)))
  (convert-color (convert-color color :rgb8 :rgb) :rgb :hsl))

(register-color-model :hsl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @@@ I wish we could detect how many bits are needed and adjust appropriately,
;; but I'm not sure how to do that with floating point.

(defun color-to-xcolor (color &key (bits 8) integer-p)
  "Return a string in XParseColor format for a color with the given RED, BLUE,
and GREEN, components. Default to 8 bit color."
  (let* ((c (convert-color-to color :rgb))
	 (r  (color-component c :red))
	 (g  (color-component c :green))
	 (b  (color-component c :blue))
	 (l (list r g b)))
    (cond
      ((and (not integer-p) (every #'floatp l))
       (format nil "rgbi:~f/~f/~f" r g b))
      ;; ((every #'integerp l)
      ((every #'numberp l)
       (let (fmt ri gi bi)
	 (setf ri (component-to-bits r bits)
	       gi (component-to-bits g bits)
	       bi (component-to-bits b bits)
	       fmt (case bits
		     (4  "~x")
		     (8  "~2,'0x")
		     (12 "~3,'0x")
		     (16 "~4,'0x")
		     (t (error "Bad color bit magnitudes: ~s" bits))))
	   (format nil (s+ "rgb:" fmt "/" fmt "/" fmt) ri gi bi)))
      (t
       (error "Bad color formats: ~s" l)))))

#|
  rgb:<red>/<green>/<blue>
  <red>, <green>, <blue> := h | hh | hhh | hhhh
  h := single hexadecimal digits (case insignificant)
  The component value is scaled.

  #RGB            (4 bits each)
  #RRGGBB         (8 bits each)
  #RRRGGGBBB      (12 bits each)
  #RRRRGGGGBBBB   (16 bits each)
  The component values specify the high bits of a 16 bit value.

  rgbi:<red>/<green>/<blue>
  Floating-point values between 0.0 and 1.0, inclusive.
  of the format something like: [-+]*[0-9]*[.]*[0-9]+[[eE][-+]*[0-9]+]

  Or device-independent specifications:

  CIEXYZ:<X>/<Y>/<Z>
  CIEuvY:<u>/<v>/<Y>
  CIExyY:<x>/<y>/<Y>
  CIELab:<L>/<a>/<b>
  CIELuv:<L>/<u>/<v>
  TekHVC:<H>/<V>/<C>
|#

(defparameter *hex-scanners*
  (mapcar #'ppcre:create-scanner
	  '("#([0-9A-Fa-f]{4})([0-9A-Fa-f]{4})([0-9A-Fa-f]{4})"
	    "#([0-9A-Fa-f]{3})([0-9A-Fa-f]{3})([0-9A-Fa-f]{3})"
	    "#([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})"
	    "#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])"))
  "List of regular expression scanners for hex colors.")

(defparameter *rgb-scanner*
  (ppcre:create-scanner "rgb:([0-9A-Fa-f]+)/([0-9A-Fa-f]+)/([0-9A-Fa-f]+)"))

(defparameter *rgbi-scanner*
  (ppcre:create-scanner (let ((num "([-+]*[0-9]*[.]?[0-9]+([eE][-+]*[0-9]+)?)"))
			  (s+ "rgbi:" num "/" num "/" num))))

;; @@@ Consdier how best to add HTML and CSS color parsing, which has a slightly
;; different syntax. rgb(r, g, b) vs rgb:(r/g/b), and also alpha.

(defun xcolor-to-color (string)
  "Return an :RGB color from a XParseColor format string. Doesn't
handle device independant color spaces yet, e.g. CIELab."
  (flet ((device-to-intensity (c n)
	   "Convert a scaled device color of N hex digits to a 0 - 1 intensity."
	   (/ c (1- (expt 16 n)))))
    (cond
      ((begins-with "rgb:" string)
       (multiple-value-bind (begin end starts ends)
	   (ppcre:scan *rgb-scanner* string)
	 (when (and (not (zerop begin)) (/= (length string) end))
	   (error "Junk in an rgb color string: ~s." string))
	 (when (or (/= (length starts) 3) (/= (length ends) 3))
	   (error "Not enough colors in rgb color string: ~s." string))
	 (apply
	  #'vector :rgb
	  (loop :for i :from 0 :to 2
	     :collect (device-to-intensity
		       (parse-integer string
				     :start (elt starts i)
				     :end (elt ends i)
				     :radix 16)
		       (- (elt ends i) (elt starts i)))))))
      ((begins-with "rgbi:" string)
       (multiple-value-bind (begin end starts ends)
	   (ppcre:scan *rgbi-scanner* string)
	 (when (and (not (zerop begin)) (/= (length string) end))
	   (error "Junk in an rgbi color string: ~s." string))
	 (when (or (/= (length starts) 6) (/= (length ends) 6))
	   (error "Not enough colors in rgbi color string: ~s." string))
	 (apply #'vector :rgb
		(loop :for i :from 0 :to 2
		   :collect (safe-read-from-string string nil nil
						   :start (elt starts (* i 2))
						   :end (elt ends (* i 2)))))))
      ((begins-with "#" string)
       (when (not (position (1- (length string)) #(3 6 9 12)))
	 (error "Inappropriate length of # color string: ~s." string))
       (let (begin end starts ends)
	 (loop :for exp :in *hex-scanners*
	    :until (multiple-value-setq (begin end starts ends)
		     (ppcre:scan exp string)))
	 (when (and (not (zerop begin)) (/= (length string) end))
	   (error "Junk in an # color string: ~s." string))
	 (when (or (/= (length starts) 3) (/= (length ends) 3))
	   (error "Not enough colors in # color string: ~s." string))
	 (apply #'vector :rgb
		(loop :for i :from 0 :to 2
		   :collect
		   (device-to-intensity
		    (parse-integer string
				   :start (elt starts i)
				   :end (elt ends i)
				   :radix 16)
		    (- (elt ends i) (elt starts i)))))))
      (t
       (error "Can't parse the color string ~s." string)))))

;; EOF
