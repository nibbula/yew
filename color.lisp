;;
;; color.lisp - you are this
;;

(defpackage :color
  (:documentation "What you see is not even real.")
  (:use :cl :dlib :cl-ppcre)
  (:export
   #:*default-color-model*
   #:color-model-name
   #:color-model-component
   #:color-component
   #:set-color-component
   #:convert-color
   #:make-color
   #:*color-models*
   #:register-color-model
   #:color-to-xcolor
   #:xcolor-to-color
   ))
(in-package :color)

(defparameter *simple-colors*
  '(:black :red :green :yellow :blue :magenta :cyan :white)
  "Simple color names.")

;; Colors are stored as:
;;
;; (color-model number [number...])
;; where color-model is a name defining the color model,
;; and the numbers define values in that model
;; OR
;; (number [number...])
;; Values in the current color model, which shoud probably default to:
;; standard RGB, or maybe R G B A
;;
;; Some standard color names should be defined as keywords.
;;
;; Note that a color model is not a color space. A color model just says
;; what color components we store, not a precise definition of how to render,
;; or what exactly that means in terms of light and/or pigments and/or a
;; specific device, e.g. and ICC color profile or something. Although, the color
;; model can affect the color space we can represent.

;; @@@ perhaps I should use a defstruct rigamarole for this?
;; like defstruct with :type vector?

(defvar *default-color-model* :rgb)

(defun color-model-name (color)
  "Return the color model name of a color."
  (assert (vector color))
  (if (symbolp (aref color 0))
      (aref color 0)
      *default-color-model*))

(defgeneric color-model-component (color-model color component-name)
  (:documentation
   "Return the component COMPONENT-NAME of the color of the COLOR-MODEL."))

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

(defsetf color-component set-color-compoent)

;; The from-color-model is separated out so we can specialize on it.
(defgeneric convert-color (color from-color-model to-color-model)
  (:documentation "Return the COLOR converted to the TO-COLOR-MODEL."))

(defun convert-color-to (color to-color-model)
  (convert-color color (color-model-name color) to-color-model))

(defgeneric make-color (color-model &key &allow-other-keys)
  (:documentation
   "Make a color of the COLOR-MODEL with the given components."))

;; We should probably support:
;; gray, rgb, rgba, hsv, hsl, cmyk, and perhaps yuv

(defvar *color-models* nil
  "A list of known color models.")

(defun register-color-model (model-name)
  (pushnew model-name *color-models*))

;;
;; The RGB color model:
;;

(defmethod color-model-component ((color-model (eql :rgb)) color component-name)
  (case component-name
    (:red   (svref color 1))
    (:green (svref color 2))
    (:blue  (svref color 3))
    (t
     (error "There's no color component ~a in the ~a color-model."
	    component-name color-model))))

(defmethod set-color-model-component ((color-model (eql :rgb))
				      color component-name value)
  (case component-name
    (:red   (setf (svref color 1) value))
    (:green (setf (svref color 2) value))
    (:blue  (setf (svref color 3) value))
    (t
     (error "There's No color component ~a in the ~a color-model."
	    component-name color-model))))

(defmethod make-color ((color-model (eql :rgb)) &key red green blue)
  (vector color-model red green blue))

(defmethod convert-color (color (from-color-model (eql :rgb))
			          (to-color-model (eql :gray)))
  (make-color :gray :value
	      (/ (+ (color-component color :red)
		    (color-component color :green)
		    (color-component color :blue))
		 3)))

(defmethod convert-color (color (from-color-model (eql :gray))
			          (to-color-model (eql :rgb)))
  (make-color :rgb
	      :red   (color-component color :value)
	      :green (color-component color :value)
	      :blue  (color-component color :value)))

(register-color-model :rgb)

;;
;; The RGBA color model:
;;

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

(defun color-to-xcolor (color &key bits)
  "Return a string in XParseColor format for a color with the given RED, BLUE,
and GREEN, components. Default to 8 bit color. If values are over 8 bits,
default to 16 bit color."
  (let* ((c (convert-color-to color :rgb))
	 (r (color-component c :red))
	 (g (color-component c :green))
	 (b (color-component c :blue))
	 (l (list r g b)))
    (cond
      ((every #'floatp l)
       (format nil "rgbi:~f/~f/~f" r g b))
      ((every #'integerp l)
       (let (fmt)
	 (when (not bits)
	   (setf bits (if (some (_ (> _ #xff)) l) 16 8)))
	 (setf fmt
	       (case bits
		 (4  "~x")
		 (8  "~2,'0x")
		 (12 "~3,'0x")
		 (16 "~4,'0x")
		 (t (error "Bad color bit magnitudes: ~s" l))))
	 (format nil (s+ "rgb:" fmt "/" fmt "/" fmt) r g b)))
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
  The compoent values specify the high bits of a 16 bit value.

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

(defun parse-color (string)
  "Return an :RGB color from a XParseColor format string. Doesn't
handle device independant color spaces yet, e.g. CIELab."
  (cond
    ((begins-with "rgb:" string)
     (multiple-value-bind (begin end starts ends)
	 (ppcre:scan "rgb:([0-9A-Fa-f]+)/([0-9A-Fa-f]+)/([0-9A-Fa-f]+)" string)
       (when (and (not (zerop begin)) (/= (length string) end))
	 (error "Junk in an rgb color string: ~s." string))
       (when (or (/= (length starts) 3) (/= (length ends) 3))
	 (error "Not enough colors in rgb color string: ~s." string))
       (apply
	#'vector :rgb
	(loop :for i :from 0 :to 2
	   :collect (parse-integer string
				   :start (elt starts i)
				   :end (elt ends i)
				   :radix 16)))))
    ((begins-with "rgbi:" string)
     (multiple-value-bind (begin end starts ends)
	 (ppcre:scan (let ((num "([-+]*[0-9]*[.]?[0-9]+([eE][-+]*[0-9]+)?)"))
		       (s+ "rgbi:" num "/" num "/" num))
		     string)
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
       (loop :for exp :in '("#([0-9A-Fa-f]{4})([0-9A-Fa-f]{4})([0-9A-Fa-f]{4})"
			    "#([0-9A-Fa-f]{3})([0-9A-Fa-f]{3})([0-9A-Fa-f]{3})"
			    "#([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})([0-9A-Fa-f]{2})"
			    "#([0-9A-Fa-f])([0-9A-Fa-f])([0-9A-Fa-f])")
	  :until (multiple-value-setq (begin end starts ends)
		   (ppcre:scan exp string)))
       (when (and (not (zerop begin)) (/= (length string) end))
	 (error "Junk in an # color string: ~s." string))
       (when (or (/= (length starts) 3) (/= (length ends) 3))
	 (error "Not enough colors in # color string: ~s." string))
       (apply #'vector :rgb
	      (loop :for i :from 0 :to 2
		 :collect (parse-integer string
					 :start (elt starts i)
					 :end (elt ends i)
					 :radix 16)))))
    (t
     (error "Can't parse the color string ~s." string))))

;; EOF
