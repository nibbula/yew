;;;
;;; ansi.lisp - Generic ANSI stuff.
;;;

(defpackage :ansi
  (:documentation "Generic ANSI stuff.

Things that can apply to various things using ANSI terminal protocol, including
modern xterm style functionality.")
  (:use :cl :dlib :terminal)
  (:export
   ;; Character sequences
   #:+csi+
   #:+st+
   #:+osc+
   #:+dcs+
   #:*attributes*
   #:*attributes-off*
   #:*colors*
   ;; keys
   #:*key-tag*
   #:*key-num*
   ))
(in-package :ansi)

(define-constant +csi+ (s+ #\escape #\[)
  "Control Sequence Introducer. Hooking up control sequences since 1970.")
(define-constant +st+  (s+ #\escape #\\)
  "String terminator. Death to strings.")
(define-constant +osc+ (s+ #\escape #\])
  "Operating System Command. C'est vrai? o_O")
(define-constant +dcs+ (s+ #\escape #\p)
  "Device Control String")

(defparameter *attributes*
  '((:normal	       . 22)		; not bold or faint
    (:bold	       . 1)
    (:faint	       . 2)
    (:dim	       . 2)
    (:italic	       . 3)
    (:underline	       . 4)
    (:blink	       . 5)
    (:inverse	       . 7)
    (:reverse	       . 7)
    (:standout	       . 7)
    (:invisible	       . 8)
    (:crossed-out      . 9)
    (:double-underline . 21)))

(defparameter *attributes-off*
  '((:all	       . 0)		; No attributes
    (:bold	       . 22)
    (:faint	       . 22)
    (:dim	       . 22)
    (:italic	       . 23)
    (:underline	       . 24)
    (:blink	       . 25)
    (:inverse	       . 27)
    (:reverse	       . 27)
    (:standout	       . 27)
    (:invisible	       . 28)
    (:crossed-out      . 29)
    (:double-underline . 24)))		; same as not underline

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defparameter *key-tag*
  '((#\A . :up) 			; Arrow keys
    (#\B . :down)
    (#\C . :right)
    (#\D . :left)
    (#\E . :center)			; center of the keypad
    (#\F . :end)
    (#\H . :home)			; Movement keys
    (#\P . :f1)				; function keys
    (#\Q . :f2)
    (#\R . :f3)
    (#\S . :f4)
    (#\Z . :back-tab)			; non-standard
    ))

(defparameter *key-num*
  '((1  . :home)			; linux console
    (2  . :insert)			; Editing keys
    (3  . :delete)
    (4  . :end)				; linux console
    (5  . :page-up)
    (6  . :page-down)
    (15 . :f5)				; Function keys
    (17 . :f6)
    (18 . :f7)
    (19 . :f8)
    (20 . :f9)
    (21 . :f10)
    (23 . :f11)
    (24 . :f12)
    (200 . :bracketed-paste)))

;; End
