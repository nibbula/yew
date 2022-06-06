;;;
;;; ansi.lisp - Generic ANSI stuff.
;;;

(defpackage :ansi
  (:documentation "Generic ANSI stuff.

Things that can apply to various things using ANSI terminal protocol, including
modern xterm style functionality.")
  (:use :cl :terminal)
  (:export
   ;; Character sequences
   #:+csi+
   #:+st+
   #:+osc+
   #:+dsc+
   #:*attributes*
   #:*attributes-off*
   ;; 
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

;; End
