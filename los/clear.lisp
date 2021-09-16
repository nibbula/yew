;;;
;;; clear.lisp - Clear the screen.
;;;

(defpackage :clear
  (:documentation "Clear the screen.")
  (:use :cl :terminal :lish)
  (:export
   #:!clear
   ))
(in-package :clear)

(defcommand clear ()
  "Clear the screen. Or you can probably just press Ctrl-L."
  (with-terminal ()
    (tt-home)
    (tt-clear)
    (tt-finish-output)))

;; End
