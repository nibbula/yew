;;;
;;; terminal-dumb-color.lisp - Dumb terminal with text effects and color.
;;;

(defpackage :terminal-dumb-color
  (:documentation "Dumb color terminal")
  (:use :cl :dlib :terminal :char-util :trivial-gray-streams :fatchar
	:terminal-dumb :terminal-ansi)
  (:export
   #:terminal-dumb-color-stream
   #:terminal-dumb-color
   ))
(in-package :terminal-dumb-color)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

(defparameter *dumb-color-height* nil #| 240000 |#)
(defparameter *dumb-color-width* nil #| 8000 |#)

;; We inherit most methods from terminal-dumb. The color methods are from
;; the color-mixin in terminal-ansi.

(defclass terminal-dumb-color (terminal-color-mixin terminal-dumb)
  ()
  (:default-initargs
   :output-stream *standard-output*
   :input-stream *standard-input*
   :window-rows *dumb-color-height*
   :window-columns *dumb-color-width*
   )
  (:documentation "A fake color terminal just using standard Lisp streams."))

(defmethod terminal-default-device-name ((type (eql 'terminal-dumb-color)))
  "Return the default device name for a TERMINAL-DUMB-COLOR."
  "dumb-color")

(defmethod initialize-instance
    :after ((o terminal-dumb-color) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-dumb-color."
  (declare (ignore initargs)))

;; We have no idea what the size is. But it's big.

(defmethod terminal-get-size ((tty terminal-dumb-color))
  "Get the window size from the kernel and store it in tty."
  (setf (terminal-window-rows tty) *dumb-color-height*
	(terminal-window-columns tty) *dumb-color-width*))

(defmethod terminal-get-cursor-position ((tty terminal-dumb-color))
  "Try to somehow get the row of the screen the cursor is on."
  ;;(values nil nil)
  (values (terminal-window-rows tty)
	  (terminal-window-columns tty)))

(defmethod terminal-ansi::cached-color-count ((tty terminal-dumb-color))
  ;; @@@ This is not your mother's cached-color-count.
  nil)

(defmethod terminal-colors ((tty terminal-dumb-color))
  ;; For a stream, we don't have a back channel, so we can't ask the terminal.
  ;; We could try (string-equal (nos:env "COLORTERM") "truecolor")
  ;; or a bunch of other bullcrap from the environment, but for now,
  ;; just try to respect FORCE_COLOR turning it off,
  ;; and assume we do have full color.
  (cond
    ((let ((fc (nos:env "FORCE_COLOR")))
       (or (equal fc "0")
	   (string-equal fc "false")))
     0)
    (t ;; Assume the most, because so what.
     (* 256 256 256))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :dumb-color 'terminal-dumb-color)

;; EOF
