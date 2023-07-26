;;;
;;; resize.lisp - Resize the terminal.
;;;

(defpackage :resize
  (:documentation "Resize the terminal.")
  (:use :cl :terminal :char-util :keymap :inator :terminal-inator :fui)
  (:export
   #:resize-terminal
   #:resize-interactively
   #:!resize
   ))
(in-package :resize)

(defkeymap *resizer-keymap* ()
  `("Resizer"
    (#\q     . quit)
    (#\?     . help)
    (:up     . increase-height)
    (:down   . decrease-height)
    (:right  . increase-width)
    (:left   . decrease-width)
    (#\k     . increase-height)
    (#\j     . decrease-height)
    (#\l     . increase-width)
    (#\h     . decrease-width)
    ))

(defclass resizer (terminal-inator)
  ()
  (:default-initargs
   :default-keymap *resizer-keymap*)
  (:documentation "Interactive terminal resizer."))

(defvar *resizer* nil
  "The current resizer app.")

(defun increase-height (o)
  "Increate the height."
  (declare (ignore o))
  (incf (tt-height)))

(defun decrease-height (o)
  "Decrease the height."
  (declare (ignore o))
  (decf (tt-height)))

(defun increase-width (o)
  "Increase the width."
  (declare (ignore o))
  (incf (tt-width)))

(defun decrease-width (o)
  "Decrease the width."
  (declare (ignore o))
  (decf (tt-width)))

(defun center (text &optional (offset 0))
  (tt-format-at (+ (truncate (tt-height) 2) offset)
		(- (truncate (tt-width) 2)
		   (truncate (display-length text) 2))
		text))

(defmethod update-display (o)
  (declare (ignore o))
  (tt-home)
  (tt-clear)
  ;; (tt-erase-below)
  (draw-box 0 0 (tt-width) (tt-height))
  (draw-eks (tt-width) (tt-height))
  (center "Press ? for help" 1)
  (center (format nil "~d x ~d" (tt-width) (tt-height)))
  (tt-finish-output))

(defun resize-interactively ()
  (with-terminal-inator (*resizer* 'resizer)
    (with-enabled-events (:resize)
      (event-loop *resizer*))))

(defun resize-terminal (&key old width height interactive)
  "Resize the window or get the window size."
  (when (not terminal:*terminal*)
    (error "You have to run this in a terminal."))
  (cond
    (old
     (format t "COLUMNS=~d;~%LINES=~d;~%export COLUMNS LINES;~%"
	     (tt-width) (tt-height)))
    (interactive
     (resize-interactively))
    ((and (not width) (not height))
     (multiple-value-bind (h w) (terminal-get-size *terminal*)
       (format t "~s ~s~%" w h)))
    ((not height)
     (setf (tt-width) width))
    (t
     (setf (tt-size) `(,height ,width))))
  (list (tt-width) (tt-height)))


;; End
