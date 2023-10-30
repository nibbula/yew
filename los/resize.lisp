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
    "Resizing"
    (:up     . increase-height)
    (:down   . decrease-height)
    (:right  . increase-width)
    (:left   . decrease-width)
    (#\h     . decrease-width)
    (#\j     . increase-height)
    (#\k     . decrease-height)
    (#\l     . increase-width)
    (#\H     . decrease-width-some)
    (#\J     . increase-height-some)
    (#\K     . decrease-height-some)
    (#\L     . increase-width-some)
    (#\i     . increment-increment)
    (#\I     . decrement-increment)
    ))

;; @@@ defkeymap should be able to do this
(set-key `(,(ctrl #\x) ,(ctrl #\r)) 'revert *resizer-keymap*)
(set-key `(#\esc #\p) 'decrease-height-some *resizer-keymap*)
(set-key `(#\esc #\n) 'increase-height-some *resizer-keymap*)

(defclass resizer (terminal-inator)
  ((increment
    :initarg :increment :accessor resizer-increment :type integer :initform 5
    :documentation "The amount to resize by.")
   (message
    :initarg :message :accessor resizer-message :initform nil
    :documentation "A message to display.")
   (starting-size
    :initarg :starting-size :accessor starting-size :initform nil
    :documentation "Starting size as a cons of (width . height)"))
  (:default-initargs
   :default-keymap *resizer-keymap*
   :starting-size (cons (tt-width) (tt-height)))
  (:documentation "Interactive terminal resizer."))

(defvar *resizer* nil
  "The current resizer app.")

(defun increase-height (o &optional (cells 1))
  "Increase the height."
  (declare (ignore o))
  (incf (tt-height) cells))

(defun increase-height-some (o)
  "Increase the height by the increment."
  (increase-height o (resizer-increment o)))

(defun decrease-height (o &optional (cells 1))
  "Decrease the height."
  (declare (ignore o))
  (decf (tt-height) cells))

(defun decrease-height-some (o)
  "Decrease the height by the increment."
  (decrease-height o (resizer-increment o)))

(defun increase-width (o &optional (cells 1))
  "Increase the width."
  (declare (ignore o))
  (incf (tt-width) cells))

(defun increase-width-some (o)
  "Increase the width by the increment."
  (increase-width o (resizer-increment o)))

(defun decrease-width (o &optional (cells 1))
  "Decrease the width."
  (declare (ignore o))
  (decf (tt-width) cells))

(defun decrease-width-some (o)
  "Decrease the width by the increment."
  (decrease-width o (resizer-increment o)))

(defun increment-increment (o)
  "Increase the default increment for other functions."
  (incf (resizer-increment o))
  (message o "Increment: ~s" (resizer-increment o)))

(defun decrement-increment (o)
  "Decrease the default increment for other functions."
  (decf (resizer-increment o))
  (message o "Increment: ~s" (resizer-increment o)))

(defmethod next              ((o resizer)) (increase-height o))
(defmethod previous          ((o resizer)) (decrease-height o))
(defmethod forward-unit      ((o resizer)) (increase-width o))
(defmethod backward-unit     ((o resizer)) (decrease-width o))
(defmethod forward-multiple  ((o resizer)) (increase-width-some o))
(defmethod backward-multiple ((o resizer)) (decrease-width-some o))

(defun center (text &optional (offset 0))
  (tt-format-at (+ (truncate (tt-height) 2) offset)
		(- (truncate (tt-width) 2)
		   (truncate (display-length text) 2))
		text))

(defmethod message ((o resizer) format-string &rest args)
  (setf (resizer-message o) (apply #'format nil format-string args)))

(defmethod accept ((o resizer))
  (inator:quit o))

(defun revert (o)
  "Revert to the original size."
  (setf (tt-width) (car (starting-size o))
	(tt-height) (cdr (starting-size o))))

(defmethod update-display (o)
  (tt-home)
  (tt-clear)
  ;; (tt-erase-below)
  (draw-box 0 0 (tt-width) (tt-height))
  (center "Press ? for help" 1)
  (center (format nil "~d x ~d" (tt-width) (tt-height)))
  (when (resizer-message o)
    (center (resizer-message o) 2)
    (setf (resizer-message o) nil))
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
