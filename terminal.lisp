;;
;; terminal.lisp - The end of the line.
;;

;; TODO:
;;  - I think I want to this to have an implicit argument interface, like grout.
;;  - Why doesn't terminal-get-size set the size slots?!?

(defpackage :terminal
  (:documentation "The end of the line.")
  (:use :cl :opsys)
  (:export
   #:*standard-output-has-terminal-attributes*
   #:*terminal*
   #:*default-terminal-type*
   #:has-terminal-attributes
   #:terminal-default-device-name
   #:terminal-stream
   #:terminal
   #:terminal-file-descriptor #:file-descriptor
   #:terminal-device-name     #:device-name
   #:terminal-output-stream   #:output-stream
   #:terminal-window-rows     #:window-rows
   #:terminal-window-columns  #:window-columns
   #:terminal-get-size
   #:terminal-get-cursor-position
   #:terminal-start
   #:terminal-end
   #:terminal-done
   #:make-terminal-stream
   #:with-terminal
   #:tt-format
   #:tt-write-string
   #:tt-write-char
   #:tt-move-to
   #:tt-move-to-col
   #:tt-beginning-of-line
   #:tt-del-char
   #:tt-ins-char
   #:tt-backward
   #:tt-forward
   #:tt-up
   #:tt-down
   #:tt-scroll-down
   #:tt-erase-to-eol
   #:tt-erase-line
   #:tt-erase-above
   #:tt-erase-below
   #:tt-clear
   #:tt-home
   #:tt-cursor-off
   #:tt-cursor-on
   #:tt-standout
   #:tt-normal
   #:tt-underline
   #:tt-bold
   #:tt-inverse
   #:tt-color
   #:tt-beep
   #:tt-set-scrolling-region
   #:tt-finish-output
   #:tt-get-char
   #:tt-get-key
   #:tt-listen-for
   #:tt-reset
   #:tt-save-cursor
   #:tt-restore-cursor
   #:with-saved-cursor
   ))
(in-package :terminal)

(declaim (optimize (debug 3)))

(defvar *standard-output-has-terminal-attributes* nil
  "True if we want programs to treat *standard-output* like it can display
terminal attributes.")

(defvar *terminal* nil
  "The default terminal to use for I/O.")

(defvar *default-terminal-type* nil
  "The type of terminal to create when unspecified.")

(defclass terminal-stream ()
  ((output-stream
    :accessor terminal-output-stream
    :initarg :output-stream
    :documentation "Lisp stream for output."))
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal (terminal-stream)
  ((file-descriptor
    :accessor terminal-file-descriptor
    :initarg :file-descriptor
    :documentation "System file descriptor.")
   (device-name
    :accessor terminal-device-name
    :initarg :device-name
    :documentation "System device name.")
   (window-rows
    :accessor terminal-window-rows
    :initarg :window-rows
    :documentation "Number of rows of characters in the window.")
   (window-columns
    :accessor terminal-window-columns
    :initarg :window-columns
    :documentation "Number of columns of characters in the window.")
   )
  (:default-initargs
    :file-descriptor		nil
    :device-name		*default-console-device-name*
    :output-stream		nil
  )
  (:documentation "What we need to know about terminal device."))

(defun has-terminal-attributes (stream)
  "Return true if we should treat `STREAM` as if it has terminal attributes."
  (or (and (eq stream *standard-output*)
	   *standard-output-has-terminal-attributes*)
      (let ((ss (stream-system-handle stream)))
	(and ss (file-handle-terminal-p ss)))))

(defgeneric terminal-default-device-name (type)
  (:documentation "Return the default device name that would be picked if we
made a terminal of the given TYPE."))

(defgeneric terminal-get-size (terminal)
  (:documentation "Get the window size."))

(defgeneric terminal-get-cursor-position (terminal)
  (:documentation
   "Try to somehow get the row of the screen the cursor is on. Returns the
two values ROW and COLUMN."))

(defgeneric terminal-start (terminal)
  (:documentation
   "Set up the terminal for reading a character at a time without echoing."))

(defgeneric terminal-end (terminal)
  (:documentation
   "Put the terminal back to the way it was before we called terminal-start."))

(defgeneric terminal-done (terminal)
  (:documentation "Forget about the whole terminal thing and stuff."))

;; (defmacro with-terminal-stream ((var stream) &body body)
;;   "Evaluate the body with VAR set to a new terminal-stream."
;;   `(let ((,var (make-instance 'terminal-stream :output-stream ,stream)))
;;      (unwind-protect
;; 	  (progn
;; 	    ,@body)
;;        (terminal-done ,var))))

(defun make-terminal-stream (stream type)
  (make-instance type :output-stream stream))

(defmacro with-terminal ((&optional (var '*terminal*)
				    (type *default-terminal-type*)
				    device-name)
			 &body body)
  "Evaluate the body with VAR set to a new terminal. Cleans up afterward."
  `(progn
     (when (not (find-class ,type))
       (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
     (let ((,var (if ,device-name
		   (make-instance ,type :device-name ,device-name)
		   (make-instance ,type))))
       (unwind-protect
	    (progn
	      (terminal-start ,var)
	      ,@body)
	 (terminal-done ,var)))))

(defgeneric tt-format (tty fmt &rest args)
  (:documentation "Output a formatted string to the terminal."))

(defgeneric tt-write-string (tty str)
  (:documentation "
Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is \"line buffered\""))

(defgeneric tt-write-char (tty char)
  (:documentation "
Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is \"line buffered\""))

(defgeneric tt-move-to (tty row col))
(defgeneric tt-move-to-col (tty col))
(defgeneric tt-beginning-of-line (tty))
(defgeneric tt-del-char (tty n))
(defgeneric tt-ins-char (tty n))
(defgeneric tt-backward (tty n))
(defgeneric tt-forward (tty n))
(defgeneric tt-up (tty n))
(defgeneric tt-down (tty n))
(defgeneric tt-scroll-down (tty n))
(defgeneric tt-erase-to-eol (tty))
(defgeneric tt-erase-line (tty))
(defgeneric tt-erase-above (tty))
(defgeneric tt-erase-below (tty))
(defgeneric tt-clear (tty))
(defgeneric tt-home (tty))
(defgeneric tt-cursor-off (tty))
(defgeneric tt-cursor-on (tty))
(defgeneric tt-standout (tty state))
(defgeneric tt-normal (tty))
(defgeneric tt-underline (tty state))
(defgeneric tt-bold (tty state))
(defgeneric tt-inverse (tty state))
(defgeneric tt-color (tty fg bg))
(defgeneric tt-beep (tty))
(defgeneric tt-set-scrolling-region (tty start end))
(defgeneric tt-finish-output (tty))
; (defgeneric tt-get-row (tty))

(defgeneric tt-get-char (tty)
  (:documentation "Read a character from the terminal."))

(defgeneric tt-get-key (tty)
  (:documentation "Read a key from the terminal."))

(defgeneric tt-listen-for (tty seconds)
  (:documentation
  "Listen for at most N seconds or until input is available. SECONDS can be
fractional, down to some limit."))

(defgeneric tt-reset (tty)
  (:documentation
   "Try to reset the terminal to a sane state, without being too disruptive."))

(defgeneric tt-save-cursor (tty)
  (:documentation "Save the cursor position."))

(defgeneric tt-restore-cursor (tty)
  (:documentation "Restore the cursor position, from the last saved postion."))

(defmacro with-saved-cursor ((tty) &body body)
  "Save the cursor position, evaluate the body forms, and restore the cursor
position. Return the primary result of evaluating the body."
  (let ((result-sym (gensym "tt-result")))
    `(progn
       (tt-save-cursor ,tty)
       (let (,result-sym)
	 (unwind-protect
	      (setf ,result-sym (progn ,@body))
	   (tt-restore-cursor ,tty))
	 ,result-sym))))

#| @@@@ Make an output-table method, with underlined titles

(defun print-col (tt n v &key no-space)
  "Print column number N with value V."
  (let* ((col   (elt *cols* n))
	 (width (second col))
	 (left  (eql (third col) :left))
	 (fmt   (if width (if left "~va" "~v@a") "~a")))
    (if width
	(tt-format tt fmt width (subseq v 0 (min width (length v))))
	(tt-format tt fmt v))
    (if (= n (1- (length *cols*)))
	(tt-write-char tt #\newline)
	(when (not no-space)
	  (tt-write-char tt #\space)))))

(defun print-title (tt n)
  (tt-underline tt t)
  (print-col tt n (first (elt *cols* n)) :no-space t)
  (tt-underline tt nil)
  (when (< n (1- (length *cols*)))
    (tt-write-char tt #\space)))
  )

(defmethod output-table ((table table) (destination terminal-stream)
			 &key long-titles column-names)
  )
|# 

;; EOF
