;;
;; ansiterm.lisp - Deal with ANSI-like terminals
;;

;; I make no further apologies.

(defpackage :ansiterm
  (:documentation "Deal with ANSI-like terminals.")
  (:use :cl :cffi :opsys :termios :table)
  (:export
   #:*standard-output-has-terminal-attributes*
   #:has-terminal-attributes
   #:terminal-stream
   #:terminal
   #:terminal-file-descriptor
   #:terminal-device-name
   #:terminal-output-stream
   #:terminal-raw-state
   #:terminal-cooked-state
   #:terminal-window-rows
   #:terminal-window-columns
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
   #:tt-clear
   #:tt-home
   #:tt-cursor-off
   #:tt-cursor-on
   #:tt-standout
   #:tt-standend
   #:tt-normal
   #:tt-underline
   #:tt-bold
   #:tt-inverse
   #:tt-color
   #:tt-beep
   #:tt-finish-output
   #:tt-get-char
   #:tt-reset
   #:with-saved-cursor
   ))
(in-package :ansiterm)

(defvar *standard-output-has-terminal-attributes* nil
  "True if we want programs to treat *standard-output* like it can display
terminal attributes.")

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
   ;; (output-stream
   ;;  :accessor terminal-output-stream
   ;;  :initarg :output-stream
   ;;  :documentation "Lisp stream for output.")
   (raw-state
    :accessor terminal-raw-state
    :initarg :terminal-raw-state
    :documentation
    "A foreign pointer to a POSIX termios struct of the terminal in its ~
     raw state.")
   (cooked-state
    :accessor terminal-cooked-state
    :initarg :terminal-cooked-state
    :documentation
    "A foreign pointer to a POSIX termios struct of the terminal in its ~
     cooked state")
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
    :device-name		"/dev/tty"
    :output-stream		nil
    :terminal-raw-state		nil
    :terminal-cooked-state	nil
  )
  (:documentation "What we need to know about terminal device."))

(defun has-terminal-attributes (stream)
  "Return true if we should treat `STREAM` as if it has terminal attributes."
  (or (and (eq stream *standard-output*)
	   *standard-output-has-terminal-attributes*)
      (let ((ss (nos:stream-system-handle stream)))
	(and ss (file-handle-terminal-p ss)))))

(defun terminal-get-size (tty)
  "Get the window size from the kernel and store it in tty."
  (with-slots (file-descriptor window-rows window-columns) tty
    (when file-descriptor
      (with-foreign-object (ws '(:struct winsize))
	(when (< (posix-ioctl file-descriptor TIOCGWINSZ ws) 0)
	  (error "Can't get terminal window size."))
	(setf
	 window-rows    (foreign-slot-value ws '(:struct winsize) 'ws_row)
	 window-columns (foreign-slot-value ws '(:struct winsize) 'ws_col))))))

;; There are two possibilities for getting this right:
;;  1. We do all output thru our routines and keep track
;;  2. We ask the terminal (and get an accurate response)
;; We really can't assume 1, nor can we reliably assume we can check if
;; any output was done (e.g. not all OS's update the access time on the
;; tty device or even have such a thing). So we have to do 2. Since hardware
;; terminals are mostly extinct, emulators should be damn well able to do this
;; by now. Curses just assumes if you do any output not thru it, you are on
;; your own, hence the necessity in most programs of a complete erase and
;; redraw user command.

(defun terminal-get-cursor-position (tty)
  "Try to somehow get the row of the screen the cursor is on."
;  (tt-report "~c[6n" #\escape) @@@@
; fake it till you make it, for now
  (tt-write-char tty #\return)
  (values 0 0)
  )		; acutally should return values ROW COL

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defun terminal-start (tty)
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots (file-descriptor device-name output-stream raw-state cooked-state
               window-rows window-columns) tty
    (when (not file-descriptor)
;      (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (posix-open device-name O_RDWR 0))
      (when (< file-descriptor 0)
	(error "Can't open ~a ~d." device-name file-descriptor)))
    (when (not cooked-state)
      (setf cooked-state (foreign-alloc '(:struct termios))))
    (when (not raw-state)
      (setf raw-state (foreign-alloc '(:struct termios))))
    (when (< (tcgetattr file-descriptor cooked-state) 0)
      (error "Can't get cooked state."))
    (when (< (tcgetattr file-descriptor raw-state) 0)
      (error "Can't get raw state."))
    (with-foreign-slots ((c_lflag c_cc) raw-state (:struct termios))
      ;; Read returns immediately only after one character read
      (setf (mem-aref c_cc :char VMIN) 1)
      (setf (mem-aref c_cc :char VTIME) 0)
      ;; Turn off canonical input, echo, and extended chars(lnext discard...)
      (setf c_lflag (logand c_lflag (lognot (logior ICANON ECHO IEXTEN)))))
    (when (not output-stream)
      (setf output-stream (open device-name :direction :output
				#-clisp :if-exists #-clisp :append)))
    (when (< (tcsetattr file-descriptor TCSANOW raw-state) 0)
;    (when (< (tcsetattr file-descriptor TCSAFLUSH raw-state) 0)
      (error "Can't set raw state. ~d ~d" nos:*errno* file-descriptor))
;    (format t "[terminal-start]~%")
    (terminal-get-size tty)))

;(defun te () (terminal-end))		; for in the debugger
(defun terminal-end (tty)
  "Put the terminal back to the way it was before we called terminal-start."
;  (format t "[terminal-end]~%")
  (when (< (tcsetattr (terminal-file-descriptor tty) TCSANOW #+null TCSAFLUSH
		      (terminal-cooked-state tty))
	   0)
    (error "Can't set cooked state.")))

(defun terminal-done (tty)
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty)
  (posix-close (terminal-file-descriptor tty))
  (foreign-free (terminal-raw-state tty))
  (foreign-free (terminal-cooked-state tty))
;  (format t "[terminal-done]~%")
;  (setf *tty* nil)
  (values))

;; (defmacro with-terminal-stream ((var stream) &body body)
;;   "Evaluate the body with VAR set to a new terminal-stream."
;;   `(let ((,var (make-instance 'terminal-stream :output-stream ,stream)))
;;      (unwind-protect
;; 	  (progn
;; 	    ,@body)
;;        (terminal-done ,var))))

(defun make-terminal-stream (stream)
  (make-instance 'terminal-stream :output-stream stream))

(defmacro with-terminal ((var &optional device-name) &body body)
  "Evaluate the body with VAR set to a new terminal. Cleans up afterward."
  `(let ((,var (if ,device-name
		   (make-instance 'terminal :device-name ,device-name)
		   (make-instance 'terminal))))
     (unwind-protect
	  (progn
	    (terminal-start ,var)
	    ,@body)
       (terminal-done ,var))))

(defgeneric tt-format (tty fmt &rest args)
  (:documentation "Output a formatted string to the terminal."))
(defmethod tt-format ((tty terminal-stream) fmt &rest args)
  (let ((string (apply #'format nil fmt args))
	(stream (terminal-output-stream tty)))
    (write-string string stream)
    (when (position #\newline string)
      (finish-output stream))))

; (defgeneric tt-report (tty end-char fmt &rest args)
;   (:documentation
; "Output a formatted string to the terminal and get an immediate report back.
; Report parameters are returned as values. Report is assumed to be in the form:
; #\escape #\[ { p1 { ; pn } } end-char"))
; (defmethod tt-report ((tty terminal) end-char fmt &rest args) 
;   (let ((stream (terminal-output-stream tty)))
;     (apply #'format stream fmt args)
;     (finish-output stream)
;     ;; The stream is assumed to be in "char at a time" mode.
;     (let ((str (read-until #\R :timeout 1))
; 	  (s ""))
;       (when (nullp str)
; 	(error "Terminal failed to report \"~a\"." fmt))
;       (concatenate 'string #\c)
;       @@@@@@)))

(defgeneric tt-write-string (tty str)
  (:documentation "
Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is \"line buffered\""))
(defmethod tt-write-string ((tty terminal-stream) str)
  (let ((stream (terminal-output-stream tty)))
    (write-string str stream)
    (when (position #\newline str)
      (finish-output stream))))

(defgeneric tt-write-char (tty char)
  (:documentation "
Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is \"line buffered\""))
(defmethod tt-write-char ((tty terminal-stream) char)
  (let ((stream (terminal-output-stream tty)))
    (write-char char stream)
    (when (eql char #\newline)
      (finish-output stream))))

(defgeneric tt-move-to (tty row col))
(defmethod tt-move-to ((tty terminal-stream) row col)
  (tt-format tty "~c[~d;~dH" #\escape (1+ row) (1+ col)))

(defgeneric tt-move-to-col (tty col))
(defmethod tt-move-to-col ((tty terminal-stream) col)
  (tt-format tty "~c[~dG" #\escape (1+ col)))

(defgeneric tt-beginning-of-line (tty))
(defmethod tt-beginning-of-line ((tty terminal-stream))
  ;; (tt-format tty "~c[G" #\escape))
  ;; How about just:
  (tt-write-char tty #\return))

(defgeneric tt-del-char (tty n))
(defmethod tt-del-char ((tty terminal-stream) n)
  (tt-format tty "~c[~aP" #\escape (if (> n 1) n "")))

(defgeneric tt-ins-char (tty n))
(defmethod tt-ins-char ((tty terminal-stream) n)
  (tt-format tty "~c[~a@" #\escape (if (> n 1) n "")))

(defgeneric tt-backward (tty n))
(defmethod tt-backward ((tty terminal-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dD" #\escape n)
	  (tt-format tty "~c[D" #\escape))))

(defgeneric tt-forward (tty n))
(defmethod tt-forward ((tty terminal-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dC" #\escape n)
	  (tt-format tty "~c[C" #\escape))))

(defgeneric tt-up (tty n))
(defmethod tt-up ((tty terminal-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dA" #\escape n)
	  (tt-format tty "~c[A" #\escape))))

(defgeneric tt-down (tty n))
(defmethod tt-down ((tty terminal-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dB" #\escape n)
	  (tt-format tty "~c[B" #\escape))))

(defgeneric tt-scroll-down (tty n))
(defmethod tt-scroll-down ((tty terminal-stream) n)
  (if (> n 0)
      (loop :with stream = (terminal-output-stream tty) and i = 0
	 :while (< i n)
	 :do (write-char #\newline stream) (incf i)
	 :finally (finish-output stream))))

(defgeneric tt-erase-to-eol (tty))
(defmethod tt-erase-to-eol ((tty terminal-stream))
  (tt-format tty "~c[K" #\escape))

(defgeneric tt-erase-line (tty))
(defmethod tt-erase-line ((tty terminal-stream))
  (tt-format tty "~c[2K" #\escape))

(defgeneric tt-clear (tty))
(defmethod tt-clear ((tty terminal-stream))
  (tt-format tty "~c[2J" #\escape))

(defgeneric tt-home (tty))
(defmethod tt-home ((tty terminal-stream))
  (tt-format tty "~c[H" #\escape))

(defgeneric tt-cursor-off (tty))
(defmethod tt-cursor-off ((tty terminal-stream))
  (tt-format tty "~c7" #\escape))

(defgeneric tt-cursor-on (tty))
(defmethod tt-cursor-on ((tty terminal-stream))
  (tt-format tty "~c8" #\escape))

(defgeneric tt-standout (tty))
(defmethod tt-standout ((tty terminal-stream))
  (tt-format tty "~c[7m" #\escape))

(defgeneric tt-standend (tty))
(defmethod tt-standend ((tty terminal-stream))
  (tt-format tty "~c[0m" #\escape))

(defgeneric tt-normal (tty))
(defmethod tt-normal ((tty terminal-stream))
  (tt-format tty "~c[0m" #\escape))

(defgeneric tt-underline (tty state))
(defmethod tt-underline ((tty terminal-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 4 24)))

(defgeneric tt-bold (tty state))
(defmethod tt-bold ((tty terminal-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 1 22)))

(defgeneric tt-inverse (tty state))
(defmethod tt-inverse ((tty terminal-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 7 27)))

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defgeneric tt-color (tty fg bg))
(defmethod tt-color ((tty terminal-stream) fg bg)
  (let ((fg-pos (position fg *colors*))
	(bg-pos (position bg *colors*)))
    (when (not fg-pos)
      (error "Forground ~a is not a known color." fg))
    (when (not bg-pos)
      (error "Background ~a is not a known color." bg))
    (tt-format tty "~c[~d;~dm" #\escape (+ 30 fg-pos) (+ 40 bg-pos))))

;; 256 color? ^[[ 38;5;color <-fg 48;5;color <- bg
;; set color tab = ^[] Ps ; Pt BEL
;;;  4; color-number ; #rrggbb ala XParseColor

(defgeneric tt-beep (tty))
(defmethod tt-beep ((tty terminal-stream))
  (tt-write-char tty #\bel))		; Not #\bell!!

(defgeneric tt-finish-output (tty))
(defmethod tt-finish-output ((tty terminal-stream))
  (finish-output (terminal-output-stream tty)))

; (defgeneric tt-get-row (tty))
; (defmethod tt-get-row ((tty terminal))
;   (let ((string (format nil "~a[R" #\escape))
; 	(stream (terminal-output-stream tty)))
;     (write-string string stream)
;     (finish-output stream)
;   (with-foreign-object (c :unsigned-char)
;     (let ((status (posix-read (terminal-file-descriptor tty) c 1)))
;       (cond
; 	((< status 0)
; 	 (error "Read error ~d~%" status))
; 	((= status 0)
; 	 nil)
; 	((= status 1)
; 	 (code-char (mem-ref c :unsigned-char)))))))

(defgeneric tt-get-char (tty))
(defmethod tt-get-char ((tty terminal))
  "Read a character from the terminal."
  (tt-finish-output tty)
  (with-foreign-object (c :unsigned-char)
    (let (status)
      (loop
	 :do (setf status (posix-read (terminal-file-descriptor tty) c 1))
	 :if (and (< status 0) (or (= *errno* +EINTR+) (= *errno* +EAGAIN+)))
	 :do
	   ;; Probably returning from ^Z or terminal resize, or something,
	   ;; so keep trying. Enjoy your trip to plusering town.
	   (terminal-start tty) #| (redraw) |# (tt-finish-output tty)
	 :else
	   :return
	 :end)
      (cond
	((< status 0)
	 (error "Read error ~d ~d ~a~%" status nos:*errno*
		(nos:strerror nos:*errno*)))
	((= status 0) ; Another possible plusering extravaganza
	 nil)
	((= status 1)
	 (code-char (mem-ref c :unsigned-char)))))))

(defgeneric tt-reset (tty))
(defmethod tt-reset ((tty terminal-stream))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (flet ((out (s) (tt-write-string tty (format nil "~c~a" #\escape s))))
    ;; Then try to reset the terminal itself to a sane state. We could just do
    ;; ^[c, which is quite effective, but it's pretty drastic, and usually
    ;; clears the screen and can even resize the window, which is so amazingly
    ;; annoying. So let's just try do individual things that need resetting.
    ;; This is pretty much the idea of termcap/info reset string, usually the
    ;; "rs2", since "rs" usually just does ^[c.
    (mapcar
     #'out '(" F"    ;; 7 bit controls
	     "[0m"   ;; color and attributes
	     ">"     ;; normal keypad
	     "#@"    ;; default char set
	     "m"     ;; memory unlock
	     "[4l"   ;; replace mode (vs insert mode)
	     "[?4l"  ;; jump scroll (vs smooth scroll)
	     "[?25h" ;; show the cursor
	     "[?9l"  ;; Don't send position on mouse press
	     "[?47l" ;; Use normal screen buffer
	     ))
    (tt-finish-output tty)))

(defmethod tt-reset ((tty terminal))
  ;; First reset the terminal driver to a sane state.
  (termios:sane)
  (call-next-method)) ;; Do the terminal-stream version

(defmacro with-saved-cursor ((tty) &body body)
  "Save the cursor position, evaluate the body forms, and restore the cursor
position. Return the result of evaluating the body."
  (let ((result-sym (gensym "tt-result")))
    `(progn
       (tt-format ,tty "~c7" #\escape)
       (tt-finish-output ,tty)
       (let (,result-sym)
	 (unwind-protect
	      (setf ,result-sym (progn ,@body))
	   (tt-format ,tty "~c8" #\escape)
	   (tt-finish-output ,tty))
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
