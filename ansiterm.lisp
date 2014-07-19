;;
;; ansiterm.lisp - Deal with ANSI-like terminals
;;

;; $Revision: 1.1 $

(defpackage :ansiterm
  (:documentation "Deal with ANSI-like terminals")
  (:use :cl :termios :opsys :cffi)
  (:export
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
   #:tt-beep
   #:tt-finish-output
   ))
(in-package :ansiterm)

(defclass terminal ()
  ((file-descriptor
    :accessor terminal-file-descriptor
    :initarg :file-descriptor
    :documentation "System file descriptor.")
   (device-name
    :accessor terminal-device-name
    :initarg :device-name
    :documentation "System device name.")
   (output-stream
    :accessor terminal-output-stream
    :initarg :output-stream
    :documentation "Lisp stream for output.")
   (raw-state
    :accessor terminal-raw-state
    :initarg :terminal-raw-state
    :documentation "A foreign pointer to a POSIX termios struct of the terminal in its raw state.")
   (cooked-state
    :accessor terminal-cooked-state
    :initarg :terminal-cooked-state
    :documentation "A foreign pointer to a POSIX termios struct of the terminal in its cooked state")
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

(defgeneric tt-format (tty fmt &rest args)
  (:documentation "Output a formatted string to the terminal."))
(defmethod tt-format ((tty terminal) fmt &rest args)
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
(defmethod tt-write-string ((tty terminal) str)
  (let ((stream (terminal-output-stream tty)))
    (write-string str stream)
    (when (position #\newline str)
      (finish-output stream))))

(defgeneric tt-write-char (tty char)
  (:documentation "
Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is \"line buffered\""))
(defmethod tt-write-char ((tty terminal) char)
  (let ((stream (terminal-output-stream tty)))
    (write-char char stream)
    (when (eql char #\newline)
      (finish-output stream))))

(defgeneric tt-move-to (tty row col))
(defmethod tt-move-to ((tty terminal) row col)
  (tt-format tty "~c[~d;~dH" #\escape (1+ row) (1+ col)))

(defgeneric tt-move-to-col (tty col))
(defmethod tt-move-to-col ((tty terminal) col)
  (tt-format tty "~c[~dG" #\escape (1+ col)))

(defgeneric tt-beginning-of-line (tty))
(defmethod tt-beginning-of-line ((tty terminal))
  ;; (tt-format tty "~c[G" #\escape))
  ;; How about just:
  (tt-write-char tty #\return))

(defgeneric tt-del-char (tty n))
(defmethod tt-del-char ((tty terminal) n)
  (tt-format tty "~c[~aP" #\escape (if (> n 1) n "")))

(defgeneric tt-ins-char (tty n))
(defmethod tt-ins-char ((tty terminal) n)
  (tt-format tty "~c[~a@" #\escape (if (> n 1) n "")))

(defgeneric tt-backward (tty n))
(defmethod tt-backward ((tty terminal) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dD" #\escape n)
	  (tt-format tty "~c[D" #\escape))))

(defgeneric tt-forward (tty n))
(defmethod tt-forward ((tty terminal) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dC" #\escape n)
	  (tt-format tty "~c[C" #\escape))))

(defgeneric tt-up (tty n))
(defmethod tt-up ((tty terminal) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dA" #\escape n)
	  (tt-format tty "~c[A" #\escape))))

(defgeneric tt-down (tty n))
(defmethod tt-down ((tty terminal) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dB" #\escape n)
	  (tt-format tty "~c[B" #\escape))))

(defgeneric tt-scroll-down (tty n))
(defmethod tt-scroll-down ((tty terminal) n)
  (if (> n 0)
      (loop :with stream = (terminal-output-stream tty) and i = 0
	 :while (< i n)
	 :do (write-char #\newline stream) (incf i)
	 :finally (finish-output stream))))

(defgeneric tt-erase-to-eol (tty))
(defmethod tt-erase-to-eol ((tty terminal))
  (tt-format tty "~c[K" #\escape))

(defgeneric tt-erase-line (tty))
(defmethod tt-erase-line ((tty terminal))
  (tt-format tty "~c[2K" #\escape))

(defgeneric tt-clear (tty))
(defmethod tt-clear ((tty terminal))
  (tt-format tty "~c[2J" #\escape))

(defgeneric tt-home (tty))
(defmethod tt-home ((tty terminal))
  (tt-format tty "~c[H" #\escape))

(defgeneric tt-cursor-off (tty))
(defmethod tt-cursor-off ((tty terminal))
  (tt-format tty "~c7" #\escape))

(defgeneric tt-cursor-on (tty))
(defmethod tt-cursor-on ((tty terminal))
  (tt-format tty "~c8" #\escape))

(defgeneric tt-standout (tty))
(defmethod tt-standout ((tty terminal))
  (tt-format tty "~c[7m" #\escape))

(defgeneric tt-standend (tty))
(defmethod tt-standend ((tty terminal))
  (tt-format tty "~c[0m" #\escape))

(defgeneric tt-beep (tty))
(defmethod tt-beep ((tty terminal))
  (tt-write-char tty #\bell))

(defgeneric tt-finish-output (tty))
(defmethod tt-finish-output ((tty terminal))
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

;; EOF
