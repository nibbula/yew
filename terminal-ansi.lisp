;;
;; terminal-ansi.lisp - Standard terminals (aka ANSI).
;;

;; The actual related standard is:
;;   ISO/IEC 6429 (Ecma-048.pdf)
;; Some other documentation is in:
;;   "Xterm Control Sequences" by Moy, Gildea, and Dickey (1994-2008)
;;   (ctlseqs.txt)
;;   "Summary of ANSI standards for ASCII terminals" by Smith, 1984
;;   (ansicode.txt)

(defpackage :terminal-ansi
  (:documentation "Standard terminal (ANSI).")
  (:use :cl :terminal :cffi :opsys :termios)
  (:export
   #:terminal-ansi-stream
   #:terminal-ansi
   #:tty-slurp
   ))
(in-package :terminal-ansi)

(defclass terminal-ansi-stream (terminal-stream)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal-ansi (terminal terminal-ansi-stream)
  (
#|   (file-descriptor
    :accessor terminal-file-descriptor
    :initarg :file-descriptor
    :documentation "System file descriptor.")
   (device-name
    :accessor terminal-device-name
    :initarg :device-name
    :documentation "System device name.") |#
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
#|   (window-rows
    :accessor terminal-window-rows
    :initarg :window-rows
    :documentation "Number of rows of characters in the window.")
   (window-columns
    :accessor terminal-window-columns
    :initarg :window-columns
    :documentation "Number of columns of characters in the window.") |#
   )
  (:default-initargs
    :file-descriptor		nil
    :device-name		"/dev/tty"
    :output-stream		nil
    :terminal-raw-state		nil
    :terminal-cooked-state	nil
  )
  (:documentation "What we need to know about terminal device."))

(defmethod terminal-get-size ((tty terminal-ansi))
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

(defmethod terminal-get-cursor-position ((tty terminal-ansi))
  "Try to somehow get the row of the screen the cursor is on."
  (let ((row 1) (col 1) sep
	(result (tt-report tty #\R "~c[6n" #\escape)))
    (when (and result (>= (length result) 6))
      (setf sep (position #\; result)
	    row (parse-integer (subseq result 2 sep) :junk-allowed t)
	    col (parse-integer (subseq result (1+ sep) (length result))
			       :junk-allowed t)))
    (values (1- row) (1- col))))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defmethod terminal-start ((tty terminal-ansi))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots (file-descriptor device-name output-stream raw-state cooked-state
               window-rows window-columns) tty
    (when (not file-descriptor)
;      (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (posix-open device-name O_RDWR 0))
      ;; (dbug "terminal-ansi open in~%")
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
      ;; (dbug "terminal-ansi open out~%"))
    (when (< (tcsetattr file-descriptor TCSANOW raw-state) 0)
;    (when (< (tcsetattr file-descriptor TCSAFLUSH raw-state) 0)
      (error "Can't set raw state. ~d ~d" nos:*errno* file-descriptor))
;    (format t "[terminal-start]~%")
    (terminal-get-size tty)))

(defmethod terminal-end ((tty terminal-ansi))
  "Put the terminal back to the way it was before we called terminal-start."
;  (format t "[terminal-end]~%")
  (when (< (tcsetattr (terminal-file-descriptor tty) TCSANOW #+null TCSAFLUSH
		      (terminal-cooked-state tty))
	   0)
    (error "Can't set cooked state.")))

(defmethod terminal-done ((tty terminal-ansi))
  "Forget about the whole terminal thing and stuff."
  (with-slots (file-descriptor raw-state cooked-state output-stream) tty
    (terminal-end tty)
    (posix-close file-descriptor)
    ;; (dbug "terminal-ansi close in~%")
    (foreign-free raw-state)
    (foreign-free cooked-state)
    (when output-stream
      (close output-stream))
    ;; (dbug "terminal-ansi close out~%")
    ;; (format t "[terminal-done]~%")
    ;; (setf *tty* nil)
    (values)))

(defmethod tt-format ((tty terminal-ansi-stream) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'format nil fmt args))
	(stream (terminal-output-stream tty)))
    (write-string string stream)
    (when (position #\newline string)
      (finish-output stream))))

(defun tty-slurp (tty)
  "Read until EOF. Return a string of the results. TTY is a file descriptor."
  (let* ((size (nos:memory-page-size))
	 (result (make-array size
			     :element-type 'base-char
			     :fill-pointer 0 :adjustable t))
	 status)
    (with-output-to-string (str result)
      (with-foreign-object (buf :char size)
	(loop
	   :do (setf status (posix-read tty buf size))
	   :while (= status size)
	   :do (princ (cffi:foreign-string-to-lisp buf) str))
	(cond
	  ((> status size)
	   (error "Read returned too many characters? ~a" status))
	  ((< status 0)
	   (error "Read error ~d~%" status))
	  ((= status 0)
	   (or (and (length result) result) nil))
	  (t
	   (princ (cffi:foreign-string-to-lisp buf :count status) str)
	   result))))))

(defun read-until (tty stop-char)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor."
  (let (status cc
        (result (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (str result)
      (with-foreign-object (c :char)
	(loop
	   :do (setf status (posix-read tty c 1))
	   :while (= status 1)
	   :do (setf cc (code-char (mem-ref c :unsigned-char)))
	   :while (char/= cc stop-char) :do (princ cc str))
	(cond
	  ((> status 1)
	   (error "Read returned too many characters? ~a" status))
	  ((< status 0)
	   (error "Read error ~d~%" status))
	  ((= status 0)
	   (or (and (length result) result) nil))
	  (t
	   (princ cc str)
	   result))))))

(defun tt-report (tty end-char fmt &rest args)
  "Output a formatted string to the terminal and get an immediate report back.
Report parameters are returned as values. Report is assumed to be in the form:
#\escape #\[ { p1 { ; pn } } end-char"
  (let ((fd (terminal-file-descriptor tty))
	(q (apply #'format nil fmt args)))
    (with-foreign-string (qq q)
      (let ((str (call-with-raw
		  fd #'(lambda (x)
			 (posix-write x qq (length q))
			 (read-until x end-char))
		  :very-raw nil :timeout 10)))
	(when (null str)
	  (error "Terminal failed to report \"~a\"." fmt))
	str))))

(defmethod tt-write-string ((tty terminal-ansi-stream) str)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (write-string str stream)
    (when (position #\newline str)
      (finish-output stream))))

(defmethod tt-write-char ((tty terminal-ansi-stream) char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (write-char char stream)
    (when (eql char #\newline)
      (finish-output stream))))

(defmethod tt-move-to ((tty terminal-ansi-stream) row col)
  (tt-format tty "~c[~d;~dH" #\escape (1+ row) (1+ col)))

(defmethod tt-move-to-col ((tty terminal-ansi-stream) col)
  (tt-format tty "~c[~dG" #\escape (1+ col)))

(defmethod tt-beginning-of-line ((tty terminal-ansi-stream))
  ;; (tt-format tty "~c[G" #\escape))
  ;; How about just:
  (tt-write-char tty #\return))

(defmethod tt-del-char ((tty terminal-ansi-stream) n)
  (tt-format tty "~c[~aP" #\escape (if (> n 1) n "")))

(defmethod tt-ins-char ((tty terminal-ansi-stream) n)
  (tt-format tty "~c[~a@" #\escape (if (> n 1) n "")))

(defmethod tt-backward ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dD" #\escape n)
	  (tt-format tty "~c[D" #\escape))))

(defmethod tt-forward ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dC" #\escape n)
	  (tt-format tty "~c[C" #\escape))))

(defmethod tt-up ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dA" #\escape n)
	  (tt-format tty "~c[A" #\escape))))

(defmethod tt-down ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (if (> n 1)
	  (tt-format tty "~c[~dB" #\escape n)
	  (tt-format tty "~c[B" #\escape))))

(defmethod tt-scroll-down ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (loop :with stream = (terminal-output-stream tty) and i = 0
	 :while (< i n)
	 :do (write-char #\newline stream) (incf i)
	 :finally (finish-output stream))))

(defmethod tt-erase-to-eol ((tty terminal-ansi-stream))
  (tt-format tty "~c[K" #\escape))

(defmethod tt-erase-line ((tty terminal-ansi-stream))
  (tt-format tty "~c[2K" #\escape))

(defmethod tt-erase-above ((tty terminal-ansi-stream))
  (tt-format tty "~c[1J" #\escape))

(defmethod tt-erase-below ((tty terminal-ansi-stream))
  (tt-format tty "~c[0J" #\escape))

(defmethod tt-clear ((tty terminal-ansi-stream))
  (tt-format tty "~c[2J" #\escape))

(defmethod tt-home ((tty terminal-ansi-stream))
  (tt-format tty "~c[H" #\escape))

(defmethod tt-cursor-off ((tty terminal-ansi-stream))
  (tt-format tty "~c7" #\escape))

(defmethod tt-cursor-on ((tty terminal-ansi-stream))
  (tt-format tty "~c8" #\escape))

(defmethod tt-standout ((tty terminal-ansi-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 7 27)))

(defmethod tt-normal ((tty terminal-ansi-stream))
  (tt-format tty "~c[0m" #\escape))

(defmethod tt-underline ((tty terminal-ansi-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 4 24)))

(defmethod tt-bold ((tty terminal-ansi-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 1 22)))

(defmethod tt-inverse ((tty terminal-ansi-stream) state)
  (tt-format tty "~c[~dm" #\escape (if state 7 27)))

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defmethod tt-color ((tty terminal-ansi-stream) fg bg)
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

(defmethod tt-beep ((tty terminal-ansi-stream))
  (tt-write-char tty #\bel))		; Not #\bell!!

(defmethod tt-set-scrolling-region ((tty terminal-ansi-stream) start end)
  (if (and (not start) (not end))
      (tt-format tty "~c[r" #\escape)
      (tt-format tty "~c[~d;~dr" #\escape start end)))

(defmethod tt-finish-output ((tty terminal-ansi-stream))
  (finish-output (terminal-output-stream tty)))

; (defmethod tt-get-row ((tty terminal-ansi))
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

(defmethod tt-get-char ((tty terminal-ansi))
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

(defmethod tt-reset ((tty terminal-ansi-stream))
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

(defmethod tt-reset ((tty terminal-ansi))
  ;; First reset the terminal driver to a sane state.
  (termios:sane)
  (call-next-method)) ;; Do the terminal-stream version

(defmethod tt-save-cursor ((tty terminal-ansi))
  "Save the cursor position."
  (tt-format tty "~c7" #\escape)
  (tt-finish-output tty))

(defmethod tt-restore-cursor ((tty terminal-ansi))
  "Restore the cursor position, from the last saved postion."
  (tt-format tty "~c8" #\escape)
  (tt-finish-output tty))

;; EOF
