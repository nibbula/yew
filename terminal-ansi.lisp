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
  (:use :dlib :cl :terminal :cffi :opsys)
  (:export
   #:terminal-ansi-stream
   #:terminal-ansi
   #:tty-slurp
   ))
(in-package :terminal-ansi)

;; To be portable we have to restrict ourselves to calls to the system
;; independent part of OPSYS. So we shouldn't use anything in TERMIO or UNIX.

(defvar *default-device-name* *default-console-device-name*
  "The default device to create a terminal on.")

(defclass terminal-ansi-stream (terminal-stream)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal-ansi (terminal terminal-ansi-stream)
  ((typeahead
    :accessor typeahead
    :initform nil
    :initarg :typeahead
    :documentation "Things already input, dag blast it.")
   (typeahead-pos
    :accessor typeahead-pos
    :initform nil
    :initarg :typeahead-pos
    :documentation "How far into the typeahead we are."))
  (:default-initargs
    :file-descriptor		nil
    :device-name		*default-device-name*
    :output-stream		nil
  )
  (:documentation "What we need to know about terminal device."))

(defmethod terminal-default-device-name ((type (eql 'terminal-ansi)))
  "Return the default device name for a TERMINAL-ANSI."
  *default-device-name*)

(defmethod terminal-get-size ((tty terminal-ansi))
  "Get the window size from the kernel and store it in tty."
  (with-slots (file-descriptor window-rows window-columns) tty
    (when file-descriptor
      (multiple-value-setq (window-columns window-rows)
	(get-window-size file-descriptor)))))

;; There seems to be two possibilities for getting this right:
;;  1. We do all output thru our routines and keep track
;;  2. We ask the terminal (and get an accurate response)
;; We really can't assume 1, nor can we reliably assume we can check if
;; any output was done (e.g. not all OS's update the access time on the
;; tty device or even have such a thing). So we have to do 2. Since hardware
;; terminals are mostly extinct, emulators should be damn well able to do this
;; by now. Curses just assumes if you do any output not thru it, you are on
;; your own, hence the necessity in most programs of a complete erase and
;; redraw user command.

(defun eat-typeahead (tty)
  (let (ta (fd (terminal-file-descriptor tty)))
    (set-terminal-mode fd :raw t)
    (setf ta (slurp-terminal fd :timeout 1))
    (set-terminal-mode fd :raw nil)
    (when (and ta (> (length ta) 0))
;      (log-message e "ta[~a]=~w" (length ta) ta)
      (if (typeahead tty)
	  (setf (typeahead tty) (s+ (typeahead tty) ta))
	  (setf (typeahead tty) ta
		(typeahead-pos tty) 0)))))

(defmethod terminal-get-cursor-position ((tty terminal-ansi))
  "Try to somehow get the row of the screen the cursor is on. Returns the
two values ROW and COLUMN."
  (eat-typeahead tty)
  (let ((row 1) (col 1) sep
	(result (tt-report tty #\R "~c[6n" #\escape)))
    (when (and result (>= (length result) 5))
      (setf sep (position #\; result)
	    row (parse-integer (subseq result 2 sep) :junk-allowed t)
	    col (parse-integer (subseq result (1+ sep) (length result))
			       :junk-allowed t)))
    (when (or (not row) (not col))
      ;; Probabbly because there was other I/O going on.
      (error "terminal reporting failed"))
    (values (1- row) (1- col))))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defmethod terminal-start ((tty terminal-ansi))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots (file-descriptor device-name output-stream
               window-rows window-columns) tty
    (when (not file-descriptor)
      ;; (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (open-terminal device-name)))
      ;; (dbug "terminal-ansi open in~%")
    (set-terminal-mode file-descriptor :line nil :echo nil)
    (when (not output-stream)
      (setf output-stream (open device-name :direction :output
				#-clisp :if-exists #-clisp :append)))
      ;; (dbug "terminal-ansi open out~%"))
    (terminal-get-size tty)))

(defmethod terminal-end ((tty terminal-ansi))
  "Put the terminal back to the way it was before we called terminal-start."
					;  (format t "[terminal-end]~%")
  (set-terminal-mode (terminal-file-descriptor tty)
		     :line t :echo t :raw nil :timeout nil))

(defmethod terminal-done ((tty terminal-ansi))
  "Forget about the whole terminal thing and stuff."
  (with-slots (file-descriptor raw-state cooked-state output-stream) tty
    (terminal-end tty)
    (close-terminal file-descriptor)
    ;; (dbug "terminal-ansi close in~%")
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

;; resumed -> (terminal-start tty) #| (redraw) |# (tt-finish-output tty)
;; resized -> (terminal-get-size tt)

;; @@@ BROKEN! FIX!
;; (defmacro with-raw ((tty) &body body)
;;   (with-unique-names (mode)
;;     `(let ((,mode (get-terminal-mode ,tty)))
;;        (unwind-protect
;; 	    (progn
;; 	      (set-terminal-mode ,tty :raw t :echo nil)
;; 	      ,@body)
;; 	 (set-terminal-mode ,tty :mode ,mode)))))

(defmacro with-raw ((tty) &body body)
    `(unwind-protect
	  (progn
	    (set-terminal-mode ,tty :raw t :echo nil)
	    ,@body)
       (set-terminal-mode ,tty :raw nil)))

(defun tt-report (tty end-char fmt &rest args)
  "Output a formatted string to the terminal and get an immediate report back.
Report parameters are returned as values. Report is assumed to be in the form:
#\escape #\[ { p1 { ; pn } } end-char"
  (let ((fd (terminal-file-descriptor tty))
	(q (apply #'format nil fmt args)))
    (let ((str (with-raw (fd)
		 ;;(posix-write fd qq (length q))
		 ;;(tt-write-string tty q) (tt-finish-output tty)
		 (write-terminal-string fd q)
		 (read-until fd end-char :timeout 1)))) ; 10
      (when (null str)
	(error "Terminal failed to report \"~a\"." fmt))
      str)))

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

(defparameter *attributes*
  '((:normal	       22)		; not bold or faint
    (:bold	       1)
    (:faint	       2)
    (:italic	       3)
    (:underline	       4)
    (:blink	       5)
    (:inverse	       7)
    (:invisible	       8)
    (:crossed-out      9)
    (:double-underline 21)))

(defparameter *attributes-off*
  '((:all	       0)		; No attributes
    (:bold	       22)
    (:faint	       22)
    (:italic	       23)
    (:underline	       24)
    (:blink	       25)
    (:inverse	       27)
    (:invisible	       28)
    (:crossed-out      29)
    (:double-underline 24)))		; same as not underline

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defmethod tt-color ((tty terminal-ansi-stream) fg bg)
  (let ((fg-pos (position fg *colors*))
	(bg-pos (position bg *colors*)))
    (when (not fg-pos)
      (error "Forground ~a is not a known color." fg))
    (when (not bg-pos)
      (error "Background ~a is not a known color." bg))
    (cond
      ((and fg bg)
       (tt-format tty "~c[~d;~dm" #\escape (+ 30 fg-pos) (+ 40 bg-pos)))
      (fg
       (tt-format tty "~c[~dm" #\escape (+ 30 fg-pos)))
      (bg
       (tt-format tty "~c[~dm" #\escape (+ 40 bg-pos)))
      (t
       (tt-format tty "~c[m" #\escape)))))

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

(defun get-char (tty)
  (with-slots (typeahead typeahead-pos file-descriptor) tty
    (when typeahead
      (return-from get-char
	(prog1
	    (aref typeahead typeahead-pos)
	  (incf typeahead-pos)
	  ;;(format t "ta->~a~%" (incf typeahead-pos))
	  (when (>= typeahead-pos (length typeahead))
	    (setf typeahead nil)))))
    (let (result borked)
      (loop :do
	 (setf borked nil)
	 (handler-case
	     (setf result (read-terminal-char file-descriptor))
	   (opsys-resumed ()
	     (terminal-start tty) (tt-finish-output tty)
	     (setf borked t))
	   (opsys-resized ()
	     (terminal-get-size tty)
	     (setf borked t)))
	 :while borked)
      result)))

(defmethod tt-get-char ((tty terminal-ansi))
  "Read a character from the terminal."
  (tt-finish-output tty)
  ;;(read-terminal-char tty))
  (get-char tty))

(defmethod tt-get-key ((tty terminal-ansi))
  (tt-finish-output tty)
  (get-char tty))

(defmethod tt-listen-for ((tty terminal-ansi) seconds)
  (listen-for seconds (terminal-file-descriptor tty)))

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
