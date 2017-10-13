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
  (:use :cl :cffi :dlib :dlib-misc :terminal :char-util :opsys
	:trivial-gray-streams)
  (:export
   #:terminal-ansi-stream
   #:terminal-ansi
   ;; extensions:
   #:describe-terminal
   #:+csi+ #:+st+ #:+osc+
   #:query-parameters #:query-string
   #:with-raw #:with-immediate
   ))
(in-package :terminal-ansi)

;; To be portable we have to restrict ourselves to calls to the system
;; independent part of OPSYS. So we shouldn't use anything in TERMIO or UNIX.

(defvar *default-device-name* *default-console-device-name*
  "The default device to create a terminal on.")

(define-constant +csi+ (s+ #\escape #\[)
  "Control Sequence Introducer. Hooking up control sequences since 1970.")
(define-constant +st+  (s+ #\escape #\\)
  "String terminator. Death to strings.")
(define-constant +osc+ (s+ #\escape #\])
  "Operating System Command. C'est vrai? o_O")

(defclass terminal-ansi-stream (terminal-stream)
  ((fake-column
   :initarg :fake-column :accessor terminal-ansi-stream-fake-column
   :initform 0 :type fixnum
   :documentation "Guess for the current column."))
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
  (when (terminal-file-descriptor tty)
    (multiple-value-bind (cols rows)
	(get-window-size (terminal-file-descriptor tty))
      (setf (terminal-window-rows tty) rows
	    (terminal-window-columns tty) cols))))

(defun add-typeahead (tty thing)
  "Add THING to the typeahead buffer of TTY."
  (setf (typeahead tty) (if (typeahead tty)
			    (s+ (typeahead tty) thing)
			    (s+ thing)))
  (when (not (typeahead-pos tty))
    (setf (typeahead-pos tty) 0)))

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
      ;; (if (typeahead tty)
      ;; 	  (setf (typeahead tty) (s+ (typeahead tty) ta))
      ;; 	  (setf (typeahead tty) ta
      ;; 		(typeahead-pos tty) 0)))))
      (add-typeahead tty ta))))

(defmethod terminal-get-cursor-position ((tty terminal-ansi))
  "Try to somehow get the row of the screen the cursor is on. Returns the
two values ROW and COLUMN."
  (eat-typeahead tty)
  (let ((row 1) (col 1) sep
	(result (terminal-report tty #\R "~c[6n" #\escape)))
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
  (with-slots ((file-descriptor	   terminal::file-descriptor)
	       (device-name   	   terminal::device-name)
	       (output-stream 	   terminal::output-stream)) tty
    (when (not file-descriptor)
      ;; (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (open-terminal device-name)))
      ;; (dbug "terminal-ansi open in~%")
    (set-terminal-mode file-descriptor :line nil :echo nil)
    (when (not output-stream)
      (setf output-stream (open device-name :direction :output
				#-(or clisp abcl) :if-exists
				#-(or clisp abcl) :append))
      ;; @@@ Why do we have to do this?
      #+ccl (setf (stream-external-format output-stream)
		  (ccl:make-external-format :character-encoding :utf-8
					    :domain :file))
      )
      ;; (dbug "terminal-ansi open out~%"))
    (terminal-get-size tty)))

(defmethod terminal-end ((tty terminal-ansi))
  "Put the terminal back to the way it was before we called terminal-start."
					;  (format t "[terminal-end]~%")
  (set-terminal-mode (terminal-file-descriptor tty)
		     :line t :echo t :raw nil :timeout nil))

(defmethod terminal-done ((tty terminal-ansi))
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty)
  (close-terminal (terminal-file-descriptor tty))
  ;; (dbug "terminal-ansi close in~%")
  (when (terminal-output-stream tty)
    (close (terminal-output-stream tty)))
  ;; (dbug "terminal-ansi close out~%")
  ;; (format t "[terminal-done]~%")
  ;; (setf *tty* nil)
  (values))

(defun update-column-for-char (tty char)
  (with-slots (fake-column) tty
    (cond
      ((graphic-char-p char)
       (cond
	 ((combining-char-p char) 0)
	 ((double-wide-char-p char) 2)
	 (t 1)))			;normal case
      (t
       (case char
	 (#\return
	  (setf fake-column 0))
	 (#\tab
	  (incf fake-column (- (1+ (logior 7 fake-column)) fake-column)))
	 (otherwise
	  0 ;; some non-graphic control char?
	  ))))))

(defun update-column (tty thing)
  (etypecase thing
    (character (update-column-for-char tty thing))
    (string
     (map nil (_ (update-column-for-char tty _)) thing))))

(defgeneric terminal-raw-format (tty fmt &rest args))
(defmethod terminal-raw-format ((tty terminal-ansi-stream) fmt &rest args)
  "Output a formatted string to the terminal, without doing any content
processing."
  (let ((string (apply #'format nil fmt args))
	(stream (terminal-output-stream tty)))
    (write-string string stream)))

(defmethod terminal-format ((tty terminal-ansi-stream) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'terminal-raw-format tty fmt args)))
    (update-column tty string)
    (when (position #\newline string)
      (finish-output tty))))

;; resumed -> (terminal-start tty) #| (redraw) |# (terminal-finish-output tty)
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

(defmacro with-immediate ((tty) &body body)
  (with-unique-names (mode)
    `(let ((,mode (get-terminal-mode (terminal-file-descriptor ,tty))))
       (unwind-protect
	    (progn
	      (set-terminal-mode (terminal-file-descriptor ,tty)
				 :line nil :echo nil)
	      ,@body)
	 (set-terminal-mode (terminal-file-descriptor ,tty) :mode ,mode)))))

(defun terminal-report (tty end-char fmt &rest args)
  "Output a formatted string to the terminal and get an immediate report back.
Report parameters are returned as values. Report is assumed to be in the form:
#\escape #\[ { p1 { ; pn } } end-char"
  (let ((fd (terminal-file-descriptor tty))
	(q (apply #'format nil fmt args)))
    (let ((str (with-raw (fd)
		 ;;(posix-write fd qq (length q))
		 ;;(terminal-write-string tty q) (terminal-finish-output tty)
		 (write-terminal-string fd q)
		 (read-until fd end-char :timeout 1)))) ; 10
      (when (null str)
	(error "Terminal failed to report \"~a\"." fmt))
      str)))

(defmethod terminal-write-string ((tty terminal-ansi-stream) str
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (apply #'write-string `(,str ,stream
				 ,@(and start `(:start ,start))
				 ,@(and end `(:start ,end))))
    ;;(write-string str stream :start start :end end)
    (update-column tty str)
    (when (apply #'position `(#\newline ,str
					,@(and start `(:start ,start))
					,@(and end `(:start ,end))))
      (finish-output stream))))

(defmethod terminal-write-char ((tty terminal-ansi-stream) char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (write-char char stream)
    (update-column tty char)
    (when (eql char #\newline)
      (finish-output stream))))

(defmethod terminal-move-to ((tty terminal-ansi-stream) row col)
  (terminal-raw-format tty "~c[~d;~dH" #\escape (1+ row) (1+ col))
  (setf (terminal-ansi-stream-fake-column tty) col))

(defmethod terminal-move-to-col ((tty terminal-ansi-stream) col)
  (terminal-raw-format tty "~c[~dG" #\escape (1+ col))
  (setf (terminal-ansi-stream-fake-column tty) col))

(defmethod terminal-beginning-of-line ((tty terminal-ansi-stream))
  ;; (terminal-format tty "~c[G" #\escape))
  ;; How about just:
  (terminal-write-char tty #\return))

(defmethod terminal-del-char ((tty terminal-ansi-stream) n)
  (terminal-raw-format tty "~c[~aP" #\escape (if (> n 1) n "")))

(defmethod terminal-ins-char ((tty terminal-ansi-stream) n)
  (terminal-raw-format tty "~c[~a@" #\escape (if (> n 1) n "")))

(defun moverize (tty n pos neg)
  (cond
    ((= n 1)  (terminal-raw-format tty "~c[~c" #\escape pos))
    ((> n 1)  (terminal-raw-format tty "~c[~d~c" #\escape n pos))
    ((= n 0)  #| do nothing |#)
    ((= n -1) (terminal-raw-format tty "~c[~c" #\escape neg))
    ((< n -1) (terminal-raw-format tty "~c[~d~c" #\escape n neg))))

(defmethod terminal-backward ((tty terminal-ansi-stream) n)
  (moverize tty n #\D #\C)
  (decf (terminal-ansi-stream-fake-column tty) n))

(defmethod terminal-forward ((tty terminal-ansi-stream) n)
  (moverize tty n #\C #\D)
  (incf (terminal-ansi-stream-fake-column tty) n))

(defmethod terminal-up ((tty terminal-ansi-stream) n)
  (moverize tty n #\A #\B))

(defmethod terminal-down ((tty terminal-ansi-stream) n)
  (moverize tty n #\B #\A))

(defmethod terminal-scroll-down ((tty terminal-ansi-stream) n)
  (if (> n 0)
      (loop :with stream = (terminal-output-stream tty) and i = 0
	 :while (< i n)
	 :do (write-char #\newline stream) (incf i)
	 :finally (finish-output stream))))

(defmethod terminal-erase-to-eol ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[K" #\escape))

(defmethod terminal-erase-line ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[2K" #\escape))

(defmethod terminal-erase-above ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[1J" #\escape))

(defmethod terminal-erase-below ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[0J" #\escape))

(defmethod terminal-clear ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[2J" #\escape))

(defmethod terminal-home ((tty terminal-ansi-stream))
  (terminal-raw-format tty "~c[H" #\escape)
  (setf (terminal-ansi-stream-fake-column tty) 0))

(defmethod terminal-cursor-off ((tty terminal-ansi-stream))
  ;;(terminal-format tty "~c7" #\escape))
  (terminal-format tty "~c[?25l" #\escape))

(defmethod terminal-cursor-on ((tty terminal-ansi-stream))
  ;;(terminal-format tty "~c8" #\escape))
  (terminal-format tty "~c[?25h" #\escape))

(defmethod terminal-standout ((tty terminal-ansi-stream) state)
  (terminal-format tty "~c[~dm" #\escape (if state 7 27)))

(defmethod terminal-normal ((tty terminal-ansi-stream))
  (terminal-format tty "~c[0m" #\escape))

(defmethod terminal-underline ((tty terminal-ansi-stream) state)
  (terminal-format tty "~c[~dm" #\escape (if state 4 24)))

(defmethod terminal-bold ((tty terminal-ansi-stream) state)
  (terminal-format tty "~c[~dm" #\escape (if state 1 22)))

(defmethod terminal-inverse ((tty terminal-ansi-stream) state)
  (terminal-format tty "~c[~dm" #\escape (if state 7 27)))

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

(defun format-color (red green blue &key bits)
  "Return a string in XParseColor format for a color with the given RED, BLUE,
and GREEN, components. Default to 8 bit color. If values are over 8 bits,
default to 16 bit color."
  (let ((r red) (g green) (b blue) (l (list red green blue)))
    (cond
      ((every #'floatp l)
       (format nil "rgbi:~f/~f/~f" r g b))
      ((every #'integerp l)
       (let (fmt)
	 (when (not bits)
	   (setf bits (if (some (_ (> _ #xff)) l) 16 8)))
	 (setf fmt
	       (case bits
		 (4  "~x")
		 (8  "~2,'0x")
		 (12 "~3,'0x")
		 (16 "~4,'0x")
		 (t (error "Bad color bit magnitudes: ~s" l))))
	 (format nil (s+ "rgb:" fmt "/" fmt "/" fmt) r g b)))
      (t
       (error "Bad color formats: ~s" l)))))

(defun rgb-color-p (x)
  (and (not (null x))
       (or (consp x) (arrayp x))
       (= (length x) 3)
       (every #'numberp x)))

(defun   color-red   (c) (elt c 0))
(defsetf color-red   (c) (val) `(setf (elt ,c 0) ,val))
(defun   color-green (c) (elt c 1))
(defsetf color-green (c) (val) `(setf (elt ,c 1) ,val))
(defun   color-blue  (c) (elt c 2))
(defsetf color-blue  (c) (val) `(setf (elt ,c 2) ,val))

(defun set-foreground-color (color)
  (tt-format "~a10;~a~a" +osc+
	     (format-color (color-red   color)
			   (color-green color)
			   (color-blue  color)) +st+))

(defun set-background-color (color)
  (tt-format "~a11;~a~a" +osc+
	     (format-color (color-red   color)
			   (color-green color)
			   (color-blue  color)) +st+))

(defmethod terminal-color ((tty terminal-ansi-stream) fg bg)
  (let ((fg-pos (position fg *colors*))
	(bg-pos (position bg *colors*)))
    (when (and (keywordp fg) (not fg-pos))
      (error "Forground ~a is not a known color." fg))
    (when (and (keywordp bg) (not bg-pos))
      (error "Background ~a is not a known color." bg))
    (cond
      ((or (rgb-color-p fg) (rgb-color-p bg))
       (when (rgb-color-p fg)
	 (let ((red   (elt fg 0))
	       (green (elt fg 1))
	       (blue  (elt fg 2)))
	   (terminal-raw-format tty "~a38;2;~a;~a;~am" +csi+
				red green blue)))
       (when (rgb-color-p bg)
	 (let ((red   (elt bg 0))
	       (green (elt bg 1))
	       (blue  (elt bg 2)))
	   (terminal-raw-format tty "~a48;2;~a;~a;~am" +csi+
				red green blue))))
      ((and fg bg)
       (terminal-raw-format tty "~c[~d;~dm" #\escape
			    (+ 30 fg-pos) (+ 40 bg-pos)))
      (fg
       (terminal-raw-format tty "~c[~dm" #\escape (+ 30 fg-pos)))
      (bg
       (terminal-raw-format tty "~c[~dm" #\escape (+ 40 bg-pos)))
      (t
       (terminal-raw-format tty "~c[m" #\escape)))))

;; 256 color? ^[[ 38;5;color <-fg 48;5;color <- bg
;; set color tab = ^[] Ps ; Pt BEL
;;;  4; color-number ; #rrggbb ala XParseColor

(defmethod terminal-beep ((tty terminal-ansi-stream))
  (terminal-write-char tty #\bel))		; Not #\bell!!

(defmethod terminal-set-scrolling-region ((tty terminal-ansi-stream) start end)
  (if (and (not start) (not end))
      (terminal-raw-format tty "~c[r" #\escape)
      (terminal-raw-format tty "~c[~d;~dr" #\escape start end)))

(defmethod terminal-finish-output ((tty terminal-ansi-stream))
  (finish-output (terminal-output-stream tty)))

; (defmethod terminal-get-row ((tty terminal-ansi))
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

(defun get-char (tty &key timeout)
  (with-slots (typeahead typeahead-pos
	       (file-descriptor terminal::file-descriptor)) tty
    (when typeahead
      (return-from get-char
	(prog1
	    (aref typeahead typeahead-pos)
	  (incf typeahead-pos)
	  ;;(format t "ta->~a~%" (incf typeahead-pos))
	  (when (>= typeahead-pos (length typeahead))
	    (setf typeahead nil
		  typeahead-pos nil)))))
    (let (result borked)
      (loop :do
	 (setf borked nil)
	 (handler-case
	     (setf result (read-terminal-char file-descriptor :timeout timeout))
	   (opsys-resumed ()
	     (terminal-start tty) (terminal-finish-output tty)
	     (setf borked t))
	   (opsys-resized ()
	     (terminal-get-size tty)
	     (setf borked t)))
	 :while borked)
      result)))

(defmethod terminal-get-char ((tty terminal-ansi))
  "Read a character from the terminal."
  (terminal-finish-output tty)
  ;;(read-terminal-char tty))
  (get-char tty))

;; This can unfortunately really vary between emulations, so we try to code
;; for multiple interpretations.
(defun read-function-key (tty)
  "Read the part of a function key after the ESC [ and return an indicative
keyword. If we don't recognize the key, return #\escape and add the characters
to the typeahead."
  (let ((c (get-char tty :timeout 1)))
    ;;(format t "got ~s~%" c)
    (case c
      ;; Arrow keys
      (#\A :up)
      (#\B :down)
      (#\C :right)
      (#\D :left)
      ;; Movement keys
      (#\H :home)
      (#\F :end)
      (#\Z :back-tab)			; non-standard
      (t
       (cond
	 ((null c) ; timeout
	  (add-typeahead tty "[")
	  #\escape)
	 ;; read a number followed by a tilde
	 ((digit-char-p c)
	  (let ((num (parse-integer (string c))))
	    (setf c (get-char tty :timeout 1))
	    (loop :while (digit-char-p c)
	       :do
	       (setf num (+ (* num 10) (parse-integer (string c))))
	       ;;(format t "(~a ~c)" num c)
	       (setf c (get-char tty :timeout 1)))
	    ;;(message tty (format nil "~a ~c" n c))
	    ;;(format t "[~d ~c]" num c)
	    (if (eql c #\~)
		(case num
		  (2 :insert)
		  (3 :delete)
		  (5 :page-up)
		  (6 :page-down)
		  (15 :f5)
		  (17 :f6)
		  (18 :f7)
		  (19 :f8)
		  (20 :f9)
		  (21 :f10)
		  (23 :f11)
		  (24 :f12)
		  (t
		   (add-typeahead tty (s+ "["))
		   (when num
		     (add-typeahead tty (s+ num)))
		   (when c
		     (add-typeahead tty c))
		   #\escape))
		(progn
		  (add-typeahead tty (s+ "[" num))
		  (when c
		    (add-typeahead tty c))))))
	 (t
	  (add-typeahead tty "[")
	  (when c
	    (add-typeahead tty c))
	  #\escape))))))

(defun read-app-key (tty)
  "Read the part of an application mode function key after the ESC O and
 return an indicative keyword. If we don't recognize the key, return #\escape
and add the characters the typeahead."
  (let ((c (get-char tty :timeout 1)))
    (case c
      ;; Arrow keys
      (#\A :up)
      (#\B :down)
      (#\C :right)
      (#\D :left)
      ;; Movement keys
      (#\H :home)
      (#\F :end)
      ;; Function keys
      (#\P :f1)
      (#\Q :f2)
      (#\R :f3)
      (#\S :f4)
      (t
       (add-typeahead tty "O")
       (when c
	 (add-typeahead tty c))
       #\escape))))

(defmethod terminal-get-key ((tty terminal-ansi))
  (terminal-finish-output tty)
  (let ((c (get-char tty)))
    (if (char= c #\escape)
	(case (setf c (get-char tty :timeout 1))
	  (#\[ (read-function-key tty))
	  (#\O (read-app-key tty))
	  (t
	   (when c ;; if it didn't time out
	     (add-typeahead tty c))
	   #\escape))
	c)))

(defmethod terminal-listen-for ((tty terminal-ansi) seconds)
  (listen-for seconds (terminal-file-descriptor tty)))

(defmethod terminal-reset ((tty terminal-ansi-stream))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (flet ((out (s) (terminal-write-string tty (format nil "~c~a" #\escape s))))
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
    (terminal-finish-output tty)))

(defmethod terminal-reset ((tty terminal-ansi))
  ;; First reset the terminal driver to a sane state.
  (termios:sane)
  (call-next-method)) ;; Do the terminal-stream version

(defmethod terminal-save-cursor ((tty terminal-ansi))
  "Save the cursor position."
  (terminal-format tty "~c7" #\escape)
  (terminal-finish-output tty))

(defmethod terminal-restore-cursor ((tty terminal-ansi))
  "Restore the cursor position, from the last saved postion."
  (terminal-format tty "~c8" #\escape)
  (terminal-finish-output tty))

(defun response-terminal-type (n)
  (case n
    (0 "VT100")
    (1 "VT220")
    (2 "VT240")
    (18 "VT330")
    (19 "VT340")
    (24 "VT320")
    (41 "VT420") ;; 🍁
    (61 "VT510")
    (64 "VT520")
    (65 "VT525")))


(defun query-parameters (s &key (offset 3))
  (let ((response (termios:terminal-query (s+ +csi+ s))))
    (if (zerop (length response))
	'()
	(mapcar (_ (ignore-errors (parse-integer _)))
		(split-sequence
		 #\;
		 (coerce (subseq response offset
				 (1- (length response)))
			 'string))))))

(defun query-string (s &key (offset 3) (ending 2) (lead-in +csi+))
  (let ((response (termios:terminal-query (s+ lead-in s))))
    (if (zerop (length response))
	'()
	(coerce (subseq response offset
			(- (length response) ending))
		'string))))

(defun describe-terminal ()
  "Interrogate the terminal properties and report the results."
  (let (a props)
    ;; Terminal type
    (setf a (query-parameters ">c"))
    (push `("Terminal type" ,(response-terminal-type (first a))) props)
    (when (second a)
      (push `("Firmware version" ,(second a)) props))
    ;; Features
    (setf a (query-parameters "c"))
    (loop :for prop :in (cdr a) :do
       (push `(,(case prop
		      (1 "132-columns")
		      (2 "Printer")
		      (6 "Selective erase")
		      (8 "User-defined keys")
		      (9 "National Replacement Character sets")
		      (15 "Technical characters")
		      (18 "User windows")
		      (21 "Horizontal scrolling")
		      (22 "ANSI color")
		      (29 "ANSI text locator")
		      (t "Unknown property"))
		"Yes") props))
    ;; Cursor position
    (setf a (query-parameters "?6n"))
    (push `("Cursor position" ,(format nil "~a ~a" (first a) (second a)))
	  props)
    ;; Printer
    (setf a (query-parameters "?15n"))
    (push `("Printer status"
	    ,(case (first a)
		   (10 "Ready")
		   (11 "Not Ready")
		   (13 "No Printer")
		   (t "Unknown")))
	  props)
    ;; Locator status
    (setf a (query-parameters "?55n"))
    (push `("Locator status"
	    ,(case (first a)
		   (53 "Available")
		   (50 "No locator")
		   (t "Unknown")))
	  props)
    ;; Locator type
    (setf a (query-parameters "?56n"))
    (push `("Locator type"
	    ,(case (second a)
		   (1 "Mouse")
		   (t "Unknown")))
	  props)
    ;; Window state
    (setf a (query-parameters "11t" :offset 2))
    (push `("Window state"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (case (first a)
		   (1 "Open")
		   (2 "Iconified")
		   (t "Unknown"))))
	  props)
    ;; Window position
    (setf a (query-parameters "13t" :offset 2))
    (push `("Window position"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Window size
    (setf a (query-parameters "14t" :offset 2))
    (push `("Window size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text size
    (setf a (query-parameters "18t" :offset 2))
    (push `("Text size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Text screen size
    (setf a (query-parameters "19t" :offset 2))
    (push `("Text screen size"
	    ,(if (zerop (length a))
		 "Unavailable"
		 (format nil "~a ~a" (second a) (third a))))
	  props)
    ;; Icon label
    (setf a (query-string "20t"))
    (push `("Icon label"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;; Title
    (setf a (query-string "21t"))
    (push `("Title"
	    ,(if (zerop (length a))
		 "Unavailable"
		 a))
	  props)
    ;;
    (setf props (nreverse props))
    (print-properties props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

;; common methods

(defmethod-quiet close ((stream terminal-ansi-stream) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-ansi-stream))
  (clear-output (terminal-output-stream stream)))

(defmethod stream-finish-output ((stream terminal-ansi-stream))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-ansi-stream))
  (terminal-finish-output stream)
  (force-output (terminal-output-stream stream)))

(defmethod stream-write-sequence ((stream terminal-ansi-stream) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string stream seq :start start :end end))
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	    (when (>= i start)
	      (write-char (car l) output-stream)
	      (update-column stream (car l)))
	    (setf l (cdr l))
	    (incf i))))))

;; character output stream methods

;; Blarg! The hideous cursor position problem again.
;; We could just do:
;;   (terminal-move-to-col stream column)
;; but it's WRONG, because it doesn't erase. So to be fast, it would seem we
;; could either make a tt-erase-area, or have full screen contents knowledge.
;; Even modern terminal emulators (such as libvte) don't do erase area
;; currently, and full screen contents knowledge is a curses implementation.
;; So, until that time, to be correct, we would have to be slow and just output
;; spaces. Unfortunately, even outputting spaces requires knowing what column
;; we're at, which we can't currently. Even doing the old counting newlines,
;; backspaces, and tabs is unlikely to work, for the usual reasons.
;; So fuck it. Let's not implement any of the column dependent methods.
;; The sad thing is that we *should* be able to implement better column
;; tracking than most streams. Although even with full screen contents we
;; *still* won't know if something not under our control does output. Even with
;; everything under our control, we *still* won't know exactly what the terminal
;; does with the output, unless we ask it.
;;
;; (defmethod stream-advance-to-column ((stream terminal-ansi) column)
;;   ;; @@@
;;   t)

;; This is a weird trick to presumably make it so we don't have to do our own
;; buffering and we can also be relatively quick?
(defvar *endless-spaces* '#1=(#\space . #1#)
  "The vast emptyness of space.")

(defmethod stream-line-column ((stream terminal-ansi-stream))
  (terminal-ansi-stream-fake-column stream))

(defmethod stream-start-line-p ((stream terminal-ansi-stream))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-ansi-stream) column)
  (write-sequence *endless-spaces*
		  (terminal-output-stream stream) :start 0
		  :end (- column (stream-line-column stream)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-ansi-stream))

;; (defmethod stream-line-length ((stream terminal-ansi-stream))
;;   )

(defmethod stream-write-char ((stream terminal-ansi-stream) char
			     #| &optional start end |#)
  (terminal-write-char stream char))

(defmethod stream-write-string ((stream terminal-ansi-stream) string
			       &optional start end)
  (terminal-write-string stream string :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-ansi, which is also an input stream.

(defmethod stream-clear-input ((stream terminal-ansi))
  (with-slots (typeahead typeahead-pos output-stream) stream
    (setf typeahead nil
	  typeahead-pos nil)
    (clear-input output-stream)))

(defmethod stream-read-sequence ((stream terminal-ansi) seq start end
				 &key &allow-other-keys
				 #| &optional (start 0) end |#)
  nil)

;;(defgeneric stream-peek-char ((stream terminal-ansi))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-ansi))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (get-char stream :timeout 0))

(defmethod stream-read-char ((stream terminal-ansi))
  (terminal-get-char stream))

(defmethod stream-read-line ((stream terminal-ansi))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (multiple-value-bind (result got-eof)
      (read-until (terminal-file-descriptor stream) #\newline)
    (values (or result "")
	    got-eof)))

(defmethod stream-listen ((stream terminal-ansi))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (with-slots (typeahead output-stream) stream
    (or typeahead
	(terminal-listen-for stream 0))))

(defmethod stream-unread-char ((stream terminal-ansi) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (add-typeahead stream character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :ansi 'terminal-ansi)

;; EOF
