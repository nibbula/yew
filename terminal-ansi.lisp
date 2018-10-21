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
	:trivial-gray-streams :fatchar :color :terminal-crunch
	#+unix :opsys-unix #+unix :cffi)
  (:export
   #:terminal-ansi-stream
   #:terminal-ansi
   ;; extensions:
   #:describe-terminal
   #:+csi+ #:+st+ #:+osc+
   #:query-parameters #:query-string
   #:with-raw
   ;; #:with-immediate
   #:set-bracketed-paste-mode
   #:read-bracketed-paste
   #:foreground-color
   #:background-color
   #:set-foreground-color
   #:set-background-color
   #:reset-color-pallet
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

(defparameter *attributes*
  '((:normal	       . 22)		; not bold or faint
    (:bold	       . 1)
    (:faint	       . 2)
    (:dim	       . 2)
    (:italic	       . 3)
    (:underline	       . 4)
    (:blink	       . 5)
    (:inverse	       . 7)
    (:reverse	       . 7)
    (:standout	       . 7)
    (:invisible	       . 8)
    (:crossed-out      . 9)
    (:double-underline . 21)))

(defparameter *attributes-off*
  '((:all	       . 0)		; No attributes
    (:bold	       . 22)
    (:faint	       . 22)
    (:dim	       . 22)
    (:italic	       . 23)
    (:underline	       . 24)
    (:blink	       . 25)
    (:inverse	       . 27)
    (:reverse	       . 27)
    (:standout	       . 27)
    (:invisible	       . 28)
    (:crossed-out      . 29)
    (:double-underline . 24)))		; same as not underline

(defclass terminal-ansi-stream (terminal-stream)
  ((fake-column
   :initarg :fake-column :accessor terminal-ansi-stream-fake-column
   :initform 0 :type fixnum
   :documentation "Guess for the current column.")
   (cost-stream
    :initarg :cost-stream :accessor terminal-ansi-stream-cost-stream
    :initform nil
    :documentation
    "Another terminal-ansi-stream to keep around for calculating costs.")
   (line-buffered-p
    :initarg :line-buffered-p :accessor line-buffered-p
    :initform nil :type boolean
    :documentation "True if we always flush after outputting a newline.")
   (translate-alternate-characters
    :initarg :translate-alternate-characters
    :accessor translate-alternate-characters
    :initform nil :type boolean
    :documentation
    "True to translate some unicode characters into the alternate character set."))
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defmethod terminal-start ((tty terminal-ansi-stream))
  "This doesn't do anything for a stream."
  (declare (ignore tty)))

(defmethod terminal-end ((tty terminal-ansi-stream) &optional state)
  "Stop using a stream."
  (declare (ignore state))
  (terminal-finish-output tty))

(defmethod terminal-done ((tty terminal-ansi-stream) &optional state)
  "Forget about the whole terminal stream."
  (declare (ignore state))
  (terminal-end tty)
  ;; don't close the stream
  (values))

(defmethod terminal-has-attribute ((tty terminal-ansi-stream) attribute)
  "Return true if the terminal can display the character attribute."
  (case attribute
    ((:standout :underline :bold :inverse :color) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass terminal-ansi (terminal terminal-ansi-stream)
  ((typeahead
    :accessor typeahead
    :initform nil
    :initarg :typeahead
    :documentation "Things already input, dag blast it.")
   (saved-mode
    :initarg :saved-mode :accessor saved-mode
    :documentation "Saved terminal modes for restoring on exit.")
   (previous-siggy
    :initarg :previous-siggy :accessor previous-siggy
    :initform nil
    :documentation "Previous signal handler if we set it.")
   (mouse-down
    :initarg :mouse-down :accessor mouse-down :initform nil :type boolean
    :documentation "True if we think a mouse button is down."))
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
  (push thing (typeahead tty)))

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
    ;; (dbugf :terminal-ansi "eat typeahead ~a ~a~%" fd (uos:tcgetpgrp fd))
    (with-terminal-mode (fd)
      (set-terminal-mode fd :raw t)
      (setf ta (slurp-terminal fd :timeout 1))
      (when (and ta (> (length ta) 0))
	;; (log-message e "ta[~a]=~w" (length ta) ta)
	;; (if (typeahead tty)
	;; 	  (setf (typeahead tty) (s+ (typeahead tty) ta))
	;; 	  (setf (typeahead tty) ta
	;; 		(typeahead-pos tty) 0)))))
	(loop :for i :from (1- (length ta)) :downto 0 :do
	   (add-typeahead tty (aref ta i)))))))

(defmethod terminal-get-cursor-position ((tty terminal-ansi))
  "Try to somehow get the row of the screen the cursor is on. Returns the
two values ROW and COLUMN."
  (eat-typeahead tty)
  (let ((row 1) (col 1) sep
	(result (terminal-report tty #\R "~c[6n" #\escape)))
    ;; (format t "result = ~s~%" (coerce result 'list))
    (when (and result (>= (length result) 5))
      (setf sep (position #\; result)
	    row (parse-integer (subseq result 2 sep) :junk-allowed t)
	    col (parse-integer (subseq result (1+ sep) (length result))
			       :junk-allowed t)))
    #| @@@ temporarily get rid of this error |#
    (if (or (not row) (not col))
	;; Probabbly because there was other I/O going on.
	(values 0 0)
	;;(error "terminal reporting failed"))
	(values (1- row) (1- col)))))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

;; Signal handlers are a very stupid idea.
(defcallback extraneous-sigwinch-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  (when (find :resize (terminal-events-enabled *terminal*))
    ;; If defcallback doesn't prevent GC, then we're screwed.
    ;; A without-gcing would have to be called before entering here.
    ;; This might be too dangerous.
    (add-typeahead *terminal* :resize)))

(defmethod terminal-start ((tty terminal-ansi))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots ((file-descriptor	   terminal::file-descriptor)
	       (device-name   	   terminal::device-name)
	       (output-stream 	   terminal::output-stream)
	       saved-mode) tty
    (when (not file-descriptor)
      ;; (format t "[terminal-open ~s]~%" device-name)
      (setf file-descriptor (open-terminal (or device-name
					       *default-device-name*)
					   :input)))
    ;; (dbug "terminal-ansi open in~%")
    (setf saved-mode (get-terminal-mode file-descriptor))
    (dbugf :terminal-ansi "saving terminal modes ~s ~s~%" tty saved-mode)
    (when (or (terminal-mode-line saved-mode)
	      (terminal-mode-echo saved-mode))
      (set-terminal-mode file-descriptor :line nil :echo nil))
    (when (not output-stream)
      (setf output-stream (open-terminal
			   (or device-name *default-device-name*)
			   :output))
      (dbugf :terminal-ansi "terminal-ansi open out ~s~%" output-stream)
      ;; @@@ Why do we have to do this?
      #+ccl (setf (stream-external-format output-stream)
		  (ccl:make-external-format :character-encoding :utf-8
					    :domain :file))
      )
    (terminal-get-size tty)
    #+unix
    (when (not (equal (signal-action +SIGWINCH+) 'extraneous-sigwinch-handler))
      (setf (previous-siggy tty) (signal-action +SIGWINCH+))
      (set-signal-action +SIGWINCH+ 'extraneous-sigwinch-handler))
    saved-mode))

(defmethod terminal-end ((tty terminal-ansi) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  ;;  (format t "[terminal-end]~%")
  ;; (set-terminal-mode (terminal-file-descriptor tty)
  ;; 		     :line t :echo t :raw nil :timeout nil)
  (when (or state (saved-mode tty))
    (dbugf :terminal-ansi "restoring terminal modes ~s ~s~%"
	   tty (or state (saved-mode tty)))
    (set-terminal-mode (terminal-file-descriptor tty)
		       :mode (or state (saved-mode tty)))
    #+unix
    (when (previous-siggy tty)
      (set-signal-action +SIGWINCH+ (previous-siggy tty)))))

(defmethod terminal-done ((tty terminal-ansi) &optional state)
  "Forget about the whole terminal thing and stuff."
  (terminal-end tty state)
  (close-terminal (terminal-file-descriptor tty))
  ;; (dbug "terminal-ansi close in~%")
  (when (terminal-output-stream tty)
    (close-terminal (terminal-output-stream tty)))
  ;; (dbug "terminal-ansi close out~%")
  ;; (format t "[terminal-done]~%")
  ;; (setf *tty* nil)
  (values))

(defparameter *acs-table* nil
  "Hash table of unicode character to ACS character.")

;; Modern curses “cheats” and uses some unicode characters. This junk is
;; for antique terminals or emulators that can't do that. If you want the
;; unicode characters, you can just print them normally.
(defparameter *acs-table-data*
  `((#.(code-char #x250c) . #\l) ;; upper left corner         ulcorner   ┌
    (#.(code-char #x2514) . #\m) ;; lower left corner         llcorner   └
    (#.(code-char #x2510) . #\k) ;; upper right corner        urcorner   ┐
    (#.(code-char #x2518) . #\j) ;; lower right corner        lrcorner   ┘
    (#.(code-char #x251c) . #\t) ;; tee pointing right        ltee       ├
    (#.(code-char #x2524) . #\u) ;; tee pointing left         rtee       ┤
    (#.(code-char #x2534) . #\v) ;; tee pointing up           btee       ┴
    (#.(code-char #x252c) . #\w) ;; tee pointing down         ttee       ┬
    (#.(code-char #x2500) . #\q) ;; horizontal line           hline      ─
    (#.(code-char #x2502) . #\x) ;; vertical line             vline      │
    (#.(code-char #x253c) . #\n) ;; large plus or crossover   plus       ┼
    (#.(code-char #x23ba) . #\o) ;; scan line 1               s1         ⎺
    (#.(code-char #x23bd) . #\s) ;; scan line 9               s9         ⎽
    (#.(code-char #x25c6) . #\`) ;; diamond                   diamond    ◆
    (#.(code-char #x2592) . #\a) ;; checker board (stipple)   ckboard    ▒
    (#.(code-char #x00b0) . #\f) ;; degree symbol             degree     °
    (#.(code-char #x00b1) . #\g) ;; plus/minus                plminus    ±
    (#.(code-char #x00b7) . #\~) ;; bullet                    bullet     ·
    (#.(code-char #x2190) . #\,) ;; arrow pointing left       larrow     ←
    (#.(code-char #x2192) . #\+) ;; arrow pointing right      rarrow     →
    (#.(code-char #x2193) . #\.) ;; arrow pointing down       darrow     ↓
    (#.(code-char #x2191) . #\-) ;; arrow pointing up         uarrow     ↑
    (#.(code-char #x2591) . #\h) ;; board of squares          board      ▒
    (#.(code-char #x240b) . #\i) ;; lantern symbol            lantern    ␋
    (#.(code-char #x2588) . #\a) ;; solid square block        block      █
    (#.(code-char #x23bb) . #\p) ;; scan line 3               s3         ⎻
    (#.(code-char #x23bc) . #\r) ;; scan line 7               s7         ⎼
    (#.(code-char #x2264) . #\y) ;; less/equal                lequal     ≤
    (#.(code-char #x2265) . #\z) ;; greater/equal             gequal     ≥
    (#.(code-char #x03c0) . #\{) ;; Pi                        pi         π
    (#.(code-char #x2260) . #\|) ;; not equal                 nequal     ≠
    (#.(code-char #x00a3) . #\}) ;; UK pound sign             sterling   £
    ))

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

(defun update-column (tty thing &key start end)
  (etypecase thing
    (character (update-column-for-char tty thing))
    (string
     (loop
	:with the-end = (or end (length thing))
	:and the-start = (or start 0)
	:for i :from the-start :below the-end
	:do (update-column-for-char tty (char thing i))))))

(defun make-acs-table ()
  "Make the alternate character set table."
  (setf *acs-table* (make-hash-table))
  (loop :for (uc . ac) :in *acs-table-data* :do
     (setf (gethash uc *acs-table*) ac)))

(defun translate-acs-chars (string &key start end)
  "Translate unicode characters to alternate character set characters.
Only replace in START and END range."
  (if (or start end)
      (loop :with replacement
	 :for i :from (or start 0) :below (or end (length string))
	 :do
	 (setf replacement (gethash (char string i) *acs-table*))
	 (when replacement
	   (setf (char string i) replacement)))
      ;; Assuming this could be faster:
      (map 'string (_ (or (gethash _ *acs-table*) _)) string)))

(defgeneric terminal-raw-format (tty fmt &rest args))
(defmethod terminal-raw-format ((tty terminal-ansi-stream) fmt &rest args)
  "Output a formatted string to the terminal, without doing any content
processing."
  ;; (let ((string (apply #'format nil fmt args))
  ;; 	(stream (terminal-output-stream tty)))
  ;;   (write-string string stream)))
  (apply #'format (terminal-output-stream tty) fmt args))

(defmethod terminal-format ((tty terminal-ansi-stream) fmt &rest args)
  "Output a formatted string to the terminal."
  (let ((string (apply #'format nil fmt args)))
    ;; @@@ Let's try to think of some way we could do the ACS translation.
    ;; For now, ACS characters printed with format might not work right.
    ;; (apply #'format (terminal-output-stream tty) fmt args)
    (apply #'format tty fmt args)
    (update-column tty string)
    (when (and (line-buffered-p tty) (position #\newline string))
      (finish-output tty))))

(defmethod terminal-alternate-characters ((tty terminal-ansi-stream) state)
  (setf (translate-alternate-characters tty) state)
  (when (and state (not *acs-table*))
    (make-acs-table))
  (if state
      (terminal-raw-format tty "~c(0" #\escape)
      (terminal-raw-format tty "~c(B" #\escape)))

(defvar *allow-resize* nil
  "True to allow throwing resize events.")

(defmacro with-resize (&body body)
  `(let ((*allow-resize* t))
     (catch 'resize-event
       ,@body)))

;; resumed -> (terminal-start tty) #| (redraw) |# (terminal-finish-output tty)
;; resized -> (terminal-get-size tt)

(defmacro with-interrupts-handled ((tty) &body body)
  "Evaluate the BODY while handling terminal interrupts on TTY appropritately.
TTY is a terminal, in case you didn't know."
  (with-unique-names (borked result)
    `(let (,result ,borked)
       (loop :do
	  (setf ,borked nil)
	  (handler-case
	      (setf ,result (progn ,@body))
	    (opsys-resumed ()
	      (terminal-start ,tty) (terminal-finish-output ,tty)
	      (setf ,borked t))
	    (opsys-resized ()
	      (dbugf :terminal "events enabled ~s~%"
		     (terminal-events-enabled ,tty))
	      (terminal-get-size ,tty)
	      (when (and (find :resize (terminal-events-enabled ,tty))
			 *allow-resize*)
		(dbugf :terminal "terminal-ansi resize event is enabled~%")
		(throw 'resize-event :resize))
	      (setf ,borked t)))
	  :while ,borked)
       ,result)))

(defmacro with-raw ((tty) &body body)
  (with-unique-names (mode)
    `(let ((,mode (get-terminal-mode ,tty)))
       (unwind-protect
	    (progn
	      (set-terminal-mode ,tty :raw t :echo nil)
	      ,@body)
	 (set-terminal-mode ,tty :mode ,mode)))))

;; (defmacro with-immediate ((tty) &body body)
;;   (with-unique-names (mode)
;;     `(let ((,mode (get-terminal-mode (terminal-file-descriptor ,tty))))
;;        (unwind-protect
;; 	    (progn
;; 	      (set-terminal-mode (terminal-file-descriptor ,tty)
;; 				 :line nil :echo nil)
;; 	      ,@body)
;; 	 (set-terminal-mode (terminal-file-descriptor ,tty) :mode ,mode)))))

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
		 (with-interrupts-handled (tty)
		   (read-until fd end-char :timeout 1)))))
      #| @@@ temporarily get rid of this error
      (when (null str)
	(error "Terminal failed to report \"~a\"." fmt))
      |#
      str)))

(defun %write-string/line (tty str func start end)
  (when (not (and (and start (zerop start)) (and end (zerop end))))
    (let ((stream (terminal-output-stream tty)))
      (when (and (translate-alternate-characters tty)
		 (stringp str))
	(translate-acs-chars str :start start :end end))
      (apply func `(,str ,stream
			 ,@(and start `(:start ,start))
			 ,@(and end `(:end ,end))))
      ;;(write-string str stream :start start :end end)
      (update-column tty str :start start :end end))))

(defmethod terminal-write-string ((tty terminal-ansi-stream) str
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (%write-string/line tty str #'write-string start end)
  (when (and (line-buffered-p tty)
	     (apply #'position `(#\newline ,str
					   ,@(and start `(:start ,start))
					   ,@(and end `(:end ,end)))))
    (finish-output (terminal-output-stream tty))))

(defmethod terminal-write-line ((tty terminal-ansi-stream) str
				&key start end)
  "Output a string to the terminal, followed by a newline."
  (%write-string/line tty str #'write-line start end)
  (update-column tty #\newline)
  (when (line-buffered-p tty)
    (finish-output (terminal-output-stream tty))))

(defmethod terminal-write-char ((tty terminal-ansi-stream) char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (when (and (translate-alternate-characters tty)
	       (characterp char))
      (let ((replacement (gethash char *acs-table*)))
	(when replacement
	  (setf char replacement))))
    (write-char char stream)
    (update-column tty char)
    (when (and (line-buffered-p tty) (eql char #\newline))
      (finish-output stream))))

(defparameter *line-table-unicode*
  `#(,#\space
     ,(code-char #x2577) ;; #\box_drawings_light_down)                     ╷
     ,(code-char #x2576) ;; #\box_drawings_light_right)                    ╶
     ,(code-char #x250c) ;; #\box_drawings_light_down_and_right)           ┌
     ,(code-char #x2575) ;; #\box_drawings_light_up)                       ╵
     ,(code-char #x2502) ;; #\box_drawings_light_vertical)                 │
     ,(code-char #x2514) ;; #\box_drawings_light_up_and_right)             └
     ,(code-char #x251c) ;; #\box_drawings_light_vertical_and_right)       ├
     ,(code-char #x2574) ;; #\box_drawings_light_left)                     ╴
     ,(code-char #x2510) ;; #\box_drawings_light_down_and_left)            ┐
     ,(code-char #x2500) ;; #\box_drawings_light_horizontal)               ─
     ,(code-char #x252c) ;; #\box_drawings_light_down_and_horizontal)      ┬
     ,(code-char #x2518) ;; #\box_drawings_light_up_and_left)              ┘
     ,(code-char #x2524) ;; #\box_drawings_light_vertical_and_left)        ┤
     ,(code-char #x2534) ;; #\box_drawings_light_up_and_horizontal)        ┴
     ,(code-char #x253c) ;; #\box_drawings_light_vertical_and_horizontal)  ┼
     )
  "Line drawing characters from Unicode.")

(defparameter *line-table-vt100*
  `#(#\space ;;            0 - 0000 - blank
     #\x     ;; VLINE      1 - 0001 - bottom
     #\q     ;; HLINE      2 - 0010 - right
     #\l     ;; ULCORNER   3 - 0011 - bottom + right
     #\x     ;; VLINE      4 - 0100 - top
     #\x     ;; VLINE      5 - 0101 - top + bottom
     #\m     ;; LLCORNER   6 - 0110 - top + right
     #\t     ;; LTEE       7 - 0111 - bottom + right + top
     #\q     ;; HLINE      8 - 1000 - left
     #\k     ;; URCORNER   9 - 1001 - left + bottom
     #\q     ;; HLINE     10 - 1010 - left + right
     #\w     ;; TTEE      11 - 1011 - left + right + bottom
     #\j     ;; LRCORNER  12 - 1100 - left + top
     #\t     ;; RTEE      13 - 1101 - left + top + bottom
     #\v     ;; BTEE      14 - 1110 - left + top + right
     #\n     ;; PLUS      15 - 1111 - left + top + right + bottom
     ))

(defparameter *line-table* *line-table-unicode* ;; *line-table-vt100*
  "The table to use for looking up line drawing characters.")

(defun line-char (line)
  "Convert line bits into line drawing characters."
  (aref *line-table* line))

(define-constant +intro+ (s+ +csi+ "0") "Introduce a new fatchar.")
(define-constant +zero-effect+ (s+ +csi+ "m") "No effects.")

;; This is a slightly more direct way to write a fatchar than with
;; fatchar:render-fatchar.
(defun %terminal-write-char (tty char &key reset)
  "Output a fatchar to the terminal. Flush output if it is a newline,
i.e. the terminal is 'line buffered'."
  (let ((stream (terminal-output-stream tty)))
    (with-slots ((cc fatchar::c)
		 (fg fatchar::fg)
		 (bg fatchar::bg)
		 (line fatchar::line)
		 (attrs fatchar::attrs)) char
      ;; We still do this dumb replacing, just in case.
      (when (and (translate-alternate-characters tty)
		 (characterp cc))
	(let ((replacement (gethash cc *acs-table*)))
	  (when replacement
	    (setf cc replacement))))
      (if (or fg bg attrs)
	  (progn
	    (when (or fg bg)
	      (write-string +intro+ stream)
	      (write-char #\; stream)
	      (%terminal-color tty fg bg :unwrapped t))
	    (when attrs
	      (when (not (or fg bg))
		(write-string +intro+ stream))
	      (loop :with n
		 :for a :in attrs :do
		 (when (setf n (assoc a *attributes*))
		   (terminal-raw-format tty ";~d" (cdr n)))))
	    (write-char #\m stream))
	  (write-string +zero-effect+ stream))
      (if (zerop line)
	  (write-char cc stream)
	  (write-char (line-char line) stream))
      (update-column tty cc)
      (when (and (line-buffered-p tty) (eql cc #\newline))
	(finish-output stream))
      (when reset
	(write-string +zero-effect+ stream)))))

(defmethod terminal-write-char ((tty terminal-ansi-stream) (char fatchar))
  (%terminal-write-char tty char :reset t))

(defun %write-fat-string (tty str start end)
  "Write a fat string STR to TTY from START to END. Return true if there was a
newline in it."
  (when (and (not (and (and start (zerop start)) (and end (zerop end))))
	     (fat-string-string str))
    (let ((stream (terminal-output-stream tty))
	  (fs (fat-string-string str))
	  (translate (translate-alternate-characters tty))
	  had-newline replacement)
      (loop
	 :with i = (or start 0)
	 :and our-end = (or end (length fs))
	 :and c :and last-c
	 :while (< i our-end)
	 :do
	 (setf c (aref fs i))
	 (with-slots ((cc fatchar::c)
		      (line fatchar::line)) c
	   (if (and last-c (same-effects c last-c))
	       (if (zerop line)
		   (if (and translate
			    (setf replacement (gethash cc *acs-table*)))
		       (write-char replacement stream)
		       (write-char cc stream))
		   (write-char (line-char line) stream))
	       (progn
		 ;;(terminal-raw-format tty "~c[0m" #\escape)
		 (%terminal-write-char tty c :reset nil)))
	   (setf last-c c)
	   (when (char= cc #\newline)
	     (setf had-newline t))
	   (update-column tty cc))
	 (incf i))
      (write-string +zero-effect+ stream)
      had-newline)))

(defmethod terminal-write-string ((tty terminal-ansi-stream) (str fat-string)
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (when (and (%write-fat-string tty str start end) (line-buffered-p tty))
    (finish-output (terminal-output-stream tty))))

(defmethod terminal-write-line ((tty terminal-ansi-stream) (str fat-string)
				&key start end)
  "Output a string to the terminal, followed by a newline."
  (%write-fat-string tty str start end)
  (write-char #\newline (terminal-output-stream tty))
  (when (line-buffered-p tty)
    (finish-output (terminal-output-stream tty))))

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

(defmethod terminal-delete-char ((tty terminal-ansi-stream) n)
  (terminal-raw-format tty "~c[~aP" #\escape (if (> n 1) n "")))

(defmethod terminal-insert-char ((tty terminal-ansi-stream) n)
  (terminal-raw-format tty "~c[~a@" #\escape (if (> n 1) n "")))

(defun moverize (tty n pos neg)
  (cond
    ((= n 1)  (terminal-raw-format tty "~c[~c" #\escape pos))
    ((> n 1)  (terminal-raw-format tty "~c[~d~c" #\escape n pos))
    ((= n 0)  #| do nothing |#)
    ((= n -1) (terminal-raw-format tty "~c[~c" #\escape neg))
    ((< n -1) (terminal-raw-format tty "~c[~d~c" #\escape n neg))))

(defmethod terminal-backward ((tty terminal-ansi-stream) &optional (n 1))
  (moverize tty n #\D #\C)
  (decf (terminal-ansi-stream-fake-column tty) n))

(defmethod terminal-forward ((tty terminal-ansi-stream) &optional (n 1))
  (moverize tty n #\C #\D)
  (incf (terminal-ansi-stream-fake-column tty) n))

(defmethod terminal-up ((tty terminal-ansi-stream) &optional (n 1))
  (moverize tty n #\A #\B))

(defmethod terminal-down ((tty terminal-ansi-stream) &optional (n 1))
  (moverize tty n #\B #\A))

(defmethod terminal-scroll-down ((tty terminal-ansi-stream) n)
  (when (> n 0)
    (loop :with stream = (terminal-output-stream tty) and i = 0
       :while (< i n)
       :do (write-char #\newline stream) (incf i)
       :finally (when (line-buffered-p tty) (finish-output stream)))))

(defmethod terminal-scroll-up ((tty terminal-ansi-stream) n)
  (when (> n 0)
    (loop :with stream = (terminal-output-stream tty) and i = 0
       :while (< i n)
       :do (terminal-raw-format tty "~cM" #\escape)
       (incf i)
       :finally (when (line-buffered-p tty) (finish-output stream)))))

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

(defparameter *colors*
  #(:black :red :green :yellow :blue :magenta :cyan :white nil :default))

(defun foreground-color ()
  "Get the default foreground color for text."
  (let ((qq (query-string (s+ "10;?" +st+) :lead-in +osc+ :offset 5
			  :tty (terminal-file-descriptor *terminal*))))
    (and qq (xcolor-to-color qq))))

(defun background-color ()
  "Get the default background color for text."
  (let ((qq (query-string (s+ "11;?" +st+) :lead-in +osc+ :offset 5
			  :tty (terminal-file-descriptor *terminal*))))
    (and qq (xcolor-to-color qq))))

(defun set-foreground-color (color)
  "Set the default forground color for text."
  (when (not (known-color-p color))
    (error "Unknown color ~s." color))
  (tt-format "~a10;~a~a" +osc+ (color-to-xcolor (lookup-color color)) +st+))

(defun set-background-color (color)
  "Set the default background color for the terminal."
  (when (not (known-color-p color))
    (error "Unknown color ~s." color))
  (tt-format "~a11;~a~a" +osc+ (color-to-xcolor (lookup-color color)) +st+))

(defun reset-color-pallet ()
  "Reset the whole color pallet to the default."
  (terminal-raw-format *terminal* "~a104~a" +osc+ +st+))

(defun %terminal-color (tty fg bg &key unwrapped)
  (let ((fg-pos (and (keywordp fg) (position fg *colors*)))
	(bg-pos (and (keywordp bg) (position bg *colors*)))
	did-one)
    (when (and (keywordp fg) (not fg-pos))
      (error "Forground ~a is not a known color." fg))
    (when (and (keywordp bg) (not bg-pos))
      (error "Background ~a is not a known color." bg))
    (when (not unwrapped)
      (terminal-raw-format tty +csi+))
    (when (structured-color-p fg)
      (let ((c (convert-color-to fg :rgb8)))
	(terminal-raw-format tty "38;2;~d;~d;~d" 
			     (color-component c :red)
			     (color-component c :green)
			     (color-component c :blue))
	(setf did-one t)))
    (when (structured-color-p bg)
      (let ((c (convert-color-to bg :rgb8)))
	(if did-one (write-char #\; (terminal-output-stream tty)))
	(terminal-raw-format tty "48;2;~d;~d;~d"
			     (color-component c :red)
			     (color-component c :green)
			     (color-component c :blue))
	(setf did-one t)))
    (cond
      ((and fg bg fg-pos bg-pos)
       (terminal-raw-format tty "~d;~d" (+ 30 fg-pos) (+ 40 bg-pos)))
      ((and fg fg-pos)
       (if did-one (write-char #\; (terminal-output-stream tty)))
       (terminal-raw-format tty "~d" (+ 30 fg-pos)))
      ((and bg bg-pos)
       (if did-one (write-char #\; (terminal-output-stream tty)))
       (terminal-raw-format tty "~d" (+ 40 bg-pos))))
    (when (not unwrapped)
      (terminal-raw-format tty "m"))))

(defmethod terminal-color ((tty terminal-ansi-stream) fg bg)
  (%terminal-color tty fg bg))

;; 256 color? ^[[ 38;5;color <-fg 48;5;color <- bg
;; set color tab = ^[] Ps ; Pt BEL
;;;  4; color-number ; #rrggbb ala XParseColor

(defmethod terminal-beep ((tty terminal-ansi-stream))
  (terminal-write-char tty #\bel))		; Not #\bell!!

(defmethod terminal-set-scrolling-region ((tty terminal-ansi-stream) start end)
  (if (and (not start) (not end))
      (terminal-raw-format tty "~c[r" #\escape)
      (terminal-raw-format tty "~c[~d;~dr" #\escape start end)))

(defmethod terminal-set-attributes ((tty terminal-ansi-stream) attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords."
  (with-slots ((stream terminal::output-stream)) tty
    (etypecase attributes
      (list
       (write-string +csi+ stream)
       (loop :with n :and first = t
	  :for a :in attributes :do
	  (when (setf n (assoc a *attributes*))
	    (if first
		(setf first nil)
		(write-char #\; stream))
	    (terminal-raw-format tty "~d" (cdr n))))
       (write-char #\m stream))
      (keyword
       (let ((n (assoc attributes *attributes*)))
	 (when n
	   (terminal-raw-format tty "~a~dm" +csi+ (cdr n))))))))

(defmethod terminal-finish-output ((tty terminal-ansi-stream))
  ;; (when (maybe-refer-to :cl-user :*duh*)
  ;;   (with-new-terminal (:ansi
  ;; 			*terminal*
  ;; 			:device-name (nos:file-handle-terminal-name
  ;; 				      (nos:stream-system-handle *debug-io*))
  ;; 			:output-stream (make-broadcast-stream *debug-io*))
  ;;     (symbol-call :deblarg :debugger-backtrace 20))
  ;;   ;; (cerror "Nah" "Crap")
  ;;   )
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
  (with-slots (typeahead
	       (file-descriptor terminal::file-descriptor)) tty
    (when typeahead
      (return-from get-char
	(pop typeahead)))
    (let (result)
      (labels ((read-it ()
		 (or
		  (read-terminal-byte file-descriptor :timeout timeout)
		  (return-from get-char nil)))
	       (set-it (x)
		 (setf result x)))
	(with-interrupts-handled (tty)
	  (with-terminal-signals ()
	    (char-util::%get-utf8b-char read-it set-it)))
	result))))

(defun raw-get-char (tty &key timeout)
  "Like get-char, but without UTF8b processing."
  (with-slots (typeahead
	       (file-descriptor terminal::file-descriptor)) tty
    (when typeahead
      (return-from raw-get-char
	(pop typeahead)))
    (with-interrupts-handled (tty)
      (with-terminal-signals ()
	(read-terminal-byte file-descriptor :timeout timeout)))))

(defmethod terminal-get-char ((tty terminal-ansi))
  "Read a character from the terminal."
  (terminal-finish-output tty)
  ;;(read-terminal-char tty))
  (get-char tty))

(defparameter *key-tag*
  '((#\A . :up) 			; Arrow keys
    (#\B . :down)
    (#\C . :right)
    (#\D . :left)
    (#\E . :center)			; center of the keypad
    (#\F . :end)
    (#\H . :home)			; Movement keys
    (#\P . :f1)				; function keys
    (#\Q . :f2)
    (#\R . :f3)
    (#\S . :f4)
    (#\Z . :back-tab)			; non-standard
    ))

(defparameter *key-num*
  '((2  . :insert)			; Editing keys
    (3  . :delete)
    (5  . :page-up)
    (6  . :page-down)
    (15 . :f5)				; Function keys
    (17 . :f6)
    (18 . :f7)
    (19 . :f8)
    (20 . :f9)
    (21 . :f10)
    (23 . :f11)
    (24 . :f12)
    (200 . :bracketed-paste)))

(defun modifier-prefixed (symbol params)
  "Return a keyword of SYMBOL prefixed by modifiers determined in PARAMS."
  (if (second params)
      (intern (format nil "~:[~;S-~]~:[~;A-~]~:[~;C-~]~:[~;M-~]~@:(~a~)"
		      (logtest (1- (second params)) (ash 1 0))
		      (logtest (1- (second params)) (ash 1 1))
		      (logtest (1- (second params)) (ash 1 2))
		      (logtest (1- (second params)) (ash 1 3))
		      (symbol-name symbol)) :keyword)
      symbol))

;; @@@ should probably make an event class
(defun get-mouse-event (tty)
  "Read a mouse event from TTY. Something like:
  (:mouse x y [:button-<n> | :release] [:motion] [modifiers..])"
  (block nil
    (flet ((next ()
	     (or (raw-get-char tty :timeout 1) (return :mouse-error))))
      (with-slots (mouse-down) tty
	(let (cb cx cy result button button-change motion)
	  (setf cb (next)
		cx (1- (- (next) 32))	; terminal coords start at 1
		cy (1- (- (next) 32)))
	  (push :mouse result)
	  (push cx result)
	  (push cy result)
	  (when (not (zerop (logand cb #b0100000))) (setf button-change t))
	  (when (not (zerop (logand cb #b1000000))) (setf motion t))
	  (case (logand cb #b11)
	    (0 (setf button (if (and button-change motion) :button-4 :button-1)
		     mouse-down t))
	    (1 (setf button (if (and button-change motion) :button-5 :button-2)
		     mouse-down t))
	    (2 (setf button :button-3 mouse-down t))
	    (3 (setf button :release mouse-down nil)))
	  (push button result)
	  (when (and motion (not button-change))
	    (push :motion result))
	  (when (not (zerop (logand cb #b00100))) (push :shift   result))
	  (when (not (zerop (logand cb #b01000))) (push :meta    result))
	  (when (not (zerop (logand cb #b10000))) (push :control result))
	  ;;(push cb result)
	  (nreverse result))))))

;; This can unfortunately really vary between emulations, so we try to code
;; for multiple interpretations.
(defun read-function-key (tty &key app-key-p)
  "Read the part of a function key after the lead in and return a keyword
representing the key. The lead in is ESC O if APP-KEY-P is true, and ESC [
otherwise. If we don't recognize the key, return #\escape and add the
characters to the typeahead."
  (let ((c (get-char tty :timeout 1))
	(start-char (if app-key-p "O" "["))
	k)
    (labels ((read-number ()
	       (let ((num (parse-integer (string c))))
		 (setf c (get-char tty :timeout 1))
		 (loop :while (digit-char-p c)
		    :do
		    (setf num (+ (* num 10) (parse-integer (string c))))
		    (setf c (get-char tty :timeout 1)))
		 num))
	     (read-params ()
	       (let (params)
		 (loop :do (push (read-number) params)
		    :while (eql c #\;)
		    :do (setf c (get-char tty :timeout 1)))
		 (reverse params))))
      (cond
	((setf k (assoc c *key-tag*))
	 (cdr k))
	((null c)			; timeout
	 (add-typeahead tty start-char)
	 #\escape)
	((char= c #\M) 			; mouse
	 (get-mouse-event tty))
	((digit-char-p c)
	 ;; read a parameters followed by a tilde or tag
	 (let ((param (read-params)))
	   (cond
	     ((and (eql c #\~) (not app-key-p))
	      (setf k (assoc (first param) *key-num*))
	      (modifier-prefixed (cdr k) param))
	     ((setf k (assoc c *key-tag*))
	      (modifier-prefixed (cdr k) param))
	     (t ;; Stuff whatever characters we read.
	      (add-typeahead tty start-char)
	      (when (first param)
		(add-typeahead tty (digit-char (car param)))
		)
	      (loop :for p :in (rest param) :do
		 (add-typeahead tty #\;)
		 (add-typeahead tty (digit-char p))
		 )
	      (when c
		(add-typeahead tty c))
	      #\escape))))))))

#|
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
       (add-typeahead tty #\O)
       (when c
	 (add-typeahead tty c))
       #\escape))))
|#

(defmethod terminal-get-key ((tty terminal-ansi))
  (terminal-finish-output tty)
  (let ((c (with-resize (get-char tty))))
    (if (and c (eql c #\escape))
	(case (setf c (get-char tty :timeout 1))
	  (#\[ (read-function-key tty))
	  (#\O (read-function-key tty :app-key-p t))
	  (t
	   (when c ;; if it didn't time out
	     (add-typeahead tty c))
	   #\escape))
	c)))

(defmethod terminal-listen-for ((tty terminal-ansi) seconds)
  (let (result) ;; @@@ I think this "result" is superfluous.
    (when (eq (with-resize
		  (with-interrupts-handled (tty)
		    (with-terminal-signals ()
		      (setf result (listen-for
				    seconds (terminal-file-descriptor tty))))))
	      :resize)
      (dbugf :resize "terminal-ansi resized in listen~%")
      (add-typeahead tty :resize)
      (setf result t))
    result))

(defmethod terminal-input-mode ((tty terminal-ansi))
  (let ((mode (get-terminal-mode (terminal-file-descriptor tty))))
    (and mode
	 (if (terminal-mode-line mode) :line :char))))

(defmethod (setf terminal-input-mode) (mode (tty terminal-ansi))
  (case mode
    (:line
     (set-terminal-mode (terminal-file-descriptor tty) :line t :echo t))
    (:char
     (set-terminal-mode (terminal-file-descriptor tty) :line nil :echo nil))
    (t (error "Unknown terminal input mode ~s" mode))))

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
  (reset-terminal-modes :file-descriptor (terminal-file-descriptor tty))
  (call-next-method)) ;; Do the terminal-stream version

(defmethod terminal-save-cursor ((tty terminal-ansi))
  "Save the cursor position."
  (terminal-format tty "~c7" #\escape)
  ;; (terminal-finish-output tty)
  )

(defmethod terminal-restore-cursor ((tty terminal-ansi))
  "Restore the cursor position, from the last saved postion."
  (terminal-format tty "~c8" #\escape)
  ;; (terminal-finish-output tty)
  )

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
  (let ((response (terminal-query (s+ +csi+ s))))
    (if (zerop (length response))
	'()
	(mapcar (_ (ignore-errors (parse-integer _)))
		(split-sequence
		 #\;
		 (coerce (subseq response offset
				 (1- (length response)))
			 'string))))))

(defun query-string (s &key (offset 3) (ending 2) (lead-in +csi+) tty)
  (let ((response (terminal-query (s+ lead-in s) :tty tty)))
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

(defgeneric set-bracketed-paste-mode (tty &optional state)
  (:documentation "Set bracketed paste mode to STATE.")
  (:method ((tty terminal-ansi) &optional (state t))
    (terminal-raw-format tty "~a?2004~c" +csi+ (if state #\h #\l))))

(defvar *bracketed-read-timeout* 4
  "Maximum time in seconds before bailing out of reading one buffer full of a
bracketed read.")

(defgeneric read-bracketed-paste (tty)
  (:documentation "Read a bracketed paste and return it as a string.")
  (:method ((tty terminal-ansi))
    (let ((end-string (s+ +csi+ "201~"))
	  ;; (buf (make-string *buffer-size*))
	  (fd (terminal-file-descriptor tty)))
      (with-output-to-string (str)
	(with-raw (fd)
	  (loop :with done :and i = 0 :and len = (length end-string) :and s
	     :while (not done)
	     :if (listen-for *bracketed-read-timeout* fd) :do
	     (with-interrupts-handled (tty)
	       (setf s (read-until fd (char end-string i)
				   :timeout (* *bracketed-read-timeout* 10)
				   :octets-p t)))
	     ;; (dbugf :bp "got dingus ~s ~s~%length ~s~%fill-pointer ~s"
	     ;; 	    s (type-of s) (length s)
	     ;; 	    (when s (fill-pointer s))
	     ;; 	    )
	     (if s
		 (progn
		   ;; (princ s str)
		   (let ((uu (char-util:utf8-bytes-to-string s)))
		     ;; (dbugf :bp "why? ~s ~s~%" uu (type-of uu))
		     (princ uu str))
		   (setf i 1))
		 (progn
		   (incf i)))
	     (when (= i len)
	       (setf done t))
	     :else :do
	     (cerror "Return what we got so far."
		     "Bracketed paste timed out.")
	     (setf done t)))))))

(defun set-utf8-title-mode (tty state)
  (terminal-raw-format tty "~c[>2;3~c" #\escape (if state #\t #\T))
  (terminal-finish-output tty))

(defun set-title (tty title &optional (which :window))
  (let ((param (case which
		 (:window 2)
		 (:icon 1)
		 (:both 0))))
    (terminal-raw-format tty "~a~a;~a~c"
			 +osc+ param title (char-util:ctrl #\G))
    (terminal-finish-output tty)))

(defun get-title (tty &optional (which :window))
  (set-utf8-title-mode tty t)
  (let ((param (case which
		 (:icon "20")
		 (:window "21")
		 (otherwise "21"))))
    (query-string (s+ param "t") :tty (terminal-file-descriptor tty))))

;; If this is mysteriously not working, you might have to make sure to enable
;; it in your emulator. Like in xterm: "Allow Window Ops".
(defmethod terminal-title ((tty terminal-ansi))
  (get-title tty))

(defmethod (setf terminal-title) (title (tty terminal-ansi))
  "Set the title of a terminal window. The terminal is assumed to work like
XTerm or something."
  (set-title tty title))

(defmethod terminal-has-attribute ((tty terminal-ansi) attribute)
  "Return true if the terminal can display the character attribute."
  (case attribute
    ((:standout :underline :bold :inverse :color) t)))

(defun set-mouse-event (tty event state)
  (case event
    (:mouse-buttons
     (terminal-raw-format tty "~a?1002~c" +csi+ (if state #\h #\l)))
    (:mouse-motion
     (terminal-raw-format tty "~a?1003~c" +csi+ (if state #\h #\l)))))

(defmethod terminal-enable-event ((tty terminal-ansi) event)
  "Enable event and return true if the terminal can enable event."
  (let (result)
    (when (member event '(:resize :mouse-buttons :mouse-motion))
      (pushnew event (terminal-events-enabled tty))
      (setf result t))
    (case event
      (:mouse-buttons (set-mouse-event tty :mouse-buttons t))
      (:mouse-motion (set-mouse-event tty :mouse-motion t)))
    result))

(defmethod terminal-disable-event ((tty terminal-ansi) event)
  "Enable event and return true if the terminal can disable event."
  (let (result)
    (when (member event '(:resize :mouse-buttons :mouse-motion))
       (setf (terminal-events-enabled tty)
	     (remove event (terminal-events-enabled tty)))
      (setf result t))
    (case event
      (:mouse-buttons (set-mouse-event tty :mouse-buttons nil))
      (:mouse-motion (set-mouse-event tty :mouse-motion nil)))
    result))

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
  (terminal-ansi-stream-fake-column stream)
  ;;; On clisp or something this was getting a negative number?
  #|
  (let ((col (terminal-ansi-stream-fake-column stream)))
    (or (and (integerp col) (not (minusp col)) col)
	;; @@@ Mindlessly patch over problems???
	;; @@@ Make this an error and fix it.
	0))
  |#
  )

(defmethod stream-start-line-p ((stream terminal-ansi-stream))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-ansi-stream) column)
  (write-sequence *endless-spaces*
		  (terminal-output-stream stream)
		  :start 0
		  :end (- column (stream-line-column stream)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-ansi-stream))

;; #+sbcl (defmethod sb-gray:stream-line-length ((stream terminal-ansi-stream))
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
    (setf typeahead nil)
    (clear-input output-stream)))

(defmethod stream-read-sequence ((stream terminal-ansi) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore stream seq start end))
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
      (with-interrupts-handled (stream)
	(read-until (terminal-file-descriptor stream) #\newline))
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
;; Costs

;; To figure out costs, we just output to string and count the characters.
;; We keep around a special terminal-stream just for the purpose.

;; @@@ Could some of these benefit from memo-ization?

(defun ensure-cost-stream (tty)
  (when (not (terminal-ansi-stream-cost-stream tty))
    (setf (terminal-ansi-stream-cost-stream tty)
	  (make-instance 'terminal-ansi-stream))))

(defmacro calculate-cost ((tty) &body body)
  "Cacluate the cost, in chacaters output, of evaluating terminal operations
in BODY. Output should be done to COST-STREAM."
  ;; (declare (ignore tty body))
  ;; 8
  (with-unique-names (str)
    `(progn
       (ensure-cost-stream ,tty)
       (length
	(with-output-to-string (,str)
	  (setf (terminal-output-stream
		 (terminal-ansi-stream-cost-stream ,tty)) ,str)
	  (let ((cost-stream (terminal-ansi-stream-cost-stream ,tty)))
	    ,@body)))))
  )

(defmethod output-cost ((tty terminal-ansi) (op (eql :move-to)) &rest params)
  (calculate-cost (tty)
   (terminal-move-to cost-stream (first params) (second params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :move-to-col)) &rest params)
  (calculate-cost (tty)
   (terminal-move-to-col cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :up)) &rest params)
  (calculate-cost (tty)
   (terminal-up cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :down)) &rest params)
  (calculate-cost (tty)
   (terminal-down cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :backward)) &rest params)
  (calculate-cost (tty)
   (terminal-backward cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :forward)) &rest params)
  (calculate-cost (tty)
   (terminal-forward cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :color)) &rest params)
  (calculate-cost (tty)
   (terminal-color cost-stream (first params) (second params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :write-fatchar))
			&rest params)
  (calculate-cost (tty)
   (terminal-write-char cost-stream (first params))))

(defmethod output-cost ((tty terminal-ansi) (op (eql :write-fatchar-string))
			&rest params)
  (calculate-cost (tty)
   (terminal-write-string cost-stream (make-fat-string :string (first params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :ansi 'terminal-ansi)
(register-terminal-type :ansi-stream 'terminal-ansi-stream)

;; EOF
