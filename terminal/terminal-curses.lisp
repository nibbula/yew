;;;
;;; terminal-curses.lisp - Curses terminal
;;;

(defpackage :terminal-curses
  (:documentation "Curses terminal")
  (:use :cl :dlib :terminal :curses :trivial-gray-streams :fatchar :dcolor)
  (:export
   #:terminal-curses-stream
   #:terminal-curses
   ;; extensions:
   #:+color-names+
   #:color-index
   #:color-number
   ))
(in-package :terminal-curses)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

(defparameter *acs-table* nil
  "Hash table of unicode character to ACS character.")

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

(defun make-acs-table ()
  "Make the alternate character set table."
  (setf *acs-table* (make-hash-table))
  (loop :for (uc . ac) :in *acs-table-data* :do
     (setf (gethash uc *acs-table*) ac)))

(defclass color-izer ()
  ((pair-count
    :initarg :pair-count :accessor pair-count :initform 0 :type fixnum
    :documentation "The number of color pairs allocated.")
   (color-table
    :initarg :color-table :accessor color-table
    :documentation "Table of color pair numbers.")
   (pair-color-table
    :initarg :pair-color :accessor pair-color-table
    :documentation
    "Table of pair numbers to forground and background colors. Elements are a
cons of (foreground . backaground)"))
  (:documentation
   "An object to encapsulate color handling for different amounts of colors."))

(defgeneric make-color-tables (color-izer)
  (:documentation "Make the color tables for a color-izer."))

(defgeneric make-color-pair (color-izer fg bg)
  (:documentation
   "Make a new color pair for the color numbers FG and BG, and return the pair
number."))

(defgeneric color-init (color-izer)
  (:documentation ""))

(defgeneric color-number (color-izer color)
  (:documentation "Return the curses color number given a color."))

(defgeneric color-name (color-izer color-number)
  (:documentation "Return the color name keyword given a curses color number."))

(defgeneric index-color (color-izer color-number)
  (:documentation "Return the color given a curses color number."))

(defgeneric get-pair (color-izer fg bg)
  (:documentation "Return the color pair for FG and BG."))

(defgeneric pair-color (color-izer pair-number)
  (:documentation "Return a cons of (FG . BG) for the PAIR-NUMBER."))

;; @@@ Does this even make sense?
(defclass terminal-curses-stream (terminal-stream)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defparameter *default-default-bg* :black)

(defclass terminal-curses (terminal)
  ((screen
    :initarg :screen :accessor screen :initform nil
    :documentation "The curses screen.")
   (device
    :initarg :device :accessor device :initform nil
    :documentation "The device for the terminal.")
   (term-type
    :initarg :term-type :accessor term-type :initform nil
    :documentation "The type name of the terminal.")
   (in-fp
    :initarg :in-fp :accessor in-fp :initform nil
    :documentation "The input FILE pointer for the terminal.")
   (out-fp
    :initarg :out-fp :accessor out-fp :initform nil
    :documentation "The output FILE pointer for the terminal.")
   (translate-alternate-characters
    :initarg :translate-alternate-characters
    :accessor translate-alternate-characters
    :initform nil :type boolean
    :documentation
    "True to translate some unicode characters into the alternate character
set.")
   (delay-scroll
    :initarg :delay-scroll :accessor delay-scroll :initform nil :type boolean
    :documentation
    "True to delay scrolling until the next character is output.")
   (start-line
    :initarg :start-line :accessor start-line :initform 0
    :documentation "Line we started at.")
   (default-fg
    :initarg :default-fg :accessor default-fg :initform :white
    :documentation "Default foreground color.")
   (default-bg
    :initarg :default-bg :accessor default-bg :initform *default-default-bg*
    :documentation "Default background color.")
   (has-color-p
    :initarg :has-color-p :accessor has-color-p :initform nil :type boolean
    :documentation "True if the device has color.")
   (color-izer
    :initarg :color-izer :accessor color-izer
    :documentation "The color handling object."))
  (:documentation "A terminal using the curses library."))

(defmethod terminal-default-device-name ((type (eql 'terminal-curses)))
  "Return the default device name for a TERMINAL-CURSES."
  ;; This is silly.
  "stdscr")

(defun initialize-device (o)
  (with-slots (device term-type in-fp out-fp screen) o
    (when (not term-type)
      (setf term-type (nos:environment-variable "TERM")))
    (when (and (slot-boundp o 'device) device)
      (when (cffi:null-pointer-p (setf in-fp (nos:fopen device "r")))
	(error "Can't open curses input device ~a" device))
      (when (cffi:null-pointer-p (setf out-fp (nos:fopen device "w")))
	(error "Can't open curses output device ~a" device))
      (when (cffi:null-pointer-p
	     (setf screen (newterm term-type out-fp in-fp)))
	(error "Can't initialize curses terminal ~a" term-type)))))

(defmethod initialize-instance
    :after ((o terminal-curses) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-curses."
  (declare (ignore initargs))
  (initialize-device o))

(defmethod terminal-get-size ((tty terminal-curses))
  "Get the window size from the kernel and store it in tty."
    (setf (terminal-window-rows tty) curses:*lines*
	  (terminal-window-columns tty) curses:*cols*))

;; This isn't really accurate if any output has been done not through curses,
;; so it's not as useful as the one in terminal-ansi.

(defmethod terminal-get-cursor-position ((tty terminal-curses))
  "Try to somehow get the row of the screen the cursor is on."
  (values (getcury (screen tty)) (getcurx (screen tty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
;;
;; This sets up a simple way to use all pairs of the standard eight colors.
;; Call INIT-COLORS first, then say, for example:
;; (setattr (color-attr +COLOR-YELLOW+ +COLOR-BLUE+))
;; or
;; (set-colors (color-index +COLOR-YELLOW+ +COLOR-BLUE+))

(defparameter +color-names+
  `((:black 	. ,+color-black+)
    (:red 	. ,+color-red+)
    (:green 	. ,+color-green+)
    (:yellow 	. ,+color-yellow+)
    (:blue 	. ,+color-blue+)
    (:magenta 	. ,+color-magenta+)
    (:cyan 	. ,+color-cyan+)
    (:white 	. ,+color-white+))
  "Associate symbols with color numbers.")

(defun init-8-colors (o)
  "Initialize the first 64 color pairs of the standard 8 colors."
  (let ((ncolors 8))
    (loop :for fg :from (- ncolors 1) :downto 0 :do
       (loop :for bg :from 0 :below ncolors :do
	  (make-color-pair o fg bg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass color-izer-0 (color-izer)
  ()
  (:documentation "A color-izer for NO colors."))

(defmethod make-color-tables ((o color-izer-0))
  (declare (ignore o)))

(defmethod make-color-pair ((o color-izer-0) fg bg)
  (declare (ignore o fg bg)))

(defmethod color-init ((o color-izer-0))
  (declare (ignore o)))

(defmethod color-number ((o color-izer-0) color)
  (declare (ignore o))
  (case color
    (:black 0)
    (:white 1)))

(defmethod color-name ((o color-izer-0) color-number)
  (declare (ignore o))
  (case color-number
    (0 :black)
    (1 :white)))

(defmethod index-color ((o color-izer-0) color-number)
  ;; This is the same as color-name.
  (color-name o color-number))

(defmethod get-pair ((o color-izer-0) fg bg)
  (declare (ignore o fg bg))
  0)

(defmethod pair-color ((o color-izer-0) pair-number)
  (declare (ignore o pair-number))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass color-izer-8 (color-izer)
  ()
  (:documentation "A color-izer for only 8 colors."))

(defmethod make-color-tables ((o color-izer-8))
  (let ((ncolors 8))
    (setf (color-table o) (make-array (list ncolors ncolors))
	  (pair-color-table o) (make-array (* ncolors ncolors)))))

(defmethod make-color-pair ((o color-izer-8) fg bg)
  (with-slots (pair-count) o
    (when (plusp pair-count) ;; Pair 0 defaults to WHITE on BLACK
      (init-pair pair-count fg bg))
    (setf (aref (color-table o) fg bg) pair-count
	  (aref (pair-color-table o) pair-count) (cons fg bg))
    (incf pair-count)))

(defmethod color-init ((o color-izer-8))
  (init-8-colors o))

(defmethod color-number ((o color-izer-8) color)
  (declare (ignore o))
  (cdr (assoc color +color-names+)))

(defmethod color-name ((o color-izer-8) color-number)
  (declare (ignore o))
  (car (rassoc color-number +color-names+)))

(defmethod index-color ((o color-izer-8) color-number)
  ;; This is the same as color-name.
  (color-name o color-number))

(defmethod get-pair ((o color-izer-8) fg bg)
  (aref (color-table o) fg bg))

(defmethod pair-color ((o color-izer-8) pair-number)
  (aref (pair-color-table o) pair-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass color-izer-small (color-izer)
  ()
  (:documentation "A color-izer for a small color cube. Usually 88 or 256."))

(defmethod make-color-tables ((o color-izer-small))
  (setf (color-table o) (make-hash-table :test #'equalp)
	(pair-color-table o) (make-hash-table)))

(defmethod make-color-pair ((o color-izer-small) fg bg)
  (with-slots (pair-count) o
    (if (< pair-count *colors*)
	(progn
	  (when (plusp pair-count) ;; Pair 0 defaults to WHITE on BLACK
	    (init-pair pair-count fg bg))
	  (let ((p (cons fg bg)))
	    (setf (gethash p (color-table o)) pair-count
		  (gethash pair-count (pair-color-table o)) p))
	  (incf pair-count))
	;; We could error here, but it's probably a bad idea.
	0))) ;; @@@ maybe should be nil ?

(defmethod color-init ((o color-izer-small))
  (init-8-colors o))

(defmethod color-number ((o color-izer-small) color)
  "Return the curses color number given a color."
  (typecase color
    (keyword
     (or (cdr (assoc color +color-names+))
	 (get-nearest-xterm-color-index (lookup-color color))))
    ((or vector list)
     (get-nearest-xterm-color-index color))))

(defmethod color-name ((o color-izer-small) color-number)
  (declare (ignore o))
  (car (rassoc color-number +color-names+)))

(defmethod index-color ((o color-izer-small) color-number)
  (declare (ignore o))
  (cond
    ((< 0 color-number 256)
     ;; @@@ This is wrong in a number of ways
     (aref fatchar:*xterm-256-color-table* color-number))))

(defmethod get-pair ((o color-izer-small) fg bg)
  (gethash (cons fg bg) (color-table o)))

(defmethod pair-color ((o color-izer-small) pair-number)
  (gethash pair-number (pair-color-table o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass color-izer-big (color-izer-small)
  ()
  (:documentation "A color-izer for at least 24 bit direct color."))

#|
(defun color-number-direct-16 (color)
  "Return the curses color number given a symbol name."
  (let ((c (typecase color
	     (keyword (lookup-color color))
	     ((or vector list)
	      (convert-color-to color :rgb8)))))
    ;; Assume a 5 5 5
    (a

(defun color-number-direct-24 (color)
  "Return the curses color number given a symbol name."
  (let ((c (typecase color
	     (keyword (lookup-color color))
	     ((or vector list)
	      (convert-color-to color :rgb8)))))
    ;; 24 bit? 32 bit?
    (a
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-colors (tty)
  ;; Initialize all the color pairs
  (start-color)
  (if (/= (has-colors) 1)
      (progn
	(setf (has-color-p tty) nil
	      (color-izer tty) (make-instance 'color-izer-0)))
      (progn
	(setf (has-color-p tty) t
	      (color-izer tty)
	      (make-instance (cond
			       ((<= *colors* 8) 'color-izer-8)
			       ((<= *colors* 256) 'color-izer-small)
			       ((> *colors* 256) 'color-izer-big))))
	;; @@@ Maybe we should put this in initialize-instance?
	(make-color-tables (color-izer tty))
	(color-init (color-izer tty))
	))
  (bkgd (color-pair 0)))

(defun color-index (tty fg bg)
  "Return the color pair number for the foreground FG and background BG."
  (get-pair (color-izer tty) fg bg))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defvar *default-scroll-ok* 1)

(defmethod terminal-start ((tty terminal-curses))
  "Set up the terminal for reading a character at a time without echoing."
  (with-slots ((start-at-current-line terminal::start-at-current-line)
	       start-line) tty
    (when start-at-current-line
      (filter))

    (when (not (device tty))		; already done
      (initscr)
      (setf (screen tty) *stdscr*))
    (noecho)
    (nonl)
    (cbreak)
    (meta curses:*stdscr* 1)
    (keypad curses:*stdscr* 1)
    (typeahead -1)
    (start-color)
    ;; additional resets that wouldn't need to be done on a fresh application
    (attrset 0)
    (bkgd 0)
    (idlok curses:*stdscr* 0)
    (leaveok curses:*stdscr* 0)
    (scrollok curses:*stdscr* *default-scroll-ok*)
    (curs-set 1)
    (init-colors tty)
    (terminal-get-size tty)
    (setf start-line (terminal-get-cursor-position tty))
    (when (zerop start-line)
      (nofilter))))

(defmethod terminal-end ((tty terminal-curses) &optional state)
  "Put the terminal back to the way it was before we called terminal-start."
  (declare (ignore state))
;;;  (format t "[terminal-end]~%")
  (endwin))

(defmethod terminal-done ((tty terminal-curses) &optional state)
  "Forget about the whole terminal thing and stuff."
  (declare (ignore state))
  (terminal-end tty)
  (with-slots (device screen in-fp out-fp) tty
    (when device
      (delscreen screen)
      (nos:fclose out-fp)
      (nos:fclose in-fp)))
  (values))

(defun unfilter (tty)
  "Stop doing the filter thing."
  (with-slots (screen term-type in-fp out-fp) tty
    (nofilter)
    (endwin)
    ;; (nofilter)
    (initialize-device tty)
    ;; (nofilter)
    (initscr)))

#-curses-dont-use-wide
(defun add-wide-string (wide-string)
  (declare (type string wide-string)
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (cffi:with-foreign-object (fstr :int (1+ (length wide-string)))
      (loop :with i = 0 :for c :across wide-string :do
	 (setf (cffi:mem-aref fstr :int i) (char-code c))
	 (incf i))
      (setf (cffi:mem-aref fstr :int (length wide-string)) 0)
      (addnwstr fstr (length wide-string))))

(defmethod terminal-write-string ((tty terminal-curses) str &key start end)
  "Output a string to the terminal."
  (let ((out-str (if (or start end)
		     ;; So we don't end up making a yet another copy.
		     (let ((real-start (or start 0))
			   (real-end (or end (length str))))
		       (make-array (- real-end real-start)
				   :element-type (array-element-type str)
				   :displaced-to str
				   :displaced-index-offset real-start))
		     str)))
    (when (and (translate-alternate-characters tty)
	       (stringp out-str))
      (translate-acs-chars out-str :start start :end end))
    #-curses-dont-use-wide
    ;; This seems very inefficient, but I'm not sure how to avoid it.
    (if (position-if (_ (> (char-code _) #xff)) out-str)
	(add-wide-string out-str)
	(%addstr tty out-str))
    #+curses-dont-use-wide
    (%addstr tty out-str)))

(defun bottom-right-p (tty &optional x)
  ;; (dbugf :curses "bottom-right-p ~s ~s ~s ~s ~s~%"
  ;; 	 (getcury (screen tty)) (1- curses:*lines*)
  ;; 	 (or x (getcurx (screen tty))) (1- curses:*cols*)
  ;; 	 (and (= (getcury (screen tty)) (1- curses:*lines*))
  ;; 	      (>= (or x (getcurx (screen tty))) (1- curses:*cols*))))
  (and (= (getcury (screen tty)) (1- curses:*lines*))
       (>= (or x (getcurx (screen tty))) (1- curses:*cols*))
       t))

(defun delayed-scroll (tty)
  (when (delay-scroll tty)
    (dbugf :curses "delay scroll triggered~%")
    (setf (delay-scroll tty) nil)
    (when (bottom-right-p tty)
      ;; (scroll-one-line)
      (addch #.(char-code #\newline)))))

(defun %addch (tty code)
  "A version of addch that does delayed scroll only for the bottom right corner."
  (delayed-scroll tty)
  (if (bottom-right-p tty)
      (progn
	(dbugf :curses "delay scroll activated~%")
	(setf (delay-scroll tty) t)
	(scrollok (screen tty) 0)
	(addch code)
	(scrollok (screen tty) *default-scroll-ok*)
	)
      (addch code)))

(defun %addstr (tty str)
  (delayed-scroll tty)
  (if (bottom-right-p tty (+ (getcurx (screen tty))
			     (char-util:display-length str)))
      (progn
	(dbugf :curses "delay scroll string activated~%")
	(setf (delay-scroll tty) t)
	(scrollok (screen tty) 0)
	(addstr str)
	(scrollok (screen tty) *default-scroll-ok*)
	)
      (addstr str)))

(defmethod terminal-write-line ((tty terminal-curses) str &key start end)
  (terminal-write-string tty str :start start :end end)
  (%addch tty (char-code #\newline)))

(defmethod terminal-format ((tty terminal-curses) fmt &rest args)
  "Output a formatted string to the terminal."
  ;; (let ((string (apply #'format nil fmt args)))
  ;;   (terminal-write-string tty string))
  (apply #'format tty fmt args))

#-curses-dont-use-wide
(defvar *wide-char* (cffi:foreign-alloc :int :count 2))
#-curses-dont-use-wide
(defun add-wide-char (wide-char)
  (setf (cffi:mem-aref *wide-char* :int 0) wide-char
	(cffi:mem-aref *wide-char* :int 1) 0)
  (addnwstr *wide-char* 2))

(defmethod terminal-write-char ((tty terminal-curses) (char character))
  "Output a character to the terminal."
  ;; @@@ unicode!
  (when (and (translate-alternate-characters tty)
	     (characterp char))
    (let ((replacement (gethash char *acs-table*)))
      (when replacement
	(setf char replacement))))
  #-curses-dont-use-wide
  (if (> (char-code char) 255)
      (add-wide-char (char-code char))
      (%addch tty (char-code char)))
  #+curses-dont-use-wide
  (%addch tty (char-code char)))

(defparameter *attributes*
  `((:STANDOUT  . ,+A-STANDOUT+)
    (:UNDERLINE . ,+A-UNDERLINE+)
    (:REVERSE   . ,+A-REVERSE+)
    (:BLINK     . ,+A-BLINK+)
    (:DIM       . ,+A-DIM+)
    (:BOLD      . ,+A-BOLD+)))

(defun %terminal-write-char (tty char &key reset)
  "Output a character to the terminal."
  ;; @@@ unicode!
  (dbugf :curses "howdy ~s ~s~%" reset char)
  (with-slots ((cc fatchar::c)
	       (fg fatchar::fg)
	       (bg fatchar::bg)
	       (line fatchar::line)
	       (attrs fatchar::attrs)) char
    (let ((c cc))
      ;; We still do this dumb replacing, just in case.
      (when (and (translate-alternate-characters tty)
      		 (characterp cc))
      	(let ((replacement (gethash cc *acs-table*)))
      	  (when replacement
      	    (setf c replacement))))

      (terminal-color tty fg bg)
      (if (or fg bg attrs)
	  (progn
	    ;; (when (or fg bg)
	    ;;   ;; (terminal-color tty (or fg :default) (or bg :default)))
	    ;;   (dbugf :curses "chippy ~s ~s~%" fg bg)
	    (when attrs
	      (loop :with n
		 :for a :in attrs :do
		 (when (setf n (assoc a *attributes*))
		   (attron (cdr n))))))
	  (when reset
	    (dbugf :curses "neow~%")
	    (attrset +a-normal+)
	    (color-set 0 (cffi:null-pointer))
	    ))
      (when (not (zerop line))
	(attron +A-ALTCHARSET+)
	(setf c (line-char line)))
      #-curses-dont-use-wide
      (if (> (char-code c) 255)
	  (add-wide-char (char-code c))
	  (%addch tty (char-code c)))
      #+curses-dont-use-wide
      (%addch tty (char-code c))
      (when reset
	(dbugf :curses "neow 2~%")
	(attrset +a-normal+)
	(color-set 0 (cffi:null-pointer))
	))))

(defmethod terminal-write-char ((tty terminal-curses) (char fatchar))
  "Output a character to the terminal."
  (%terminal-write-char tty char :reset t))

(defmethod terminal-write-string ((tty terminal-curses) (str fat-string)
				  &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is 'line buffered'."
  (when (and (not (and (and start (zerop start)) (and end (zerop end))))
	     (fat-string-string str))
    (let ((fs (fat-string-string str))
	  (translate (translate-alternate-characters tty))
	  replacement)
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
	       	       (terminal-write-char tty replacement)
	       	       (terminal-write-char tty cc))
	       	   (terminal-write-char tty (line-char line)))
	       (progn
		 ;;(terminal-raw-format tty "~c[0m" #\escape)
		 ;; (%terminal-write-char tty c :reset nil)))
		 (%terminal-write-char tty c :reset nil)))
	   (setf last-c c))
	 (incf i))
      (attrset +a-normal+))))

(defmethod terminal-write-line ((tty terminal-curses) (str fat-string)
				&key start end)
  (terminal-write-string tty str :start start :end end)
  (%addch tty (char-code #\newline)))

(defmethod terminal-newline ((tty terminal-curses))
  (%addch tty (char-code #\newline)))

(defmethod terminal-fresh-line ((tty terminal-curses))
  (when (not (zerop (getcurx (screen tty))))
    (%addch tty (char-code #\newline))
    t))

(defmethod terminal-move-to ((tty terminal-curses) row col)
  (with-slots (start-line) tty
    (when (< row start-line)
      (unfilter tty))
    (move row col)))

(defmethod terminal-move-to-col ((tty terminal-curses) col)
  (move (getcury (screen tty)) col))

(defmethod terminal-beginning-of-line ((tty terminal-curses))
  (terminal-move-to-col tty 0))

(defmethod terminal-delete-char ((tty terminal-curses) n)
  (dotimes (i n)
    (delch)))

(defmethod terminal-insert-char ((tty terminal-curses) n)
  (dotimes (i n)
    (insch (char-code #\space))))

(defmethod terminal-backward ((tty terminal-curses) &optional (n 1))
  (move (getcury (screen tty)) (- (getcurx (screen tty)) n)))

(defmethod terminal-forward ((tty terminal-curses) &optional (n 1))
  (move (getcury (screen tty)) (+ (getcurx (screen tty)) n)))

(defmethod terminal-up ((tty terminal-curses) &optional (n 1))
  (with-slots (start-line) tty
    (let ((new-y (- (getcury (screen tty)) n)))
      (when (< new-y start-line)
	(unfilter tty))
      (move new-y (getcurx (screen tty))))))

(defmethod terminal-down ((tty terminal-curses) &optional (n 1))
  (move (+ (getcury (screen tty)) n) (getcurx (screen tty))))

(defmethod terminal-scroll-down ((tty terminal-curses) n)
  (when (> n 0)
    (scrollok *stdscr* 1)
    (scrl n)
    (scrollok *stdscr* *default-scroll-ok*)))

(defmethod terminal-scroll-up ((tty terminal-curses) n)
  (when (> n 0)
    (scrollok *stdscr* 1)
    (scrl (- n))
    (scrollok *stdscr* *default-scroll-ok*)))
  
(defmethod terminal-erase-to-eol ((tty terminal-curses))
  (clrtoeol))

(defmethod terminal-erase-line ((tty terminal-curses))
  (let ((x (getcurx (screen tty)))
	(y (getcury (screen tty))))
    (move y 0)
    (clrtoeol)
    (move y x)))

(defmethod terminal-erase-above ((tty terminal-curses))
  (let ((x (getcurx (screen tty)))
	(y (getcury (screen tty))))
    (loop :for i :from 0 :below y :do
       (move i 0)
       (clrtoeol))
    (when (> x 0)
      (mvaddstr y 0 (format nil "~va" x #\space)))
    (move y x)))

(defmethod terminal-erase-below ((tty terminal-curses))
  (clrtobot))

(defmethod terminal-clear ((tty terminal-curses) &key saved-p)
  (declare (ignore saved-p))
  (with-slots (start-line) tty
    (when (not (zerop start-line))
      (unfilter tty))
    (clear)))

(defmethod terminal-home ((tty terminal-curses))
  (with-slots (start-line) tty
    (when (not (zerop start-line))
      (unfilter tty))
    (move 0 0)))

(defmethod terminal-cursor-off ((tty terminal-curses))
  (curs-set 0))

(defmethod terminal-cursor-on ((tty terminal-curses))
  (curs-set 1))

(defmethod terminal-standout ((tty terminal-curses) state)
  (if state
      (attron +a-standout+)
      (attroff +a-standout+)))

(defmethod terminal-normal ((tty terminal-curses))
  (attrset +a-normal+))

(defmethod terminal-underline ((tty terminal-curses) state)
  (if state
      (attron +a-underline+)
      (attroff +a-underline+)))

(defmethod terminal-bold ((tty terminal-curses) state)
  (if state
      (attron +a-bold+)
      (attroff +a-bold+)))

(defmethod terminal-inverse ((tty terminal-curses) state)
  (if state
      (attron +a-reverse+)
      (attroff +a-reverse+)))

(defmethod terminal-color ((tty terminal-curses) fg bg)
  ;; This defaulting is bullcrap. But so is curses defaulting.
  ;; See man default_colors.
  (when (or (null fg) (eq fg :default))
    (setf fg (default-fg tty)))
  (when (or (null bg) (eq bg :default))
    (setf bg (default-bg tty)))
  (let ((fg-num (color-number (color-izer tty) fg))
	(bg-num (color-number (color-izer tty) bg)))
    (when (and fg-num bg-num)
      (let ((pair (or (color-index tty fg-num bg-num)
		      (make-color-pair (color-izer tty) fg-num bg-num))))
	(when pair
	  (color-set pair (cffi:null-pointer)))))))

(defmethod terminal-colors ((tty terminal-curses))
  (declare (ignore tty))
  (if (not (has-colors))
      0
      ;; @@@ Is this really right?
      *colors*))

(defmethod terminal-window-foreground ((tty terminal-curses))
  "Get the default foreground color for text."
  ;; @@@ This isn't really right.
  ;; I don't think there's a way to know this with ncurses?
  (default-fg tty))

(defmethod (setf terminal-window-foreground) (color (tty terminal-curses))
  ;; @@@ This isn't really right either.
  (setf (default-fg tty) color))

(defmethod terminal-window-background ((tty terminal-curses))
  "Get the default background color for text."
  (let* ((attr (getbkgd (screen tty)))
	 (pair (pair-number attr)))
    (color-name (color-izer tty) (cdr (pair-color (color-izer tty) pair)))))

(defmethod (setf terminal-window-background) (color (tty terminal-curses))
  (when (or (null color) (eq color :default))
    (setf color *default-default-bg*))
  (let ((color-num (color-number (color-izer tty) color)))
    (when color-num
      (setf (default-bg tty) color)
      (bkgd (color-pair
	     (color-index tty (color-number (color-izer tty) (default-fg tty))
			  color-num))))))

#+(or)
(defun zerq ()
  (with-terminal (:curses)
    (tt-home)
    (tt-format "colors : ~a~%pairs : ~a~%" curses:*colors* curses:*color-pairs*) 
    (tt-format"color pair 23 : ~{~x ~}~%"
	      (multiple-value-list (curses::pair-content 23)))
    (tt-get-key)))

(defmethod terminal-beep ((tty terminal-curses))
  (beep))

(defmethod terminal-set-scrolling-region ((tty terminal-curses) start end)
  (if (and (not start) (not end))
      ;; Is this sensible? Or should we just unset 'scrollok'?
      (setscrreg 0 (1- *lines*))
      (progn
	(scrollok *stdscr* 1)
	(setscrreg start end))))

(defmethod terminal-finish-output ((tty terminal-curses))
  (refresh))

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

(defun translate-key (key)
  "Fix some key names to conform to terminal naming."
  (case key
    (:ic    :insert)
    (:dc    :delete)
    (:npage :page-down)
    (:ppage :page-up)
    (:b2    :center)
    (:btab  :back-tab)
    (:sdc   :s-delete)
    (:send  :s-end)
    (:shome :s-home)
    (:sleft :s-left)
    (otherwise key)))

(defun get-char ()
  "Get a lisp character or function key from curses."
  (let ((cc (getch)))
    (cond
      ((> cc #xff)
       (function-key cc))
      ((and (integerp cc) (not (minusp cc)))
       (code-char cc))
      (t ;; Just return a negative
       cc))))

(defmethod terminal-get-char ((tty terminal-curses))
  "Read a character from the terminal."
  (get-char))

(defmethod terminal-get-key ((tty terminal-curses))
  "Read a character from the terminal."
  (translate-key (get-char)))

(defmethod terminal-listen-for ((tty terminal-curses) seconds)
  (let (c)
    (unwind-protect
	 (progn
	   (curses::timeout (round (* seconds 1000)))
	   (setf c (getch))
	   (when (not (equal c +ERR+))
	     (ungetch c)))
      ;; This assumes timeout was already -1. Since there's no prescribed way to
      ;; get it, the caller has to reset it after this if they want it to be
      ;; different.
      (curses::timeout -1))
    (and (not (equal c +ERR+)) c)))

(defmethod terminal-input-mode ((tty terminal-curses))
  (declare (ignore tty))
  ;; @@@ fake it for the moment!!
  :char
  )

(defmethod (setf terminal-input-mode) (mode (tty terminal-curses))
  (case mode
    (:line
     (echo)
     (nl)
     (nocbreak))
    (:char
     (noecho)
     (nonl)
     (cbreak))
    (t (error "Unknown terminal input mode ~s" mode))))

(defmethod terminal-reset ((tty terminal-curses))
  "Try to reset the terminal to a sane state, without being too disruptive."
  (reset-shell-mode)) ; or something..

(defvar *saved-positions* nil
  "List of conses of saved positions, e.g. (x . y).")

(defmethod terminal-save-cursor ((tty terminal-curses))
  "Save the cursor position."
  ;; @@@ some thread safe incantation
  (let ((spot (cons (getcury (screen tty)) (getcurx (screen tty)))))
    (push spot *saved-positions*)))

(defmethod terminal-restore-cursor ((tty terminal-curses))
  "Restore the cursor position, from the last saved postion."
  (let ((bunkle (pop *saved-positions*)))
    (move (car bunkle) (cdr bunkle))))

(defmethod terminal-title ((tty terminal-curses))
  "Get the title of the terminal window."
  (declare (ignore tty)))

(defmethod (setf terminal-title) (title (tty terminal-curses))
  "Set the title of a terminal window."
  (declare (ignore title tty)))

(defmethod terminal-has-attribute ((tty terminal-curses) attribute)
  "Return true if the terminal can display the character attribute."
  (case attribute
    (:standout   (tigetstr "rev"))	; not exactly right
    (:underline  (tigetstr "smul"))
    (:bold       (tigetstr "bold"))
    (:inverse    (tigetstr "rev"))
    (:color 	 (has-colors))))

(defmethod terminal-has-autowrap-delay ((tty terminal-curses))
  "Return true if the terminal delays automatic wrapping at the end of a line."
  ;; @@@ Is this really true??
  nil)

(defmethod terminal-set-attributes ((tty terminal-curses) attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords."
  (loop :with n :for a :in attributes :do
     (when (setf n (assoc a *attributes*))
       (attron (cdr n)))))
     
(defmethod terminal-alternate-characters ((tty terminal-curses) state)
  (setf (translate-alternate-characters tty) state)
  (when (and state (not *acs-table*))
    (make-acs-table))
  (if state
      (attron +A-ALTCHARSET+)
      (attroff +A-ALTCHARSET+)))

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
     #\u     ;; RTEE      13 - 1101 - left + top + bottom
     #\v     ;; BTEE      14 - 1110 - left + top + right
     #\n     ;; PLUS      15 - 1111 - left + top + right + bottom
     ))

(defparameter *line-table* *line-table-vt100*
  "The table to use for looking up line drawing characters.")

(defun line-char (line)
  "Convert line bits into line drawing characters."
  (aref *line-table* line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods

(defmethod-quiet close ((stream terminal-curses) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream terminal-curses))
  ;;(clear-output (terminal-output-stream stream))
  (declare (ignore stream))
  )

(defmethod stream-finish-output ((stream terminal-curses))
  (terminal-finish-output stream))

(defmethod stream-force-output ((stream terminal-curses))
  (terminal-finish-output stream)
  ;;(force-output (terminal-output-stream stream))
  )

(defmethod stream-write-sequence ((stream terminal-curses) seq start end
				  &key &allow-other-keys)
  (etypecase seq
    (string
     (terminal-write-string stream seq :start start :end end)
     seq)
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	    (when (>= i start)
	      (%addch stream (car l)))
	    (setf l (cdr l))
	    (incf i))))))

;; character output stream methods

;; This is a weird trick to presumably make it so we don't have to do our own
;; buffering and we can also be relatively quick?
(defvar *endless-spaces* '#1=(#\space . #1#)
  "The vast emptyness of space.")

(defmethod stream-line-column ((stream terminal-curses))
  (getcurx (screen stream)))

(defmethod stream-start-line-p ((stream terminal-curses))
  (zerop (stream-line-column stream)))

(defmethod stream-advance-to-column ((stream terminal-curses) column)
  (write-sequence *endless-spaces*
		  (terminal-output-stream stream) :start 0
		  :end (- column (stream-line-column stream)))
  t)

;;(defmethod stream-fresh-line ((stream terminal-curses-stream))

#+sbcl
(defmethod sb-gray:stream-line-length ((stream terminal-curses-stream))
  (declare (ignore stream))
  *cols*)

(defmethod stream-write-char ((stream terminal-curses) char
			     #| &optional start end |#)
  (terminal-write-char stream char))

(defmethod stream-write-string ((stream terminal-curses) string
			       &optional start end)
  (terminal-write-string stream string :start start :end end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream methods for terminal-curses, which is also an input stream.

(defmethod stream-clear-input ((stream terminal-curses))
  (declare (ignore stream))
  (flushinp))

(defmethod stream-read-sequence ((stream terminal-curses) seq start end
				 &key &allow-other-keys
					#| &optional (start 0) end |#)
  (declare (ignore seq start end))
  nil)

;;(defgeneric stream-peek-char ((stream terminal-curses))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
;; )

(defmethod stream-read-char-no-hang ((stream terminal-curses))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (let (result c)
    (unwind-protect
	 (progn
	   (curses::timeout 0)
	   (setf c (getch))
	   ;; This pretty bogusly changes curses function keys into characters.
	   (setf result (and (not (equal c +ERR+)) (code-char c))))
      ;; This assumes timeout was already -1. Since there's no prescribed way to
      ;; get it, the caller has to reset it after this if they want it to be
      ;; different.
      (curses::timeout -1))
    result))

(defmethod stream-read-char ((stream terminal-curses))
  (terminal-get-char stream))

(defmethod stream-read-line ((stream terminal-curses))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (let (result got-eof c cc)
    (setf result
	  (with-output-to-string (str)
	    (loop :while (and (/= +ERR+ (setf c (getch)))
			      (char/= (setf cc (code-char c)) #\newline))
	       :do
	       (princ cc str))
	    (when (= c +ERR+)
	      (setf got-eof t))))
    (values result got-eof)))

(defmethod stream-listen ((stream terminal-curses))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (with-slots (typeahead output-stream) stream
    (or typeahead
	(terminal-listen-for stream 0))))

(defmethod stream-unread-char ((stream terminal-curses) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (ungetch (char-code character))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-terminal-type :curses 'terminal-curses)

;; EOF
