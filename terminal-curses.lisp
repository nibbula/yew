;;
;; terminal-curses.lisp - Curses terminal
;;

(defpackage :terminal-curses
  (:documentation "Curses terminal")
  (:use :cl :dlib :terminal :curses :trivial-gray-streams :fatchar :color)
  (:export
   #:terminal-curses-stream
   #:terminal-curses
   ;; extensions:
   #:+color-names+
   #:*color-table*
   #:color-index
   #:color-number
   ))
(in-package :terminal-curses)

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

;; @@@ Does this even make sense?
(defclass terminal-curses-stream (terminal-stream)
  ()
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

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
    "True to translate some unicode characters into the alternate character set."))
  (:default-initargs
  )
  (:documentation "A terminal using the curses library."))

(defmethod terminal-default-device-name ((type (eql 'terminal-curses)))
  "Return the default device name for a TERMINAL-CURSES."
  ;; This is silly.
  "stdscr")

(defmethod initialize-instance
    :after ((o terminal-curses) &rest initargs &key &allow-other-keys)
  "Initialize a terminal-curses."
  (declare (ignore initargs))
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

(defvar *has-color* nil
  "True if the device has color.")

(defparameter *color-table* nil
  "Table of color pair numbers.")

(defparameter +color-names+
  `((:black 	,+color-black+)
    (:red 	,+color-red+)
    (:green 	,+color-green+)
    (:yellow 	,+color-yellow+)
    (:blue 	,+color-blue+)
    (:magenta 	,+color-magenta+)
    (:cyan 	,+color-cyan+)
    (:white 	,+color-white+))
  "Associate symbols with color numbers.")

(defun init-colors ()
  ;; Initialize all the color pairs
  (start-color)
  (let ((ncolors 8))
    (setf *color-table* (make-array (list ncolors ncolors)))
    (if (= (has-colors) 1)
    	(prog ((pair 0))
	   (setf *has-color* t)
	   (loop :for fg :from (- ncolors 1) :downto 0 :do
	      (loop :for bg :from 0 :below ncolors :do
		 (when (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		   (init-pair pair fg bg))
		 (setf (aref *color-table* fg bg) pair)
		 (incf pair))))
	(setf *has-color* nil)))
  (bkgd (color-pair 0)))

(defun color-index (fg bg)
  "Return the color pair number for the foreground FG and background BG."
  (aref *color-table* fg bg))

(defun color-number (color)
  "Return the curses color number given a symbol name."
  (cadr (assoc color +color-names+)))

;; Just for debugging
; (defun terminal-report-size ()
;   (let ((tty (line-editor-terminal *line-editor*)))
;     (terminal-get-size tty)
;     (with-slots (window-rows window-columns) tty
;       (format t "[~d x ~d]~%" window-columns window-rows))))

(defmethod terminal-start ((tty terminal-curses))
  "Set up the terminal for reading a character at a time without echoing."
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
  (scrollok curses:*stdscr* 0)
  (curs-set 1)
  (init-colors)
  (terminal-get-size tty))

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

#+curses-use-wide
(defun add-wide-string (wide-string)
  (declare (type string wide-string))
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
    #+curses-use-wide
    ;; This seem very inefficient, but I'm not sure how to avoid it.
    (if (position-if (_ (> (char-code _) #xff)) out-str)
	(add-wide-string out-str)
	(addstr out-str))
    #-curses-use-wide
    (addstr out-str)))

(defmethod terminal-write-line ((tty terminal-curses) str &key start end)
  (terminal-write-string tty str :start start :end end)
  (addch (char-code #\newline)))

(defmethod terminal-format ((tty terminal-curses) fmt &rest args)
  "Output a formatted string to the terminal."
  ;; (let ((string (apply #'format nil fmt args)))
  ;;   (terminal-write-string tty string))
  (apply #'format tty fmt args))

#+curses-use-wide
(defvar *wide-char* (cffi:foreign-alloc :int :count 2))
(defun add-wide-char (wide-char)
  (setf (cffi:mem-aref *wide-char* :int 0) wide-char
	(cffi:mem-aref *wide-char* :int 1) 0)
  (addnwstr *wide-char* 2))

(defmethod terminal-write-char ((tty terminal-curses) char)
  "Output a character to the terminal."
  ;; @@@ unicode!
  (when (and (translate-alternate-characters tty)
	     (characterp char))
    (let ((replacement (gethash char *acs-table*)))
      (when replacement
	(setf char replacement))))
  #+curses-use-wide
  (if (> (char-code char) 255)
      (add-wide-char (char-code char))
      (addch (char-code char)))
  #-curses-use-wide
  (addch (char-code char)))

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
      (if (or fg bg attrs)
	  (progn
	    (when (or fg bg)
	      (terminal-color tty (or fg :default) (or bg :default)))
	    (when attrs
	      (loop :with n
		 :for a :in attrs :do
		 (when (setf n (assoc a *attributes*))
		   (attron (cdr n))))))
	  (when reset
	    (attrset +a-normal+)))
      (when (not (zerop line))
	(attron +A-ALTCHARSET+)
	(setf c (line-char line)))
      #+curses-use-wide
      (if (> (char-code c) 255)
	  (add-wide-char (char-code c))
	  (addch (char-code c)))
      #-curses-use-wide
      (addch (char-code c))
      (when reset
	(attrset +a-normal+)))))

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
		 (%terminal-write-char tty c :reset nil)))
	   (setf last-c c))
	 (incf i))
      (attrset +a-normal+))))

(defmethod terminal-write-line ((tty terminal-curses) (str fat-string)
				&key start end)
  (terminal-write-string tty str :start start :end end)
  (addch (char-code #\newline)))

(defmethod terminal-move-to ((tty terminal-curses) row col)
  (move row col))

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
  (move (- (getcury (screen tty)) n) (getcurx (screen tty))))

(defmethod terminal-down ((tty terminal-curses) &optional (n 1))
  (move (+ (getcury (screen tty)) n) (getcurx (screen tty))))

(defmethod terminal-scroll-down ((tty terminal-curses) n)
  (when (> n 0)
    (scrl n)))
  
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

(defmethod terminal-clear ((tty terminal-curses))
  (clear))

(defmethod terminal-home ((tty terminal-curses))
  (move 0 0))

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
  (when (eq fg :default)
    (setf fg :white))
  (when (eq bg :default)
    (setf bg :black))
  (let ((fg-num (color-number fg))
	(bg-num (color-number bg)))
    (when (and fg-num bg-num)
      (color-set (color-index fg-num bg-num) (cffi:null-pointer)))))

;; 256 color? ^[[ 38;5;color <-fg 48;5;color <- bg
;; set color tab = ^[] Ps ; Pt BEL
;;;  4; color-number ; #rrggbb ala XParseColor

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
  (get-char))

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
     (terminal-write-string stream seq :start start :end end))
    (list
     (with-slots (output-stream) stream
       (loop :with i = 0 :and l = seq
	  :while (and l (< i end))
	  :do
	    (when (>= i start)
	      (addch (car l)))
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
