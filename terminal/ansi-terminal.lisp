;;;
;;; ansi-terminal.lisp - ANSI terminal emulation
;;;

(defpackage :ansi-terminal
  (:documentation "ANSI terminal emulation

This provides a stream that emulates an ANSI terminal in terms of the terminal
protocol. Given a terminal object, it turns xterm-like escape sequences into
terminal protocol calls. You can write to the stream and it will operate the
terminal. Keys pressed on the terminal will be available to be read as
appropriate xterm escape sequences.")
  (:use :cl :dlib :collections :terminal :ansi :fatchar :stretchy :dcolor
        :trivial-gray-streams)
  (:export
   #:ansi-stream
   #:make-stream
   #:make-stream-on
   #:stream-done
   #:key-string
   ))
(in-package :ansi-terminal)

(defparameter *states*
  '(:start			       ; the initial ground state
    :escape			       ; read an escape
    ;; various escape prefixes
    :escape-space		       ; #\space
    :escape-hash		       ; #
    :escape-percent		       ; %
    :escape-open		       ; (
    :escape-close		       ; )
    :escape-star		       ; *
    :escape-plus		       ; +
    :escape-minus		       ; -
    :escape-dot			       ; .
    :escape-slash		       ; /
    :single-shift-g2		       ; ^[ N <char>
    :single-shift-g3		       ; ^[ O <char>
    :control			       ; aka CSI
    :system                            ; aka OSC
    :device                            ; aka DCS
    :param			       ; reading a numeric parameter
    :data			       ; reading until a terminator: ^G or ^[\
    :string-terminator		       ; waiting for a \
    )
  "Emulator states.")

#|──────────────────────────────────────────────────────────────────────────╮ 
 │ Character set terminology:                                               │
 │                                                                          │
 │ From ECMA-48:                                                            │
 │                                                                          │
 │ 7 bit                                                                    │
 │   C0  #x00 - #x1f    Control characters                                  │
 │   G0  #x21 - #x7e    Graphic characters                                  │
 │ 8 bit                                                                    │
 │   C1  #x80 - #x9f    Control characters  (or Esc + #x40 - #x5f in 7 bit) │
 │   G1  #xa1 - #xfe    Graphic characters                                  │
 │                                                                          │
 │ From DEC VTXXX manuals?                                                  │
 │                                                                          │
 │ 7 bit                                                                    │
 │   GL  #x20 - #x7e    Graphic characters (left side)                      │
 │ 8 bit                                                                    │
 │   GR  #xa1 - #xfe    Graphic characters (right side)                     │
 │                                                                          │
 │ G1, G2, G3                                                               │
 │   93 character sets that can be loaded into GL or GR                     │
 │                                                                          │
 │ These include:                                                           │
 │                                                                          │
 │ DEC Multinational Character Set                                          │
 │ DEC Special Graphics Character Set                                       │
 │ National Replacement Character Sets (many)                               │
 │                                                                          │
 │ The national character sets usually are like an overlay on G0, that are  │
 │ only a few characters.                                                   │
 ├──────────────────────────────────────────────────────────────────────────┤
 │ Character sets are an historic relic from the pre-unicode days, which    │
 │ are't really useful anymore, so it's a shame we have to go through all   │
 │ this, just to make sure old things work. But of course that describes    │
 │ almost everything about terminals.                                       │
 ╰──────────────────────────────────────────────────────────────────────────|#

(defconstant +G0+ 0 "Graphic character set 0.")
(defconstant +G1+ 1 "Graphic character set 1.")
(defconstant +G2+ 2 "Graphic character set 2.")
(defconstant +G3+ 3 "Graphic character set 2.")

(defparameter *dec-multinational-gr*
  (s+ "¡¢£ ¥ §¤©ª«    "
      "°±²³ µ¶· ¹º»¼½ ¿"
      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ"
      " ÑÒÓÔÕÖŒØÙÚÛÜÝ ß"
      "àáâãäåæçèéêëìíîï"
      " ñòóôõöœøùúûüý "))

(defparameter *unicode-gr*
  (s+ "¡¢£¤¥¦§¨©ª«¬ ®¯"
      "°±²³´µ¶·¸¹º»¼½¾¿"
      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ"
      "ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß"
      "àáâãäåæçèéêëìíîï"
      "ðñòóôõö÷øùúûüýþ"))

(defparameter *dec-graphics-gl*
   (s+ "!\"#$%&'()*+,-./"
      "0123456789:;<=>?"
      "@ABCDEFGHIJKLMNO"
      "PQRSTUVWXYZ[\\]^" (code-char #xa0) ; no-break_space
      "◆▒␉␌␍␊°±␤␋┘┐┌└┼⎺"
      "⎻─⎼⎽├┤┴┬│≤≥π≠£·"))

(defparameter *normal-gl*
  (let ((a (make-array 94)))
    (loop :for c :from #x21 :to #x7e
	  :for i :from 0
	  :do (setf (aref a i) (code-char c)))
    a)
  "The normal GL character set.")

(defparameter *normal-gr*
  (let ((a (make-array 94)))
    (loop :for c :from #xa1 :to #xfe
	  :for i :from 0
	  :do (setf (aref a i) (code-char c)))
    a)
  "The normal GR character set.")

(Defparameter *max-param* 100
  "If you have more parameters than this, it's fail.")

(defclass ansi-stream (fundamental-binary-input-stream
		       fundamental-binary-output-stream
		       fundamental-character-input-stream
		       fundamental-character-output-stream)
  ((terminal
    :initarg :terminal :accessor ansi-stream-terminal
    :documentation "The terminal we wrap.")
   (pushback-function
    :initarg :pushback-function :accessor pushback-function
    :documentation "Function to call with bytes to write back to the program.")
   (state
    :initarg :state :accessor state
    :initform :start #| :type `(member ,@*states*) |#
    :documentation "State code of the emulator.")
   (params
    :initarg :param :accessor params
    :initform (make-array *max-param* :fill-pointer 0)
    :type vector
    :documentation "Vector of parameters.")
   (prefixes
    :initarg :prefix :accessor prefixes :initform nil
    :documentation
    "A list of prefixes for controls. Probably one of: ? > = ! # $")
   (control-type
    :initarg :control-type :accessor control-type :initform :control
    :type (member :control :system :device)
    :documentation "Type of control sequence we're in.")
   (data
    :initarg :data :accessor ansi-stream-data
    :initform (make-stretchy-vector 40)
    :documentation "Data parameter.")
   (device-command
    :initarg :device-command :accessor device-command :initform nil
    :documentation "Device control command type we're in.")
   (left-charset
    :initarg :left-charset :accessor left-charset :initform +G0+
    :documentation "GL charset.")
   (right-charset
    :initarg :right-charset :accessor right-charset :initarg +G1+
    :documentation "GR charset.")
   (charsets
    :initarg :charsets :accessor charsets
    :initform (make-array 4)
    :documentation "Current character sets.")
   (8-bit-controls ;; @@@ maybe should be in modes?
    :initarg :8-bit-controls :accessor 8-bit-controls
    :initform nil :type boolean
    :documentation
    "Accept 8 bit control characters. This bad for many encodings, including
     UTF-8 and Latin-1,")
   (modes
    :initarg :modes :accessor modes
    :documentation "“Private” modes.")
   (standard-modes
    :initarg :modes :accessor standard-modes
    :documentation "Not so private modes. Modes in some standard.")
   (last-char
    :initarg :last-char :accessor last-char :initform #\nul :type character
    :documentation "The last character that was output."))
  (:documentation "An IO stream that emulates an ANSI terminal."))

(defvar *stream* nil
  "The current dynamic terminal emulator stream.")

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Modes 
 ╰|#

(defclass mode-table ()
  ((table
    :initarg :table :accessor mode-table-table
    :documentation "Hash table of modes.")
   (property
    :initarg :property :accessor mode-table-property
    :documentation "Symbol property for storing the ID number.")))

(defclass standard-mode-table (mode-table) ; aka ANSI mode
  ()
  (:default-initargs
   :property 'standard-mode-number)
  (:documentation "Standardized modes."))

(defclass private-mode-table (mode-table) ; aka DEC private mode
  ()
  (:default-initargs
   :property 'mode-number)
  (:documentation "DEC private mode. Now it's just normal non-standard modes."))

(defclass mode ()
  ((number
    :initarg :number :accessor mode-number :initform 0 :type fixnum
    :documentation "ID number of the mode. Use in control sequences.")
   (name
    :initarg :name :accessor mode-name
    :documentation "Name of the mode.")
   (off-name
    :initarg :off-name :accessor mode-off-name
    :documentation "The name for when it's turned off.")
   (read-only
    :initarg :read-only :accessor mode-read-only :initform nil :type boolean
    :documentation "True if we shouldn't change it.")
   (value
    :initarg :value :accessor mode-value
    :documentation
    "Value of the mode. Also the default value if in the prototype table."))
  (:documentation
   "Settings / preferences / options for the terminal."))

(defgeneric %mode (id table)
  (:documentation "Get the mode setting for ‘id’ in ‘table’."))

(defmethod %mode ((id number) (table mode-table))
  "Return the mode setting for ‘id’ in ‘table’. "
  (gethash id (mode-table-table table)))

(defmethod %mode ((id symbol) (table mode-table))
  "Return the mode setting for ‘id’ in ‘table’."
  (gethash (get id (mode-table-property table)) (mode-table-table table)))

(defmethod (setf %mode) (value (id number) (table mode-table))
  "Set the mode setting value for ‘id’ in ‘table’."
  (let ((m (gethash id (mode-table-table table))))
    (setf (mode-value m) value)
    ;; Call the action function if it's defined.
    (when (fboundp (mode-name m))
      (funcall (mode-name m) value))))

(defmethod (setf %mode) (value (id symbol) (table mode-table))
  "Set the mode setting value for ‘id’ in ‘table’."
  (let ((m (gethash (get id (mode-table-property table))
		    (mode-table-table table))))
    (setf (mode-value m) value)
    ;; Call the action function if it's defined.
    (when (fboundp (mode-name m))
      (funcall (mode-name m) value))))

(defvar *non-existent* (gensym "floaty"))

(defgeneric %has-mode (id table)
  (:documentation
   "Return true if the mode ‘id’ is defined in ‘table’."))

(defmethod %has-mode ((id number) (table mode-table))
  "Return true if we know about the mode with ‘id’."
  (not (eq *non-existent*
	   (gethash id (mode-table-table table) *non-existent*))))

(defmethod %has-mode ((id symbol) (table mode-table))
  "Return true if we know about the mode with ‘id’."
  (not (eq *non-existent*
	   (gethash (get id (mode-table-property table))
		    (mode-table-table table) *non-existent*))))

#|
(defstruct mode
  "Settings / preferences / options for the terminal. Used to be DIP switches."
  (number 0 :type fixnum)
  name
  off-name
  (read-only nil :type boolean)		; true if we shouldn't change it
  (value nil :type boolean))		; doubles as the default value

(defmacro def-mode-set (name)		; living in a dry town
  (let ((setter-name (symbolify (s+ "set-" name)))
	(accessor-name (symbolify (s+ name "s")))
	(existence-name (symbolify (s+ "has-" name)))
	(prop-name (symbolify (s+ name "-number"))))
  `(progn
     (defun ,name (stream id)
       "Return the mode setting for ‘id’ in ‘stream’. ‘id’ can be a mode name
symbol, or a mode number."
       (typecase id
	 (number
	  (gethash id (,accessor-name stream)))
	 (symbol
	  (gethash (get id ',prop-name) (,accessor-name stream)))))

     (defun ,setter-name (stream id value)
       "Set the mode setting value for ‘id’ in ‘stream’. ‘id’ can be a mode name
symbol, or a mode number."
       (let ((m (typecase id
		  (number
		   (gethash id (,accessor-name stream)))
		  (symbol
		   (gethash (get id ',prop-name) (,accessor-name stream))))))
	 (setf (mode-value m) value)
	 ;; Call the action function if it's defined.
	 (when (fboundp (mode-name m))
	   (funcall (mode-name m) stream value))))

     (defvar *non-existent* (gensym "floaty"))

     (defun ,existence-name (id)
       "Return true if we know about the mode with ‘id’."
       (not (eq *non-existent*
		(typecase id
		  (number
		   (gethash id (,accessor-name stream) *non-existent*))
		  (symbol
		   (gethash (get id ',prop-name) (,accessor-name stream)
			    *non-existent*))))))

     (defun ,default-name (stream id)
       "Return the default mode setting for ‘id’ in ‘stream’. ‘id’ can be a mode
name symbol, or a mode number."
       (typecase id
	 (number
	  (gethash id (,accessor-name stream)))
	 (symbol
	  (gethash (get id ',prop-name) (,accessor-name stream)))))

     (defsetf ,name ,setter-name
       "Set the mode setting value for ‘id’ in ‘stream’. ‘id’ can be a mode name
symbol, or a mode number."))))

(def-mode-set mode)
(def-mode-set public-mode)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-mode-hash-table (mode-list)
    "Make a hash table of node structs from ‘list’."
    (let ((table (make-hash-table)))
      (loop
	:for (num name off-name value) :in mode-list
	:do
	   (setf (gethash num table)
		 (make-instance 'mode
				:number num
				:name name
				:off-name off-name
				:read-only nil
				:value value))
	   (setf (get name 'mode-number) num)) ;; @@@ what about standard-modes
      table)))

(defparameter *modes*
  (make-mode-hash-table
    ;; num  name                          off-name                       default
    `((1    application-cursor-keys       "normal cursor keys"            nil)
      (2    vt100-emulation               "vt52 emulation"                t)
      (3    132-column-mode               "80 column mode"                nil)
      (4    smooth-scroll                 "jump scroll"                   nil)
      (5    normal-video                  "reverse video"                 nil)
      (6    origin-mode                   "normal cursor mode"            nil)
      (7    auto-wrap                     "no auto wrap"                  t)
      (8    auto-repeat                   "no auto repeat"                t)
      (9    x10-mouse-tracking            "no x10 mouse send"             nil)
      (10   show-toolbar                  "hide toolbar"                  nil)
      (12   blinking-cursor-att           "no att cursor blink"           nil)
      (13   blinking-cursor               "no cursor blink"               nil)
      (14   blinking-cursor-xor           "no xor cursor blink"           nil)
      (18   print-form-feed               "no print form feed"            t)
      (19   print-full-screen             "print scrolling region"        nil)
      (25   show-cursor                   "hide cursor"                   t)
      (30   show-scrollbar                "hide scrollbar"                nil)
      (35   enable-font-shifting          "disable font shifting"         nil)
      (40   allow-132-columns             "disable 132 columns"           nil)
      (41   more-fix                      "no more fix"                   nil)
      (42   national-charsets             "no national charsets"          nil)
      (44   margin-bell                   "no margin bell"                nil)
      (45   reverse-wraparound            "no reverse-wraparound"         t)
      (46   logging                       "no logging"                    nil)
      (47   alternate-screen              "normal screen"                 nil)
      (66   application-keypad            "numeric keypad"                t)
      (67   backspace                     "delete"                        nil)
      (69   margin-mode                   "no margin mode"                nil)
      (95   clear-on-column-set           "no clear on column set"        nil)
      (1000 x11-mouse-tracking            "no x11 mouse tracking"         nil)
      (1001 hilite-mouse-tracking         "no hilite mouse tracking"      nil)
      (1002 cell-mouse-tracking           "no cell mouse tracking"        nil)
      (1003 all-mouse-tracking            "no all mouse tracking"         nil)
      (1004 focus-tracking                "no focus tracking"             nil)
      (1005 utf8-mouse-mode               "no utf8 mouse mode"            nil)
      (1006 sgr-mouse-mode                "no sgr mouse mode"             nil)
      (1007 alternate-scroll              "no alternate scroll"           nil)
      (1010 output-scroll-to-bottom       "no output scroll to bottom"    nil)
      (1011 key-scroll-to-bottom          "no key scroll to bottom"       nil)
      (1015 urxvt-mouse                   "no urxvt mouse"                nil)
      (1034 meta-key                      "no meta key"                   nil)
      (1035 alt-numlock-modifiers         "no alt numlock modifiers"      nil)
      (1036 meta-sends-escape             "no meta sends escape"          t)
      (1037 vt220-remove-key              "del keypad key"                nil)
      (1039 alt-sends-escape              "no alt sends escape"           t)
      (1040 keep-selection                "no keep selection"             nil)
      (1041 clipboard-selection           "no clipboard selection"        t)
      (1042 bell-is-urgent                "no bell is urgent"             nil)
      (1043 pop-on-bell                   "no pop on bell"                nil)
      (1046 alternate-screen-switching    "no alternate screen switching" t)
      (1047 alternate-screen-buffer       "no alternate screen buffer"    t)
      (1048 save-cursor                   "no save cursor"                t)
      (1049 alt-save-cursor               "no alt save cursor"            t)
      (1050 terminfo-function-keys        "no terminfo function key"      t)
      (1051 sun-function-keys             "no sun function keys"          nil)
      (1052 hp-function-keys              "no hp function keys"           nil)
      (1053 sco-function-keys             "no sco function keys"          nil)
      (1060 legacy-keyboard               "no legacy keyboard"            nil)
      (1061 pc-keyboard                   "vt220 keyboard"                t)
      (2004 bracketed-paste               "no bracketed paste"            nil))))

;; @@@ ctlseqs.txt says these are read-only, but are they?
;; Set read-only values:
;; (setf (mode-read-only (gethash 13 *modes*)) t) ; blinking-cursor
;;       (mode-read-only (gethash 14 *modes*)) t) ; blinking-cursor-xor

(defparameter *modes-defaults*
  (make-instance 'private-mode-table
		 :table *modes*
		 :property 'mode-number))

(defparameter *standard-modes*
  (make-mode-hash-table
    ;; num  name             off-name               default
    `((2  keyboard-action    "no keyboard action"   nil)
      (4  insert-mode        "replace mode"         nil)
      (12 send/recieve-mode  "no send/receive mode" nil)
      (20 automatic-newline  "normal linefeed"      nil))))

(defparameter *standard-modes-defaults*
  (make-instance 'standard-mode-table
		 :table *standard-modes*
		 :property 'standard-mode-number))

;; private modes

(defun mode (stream id)
  "Return the mode setting for ‘id’ in ‘stream’."
  (%mode id (modes stream)))

(defun set-mode (stream id value)
  "Set the mode for ‘id’ in ‘stream’ to ‘value’."
  (setf (%mode id (modes stream)) value))

(defsetf mode set-mode)

(defun has-mode (stream id)
  (%has-mode id (modes stream)))

(defun mode-default (id)
  "Return the default mode for ‘id’ in ‘stream’."
  (%mode id *modes-defaults*))

;; standard modes

(defun standard-mode (stream id)
  "Return the standard mode setting for ‘id’ in ‘stream’."
  (%mode id (standard-modes stream)))

(defun set-standard-mode (stream id value)
  "Set the standard mode for ‘id’ in ‘stream’ to ‘value’."
  (setf (%mode id (standard-modes stream)) value))

(defsetf standard-mode set-standard-mode)

(defun has-standard-mode (stream id)
  (%has-mode id (standard-modes stream)))

(defun standard-mode-default (id)
  "Return the default standard mode for ‘id’ in ‘stream’."
  (%mode id *standard-modes-defaults*))

;; Custom mode functions

(defun show-cursor (value)
  (with-slots ((tty terminal)) *stream*
    (if value
	(terminal-cursor-on tty)
	(terminal-cursor-off tty))))

#|──────────────────────────────────────────────────────────────────────────|#

(defmethod initialize-instance
    :after ((o ansi-stream) &rest initargs &key &allow-other-keys)
  "Initialize a ansi-stream."
  (declare (ignore initargs))
  ;; We could, in theory, do the terminal-start in here, but I think it's
  ;; best left to callers discretion when to do that.
  (setf (modes o)
	(make-instance 'mode-table
		       :table (copy-hash-table *modes*)
		       :property 'mode-number)
	(standard-modes o)
	(make-instance 'standard-mode-table
		       :table (copy-hash-table *standard-modes*)
		       :property 'standard-mode-number))

  ;; Set up the character sets.
  (setf (aref (charsets o) +G0+) *normal-gl*
	(aref (charsets o) +G1+) *normal-gr*
	(aref (charsets o) +G2+) *dec-graphics-gl*
	(aref (charsets o) +G3+) *unicode-gr*
	(left-charset o) +G0+
	(right-charset o) +G1+))

(defun pushback (stream bytes)
  "Send a vector of bytes back to the application for status reporting, etc."
  (typecase bytes
    (string
     (funcall (pushback-function stream) stream
	      (unicode:string-to-utf8b-bytes bytes)))
    (vector
     (funcall (pushback-function stream) stream bytes))))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Parameters 
 ╰|#

(defun param (stream n &key default)
  "Return parameter number ‘n’. If parameter ‘n’ isn't set return ‘default’
or NIL if default isn't given."
  (with-slots (params) stream
    (if (<= n (fill-pointer params))
	(aref (params stream) (1- n))	; ANSI parameters are 1 indexed
	default)))

(defun param-index (stream n &key default)
  "Return parameter number ‘n’ to be used as an index. This adjusts for terminal
parameters being 1 indexed vs. 0 indexed. If parameter ‘n’ isn't set, return
‘default’ if it's given, or NIL if it's not. Note that without ‘default’ set,
this assumes the parameter is a number."
  (with-slots (params) stream
    (if (<= n (fill-pointer params))
	(1- (aref (params stream) (1- n)))
	default)))

(defun set-param (stream n value)
  (with-slots (params) stream
    (setf (aref params (1- n)) value)))

(defun add-to-param (stream digit)
  "Add the ‘digit’ to the current parameter."
  (with-slots (params) stream
    (let ((n (fill-pointer params)))
      (set-param stream n (+ (* (param stream n) 10)
			     (digit-char-p (code-char digit))))
      ;; (format *debug-io* "param add ~s ~s ~s~%"
      ;; 	    digit (code-char digit)
      ;; 	    (param stream n))
      (dbug "param add ~s ~s ~s~%"
	    digit (code-char digit)
	    (param stream n))
      )))

(defun new-param (stream)
  "Start a new parameter with the first ‘digit’."
  (with-slots (params) stream
    (when (= (fill-pointer params) (array-total-size params))
      ;; @@@ Or maybe we should just silently fail.
      (error "Too many parameters."))
    (incf (fill-pointer params))
    (setf (aref params (fill-pointer params)) 0)
    ;; (format *debug-io* "new-param ~s~%" (fill-pointer params))
    (dbug "new-param ~s~%" (fill-pointer params))
    ))

(defun param-count (stream)
  "Return the number of parameters."
  (fill-pointer (params stream)))

(defun clear-params (stream)
  "Erase the current parameter set."
  (with-slots (params) stream
    (setf (fill-pointer params) 0
	  (aref params (fill-pointer params)) 0)))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Prefixes
 │
 │ Probably one of:
 │  ' ' '!'  '"'  '#'  '$'  '''  '*'  '='  '>'  '?'
 ╰|#

(defun has-prefix (stream prefix)
  "Return true if ‘prefix’ is set in ‘stream’."
  (and (find prefix (prefixes stream)) t))

(defun add-prefix (stream prefix)
  "Add ‘prefix’ to the list of prefixes in ‘stream’."
  (pushnew prefix (prefixes stream))) 

(defun clear-prefixes (stream)
  "Clear the list of prefixes in ‘stream’."
  (setf (prefixes stream) nil))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Keys
 ╰|#

(defun key-string (stream key)
  "Return a bytes vector for the keyword ‘key’."
  (dbug "key-string application-cursor-keys ~s~%"
	(mode-value (mode stream 'application-cursor-keys)))
  (let (tag-key num-key c)
    (when (setf c (car (or (setf tag-key (rassoc key *key-tag*))
			   (setf num-key (rassoc key *key-num*)))))
      (cond
	(tag-key
	 (if (mode-value (mode stream 'application-cursor-keys))
	     (vector (char-code #\escape) (char-code #\O) (char-code c))
	     (vector (char-code #\escape) (char-code #\[) (char-code c))))
	(num-key
	 (map 'vector #'char-code (s+ #\escape #\[ c #\~)))))))

#|──────────────────────────────────────────────────────────────────────────|#

(defvar *break-on-unknown-controls* nil)
(defvar *break-on-unimplemented-controls* nil)

(defun unknown (x &rest other)
  (when *break-on-unknown-controls*
    (cerror "Yeah, so what?"
	    "Got an unknown control ~s ~:[~2*~;#x~x ~@c~]~{ ~a~}" x
	    (numberp x) x (when (numberp x) (code-char x))
	    other)))

(defun unimplemented (x &rest other)
  (when *break-on-unimplemented-controls*
    (cerror "Yeah, so what?"
	    "Got an un-implemented control ~s ~:[~2*~;#x~x ~@c~]~{ ~a~}" x
	    (numberp x) x (when (numberp x) (code-char x))
	    other)))

(defgeneric filter (stream thing)
  (:documentation "Filter an object through the emulator."))

(defmethod filter ((stream ansi-stream) (thing character))
  ;; Just defer to the byte filter.
  (filter stream (char-code thing)))

(defmethod filter ((stream ansi-stream) (thing string))
  ;; Mostly defer to the byte filter.
  ;; (let ((results
  ;; 	  (loop :for b :across thing
  ;; 		:collect (filter stream (char-code b)))))
  ;;   (apply #'s+ results))
  (loop :for b :across thing
	:do (filter stream (char-code b)))
  )

(defparameter *name* (map 'vector #'char-code "Lisp-Term")
  "The name of the terminal type, returned in query reports.")

(defun single-byte-control (stream thing out)
  "Process the signle byte control ‘thing’. ‘stream’ is the ansi-stream, and
‘out’ is the output stream."
  (with-slots ((tty terminal) left-charset charsets state last-char) stream
    (case thing
      (#x05				; ^E ENQ  Terminal status (answerback)
       ;; @@@ This is probably an annoyingly common mistake, so maybe we
       ;; should leave it blank. Like if you cat a binary file you don't
       ;; want a bunch of <terminal-name> ending up in your prompt.
       ;; Other reports are much harder to accidentally generate.
       (pushback stream *name*))
      (#x07				; ^G BEL  Bell
       (terminal-beep tty))
      (#x08				; ^H BS   Backspace
       (terminal-backward tty))
      (#x09				; ^I HT   Horizontal Tab
       ;; @@@ unless we want to maintain our own tab stops
       (write-char #\tab out)
       (setf last-char #\tab))
      (#x0a				; ^J LF   Line feed
       (terminal-scroll-down tty 1))
      (#x0b				; ^K VT   Vertical tab
       (terminal-scroll-down tty 1))
      (#x0c				; ^L FF   Form feed
       (terminal-scroll-down tty 1))
      (#x0d				; ^M CR   Carriage return
       (terminal-move-to-col tty 0))
      (#x0e				; ^N LS0  Shift out
       (setf left-charset +G1+))
      (#x0f				; ^O LS0  Shift in
       (setf left-charset +G0+))
      (#x1b				; ^[ ESC  Escape
       (setf state :escape))
      (#x20				; Space
       (write-char #\space out)
       (setf last-char #\space))
      (t
       ;; @@@ Is this right?
       (unknown thing "single byte control")
       (write-char #\space out)
       (setf last-char #\space)))))

(defun send-device-attributes (stream out &optional (parameter 0))
  (declare (ignore out))
  (case parameter
    (0
     ;; (pushback stream (s+ +csi+ "?1;2c")) ;; VT100 with Advanced Video Option
     ;; (pushback stream (s+ +csi+ "?1;0c")) ;; VT100 with No Options
     ;; (pushback stream (s+ +csi+ "?6c"))   ;; VT102
     (pushback stream (s+ +csi+ "?63;22c")) ;; VT320 with ANSI color
     #|
     (format out "~a?~a;~{~a~^;~}c" +csi+
	     (case emulation
	       (vt100 1)
	       (vt102 6)
	       (vt220 62)
	       (vt320 63)
	       (vt430 64))
	     ;; params for vt100
	     ()
	     #|
	     `((advanced-video 2)
	       (otherwise 0))
	     ;; params for >= vt220
	     '((132-columns                         1)
	       (printer                             2)
	       (regis-graphics                      3)
	       (sixel-graphics                      4)
	       (selective-erase                     6)
	       (user-defined-key                    8)
	       (national-replacement-character-sets 9)
	       (technical-characters                15)
	       (user-windows                        18)
	       (horizontal-scrolling                21)
	       (ansi-color                          22)
	       (ansi-text-locator                   29))
	     |#
     |#
     )))

(defparameter *terminal-type-number* #x434c)
(defparameter *firmware-version* 420)
(defparameter *rom-cartridge* 69)

(defun send-device-attributes-2 (stream out &optional (parameter 0))
  (declare (ignore out parameter))
  (pushback stream (format nil "~a>~a;~a;~ac" +csi+
			   *terminal-type-number* *firmware-version*
			   *rom-cartridge*)))

(defun send-device-attributes-3 (stream out &optional (parameter 0))
  (declare (ignore out parameter))
  ;; Nobody implements this, so does it matter?
  (pushback stream (format nil "~a=42c" +csi+)))

(defun escape-control (stream thing out)
  "Process the escape control ‘thing’. ‘stream’ is the ansi-stream, and
‘out’ is the output stream. Associated with the :escape state."
  (with-slots (terminal state charsets left-charset right-charset) stream
    (let ((new-state :start))
      (case thing
	;; non CSI Prefixes
	(#x20				; space
	 (setf new-state :escape-space))
	(#x23				; '#'
	 (setf new-state :escape-hash))
	(#x25				; '%'
	 (setf new-state :escape-percent))
	(#x28				; '('
	 (setf new-state :escape-open))
	(#x29				; ')'
	 (setf new-state :escape-close))
	(#x2a				; '*'
	 (setf new-state :escape-star))
	(#x2b				; '+'
	 (setf new-state :escape-plus))
	(#x2d				; '-'
	 (setf new-state :escape-minus))
	(#x2e				; '.'
	 (setf new-state :escape-dot))
	(#x2f				; '/'
	 (setf new-state :escape-slash))
	(#x4e				; 'N'  SS2    Single Shift of G2
	 (setf new-state :single-shift-g2))
	(#x4f				; 'O'  SS3    Single Shift of G3
	 (setf new-state :single-shift-g3))
	(#x50				; 'P'  DCS    Device Control String
	 (setf new-state :device))
	;; Immediate
	(#x44				; 'D'  IND    Index
	 (terminal-scroll-down terminal 1))
	(#x45				; 'E'  NEL    Next Line
	 (terminal-newline terminal))
	(#x48				; 'H'  HTS    Horizontal Tab Set
	 (unimplemented #x48 "horizontal tab set"))
	(#x4d				; 'M'  RI     Reverse index
	 (terminal-scroll-up terminal 1))
	;; Guarded areas seem like a bad idea, so we just ignore it.
	(#x56				; 'V'  SPA    Start of guarded area
	 (unimplemented #x56 "start of guarded area"))
	(#x57				; 'W'  EPA    End of guarded area
	 (unimplemented #x57 "end of guarded area"))
	(#x58			        ; 'X'  SOS    Start of string
	 ;; @@@ eat characters until something ???
	 (setf new-state :string))
	(#x5a			        ; 'Z'  DECID Return ID, like ^[[c
	 (send-device-attributes stream out))
	(#x5b			        ; ^[[  CSI   Control sequence introducer
	 (setf new-state :control))
	(#x5c				; ^[\  ST    String terminator
	 (unknown #x5c "string terminator should not occur by itself?"))
	(#x5d				; ^[]  OSC   Operating system command
	 (setf new-state :system))
	(#x5e			        ; ^[^  PM    Privacy message
	 (unimplemented #x5e "privacy message"))
	(#x5f			        ; ^[_  APC   Application program command
	 (unimplemented #x5f "application program command"))
	(#x36			        ; '6' Back index
	 (terminal-scroll-up terminal 1))
	(#x37				; '7' Save cursor
	 (terminal-save-cursor terminal))
	(#x38				; '8' Restore cursor
	 (terminal-restore-cursor terminal))
	(#x39				; '9' Forward index
	 (terminal-scroll-down terminal 1))
	(#x3d				; '=' Application keypad
	 (setf (mode stream 'application-keypad) t))
	(#x3e				; '>' Normal keypad
	 (setf (mode stream 'application-keypad) nil))
	(#x46				; 'F' Cursor to lower left
	 (terminal-move-to terminal (1- (terminal-window-rows terminal)) 0))
	(#x63				; 'c' Full reset
	 (terminal-reset terminal))
	(#x6c				; 'l' Memory lock
	 (unimplemented #x6c "memory lock"))
	(#x6d				; 'm' Memory unlock
	 (unimplemented #x6d "memory unlock"))
	(#x6e				; 'n' G2 charset to GL
	 (setf left-charset +g2+))
	(#x6f				; 'o' G3 charset to GL
	 (setf left-charset +g3+))
	(#x7c				; '|' G3 charset to GR
	 (setf right-charset +g3+))
	(#x7d				; '}' G2 charset to GR
	 (setf right-charset +g2+))
	(#x7e				; '~' G1 charset to GR
	 (setf right-charset +g1+))
	(otherwise
	 (unknown thing "escape control")))
      (setf state new-state))))

(defun find-charset (code)
  "Return the charset for ‘code’."
  (or
   (case code
     (#x41)		      ; 'A' United Kingdom (UK)
     (#x42		      ; 'B' United States (USASCII)
      *normal-gl*)
     (#x34)		      ; '4' Dutch
     ((#x43 #x35))	      ; 'C' or '5' Finnish
     ((#x52 #x66))	      ; 'R' or 'f' French
     ((#x51 #x39))	      ; 'Q' or '9' French Canadian
     (#x4b)		      ; 'K' German
     ;;((#x22 #x3e))	      ; '"' '>' Greek
     ;; ((#x25 #x3d))	      ; '%' '=' Hebrew
     ;; (#x59)		      ; 'Y' Italian
     ;; ((#x60 #x45 #x54))      ; '`' , 'E' or '6' Norwegian/Danish
     ;; ((#x25 #x36))	      ; '%' '6' portuguese
     ;; (#x5a)		      ; 'Z' Spanish
     ;; ((#x48 #x37))	      ; 'H' or '7' Swedish
     ;; (#x3d)		      ; '=' Swiss
     ;; ((#x25 #x32))	      ; '%' '2' Turkish
     (#x30		      ; '0' DEC Special Character and Line Drawing Set
      *dec-graphics-gl*)
     (#x3c)		      ; '<' DEC Supplemental
     (#x3e)		      ; '>' DEC Technical
     ;; ((#x25 #x35))	      ; '%' '5' DEC Supplemental Graphics
     ;; ((#x26 #x34))	      ; '&' '4' DEC Cyrillic
     ;; ((#x22 #x3f))	      ; '"' '?' DEC Greek
     ;; ((#x22 #x34))	      ; '"' '4' DEC Hebrew
     ;; (#x25)		      ; '%' '0' DEC Turkish
     )
   *normal-gl*)) ;; mostly moot

(defun widen-string (string)
  "In case ‘string’ is a base-char string, as can be returned by certain 
functions like ‘format’, turn it into a ‘character’ string."
  (if (typep string 'base-string)
      (make-array (length string) :element-type 'character
				  :initial-contents string)
      string))

(defun de-escape (string)
  "Get rid of real escapes in ‘string’ to make it safe for debugging printing."
  (osubstitute (code-char #x241b) #\escape (widen-string string)))

(define-constant +blank-fatchar+ (make-fatchar) "Yerp." #'equalp)

;; Slow cheat for now until we can refactor grok-ansi-color.
;; Probably a good refactoring spot would be a list of parameter numbers.
(defun set-attributes (stream out)
  "Set the character attributes given what's in the parameters."
  (declare (ignore out))
  #|
  (with-slots (state (tty terminal)) stream
    (let* ((esc-str (format nil "~c[~{~a~^;~}mX"
			    #\escape
			    (loop :for i :from 1 :to (param-count stream)
				  :collect (param stream i))))
	   (str (make-fat-string :string esc-str))
	   (new-str #| (process-ansi-colors str) |#)
	   (c #| (oelt new-str 0) |#))
      ;; (format *debug-io* "fc ~s ~s ~s~%" (de-escape str)
      ;; 	      (de-escape esc-str)
      ;; 	      (param-count stream))
      (dbug "fc ~s ~s ~s~%" (de-escape str)
	    (de-escape esc-str)
	    (param-count stream))
      (setf new-str (process-ansi-colors str))
      (setf c (oelt new-str 0))
      (terminal-set-attributes tty (fatchar-attrs c))
      (terminal-color tty (or (fatchar-fg c) :default)
		      (or (fatchar-bg c) :default))
      ;; (format *debug-io* "fc ~s~%" c)
      (dbug "fc ~s~%" c)
      )))
  |#
  (with-slots ((tty terminal)) stream
    (let ((fc (make-fatchar :attrs '(all-ball)))
	  (params
	    (loop :for i :from 1 :to (param-count stream)
		  :collect (param stream i))))
      (multiple-value-bind (new-fc added removed new-fg new-bg)
	  (fatchar::ansi-params-to-fatchar fc params)
	(cond
	  ((not (find 'all-ball (fatchar-attrs new-fc)))
	   (when (eq new-fg :unset)
	     (setf new-fg nil))
	   (when (eq new-bg :unset)
	     (setf new-bg nil))
	   (terminal-set-rendition tty (make-fatchar
					:fg new-fg :bg new-bg
					:attrs (fatchar-attrs new-fc)))
	   (dbug "[][][] blanked the monkey [][][]~%"))
	  (t
	   (when (eq new-fg :unset)
	     (setf new-fg nil))
	   (when (eq new-bg :unset)
	     (setf new-bg nil))
	   (when (or new-fg new-bg)
	     (dbug "color to ~s ~s~%" new-fg new-bg)
	     (terminal-color tty new-fg new-bg))
	   ;; (terminal-color tty new-fg new-bg)
	   (dbug "attributes added to ~s~%" added)
	   (dbug "attributes removed to ~s~%" removed)
	   (loop :for a :in added
		 :do (terminal-set-attribute tty a t))
	   (loop :for a :in removed
		 :do (terminal-set-attribute tty a nil))))))))

(defun set-ansi-modes (stream value)
  "Set the modes in the current parameters in ‘stream’ to ‘value’. Set the
standard or private modes depending on the prefix slot in ‘stream’."
  (dbug "set-ansi-modes~%")
  (let ((modes
	  (loop :for i :from 1 :to (param-count stream)
		:collect (param stream i))))
    (dbug "set-ansi-modes ~s ~s~%" modes (params stream))
    (if (has-prefix stream #x3f) ; ?
	;; Private mode reset
	(loop :for m :in modes :do
	  (dbug "set private mode ~s to ~a~%"  (mode-name (mode stream m)) value)
	  (setf (mode stream m) value))
	;; Reset mode (which I call standard)
	(loop :for m :in modes :do
	  (dbug "set standard mode ~s to ~a~%" (mode-name (mode stream m)) value)
	  (setf (standard-mode stream m) value)))))

;; This is a very irregular control. It can read arbitrary data.
;; It has non-numeric parameters. It selects different subsequent paramter
;; formats, based on previous parameters, etc.
;; But when we get in here, all that stuff should be in the ‘data’ slot.
(defun system-control (stream thing out)
  "Process the system control ‘thing’. ‘stream’ is the ansi-stream, and
‘out’ is the output stream. Associated with the :system state. Usually after
an +OSC+ is read."
  (declare (ignore thing out))
  (with-slots ((tty terminal) data state) stream
    (let ((new-state :start))
      (case (param stream 1)
	(0 ; Change icon name and window title
	 (if (= (aref data 0) #x3f)	;; '?'
	     ;; report the title
	     (pushback stream (terminal-title tty))
	     ;; set the title
	     (setf (terminal-title tty) (unicode:utf8b-bytes-to-string data))))
	(1 ; Change icon name
	 ;; @@@ we would have to add or change a method for this
	 )
	(2 ; Change window title
	 (if (= (aref data 0) #x3f)	;; '?'
	     ;; report the title
	     (pushback stream (terminal-title tty))
	     ;; set the title
	     (setf (terminal-title tty) (unicode:utf8b-bytes-to-string data))))
	(3 ; Set X proptery on top-level window
	 ;; @@@ I'm not sure this is even a good idea. Also we're not necessarily
	 ;; running on X11.
	 )
	(4 ; Change or report color number
	 ;; (or <color-number> '?') ';' (or <color-spec> '?')
	 ;; Color numbers are:
	 ;;    0-7   normal ANSI color
	 ;;    8-15  bright ANSI color
	 ;;   16-256 color table (88 or 256)
	 ;; Color specs are:
	 ;;   a color name (like from X11 rgb.txt)
	 ;;   something XParseColor can understand
	 )
	(5 ; Change or report special color number
	 ;; (or <color-number> '?') ';' (or <color-spec> '?')
	 ;; Special color numbers are:
	 ;;   0 color for bold
	 ;;   1 color for underline
	 ;;   2 color for blink
	 ;;   3 color for reverse
	 ;;   4 color for italic
	 )
	(6 ; Enable/disbale special color number
	 ;; (or <color-number> '?') ';' (or <color-spec> '?')
	 ;; These
	 ;;    0  resource colorBDMode (BOLD).
	 ;;    1  resource colorULMode (UNDERLINE).
	 ;;    2  resource colorBLMode (BLINK).
	 ;;    3  resource colorRVMode (REVERSE).
	 ;;    4  resource colorITMode (ITALIC).
	 ;;    5  resource colorAttrMode (Override ANSI).
	 )
	(10 ; Set/Get VT100 text foreground color
	 (cond
	   ((= (aref data 0) #x3f)	; '?'
	    (dbug "report window foreground ~s~%"
		  (color-to-xcolor
		   (lookup-color (terminal-window-foreground tty))))
	    (pushback stream
		      (s+ +osc+ 10 #\;
			  (color-to-xcolor
			   (lookup-color(terminal-window-foreground tty)))
			  (code-char 7))))
	   (t
	    (dbug "set window foreground ~s~%"
		  (unicode:utf8b-bytes-to-string data))
	    (setf (terminal-window-foreground tty)
		  (xcolor-to-color (unicode:utf8b-bytes-to-string data))))))
	(11 ; Set/Get VT100 text background color
	 (cond
	   ((= (aref data 0) #x3f)	; '?'
	    (dbug "report window foreground ~s~%"
		  (color-to-xcolor
		   (lookup-color (terminal-window-background tty))))
	    (pushback stream
		      (s+ +osc+ 10 #\;
			  (color-to-xcolor
			   (lookup-color (terminal-window-background tty)))
			  (code-char 7))))
	   (t
	    (dbug "set window foreground ~s~%"
		  (unicode:utf8b-bytes-to-string data))
	    (setf (terminal-window-background tty)
		  (xcolor-to-color (unicode:utf8b-bytes-to-string data))))))
	(12 ; Set/Get text cursor color
	 ;; We would need a new terminal method for this.
	 ;; Not all terminals would support it.
	 )
	(13 ; Set/Get mouse foreground color
	 ;; We would need a new terminal method for this.
	 ;; Not all terminals would support it.
	 )
	(14 ; Set/Get mouse background color
	 ;; We would need a new terminal method for this.
	 ;; Not all terminals would support it.
	 )
	(15 ; Set/Get Tektronix foreground color
	 ;; I don't think we're gonna do Tektronix emulation.
	 )
	(16) ; Set/Get Tektronix background color
	(17 ; Set/Get highlight color
	 ;; We would need a new terminal method for this.
	 ;; Not all terminals would support it.
	 )
	(18) ; Set/Get Tektronix cursor color
	(19 ; Set/Get highlight foreground color
	 ;; We would need a new terminal method for this.
	 ;; Not all terminals would support it.
	 )
	(46 ; Change log file
	 ;; Is this even useful? Or a good idea?
	 )
	(50 ; Set font
	 ;; The data is of the format
	 ;; '#' [0-9]+       A relative index into the font menu
	 ;; '#' [+-] [0-9]+  An absolute index into the font menu
	 ;; '#' ' ' <font name> ??? maybe
	 )
	(51 ; Emacs shell!! WTF?
	 )
	(52 ; Manipulate selection data
	 ;; This has two sub parameters:
	 ;;
	 ;; <selection type> which is one of:
	 ;;   'c' clipboard
	 ;;   'p' primary
	 ;;   'q' secondary
	 ;;   's' select
	 ;;   '0' cut buffer 0
	 ;;   '1' cut buffer 1
	 ;;   '2' cut buffer 2
	 ;;   '3' cut buffer 3
	 ;;   '4' cut buffer 4
	 ;;   '5' cut buffer 5
	 ;;   '6' cut buffer 6
	 ;;   '7' cut buffer 7
	 ;; and
	 ;;
	 ;; <selection data> encoded in base64, or a '?' to report it.
	 ;; If the selection isn't a '?' or base64 string, the selection is
	 ;; cleared.
	 )
	(104 ; Reset color number
	 ;; same color numbers as OSC 4
	 ;; If no color given, reset the entire table.
	 )
	(105 ; Reset special color number
	 ;; same color numbers as OSC 5
	 )
	(106 ; enable/disable special color number
	 ;; same color numbers as OSC 6
	 )
	;; These reset methods assume we knew what it was to start with.
	;; Since we're just in the middle, we can't without adding a new method
	;; to ask the back end.
	(110)			  ; Reset VT100 text foreground color
	(111)			  ; Reset VT100 text background color
	(112)			  ; Reset text cursor color
	(113)			  ; Reset mouse foreground color
	(114)			  ; Reset mouse background color
	(115)			  ; Reset Tektronix foreground color
	(116)			  ; Reset Tektronix background color
	(117)			  ; Reset highlight color
	(118)			  ; Reset Tektronix cursor color
	(119)			  ; Reset highlight foreground color
	(#x49)			  ; 'I' Set icon to file?
	(#x6c)			  ; 'l' Set window title, shelltool and dtterm
	(#x4c)			  ; 'L' Set icon label, shelltool and dtterm
	)
      (setf state new-state))))

(defun device-control (stream thing out)
  "Process the device control ‘thing’. ‘stream’ is the ansi-stream, and
‘out’ is the output stream. Associated with the :device state. Usually after an
+dcs+ is read."
  (declare (ignore thing out))
  (with-slots ((tty terminal) state data device-command) stream
    (let ((new-state :start))
      (case device-command
	(:user-keys
	 )
	(:status-request
	 (case (aref data 0)
	   (#x6d ; 'm' SGR
	    )
	   (#x22 ; '"'
	    (case (aref data 1)
	      (#x70 ; 'p'  DECSCL @@@ ?
	       )
	      (#x71 ; 'q'  DECSCA @@@ ?
	       )))
	   (#x20 ; space
	    (case (aref data 1)
	      (#x71 ; 'q'  DECSCUSR @@@ ?
	       )))
	   (#x72 ; 'r' DECSTBM
	    )
	   (#x73 ; 's' DECSLRM
	    )
	   (#x74 ; 't' DECSLPP
	    )
	   (#x24 ; '$'
	    (case (aref data 1)
	      (#x7c ; '|'  DECSCPP
	       )))
	   (#x2a ; '*'
	    (case (aref data 1)
	      (#x7c ; '|'  DECSNLS
	       )))))
	(:presentation-status)
	(:terminfo-set
	 ;; encoded in hex
	 ;; @@@@
	 )
	(:terminfo-get))
      (setf state new-state))))

(defun control (stream thing out)
  "Process the control ‘thing’. ‘stream’ is the ansi-stream, and ‘out’ is the
output stream. Associated with the :control state. Usually after an +CSI+
is read."
  (with-slots (state (tty terminal) last-char 8-bit-controls) stream
    (let ((new-state :start))
      (case thing
	;; prefixes
	;; ' ' '!'  '"'  '#'  '$'  '''  '*'  '='  '>'  '?'
	((#x20 #x21 #x22 #x23 #x24 #x27 #x2a #x3d #x3e #x3f)
	 (add-prefix stream thing)
	 (setf new-state :control))

	;; Not prefixes
	(#x40 ; '@' Insert blank characters
	 (terminal-insert-char tty (param stream 1 :default 1)))
	(#x41 ; 'A' Cursor up
	 (terminal-up tty (param stream 1 :default 1)))
	(#x42 ; 'B' Cursor down
	 (terminal-down tty (param stream 1 :default 1)))
	(#x43 ; 'C' Cursor forward
	 (terminal-forward tty (param stream 1 :default 1)))
	(#x44 ; 'D' Cursor backward
	 (terminal-backward tty (param stream 1 :default 1)))
	(#x45 ; 'E' Cursor next line
	 (dotimes (i (param stream 1 :default 1))
	   (terminal-newline tty)))
	(#x46 ; 'F' Previous line
	 (terminal-scroll-up tty (param stream 1 :default 1)))
	(#x47 ; 'G' Goto column
	 (terminal-move-to-col tty (param-index stream 1 :default 0)))
	(#x48 ; 'H' Cursor position
	 ;; (format *debug-io* "move-to ~s ~s~%"
	 ;; 	 (1- (or (param stream 1) 1))
	 ;; 	 (1- (or (param stream 2) 1)))
	 ;; (format *debug-io* "params ~s ~s~%"
	 ;; 	 (params stream) (fill-pointer (params stream)))
	 (dbug "move-to ~s ~s~%"
	       (param-index stream 1 :default 0)
	       (param-index stream 2 :default 0))
	 (dbug "params ~s ~s~%"
	       (params stream) (fill-pointer (params stream)))
	 (terminal-move-to tty (param-index stream 1 :default 0)
			       (param-index stream 2 :default 0)))
	(#x49 ; 'I' Cursor tab
	 (terminal-format tty "~v,,,va" (param stream 1 :default 1) #\tab #\tab))
	(#x4a ; 'J' Erase
	 (case (param stream 1 :default 0)
	   (0 (terminal-erase-below tty))
	   (1 (terminal-erase-above tty))
	   (2
	     (terminal-erase-above tty)
	     (terminal-erase-below tty))
	   (3
	    (tt-clear :saved-p t))))
	(#x4b ; 'K' Erase in line
	 (case (param stream 1 :default 0)
	   (0 (terminal-erase-to-eol tty))
	   (1 (terminal-erase-to-bol tty))
	   (2 (terminal-erase-line tty))))
	(#x4c ; 'L' Insert line
	 (dbug "insert ~s line~:*~p~%" (param stream 1 :default 1))
	 (terminal-insert-line tty (param stream 1 :default 1)))
	(#x4d ; 'M' Delete line
	 (dbug "delete ~s line~:*~p~%" (param stream 1 :default 1))
	 (terminal-delete-line tty (param stream 1 :default 1)))
	(#x50 ; 'P' Delete char
	 (dbug "delete ~s char~:*~p~%" (param stream 1 :default 1))
	 (terminal-delete-char tty (param stream 1 :default 1)))
	(#x53 ; 'S' Scroll up
	 (dbug "scroll up ~s~%" (param stream 1 :default 1))
	 (terminal-scroll-screen-up tty (param stream 1 :default 1)))
	(#x54 ; 'T'
	 (cond
	   ((= (param-count stream) 5)
	    ;; with 5 params? 
	    ;; @@@ mouse highlight tracking
	    (unimplemented #x54 "mouse highlight tracking"))
	   (t ;; Scroll down
	    (terminal-scroll-screen-down tty (param stream 1 :default 1)))))
	(#x58 ; 'X' Erase characters
	 ;; @@@ Erase char isn't really the space as spamming spaces.
	 ;; But we would have to add a terminal method.
	 (terminal-save-cursor tty)
	 (terminal-format tty "~v,,,va" (param stream 1 :default 1)
			  #\space #\space)
	 (terminal-restore-cursor tty))
	(#x5a ; 'Z' Back tab
	 (unimplemented #x5a "back tab")) ;; @@@
	(#x60 ; '`' character position absolute - variable # of params
	 (cond
	   ((param stream 2)
	    (terminal-move-to tty
			      (param-index stream 2)
			      (param-index stream 1)))
	   (t
	    (terminal-move-to-col tty (param-index stream 1)))))
	(#x61 ; 'a' character position relative
	 (cond
	   ((param stream 2)
	    (terminal-down tty (param stream 2))
	    (terminal-forward tty (param stream 1)))
	   (t
	    (terminal-forward tty (param stream 1 :default 1)))))
	(#x62 ; 'b' repeat preceding character
	 (loop :repeat (param stream 1 :default 1)
	       ;; :do (output-char stream last-char out)
	       :do (write-char last-char out)))
	(#x63 ; 'c' Send device attributes
	 (cond
	   ((null (prefixes stream))
	    (send-device-attributes stream out (param stream 1)))
	   ((has-prefix stream #x3d) ; =
	    (send-device-attributes-3 stream out (param stream 1)))
	   ((has-prefix stream #x3e) ; >
	    (send-device-attributes-2 stream out (param stream 1)))))
	(#x64 ; 'd' line position absolute
	 (cond
	   ((param stream 2)
	    (terminal-move-to tty (param-index stream 1)
			          (param-index stream 2)))
	   (t
	    (terminal-move-to-row tty (or (param-index stream 1) 1)))))
	(#x65 ; 'e' line position relative
	 (cond
	   ((param stream 2)
	    (terminal-down tty (param stream 2))
	    (terminal-forward tty (param stream 1)))
	   (t
	    (terminal-down tty (param stream 1 :default 1)))))
	(#x66 ; 'f' Horizontal and vertical position
	 (terminal-move-to tty (param-index stream 1) (param-index stream 2)))
	(#x67 ; 'g' Tab clear
	 (case (param stream 1 :default 0)
	   (0
	    ;; @@@ clear current column tab stop
	    )
	   (3
	    ;; @@@ clear all tab stops
	    )))
	(#x68 ; 'h' Set mode
	 (set-ansi-modes stream t))
	(#x69 ; 'i' media copy
	 (let ((modes
		 (loop :for i :from 1 :to (param-count stream)
		       :collect (param stream i))))
	   (when (null modes)
	     (setf modes '(0)))
	   (when (member 0 modes)
	     ;; @@@ print screen
	     )
	   (when (member 4 modes)
	     ;; @@@ turn off printer
	     )
	   (when (member 5 modes)
	     ;; @@@ turn on printer
	     )
	   (when (member 10 modes)
	     ;; @@@ HTML screen dump
	     )
	   (when (member 11 modes)
	     ;; @@@ svg screen dump
	     )))
	(#x6c ; 'l' Reset mode
	 (set-ansi-modes stream nil))
	(#x6d ; 'm' Character attributes
	 (set-attributes stream out))
	(#x6e ; 'n' device status report
	 (case (param stream 1)
	   (5
	    (pushback stream (format nil "~a0n" +csi+)))
	   (6
	    (multiple-value-bind (row col)
		(terminal-get-cursor-position tty)
	      (dbug "cursor pos report ~s ~s~%" row col)
	      (pushback stream (format nil "~a~a;~aR"
				       +csi+ (1+ row) (1+ col)))))))
	(#x70 ; 'p' various settings and reports
	 (cond
	   ;; Set pointer mode
	   ((has-prefix stream #x3e) ; '>'
	    (unimplemented #x3e "set pointer mode")
	    (case (param stream 1 :default 1)
	      (0) ;; never hide
	      (1) ;; hide if not tracking mouse
	      (2) ;; always hide, except when focus in/out
	      (3) ;; always hide
	      ))
	   ;; Soft terminal reset
	   ((has-prefix stream #x21) ; '!'
	    (unimplemented #x21 "soft-reset"))
	   ;; Set conformance level
	   ((has-prefix stream #x22) ; '"'
	    (unimplemented #x21 "set conformance level")
	    (case (param stream 1)
	      (61) ; VT100
	      (62) ; VT200
	      (63) ; VT300
	      (64) ; VT400
	      (65)) ; VT500
	    ;; Set 7 or 8 bit controls
	    (when (> (param-count stream) 1)
	      (setf 8-bit-controls
		    (case (param stream 2 :default 1)
		      (0 t)
		      (1 nil)
		      (2 t)))))
	   ((has-prefix stream #x24) ; '$'
	    (cond
	      ;; '?' Ps '$' - Request DEC private mode
	      ((has-prefix stream #x3f) ; '?'
	       (let* ((p1 (param stream 1))
		      (m (cond
			   ((has-mode stream p1)
			    (let ((default (mode-default p1)))
			      (if (mode-read-only default)
				  (if (mode-value default)
				      3	; permanently set
				      4) ; permanently reset
				  (if (eq (mode-value (mode stream p1))
					  default)
				      1	   ; set
				      2)))) ; reset
			   (t 0)))) ;; not recognized
		 (pushback stream (s+ +CSI+ #\? p1 #\; m #\$ #\y))))
	      (t ; Ps '$' 'p' - Request ANSI mode
	       (let* ((p1 (param stream 1))
		      (m (cond
			   ((has-standard-mode stream p1)
			    (let ((default (standard-mode-default p1)))
			      (if (mode-read-only default)
				  (if (mode-value default)
				      3	; permanently set
				      4) ; permanently reset
				  (if (eq (mode-value (standard-mode stream p1))
					  default)
				      1	   ; set
				      2)))) ; reset
			   (t 0)))) ;; not recognized
		 (pushback stream (s+ +CSI+ p1 #\; m))))))
	   ((has-prefix stream #x23) ; '#'
	    (unimplemented #x23 "push video attributes"))))
	(#x71 ; 'q' more random settings
	 (cond
	   ((has-prefix stream #x20) ; ' ' Set cursor style
	    (unimplemented #x71 "set cursor style")
	    (case (param stream 1 :default 1)
               (0) ;; blinking block
               (1) ;; blinking block (default)
               (2) ;; steady block
               (3) ;; blinking underline
               (4) ;; steady underline
               (5) ;; blinking bar (xterm)
               (6) ;; steady bar (xterm)
	       (otherwise
		(unknown (param stream 1) "set cursor style"))))
	   ((has-prefix stream #x22) ; '"' Set protection attribute
	    (unimplemented #x71 "set protection attribute")
	    (case (param stream 1 :default 0)
               (0) ;; can erase
               (1) ;; can't erase
               (2) ;; can erase
	       (otherwise
		(unknown (param stream 1) "set protection attribute"))))
	   ((has-prefix stream #x23) ; '#' pop attributes
	    (unimplemented #x71 "pop attributes"))
	   (t ;  load LEDs
	    (unimplemented #x71 "load LED")
	    (case (param stream 1 :default 0)
	      (0) ; clear all
	      (1) ; light num lock
	      (2) ; light caps lock
	      (3) ; light scroll lock
	      (21) ; extinguish num lock
	      (22) ; extinguish caps lock
	      (23) ; extinguish scroll lock
	      (otherwise
	       (unknown (param stream 1 :default 0) "load LED parameter"))))))
	(#x72 ; 'r' 
	 (cond
	   ;; Set scrolling region
	   ((/= (param-count stream) 5) 
	    (let ((p1 (param-index stream 1 :default 0))
		  (p2 (param-index stream 2 :default
				   (1- (terminal-window-rows tty)))))
	      (dbug "set scrolling region ~s ~s~%" p1 p2)
	      (terminal-set-scrolling-region tty p1 p2)))
	   ;; rectangular attribute change
	   (t
	    (unimplemented #x72 "rectangular attribute change")))) ;; @@@
	(#x73 ; 's'
	 (cond
	   ;; save cursor
	   ((zerop (param-count stream))
	    (terminal-save-cursor tty))
	   ((= (param-count stream) 2)
	    (unimplemented #x73 "set left and right margins")))) ;; @@@
	(#x74 ; 't' window manipulation
	 (unimplemented #x74 "window manipulation" (param stream 1))
	 (case (param stream 1)
	   (1) ;; de-iconify window
	   (2) ;; iconify window
	   (3) ;; move window to x = p2 y = p3
	   (4) ;; resize window to width = p2 height = p3 in pixels
	   (5) ;; raise window to front
	   (6) ;; lower window to bottom
	   (7) ;; refresh the window
	   (8) ;; resize the text to width = p2 height = p3 in characters
	   (9 ;; maximize
	    (case (param stream 2)
	      (0) ;; restore maximized window
	      (1) ;; maximize window
	      (2) ;; maximize vertically
	      (3))) ;; maximize horizontally
	   (10 ;; full-screen
	    (case (param stream 2)
	      (0) ;; undo full screen
	      (1) ;; change to full screen
	      (2))) ;; toggle full screen
	   (11 ;; report window state
	    ;; not iconified ^[[1t
	    ;; iconified ^[[2t
	    )
	   (13 ;; report window position
	    ;; ^[[ 3 ; <x> ; <y> t
	    (case (param stream 2)
	      (2) ;; report text area position
	      (t))) ;; report window position
	   (14
	    ;; ^[[ 4 ; <height> ; <width> t
	    (case (param stream 2)
	      (2) ;; report window size in pixels
	      (t))) ;; report text size in pixels
	   (15 ;; report size of the screen in pixels
            ;; ^[[ 5 ; <height> ; <width> t
	    )
	   (16 ;; report xterm character size in pixels
            ;; ^[[ 6 ; <height> ; <width> t
	    )
           (18 ;; report size of the text area in characters
	    ;; ^[[ 8 ; <height> ; <width> t
	    )
	   (19 ;; report the size of the screen in characters
	    ;; ^[[ 9 ; <height> ; <width> t
	    )
	   (20 ;; report window's icon label
            ;; ^[] L <label> ^[\
	    )
	   (21 ;; report window's title
            ;; ^[] l <label> ^[\
	    )
           (22
	    (case (param stream 2)
	      (0) ; save icon and window title on stack
	      (1) ; save icon title on stack
	      (2))) ; save window title on stack
           (23
	    (case (param stream 2)
	      (0) ;; restore icon and window title from stack
              (1) ;; restore icon title from stack
              (2))) ;; restore window title from stack
	   (t
	    (let ((p1 (param stream 1)))
	      (when (and p1 (>= p1 24))
		;; resize to <p1> lines
		)))))
	(#x75 ; u Restore cursor
	 (cond
	   ;; save cursor
	   ((zerop (param-count stream))
	    (terminal-restore-cursor tty))
	   ((= (param-count stream) 2)
	    (unimplemented #x75 "set margin bell volume"))))
	(#x76 ; 'v'
	 (unimplemented #x76 "copy rectangular area")) ;; @@@
	(#x77 ; 'w'
	 ;; presentation state report
	 ;; filter rectangle
	 (unimplemented #x77 "presentation state report or filter rectangle"))
	(#x78 ; 'x' Request terminal parameters
	 (case (param stream 1)
	   ((0 1)
	    (unimplemented #x78 "request terminal parameters"))
	   (otherwise
	    (unknown (param stream 1) "request terminal parameters"))))
	(otherwise
	 (unknown thing "control sequence")))
      (setf state new-state))))

#|
(loop for c across ansi-terminal::*dec-graphics-gl*
for i from 0
do
(format t "~2s ~2x ~:c ~s~%" i (+ i #x20) c
   (rassoc (code-char (+ i #x20)) terminal-ansi::*acs-table-data*)))
|#

(defun output-char (stream char out)
  "Output a character that's not in a control sequence."
  (with-slots (left-charset right-charset charsets 8-bit-controls last-char)
      stream
    (let ((c 0))
    (cond
      ;; low control chars C0
      ((<= char #x1f)
       ;; This shouldn't happen, because they're handled by single-char-control
       ;; @@@ maybe an assert?
       (error "Tried to output a control as a non-control."))
      ((= char #x20) ;; space is always space
       (write-char #\space out)
       (setf last-char #\space))
      ;; graphic chars in GL
      ((<= #x21 char #x7e) ; '!' - '~'
       ;; (dbug "output GL char~%")
       (setf c (aref (aref charsets left-charset) (- char #x21)))
       (write-char c out)
       (setf last-char c))
      ((= #x7f char)
       ;; @@@ what should we do with ^?
       )
      ((<= #x80 char #xa0) ; 8 bit controls
       ;; @@@ actually handled in filter
       (when 8-bit-controls
	 ;; @@@
	 (unimplemented char "8-bit controls")))
      ;; graphic chars in GR
      ((<= #xa1 char #xfe) ; '¡' - 'þ'
       ;; (dbug "output GR char~%")
       (setf c (aref (aref charsets right-charset) (- char #xa1)))
       (write-char c out)
       (setf last-char c))
      ((= #xff char)
       ;; @@@ what should we do with #xff 'ÿ' ?
       (setf c (code-char char))
       (write-char c out)
       (setf last-char c))
      (t
       ;; (error "char wan't an 8-bit byte? but a ~a" (type-of char))
       ;; @@@ actually we get unicode chars > #xff here
       (setf c (code-char char))
       (write-char c out)
       (setf last-char c))))))

(defun control-dispatch (stream thing out)
  "Dispatch based on a previous control type."
  (with-slots (control-type) stream
    (case control-type
      (:control (control stream thing out))
      (:system  (system-control stream thing out))
      (:device  (device-control stream thing out))
      (otherwise
       (error "unknown control type ~a" control-type)))))

(defmethod filter ((stream ansi-stream) (thing integer))
  "This is the main entry point to the emulation state machine. ‘stream’ is an
ansi-stream. ‘thing’ is a byte that was read."
  (let ((*stream* stream))
    (with-slots ((tty terminal) state control-type left-charset right-charset
		 charsets data 8-bit-controls device-command last-char) stream
    ;; (format *debug-io* "state ~s ~x ~a~%" state thing
    ;; 	    (char-name (code-char thing)))
    (dbug "state ~s #x~x ~a~%" state thing
	  (char-name (code-char thing)))
    ;; (terminal-finish-output tty)
    ;; (dbug "~&--> ")
    ;; (read-line *debug-io*)
    ;;(with-output-to-string (out)
    (let ((out tty) (c 0))
      (case state
	(:start
	 (clear-params stream)
	 (clear-prefixes stream)
	 (setf control-type :control)
	 (cond
	   ;; 7-bit controls C0
	   ;; 0-31
	   ;; Single byte controls
	   ((<= 0 thing 31)
	    (single-byte-control stream thing out))
	   ((< #x80 thing #x9f)
	    (cond
	      (8-bit-controls
	       ;; 8-bit controls
	       ;; 128-159 C1
	       ;; 160-255 maps to 32-127 in 7-bit
	       (case thing
		 ((#x84		  ; ^[D  IND    Index
		   #x85		  ; ^[E  NEL    Next Line
		   #x88		  ; ^[H  HTS    Horizontal Tab Set
		   #x8d		  ; ^[M  RI     Reverse Index
		   #x8e		  ; ^[N  SS2    Single Shift of G2
		   #x8f		  ; ^[O  SS3    Single Shift of G3
		   #x90		  ; ^[P  DCS    Device Control String
		   #x96		  ; ^[V  SPA    Start of guarded area
		   #x97		  ; ^[W  EPA    End of guarded area
		   #x98		  ; ^[X  SOS    Start of string
		   #x9a		  ; ^[Z  DECID  Retuern terminal ID, like ^[[c
		   #x9b		  ; ^[[  CSI    Control sequence introducer
		   #x9c		  ; ^[\  ST     String terminator
		   #x9d		  ; ^[]  OSC    Operating system command
		   #x9e		  ; ^[^  PM     Privacy message
		   #x9f)	  ; ^[_  APC    Application program command
		  (escape-control stream
				  (dpb 1 (byte 1 6) (logand thing #x7f))
				  out))
		 (t
		  (write-char (setf c (code-char thing)) out)
		  (setf last-char c))))
	      ;; not 8-bit controls
	      (t
	       (write-char (setf c (code-char thing)) out)
	       (setf last-char c))))
	   ;; not in either 7-bit or 8-bit control range
	   (t
	    (output-char stream thing out))))
	(:escape
	 (escape-control stream thing out))
	(:control
	 (setf control-type :control)
	 (cond
	   ((<= (char-code #\0) thing (char-code #\9))
	    (setf state :param)
	    (new-param stream)
	    (add-to-param stream thing))
	   (t
	    (control stream thing out))))
	(:param
	 (cond
	   ((<= (char-code #\0) thing (char-code #\9))
	    (add-to-param stream thing))
	   ((eql #x3b thing) ; #\; the parameter separator
	    ;; (new-param stream)
	    (setf state control-type))
	   (t
	    (control-dispatch stream thing out))))
	(:system
	 (setf control-type :system)
	 (dbug "system command~%")
	 (cond
	   ((>= (param-count stream) 1)
	    ;; We alread read the first parameter, 
	    (dbug "system command read data~%")
	    (setf state :data)
	    (stretchy-truncate data)
	    (stretchy-append data thing)) ;; don't lose the first thing
	   ((<= (char-code #\0) thing (char-code #\9))
	    (setf state :param)
	    (new-param stream)
	    (add-to-param stream thing))
	   (t
	    (system-control stream thing out))))
	(:device
	 ;; @@@ not really right yet
	 (setf control-type :device)
	 (cond
	   ((<= (char-code #\0) thing (char-code #\9))
	    (setf state :param)
	    (new-param stream)
	    (add-to-param stream thing))
	   (t
	    (case thing
              (#x24 ; '$'
	       (setf state :device-dollar))
	      (#x2b ; '+'
	       (setf state :device-plus))
	      (#x7c ; '|'
	       (setf device-command :user-keys
		     state :data))))))
	(:device-dollar
	 (case thing
	   (#x71 ; 'q' Request status string
	    (setf device-command :status-request
		  state :data))
	   (#x74 ; 't' Restore/get presentation status
	    (setf device-command :presentation-status
		  state :data))
	   (t
	    (setf state :start))))
	(:device-plus
	 (case thing
	   (#x70 ; 'p' Set termcap/terminfo data
	    (setf device-command :terminfo-set
		  state :data))
	   (#x71 ; 'q' Request termcap/terminfo data
	    (setf device-command :terminfo-get
		  state :data))
	   (t
	    (setf state :start))))
	(:data
	 (case thing
	   (#x07 ; ^G
	    (dbug "^G data ending~%")
	    (setf state control-type)
	    ;; (system-control stream thing out)
	    (control-dispatch stream thing state))
	   (#x1b ; escape
	    (setf state :string-terminator))
	   (otherwise
	    (stretchy-append data thing))))
	 (:string-terminator
	  (cond
	    ((= thing #x5c) ; '\'
	     (dbug "^[\ data ending~%")
	     ;; (system-control stream thing out))
	     (control-dispatch stream thing state))
	    (t
	     ;; It wasn't second character of the termintor, so ignore the
	     ;; whole thing.
	     (setf state :start))))
	 (:escape-open
	  (setf (aref charsets +G0+) (find-charset thing))
	  (setf state :start))
	 (:escape-space
	  (case thing
	    (#x46			; 'F' send 7-bit control responses
	     (setf (8-bit-controls stream) nil))
	    (#x47			; 'G' send 8-bit control responses
	     (setf (8-bit-controls stream) t))
	    (#x4c			; 'L' set ANSI conformance level 1
	     (unimplemented thing "set ANSI conformance level 1"))
	    (#x4d			; 'M' set ANSI conformance level 2
	     (unimplemented thing "set ANSI conformance level 2"))
	    (#x4e			; 'N' set ANSI conformance level 3
	     (unimplemented thing "set ANSI conformance level 3"))
	    (otherwise
	     (unknown thing "escape space")))
	  (setf state :start))
	 (:escape-hash
	  (case thing
	    (#x33			; '3' Double height line top half
	     (unimplemented thing "double height line top half"))
	    (#x34			; '4' Double height line bottom half
	     (unimplemented thing "Double height line bottom half"))
	    (#x35			; '5' Single width line
	     (unimplemented thing "Single width line"))
	    (#x36			; '6' Double width line
	     (unimplemented thing "Double width line"))
	    (#x38			; '8' Screen alignment test
	     (unimplemented thing "Screen alignment test"))
	    (otherwise
	     (unknown thing "escape hash")))
	  (setf state :start))
	 (:escape-percent
	  (case thing
	    (#x40			; '@' default character set
	     (unimplemented thing "default character set"))
	    (#x47			; 'G' UTF-8 character set
	     (unimplemented thing "UTF-8 character set"))
	    (otherwise
	     (unknown thing "escape percent")))
	  (setf state :start))
	 (:escape-close
	  (setf (aref charsets +g1+) (find-charset thing))
	  (setf state :start))
	 (:escape-star
	  (setf (aref charsets +g2+) (find-charset thing))
	  (setf state :start))
	 (:escape-plus
	  (setf (aref charsets +g3+) (find-charset thing))
	  (setf state :start))
	 (:escape-minus
	  (setf (aref charsets +g1+) (find-charset thing))
	  (setf state :start))
	 (:escape-dot
	  (setf (aref charsets +g2+) (find-charset thing))
	  (setf state :start))
	 (:escape-slash
	  (setf (aref charsets +g3+) (find-charset thing))
	  (setf state :start))
	 (:single-shift-g2
	  (setf c (aref (aref charsets +G2+) (- thing #x21)))
	  (write-char c out)
	  (setf last-char c)
	  (setf state :start))
	 (:single-shift-g3
	  (setf c (aref (aref charsets +G3+) (- thing #x21)))
	  (write-char c out)
	  (setf last-char c)
	  (setf state :start))
	 (t
	  (error "Unknown state ~s" state)))))))

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Stream methods
 ╰|#

;; common methods

(defmethod-quiet close ((stream ansi-stream) &key abort)
  (declare (ignore abort))
  (terminal-done stream))

;; output stream methods

(defmethod stream-clear-output ((stream ansi-stream))
  (clear-output (ansi-stream-terminal stream)))

(defmethod stream-finish-output ((stream ansi-stream))
  (terminal-finish-output (ansi-stream-terminal stream)))

(defmethod stream-force-output ((stream ansi-stream))
  (terminal-finish-output (ansi-stream-terminal stream)))

(defmethod stream-write-sequence ((stream ansi-stream) seq start end
				  &key &allow-other-keys)
  (with-slots ((tty terminal)) stream
    (etypecase seq
      (string
       (filter stream (if end
			  (subseq seq (or start 0) end)
			  (subseq seq (or start 0)))))
      (list
       (loop
	 :with i = 0 :and l = seq #|:and c |#
	 :while (and l (< i end))
	 :do
	    ;; (when (and (>= i start)
	    ;; 	       (setf c (filter stream (car l))))
	    ;;   (terminal-write-char tty c))
	    (filter stream (car l))
	    (setf l (cdr l))
	    (incf i))))))

;; character output stream methods

(defmethod stream-advance-to-column ((stream ansi-stream) column)
  (terminal-move-to-col (ansi-stream-terminal stream) column)
  t)

;; This is a weird trick to presumably make it so we don't have to do our own
;; buffering and we can also be relatively quick?
(defvar *endless-spaces* '#1=(#\space . #1#)
  "The vast emptyness of space.")

(defmethod stream-line-column ((stream ansi-stream))
  (terminal-get-cursor-position (ansi-stream-terminal stream)))

(defmethod stream-start-line-p ((stream ansi-stream))
  (zerop (stream-line-column stream)))

;;(defmethod stream-fresh-line ((stream terminal-ansi-stream))

;; #+sbcl (defmethod sb-gray:stream-line-length ((stream terminal-ansi-stream))
;;   )

(defmethod stream-write-char ((stream ansi-stream) char
			      #| &optional start end |#)
  (with-slots ((tty terminal)) stream
    ;; (let ((out (filter stream char)))
    ;;   (typecase out
    ;; 	(null)
    ;; 	(character (terminal-write-char   tty out))
    ;; 	(t         (terminal-write-string tty out)))))
    (filter stream char))
  char)

(defmethod stream-write-byte ((stream ansi-stream) integer)
  "Implements ‘write-byte’; writes the integer to the stream and returns the
integer as the result."
  ;; @@@ This isn't really right. It should convert encoding.
  ;; So e.g. it might have to wait for another byte to output a character.
  (with-slots ((tty terminal)) stream
    ;; (let ((out (filter stream integer)))
    ;;   (typecase out
    ;; 	(null)
    ;; 	(character (terminal-write-char   tty out))
    ;; 	(t         (terminal-write-string tty out)))))
    (filter stream integer))
  integer)

(defmethod stream-write-string ((stream ansi-stream) string
			       &optional start end)
  (with-slots ((tty terminal)) stream
    ;; (let* ((str (if end
    ;; 		    (displaced-subseq string (or start 0) end)
    ;; 		    (displaced-subseq string (or start 0))))
    ;; 	   (out (filter stream str)))
    ;;   (typecase out
    ;; 	(null)
    ;; 	(character (terminal-write-char   tty out))
    ;; 	(t         (terminal-write-string tty out)))))
    (filter stream (if end
     		       (displaced-subseq string (or start 0) end)
     		       (displaced-subseq string (or start 0)))))
  string)

#|──────────────────────────────────────────────────────────────────────────┤#
 │ Input stream methods.
 ╰|#

(defmethod stream-clear-input ((stream ansi-stream))
  ;; @@@ This isn't exactly right either.
  ;; Since we might have to translate function keys, and we might be in the
  ;; middle of a function key sequence.
  (clear-input (ansi-stream-terminal stream)))

(defmethod stream-read-sequence ((stream ansi-stream) seq start end
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

(defmethod stream-read-char-no-hang ((stream ansi-stream))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (with-slots (terminal) stream
    (when (terminal-listen-for terminal 0)
      (terminal-get-char terminal))))

(defmethod stream-read-char ((stream ansi-stream))
  (let ((c (terminal-get-char (ansi-stream-terminal stream))))
    (or c :eof)))

(defmethod stream-read-byte ((stream ansi-stream))
  "Used by ‘read-byte’; returns either an integer, or the symbol ‘:eof’ if the
stream is at end-of-file."
  (let ((c (terminal-get-char (ansi-stream-terminal stream))))
    (or (and c (char-code c)) :eof)))

(defmethod stream-read-line ((stream ansi-stream))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (with-slots ((tty terminal)) stream
    (let* ((c)
	   (result
	    (with-output-to-string (line)
	      (loop
		:do (setf c (terminal-get-char tty))
		:while (and c (char/= c #\newline))
		:do (write-char c line)))))
      (when (and result (not (zerop (length result))))
	(remove-suffix result +newline-string+))
      (values result
	      (null c)))))

(defmethod stream-listen ((stream ansi-stream))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (terminal-listen-for (ansi-stream-terminal stream) 0))

(defmethod stream-unread-char ((stream ansi-stream) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (unread-char (ansi-stream-terminal stream) character))

#|──────────────────────────────────────────────────────────────────────────|#

(defun make-stream (&optional (terminal *terminal*))
  (make-instance 'ansi-stream :terminal terminal))

(defun make-stream-on (device)
  "Make a stream with an ANSI backend, that will output on ‘device’."
  (let ((new-term (make-instance 'terminal-ansi:terminal-ansi
				 :device-name device)))
    (terminal-start new-term)
    (make-instance 'ansi-stream :terminal new-term)))

(defun stream-done (stream)
  "Call to free resources in the ‘stream’."
  (terminal-done (ansi-stream-terminal stream))
  (setf (ansi-stream-terminal stream) nil)
  (values))

;; End
