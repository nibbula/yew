;;
;; terminal.lisp - The end of the line.
;;

(defpackage :terminal
  (:use :cl :dlib :opsys :trivial-gray-streams :fatchar)
  (:export
   #:*standard-output-has-terminal-attributes*
   #:*terminal*
   #:*default-terminal-type*
   #:*terminal-types*
   #:has-terminal-attributes
   #:likely-a-terminal-p
   #:terminal-default-device-name
   #:register-terminal-type
   #:find-terminal-class-for-type
   #:find-terminal-type-for-class
   #:terminal-types
   #:platform-default-terminal-type
   #:pick-a-terminal-type
   #:terminal-stream
   #:terminal
   #:terminal-file-descriptor       ;; #:file-descriptor
   #:terminal-device-name           ;; #:device-name
   #:terminal-output-stream         ;; #:output-stream
   #:terminal-window-rows           ;; #:window-rows
   #:terminal-window-columns        ;; #:window-columns
   #:terminal-start-at-current-line ;; #:start-at-current-line
   #:terminal-wrapper
   #:terminal-wrapped-terminal      ;; #:wrapped-terminal
   #:terminal-events-enabled	    ;; #:events-enabled
   #:terminal-get-size
   #:terminal-get-cursor-position
   #:terminal-start
   #:terminal-end
   #:terminal-done
   #:terminal-reinitialize
   #:make-terminal-stream
   #:with-terminal #:with-new-terminal
   #:tt-format			  #:terminal-format
   #:tt-format-at
   #:tt-write-string		  #:terminal-write-string
   #:tt-write-string-at
   #:tt-write-line		  #:terminal-write-line
   #:tt-write-line-at
   #:tt-write-char		  #:terminal-write-char
   #:tt-write-char-at
   #:tt-write-span		  #:terminal-write-span
   #:tt-write-span-at
   #:tt-newline			  #:terminal-newline
   #:tt-fresh-line		  #:terminal-fresh-line
   #:tt-move-to			  #:terminal-move-to
   #:tt-move-to-col		  #:terminal-move-to-col
   #:tt-beginning-of-line	  #:terminal-beginning-of-line
   #:tt-delete-char		  #:terminal-delete-char
   #:tt-insert-char		  #:terminal-insert-char
   #:tt-backward		  #:terminal-backward
   #:tt-forward			  #:terminal-forward
   #:tt-up			  #:terminal-up
   #:tt-down			  #:terminal-down
   #:tt-scroll-down		  #:terminal-scroll-down
   #:tt-scroll-up		  #:terminal-scroll-up
   #:tt-erase-to-eol		  #:terminal-erase-to-eol
   #:tt-erase-line		  #:terminal-erase-line
   #:tt-erase-above		  #:terminal-erase-above
   #:tt-erase-below		  #:terminal-erase-below
   #:tt-clear			  #:terminal-clear
   #:tt-home			  #:terminal-home
   #:tt-cursor-off		  #:terminal-cursor-off
   #:tt-cursor-on		  #:terminal-cursor-on
   #:tt-standout		  #:terminal-standout
   #:tt-normal			  #:terminal-normal
   #:tt-underline		  #:terminal-underline
   #:tt-bold			  #:terminal-bold
   #:tt-inverse			  #:terminal-inverse
   #:tt-color			  #:terminal-color
   #:tt-colors			  #:terminal-colors
   #:tt-window-foreground	  #:terminal-window-foreground
   #:tt-window-background	  #:terminal-window-background
   #:tt-beep			  #:terminal-beep
   #:tt-set-attributes            #:terminal-set-attributes
   #:tt-set-scrolling-region	  #:terminal-set-scrolling-region
   #:tt-finish-output		  #:terminal-finish-output
   #:tt-get-char		  #:terminal-get-char
   #:tt-get-key			  #:terminal-get-key
   #:tt-listen-for		  #:terminal-listen-for
   #:tt-input-mode                #:terminal-input-mode
   #:tt-reset			  #:terminal-reset
   #:tt-save-cursor		  #:terminal-save-cursor
   #:tt-restore-cursor		  #:terminal-restore-cursor
   #:tt-width
   #:tt-height
   #:tt-title                     #:terminal-title
   #:tt-has-attribute             #:terminal-has-attribute
   #:tt-has-autowrap-delay	  #:terminal-has-autowrap-delay
   #:tt-enable-events             #:terminal-enable-events
   #:tt-disable-events            #:terminal-disable-events
   #:terminal-enable-event
   #:terminal-disable-event
   #:tt-event #:tt-event-terminal
   #:tt-mouse-event #:tt-mouse-event-x #:tt-mouse-event-y
   #:tt-mouse-button-event #:tt-mouse-button #:tt-mouse-modifiers
   #:tt-mouse-button-press #:tt-mouse-button-release #:tt-mouse-button-motion
   #:tt-mouse-motion #:tt-resize-event
   #:tt-alternate-characters	  #:terminal-alternate-characters
   #:with-saved-cursor
   #:with-terminal-output-to-string
   #:with-style
   #:with-immediate
   )
  (:documentation
   "The TERMINAL package provides a generic interface to terminals.

# Terminal classes

Two generic classes are defined: TERMINAL and TERMINAL-STREAM. The TERMINAL
class is the normal class that should be used, which handles input and output.
TERMINAL-STREAM is supported by some terminal types, to potentially capture the
output that would go to a terminal. Note that terminals are output streams, so
they can also be used by the standard output functions.

Terminal have a width, height and cursor position. Terminals have a set of
text attributes that they support, which could be empty. Terminals have an
input mode, which can be :line for a line at a time, or :char for a character
at a time. Terminals can optionally have a device name. Terminals may have a
title. Depending on the terminal type, some terminals can have a saved cursor
position, a scrolling region, and a state where they output alternate
characters.

The TERMINAL package must be used in conjunction with another TERMINAL-*
package which implements the actual functionality for a specific type of
terminal. For example, the TERMINAL-ANSI package implements terminal
functionality that should work on most ANSI compatible terminals, which
probably includes most modern terminal emulators.

Unlike traditional terminal packages, we don't assume a large terminal
database, since use of hardware terminals, and their diversity of
incompatibility, is nearly abated. If one wants to use breadth of historical
terminals, the :curses terminal type can be used. The use of the sub-classing
instead of a database of character codes, not only reflects current reality
better, but allows for terminals that don't use ‘in-stream’ control codes,
a.k.a. escape sequences, but instead use an API. Examples are as the Windows
console, and the :crunch and :curses terminal types. It is also easy to imagine
other useful cases.

# Terminal use

The expected way to use this package is with the TT-* macros, which operate
on *TERMINAL*, which is considered the current terminal. The TERMINAL-*
methods can also be used with an explicit terminal instance.

The convenient way to create a terminal, is with the WITH-TERMINAL or
WITH-NEW-TERMINAL macros. These handle creating, starting and stopping, and
picking a default terminal type.

Terminals can also be created with MAKE-INSTANCE. In this case TERMINAL-START
should be called before calling any other functions, and TERMINAL-END should be
called to reset the terminal to the state before TERMINAL-START. TERMINAL-DONE
should be called if the program is completely done using the terminal, to free
up resources.

# Terminal types

Terminal types are specified by a keyword. When a terminal implementation is
loaded, it registers its type in *TERMINAL-TYPES*, which is used to find the
class. WITH-TERMINAL calls PICK-A-TERMINAL-TYPE to get its terminal type,
which further consults PLATFORM-DEFAULT-TERMINAL-TYPE, to try to come up with
a reasonable terminal. The terminal type keyword can be given to WITH-TERMINAL,
or *DEFAULT-TERMINAL-TYPE* can be set, to pick a specific terminal. This idea
is that this can be set by the user.

# Terminal functionality

## Terminal output

It is important to remember that terminals may not do any output until 
TT-FINISH-OUTPUT is called. The input functions, such as TT-GET-KEY, and the
TERMINAL-END usually call TT-FINISH-OUTPUT, so it can be easy to forget that
it may need to be called sometimes.

Output functions that accept characters also accept fatchars.
Output functions that accept strings also accept fat-strings.
Movement functions are usual row first then column.

### Output functions:

  tt-format
  tt-write-string
  tt-write-string-at
  tt-write-line
  tt-write-line-at
  tt-write-char
  tt-newline
  tt-fresh-line
  tt-beep
  tt-finish-output

### Movement functions:

  tt-move-to
  tt-move-to-col
  tt-beginning-of-line
  tt-backward
  tt-forward
  tt-up
  tt-down
  tt-home
  tt-scroll-down
  tt-scroll-up

### Erasing and editing functions:

  tt-erase-to-eol
  tt-erase-line
  tt-erase-above
  tt-erase-below
  tt-clear
  tt-delete-char
  tt-insert-char

### Attribute functions

  tt-standout
  tt-normal
  tt-underline
  tt-bold
  tt-inverse
  tt-color
  tt-set-attributes
  tt-has-attribute
  tt-colors

### Terminal state functions

  tt-cursor-off
  tt-cursor-on
  tt-set-scrolling-region
  tt-alternate-characters
  tt-enable-events
  tt-disable-events
  tt-reset
  tt-width
  tt-height

## Terminal input

  tt-get-char
  tt-get-key
  tt-listen-for
  tt-input-mode

## Extending

The functions provided by this library are much more limited than the
functionality provided by most terminal emulators and historical terminals,
but they seem adequate to implement a broad range of terminal applications.
Most of the functionality can be boiled down to, move to a location, change
text attributes or color, and output or erase characters. It is probably best
to keep the generic functionality to a minimum. Terminal specific
functionality can be easily added and called for specific types, and it's
fairly easy to sub-class a type.
"))
(in-package :terminal)

(declaim #.`(optimize ,.(getf terminal-config::*config* :optimization-settings)))

(defvar *standard-output-has-terminal-attributes* nil
  "True if we want programs to treat *standard-output* like it can display
terminal attributes.")

(defvar *terminal* nil
  "The default terminal to use for I/O.")

(defvar *default-terminal-type* nil
  "The type of terminal to create when unspecified.")

(defvar *terminal-types* nil
  "Alist of (keyword terminal-type) to provide easy names for terminal 
subclasses.")

(defun register-terminal-type (name type)
  "Subclasses should call this to register their type keyword."
  ;;(pushnew (list name type) *terminal-types*)
  (setf (getf *terminal-types* name) type))

(defun find-terminal-class-for-type (name)
  "Return the class for a registered terminal type."
  ;;(cadr (assoc type *terminal-types*))
  (getf *terminal-types* name))

(defun find-terminal-type-for-class (class)
  "Return the terminal type keyword for a registered terminal class name."
  (loop :with last
     :for i :in terminal:*terminal-types* :do
     (when (eq i class)
       (return last))
     (setf last i)))

(defun terminal-types ()
  "Return the list of registered terminal type keywords."
  (loop
     :for i = 0 :then (1+ i)
     :for tt :in *terminal-types*
     :when (evenp i) :collect tt))

(defun terminal-type-based-on-environemt ()
  (cond
    ((equal (environment-variable "TERM") "dumb")
     :dumb)
    (t :ansi)))

(defun platform-default-terminal-type ()
  "Return the platform default terminal type for the current circumstance."
  #+(and windows unix) (terminal-type-based-on-environemt)
  #+windows (if (environment-variable "TERM")
		(terminal-type-based-on-environemt)
		:ms)
  #+unix (terminal-type-based-on-environemt)
  #-(or windows unix) :ansi)

(defun pick-a-terminal-type ()
  "Pick some terminal type. Hopefully appropriate, but perhaps semi-arbitrary."
  (let ((platform-default (platform-default-terminal-type)))
    (or *default-terminal-type*
	(if (find-terminal-class-for-type platform-default)
	    platform-default
	    ;; This picks :ansi-stream if nothing else is loaded.
	    ;;(or (first *terminal-types*) :ansi)
	    :ansi))))

(defclass terminal-stream (fundamental-character-output-stream)
  ((output-stream
    :accessor terminal-output-stream
    :initarg :output-stream
    :documentation "Lisp stream for output.")
   (window-rows
    :accessor terminal-window-rows
    :initarg :window-rows
    :documentation "Number of rows of characters in the window.")
   (window-columns
    :accessor terminal-window-columns
    :initarg :window-columns
    :documentation "Number of columns of characters in the window."))
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal (terminal-stream fundamental-character-input-stream)
  ((file-descriptor
    :accessor terminal-file-descriptor
    :initarg :file-descriptor
    :documentation "System file descriptor.")
   (device-name
    :accessor terminal-device-name
    :initarg :device-name
    :documentation "System device name.")
   (start-at-current-line
    :initarg :start-at-current-line
    :accessor terminal-start-at-current-line
    :initform nil :type boolean
    :documentation
    "Tell terminals that care about it, to start managing the screen at the
current line.")
   (events-enabled
    :initarg :events-enabled :accessor terminal-events-enabled
    :initform nil :type list
    :documentation "List of events enabled."))
  (:default-initargs
    :file-descriptor		nil
    :device-name		*default-console-device-name*
    :output-stream		nil
  )
  (:documentation "What we need to know about terminal device."))

(defclass terminal-wrapper ()
  ((wrapped-terminal
    :initarg :wrapped-terminal :accessor terminal-wrapped-terminal
    :documentation
    "The terminal we are wrapping and we usually do I/O through."))
  (:documentation
   "A terminal that uses another terminal inside of it, but isn't a subclass."))

(defun has-terminal-attributes (stream)
  "Return true if we should treat `STREAM` as if it has terminal attributes."
  (or
   ;; It's a straight up terminal.
   (typep stream 'terminal-stream)
   ;; It's *standard-output* & we know *standard-output-has-terminal-attributes*
   (and (eq stream *standard-output*)
	*standard-output-has-terminal-attributes*)
   ;; Or the file handle can be determined to be a terminal by the OS.
   (let ((ss (stream-system-handle stream)))
     (and ss (file-handle-terminal-p ss)))))

;; Unfortunately interactive-stream-p isn't adequate.
(defun likely-a-terminal-p (stream)
  "Return true if we guess that `STREAM` is a terminal."
  (or
   ;; It's a straight up terminal.
   (typep stream 'terminal-stream)
   ;; Or the file handle can be determined to be a terminal by the OS.
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
   "Set up the terminal for reading a character at a time without echoing.
Return state object that should be passed to terminal-end or terminal-done."))

(defgeneric terminal-end (terminal &optional state)
  (:documentation
   "Put the terminal back to the way it was before we called terminal-start.
STATE should be an object returned by TERMINAL-START."))

(defgeneric terminal-done (terminal &optional state)
  (:documentation "Forget about the whole terminal thing and stuff.
STATE should be an object returned by TERMINAL-START."))

(defgeneric terminal-reinitialize (terminal)
  (:documentation
   "Do anything necessary to make an already started terminal ready for
fresh use. Return any saved state to be restored and passed to terminal-end
or terminal-done."))

;; (defmacro with-terminal-stream ((var stream) &body body)
;;   "Evaluate the body with VAR set to a new terminal-stream."
;;   `(let ((,var (make-instance 'terminal-stream :output-stream ,stream)))
;;      (unwind-protect
;; 	  (progn
;; 	    ,@body)
;;        (terminal-done ,var))))

(defun make-terminal-stream (stream type &rest args)
  (apply #'make-instance type :output-stream stream args))

;; @@@ Could this be simplified?
(defmacro %with-terminal ((&optional type
				     (var '*terminal*)
				     (new-p nil)
				     &rest initargs)
			  &body body)
  "Evaluate the body with VAR possibly set to a new terminal depending on NEW-P.
Cleans up afterward."
  (with-names (result make-it term-class new-type terminal-state)
    `(progn
       (let* ((,new-type (or ,type *default-terminal-type*
			     (pick-a-terminal-type)))
	      (,term-class
	       (if ,new-p
		   ;; If we're making a new one, use a given type or
		   ;; get the type from the existing *terminal*.
		   (or
		    (if ,new-type
			(find-terminal-class-for-type ,new-type)
			(and ,var (typep ,var 'terminal:terminal)
			     (class-of ,var)))
		    (error
		     "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
		   ;; If we're not making a new one, use the given type
		   ;; or and existing variable, or lastly, the default.
		   (or (and ,type
			    (find-terminal-class-for-type ,type))
		       (and ,var (typep ,var 'terminal:terminal)
			    (class-of ,var))
		       (and *default-terminal-type*
			    (find-terminal-class-for-type ,type))
		       (error
			"Provide a type or set *DEFAULT-TERMINAL-TYPE*."))))
	      (,make-it (or ,new-p
			    (or (not ,var)
				(and ,var (not (typep ,var ,term-class))))))

	      (,var (if ,make-it
			(make-instance ,term-class ,@initargs)
			,var))
	      ;; (*standard-output* ,var)
	      ;; (*standard-input* ,var)
	      ,result
	      ,terminal-state
	      )
	 ;; This ignorable is just to stop CCL from complaining.
	 (declare (ignorable ,new-type))
	 ;;(dbugf :terminal "term-class = ~s~%" ,term-class)
	 ;; (when (not ,term-class)
	 ;;   (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
	 ;; Make a new terminal if the we were told to or the var isn't
	 ;; set or isn't of the correct type.
	 ;; (setf ,make-it (or ,new-p
	 ;; 		    (or (not ,var)
	 ;; 			(and ,var (not (typep ,var ,term-class)))))
	 ;;       ,var (if ,make-it
	 ;; 		(make-instance ,term-class ,@initargs)
	 ;; 		,var))
	 ;; (dbugf :terminal "make-it = ~s~%" ,make-it)
	 ;; (dbugf :terminal "~s = ~s~%" ',var ,var)
	 (unwind-protect
	      (progn
		(setf ,terminal-state (terminal-start ,var))
		;;(dbugf :terminal "start terminal-state = ~s~%" ,terminal-state)
		(setf ,result (progn ,@body)))
	   (if ,make-it
	       (terminal-done ,var ,terminal-state)
	       (progn
		 ;;(dbugf :terminal "end terminal-state = ~s~%" ,terminal-state)
		 (terminal-end ,var ,terminal-state))))
	 ,result))))

(defmacro with-terminal ((&optional type
				    (var '*terminal*)
				    &rest initargs)
			 &body body)
  "Evaluate the body with VAR set to a terminal indicated by TYPE. If VAR is
already a terminal of that type, use it. TYPE should be one of the types
registerd in *TERMINAL-TYPES*. Initialized the terminal and cleans up
afterward."
  `(%with-terminal (,type ,var nil ,@initargs) ,@body))

(defmacro with-new-terminal ((&optional type
					(var '*terminal*)
					&rest initargs)
			     &body body)
  "Evaluate the body with VAR set to a new terminal indicated by TYPE.
TYPE should be one of the types registerd in *TERMINAL-TYPES*. Cleans up
afterward."
  `(%with-terminal (,type ,var t ,@initargs) ,@body))

(defmacro deftt (name (&rest args) doc-string)
  "Defines a terminal generic function along with a macro that calls that
generic function with *TERMINAL* as it's first arg, for API prettyness."
  (let ((tt-name (symbolify (s+ "TT-" name)))
	(tt-generic (symbolify (s+ "TERMINAL-" name)))
	(whole-arg (gensym "DEFTT-WHOLE-ARG"))
	;;(ignorables (remove-if (_ (char= #\& (char (string _) 0))) args)))
	(ignorables (lambda-list-vars args :all-p t)))
    `(progn
       (defgeneric ,tt-generic (tt ,@args) (:documentation ,doc-string))
       (defmacro ,tt-name (&whole ,whole-arg ,@args)
	 (declare (ignorable ,@ignorables))
	 ,doc-string
	 (append (list ',tt-generic '*terminal*) (cdr ,whole-arg))))))

(deftt format (fmt &rest args)
  "Output a formatted string to the terminal.")

(deftt write-string (str &key start end)
  "Output a string to the terminal. Flush output if it contains a newline,
i.e. the terminal is \"line buffered\"")

(deftt write-line (str &key start end)
  "Output a string to the terminal, followed by a newline.")

(deftt write-char (char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is \"line buffered\"")

(deftt newline ()
  "Output a newline character. In other words, move to the cursor to the first
column of the next row, scrolling if at the bottom line and it's necessary and
allowed.")

(deftt fresh-line ()
  "Call newline if we're not at the first column. If for some reason
this cannot be determined, then a newline is output anyway. fresh-line
returns true if it outputs a newline; otherwise it returns false.")

(deftt move-to (row column) "Move the cursor to ROW and COLUMN.")

(defun tt-format-at (row column fmt &rest args)
  (tt-move-to row column)
  (apply #'terminal-format *terminal* fmt args))

(defun tt-write-string-at (row column str &key start end)
  (tt-move-to row column)
  (tt-write-string str :start start :end end))

(defun tt-write-line-at (row column str &key start end)
  (tt-move-to row column)
  (tt-write-line str :start start :end end))

(defun tt-write-char-at (row column char)
  (tt-move-to row column)
  (tt-write-char char))

;; @@@ I'm unsure if this is good idea. But it's very convenient.
(defun terminal-write-span (terminal span)
  "Output SPAN to TERMINAL. A span is attributed text as a list, as is 
handled by fatchar:span-to-fat-string."
  (terminal-write-string terminal (fatchar:span-to-fat-string span)))

(defmacro tt-write-span (span)
  "Output SPAN to the terminal. A span is attributed text as a list, as is
handled by fatchar:span-to-fat-string."
  `(terminal-write-span *terminal* ,span))

(defun tt-write-span-at (row column span)
  (tt-move-to row column)
  (tt-write-span span))

(deftt move-to-col (column) "Move the cursor to COLUMN.")
(deftt beginning-of-line () "Move the cursor to the beginning of the line.")
(deftt delete-char (n) "Delete N characters in front of the cursor.")
(deftt insert-char (n) "Insert N blank characters in front of the cursor.")
(deftt backward (&optional n) "Move the cursor backward N characters.")
(deftt forward (&optional n) "Move the cursor forward N character.")
(deftt up (&optional n) "Move the cursor up N rows.")
(deftt down (&optional n) "Move the cursor down N rows.")
(deftt scroll-down (n)
  "Move the cursor down N rows. If at the bottom of the screen or scrolling
region, scroll the screen down.")
(deftt scroll-up (n)
  "Move the cursor up N rows. If at the top of the screen or scrolling region,
scroll the screen up.")
(deftt erase-to-eol () "Erase from the cursor to the end of the current row.")
(deftt erase-line () "Erase the whole row the cursor is on.")
(deftt erase-above () "Erase everything above the row the cursor is on.")
(deftt erase-below () "Erase everything below the row the cursor is on.")
(deftt clear () "Erase the entire screen.")
(deftt home () "Move the cursor to the first row and column.")
(deftt cursor-off () "Try to make the cursor invisible.")
(deftt cursor-on () "Try to make the cursor visible.")
(deftt standout (state)
  "Turn the standout attribute on or off, depending on the boolean STATE.")
(deftt normal () "Reset the text attributes to normal.")
(deftt underline (state)
  "Turn the underline attribute on or off, depending on the boolean STATE.")
(deftt bold (state)
  "Turn the underline attribute on or off, depending on the boolean STATE.")
(deftt inverse (state)
  "Turn the inverse attribute on or off, depending on the boolean STATE.")
(deftt color (fg bg)
  "Set the text foreground color to FG, and the background attribute to BG.")
(deftt colors ()
  "Return the number of colors the terminal supports. Return 0 if it doesn't
support color, or NIL if it can't be determined.")
(deftt window-foreground ()
  "Return the window foreground color, which is probably also the default text foreground color.")
(defgeneric (setf terminal-window-foreground) (color tt)
  (:documentation
   "Set the window foreground color, which is probably also the default text foreground color."))
(deftt window-background ()
  "Return the window bacground color.")
(defgeneric (setf terminal-window-background) (color tt)
  (:documentation
   "Set the window background color."))
(deftt beep ()
  "Make the terminal emit a sound, or perhaps flash the screen.")
(deftt set-attributes (attributes)
  "Set the attributes given in the list. If NIL turn off all attributes.
Attributes are usually keywords.")
(deftt set-scrolling-region (start end)
  "Set the scrolling region starting at row START and ending at END.")
(deftt finish-output ()
  "Attempts to ensure that any buffered output is sent to the terminal.")
; (deftt get-row () "")

(deftt get-char () "Read a character from the terminal.")
(deftt get-key () "Read a key from the terminal.")

(deftt listen-for (seconds)
  "Listen for at most N seconds or until input is available. SECONDS can be
fractional, down to some limit. Returns true if we got an event before SECONDS
elapsed, false otherwise.")

(deftt input-mode ()
  "Accessor for the input mode. Modes are :LINE for line at time with echo
or :CHAR for character at time with no echo.")

(defgeneric (setf terminal-input-mode) (mode tt)
  (:documentation
   "Set the input mode to MODE. Modes are :LINE for line at time with echo
or :CHAR for character at time with no echo."))

(deftt reset ()
  "Try to reset the terminal to a sane state, without being too disruptive.")

(deftt save-cursor ()  "Save the cursor position.")
(deftt restore-cursor ()
  "Restore the cursor position, from the last saved postion.")

(deftt alternate-characters (state)
  "Turn the alternate character translation on or off, depending on the boolean STATE.")

;; We don't use DEFTT for these since they don't exactly mirror the generics.
(defmacro tt-width ()
  "Return the width of the terminal window in characters."
  '(terminal-window-columns *terminal*))

(defmacro tt-height ()
  "Return the height of the terminal window in characters."
  '(terminal-window-rows *terminal*))

(deftt title ()
  "Accessor for the terminal title, if it has one.")
(defgeneric (setf terminal-title) (title tt)
  (:documentation
   "Set the terminal title to TITLE, if it has a title."))

(deftt has-attribute (attribute)
  "Return true if the terminal can display the character attribute.")

(deftt has-autowrap-delay ()
  "Return true if the terminal delays automatic wrapping at the end of a line.")

(defclass tt-event ()
  ((terminal
   :initarg :terminal :accessor tt-event-terminal
   :documentation "NURBS"))
  (:documentation
   "Just in case you were missing having a whole nother event class hierarchy."))

(defclass tt-mouse-event (tt-event)
  ((x
    :initarg :x :accessor tt-mouse-event-x
    :documentation "Horizontal coordinate.")
   (y
    :initarg :y :accessor tt-mouse-event-y
    :documentation "Vertical coordinate"))
  (:documentation "Somthing happened to the mouse."))

(defclass tt-mouse-button-event (tt-mouse-event)
  ((button
   :initarg :button :accessor tt-mouse-button
   :documentation "The button that was changed.")
   (modifiers
    :initarg :modifiers :accessor tt-mouse-modifiers
    :initform nil :type list
    :documentation "List of modifiers that were pressed at the time."))
  (:documentation "A mouse button changed."))

(defmethod print-object ((object tt-mouse-button-event) stream)
  "Print a tt-mouse-button-event to STREAM."
  (with-slots (x y button modifiers) object
    (print-unreadable-object (object stream :type t)
      (format stream "~s ~s ~s~@[ ~s~]" x y button modifiers))))

;; I apoligize to those who hold consistency foremost.

(defclass tt-mouse-button-press (tt-mouse-button-event)
  ()
  (:documentation "A mouse button was pressed."))

(defclass tt-mouse-button-release (tt-mouse-button-event)
  ()
  (:documentation "A mouse button was released."))

(defclass tt-mouse-button-motion (tt-mouse-button-event)
  ()
  (:documentation "The mouse position changed while a button was pressed."))

(defclass tt-mouse-motion (tt-mouse-event)
  ()
  (:documentation "The mouse position changed."))

(defclass tt-resize-event (tt-event)
  () ;; I'm sorry, but you'll have to get the size separately.
  (:documentation "The window changed size."))

(deftt enable-events (events)
  "Allow tt-get-* to return non-key events. EVENTS is a keyword or list of
keywords specifiying events to alllow. Events supported by different terminal
types vary, but you specify :ALL or :NONE for any terminal. If a terminal
doesn't support an event type, it silently ignores it, but should return NIL
when given that event alone. The starting state is to allow no events.")

(deftt disable-events (events)
  "The opposite of tt-enable-events. Returns true if the events were enabled.")

;; Terminals can either provide their own version of terminal-enable-events,
;; or just provide a terminal-enable-event which returns true to add the event
;; to the enabled events list.

(defmethod terminal-enable-events (terminal events)
  "Enable the EVENTS and return true if the terminal supports the events."
  (with-slots (events-enabled) terminal
    (let (result)
      (loop :for e :in (if (atom events) (list events) events)
	 :do
	 (case e
	   (:all    (setf result t events-enabled '(:all)))
	   (:none   (setf result t events-enabled nil))
	   (otherwise
	    (when (terminal-enable-event terminal e)
	      (setf result t)))))
      result)))

(defmethod terminal-disable-events (terminal events)
  "Disable the EVENTS and return true if the terminal supports the events."
  (with-slots (events-enabled) terminal
    (let (result)
      (loop :for e :in (if (atom events) (list events) events)
	 :do
	 (case e
	   (:all    (setf result t events-enabled nil))
	   (:none   (setf result t events-enabled '(:all)))
	   (otherwise
	    (when (terminal-disable-event terminal e)
	      (setf result t)))))
      result)))

(defgeneric terminal-enable-event (tty event)
  (:documentation
   "Enable EVENT and return true if the terminal can enable it.")
  ;; A default method which doesn't handle any special events.
  (:method (tty event)
    (declare (ignore tty event))
    nil))

(defgeneric terminal-disable-event (tty event)
  (:documentation
   "Disable EVENT and return true if it was enabled.")
  ;; A default method which doesn't handle any special events.
  (:method (tty event)
    (declare (ignore tty event))
    nil))

(defmacro with-saved-cursor ((tty) &body body)
  "Save the cursor position, evaluate the body forms, and restore the cursor
position. Return the primary result of evaluating the body."
  (let ((result-sym (gensym "tt-result")))
    `(progn
       (terminal-save-cursor ,tty)
       (let (,result-sym)
	 (unwind-protect
	      (setf ,result-sym (progn ,@body))
	   (terminal-restore-cursor ,tty))
	 ,result-sym))))

(defmacro with-terminal-output-to-string
    ((&optional (type :ansi-stream)) &body body)
  "Evaluate the body with *TERMINAL* bound to a terminal-stream which outputs to
a string and return the string."
  (with-names (stream terminal-state result)
    `(with-output-to-string (,stream)
       (when (not (find-terminal-class-for-type ,type))
	 (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
       (let* ((*terminal* (make-terminal-stream
			  ,stream
			  (find-terminal-class-for-type ,type)
			  :window-rows (if *terminal*
					   (terminal-window-rows *terminal*)
					   24)
			  :window-columns (if *terminal*
					      (terminal-window-columns
					       *terminal*)
					      80)))
	      (*standard-output* *terminal*) ;; mostly for grout?
	      ,terminal-state ,result)
	 (unwind-protect
	      (progn
		(setf ,terminal-state (terminal-start *terminal*))
		(setf ,result (progn ,@body)))
	   (terminal-done *terminal* ,terminal-state))
	 ,result))))

;; @@@ There is overlap here between this and fatchar:span-to-fatchar-string.
;; fatchar depends on us, so maybe consider moving part of that code here?

(defmacro with-style ((&rest style) &body body)
  "Evaluate the BODY with the style set to SYTLE. Unfortunately we can't set
the color back to what it was, since terminals necessarily support querying
the current color, so the caller will have to do that itself."
  (with-names (fg bg color-set s)
    `(let (,fg ,bg ,color-set)
       (loop :for ,s :in (flatten ',style) :do
	  (case ,s
	    (:normal    (tt-normal))
	    (:standout  (tt-standout t))
	    (:underline (tt-underline t))
	    (:bold      (tt-bold t))
	    (:inverse   (tt-inverse t))
	    (otherwise
	     (cond
	       ((equalp (subseq (string ,s) 0 3) "FG-")
		(setf ,fg (keywordify (subseq (string ,s) 3))
		      ,color-set t))
	       ((equalp (subseq (string ,s) 0 3) "BG-")
		(setf ,bg (keywordify (subseq (string ,s) 3))
		      ,color-set t))
	       ((member ,s *standard-colors*)
		;; An un-prefixed color is a foreground color.
		(setf ,fg ,s
		      ,color-set t))))))
       (when ,color-set
	 (tt-color ,fg ,bg))
       ,@body)))

(defmacro with-alternate-characters (() &body body)
  "Hint to the terminal that we want to output 'alternate' characters. If the
terminal has this ability, or rather perhaps the disability of not being
otherwise able to display such characters, it will translate certain unicode
characters, such as line drawing characters, into some antique representation."
  `(unwind-protect
	(progn
	  (tt-alternate-characters t)
	  ,@body)
     (tt-alternate-characters nil)))

(defmacro with-immediate ((&optional tty) &body body)
  "Evaluate BODY with the terminal input mode set to :CHAR, and restore it
afterwards."
  (with-names (mode ztty)
    `(let* ((,ztty (or ,tty *terminal*))
	    (,mode (terminal-input-mode ,ztty)))
       (unwind-protect
	    (progn
	      (when (not (eq ,mode :char))
		(setf (terminal-input-mode ,ztty) :char))
	      ,@body)
	 (when (not (eq ,mode :char))
	   (setf (terminal-input-mode ,ztty) ,mode))))))

;; EOF
