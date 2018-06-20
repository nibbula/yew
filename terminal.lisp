;;
;; terminal.lisp - The end of the line.
;;

;; TODO:
;;  - Why doesn't terminal-get-size set the size slots?!?

(defpackage :terminal
  (:documentation "The end of the line.")
  (:use :cl :dlib :opsys :trivial-gray-streams)
  (:export
   #:*standard-output-has-terminal-attributes*
   #:*terminal*
   #:*default-terminal-type*
   #:*terminal-types*
   #:*standard-colors*
   #:has-terminal-attributes
   #:terminal-default-device-name
   #:register-terminal-type
   #:find-terminal-class-for-type
   #:pick-a-terminal-type
   #:terminal-stream
   #:terminal
   #:terminal-file-descriptor ;; #:file-descriptor
   #:terminal-device-name     ;; #:device-name
   #:terminal-output-stream   ;; #:output-stream
   #:terminal-window-rows     ;; #:window-rows
   #:terminal-window-columns  ;; #:window-columns
   #:terminal-get-size
   #:terminal-get-cursor-position
   #:terminal-start
   #:terminal-end
   #:terminal-done
   #:make-terminal-stream
   #:with-terminal #:with-new-terminal
   #:tt-format			  #:terminal-format			;
   #:tt-write-string		  #:terminal-write-string		;
   #:tt-write-char		  #:terminal-write-char			;
   #:tt-move-to			  #:terminal-move-to			;
   #:tt-move-to-col		  #:terminal-move-to-col		;
   #:tt-beginning-of-line	  #:terminal-beginning-of-line		;
   #:tt-del-char		  #:terminal-del-char			;
   #:tt-ins-char		  #:terminal-ins-char			;
   #:tt-backward		  #:terminal-backward
   #:tt-forward			  #:terminal-forward
   #:tt-up			  #:terminal-up
   #:tt-down			  #:terminal-down
   #:tt-scroll-down		  #:terminal-scroll-down
   #:tt-erase-to-eol		  #:terminal-erase-to-eol		;
   #:tt-erase-line		  #:terminal-erase-line			;
   #:tt-erase-above		  #:terminal-erase-above		;
   #:tt-erase-below		  #:terminal-erase-below		;
   #:tt-clear			  #:terminal-clear			;
   #:tt-home			  #:terminal-home			;
   #:tt-cursor-off		  #:terminal-cursor-off			;
   #:tt-cursor-on		  #:terminal-cursor-on			;
   #:tt-standout		  #:terminal-standout			;
   #:tt-normal			  #:terminal-normal			;
   #:tt-underline		  #:terminal-underline			;
   #:tt-bold			  #:terminal-bold			;
   #:tt-inverse			  #:terminal-inverse			;
   #:tt-color			  #:terminal-color			;
   #:tt-beep			  #:terminal-beep			;
   #:tt-set-scrolling-region	  #:terminal-set-scrolling-region	;
   #:tt-finish-output		  #:terminal-finish-output		;
   #:tt-get-char		  #:terminal-get-char
   #:tt-get-key			  #:terminal-get-key
   #:tt-listen-for		  #:terminal-listen-for
   #:tt-input-mode                #:terminal-input-mode
   #:tt-reset			  #:terminal-reset
   #:tt-save-cursor		  #:terminal-save-cursor		;
   #:tt-restore-cursor		  #:terminal-restore-cursor		;
   #:tt-width
   #:tt-height
   #:tt-title                     #:terminal-title
   #:tt-has-attribute             #:terminal-has-attribute
   #:with-saved-cursor
   #:with-terminal-output-to-string
   #:with-style
   ))
(in-package :terminal)

(declaim (optimize (debug 3)))

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

(defparameter *standard-colors*
  '(:black :red :green :yellow :blue :magenta :cyan :white :default)
  "Standard color names.")

(defun register-terminal-type (name type)
  "Subclasses should call this to register their type keyword."
  ;;(pushnew (list name type) *terminal-types*)
  (setf (getf *terminal-types* name) type))

(defun find-terminal-class-for-type (name)
  "Return the class for a registered terminal type."
  ;;(cadr (assoc type *terminal-types*))
  (getf *terminal-types* name))

(defun terminal-type-based-on-environemt ()
  (cond
    ((equal (environment-variable "TERM") "dumb")
     :dumb)
    (t :ansi)))

(defun pick-a-terminal-type ()
  "Pick some terminal type. Hopefully appropriate, but perhaps semi-arbitrary."
  (let ((platform-default
	 #+(and windows unix) (terminal-type-based-on-environemt)
	 #+windows (if (environment-variable "TERM")
		       (terminal-type-based-on-environemt)
		       :ms)
	 #+unix (terminal-type-based-on-environemt)
	 #-(or windows unix) :ansi))
    (or *default-terminal-type*
	(if (find-terminal-class-for-type platform-default)
	    platform-default
	    ;; This picks :ansi-stream if nothing else is loaded.
	    ;;(or (first *terminal-types*) :ansi)
	    :ansi
	    ))))

(defclass terminal-stream (fundamental-character-output-stream)
  ((output-stream
    :accessor terminal-output-stream
    :initarg :output-stream
    :documentation "Lisp stream for output."))
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
    :device-name		*default-console-device-name*
    :output-stream		nil
  )
  (:documentation "What we need to know about terminal device."))

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
   "Set up the terminal for reading a character at a time without echoing."))

(defgeneric terminal-end (terminal)
  (:documentation
   "Put the terminal back to the way it was before we called terminal-start."))

(defgeneric terminal-done (terminal)
  (:documentation "Forget about the whole terminal thing and stuff."))

;; (defmacro with-terminal-stream ((var stream) &body body)
;;   "Evaluate the body with VAR set to a new terminal-stream."
;;   `(let ((,var (make-instance 'terminal-stream :output-stream ,stream)))
;;      (unwind-protect
;; 	  (progn
;; 	    ,@body)
;;        (terminal-done ,var))))

(defun make-terminal-stream (stream type)
  (make-instance type :output-stream stream))

(defmacro %with-terminal ((&optional (type *default-terminal-type*)
				     (var '*terminal*)
				     (new-p nil)
				     &rest initargs)
			  &body body)
  "Evaluate the body with VAR possibly set to a new terminal depending on NEW-P.
Cleans up afterward."
  (with-unique-names (result make-it term-class)
    `(progn
       (when (and ,type (not (find-terminal-class-for-type ,type)))
	 (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
       (let* ((,term-class
	       (or (and ,type (find-terminal-class-for-type ,type))
		   (and ,var (typep ,var 'terminal:terminal)
			(class-of ,var))))
	      (,make-it (not
			 (and ,var (typep ,var ,term-class)
			      (not ,new-p))))
	      (,var (if ,make-it
			(make-instance ,term-class ,@initargs)
			,var))
	      ;; (*standard-output* ,var)
	      ;; (*standard-input* ,var)
	      ,result)
	 (unwind-protect
	      (progn
		(terminal-start ,var)
		(setf ,result (progn ,@body)))
	   (if ,make-it
	       (terminal-done ,var)
	       (terminal-end ,var)))
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

(deftt write-char (char)
  "Output a character to the terminal. Flush output if it is a newline,
i.e. the terminal is \"line buffered\"")

(deftt move-to (row column) "Move the cursor to ROW and COLUMN.")
(deftt move-to-col (column) "Move the cursor to COLUMN.")
(deftt beginning-of-line () "Move the cursor to the beginning of the line.")
(deftt del-char (n) "Delete N characters in front of the cursor.")
(deftt ins-char (n) "Insert N blank characters in front of the cursor.")
(deftt backward (n) "Move the cursor backward N characters.")
(deftt forward (n) "Move the cursor forward N character.")
(deftt up (n) "Move the cursor up N rows.")
(deftt down (n) "Move the cursor down N rows.")
(deftt scroll-down (n)
  "Move the cursor down N rows. If at the bottom of the screen, scroll the
screen down.")
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
(deftt beep ()
  "Make the terminal emit a sound, or perhaps flash the screen.")
(deftt set-scrolling-region (start end)
  "Set the scrolling region starting at row START and ending at END.")
(deftt finish-output ()
  "Attempts to ensure that any buffered output is sent to the terminal.")
; (deftt get-row () "")

(deftt get-char () "Read a character from the terminal.")
(deftt get-key () "Read a key from the terminal.")

(deftt listen-for (seconds)
  "Listen for at most N seconds or until input is available. SECONDS can be
fractional, down to some limit.")

(deftt input-mode ()
  "Accessor for the input mode. Modes are :LINE for line at time with echo
or :CHAR for character at time with no echo.")

(defgeneric (setf terminal-input-mode) (mode tt)
  (:documentation
   "Set the input mode to MODE. Modes are :LINE for line at time with echo
or :CHAR for character at time with no echo."))
;; I think the macro for tt-input-mode will work for setf?

(deftt reset ()
  "Try to reset the terminal to a sane state, without being too disruptive.")

(deftt save-cursor ()  "Save the cursor position.")
(deftt restore-cursor ()
  "Restore the cursor position, from the last saved postion.")

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
    ((&optional (type *default-terminal-type*)) &body body)
  "Evaluate the body with *TERMINAL* bound to a terminal-stream which outputs to
a string and return the string."
  (with-unique-names (stream)
    `(with-output-to-string (,stream)
       (when (not (find-terminal-class-for-type ,type))
	 (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
       (let ((*terminal* (make-terminal-stream
			  ,stream
			  (find-terminal-class-for-type ,type))))
	 (unwind-protect
	      ,@body
	   (tt-finish-output))))))

;; @@@ There is overlap here between this and fatchar:span-to-fatchar-string.
;; fatchar depends on us, so maybe consider moving part of that code here?

(defmacro with-style ((&rest style) &body body)
  "Evaluate the BODY with the style set to SYTLE. Unfortunately we can't set
the color back to what it was, since terminals necessarily support querying
the current color, so the caller will have to do that itself."
  (with-unique-names (fg bg color-set s)
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

#| @@@@ Make an output-table method, with underlined titles

(defun print-col (tt n v &key no-space)
  "Print column number N with value V."
  (let* ((col   (elt *cols* n))
	 (width (second col))
	 (left  (eql (third col) :left))
	 (fmt   (if width (if left "~va" "~v@a") "~a")))
    (if width
	(terminal-format tt fmt width (subseq v 0 (min width (length v))))
	(terminal-format tt fmt v))
    (if (= n (1- (length *cols*)))
	(terminal-write-char tt #\newline)
	(when (not no-space)
	  (terminal-write-char tt #\space)))))

(defun print-title (tt n)
  (terminal-underline tt t)
  (print-col tt n (first (elt *cols* n)) :no-space t)
  (terminal-underline tt nil)
  (when (< n (1- (length *cols*)))
    (terminal-write-char tt #\space)))
  )

(defmethod output-table ((table table) (destination terminal-stream)
			 &key long-titles column-names)
  )
|# 

;; EOF
