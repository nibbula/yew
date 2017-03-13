;;
;; terminal.lisp - The end of the line.
;;

;; TODO:
;;  - Why doesn't terminal-get-size set the size slots?!?

(defpackage :terminal
  (:documentation "The end of the line.")
  (:use :cl :dlib :opsys)
  (:export
   #:*standard-output-has-terminal-attributes*
   #:*terminal*
   #:*default-terminal-type*
   #:*terminal-types*
   #:has-terminal-attributes
   #:terminal-default-device-name
   #:register-terminal-type
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
   #:with-terminal
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
   #:tt-reset			  #:terminal-reset
   #:tt-save-cursor		  #:terminal-save-cursor		;
   #:tt-restore-cursor		  #:terminal-restore-cursor		;
   #:with-saved-cursor
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

(defun register-terminal-type (name type)
  "Subclasses should call this to register their type keyword."
  (pushnew (list name type) *terminal-types*))

(defun find-type (type)
  "Return the class for a registered terminal type."
  (cadr (assoc type *terminal-types*)))

(defclass terminal-stream ()
  ((output-stream
    :accessor terminal-output-stream
    :initarg :output-stream
    :documentation "Lisp stream for output."))
  (:documentation
   "Terminal as purely a Lisp output stream. This can't do input or things that
require terminal driver support."))

(defclass terminal (terminal-stream)
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
  (or (and (eq stream *standard-output*)
	   *standard-output-has-terminal-attributes*)
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

(defmacro with-terminal ((&optional (type *default-terminal-type*)
				    (var '*terminal*)
				    device-name)
			 &body body)
  "Evaluate the body with VAR set to a new terminal. Cleans up afterward."
  (with-unique-names (result)
    `(progn
       (when (not (find-type ,type))
	 (error "Provide a type or set *DEFAULT-TERMINAL-TYPE*."))
       (let ((,var (if ,device-name
		       (make-instance (find-type ,type)
				      :device-name ,device-name)
		       (make-instance (find-type ,type))))
	     ,result)
	 (unwind-protect
	      (progn
		(terminal-start ,var)
		(setf ,result ,@body))
	   (terminal-done ,var))
	 ,result))))

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

(deftt write-string (str)
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
(deftt standout (state) "")
(deftt normal () "")
(deftt underline (state) "")
(deftt bold (state) "")
(deftt inverse (state) "")
(deftt color (fg bg) "")
(deftt beep () "")
(deftt set-scrolling-region (start end) "")
(deftt finish-output () "")
; (deftt get-row () "")

(deftt get-char ()
  "Read a character from the terminal.")

(deftt get-key ()
  "Read a key from the terminal.")

(deftt listen-for (seconds)
  "Listen for at most N seconds or until input is available. SECONDS can be
fractional, down to some limit.")

(deftt reset ()
  "Try to reset the terminal to a sane state, without being too disruptive.")

(deftt save-cursor ()
  "Save the cursor position.")

(deftt restore-cursor ()
  "Restore the cursor position, from the last saved postion.")

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
