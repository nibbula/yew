;;
;; editor.lisp
;;

(in-package :rl)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defvar *lisp-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\" #\' #\\ #\# #\, #\` #\| #\.)
  "Characters that are not considered to be part of a word in lisp.")
;; removed #\/ since it's common in package names

(defvar *default-non-word-chars*
  (concatenate 'vector *lisp-non-word-chars* #(#\- #\/))
  "Characters that are not considered to be part of a word by default.")

(defvar *default-prompt* "> "
  "Output before reading to let you know it's your turn.")

(defun default-output-prompt (e &optional (p nil prompt-supplied))
  "The default prompt output function. Prints *default-prompt* unless a ~
   prompt is supplied."
  ;; (let ((str (princ-to-string (if prompt-supplied p *default-prompt*))))
  ;;   (editor-write-string e str)
  ;;   str))
  (declare (ignore e))
  (let ((s (if prompt-supplied p *default-prompt*)))
    (typecase p
      ((or string fatchar-string fat-string)
       s)
      (t (princ-to-string s)))))

(defparameter *normal-keymap* nil
  "The normal key for use in the line editor.")

(defvar *line-editor* nil
  "The last line editor that was instantiated. This is for debugging, since
it can be somewhat unpredictable, especially with threads. Don't use it for
anything important.")

;; The history is not in here because it is shared by all editors.
(defclass line-editor (terminal-inator)
  ((last-event
    :accessor last-event
    :initform nil
    :initarg :last-event
    :documentation "Last input event.")
   (buf
    :accessor buf
    :initform nil
    :initarg :buf
    :documentation "Current line buffer.")
   (screen-row
    :accessor screen-row
    :initform 0
    :documentation "Screen row of the cursor.")
   (screen-col
    :accessor screen-col
    :initform 0
    :documentation "Screen column of the cursor.")
   (start-col
    :accessor start-col
    :initform 0
    :documentation "Starting column of the input area after the prompt.")
   (start-row
    :accessor start-row
    :initform 0
    :documentation "Starting row of the input area after the prompt.")
   (clipboard
    :accessor clipboard
    :initform nil
    :documentation "A string to copy and paste with.")
   (mark
    :accessor mark
    :initform nil
    :documentation "A reference position in the buffer.")
   (context
    :accessor context
    :initarg :context
    :initform :tiny
    :documentation "A symbol selecting what line history to use.")
   (saved-line
    :accessor saved-line
    :initarg :saved-line
    :initform nil
    :documentation "Current line, saved when navigating history.")
   (undo-history
    :accessor undo-history
    :initform nil
    :initarg :undo-history
    :documentation "Record of undo-able edits.")
   (undo-current
    :accessor undo-current
    :initform nil
    :initarg :undo-current
    :documentation "Spot in undo history where we are currently undoing from.")
   (record-undo-p
    :accessor record-undo-p
    :initform t
    :initarg :record-undo-p
    :documentation "True to enable undo recording.")
   (exit-flag
    :accessor exit-flag
    :initform nil
    :initarg :exit-flag
    :documentation "True if the user requested to stop editing.")
   (non-word-chars
    :accessor non-word-chars
    :initarg :non-word-chars
    :documentation "Characters that are not considered part of a word.")
   (prompt-string
    :accessor prompt-string
    :initarg :prompt-string
    :documentation "String to print before reading user input.")
   (prompt-func
    :accessor prompt-func
    :initarg :prompt-func
    :initform nil
    :documentation "Function to call to output the prompt.")
   (prompt-height
    :accessor prompt-height
    :initarg :prompt-height
    :initform nil
    :documentation "Height of the prompt in lines.")
   (completion-func
    :accessor completion-func
    :initarg :completion-func
    :documentation "Function to call to generate completions.")
   (terminal
    :accessor line-editor-terminal
    :initarg :terminal
    :documentation "The terminal device we are using.")
   (terminal-device-name
    :accessor line-editor-terminal-device-name
    :initarg :terminal-device-name
    :documentation "The name of the terminal device.")
   (terminal-class
    :accessor line-editor-terminal-class
    :initarg :terminal-class
    :documentation "The class of terminal we are using.")
   (did-complete
    :initarg :did-complete
    :accessor did-complete
    :initform nil :type boolean
    :documentation "True if we called complete.")
   (did-under-complete
    :initarg :did-under-complete
    :accessor did-under-complete
    :initform nil :type boolean
    :documentation "True if we did any under style completion.")
   (last-command-was-completion
    :initarg :last-command-was-completion
    :accessor last-command-was-completion
    :initform nil
    :type boolean
    :documentation "True if the last command was a completion.")
   (last-completion-not-unique-count
    :accessor last-completion-not-unique-count
    :initarg :last-completion-not-unique-count
    :initform 0
    :type fixnum
    :documentation "How many times the last completion and was not unique.")
   (need-to-redraw
    :accessor need-to-redraw
    :initarg :need-to-redraw
    :initform nil
    :documentation "True if we need to redraw the whole line.")
   (temporary-message
    :initarg :temporary-message :accessor temporary-message
    :initform nil :type (or null fixnum)
    :documentation
    "Lines of temporary message we need to clear, or NIL if none.")
   (input-callback
    :accessor line-editor-input-callback
    :initarg :input-callback
    :initform nil
    :documentation "Function to call on character input.")
   (output-callback
    :accessor line-editor-output-callback
    :initarg :output-callback
    :initform nil
    :documentation "Function to call on output.")
   (debugging
    :accessor debugging
    :initarg :debugging
    :initform nil
    :documentation "True to turn on debugging features.")
   (debug-log
    :accessor line-editor-debug-log
    :initarg :debug-log
    :initform nil
    :documentation "A list of messages logged for debugging.")
   (local-keymap
    :accessor line-editor-local-keymap
    :initarg :local-keymap
    :documentation "The local keymap.")
   (accept-does-newline
    :accessor accept-does-newline
    :initarg :accept-does-newline
    :initform t :type boolean
    :documentation "True if accept-line outputs a newline.")
   )
  (:default-initargs
    :non-word-chars *default-non-word-chars*
    :prompt-string *default-prompt*
    ;;:terminal-class 'terminal-ansi
    :terminal-class (or (and *terminal* (class-of *terminal*))
			;;'terminal-ansi
			(find-terminal-class-for-type *default-terminal-type*)
			)
  )
  (:documentation "State for a stupid little line editor."))

(defvar *initial-line-size* 20)

(defmethod initialize-instance :after ((e line-editor) &rest initargs)
  (declare (ignore initargs))

  ;; Make a terminal using the device name and class, or use *TERMINAL*.
  (setf (slot-value e 'terminal)
	(if (and (slot-boundp e 'terminal-device-name)
		 (slot-value e 'terminal-device-name))
	    (make-instance (slot-value e 'terminal-class)
			   :device-name (line-editor-terminal-device-name e)
			   :start-at-current-line t)
	    (or (progn
		  (when *terminal*
		    (dbug "Using *TERMINAL* ~a~%" (type-of *terminal*)))
		  *terminal*)
		(make-instance (slot-value e 'terminal-class)
			       :start-at-current-line t))))

  ;; If the local keymap wasn't given, make an empty one.
  (unless (and (slot-boundp e 'local-keymap) (slot-value e 'local-keymap))
    (setf (slot-value e 'local-keymap)
	  (make-instance 'keymap)))

  ;; Unless keymap was given, set the it to use the normal keymaps and
  ;; the local keymap.
  (unless (and (slot-boundp e 'keymap) (slot-value e 'keymap)
	       (not (eq (slot-value e 'keymap) *default-inator-keymap*)))
    (setf (slot-value e 'keymap)
	  `(,(slot-value e 'local-keymap) ,*normal-keymap*
	     ,*default-inator-keymap*)))

  ;; Make a default line sized buffer if one wasn't given.
  (when (or (not (slot-boundp e 'buf)) (not (slot-value e 'buf)))
    (setf (slot-value e 'buf)
	  (make-stretchy-vector *initial-line-size* :element-type 'fatchar)))

  ;; Set the current dynamic var.
  (setf *line-editor* e))

(defgeneric freshen (e)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

(defmethod freshen ((e line-editor))
  "Make the editor ready to read a fresh line."
  (setf (inator-command e)	nil
	(inator-last-command e) nil
	(last-event e)          nil
	(inator-point e)	0
	(inator-quit-flag e)	nil
	(fill-pointer (buf e))	0
;;;	(screen-row e) (terminal-get-cursor-position (line-editor-terminal e))
	(screen-row e)		0
	(screen-col e)		0
	(start-col e)		0
	(start-row e)		0
	(undo-history e)	nil
	(undo-current e)	nil
	(need-to-redraw e)	nil
	(exit-flag e)		nil
	(did-under-complete e)	nil))

;; For use in external commands.

(defun get-buffer-string (e)
  "Return a string of the buffer."
  (buffer-string (buf e)))

;; @@@ compatibility
(defalias 'point 'inator-point)
(defalias 'line-editor-keymap 'inator-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input

(defun get-a-char (e)
  "Read a character from the editor's terminal."
  (declare (type line-editor e))
  ;; (tt-finish-output)
  (let ((c (tt-get-key)))
    (when (line-editor-input-callback e)
      (funcall (line-editor-input-callback e) c))
    c))

(defmethod await-input ((e line-editor))
  (setf (last-event e) (get-a-char e)))

;; @@@ What was the idea?
;; (defvar *key-tree* '())
;;   "")
;; (defun record-key (key)
;;   )

;; EOF
