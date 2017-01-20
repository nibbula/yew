;;
;; editor.lisp
;;

;; Copyright © 2007-2017 Nibby Nebbulous
;; Licensed under the GPL (See file LICENSE for details).

(in-package :rl)

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
  (princ-to-string (if prompt-supplied p *default-prompt*)))

(defparameter *normal-keymap* nil
  "The normal key for use in the line editor.")

;; The history is not in here because it is shared by all editors.
(defclass line-editor ()
  ((cmd
    :accessor cmd
    :initform nil
    :initarg :cmd
    :documentation "Current command. Usually the input character")
   (last-input
    :accessor last-input
    :initform nil
    :initarg :last-input
    :documentation "Last input command.")
   (buf
    :accessor buf
    :initform nil
    :initarg :buf
    :documentation "Current line buffer.")
   (point
    :accessor point
    :initform 0
    :initarg :point
    :documentation "Cursor position in buf.")
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
   (quit-flag
    :accessor quit-flag
    :initform nil
    :initarg :quit-flag
    :documentation "True to stop editing.")
   (exit-flag
    :accessor exit-flag
    :initform nil
    :initarg :exit-flag
    :documentation "True if the user requested to stop editing.")
   (non-word-chars
    :accessor non-word-chars
    :initarg :non-word-chars
    :documentation "Characters that are not considered part of a word.")
   (prompt
    :accessor prompt
    :initarg :prompt
    :documentation "Prompt string.")
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
   (in-callback
    :accessor line-editor-in-callback
    :initarg :in-callback
    :initform nil
    :documentation "Function to call on character input.")
   (out-callback
    :accessor line-editor-out-callback
    :initarg :out-callback
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
   (keymap
    :accessor line-editor-keymap
    :initarg :keymap
    :documentation "The keymap.")
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
    :prompt *default-prompt*
    :terminal-class 'terminal-ansi
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
			   :device-name (line-editor-terminal-device-name e))
	    (or (progn
		  (when *terminal*
		    (dbug "Using *TERMINAL* ~a" (type-of *terminal*)))
		  *terminal*)
		(make-instance (slot-value e 'terminal-class)))))

  ;; If the local keymap wasn't given, make an empty one.
  (unless (and (slot-boundp e 'local-keymap) (slot-value e 'local-keymap))
    (setf (slot-value e 'local-keymap)
	  (make-instance 'keymap)))

  ;; Unless keymap was given, set the it to use the normal keymap and
  ;; the local keymap.
  (unless (and (slot-boundp e 'keymap) (slot-value e 'keymap))
    (setf (slot-value e 'keymap)
	  `(,(slot-value e 'local-keymap) ,*normal-keymap*)))

  ;; Make a default line sized buffer if one wasn't given.
  (when (or (not (slot-boundp e 'buf)) (not (slot-value e 'buf)))
    (setf (slot-value e 'buf) (make-stretchy-string *initial-line-size*)))

  ;; Set the current dynamic var.
  (setf *line-editor* e))

(defgeneric freshen (e)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

(defmethod freshen ((e line-editor))
  "Make the editor ready to read a fresh line."
  (setf (cmd e)			nil
	(last-input e)		nil
	(point e)		0
	(fill-pointer (buf e))	0
;;;	(screen-row e) (terminal-get-cursor-position (line-editor-terminal e))
	(screen-row e)		0
	(screen-col e)		0
	(start-col e)		0
	(start-row e)		0
	(undo-history e)	nil
	(undo-current e)	nil
	(need-to-redraw e)	nil
	(quit-flag e)		nil
	(exit-flag e)		nil
	(did-under-complete e)	nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input

(defun get-a-char (e)
  "Read a character from the editor's tty."
  (declare (type line-editor e))
  (tt-finish-output)
  (let ((c (tt-get-key)))
    ;; when read returns eagain,
    ;; (terminal-start tty) (redraw e) (tt-finish-output)
    (when (line-editor-in-callback e)
      (funcall (line-editor-in-callback e) c))
    c))

(defun get-lone-key ()
  "Get a key, but easily usable from outside the editor. Don't use this for
anything serious."
  (terminal-start (line-editor-terminal *line-editor*))
  (unwind-protect
    (progn
      (get-a-char *line-editor*))
    (terminal-end (line-editor-terminal *line-editor*))))

;; This can unfortunately really vary between emulations, so we try
;; to code for multiple interpretations.
;; @@@ this or something like it should probably be moved to terminal-ansi
(defun read-function-key (e)
  "Read the part of a function key after the ESC [ and return an
 indicative keyword or nil if we don't recognize the key."
  (declare (type line-editor e))
  (let ((c (get-a-char e)))
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
	 ;; read a number followed by a tilde
	 ((digit-char-p c)
	  (let ((num (parse-integer (string c))))
	    (setf c (get-a-char e))
	    (loop :while (digit-char-p c)
	      :do
	      (setf num (+ (* num 10)
			   (parse-integer (string c))))
;	      (format t "(~a ~c)" num c)
	      (setf c (get-a-char e)))
;	    (message tty (format nil "~a ~c" n c))
;	    (format t "[~d ~c]" num c)
	    (when (eql c #\~)
	      (case num
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
		(t nil)))))
	 (t
	  nil))))))

;; @@@ this or something like it should probably be moved to terminal-ansi
(defun read-app-key (e)
  "Read the part of an application mode function key after the ESC O and
 return an indicative keyword or nil if we don't recognize the key."
  (declare (type line-editor e))
  (let ((c (get-a-char e)))
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
      (t nil))))

#|
;; Perhaps we should consider refactoring some part of get-a-char?
(defun read-utf8-char (e)
  "Read one UTF-8 character from the terminal of the line-editor E and return it."
  (declare (type line-editor e))
  (tt-finish-output)
  (with-foreign-object (c :unsigned-char)
    (let (status (tty (line-editor-terminal e)))
      (loop
	 :do (setf status (posix-read (terminal-file-descriptor tty) c 1))
	 :if (and (< status 0) (or (= *errno* +EINTR+) (= *errno* +EAGAIN+)))
	 :do
	   (terminal-start tty) (redraw e) (tt-finish-output)
	 :else
	   :return
	 :end)
      (cond
	((< status 0)
	 (error "Read error ~d ~d ~a~%" status nos:*errno*
		(nos:strerror nos:*errno*)))
	((= status 0)
	 nil)
	((= status 1)
	 (when (line-editor-in-callback e)
	   (let ((cc (code-char (mem-ref c :unsigned-char))))
	     (funcall (line-editor-in-callback e) cc)
	     cc))
	 (code-char (mem-ref c :unsigned-char)))))))
|#

;; (defvar *key-tree* '())
;;   "")
;; @@@@@@@@@@@@@@@@@@@@@ SUPA
;; (defun record-key (key)
;;   (
;;   )

;; EOF
