;;
;; tiny-rl.lisp - An input line editor.
;;

;; TODO:
;;   - character display bugs
;;    - tab
;;    - newline
;;    - wide characters
;;   - history saving
;;   - search by :UP (or C-P)
;;   - right prompt?
;;   - fix undo boundaries
;;   - change the the name

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defpackage "TINY-RL"
  (:use :cl :dlib :dlib-misc :keymap :char-util :dl-list :stretchy :cffi
	:opsys :terminal :terminal-ansi :terminal-curses :fatchar
	:completion :syntax-lisp :unipose)
  (:documentation
   "A readline replacement for ANSI terminals.")
  (:export
   ;; main functionality
   #:tiny-read-line
   #:tiny-rl
   #:line-editor
   #:line-editor-p
   #:make-line-editor
   #:screen-row
   #:screen-col
   #:line-editor-terminal
   #:line-editor-terminal-device-name
   #:line-editor-terminal-class
   #:line-editor-in-callback
   #:line-editor-out-callback
   #:line-editor-debug-log
   #:line-editor-keymap
   #:line-editor-local-keymap
   #:show-history
   #:history-clear
   #:*line-editor*
   #:*default-prompt*
   #:*completion-list-technique*
   #:*completion-really-limit*
   #:*completion-short-divisor*
   #:*normal-keymap*
   #:*ctlx-keymap*
   #:*escape-keymap*
   #:*terminal-name*
   ;; misc
   #:get-lone-key
   #:read-filename
   #:read-choice
   #:complete-filename-command
  )
)
(in-package "TINY-RL")

(defvar *line-editor* nil
  "The last line editor that was instantiated. This is for debugging, since
it can be somewhat unpredictable, especially with threads. Don't use it for
anything important.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History
;;;
;;; History is a forward and backward navigable list of strings. We maintain
;;; separate history contexts, so that line editor instances can have separate
;;; histories or share the same history.
;;;
;;; *history* is an alist of (CONTEXT . HISTORY), where CONTEXT is a symbol
;;; and HISTORY is a history structure, which contains a dl-list of strings.
;;;
;;; I know it's confusing, but the tail is the oldest entry and the head
;;; is the most recent entry. We push new entries on to the head, but we
;;; print the list backwards from tail to the head. So, for example, the
;;; previous history item is accessed by dl-next.
;;;
;;; The lines are editable, and the last line is usually the one we're working
;;; on. When we go back and edit a line and accept it (hit enter) we don't
;;; change the history, we just add it at the bottom. If you go back and
;;; accept a history line, the line you started with gets lost. That's how
;;; people expect to work, so c'est la vie.
;;;
;;; To have things work the way that's expected, we:
;;;   - Always add a history line initially
;;;   - When accepting go to the last line and put it (if it meets criteria).
;;;   - When moving, put and move
;;;   - When not meeting the criteria, delete the last line.

(defstruct history
  "Line editor history."
  (head nil :type dl-node)		; Start of history list
  (tail nil :type dl-node)		; End of history list
  (cur nil :type dl-node))		; Current node

(defstruct history-entry ;; @@@ not used yet
  "An entry in the line editor history."
  time					; a universal time
  line					; a string
  (modified nil :type boolean))		; true if the entry has be edited

(defvar *history* '() "Line history of some sort.")

(defun get-history (context)
  (cdr (assoc context *history*)))

(defun history-init (context)
  "Initialize history for CONTEXT."
  (let ((context-hist (assoc context *history*)))
    (when (not context-hist)
      (let ((l (make-dl-list)))
	(setf *history* (acons context (make-history :head l :tail l :cur l)
			       *history*))))))

(defun history-clear (context)
  "Clear all of the history for the context."
  (let ((hist (get-history context))
	(lst (make-dl-list)))
    (setf (history-head hist) lst
	  (history-tail hist) lst
	  (history-cur hist) lst)))

(defun history-add (context buf)
  "Adds the content BUF as the most recent history item."
  (let* ((hist (get-history context)))
    (dl-push (history-head hist) (copy-seq buf))
    (when (not (history-cur hist))
      (setf (history-cur hist) (history-head hist)))
    (when (not (history-tail hist))
      (setf (history-tail hist) (history-head hist)))))

(defun history-delete-last (context)
  "Delete the last history entry."
  (dl-pop (history-head (get-history context))))

(defun history-put (context buf)
  "Save the BUF in the current history item."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (when cur
      (setf (dl-content cur) (copy-seq buf)))))

(defun history-prev (context)
  "Move the current history to the next oldest."
  (let* ((hist (get-history context))
	 (cur (history-cur hist))
	 (head (history-head hist))
	 (next (if cur (dl-next cur) head)))
    (when next
      (setf (history-cur hist) next))))

(defun history-next (context)
  "Move the current history to the next most recent."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (when (and cur (dl-prev cur))
      (setf (history-cur hist) (dl-prev cur)))))

(defun history-first (context)
  "Move the current history to the oldest history item."
  (let ((hist (get-history context)))
    (setf (history-cur hist) (history-tail hist))))

(defun history-last (context)
  "Move the current history to the most recent history item."
  (let ((hist (get-history context)))
    (setf (history-cur hist) (history-head hist))))

(defun history-current (context)
  "Return the content of the current item."
  (let* ((hist (get-history context))
	 (cur (history-cur hist)))
    (if cur (dl-content cur) nil)))

(defun history-current-get (context)
  "Return the current history node."
  (history-cur (get-history context)))

(defun history-current-set (context newval)
  "Set the current history node to NEWVAL."
  (setf (history-cur (get-history context)) newval))

(defsetf history-current history-current-set
  "SETF form for the current history node.")

(defun show-history (context)
  "Print the history with numbers."
  (let ((hist (get-history context))
	(i 1))
    (dl-list-do-backward
     (history-tail hist)
     #'(lambda (x)
	 (format t "~4d  ~a~%" i x)
	 (incf i))))
  (values))

(defun history-file-name (context)
  (merge-pathnames
   (make-pathname :name (format nil ".~(~a~)_history" (string context)))
   (user-homedir-pathname)))

;; Increment for every incompatible change.
(defparameter *history-version* 1
  "Version number of history format file.")

(defun history-save (context)
  (let ((hist (get-history context)))
    (with-open-file (str (history-file-name context)
			 :direction :output
			 :if-exists :supersede)
      ;; write version
      (format str "trlh ~a~%" *history-version*)
      ;; history list
      (princ #\()
      (dl-list-do hist
		  #'(lambda (x)
		      (format str "~s~%" x)))
      (princ #\) (terpri)))))

(defun history-load (context)
  (declare (ignore context)))
  ;; (let ((hist (get-history context))
  ;; 	(s (make-string 4)) i)
  ;;   (with-open-file (stm (history-file-name context)
  ;; 			 :direction :input)
  ;;     (when (string/= (read-sequence s stm :end 4) "trlh")
  ;; 	(error "Bad magic tag ~a in history file." s))
  ;;     (when (/= *history-version*
  ;; 		(setq i (parse-integer (setq s (read-line stm)))))
  ;; 	(error "Bad version number ~a in history file." s))
  ;;     ;; @@@ SECURITY ALERT !!!
  ;;     (let ((*read-eval* nil))
  ;;       (setq s (read stm)))
  ;;     (when (not (listp s))
  ;; 	(error "Malformed history list in history file: ~a." s))
  ;;     (setf (history-current hist)
  ;; 	    (make-dl-list :from-list s)))))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ;;:terminal-class 'terminal-ansi
    :terminal-class (or (and *terminal* (class-of *terminal*))
			'terminal-ansi)
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

#|
;; Make line-editor versions of the tt- funcs for code readability

(defun tt-format (e fmt &rest args)
  (apply #'terminal-format (line-editor-terminal e) fmt args))

(defmacro tt-alias (name &rest args)
  (let ((tt-name (symbolify (s+ "TT-" name)))
	(real-name (symbolify (s+ "TERMINAL-" name))))
    `(defun ,tt-name (e ,@args)
       (,real-name (line-editor-terminal e) ,@args))))

(tt-alias write-char c)
(tt-alias write-string s)
(tt-alias move-to row col)
(tt-alias move-to-col col)
(tt-alias beginning-of-line)
(tt-alias del-char n)
(tt-alias ins-char n)
(tt-alias backward n)
(tt-alias forward n)
(tt-alias up n)
(tt-alias down n)
(tt-alias scroll-down n)
(tt-alias erase-to-eol)
(tt-alias erase-line)
(tt-alias erase-below)
(tt-alias clear)
(tt-alias home)
(tt-alias cursor-off)
(tt-alias cursor-on)
(tt-alias standout state)
(tt-alias underline state)
(tt-alias beep)
(tt-alias finish-output)
(tt-alias get-key)
(tt-alias get-char)
(tt-alias listen-for n)
|#

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

#|
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
|#

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; undo
;;
;; Emacs style undo is weird in that it records the undoing as undoable, but
;; only puts it there when you type a non-undo command after a series of undos.
;;
;; Standard undo/redo in is kind of stupid, but easier to understand,
;; since when you do a modifying action after redoing, it loses your redo info.
;;
;; We're gonna do emacs style here, but see neox for the new tree style.

(defclass undo-item ()
  ((position :initarg :position :accessor undo-item-position)
   (data     :initarg :data     :accessor undo-item-data)
   (point    :initarg :point    :accessor undo-item-point))
  (:default-initargs
   :position nil
   :data nil
   :point nil)
  (:documentation "Record an undoable action."))

(defmethod print-object ((obj undo-item) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s ~s" (undo-item-position obj) (undo-item-data obj))))

(defclass deletion (undo-item) ())
(defclass insertion (undo-item) ())
(defclass boundary (undo-item) ())

(defun undo-item-length (item)
  "Return the length of an undo item in buffer characters."
  (with-slots (data) item
    (etypecase data
      (character 1)
      (string (length data)))))

(defgeneric undo-one-item (e item)
  (:documentation "Undo an undo item.")
  (:method (e (item boundary)) (declare (ignore e)) #| do nothing |# )
  (:method (e (item deletion))
    (with-slots (point) e
      (move-over e (- (undo-item-position item) point))
      (buffer-insert e (undo-item-position item) (undo-item-data item))
      (setf point (undo-item-position item))
      (update-for-insert e)))
  (:method (e (item insertion))
    (with-slots (point buf) e
      (let* ((item-len (undo-item-length item))
	     (disp-len (display-length (undo-item-data item))))
	(move-over e (- (undo-item-position item) point))
	(tt-del-char disp-len)
	(buffer-delete
	 e (undo-item-position item) (+ (undo-item-position item) item-len))
	(setf (point e) (undo-item-position item))
	(update-for-delete e disp-len item-len)))))

(defun record-undo (e type &optional position data point)
  (let ((hist (car (undo-history e))))
    (cond
      ((and (eql type 'boundary) (typep hist 'boundary))
       #| Don't record multiple consecutive boundaries |#)
;       ((and (eql type 'insertion) (typep hist 'insertion))
;        (cond
; 	 ;; convert two consecutive adjacent char insertions to a string
; 	 ((and (characterp (undo-item-data hist))
; 	       (characterp data)
; 	       (= position (1+ (undo-item-position hist))))
; 	  (let ((str (make-string 2)))
; 	    (setf (aref str 0) (undo-item-data hist)
; 		  (aref str 1) data)
; 	    (push (make-instance
; 		   type :position (undo-item-position hist) :data str)
; 		  (undo-history e))))
; 	 ;; add a adjacent character insertion onto a string
; 	 ((and (characterp data)
; 	       (stringp (undo-item-data hist))
; 	       (= position (+ (undo-item-position hist)
; 			      (length (undo-item-data hist)))))
; 	  (push (make-instance
; 		 type :position (undo-item-position hist)
; 		 :data (concatenate 'string (undo-item-data hist)
; 				    (string data)))
; 		(undo-history e)))))
      (t
       (push (make-instance type :position position :data data :point point)
	     (undo-history e))))))

(defun undo-one (e)
  "Undo one item from the undo history. Return true if we should undo more."
  (let (item)
    (if (equal (last-input e) (ctrl #\O)) ; @@@ bogus ^O until keymaps, etc
      (progn
	(if (undo-current e)
	  (progn
	    (setf item (car (undo-current e)))
	    (undo-one-item e item)
	    (setf (undo-current e) (cdr (undo-current e))))
	  (beep e "No more undo information.")))
      (progn
	(if (undo-history e)
	  (progn
	    (setf (undo-current e) (cdr (undo-history e))
		  item (car (undo-history e)))
	    (undo-one-item e item))
	  (beep e "No undo history."))))
;    (message-pause e "Undid ~s" item)
    (and item (not (typep item 'boundary)))))

(defun undo (e)
  "Undo until an undo boundry or all undone."
  (do () ((not (undo-one e)))))

(defmacro without-undo ((e) &body body)
  "Execute the body with undo recording off in the given editor."
  (let ((old-undo (gensym)))
    `(let ((,old-undo (record-undo-p ,e)))
      (unwind-protect
	   (progn
	     (setf (record-undo-p ,e) nil)
	     ,@body)
	(setf (record-undo-p ,e) ,old-undo)))))

(defun undo-command (e)
  ;;(format t "~s~%" (undo-history e))
  ;;(undo e) ;; @@@ Please make undo boundries work @@@
  (undo-one e)
  ;; (redraw e) ;; @@@ This is overkill! (and screws up multiline prompts)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer interface
;;
;; The buf slot of line-editor should only be modified thru these.

(defgeneric buffer-delete (e start end)
  (:documentation
   "Delete characters from the buffer from the position START to END.")
  (:method ((e line-editor) start end)
    (with-slots (buf point) e
;      (format t "del (~s ~s)~%" start end)
      ;; If end and start are reversed, swap them.
      (when (< end start)
	(rotatef start end))
      ;; If start equals end, it's a empty deletion, so we don't have to do
      ;; anything.
      (when (> end start)
	(record-undo e 'deletion start (subseq buf start end) point)
	;; Optimization: Deleting to the end, just decrement fill pointer.
	(when (not (= end (fill-pointer buf)))
	  (setf (subseq buf start) (subseq buf end)))
	(decf (fill-pointer buf) (- end start))))))

(defgeneric buffer-insert (e pos thing)
  (:documentation
   "Insert something into the buffer at position POS.")
  (:method ((e line-editor) pos (c character))
;    (format t "ins (~s ~s)~%" pos c)
    (with-slots (buf) e
      (record-undo e 'insertion pos (string c))
      (if (= pos (length buf))
	  ;; Appending to the end
	  (progn
	    (vector-push-extend c buf
				(+ (array-total-size buf)
				   (truncate (* (array-total-size buf) 2/3)))))
	  ;; Inserting in the middle
	  (progn
	    (when (= (length buf) (array-total-size buf))
	      (setf buf (adjust-array
			 buf (+ (array-total-size buf)
				(truncate (* (array-total-size buf) 2/3))))))
	    (incf (fill-pointer buf))
	    (setf (subseq buf (1+ pos)) (subseq buf pos))
	    (setf (aref buf pos) c)))))
  (:method ((e line-editor) pos (s string))
;    (format t "ins (~s ~s)~%" pos s)
    (with-slots (buf) e
      (let ((len (length s)))
	(record-undo e 'insertion pos s)
	(when (>= (+ len (length buf)) (array-total-size buf))
	  (setf buf (adjust-array
		     buf (+ (array-total-size buf) len 
			    (truncate (* (array-total-size buf) 2/3))))))
	(incf (fill-pointer buf) len)
	(setf (subseq buf (+ pos len)) (subseq buf pos))
	(setf (subseq buf pos (+ pos len)) s)))))

;; Replace could just be a delete followed by an insert, but
;; for efficiency sake we do something special.

(defgeneric buffer-replace (e pos thing)
  (:documentation
   "Insert something into the buffer at position POS.")
  (:method ((e line-editor) pos (c character))
;    (format t "replace (~s ~s)~%" pos c)
    (with-slots (buf point) e
      (record-undo e 'deletion pos (string (aref buf pos)) point)
      (record-undo e 'insertion pos (string c))
      (setf (aref buf pos) c)))
  (:method ((e line-editor) pos (s string))
;    (format t "replace (~s ~s)~%" pos s)
    (with-slots (buf point) e
      (let ((len (length s)))
	(when (> (+ pos len) (length buf))
	  (error "Replacement doesn't fit in the buffer."))
	(when (> len 0)
	  (record-undo e 'deletion pos (subseq buf pos (+ pos len)) point)
	  (record-undo e 'insertion pos s)
	  (setf (subseq buf pos (+ pos len)) s))))))

;; @@@ Currently unused.
;; (defun eobp (e)
;;   "Return true if point is at (or after) the end of the buffer."
;;   (with-slots (point buf) e
;;     (>= point (length buf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-ish code

(defun message (e fmt &rest args)
  "Show a little message for debugging."
  (declare (type line-editor e))
  (tt-cursor-off)
  (tt-move-to 5 5)		; The only place we should call this
  (tt-standout t)
  ;; (apply #'tt-format (cons (line-editor-terminal e) (cons fmt args)))
  (apply #'terminal-format (cons (line-editor-terminal e) (cons fmt args)))
  (tt-standout nil)
  (tt-cursor-on))

(defun log-message (e fmt &rest args)
  (when (debugging e)
    (push (apply #'format nil fmt args) (line-editor-debug-log e))))

(defun show-message-log (e)
  "Show the debugging message log."
  (declare (type line-editor e))
  (tt-cursor-off)
  (tt-standout t)
  (loop :for i :from 0 :below 8
     :for dd :in (line-editor-debug-log e)
     :do
     (tt-move-to (+ 10 i) 40)		; The “only” place we should call this
     (tt-erase-to-eol)
     (tt-write-string dd))
  (tt-standout nil)
  (tt-cursor-on))

(defun message-pause (e fmt &rest args)
  "Show a little message for debugging."
  (apply #'message e fmt args)
  (get-a-char e))

(defun beep (e fmt &rest args)
  "Beep or display an error message."
  (when (debugging e)
    (apply #'message e fmt args))
  (tt-beep))

;; Note: no tab or newline
(defparameter *control-char-graphics-vec*
  `((#\Null . #\@) (,(ctrl #\A) . #\A) (,(ctrl #\B) . #\B) (,(ctrl #\C) . #\C)
    (,(ctrl #\D) . #\D) (,(ctrl #\E) . #\E) (,(ctrl #\F) . #\F)
    (,(ctrl #\G) . #\G) (,(ctrl #\H) . #\H) (,(ctrl #\J) . #\J)
    (,(ctrl #\K) . #\K) (,(ctrl #\L) . #\L) (,(ctrl #\M) . #\M)
    (,(ctrl #\N) . #\N) (,(ctrl #\O) . #\O) (,(ctrl #\P) . #\P)
    (,(ctrl #\Q) . #\Q) (,(ctrl #\R) . #\R) (,(ctrl #\S) . #\S)
    (,(ctrl #\T) . #\T) (,(ctrl #\U) . #\U) (,(ctrl #\V) . #\V)
    (,(ctrl #\W) . #\W) (,(ctrl #\X) . #\X) (,(ctrl #\Y) . #\Y)
    (,(ctrl #\Z) . #\Z) (#\Escape . #\[) (#\Fs . #\\) (#\Gs . #\])
    (#\Rs . #\^) (#\Us . #\_) (#\Rubout . #\?))
  "Vector of control characters and corresponding caret notation char.")

(defun pair-vector-to-hash-table (vec table)
  (loop :for (k . v) :across vec
     :do (setf (gethash k table) v))
  table)

(defparameter *control-char-graphics* nil)

;; @@@ Perhaps this should so be somewhere else.
(defun control-char-graphic (c)
  (when (not *control-char-graphics*)
    (setf *control-char-graphics* (make-hash-table :test #'eql))
    (alist-to-hash-table *control-char-graphics-vec*
			 *control-char-graphics*))
  (gethash c *control-char-graphics*))

(defgeneric display-length (obj)
  (:documentation "Return how long is the object should be when displayed."))

#+sbcl
;; Older versions of SBCL don't have this.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-unicode)
    (d-add-feature :has-sb-unicode)))

;; @@@ Perhaps this should so be somewhere else.
(defun double-wide-p (c)
  #+(and sbcl has-sb-unicode) (eq (sb-unicode:east-asian-width c) :w)
  #-(and sbcl has-sb-unicode) (declare (ignore c))
  #-(and sbcl has-sb-unicode) nil	; @@@ too hard without tables
  )

;; XXX This is still wrong for unicode! @@@
(defmethod display-length ((c character))
  "Return the length of the character for display."
  (cond
    ((graphic-char-p c)
     (cond
       ((combining-character-p c) 0)
       ((double-wide-p c) 2)
       (t 1)))				;normal case
    ((eql c #\tab)
     8)					;XXX @@@ wrong!
    ((eql c #\newline)
     0)					;XXX @@@ wrong!
    (t
     (if (control-char-graphic c)
	 2   ; ^X
	 4)  ; \000
     )))

(defmethod display-length ((s string))
  "Return the length of the string for display."
  (let ((sum 0))
    (map nil #'(lambda (c) (incf sum (display-length c))) s)
    sum))

(defun moo (e s)
  (let* ((tt (line-editor-terminal e))
	 (width (terminal-window-columns tt))
	 ;;(height (terminal-window-rows tt))
	 (row (screen-row e))
	 (col (screen-col e))
	 (plain (remove-effects s))
	 (last-col col))
    (loop :for c :across plain :do
       (cond
	 ((graphic-char-p c)
	  (cond
	    ((combining-character-p c) #|nothing|#)
	    ((double-wide-p c) (incf col 2))
	    (t (incf col))))		; normal case
	 ((eql c #\tab)
	  (setf col (1+ (logior 7 col)))
	  (when (>= col width)
	    (setf col (1- width))))
	 ((eql c #\newline)
	  (setf col 0) (incf row))
	 ((control-char-graphic c)	; ^X
	  (incf col 2))
	 (t (incf col 4)))		; \000
       (when (>= col width)
	 (setf col (- col last-col))
	 (incf row))
       (setf last-col col))
    (values col row)))

;; @@@ XXX wrong, because it doesn't account for double-width last col issues
(defun editor-update-pos-for-string (e s)
  "Update the screen row and column for inserting the string S.
Assumes S is already converted to display characters."
  (let* ((width (terminal-window-columns (line-editor-terminal e)))
	 ;;(len (length s))
	 (len (display-length s))
	 (last-remain (+ (screen-col e) len)))
     (loop :with remain = (max 0 (- len (- width (screen-col e))))
	  :while (> remain 0)
	  :do
	  (incf (screen-row e))
	  (setf (screen-col e) 0)
	  (decf remain (setf last-remain (min width remain))))
     (setf (screen-col e) last-remain)
;    (message-pause (line-editor-terminal e) "fug ~a" (screen-col e))
    ))

; Test code for editor-update-pos-for-string
; (defun fuglor (s col row)
;   (let* ((width 80)
; 	 (len (length s))
; 	 (last-remain (+ col len)))
;      (loop with remain = (max 0 (- len (- width col)))
; 	  while (> remain 0)
; 	  do
; 	  (incf row)
; 	  (setf col 0)
; 	  (decf remain (setf last-remain (min width remain))))
;     (setf col last-remain)
;     (format t "[~d ~d]~%" col row)))

(defun editor-write-char (e c)
  "Write a display character to the screen. Update screen coordinates."
  (with-slots (screen-col screen-row) e
    (let ((len (display-length c))
	  (width (terminal-window-columns (line-editor-terminal e))))
      (cond
	((> (+ screen-col len) width)
	 (setf screen-col len)
	 (incf screen-row)
	 (tt-scroll-down 1)
	 (tt-beginning-of-line)
	 (tt-write-char c))
	((= (+ screen-col len) width)
	 (tt-write-char c)
	 (setf screen-col 0)
	 (incf screen-row)
	 (tt-scroll-down 1)
	 (tt-beginning-of-line))
	(t
	 (tt-write-char c)
	 (incf screen-col len))))))

(defun editor-write-string (e s)
  "Write a display string to the screen. Update screen coordinates."
  ;; (finish-all-output) ;; @@@@ XXX for testing
  ;;(tt-write-string (line-editor-terminal e) s)
  (tt-write-string s)
  ;; (finish-all-output) ;; @@@@ XXX for testing
  (editor-update-pos-for-string e s))

(defun display-char (e c)
  "Output a character with visible display of control characters."
  (cond
    ((graphic-char-p c)
     (editor-write-char e c))
    ((eql c #\tab)
     ;; (editor-write-string e (make-string (- 8 (mod (screen-col e) 8))
     ;; 					 :initial-element #\space)))
     (dotimes (_ (- (1+ (logior 7 (screen-col e))) (screen-col e)))
       (tt-write-char #\space)))
    ((eql c #\newline)
     (setf (screen-col e) 0)
     (incf (screen-row e))
     (tt-write-char c))
    ((setf c (control-char-graphic c))
     (editor-write-char e #\^)
     (editor-write-char e c))
    (t ;; output non-graphic chars as char code
     (editor-write-char e #\\)
     (editor-write-string e (format nil "\\~3,'0o" (char-code c))))))

(defun display-buf (e &optional (start 0) end)
  "Display the buffer."
  (with-slots (buf) e
    ;; XXX Wrong!
    ;; ;; Just in case write-char does system calls, we output to a string stream.
    ;; (tt-write-string
    ;;  (with-output-to-string (s)
    ;; (if end
    ;; 	(subseq buf start end)
    ;; 	(subseq buf start))
    (loop :with sub = (subseq buf start end)
       :for c :across sub :do
       (display-char e c))))

(defmacro without-messing-up-cursor ((e) &body body)
  (let ((old-row (gensym "OLD-ROW"))
	(old-col (gensym "OLD-COL")))
  `(let ((,old-row (screen-row ,e))
	 (,old-col (screen-col ,e)))
     (prog1 ,@body
       (if (< ,old-row (screen-row ,e))
	   (tt-up (- (screen-row ,e) ,old-row))
	   (tt-down (- ,old-row (screen-row ,e))))
       (tt-beginning-of-line)
       (tt-forward ,old-col)
       (setf (screen-row ,e) ,old-row
	     (screen-col ,e) ,old-col)))))

(defun buffer-length-to (buf to-length)
  (loop :with i = 0
     :for buf-i = 0 :then (1+ buf-i)
     :while (< i to-length)
     :do (incf i (display-length (aref buf buf-i)))
     :finally (return buf-i)))

(defun update-for-delete (e delete-length char-length)
  "Update the display, assuming DELETE-LENGTH characters were just deleted at 
the current cursor position."
  (declare (ignore char-length)) ;; @@@
  (with-slots (buf point terminal) e
    (let ((width (terminal-window-columns terminal))
	  (col (screen-col e))
	  (right-len (display-length (subseq buf point)))
	  to-delete)
      ;; If the rest of the buffer extends past the edge of the window.
      ;;(when (>= (+ col right-len) width)
      (when (>= (+ col (+ right-len delete-length)) width)
	;; Cheaty way out: redraw whole thing after point
	(without-messing-up-cursor (e)
	  (display-buf e point)
	  ;;(tt-del-char delete-length)
	  ;; We can't get it right here, because the characters aren't in the
	  ;; buffer, so we overshoot a little.
	  (tt-erase-to-eol)
	  (log-message e "Smoot ~a ~a ~a" width delete-length (screen-col e))
	  (when (< (- width delete-length) (screen-col e))
	    (setf to-delete (- delete-length (- width (screen-col e))))
	    (tt-down 1)
	    (incf (screen-row e))
	    (tt-move-to-col 0)
	    (setf (screen-col e) 0)
	    (tt-erase-to-eol)
	    (loop :while (> to-delete 0) :do
	       (tt-down 1)
	       (incf (screen-row e))
	       (tt-erase-to-eol)
	       (log-message e "Bloot ~a" to-delete)
	       (decf to-delete width))))))))

(defun update-for-insert (e)
  "Assuming we just inserted something, redisplay the rest of the buffer."
  (with-slots (buf point) e
    (when (not (= point (length buf)))	; At the end of the buffer.
      ;; Do this an horribly inefficient and cheating way:
      ;; just rewrite the whole thing relying on terminal wrap around.
      ;;
      ;; @@@ We should really fix this someday to do it the right way, and
      ;; just fiddle the beginnings and ends of the lines. Or even better.
      (let ((old-row (screen-row e))
	    (old-col (screen-col e)))
	(display-buf e point)
	(if (< old-row (screen-row e))
	    (tt-up (- (screen-row e) old-row))
	    (tt-down (- old-row (screen-row e))))
	(tt-beginning-of-line)
	(tt-forward old-col)
	(setf (screen-row e) old-row
	      (screen-col e) old-col)))))

(defun erase-display (e)
  "Erase the display of the buffer, but not the buffer itself."
  (with-slots (buf) e
    (beginning-of-line e)
    (tt-erase-to-eol)
    (let* ((cols (terminal-window-columns (line-editor-terminal e)))
	   ;;(buf-len (length buf))
	   (buf-len (display-length buf))
	   ;;(lines-to-clear (truncate (+ (screen-col e) buf-len) 80)))
	   (lines-to-clear (truncate (+ (screen-col e) buf-len) cols)))
      (when (> (+ buf-len (screen-col e)) cols)
	(loop :for i :from 1 :to lines-to-clear
	      :do (tt-down 1)
	      (tt-erase-line))
	(tt-up lines-to-clear)))))

(defun replace-buffer (e str)
  "Replace the buffer with the given string STR."
  (declare (type string str))
  (with-slots (buf point) e
    (erase-display e)
    (setf point (length str))
    (buffer-delete e 0 (length buf))
    (buffer-insert e 0 str)
    (display-buf e)))

(defun use-hist (e)
  "Replace the current line with the current history line."
  ;; @@@ Problems:
  ;; - The current line is lost.
  ;; - The undo history (and all other buffer properties) are not
  ;;   retained.
  (without-undo (e)
    (if (history-current (context e))
	(replace-buffer e (history-current (context e)))
	(replace-buffer e ""))))

;; If performance was really a problem, we could carefully maintain some of
;; the line endings. But for a line editor, unless we're running on very slow
;; systems, this seems unlikely to be a problem. It's not really advisable to
;; edit megabytes of text in a line editor. If someone wants to do a really
;; big paste, using an unprocessed read is better. Also for big pastes,
;; there's timing tricks that one can do with the lower level read.

(defun calculate-line-endings (e &key (buffer (buf e)) (start (start-col e)))
  "Return a list of pairs of character positions and columns, in reverse order
of character position, which should be the end of the displayed lines in the
buffer."
  (let (endings
	(col start)			; Start after the prompt
	(cols (terminal-window-columns (line-editor-terminal e)))
	(char-width 0)
	(last-col start)
	(i 0))
    (loop :while (< i (length buffer)) :do
       (if (char= (aref buffer i) #\newline)
	   (progn
	     (push (cons (1- i) last-col) endings)
	     (setf last-col col)
	     (setf col 0))
	   (progn
	     (setf char-width (display-length (aref buffer i)))
	     (if (> (+ col char-width) cols)
		 (progn
		   (push (cons (1- i) last-col) endings)
		   (setf last-col col)
		   (setf col char-width))
		 (progn
		   (setf last-col col)
		   (incf col char-width)))))
       (incf i))
    ;; Make sure we get the last one
    (when (> (+ col char-width) cols)
      (push (cons (1- i) last-col) endings))
    endings))

(defun line-ending (pos endings)
  "Return the line ending at character position POS, from the line ENDINGS,
or NIL is there is none."
  (cdr (assoc pos endings)))

(defun move-over (e n &key (start (point e)) (buffer (buf e)))
  "Move over N characters in the buffer, from START. N is negative for backward,
positive for forward. Doesn't adjust POINT, but does move the cursor and update
the SCREEN-COL and SCREEN-ROW."
  (with-slots (screen-row screen-col) e
    (let ((cols (terminal-window-columns (line-editor-terminal e)))
	  (col screen-col)
	  (row screen-row)
	  (endings (calculate-line-endings e))
	  len)
      (log-message e "move-over ~a ~a" n start)
      (if (minusp n)
	  ;; backward
	  (progn
	    (loop :for i :from start :above (+ start n) :do
	       ;;(tt-move-to 20 5)
	       ;;(tt-format "--> ~a ~a ~a ~a" row col i endings)
	       ;;(tt-move-to row col)
	       ;;(tt-get-key)
	       (setf len (display-length (aref buffer (1- i))))
	       (if (< (- col len) 0)
		   (progn
		     ;;(log-message e "mooo ~a ~a ~a" i col endings)
		     ;;(setf col (- cols len))
		     (assert (assoc (1- i) endings))
		     (setf col (cdr (assoc (1- i) endings)))
		     (decf row))
		   (progn
		     (decf col len))))
	    ;;(tt-move-to 20 5)
	    ;;(tt-erase-to-eol)
	    ;;(tt-format "--> DONE ~a ~a" row col)
	    ;;(tt-move-to row col)
	    (log-message e "row=~a col=~a" row col)
	    #| because we tt-move-to in the debug code |#
	    (tt-up (- screen-row row))
	    (if (< col screen-col)
		(progn
		  ;; could optimize by:
		  ;;(tt-beginning-of-line)
		  ;;(tt-forward col)
		  (tt-backward (- screen-col col)))
		(progn
		  (tt-forward (- col screen-col))))
	    ;;(tt-move-to row col)
	    (setf screen-col col
		  screen-row row))
	  ;; forward
	  (progn
	    (loop :for i :from start :below (+ start n) :do
	       (setf len (display-length (aref buffer i)))
	       (cond
		 ((assoc i endings)
		  (setf col 0)
		  (incf row))
		 ((> (+ col len) cols)
		  (setf col len)
		  (incf row))
		 (t
		  (incf col len))))
	    (log-message e "row=~a col=~a" row col)
	    ;; (tt-beginning-of-line)
	    ;; (tt-forward col)
	    ;; (tt-down (- screen-row row))
	    (tt-down (- row screen-row))
	    (if (< col screen-col)
		(progn
		  ;; could optimize by:
		  ;;(tt-beginning-of-line)
		  ;;(tt-forward col)
		  (tt-backward (- screen-col col)))
		(progn
		  (tt-forward (- col screen-col))))
	    (setf screen-col col
		  screen-row row))))))

(defun move-backward (e n)
  "Move backward N columns on the screen. Properly wraps to previous lines.
Updates the screen coordinates."
  (move-over e (- n)))

(defun move-forward (e n)
  "Move forward N columns on the screen. Properly wraps to subsequent lines.
Updates the screen coordinates."
  (move-over e n))

#|
;;; @@@ Consider the issues of merging this with display-length.
;;; @@@ Consider that this is quite wrong, especially since it would have to
;;; do everything a terminal would do.
(defun display-cols (str)
  "Return the column the cursor is at after outputting STR."
  (let ((sum 0))
    (map nil
	 #'(lambda (c)
	     (cond
	       ((graphic-char-p c)
		(incf sum))
	       ((eql c #\tab)
		(incf sum (- 8 (mod sum 8))))
	       ((eql c #\newline)
		(setf sum 0))
	       ((eql c #\backspace)
		(decf sum))
	       ((eql c #\escape)
		#| here's where we're screwed |#)
	       (t
		(if (control-char-graphic c)
		    2			; ^X
		    4)			; \000
		)))
	 s)
    sum))
|#

;; Here's the problem:
;;
;; People can put any old stuff in the prompt that they want. This includes
;; things that move the cursor around, characters that might be of different
;; widths, escape sequences that may or may not move the cursor. So unless we
;; emulate the terminal exactly, just to figure out where the cursor is after
;; the prompt, things can get messed up.
;;
;; We could be like other shells and require that you delimit non-echoing
;; characters yourself and allow you to specifiy an output width for
;; characters, but not only is annoying, but it won't always work.
;;
;; Since emulating the terminal seems infeasible, unless we wrapped ourselves
;; in an emulation layer like screen or tmux, if we want to be sure to get
;; things right, we are stuck with with asking the terminal where the cursor
;; might be.
;;
;; The problem with asking the terminal, is that we have to output something,
;; and then read the coordinates back in. But there might be a bunch of input,
;; like a giant paste or something, or typing ahead, already in the terminal's
;; input queue, in front of the response to our "where is the cursor" query,
;; which blocks us from getting an answer.
;;
;; So we have to read all input available, BEFORE asking where the cursor
;; is. This is the reason for all the otherwise useless 'eat-typeahead' and
;; 'tty-slurp'ing. Of course this whole thing is quite kludgey and I think we
;; should really be able ask the terminal where the cursor is with a nice
;; _function call_, not going through the I/O queue. Of course that would
;; require the terminal to be in our address space, or to have a separate
;; command channel if it's far, far away.
;;
;; There is still a small opportunity for a race condition, between outputing
;; the query, and getting an answer back, but it seems unlikely. I wonder if
;; there's some way to 'lock' the terminal input queue for that time.
;;

(defun finish-all-output ()
  "Makes all output be in Finnish."
  (when (not (environment-variable "EMACS")) ; XXX so wrong
    ;;#+ccl (ccl::auto-flush-interactive-streams) ;; Jiminy Crickets!
    (finish-output *standard-output*)
    (finish-output *terminal-io*)
    (finish-output t)
    (finish-output)
    ;(finish-output *standard-input*)
    )
  (tt-finish-output)
  )

(defun do-prefix (e prompt-str)
  "Output a prefix."
  (finish-all-output)
  (let (row col start-row)
    (multiple-value-setq (row col)
      (terminal-get-cursor-position (line-editor-terminal e)))
    (setf start-row row)
    (tt-write-string prompt-str)
    ;;(finish-all-output)
    (tt-finish-output)
    ;; (eat-typeahead e)
    (multiple-value-setq (row col)
      (terminal-get-cursor-position (line-editor-terminal e)))
    (setf (screen-row e) row
	  (screen-col e) col
	  ;; save end of the prefix as the starting column
	  (start-col e) col
	  (start-row e) row
	  (prompt-height e) (- row start-row))
    (log-message e "prompt-height = ~s" (prompt-height e))))

(defun do-prompt (e prompt output-prompt-func &key only-last-line)
  "Output the prompt in a specified way."
;  (format t "e = ~w prompt = ~w output-prompt-func = ~w~%"
;	  e prompt output-prompt-func)
  (let* ((s (if (and output-prompt-func
		     (or (functionp output-prompt-func)
			 (fboundp output-prompt-func)))
		(with-output-to-string (*standard-output*)
		  (log-message e "do-prompt output-prompt-func -> ~s"
			       output-prompt-func)
		  (or (ignore-errors (funcall output-prompt-func e prompt))
		      "Your prompt Function failed> "))
		(progn
		  (log-message e "do-prompt default-output-prompt")
		  (default-output-prompt e prompt))))
	 last-newline)
    (log-message e "do-prompt only-last-line = ~s" only-last-line)
    (log-message e "do-prompt last-newline = ~s"
		 (position #\newline s :from-end t))
    (when (and only-last-line
	       (setf last-newline (position #\newline s :from-end t)))
      (setf s (subseq s (1+ last-newline)))
      (log-message e "partial prompt ~s" s))
    (log-message e "do-prompt s = ~s ~s" (length s) s)
    (do-prefix e s)))

(defun redraw (e)
  "Erase and redraw the whole line."
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  (setf (screen-col e) 0)
  (do-prompt e (prompt e) (prompt-func e) :only-last-line t)
  (finish-output (terminal-output-stream (line-editor-terminal e)))
  (display-buf e)
  (with-slots (point buf) e
    (when (< point (length buf))
      ;;(let ((disp-len (display-length (subseq buf point))))
      ;;  (move-backward e disp-len))))
      (move-over e (- (length buf) point) :start (length buf))))
  (setf (need-to-redraw e) nil))

(defun tmp-prompt (e fmt &rest args)
  (tt-move-to-col 0)
  (tt-erase-to-eol)
  (setf (screen-col e) 0)
  (do-prefix e (apply #'format `(nil ,fmt ,@args))))

(defun tmp-message (e fmt &rest args)
  (apply #'tmp-prompt e fmt args)
  (setf (need-to-redraw e) t))

(defun clear-completions (e)
  "Erase completions, if there are any."
  (when (did-under-complete e)
    (without-messing-up-cursor (e)
      (when (< (screen-row e)
	       (1- (terminal-window-rows (line-editor-terminal e))))
	(tt-down 1)
	(incf (screen-row e))
	(tt-beginning-of-line)
	(setf (screen-col e) 0)
	(tt-erase-below)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

;; @@@ Perhaps this should be merged with one in completion?
(defun scan-over (e dir &key func not-in action)
  "If FUNC is provied move over characters for which FUNC is true.
If NOT-IN is provied move over characters for which are not in it.
DIR is :forward or :backward. E is a line-editor.
If ACTION is given, it's called with the substring scanned over and replaces
it with ACTION's return value."
  (if (and (not func) not-in)
      (setf func #'(lambda (c) (not (position c not-in)))))
  (with-slots (point buf) e
    (let (cc)
      (if (eql dir :backward)
	  ;; backward
	  (loop :while (and (> point 0)
			   (funcall func (aref buf (1- point))))
	    :do
	    (when action
	      (when (setf cc (funcall action (aref buf (1- point))))
		(buffer-replace e (1- point) cc)))
	    (decf point))
	  (let ((len (length buf))
		(did-one nil))
	    (loop :while (and (< point len)
			     (funcall func (aref buf point)))
	      :do
	      (progn
		(when action
		  (when (setf cc (funcall action (aref buf point)))
		    (buffer-replace e point cc)
		    (setf did-one t)))
		(incf point)))
	    (when did-one (decf point)))))))

(defun backward-word (e)
  "Move the insertion point to the beginning of the previous word or the
beginning of the buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (move-over e (- (- start point)) :start start))))

(defun forward-word (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (move-over e (- point start) :start start))))

(defun backward-char (e)
  "Move the insertion point backward one character in the buffer."
  (with-slots (point) e
    (when (> point 0)
      (move-over e -1)
      (decf point))))

(defun forward-char (e)
  "Move the insertion point forward one character in the buffer."
  (with-slots (point buf) e
    (when (< point (fill-pointer buf))
      (move-over e 1)
      (incf point))))

(defun beginning-of-line (e)
 "Move the insertion point to the beginning of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-over e (- point))
    (setf point 0)))

(defun end-of-line (e)
  "Move the insertion point to the end of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-over e (- (length buf) point))
    (setf point (fill-pointer buf))))

(defun previous-history (e)
  "Go to the previous history entry."
  (history-put (context e) (buf e))
  (history-prev (context e))
  (use-hist e))

(defun next-history (e)
  "Go to the next history entry."
  (history-put (context e) (buf e))
  (history-next (context e))
  (use-hist e))

(defun beginning-of-history (e)
  "Go to the beginning of the history."
  (history-put (context e) (buf e))
  (history-first (context e))
  (use-hist e))

(defun end-of-history (e)
  "Go to the end of the history."
  (history-put (context e) (buf e))
  (history-last (context e))
  (use-hist e))

(defun add-to-history-p (e buf)
  "Returns true if we should add the current line to the history. Don't add it
if it's blank or the same as the previous line."
  (with-slots (context) e
    (let* ((cur (history-current-get context))
	   (prev (dl-next cur)))
#|
      (dbug "add-to-history-p = ~w~%  buf = ~w~%  (length buf) = ~w~%~
  cur = ~w~%  prev = ~w~%  (dl-content prev) = ~w~%"
	    (not (or (and buf (= (length buf) 0))
		     (and prev (dl-content prev)
			  (equal (dl-content prev) buf))))
	    buf (length buf) cur prev (dl-content prev))
|#
      (not (or (and buf (= (length buf) 0))
	       (and prev (dl-content prev) (equal (dl-content prev) buf)))))))

(defun accept-line (e)
  (with-slots (buf quit-flag context accept-does-newline) e
    (history-last context)
    (if (add-to-history-p e buf)
	(history-put context buf)
	(history-delete-last context))
    (when accept-does-newline
      (move-over e (- (length (buf e)) (point e)))
      (tt-write-char #\newline)
      (tt-write-char #\return)
      (when (did-under-complete e)
	(tt-erase-below))
      (tt-finish-output))
    (setf quit-flag t)))

(defun copy-region (e)
  "Copy the text between the insertion point and the mark to the clipboard."
  (with-slots (point mark buf clipboard) e
    (setf clipboard (subseq buf mark point))))

(defun set-mark (e)
  "Set the mark to be the current point."
  (with-slots (point mark) e
    (setf mark point)))

(defun exchange-point-and-mark (e)
  "Move point to the mark. Set the mark at the old point."
  (with-slots (point mark) e
    (when mark
      (cond
	((< mark point)
	 ;;(move-backward e (- point mark))
	 (move-over e (- (- point mark)) :start point))
	((> mark point)
	 ;;(move-forward e (- mark point))
	 (move-over e (- mark point))))
      (rotatef point mark))))

(defun isearch-backward (e)
  "Incremental search backward."
  (isearch e :backward))

(defun isearch-forward (e)
  "Incremental search forward."
  (isearch e :forward))

;; Sadly ASCII / UTF-8 specific. @@@ And should be moved to char-util?
;; (defun control-char-p (c)
;;   (let ((code (char-code c)))
;;     (or (< code 32) (= code 128))))

(defparameter *isearch-prompt* "isearch: ")

#|
(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (buf point) e
    ;;(setf point (min (or pos (length buf)) (length buf)))
    (erase-display e)
    ;;(tt-move-to-col 0)
    ;;(tt-erase-to-eol)
    ;;(setf (screen-col e) 0)
    ;;(do-prefix e *isearch-prompt*)
    (when (and str pos)
      (without-undo (e)
	;;(erase-display e)
	(setf point 0)
	(buffer-delete e 0 (length buf))
	(buffer-insert e 0 (or (history-current (context e)) ""))
	(move-over e (min (or pos (length buf)) (length buf)))
	(setf point (min (or pos (length buf)) (length buf)))
	)
      )
    #|
      (loop :with end = (if pos (+ pos (length str)) nil)
	   :for c :across buf :and i = 0 :then (1+ i) :do
	   (cond
	     ((and pos (= i pos))
	      (tt-underline t))
	     ((and end (= i end))
	      (tt-underline nil)))
	   (display-char e c))
	(tt-underline nil))
    (tt-finish-output)
    |#
    ;;(display-buf e 0 pos)
    ;;(tt-underline t)
    ;;(display-buf e pos (+ pos (length str)))
    ;;(tt-underline nil)
    ;;(display-buf e (+ pos (length str))
    ))
|#

(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (point context) e
    ;;(setf point (min (or pos (length buf)) (length buf)))
    (tt-move-to-col 0)
    (erase-display e)
    ;; (tt-erase-to-eol)
    (setf (screen-col e) 0)
    (do-prefix e *isearch-prompt*)
    ;;(log-message e "buf = ~s" buf)
    (when (history-current context)
      (loop :with end = (if pos (+ pos (length str)) nil)
	 :for c :across (history-current context)
	 :and i = 0 :then (1+ i) :do
	 (cond
	   ((and pos (= i pos))
	    (tt-underline t))
	   ((and end (= i end))
	    (tt-underline nil)))
	 ;;(display-char e c)
	 (tt-write-char c)
	 ))
    (tt-underline nil)
    (tt-finish-output)))

(defun search-start-forward (context)
  ;; (or (and (history-current-get context)
  ;; 	   (dl-prev (history-current-get context)))
  (or (history-current-get context)
      (history-head (get-history context))))

(defun search-start-backward (context)
  ;; (or (and (history-current-get context)
  ;; 	   (dl-next (history-current-get context)))
  (or (history-current-get context)
      (history-tail (get-history context))))

(defun backward-start-pos (str pos)
  ;; (cond
  ;;   ((not pos)
  ;;    (length str))
  ;;   ((> pos 0)
  ;;    (min (1- pos) (length str)))
  ;;   (t 0)))
  (min (length str)
       (or pos (length str))))

(defun forward-start-pos (str pos)
  (cond
    ((not pos)
     0)
    ((< pos (1- (length str)))
     (1+ pos))
    (t (length str))))

(defun search-history (e str direction start-from search-pos)
  (with-slots (point context) e
    (let ((hist (get-history context))
	  (first-time t))
;      (dbug "yoyo context ~w ~w~%" context hist)
      (if (eq direction :backward)
	  (progn
;	    (dbug "starting-at ~w~%" start-from)
	    (dl-list-do-element
	     start-from
	     #'(lambda (x)
		 (when (dl-content x)
		   (dbug "(search ~w ~w :end2 ~w) search-pos = ~w~%"
			 str (dl-content x)
			 (backward-start-pos (dl-content x) search-pos)
			 search-pos)
		   (let (pos)
		     (if first-time
			 (setf pos (search str (dl-content x)
					   :from-end t
					   :end2 (backward-start-pos
						  (dl-content x) search-pos))
			       first-time nil)
			 (setf pos (search str (dl-content x) :from-end t)))
		     (when pos
		       (dbug "found pos = ~w in ~w (~w) x=~a~%"
			     pos (dl-content x) str x)
		       (setf (history-cur hist) x)
		       (return-from search-history pos)))))))
	  (dl-list-do-backward-element
	   start-from
	   #'(lambda (x)
	       (when (dl-content x)
		 (let (pos)
		   (if first-time
		       (setf pos (search str (dl-content x)
					 :start2 (forward-start-pos
						  (dl-content x) search-pos))
			     first-time nil)
		       (setf pos (search str (dl-content x))))
		   (when pos
		     (setf (history-cur hist) x)
		     (return-from search-history pos)))))))))
  nil)

(defun isearch (e &optional (direction :backward))
  "Incremental search which updates the search position as the user types. The
search can be ended by typing a control character, which usually performs a
command, or Control-G which stops the search and returns to the start.
Control-R searches again backward and Control-S searches again forward."
  (with-slots (point buf cmd context) e
    (let ((quit-now nil)
	  (start-point point)
	  (start-hist (history-current-get context))
	  (search-string (make-stretchy-string *initial-line-size*))
	  (start-from (or (history-current-get context)
			  (history-head (get-history context))))
	  (pos point) old-pos c added)
      (labels ((redisp ()
		 (display-search e search-string pos))
	       (resync ()
		 (buffer-delete e 0 (length buf))
		 (buffer-insert e 0 (or (history-current (context e)) ""))
		 (setf point (min (or pos (length buf)) (length buf)))))
	(redisp)
	(loop :while (not quit-now)
	   :do
	   (when (debugging e)
	     (message e "pos = ~a start-from = ~a" pos start-from))
	   (display-search e search-string pos)
	   (setf c (get-a-char e)
		 added nil)
	   (cond
	     ((eql c (ctrl #\G))
	      (setf point start-point)
	      (setf (history-current context) start-hist)
	      (use-hist e)
	      (setf quit-now t))
	     ((eql c (ctrl #\S))
	      (setf direction :forward
		    start-from (search-start-forward context)))
	     ((eql c (ctrl #\R))
	      (setf direction :backward
		    start-from (search-start-backward context)))
	     ((eql c (ctrl #\L))
	      (redisp))
	     ((or (eql c (ctrl #\h)) (eql c #\backspace) (eql c #\rubout))
	      (stretchy-truncate search-string
				 (max 0 (1- (length search-string)))))
	     ((or (control-char-p c) (meta-char-p (char-code c)))
	      (resync)
	      (redraw e)
	      (return-from isearch c))
	     (t
	      (stretchy-append search-string c)
	      (setf added t)))
	   (if (setf pos (search-history
			  e search-string direction start-from pos))
	       (progn
		 (setf old-pos pos
		       point pos))
	       (progn
		 (when added
		   (stretchy-truncate search-string
				      (max 0 (1- (length search-string))))
		   (setf pos old-pos))
		 (beep e "Not found"))))
	(resync)
	(redraw e)))))

;; @@@ Consider calling redraw?
(defun redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt prompt-func point buf need-to-redraw) e
    (tt-clear) (tt-home)
    (setf (screen-col e) 0 (screen-row e) 0)
    (do-prompt e prompt prompt-func)
    (finish-output (terminal-output-stream
		    (line-editor-terminal e)))
    (display-buf e)
    (when (< point (length buf))
      (move-over e (- (- (length buf) point)) :start (length buf)))
    (setf need-to-redraw nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer editing

(defun insert-char (e c)
  "Insert a character into the buffer at point. Don't update the display."
  (declare (type character c))
  (buffer-insert e (point e) c))

(defun insert-string (e s)
  "Insert a string into the buffer at point. Don't update the display."
;  (declare (type string s))
  (buffer-insert e (point e) s))

(defun delete-region (e start end)
  "Delete the region of the buffer between the positions start and end.
Don't update the display."
  (with-slots (point buf) e
    (buffer-delete e start end)
    ;; Make sure the point stays in the buffer.
    (when (> point (fill-pointer buf))
      (setf point (fill-pointer buf)))))

(defun delete-backward-char (e)
  "Backward delete a character from buf at point"
  (with-slots (point buf) e
    (when (> point 0)
      (let ((del-len (display-length (aref buf (1- point)))))
	(move-over e -1)
	(buffer-delete e (1- point) point)
	(decf point)
	(tt-del-char del-len)
	(update-for-delete e del-len 1)))))

(defun delete-char (e)
  "Delete the character following the cursor."
  (with-slots (point buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(progn
	  (let ((del-len (display-length (aref buf point))))
	    (buffer-delete e point (1+ point))
	    (tt-del-char del-len)
	    (update-for-delete e del-len 1))))))

(defun delete-char-or-exit (e)
  "At the beginning of a blank line, exit, otherwise delete-char."
  (with-slots (point buf last-input quit-flag exit-flag) e
    (if (and (= point 0) (= (length buf) 0)
	     (not (eql last-input (ctrl #\d))))
	;; At the beginning of a blank line, we exit,
	;; so long as the last input wasn't ^D too.
	(setf quit-flag t
	      exit-flag t)
	(delete-char e))))

;;; Higher level editing functions that DO update the display

(defun backward-kill-word (e)
  (with-slots (buf point non-word-chars clipboard) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (let* ((region-str (subseq buf point start))
	     (del-len (display-length region-str)))
	(move-over e (- (- start point)) :start start)
	(tt-del-char del-len)
	(setf clipboard region-str)
	(buffer-delete e point start)
	(update-for-delete e del-len (- start point))))))

(defun kill-word (e)
  (with-slots (buf point non-word-chars clipboard) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (if (< point (length buf)) (incf point))
      (let* ((region-str (subseq buf start point))
	     (del-len (display-length region-str)))
	(tt-del-char del-len)
	(setf clipboard region-str)
	(buffer-delete e point start)
	(setf point start)
	(update-for-delete e del-len (- start point))))))

(defun kill-line (e)
  (with-slots (clipboard buf point screen-row screen-col) e
    (setf clipboard (subseq buf point))
    (let ((saved-row screen-row)
	  (saved-col screen-col))
      (move-over e (- (length buf) point))
      (tt-erase-to-eol)
      (loop :with row = screen-row
	 :while (> row saved-row) :do
	 (tt-beginning-of-line)
	 (tt-erase-to-eol)
	 (tt-up 1)
	 (decf row))
      (setf screen-row saved-row
	    screen-col saved-col)
      (tt-move-to screen-row screen-col)
      (tt-erase-to-eol)
      (buffer-delete e point (fill-pointer buf)))))

(defun backward-kill-line (e)
  (with-slots (point clipboard buf) e
    (when (> point 0)
      (setf clipboard (subseq buf 0 point))
      (replace-buffer e (subseq buf point))
      (beginning-of-line e))
    (clear-completions e)))

(defun yank (e)
  (with-slots (clipboard point) e
    (when clipboard
      (let ((len (length clipboard))
	    #| (disp-len (display-length clipboard)) |#)
	(insert-string e clipboard)
	;; (tt-ins-char disp-len)
	(display-buf e point (+ point len))
	(incf point len)
	(update-for-insert e)
	))))

(defun forward-word-action (e action)
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars :action action)
      (if (< point (length buf)) (incf point))
;      (move-forward e (display-length (subseq buf start point))))))
      (display-buf e start point))))

(defun apply-char-action-to-region (e char-action &optional beginning end)
  "Apply a function that takes a character and returns a character, to
every character in the region delimited by BEGINING and END. If BEGINING
and END aren't given uses the the current region, or gets an error if there
is none."
  (with-slots (point mark) e
    (if (or (not beginning) (not end))
	(error "Mark must be set if beginning or end not given."))
    (if (not beginning)
	(setf beginning (min mark point)))
    (when (not end)
      (setf end (max mark point)))
    (when (> end beginning)
      (rotatef end beginning))
    (let ((old-mark mark))
      (unwind-protect
	   (progn
	     (setf mark beginning)
	     (exchange-point-and-mark e)
	     (scan-over e :forward :func (constantly t) :action char-action))
	(setf mark old-mark)))))

(defun downcase-region (e &optional begining end)
  (apply-char-action-to-region e #'char-downcase begining end))

(defun upcase-region (e &optional begining end)
  (apply-char-action-to-region e #'char-upcase begining end))

(defun downcase-word (e)
  (forward-word-action e #'(lambda (c) (char-downcase c))))

(defun upcase-word (e)
  (forward-word-action e #'(lambda (c) (char-upcase c))))

(defun capitalize-word (e)
  (let (bonk)
    (forward-word-action e #'(lambda (c)
			       (if (not bonk)
				   (progn (setf bonk t) (char-upcase c))
				   (char-downcase c))))))

;; This is just an experiment to see how I would do it.
(defun un-studly-cap (e)
  "Convert from StupidVarName to stupid-var-name."
  (with-slots (point buf) e
    (record-undo e 'boundary)
    (let ((overall-start point) c start)
      (loop :do
	 (setf start point)
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))))
	 (scan-over
	  e :forward
	  :func #'(lambda (c) (and (alpha-char-p c) (lower-case-p c))))
	 (downcase-region e start point)
	 (setf c (aref buf point))
	 (when (and (alpha-char-p c) (upper-case-p c))
	   (insert-char e #\-))
	 :while (and (alpha-char-p c) (upper-case-p c)))
      (record-undo e 'boundary)
      (display-buf e overall-start point))))

(defun delete-horizontal-space (e)
  "Delete space before and after the cursor."
  (with-slots (buf point) e
    (let ((origin point)
	  start end del-len disp-len deleted-string
	  #| first-half-disp-len |#)
      (setf origin point)
      (scan-over e :forward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
      (setf end point
	    point origin)
      (scan-over e :backward
		 :func #'(lambda (c) (position c dlib::*whitespace*)))
      (setf start point
	    del-len (- end start)
	    deleted-string (subseq buf start end)
	    ;;first-half-disp-len (display-length (subseq buf start origin))
	    disp-len (display-length deleted-string))
      (delete-region e start end)
      ;;(move-backward e first-half-disp-len)
      (move-over e (- (- origin start)) :start origin)
      (tt-del-char disp-len)
      (update-for-delete e disp-len del-len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hacks for typing lisp

(defun flash-paren (e c)
  (declare (ignore c))
  (let* ((str (buf e))
	 (point (point e))
	 (ppos (matching-paren-position str :position point))
	 (offset (and ppos (1+ (- point ppos)))))
    (if ppos
	(let ((saved-col (screen-col e)))
	  (declare (ignore saved-col))
	  ;; The +1 to point is because we haven't incremented point for the
	  ;; close paren yet.
	  (move-over e (- offset) :start (1+ point))
	  (tt-finish-output)
	  (tt-listen-for .5)
	  (move-over e offset :start (- (1+ point) offset)))
	(beep e "No match."))))

(defun finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position buf)
       :do (insert-char e #\)) (display-char e #\)))
    (accept-line e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defvar *completion-list-technique* :under
  "Technique for dispalying completion lists.
:UNDER - Display the the completions under the command line.
:OVER  - Display the completion list like normal output scrolled up over the
command line.")

; XXX This should depend on the size of the screen.
(defvar *completion-really-limit* 100
  "How much is too much.")

(defun set-completion-count (e n)
  "Set the completion count to N."
  (setf *completion-count* n
	(last-completion-not-unique-count e) n))

;; Most of the work is done by print-columns, from dlib-misc.
(defun print-completions-over (e comp-list)
  (with-slots (completion-func buf point saved-point prompt prompt-func) e
    ;; downcased list 1 per line
    (let ((saved-point point))
      (end-of-line e)
      (setf point saved-point))
    (tt-write-char #\newline)
    #| (tt-format "~{~a~%~}" comp-list) |#
    (let ((len (length comp-list)))
      (when (> len *completion-really-limit*)
	(tt-format "Really list ~a things? (y or n) " len)
	(let ((chr (get-a-char e)))
	  (tt-write-char #\newline)
	  (when (not (equalp #\y chr))
	    (return-from print-completions-over))))
      (tt-write-string
       (with-output-to-string (str)
	 (print-columns comp-list :smush t
			:columns (terminal-window-columns
				  (line-editor-terminal e))
			:stream str))))
    (setf (screen-col e) 0)
    (do-prompt e prompt prompt-func)
    (display-buf e)
    (when (< point (length buf))
      (move-over e (- (- (length buf) point))))))

(defun figure-content-rows (e content)
  "Take a sequence of strings and return how many rows it takes up when output
to the terminal."
  ;; (apply #'+ (map 'list
  ;; 		  (_ (length
  ;; 		      (calculate-line-endings e
  ;; 		       :buffer (fat-string-to-string
  ;; 				(process-ansi-colors
  ;; 				 (make-fat-string _)))
  ;; 		       :start 0)))
  ;; 		  content)))
  (length (calculate-line-endings e
				  :buffer (fat-string-to-string
					   (process-ansi-colors
					    (make-fat-string content)))
				  :start 0)))

(defvar *completion-short-divisor* 3
  "Divisor of your screen height for short completion.")

(defun print-completions-under (e comp-list)
  (let* ((term (line-editor-terminal e))
	 (rows (terminal-window-rows term))
	 (cols (terminal-window-columns term))
	 (short-limit (truncate rows *completion-short-divisor*))
	 content-rows content-cols column-size
	 output-string
	 real-content-rows
	 rows-output
	 rows-scrolled
	 back-adjust
	 (x (screen-col e))
	 (y (screen-row e))
	 end-x end-y
	 row-limit)
    (declare (ignorable end-x column-size content-cols))
    ;;(multiple-value-setq (y x) (terminal-get-cursor-position term))
    (multiple-value-setq (content-rows content-cols column-size)
      (print-columns-sizer comp-list :columns cols))

    ;; Move over the rest of the input line.
    (move-over e (- (length (buf e)) (point e)))
    (tt-write-char #\newline)
    (tt-erase-below) (tt-finish-output)
    (setf (did-under-complete e) t)

    (setf row-limit
	  (if (and (< (last-completion-not-unique-count e) 2)
		   (< short-limit rows))
	      short-limit
	      (- (- rows 2)			  ;; minus the "more" line
		 (prompt-height e)
		 (- (screen-row e) (start-row e)) ;; height of the input line
		 ))
	  output-string
	  (with-output-to-string (str)
	    (setf content-rows
		  (print-columns comp-list :columns cols
				 :smush t :row-limit row-limit :stream str)))
	  real-content-rows (figure-content-rows e output-string)
	  rows-output (min real-content-rows row-limit))
    (tt-write-string output-string)
    ;; (tt-format "content-rows ~a rows-output ~s " content-rows rows-output)
    (when (plusp (- content-rows rows-output))
      (tt-format "[~d more lines]" (- content-rows row-limit)))
    (multiple-value-setq (end-y end-x) (terminal-get-cursor-position term))
    ;;(setf rows-scrolled (max 0 (- (+ y (1+ rows-output)) (1- rows)))
    (setf rows-scrolled (max 0 (- (+ y (1+ real-content-rows)) (1- rows)))
     	  back-adjust (+ (- end-y y) rows-scrolled))
    (log-message e "row-limit = ~s real-content-rows = ~s"
		 row-limit real-content-rows)
    (log-message e "rows-scrolled = ~s prompt-height = ~s" rows-scrolled
		 (prompt-height e))
    (tt-up back-adjust)
    (tt-beginning-of-line)
    (tt-move-to-col x)
    (setf (screen-row e) (- end-y back-adjust)
	  (screen-col e) x)))

(defun show-completions (e)
  (with-slots (completion-func buf point) e
    (if (not completion-func)
      (beep e "No completion installed.")
      (progn
	(multiple-value-bind (comp-list comp-count)
	    (funcall completion-func buf point t)
	  (when (and comp-count (> comp-count 0))
	    (setf (did-complete e) t)
	    (set-completion-count e (1+ (last-completion-not-unique-count e)))
	    (if (eq *completion-list-technique* :under)
		(print-completions-under e comp-list)
		(print-completions-over e comp-list))))))))

#|
(defun last-input-was-completion (e)
  "Return true if the last input invoked a completion function."
  (with-slots (last-input completion-func) e
    (let ((func (key-sequence-binding last-input *normal-keymap*)))
      (log-message e "func = ~s" func)
      ;; @@@ This is not really ideal. Perhaps we should specify that completion
      ;; functions should set another flag, or that last last-completion-unique
      ;; also specifies that last-input-was-completion?
      (or (eql func completion-func)
	  (eql func #'complete)
	  (eql func 'complete)
	  (eql func #'complete-filename-command)
	  (eql func 'complete-filename-command)))))
|#

(defun complete (e &optional comp-func)
  "Call the completion function and display the results, among other things."
  (with-slots (completion-func point buf last-input) e
    (setf comp-func (or comp-func completion-func))
    (when (not comp-func)
      (beep e "No completion active.")
      (return-from complete))
    (let* ((saved-point point) comp replace-pos unique)
      (multiple-value-setq (comp replace-pos unique)
	(funcall comp-func buf point nil))
      (when (and (not (zerop (last-completion-not-unique-count e)))
		 (last-command-was-completion e))
	(log-message e "show mo")
	(show-completions e))
      (setf (did-complete e) t)
      (if (not unique)
	  (set-completion-count e (1+ (last-completion-not-unique-count e)))
	  (set-completion-count e 0))
      ;; (format t "comp = ~s replace-pos = ~s~%" comp replace-pos)
      ;; If the completion succeeded we need a replace-pos!
      (assert (or (not comp) (numberp replace-pos)))
      (if comp
	  (let* ((same (- saved-point replace-pos))) ; same part
	    ;; f o o b a r
	    ;;       ^    ^
	    ;;       |    |___ saved-point
	    ;;       |
	    ;;       +-- replace-pos
	    ;;
	    ;; f o o b a r n a c l e

	    ;; back up
	    (move-over e (- same))

	    ;; delete the different part
	    (delete-region e replace-pos saved-point)
	    (setf point replace-pos)
	    (insert-string e comp)

	    ;; write out the new part
	    (editor-write-string e comp)
	    (incf point (length comp))
	    (update-for-insert e))
	  (progn
	    (setf point saved-point)		   ; go back to where we were
	    (beep e "No completions"))))))	   ; ring the bell

(defun complete-filename-command (e)
  "Filename completion. This useful for when you want to explicitly complete a
filename instead of whatever the default completion is. Convenient for a key
binding."
  (complete e #'completion::complete-filename))

(defun pop-to-lish (e)
  "If we're inside lish, throw to a quick exit. If we're not in lish, enter it."
  (let* ((lish-package (find-package :lish))
	 (level-symbol (intern "*LISH-LEVEL*" lish-package)))
    (when lish-package
      (if (and (boundp level-symbol) (numberp (symbol-value level-symbol)))
	  (funcall (find-symbol "LISHITY-SPLIT" :lish))
	  (progn
	    (tt-beginning-of-line)
	    (tt-erase-line)
	    (finish-output (terminal-output-stream (line-editor-terminal e)))
	    (terminal-end (line-editor-terminal e))
	    (if (line-editor-terminal-device-name e)
		(funcall (find-symbol "LISH" :lish)
			 :terminal-name (line-editor-terminal-device-name e))
		(funcall (find-symbol "LISH" :lish)))
	    (tt-beginning-of-line)
	    (tt-erase-line)
	    (setf (screen-col e) 0)
	    (with-slots (prompt prompt-func point buf) e
	      (do-prompt e prompt prompt-func)
	      (display-buf e)
	      (when (< point (length buf))
		(move-backward e (display-length (subseq buf point)))))
	    (terminal-start (line-editor-terminal e)))))))

(defun abort-command (e)
  "Invoke the debugger from inside."
  (declare (ignore e))
  ;; Maybe this should just flash the screen?
  ;; (with-simple-restart (continue "Continue TINY-RL")
  ;;   (invoke-debugger (make-condition
  ;; 		      'simple-condition
  ;; 		      :format-control "Abort command")))
  (abort))

(defun toggle-debugging (e)
  "Toggle debugging output."
  (with-slots (debugging) e
    (setf debugging (not debugging))))

(defun quoted-insert (e)
  "Insert the next character input without interpretation."
  (setf (cmd e) (get-a-char e))
  (self-insert e t))

(defun self-insert (e &optional quoted char)
  "Try to insert a character into the buffer."
  (with-slots (cmd buf point screen-col) e
    (when (not char)
      (setf char cmd))
    (cond
      ((not (characterp char))
       (beep e "~a is not a character." char))
      ((and (not (graphic-char-p char)) (not quoted))
       (beep e "~a is unbound." char))
      (t
       ;; a normal character
       (if (= (length buf) point)
	   ;; end of the buf
	   (progn
	     (display-char e char)
	     (insert-char e char)
	     ;; flash paren and keep going
	     (when (or (eql char #\)) (eql char #\]) (eql char #\}))
	       (flash-paren e char))
	     (incf point))
	   ;; somewhere in the middle
	   (progn
	     (let* ((endings (calculate-line-endings e))
		    (end (line-ending point endings))
		    (len (display-length char))
		    (at-len (display-length (aref buf point)))
		    (cols (terminal-window-columns (line-editor-terminal e)))
		    prev-end)
	       (cond
		 ;; We're at a wide char at EOL and inserting a smaller one.
		 ((and (and end (< end screen-col))
		       (< at-len len))
		  (tt-erase-to-eol)
		  (tt-down 1)
		  (tt-beginning-of-line)
		  (tt-ins-char len))
		 ;; We're at the beginning with an empty col on the previous
		 ((and (= screen-col 0)
		       (setf prev-end (line-ending (1- point) endings))
		       (< prev-end cols))
		  (tt-backward 1))
		 (t
		  ;; normal
		  (tt-ins-char len)))
	       (display-char e char)
	       (when (or (eql char #\)) (eql char #\]) (eql char #\}))
		 (flash-paren e char))
	       (insert-char e char)
	       (incf point)
	       ;; do the dumb way out
	       (update-for-insert e))))))))

(defun read-key-sequence (e &optional keymap)
  "Read a key sequence from the user. Descend into keymaps.
 Return a key or sequence of keys."
  (let* ((c (get-a-char e))
	 (action (key-definition c (or keymap (line-editor-keymap e)))))
    (if (and (symbolp action) (boundp action) (keymap-p (symbol-value action)))
	(let ((result-seq (read-key-sequence e (symbol-value action))))
	  (if (listp result-seq)
	      (append (list c) result-seq)
	      (list c result-seq)))
	c)))

(defun ask-function-name (&optional (prompt "Function: "))
  "Prompt for a function name and return symbol of a function."
  (let* ((str (tiny-rl :prompt prompt :context :ask-function-name))
	 (cmd (and str (stringp str)
		   (ignore-errors (safe-read-from-string str)))))
    (and (symbolp cmd) (fboundp cmd) cmd)))

(defun set-key-command (e)
  "Bind a key interactively."
  (tmp-prompt e "Set key: ")
  (let* ((key-seq (read-key-sequence e))
	 (cmd (ask-function-name (format nil "Set key ~a to command: "
					 (key-sequence-string key-seq)))))
    (if cmd
	(set-key key-seq cmd (line-editor-local-keymap e))
	(tmp-message e "Not a function."))))

(defun describe-key-briefly (e)
  "Tell what function a key invokes."
  (tmp-prompt e "Describe key: ")
  (let* ((key-seq (read-key-sequence e))
	 (def (key-sequence-binding key-seq (line-editor-keymap e))))
    (if def
	(tmp-message e "Describe key: ~w is bound to ~a"
		     (key-sequence-string key-seq) def)
	(tmp-message e "Describe key: ~w is not bound"
		     (key-sequence-string key-seq)))))

(defun exit-editor (e)
  "Stop editing."
  (with-slots (quit-flag exit-flag) e
    (setf quit-flag t
	  exit-flag t)))

(defun unipose-command (e)
  "Compose unicode characters."
  (let ((first-ccc (get-a-char e)) second-ccc result)
    (setq second-ccc (get-a-char e))
    (setq result (unipose first-ccc second-ccc))
    (if result
	(self-insert e t result)
	(beep e "unipose ~c ~c unknown" first-ccc second-ccc))))

(defmacro with-external ((e) &body body)
  (with-unique-names (result)
    `(let (,result)
       (finish-output (terminal-output-stream (line-editor-terminal ,e)))
       (terminal-end (line-editor-terminal ,e))
       (setf ,result (progn ,@body))
       (terminal-start (line-editor-terminal ,e))
       (redraw-command ,e) 			; maybe could do better?
       ,result)))

(defun char-picker-command (e)
  "Pick unicode (or whatever) characters."
  (let ((result
	 (with-external (e)
	   (when (not (find-package :char-picker))
	     (asdf:load-system :char-picker))
	   (symbol-call :char-picker :char-picker))))
    (if result
	(self-insert e t result)
	(beep e "char-picker failed"))))

(defkeymap *normal-keymap*
  `(
    ;; Movement
    (,(ctrl #\B)		. backward-char)
    (,(ctrl #\F)		. forward-char)
    (,(ctrl #\A)		. beginning-of-line)
    (,(ctrl #\E)		. end-of-line)
    (,(ctrl #\P)		. previous-history)
    (,(ctrl #\N)		. next-history)
    (,(meta-char #\b)		. backward-word)
    (,(meta-char #\f)		. forward-word)
    (,(meta-char #\<)		. beginning-of-history)
    (,(meta-char #\>)		. end-of-history)

    ;; Editing
    (#\return			. accept-line)
    (#\newline			. accept-line)
    (#\backspace		. delete-backward-char)
    (#\rubout			. delete-backward-char)
    (,(ctrl #\D)		. delete-char-or-exit)
    (,(ctrl #\W)		. backward-kill-word)
    (,(ctrl #\K)		. kill-line)
    (,(ctrl #\@)		. set-mark)
    (,(ctrl #\Y)		. yank)
    (,(ctrl #\U)		. backward-kill-line)
    (,(ctrl #\O)		. undo-command)
    (,(meta-char #\d)		. kill-word)
    (,(meta-char #\rubout)	. backward-kill-word)
    (,(meta-char #\u)		. upcase-word)
    (,(meta-char #\l)		. downcase-word)
    (,(meta-char #\c)		. capitalize-word)
    (,(meta-char #\w)		. copy-region)
    (,(meta-char #\\)		. delete-horizontal-space)

    ;; Completion
    (#\tab			. complete)
    (#\?			. show-completions)
    (,(meta-char #\/)		. complete-filename-command)

    ;; Misc
    (,(ctrl #\L)		. redraw-command)
    (,(ctrl #\G)		. abort-command)
    (,(ctrl #\S)		. isearch-forward)
    (,(ctrl #\R)		. isearch-backward)
    (,(ctrl #\T)		. toggle-debugging) ; @@@ temporary?
    (,(ctrl #\Q)		. quoted-insert)

    ;; key binding
    (,(meta-char #\=)		. describe-key-briefly)
    (,(meta-char #\+)		. set-key-command)

    ;; dorky temporary
    (,(meta-char #\t)		. un-studly-cap)
    
    ;; Other keymaps
    (#\escape			. *escape-keymap*)
    (,(ctrl #\X)		. *ctlx-keymap*)
    )
  :default-binding 'self-insert
)

;; These ^X commands are quite impoverished.
(defkeymap *ctlx-keymap*
  `(
    ;; (,(ctrl #\F) (edit-function))
    ;; (,(ctrl #\Q) (toggle-read-only))
    ;; (,(ctrl #\S) (save-history))
    ;; (,(ctrl #\Z) (suspend))
    ;; (#\h (hyperspec-lookup))
    ;; (#\i (insert-file))
    ;; (#\= (describe-cursor-position))
    ;; (#\! (shell-command))
    ;; (#\( (start-macro))
    ;; (#\) (end-macro))
    (#\9		. unipose-command)
    (#\7		. char-picker-command)
    (,(ctrl #\C)	. exit-editor)
    (,(ctrl #\X)	. exchange-point-and-mark)))
;  :default-binding #| (beep e "C-x ~a is unbound." cmd |#

#|
(defkeymap *app-key-keymap*
  `((#\A . previous-history)		; :up
    (#\B . next-history)		; :down
    (#\C . forward-char) 		; :right
    (#\D . backward-char) 		; :left
    ;; Movement keys
    (#\H . beginning-of-line) 		; :home
    (#\F . end-of-line) 		; :end
    ;; Function keys
;    (#\P . ) 				; :f1
;    (#\Q . ) 				; :f2
;    (#\R . ) 				; :f3
;    (#\S . ) 				; :f4
    ))

(defkeymap *escape-raw-keymap*
  `(
    ;;(#\O	. do-app-key)
    (#\O	. *app-key-keymap*)
    (#\[	. do-function-key)
    (#\newline  . finish-line)
   ))
|#

(defkeymap *special-keymap*
  `(
    (:left      . backward-char)
    (:right     . forward-char)
    (:up        . previous-history)
    (:down      . next-history)
    (:backspace . delete-backward-char)
    (:home      . beginning-of-line)
    (:end       . end-of-line)
    ;; XXX @@@ this shouldn't be here. It should be in the repl or lish
    (:f9        . pop-to-lish)
    ))

;; Normal mode commands prefaced by escape.
(defparameter *escape-keymap*
  ;;(add-keymap (build-escape-map *normal-keymap*) *escape-raw-keymap*))
  (build-escape-map *normal-keymap*))

;; Make the stuff in the special keymap appear in the normal keymap too.
(add-keymap *special-keymap* *normal-keymap*)

;; @@@ do we really need this?
;; (defun bad-special-key (e)
;;   (beep e "Bad special key ~s." key))

#|
(defun do-app-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-app-key e)))))

(defun do-function-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-function-key e)))))

(defun do-special-key (e key)
  (perform-key e key *special-keymap*))
|#

;; Key bindings can be a list to apply, or a symbol bound to function to call,
;; or a keymap in which to look up further key presses.
(defun perform-key (e key keymap)
  "Perform the action for the key in the keymap."
  (let* ((action (key-definition key keymap)))
    (cond
      ((not action)
       (beep e "Key ~a is not bound in keymap ~w." (nice-char key) keymap)
       (return-from perform-key))
      ;; a list to apply
      ((consp action)
       ;;(dbug "action is a cons~%")
       (if (fboundp (car action))
	   (apply (car action) (cdr action))
	   (beep e "(~S) is not defined." (car action))))
      ;; something represted by a symbol
      ((symbolp action)
       (cond
	 ((fboundp action)		; a function
	  (funcall action e))
	 ((keymap-p (symbol-value action)) ; a keymap
	  (setf (cmd e) (get-a-char e))
	  (log-message e "keymap cmd ~s" (cmd e))
	  (perform-key e (cmd e) (symbol-value action)))
	 (t				; anything else
	  (beep e "Key binding ~S is not a function or a keymap." action))))
      ;; a function object
      ((functionp action)
       ;;(dbug "action is a function~%")
       (funcall action e))
      (t					; anything else is an error
       (error "Weird thing in keymap: ~s." action)))))

(defvar *terminal-name* nil
  "Device name of the terminal to use for input.")

;; The main entry point

(defun tiny-rl (&key (input-stream *standard-input*)
		  (eof-error-p t)
		  (eof-value nil)
		  (quit-value nil)
		  (recursive-p nil)
		  (prompt *default-prompt*)
		  (output-prompt-func nil)
		  (completion-func #'complete-symbol)
		  (string nil)
		  (in-callback nil)
		  (out-callback nil)
		  (debug nil)
		  (editor nil)
		  (local-keymap nil)
		  (keymap nil)
		  (terminal-name *terminal-name*)
		  (terminal-class 'terminal-ansi)
		  (accept-does-newline t)
		  (context :tiny))
  "Read a line from the terminal, with line editing and completion.
Return the string read and the line-editor instance created.
Keyword arguments: 
  EOF-ERROR-P (T)                 
    True to signal an error on end of file.
  EOF-VALUE (nil)
    Value to return on end of file. 
  QUIT-VALUE (nil)
    Value to return if the user quit.
  PROMPT (*default-prompt*)
    String to prompt with.
  OUTPUT-PROMPT-FUNC (nil)
    Function to print out a prompt. Called with the LINE-EDITOR instance and a
    prompt string.
  COMPLETION-FUNC (#'complete-symbol)
    Completion function to use. See the completion package for details.
  EDITOR (nil)
    LINE-EDITOR instance to use.
  LOCAL-KEYMAP (nil)
    A LOCAL-KEYMAP to use. For when you want to add your own customized key
    bindings.
  KEYMAP (nil)
    A KEYMAP to use. If you want to completely replace all the key bindings
    by your own. This defaults to a list of (LOCAL-KEYMAP *NORMAL-KEYMAP*).
  TERMINAL-NAME (*terminal-name*)
    Name of a terminal device to use. If NIL 
  ACCEPT-DOES-NEWLINE (t)
    True if accept-line outputs a newline.
  CONTEXT (nil)
    Symbol or string which defines the context for keeping history.
"			    ; There must be a better way to format docstrings.
  (declare (ignore recursive-p))
  (history-init context)

  ;; Initialize the buffer
  (let* ((e (or editor (make-instance
			'line-editor
			:point 0
			:prompt prompt
			:prompt-func output-prompt-func
			:completion-func completion-func
			:context context
			:in-callback in-callback
			:out-callback out-callback
			:debugging debug
			:local-keymap local-keymap
			:keymap keymap
			:accept-does-newline accept-does-newline
			:terminal-device-name terminal-name
			:terminal-class terminal-class)))
	 (*terminal* (line-editor-terminal e))
	 (*completion-count* 0))
    (when editor
      (freshen editor))
    (setf (fill-pointer (buf e)) (point e))
    #+ccl (setf ccl::*auto-flush-streams* nil)
    (terminal-start (line-editor-terminal e))

    ;; Add the new line we're working on.
    (history-add context nil)
    (history-next context)

    ;; Output the prompt
    (setf (prompt e) prompt (prompt-func e) output-prompt-func)
    (do-prompt e (prompt e) (prompt-func e))
    (when string
      (without-undo (e)
	(buffer-insert e 0 string)
	(setf (point e) (length string))
	(display-buf e 0)))

    ;; Command loop
    (with-slots (quit-flag exit-flag cmd buf point last-input terminal
		 debugging) e
      (let ((result nil))
	(unwind-protect
	     (loop :do
		(finish-output)
		(when debugging
		  (message e "~d ~d [~d x ~d] ~a ~w"
			   (screen-col e) (screen-row e)
			   (terminal-window-columns terminal)
			   (terminal-window-rows terminal)
			   point cmd)
		  (show-message-log e))
		(setf cmd (get-a-char e))
		(log-message e "cmd ~s" cmd)
		(when (need-to-redraw e)
		  (redraw e))
		(if (equal cmd '(nil))
		    (if eof-error-p
			(error (make-condition 'end-of-file
					       :stream input-stream))
			(setf result eof-value))
		    (progn
		      (setf (did-complete e) nil)
		      (perform-key e cmd (line-editor-keymap e))
		      (setf (last-command-was-completion e) (did-complete e))
		      (when (not (last-command-was-completion e))
			(set-completion-count e 0))
		      (when exit-flag (setf result quit-value))))
		(setf last-input cmd)
		:while (not quit-flag))
	  (block nil
	    (tt-finish-output)
	    (terminal-end terminal)))
	(values (if result result buf) e)))))

;; This is for compatability with read-line.
(defun tiny-read-line (&optional (input-stream *standard-input*)
				 (eof-error-p t)
				 (eof-value nil)
				 (recursive-p nil)
				 (prompt ""))
  "Replacement for read-line, with line editing."
  (tiny-rl :input-stream input-stream
	   :eof-error-p eof-error-p
	   :eof-value eof-value
	   :recursive-p recursive-p
	   :prompt prompt))

(defun read-filename (&key (prompt *default-prompt*))
  "Read a file name."
  (let (filename editor)
    (loop :do
       (setf (values filename editor)
	     (tiny-rl :prompt prompt
		      :completion-func #'complete-filename
		      :context :read-filename
		      :accept-does-newline nil
		      :editor editor))
       :until (probe-file filename)
       :do (tmp-message editor "File not found."))
    filename))

(defun read-choice (list &key (prompt *default-prompt*))
  "Read a choice from a list."
  (let (item editor)
    (loop :do
       (setf (values item editor)
	     (tiny-rl :prompt prompt
		      :completion-func (list-completion-function list)
		      :context :read-choice
		      :accept-does-newline nil
		      :editor editor))
       :until (position item list :key #'princ-to-string :test #'equal)
       :do (tmp-message editor "~a is not a valid choice." item))
    item))

;; EOF
