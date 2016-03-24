;;
;; tiny-rl.lisp - An input line editor for ANSI terminals.
;;

;; TODO:
;;   - multi line - display bugs (undo, delete word, etc)
;;     - fix update-for-delete
;;   - tab display bugs
;;   - history saving
;;   - M-\ delete-horizontal-space
;;   - search by :UP (or C-P)
;;   - fix undo boundaries
;;   - unicode input gadget (from neox)
;;   - change the the name

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defpackage "TINY-RL"
  (:use :cl :dlib :dlib-misc :keymap :char-util :dl-list :stretchy :cffi
	:opsys :terminal :terminal-ansi :terminal-curses
	:completion :syntax-lisp :unipose)
  (:documentation
   "A readline replacement for ANSI terminals.")
  (:export
   ;; main functionality
   #:tiny-read-line
   #:tiny-rl
   #:line-editor
   #:show-history
   #:history-clear
   #:*default-prompt*
   #:*really-limit*
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
  (let ((str (princ-to-string (if prompt-supplied p *default-prompt*))))
    (editor-write-string e str)
    str))

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
   (last-command-was-completion
    :initarg :last-command-was-completion
    :accessor last-command-was-completion
    :initform nil
    :type boolean
    :documentation "True if the last command was a completion.")
   (last-completion-not-unique
    :accessor last-completion-not-unique
    :initarg :last-completion-not-unique
    :initform nil
    :type boolean
    :documentation "True if the last completion and was not unique.")
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
    :initform nil
    :documentation "The keymap.")
   (accept-does-newline
    :accessor accept-does-newline
    :initarg :accept-does-newline
    :initform t :type boolean
    :documentation "True if accept-line outputs a newline.")
   )
  (:default-initargs
    :non-word-chars *default-non-word-chars*
    :prompt *default-prompt*
    :keymap *normal-keymap*
    :terminal-class 'terminal-ansi
  )
  (:documentation "State for a stupid little line editor."))

(defvar *initial-line-size* 20)

(defmethod initialize-instance :after ((e line-editor) &rest initargs)
  (declare (ignore initargs))
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
  ;; Make a default line sized buffer if one wasn't given.
  (when (or (not (slot-boundp e 'buf)) (not (slot-value e 'buf)))
    (setf (slot-value e 'buf) (make-stretchy-string *initial-line-size*)))
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
	(exit-flag e)		nil))

(defun get-a-char (e)
  "Read a character from the editor's tty."
  (declare (type line-editor e))
  (tt-finish-output e)
  (let ((c (tt-get-key e)))
    ;; when read returns eagain,
    ;; (terminal-start tty) (redraw e) (tt-finish-output e)
    (when (line-editor-in-callback e)
      (funcall (line-editor-in-callback e) c))
    c))

#|
(defun get-a-char (e)
  "Read a character from the editor's tty."
  (declare (type line-editor e))
  (tt-finish-output e)
  (with-slots (typeahead typeahead-pos) e
    (when typeahead
      (return-from get-a-char
	(prog1
	    (aref typeahead typeahead-pos)
	  (incf typeahead-pos)
;	  (format t "ta->~a~%" (incf typeahead-pos))
	  (when (>= typeahead-pos (length typeahead))
	    (setf typeahead nil))))))
  (with-foreign-object (c :unsigned-char)
    (let (status (tty (line-editor-terminal e)))
      (loop
	 :do (setf status (posix-read (terminal-file-descriptor tty) c 1))
	 :if (and (< status 0) (or (= *errno* +EINTR+) (= *errno* +EAGAIN+)))
	 :do
	   ;; Probably returning from ^Z or terminal resize, or something,
	   ;; so redraw and keep trying. Enjoy your trip to plusering town.
	   (terminal-start tty) (redraw e) (tt-finish-output e)
	 :else
	   :return
	 :end)
      (cond
	((< status 0)
	 (error "Read error ~d ~d ~a~%" status nos:*errno*
		(nos:strerror nos:*errno*)))
	((= status 0) ; Another possible plusering extravaganza
	 nil)
	((= status 1)
	 (when (line-editor-in-callback e)
	   (let ((cc (code-char (mem-ref c :unsigned-char))))
	     (funcall (line-editor-in-callback e) cc)
	     cc))
	 (code-char (mem-ref c :unsigned-char)))))))
|#

(defun get-lone-key ()
  "Get a key, but easily usable from outside the editor. Don't use this for
anything serious."
  (terminal-start (line-editor-terminal *line-editor*))
  (unwind-protect
    (progn
      (get-a-char *line-editor*))
    (terminal-end (line-editor-terminal *line-editor*))))

; (defun read-until (tty c &key timeout)
;   "Read characters from the tty until the character C is read, or TIMEOUT
; seconds have elapsed. Return the result as a string. If the timeout is hit
; before C is read, return nil. If timeout is not specified, just hang until C
; is read."
;   (finish-output (terminal-output-stream tty))
;   (let ((fd (terminal-file-descriptor tty)))
;     @@@ set the timeout with tcsetattr
;     (with-foreign-object (c :unsigned-char)
;       (loop @@@@@ until something
; 	  (let ((status (posix-read fd c 1)))
; 	    (cond
; 	      ((< status 0)
; 	       (error "Read error ~d~%" status))
; 	      ((= status 0)
; 	       (return nil))
; 	      ((= status 1)
; 	       (string
; 		(code-char (mem-ref c :unsigned-char)))))))

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

;; Perhaps we should consider refactoring some part of get-a-char?
(defun read-utf8-char (e)
  "Read one UTF-8 character from the terminal of the line-editor E and return it."
  (declare (type line-editor e))
  (tt-finish-output e)
  (with-foreign-object (c :unsigned-char)
    (let (status (tty (line-editor-terminal e)))
      (loop
	 :do (setf status (posix-read (terminal-file-descriptor tty) c 1))
	 :if (and (< status 0) (or (= *errno* +EINTR+) (= *errno* +EAGAIN+)))
	 :do
	   (terminal-start tty) (redraw e) (tt-finish-output e)
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

(defgeneric undo-one-item (e item)
  (:documentation "Undo an undo item.")
  (:method (e (item boundary)) (declare (ignore e)) #| do nothing |# )
  (:method (e (item deletion))
    (buffer-insert e (undo-item-position item) (undo-item-data item))
    (setf (point e) (undo-item-point item)))
  (:method (e (item insertion))
     (buffer-delete
      e (undo-item-position item) (+ (undo-item-position item)
				     (length (undo-item-data item))))
     (setf (point e) (undo-item-position item))))

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
  (redraw e)) ;; @@@ This is overkill! (and screws up multiline prompts)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make line-editor versions of the tt- funcs for code readability

(defmethod tt-format ((e line-editor) fmt &rest args)
  (apply #'tt-format (line-editor-terminal e) fmt args))

(defmacro tt-alias (m &rest args)
  `(defmethod ,m ((e line-editor) ,@args)
    (,m (line-editor-terminal e) ,@args)))

(tt-alias tt-write-char c)
(tt-alias tt-write-string s)
(tt-alias tt-move-to row col)
(tt-alias tt-move-to-col col)
(tt-alias tt-beginning-of-line)
(tt-alias tt-del-char n)
(tt-alias tt-ins-char n)
(tt-alias tt-backward n)
(tt-alias tt-forward n)
(tt-alias tt-up n)
(tt-alias tt-down n)
(tt-alias tt-scroll-down n)
(tt-alias tt-erase-to-eol)
(tt-alias tt-erase-line)
(tt-alias tt-clear)
(tt-alias tt-home)
(tt-alias tt-cursor-off)
(tt-alias tt-cursor-on)
(tt-alias tt-standout state)
(tt-alias tt-underline state)
(tt-alias tt-beep)
(tt-alias tt-finish-output)
(tt-alias tt-get-key)
(tt-alias tt-get-char)
(tt-alias tt-listen-for n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-ish code

(defun message (e fmt &rest args)
  "Show a little message for debugging."
  (declare (type line-editor e))
  (tt-cursor-off e)
  (tt-move-to e 5 5)		; The only place we should call this
  (tt-standout e t)
  (apply #'tt-format (cons (line-editor-terminal e) (cons fmt args)))
  (tt-standout e nil)
  (tt-cursor-on e))

(defun log-message (e fmt &rest args)
  (when (debugging e)
    (push (apply #'format nil fmt args) (line-editor-debug-log e))))

(defun show-message-log (e)
  "Show the debugging message log."
  (declare (type line-editor e))
  (tt-cursor-off e)
  (loop :for i :from 0 :below 8
     :for dd :in (line-editor-debug-log e)
     :do
     (tt-move-to e (+ 10 i) 40)		; The “only” place we should call this
     (tt-erase-to-eol e)
     (tt-write-string e dd))
  (tt-cursor-on e))

(defun message-pause (e fmt &rest args)
  "Show a little message for debugging."
  (apply #'message e fmt args)
  (get-a-char e))

(defun beep (e fmt &rest args)
  "Beep or display an error message."
  (when (debugging e)
    (apply #'message e fmt args))
  (tt-beep e))

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

(defun control-char-graphic (c)
  (when (not *control-char-graphics*)
    (setf *control-char-graphics* (make-hash-table :test #'eql))
    (alist-to-hash-table *control-char-graphics-vec*
			 *control-char-graphics*))
  (gethash c *control-char-graphics*))

(defgeneric display-length (obj)
  (:documentation "Return how long is the object should be when displayed."))

(defparameter *combining-chars*
  (vector
   (code-char #x00000300) ; ̀ COMBINING_GRAVE_ACCENT
   (code-char #x00000301) ; ́ COMBINING_ACUTE_ACCENT
   (code-char #x00000302) ; ̂ COMBINING_CIRCUMFLEX_ACCENT
   (code-char #x00000303) ; ̃ COMBINING_TILDE
   (code-char #x00000304) ; ̄ COMBINING_MACRON
   (code-char #x00000305) ; ̅ COMBINING_OVERLINE
   (code-char #x00000306) ; ̆ COMBINING_BREVE
   (code-char #x00000307) ; ̇ COMBINING_DOT_ABOVE
   (code-char #x00000308) ; ̈ COMBINING_DIAERESIS
   (code-char #x00000309) ; ̉ COMBINING_HOOK_ABOVE
   (code-char #x0000030A) ; ̊ COMBINING_RING_ABOVE
   (code-char #x0000030B) ; ̋ COMBINING_DOUBLE_ACUTE_ACCENT
   (code-char #x0000030C) ; ̌ COMBINING_CARON
   (code-char #x0000030D) ; ̍ COMBINING_VERTICAL_LINE_ABOVE
   (code-char #x0000030E) ; ̎ COMBINING_DOUBLE_VERTICAL_LINE_ABOVE
   (code-char #x0000030F) ; ̏ COMBINING_DOUBLE_GRAVE_ACCENT
   (code-char #x00000310) ; ̐ COMBINING_CANDRABINDU
   (code-char #x00000311) ; ̑ COMBINING_INVERTED_BREVE
   (code-char #x00000312) ; ̒ COMBINING_TURNED_COMMA_ABOVE
   (code-char #x00000313) ; ̓ COMBINING_COMMA_ABOVE
   (code-char #x00000314) ; ̔ COMBINING_REVERSED_COMMA_ABOVE
   (code-char #x00000315) ; ̕ COMBINING_COMMA_ABOVE_RIGHT
   (code-char #x00000316) ; ̖ COMBINING_GRAVE_ACCENT_BELOW
   (code-char #x00000317) ; ̗ COMBINING_ACUTE_ACCENT_BELOW
   (code-char #x00000318) ; ̘ COMBINING_LEFT_TACK_BELOW
   (code-char #x00000319) ; ̙ COMBINING_RIGHT_TACK_BELOW
   (code-char #x0000031A) ; ̚ COMBINING_LEFT_ANGLE_ABOVE
   (code-char #x0000031B) ; ̛ COMBINING_HORN
   (code-char #x0000031C) ; ̜ COMBINING_LEFT_HALF_RING_BELOW
   (code-char #x0000031D) ; ̝ COMBINING_UP_TACK_BELOW
   (code-char #x0000031E) ; ̞ COMBINING_DOWN_TACK_BELOW
   (code-char #x0000031F) ; ̟ COMBINING_PLUS_SIGN_BELOW
   (code-char #x00000320) ; ̠ COMBINING_MINUS_SIGN_BELOW
   (code-char #x00000321) ; ̡ COMBINING_PALATALIZED_HOOK_BELOW
   (code-char #x00000322) ; ̢ COMBINING_RETROFLEX_HOOK_BELOW
   (code-char #x00000323) ; ̣ COMBINING_DOT_BELOW
   (code-char #x00000324) ; ̤ COMBINING_DIAERESIS_BELOW
   (code-char #x00000325) ; ̥ COMBINING_RING_BELOW
   (code-char #x00000326) ; ̦ COMBINING_COMMA_BELOW
   (code-char #x00000327) ; ̧ COMBINING_CEDILLA
   (code-char #x00000328) ; ̨ COMBINING_OGONEK
   (code-char #x00000329) ; ̩ COMBINING_VERTICAL_LINE_BELOW
   (code-char #x0000032A) ; ̪ COMBINING_BRIDGE_BELOW
   (code-char #x0000032B) ; ̫ COMBINING_INVERTED_DOUBLE_ARCH_BELOW
   (code-char #x0000032C) ; ̬ COMBINING_CARON_BELOW
   (code-char #x0000032D) ; ̭ COMBINING_CIRCUMFLEX_ACCENT_BELOW
   (code-char #x0000032E) ; ̮ COMBINING_BREVE_BELOW
   (code-char #x0000032F) ; ̯ COMBINING_INVERTED_BREVE_BELOW
   (code-char #x00000330) ; ̰ COMBINING_TILDE_BELOW
   (code-char #x00000331) ; ̱ COMBINING_MACRON_BELOW
   (code-char #x00000332) ; ̲ COMBINING_LOW_LINE
   (code-char #x00000333) ; ̳ COMBINING_DOUBLE_LOW_LINE
   (code-char #x00000334) ; ̴ COMBINING_TILDE_OVERLAY
   (code-char #x00000335) ; ̵ COMBINING_SHORT_STROKE_OVERLAY
   (code-char #x00000336) ; ̶ COMBINING_LONG_STROKE_OVERLAY
   (code-char #x00000337) ; ̷ COMBINING_SHORT_SOLIDUS_OVERLAY
   (code-char #x00000338) ; ̸ COMBINING_LONG_SOLIDUS_OVERLAY
   (code-char #x00000339) ; ̹ COMBINING_RIGHT_HALF_RING_BELOW
   (code-char #x0000033A) ; ̺ COMBINING_INVERTED_BRIDGE_BELOW
   (code-char #x0000033B) ; ̻ COMBINING_SQUARE_BELOW
   (code-char #x0000033C) ; ̼ COMBINING_SEAGULL_BELOW
   (code-char #x0000033D) ; ̽ COMBINING_X_ABOVE
   (code-char #x0000033E) ; ̾ COMBINING_VERTICAL_TILDE
   (code-char #x0000033F) ; ̿ COMBINING_DOUBLE_OVERLINE
   (code-char #x00000340) ; ̀ COMBINING_GRAVE_TONE_MARK
   (code-char #x00000341) ; ́ COMBINING_ACUTE_TONE_MARK
   (code-char #x00000342) ; ͂ COMBINING_GREEK_PERISPOMENI
   (code-char #x00000343) ; ̓ COMBINING_GREEK_KORONIS
   (code-char #x00000344) ; ̈́ COMBINING_GREEK_DIALYTIKA_TONOS
   (code-char #x00000345) ; ͅ COMBINING_GREEK_YPOGEGRAMMENI
   (code-char #x00000346) ; ͆ COMBINING_BRIDGE_ABOVE
   (code-char #x00000347) ; ͇ COMBINING_EQUALS_SIGN_BELOW
   (code-char #x00000348) ; ͈ COMBINING_DOUBLE_VERTICAL_LINE_BELOW
   (code-char #x00000349) ; ͉ COMBINING_LEFT_ANGLE_BELOW
   (code-char #x0000034A) ; ͊ COMBINING_NOT_TILDE_ABOVE
   (code-char #x0000034B) ; ͋ COMBINING_HOMOTHETIC_ABOVE
   (code-char #x0000034C) ; ͌ COMBINING_ALMOST_EQUAL_TO_ABOVE
   (code-char #x0000034D) ; ͍ COMBINING_LEFT_RIGHT_ARROW_BELOW
   (code-char #x0000034E) ; ͎ COMBINING_UPWARDS_ARROW_BELOW
   (code-char #x0000034F) ;   COMBINING_GRAPHEME_JOINER
   (code-char #x00000350) ; ͐ COMBINING_RIGHT_ARROWHEAD_ABOVE
   (code-char #x00000351) ; ͑ COMBINING_LEFT_HALF_RING_ABOVE
   (code-char #x00000352) ; ͒ COMBINING_FERMATA
   (code-char #x00000353) ; ͓ COMBINING_X_BELOW
   (code-char #x00000354) ; ͔ COMBINING_LEFT_ARROWHEAD_BELOW
   (code-char #x00000355) ; ͕ COMBINING_RIGHT_ARROWHEAD_BELOW
   (code-char #x00000356) ; ͖ COMBINING_RIGHT_ARROWHEAD_AND_UP_ARROWHEAD_BELOW
   (code-char #x00000357) ; ͗ COMBINING_RIGHT_HALF_RING_ABOVE
   (code-char #x00000358) ; ͘ COMBINING_DOT_ABOVE_RIGHT
   (code-char #x00000359) ; ͙ COMBINING_ASTERISK_BELOW
   (code-char #x0000035A) ; ͚ COMBINING_DOUBLE_RING_BELOW
   (code-char #x0000035B) ; ͛ COMBINING_ZIGZAG_ABOVE
   (code-char #x0000035C) ; ͜ COMBINING_DOUBLE_BREVE_BELOW
   (code-char #x0000035D) ; ͝ COMBINING_DOUBLE_BREVE
   (code-char #x0000035E) ; ͞ COMBINING_DOUBLE_MACRON
   (code-char #x0000035F) ; ͟ COMBINING_DOUBLE_MACRON_BELOW
   (code-char #x00000360) ; ͠ COMBINING_DOUBLE_TILDE
   (code-char #x00000361) ; ͡ COMBINING_DOUBLE_INVERTED_BREVE
   (code-char #x00000362) ; ͢ COMBINING_DOUBLE_RIGHTWARDS_ARROW_BELOW
   (code-char #x00000363) ; ͣ COMBINING_LATIN_SMALL_LETTER_A
   (code-char #x00000364) ; ͤ COMBINING_LATIN_SMALL_LETTER_E
   (code-char #x00000365) ; ͥ COMBINING_LATIN_SMALL_LETTER_I
   (code-char #x00000366) ; ͦ COMBINING_LATIN_SMALL_LETTER_O
   (code-char #x00000367) ; ͧ COMBINING_LATIN_SMALL_LETTER_U
   (code-char #x00000368) ; ͨ COMBINING_LATIN_SMALL_LETTER_C
   (code-char #x00000369) ; ͩ COMBINING_LATIN_SMALL_LETTER_D
   (code-char #x0000036A) ; ͪ COMBINING_LATIN_SMALL_LETTER_H
   (code-char #x0000036B) ; ͫ COMBINING_LATIN_SMALL_LETTER_M
   (code-char #x0000036C) ; ͬ COMBINING_LATIN_SMALL_LETTER_R
   (code-char #x0000036D) ; ͭ COMBINING_LATIN_SMALL_LETTER_T
   (code-char #x0000036E) ; ͮ COMBINING_LATIN_SMALL_LETTER_V
   (code-char #x0000036F) ; ͯ COMBINING_LATIN_SMALL_LETTER_X
   (code-char #x00000483) ; ҃ COMBINING_CYRILLIC_TITLO
   (code-char #x00000484) ; ҄ COMBINING_CYRILLIC_PALATALIZATION
   (code-char #x00000485) ; ҅ COMBINING_CYRILLIC_DASIA_PNEUMATA
   (code-char #x00000486) ; ҆ COMBINING_CYRILLIC_PSILI_PNEUMATA
   (code-char #x00000487) ;   COMBINING_CYRILLIC_POKRYTIE
   (code-char #x00000488) ; ҈ COMBINING_CYRILLIC_HUNDRED_THOUSANDS_SIGN
   (code-char #x00000489) ; ҉ COMBINING_CYRILLIC_MILLIONS_SIGN
   (code-char #x000007EB) ;   NKO_COMBINING_SHORT_HIGH_TONE
   (code-char #x000007EC) ;   NKO_COMBINING_SHORT_LOW_TONE
   (code-char #x000007ED) ;   NKO_COMBINING_SHORT_RISING_TONE
   (code-char #x000007EE) ;   NKO_COMBINING_LONG_DESCENDING_TONE
   (code-char #x000007EF) ;   NKO_COMBINING_LONG_HIGH_TONE
   (code-char #x000007F0) ;   NKO_COMBINING_LONG_LOW_TONE
   (code-char #x000007F1) ;   NKO_COMBINING_LONG_RISING_TONE
   (code-char #x000007F2) ;   NKO_COMBINING_NASALIZATION_MARK
   (code-char #x000007F3) ;   NKO_COMBINING_DOUBLE_DOT_ABOVE
   (code-char #x00000C00) ;   TELUGU_SIGN_COMBINING_CANDRABINDU_ABOVE
   (code-char #x0000135D) ;   ETHIOPIC_COMBINING_GEMINATION_AND_VOWEL_LENGTH_MARK
   (code-char #x0000135E) ;   ETHIOPIC_COMBINING_VOWEL_LENGTH_MARK
   (code-char #x0000135F) ;   ETHIOPIC_COMBINING_GEMINATION_MARK
   (code-char #x00001A7F) ;   TAI_THAM_COMBINING_CRYPTOGRAMMIC_DOT
   (code-char #x00001AB0) ;   COMBINING_DOUBLED_CIRCUMFLEX_ACCENT
   (code-char #x00001AB1) ;   COMBINING_DIAERESIS-RING
   (code-char #x00001AB2) ;   COMBINING_INFINITY
   (code-char #x00001AB3) ;   COMBINING_DOWNWARDS_ARROW
   (code-char #x00001AB4) ;   COMBINING_TRIPLE_DOT
   (code-char #x00001AB5) ;   COMBINING_X-X_BELOW
   (code-char #x00001AB6) ;   COMBINING_WIGGLY_LINE_BELOW
   (code-char #x00001AB7) ;   COMBINING_OPEN_MARK_BELOW
   (code-char #x00001AB8) ;   COMBINING_DOUBLE_OPEN_MARK_BELOW
   (code-char #x00001AB9) ;   COMBINING_LIGHT_CENTRALIZATION_STROKE_BELOW
   (code-char #x00001ABA) ;   COMBINING_STRONG_CENTRALIZATION_STROKE_BELOW
   (code-char #x00001ABB) ;   COMBINING_PARENTHESES_ABOVE
   (code-char #x00001ABC) ;   COMBINING_DOUBLE_PARENTHESES_ABOVE
   (code-char #x00001ABD) ;   COMBINING_PARENTHESES_BELOW
   (code-char #x00001ABE) ;   COMBINING_PARENTHESES_OVERLAY
   (code-char #x00001B6B) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_TEGEH
   (code-char #x00001B6C) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_ENDEP
   (code-char #x00001B6D) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPUL
   (code-char #x00001B6E) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPLI
   (code-char #x00001B6F) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_JEGOGAN
   (code-char #x00001B70) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPUL_WITH_JEGOGAN
   (code-char #x00001B71) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_KEMPLI_WITH_JEGOGAN
   (code-char #x00001B72) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_BENDE
   (code-char #x00001B73) ;   BALINESE_MUSICAL_SYMBOL_COMBINING_GONG
   (code-char #x00001DC0) ; ᷀ COMBINING_DOTTED_GRAVE_ACCENT
   (code-char #x00001DC1) ; ᷁ COMBINING_DOTTED_ACUTE_ACCENT
   (code-char #x00001DC2) ; ᷂ COMBINING_SNAKE_BELOW
   (code-char #x00001DC3) ; ᷃ COMBINING_SUSPENSION_MARK
   (code-char #x00001DC4) ;   COMBINING_MACRON-ACUTE
   (code-char #x00001DC5) ;   COMBINING_GRAVE-MACRON
   (code-char #x00001DC6) ;   COMBINING_MACRON-GRAVE
   (code-char #x00001DC7) ;   COMBINING_ACUTE-MACRON
   (code-char #x00001DC8) ;   COMBINING_GRAVE-ACUTE-GRAVE
   (code-char #x00001DC9) ;   COMBINING_ACUTE-GRAVE-ACUTE
   (code-char #x00001DCA) ;   COMBINING_LATIN_SMALL_LETTER_R_BELOW
   (code-char #x00001DCB) ;   COMBINING_BREVE-MACRON
   (code-char #x00001DCC) ;   COMBINING_MACRON-BREVE
   (code-char #x00001DCD) ;   COMBINING_DOUBLE_CIRCUMFLEX_ABOVE
   (code-char #x00001DCE) ;   COMBINING_OGONEK_ABOVE
   (code-char #x00001DCF) ;   COMBINING_ZIGZAG_BELOW
   (code-char #x00001DD0) ;   COMBINING_IS_BELOW
   (code-char #x00001DD1) ;   COMBINING_UR_ABOVE
   (code-char #x00001DD2) ;   COMBINING_US_ABOVE
   (code-char #x00001DD3) ;   COMBINING_LATIN_SMALL_LETTER_FLATTENED_OPEN_A_ABOVE
   (code-char #x00001DD4) ;   COMBINING_LATIN_SMALL_LETTER_AE
   (code-char #x00001DD5) ;   COMBINING_LATIN_SMALL_LETTER_AO
   (code-char #x00001DD6) ;   COMBINING_LATIN_SMALL_LETTER_AV
   (code-char #x00001DD7) ;   COMBINING_LATIN_SMALL_LETTER_C_CEDILLA
   (code-char #x00001DD8) ;   COMBINING_LATIN_SMALL_LETTER_INSULAR_D
   (code-char #x00001DD9) ;   COMBINING_LATIN_SMALL_LETTER_ETH
   (code-char #x00001DDA) ;   COMBINING_LATIN_SMALL_LETTER_G
   (code-char #x00001DDB) ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_G
   (code-char #x00001DDC) ;   COMBINING_LATIN_SMALL_LETTER_K
   (code-char #x00001DDD) ;   COMBINING_LATIN_SMALL_LETTER_L
   (code-char #x00001DDE) ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_L
   (code-char #x00001DDF) ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_M
   (code-char #x00001DE0) ;   COMBINING_LATIN_SMALL_LETTER_N
   (code-char #x00001DE1) ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_N
   (code-char #x00001DE2) ;   COMBINING_LATIN_LETTER_SMALL_CAPITAL_R
   (code-char #x00001DE3) ;   COMBINING_LATIN_SMALL_LETTER_R_ROTUNDA
   (code-char #x00001DE4) ;   COMBINING_LATIN_SMALL_LETTER_S
   (code-char #x00001DE5) ;   COMBINING_LATIN_SMALL_LETTER_LONG_S
   (code-char #x00001DE6) ;   COMBINING_LATIN_SMALL_LETTER_Z
   (code-char #x00001DE7) ;   COMBINING_LATIN_SMALL_LETTER_ALPHA
   (code-char #x00001DE8) ;   COMBINING_LATIN_SMALL_LETTER_B
   (code-char #x00001DE9) ;   COMBINING_LATIN_SMALL_LETTER_BETA
   (code-char #x00001DEA) ;   COMBINING_LATIN_SMALL_LETTER_SCHWA
   (code-char #x00001DEB) ;   COMBINING_LATIN_SMALL_LETTER_F
   (code-char #x00001DEC) ;   COMBINING_LATIN_SMALL_LETTER_L_WITH_DOUBLE_MIDDLE_TILDE
   (code-char #x00001DED) ;   COMBINING_LATIN_SMALL_LETTER_O_WITH_LIGHT_CENTRALIZATION_STROKE
   (code-char #x00001DEE) ;   COMBINING_LATIN_SMALL_LETTER_P
   (code-char #x00001DEF) ;   COMBINING_LATIN_SMALL_LETTER_ESH
   (code-char #x00001DF0) ;   COMBINING_LATIN_SMALL_LETTER_U_WITH_LIGHT_CENTRALIZATION_STROKE
   (code-char #x00001DF1) ;   COMBINING_LATIN_SMALL_LETTER_W
   (code-char #x00001DF2) ;   COMBINING_LATIN_SMALL_LETTER_A_WITH_DIAERESIS
   (code-char #x00001DF3) ;   COMBINING_LATIN_SMALL_LETTER_O_WITH_DIAERESIS
   (code-char #x00001DF4) ;   COMBINING_LATIN_SMALL_LETTER_U_WITH_DIAERESIS
   (code-char #x00001DF5) ;   COMBINING_UP_TACK_ABOVE
   (code-char #x00001DFC) ;   COMBINING_DOUBLE_INVERTED_BREVE_BELOW
   (code-char #x00001DFD) ;   COMBINING_ALMOST_EQUAL_TO_BELOW
   (code-char #x00001DFE) ;   COMBINING_LEFT_ARROWHEAD_ABOVE
   (code-char #x00001DFF) ;   COMBINING_RIGHT_ARROWHEAD_AND_DOWN_ARROWHEAD_BELOW
   (code-char #x000020D0) ; ⃐ COMBINING_LEFT_HARPOON_ABOVE
   (code-char #x000020D1) ; ⃑ COMBINING_RIGHT_HARPOON_ABOVE
   (code-char #x000020D2) ; ⃒ COMBINING_LONG_VERTICAL_LINE_OVERLAY
   (code-char #x000020D3) ; ⃓ COMBINING_SHORT_VERTICAL_LINE_OVERLAY
   (code-char #x000020D4) ; ⃔ COMBINING_ANTICLOCKWISE_ARROW_ABOVE
   (code-char #x000020D5) ; ⃕ COMBINING_CLOCKWISE_ARROW_ABOVE
   (code-char #x000020D6) ; ⃖ COMBINING_LEFT_ARROW_ABOVE
   (code-char #x000020D7) ; ⃗ COMBINING_RIGHT_ARROW_ABOVE
   (code-char #x000020D8) ; ⃘ COMBINING_RING_OVERLAY
   (code-char #x000020D9) ; ⃙ COMBINING_CLOCKWISE_RING_OVERLAY
   (code-char #x000020DA) ; ⃚ COMBINING_ANTICLOCKWISE_RING_OVERLAY
   (code-char #x000020DB) ; ⃛ COMBINING_THREE_DOTS_ABOVE
   (code-char #x000020DC) ; ⃜ COMBINING_FOUR_DOTS_ABOVE
   (code-char #x000020DD) ; ⃝ COMBINING_ENCLOSING_CIRCLE
   (code-char #x000020DE) ; ⃞ COMBINING_ENCLOSING_SQUARE
   (code-char #x000020DF) ; ⃟ COMBINING_ENCLOSING_DIAMOND
   (code-char #x000020E0) ; ⃠ COMBINING_ENCLOSING_CIRCLE_BACKSLASH
   (code-char #x000020E1) ; ⃡ COMBINING_LEFT_RIGHT_ARROW_ABOVE
   (code-char #x000020E2) ; ⃢ COMBINING_ENCLOSING_SCREEN
   (code-char #x000020E3) ; ⃣ COMBINING_ENCLOSING_KEYCAP
   (code-char #x000020E4) ; ⃤ COMBINING_ENCLOSING_UPWARD_POINTING_TRIANGLE
   (code-char #x000020E5) ; ⃥ COMBINING_REVERSE_SOLIDUS_OVERLAY
   (code-char #x000020E6) ; ⃦ COMBINING_DOUBLE_VERTICAL_STROKE_OVERLAY
   (code-char #x000020E7) ; ⃧ COMBINING_ANNUITY_SYMBOL
   (code-char #x000020E8) ; ⃨ COMBINING_TRIPLE_UNDERDOT
   (code-char #x000020E9) ; ⃩ COMBINING_WIDE_BRIDGE_ABOVE
   (code-char #x000020EA) ; ⃪ COMBINING_LEFTWARDS_ARROW_OVERLAY
   (code-char #x000020EB) ; ⃫ COMBINING_LONG_DOUBLE_SOLIDUS_OVERLAY
   (code-char #x000020EC) ;   COMBINING_RIGHTWARDS_HARPOON_WITH_BARB_DOWNWARDS
   (code-char #x000020ED) ;   COMBINING_LEFTWARDS_HARPOON_WITH_BARB_DOWNWARDS
   (code-char #x000020EE) ;   COMBINING_LEFT_ARROW_BELOW
   (code-char #x000020EF) ;   COMBINING_RIGHT_ARROW_BELOW
   (code-char #x000020F0) ;   COMBINING_ASTERISK_ABOVE
   (code-char #x00002CEF) ;   COPTIC_COMBINING_NI_ABOVE
   (code-char #x00002CF0) ;   COPTIC_COMBINING_SPIRITUS_ASPER
   (code-char #x00002CF1) ;   COPTIC_COMBINING_SPIRITUS_LENIS
   (code-char #x00002DE0) ;   COMBINING_CYRILLIC_LETTER_BE
   (code-char #x00002DE1) ;   COMBINING_CYRILLIC_LETTER_VE
   (code-char #x00002DE2) ;   COMBINING_CYRILLIC_LETTER_GHE
   (code-char #x00002DE3) ;   COMBINING_CYRILLIC_LETTER_DE
   (code-char #x00002DE4) ;   COMBINING_CYRILLIC_LETTER_ZHE
   (code-char #x00002DE5) ;   COMBINING_CYRILLIC_LETTER_ZE
   (code-char #x00002DE6) ;   COMBINING_CYRILLIC_LETTER_KA
   (code-char #x00002DE7) ;   COMBINING_CYRILLIC_LETTER_EL
   (code-char #x00002DE8) ;   COMBINING_CYRILLIC_LETTER_EM
   (code-char #x00002DE9) ;   COMBINING_CYRILLIC_LETTER_EN
   (code-char #x00002DEA) ;   COMBINING_CYRILLIC_LETTER_O
   (code-char #x00002DEB) ;   COMBINING_CYRILLIC_LETTER_PE
   (code-char #x00002DEC) ;   COMBINING_CYRILLIC_LETTER_ER
   (code-char #x00002DED) ;   COMBINING_CYRILLIC_LETTER_ES
   (code-char #x00002DEE) ;   COMBINING_CYRILLIC_LETTER_TE
   (code-char #x00002DEF) ;   COMBINING_CYRILLIC_LETTER_HA
   (code-char #x00002DF0) ;   COMBINING_CYRILLIC_LETTER_TSE
   (code-char #x00002DF1) ;   COMBINING_CYRILLIC_LETTER_CHE
   (code-char #x00002DF2) ;   COMBINING_CYRILLIC_LETTER_SHA
   (code-char #x00002DF3) ;   COMBINING_CYRILLIC_LETTER_SHCHA
   (code-char #x00002DF4) ;   COMBINING_CYRILLIC_LETTER_FITA
   (code-char #x00002DF5) ;   COMBINING_CYRILLIC_LETTER_ES-TE
   (code-char #x00002DF6) ;   COMBINING_CYRILLIC_LETTER_A
   (code-char #x00002DF7) ;   COMBINING_CYRILLIC_LETTER_IE
   (code-char #x00002DF8) ;   COMBINING_CYRILLIC_LETTER_DJERV
   (code-char #x00002DF9) ;   COMBINING_CYRILLIC_LETTER_MONOGRAPH_UK
   (code-char #x00002DFA) ;   COMBINING_CYRILLIC_LETTER_YAT
   (code-char #x00002DFB) ;   COMBINING_CYRILLIC_LETTER_YU
   (code-char #x00002DFC) ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_A
   (code-char #x00002DFD) ;   COMBINING_CYRILLIC_LETTER_LITTLE_YUS
   (code-char #x00002DFE) ;   COMBINING_CYRILLIC_LETTER_BIG_YUS
   (code-char #x00002DFF) ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_BIG_YUS
   (code-char #x00003099) ; ゙ COMBINING_KATAKANA-HIRAGANA_VOICED_SOUND_MARK
   (code-char #x0000309A) ; ゚ COMBINING_KATAKANA-HIRAGANA_SEMI-VOICED_SOUND_MARK
   (code-char #x0000A66F) ;   COMBINING_CYRILLIC_VZMET
   (code-char #x0000A670) ;   COMBINING_CYRILLIC_TEN_MILLIONS_SIGN
   (code-char #x0000A671) ;   COMBINING_CYRILLIC_HUNDRED_MILLIONS_SIGN
   (code-char #x0000A672) ;   COMBINING_CYRILLIC_THOUSAND_MILLIONS_SIGN
   (code-char #x0000A674) ;   COMBINING_CYRILLIC_LETTER_UKRAINIAN_IE
   (code-char #x0000A675) ;   COMBINING_CYRILLIC_LETTER_I
   (code-char #x0000A676) ;   COMBINING_CYRILLIC_LETTER_YI
   (code-char #x0000A677) ;   COMBINING_CYRILLIC_LETTER_U
   (code-char #x0000A678) ;   COMBINING_CYRILLIC_LETTER_HARD_SIGN
   (code-char #x0000A679) ;   COMBINING_CYRILLIC_LETTER_YERU
   (code-char #x0000A67A) ;   COMBINING_CYRILLIC_LETTER_SOFT_SIGN
   (code-char #x0000A67B) ;   COMBINING_CYRILLIC_LETTER_OMEGA
   (code-char #x0000A67C) ;   COMBINING_CYRILLIC_KAVYKA
   (code-char #x0000A67D) ;   COMBINING_CYRILLIC_PAYEROK
   (code-char #x0000A69F) ;   COMBINING_CYRILLIC_LETTER_IOTIFIED_E
   (code-char #x0000A6F0) ;   BAMUM_COMBINING_MARK_KOQNDON
   (code-char #x0000A6F1) ;   BAMUM_COMBINING_MARK_TUKWENTIS
   (code-char #x0000A8E0) ;   COMBINING_DEVANAGARI_DIGIT_ZERO
   (code-char #x0000A8E1) ;   COMBINING_DEVANAGARI_DIGIT_ONE
   (code-char #x0000A8E2) ;   COMBINING_DEVANAGARI_DIGIT_TWO
   (code-char #x0000A8E3) ;   COMBINING_DEVANAGARI_DIGIT_THREE
   (code-char #x0000A8E4) ;   COMBINING_DEVANAGARI_DIGIT_FOUR
   (code-char #x0000A8E5) ;   COMBINING_DEVANAGARI_DIGIT_FIVE
   (code-char #x0000A8E6) ;   COMBINING_DEVANAGARI_DIGIT_SIX
   (code-char #x0000A8E7) ;   COMBINING_DEVANAGARI_DIGIT_SEVEN
   (code-char #x0000A8E8) ;   COMBINING_DEVANAGARI_DIGIT_EIGHT
   (code-char #x0000A8E9) ;   COMBINING_DEVANAGARI_DIGIT_NINE
   (code-char #x0000A8EA) ;   COMBINING_DEVANAGARI_LETTER_A
   (code-char #x0000A8EB) ;   COMBINING_DEVANAGARI_LETTER_U
   (code-char #x0000A8EC) ;   COMBINING_DEVANAGARI_LETTER_KA
   (code-char #x0000A8ED) ;   COMBINING_DEVANAGARI_LETTER_NA
   (code-char #x0000A8EE) ;   COMBINING_DEVANAGARI_LETTER_PA
   (code-char #x0000A8EF) ;   COMBINING_DEVANAGARI_LETTER_RA
   (code-char #x0000A8F0) ;   COMBINING_DEVANAGARI_LETTER_VI
   (code-char #x0000A8F1) ;   COMBINING_DEVANAGARI_SIGN_AVAGRAHA
   (code-char #x0000FE20) ; ︠ COMBINING_LIGATURE_LEFT_HALF
   (code-char #x0000FE21) ; ︡ COMBINING_LIGATURE_RIGHT_HALF
   (code-char #x0000FE22) ; ︢ COMBINING_DOUBLE_TILDE_LEFT_HALF
   (code-char #x0000FE23) ; ︣ COMBINING_DOUBLE_TILDE_RIGHT_HALF
   (code-char #x0000FE24) ;   COMBINING_MACRON_LEFT_HALF
   (code-char #x0000FE25) ;   COMBINING_MACRON_RIGHT_HALF
   (code-char #x0000FE26) ;   COMBINING_CONJOINING_MACRON
   (code-char #x0000FE27) ;   COMBINING_LIGATURE_LEFT_HALF_BELOW
   (code-char #x0000FE28) ;   COMBINING_LIGATURE_RIGHT_HALF_BELOW
   (code-char #x0000FE29) ;   COMBINING_TILDE_LEFT_HALF_BELOW
   (code-char #x0000FE2A) ;   COMBINING_TILDE_RIGHT_HALF_BELOW
   (code-char #x0000FE2B) ;   COMBINING_MACRON_LEFT_HALF_BELOW
   (code-char #x0000FE2C) ;   COMBINING_MACRON_RIGHT_HALF_BELOW
   (code-char #x0000FE2D) ;   COMBINING_CONJOINING_MACRON_BELOW
   (code-char #x000101E4) ;   PHAISTOS_DISC_SIGN_COMB
   (code-char #x000101FD) ;   PHAISTOS_DISC_SIGN_COMBINING_OBLIQUE_STROKE
   (code-char #x00010376) ;   COMBINING_OLD_PERMIC_LETTER_AN
   (code-char #x00010377) ;   COMBINING_OLD_PERMIC_LETTER_DOI
   (code-char #x00010378) ;   COMBINING_OLD_PERMIC_LETTER_ZATA
   (code-char #x00010379) ;   COMBINING_OLD_PERMIC_LETTER_NENOE
   (code-char #x0001037A) ;   COMBINING_OLD_PERMIC_LETTER_SII
   (code-char #x00011366) ;   COMBINING_GRANTHA_DIGIT_ZERO
   (code-char #x00011367) ;   COMBINING_GRANTHA_DIGIT_ONE
   (code-char #x00011368) ;   COMBINING_GRANTHA_DIGIT_TWO
   (code-char #x00011369) ;   COMBINING_GRANTHA_DIGIT_THREE
   (code-char #x0001136A) ;   COMBINING_GRANTHA_DIGIT_FOUR
   (code-char #x0001136B) ;   COMBINING_GRANTHA_DIGIT_FIVE
   (code-char #x0001136C) ;   COMBINING_GRANTHA_DIGIT_SIX
   (code-char #x00011370) ;   COMBINING_GRANTHA_LETTER_A
   (code-char #x00011371) ;   COMBINING_GRANTHA_LETTER_KA
   (code-char #x00011372) ;   COMBINING_GRANTHA_LETTER_NA
   (code-char #x00011373) ;   COMBINING_GRANTHA_LETTER_VI
   (code-char #x00011374) ;   COMBINING_GRANTHA_LETTER_PA
   (code-char #x00016AF0) ;   BASSA_VAH_COMBINING_HIGH_TONE
   (code-char #x00016AF1) ;   BASSA_VAH_COMBINING_LOW_TONE
   (code-char #x00016AF2) ;   BASSA_VAH_COMBINING_MID_TONE
   (code-char #x00016AF3) ;   BASSA_VAH_COMBINING_LOW-MID_TONE
   (code-char #x00016AF4) ;   BASSA_VAH_COMBINING_HIGH-LOW_TONE
   (code-char #x0001D165) ; 𝅥 MUSICAL_SYMBOL_COMBINING_STEM
   (code-char #x0001D166) ; 𝅦 MUSICAL_SYMBOL_COMBINING_SPRECHGESANG_STEM
   (code-char #x0001D167) ; 𝅧 MUSICAL_SYMBOL_COMBINING_TREMOLO-1
   (code-char #x0001D168) ; 𝅨 MUSICAL_SYMBOL_COMBINING_TREMOLO-2
   (code-char #x0001D169) ; 𝅩 MUSICAL_SYMBOL_COMBINING_TREMOLO-3
   (code-char #x0001D16D) ; 𝅭 MUSICAL_SYMBOL_COMBINING_AUGMENTATION_DOT
   (code-char #x0001D16E) ; 𝅮 MUSICAL_SYMBOL_COMBINING_FLAG-1
   (code-char #x0001D16F) ; 𝅯 MUSICAL_SYMBOL_COMBINING_FLAG-2
   (code-char #x0001D170) ; 𝅰 MUSICAL_SYMBOL_COMBINING_FLAG-3
   (code-char #x0001D171) ; 𝅱 MUSICAL_SYMBOL_COMBINING_FLAG-4
   (code-char #x0001D172) ; 𝅲 MUSICAL_SYMBOL_COMBINING_FLAG-5
   (code-char #x0001D17B) ; 𝅻 MUSICAL_SYMBOL_COMBINING_ACCENT
   (code-char #x0001D17C) ; 𝅼 MUSICAL_SYMBOL_COMBINING_STACCATO
   (code-char #x0001D17D) ; 𝅽 MUSICAL_SYMBOL_COMBINING_TENUTO
   (code-char #x0001D17E) ; 𝅾 MUSICAL_SYMBOL_COMBINING_STACCATISSIMO
   (code-char #x0001D17F) ; 𝅿 MUSICAL_SYMBOL_COMBINING_MARCATO
   (code-char #x0001D180) ; 𝆀 MUSICAL_SYMBOL_COMBINING_MARCATO-STACCATO
   (code-char #x0001D181) ; 𝆁 MUSICAL_SYMBOL_COMBINING_ACCENT-STACCATO
   (code-char #x0001D182) ; 𝆂 MUSICAL_SYMBOL_COMBINING_LOURE
   (code-char #x0001D185) ; 𝆅 MUSICAL_SYMBOL_COMBINING_DOIT
   (code-char #x0001D186) ; 𝆆 MUSICAL_SYMBOL_COMBINING_RIP
   (code-char #x0001D187) ; 𝆇 MUSICAL_SYMBOL_COMBINING_FLIP
   (code-char #x0001D188) ; 𝆈 MUSICAL_SYMBOL_COMBINING_SMEAR
   (code-char #x0001D189) ; 𝆉 MUSICAL_SYMBOL_COMBINING_BEND
   (code-char #x0001D18A) ; 𝆊 MUSICAL_SYMBOL_COMBINING_DOUBLE_TONGUE
   (code-char #x0001D18B) ; 𝆋 MUSICAL_SYMBOL_COMBINING_TRIPLE_TONGUE
   (code-char #x0001D1AA) ; 𝆪 MUSICAL_SYMBOL_COMBINING_DOWN_BOW
   (code-char #x0001D1AB) ; 𝆫 MUSICAL_SYMBOL_COMBINING_UP_BOW
   (code-char #x0001D1AC) ; 𝆬 MUSICAL_SYMBOL_COMBINING_HARMONIC
   (code-char #x0001D1AD) ; 𝆭 MUSICAL_SYMBOL_COMBINING_SNAP_PIZZICATO
   (code-char #x0001D242) ;   COMBINING_GREEK_MUSICAL_TRISEME
   (code-char #x0001D243) ;   COMBINING_GREEK_MUSICAL_TETRASEME
   (code-char #x0001D244) ;   COMBINING_GREEK_MUSICAL_PENTASEME
   (code-char #x0001E8D0) ;   MENDE_KIKAKUI_COMBINING_NUMBER_TEENS
   (code-char #x0001E8D1) ;   MENDE_KIKAKUI_COMBINING_NUMBER_TENS
   (code-char #x0001E8D2) ;   MENDE_KIKAKUI_COMBINING_NUMBER_HUNDREDS
   (code-char #x0001E8D3) ;   MENDE_KIKAKUI_COMBINING_NUMBER_THOUSANDS
   (code-char #x0001E8D4) ;   MENDE_KIKAKUI_COMBINING_NUMBER_TEN_THOUSANDS
   (code-char #x0001E8D5) ;   MENDE_KIKAKUI_COMBINING_NUMBER_HUNDRED_THOUSANDS
   (code-char #x0001E8D6) ;   MENDE_KIKAKUI_COMBINING_NUMBER_MILLIONS
   )
  "")

;; @@@ Make these better on non-SBCL. And move them out of here!
(defun combining-character-p (c)
  ;; #+sbcl (/= (sb-unicode:combining-class c) 0)
  ;; This is just plain better.
  (position c *combining-chars*))

#+sbcl
;; Older versions of SBCL don't have this.
(when (find-package :sb-unicode)
  (d-add-feature :has-sb-unicode))

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
  (tt-write-char (line-editor-terminal e) c)
  (incf (screen-col e))
  (when (>= (screen-col e) (terminal-window-columns (line-editor-terminal e)))
    (setf (screen-col e) 0)
    (incf (screen-row e))
    (tt-scroll-down e 1)
    (tt-beginning-of-line e)))

(defun editor-write-string (e s)
  "Write a display string to the screen. Update screen coordinates."
  (tt-write-string (line-editor-terminal e) s)
  (editor-update-pos-for-string e s))

(defun display-char (e c)
  "Output a character with visible display of control characters."
  (cond
    ((graphic-char-p c)
     (editor-write-char e c))
    ((eql c #\tab)
     (editor-write-string e (make-string (- 8 (mod (screen-col e) 8))
					 :initial-element #\space)))
    ((eql c #\newline)
     (setf (screen-col e) 0)
     (incf (screen-row e))
     (tt-write-char e c))
    ((setf c (control-char-graphic c))
     (editor-write-char e #\^)
     (editor-write-char e c))
    (t ;; output non-graphic chars as char code
     (editor-write-char e #\\)
     (editor-write-string e (format nil "\\~3,'0o" (char-code c))))))

(defun display-buf (e &optional (start 0) end)
  "Display the buffer."
  (with-slots (buf) e
    ;; Just in case write-char does system calls, we output to a string stream.
    (tt-write-string
     e (with-output-to-string (s)
	 (loop :with sub = (if end
			       (subseq buf start end)
			       (subseq buf start))
	    :for c :across sub
	    :do
	    (display-char e c))))))

(defmacro without-messing-up-cursor ((e) &body body)
  (let ((old-row (gensym))
	(old-col (gensym)))
  `(let ((,old-row (screen-row ,e))
	 (,old-col (screen-col ,e)))
     (prog1 ,@body
       (if (< ,old-row (screen-row ,e))
	   (tt-up ,e (- (screen-row ,e) ,old-row))
	   (tt-down ,e (- ,old-row (screen-row ,e))))
       (tt-beginning-of-line ,e)
       (tt-forward ,e ,old-col)
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
      (when (>= (+ col right-len) width)
	;; Cheaty way out: redraw whole thing after point
	(without-messing-up-cursor (e)
	  (display-buf e point)
	  (tt-del-char e delete-length)
	  (log-message e "Smoot ~a ~a ~a" width delete-length (screen-col e))
	  (when (< (- width delete-length) (screen-col e))
	    (setf to-delete (- delete-length (- width (screen-col e))))
	    (tt-down e 1)
	    (incf (screen-row e))
	    (tt-move-to-col e 0)
	    (setf (screen-col e) 0)
	    (tt-erase-to-eol e)
	    (loop :while (> to-delete 0) :do
	       (tt-down e 1)
	       (incf (screen-row e))
	       (tt-erase-to-eol e)
	       (log-message e "Bloot ~a" to-delete)
	       (decf to-delete width))))))))

(defun erase-display (e)
  "Erase the display of the buffer, but not the buffer itself."
  (with-slots (buf) e
    (beginning-of-line e)
    (tt-erase-to-eol e)
    (let* ((cols (terminal-window-columns (line-editor-terminal e)))
	   ;;(buf-len (length buf))
	   (buf-len (display-length buf))
	   ;;(lines-to-clear (truncate (+ (screen-col e) buf-len) 80)))
	   (lines-to-clear (truncate (+ (screen-col e) buf-len) cols)))
      (when (> (+ buf-len (screen-col e)) cols)
	(loop :for i :from 1 :to lines-to-clear
	      :do (tt-down e 1)
	      (tt-erase-line e))
	(tt-up e lines-to-clear)))))

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

(defun move-backward (e n)
  "Move backward N columns on the screen. Properly wraps to previous lines.
Updates the screen coordinates."
  (let ((orig-col (screen-col e)))
    (decf (screen-col e) n)
    (if (< (screen-col e) 0)
	(let* ((cols (terminal-window-columns (line-editor-terminal e)))
	       (rows-up (abs (- 1 (truncate (- orig-col (- n 1)) cols)))))
	  (decf (screen-row e) rows-up)
	  (setf (screen-col e) (mod (screen-col e) cols))
	  (tt-up e rows-up)
	  (tt-beginning-of-line e)
	  (tt-forward e (screen-col e)))
	(tt-backward e n))))

(defun move-forward (e n)
  "Move forward N columns on the screen. Properly wraps to subsequent lines.
Updates the screen coordinates."
  (let ((orig-col (screen-col e)))
    (incf (screen-col e) n)
    (if (> (screen-col e) (terminal-window-columns (line-editor-terminal e)))
	(let* ((cols (terminal-window-columns (line-editor-terminal e)))
	       (rows-down (truncate (+ orig-col n) cols)))
	  (incf (screen-row e) rows-down)
	  (setf (screen-col e) (mod (screen-col e) cols))
	  (tt-beginning-of-line e)
	  (tt-forward e (screen-col e))
	  (tt-down e rows-down))
	(tt-forward e n))))

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
;; Since emulating the terminal seems infeasible, unless we wrapped ourselves
;; in an emulation layer like screen or tmux, if we want to be sure to get
;; things right, we are stuck with with asking the terminal where the cursor
;; might be.
;;
;; Now the problem with asking the terminal, is that we have to output
;; something, and then read the coordinates back in. But there might be a
;; bunch of input, like a giant paste or something, or typing ahead, already
;; in the terminal's input queue, in front of the response to our "where is
;; the cursor" query, which blocks us from getting an answer.
;;
;; So we have to read all input available, BEFORE asking where the heck the
;; cursor is. This is the reason for all the otherwise useless 'eat-typeahead'
;; and 'tty-slurp'ing. Of course this whole thing is quite kludgey and I think
;; we should really be able ask the terminal where the cursor is with a nice
;; _function call_, not going through the I/O queue. Of course that would
;; require the terminal to be in our address space, or to have a separate
;; command channel if it's far, far away.
;;
;; There is still a small opportunity for a race condition, between outputing
;; the query, and getting an answer back, but it seems unlikely. I wonder if
;; there's some way to 'lock' the terminal input queue for that time.
;;

#|
(defun eat-typeahead (e)
  (let ((ta (termios:call-with-raw
	     (terminal-file-descriptor (line-editor-terminal e))
	     (_ (tty-slurp _)) :timeout 1)))
    (when (and ta (> (length ta) 0))
;      (log-message e "ta[~a]=~w" (length ta) ta)
      (if (typeahead e)
	  (setf (typeahead e) (s+ (typeahead e) ta))
	  (setf (typeahead e) ta
		(typeahead-pos e) 0)))))
|#

(defun finish-all-output (e)
  "Makes all output be in Finish."
  (when (not (getenv "EMACS"))		      ; XXX so wrong
    ;;#+ccl (ccl::auto-flush-interactive-streams) ;; Jiminy Crickets!
    (finish-output *standard-output*)
    (finish-output *terminal-io*)
    (finish-output t)
    (finish-output)
    ;(finish-output *standard-input*)
    )
  (tt-finish-output e)
  )

(defun do-prefix (e prompt-str)
  "Output a prefix. The prefix should not span more than one line."
  (finish-all-output e)
  (tt-write-string e prompt-str)
  ;;(finish-all-output e)
  (tt-finish-output e)
  ;; (eat-typeahead e)
  (multiple-value-bind (row col)
      (terminal-get-cursor-position (line-editor-terminal e))
    (setf (screen-row e) row
	  (screen-col e) col
	  ;; save end of the prefix as the starting column
	  (start-col e) col
	  (start-row e) row)))

(defun do-prompt (e prompt output-prompt-func &key only-last-line)
  "Output the prompt in a specified way."
;  (format t "e = ~w prompt = ~w output-prompt-func = ~w~%"
;	  e prompt output-prompt-func)
  (let* ((s (with-output-to-string (*standard-output*)
              (if (and output-prompt-func
		       (or (functionp output-prompt-func)
			   (fboundp output-prompt-func)))
		  (progn
		    (or (ignore-errors (funcall output-prompt-func e prompt))
			"Your prompt Function failed> ")
		    (log-message e "do-prompt output-prompt-func"))
		  (progn
		    (default-output-prompt e prompt)
		    (log-message e "do-prompt default-output-prompt")))))
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
  (tt-move-to-col e 0)
  (tt-erase-to-eol e)
  (setf (screen-col e) 0)
  (do-prompt e (prompt e) (prompt-func e) :only-last-line t)
  (finish-output (terminal-output-stream (line-editor-terminal e)))
  (display-buf e)
  (with-slots (point buf) e
    (when (< point (length buf))
      (let ((disp-len (display-length (subseq buf point))))
	(move-backward e disp-len))))
  (setf (need-to-redraw e) nil))

(defun tmp-prompt (e fmt &rest args)
  (tt-move-to-col e 0)
  (tt-erase-to-eol e)
  (setf (screen-col e) 0)
  (do-prefix e (apply #'format `(nil ,fmt ,@args))))

(defun tmp-message (e fmt &rest args)
  (apply #'tmp-prompt e fmt args)
  (setf (need-to-redraw e) t))

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
      (move-backward e (display-length (subseq buf point start))))))

(defun forward-word (e)
  "Move the insertion point to the end of the next word or the end of the
buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :forward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :forward :not-in non-word-chars)
      (move-forward e (display-length (subseq buf start point))))))

(defun backward-char (e)
  "Move the insertion point backward one character in the buffer."
  (with-slots (point buf screen-col screen-row) e
    (when (> point 0)
      (decf point)
      (dotimes (i (display-length (aref buf point)))
	(tt-write-char e #\backspace)
	(decf screen-col)
	(when (< screen-col 0)
	  (setf screen-col (1- (terminal-window-columns
				(line-editor-terminal e))))
	  (if (> screen-row 0) (decf screen-row)))))))

(defun forward-char (e)
  "Move the insertion point forward one character in the buffer."
  (with-slots (point buf screen-col screen-row) e
    (when (< point (fill-pointer buf))
      (dotimes (i (display-length (aref buf point)))
	(tt-forward e 1)
	(incf screen-col)
	(when (>= screen-col
		  (terminal-window-columns (line-editor-terminal e)))
	  (incf screen-row)
	  (setf screen-col 0)
	  (tt-down e 1)
	  (tt-beginning-of-line e)))
      (incf point))))

(defun beginning-of-line (e)
 "Move the insertion point to the beginning of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-backward e (display-length (subseq buf 0 point)))
    (setf point 0)))

(defun end-of-line (e)
  "Move the insertion point to the end of the line (actually the buffer)."
  (with-slots (point buf) e
    (move-forward e (display-length (subseq buf point)))
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
      (tt-write-char e #\newline))
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
	 (move-backward e (- point mark)))
	((> mark point)
	 (move-forward e (- mark point))))
      (rotatef point mark))))

(defun isearch-backward (e)
  "Incremental search backward."
  (isearch e :backward))

(defun isearch-forward (e)
  "Incremental search forward."
  (isearch e :forward))

;; Sadly ASCII / UTF-8 specific.
(defun control-char-p (c)
  (let ((code (char-code c)))
    (or (< code 32) (= code 128))))

(defparameter *isearch-prompt* "isearch: ")

(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (buf point) e
    (setf point (min (or pos (length buf)) (length buf)))
    (erase-display e)
    (tt-move-to-col e 0)
    (tt-erase-to-eol e)
    (setf (screen-col e) 0)
    (do-prefix e *isearch-prompt*)
    (when str
      (without-undo (e)
;;;	(erase-display e)
	(buffer-delete e 0 (length buf))
	(buffer-insert e 0 (or (history-current (context e)) ""))
	(setf point (min (or pos (length buf)) (length buf))))
      (loop :with end = (if pos (+ pos (length str)) nil)
	   :for c :across buf :and i = 0 :then (1+ i) :do
	   (cond
	     ((and pos (= i pos))
	      (tt-underline e t))
	     ((and end (= i end))
	      (tt-underline e nil)))
	   (display-char e c))
	(tt-underline e nil))
    (tt-finish-output e)))

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

(defun redraw-command (e)
  "Clear the screen and redraw the prompt and the input line."
  (with-slots (prompt prompt-func point buf need-to-redraw) e
    (tt-clear e) (tt-home e)
    (setf (screen-col e) 0 (screen-row e) 0)
    (do-prompt e prompt prompt-func)
    (finish-output (terminal-output-stream
		    (line-editor-terminal e)))
    (display-buf e)
    (when (< point (length buf))
      (let ((disp-len (display-length (subseq buf point))))
;	(message-pause e "~a ~a ~a" (screen-row e) (screen-col e)
;		       disp-len)
	(move-backward e disp-len)))
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
	(buffer-delete e (1- point) point)
	(decf point)
	(move-backward e del-len)
	(tt-del-char e del-len)
	(update-for-delete e del-len 1)))))

(defun delete-char (e)
  "Delete the character following the cursor."
  (with-slots (point buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(progn
	  (let ((del-len (display-length (aref buf point))))
	    (buffer-delete e point (1+ point))
	    (tt-del-char e del-len)
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
	(move-backward e del-len)
	(tt-del-char e del-len)
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
	(tt-del-char e del-len)
	(setf clipboard region-str)
	(buffer-delete e point start)
	(setf point start)
	(update-for-delete e del-len (- start point))))))

(defun kill-line (e)
  (with-slots (clipboard buf point) e
    (setf clipboard (subseq buf point))
    (buffer-delete e point (fill-pointer buf))
    (tt-erase-to-eol e)
    (let* ((cols (terminal-window-columns (line-editor-terminal e)))
	   (cut-len (length clipboard))
	   (lines-to-clear (truncate (+ (screen-col e) cut-len) 80)))
      (when (> (+ cut-len (screen-col e)) cols)
	(loop :for i :from 1 :to lines-to-clear
	   :do (tt-down e 1)
	   (tt-erase-line e))
	(tt-up e lines-to-clear)))))

(defun backward-kill-line (e)
  (with-slots (point clipboard buf) e
    (when (> point 0)
      (setf clipboard (subseq buf 0 point))
      (replace-buffer e (subseq buf point))
      (beginning-of-line e))))

(defun yank (e)
  (with-slots (clipboard point) e
    (when clipboard
      (let ((len (length clipboard))
	    (disp-len (display-length clipboard)))
	(insert-string e clipboard)
	(tt-ins-char e disp-len)
	(display-buf e point (+ point len))
	(incf point len)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hacks for typing lisp

(defun flash-paren (e c)
  (declare (ignore c))
  (let* ((str (buf e))
	 (point (point e))
	 (ppos (matching-paren-position str :position point))
	 (offset (and ppos (1+ (- point ppos))))
	 ;;(tty-fd (terminal-file-descriptor (line-editor-terminal e)))
	 )
    (if ppos
	(let ((saved-col (screen-col e)))
	  (declare (ignore saved-col))
;;;	  (tt-move-to-col e (+ ppos (start-col e)))
	  (move-backward e offset)
	  (tt-finish-output e)
	  (tt-listen-for e .5)
;;;	  (tt-move-to-col e saved-col)
	  (move-forward e offset)
	  )
	(beep e "No match."))))

(defun finish-line (e)
  "Add any missing close parentheses and accept the line."
  (with-slots (buf) e
    (loop :while (matching-paren-position buf)
       :do (insert-char e #\)) (display-char e #\)))
    (accept-line e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

; XXX This should depend on the size of the screen.
(defvar *really-limit* 100
  "How much is too much.")

;; Most of the work is done by print-columns, from dlib-misc.
(defun print-completions (e comp-list)
  (let ((len (length comp-list)))
    (when (> len *really-limit*)
      (tt-format e "Really list ~a things? (y or n) " len)
      (let ((chr (get-a-char e)))
	(tt-write-char e #\newline)
	(when (not (equalp #\y chr))
	  (return-from print-completions))))
    (print-columns comp-list
		   :columns (terminal-window-columns
			     (line-editor-terminal e)))))

(defun show-completions (e)
  (with-slots (completion-func buf point saved-point prompt prompt-func) e
    (if (not completion-func)
      (beep e "No completion installed.")
      (progn
	(multiple-value-bind (comp-list comp-count)
	    (funcall completion-func buf point t)
	  (when (and comp-count (> comp-count 0))
	    ;; downcased list 1 per line
	    (let ((saved-point point))
	      (end-of-line e)
	      (setf point saved-point))
	    (tt-write-char e #\newline)
	    #| (tt-format e "~{~a~%~}" comp-list) |#
	    (print-completions e comp-list)
	    (setf (screen-col e) 0)
	    (do-prompt e prompt prompt-func)
	    (display-buf e)
	    (when (< point (length buf))
	      (move-backward e (display-length
				(subseq buf point))))))))))

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
  (with-slots (completion-func point buf last-input) e
    (setf comp-func (or comp-func completion-func))
    (if (not comp-func)
	(beep e "No completion active.")
	(progn
	  (let* ((saved-point point) comp replace-pos unique)
	    (multiple-value-setq (comp replace-pos unique)
	      (funcall comp-func buf point nil))
	    (when (and (last-completion-not-unique e)
		       (last-command-was-completion e))
	      (log-message e "show mo")
	      (show-completions e))
	    (setf (did-complete e) t
		  (last-completion-not-unique e) (not unique))
	    ;; (format t "comp = ~s replace-pos = ~s~%" comp replace-pos)
	    ;; If the completion succeeded we need a replace-pos!
	    (assert (or (not comp) (numberp replace-pos)))
	    (if comp
		(let* ((same (- saved-point replace-pos)) ; same part
		       (diff (- (length comp) same)))     ; different part
;		  (format t "not supposed!~%")
		  (delete-region e replace-pos saved-point)
		  (setf point replace-pos)
		  (insert-string e comp)
		  ;; XXX: assmuing 1 wide chars in expansion
		  ;; backup over the same part
		  ;; insert enuff blanks to cover the difference
		  ;; then overwrite the whole new completion
		  (when (> same 0)
		    (move-backward e same))
		  (when (> diff 0)
		    (tt-ins-char e diff))
		  (editor-write-string e comp)
		  (incf point (length comp)))
		(progn
		  (setf point saved-point)	   ; go back to where we were
		  (beep e "No completions")))))))) ; ring the bell

(defun complete-filename-command (e)
  "Filename completion. This useful for when you want to explicitly complete a
filename instead of whatever the default completion is. Convenient for a key
binding."
  (complete e #'completion::complete-filename))

(defun pop-to-lish (e)
  "If we're inside lish, throw to a quick exit. If we're not in lish, enter it."
;  (break)
  (let* ((lish-package (find-package :lish))
	 (level-symbol (intern "*LISH-LEVEL*" lish-package)))
    (when lish-package
      (if (and (boundp level-symbol) (numberp (symbol-value level-symbol)))
	  (funcall (find-symbol "LISHITY-SPLIT" :lish))
	  (progn
	    (tt-beginning-of-line e)
	    (tt-erase-line e)
	    (finish-output (terminal-output-stream (line-editor-terminal e)))
	    (terminal-end (line-editor-terminal e))
	    (if (line-editor-terminal-device-name e)
		(funcall (find-symbol "LISH" :lish)
			 :terminal-name (line-editor-terminal-device-name e))
		(funcall (find-symbol "LISH" :lish)))
	    (tt-beginning-of-line e)
	    (tt-erase-line e)
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
  (with-slots (cmd buf point) e
    (when (not char)
      (setf char cmd))
    (if (and (not (graphic-char-p char)) (not quoted))
	(beep e "~a is unbound." char)
	(progn 
	  ;; a normal character
	  (if (= (length buf) point)
	      ;; end of the buf
	      (progn
		(display-char e char)
		;; flash paren and keep going
		(when (or (eql char #\)) (eql char #\]) (eql char #\}))
		  (flash-paren e char))
		(insert-char e char)
		(incf point))
	      ;; somewhere in the middle
	      (progn
		(tt-ins-char e (display-length char))
		(display-char e char)
		(when (or (eql char #\)) (eql char #\]) (eql char #\}))
		  (flash-paren e char))
		(insert-char e char)
		(incf point)
		;; dumb way out: just rewrite the whole thing
		;; relying on terminal wrap around
		(let ((old-row (screen-row e))
		      (old-col (screen-col e)))
		  (display-buf e point)
		  (if (< old-row (screen-row e))
		      (tt-up e (- (screen-row e) old-row))
		      (tt-down e (- old-row (screen-row e))))
		  (tt-beginning-of-line e)
		  (tt-forward e old-col)
		  (setf (screen-row e) old-row
			(screen-col e) old-col))))))))
;;		(tt-move-to e (screen-row e) (screen-col e))

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
	(set-key key-seq cmd (line-editor-keymap e))
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

    ;; Completion
    (#\tab			. complete)
    (#\?			. show-completions)

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
    (,(ctrl #\C)	. exit-editor)
    (,(ctrl #\X)	. exchange-point-and-mark)))
;  :default-binding #| (beep e "C-x ~a is unbound." cmd |#

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

(defkeymap *special-keymap*
  `(
    (:left  . backward-char)
    (:right . forward-char)
    (:up    . previous-history)
    (:down  . next-history)
    (:home  . beginning-of-line)
    (:end   . end-of-line)
    (:f9    . pop-to-lish)
    ))

;; Normal mode commands prefaced by escape.
(defparameter *escape-keymap*
    (add-keymap (build-escape-map *normal-keymap*) *escape-raw-keymap*))

;; @@@ do we really need this?
;; (defun bad-special-key (e)
;;   (beep e "Bad special key ~s." key))

(defun do-app-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-app-key e)))))

(defun do-function-key (e)
  (with-slots (cmd) e
    (do-special-key e (setf cmd (read-function-key e)))))

(defun do-special-key (e key)
  (perform-key e key *special-keymap*))

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
		  (keymap *normal-keymap*)
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
  KEYMAP (nil)
    A KEYMAP to use. For when you want to use your own customized keymap.
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
  (let ((e (or editor (make-instance
		       'line-editor
		       :point 0
		       :prompt prompt
		       :prompt-func output-prompt-func
		       :completion-func completion-func
		       :context context
		       :in-callback in-callback
		       :out-callback out-callback
		       :debugging debug
		       :keymap (or keymap *normal-keymap*)
		       :accept-does-newline accept-does-newline
		       :terminal-device-name terminal-name
		       :terminal-class terminal-class))))
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
    (with-slots (quit-flag exit-flag cmd buf last-input terminal debugging) e
      (let ((result nil))
	(unwind-protect
	     (loop :do
		(finish-output)
		(when debugging
		  (message e "~d ~d [~d x ~d] ~w"
			   (screen-col e) (screen-row e)
			   (terminal-window-columns terminal)
			   (terminal-window-rows terminal)
			   cmd)
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
		      (when exit-flag (setf result quit-value))))
		(setf last-input cmd)
		:while (not quit-flag))
	  (block nil
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
