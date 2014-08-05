;;
;; tiny-rl.lisp - A tiny readline replacement for ANSI terminals.
;;

;; OK, it's getting to be not so tiny anymore.
;; OK, so it's kind of like a huge guy you might facetiously call tiny.
;; OK, so it's a 300 pound gorilla called "Tiny".
;; OK, so I haven't learned my lesson yet.

;; TODO:
;;   multi line - paren flashing, display bugs (undo, delete word, etc)
;;   history saving

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defpackage "TINY-RL"
  (:use :common-lisp :dl-list :stretchy :cffi :opsys :ansiterm :termios
	:completion :dlib :dlib-misc :keymap :char-util)
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
   ;; misc
   #:rl-on-device
  )
)
(in-package "TINY-RL")

; @@@ just for debugging, remove later
(defvar *line-editor* nil "The last line editor that was instantiated.")

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
;;; print the list backwards from tail to the head.

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
  (concatenate 'vector *lisp-non-word-chars* #(#\-))
  "Characters that are not considered to be part of a word by default.")

(defvar *default-prompt* "> "
  "Output before reading to let you know it's your turn.")

(defun default-output-prompt (e &optional (p nil prompt-supplied))
  "The default prompt output function. Prints *default-prompt* unless a ~
   prompt is supplied."
  (editor-write-string
   e (format nil "~a" (if prompt-supplied p *default-prompt*))))

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
   (last-completion-unique
    :accessor last-completion-unique
    :initarg :last-completion-unique
    :initform nil
    :documentation "True if the last completion was unique.")
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
   )
  (:default-initargs
    :non-word-chars *default-non-word-chars*
    :prompt *default-prompt*
  )
  (:documentation "State for a stupid little line editor."))

(defvar *initial-line-size* 20)

(defmethod initialize-instance :after ((e line-editor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value e 'terminal)
	(if (and (slot-boundp e 'terminal-device-name)
		 (slot-value e 'terminal-device-name))
	    (make-instance 'terminal
			   :device-name (line-editor-terminal-device-name e))
	    (make-instance 'terminal)))
  ;; Make a default line sized buffer if one wasn't given.
  (when (or (not (slot-boundp e 'buf)) (not (slot-value e 'buf)))
    (setf (slot-value e 'buf) (make-stretchy-string *initial-line-size*)))
  (setf *line-editor* e))

(defgeneric freshen (e)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized, but perhaps reuse some resources."))

(defmethod freshen ((e line-editor))
  "Make the editor ready to read a fresh line."
  (setf (cmd e)			nil
	(last-input e)		nil
	(point e)		0
	(fill-pointer (buf e))	0
	(screen-row e) (terminal-get-cursor-position (line-editor-terminal e))
	(screen-col e)		0
	(start-col e)		0
	(undo-history e)	nil
	(undo-current e)	nil
	(quit-flag e)		nil
	(exit-flag e)		nil))

(defun get-a-char (e)
  "Read a character from the editor's tty."
  (declare (type line-editor e))
  (tt-finish-output e)
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

;; This can unfortunatly really vary between emulations, so we try
;; to code for multiple intpretations.
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
  (undo-one e)
  (redraw e))

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
(tt-alias tt-standout)
(tt-alias tt-standend)
(tt-alias tt-underline state)
(tt-alias tt-beep)
(tt-alias tt-finish-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display-ish code

(defun message (e fmt &rest args)
  "Show a little message for debugging."
  (declare (type line-editor e))
  (tt-cursor-off e)
  (tt-move-to e 5 5)		; The only place we should call this
  (tt-standout e)
  (apply #'tt-format (cons (line-editor-terminal e) (cons fmt args)))
  (tt-standend e)
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

;; Moved to dlib
;; (defun alist-to-hash-table (vec table)
;;   (loop :for (k . v) :in vec
;;      :do (setf (gethash k table) v))
;;   table)

(defparameter *control-char-graphics* nil)

(defun control-char-graphic (c)
  (when (not *control-char-graphics*)
    (setf *control-char-graphics* (make-hash-table :test #'eql))
    (alist-to-hash-table *control-char-graphics-vec*
			 *control-char-graphics*))
  (gethash c *control-char-graphics*))

(defgeneric display-length (obj)
  (:documentation "Return how long is the object should be when displayed."))

(defmethod display-length ((c character))
  "Return the length of the character for display."
  (cond
    ((graphic-char-p c)
     1)					;normal case XXX wrong for unicode
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
	 (len (length s))
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
    ;; @@@ Maybe we should try to see if this is useful?
;    (editor-write-string e
    (tt-write-string e
     (with-output-to-string (s)
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

(defun update-for-delete (e delete-length)
  "Update the display, assuming DELETE-LENGTH characters were just deleted at 
the current cursor position."
  (with-slots (buf point terminal) e
    (let ((width (terminal-window-columns terminal))
	  (col (screen-col e))
	  (right-len (display-length (subseq buf point))))
      ;; If the rest of the buffer extends past the edge of the window.
      (when (>= (+ col right-len) width)
	 ;; Cheaty way out: redraw whole thing after point
	(without-messing-up-cursor (e)
	  (let* ((new-col (- width (screen-col e) delete-length))
		 (from (+ point new-col))) ; wrong XXX
	    (move-forward e new-col)
	    (display-buf e from)
	    (tt-erase-to-eol e)))))))

#|
;; (defun new-update-for-delete (e delete-length)
;;   "Update the display, assuming DELETE-LENGTH characters were just deleted at 
;; the current cursor position."
;;   (with-slots (buf point terminal) e
;;     (let ((width     (terminal-window-columns terminal))
;; 	  (col       (screen-col e))
;; 	  (right-len (display-length (subseq buf point))))
;;       (cond
;; 	;; If the rest of the buffer extends past the edge of the window.
;; 	((>= (+ col right-len) width)
;; 	 ;; Cheaty way out: redraw whole thing after point
;; 	 (loop
;; 	   :with remain = 
;; 	   :while (> remain 0)
;; 	   :do
;; 	   (setf sub-remain (min (- pos width)))
;; 	    (tt-del-char ))
;; 	 (without-messing-up-cursor (e)
;; 	   (let* ((new-col (- width (screen-col e) display-length))
;; 		  (from (+ point new-col))) ; wrong XXX
;; 	     (move-forward e new-col)
;; 	     (display-buf e from)
;; 	     (tt-erase-to-eol e))))
;; 	;; If the deleted part extends past the edge of the window.
;; 	((>= (+ col delete-len) width)
|#

#|
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbb ccccccccccccccccccccccccccccccccccccdddd

aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ddddeeeee

|#

(defun erase-display (e)
  "Erase the display of the buffer, but not the buffer itself."
  (with-slots (buf) e
    (beginning-of-line e)
    (tt-erase-to-eol e)
    (let* ((cols (terminal-window-columns (line-editor-terminal e)))
	   (buf-len (length buf))
	   (lines-to-clear (truncate (+ (screen-col e) buf-len) 80)))
      (when (> (+ buf-len (screen-col e)) cols)
	(loop for i from 1 to lines-to-clear
	      do (tt-down e 1)
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

(defun redraw (e)
  "Erase and redraw the whole line."
  (tt-move-to-col e 0)
  (tt-erase-to-eol e)
  (setf (screen-col e) 0)
  (do-prompt e (prompt e) (prompt-func e))
  (finish-output (terminal-output-stream
		  (line-editor-terminal e)))
  (display-buf e)
  (with-slots (point buf) e
    (when (< point (length buf))
      (let ((disp-len (display-length (subseq buf point))))
	(move-backward e disp-len)))))

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
	  (loop while (and (> point 0)
			   (funcall func (aref buf (1- point))))
	    do
	    (when action
	      (when (setf cc (funcall action (aref buf (1- point))))
		(buffer-replace e (1- point) cc)))
	    (decf point))
	  (let ((len (length buf))
		(did-one nil))
	    (loop while (and (< point len)
			     (funcall func (aref buf point)))
	      do
	      (progn
		(when action
		  (when (setf cc (funcall action (aref buf point)))
		    (buffer-replace e point cc)
		    (setf did-one t)))
		(incf point)))
	    (when did-one (decf point)))))))

(defun backward-word (e)
  "Move the insertion point to the beginning of the previous word or the beginning of the buffer if there is no word."
  (with-slots (point buf non-word-chars) e
    (let ((start point))
      (scan-over e :backward :func #'(lambda (c) (position c non-word-chars)))
      (scan-over e :backward :not-in non-word-chars)
      (move-backward e (display-length (subseq buf point start))))))

(defun forward-word (e)
  "Move the insertion point to the end of the next word or the end of the buffer if there is no word."
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

(defun accept-line (e)
  (with-slots (buf quit-flag context) e
;    (if (> (length buf) 0)
    (history-put (context e) (buf e))
    (history-last (context e))
    (tt-write-char e #\newline)
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

(defun display-search (e str pos)
  "Display the current line with the search string highlighted."
  (with-slots (buf point) e
    (tt-move-to-col e 0)
    (tt-erase-to-eol e)
    (setf (screen-col e) 0)
    (do-prefix e "isearch: ")
    (when str
      (without-undo (e)
;	(erase-display e)
	(buffer-delete e 0 (length buf))
	(buffer-insert e 0 (or (history-current (context e)) ""))
	(loop :with end = (if pos (+ pos (length str)) nil)
	   :for c :across buf :and i = 0 :then (1+ i) :do
	   (cond
	     ((and pos (= i pos))
	      (tt-underline e t))
	     ((and end (= i end))
	      (tt-underline e nil)))
	   (display-char e c))
	(tt-underline e nil)))
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
  "Incremental search which updates the search position as the user types. The search can be ended by typing a control character, which usually performs a command, or Control-G which stops the search and returns to the start. Control-R searches again backward and Control-S searches again forward."
  (with-slots (point buf cmd context) e
    (let ((quit-now nil)
	  (start-point point)
	  (start-hist (history-current-get context))
	  (search-string (make-stretchy-string *initial-line-size*))
	  (start-from (or (history-current-get context)
			  (history-head (get-history context))))
	  (pos point) old-pos c added)
      (labels ((redisp ()
		 (tt-move-to-col e 0)
		 (tt-erase-to-eol e)
		 (setf (screen-col e) 0)
		 (do-prefix e "isearch: ")
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
  (with-slots (prompt prompt-func point buf) e
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
	(move-backward e disp-len)))))

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
	(update-for-delete e del-len)))))

(defun delete-char (e)
  "Delete the character following the cursor."
  (with-slots (point buf) e
    (if (= point (fill-pointer buf))
	(beep e "End of buffer")
	(progn
	  (let ((del-len (display-length (aref buf point))))
	    (buffer-delete e point (1+ point))
	    (tt-del-char e del-len)
	    (update-for-delete e del-len))))))

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
	(update-for-delete e del-len)))))

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
	(update-for-delete e del-len)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hacks for typing lisp

;; I suppose we could try to use the reader, but I'm not sure I know
;; how to turn off the stuff we don't want while leaving on the stuff
;; we do. It's probably easiest to just do simplistic scanning.

(defun matching-paren-pos (str &key (pos (length str)))
  "Return the position in STR of an open paren matching a, perhaps ~
hypothetical, one at POS. If there is none, return nil. If POS is not ~
provided, it defaults to the end of the string."
  (declare (type string str))
  (let ((starts nil))
    (loop
       :with i = 0 :and c = nil
       :while (< i pos)
       :do (setf c (aref str i))
       (cond
	 ((or (eql c #\() (eql c #\[)) (push i starts))
	 ((or (eql c #\)) (eql c #\])) (pop starts))
	 ((eql c #\")
	  ;; scan past the string
	  (incf i)
	  (loop while (and (< i pos)
			   (not (eql #\" (setf c (aref str i)))))
	     do (when (eql c #\\)
		  (incf i))
	       (incf i)))
	 ;; # reader macro char
	 ((and (eql c #\#) (< i (- pos 2)))
	  (incf i)
	  (case (setf c (aref str i))
	    (#\\ (incf i))		; ignore the quoted char
	    ;; scan past matched #| comments |#
	    (#\|
	     (incf i)
	     (loop :with level = 1
		:while (and (< i pos) (> level 0))
		:do
		  (setf c (aref str i))
		  (cond
		    ((and (eql c #\|) (and (< i (1- pos))
					   (eql #\# (aref str (1+ i)))))
		     (decf level))
		    ((and (eql c #\#) (and (< i (1- pos))
					   (eql #\| (aref str (1+ i)))))
		     (incf level)))
		(incf i)))))
	 ;; single line comment
	 ((eql c #\;)
	  (loop :while (and (< i pos)
			    (not (eql #\newline (setf c (aref str i)))))
	     :do (incf i))))
	(incf i))
    (first starts)))

;; @@@ BUG: this doesn't really work for multi-line
(defun flash-paren (e c)
  (declare (ignore c))
  (let* ((str (buf e))
	 (point (point e))
	 (ppos (matching-paren-pos str :pos point))
	 (tty-fd (terminal-file-descriptor (line-editor-terminal e))))
    (if ppos
	(let ((saved-col (screen-col e)))
	  (tt-move-to-col e (+ ppos (start-col e)))
	  (tt-finish-output e)
	  (listen-for .5 tty-fd)
	  (tt-move-to-col e saved-col))
	(beep e "No match."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

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
	  (when (> comp-count 0)
	    ;; downcased list 1 per line
	    (let ((saved-point point))
	      (end-of-line e)
	      (setf point saved-point))
	    (tt-write-char e #\newline)
;		   (tt-format e "~{~a~%~}" comp-list)
	    (print-completions e comp-list)
	    (setf (screen-col e) 0)
	    (do-prompt e prompt prompt-func)
	    (display-buf e)
	    (when (< point (length buf))
	      (move-backward e (display-length
				(subseq buf point))))))))))

(defun complete (e)
  (with-slots (completion-func point buf last-input) e
    (if (not completion-func)
	(beep e "No completion active.")
	(progn
	  (let* ((saved-point point) comp replace-pos unique)
	    (multiple-value-setq (comp replace-pos unique)
	      (funcall completion-func buf point nil))
	    (when (and (eql last-input #\tab)
		       (not (last-completion-unique e)))
	      (show-completions e))
	    (setf (last-completion-unique e) unique)
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

(defun do-prefix (e prompt-str)
  "Output a prefix. The prefix should not span more than one line."
  (incf (screen-col e) (length prompt-str))
  (setf (start-col e) (screen-col e))	; save the starting column
  (tt-write-string e prompt-str)
  (finish-output))

(defun do-prompt (e prompt output-prompt-func)
  "Output the prompt in a specified way."
;  (format t "e = ~w prompt = ~w output-prompt-func = ~w~%"
;	  e prompt output-prompt-func)
  (let* ((s (with-output-to-string (*standard-output*)
              (if output-prompt-func
		  (funcall output-prompt-func e prompt)
		  (default-output-prompt e prompt)))))
    (do-prefix e s)))

(defun pop-to-lish (e)
  "If we're inside lish, throw to a quick exit. If we're not in lish,
enter it."
;  (break)
  (let ((lish-package (find-package :lish)))
    (when lish-package
      (if (and (boundp (intern "*LISH-LEVEL*" lish-package))
	       (numberp (symbol-value (intern "*LISH-LEVEL*" lish-package))))
	  (funcall (find-symbol "LISHITY-SPLIT" :lish))
	  (progn
	    (tt-beginning-of-line e)
	    (tt-erase-line e)
	    (finish-output (terminal-output-stream (line-editor-terminal e)))
	    (terminal-end (line-editor-terminal e))
	    (funcall (find-symbol "LISH" :lish))
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
  (with-simple-restart (continue "Continue TINY-RL")
    (invoke-debugger (make-condition
		      'simple-condition
		      :format-control "Abort command"))))

(defun toggle-debugging (e)
  "Toggle debugging output."
  (with-slots (debugging) e
    (setf debugging (not debugging))))

(defun quoted-insert (e)
  "Insert the next character input without interpretation."
  (setf (cmd e) (get-a-char e))
  (self-insert e t))

(defun self-insert (e &optional quoted)
  "Try to insert a character into the buffer."
  (with-slots (cmd buf point) e
    (if (and (not (graphic-char-p cmd)) (not quoted))
	(beep e "~a is unbound." cmd)
	(progn 
	  ;; a normal character
	  (if (= (length buf) point)
	      ;; end of the buf
	      (progn
		(display-char e cmd)
		;; flash paren and keep going
		(when (or (eql cmd #\)) (eql cmd #\]))
		  (flash-paren e cmd))
		(insert-char e cmd)
		(incf point))
	      ;; somewhere in the middle
	      (progn
		(tt-ins-char e (display-length cmd))
		(display-char e cmd)
		(when (or (eql cmd #\)) (eql cmd #\]))
		  (flash-paren e cmd))
		(insert-char e cmd)
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

(defun exit-editor (e)
  "Stop editing."
  (with-slots (quit-flag exit-flag) e
    (setf quit-flag t
	  exit-flag t)))

(defkeymap *normal-keymap*
  `(
    ;; Movement
    (,(ctrl #\B)		. backward-char)
    (,(ctrl #\F)		. forward-char)
    (,(ctrl #\A)		. beginning-of-line)
    (,(ctrl #\E)		. end-of-line)
    (,(ctrl #\P)		. previous-history)
    (,(ctrl #\N)		. next-history)
    (,(meta-char #\b)	. backward-word)
    (,(meta-char #\f)	. forward-word)
    (,(meta-char #\<)	. beginning-of-history)
    (,(meta-char #\>)	. end-of-history)

    ;; Editing
    (#\return		. accept-line)
    (#\newline		. accept-line)
    (#\backspace	. delete-backward-char)
    (#\rubout		. delete-backward-char)
    (,(ctrl #\D)		. delete-char-or-exit)
    (,(ctrl #\W)		. backward-kill-word)
    (,(ctrl #\K)		. kill-line)
    (,(ctrl #\@)		. set-mark)
    (,(ctrl #\Y)		. yank)
    (,(ctrl #\U)		. backward-kill-line)
    (,(ctrl #\O)		. undo-command)
    (,(meta-char #\d)	. kill-word)
    (,(meta-char #\rubout) . backward-kill-word)
    (,(meta-char #\u)	. upcase-word)
    (,(meta-char #\l)	. downcase-word)
    (,(meta-char #\c)	. capitalize-word)
    (,(meta-char #\w)	. copy-region)

    ;; Completion
    (#\tab		. complete)
    (#\?		. show-completions)

    ;; Misc
    (,(ctrl #\L)		. redraw-command)
    (,(ctrl #\G)		. abort-command)
    (,(ctrl #\S)		. isearch-forward)
    (,(ctrl #\R)		. isearch-backward)
    (,(ctrl #\T)		. toggle-debugging) ; @@@ temporary?
    (,(ctrl #\Q)		. quoted-insert)

    ;; key binding
;    (,(meta-char #\=)	. describe-key-briefly)
;    (,(meta-char #\+)	. set-key-command)

    ;; Other keymaps
    (#\escape		. *escape-keymap*)
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
    (,(ctrl #\C)	. exit-editor)
    (,(ctrl #\X)	. exchange-point-and-mark)))
;  :default-binding #| (beep e "C-x ~a is unbound." cmd |#

(defkeymap *escape-raw-keymap*
  `(
    (#\O	. do-app-key)
    (#\[	. do-function-key)
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
       (funcall action e))
      (t					; anything else is an error
       (error "Weird thing in keymap: ~s." action)))))

;; The main entry point

(defun tiny-rl (&key (input-stream *standard-input*)
		     (eof-error-p t)
		     (eof-value nil)
		     (quit-value nil)
		     (recursive-p nil)
		     (prompt *default-prompt*)
		     (output-prompt-func nil)
		     (completion-func #'complete-symbol)
		     (in-callback nil)
		     (out-callback nil)
		     (debug nil)
		     (editor nil)
		     (terminal-name nil)
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
    Function to print out a prompt. Called with the LINE-EDITOR instance and a prompt string.
  COMPLETION-FUNC (#'complete-symbol)
    Completion function to use. See the completion package for details.
  EDITOR (nil)
    LINE-EDITOR instance to use.
  TERMINAL-NAME (nil)
    Name of a terminal device to use.
  CONTEXT (nil)
    Symbol or string which defines the context for keeping history.
" ;; **Edit the docstring with orgtbl-mode, which I'm not really sure is a good idea.
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
		       :terminal-device-name terminal-name))))
    (when editor
      (freshen editor))
    (setf (fill-pointer (buf e)) (point e))
    (terminal-start (line-editor-terminal e))

    ;; Don't add blank or duplicate lines.
    (let* ((cur (history-current-get context))
	   (prev (dl-prev cur)))
      (when (not (or (and cur (dl-content cur) (= (length (dl-content cur)) 0))
		     (and prev (dl-content prev)
			  (equal (dl-content prev) (dl-content cur)))))
	(history-add context nil)
	(history-next context)))

    ;; Output the prompt
    (setf (prompt e) prompt (prompt-func e) output-prompt-func)
    (do-prompt e (prompt e) (prompt-func e))

    ;; Command loop
    (with-slots (quit-flag exit-flag cmd buf last-input terminal debugging) e
      (let ((result nil))
	(unwind-protect
	   (loop :do
	      (finish-output)
	      (when debugging
		(message e "~d ~d [~d x ~d] ~w ~w"
			 (screen-col e) (screen-row e)
			 (terminal-window-columns terminal)
			 (terminal-window-rows terminal)
			 cmd (history-current-get context))
		(show-message-log e))
	      (setf cmd (get-a-char e))
	      (log-message e "cmd ~s" cmd)
	      (if (equal cmd '(nil))
		  (if eof-error-p
		      (error (make-condition 'end-of-file
					     :stream input-stream))
		      (setf result eof-value))
		  (progn
		    (perform-key e cmd *normal-keymap*)
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

;; EOF
