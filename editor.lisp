;;;
;;; editor.lisp
;;;

(in-package :rl)

(declaim #.`(optimize ,.(getf rl-config::*config* :optimization-settings)))

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

#|
(defclass line-editing-location (editing-location)
  ((position
    :initarg :position :accessor line-editing-location-position
    :initform 0 :type fixnum
    :documentation "Position in the sequence."))
  (:documentation "An editing location in the line editor."))

(defun make-point (n)
  (make-instance 'line-editing-location :position n))
|#

(defclass line-editing-context (editing-context)
  ()
  (:default-initargs
   :point 0
   :mark nil
   :clipboard nil)
  (:documentation "Editing context for the line editor."))

(defmethod print-object ((object line-editing-context) stream)
  "Print a line-editing-context to STREAM."
  (with-slots (point mark clipboard) object
    (print-unreadable-object (object stream :type t)
      (format stream "~s ~s ~a" point mark
	      (typecase clipboard
		(fatchar-string (make-fat-string :string clipboard))
		(t clipboard))))))

(defun make-contexts (&key (n 1) copy-from)
  (if copy-from
      (make-array n :element-type 'line-editing-context
		  :initial-contents
		  (map 'list (_ (copy-editing-context _)) copy-from)
		  :adjustable t)
      (make-array n :element-type 'editing-context
		  :initial-element (make-instance 'line-editing-context)
		  :adjustable t)))

(defvar *line-editor* nil
  "The last line editor that was instantiated. This is for debugging, since
it can be somewhat unpredictable, especially with threads. Don't use it for
anything important.")

;; The history is not in here because it is shared by all editors.
(defclass line-editor (terminal-inator multi-inator-mixin)
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
   (buf-str
    :accessor buf-str
    :initform nil
    :initarg :buf-str
    :documentation "The buffer as a fat-string.")
   ;; (screen-row
   ;;  :accessor screen-row
   ;;  :initform 0
   ;;  :documentation "Screen row of the cursor.")
   (screen-relative-row
    :accessor screen-relative-row
    :initform 0
    :documentation "Screen row of the cursor relative to where we started.")
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
   (last-line
    :initarg :last-line
    :accessor last-line
    :initform nil
    :documentation "Last line of the buffer.")
   (history-context
    :accessor history-context
    :initarg :history-context
    :initform :tiny
    :documentation "A symbol selecting what line history to use.")
   (allow-history-duplicates
    :initarg :allow-history-duplicates
    :accessor line-editor-allow-history-duplicates
    :initform nil :type boolean
    :documentation
    "True to allow adding multiple history lines with the same text.")
   (allow-history-blanks
    :initarg :allow-history-blanks
    :accessor line-editor-allow-history-blanks
    :initform nil :type boolean
    :documentation "True to allow adding blank lines to the history.")
   (history-store-style
    :initarg :history-store-style
    :accessor line-editor-history-store-style
    :initform :fancy :type (member :fancy :simple)
    :documentation
    "Style of the history storage. Use :SIMPLE to store only the text,
:FANCY to store other attributes.")
   (history-store-format
    :initarg :history-store-format
    :accessor line-editor-history-store-backend
    :initform :database :type (member :database :text-file)
    :documentation
    "Format for the history store. Either :DATABASE or :TEXT-FILE.")
   (history-store-file-name
    :initarg :history-store-file-name
    :accessor line-editor-history-store-file-name  
    :documentation "File name of the history store.")
   (history-storage-unified
    :initarg :history-storage-unified
    :accessor line-editor-history-storage-unified :initform nil :type boolean
    :documentation
    "True to all command history in the same database. This probably only works
for database formats.")
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
   (undo-recent-count
    :initarg :undo-recent-count :accessor undo-recent-count
    :initform 0 :type fixnum
    :documentation "How many undos have been done recently.")
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
   (right-prompt
    :accessor right-prompt
    :initarg :right-prompt
    :documentation "Something to display on the right side of the command line.")
   (prompt-height
    :accessor prompt-height
    :initarg :prompt-height
    :initform nil
    :documentation "Height of the prompt in lines.")
   (completion-func
    :accessor completion-func
    :initarg :completion-func
    :documentation "Function to call to generate completions.")
   (filter-hook
    :accessor filter-hook
    :initarg :filter-hook
    :initform nil
    :documentation "Functions to call to filter the buffer.")
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
   (need-to-recolor
    :accessor need-to-recolor
    :initarg :need-to-recolor
    :initform nil
    :documentation "True if we need to recolor some of the line.")
   (old-line
    :initarg :old-line :accessor old-line :initform nil
    :documentation "A copy of the line as it was previously.")
   (temporary-message
    :initarg :temporary-message :accessor temporary-message
    :initform nil
    :documentation
    "Temporary message to display, or NIL if none.")
   (max-message-lines
    :initarg :max-message-lines :accessor max-message-lines
    :initform 0 :type fixnum
    :documentation
    "The maximum number of message lines available. Set by the last redisplay.")
   (keep-message
    :initarg :keep-message :accessor keep-message :initform nil :type boolean
    :documentation "True to keep the temporary message.")
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
   (highlight-region
    :initarg :highlight-region :accessor line-editor-highlight-region
    :initform t :type boolean
    :documentation "True to highlight the region.")
   (region-active
    :initarg :region-active :accessor line-editor-region-active
    :initform nil :type boolean
    :documentation
    "True if the region is active, which makes it eligible for highlighting.")
   (keep-region-active
    :initarg :keep-region-active :accessor line-editor-keep-region-active
    :initform nil :type boolean
    :documentation "True to keep the region active after the command is done.
Otherwise the region is deactivated every command loop.")
   (translate-return-to-newline-in-bracketed-paste
    :initarg :translate-return-to-newline-in-bracketed-paste
    :accessor translate-return-to-newline-in-bracketed-paste
    :initform t :type boolean
    :documentation "Does what is says on the package.")
   (last-search
    :initarg :last-search :accessor last-search :initform nil
    :documentation "The last string searched for."))
  (:default-initargs
    :contexts (make-contexts)
    :non-word-chars *default-non-word-chars*
    :prompt-string *default-prompt*
    :right-prompt nil
    :terminal-class (or (and *terminal* (class-of *terminal*))
			(find-terminal-class-for-type
			 (pick-a-terminal-type)))
  )
  (:documentation "State for a stupid little line editor."))

(defvar *initial-line-size* 20)

(defmethod initialize-instance :after ((e line-editor) &rest initargs)
  (dbugf :rl "init editor~%")
  ;; Make a terminal using the device name and class, or use *TERMINAL*.
  (when (not (and (slot-boundp e 'terminal) (slot-value e 'terminal)))
    (let ((default-class (or (slot-value e 'terminal-class)
		     (getf initargs :terminal-class))))
      (setf (slot-value e 'terminal)
	    (if (and (slot-boundp e 'terminal-device-name)
		     (slot-value e 'terminal-device-name))
		(make-instance default-class
			       :device-name (line-editor-terminal-device-name e)
			       :start-at-current-line t)
		(or (progn
		      (when *terminal*
			(dbug "Using *TERMINAL* ~a~%" (type-of *terminal*)))
		      *terminal*)
		    (make-instance default-class :start-at-current-line t))))))
  (dbugf :rl "terminal = ~s~%" (slot-value e 'terminal))

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
	  (make-stretchy-vector *initial-line-size* :element-type 'fatchar)
	  (slot-value e 'buf-str) (make-fat-string :string (slot-value e 'buf))))

  ;; Set the current dynamic var.
  (setf *line-editor* e))

(declaim (ftype (function (line-editor) fixnum) first-point)
	 (inline first-point))
(defun first-point (e)
  "Get the value of the first point."
  (inator-point (aref (inator-contexts e) 0)))

(defun set-first-point (e p)
  "Set the first point of the editor E to P."
  (setf (inator-point (aref (inator-contexts e) 0)) p))

(defsetf first-point set-first-point
  "Set the first point.")

#|
(defun incf-all-points (e increment)
  (with-slots (point) e
    ;; (map-into point (_ (+ _ increment)) point)
    (loop :for i :from 0 :below (length point)
       :do (incf (aref point i) increment))
    ))

(defun new-points (e)
  (make-array (length point) :element-type 'fixnum :initial-element pos))
|#

(defvar *context* nil
  "The current editing context.")

(defmacro use-context ((context) &body body)
  "Use CONTEXT as the dynamic editing context."
  `(let ((*context* ,context))
     ,@body))

(defmacro use-first-context ((e) &body body)
  "Use the first context in the editor E, as the dynamic editing context."
  `(use-context ((aref (inator-contexts ,e) 0))
     ,@body))

(defmacro with-context (() &body body)
  "Evaluate the BODY with point, mark, and clipboard bound from *CONTEXT*."
  `(with-slots ((point     inator::point)
		(mark      inator::mark)
		(clipboard inator::clipboard)) *context*
     (declare (ignorable point mark clipboard))
     ,@body))

(defmacro do-contexts ((e) &body body)
  "Evaluate the BODY once for each context in the editor E, with point, mark,
and clipboard bound."
  (with-unique-names (c)
    `(loop :for ,c :across (inator-contexts ,e) :do
	(use-context (,c) ,@body))))

(defun set-all-points (e pos)
  (do-contexts (e)
    (with-context ()
      (setf point pos))))

(defun copy-contexts (e)
  "Return a copy of all the editing contexts in the editor E."
  (make-contexts :n (length (inator-contexts e))
		 :copy-from (inator-contexts e)))

(defun add-context (e point mark)
  (with-slots ((contexts inator::contexts)) e
    (adjust-array contexts (1+ (length contexts))
		  :element-type 'editing-context
		  :initial-element (make-instance 'line-editing-context))
    (let ((new (aref contexts (1- (length contexts)))))
      (setf (inator-point new) point
	    (inator-mark new) mark))))

(defgeneric freshen (e)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

(defmethod freshen ((e line-editor))
  "Make the editor ready to read a fresh line."
  (setf (inator-command e)	nil
	(inator-last-command e) nil
	(last-event e)          nil
	;;(inator-point e)	#(0)
	;;(inator-mark e)		nil
	;;(inator-clipboard e)	nil	; should we really?
	(inator-contexts e)     (make-contexts)
	(inator-quit-flag e)	nil
	(fill-pointer (buf e))	0
;;;	(screen-row e) (terminal-get-cursor-position (line-editor-terminal e))
	(screen-relative-row e) 0
	(screen-col e)		0
	(start-col e)		0
	(start-row e)		0
	(undo-history e)	nil
	(undo-current e)	nil
	(need-to-redraw e)	nil
	(exit-flag e)		nil
	(did-under-complete e)	nil))

#| old-way without contexts
(defmacro save-excursion ((e) &body body)
  "Evaluate the body with the buffer, point, and mark restored afterward."
  (with-unique-names (saved-buf saved-point saved-mark)
    `(let ((,saved-buf (buf ,e))
	   (,saved-point (inator-point ,e))
	   (,saved-mark (inator-mark ,e)))
       (unwind-protect
	    (progn
	      ,@body)
	 (setf (buf ,e) ,saved-buf
	       (inator-point ,e) ,saved-point
	       (inator-mark ,e) ,saved-mark)))))
|#

(defmacro save-excursion ((e) &body body)
  "Evaluate the body with the buffer, point, and mark restored afterward."
  (with-unique-names (saved-buf saved-contexts)
    `(let ((,saved-buf (buf ,e))
	   (,saved-contexts (inator-contexts e)))
       (unwind-protect
	    (progn
	      (setf (inator-contexts e) (copy-contexts e))
	      ,@body)
	 (setf (buf ,e) ,saved-buf
	       (inator-contexts e) ,saved-contexts)))))

;; For use in external commands.

(defun get-buffer-string (e)
  "Return a string of the buffer."
  (buffer-string (buf e)))

;; @@@ compatibility
;; (defalias 'point 'inator-point)
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
