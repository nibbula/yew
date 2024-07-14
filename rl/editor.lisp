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

(defvar *default-partial-line-indicator*
  (make-fatchar :c #\% #| #\⏎ |# :attrs '(:standout))
  "A suggestion to use for the partial-line-indicator. The default is actually
NIL, so we don't assume what's come before.")

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

(defclass line-editing-spot (spot)
  ((editor
    :initarg :editor :accessor spot-editor :initform nil
    :documentation "The editor the spot in in.")
   (position
    :initarg :position :accessor spot-position
    :initform 0 :type fixnum
    :documentation "Position in the sequence."))
  (:documentation "An editing location in the line editor."))

(defclass line-editing-spot-range (spot-range)
  ()
  (:documentation "A range of characters in the line editor buffer."))

(defclass line-editing-context (editing-context)
  ()
  (:default-initargs
   :point 0
   :mark nil
   :clipboard nil)
  (:documentation "Editing context for the line editor."))

(defvar *line-editor* nil
  "The last line editor that was instantiated. This is for debugging, since
it can be somewhat unpredictable, especially with threads. Don't use it for
anything important.")

(defclass line-editor (terminal-inator multi-inator-mixin options:options-mixin)
  ((buf
    :accessor buf
    :initform nil
    :initarg :buf
    :documentation "Current line buffer.")
   (buf-str
    :accessor buf-str
    :initform nil
    :initarg :buf-str
    :documentation "The buffer as a fat-string.")

   ;; Events
   (exit-flag
    :accessor exit-flag
    :initform nil
    :initarg :exit-flag
    :documentation "True if the user requested to stop editing.")
   (quit-value
    :accessor quit-value
    :initform nil
    :initarg :quit-value
    :documentation "Value to return from editing when quitting.")
   (filter-hook
    :accessor filter-hook
    :initarg :filter-hook
    :initform nil
    :documentation "Functions to call to filter the buffer.")
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
   (queued-input
    :accessor queued-input
    :initarg :queued-input :initform nil :type list
    :documentation "A list of queued events.")

   ;; Display
   (screen-relative-row
    :accessor screen-relative-row
    :initform 0
    :documentation "Screen row of the cursor relative to where we started.")
   ;; (screen-row
   ;;  :accessor screen-row
   ;;  :initform 0
   ;;  :documentation "Screen row of the cursor.")
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
   (show-mode-line
    :initarg :show-modeline :accessor show-mode-line
    :initform nil :type boolean
    :documentation "True to draw the modeline.")
   (mode-line
    :initarg :mode-line :accessor line-editor-mode-line :initform nil
    :documentation "ostring to show as a modeline.")
   (gutter-char
    :initarg :gutter-char :accessor gutter-char
    ;; :initform (make-fatchar :c #\… :fg :black :attrs '(:bold :inverse))
    :initform #\space
    :documentation
    "Character to fill the prompt gutter with, or NIL for no gutter.")

   ;; History
   ;;
   ;; The actual history content is not in here because it is shared by all
   ;; editors.
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
    "True to store all command history in the same database. This probably only
works for database formats.")
   (saved-line
    :accessor saved-line
    :initarg :saved-line
    :initform nil
    :documentation "Current line, saved when navigating history.")
   (match-element
    :initarg :match-element :accessor match-element :initform nil
    :documentation
    "History line element the match is on for match navigation, or NIL if there
is none.")

   ;; Undo
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

   ;; Susggestion
   (auto-suggest-p
    :initarg :auto-suggest-p
    :accessor line-editor-auto-suggest-p :initform t :type boolean
    :documentation
    "True to automatically suggest stuff.")
   (auto-suggest-style
    :initarg :auto-suggest-style
    :accessor line-editor-auto-suggest-style
    ;;:initform (make-fatchar :fg #(:rgb8 #x50 #x50 #x50)) :type fatchar
    :initform '(:fg :color #(:rgb8 #x50 #x50 #x50)) :type list
    :documentation "The style for the suggestion.")
   (suggestion
    :initarg :suggestion :accessor line-editor-suggestion :initform nil 
    :documentation "The thing that was last suggested.")

   ;; Prompt
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
    :initarg :right-prompt :initform nil
    :documentation "Something to display on the right side of the command line.")
   (prompt-start-at-left
    :initarg :prompt-start-at-left :accessor prompt-start-at-left
    :initform t :type boolean
    :documentation
    "True to make prompts start at the left side of the terminal.")
   (partial-line-indicator
    :initarg :partial-line-indicator :accessor partial-line-indicator
    :initform nil
    :documentation
    "A character printed to indicate a partial line before the prompt, or NIL
not to print one.")
   (prompt-height
    :accessor prompt-height
    :initarg :prompt-height
    :initform nil
    :documentation "Height of the prompt in lines.")

   ;; Completion
   (completion-func
    :accessor completion-func
    :initarg :completion-func
    :documentation "Function to call to generate completions.")
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

   ;; Message
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
   (message-lines
    :initarg :message-lines :accessor message-lines
    :initform 0 :type fixnum
    :documentation
    "The actual number of message lines. Set by the last redisplay.")
   (message-endings
    :initarg :message-endings :accessor message-endings
    :initform nil
    :documentation "Cached line endings of the message.")
   (message-top
    :initarg :message-top
    :accessor message-top
    :initform 0 :type integer
    :documentation "First line the message to display.")
   (keep-message
    :initarg :keep-message :accessor keep-message :initform nil :type boolean
    :documentation "True to keep the temporary message.")

   ;; Terminal
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
   (highlight-region
    :initarg :highlight-region :accessor line-editor-highlight-region
    :initform t :type boolean
    :documentation "True to highlight the region.")
   (copy-region-sets-selection
    :initarg :copy-region-sets-selection
    :accessor line-editor-copy-region-sets-selection :initform t :type boolean
    :documentation "True if copy-region should set the terminal selection.")
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
   (matching-char-pos
    :initarg :matching-char-pos :accessor matching-char-pos
    :initform nil :type (or null fixnum)
    :documentation
    "Position of a currently highlighted matching character, or NIL for none.")
   (saved-matching-char
    :initarg :saved-matching-char :accessor saved-matching-char
    :initform nil
    :documentation
    "Copy of the matching highlighted character to restore, or NIL for none.")

   (non-word-chars
    :accessor non-word-chars
    :initarg :non-word-chars
    :documentation "Characters that are not considered part of a word.")
   ;; (old-line
   ;;  :initarg :old-line :accessor old-line :initform nil
   ;;  :documentation "A copy of the line as it was previously.")
   (accept-does-newline
    :accessor accept-does-newline
    :initarg :accept-does-newline
    :initform t :type boolean
    :documentation "True if accept-line outputs a newline.")
   (translate-return-to-newline-in-bracketed-paste
    :initarg :translate-return-to-newline-in-bracketed-paste
    :accessor translate-return-to-newline-in-bracketed-paste
    :initform t :type boolean
    :documentation "Does what is says on the package.")
   (last-search
    :initarg :last-search :accessor last-search :initform nil
    :documentation "The last string searched for.")
   (recording-p
    :initarg :recording :accessor recording-p
    :type boolean :initform nil
    :documentation "Tro to record input events.")
   (recording
    :initarg :recording :accessor recording :initform nil
    :documentation "Record of input events.")
   (replay-count
    :initarg :replay-count :accessor replay-count :type integer :initform 0
    :documentation "Count of replays of a recording.")
   (pushed-buffers
    :initarg :pushed-buffers :accessor pushed-buffers
    :initform nil :type list
    :documentation "List of buffers and points to go back to editing.")
   (line-ending-cache
    :initarg :line-ending-cache :accessor line-ending-cache
    :initform nil
    :documentation "Cache of the results of calculate-line-endings.")

   (package
    :initarg :package :accessor line-editor-package :initform nil
    :documentation "Package for buffer local variables."))
  (:default-initargs
    :contexts (make-contexts)
    :non-word-chars *default-non-word-chars*
    :prompt-string *default-prompt*
    :right-prompt nil
    :default-keymap *normal-keymap*
    :terminal-class (or (and *terminal* (class-of *terminal*))
			(find-terminal-class-for-type
			 (pick-a-terminal-type)))
  )
  (:documentation "State for a stupid little line editor."))

(defvar *line-editor-prototype* nil
  "Thing for options mixin.")

(defvar *initial-line-size* 20)

(defstruct lossage-item
  "Recent keystroke history item."
  key
  command)

(defvar *lossage-default-size* 100
  "Default amount of keystrokes to remember.")

(defstruct lossage
  "Recent keystrokes."
  (size *lossage-default-size* :type fixnum)
  (fill 0 :type fixnum)
  (items #() :type vector))

(defvar *lossage* nil)

(defun init-lossage ()
  "Initialize the recent key history for editor ‘e’."
  (when (not *lossage*)
    (setf *lossage*
	  (make-lossage
	   :items (make-array *lossage-default-size* :initial-element nil)))))

(defun save-lossage (key command)
  "Save the ‘key’ and ‘command’ in the recent input history, if it's enabled."
  (with-slots (size fill items) *lossage*
    (when (not (zerop size))
      (if (aref items fill)
	  (setf (lossage-item-key (aref items fill)) key
		(lossage-item-command (aref items fill)) command)
	  (setf (aref items fill)
		(make-lossage-item :key key :command command)))
      (incf fill)
      (when (>= fill (length items))
	(setf fill 0)))))

(defun lossage-list ()
  "Return a list of lossage for editor ‘e’, from the most recent to the oldest."
  (with-slots (fill items) *lossage*
    (concatenate 'list
      (when (plusp fill)
	(loop :for i :from (1- fill) :downto 0
	  :when (aref items i)
	  :collect (vector (lossage-item-key (aref items i))
			   (lossage-item-command (aref items i)))))
      (loop :for i :from (1- (length items)) :downto fill
        :when (aref items i)
	:collect (vector (lossage-item-key (aref items i))
			 (lossage-item-command (aref items i)))))))

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

  ;; Make a default line sized buffer if one wasn't given.
  (when (or (not (slot-boundp e 'buf)) (not (slot-value e 'buf)))
    (setf (slot-value e 'buf)
	  (make-stretchy-vector *initial-line-size* :element-type 'fatchar)))
  (setf (slot-value e 'buf-str) (make-fat-string :string (slot-value e 'buf)))

  (init-lossage)

  ;; Set the current dynamic var.
  (setf *line-editor* e))

(defgeneric editor-package (e)
  (:documentation "Return the editor package.")
  (:method ((e line-editor))
    (or (line-editor-package e)
	(setf (line-editor-package e) (make-package (gensym "RL-"))))))

(defmethod (setf editor-package) (package (e line-editor))
  (setf (line-editor-package e) package))

;; @@@ Could this be simpler somehow??
(defmacro with-editor-vars ((editor &rest vars) &body body)
  "Evaluate ‘body’ with the list of symbols in ‘vars’ being macros for the same
named symbols in the local package for ‘editor’."
  (let* ((pkg-name (gensym "WEV-PKG"))
	 (pkg-var `(,pkg-name (editor-package ,editor)))
	 (prefix (gensym "WEV-PREFIX"))
	 (symbol-names
	   (loop :for v :in vars
		 :collect (symbolify (s+ prefix "-" v))))
	 (symbol-bindings
	   (loop :for v :in vars
		 :for s :in symbol-names
		 :collect `(,s (intern (symbol-name ',v) ,pkg-name))))
	 (macro-list
	   (loop :for v :in vars
		 :for s :in symbol-names
		 :collect `(,v (symbol-value ,s)))))
    `(let* (,pkg-var ,@symbol-bindings)
       (symbol-macrolet (,@macro-list)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spot methods

(defmethod object-at (spot)
  (aref (buf (spot-editor spot)) (spot-position spot)))

(defmethod (setf object-at) (value spot)
  (setf (aref (buf (spot-editor spot)) (spot-position spot)) value))

(defun make-point (n)
  (make-instance 'line-editing-spot :position n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spot range methods

(defmethod make-spot-range ((start line-editing-spot) (end line-editing-spot))
  (make-instance 'line-editing-spot-range :start start :end end))

(defmethod range-objects ((spot-range line-editing-spot-range))
  (with-slots ((start spot::start) (end spot::end)) spot-range
    (assert (eq (spot-editor start) (spot-editor end)))
    (subseq (buf (spot-editor (spot-range-start start)))
	   (spot-range-start spot-range)
	   (spot-range-end spot-range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context methods

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

(declaim (ftype (function (line-editor) fixnum) first-point)
	 (inline first-point))
(defun first-point (e)
  "Get the value of the first point."
  (values (inator-point (aref (inator-contexts e) 0))))

(defun set-first-point (e p)
  "Set the first point of the editor E to P."
  (setf (inator-point (aref (inator-contexts e) 0)) p))

(defsetf first-point set-first-point
  "Set the first point.")

(defgeneric get-buf-str (editor)
  (:documentation "Return the buffer as a fat string.")
  (:method ((editor line-editor))
    (if (slot-value editor 'buf-str)
	(progn
	  (setf (fat-string-string (slot-value editor 'buf-str))
		(slot-value editor 'buf))
	  (slot-value editor 'buf-str))
	(make-fat-string :string (slot-value editor 'buf)))))

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
  (with-names (c)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric freshen (e)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

;;; @@@@ this should be the approprate MOP initialize thing!
(defmethod freshen ((e line-editor))
  "Make the editor ready to read a fresh line."
  (when (line-editor-package e)
    (delete-package (line-editor-package e)))

  ;; Note that pushed-buffers should probably not be cleared so the
  ;; park-it command can work.
  (setf (fill-pointer (buf e))	0
	(exit-flag e)		nil
	(queued-input e)	nil
	(screen-relative-row e) 0
	(screen-col e)		0
	(start-col e)		0
	(start-row e)		0
	(last-line e)		nil
	(need-to-redraw e)	nil
	(need-to-recolor e)	nil
	(match-element e)	nil
	(undo-history e)	nil
	(undo-current e)	nil
	(undo-recent-count e)	0
	(line-editor-suggestion e) nil
	(did-complete e)	nil
	(did-under-complete e)	nil
	(last-command-was-completion e)	nil
	(last-completion-not-unique-count e) 0
	(temporary-message e)	nil
	(max-message-lines e)	0
	(message-lines e)	0
	(message-endings e)	nil
	(message-top e)		0
	(keep-message e)	nil
	(line-editor-debug-log e) nil
	(line-editor-region-active e) nil
	(matching-char-pos e)	nil
	(saved-matching-char e) nil
	(recording-p e)		nil
	(replay-count e)	0
	(line-ending-cache e)	nil
	(line-editor-package e) nil

	;; inator
	(inator-command e)	nil
	(inator-last-command e) nil
	(last-event e)          nil
	(inator-contexts e)     (make-contexts)
	(inator-quit-flag e)	nil))

#| old-way without contexts
(defmacro save-excursion ((e) &body body)
  "Evaluate the body with the buffer, point, and mark restored afterward."
  (with-names (saved-buf saved-point saved-mark)
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
  (with-names (saved-buf saved-contexts saved-region-active editor)
    `(let* ((,editor ,e)
	    (,saved-buf (buf ,editor))
	    (,saved-contexts (inator-contexts ,editor))
	    (,saved-region-active (line-editor-region-active ,editor)))
       (unwind-protect
	    (progn
	      (setf (inator-contexts ,e) (copy-contexts ,editor))
	      ,@body)
	 (setf (buf ,e) ,saved-buf
	       (inator-contexts ,editor) ,saved-contexts
	       (line-editor-region-active ,editor) ,saved-region-active)))))

(defmethod point-line-number (e point)
  "Return the abstract line number of ‘point’ in the editor."
  (declare (ignore e point))
  )

(defmethod point-coordinates (e point)
  "Return the screen line and column number of ‘point’ in the editor."
  (declare (ignore e point))
#|
  (let ((endings (line-ending-cache e)))
    (if endings
	@@@@@
	(let ((spots (list (list point))))
	  (setf (line-ending-cache e)
		(editor-calculate-line-endings e :spots spots))
	  (cdr (assoc point spots)))))
|#
)


(defmethod coordinates-point (e line column)
  "Return the point at the ‘line’ and ‘column’ coordinates in the editor."
  (declare (ignore e line column))
  )

;; For use in external commands.

(defun get-buffer-string (e)
  "Return a string of the buffer."
  (buffer-string (buf e)))

;; @@@ compatibility
;; (defalias 'point 'inator-point)
(defalias 'line-editor-keymap 'inator-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input

;; @@@ For backwards compatibility. Remove?
(defgeneric last-event (e)
  (:documentation "The last input event that occured.")
  (:method ((e line-editor)) (terminal-inator-last-event e)))

(defmethod (setf last-event) (value (e line-editor))
  (setf (terminal-inator-last-event e) value))

(defun get-a-char (e)
  "Read a character from the editor's terminal."
  (declare (type line-editor e))
  ;; (tt-finish-output)
  (let ((c (if (queued-input e)
	       (pop (queued-input e))
	       (tt-get-key))))
    (when (line-editor-input-callback e)
      (funcall (line-editor-input-callback e) c))
    c))

(defmethod await-event ((e line-editor))
  (setf (last-event e) (get-a-char e)))

;; @@@ What was the idea?
;; (defvar *key-tree* '())
;;   "")
;; (defun record-key (key)
;;   )

;; EOF
