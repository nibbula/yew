;;
;; curses-inator.lisp - Inator stuff for curses
;;

(defpackage :curses-inator
  (:documentation "Inator stuff for curses")
  (:use :cl :dlib :dlib-misc :stretchy :opsys :char-util :keymap :cffi
	:curses :terminal :terminal-curses :inator :fui)
  (:export
   #:curses-inator
   #:color-attr
   #:with-curses
   #:with-fg
   #:with-bg
   #:with-color
   #:set-colors
   #:add-char
   #:add-string
   ))
(in-package :curses-inator)

;; This used to be FUI before we converted it to terminal.
;; I think this whole thing should just go away eventually in favor of
;; using terminal-curses, with a terminal-inator. Eventually nothing should
;; use it.

;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1) (compilation-speed 2)))

#|
(defvar *interactive* t
  "True when we can expect user interaction.")
|#

(defvar *curses-device* nil
   "A device to do run a curses program on.")

(defvar *has-mouse* nil
  "True if the device has a mouse or pointer of some sort.")

(defvar *saved-terminal* nil
  "The saved (non-curses) terminal.")

(defun start-curses ()
    #|
    (when (not *device*)
      (initscr))
    (noecho)
    (nonl)
    ; (raw)
    (cbreak)
    (meta curses:*stdscr* 1)
    (keypad curses:*stdscr* 1)
    (typeahead -1)
    (start-color)
    ;; additional resets that wouldn't need to be done on a fresh application
    (attrset 0)
    (bkgd 0)
    (idlok curses:*stdscr* 0)
    (leaveok curses:*stdscr* 0)
    (scrollok curses:*stdscr* 0)
    (curs-set 1)
    (init-colors)
    |#
  (terminal-start *terminal*)
    
  ;; When curses SIGWINCH handler is overridden, we may have to set the
  ;; terminal size manually here.
  (with-os-file (tt (or *curses-device* *default-console-device-name*))
    (multiple-value-bind (cols lines)
	(get-window-size tt)
      (when (is-term-resized lines cols)
	(resizeterm lines cols))))

  ;; See if we can get mouse events
  (setf *has-mouse*
	(= (mousemask curses:+ALL-MOUSE-EVENTS+ (cffi:null-pointer)) 0)))

(defun end-curses ()
  ;; (when (and *terminal* (typep *terminal* 'terminal-curses)
  ;; 	     ;;(not (typep *saved-terminal* 'terminal-curses))
  ;; 	     )
  ;;(endwin)
  (terminal-end *terminal*))
;;(setf *terminal* *saved-terminal*)))

(defmacro with-curses (&body body)
  "Do the stuff in the body with curses initialized. Clean up properly."
  (let ((thunk (gensym "thunk")))
    `(flet ((,thunk () ,@body))
       (if (or (not *terminal*)
	       (and *terminal* (not (typep *terminal* 'terminal-curses))))
	   (let ((*terminal*
		  (if *curses-device*
		      (make-instance 'terminal-curses:terminal-curses
				     :device *curses-device*)
		      (make-instance 'terminal-curses:terminal-curses))))
	     (unwind-protect
	       (progn
		 (start-curses)
		 (,thunk))
	       (end-curses)))
	   (,thunk)))))

;; This is quite useful for having interaction go somewhere else when
;; debugging.
(defmacro with-device ((device term-type) &body body)
  "Do something with curses attached to DEVICE of of type TERM-TYPE."
  (dlib::with-unique-names (screen fd-in fd-out)
    `(let (,screen ,fd-in ,fd-out (*curses-device* ,device))
       (unwind-protect
	  (progn
	    (if (cffi:null-pointer-p (setf ,fd-in (nos:fopen ,device "r")))
		(error "Can't open curses input device ~a" ,device))
	    (if (cffi:null-pointer-p (setf ,fd-out (nos:fopen ,device "w")))
		(error "Can't open curses output device ~a" ,device))
	    (if (cffi:null-pointer-p (setf ,screen
				      (newterm ,term-type ,fd-out ,fd-in)))
		(error "Can't initialize curses terminal ~a" ,term-type))
	    (set-term ,screen)
	    (with-curses
		,@body))
	 (when ,screen
	   (delscreen ,screen))
	 (when ,fd-out
	   (nos:fclose ,fd-out))
	 (when ,fd-in
	   (nos:fclose ,fd-in))))))

(defun color-attr (fg bg)
  "Return the text attribute, e.g. for passing to setattr, for the
foreground FG and background BG."
  (color-pair (aref *color-table* fg bg)))

(defmacro with-fg ((color) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (terminal-curses:color-index ,color +color-black+)
		  (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (terminal-curses:color-index +color-white+ +color-black+)
		  (cffi:null-pointer))
       ,result)))

(defmacro with-bg ((color) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (terminal-curses:color-index +color-black+ ,color)
		  (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (terminal-curses:color-index +color-white+ +color-black+)
		  (cffi:null-pointer))
       ,result)))

(defmacro with-color ((fg bg) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (terminal-curses:color-index ,fg ,bg) (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (terminal-curses:color-index +color-white+ +color-black+)
		  (cffi:null-pointer))
       ,result)))

(defun set-colors (fg bg)
  (assert (and (or (integerp fg) (keywordp fg))
	       (or (integerp bg) (keywordp bg))))
  (color-set
   (color-index (if (integerp fg) fg (color-number fg))
		(if (integerp bg) bg (color-number bg)))
   (cffi:null-pointer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input

;; (defun get-char ()
;;   "Get a lisp character or function key from curses."
;;   (let ((cc (getch)))
;;     (cond
;;       ((> cc #xff)
;;        (function-key cc))
;;       ((and (integerp cc) (not (minusp cc)))
;;        (code-char cc))
;;       (t ;; Just return a negative
;;        cc))))

#| now in fui
(defmacro non-interactively (&body body)
  "Evaluate body without pausing for PAUSE."
  `(let ((*interactive* nil))
     ,@body))

(defmacro interactively (&body body)
  "Evaluate body with pausing for PAUSE."
  `(let ((*interactive* t))
     ,@body))

(defun pause (&optional (prompt "[Press Enter]") &rest args)
  "Print a message and wait for Enter to be pressed. Does nothing if not
*interactive*."
  (when *interactive*
    (apply #'format *standard-output* prompt args)
    (finish-output *standard-output*)
    (read-line)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

;; @@@ This should really be per-thread, but we should then probably make a
;; per-thread curses state object for use with `with-curses'. And it is also
;; currently a memory leak.
(defvar *wied-char* (cffi:foreign-alloc :int :count 2))
(defun %add-char (c)
  (setf (cffi:mem-aref *wied-char* :int 0) (char-code c))
  (addnwstr *wied-char* 1))

(defun add-char (thing)
  (ctypecase thing
    (character (%add-char thing))
    (string
     ;; This is rather stupid. But I think allocating a foreign string is not
     ;; a good idea here. Probably the calling code should just do the addnwstr.
     (loop :for c :across thing :do (%add-char c)))))

;; @@@ The comment for add-char also applies to this. Also this is slow and
;; tripple consy. We should probably have a static per-thread exapndable
;; conversion buffer, and only convert if we need to.
(defun add-string (s)
  (let ((len (length s)))
    (with-foreign-object (fstr :int (1+ len))
      (loop :with i = 0 :for c :across s :do
	 (setf (mem-aref fstr :int i) (char-code c))
	 (incf i))
      (setf (mem-aref fstr :int len) 0)
      (addnwstr fstr len))))

#| Now in fui using terminal-*

(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

;; This is kind of like a old-timey typeout window.
(defun display-text (title text-lines &key input-func (justify t))
  "Display text in a pop up window. Optionally calls INPUT-FUNC with the
window as an argument to get input. If no INPUT-FUNC is provided it just
waits for a key press and then returns."
  (with-curses
    (let* ((mid    (truncate (/ *cols* 2)))
	   (width  (min (- *cols* 6)
			(+ 4 (loop :for l :in text-lines
				:maximize (length l)))))
	   (height (min (+ 4 (loop :for l :in text-lines
				:sum
				(1+ (count
				     #\newline
				     (justify-text l :cols (- width 2)
						   :stream nil)))))
			(- *lines* 4)))
	   (xpos   (truncate (- mid (/ width 2))))
	   (w      (newwin height width 3 xpos))
	   result)
      (box w 0 0)
      (wcentered w width 0 title)
      (loop :with i = 2
	 :for l :in text-lines
	 :do
	 (if justify
	     (loop :for sub-line
		:in (split-sequence #\newline (justify-text l :cols (- width 2)
							    :stream nil))
		:do
		(mvwaddstr w i 2 sub-line)
		(incf i))
	     (progn
	       (mvwaddstr w i 2 l)
	       (incf i))))
      (refresh)
      (wrefresh w)
      (setf result (if input-func
		       (funcall input-func w)
		       (tt-get-char)))
      (delwin w)
      (clear)
      (refresh)
      result)))

(defun help-list (keymap &optional special-doc-finder)
  "Return a list of key binding help lines, suitable for the HELP function.
The optional SPECIAL-DOC-FINDER is a function which looks up documentation for
keymap bindings."
  ;; Make a reverse hash of functions to keys, so we can put all the bindings
  ;; for a function on one line.
  (let ((rev-hash (make-hash-table)) key-col-len)
    (flet ((add-key (k v) (push k (gethash v rev-hash))))
      (map-keymap #'add-key keymap))
    ;; Get the maxiumum size of the keys section
    (setf key-col-len
	  (loop :for func :being :the :hash-keys :of rev-hash
	     :maximize
	     (length (format nil "~{~a~^, ~}"
			     (loop :for k :in (gethash func rev-hash)
				:collect (nice-char k :caret t))))))
    ;; Actually collect the strings
    (loop :with doc
       :for func :being :the :hash-keys :of rev-hash
       ;; Look up the documentation for the function.
       :if (setf doc (or (and (or (functionp func)
				  (and (symbolp func) (fboundp func)))
			      (documentation func 'function))
			 (and special-doc-finder
			      (funcall special-doc-finder func))
			 (and (keymap-p func)
			      (string-downcase func))
			 nil))
       :collect
       (with-output-to-string (str)
	 (format str "~va - ~a" key-col-len
		 (format nil "~{~a~^, ~}"
			 (loop :for k :in (gethash func rev-hash)
			    :collect (nice-char k :caret t)))
		 doc)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; curses-inator

;; ToDo:
;; - maybe should have it's own augmented keymap
;; - bind-to-key

(defclass curses-inator (inator)
  ()
  (:documentation "A curses-inator."))

(defmethod initialize-instance
    :after ((o curses-inator) &rest initargs &key &allow-other-keys)
  "Initialize a curses-inator."
  (declare (ignore initargs)))

(defmethod start-inator ((i curses-inator))
  "Start a CURSES-INATOR."
  (start-curses)
  (call-next-method))

(defmethod finish-inator ((i curses-inator))
  "Stop a CURSES-INATOR."
  (end-curses)
  (call-next-method))

(defmethod update-display ((i curses-inator))
  "Update the view of a CURSES-INATOR."
  (call-next-method)
  (refresh))

(defmethod await-event ((i curses-inator))
  "Get an event from a CURSES-INATOR."
  (declare (ignore i))
  (tt-get-char))

(defmethod message ((i curses-inator) format-string &rest args)
  "Display a short message."
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (addstr (apply #'format nil format-string args)))

(defmethod prompt ((i curses-inator) format-string &rest args)
  (apply #'message i format-string args)
  (refresh))

(defun inator-doc-finder (i func)
  "Find documentation for an inator (subclass) method."
  (when (fboundp func)
    (let ((method
	   (and (typep (symbol-function func) 'generic-function)
		(find-method (symbol-function func) '()
			     (list (class-of i)) nil))))
      (when method (documentation method t)))))

(defmethod help ((i curses-inator))
  "Show help for the inator."
  (typecase (inator-keymap i)
    (keymap
     (display-text "Help"
		   (help-list (inator-keymap i) (_ (inator-doc-finder i _)))
		   :justify nil))
    (list
     (display-text "Help"
		   (loop :for k :in (inator-keymap i)
		      :append
		      (help-list k (_ (inator-doc-finder i _))))
		   :justify nil))))

(defmethod redraw ((i curses-inator))
  "Redraw the screen."
  (clear)
  (refresh)
  (update-display i))

(defmethod read-key-sequence ((i curses-inator))
  "Read a key sequence."
  (get-key-sequence (λ () (terminal-get-char *terminal*))
		    (inator-keymap i)))

(defmethod describe-key-briefly ((i curses-inator))
  (prompt i "Press a key to describe: ")
  (let* ((key-seq (read-key-sequence i))
	 (action (key-sequence-binding key-seq (inator-keymap i))))
    (if action
	(message i "~a is bound to ~a" (key-sequence-string key-seq) action)
	(message i "~a is not defined" (key-sequence-string key-seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defstruct progress-bar
  win
  width
  height)

(defun progress-bar (title &key message)
  "Display a progress bar in a pop up window."
  (with-curses
    (let* ((mid (truncate (/ *cols* 2)))
	   (width (min (- *cols* 6)
		       (+ 4 (max 50 (length message)))))
	   (height (min (+ 4 (+ 5))
			(- *lines* 4)))
	   (xpos   (truncate (- mid (/ width 2))))
	   (win    (newwin height width 3 xpos))
	   (bar    (make-progress-bar :win win :width width :height height)))
      (box win 0 0)
      (wcentered win width 0 title)
      (mvwaddstr win 1 2 message)
      (mvwaddstr win 2 2 (format nil ""))
      (refresh)
      (wrefresh w)
      result)))

(defun progress-bar-update (bar percent &optional status)
  (with-slots (win) bar
    (mvwaddstr win 1 2 message)
    (mvwaddstr win 2 2 (format nil ""))
    (refresh)
    (wrefresh win)))

(defun progress-bar-done (bar)
  (with-slots (win) bar
    (delwin w)
  (delwin w)
  (clear)
  (refresh))
|#

;; EOF
