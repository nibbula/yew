;;
;; fui.lisp - Fake UI
;;

(defpackage :fui
  (:documentation "Fake UI")
  (:use :cl :dlib :dlib-misc :stretchy :opsys :char-util :curses :keymap
	:inator)
  (:export
   #:with-curses
   #:with-device
   #:init-colors
   #:color-index
   #:color-attr
   #:+color-names+
   #:color-number
   #:get-char
   #:interactively
   #:non-interactively
   #:*interactive*
   #:pause
   #:display-text
   #:help-list
   #:pick-list
   #:pick-file
   #:do-menu*
   #:do-menu
   #:menu-load
   ))
(in-package :fui)

;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1) (compilation-speed 2)))

(defvar *in-curses* nil
  "True when curses is active.")

(defvar *interactive* t
  "True when we can expect user interaction.")

(defvar *device* nil
  "A device to do run a curses program on.")

(defun start-curses ()
  (when (not *in-curses*)
    (setf *in-curses* t)
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
    (init-colors)))

(defun end-curses ()
  (when *in-curses*
    (endwin)
    (setf *in-curses* nil)))

(defmacro with-curses (&body body)
  "Do the stuff in the body with curses initialized. Clean up properly."
  (let ((thunk (gensym "thunk")))
    `(flet ((,thunk () ,@body))
       (if (not *in-curses*)
	   (progn
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
    `(let (,screen ,fd-in ,fd-out (*device* ,device))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
;;
;; This sets up a simple way to use all pairs of the standard eight colors.
;; Call INIT-COLORS first, then say, for example:
;; (setattr (color-attr +COLOR-YELLOW+ +COLOR-BLUE+))
;; or
;; (set-color (color-index +COLOR-YELLOW+ +COLOR-BLUE+))

(defparameter *color-table* nil
  "Table of color pair numbers.")

(defun init-colors ()
  ;; Initialize all the color pairs
  (start-color)
  (let ((ncolors 8))
    (setf *color-table* (make-array (list ncolors ncolors)))
    (if (= (has-colors) 1)
    	(prog ((pair 0))
	   (loop :for fg :from (- ncolors 1) :downto 0 :do
	      (loop :for bg :from 0 :below ncolors :do
		 (when (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		   (init-pair pair fg bg))
		 (setf (aref *color-table* fg bg) pair)
		 (incf pair))))))
  (bkgd (color-pair 0)))

(defun color-index (fg bg)
  "Return the color pair number for the foreground FG and background BG."
  (aref *color-table* fg bg))

(defun color-attr (fg bg)
  "Return the text attribute, e.g. for passing to setattr, for the
foreground FG and background BG."
  (color-pair (aref *color-table* fg bg)))

(defparameter +color-names+
  `((:black 	,+color-black+)
    (:red 	,+color-red+)
    (:green 	,+color-green+)
    (:yellow 	,+color-yellow+)
    (:blue 	,+color-blue+)
    (:magenta 	,+color-magenta+)
    (:cyan 	,+color-cyan+)
    (:white 	,+color-white+))
  "Associate symbols with color numbers.")

(defun color-number (color)
  "Return the curses color number given a symbol name."
  (cadr (assoc color +color-names+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input

(defun get-char ()
  "Get a lisp character or function key from curses."
  (let ((cc (getch)))
    (cond
      ((> cc #xff)
       (function-key cc))
      ((and (integerp cc) (plusp cc))
       (code-char cc))
      (t ;; Just return a negative
       cc))))

(defmacro non-interactively (&body body)
  "Evaluate body without pausing for PAUSE."
  `(let ((*interactive* nil))
     ,@body))

(defmacro interactively (&body body)
  "Evaluate body with pausing for PAUSE."
  `(let ((*interactive* t))
     ,@body))

(defun pause (&optional (prompt "[Press Enter]") &rest args)
  "Print a message and wait for Enter to be pressed. Does nothing if not *interactive*."
  (when *interactive*
    (apply #'format *standard-output* prompt args)
    (finish-output *standard-output*)
    (read-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUI-inators

(defclass fui-inator (inator)
  ()
  (:documentation "A curses-inator."))

(defmethod initialize-instance
    :after ((o fui-inator) &rest initargs &key &allow-other-keys)
  "Initialize a fui-inator."
  (declare (ignore initargs))
  (start-curses))

(defmethod update ((i fui-inator))
  "Update the view of I the FUI-INATOR."
  (call-next-method)
  (refresh))

(defmethod stop-inator ((i fui-inator))
  "Stop a FUI-INATOR."
  (end-curses)
  (call-next-method))

(defmethod event-pending-p ((i fui-inator))
  "Return true if there are any events pending for a FUI-INATOR."
  (declare (ignore i))
  nil)

(defmethod has-event-p ((i fui-inator))
  "Return true if there is an event ready for a FUI-INATOR."
  (declare (ignore i))
  )

(defmethod get-event ((i fui-inator))
  "Get an event from a FUI-INATOR."
  (declare (ignore i))
  ;; @@@ turn into an event
  (get-char))

(defclass fui-view (view)
  ()
  (:documentation "A curses view of a -inator."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

(defun display-text (title text-lines &key input-func)
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
      (loop :with i = 2 :for l :in text-lines
	 :do
	 (loop :for sub-line :in (split-sequence
				  #\newline (justify-text l :cols (- width 2)
							  :stream nil))
	    :do
	    (mvwaddstr w i 2 sub-line)
	    (incf i)))
      (refresh)
      (wrefresh w)
      (setf result (if input-func
		       (funcall input-func w)
		       (get-char)))
      (delwin w)
      (clear)
      (refresh)
      result)))

(defun help-list (keymap)
  "Return a list of key binding help lines, suitable for the HELP function."
  ;; Make a reverse hash of functions to keys, so we can put all the bindings
  ;; for a function on one line.
  (let ((rev-hash (make-hash-table)))
    (flet ((add-key (k v) (push k (gethash v rev-hash))))
      (map-keymap #'add-key keymap))
    (loop :for func :being :the :hash-keys :of rev-hash
       :collect
       (with-output-to-string (str)
	 (format str "~{~a~^, ~} - ~a"
		 (loop :for k :in (gethash func rev-hash)
		    :collect (nice-char k :caret t))
		 ;; Look up the documentation for the function.
		 (cond
		   ((or (functionp func)
			(and (symbolp func) (fboundp func)))
		    (let ((doc (documentation func 'function)))
		      (or doc (string-downcase func))))
		   ((keymap-p (string-downcase func)))
		   (t func)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List picker
;;;

;; TODO:
;; things from pager:
;; - best key compatability?

(defkeymap *pick-list-keymap*
  `((#\escape		. *pick-list-escape-keymap*)
    (,(ctrl #\G)	. pick-list-quit)
    (#\return		. pick-list-pick)
    (#\newline		. pick-list-pick)
    (#\space		. pick-list-toggle-item)
    (,(ctrl #\X)	. pick-list-toggle-region)
    (,(ctrl #\N)	. pick-list-next-line)
    (:down		. pick-list-next-line)
    (,(ctrl #\P)	. pick-list-previous-line)
    (:up		. pick-list-previous-line)
    (#\>		. pick-list-end-of-list)
    (,(meta-char #\>)	. pick-list-end-of-list)
    (:end		. pick-list-end-of-list)
    (#\<		. pick-list-beginning-of-list)
    (,(meta-char #\<)	. pick-list-beginning-of-list)
    (:home		. pick-list-beginning-of-list)
    (,(ctrl #\F)	. pick-list-next-page)
    (,(ctrl #\V)	. pick-list-next-page)
    (:npage		. pick-list-next-page)
    (,(ctrl #\B)	. pick-list-previous-page)
    (,(meta-char #\v)	. pick-list-previous-page)
    (:ppage		. pick-list-previous-page)
    (,(meta-char #\=)   . pick-list-binding-of-key)
    (#\?                . pick-list-help)
    ))

(defparameter *pick-list-escape-keymap*
  (build-escape-map *pick-list-keymap*))

(defstruct pick
  "State for a pick-list-inator."
  quit-flag
  multiple
  by-index
  typing-searches
  message
  items					; @@@ this should be renamed
  item-line
  result
  second-result				; @@@ this should be renamed
  mark
  cur-line
  max-y
  (top 0)
  ttop					; @@@ this should be renamed
  max-line
  page-size
  (input 0)				; aka 'c'
  error-message
  (search-str (make-stretchy-string 10)))

(defvar *pick* nil
  "The current pick list state.")

(defun pick-list-quit ()
  "Quit the list picker."
  (setf (pick-quit-flag *pick*) t))

(defun pick-list-pick ()
  "Pick the current item."
  (with-slots (multiple by-index result items item-line quit-flag input
	       second-result) *pick*
    (if multiple
	(when (not by-index)
	  (setf result (mapcar (_ (cdr (elt items _))) result)))
	(if by-index
	    (setf result item-line)
	    (setf result (cdr (elt items item-line)))))
    (setf quit-flag t
	  second-result (eq input #\newline)))) ; @@@ bogus check for newline

(defun pick-list-toggle-item ()
  "Toggle the item for multiple choice."
  (with-slots (multiple item-line result) *pick*
    (when multiple
      (if (position item-line result)
	  (setf result (delete item-line result))
	  (push item-line result)))))

(defun pick-list-toggle-region ()
  "Toggle the items in the region."
  (with-slots (multiple mark item-line result) *pick*
    (when (and multiple mark)
      (loop :for i :from (min mark item-line)
	 :to (max mark item-line)
	 :do
	 (if (position i result)
	     (setf result (delete i result))
	     (push i result))))))

(defun pick-list-next-line ()
  "Go to the next line. Scroll and wrap around if need be."
  (with-slots (cur-line max-y top item-line max-line) *pick*
    (when (>= (+ cur-line 1) max-y)
      (incf top))
    (if (< item-line (1- max-line))
	(incf item-line)
	(setf item-line 0 top 0))))

(defun pick-list-previous-line ()
  "Go to the previous line. Scroll and wrap around if need be."
  (with-slots (item-line top max-line items max-y ttop) *pick*
    (when (<= item-line top)
      (decf top))
    (if (> item-line 0)
	(decf item-line)
	(progn
	  (setf item-line (1- max-line))
	  (setf top (max 0 (- (length items)
			      (- max-y ttop))))))))

(defun pick-list-end-of-list ()
  "Go to the end of the list."
  (with-slots (item-line max-line top items max-y ttop) *pick*
    ;; (pause (format nil "~d ~d ~d ~d ~d"
    ;; 		    item-line max-line top max-y ttop))
    (setf item-line (1- max-line))
    (setf top (max 0 (- (length items) (- max-y ttop))))))

(defun pick-list-beginning-of-list ()
  "Go to the beginning of the list."
  (with-slots (item-line top) *pick*
    (setf item-line 0 top 0)))

(defun pick-list-next-page ()
  "Scroll to the next page."
  (with-slots (item-line max-line page-size cur-line top) *pick*
    (setf item-line (min (1- max-line) (+ item-line page-size))
	  cur-line  (+ top item-line)
	  top       item-line)))

(defun pick-list-previous-page ()
  "Scroll to the previous page."
  (with-slots (item-line page-size cur-line top) *pick*
    (setf item-line (max 0 (- item-line page-size))
	  cur-line  (+ top item-line)
	  top       item-line)))

(defun pick-error (message &rest args)
  "Set the list picker error message."
  (setf (pick-error-message *pick*) (apply #'format nil message args)))

(defun pick-list-tmp-message (message &rest args)
  "Display a temporary message."
  (apply #'pick-error message args)
  (move (- *lines* 1) 0)
  (clrtoeol)
  (addstr (pick-error-message *pick*)))

(defun pick-list-binding-of-key ()
  (pick-list-tmp-message "Press a key: ")
  (let* ((key (get-char))
	 (action (key-definition key *pick-list-keymap*)))
    (if action
	(pick-list-tmp-message
	 (format nil "~a is bound to ~a" (nice-char key) action))
	(pick-list-tmp-message
	 (format nil "~a is not defined" (nice-char key))))))

(defun pick-list-help ()
  (display-text "List picker keys" (help-list *pick-list-keymap*)))

(defun pick-list-display ()
  "Display the list picker."
  (with-slots (message multiple items item-line result cur-line
	       max-y top ttop error-message) *pick*
    (erase)
    (move 0 0)
    (when message (addstr (format nil message)))
    (setf ttop (getcury *stdscr*))
    ;; display the list
    (loop :with i = top :and y = ttop :and f = nil
       :do
       (setf f (car (elt items i)))
       (if (and multiple (position i result))
	   (addstr "X ")
	   (addstr "  "))
       (when (= i item-line)
	 (standout)
	 (setf cur-line (getcury *stdscr*)))
       (addstr f)
       (when (= i item-line)
	 (standend))
       (addch (char-code #\newline))
       (incf i)
       (incf y)
       :while (and (< y max-y) (< i (length items))))
    (when error-message (mvaddstr (- *lines* 1) 0 error-message))
    (move cur-line 0)))

(defun pick-typing-search ()
  "Try to search for typed input and return T if we did."
  (with-slots (typing-searches input search-str item-line max-line
	       items top page-size) *pick*
    (if (and typing-searches
	     (and (characterp input)
		  (graphic-char-p input) (not (position input "<> "))))
	;; Search for the seach string
	(progn
	  (stretchy-append search-str input)
	  (loop :for i :from item-line :below max-line
	     :if (search search-str (car (elt items i)) :test #'equalp)
	     :return (setf item-line i))
	  (when (> item-line (+ top page-size))
	    (setf top item-line))
	  t)
	;; Clear the string
	(progn
	  (stretchy-truncate search-str)
	  nil))))

(defun pick-perform-key (key &optional (keymap *pick-list-keymap*))
  (with-slots () *pick*
    (let ((command (key-definition key keymap)))
      (cond
	((not command)
	 (pick-error "Key ~a is not bound."
		     (nice-char key) keymap)
	 (return-from pick-perform-key))
	((consp command)
	 (if (fboundp (car command))
	     (apply (car command) (cdr command))
	     (pick-error "(~S) is not defined." (car command))))
	((symbolp command)
	 (cond
	   ((fboundp command)		; a function
	    (funcall command))
	   ((keymap-p (symbol-value command)) ; a keymap
	    (pick-perform-key (fui:get-char) (symbol-value command)))
	   (t				; anything else
	    (pick-error "Key binding ~S is not a function or a keymap."
			command))))
	;; a function object
	((functionp command)
	 (funcall command))
	(t				; anything else is an error
	 (error "Weird thing in keymap: ~s." command))))))

(defun pick-list (the-list &key message by-index sort-p default-value
			     selected-item (typing-searches t) multiple)
  "Have the user pick a value from THE-LIST and return it. Arguments:
  MESSAGE         - A string to be displayed before the list.
  BY-INDEX        - If true, return the index number of the item picked.
  SORT-P          - If true sort the list before displaying it.
  DEFAULT-VALUE   - Return if no item is selected.
  SELECTED-ITEM   - Item to have initially selected.
  TYPING-SEARCHES - True to have alphanumeric input search for the item.
  MULTIPLE        - True to allow multiple items to be selected."
  (with-curses
    (clear)
    (let* ((string-list (mapcar (_ (cons (princ-to-string _) _)) the-list))
	   (max-y (1- curses:*lines*))
	   (*pick* (make-pick
		    :message	     message
		    :by-index	     by-index
		    :multiple	     multiple
		    :typing-searches typing-searches 
		    :item-line	     (or selected-item 0)
		    :items	     (if (not (null sort-p))
				         ;; Where's the unreachable code??
				         (sort string-list #'string-lessp
					       :key #'car)
				         string-list)
		    :max-line        (length string-list)
		    :max-y           max-y
		    :page-size       (- max-y 2)
		    :result	     default-value)))
      (with-slots (quit-flag multiple by-index items item-line result
		   second-result mark cur-line max-y top ttop max-line
		   page-size input error-message search-str) *pick*
	(loop :while (not quit-flag)
	   :do
	   (pick-list-display)
	   (setf error-message nil
		 input (get-char))
	   (when (not (pick-typing-search))
	     (if (or (eql input (ctrl #\@)) (eql input 0))
		 (setf mark item-line
		       error-message "Set mark")
		 (pick-perform-key input))))
	(values result second-result)))))

;;; This is the version before keymapification.
#|
(defun old-pick-list (the-list &key message by-index sort-p default-value
		  selected-item (typing-searches t) multiple)
  "Have the user pick a value from THE-LIST and return it. Arguments:
  MESSAGE         - A string to be displayed before the list.
  BY-INDEX        - If true, return the index number of the item picked.
  SORT-P          - If true sort the list before displaying it.
  DEFAULT-VALUE   - Return if no item is selected.
  SELECTED-ITEM   - Item to have initially selected.
  TYPING-SEARCHES - True to have alphanumeric input search for the item.
  MULTIPLE        - True to allow multiple items to be selected."
  (with-curses
    (clear)
    (let* ((c           0)
	   (file-line   (or selected-item 0))
	   (string-list (mapcar #'princ-to-string the-list))
	   (files       (if (not (null sort-p))
			    ;; Where's the unreachable code??
			    (sort string-list #'string-lessp)
			    string-list))
	   (max-line    (length files))
	   (max-y       (1- curses:*lines*))
	   (page-size   (- max-y 2))
	   (result      default-value)
	   second-result
	   cur-line
	   quit-flag
	   (top         0)
	   ttop
	   mark
	   msg				; @@@ debug
	   (search-str   (make-stretchy-string 10)))
      (loop :while (not quit-flag)
	 :do
	 (erase)
	 (move 0 0)
	 (when message (addstr (format nil message)))
	 (setf ttop (getcury *stdscr*))
	 ;; display the list
	 (loop :with i = top :and y = ttop :and f = nil
	    :do
	    (setf f (elt files i))
	    (if (and multiple (position i result))
		(addstr "X ")
		(addstr "  "))
	    (when (= i file-line)
	      (standout)
	      (setf cur-line (getcury *stdscr*)))
	    (addstr f)
	    (when (= i file-line)
	      (standend))
	    (addch (char-code #\newline))
	    (incf i)
	    (incf y)
	    :while (and (< y max-y) (< i (length files))))
;;;	 (mvaddstr 20 0 (format nil "file-line = ~s top = ~s max-y = ~s ttop = ~s" file-line top max-y ttop))
;;;	 (mvaddstr 50 0 (format nil "~a ~s | ~a" file-line result msg))
	 (move cur-line 0)
	 (setf c (get-char))
	 (if (and typing-searches
		  (and (characterp c)
		       (graphic-char-p c) (not (position c "<> "))))
	     (progn
	       (stretchy-append search-str c)
	       (loop :for i :from file-line :below max-line
		  :if (search search-str (elt files i) :test #'equalp)
		  :return (setf file-line i))
	       (when (> file-line (+ top page-size))
		 (setf top file-line)))
	     (progn
	       (stretchy-truncate search-str)
	       (cond
	       	 ((or (eql c (ctrl #\@))
	       	      (eql c 0))
	       	  (setf mark file-line
			msg "Set mark"))
	       	 (t
		  (case c
		    ((#\escape #\^G) (setf quit-flag t))
		    ((#\return #\newline)
		     (if multiple
			 (when (not by-index)
			   (setf result (mapcar (_ (elt files _)) result)))
			 (if by-index
			     (setf result file-line)
			     (setf result (elt files file-line))))
		     (setf quit-flag t
			   second-result (eq c #\newline)))
		    (#\space
		     (when multiple
		       (if (position file-line result)
			   (setf result (delete file-line result))
			   (push file-line result))))
		    (#\^X ;; toggle region
		     (when (and multiple mark)
		       (loop :for i :from (min mark file-line)
			  :to (max mark file-line)
			  :do
			  (if (position i result)
			      (setf result (delete i result))
			      (push i result)))))
		    ((#\^N :down)
		     (when (>= (+ cur-line 1) max-y)
		       (incf top))
		     (if (< file-line (1- max-line))
			 (incf file-line)
			 (setf file-line 0 top 0)))
		    ((#\^P :up)
		     (when (<= file-line top)
		       (decf top))
		     (if (> file-line 0)
			 (decf file-line)
			 (progn
			   (setf file-line (1- max-line))
			   (setf top (max 0 (- (length files)
					       (- max-y ttop)))))))
		    ((#\> :end)
		     ;; (pause (format nil "~d ~d ~d ~d ~d"
		     ;; 		    file-line max-line top max-y ttop))
		     (setf file-line (1- max-line))
		     (setf top (max 0 (- (length files) (- max-y ttop)))))
		    ((#\< :home)
		     (setf file-line 0 top 0))
		    ((#\^F :npage)
		     (setf file-line (min (1- max-line) (+ file-line page-size))
			   cur-line  (+ top file-line)
			   top       file-line))
		    ((#\^B :ppage #\b #\B)
		     (setf file-line (max 0 (- file-line page-size))
			   cur-line  (+ top file-line)
			   top       file-line))))))))
      (values result second-result))))
|#

;; Test scrolling with:
;; (fui:pick-list (loop for i from 1 to 60 collect (format nil "~@r~8t~r" i i)))

;; It's pretty lame when implementations differ so much on the same OS.
;; I would say that current implementations don't really reach the goal of
;; system independant file handling. Too many unspecified gray areas
;; make it such that I seem to get errors all the time.
(defun fake-dir (spec)
  #+(or ccl ecl)
  (let* ((dir (pathname-directory (pathname spec)))
	 (str (cond
		((stringp spec)
		 spec)
		(dir
		 (namestring (make-pathname :directory dir))))))
    (nos:read-directory :dir (or str ".") :append-type t))
  #+clisp (nconc (directory (make-pathname
			     :directory `(,@(or (pathname-directory spec)
						'(:relative)) :wild)))
		 (directory (make-pathname
			     :directory (pathname-directory spec)
			     :name :wild :type :wild)))
;  #+ccl (directory spec :directories t :all t :follow-links t)
  #-(or ecl clisp ccl) (directory spec)
  )

;; Pure common lisp pick-file, which will work on implementations with decent
;; filename and directory functions. Too bad it doesn't work very well on
;; different implementations.
#|
(defun old-pick-file (&key message directory (name :wild) (type :wild)
		  (allow-browse t) (pick-directories))
  "Have the user choose a file."
  (declare (ignore pick-directories))	;@@@
  (let* ((dir (make-pathname :directory directory))
	 files file-list filename)
    (flet ((generate-list ()
	     (setf files
		   (loop :for file :in (fake-dir (merge-pathnames dir
						  (make-pathname :name name
								 :type type)))
		      :collect
		      (let* ((ff (namestring
				  (make-pathname
				   :name (pathname-name file)
				   :type (pathname-type file))))
			     (last-dir (car (last (pathname-directory file))))
			     (dd (concatenate
				  'string (or (and (stringp last-dir) last-dir)
					      "")
				  "/")))
			(format nil "~a" (or (and (> (length ff) 0) ff)
					     (and (> (length dd) 0) dd)))))
		   file-list (if allow-browse
				 (append '(" [Up..]") files)
				 files))))
      (generate-list)
      (if allow-browse
	  (loop :with done = nil
	     :while (not done)
	     :do
	     (setf message (format nil "~a~%~%" dir))
	     (setf filename (pick-list file-list :sort-p t :message message))
	     (cond
	       ;; picked up level
	       ((and filename (equal " [Up..]" filename))
		(let ((d (pathname-directory (truename dir))))
		  (when (> (length d) 1)
		    (setf dir (make-pathname
			       :directory (subseq d 0 (- (length d) 1))))
		    (generate-list))))
	       ;; picked a directory
	       ((and filename (not (pathname-name (pathname filename))))
;		(setf dir (make-pathname :directory filename))
		(setf dir (merge-pathnames (pathname filename) dir))
		(generate-list))
	       ;; other files
	       (t
		(setf done t))))
	  (setf filename (pick-list file-list :sort-p t :message message)))
      (when filename
	(merge-pathnames (pathname filename) dir)))))
|#

;; @@@ This PF-DIR-ENTRY stuff should probably be added as a feature to
;; read-directory. Maybe a :printable option?

(defun pf-print-dir-entry (obj stream depth)
  "Print OBJ which should be a PF-DIR-ENTRY to stream."
  (declare (ignore depth))
  (princ (dir-entry-name obj) stream)
  (let ((type (dir-entry-type obj)))
    (cond
      ((eq type :pipe)	 (write-char #\| stream))
      ((eq type :dir)	 (write-char #\/ stream))
      ((eq type :link)	 (write-char #\@ stream))
      ((eq type :socket) (write-char #\= stream)))))

(defstruct (pf-dir-entry (:include nos:dir-entry)
			 (:print-function pf-print-dir-entry))
  "A dir entry that prints nicely.")

(defun pick-file-list-generator (dir)
  (loop :for f :in (nos:read-directory :dir dir :full t)
     :collect (make-pf-dir-entry
	       :name (dir-entry-name f) :type (dir-entry-type f))))

(defun pick-file (&key message (directory ".") (allow-browse t) show-hidden
		    (pick-directories))
  "Have the user choose a file."
  ;;@@@ to allow choosing directories instead of going to them
  (let* ((dir directory)
	 files file-list filename msg did-dir)
    (flet ((generate-list ()
	     (setf files
		   (loop :for file :in (pick-file-list-generator dir)
		      :if (or (char/= #\. (char (dir-entry-name file) 0))
			      show-hidden)
		      :collect file)
		   file-list (if allow-browse
				 (append '(" [Up..]") files)
				 files)))
	   (cat (a b) (concatenate 'string a b)))
      (generate-list)
      (if allow-browse
	  (loop :with done = nil
	     :while (not done)
	     :do
	     (setf msg (format nil "~@[~a~%~]~a~%" message dir))
	     (setf (values filename did-dir)
		   (pick-list file-list :sort-p t :message msg))
	     (cond
	       ;; picked up level
	       ((and filename (equal " [Up..]" filename))
		(setf dir (dirname (cat (abspath (cat dir "/..")) "/")))
		(generate-list))
	       ;; picked a directory
	       ((and filename
		     (pf-dir-entry-p filename)
		     (or (eq (dir-entry-type filename) :dir)
			 (eq (dir-entry-type filename) :link))
		     (probe-directory
		      (path-append dir (dir-entry-name filename)))
		     (and (not pick-directories) (not did-dir)))
		(setf dir (path-append dir (dir-entry-name filename)))
		(generate-list))
	       ;; other files
	       (t
		(setf done t))))
	  ;; Just pick from the current directory
	  (setf filename (pick-list file-list :sort-p t :message message)))
      (when filename
	(values (abspath (path-append dir (dir-entry-name filename)))
		dir)))))
 
;; Has problems because the eval'd code can't reference local variables.
;; More evidence that use of eval is usually a problem.
(defmacro old-do-menu (menu &key message selected-item)
  "Perform an action from a menu. Menu is an alist of (item . action)."
  ;;; @@@ improve to one loop
  (let ((items (loop :for m :in menu :collect (car m)))
	(funcs (loop :for m :in menu :collect (cdr m))))
    `(block menu
      (let* ((n (pick-list ',items :by-index t
			   :message ,message
			   :selected-item ,selected-item))
	     (code (if n (elt ',funcs n))))
	(if code
	    (eval code)
	    :quit)))))

(defun do-menu* (menu &key message selected-item)
  "Perform an action from a menu. Menu is an alist of (item . action)."
  (let ((items (loop :for m :in menu :collect (car m)))
	(funcs (loop :for m :in menu :collect (cdr m))))
    (let* ((n (pick-list items :by-index t
			       :message message
			       :selected-item selected-item))
	   (code (if n (elt funcs n))))
      (if code
	  (eval code)
	  :quit))))

(defmacro do-menu (menu &key message selected-item)
  "Perform an action from a menu. MENU is an alist of (ITEM . ACTION), where
ITEM is something to print, as with princ, and ACTION is a function to call.
Arguments:
MESSAGE is some text to display before the menu.
SELECTED-ITEM is the item that is initially selected.
This is a macro so you can use lexically scoped things in the menu."
  ;;; @@@ improve to one loop
  (cond
    ((symbolp menu)
     `(do-menu* ,menu :message ,message :selected-item ,selected-item))
    ((listp menu)
     (let ((items (loop :for (i . nil) :in menu :collect i))
	   (funcs (loop :for (nil . f) :in menu :collect f))
	   (n-sym (gensym)))
       `(block menu			; ! anaphoric or un-hygenic ?
	  (let* ((,n-sym (pick-list ',items :by-index t
				    :message ,message
				    :selected-item ,selected-item)))
	    (if ,n-sym
		(case ,n-sym
		  ,@(loop :for i :from 0 :below (length funcs)
;;; Why did I do this?
;;;		 :collect (list i (let ((f (elt funcs i)))
;;;				    (if (listp f) (car f))))))
		       :collect (list i (elt funcs i))))
		:quit)))))
    (t
     (error "MENU must be a symbol or a list."))))

(defun show-result (expr)
  (unwind-protect
     (progn
       (end-curses)
;       (format t "showing ~s~%" expr)
       (let ((vals (multiple-value-list (eval expr))))
	 (loop :for v :in vals :do (print v))
	 (pause)))
    (start-curses))
  (values))

(defun menu-load (&optional filename)
  (let* ((fn (or filename (pick-file)))
	 lines)
    (when fn
      (setf lines
	    (with-open-file (ss fn)
	      (loop :with line = nil
		 :while (setf line (ignore-errors (read ss nil)))
		 :collect (cons (format nil "~s" line)
				`(show-result ',line))))))
      (setf lines (append lines (list (cons "[Quit]" :quit))))
      (loop :while (not (eql :quit (do-menu* lines))))))

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
