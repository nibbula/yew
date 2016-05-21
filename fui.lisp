;;
;; fui.lisp - Fake UI
;;

(defpackage :fui
  (:documentation "Fake UI")
  (:use :cl :dlib :dlib-misc :stretchy :opsys :char-util :keymap :cffi
	:curses :inator)
  (:export
   #:*in-curses*
   #:*device*
   #:*has-color*
   #:*has-mouse*
   #:*interactive*
   #:start-curses
   #:end-curses
   #:with-curses
   #:with-device
   #:init-colors
   #:color-index
   #:color-attr
   #:+color-names+
   #:color-number
   #:with-fg
   #:with-bg
   #:with-color
   #:set-colors
   #:get-char
   #:interactively
   #:non-interactively
   #:pause
   #:add-char
   #:add-string
   #:display-text
   #:help-list
   #:fui-inator
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

(defvar *has-color* nil
  "True if the device has color.")

(defvar *has-mouse* nil
  "True if the device has a mouse or pointer of some sort.")

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
    (init-colors)

    ;; See if we can get mouse events
    (setf *has-mouse*
	  (= (mousemask curses:+ALL-MOUSE-EVENTS+ (cffi:null-pointer)) 0))))

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
;; (set-colors (color-index +COLOR-YELLOW+ +COLOR-BLUE+))

(defparameter *color-table* nil
  "Table of color pair numbers.")

(defun init-colors ()
  ;; Initialize all the color pairs
  (start-color)
  (let ((ncolors 8))
    (setf *color-table* (make-array (list ncolors ncolors)))
    (if (= (has-colors) 1)
    	(prog ((pair 0))
	   (setf *has-color* t)
	   (loop :for fg :from (- ncolors 1) :downto 0 :do
	      (loop :for bg :from 0 :below ncolors :do
		 (when (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		   (init-pair pair fg bg))
		 (setf (aref *color-table* fg bg) pair)
		 (incf pair))))
	(setf *has-color* nil)))
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

(defmacro with-fg ((color) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (fui:color-index ,color +color-black+)
		  (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (fui:color-index +color-white+ +color-black+)
		  (cffi:null-pointer))
       ,result)))

(defmacro with-bg ((color) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (fui:color-index +color-black+ ,color)
		  (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (fui:color-index +color-white+ +color-black+)
		  (cffi:null-pointer))
       ,result)))

(defmacro with-color ((fg bg) &body body)
  (with-unique-names (result)
    `(let (,result)
       (color-set (fui:color-index ,fg ,bg) (cffi:null-pointer))
       (setf ,result (progn ,@body))
       (color-set (fui:color-index +color-white+ +color-black+)
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

(defun get-char ()
  "Get a lisp character or function key from curses."
  (let ((cc (getch)))
    (cond
      ((> cc #xff)
       (function-key cc))
      ((and (integerp cc) (not (minusp cc)))
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
  "Print a message and wait for Enter to be pressed. Does nothing if not
*interactive*."
  (when *interactive*
    (apply #'format *standard-output* prompt args)
    (finish-output *standard-output*)
    (read-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

;; @@@ This should really be per-thread, but we should then probably make a
;; per-thread curses state object for use with `with-curses'. And it is also
;; currently a memory leak.
(defvar *wied-char* (cffi:foreign-alloc :int :count 2))
(defun add-char (c)
  (setf (cffi:mem-aref *wied-char* :int 0) (char-code c))
  (addnwstr *wied-char* 1))

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

(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

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
		       (get-char)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUI-inators

(defclass fui-inator (inator)
  ()
  (:documentation "A curses-inator."))

(defmethod initialize-instance
    :after ((o fui-inator) &rest initargs &key &allow-other-keys)
  "Initialize a fui-inator."
  (declare (ignore initargs)))

(defmethod start-inator ((i fui-inator))
  "Start a FUI-INATOR."
  (start-curses)
  (call-next-method))

(defmethod finish-inator ((i fui-inator))
  "Stop a FUI-INATOR."
  (end-curses)
  (call-next-method))

(defmethod update-display ((i fui-inator))
  "Update the view of a FUI-INATOR."
  (call-next-method)
  (refresh))

(defmethod await-event ((i fui-inator))
  "Get an event from a FUI-INATOR."
  (declare (ignore i))
  (get-char))

(defmethod message ((i fui-inator) format-string &rest args)
  "Display a short message."
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (addstr (apply #'format nil format-string args)))

(defun inator-doc-finder (i func)
  "Find documentation for an inator (subclass) method."
  (when (fboundp func)
    (let ((method
	   (find-method (symbol-function func) '() (list (class-of i)) nil)))
      (when method (documentation method t)))))

(defmethod help ((i fui-inator))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

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
