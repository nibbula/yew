;;
;; fui.lisp - Fake UI
;;

;; $Revision: 1.4 $

(defpackage :fui
  (:documentation "Fake UI")
  (:use :cl :curses :stretchy :dlib :dlib-misc :opsys :char-util)
  (:export
   #:with-curses
   #:init-colors
   #:color-index
   #:color-attr
   #:get-char
   #:interactively
   #:non-interactively
   #:*interactive*
   #:pause
   #:pick-list
   #:pick-file
   #:dirname
   #:basename
   #:abspath
   #:do-menu
   #:menu-load
   #:display-text
   #:print-tree
   #:package-dependency-tree
   #:subdirs
   #:make-tree
   #:browse-tree
   #:browse-packages
   ))
(in-package :fui)

;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 2)))

(defvar *in-curses* nil
  "True when curses is active.")

(defvar *interactive* t
  "True when we can expect user interaction.")

(defun start-curses ()
  (when (not *in-curses*)
    (setf *in-curses* t)
    (initscr)
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
    (curs-set 1)))

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
	   (loop :for fg :from (- ncolors 1) :downto 0 :by 1 :do
	      (loop :for bg :from 0 :below ncolors :by 1 :do
		 (when (> pair 0) ;; Pair 0 defaults to WHITE on BLACK
		   (init-pair pair fg bg)
		   (setf (aref *color-table* fg bg) pair))
		 (setq pair (+ pair 1)))))))
  (bkgd (color-pair 0)))

(defun color-index (fg bg)
  "Return the color pair number for the foreground FG and background BG."
  (aref *color-table* fg bg))

(defun color-attr (fg bg)
  "Return the text attribute, e.g. for passing to setattr, for the foreground FG and background BG."
  (color-pair (aref *color-table* fg bg)))

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

;; TODO:
;; goto by typing
;; things from pager:
;; - search
;; - best key compatability?
;; - keyampification?

(defun pick-list (the-list &key message by-index sort-p default-value
		  selected-item typing-searches)
  "Have the user pick a value from THE-LIST. Display the MESSAGE if it's given."
  (with-curses
    (clear)
    (let* ((c           0)
	   (file-line   (or selected-item 0))
	   (string-list (mapcar #'(lambda (x) (format nil "~a" x)) the-list))
	   (files       (if sort-p
			    (sort string-list #'string-lessp) string-list))
	   (max-line    (length files))
	   (max-y       (1- curses:*lines*))
	   (page-size   (- max-y 2))
	   (result      default-value)
	   cur-line
	   quit-flag
	   (top         0)
	   ttop
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
	    (addstr "  ")
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
;	 (mvaddstr 20 0 (format nil "file-line = ~s top = ~s max-y = ~s ttop = ~s" file-line top max-y ttop))
	 (move cur-line 0)
	 (setf c (get-char))
	 (if (and typing-searches (graphic-char-p c))
	     (progn
	       (stretchy-append search-str c)
	       ;; @@@
	       )
	     (case c
	       ((#\escape #\^G) (setf quit-flag t))
	       ((#\return #\newline)
		(if by-index
		    (setf result file-line)
		    (setf result (elt files file-line)))
		(setf quit-flag t))
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
		      (setf top (max 0 (- (length files) (- max-y ttop)))))))
	       ((#\> :end)
		;; 	    (pause (format nil "~d ~d ~d ~d ~d"
		;; 			   file-line max-line top max-y ttop))
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
		      top       file-line))
	       )))
      result
      )))

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

;; @@@ This should probably go somewhere else. Opsys? Somewhere else?
(defun abspath (path)
  "Turn the PATH into an absolute path."
  ;; Make sure path is a string.
  (setf path (etypecase path
	       (null (return-from abspath nil))
	       (string path)
	       (pathname (namestring path))))
    (let* ((p (if (char= #\/ (char path 0))
		 path			; already absolute
		 (concatenate 'string (current-directory) "/" path)))
	   (pp (split-sequence #\/ p :omit-empty t)))
      (macrolet
	  ((get-rid-of (str snip)
	     "Get rid of occurances of STR by snipping back to SNIP, which
              is a numerical expression in terms of the current position POS."
	     `(loop :with start = 0 :and pos
		 :while (setq pos (position ,str pp
					    :start start :test #'equal))
		 :do (setq pp (concatenate 'list
					   (subseq pp 0 (max 0 ,snip))
					   (subseq pp (1+ pos)))))))
	;; Get rid of ".."
	(dbug "starting with ~s~%" pp)
	(get-rid-of "." pos)
	(dbug "after . ~s~%" pp)
	(get-rid-of ".." (1- pos))
	(dbug "after .. ~s~%" pp)
	)
      (if (zerop (length pp))
	  "/"
	  (apply #'concatenate 'string
		 (loop :for e :in pp :collect "/" :collect e)))))

(defun clip-path (path side)
  "Return the directory portion of a path."
  ;; Go backwards from the end until we hit a separator.
  (let ((i (1- (length path))))
    (loop :while (and (>= i 0) (char/= #\/ (char path i)))
       :do (decf i))
    (if (eq side :dir)
	(if (< i 0)
	    (subseq path 0 0)
	    (subseq path 0 i))
	(if (< i 0)
	    path
	    (subseq path (1+ i))))))

(defun dirname (path)
  "Return the directory portion of a path."
  (clip-path path :dir))

(defun basename (path)
  "Return the last portion of a path."
 (clip-path path :file))

(defun pick-file (&key message (directory ".") (allow-browse t) show-hidden
		    (pick-directories))
  "Have the user choose a file."
  ;;@@@ to allow choosing directories instead of going to them
  (declare (ignore pick-directories))
  (let* ((dir directory)
	 files file-list filename)
    (flet ((generate-list ()
	     (setf files
		   (loop :for file :in (nos:read-directory
					:dir dir :append-type t)
		      :if (or (char/= #\. (char file 0)) show-hidden)
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
	     (setf message (format nil "~a~%~%" dir))
	     (setf filename (pick-list file-list :sort-p t :message message))
	     (cond
	       ;; picked up level
	       ((and filename (equal " [Up..]" filename))
		(setf dir (dirname (cat (abspath (cat dir "/..")) "/")))
		(generate-list))
	       ;; picked a directory
	       ((and filename
		     (char= #\/ (char filename (1- (length filename)))))
		(setf dir (concatenate 'string dir "/" filename))
		(generate-list))
	       ;; other files
	       (t
		(setf done t))))
	  ;; Just pick from the current directory
	  (setf filename (pick-list file-list :sort-p t :message message)))
      (when filename
	(abspath (concatenate 'string dir "/" filename))))))
 
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

(defun dork-menu (menu &key message selected-item)
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
  "Perform an action from a menu. Menu is an alist of (item . action)."
  ;;; @@@ improve to one loop
  (let ((items (loop :for (i . nil) :in menu :collect i))
	(funcs (loop :for (nil . f) :in menu :collect f))
	(n-sym (gensym)))
    `(block menu
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

(defun show-result (expr)
  (unwind-protect
       (progn
	 (end-curses)
	 (format t "showing ~s~%" expr)
	 (let ((vals (multiple-value-list (eval expr))))
	   (loop :for v :in vals :do (print v))
	   (pause)))
    (start-curses)))

(defun menu-load (&optional filename)
  (let* ((fn (or filename (pick-file)))
	 lines)
    (when fn
      (setf lines
	    (with-open-file (ss fn)
	      (loop :with line = nil
		 :while (setf line (ignore-errors (read ss nil)))
		 :collect (cons (format nil "~s" line) `(show-result ',line))))))
      (setf lines (append lines (list (cons "[Quit]" :quit))))
;      (print lines)
;      (pause)
      (loop :while (not (eql :quit (dork-menu lines))))))

(defun wcentered (w width row str)
  "Put a centered string STR in window W of width WIDTH at row ROW."
  (mvwaddstr w row (round (- (/ width 2) (/ (length str) 2))) str))

(defun display-text (title text-lines)
  "Display text in a pop up window."
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
	     (w 	 (newwin height width 3 xpos)))
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
	(wrefresh w)
	(get-char)
	(delwin w))
    (clear)
    (refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees

;; These are just for testing:
(defparameter *opsys-tree*
  '(opsys
    (common-lisp
     (clos))
    (cffi
     (common-lisp
      (clos))
     (cffi-sys
      (common-lisp
       (clos))
      (alexandria))
     (babel-encodings
      (common-lisp
       (clos))
      (alexandria
       (common-lisp
	(clos)))))))

(defparameter *opsys-simple-tree*
  '(opsys
    (cffi
     (cffi-sys
      (alexandria))
     (babel-encodings
      (alexandria)))))

(defgeneric %print-tree (tree &key max-depth indent level key)
  (:documentation "The inner part of print-tree which recurses and does all the work. This is probably the only part which needs to be overridden for other tree types."))
(defmethod %print-tree ((tree list) &key max-depth (indent 2) (level 0) key)
  (when (or (not max-depth) (< level max-depth))
    (loop :for n :in tree :do
       (if (atom n)
	   (format t "~v,,,va~a~%" (* level indent) #\space ""
		   (if key (funcall key n) n))
	   (%print-tree n :level (1+ level)
			:indent indent :max-depth max-depth :key key)))))

(defun print-tree (tree &key max-depth (indent 2) key)
  "Print a tree up to a depth of MAX-DEPTH. Indent by INDENT spaces for every level. INDENT defaults to 2. Apply KEY function to each element before printing."
  (%print-tree tree :max-depth max-depth :indent indent :key key))

#| This is pretty much obsoleced by browe-packages stuff below.

(defvar *pdt-memo* nil
  "Memoization for the package dependency tree. An alist of (package . dep-tree) which is bound dynamically for each call.")

;; I suppose I could just use make-tree, but this has special stuff to ignore
;; the common-lisp package.
(defun %package-dependency-tree (package &key max-depth (depth 0) flat)
  "Generate a tree of dependencies for PACKAGE, up to a depth of MAX-DEPTH. DEPTH is the current depth in the tree, and FLAT is a flat list of packages encountered to prevent following infinite cycles."
  (declare
   (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
   (type (or null fixnum) max-depth)
   (type fixnum depth)
   (type package package))
  (when (and max-depth (> depth max-depth))
    (return-from %package-dependency-tree nil))
  (when (assoc package *pdt-memo*)
    (return-from %package-dependency-tree (cdr (assoc package *pdt-memo*))))
  (let ((tree '())
	(deps (package-use-list package)))
    (push (package-name package) tree)
    (pushnew (find-package package) flat)
    (when deps
      (loop :for p :of-type package :in deps :do
	 (when (and (not (position p flat)) ; don't follow circular deps
		    (not (eq (find-package p) (find-package :common-lisp))))
	   (let ((sub-tree (%package-dependency-tree
	   		    p :depth (the fixnum (+ depth 1)) :flat flat
	   		    :max-depth max-depth)))
	     (when sub-tree
;;;		   (if (= 1 (length sub-tree))
;;;		   (push (first sub-tree) tree)
	       (push sub-tree tree))))))
    (let ((result (nreverse tree)))
      (prog1 result
	(when (not (assoc package *pdt-memo*))
	  (acons package result *pdt-memo*))))))

(defun package-dependency-tree (package &key max-depth)
  "Generate a tree of dependencies for PACKAGE, up to a depth of MAX-DEPTH."
  (let ((*pdt-memo* '()))
    (%package-dependency-tree package :max-depth max-depth)))

|#

(defun %make-tree (thing func &key max-depth (test #'equal)
				(depth 0) (flat '()))
  "Generate a tree for THING, where FUNC is a function (FUNC THING) which returns a list of the brances of THING. Makes a tree of up to a depth of MAX-DEPTH. TEST is used to compare THINGS. TEST defaults to EQUAL. DEPTH is the current depth in the tree, and FLAT is a flat list of things encountered to prevent following infinite cycles."
  (declare (ignore test))
  (when (and max-depth (> depth max-depth))
    (return-from %make-tree nil))
  (let ((tree '())
	(branches (funcall func thing)))
    (push thing tree)
    (pushnew thing flat)
    (when branches
      (loop :for b :in branches :do
	 (when (not (find b flat)) ; don't follow cycles
	   (let ((sub-tree (%make-tree
			    b func :depth (1+ depth) :flat flat
			    :max-depth max-depth)))
	     (when sub-tree
	       (push sub-tree tree))))))
    (reverse tree)))

(defun make-tree (thing func &key max-depth (test #'equal))
  "Generate a tree for THING, where FUNC is a function (FUNC THING) which returns a list of the brances of THING. Makes a tree of up to a depth of MAX-DEPTH. TEST is used to compare THINGS. TEST defaults to EQUAL."
  (%make-tree thing func :max-depth max-depth :test test))

;; This is handy for a tree generation function.
(defun subdirs (dir)
  "Generating function for filesystem tree starting at DIR."
    (loop :for d :in (ignore-errors
		       (read-directory :dir dir :full t :omit-hidden t))
       :if (eql :dir (dir-entry-type d))
       :collect (concatenate 'string dir "/" (dir-entry-name d))))

;; Static node

(defclass node ()
  ((object   :initarg :object   :accessor node-object)
   (branches :initarg :branches :accessor node-branches)
   (open     :initarg :open     :accessor node-open))
  (:default-initargs
   :object nil
   :branches nil
   :open nil
   )
  (:documentation "A node in a browseable tree."))

(defun make-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'node args))

(defgeneric node-has-branches (node)
  (:documentation "Return true if the node supposedly has branches."))

(defmethod node-has-branches ((node node))
  "Return true if the node has branches."
  ;; Static nodes just check the branches slot.
  (node-branches node))

;; Dynamic node

(defclass dynamic-node (node)
  ((func
    :initarg :func :accessor node-func :documentation
    "A function that given an OBJECT generates a list of branches."))
  (:default-initargs
   :func nil
   )
  (:documentation "A dynamic node in a browseable tree. A dynamic node has a function that generates the branches."))

(defun make-dynamic-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'dynamic-node args))

(defmethod node-branches ((node dynamic-node))
;  (dbug "node-branches(dynamic-node)")
  (if (node-func node)
      (mapcar #'(lambda (x)
		  (make-dynamic-node
		   :object x :open t :func (node-func node)))
	      (funcall (node-func node) (node-object node)))
      nil))

(defmethod node-has-branches ((node dynamic-node))
  "Return true if the node has branches."
  ;; Dynamic nodes are optimistic and return true if there's a
  ;; generating function.
  (not (null (node-func node))))

(defun make-dynamic-tree (thing func)
  "Return a dynamic tree for THING, where FUNC is a function (FUNC THING) which returns a list of the branches of THING."
  (make-dynamic-node :object thing :func func :open t))

;; Dynamic cached node

(defclass cached-dynamic-node (dynamic-node)
  ((cached
    :initarg :cached :accessor node-cached
    :documentation "True if the results of FUNC were already retrieved."))
  (:default-initargs
   :cached nil
   )
  (:documentation "A dynamic node in a browseable tree. A dynamic node has a function that generates the branches. It caches the results of the branch generating function, so it will be called only the first time."))

(defun make-cached-dynamic-node (&rest args &key &allow-other-keys)
  (apply #'make-instance 'cached-dynamic-node args))

(defmethod node-branches ((node cached-dynamic-node))
;  (dbug "node-branches(cached-dynamic-node)")
  (if (node-cached node)
      (slot-value node 'branches)
      (if (node-func node)
	  (setf (node-cached node) t
		(slot-value node 'branches)
		(mapcar #'(lambda (x) (make-cached-dynamic-node
				       :object x :open nil
				       :func (node-func node)
				       :cached nil))
			(funcall (node-func node) (node-object node))))
	  nil)))

(defmethod node-has-branches ((node cached-dynamic-node))
  "Return true if the node has branches."
  ;; Cached dynamic nodes are optimistic and return true if there's a
  ;; generating function and it's not cached, but if it's cached return
  ;; based on if we have any cached branches.
  (if (node-cached node)
      (not (null (node-branches node)))
      (not (null (node-func node)))))

(defun make-cached-dynamic-tree (thing func)
  "Return a cached dynmaic tree for THING, where FUNC is a function (FUNC THING) which returns a list of the branches of THING."
  (make-cached-dynamic-node :object thing :func func :open nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-open-p (node)
  "If it has no branches it's both open and closed."
  (or (not (node-has-branches node)) (node-open node)))

(defun all-subnodes-open-p (node)
  (if (not (node-open-p node))
      nil
      (loop :for n :in (node-branches node) :do
	 (when (or (not (node-open-p n))
		   (not (all-subnodes-open-p n)))
	   (return-from all-subnodes-open-p nil))))
  t)

(defvar *map-tree-count* nil
  "How many nodes we've processed with map-tree.")

(defvar *map-tree-max-count* nil
  "When to stop processing with map-tree.")

(defun %map-tree (func node)
  (incf *map-tree-count*)
  (when (or (not *map-tree-max-count*)
	    (< *map-tree-count* *map-tree-max-count*))
    (funcall func node)
#|    (when dlib:*dbug*
      (mvaddstr 5 50 (format nil "~a ~a" *map-tree-count* *map-tree-max-count*))
      (refresh)) |#
    (mapc #'(lambda (x) (%map-tree func x))
	  (node-branches node))))

(defun map-tree (func node &key (max-count 1000))
  (let ((*map-tree-count* 0)
	(*map-tree-max-count* max-count))
    (%map-tree func node)))

(defun close-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) nil)) node))

(defun open-all-subnodes (node)
  (map-tree #'(lambda (x) (setf (node-open x) t)) node))

(defun convert-tree (tree &key (level 0))
  "Convert a list based TREE to a node based tree. "
  (if (atom tree)
      (make-node :object tree
		 :open nil
		 :branches nil)
      (let ((node (make-node :object (car tree)
			     :open (zerop level))))
	(setf (node-branches node)
	      (let ((list-of-rest-of-obj
		     (if (listp (cdr tree)) (cdr tree) (list (cdr tree)))))
		(loop :for n :in list-of-rest-of-obj
		   :collect (convert-tree n :level (1+ level)))))
	node)))

(defmethod %print-tree ((tree node) &key max-depth (indent 2) (level 0) key)
  (format t "~v,,,va~a ~:[Closed~;Open~]~%" (* level indent) #\space ""
	  (if key (funcall key (node-object tree)) (node-object tree))
	  (node-open tree))
  (when (or (not max-depth) (< level max-depth))
    (loop :for n :in (node-branches tree) :do
       (%print-tree n :level (1+ level)
		    :indent indent :max-depth max-depth :key key))))

;; These are just dynamic. They have no meaning outside of the dynamic scope
;; of browse-tree.
(defparameter *line-index* nil
  "Array of nodes at a given line.")
(defparameter *line-count* 0
  "Count of lines diplayed and valid lines in *line-index*")

(defun normal-format-node (node &key level indent key)
  (format nil "~v,,,va~c ~a" (* level indent) #\space ""
	  (if (node-branches node)
	      (if (node-open node) #\- #\+)
	      #\space)
	  (if key (funcall key (node-object node))
	      (node-object node))))

(defparameter *node-formatter* #'normal-format-node)

(defun display-tree (tree &key max-depth (indent 2) key (top 0) (left 0)
			    (level 0))
  "Display the tree. Also create the *line-index*."
  (when (and (>= *line-count* top)
	     (< *line-count* (+ top (- *lines* 2))))
    (let ((str (funcall *node-formatter* tree
			:level level :indent indent :key key)))
      ;; horizontal scrolling
      (when (> left 0)
	(setf str (subseq str (min left (length str)))))
      ;; clipping to right edge
      (when (> (length str) (1- *cols*))
	(setf str (subseq str 0 (- *cols* 1))))
      (addstr str)
      (addch (char-code #\newline))))
  ;; Keep an index of nodes by screen line.
  (stretchy-set *line-index* *line-count* tree)
  (incf *line-count*)
  ;; Recurse and display subtrees
  (if (and (node-open tree) (node-branches tree))
      (loop :for n :in (node-branches tree) :do
	 (display-tree n :level (1+ level)
		       :indent indent :max-depth max-depth :key key
		       :top top :left left))))

;; @@@ another candidate for keymappificationalismization
(defun browse-tree (tree)
  "Look at a tree, with expandable and collapsible branches."
  (with-curses
    (when (listp tree)
      (setf tree (convert-tree tree)))
    (setf *line-index* (make-stretchy-vector 100))
    (let ((line 0) (cur tree)
	  (top 0) (left 0) (height (- *lines* 3))
;	  (*line-index* (make-stretchy-vector 100))
;	  (*line-count* 0)
	  )
      (declare (special *line-index* *line-count*))
      (loop :do
	 (move 0 0) (erase)
	 (setf *line-count* 0)
	 (display-tree tree :top top :left left)
	 (mvaddstr (1- *lines*) 0
		   (format nil "~a of ~a top=~a" line *line-count* top))
	 (move (- line top) 0)
	 (case (get-char)
	   ((#\q #\Q #.(ctrl #\C)) (loop-finish))
	   ((#\return #\newline)
	    (return-from browse-tree (node-object (aref *line-index* line))))
	   (#\space ; toggle
	    (setf (node-open cur) (not (node-open cur))))
	   (#\tab ; cycle
	    (if (node-open cur)
		(if (all-subnodes-open-p cur)
		    (close-all-subnodes cur)
		    (open-all-subnodes cur))
	    	(setf (node-open cur) t)))
	   (#\+		     (setf (node-open cur) t))
	   (#\-		     (setf (node-open cur) nil))
	   ((#\n #.(ctrl #\N) :down) (incf line))
	   ((#\p #.(ctrl #\P) :up)   (decf line))
	   (#.(ctrl #\F)	     (incf line 15))
	   (#.(ctrl #\B)	     (decf line 15))
	   ((#.(ctrl #\V) :npage) (setf line (min *line-count*
						(+ line (- height 1)))))
	   (:ppage           (setf line (max 0 (- line (- height 1)))))
	   (:left	     (decf left 10))
	   (:right	     (incf left 10))
	   ((#\< :home)	     (setf line 0))
	   ((#\> :end)       (setf line (1- *line-count*)))
	   (#\escape
	    (case (get-char)
	      (#\v (setf line (min *line-count* (+ line (- height 1)))))
	      (t
	       (beep))))
	   (t (beep)))
	 ;; bound checking
	 (when (< left 0)
	   (setf left 0))
#| I don't like this:
	 ;; wrap around the top and bottom
	 (when (< line 0)
	   (setf line (1- *line-count*)))
	 (when (>= line *line-count*)
	   (setf line 0))
|#
	 ;; clamp line to the top and bottom
	 (when (< line 0)
	   (setf line 0))
	 (when (>= line *line-count*)
	   (setf line *line-count*))
	 ;; scrolling past the top and bottom
	 (when (< line top)
	   (setf top line))
	 (when (> line (+ top height))
	   (incf top (- line (+ top height))))
	 (setf cur (aref *line-index* line))))))

(defun code-format-node (node &key level indent key)
  (declare (ignore key))
  (format nil "~v,,,va~c ~s" (* level indent) #\space ""
	  (if (node-branches node)
	      (if (node-open node) #\- #\+)
	      #\space)
	  (node-object node)))

;; This could be the start of something!
(defun fake-code-browse (&optional (file (pick-file)))
  "This shows why s-exps are cool."
  (with-open-file (stm file)
    (let ((*node-formatter* #'code-format-node))
      (browse-tree
       (loop :with exp
	  :while (setf exp (read stm nil nil))
	  :collect exp)))))

(defun all-packages-tree ()
  "Return a tree browser tree of all packages."
  (labels ((package-mostly-use-list (pkg)
	     "All packages except the superfluous :COMMON-LISP package."
	     (remove nil 
		     (mapcar #'(lambda (p) 
				 (let ((name (package-name p)))
				   (when (not (equal name "COMMON-LISP"))
				     name)))
			     (package-use-list pkg)))))
    (make-node
     :object "All Packages"
     :open t
     :branches
     (loop :for p :in (list-all-packages)
	:collect (make-cached-dynamic-node
		  :object (package-name p)
		  :func #'package-mostly-use-list
		  :open nil)))))

(defun browse-packages ()
  "Browse the entire package dependency hierarchy with the tree browser."
  (browse-tree (all-packages-tree)))

;; EOF
