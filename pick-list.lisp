;;
;; pick-list.lisp - Choose things from a list.
;;

;; This is basically part of FUI, but it got too big.

(defpackage :pick-list
  (:documentation "Choose things from a list.")
  (:use :cl :dlib :curses :char-util :stretchy :keymap :opsys :inator :fui)
  (:export
   #:pick-list
   #:pick-file
   #:do-menu*
   #:do-menu
   #:menu-load
   ))
(in-package :pick-list)

(declaim (optimize (debug 3)))
;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

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

;; Test scrolling with:
;; (:pick-list (loop for i from 1 to 60 collect (format nil "~@r~8t~r" i i)))

#+lish
(lish:defcommand pick-list
    ((multiple boolean :short-arg #\m
      :help "True to pick multiple results.")
     (lines string :repeating t))
  :accepts (:stream :list)
  "Pick something from the list of lines of input."
  ;; (when lish:*input*
  ;;   (format t "pick-list *input* = ~s~%" lish:*input*))
  (setf lish:*output*
	(pick-list
	 (or lines
	     (and (listp lish:*input*) lish:*input*)
	     (lish:input-line-list (and (streamp lish:*input*) lish:*input*)))
	 :multiple multiple))
  (when (lish:accepts :stream :grotty-stream :unspecified)
    (if (listp lish:*output*)
	(loop :for o :in lish:*output* :do (princ o) (terpri))
	(progn (princ lish:*output*) (terpri)))))

;; @@@ Maybe this PF-DIR-ENTRY stuff should be added as a feature to
;; read-directory? Like a :printable option?

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

;; EOF
