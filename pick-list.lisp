;;
;; pick-list.lisp - Choose things from a list.
;;

;; This is basically part of FUI, but it got too big.

(defpackage :pick-list
  (:documentation "Choose things from a list.")
  (:use :cl :dlib :curses :char-util :stretchy :keymap :opsys :inator :fui
	:terminal :terminal-curses)
  (:export
   #:pick-list
   #:pick-file
   #:pick-files
   #:do-menu*
   #:do-menu
   #:menu-load
   ))
(in-package :pick-list)

(declaim (optimize (debug 3)))
;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defkeymap *pick-list-keymap*
  `((#\escape		  . *pick-list-escape-keymap*)
    (,(ctrl #\G)	  . quit)
    (#\return		  . accept)
    (#\newline		  . accept)
    (#\space		  . pick-list-toggle-item)
    (,(ctrl #\X)	  . pick-list-toggle-region)
    ;; (,(ctrl #\N)	  . pick-list-next-line)
    (:down		  . next)
    ;; (,(ctrl #\P)	  . pick-list-previous-line)
    (:up		  . previous)
    (#\>		  . move-to-bottom)
    ;; (,(meta-char #\>) . pick-list-end-of-list)
    (:end		  . move-to-bottom)
    (#\<		  . move-to-top)
    ;; (,(meta-char #\<) . move-to-top)
    (:home		  . move-to-top)
    (,(ctrl #\F)	  . next-page)
    (,(ctrl #\V)	  . next-page)
    (:npage		  . next-page)
    (,(ctrl #\B)	  . previous-page)
    (,(meta-char #\v)	  . previous-page)
    (:ppage		  . previous-page)
    (,(meta-char #\=)	  . pick-list-binding-of-key)
    ;;(#\?		  . pick-list-help)
    (#\?		  . help)
    (,(ctrl #\@)	  . pick-list-set-mark)
    ))

(defparameter *pick-list-escape-keymap*
  (build-escape-map *pick-list-keymap*))

(defvar *pick* nil
  "The current pick list state.")

(defclass pick (fui-inator)
  ((multiple
    :initarg :multiple :accessor pick-multiple
    :initform nil :type boolean
    :documentation "True to select multiple items.")
   (by-index
    :initarg :by-index :accessor pick-by-index
    :initform nil :type boolean
    :documentation "True to return the index of picked items.")
   (typing-searches
    :initarg :typing-searches :accessor pick-typing-searches
    :initform t :type boolean
    :documentation "True if typing graphic characters searches for items.")
   (message
    :initarg :message :accessor pick-message
    ;; :initform nil :type (or null string)
    :documentation "Message to display before the list.")
   (items
    :initarg :items :accessor pick-items
    :initform nil :type sequence
    :documentation "Sequence of items to choose from.")
#| Probably can be replaced by POINT.
   (item-line
    :initarg :item-line :accessor pick-item-line
    :initform @@@ :type @@@
    :documentation ".") |#
   (result
    :initarg :result :accessor pick-result
    :initform nil
    :documentation "What the user picked.")
   (second-result
    :initarg :second-result :accessor pick-second-result
    :initform nil :type boolean
    :documentation "True if we exited normally. False, if we canceled.")
   (mark
    :initarg :mark :accessor pick-mark
    :initform nil
    :documentation "One end of the region.")
   (cur-line
    :initarg :cur-line :accessor pick-cur-line
    :initform 0 :type fixnum
    :documentation "The current line we are on in the screen.")
   (max-y ;; @@@ perhaps I should get rid of this and just check (1- *lines*)
    :initarg :max-y :accessor pick-max-y
    :initform 0 :type fixnum
    :documentation "The maximum valid screen line.")
   (top
    :initarg :top :accessor pick-top
    :initform 0 :type fixnum
    :documentation "Top line of the view.")
   (ttop								; @@@ this should probably be renamed
    :initarg :ttop :accessor pick-ttop
    :initform 0 :type fixnum
    :documentation "First line number of the list area.")
   (max-line
    :initarg :max-line :accessor pick-max-line
    :initform 0 :type fixnum
    :documentation "The maximum index number of the list.")
   (page-size
    :initarg :page-size :accessor pick-page-size
    :initform 0 :type fixnum
    :documentation "Number of lines displayed.")
   (input				; aka 'c'
    :initarg :input :accessor pick-input
    :initform 0
    :documentation "The last input.")
   (error-message
    :initarg :error-message :accessor pick-error-message
    :initform nil
    :documentation "A message to display to the user.")
   (search-str
    :initarg :search-str :accessor pick-search-str
    :initform (make-stretchy-string 10) :type string
    :documentation "The string to search for."))
  (:documentation "State for a pick-list-inator."))

;; (defmethod quit ((i pick))
;;   "Quit the list picker."
;;   (setf (inator-quit-flag i) t))

(defmethod accept ((pick pick)) ; pick-list-pick
  "Pick the current item."
  (with-slots (multiple by-index result items input second-result
	       (point inator::point)
	       (quit-flag inator::quit-flag)) pick
    (if multiple
	(when (not by-index)
	  (setf result (mapcar (_ (cdr (elt items _))) result)))
        (if by-index
	    (setf result point)
	    (setf result (cdr (elt items point)))))
    (setf quit-flag t
	  second-result (eq input #\newline)))) ; @@@ bogus check for newline

(defun pick-list-toggle-item (pick)
  "Toggle the item for multiple choice."
  (with-slots (multiple (point inator::point) result) pick
    (when multiple
      (if (position point result)
	  (setf result (delete point result))
	  (push point result)))))

(defun pick-list-set-mark (pick)
  "Set the mark to where the point is."
  (setf (pick-mark pick) (inator-point pick)))

(defun pick-list-toggle-region (pick)
  "Toggle the items in the region."
  (with-slots (multiple mark (point inator::point) result) pick
    (when (and multiple mark)
      (loop :for i :from (min mark point)
	    :to (max mark point)
	    :do
	    (if (position i result)
		(setf result (delete i result))
	        (push i result))))))

(defmethod next ((i pick))		; pick-list-next-line
  "Go to the next line. Scroll and wrap around if need be."
  (with-slots (cur-line max-y top (point inator::point) max-line) i
    (when (>= (+ cur-line 1) max-y)
      (incf top))
    (if (< point (1- max-line))
	(incf point)
        (setf point 0 top 0))))

(defmethod previous ((i pick))		; pick-list-previous-line
  "Go to the previous line. Scroll and wrap around if need be."
  (with-slots ((point inator::point) top max-line items max-y ttop) i
    (when (<= point top)
      (decf top))
    (if (> point 0)
	(decf point)
        (progn
	  (setf point (1- max-line))
	  (setf top (max 0 (- (length items)
			      (- max-y ttop))))))))

(defmethod move-to-bottom ((i pick))	; pick-list-end-of-list
  "Go to the end of the list."
  (with-slots ((point inator::point) max-line top items max-y ttop) i
    ;; (pause (format nil "~d ~d ~d ~d ~d"
    ;; 		    point max-line top max-y ttop))
    (setf point (1- max-line))
    (setf top (max 0 (- (length items) (- max-y ttop))))))

(defmethod move-to-top ((i pick))	; pick-list-beginning-of-list
  "Go to the beginning of the list."
  (with-slots ((point inator::point) top) i
    (setf point 0 top 0)))

(defmethod next-page ((i pick))		; pick-list-next-page
  "Scroll to the next page."
  (with-slots ((point inator::point) max-line page-size cur-line top) i
    (setf point     (min (1- max-line) (+ point page-size))
	  cur-line  (+ top point)
	  top       point)))

(defmethod previous-page ((i pick))	; pick-list-previous-page
  "Scroll to the previous page."
  (with-slots ((point inator::point) page-size cur-line top) *pick*
    (setf point	    (max 0 (- point page-size))
	  cur-line  (+ top point)
	  top       point)))

(defun pick-error (message &rest args)
  "Set the list picker error message."
  (setf (pick-error-message *pick*) (apply #'format nil message args)))

(defun pick-list-tmp-message (message &rest args)
  "Display a temporary message."
  (apply #'pick-error message args)
  (move (- *lines* 1) 0)
  (clrtoeol)
  (addstr (pick-error-message *pick*)))

(defun pick-list-binding-of-key (inator)
  (declare (ignore inator))
  (pick-list-tmp-message "Press a key: ")
  (let* ((key (tt-get-char))
	 (action (key-definition key *pick-list-keymap*)))
    (if action
	(pick-list-tmp-message
	 (format nil "~a is bound to ~a" (nice-char key) action))
	(pick-list-tmp-message
	 (format nil "~a is not defined" (nice-char key))))))

;; (defun pick-list-help ()
;;   (display-text "List picker keys" (help-list *pick-list-keymap*)))

;; (pick-list (loop :for i from 1 to 40 collect (format nil "~@r" i)) :message (format nil "foo~%the~%bar~%~%"))

(defmethod update-display ((i pick)) ; pick-list-display
  "Display the list picker."
  (with-slots (message multiple items (point inator::point) result cur-line
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
       (when (= i point)
	 (standout)
	 (setf cur-line y #| (getcury *stdscr*) |#))
       (addstr f)
       (when (= i point)
	 (standend))
       (addch (char-code #\newline))
       (incf i)
       (incf y)
       :while (and (< y max-y) (< i (length items))))
    (when error-message (mvaddstr (- *lines* 1) 0 error-message))
    (move cur-line 0)))

(defmethod default-action ((pick pick)) ; pick-typing-search
  "Try to search for typed input and return T if we did."
  (with-slots (typing-searches input search-str (point inator::point) max-line
	       items top page-size) pick
    (if (and typing-searches
	     (and (characterp input)
		  (graphic-char-p input) (not (position input "<> "))))
	;; Search for the seach string
	(progn
	  (stretchy-append search-str input)
	  (loop :for i :from point :below max-line
		:if (search search-str (car (elt items i)) :test #'equalp)
		:return (setf point i))
	  (when (> point (+ top page-size))
	    (setf top point))
	  t)
      ;; Clear the string
      (progn
	(stretchy-truncate search-str)
	nil))))

(defmethod await-event ((i pick))
  "Pick list input."
  (with-slots (error-message input) i
    (setf error-message nil
	  input (tt-get-char))
    input))

#|

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
	    (pick-perform-key (tt-get-char) (symbol-value command)))
	   (t				; anything else
	    (pick-error "Key binding ~S is not a function or a keymap."
			command))))
	;; a function object
	((functionp command)
	 (funcall command))
	(t				; anything else is an error
	 (error "Weird thing in keymap: ~s." command))))))
|#

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
   (let* ((string-list (mapcar (_ (cons (princ-to-string _) _)) the-list))
	  (max-y (1- curses:*lines*))
	  (*pick* (make-instance
		   'pick
		   :message	     message
		   :by-index	     by-index
		   :multiple	     multiple
		   :typing-searches   typing-searches
		   :point	     (or selected-item 0)
		   :items	     (if (not (null sort-p))
					 (locally
					  #+sbcl (declare
						  (sb-ext:muffle-conditions
						   sb-ext:compiler-note))
					  ;; Where's the unreachable code??
					  (sort string-list #'string-lessp
						:key #'car))
				         string-list)
		   :max-line          (length string-list)
		   :max-y             max-y
		   :page-size         (- max-y 2)
		   :result		default-value
		   :keymap		(list *pick-list-keymap*
					      *default-inator-keymap*))))
     (event-loop *pick*)
     #| Put in the event loop: 
     (when (not (pick-typing-search))
     (if (or (eql input (ctrl #\@)) (eql input 0))
     (setf mark point
     error-message "Set mark")
     (pick-perform-key input))))
  |#
    (values (pick-result *pick*) (pick-second-result *pick*)))))

;; Test scrolling with:
;; (:pick-list (loop for i from 1 to 60 collect (format nil "~@r~8t~r" i i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pick-file

;; @@@ Maybe this PF-DIR-ENTRY stuff should be added as a feature to
;; read-directory? Like a :printable option?

(defun pf-print-dir-entry (obj stream depth)
  "Print OBJ which should be a PF-DIR-ENTRY to stream."
  (declare (ignore depth))
  (princ (dir-entry-name obj) stream)
  (let ((type (dir-entry-type obj)))
    (cond
      ((eq type :pipe)	 	(write-char #\| stream))
      ((eq type :directory)	(write-char #\/ stream))
      ((eq type :link)	 	(write-char #\@ stream))
      ((eq type :socket) 	(write-char #\= stream)))))

(defstruct (pf-dir-entry (:include nos:dir-entry)
			 (:print-function pf-print-dir-entry))
  "A dir entry that prints nicely.")

(defun pick-file-list-generator (dir)
  (loop :for f :in (nos:read-directory :dir dir :full t)
     :collect (make-pf-dir-entry
	       :name (dir-entry-name f) :type (dir-entry-type f))))

(defun pick-file (&key message (directory ".") (allow-browse t) show-hidden
		    (pick-directories) multiple)
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
	   (cat (a b) (concatenate 'string a b))
	   (absolutize (f)
	     (abspath (path-append dir (dir-entry-name f))))
	   (single-file (f)
	     (cond
	       ((and (consp f) (= 1 (length f))) (first f))
	       (t f))))
      (generate-list)
      (if allow-browse
	  (loop :with done = nil :and f
	     :while (not done)
	     :do
	     (setf msg (format nil "~@[~a~%~]~a~%" message dir)
		   (values filename did-dir)
		   (pick-list file-list :sort-p t :message msg
			      :multiple multiple)
		   f (single-file filename))
	     (cond
	       ;; picked up level
	       ((and f (equal " [Up..]" f))
		(setf dir (dirname (cat (abspath (cat dir "/..")) "/")))
		(generate-list))
	       ;; picked a directory
	       ((and f
		     (pf-dir-entry-p f)
		     (or (eq (dir-entry-type f) :directory)
			 (eq (dir-entry-type f) :link))
		     (probe-directory
		      (path-append dir (dir-entry-name f)))
		     (and (not pick-directories) (not did-dir)))
		(setf dir (path-append dir (dir-entry-name f)))
		(generate-list))
	       ;; other files
	       (t
		(setf done t))))
	  ;; Just pick from the current directory
	  (setf filename (pick-list file-list :sort-p t :message message
				    :multiple multiple)))
      (when filename
	(typecase filename
	  ((or list array)
	   (values (mapcar #'absolutize filename) dir))
	  (t
	   (values (absolutize filename) dir)))))))

(defun pick-files (&key message (directory ".") (allow-browse t) show-hidden
		    (pick-directories))
  "Choose some files. Just a shortcut for calling pick-file with MULTIPLE = T."
  (pick-file :message message :directory directory :allow-browse allow-browse
	     :show-hidden show-hidden :pick-directories pick-directories
	     :multiple t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is actually a weird experiment which I should probably get out of here.

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
