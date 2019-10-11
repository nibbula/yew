;;
;; pick-list.lisp - Choose things from a list.
;;

;; This is basically part of FUI, but it got too big.

(defpackage :pick-list
  (:documentation "Choose things from a list.")
  (:use :cl :dlib :char-util :stretchy :keymap :opsys :inator :terminal
	:terminal-inator :fui :view-generic)
  (:import-from :inator #:mark #:point #:quit-flag)
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

(defvar *pick* nil
  "The current pick list state.")

(defclass pick (terminal-inator)
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
    :documentation ".")
   Replaced by inator-mark
   (mark
    :initarg :mark :accessor pick-mark
    :initform nil
    :documentation "One end of the region.") |#
   (result
    :initarg :result :accessor pick-result
    :initform nil
    :documentation "What the user picked.")
   (second-result
    :initarg :second-result :accessor pick-second-result
    :initform nil :type boolean
    :documentation "True if we exited normally. False, if we canceled.")
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
   (left
    :initarg :left :accessor pick-left
    :initform 0 :type fixnum
    :documentation "Position of the left edge of the view.")
   (ttop				; @@@ this should probably be renamed
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
    :documentation "The string to search for.")
   (before-hook
    :initarg :before-hook :accessor pick-before-hook :initform nil
    :documentation "Function to call before entering the event loop.
The function receives a 'pick' as an argument."))
  (:documentation "State for a pick-list-inator."))

;; (defmethod quit ((i pick))
;;   "Quit the list picker."
;;   (setf (inator-quit-flag i) t))

(defmethod accept ((pick pick)) ; pick-list-pick
  "Pick the current item."
  (with-slots (multiple by-index result items input second-result point
	       quit-flag) pick
    (if multiple
	(when (not by-index)
	  (setf result (nreverse (mapcar (_ (cdr (elt items _))) result))))
        (if by-index
	    (setf result point)
	    (setf result (cdr (elt items point)))))
    (setf quit-flag t
	  second-result (eq input #\newline)))) ; @@@ bogus check for newline

(defun pick-list-toggle-item (pick)
  "Toggle the item for multiple choice."
  (with-slots (multiple point result) pick
    (when multiple
      (if (position point result)
	  (setf result (delete point result))
	  (push point result)))))

(defun pick-list-set-mark (pick)
  "Set the mark to where the point is."
  (setf (inator-mark pick) (inator-point pick)))

(defun pick-list-toggle-region (pick)
  "Toggle the items in the region."
  (with-slots (multiple mark point result) pick
    (when (and multiple mark)
      (loop :for i :from (min mark point)
	    :to (max mark point)
	    :do
	    (if (position i result)
		(setf result (delete i result))
	        (push i result))))))

(defmethod next ((i pick))		; pick-list-next-line
  "Go to the next line. Scroll and wrap around if need be."
  (with-slots (cur-line max-y top point max-line) i
    (when (>= (+ cur-line 1) max-y)
      (incf top))
    (if (< point (1- max-line))
	(incf point)
        (setf point 0 top 0))))

(defmethod previous ((i pick))		; pick-list-previous-line
  "Go to the previous line. Scroll and wrap around if need be."
  (with-slots (point top max-line items max-y ttop) i
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
  (with-slots (point max-line top items max-y ttop) i
    ;; (pause (format nil "~d ~d ~d ~d ~d"
    ;; 		    point max-line top max-y ttop))
    (setf point (1- max-line))
    (setf top (max 0 (- (length items) (- max-y ttop))))))

(defmethod move-to-top ((i pick))	; pick-list-beginning-of-list
  "Go to the beginning of the list."
  (with-slots (point top) i
    (setf point 0 top 0)))

(defmethod next-page ((i pick))		; pick-list-next-page
  "Scroll to the next page."
  (with-slots (point max-line page-size cur-line top) i
    (setf point     (min (1- max-line) (+ point page-size))
	  cur-line  (+ top point)
	  top       point)))

(defmethod previous-page ((i pick))	; pick-list-previous-page
  "Scroll to the previous page."
  (with-slots (point page-size cur-line top) *pick*
    (setf point	    (max 0 (- point page-size))
	  cur-line  (+ top point)
	  top       point)))

(defun shift-left (o)
  "Shift the edge left."
  (with-slots (left) o
    (decf left 10)
    (when (< left 0)
      (setf left 0))))

(defun shift-right (o)
  "Shift the edge right."
  (incf (pick-left o) 10))

(defun shift-beginning (o)
  "Shift the view all the way left."
  (setf (pick-left o) 0))

(defun shift-end (o)
  "Shift to the end of the rightmost content."
  (with-slots (left items point) o
    (setf left
	  (max 0
	       (- (length (car (elt items point)))
		  (- (tt-width) 3))))))

(defun pick-error (message &rest args)
  "Set the list picker error message."
  (setf (pick-error-message *pick*) (apply #'format nil message args)))

(defun pick-list-tmp-message (message &rest args)
  "Display a temporary message."
  (apply #'pick-error message args)
  (tt-move-to (- (tt-height) 1) 0)
  (tt-erase-to-eol)
  (tt-write-string (pick-error-message *pick*)))

(defmethod message ((i pick) format-string &rest args)
  (apply #'pick-list-tmp-message format-string args))

;; (pick-list (loop :for i from 1 to 40 collect (format nil "~@r" i)) :message (format nil "foo~%the~%bar~%~%"))

(defmethod update-display ((i pick)) ; pick-list-display
  "Display the list picker."
  (with-slots (message multiple items point result cur-line
	       max-y top left ttop error-message) *pick*
    (tt-home)
    (when message (tt-format message))
    (setf ttop (terminal-get-cursor-position *terminal*))
    ;; display the list
    (loop :with i = top :and y = ttop :and f = nil
       :do
       (setf f (car (elt items i)))
       (tt-erase-to-eol)
       (if (and multiple (position i result))
	   (tt-write-string "X ")
	   (tt-write-string "  "))
       (when (= i point)
	 (tt-inverse t)
	 (setf cur-line y #| (getcury *stdscr*) |#))
       (when (<= left (1- (length f)))
	 (tt-write-string
	  (subseq f (max 0 left)
		  (min (length f)
		       (+ left (- (tt-width) 3)))))) ; 3 = (length "X ") + 1
       (when (= i point)
	 (tt-inverse nil))
       (tt-write-char #\linefeed)
       (incf i)
       (incf y)
       :while (and (< y max-y) (< i (length items))))
    (tt-erase-below)
    (when error-message
      (tt-move-to (- (tt-height) 1) 0)
      (tt-write-string error-message))
    (tt-move-to cur-line 0)))

(defmethod default-action ((pick pick)) ; pick-typing-search
  "Try to search for typed input and return T if we did."
  (with-slots (typing-searches input search-str point max-line
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
	  input (tt-get-key))
    input))

(defgeneric delete-pick (pick)
  (:documentation "Delete the picker."))

(defmethod delete-pick ((pick pick))
  (declare (ignore pick))
  ;; Don't do anything.
  (tt-move-to (- (tt-height) 2) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popup pick-list

(defclass popup-pick (pick)
  ((x
    :initarg :x :accessor popup-pick-x :initform 1 :type fixnum
    :documentation "Left column of popup window.")
   (y
    :initarg :y :accessor popup-pick-y :initform 1 :type fixnum
    :documentation "Top row of popup window.")
   (window
    :initarg :window :accessor popup-pick-window
    :documentation "The FUI window."))
  (:documentation "A pop up list."))

(locally
    ;; I really don't want to have a make-load-form.
    #+sbcl (declare (sb-ext:muffle-conditions style-warning))
(defmethod initialize-instance
    :after ((o popup-pick) &rest initargs &key &allow-other-keys)
  "Initialize a popup-pick."
  (declare (ignore initargs))
  (with-slots (top max-line max-y message items x y) o
    (let ((lines (+ (count #\newline message) max-line))
	  (cols (loop :for i :across items :maximize (length (car i)))))
      ;;(dbugf :fui "popup x=~s y=~s top=~s~%" x y top)
      (setf x (max 1 x)			; leave room for the border
	    y (max 1 y)
	    max-y (if (> (+ y lines 2) (1- (tt-height)))
		      (- (- (tt-height) 2) y)
		      (1+ lines))
	    (slot-value o 'window) (make-window :height (+ max-y 1)
						:width (min (+ cols 2 2)
							    (- (tt-width) 2))
						:y y :x x
						:border t)
	    ;; top (1+ top)
	    )))))

(defmethod update-display ((i popup-pick))
  "Display the pop-up list picker."
  (with-slots (message multiple items point result cur-line
	       max-y top left ttop error-message window #|x y|#) *pick*
    (let ((message-string (and message (format nil message))))
      (draw-window window)
      (window-move-to window 0 0)
      (when message
	(window-text window message-string))
      ;;(setf ttop (getcury window))
      ;;(setf ttop (terminal-get-cursor-position *terminal*))
      (setf ttop (fui-window-text-y window))
      ;;(setf ttop (count #\newline message-string))
      (dbugf :fui "max-y=~d top=~s ttop=~s~%" max-y top ttop)
      ;; display the list
      (loop :with i = top :and y = ttop :and f = nil
	 :do
	 (setf f (car (elt items i)))
	 (if (and multiple (position i result))
	     (window-text window "X " :row y :column 0)
	     (window-text window "  "))
	 (when (= i point)
	   (tt-standout t)
	   ;;(setf cur-line y #| (getcury *stdscr*) |#)
	   ;;(setf cur-line (fui-window-text-y window))
	   (setf cur-line y)
	   )
	 ;;(dbugf :fui "cur-line=~d y=~s i=~s~%" cur-line y i)
	 (when (<= left (1- (length f)))
	     (window-text window
			  (subseq f (max 0 left)
				  (min (length f)
				       (+ left (- (tt-width) 3))))
			  :row y :column 2))
	 (when (= i point)
	   (tt-standout nil))
	 (incf i)
	 (incf y)
	 :while (and (< y max-y) (< i (length items))))
      (when error-message
	(tt-write-string-at (- (tt-height) 1) 0 error-message))
      (window-move-to window cur-line 1)
      (tt-finish-output)
      )))

(defmethod delete-pick ((pick popup-pick))
  (with-slots (window) pick
    (delete-window window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-pick (type &rest args)
  (let ((*pick*))
    (unwind-protect
	 (progn
	   (setf *pick* (apply #'make-instance type args))
	   (when (pick-before-hook *pick*)
	     (funcall (pick-before-hook *pick*) *pick*))
	   (event-loop *pick*))
      (when *pick*
	(delete-pick *pick*)))
    (values (pick-result *pick*) (pick-second-result *pick*))))

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
    (:left		  . shift-left)
    (:right	     	  . shift-right)
    (,(ctrl #\A)	  . shift-beginning)
    (,(ctrl #\E)	  . shift-end)
    (#\?		  . help)
    (,(ctrl #\@)	  . pick-list-set-mark)
    ))

(defparameter *pick-list-escape-keymap*
  (build-escape-map *pick-list-keymap*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pick-list (the-list &key message by-index sort-p default-value
			     selected-item (typing-searches t) multiple
			     popup (x 0) (y 0) height before-hook)
  "Have the user pick a value from THE-LIST and return it. Arguments:
  MESSAGE         - A string to be displayed before the list.
  BY-INDEX        - If true, return the index number of the item picked.
  SORT-P          - If true sort the list before displaying it.
  DEFAULT-VALUE   - Return if no item is selected.
  SELECTED-ITEM   - Item to have initially selected.
  TYPING-SEARCHES - True to have alphanumeric input search for the item.
  MULTIPLE        - True to allow multiple items to be selected.
  POPUP		  - True to use a pop-up window, in which case provide X and Y."
  (when (not the-list)
    (return-from pick-list nil))
  (with-terminal (#+unix :crunch)
    (let* ((max-y (1- (tt-height)))
	   (count (length the-list))
	   (string-list (make-array count :element-type 'cons
				    :initial-element (cons nil nil))))
      (loop
	 :for i :from 0
	 :for item :in the-list
	 :do (setf (aref string-list i) (cons (princ-to-string item) item)))
     (setf string-list
	   (if (not (null sort-p))
	       (locally
		   #+sbcl (declare
			   (sb-ext:muffle-conditions
			    sb-ext:compiler-note))
		   ;; Where's the unreachable code??
		   (sort string-list #'string-lessp
			 :key #'car))
	       string-list))
     (when (not popup)
       (tt-clear))
     (do-pick (if popup 'popup-pick 'pick)
       :message	        message
       :by-index	by-index
       :multiple	multiple
       :typing-searches typing-searches
       :point	        (or selected-item 0)
       :items	        string-list
       :max-line        (length string-list)
       :max-y           (or height max-y)
       :page-size       (- max-y 2)
       :x		x
       :y		y
       :result	        default-value
       :before-hook	before-hook
       :keymap	        (list *pick-list-keymap*
			      *default-inator-keymap*)))))

#| Put in the event loop: 
(when (not (pick-typing-search))
  (if (or (eql input (ctrl #\@)) (eql input 0))
      (setf mark point
	    error-message "Set mark")
      (pick-perform-key input))))
|#

;; Test scrolling with:
;; (pick-list (loop for i from 1 to 60 collect (format nil "~@r~8t~r" i i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod view ((thing list))
  (pick-list thing))

#+lish
(lish:defcommand pick-list
  ((multiple boolean :short-arg #\m :help "True to pick multiple results.")
   (print boolean :short-arg #\p :help "True to force printing the results.")
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
  (cond
    ((lish:accepts :sequence 'list)
     lish:*output*)
    ((or print (lish:accepts :stream :grotty-stream :unspecified))
     (if (listp lish:*output*)
	 (loop :for o :in lish:*output* :do (princ o) (terpri))
	 (progn (princ lish:*output*) (terpri))))))

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
	   ;; (cat (a b) (concatenate 'string a b))
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
		(setf dir (dirname (s+ (abspath (path-append dir ".."))
				       *directory-separator*)))
		(when (zerop (length dir))
		  (setf dir (string *directory-separator*)))
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
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
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

(defun do-menu* (menu &key message selected-item popup (x 0) (y 0))
  "Perform an action from a menu. Menu is an alist of (item . action)."
  (let ((items (loop :for m :in menu :collect (car m)))
	(funcs (loop :for m :in menu :collect (cdr m))))
    (let* ((n (pick-list items :by-index t
			       :message message
			       :selected-item selected-item
			       :popup popup
			       :x x :y y))
	   (code (if n (elt funcs n))))
      (if code
	  (eval code)
	  :quit))))

(defmacro do-menu (menu &key message selected-item popup (x 0) (y 0))
  "Perform an action from a menu. MENU is an alist of (ITEM . ACTION), where
ITEM is something to print, as with princ, and ACTION is a function to call.
Arguments:
MESSAGE is some text to display before the menu.
SELECTED-ITEM is the item that is initially selected.
This is a macro so you can use lexically scoped things in the menu."
  ;;; @@@ improve to one loop
  (cond
    ((symbolp menu)
     `(do-menu* ,menu :message ,message :selected-item ,selected-item
		:popup ,popup :x ,x :y ,y))
    ((listp menu)
     (let ((items (loop :for (i . nil) :in menu :collect i))
	   (funcs (loop :for (nil . f) :in menu :collect f))
	   (n-sym (gensym)))
       `(block menu			; ! anaphoric or un-hygenic ?
	  (let* ((,n-sym (pick-list ',items :by-index t
				    :message ,message
				    :selected-item ,selected-item
				    :popup ,popup :x ,x :y ,y)))
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

(defvar *interactive* t
  "True when we can expect user interaction.")

;; @@@ cribbed from fui. we should put it somewhere else.
#|
(defun pause (&optional (prompt "[Press Enter]") &rest args)
  "Print a message and wait for Enter to be pressed. Does nothing if not
*interactive*."
  (when *interactive*
    (apply #'format *standard-output* prompt args)
    (finish-output *standard-output*)
    (read-line)))
|#

(defun show-result (expr)
  (unwind-protect
     (progn
       (terminal-end *terminal*)
;       (format t "showing ~s~%" expr)
       (let ((vals (multiple-value-list (eval expr))))
	 (loop :for v :in vals :do (print v))
	 (pause)))
    (terminal-start *terminal*))
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
