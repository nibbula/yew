;;;
;;; pick-list.lisp - Choose things from a list.
;;;

;; This is basically part of FUI, but it got too big. These are still just a
;; temporary step until we have a more comprehensive UI.

(defpackage :pick-list
  (:documentation "Choose things from a list.

This is a terminal-inator called ‘pick’ to choose things from a list.
The main entry point is the ‘pick-list’ function. Multiple items can be chosen
by specifying :multiple t. There is a popup list picker that pretends to be
in a window, by specifying :popup t. There is also ‘pick-file’ which is a
list picker customized for picking files, which allows directory navigation.
If the shell is loaded, this will define a ‘pick-list’ command.

For acting like a menu and performing an action when a list item is selected,
there is the ‘do-menu’ macro which can access lexically scoped things in the
menu without having to make closures, and the ‘do-menu*’ function which can't.
")
  (:use :cl :dlib :char-util :stretchy :keymap :opsys :inator :terminal
	:terminal-inator :fui :view-generic :collections :ochar :ostring
	:fatchar)
  (:import-from :inator #:mark #:point #:quit-flag)
  (:export
   #:pick
   #:full-pick
   #:popup-pick
   #:file-picker
   #:pick-list
   #:!pick-list
   #:pick-file
   #:pick-files
   #:do-menu*
   #:do-menu
   #:menu-load
   ))
(in-package :pick-list)

(defkeymap *pick-list-keymap* ( #|:default-binding #'default-action |#)
  `((#\escape		  . *pick-list-escape-keymap*)
    (,(ctrl #\G)	  . quit)
    (#\return		  . accept)
    (#\newline		  . accept)
    (#\space		  . pick-list-toggle-item)
    (,(ctrl #\X)	  . *pick-list-ctrl-x-keymap*)
    (:down		  . next)
    (:up		  . previous)
    (#\>		  . move-to-bottom)
    (:end		  . move-to-bottom)
    (#\<		  . move-to-top)
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
    (,(ctrl #\@)	  . select)
    (,(meta-char #\w)	  . copy)
    (,(ctrl #\R)	  . search-backward-command)
    (:press		  . handle-press)
    (:release		  . handle-release)
    (:scroll-up		  . scroll-up)
    (:scroll-down	  . scroll-down)
    ))

(defparameter *pick-list-escape-keymap*
  (build-escape-map *pick-list-keymap*))

(defkeymap *pick-list-ctrl-x-keymap* ()
  `((,(ctrl #\V)	. view-item)
    (,(ctrl #\X)	. pick-list-toggle-region)))

(defvar *pick* nil
  "The current pick list state.")

(defclass pick (terminal-inator)
  ((items
    :initarg :items :accessor pick-items
    :initform nil :type sequence
    :documentation "Sequence of items to choose from.")
   (multiple
    :initarg :multiple :accessor pick-multiple
    :initform nil :type boolean
    :documentation "True to select multiple items.")
   (sort-p
    :initarg :sort-p :accessor pick-sort-p :initform t :type boolean
    :documentation "True to sort the list before displaying.")
   (ordered-p
    :initarg :ordered-p :accessor pick-ordered-p :initform nil :type boolean
    :documentation
    "True to pick an order for multiple items. This has no effect unless
multiple is true. It also has to turn off typing-searches so numbers can be
entered.")
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
   (selected-item
    :initarg :selected-item :accessor pick-selected-item :initform nil
    :documentation "The index of the item to be pre-selected on entry.")
   (result
    :initarg :result :accessor pick-result
    :initform nil
    :documentation "What the user picked.")
   (second-result
    :initarg :second-result :accessor pick-second-result
    :initform nil :type boolean
    :documentation "True if we exited normally. False, if we canceled.")
   (default-value
    :initarg :default-value :accessor pick-default-value :initform nil
    :documentation "Value to return if nothing is picked.")
   (cur-line
    :initarg :cur-line :accessor pick-cur-line
    :initform 0 :type fixnum
    :documentation "The current line we are on in the screen.")
   (top
    :initarg :top :accessor pick-top
    :initform 0 :type fixnum
    :documentation "Top line of the view.")
   (left
    :initarg :left :accessor pick-left
    :initform 0 :type fixnum
    :documentation "Position of the left edge of the view.")
   (list-top
    :initarg :list-top :accessor pick-list-top
    :initform 0 :type fixnum
    :documentation "First line number of the list area.")
   (max-y ;; @@@ perhaps I should get rid of this and just check (1- *lines*)
    :initarg :max-y :accessor pick-max-y
    :initform 0 :type fixnum
    :documentation "The maximum valid screen line.")
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
   (mouse-event
    :initarg :mouse-event :accessor pick-mouse-event :initform nil
    :documentation "The mouse event.")
   (button-range
    :initarg :button-range :accessor pick-button-range :initform nil
    :documentation "The coordinate of items as buttons.")
   (error-message
    :initarg :error-message :accessor pick-error-message
    :initform nil
    :documentation "A message to display to the user.")
   (search-str
    :initarg :search-str :accessor pick-search-str
    :initform (make-stretchy-string 10) :type string
    :documentation "The string to search for.")
   (save-search
    :initarg :save-search :accessor pick-save-search :initform nil :type boolean
    :documentation "Flag for saving the search string while typing.")
   (last-search
    :initarg :last-search :accessor pick-last-search :initform "" :type string
    :documentation "Last string searched for.")
   (before-hook
    :initarg :before-hook :accessor pick-before-hook :initform nil
    :documentation "Function to call before entering the event loop.
The function receives a 'pick' as an argument."))
  (:default-initargs
   :default-keymap *pick-list-keymap*)
  (:documentation "State for a pick-list-inator."))

;; (defmethod initialize-instance
;;     :after ((o pick) &rest initargs &key &allow-other-keys)
;;   "Initialize a pick."
;;   (declare (ignore initargs)))

(defmethod run ((pick pick) &rest keys &key list &allow-other-keys)
  (declare (ignorable keys))
  (when (not list)
    (return-from run nil))
  (with-terminal ()
    (with-slots (point items max-line max-y page-size result second-result
		 default-value keymap selected-item sort-p) pick
      (let* ((string-list (make-array (olength list)
				      :element-type 'cons
				      :initial-element (cons nil nil))))
	;; Set the array of the items converted to strings.
	(let ((i 0))
	  (omapn (lambda (item)
		   (setf (aref string-list i)
			 (cons
			  (or (and (ostringp item) item)
			      (princ-to-string item))
			  item))
		   (incf i))
		 list))
	;; Sort the items if requested
	(setf string-list
	      (if (not (null sort-p))
		  (locally
		 #+sbcl (declare
			 (sb-ext:muffle-conditions
			  sb-ext:compiler-note))
		 ;; Where's the unreachable code??
		 (sort string-list #'ostring-lessp
		       :key #'car))
	     string-list))
	(setf point      (or selected-item 0)
	      items      string-list
	      max-y      (1- (tt-height))
	      max-line   (length string-list)
	      page-size  (- max-y 2)
	      result     default-value)
	(unwind-protect
	     (progn
	       (when (pick-before-hook *pick*)
		 (funcall (pick-before-hook *pick*) *pick*))
	       (call-next-method)) ;; (event-loop *pick*)
	  (when pick
	    (delete-pick pick))))
      (values result second-result))))

(defmethod start-inator :after ((pick pick))
  (tt-enable-events :mouse-buttons))

(defmethod finish-inator :after ((pick pick))
  (tt-disable-events :mouse-buttons))

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

(defun handle-press (pick)
  "Handlle a press event."
  (with-slots (mouse-event button-range point top) pick
    (when (and mouse-event button-range)
      (loop
	 :for item :in button-range
	 :for i = 0 :then (1+ i)
	 :when (and (= (tt-mouse-event-y mouse-event) (car item))
		    (< (tt-mouse-event-x mouse-event) (cdr item)))
	 :do
	 (setf point (+ top i))
	 ;; (update-display pick)
	 ;;(accept pick)
	 (return nil)))))

(defun handle-release (pick)
  (accept pick))

(defun pick-list-toggle-item (pick)
  "Toggle the item for multiple choice."
  (with-slots (multiple point result) pick
    (when multiple
      (if (position point result)
	  (setf result (delete point result))
	  (push point result)))))

;; (defun pick-list-set-mark (pick)
;;   "Set the mark to where the point is."
;;   (setf (inator-mark pick) (inator-point pick)))

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

(defmethod select ((pick pick))
  "Set the mark to where the point is."
  (setf (inator-mark pick) (inator-point pick)))

(defmethod copy ((pick pick))
  "Toggle selecting the items in the region."
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
  (with-slots (point top max-line items max-y list-top) i
    (when (<= point top)
      (decf top))
    (if (> point 0)
	(decf point)
        (progn
	  (setf point (1- max-line))
	  (setf top (max 0 (- (length items)
			      (- max-y list-top))))))))

(defun scroll-down (pick &optional (n 5))
  "Scroll down by N items."
  (with-slots (top point max-y) pick
    (if (> top n)
	(decf top n)
	(setf top 0))
    (when (>= point (+ top max-y))
      (setf point (+ top (- max-y 2))))))

(defun scroll-up (pick &optional (n 5))
  "Scroll up by N items."
  (with-slots (top point max-line) pick
    (if (< (+ top n) max-line)
	(incf top n)
	(setf top (1- max-line)))
    (when (< point top)
      (setf point top))))

(defmethod move-to-bottom ((i pick))	; pick-list-end-of-list
  "Go to the end of the list."
  (with-slots (point max-line top items max-y list-top) i
    ;; (pause (format nil "~d ~d ~d ~d ~d"
    ;; 		    point max-line top max-y list-top))
    (setf point (1- max-line))
    (setf top (max 0 (- (length items) (- max-y list-top))))))

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
	       (- (olength (car (elt items point)))
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
	       max-y top left list-top error-message search-str save-search
	       last-search button-range) *pick*
    (tt-home)
    (when message
      ;; @@@ Ideally we should be able to format fat-strings.
      (if (typep message 'fat-string)
	  (tt-write-string message)
	  (tt-format message)))
    (setf list-top (terminal-get-cursor-position *terminal*)
	  button-range nil)
    (when (not save-search)
      (setf last-search (copy-seq search-str))
      (stretchy-truncate search-str))
    ;; display the list
    (loop :with i = top :and y = list-top :and f = nil :and str
       :do
       (setf f (car (elt items i)))
       (tt-erase-to-eol)
       (if (and multiple (position i result))
	   (tt-write-string "X ")
	   (tt-write-string "  "))
       (when (= i point)
	 (tt-inverse t)
	 (setf cur-line y #| (getcury *stdscr*) |#))
       (when (<= left (1- (olength f)))
	 (setf str (osubseq f (max 0 left)
			    (min (olength f)
				 (+ left (- (tt-width) 3))))) ; 3 = "X " + 1
	 (tt-write-string
	  (if (= i point) (span-to-fat-string `(:inverse ,str)) str)))
       (when (= i point)
	 (tt-inverse nil))
       (push (cons y (olength str)) button-range)
       (tt-write-char #\linefeed)
       (incf i)
       (incf y)
       :while (and (< y max-y) (< i (length items))))
    (setf button-range (nreverse button-range))
    (tt-erase-below)
    (let ((msg (or error-message
		   (and (not (zerop (length search-str)))
			(format nil "Search: ~a" search-str)))))
      (when msg
	(tt-write-string-at (- (tt-height) 1) 0 msg)))
    (setf save-search nil)
    (tt-move-to cur-line 0)))

(defun search-for (o string &optional (direction :forward))
  "Search for STRING and set POINT accordingly."
  (with-slots (point max-line items top page-size) o
    (let (found)
      (ecase direction
	(:forward
	 (loop :for i :from point :below max-line
	    ;; :if (search string (car (elt items i)) :test #'equalp)
	    :if (osearch string (car (elt items i)))
	    :return (setf found i point i))
	 (when (> point (+ top page-size))
	   (setf top point)))
	(:backward
	 (loop :for i :from point :downto 0
	    ;; :if (search string (car (elt items i)) :test #'equalp)
	    :if (osearch string (car (elt items i)))
	    :return (setf found i point i))
	 (when (< point top)
	   (setf top point))))
      found)))

(defun search-list-command (o &optional (direction :forward))
  (with-slots (search-str point max-line save-search last-search) o
    (flet ((nudge-point ()
	     "Move the start one line in the direction, with wrap around."
	     (ecase direction
	       (:forward
		(if (< point max-line)
		    (incf point)
		    (move-to-top o)))
	       (:backward
		(if (= point 0)
		    (move-to-bottom o)
		    (decf point))))))
      (setf save-search t)
      (let ((old-point point))
	(cond
	  ;; Search for the next occurance of the already active search.
	  ((not (zerop (length search-str)))
	   (nudge-point)
	   (when (not (search-for o search-str direction))
	     (setf point old-point)))
	  (t
	   ;; Ask for a new string
	   (tt-move-to (1- (tt-height)) 0) (tt-erase-to-eol)
	   (tt-finish-output) ;; @@@ I wish this wasn't necessary!
	   (let ((result (rl:rl :prompt "Search for: " :accept-does-newline nil
				:string last-search)))
	     (tt-finish-output)
	     (if (and result (not (zerop (length result))))
		 (progn
		   (setf last-search (copy-seq result))
		   (nudge-point)
		   (when (not (search-for o result direction))
		     (message o "~s not found" result)))
		 ;; Clear the search string when nothing entered
		 (stretchy-truncate search-str)))))))))

(defmethod search-command ((o pick))
  (search-list-command o :forward))

(defun search-backward-command (o)
  (search-list-command o :backward))

(defmethod sort-command ((o pick))
  "Sort the list items."
  (with-slots (items) o
    (locally
	#+sbcl (declare
		(sb-ext:muffle-conditions
		 sb-ext:compiler-note))
	;; Where's the unreachable code??
	(setf items (sort items #'ostring-lessp :key #'car)))))

(defmethod default-action ((pick pick) &optional event) ; pick-typing-search
  "Try to search for typed input and return T if we did."
  (declare (ignore event)) ;; @@@ we should probably use this instead of ‘input’
  (with-slots (typing-searches input search-str point max-line
	       items top page-size save-search) pick
    (if (and typing-searches
	     (characterp input)
	     (graphic-char-p input) (not (position input "<> ")))
	;; Search for the seach string
	(progn
	  (setf save-search t)
	  (stretchy-append search-str input)
	  (loop :for i :from point :below max-line
	     :if (osearch search-str (car (elt items i)) :test #'ochar-equal)
	     :return (setf point i))
	  (when (> point (+ top page-size))
	    (setf top point))
	  t)
      ;; Clear the string
      (progn
	(setf save-search nil)
	nil))))

(defmethod await-event ((i pick))
  "Pick list input."
  (with-slots (error-message input mouse-event) i
    (setf error-message nil
	  mouse-event nil
	  input (tt-get-key))
    (when (and (typep input 'tt-mouse-button-event))
      (setf mouse-event input)
      (case (tt-mouse-button input)
	(:button-1
	 (setf input
	       (if (typep input 'tt-mouse-button-release)
		   :release
		   :press)))
	(:release (setf input :release))
	(:button-4 (setf input :scroll-down))
	(:button-5 (setf input :scroll-up))))
    input))

(defgeneric delete-pick (pick)
  (:documentation "Delete the picker."))

(defmethod delete-pick ((pick pick))
  (declare (ignorable pick))
  ;; Don't do anything.
  (tt-move-to (- (tt-height) 2) 0))

(defun view-this-item (pick item)
  (declare (ignore pick))
  (handler-case
      (view item)
    (error (c)
      (display-text
       nil
       `("" "There was an error when viewing:" ""
	    ,(princ-to-string c) ""))))
  (tt-clear))

(defgeneric view-item (pick)
  (:documentation "View an item."))

(defmethod view-item ((pick pick))
  "View an item."
  (with-slots (items point) pick
    (view-this-item pick (cdr (elt items point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  full-screen-pick

(defclass full-pick (pick)
  ()
  (:documentation "Pick a list using the full window."))

(defmethod start-inator ((pick full-pick))
  (tt-clear)
  (call-next-method))

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

;; (locally
;;     ;; I really don't want to have a make-load-form.
;;     #+sbcl (declare (sb-ext:muffle-conditions style-warning))
;; (defmethod initialize-instance
;;     :after ((o popup-pick) &rest initargs &key &allow-other-keys)
;;   "Initialize a popup-pick."
;;   (declare (ignore initargs))
;;   ))

(defmethod start-inator ((o popup-pick))
  (with-slots (top max-line max-y message items x y) o
    (let ((lines (+ (count #\newline message) max-line))
	  (cols (loop :for i :across items :maximize (length (car i)))))
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
	    ))))

(defmethod update-display ((i popup-pick))
  "Display the pop-up list picker."
  (with-slots (message multiple items point result cur-line
	       max-y top left list-top error-message window #|x y|#
	       search-str save-search last-search) *pick*
    (let ((message-string (and message (format nil message))))
      (draw-window window)
      (window-move-to window 0 0)
      (when message
	(window-text window message-string))
      (when (not save-search)
	(setf last-search (copy-seq search-str))
	(stretchy-truncate search-str))
      ;;(setf list-top (getcury window))
      ;;(setf list-top (terminal-get-cursor-position *terminal*))
      (setf list-top (fui-window-text-y window))
      ;;(setf list-top (count #\newline message-string))
      (dbugf :fui "max-y=~d top=~s list-top=~s~%" max-y top list-top)
      ;; display the list
      (loop :with i = top :and y = list-top :and f = nil
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
      (let ((msg (or error-message
		     (and (not (zerop (length search-str)))
			  (format nil "Search: ~a" search-str)))))
	(when msg
	  (window-move-to window (1- (fui-window-height window)) 0)
	  (tt-write-string msg)))
      (setf save-search nil)
      (window-move-to window cur-line 1)
      (tt-finish-output)
      )))

(defmethod delete-pick ((pick popup-pick))
  (with-slots (window) pick
    (delete-window window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pick-list (list &key message by-index sort-p default-value
			 selected-item (typing-searches t)
			 (keymap nil keymap-provided-p)
			 multiple popup (x 0) (y 0) height before-hook type)
  "Have the user pick a value from ‘list’ and return it. Arguments are:

 ‘message’         A string to be displayed before the list.
 ‘by-index’        If true, return the index number of the item picked.
 ‘sort-p’          If true sort the list before displaying it.
 ‘default-value’   Return if no item is selected.
 ‘selected-item’   Item to have initially selected.
 ‘typing-searches’ True to have alphanumeric input search for the item.
 ‘keymap’          Add a custom keymap.
 ‘multiple’        True to allow multiple items to be selected.
 ‘popup’           True to use a pop-up window, in which case provide X and Y.
 ‘before-hook’     A function to call before entering the event loop.
 ‘type’            Class of the list picker.
"
  (let ((initargs
	  `(:message         ,message
	    :by-index        ,by-index
	    :sort-p          ,sort-p
	    :default-value   ,default-value
	    :selected-item   ,selected-item
	    :typing-searches ,typing-searches
	    ;; If a keymap is given, put it in front of the standard ones.
	    ;; :keymap          (when keymap
	    ;; 		    (list keymap
	    ;; 		     *pick-list-keymap* *default-inator-keymap*))
	    :multiple        ,multiple
	    :popup           ,popup
	    :x               ,x
	    :y               ,y
	    :height          ,height
	    :before-hook     ,before-hook))
	(*pick*))
    (when keymap-provided-p
      (setf (getf initargs :local-keymap) keymap))
    (setf *pick*
	  (apply #'make-instance
		 (or type (if popup 'popup-pick 'full-pick))
		 initargs))
    (run *pick* :list list)))

#| Figure out where something like this should go in the event loop:
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

;; @@@ This seems like a somewhat inappropriate place for this.
(defmethod view ((thing dir-entry))
  "View a directory as it's file name."
  (view (dir-entry-name thing)))

(defclass file-picker (pick)
  ((directory
    :initarg :directory :accessor file-picker-directory
    :documentation "The directory displayed.")
   (allow-browse
    :initarg :allow-browse :accessor file-picker-allow-browse
    :initform t :type boolean
    :documentation
    "True to allow navigating to and picking files from other directories.")
   (show-hidden
    :initarg :show-hidden :accessor file-picker-show-hidden
    :initform nil :type boolean
    :documentation "True to show hidden files.")
   (pick-directories
    :initarg :pick-directories :accessor file-picker-pick-directories
    :initform nil :type boolean
    :documentation
    "If true, selecting a directory accepts it instead of navigating to it.")
   (directory-accepted
    :initarg :directory-accepted :accessor directory-accepted
    :initform nil :type boolean
    :documentation "True if a directory was accepted."))
  (:documentation "A list picker for files."))

(defmethod run ((pick file-picker) &rest keys &key &allow-other-keys)
  (declare (ignore keys))
  (with-slots (directory show-hidden allow-browse message pick-directories
	       quit-flag) pick
    (let* (files file-list filename msg did-dir)
      (labels ((generate-list ()
		 "Set FILES to the list of files for DIRECTORY, and FILE-LIST
                  to the list to display."
		 (setf files
		       (loop :for file :in (pick-file-list-generator directory)
			  :if (or (char/= #\. (char (dir-entry-name file) 0))
				  show-hidden)
			  :collect file)
		       file-list (if allow-browse
				     (append '(" [Up..]") files)
				     files)))
	       (absolutize (f)
		 "Return F as an absolute path in DIR."
		 (abspath (path-append directory (dir-entry-name f))))
	       (single-file (f)
		 "Make F be single file if you can, otherwise leave it."
		 (cond
		   ((and (consp f) (= 1 (length f))) (first f))
		   (t f)))
	       (fix-results (file)
		 "Make the file(s) absolute paths and give the directory as the
                  2nd result."
		 (when file
		   (typecase file
		     ((or list array)
		      (values (mapcar #'absolutize file) directory))
		     (t
		      (values (absolutize file) directory))))))
      (generate-list)
      (if (not allow-browse)
	  ;; Just pick from the current directory
	  (fix-results (call-next-method pick :list file-list))
	  ;; Allow directory browsing.
	  (progn
	    (loop :with done = nil :and f
	       :while (not done)
	       :do
		 (setf quit-flag nil
		       msg (format nil "~@[~a~%~]~a~%" message directory)
		       (values filename did-dir)
		       ;; (run pick :list file-list :message msg)
		       (call-next-method pick :list file-list :message msg)
		       f (single-file filename))
		 (cond
		   ;; picked up level
		   ((and f (equal " [Up..]" f))
		    (setf directory
			  (dirname (s+ (abspath (path-append directory ".."))
				       *directory-separator*)))
		    (when (zerop (length directory))
		      (setf directory (string *directory-separator*)))
		    (generate-list))
		   ;; picked a directory
		   ((and f
			 (pf-dir-entry-p f)
			 (or (eq (dir-entry-type f) :directory)
			     (eq (dir-entry-type f) :link))
			 (probe-directory
			  (path-append directory (dir-entry-name f)))
			 (and (not pick-directories) (not did-dir)
			      (not (directory-accepted pick))))
		    (setf directory (path-append directory (dir-entry-name f)))
		    (generate-list))
		   ;; other files
		   (t
		    (setf done t))))
	    (fix-results filename)))))))

(defgeneric accept-file-or-directory (i)
  (:documentation "Accept a file or directory."))

(defmethod accept-file-or-directory ((i file-picker))
  (accept i)
  (setf (directory-accepted i) t))

(defkeymap *pick-file-keymap* (#|:default-binding #'default-action|#)
  `((#\newline		  . accept-file-or-directory)))

(defmethod view-item ((pick file-picker))
  (with-slots (items point) pick
    ;; @@@ add directory
    (view-this-item pick (cdr (elt items point)))))

(defun pick-file (&key message (directory ".") (allow-browse t) show-hidden
		    pick-directories multiple)
  "Have the user choose a file."
  (let ((*pick* (make-instance
		 'file-picker
		 :message message
		 :directory directory
		 :allow-browse allow-browse
		 :show-hidden show-hidden
		 :pick-directories pick-directories
		 :multiple multiple
		 :sort-p t
		 :typing-searches t
		 :local-keymap (list *pick-file-keymap* *pick-list-keymap*))))
    (run *pick*)))

(defun pick-files (&key message (directory ".") (allow-browse t) show-hidden
		    (pick-directories))
  "Choose some files. Just a shortcut for calling pick-file with MULTIPLE = T."
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (pick-file :message message :directory directory :allow-browse allow-browse
	     :show-hidden show-hidden :pick-directories pick-directories
	     :multiple t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *menu-doc*
"Perform an action from a menu. Arguments are:
 ‘menu’           An alist of (item . action) where ‘item’ is something to
                  print, as with princ, and ‘action’ is a function to call.
 ‘message’        Some text to display before the menu.
 ‘selected-item’  The item that is initially selected.
 ‘popup’          True to make it a pop-up menu, in which case X and Y can be
                  provided.
 ‘keyamp’         A custom keymap.
 ‘type’           Class of the menu."))

(defun do-menu* (menu &key message selected-item popup (x 0) (y 0) keymap type)
  #.*menu-doc*
  (multiple-value-bind (items funcs)
      (loop :for (i . f) :in menu
	 :collect i :into items
	 :collect f :into funcs
	 :finally (return (values items funcs)))
    (let* ((n (pick-list items :by-index t
			       :message message
			       :selected-item selected-item
			       :popup popup
			       :keymap keymap
			       :x x :y y
			       :type type))
	   (code (if n (elt funcs n))))
      (if code
	  (eval code)
	  :quit))))

(defmacro do-menu (menu &key message selected-item popup (x 0) (y 0) keymap)
  #.(s+ *menu-doc*
	"This is a macro so you can use lexically scoped things in the menu.")
  (cond
    ((symbolp menu)
     `(do-menu* ,menu :message ,message :selected-item ,selected-item
		:popup ,popup :x ,x :y ,y :keymap ,keymap))
    ((listp menu)
     (multiple-value-bind (items funcs)
	 (loop :for (i . f) :in menu
	    :collect i :into items
	    :collect f :into funcs
	    :finally (return (values items funcs)))
       (let ((n-sym (gensym)))
	 `(block menu			; ! anaphoric or un-hygenic ?
	    (let* ((,n-sym (pick-list ',items :by-index t
				      :message ,message
				      :selected-item ,selected-item
				      :popup ,popup :x ,x :y ,y
				      :keymap ,keymap)))
	      (if ,n-sym
		  (case ,n-sym
		    ,@(loop :for i :from 0 :below (length funcs)
;;; Why did I do this?
;;;		 :collect (list i (let ((f (elt funcs i)))
;;;				    (if (listp f) (car f))))))
			 :collect (list i (elt funcs i))))
		  :quit))))))
    (t
     (error "MENU must be a symbol or a list."))))

;; EOF
