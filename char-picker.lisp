;;
;; char-picker.lisp - Pick characters that may be otherwise hard to type.
;;

(defpackage :char-picker
  (:documentation "Pick characters that may be otherwise hard to type.")
  (:use :cl :stretchy :char-util :keymap :curses :inator :fui)
  (:export
   #:char-picker
   #:!char-picker
   ))
(in-package :char-picker)

(defparameter *letters*
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Letters for character selection.")

(defvar *pick-char-start* 0
  "Saved character to start with for character picking subsystem.")

(defclass char-picker (fui-inator)
  ((start
    :initarg :start :accessor char-picker-start
    :initform 0 :type integer
    :documentation "Starting character code.")
   (saved-start
    :initarg :saved-start :accessor char-picker-saved-start
    :initform 0 :type integer
    :documentation "Saved starting character code.")
   (result
    :initarg :result :accessor char-picker-result
    :initform nil :type (or null character)
    :documentation "The character picked.")
   (searching
    :initarg :searching :accessor char-picker-searching
    :initform nil :type boolean
    :documentation "True if we are searching.")
   (search-string
    :initarg :search-string :accessor char-picker-search-string
    :initform (make-stretchy-string 20) :type string
    :documentation "Adjustable string we are searching for.")
   (direction
    :initarg :direction :accessor char-picker-direction
    :initform :forward :type (member :forward :backward)
    :documentation "Which direction we are searching.")
   (failed
    :initarg :failed :accessor char-picker-failed
    :initform nil :type boolean
    :documentation "True if searching failed.")
   (input
    :initarg :input :accessor char-picker-input
    :documentation "Last input.")
   (view-size
    :initarg :view-size :accessor char-picker-view-size
    :initform 0 :type fixnum
    :documentation "The size of the character viewing area.")
   )
  (:documentation "Select a character."))

;; @@@ What should I do when the screen is so tall I run out of letters?
(defun show-chars (start inc &optional search-string)
  "Display code for character picking. Show the INC characters starting from
*pick-char-start*."
  ;;(clear)
  (move 0 0)
  (clrtoeol)
  (addstr (format nil "~d - ~d" start (+ start inc)))
  (let* ((end (+ start inc)))
    (loop :for i :from start :to end
       :for l fixnum = 0 :then (+ 1 l)
       :do
       (move (+ 2 l) 0)
       (clrtoeol)
       (addstr (format nil "~c: (~4,'0x) "
		       (if (< l (length *letters*))
			   (aref *letters* l)
			   #\?)
		       (+ start l)))
       (add-char (code-char i))
       (let ((name (or (char-name (code-char i)) ""))
	     pos end)
	 (if (and search-string
		  (setf pos (search search-string name :test #'equalp)))
	     (progn
	       (setf end (+ pos (length search-string)))
	       (addch (char-code #\space))
	       (addstr (subseq name 0 pos))
	       (standout)
	       (addstr (subseq name pos end))
	       (standend)
	       (addstr (subseq name end)))
	     (addstr (format nil " ~a" name)))))))

(defun search-char-names (start match-str &optional (direction :forward))
  "Return char code of first match of STR in the characater names,
starting at START. If not found, return START."
  (macrolet
      ((floop (&rest steps)
	 `(loop :for c ,@steps
	     :do (let* ((code  (code-char c))
			(name  (when code (char-name code)))
			(match (when name
				 (search match-str name :test #'equalp))))
		   (when match
		     (return-from search-char-names c))))))
    (ecase direction
      (:forward  (floop :from start :below char-code-limit))
      (:backward (floop :from start :downto 0))))
  nil)

(defmethod start-inator ((i char-picker))
  (call-next-method)
  (with-slots (start view-size saved-start searching direction failed input
	       result) i
    (setf start *pick-char-start*
	  view-size (- curses:*lines* 4)
	  saved-start start
	  searching nil
	  direction :forward
	  failed nil
	  input nil
	  result nil)
    (clear)))

(defun show-prompt (i prompt)
  (with-slots (direction) i
    (move 1 0) (clrtoeol)
    (when (eql direction :backward)
      (addstr "Reverse "))
    (addstr "I-Search: ")
    (addstr prompt)))

(defmethod update-display ((i char-picker))
  (with-slots (search-string failed searching direction start view-size) i
    ;;(show-prompt i search-string)
    (when (< start 0)
      (setf start 0))
    (when (and (> (length search-string) 0) (not failed) searching)
      (let ((s (search-char-names start search-string direction)))
	(if s
	    (setf start s)
	    (setf failed t))))
    (show-chars start view-size search-string)
    (when searching
      (show-prompt i search-string))
    (refresh)))

(defmethod await-event ((i char-picker))
  "Char picker input."
  (setf (char-picker-input i) (get-char))
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (char-picker-input i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun backspace-command (i)
  (with-slots (searching search-string failed) i
    (when (and searching (> (fill-pointer search-string) 0))
      (setf (fill-pointer search-string) (1- (fill-pointer search-string))
	    failed nil))))

(defun clear-line-command (i)
  (with-slots (searching search-string failed) i
    (when searching
      (setf (fill-pointer search-string) 0
	    failed nil))))

(defun enter-command (i)
  (with-slots (result start) i
    (setf result (code-char start)
	  *pick-char-start* start
	  (inator-quit-flag i) t)))

(defmethod search-command ((i char-picker))
  (with-slots (searching start direction failed) i
    (when searching
      (incf start))
    (setf direction :forward
	  searching t
	  failed nil)))

(defun reverse-search-command (i)
  (with-slots (searching start direction failed) i
    (when (and searching (> start 0))
      (decf start))
    (setf direction :backward
	  searching t
	  failed nil)))

(defmethod quit ((i char-picker))
  (setf (inator-quit-flag i) t
	(char-picker-result i) (code-char (char-picker-saved-start i))))

(defmethod next ((i char-picker))
  (with-slots (start failed) i
    (incf start) (setf failed t)))

(defmethod previous ((i char-picker))
  (with-slots (start failed) i
    (decf start)
    (setf failed t)))

(defmethod next-page ((i char-picker))
  (with-slots (start failed view-size) i
    (incf start view-size) (setf failed t)))

(defmethod previous-page ((i char-picker))
  (with-slots (start failed view-size) i
    (decf start view-size)
    (setf failed t)))

(defmethod move-to-top ((i char-picker))
  (with-slots (start failed) i
    (setf start 0 failed t)))

(defmethod move-to-bottom ((i char-picker))
  (with-slots (start view-size) i
    (setf start (- char-code-limit view-size 1))))

(defmethod move-to-beginning ((i char-picker))
  (move-to-top i))

(defmethod move-to-end ((i char-picker))
  (move-to-bottom i)
  (setf (char-picker-failed i) t))

(defmethod previous-page ((i char-picker))
  (with-slots (start view-size) i
    (decf start view-size)))

(defmethod default-action ((i char-picker))
  "Use a character based on it's on screen letter tag."
  (with-slots (input searching search-string start result) i
    (when (and (characterp input) (graphic-char-p input))
      (if searching
	  (stretchy-append search-string input)
	  (let ((p (position input *letters*)))
	    (when p
	      (setf input (code-char (+ start p)))
	      (setf result input
		    *pick-char-start* start
		    (inator-quit-flag i) t)))))))

(defkeymap *char-picker-keymap*
  "Keymap for the character picker."
  `((#\backspace	. backspace-command)
    (#\rubout		. backspace-command)
    (,(ctrl #\U)	. clear-line-commmand)
    (#\return		. enter-command)
    (#\newline		. enter-command)
    (,(ctrl #\S)	. search-command)
    (#\/		. search-command)
    (,(ctrl #\R)	. reverse-search-command)
    (,(ctrl #\C)	. quit)
    (,(ctrl #\G)	. quit)
    (:down		. next)
    (:up		. previous)
    (:npage		. next-page)
    (,(ctrl #\F)	. next-page)
    (:ppage		. previous-page)
    (,(ctrl #\B)	. previous-page)
    (#\<		. move-to-top)
    (#\>		. move-to-bottom)
    (#\?		. help)
    ))
  

;; Use

(defun char-picker ()
  (let ((p (make-instance 'char-picker
			  :keymap (list *char-picker-keymap*
					*default-inator-keymap*))))
    (event-loop p)
    (char-picker-result p)))

#+lish
(lish:defcommand char-picker ()
  "Pick a character."
  (let ((c (char-picker)))
    (format t "~c #x~x ~a~%" c (char-code c) (char-name c))))


;; EOF
