;;
;; char-picker.lisp - Pick characters that may be otherwise hard to type.
;;

(defpackage :char-picker
  (:documentation "Pick characters that may be otherwise hard to type.")
  (:use :cl :dlib :stretchy :char-util :dlib-misc :keymap :inator
	:terminal :terminal-inator)
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

(defclass char-picker (terminal-inator)
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
   (search-start
    :initarg :search-start :accessor char-picker-search-start
    :initform 0 :type integer
    :documentation "Where the search started.")
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

(defun write-special-char (c)
  (ctypecase c
    (integer (tt-write-char (code-char c)))
    (character (tt-write-char c))
    (string (tt-write-string c))))

;; @@@ What should I do when the screen is so tall I run out of letters?
(defun show-chars (start inc &optional search-string)
  "Display code for character picking. Show the INC characters starting from
*pick-char-start*."
  ;;(clear)
  (tt-move-to 0 0)
  (tt-erase-to-eol)
  (tt-format "~d - ~d" start (+ start inc))
  (let* ((end (+ start inc))
	 cc name ss-pos ss-end)
    (loop
       :for i :from start :to end
       :for l fixnum = 0 :then (+ 1 l)
       :do
       (tt-move-to (+ 2 l) 0)
       (tt-erase-to-eol)
       (when (< i (1- char-code-limit))
	 (tt-format "~c: (~4,'0x) "
			 (if (< l (length *letters*))
			     (aref *letters* l)
			     #\?)
			 (+ start l))
	 (setf cc (code-char i)
	       name (or (char-name (code-char i)) ""))
	 (write-special-char (if (control-char-p cc)
				 (displayable-char cc :all-control t)
				 cc))
	 (if (and search-string
		  (setf ss-pos (search search-string name :test #'equalp)))
	     (progn
	       (setf ss-end (+ ss-pos (length search-string)))
	       (tt-write-char #\space)
	       (tt-write-string (subseq name 0 ss-pos))
	       (tt-inverse t)
	       (tt-write-string (subseq name ss-pos ss-end))
	       (tt-inverse nil) 
	       (tt-write-string (subseq name ss-end)))
	     (tt-format " ~a" name))))))

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
  (with-slots (start view-size saved-start searching search-start direction
	       failed input result) i
    (setf start *pick-char-start*
	  view-size (- (tt-height) 4)
	  saved-start start
	  searching nil
	  search-start start
	  direction :forward
	  failed nil
	  input nil
	  result nil)
    (tt-clear)))

(defun show-prompt (i prompt)
  (with-slots (direction) i
    (tt-move-to 1 0) (tt-erase-to-eol)
    (when (eql direction :backward)
      (tt-write-string "Reverse "))
    (tt-write-string "I-Search: ")
    (tt-write-string prompt)))

(defun erase-prompt ()
  (tt-move-to 1 0) (tt-erase-to-eol))

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
    (if searching
	(show-prompt i search-string)
	(erase-prompt))
    (tt-finish-output)))

(defmethod await-event ((i char-picker))
  "Char picker input."
  (setf (char-picker-input i) (tt-get-char))
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (char-picker-input i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun backspace-command (i)
  "Delete the last character of the search string."
  (with-slots (searching search-string failed) i
    (when (and searching (> (fill-pointer search-string) 0))
      (setf (fill-pointer search-string) (1- (fill-pointer search-string))
	    failed nil))))

(defun clear-line-command (i)
  "Clear the search string."
  (when (@ i searching)
    (setf (fill-pointer (@ i search-string)) 0
	  (@ i failed) nil)))

(defun enter-command (i)
  "Quit and return the top character as the result."
  (with-slots (result searching failed start) i
    (if searching
	(setf searching nil
	      failed nil)
	(setf result (code-char start)
	      *pick-char-start* start
	      (@ i inator::quit-flag) t))))

(defmethod search-command ((i char-picker))
  "Search forward."
  (with-slots (searching search-start start direction failed) i
    (if searching
	(incf start)
	(setf search-start start))
    (setf direction :forward
	  searching t
	  failed nil)))

(defun reverse-search-command (i)
  "Search backwards."
  (with-slots (searching search-start start direction failed) i
    (if searching
	(when (> start 0)
	  (decf start))
	(setf search-start start))
    (setf direction :backward
	  searching t
	  failed nil)))

(defmethod quit ((i char-picker))
  "Exit the character picker or exit search mode."
  (with-slots (start searching failed search-start result saved-start) i
    (if searching
	(setf searching nil
	      failed t
	      start search-start)
	(setf (inator-quit-flag i) t
	      result (code-char saved-start)))))

(defmethod next ((i char-picker))
  "Next character."
  (with-slots (start failed) i
    (when (< start (1- char-code-limit))
      (incf start) (setf failed t))))

(defmethod previous ((i char-picker))
  "Previous character."
  (with-slots (start failed) i
    (when (> start 0)
      (decf start))
    (setf failed t)))

(defmethod next-page ((i char-picker))
  "Next page."
  (with-slots (start failed view-size) i
    (incf start view-size) (setf failed t)))

(defmethod previous-page ((i char-picker))
  "Previous page."
  (with-slots (start failed view-size) i
    (decf start view-size)
    (setf failed t)))

(defmethod move-to-top ((i char-picker))
  "Go to the first character."
  (with-slots (start failed) i
    (setf start 0 failed t)))

(defmethod move-to-bottom ((i char-picker))
  "Go to the last character."
  (with-slots (start view-size) i
    (setf start (- char-code-limit view-size 1))))

(defmethod move-to-beginning ((i char-picker))
  "Go to the first character."
  (move-to-top i))

(defmethod move-to-end ((i char-picker))
  "Go to the last character."
  (move-to-bottom i)
  (setf (char-picker-failed i) t))

;; (defmethod previous-page ((i char-picker))
;;   (with-slots (start view-size) i
;;     (decf start view-size)))

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

(defun enter-char-number (i)
  "Jump to a character code."
  (with-slots (start) i
    (tt-move-to 1 0) (tt-erase-to-eol)
    (let ((result
	   ;;(with-terminal (:curses)
	   (rl:rl :prompt "Character number: "))
	  number)
      (tt-finish-output)
      (if (not (ignore-errors (setf number (parse-integer-with-radix result))))
	  (progn
	    (tt-move-to 1 0)
	    (tt-erase-to-eol)
	    (tt-write-string "That didn't seem like a number."))
	  (if (and (integerp number) (> number 0) (< number char-code-limit))
	      (setf start number)
	      (progn
		(tt-move-to 1 0)
		(tt-erase-to-eol)
		(tt-write-string
		 "That didn't seem like a valid character code.")))))))

(defmethod jump-command ((i char-picker))
  "Jump to a character code."
  (enter-char-number i))

(defkeymap *char-picker-keymap*
  "Keymap for the character picker."
  `((#\backspace	. backspace-command)
    (#\rubout		. backspace-command)
    (:backspace		. backspace-command)
    (,(ctrl #\U)	. clear-line-command)
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
    (#\=		. enter-char-number)
    ))

;; Use

(defun char-picker ()
  (with-terminal ()
    (let ((p (make-instance 'char-picker
			    :keymap (list *char-picker-keymap*
					  *default-inator-keymap*))))
      (event-loop p)
      (char-picker-result p))))

#+lish
(lish:defcommand char-picker ()
  "Pick a character."
  (let ((c (char-picker)))
    (format t "~c #x~x ~a~%" c (char-code c) (char-name c))))

;; EOF
