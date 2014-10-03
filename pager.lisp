;;
;; pager.lisp - Something like more or less
;;

;; $Revision: 1.9 $

;; TODO:
;;  - syntax highlighting

(defpackage :pager
  (:documentation "We can only see so much at one time.")
  (:use :cl :dlib :dlib-misc :curses :opsys :fui :stretchy :keymap :char-util)
  (:export
   #:*pager-prompt*
   #:page
   ;; Main entry points
   #:with-pager
   #:with-pager*
   #:pager
   #:browse
   #+lish #:!pager
   ))
(in-package :pager)

(define-constant +digits+ #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  "For reading the numeric argument." #'equalp)

;; Since *prompt* is taken on some implementations.
(defvar *pager-prompt* "line %l of %L"
  "The current default prompt.")

(defstruct line
  "Hold a line of text."
  number				; line number
  position				; file-position
  text					; text string of the line
)

(defclass pager ()
  ((stream
    :initarg :stream :accessor pager-stream
    :documentation "Input stream")
   (lines
    :initarg :lines :accessor pager-lines :initform '()
    :documentation "List of lines")
   (count
    :initarg :count :accessor pager-count :initform 0
    :documentation "Number of lines")
   (line
    :initarg :line :accessor pager-line :initform 0
    :documentation "Current line at the top of the screen")
   (left
    :initarg :left :accessor pager-left :initform 0
    :documentation "Column which display starts at")
   (max-width
    :initarg :max-width :accessor pager-max-width :initform 0
    :documentation "maximum line length")
   (page-size
    :initarg :page-size :accessor pager-page-size
    :documentation "how many lines in a page")
   (got-eof
    :initarg :got-eof :accessor pager-got-eof :initform nil
    :documentation "true if we got to the end of stream")
   (search-string
    :initarg :search-string :accessor pager-search-string :initform nil
    :documentation "if non-nil search for and highlight")
   (file-list
    :initarg :file-list :accessor pager-file-list :initform nil
    :documentation "list of files to display")
   (file-index
    :initarg :file-index :accessor pager-file-index :initform nil
    :documentation "where we are in the file list")
   (message
    :initarg :message :accessor pager-message :initform nil
    :documentation "Message to display until next input")
   (prefix-arg
    :initarg :prefix-arg :accessor pager-prefix-arg :initform nil
    :documentation "Common numeric argument for commands")
   (quit-flag
    :initarg :quit-flag :accessor pager-quit-flag :initform nil
    :documentation "true to quit the pager")
   (input-char
    :initarg :input-char :accessor pager-input-char :initform nil
    :documentation "the current input character")
   (command
    :initarg :command :accessor pager-command :initform nil
    :documentation "the current command")
   (last-command
    :initarg :last-command :accessor pager-last-command :initform nil
    :documentation "the previous command")
   ;; options
   (show-line-numbers
    :initarg :show-line-numbers :accessor pager-show-line-numbers :initform nil
    :documentation "-l"))
  (:documentation "An instance of a pager."))

(defmethod initialize-instance
    :after ((o pager) &rest initargs &key &allow-other-keys)
  "Set up the slots in a pager."
  (declare (ignore initargs))
  ;; slot initialization, such as:
  (when (not (slot-boundp o 'page-size))
    (setf (pager-page-size o) (1- curses:*lines*))))

(defgeneric freshen (o)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized, but perhaps reuse some resources."))

(defmethod freshen ((o pager))
  (setf (pager-stream o) nil
	(pager-lines o) nil
	(pager-count o) 0
	(pager-line o) 0
	(pager-left o) 0
	(pager-max-width o) 0
;	(pager-page-size o) 0
	(pager-got-eof o) nil
	(pager-search-string o) nil
;	(pager-file-list o) nil
;	(pager-index o) nil
	(pager-message o) nil
	(pager-prefix-arg o) nil
	(pager-quit-flag o) nil
	(pager-input-char o) nil
	(pager-command o) nil
	(pager-last-command o) nil)
  ;; Don't reset options?
  )

(defun read-lines (pager count)
  "Read new lines from the stream. Stop after COUNT lines. If COUNT is zero, read until we get an EOF."
  (when (not (pager-got-eof pager))
    (let ((n 0)
	  (i (pager-count pager))
	  (line nil)
	  (lines '()))
      (loop
	 :while (and (or (< n count) (zerop count))
		     (setf line
			   (resilient-read-line (pager-stream pager) nil nil)))
	 :do (push (make-line :number i
			      :position (file-position (pager-stream pager))
			      :text line)
		   lines)
	 (incf i)
	 (incf n))
      (when (not line)
	(setf (pager-got-eof pager) t))
      (setf (pager-lines pager) (nconc (pager-lines pager) (nreverse lines))
	    (pager-count pager) i))))

(defun format-prompt (pager &optional (prompt *pager-prompt*))
  "Return the prompt string with a few less-like formatting character
replacements. So far we support:
  %b / %B  Bytes at current postion / Total bytes
  %l / %L  Current line / Maximum line
  %p       Percentage line
  %f       File name
"
  (with-output-to-string (str)
    (loop :with c :for i :from 0 :below (length prompt) :do
       (setf c (aref prompt i))
       (if (equal c #\%)
         (progn
	   (incf i)
	   (when (< i (length prompt))
	     (setf c (aref prompt i))
	     (case c
	       ;; @@@ The next two are very inefficient and probably
	       ;; should be cached.
	       (#\b
		(let ((l (nth (pager-line pager) (pager-lines pager))))
		  (princ (if l (line-position l) "?") str)))
	       (#\B
		(let ((l (ignore-errors (file-length (pager-stream pager)))))
		  (princ (or l "?") str)))
	       (#\l (princ (pager-line pager) str))
	       (#\L (princ (pager-count pager) str))
	       (#\p (princ (round (/ (* (pager-lines pager) 100)
				     (pager-count pager)))
			   str))
	       (#\f
		(if (typep (pager-stream pager) 'file-stream)
		    (princ (namestring (truename (pager-stream pager))) str)
		    (format str "~a" (pager-stream pager)))))))
	 (write-char c str)))))

(defun display-prompt (pager)
  "Put the prompt at the appropriate place on the screen."
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (standout)
  (addstr (format-prompt pager))
  (standend))

(defun tmp-message (format-string &rest args)
  "Print a formatted message at the last line."
  (standout)
  (move (1- *lines*) 0)
  (clrtoeol)
  (addstr (apply #'format nil format-string args))
  (standend)
  (refresh))

(defun message (format-string &rest args)
  "Print a formatted message at the last line and pause until a key is hit."
  (apply #'tmp-message format-string args)
  (fui:get-char))

(defvar *nibby-color-scheme*
  '((default		:green)
    (comment		:cyan)
    (string		:white)
    (keyword		:magenta)
    (function-name	:red)
    (variable-name	'(:bold :yellow))
    (documentation	'(:bold :green))
    (constant		'(:bold :white))
    (type-name		:yellow))
  "A dark background color scheme that Nibby likes.")

(defun syntax-lisp ()
  )

(defun syntax-c ()
  )

(defun pick-sytax (filename)
  (let ((type (string-downcase (pathname-type filename))))
    (case type
      ("lisp"		#'syntax-lisp)
      (("c" "h")	#'syntax-c))))

(defvar *pager*) ;; @@@ DEBUG

(defun display-page (pager)
  "Display the lines already read, starting from the current."
  (move 0 0)
  (clear)
  (setf *pager* pager) ;; @@@ DEBUG
  (with-slots (line left page-size) pager
    (let ((y 0))
      (loop
	 :with l = (nthcdr line (pager-lines pager))
	 :for i :from line :to (+ line (1- page-size))
	 :while (car l)
	 :do
	 (let* ((text1 (line-text (car l)))
		(end   (min (length text1) (+ *cols* left)))
		(text  (if (> left 0)
			   (if (< left (length text1))
			       (subseq text1 left end)
			       "")
			   (subseq text1 0 end))))
	   (let* ((ss (pager-search-string pager))
		  (pos (search ss text :test #'equalp))
		  old-pos)
	     (if (and ss pos)
	       (progn
		 (move y 0)
		 (when (pager-show-line-numbers pager)
		   (addstr (format nil "~d: " i)))
		 (loop :with start = 0
		    :do
		    (addstr (subseq text start pos))
		    (standout)
		    (addstr (subseq text pos (+ pos (length ss))))
		    (standend)
		    (setf start (+ pos (length ss))
			  old-pos pos
			  pos (search ss text :test #'equalp :start2 start))
		    (when (not pos)
		      (addstr (subseq text (+ old-pos (length ss)))))
		    :while pos))
	       (if (pager-show-line-numbers pager)
		 (mvaddstr y 0 (format nil "~d: ~a" i text))
		 (mvaddstr y 0 text)))))
	 (setf l (cdr l))
	 (incf y))
      ;; Fill the rest of the screen with twiddles to indicate emptiness.
      (when (< y page-size)
	(loop :for i :from y :below page-size
	   :do (mvaddstr i 0 "~"))))))

(defun ask (&optional prompt)
  (let ((str (make-stretchy-string 10))
	(esc-count 0))
    (loop :with c :and done
       :do
       (move (1- curses:*lines*) 0)
       (clrtoeol)
       (addstr prompt) (addstr str)
       (refresh)
       (setf c (fui:get-char))
       (case c
	 (#\escape
	  (when (> (incf esc-count) 1)
	    (return-from ask nil)))
	 (#\^G
	  (return-from ask nil))
	 ((#\return #\newline)
	  (setf done t))
	 ((#\backspace #\rubout :backspace :delete)
	  (when (> (fill-pointer str) 0)
	    (decf (fill-pointer str))))
	 (#\^U
	  (setf (fill-pointer str) 0))
	 (t
	  (when (characterp c)
	    (stretchy-append str c))))
       :while (not done))
    str))

(defun search-for (pager str)
  (with-slots (lines count line page-size) pager
    (let ((ll (nthcdr line lines)) l)
      (loop
	 :do (setf l (car ll))
	 :while (and l (< (line-number l) count)
		     (not (search str (line-text l) :test #'equalp)))
	 :do
	 (when (>= (line-number l) (max 0 (- count page-size)))
	   (read-lines pager page-size))
	 (setf ll (cdr ll)))
      (if (and l (< (line-number l) count))
	  (setf line (line-number l))
	  nil))))

(defun set-option (pager)
  "Set a pager option. Propmpts for what option to toggle."
  (let ((char (fui:get-char)))
    (case char
      ((#\l #\L)
       (setf (pager-show-line-numbers pager)
	     (not (pager-show-line-numbers pager)))
       (message "show-line-numbers is ~:[Off~;On~]"
		(pager-show-line-numbers pager))))))

(defun next-file (pager)
  "Go to the next file in the set of files."
  (with-slots (count lines line got-eof file-list file-index stream
	       page-size) pager
    (if (and file-index (< file-index (length file-list)))
	(progn
	  (incf file-index)
	  (close stream)
	  (setf stream (open (nth file-index file-list) :direction :input))
	  (setf lines '() count 0 line 0 got-eof nil)
	  (read-lines pager page-size))
	(message "No next file."))))

(defun previous-file (pager)
  "Go to the previous file in the set of files."
  (with-slots (count lines line got-eof file-list file-index stream
	       page-size) pager
    (if (and file-index (> file-index 0))
	(progn
	  (decf file-index)
	  (close stream)
	  (setf stream (open (nth file-index file-list) :direction :input))
	  (setf lines '() count 0 line 0 got-eof nil)
	  (read-lines pager page-size))
	(message "No previous file."))))

(defun quit (pager)
  "Exit the pager."
  (setf (pager-quit-flag pager) t))

(defun next-page (pager)
  "Display the next page."
  (with-slots (line page-size count) pager
    (incf line page-size)
    (when (> (+ line page-size) count)
      (read-lines pager page-size))))

(defun next-line (pager)
  "Display the next line."
  (with-slots (line page-size count prefix-arg) pager
    (let ((n (or prefix-arg 1)))
      (incf line n)
      (if (> (+ line page-size) count)
	  (read-lines pager n)))))

(defun previous-page (pager)
  "Display the previous page."
  (with-slots (line page-size) pager
    (setf line (max 0 (- line page-size)))))

(defun previous-line (pager)
  "Display the previous line."
  (with-slots (prefix-arg line) pager
    (let ((n (or prefix-arg 1)))
      (setf line (max 0 (- line n))))))

(defun scroll-right (pager)
  "Scroll the pager window to the right."
  (with-slots (left prefix-arg) pager
    (incf left (or prefix-arg 10))))

(defun scroll-left (pager)
  "Scroll the pager window to the left."
  (with-slots (left prefix-arg) pager
    (setf left (max 0 (- left (or prefix-arg 10))))))

(defun scroll-beginning (pager)
  "Scroll the pager window to the leftmost edge."
  (setf (pager-left pager) 0))

(defun scroll-end (pager)
  "Scroll the pager window to the rightmost edge of the text."
  (setf (pager-left pager) 100))

(defun go-to-beginning (pager)
  "Go to the beginning of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg count line) pager
    (if prefix-arg
	(progn
	  (read-lines pager (min 1 (- prefix-arg count)))
	  (setf line (1- prefix-arg)))
	(setf line 0))))

(defun go-to-end (pager)
  "Go to the end of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg count line page-size) pager
    (if prefix-arg
	(progn
	  (read-lines pager (min 1 (- prefix-arg count)))
	  (setf line (1- prefix-arg)))
	(progn
	  (read-lines pager 0)
	  (setf line (max 0 (- count page-size)))))))

(defun search-command (pager)
  "Search for something in the stream."
  (with-slots (search-string message) pager
    (setf search-string (ask "/"))
    (when (not (search-for pager search-string))
      (setf message "--Not found--"))))

(defun search-next (pager)
  "Search for the next occurance of the current search in the stream."
  (with-slots (line search-string message) pager
    (incf line)
    (when (not (search-for pager search-string))
      (decf line)
      (setf message "--Not found--"))))

(defun clear-search (pager)
  "Clear the search string."
  (setf (pager-search-string pager) nil))

(defun show-info (pager)
  "Show information about the stream."
  (setf (pager-message pager) "%f %l of %L %b/%B"))

(defun redraw (pager)
  "Redraw the display."
  (declare (ignore pager))
  (clear) (refresh))

(defun digit-argument (pager)
  "Accumulate digits for the PREFIX-ARG."
  (with-slots (input-char prefix-arg message) pager
    (when (and (characterp input-char) (digit-char-p input-char))
      (setf prefix-arg (+ (* 10 (or prefix-arg 0))
			  (position input-char +digits+))
	    message (format nil "Prefix arg: ~d" prefix-arg)))))

(defkeymap *help-keymap*
  `(
    (#\c		. describe-key-briefly)
    (#\k		. describe-key)
    (#\?		. more-help)
    (#\q		. quit)
    ))

(defkeymap *normal-keymap*
  `(
    (#\q		. quit)
    (#\Q		. quit)
    (#\space		. next-page)
    (:npage		. next-page)
    (#\return		. next-line)
    (:down		. next-line)
    (#\j		. next-line)
    (#\b		. previous-page)
    (:ppage		. previous-page)
    (#\rubout		. previous-page)
    (#\k		. previous-line)
    (:up		. previous-line)
    (#\l		. scroll-right)
    (:right		. scroll-right)
    (#\h		. scroll-left)
    (:left		. scroll-left)
    (#\^A		. scroll-beginning)
    (:home		. scroll-beginning)
    (#\^E		. scroll-end)
    (:end		. scroll-end)
    (#\<		. go-to-beginning)
    (#\g		. go-to-beginning)
    (#\>		. go-to-end)
    (#\G		. go-to-end)
    (#\/		. search-command)
    (#\n		. search-next)
    (#\=		. show-info)
    (#\^G		. show-info)
    (#\-		. set-option)
    (#\^L		. redraw)
    (,(meta-char #\u)	. clear-search)
    (,(meta-char #\n)	. next-file)
    (,(meta-char #\p)	. previous-file)
    (#\?		. help)
    (#\^H		. help-key)
    (,(meta-char #\=)   . describe-key-briefly)
    (,(meta-char #\=)   . describe-key)
    (#\0		. digit-argument)
    (#\1		. digit-argument)
    (#\2		. digit-argument)
    (#\3		. digit-argument)
    (#\4		. digit-argument)
    (#\5		. digit-argument)
    (#\6		. digit-argument)
    (#\7		. digit-argument)
    (#\8		. digit-argument)
    (#\9		. digit-argument)
    (#\escape		. *escape-keymap*)
    ))

(defparameter *escape-keymap* (build-escape-map *normal-keymap*))

(defun describe-key-briefly (pager)
  "Prompt for a key and say what function it invokes."
  (declare (ignore pager))
  (tmp-message "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *normal-keymap*)))
    (if action
	(message "~a is bound to ~a" (nice-char key) action)
	(message "~a is not defined" (nice-char key)))))

(defun describe-key (pager)
  "Prompt for a key and describe the function it invokes."
  (declare (ignore pager))
  (tmp-message "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *normal-keymap*)))
    (cond
      (action
       (clear) (move 0 0)
       (if (documentation action 'function)
	   (progn
	     (addstr (format nil "~(~a~): ~a~%" action (nice-char key)))
	     (addstr (justify-text
		      (documentation action 'function) :stream nil)))
	   (addstr
	    (format nil "Sorry, there's no documentation for ~a.~%" action)))
       (message ""))
      (t
       (message "~a is not defined" (nice-char key))))))

(defun help-key (pager)
  "Sub-command for help commands."
  (tmp-message "Help (? for more help): ")
  (perform-key pager (fui:get-char) *help-keymap*)
  (setf (pager-quit-flag pager) nil))

(defun more-help (pager)
  "Show more help on help commands."
  (clear)
  (move 0 0)
  (addstr "c - Print the name of function that a key performs.
k - Describe the function that a key performs.
q - Abort")
  (refresh)
  (help-key pager))			; @@@ this could infinitely recurse

(defun perform-key (pager key &optional (keymap *normal-keymap*))
  (with-slots (message command last-command) pager
    (setf last-command command
	  command (key-definition key keymap))
    (cond
      ((not command)
       (setf message (format nil "Key ~a is not bound in keymap ~w."
			     (nice-char key) keymap))
       (return-from perform-key))
      ;; a list to apply
      ((consp command)
       (if (fboundp (car command))
	   (apply (car command) (cdr command))
	   (setf message (format nil "(~S) is not defined." (car command)))))
      ;; something represted by a symbol
      ((symbolp command)
       (cond
	 ((fboundp command)		; a function
	  (funcall command pager))
	 ((keymap-p (symbol-value command)) ; a keymap
	  (perform-key pager (fui:get-char) (symbol-value command)))
	 (t				; anything else
	  (setf message
		(format nil "Key binding ~S is not a function or a keymap."
			command)))))
      ;; a function object
      ((functionp command)
       (funcall command pager))
      (t				; anything else is an error
       (error "Weird thing in keymap: ~s." command)))))

(defun page (stream &optional pager file-list &aux close-me)
  "View a stream with the pager."
  (with-curses
    (unwind-protect
      (progn
	(when (and (not stream) file-list)
	  (setf stream (open (first file-list) :direction :input)
		close-me t))
	(if (not pager)
	    (setf pager (make-instance 'pager
				       :stream stream
				       :page-size (1- curses:*lines*)
				       :file-list file-list))
	    (progn
	      (freshen pager)
	      (setf (pager-stream pager) stream)))
	(with-slots (count line page-size left search-string input-char
		     file-list file-index message prefix-arg quit-flag
		     command) pager
	  (when file-list (setf file-index 0))
	  (read-lines pager page-size)
	  (setf quit-flag nil)
	  (loop :do
	     (display-page pager)
	     (if message
		 (let ((*pager-prompt* message))
		   (display-prompt pager)
		   (setf message nil))
		 (display-prompt pager))
	     (refresh)
	     (setf input-char (fui:get-char))
	     (perform-key pager input-char)
	     (when (not (equal command 'digit-argument))
	       (setf prefix-arg nil))
	     :while (not quit-flag))))
      (when (and close-me stream)
	(close stream)))))

;; This is quite inefficient. But it's easy and it works.
(defmacro with-pager (&body body)
  "Evaluate the body with the *standard-output* going to the pager."
  `(let ((*standard-output* (make-string-output-stream)))
     (prog1 (with-simple-restart
		(continue "View the output so far in the pager anyway.")
	      (progn ,@body))
       (page (make-string-input-stream
	      (get-output-stream-string *standard-output*))))))

(defmacro with-pager* (&body body)
  "Evaluate the body with the *standard-output*, *error-output* and *trace-output* going to the pager."
  (let ((str-name (gensym "wp")))
    `(let* ((,str-name (make-string-output-stream))
	    (*standard-output* ,str-name)
	    (*error-output* ,str-name)
	    (*trace-output* ,str-name)
	    ;; This doesn't seem to work on clisp :(
	    (*debug-io* (make-two-way-stream *standard-input* ,str-name))
	    )
       (prog1 (with-simple-restart
		  (continue "View the output so far in the pager anyway.")
		(progn ,@body))
	 (page (make-string-input-stream
		(get-output-stream-string ,str-name)))))))

(defun pager (&optional (file-or-files (pick-file)))
  "View the file with the pager. Prompt for a file name if one isn't given."
  ;; Let's just say if you page nil, nothing happens.
  ;; This makes it nicer to quit from pick-file without getting an error.
  (when file-or-files
    (cond
      ((streamp file-or-files)
       (page file-or-files))
      ((consp file-or-files)
       (page nil nil file-or-files))
      (t
       (with-open-file (stream file-or-files :direction :input)
	 (page stream))))))

(defun help (pager)
  "Show help on pager key commands."
  (with-input-from-string
      (input
       (with-output-to-string (output)
	 (format output 
"You are using Dan's pager. You seem to have hit '~a' again, which gives a
summary of commands. If you want a description of what a function does,
press Control-H then 'k' then the key. Press 'q' to exit this help.
" (pager-input-char pager))
	 (keymap:dump-keymap *normal-keymap* output)
	 (princ "Press 'q' to exit this help.
" output)))
    (page input)))

(defun browse ()
  "Look at files."
  (let ((pager (make-instance 'pager))
	filename
	(directory "."))
    (loop :while (setf (values filename directory)
		       (pick-file :directory directory))
       :do (with-open-file (stream filename)
	     (page stream pager)))))

;; @@@ Run in a thread:
;;
;; (defmacro run (&whole whole &optional file-or-files)
;;   (declare (ignore file-or-files))
;;   `(apply #'pager ',(cdr whole)))

;; @@@

;; (defun cat-file (file)
;;   (read

;; (defun cat (&optional (file-or-files (pick-file)))
;;   "Output the file to *standard-output*. Prompt for a file name if one isn't given."
;;   (when file-or-files
;;     (cond
;;       ((streamp file-or-files)
;;        (page file-or-files))
;;       ((consp file-or-files)
;;        (page nil nil file-or-files))
;;       (t
;;        (with-open-file (stream file-or-files :direction :input)
;; 	 (page stream))))))

#+lish
(lish:defcommand pager (&rest files)
  '((:name files :type pathname :repeating t))
  "Look through files a screenful at a time."
  (pager files))

;; EOF
