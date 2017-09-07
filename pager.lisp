;;
;; pager.lisp - More or less like more or less.
;;

;; TODO:
;;  - sub-files for keep & filter ?
;;  - syntax highlighting
;;  - simpile HTML rendering
;;  - big stream issues?
;;    - direct read for files
;;    - option to use tmp file for pipe (disk vs mem tradeoff)
;;  - maybe convert to using terminal instead of curses?
;;  - handle when terminal wraps properly

(defpackage :pager
  (:documentation "pager - More or less like more or less.

As you may know, we can only see so much at one time. A paginator is the
traditional solution, which allows one to scroll through textual output.
The funtionality of a paginator can easily and perhaps better be done by a
text editor in read-only mode. The problem is that many text editors have
significant resource requirement and start up time, as well as being ill
suited to being at the end of a shell pipeline. The paginator is a small,
modest, and simple program which should start up quickly and be able to be
conviently used as the last step of an output pipline.

I would like this pager to evolve into a thing that can be used either as an
editor mode or a standalone program. But for the time being it acts like a
traditional Unix paginator, except that it lives in a Lisp world, which means
it should be smartly compiled to a tight executable or used from a Lisp shell.

SYNOPSIS
  pager files...                                        [shell command]
  pager &optional (file-or-files (pick-file))           [function]

The function takes a file name or a stream or a list of such.
The shell command takes any number of file names.
")
  (:use :cl :dlib :opsys :dlib-misc :table-print :curses :fui :stretchy
	:keymap :char-util :fatchar :ppcre :terminal :terminal-curses
	:pick-list :table-print)
  (:export
   #:*pager-prompt*
   #:*empty-indicator*
   #:page
   ;; Main entry points
   #:with-pager
   #:with-pager*
   #:pager
   #:resume
   #:browse
   #+lish #:!pager
   ))
(in-package :pager)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 1)
 		   (compilation-speed 2)))
;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 1)
;;		   (compilation-speed 0)))

(define-constant +digits+ #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  "For reading the numeric argument." #'equalp)

;; Since *prompt* is taken on some implementations.
(defvar *pager-prompt* "%&%f line %l of %L%&"
  "The current default prompt. Supports formatting as done by FORMAT-PROMPT.")

;; "text text text"
;; ((:tag "text") "text" (:tag "text") "text")
;; (:tag "text")
;; ("text text " (:tag "text " (:tag "text") " text") "text")

(defstruct line
  "Hold a line of text."
  number				; line number
  position				; file-position
  text)					; text string of the line

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
    :documentation "True if we got to the end of stream")
   (seekable
    :initarg :seekable :accessor pager-seekable :initform nil :type boolean
    :documentation "True if the stream is seekable.")
   (search-string
    :initarg :search-string :accessor pager-search-string :initform nil
    :documentation "If non-nil search for and highlight")
   (filter-exprs
    :initarg :filter-exprs :accessor pager-filter-exprs :initform nil
    :documentation "Don't show lines matching these regular expressions.")
   (keep-expr
    :initarg :keep-expr :accessor pager-keep-expr :initform nil
    :documentation "Show only lines matching these regular expressions.")
   (mutate-function
    :initarg :mutate-function :accessor pager-mutate-function :initform nil
    :documentation "Function to alter lines.")
   (file-list
    :initarg :file-list :accessor pager-file-list :initform nil
    :documentation "List of files or file locations to display")
   (file-index
    :initarg :file-index :accessor pager-file-index :initform nil
    :documentation "Where we are in the file list")
   (message
    :initarg :message :accessor pager-message :initform nil
    :documentation "Message to display until next input")
   (prefix-arg
    :initarg :prefix-arg :accessor pager-prefix-arg :initform nil
    :documentation "Common numeric argument for commands")
   (quit-flag
    :initarg :quit-flag :accessor pager-quit-flag :initform nil
    :documentation "True to quit the pager")
   (suspend-flag
    :initarg :suspend-flag :accessor pager-suspend-flag :initform nil
    :documentation "true to suspend the pager")
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
    :initarg :show-line-numbers :accessor pager-show-line-numbers
    :initform nil :type boolean
    :documentation "True to show line numbers along the left side.")
   (ignore-case
    :initarg :ignore-case :accessor pager-ignore-case
    :initform t :type boolean
    :documentation "True to ignore case in searches.")
   (wrap-lines
    :initarg :wrap-lines :accessor pager-wrap-lines
    :initform nil :type boolean
    :documentation "True to wrap lines at the edge of the window.")
   (raw-output
    :initarg :raw-output :accessor pager-raw-output
    :initform nil :type boolean
    :documentation "True to send raw, unprocessed output to the terminal.")
   (pass-special
    :initarg :pass-special :accessor pager-pass-special
    :initform nil :type boolean
    :documentation "True to pass special escape sequences to the terminal.")
   )
  (:documentation "An instance of a pager."))

(defvar *pager* nil
  "The current pager.")

(defmethod initialize-instance
    :after ((o pager) &rest initargs &key &allow-other-keys)
  "Set up the slots in a pager."
  (declare (ignore initargs))
  ;; slot initialization, such as:
  (when (not (slot-boundp o 'page-size))
    (setf (pager-page-size o) (1- curses:*lines*))))

(defgeneric freshen (o)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

(defmethod freshen ((o pager))
  "Make the pager like new."
  (setf (pager-stream o) nil
	(pager-lines o) nil
	(pager-count o) 0
	(pager-line o) 0
	(pager-left o) 0
	(pager-max-width o) 0
;	(pager-page-size o) 0
	(pager-got-eof o) nil
	(pager-seekable o) nil
	(pager-search-string o) nil
	(pager-filter-exprs o) nil
	(pager-keep-expr o) nil
	(pager-mutate-function o) nil
;	(pager-file-list o) nil
;	(pager-index o) nil
	(pager-message o) nil
	(pager-prefix-arg o) nil
	(pager-quit-flag o) nil
	(pager-suspend-flag o) nil
	(pager-input-char o) nil
	(pager-command o) nil
	(pager-last-command o) nil)
  ;; Don't reset options?
  )

#|
(defun nop-process-line (line)
  "A line processor that doesn't do anything."
  line)
|#

#|

(defun get-span (fat-line start)
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fat-line) 0)
    (return-from get-span fat-line))
  (let ((str (make-stretchy-string (- (length fat-line) start)))
	(attr (fatchar-attrs (aref fat-line start)))
	(i start) (span '()) last)
    (loop
       :with c :and len = (length fat-line)
       :do
       (setf c (aref fat-line i))
       (if last
	 (let ((added
		(set-difference (fatchar-attrs c) (fatchar-attrs last)))
	       (removed
		(set-difference (fatchar-attrs last) (fatchar-attrs c))))
	   (cond
	     (removed
	      (when (/= 0 (length str))
		(push str span))
	      (setf span (nreverse span))
	      (return-from get-span `(,@attr ,@span)))
	     (added
	      (when (/= 0 (length str))
		(push (copy-seq str) span))
	      (let ((s (get-span fat-line i)))
		(push s span)
		(incf i (span-length s)))
	      (stretchy-truncate str))
	     (t
	      (stretchy-append str (fatchar-c c))
	      (incf i)
	      (setf last c))))
	 (progn
	   (stretchy-append str (fatchar-c c))
	   (incf i)
	   (setf last c)))
       :while (< i len))
    (when (/= 0 (length str))
      (push str span))
    (setf span (nreverse span))
    `(,@attr ,@span)))

|#

(defmacro dumpy (&rest args)
  "Print the names and values of the arguments, like NAME=value."
  (let ((za (loop :for z :in args :collect
	       `(format t "~a=~a " ',z ,z))))
    `(progn ,@za (terpri))))

(defun process-grotty-line (line)
  "Convert from grotty typewriter sequences to fatchar strings."
  (when (= (length line) 0)
    (return-from process-grotty-line line))
  (let ((fat-line (make-stretchy-vector (+ (length line) 16)
					:element-type 'fatchar)))
    (loop
       :with i = 0 :and c :and len = (length line) :and attrs
       :do
       (if (< i (- len 2))
	 (cond
	   ;; Bold bullet
	   ((and (< i (- len 7))
		 (equal "++oo" (subseq line i (+ i 7))))
	    (setf c (code-char #x2022)) ; #\bullet •
	    (pushnew :bold attrs)
	    (incf i 6))
	   ;; Normal bullet
	   ((and (< i (- len 3))
		 (equal "+o" (subseq line i (+ i 3))))
	    (setf c (code-char #x2022)) ; #\bullet •
	    (incf i 2))
	   ;; Bold
	   ((and (char= (char line (+ i 1)) #\backspace)
		 (char= (char line (+ i 2)) (char line i)))
	    (setf c (char line i))
	    (incf i 2)
	    (pushnew :bold attrs))
	   ;; Underline
	   ((and (char= (char line i) #\_)
		 (char= (char line (+ i 1)) #\backspace))
	    (incf i 2)
	    (setf c (char line i))
	    ;; Underline & Bold
	    (when (and (< i (- len 2))
		       (char= (char line (+ i 1)) #\backspace)
		       (char= (char line (+ i 2)) (char line i)))
	      (incf i 2)
	      (pushnew :bold attrs))
	    (pushnew :underline attrs))
	   (t
	    (setf c (char line i))
	    (setf attrs nil)))
	 (progn
	   (setf attrs nil c (char line i))))
       (stretchy:stretchy-append
	fat-line
	(make-fatchar :c c :attrs attrs))
       (incf i)
       :while (< i len))
    fat-line))

;; This is quite inefficient.
(defun process-control-characters (line)
  ;; Don't bother if the line length is zero or there are no control chars.
  (when (or (= (length line) 0)
	    (not (position-if
		  (_ (let ((cc (char-code (fatchar-c _))))
		       (or (< cc (char-code #\space))
			   (= cc 127))))
		  line)))
    (return-from process-control-characters line))
  (let ((fat-line (make-stretchy-vector (+ (length line) 10)
					:element-type 'fatchar))
	fc)
    (flet ((ctrl-out (c the-char)
	     (setf fc (copy-fat-char c)
		   (fatchar-c fc) #\^)
	     (stretchy-append fat-line fc)
	     (setf fc (copy-fat-char c)
		   (fatchar-c fc) the-char)
	     (stretchy-append fat-line fc)))
      (loop :with cc
	 :for c :across line :do
	 (setf cc (char-code (fatchar-c c)))
	 (cond
	   ;; ((< cc (char-code #\space))
	   ;;  (setf fc (copy-fat-char c)
	   ;; 	(fatchar-c fc) #\^)
	   ;;  (stretchy-append fat-line fc)
	   ;;  (setf fc (copy-fat-char c)
	   ;; 	(fatchar-c fc) (code-char (+ cc (char-code #\@))))
	   ;;  (stretchy-append fat-line fc))
	   ;; ((= cc 127)
	   ;;  (setf fc (copy-fat-char c)
	   ;; 	(fatchar-c fc) #\^)
	   ;;  (stretchy-append fat-line fc)
	   ;;  (setf fc (copy-fat-char c)
	   ;; 	(fatchar-c fc) #\?)
	   ;;  (stretchy-append fat-line fc))
	   ((< cc (char-code #\space))
	    (ctrl-out c (code-char (+ cc (char-code #\@)))))
	   ((= cc 127)
	    (ctrl-out c #\?))
	   (t
	    (stretchy-append fat-line (copy-fat-char c))))))
    fat-line))

(defun process-line (line)
  "Process a line of text read from a stream."
  (let ((output (fat-string-to-span
		 (process-control-characters
		  (process-ansi-colors
		   (process-grotty-line
		    (untabify line)))))))
    (if (and (= (length output) 1) (stringp (first output)))
	(first output)
	output)))

(defun read-lines (line-count)
  "Read new lines from the stream. Stop after COUNT lines. If COUNT is zero,
read until we get an EOF."
  (with-slots (got-eof count stream raw-output) *pager*
    (when (not got-eof)
      (let ((n 0)
	    (i count)
	    (line nil)
	    (lines '())
	    (pos (or (file-position stream) 0)))
	(loop
	   :while (and (or (< n line-count) (zerop line-count))
		       (setf line
			     (resilient-read-line stream nil nil)))
	   :do (push (make-line :number i
;;;			      :position (incf pos (length line))
				:position pos
				:text (if raw-output
					  line
					  (process-line line)))
		     lines)
	   (incf pos (length line))
	   (incf i)
	   (incf n))
	(when (not line)
	  (setf got-eof t))
	(setf (pager-lines *pager*)
	      (nconc (pager-lines *pager*) (nreverse lines))
	      count i)))))

(defun format-prompt (&optional (prompt *pager-prompt*))
  "Return the prompt string with a few less-like formatting character
replacements. So far we support:
  %b / %B  Bytes at current postion / Total bytes
  %l / %L  Current line / Maximum line
  %p       Percentage line
  %f       File name
  %%       A percent character '%'.
"
  (with-slots (line lines stream count page-size filter-exprs keep-expr)
      *pager*
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
		    (let ((l (nth line lines)))
		      (princ (if l (line-position l) "?") str)))
		   (#\B
		    (let ((l (ignore-errors (file-length stream))))
		      (princ (or l "?") str)))
		   (#\l (princ line str))
		   (#\L (princ count str))
		   (#\p (princ (round
				(/ (* (min (+ line page-size) count) 100)
				   count))
			       str))
		   (#\% (princ #\% str))
		   (#\f
		    (if (and (typep stream 'file-stream)
			     (ignore-errors (truename stream)))
			(princ (namestring (truename stream)) str)
			(format str "~a" stream)))
		   (#\&
		    (when filter-exprs (princ " && " str))
		    (when keep-expr    (princ " ^^ " str))))))
	     (write-char c str))))))

(defun display-prompt ()
  "Put the prompt at the appropriate place on the screen."
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (standout)
  (addstr (format-prompt))
  (standend))

(defun message (format-string &rest args)
  "Display a formatted message at the last line immediately."
  (standout)
  (move (1- *lines*) 0)
  (clrtoeol)
  (addstr (apply #'format nil format-string args))
  (standend)
  (refresh))

(defun message-pause (format-string &rest args)
  "Print a formatted message at the last line and pause until a key is hit."
  (apply #'message format-string args)
  (tt-get-char))

(defun message-pause-for (timeout format-string &rest args)
  "Print a formatted message at the last line and pause until a key is hit.
Wait for TIMEOUT milliseconds before returning an error (-1).
This assumes there is no timeout (i.e. timeout is -1) before this is called,
and resets it to be so afterward."
  (apply #'message format-string args)
  (let (c)
    (unwind-protect
      (progn
	(curses::timeout timeout)
	(setf c (tt-get-char)))
      (curses::timeout -1))
    c))

(defun tmp-message (format-string &rest args)
  "Display a message at next command loop, until next input."
  (setf (pager-message *pager*) (apply #'format nil format-string args)))

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

(defparameter *attr*
  `((:normal	. ,+a-normal+)
    (:standout	. ,+a-standout+)
    (:underline	. ,+a-underline+)
    (:reverse	. ,+a-reverse+)
    (:blink	. ,+a-blink+)
    (:dim	. ,+a-dim+)
    (:bold	. ,+a-bold+)))

(defun real-attr (a)
  "Return a curses attribute given a keyword attribute."
  (or (and a (cdr (assoc a *attr*))) +a-normal+))

#|
             left         *cols*
win  :         |------------|
line : |----||-------||---------||---|
                                 |
                                *col*
|#

;; "text text text"
;; ((:tag "text") "text" (:tag "text") "text")
;; (:tag "text")
;; ("text text " (:tag "text " (:tag "text") " text") "text")

(defvar *fat-buf* (make-array 100
			      :element-type 'fatchar
			      :initial-element (make-fatchar)
			      :fill-pointer 0 :adjustable t))
(defvar *left*)
(defvar *col*)
(defun flatten-span (span)
  "Make a fat string from a span."
  (setf (fill-pointer *fat-buf*) 0)
  (let (fg bg attrs #|(col *col*)|#)
    (labels
	((spanky (s)
	   (when s
	     (typecase s
	       (string
		(loop :for c :across s :do
		   (when (and (>= *col* *left*)
			      (or (pager-wrap-lines *pager*)
				  (< (- *col* *left*) *cols*)))
		     (vector-push-extend
		      (make-fatchar :c c :fg fg :bg bg :attrs attrs)
		      *fat-buf*))
		   (incf *col*)))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f)))
		  (if tag
		      (progn
			(push tag attrs)
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(spanky (cdr s))
			(pop attrs))
		      (progn
			(spanky f)
			(spanky (cdr s))))))))))
      (spanky span))))

(defun search-a-matize ()
  "Highlight search strings in *fat-buf*."
  (with-slots (search-string ignore-case) *pager*
    (let* ((s (fat-string-to-string *fat-buf*))
	   (ss (if (and search-string ignore-case)
		   (s+ "(?i)" search-string)
		   search-string))
	   (matches (and ss (ppcre:all-matches ss s))))
      (loop :with i = 0
	 :while (< i (length matches)) :do
	 (loop :for j :from (elt matches i) :below (elt matches (1+ i))
	    :do (pushnew :standout (fatchar-attrs (aref *fat-buf* j))))
	 (incf i 2)))))

(defun show-span (s)
  (when (not s)
    (return-from show-span 0))
;;;  (flatten-span pager s)
  (with-slots (wrap-lines search-string) *pager*
    (if wrap-lines
	(span-to-fat-string s :fat-string *fat-buf* :start *left*)
	(span-to-fat-string s :fat-string *fat-buf* :start *left*
			    :end (+ *left* *cols*)))
    (when search-string
      (search-a-matize))
    (let ((line-count 1)
	  (screen-col 0)
	  (screen-line (getcury *stdscr*)))
      (loop :with last-attr
	 :for c :across *fat-buf* :do
	 (when (not (equal last-attr (fatchar-attrs c)))
	   (when last-attr (attrset 0))
	   (mapcan (_ (attron (real-attr _))) (fatchar-attrs c)))
	 (color-set (color-index
		     (or (color-number (fatchar-fg c)) +color-white+)
		     (or (color-number (fatchar-bg c)) +color-black+))
		    (cffi:null-pointer))
;;;       (format t "~a ~a" (fatchar-fg c) (fatchar-bg c))
	 (add-char (fatchar-c c))
	 (incf screen-col)
	 (when (and wrap-lines (>= screen-col *cols*))
	   (incf screen-line)
	   (setf screen-col 0)
	   (move screen-line screen-col)
	   (incf line-count))
	 (setf last-attr (fatchar-attrs c)))
      (attrset 0)
      line-count)))

(defun render-span (line-number line)
  (with-slots (left show-line-numbers) *pager*
    (let ((*col* 0) (*left* left))
      (when show-line-numbers
	(let ((str (format nil "~d: " line-number)))
	  (incf *col* (length str))
	  (addstr str)))
      (show-span line))))

(defun span-matches (expr span)
  "Return true if the plain text of a SPAN matches the EXPR."
  ;; This is probably wasteful and slow.
  (let ((line
	 (with-output-to-string (str)
	   (labels ((glom-span (s)
		      ;;(format t "glomming ~s~%" s)
		      (typecase s
			(string (princ s str))
			(list
			 (loop :for e :in s :do (glom-span e)))
			(symbol #| ignore |# )
			(t (princ s str))))) ; probably a bit too DWIM-ish
	     (glom-span span)))))
    ;;(format t "line = ~s~%" line)
    (and (ppcre:all-matches expr line) t)))

(defun filter-this (line)
  "Return true if we should filter this line."
  (with-slots (filter-exprs keep-expr) *pager*
    (when (or filter-exprs keep-expr)
      (typecase (line-text line)
	(string
	 (when (and keep-expr
		    (not (ppcre:all-matches keep-expr (line-text line))))
	   (return-from filter-this t))
	 (loop :for e :in filter-exprs
	    :if (ppcre:all-matches e (line-text line))
	    :return t))
	(list
	 (when (and keep-expr
		    (not (span-matches keep-expr (line-text line))))
	   (return-from filter-this t))
	 (loop :for e :in filter-exprs
	    :if (span-matches e (line-text line))
	    :return t))
	(t (error "Don't know how to filter a line of ~s~%"
		  (type-of (line-text line))))))))

(defun display-line (line-number line)
  (typecase (line-text line)
    (string (render-span line-number (list (line-text line))))
    (list   (render-span line-number (line-text line)))
    (t (error "Don't know how to render a line of ~s~%"
	      (type-of (line-text line))))))

(defvar *empty-indicator* "~"
  "String that indicates emptyness, usually past the end.")

(defun display-page ()
  "Display the lines already read, starting from the current."
  (move 0 0)
  (clear)
  (with-slots (line lines left page-size) *pager*
    (let ((y 0))
      (when (and (>= line 0) lines)
	(loop
	   :with l = (nthcdr line lines) :and i = line
	   :while (and (<= y (1- page-size)) (car l))
	   :do
	   (move y 0)
	   ;; (when (not (filter-this (car l)))
	   ;;   (incf y (display-line i (car l)))
	   ;;   (incf i))
	   (incf y (display-line i (car l)))
	   (setf l (cdr l))
	   (incf i)))
      ;; Fill the rest of the screen with twiddles to indicate emptiness.
      (when (< y page-size)
	(loop :for i :from y :below page-size
	   :do (mvaddstr i 0 *empty-indicator*))))))

(defun resize ()
  "Resize the page and read more lines if necessary."
  (with-slots (page-size line count) *pager*
    (when (/= page-size (1- curses:*lines*))
      (setf page-size (1- curses:*lines*))
      ;;(message-pause "new page size ~d" page-size)
      (when (< count (+ line page-size))
	;;(message-pause "read lines ~d" (- (+ line page-size) count))
	(read-lines (- (+ line page-size) count))))))

(defun ask-for (&key prompt space-exits)
  (let ((str (make-stretchy-string 10))
	(esc-count 0))
    (loop :with c :and done
       :do
       (move (1- curses:*lines*) 0)
       (clrtoeol)
       (addstr prompt) (addstr str)
       (refresh)
       (setf c (tt-get-char))
       (case c
	 (#\escape
	  (when (> (incf esc-count) 1)
	    (return-from ask-for nil)))
	 (#.(ctrl #\G)
	  (return-from ask-for nil))
	 ((#\return #\newline)
	  (setf done t))
	 ((#\backspace #\rubout :backspace :delete)
	  (if (> (fill-pointer str) 0)
	      (decf (fill-pointer str))
	      (return-from ask-for nil))) ; backing up past beginning exits
	 (#.(ctrl #\U)
	  (setf (fill-pointer str) 0))
	 (#\space
	  (if space-exits
	      (setf done t)
	      (stretchy-append str #\space)))
	 (t
	  (when (characterp c)
	    (stretchy-append str c))))
       :while (not done))
    str))

(defun ask (&optional prompt)
  ;;(ask-for :prompt prompt)
  (move (1- curses:*lines*) 0)
  (clrtoeol)
  (refresh)
  (prog1
      (rl:rl :prompt prompt
	     :terminal-class 'terminal-curses:terminal-curses
	     :accept-does-newline nil
	     :context :pager)
    ;; Make sure we go back to raw mode, so we can ^Z
    (curses:raw)))

(defun search-line (str line)
  "Return true if LINE contains the string STR. LINE can be a string, or a
list containing strings and lists."
  (typecase line
    (string
;;;     (search str line :test #'equalp))
     (ppcre:all-matches str line))
    (cons
     (loop :with result
	:for s :in line
	:if (setf result (search-line str s))
	:return result))))

(defun search-for (str)
  (with-slots (lines count line page-size ignore-case) *pager*
    (let ((ll (nthcdr line lines))
	  (ss (if ignore-case (s+ "(?i)" str) str))
	  l)
      (loop
	 :do
	 (setf l (car ll))
	 (when (and l (>= (line-number l) (max 0 (- count page-size))))
	   (read-lines page-size))
	 :while (and l (< (line-number l) count)
		     (not (search-line ss (line-text l))))
	 :do
	 (setf ll (cdr ll)))
      (if (and l (< (line-number l) count))
	  (setf line (line-number l))
	  nil))))

;; (defun sub-pager ()
;;   (let ((*pager*))
;;     (pager-loop)))

(defun keep-lines ()
  "Show only lines matching a regular expression."
  (let ((filter (ask "Show only lines matching: ")))
    (setf (pager-keep-expr *pager*)
	  (and filter (length filter) filter))))

(defun filter-lines ()
  "Hide lines matching a regular expression."
  (let ((filter (ask "Hide lines matching: ")))
    (setf (pager-filter-exprs *pager*)
	  (if filter (list filter) nil))))

(defun filter-more ()
  "Add to the list of regular expressions for hiding lines."
  (let ((filter (ask "Hide lines also mathing: ")))
    (when filter
      (push filter (pager-filter-exprs *pager*)))))

(defun set-option ()
  "Set a pager option. Propmpts for what option to toggle."
  (message "Set option: ")
  (with-slots (show-line-numbers ignore-case wrap-lines raw-output pass-special)
      *pager*
    (let ((char (tt-get-char)))
      (case char
	((#\l #\L)
	 (setf show-line-numbers (not show-line-numbers))
	 (tmp-message "show-line-numbers is ~:[Off~;On~]"
		      show-line-numbers))
	((#\i #\I)
	 (setf ignore-case (not ignore-case))
	 (tmp-message "ignore-case is ~:[Off~;On~]" ignore-case))
	((#\w #\S) ;; S is for compatibility with less
	 (setf wrap-lines (not wrap-lines))
	 (display-page)
	 (tmp-message "wrap-lines is ~:[Off~;On~]" wrap-lines)
	 (refresh))
	(#\r
	 (setf raw-output (not raw-output))
	 (display-page)
	 (tmp-message "raw-output is ~:[Off~;On~]" raw-output)
	 (refresh))
	(#\p
	 (setf pass-special (not pass-special))
	 (display-page)
	 (tmp-message "pass-special is ~:[Off~;On~]" pass-special)
	 (refresh))
	(otherwise
	 (tmp-message "Unknown option '~a'" (nice-char char)))))))

(defun open-file (filename &key (offset 0))
  "Open the given FILENAME."
  (with-slots (count lines line got-eof stream page-size) *pager*
    (let ((new-stream (open (quote-filename filename) :direction :input)))
      (when new-stream
	(close stream)
	(setf stream new-stream
	      lines '()
	      count 0
	      line offset
	      got-eof nil)
	(read-lines (+ page-size offset)))
      new-stream)))

(defun open-file-command (filename &key (offset 0))
  "Open the given FILENAME."
  (when (not (open-file filename :offset offset))
    (tmp-message "Can't open file \"~s\".")))

;; File locations can be either a file name, or a list or vector with the first
;; element being a file name and the second being an offset in the file. If the
;; offset part doesn't exist, it is considered to be zero.
;; (@@@ maybe these should move to dlib?)

(defun file-location-file (location)
  "Return the file part of a file location."
  (typecase location
    (string location)
    ((or list vector) (elt location 0))
    (t location)))

(defun file-location-offset (location)
  "Return the offset part of a file location, which defaults to 0 if the
location doesn't have an offset part."
  (typecase location
    (string 0)
    ((or list vector)
     (if (and (> (length location) 1)
	      (numberp (elt location 1)))
	 (elt location 1)
	 0))
    (t 0)))

(defun next-file ()
  "Go to the next file in the set of files."
  (with-slots (count lines line got-eof file-list file-index stream
		     page-size) *pager*
    (let (got-it (len (length file-list)))
      (loop :while (and file-index (< file-index (1- len)))
	 :do
	 (when (not (equal (file-location-file
			    (elt file-list file-index))
			   (file-location-file
			    (elt file-list (1+ file-index)))))
	   (setf got-it t)
	   (return))
	 (incf file-index))
    (if (and file-index got-it)
	(open-file-command
	 (file-location-file (elt file-list file-index))
	 :offset (file-location-offset (elt file-list file-index)))
	(tmp-message "No next file.")))))

(defun previous-file ()
  "Go to the previous file in the set of files."
  (with-slots (count lines line got-eof file-list file-index stream
		     page-size) *pager*
    (let (got-it)
      (loop :while (and file-index (> file-index 0))
	 :do
	 (when (not (equal (file-location-file
			    (elt file-list file-index))
			   (file-location-file
			    (elt file-list (1- file-index)))))
	   (setf got-it t)
	   (return))
	 (decf file-index))
      (if (and file-index got-it)
	  (open-file-command
	   (file-location-file (elt file-list file-index))
	   :offset (file-location-offset (elt file-list file-index)))
	  (tmp-message "No previous file.")))))

(defun next-file-location ()
  "Go to the next file in the set of files."
  (with-slots (file-list file-index) *pager*
    (if (and file-index (< file-index (1- (length file-list))))
	(progn
	  (if (not (equal (file-location-file (elt file-list file-index))
			  (file-location-file (elt file-list (1+ file-index)))))
	      (progn
		(incf file-index)
		(open-file-command
		 (file-location-file (elt file-list file-index))
		 :offset (file-location-offset (elt file-list file-index))))
	      (progn
		(incf file-index)
		(go-to-line
		 (1+ (file-location-offset (elt file-list file-index)))))))
	(tmp-message "No next file."))))

(defun previous-file-location ()
  "Go to the previous file in the set of files."
  (with-slots (file-list file-index) *pager*
    (if (and file-index (> file-index 0))
	(progn
	  (if (not (equal (file-location-file (elt file-list file-index))
			  (file-location-file (elt file-list (1- file-index)))))
	      (progn
		(decf file-index)
		(open-file-command
		 (file-location-file (elt file-list file-index))
		 :offset (file-location-offset (elt file-list file-index))))
	      (progn
		(decf file-index)
		(go-to-line
		 (1+ (file-location-offset (elt file-list file-index)))))))
	(tmp-message "No previous file."))))

(defun show-file-list ()
  (with-slots (file-list file-index) *pager*
    (if (not file-list)
	(tmp-message "The file list is empty.")
	(let ((i 0))
	  (clear)
	  (map nil (_ (addstr
		       (format nil "~c ~s~%"
			       (if (= i file-index) #\* #\space) _))
		      (incf i))
	       file-list)
	  (tt-get-key)))))

(defun quit ()
  "Exit the pager."
  (setf (pager-quit-flag *pager*) t))

(defun suspend ()
  "Suspend the pager."
  (if (and (find-package :lish)
	   (find-symbol "*LISH-LEVEL*" :lish)
	   (symbol-value (find-symbol "*LISH-LEVEL*" :lish)))
      (setf (pager-suspend-flag *pager*) t)
      (tmp-message "No shell to suspend to.~%")))

(defun next-page ()
  "Display the next page."
  (with-slots (line page-size count got-eof) *pager*
    (incf line page-size)
    (when (> (+ line page-size) count)
      (read-lines page-size))
    (when (and (>= line count) got-eof)
      (setf line (1- count)))))

(defun next-line ()
  "Display the next line."
  (with-slots (line page-size count prefix-arg got-eof) *pager*
    (let ((n (or prefix-arg 1)))
      (incf line n)
      (if (> (+ line page-size) count)
	  (read-lines n))
      (when (and (>= line count) got-eof)
	(setf line (1- count))))))

(defun previous-page ()
  "Display the previous page."
  (with-slots (line page-size) *pager*
    (setf line (max 0 (- line page-size)))))

(defun previous-line ()
  "Display the previous line."
  (with-slots (prefix-arg line) *pager*
    (let ((n (or prefix-arg 1)))
      (setf line (max 0 (- line n))))))

(defun scroll-right ()
  "Scroll the pager window to the right."
  (with-slots (left prefix-arg) *pager*
    (incf left (or prefix-arg 10))))

(defun scroll-left ()
  "Scroll the pager window to the left."
  (with-slots (left prefix-arg) *pager*
    (setf left (max 0 (- left (or prefix-arg 10))))))

(defun scroll-beginning ()
  "Scroll the pager window to the leftmost edge."
  (setf (pager-left *pager*) 0))

;; We don't bother counting the max column during display, since it's possibly
;; expensive and unnecessary. We just count it here when needed, which is
;; somewhat redundant, but expected to be overall more efficient.
;; The issue is mostly that we don't have to consider the whole line when
;; clipping, but we do have to consider it when finding the maximum.
(defun scroll-end ()
  "Scroll the pager window to the rightmost edge of the text."
  (with-slots (line lines left page-size show-line-numbers) *pager*
    (let ((max-col 0))
      (loop
	 :with y = 0
	 :and l = (nthcdr line lines)
	 :and i = line
	 :and the-line
	 :while (and (<= y (1- page-size)) (car l))
	 :do
	 ;;(message-pause-for 70 "~s ~s ~s ~s" max-col y i l)
	 (setf the-line (line-text (car l))
	       (fill-pointer *fat-buf*) 0
	       max-col
	       (max max-col
		    (+ (if show-line-numbers
			   (length (format nil "~d: " i))
			   0)
		       (length
			(if (stringp the-line)
			    the-line
			    (span-to-fat-string the-line
						:fat-string *fat-buf*))))))
	 (incf y)
	 (incf i)
	 (setf l (cdr l)))
      (tmp-message "max-col = ~d" max-col)
      (setf left (max 0 (- max-col *cols*))))))

(defun go-to-line (n)
  (with-slots (count line page-size) *pager*
    ;; (read-lines (min 1 (- n count)))
    (read-lines (max 1 (- (+ n page-size) count)))
    (setf line (1- n))))

(defun go-to-beginning ()
  "Go to the beginning of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg count line) *pager*
    (if prefix-arg
	(go-to-line prefix-arg)
	(setf line 0))))

(defun go-to-end ()
  "Go to the end of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg count line page-size) *pager*
    (if prefix-arg
	(go-to-line prefix-arg)
	(progn
	  (read-lines 0)
	  (setf line (max 0 (- count page-size)))))))

(defun search-command ()
  "Search for something in the stream."
  (with-slots (search-string) *pager*
    (setf search-string (ask "/"))
    (when (not (search-for search-string))
      (tmp-message "--Not found--"))))

(defun search-next ()
  "Search for the next occurance of the current search in the stream."
  (with-slots (line search-string) *pager*
    (incf line)
    (when (not (search-for search-string))
      (decf line)
      (tmp-message "--Not found--"))))

(defun clear-search ()
  "Clear the search string."
  (setf (pager-search-string *pager*) nil))

(defun seekable-p ()
  (let* ((pos (file-position (pager-stream *pager*)))
	 (result (file-position (pager-stream *pager*) pos)))
    result))

(defun show-info ()
  "Show information about the stream."
  (tmp-message "%f %l of %L %b/%B ~:[un~;~]seekable" (seekable-p)))

(defun redraw ()
  "Redraw the display."
  (clear) (refresh))

(defun reread ()
  (if (seekable-p)
      (with-slots (stream line lines got-eof count page-size) *pager*
	(let ((l (nth line lines)))
	  (file-position stream (or (and l (line-position l)) 0)))
	(setf got-eof nil)
	(if (= line 0)
	    (setf lines nil count 0)
	    (progn
	      (setf count line)
	      ;; drop all following lines
	      (rplacd (nthcdr (1- line) lines) nil)))
	(read-lines page-size))
      (tmp-message "Can't re-read an unseekable stream.")))

(defun digit-argument ()
  "Accumulate digits for the PREFIX-ARG."
  (with-slots (input-char prefix-arg message) *pager*
    (when (and (characterp input-char) (digit-char-p input-char))
      (setf prefix-arg (+ (* 10 (or prefix-arg 0))
			  (position input-char +digits+))
	    message (format nil "Prefix arg: ~d" prefix-arg)))))

(defmacro sub-page ((stream-var) &body body)
  "Generate some output and run a sub-instance of the pager on it."
  (let ((input (gensym "SUB-PAGE-INPUT")))
    `(with-input-from-string
	 (,input
	  (with-output-to-string (,stream-var)
	    ,@body))
       (page ,input))))

;; The long commands are a meager attempt at mindless "less" compatibility.

(defmacro lc (&body body)
  `(function (lambda (c) (declare (ignorable c)) ,@body)))

(defparameter *long-commands*
  (vector
   `("edit"	  (#\e #\x)  "Examine a different file."
     ,(lc (open-file-command (ask (s+ ":" c #\space)))))
   `("next"	  (#\n)	    "Examine the next file in the arguments."
     ,(lc (next-file)))
   `("previous"	  (#\p)	    "Examine the previous file in the arguments."
     ,(lc (previous-file)))
   `("help"	  (#\h)	    "Show the pager help."
     ,(lc (help)))
   `("?"	  (#\?)	    "Show this long command help."
     ,(lc (command-help)))
   `("file"	  (#\f)	    "Show the file information."
     ,(lc (show-info)))
   `("quit"	  (#\q)	    "Quit the pager."
     ,(lc (quit)))))
    #| (#\d (remove-file-from-list)) |#

(defun command-help ()
  "Show help on long commands, aka colon commands."
  (sub-page (s)
    (nice-print-table
     (loop :for c :across *long-commands*
	:collect (list (elt c 0) (elt c 1) (elt c 2)))
     '("Command" "Abbrev" "Description")
     :stream s)))

(defun read-command ()
  (let ((cmd (ask-for :prompt ":" :space-exits t)))
    (when (and cmd (stringp cmd))
      (let (found)
	(loop
	   :for c :across *long-commands*
	   :do (when (position (char cmd 0) (elt c 1))
		 (setf found t)
		 (funcall (elt c 3) cmd)))
	(when (not found)
	  (message "~a is an unknown command." cmd))))))

;; These bindings are basically "less" compatible, but are also an horrible
;; mishmash of emacs, vi, less, more, pg, etc. Just pandering to whatever
;; people are used to scrolling with.

(defkeymap *help-keymap*
  `(
    (#\c		. describe-key-briefly)
    (#\k		. describe-key)
    (#\b		. describe-bindings)
    (#\?		. more-help)
    (#\q		. quit)
    ))

(defkeymap *normal-keymap*
  `((#\q		. quit)
    (#\Q		. quit)
    (,(ctrl #\C)	. quit)
    (,(ctrl #\Z)	. suspend)
    (:resize		. resize)
    (#\space		. next-page)
    (:npage		. next-page)
    (,(ctrl #\F)	. next-page)
    (,(ctrl #\V)	. next-page)
    (#\return		. next-line)
    (:down		. next-line)
    (#\j		. next-line)
    (,(ctrl #\N)	. next-line)
    (#\b		. previous-page)
    (:ppage		. previous-page)
    (#\rubout		. previous-page)
    (,(ctrl #\B)	. previous-page)
    (,(meta-char #\v)   . previous-page)
    (#\k		. previous-line)
    (:up		. previous-line)
    (,(ctrl #\P)	. previous-line)
    (#\l		. scroll-right)
    (:right		. scroll-right)
    (#\h		. scroll-left)
    (:left		. scroll-left)
    (,(ctrl #\A)	. scroll-beginning)
    (:home		. scroll-beginning)
    (,(ctrl #\E)	. scroll-end)
    (:end		. scroll-end)
    (#\<		. go-to-beginning)
    (#\g		. go-to-beginning)
    (,(meta-char #\<)	. go-to-beginning)
    (#\>		. go-to-end)
    (#\G		. go-to-end)
    (,(meta-char #\>)	. go-to-end)
    (#\/		. search-command)
    (,(ctrl #\S)	. search-command)
    (#\n		. search-next)
    (#\=		. show-info)
    (,(ctrl #\G)	. show-info)
    (,(meta-char #\l)	. show-file-list)
    (#\V                . show-version)
    (#\-		. set-option)
    (,(ctrl #\L)	. redraw)
    (#\R		. reread)
    (,(meta-char #\u)	. clear-search)
    (,(meta-char #\n)	. next-file-location)
    (,(meta-char #\p)	. previous-file-location)
    (,(meta-char #\N)	. next-file)
    (,(meta-char #\P)	. previous-file)
    (#\?		. help)
    (,(ctrl #\H)	. help-key)
    (:backspace		. help-key)
    (,(meta-char #\=)   . describe-key-briefly)
;    (,(meta-char #\=)   . describe-key)
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
    (#\:		. read-command)
    (#\^                . keep-lines)
    (#\&                . filter-lines)
    (#\*                . filter-more)
    ))

(defparameter *escape-keymap* (build-escape-map *normal-keymap*))

(defun describe-key-briefly ()
  "Prompt for a key and say what function it invokes."
  (message "Press a key: ")
  (let* ((key-seq (get-key-sequence (λ () (tt-get-key)) *normal-keymap*))
	 (action
	  ;;(key-definition key *normal-keymap*)
	  (key-sequence-binding key-seq *normal-keymap*)
	  ))
    (if action
	(tmp-message "~a is bound to ~a" (key-sequence-string key-seq) action)
	(tmp-message "~a is not defined" (key-sequence-string key-seq)))))

(defun describe-key ()
  "Prompt for a key and describe the function it invokes."
  (message "Press a key: ")
  (let* ((key-seq (get-key-sequence (λ () (tt-get-key)) *normal-keymap*))
    	 (action (key-sequence-binding key-seq *normal-keymap*)))
    (cond
      (action
       (clear) (move 0 0)
       (if (documentation action 'function)
	   (progn
	     (addstr (format nil "~(~a~): ~a~%" action
			     (key-sequence-string key-seq)))
	     (addstr (justify-text
		      (documentation action 'function) :stream nil)))
	   (addstr
	    (format nil "Sorry, there's no documentation for ~a.~%" action)))
       (refresh)
       (tt-get-char)
       ;;(tmp-message pager "")
       )
      (t
       (tmp-message "~a is not defined" (key-sequence-string key-seq))))))

(defun help-key ()
  "Sub-command for help commands."
  (message "Help (? for more help): ")
  (perform-key (tt-get-char) *help-keymap*)
  (setf (pager-quit-flag *pager*) nil))

(defun more-help ()
  "Show more help on help commands."
  (clear)
  (move 0 0)
  (addstr "c - Print the name of function that a key performs.
k - Describe the function that a key performs.
b - List all keys and the functions they perform.
q - Abort")
  (refresh)
  (help-key))				; @@@ this could infinitely recurse

(defun perform-key (key &optional (keymap *normal-keymap*))
  (with-slots (message command last-command) *pager*
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
	  (funcall command))
	 ((keymap-p (symbol-value command)) ; a keymap
	  (perform-key (tt-get-char) (symbol-value command)))
	 (t				; anything else
	  (setf message
		(format nil "Key binding ~S is not a function or a keymap."
			command)))))
      ;; a function object
      ((functionp command)
       (funcall command))
      (t				; anything else is an error
       (error "Weird thing in keymap: ~s." command)))))

(defstruct suspended-pager
  "State of a suspended pager."
  pager
  file-list
  stream
  close-me)

(defun pager-loop ()
  (with-slots (message input-char command prefix-arg quit-flag suspend-flag)
      *pager*
    (loop
     :do
     (display-page)
     (if message
	 (let ((*pager-prompt* message))
	   (display-prompt)
	   (setf message nil))
       (display-prompt))
     (refresh)
     (setf input-char (tt-get-char))
     (perform-key input-char)
     (when (not (equal command 'digit-argument))
       (setf prefix-arg nil))
     :while (not (or quit-flag suspend-flag)))))

(defun page (stream &optional pager file-list close-me suspended)
  "View a stream with the pager. Return whether we were suspended or not."
  (with-curses
    (unwind-protect
      (progn
	(when (and (not stream) file-list)
	  (setf stream (open (quote-filename
			      (file-location-file (elt file-list 0)))
			     :direction :input)
		close-me t))
	(let ((*pager*
	       (or pager
		   (make-instance 'pager
				  :stream stream
				  :page-size (1- curses:*lines*)
				  :file-list file-list))))
	  (when (not suspended)
	    (freshen *pager*)
	    (setf (pager-stream *pager*) stream))
	  (with-slots (count line page-size left search-string input-char
		       file-list file-index message prefix-arg quit-flag
		       suspend-flag command) *pager*
	    (when file-list
	      (setf file-index 0)
	      (when (not (zerop (file-location-offset (elt file-list 0))))
		(go-to-line (1+ (file-location-offset (elt file-list 0))))))
	    (read-lines page-size)
	    (setf quit-flag nil
		  suspend-flag nil
		  suspended nil)
	    (curses:raw)	     ; give a chance to exit during first read
#|	    (loop :do
	       (display-page)
	       (if message
		   (let ((*pager-prompt* message))
		     (display-prompt)
		     (setf message nil))
		   (display-prompt))
	       (refresh)
	       (setf input-char (tt-get-char))
	       (perform-key input-char)
	       (when (not (equal command 'digit-argument))
		 (setf prefix-arg nil))
	       :while (not (or quit-flag suspend-flag))) |#
	    (pager-loop)
	    (when suspend-flag
	      (setf suspended t)
	      (if (find-package :lish)
		  (let ((suspy (make-suspended-pager
				:pager *pager*
				:file-list file-list
				:stream stream
				:close-me close-me)))
		    (funcall (find-symbol "SUSPEND-JOB" :lish)
			     "pager" "" (lambda () (pager:resume suspy)))
		    (format t "Suspended.~%"))
		  (format t "No shell loaded to suspend to.~%"))))))
      (when (and close-me (not suspended) stream)
	(close stream))))
  suspended)

(defun resume (suspended)
  (with-slots (pager file-list stream close-me) suspended
    (page stream pager file-list close-me t)))

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
  "Evaluate the body with the *standard-output*, *error-output* and
*trace-output* going to the pager."
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
       (if (= (length file-or-files) 1)
	   (pager (first file-or-files)) ; so we get the browser interactively
	   (page nil nil file-or-files)))
      ((or (stringp file-or-files) (pathnamep file-or-files))
       (if (probe-directory file-or-files)
	   (pager (pick-file :directory file-or-files))
	   (let (stream suspended)
	     (unwind-protect
		  (progn
		    (setf stream (open (quote-filename file-or-files)
				       :direction :input)
			  suspended (page stream)))
	       (when (and stream (not suspended))
		 (close stream))))))
      (t
       (error "The pager doesn't know how to deal with a ~w"
	      (type-of file-or-files))))))

(defun help ()
  (with-input-from-string
   (input
    (with-output-to-string (output)
      (format output
"Hi! You are using Dan's pager. You seem to have hit '~a' again, which gives a
summary of commands. Press 'q' to exit this help. The keys are superficially
compatible with 'less', 'vi' and 'emacs'.
" (pager-input-char *pager*))
      (write-string "
[1mGeneral[0m
  ?              		Show this help.
  q Q ^C         		Exit the pager.
  ^Z	         		Suspend the pager.

[1mMovement[0m
  space   :NPAGE ^F ^V		Show the next page.
  :DELETE :PPAGE ^B M-v	b	Show the previous page.
  j :DOWN ^M			Show the next line.
  k :UP				Show the previous line.
  l :RIGHT			Scroll right.
  h :LEFT			Scroll left.
  ^A :HOME			Scroll to the beginning of the line.
  ^E :END			Scroll to the end of the line.
  < g M-<			Go to the beginning of the stream.
  > G M->			Go to the end of the stream.

[1mSearching[0m
  / ^S				Search.
  n				Search for next occurrence.
  M-u				Clear the search.

[1mFiltering[0m
  ^				Keep matching lines.
  &				Remove matching lines.
  *				Add to the list of removed lines.

[1mMiscellaneous[0m
  ^L 				Re-draw the screen.
  R				Re-read the stream.
  M-n				View the next file.
  M-p				View the previous file.

[1mMore information[0m
  ^H :BACKSPACE			Extended help.
  = ^G				Show information about the stream.
  V				Show the pager version.
  M-=				Show what function a key performs.
  : M-:ESCAPE			Prompt for a long command. '?' for help.

[1mOptions[0m
  -				Toggle an option. Options are:
    i I				  Ignore case when searching.
    w S				  Wrap long lines.
    l L				  Show line numbers.

Press 'q' to exit this help.
" output)))
   (page input)))

(defun describe-bindings ()
  "Show help on pager key commands."
  (with-input-from-string
      (input
       (with-output-to-string (output)
	 (write-string
"You are using Dan's pager. Here's a list of what the keys do.
If you want a description of what a function does, press Control-H then 'k',
then the key. Press 'q' to exit this help.
" output)
	 (keymap:dump-keymap *normal-keymap* :stream output)
	 (princ "Press 'q' to exit this help.
" output)))
    (page input)))

(defun browse (&optional (dir "."))
  "Look at files."
  (let ((*pager* (make-instance 'pager))
	filename
	(directory dir))
    (loop :while (setf (values filename directory)
		       (pick-file :directory directory))
       :do (with-open-file (stream (quote-filename filename))
	     (page stream)))))

;; @@@ Run in a thread:
;;
;; (defmacro run (&whole whole &optional file-or-files)
;;   (declare (ignore file-or-files))
;;   `(apply #'pager ',(cdr whole)))

#+lish
(lish:defcommand pager
  ((show-line-numbers boolean :short-arg #\l
    :help "True to show line numbers.")
   (ignore-case boolean :short-arg #\i
    :help "True to ignore case in searches.")
   (raw-output boolean :short-arg #\r
    :help "True to output characters without processing.")
   (pass-special boolean :short-arg #\p
    :help "True to pass special escape sequences to the terminal.")
   (files pathname :repeating t :help "Files to view."))
  :accepts (:grotty-stream :file-list :file-locaations)
  "Look through text, one screen-full at a time."
  (declare (ignore show-line-numbers ignore-case raw-output pass-special)) ; @@@
  (pager (or files lish:*input* *standard-input*)))

;; EOF
