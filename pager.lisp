;;
;; pager.lisp - More or less like more or less.
;;

;; $Revision: 1.11 $

;; TODO:
;;  - syntax highlighting
;;  - grotty & color processing
;;  - remove & transform lines
;;  - simpile HTML rendering
;;  - big stream issues?

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
  pager files...					[shell command]
  pager &optional (file-or-files (pick-file))		[function]

The function takes a file name or a stream or a list of such.
The shell command takes any number of file names.
")
  (:use :cl :dlib :dlib-misc :curses :opsys :fui :stretchy :keymap :char-util
	:ppcre)
  (:export
   #:*pager-prompt*
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
(defvar *pager-prompt* "%f line %l of %L"
  "The current default prompt.")

;; "text text text"
;; ((:tag "text") "text" (:tag "text") "text")
;; (:tag "text")
;; ("text text " (:tag "text " (:tag "text") " text") "text")

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
    :documentation "True if we got to the end of stream")
   (search-string
    :initarg :search-string :accessor pager-search-string :initform nil
    :documentation "If non-nil search for and highlight")
   (filter-expr
    :initarg :filter-expr :accessor pager-filter-expr :initform nil
    :documentation "Don't show lines matching this regular expression")
   (file-list
    :initarg :file-list :accessor pager-file-list :initform nil
    :documentation "List of files to display")
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
    :initarg :show-line-numbers :accessor pager-show-line-numbers :initform nil
    :documentation "True to show line numbers along the left side.")
   (ignore-case
    :initarg :ignore-case :accessor pager-ignore-case :initform t
    :documentation "True to ignore case in searches.")
   (wrap-lines
    :initarg :wrap-lines :accessor pager-wrap-lines :initform nil
    :documentation "True to wrap lines at the edge of the window."))
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
	(pager-filter-expr o) nil
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

(defun nop-process-line (pager line)
  (declare (ignore pager))
  line)

(defstruct fatchar
  (c (code-char 0) :type character)
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(defun make-fat-string (string)
  "Make a fat string from a string."
  (let ((fs (make-array (list (length string))
			:element-type 'fatchar
			:initial-element (make-fatchar))))
    (loop :for i :from 0 :below (length string) :do
       (setf (aref fs i) (make-fatchar :c (char string i))))
    fs))

(defun span-length (span)
  "Calculate the length in characters of the span."
  (the fixnum (loop :for e :in span
		 :sum (typecase e
			(string (length e))
			(cons (span-length e))
			(t 0)))))

#|
(defun get-inner-span (fat-line start tag state)
  (let ((i start)
	(len (- (length fat-line) start))
	(span '())
	c)
    (loop :do
       (setf c (aref fat-line i))
       (let ((added
	      (set-difference (fatchar-attrs c) (fatchar-attrs state))
	     (removed
	      (set-difference (fatchar-attrs state) (fatchar-attrs c)))))
	 (when (not (eql (fatchar-fg c) (fatchar-fg state)))
	   (
	   (push (fatchar-fg c) added))
	 (when (not (eql (fatchar-bg c) (fatchar-bg state)))
	   (push (fatchar-fg c) added))
	     
		  
	 (incf i)
       :while (< i len))
    `(,tag ,@span)))

(defun NEW-get-span (fat-line start)
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fat-line) 0)
    (return-from get-span fat-line))
  (let ((str (make-stretchy-string (- (length fat-line) start)))
	)
    (get-inner-span

(defun NEW-get-span (fat-line start)
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fat-line) 0)
    (return-from get-span fat-line))
  (let ((str (make-stretchy-string (- (length fat-line) start)))
	(attrs (fatchar-attrs (aref fat-line start)))
	(i start) (len (length fat-line))
	(span '())
	last c)
    (loop :do
       (setf c (aref fat-line i))
       (if last
	 (let ((added
		(set-difference (fatchar-attrs c) (fatchar-attrs last)))
	       (removed
		(set-difference (fatchar-attrs last) (fatchar-attrs c)))))
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

(defmacro dumpy (&rest args)
  "Print the names and values of the arguments, like NAME=value."
  (let ((za (loop :for z :in args :collect
	       `(format t "~a=~a " ',z ,z))))
    `(progn ,@za (terpri))))

;;; ^[[00m	normal
;;; ^[[01;34m	bold, blue fg
;;; ^[[m	normal
;;; ^[[32m	green fg
;;; ^[[1m	bold

;; We drink of the color and become the color.
(defun grok-ansi-color (str)
  "Take an string with an ANSI terminal color escape sequence, starting after
the ^[[ and return NIL if there was no valid sequence, or an integer offset
to after the sequence, the foreground, background and a list of attributes.
NIL stands for whatever the default is, and :UNSET means that they were not
set in this string."
  (let (num inc attr-was-set
	(i 0) (len (length str))
	(fg :unset) (bg :unset) (attr '()))
    (loop
       :do
       #| (princ "> ") (dumpy num i inc (subseq str i)) |#
       (setf (values num inc) (parse-integer str :start i :junk-allowed t))
       #| (princ "< ") (dumpy num i inc (subseq str i)) |#
       (if (or (not num) (not inc))
	   (progn
	     ;; Just an #\m without arguments means no attrs and unset color
	     (when (eql (char str i) #\m)
	       (setf attr '() fg nil bg nil attr-was-set t))
	     (return))
	   (progn
	     (setf i inc)
	     (when (or (eql (char str i) #\;)
		       (eql (char str i) #\m))
	       (incf i)
	       (case num
		 (0  (setf attr '() fg nil bg nil attr-was-set t))
		 (1  (pushnew :bold attr)      (setf attr-was-set t))
		 (4  (pushnew :underline attr) (setf attr-was-set t))
		 (5  (pushnew :blink attr)     (setf attr-was-set t))
		 (7  (pushnew :inverse attr)   (setf attr-was-set t))
		 (8  (pushnew :invisible attr) (setf attr-was-set t))
		 (22 (setf attr (delete :bold attr))
		     (setf attr-was-set t))
		 (24 (setf attr (delete :underline attr))
		     (setf attr-was-set t))
		 (25 (setf attr (delete :blink attr))
		     (setf attr-was-set t))
		 (27 (setf attr (delete :inverse attr))
		     (setf attr-was-set t))
		 (28 (setf attr (delete :invisible attr))
		     (setf attr-was-set t))
		 (30 (setf fg :black))
		 (31 (setf fg :red))
		 (32 (setf fg :green))
		 (33 (setf fg :yellow))
		 (34 (setf fg :blue))
		 (35 (setf fg :magenta))
		 (36 (setf fg :cyan))
		 (37 (setf fg :white))
		 (39 (setf fg nil))
		 (40 (setf bg :black))
		 (41 (setf bg :red))
		 (42 (setf bg :green))
		 (43 (setf bg :yellow))
		 (44 (setf bg :blue))
		 (45 (setf bg :magenta))
		 (46 (setf bg :cyan))
		 (47 (setf bg :white))
		 (49 (setf bg nil))
		 (otherwise #| just ignore unknown colors or attrs |#))
	       (when (eql (char str (1- i)) #\m)
		 (return)))))
       :while (< i len))
    (values
     (if (and (eq fg :unset) (eq bg :unset) (not attr-was-set))
	 nil i)
     fg bg (if (not attr-was-set) :unset attr))))

;; Just trying to figure out why I get:
;;   note: deleting unreachable code
;; on
;; (NUNION PAGER::ATTRS (PAGER::FATCHAR-ATTRS PAGER::C))

#|
(defun p-a-c-l (fat-line)
 (let ((new-fat-line '()))
    (loop :with len = (length fat-line) :and i = 0
       :and line = (map 'string #'(lambda (x) (fatchar-c x)) fat-line)
       :and fg :and bg :and c :and attrs
       :while (< i len)
       :do
#|       (when (and (< i (1- len))
		  (char= (aref line i) #\escape)
		  (char= (aref line (1+ i)) #\[))
	 (multiple-value-bind (inc i-fg i-bg i-attrs)
	     (grok-ansi-color (subseq line i))
	   (unless (eq i-fg    :unset) (setf fg i-fg) (format t "FG ~s~%" fg))
	   (unless (eq i-bg    :unset) (setf bg i-bg) (format t "BG ~s~%" bg))
	   (unless (eq i-attrs :unset) (setf attrs i-attrs))
	   (when inc
	     (incf i inc)))
       ) |#
       (when (and (< i (1- len))
		  (char= (aref line i) #\escape)
		  (char= (aref line (1+ i)) #\[))
	 (let ((i-attrs :lumpy))
	   (unless (eql i-attrs :lumpy)
	     (setf attrs i-attrs))))
       (setf c (aref fat-line i)
	     (fatchar-fg c) fg
	     (fatchar-bg c) bg
	     (fatchar-attrs c) (nunion attrs (fatchar-attrs c))
	     )
       (push c new-fat-line)
       (incf i))
    new-fat-line))
|#

(defun poo (x)
  (let ((nfl '()))
    (loop :with c
       :for i :from 1 :below (length x) :do
       (setf c (aref x i)
	     (fatchar-attrs c) (union `(1 ,i 2 3) '(do re me)))
       (push c nfl))
    nfl))

(defun process-ansi-colors-line (fat-line)
  (when (zerop (length fat-line))
    (return-from process-ansi-colors-line fat-line))
  (let ((new-fat-line (make-stretchy-vector (length fat-line)
					    :element-type 'fatchar))
	(i 0)
	(len (length fat-line))
	(line (map 'string #'(lambda (x) (fatchar-c x)) fat-line))
	fg bg attrs)
    (flet ((looking-at-attrs ()
	     "Return true if might be looking at some attrs."
	     (and (< i (1- len))
		  (char= (aref line i) #\escape)
		  (char= (aref line (1+ i)) #\[)))
	   (get-attrs ()
	     "Get the attrs we might be looking at."
	     (incf i 2) ; the ^[ and [
	     (multiple-value-bind (inc i-fg i-bg i-attrs)
		 (grok-ansi-color (subseq line i))
	       (unless (eq i-fg    :unset) (setf fg i-fg))
	       (unless (eq i-bg    :unset) (setf bg i-bg))
	       (unless (eq i-attrs :unset) (setf attrs i-attrs))
	       (when inc
		 (incf i inc))))	; for the parameters read
	   (copy-char ()
	     "Copy the current character to result."
	     (dbug "attrs = ~a~%" attrs)
	     (dbug "(aref fat-line i) = ~a~%" (aref fat-line i))
	     (let ((new-attrs (union attrs (fatchar-attrs (aref fat-line i)))))
	       (stretchy:stretchy-append
		new-fat-line (make-fatchar
			      :c (fatchar-c (aref fat-line i))
			      :fg fg :bg bg
			      :attrs new-attrs
			      )))
	     (incf i)))
      (loop :while (< i len) :do
	 (if (looking-at-attrs)
	     (get-attrs)
	     (copy-char))))
    new-fat-line))

(defun process-grotty-line (line)
  "Convert from grotty typewriter sequences to fatchar strings."
  (when (= (length line) 0)
    (return-from process-grotty-line line))
  (let ((fat-line (make-stretchy-vector (+ (length line) 16)
					:element-type 'fatchar)))
    (loop
       :with i = 0 :and len = (length line) :and attr
       :do
       (when (< i (- len 2))
	 (cond
	   ((and (char= (char line (+ i 1)) #\backspace)
		 (char= (char line (+ i 2)) (char line i)))
	    (incf i 2)
	    (setf attr :bold))
	   ((and (char= (char line i) #\_)
		 (char= (char line (+ i 1)) #\backspace))
	    (incf i 2)
	    (setf attr :underline))
	   (t
	    (setf attr nil))))
       (stretchy:stretchy-append
	fat-line
	(make-fatchar
	 :c (char line i) :attrs (and attr (list attr))))
       (incf i)
       :while (< i len))
    fat-line))

(defun process-line (pager line)
  "Process a line of text read from a stream."
  (declare (ignore pager))
  (let ((output (get-span
		 (process-ansi-colors-line (process-grotty-line line)) 0)))
    (if (and (= (length output) 1) (stringp (first output)))
	(first output)
	output)))

(defun read-lines (pager count)
  "Read new lines from the stream. Stop after COUNT lines. If COUNT is zero,
read until we get an EOF."
  (when (not (pager-got-eof pager))
    (let ((n 0)
	  (i (pager-count pager))
	  (line nil)
	  (lines '())
	  (pos (or (file-position (pager-stream pager)) 0)))
      (loop
	 :while (and (or (< n count) (zerop count))
		     (setf line
			   (resilient-read-line (pager-stream pager) nil nil)))
	 :do (push (make-line :number i
			      :position (incf pos (length line))
			      :text (process-line pager line))
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
		(if (and (typep (pager-stream pager) 'file-stream)
			 (ignore-errors (truename (pager-stream pager))))
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
  (fui:get-char))

(defun tmp-message (pager format-string &rest args)
  "Display a message at next command loop, until next input."
  (setf (pager-message pager) (apply #'format nil format-string args)))

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

(defparameter *attr*
  `((:normal	. ,+a-normal+)
    (:standout	. ,+a-standout+)
    (:underline	. ,+a-underline+)
    (:reverse	. ,+a-reverse+)
    (:blink	. ,+a-blink+)
    (:dim	. ,+a-dim+)
    (:bold	. ,+a-bold+)))

#|
             left         *cols*
win  :         |------------|
line : |----||-------||---------||---|
                                 |
                                *col*
|#

(defvar *col*)
(defvar *left*)
(defun show-span (s)
  (when s
    (typecase s
      (string
       (let* ((start (min (1- (length s)) (max 0 (- *left* *col*))))
	      (end   (min (length s) *cols*))
	      (len   (- end start)))
	 (when (> len 0)
	   (addstr (subseq s start end)))
	 (incf *col* (length s))))
      (list
       (let* ((f (first s))
	      (tag (and (or (keywordp f) (symbolp f)) f)))
	 (when tag (attron (cdr (assoc tag *attr*))))
	 (loop :while (< *col* *cols*)
	    :for ee :in (or (and tag (cdr s)) s)
	    :do (show-span ee))
	 (when tag (attroff (cdr (assoc tag *attr*)))))))))

(defun render-span (pager line-number line)
  (with-slots (left show-line-numbers) pager
    (let ((*col* left) (*left* left))
      (when show-line-numbers
	(let ((str (format nil "~d: " line-number)))
	  (incf *col* (length str))
	  (addstr str)))
      (show-span (line-text line)))))

(defun render-text-line (pager line-number line)
  (with-slots (left page-size search-string show-line-numbers ignore-case)
      pager
    (let* ((text1 (line-text line))
	   (end   (min (length text1) (+ *cols* left)))
	   (text  (if (> left 0)
		      (if (< left (length text1))
			  (subseq text1 left end)
			  "")
		      (subseq text1 0 end)))
	   (ss (if (and search-string ignore-case)
		   (s+ "(?i)" search-string)
		   search-string))
;;;	   (pos (search ss text :test #'equalp))
	   (matches (and ss (ppcre:all-matches ss text)))
	   (pos (when matches (car matches)))
	   (match-end (when matches (cadr matches)))
	   old-end)
      (if (and ss pos)
	  (progn
	    (when show-line-numbers
	      (addstr (format nil "~d: " line-number)))
	    (loop :with start = 0
	       :do
	       (addstr (subseq text start pos))
	       (standout)
;;;	       (addstr (subseq text pos (+ pos (length ss))))
	       (addstr (subseq text pos match-end))
	       (standend)
	       (setf start (1+ match-end) ;; (+ pos (length ss))
		     old-end match-end
;;;		     pos (search ss text :test #'equalp :start2 start))
		     matches (and ss (ppcre:all-matches ss text :start start))
		     pos (when matches (car matches))
		     match-end (when matches (cadr matches)))
	       (when (not pos)
;;;		 (addstr (subseq text (+ old-pos (length ss)))))
		 (addstr (subseq text old-end)))
	       :while pos))
	  (if show-line-numbers
	      (addstr (format nil "~d: ~a" line-number text))
	      (addstr text))))))

(defun span-matches (span expr)
  "Return true if the plain text of a SPAN matches the EXPR."
  ;; This is probably wasteful and slow.
  (let ((line
	 (with-output-to-string (str)
	   (labels ((glom-span (s)
		      (typecase s
			(string (princ s str))
			(list
			 (loop :for e :in (cdr s) :do (glom-span e)))
			(t (princ s str))))) ; probably a bit too DWIM-ish
	     (glom-span span)))))
    (and (ppcre:all-matches expr line) t)))

(defun filter-this (pager line)
  "Return true if we should filter this line."
  (when (pager-filter-expr pager)
    (typecase (line-text line)
      (string (and (ppcre:all-matches
		    (pager-filter-expr pager) (line-text line)) t))
      (list   (span-matches (pager-filter-expr pager) (line-text line)))
      (t (error "Don't know how to filter a line of ~s~%"
		(type-of (line-text line)))))))

(defun display-line (pager line-number line)
  (typecase (line-text line)
    (string (render-text-line pager line-number line))
    (list   (render-span pager line-number line))
    (t (error "Don't know how to render a line of ~s~%"
	      (type-of (line-text line))))))

(defun display-page (pager)
  "Display the lines already read, starting from the current."
  (move 0 0)
  (clear)
  (setf *pager* pager) ;; @@@ DEBUG
  (with-slots (line left page-size) pager
    (let ((y 0))
      (loop
	 :with l = (nthcdr line (pager-lines pager)) :and i = line
	 :while (and (<= i (+ line (1- page-size))) (car l))
	 :do
	 (move y 0)
	 (when (not (filter-this pager (car l)))
	   (display-line pager i (car l))
	   (incf y)
	   (incf i))
	 (setf l (cdr l)))
      ;; Fill the rest of the screen with twiddles to indicate emptiness.
      (when (< y page-size)
	(loop :for i :from y :below page-size
	   :do (mvaddstr i 0 "~"))))))

(defun ask-for (&key prompt space-exits)
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
	    (return-from ask-for nil)))
	 (#\^G
	  (return-from ask-for nil))
	 ((#\return #\newline)
	  (setf done t))
	 ((#\backspace #\rubout :backspace :delete)
	  (if (> (fill-pointer str) 0)
	      (decf (fill-pointer str))
	      (return-from ask-for nil))) ; backing up past beginning exits
	 (#\^U
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
  (ask-for :prompt prompt))

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

(defun search-for (pager str)
  (with-slots (lines count line page-size ignore-case) pager
    (let ((ll (nthcdr line lines))
	  (ss (if ignore-case (s+ "(?i)" str) str))
	  l)
      (loop
	 :do (setf l (car ll))
	 :while (and l (< (line-number l) count)
		     (not (search-line ss (line-text l))))
	 :do
	 (when (>= (line-number l) (max 0 (- count page-size)))
	   (read-lines pager page-size))
	 (setf ll (cdr ll)))
      (if (and l (< (line-number l) count))
	  (setf line (line-number l))
	  nil))))

(defun filter-lines (pager)
  (let ((filter (ask "&")))
    (setf (pager-filter-expr pager) filter)))

(defun set-option (pager)
  "Set a pager option. Propmpts for what option to toggle."
  (message "Set option: ")
  (let ((char (fui:get-char)))
    (case char
      ((#\l #\L)
       (setf (pager-show-line-numbers pager)
	     (not (pager-show-line-numbers pager)))
       (message-pause "show-line-numbers is ~:[Off~;On~]"
		      (pager-show-line-numbers pager)))
      ((#\i #\I)
       (setf (pager-ignore-case pager)
	     (not (pager-ignore-case pager)))
       (message-pause "ignore-case is ~:[Off~;On~]"
		      (pager-ignore-case pager)))
      (otherwise
       (message-pause "Unknown option ~c" (nice-char char))))))

(defun next-file (pager)
  "Go to the next file in the set of files."
  (with-slots (count lines line got-eof file-list file-index stream
	       page-size) pager
    (if (and file-index (< file-index (1- (length file-list))))
	(progn
	  (incf file-index)
	  (close stream)
	  (setf stream (open (quote-filename (nth file-index file-list))
			     :direction :input))
	  (setf lines '() count 0 line 0 got-eof nil)
	  (read-lines pager page-size))
	(tmp-message pager "No next file."))))

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
	(tmp-message pager "No previous file."))))

(defun open-file (pager filename)
  "Open the given FILENAME."
  (with-slots (count lines line got-eof stream page-size) pager
    (let ((new-stream (open (quote-filename filename) :direction :input)))
      (if new-stream
	  (progn
	    (close stream)
	    (setf stream new-stream
		  lines '()
		  count 0
		  line 0
		  got-eof nil)
	    (read-lines pager page-size))
	  (tmp-message pager "Can't open file \"~s\".")))))

(defun quit (pager)
  "Exit the pager."
  (setf (pager-quit-flag pager) t))

(defun suspend (pager)
  "Suspend the pager."
  (if (and (find-package :lish)
	   (find-symbol "*LISH-LEVEL*" :lish)
	   (symbol-value (find-symbol "*LISH-LEVEL*" :lish)))
      (setf (pager-suspend-flag pager) t)
      (tmp-message pager "No shell to suspend to.~%")))

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
  (with-slots (search-string) pager
    (setf search-string (ask "/"))
    (when (not (search-for pager search-string))
      (tmp-message pager "--Not found--"))))

(defun search-next (pager)
  "Search for the next occurance of the current search in the stream."
  (with-slots (line search-string) pager
    (incf line)
    (when (not (search-for pager search-string))
      (decf line)
      (tmp-message pager "--Not found--"))))

(defun clear-search (pager)
  "Clear the search string."
  (setf (pager-search-string pager) nil))

(defun show-info (pager)
  "Show information about the stream."
  (tmp-message pager "%f %l of %L %b/%B"))

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

(defmacro sub-page ((stream-var) &body body)
  "Generate some output and run a sub-instance of the pager on it."
  (let ((input (gensym "SUB-PAGE-INPUT")))
    `(with-input-from-string
	 (,input
	  (with-output-to-string (,stream-var)
	    ,@body))
       (page ,input))))

;; The long commands are a meager attempt at mindless "less" compatibility.
;; @@@ This points out the need for a curses-ification of tiny-rl.

(defmacro lc (&body body)
  `(function (lambda (p c) (declare (ignorable c)) ,@body)))

(defparameter *long-commands*
  (vector
   `("edit"	  (#\e #\x)  "Examine a different file."
     ,(lc (open-file p (ask (s+ ":" c #\space)))))
   `("next"	  (#\n)	    "Examine the next file in the arguments."
     ,(lc (next-file p)))
   `("previous"	  (#\p)	    "Examine the previous file in the arguments."
     ,(lc (previous-file p)))
   `("help"	  (#\h)	    "Show the pager help."
     ,(lc (help p)))
   `("?"	  (#\?)	    "Show this long command help."
     ,(lc (command-help p)))
   `("file"	  (#\f)	    "Show the file information."
     ,(lc (show-info p)))
   `("quit"	  (#\q)	    "Quit the pager."
     ,(lc (quit p)))))
    #| (#\d (remove-file-from-list)) |#

(defun command-help (pager)
  "Show help on long commands, aka colon commands."
  (declare (ignore pager))
  (sub-page (s)
    (table:nice-print-table
     (loop :for c :across *long-commands*
	:collect (list (elt c 0) (elt c 1) (elt c 2)))
     '("Command" "Abbrev" "Description")
     :stream s)))

(defun read-command (pager)
  (let ((cmd (ask-for :prompt ":" :space-exits t)))
    (when (and cmd (stringp cmd))
      (let (found)
	(loop
	   :for c :across *long-commands*
	   :do (when (position (char cmd 0) (elt c 1))
		 (setf found t)
		 (funcall (elt c 3) pager cmd)))
	(when (not found)
	  (message "~a is an unknown command." cmd))))))

;; These bindings are basically "less" compatible, but are also an horrible
;; mishmash of emacs, vi, less, more, pg, etc. Just pandering to whatever
;; people are used to scrolling with.

(defkeymap *help-keymap*
  `(
    (#\c		. describe-key-briefly)
    (#\k		. describe-key)
    (#\?		. more-help)
    (#\q		. quit)
    ))

(defkeymap *normal-keymap*
  `((#\q		. quit)
    (#\Q		. quit)
    (#\^C		. quit)
    (#\^Z		. suspend)
    (#\space		. next-page)
    (:npage		. next-page)
    (#\^F               . next-page)
    (#\^V               . next-page)
    (#\return		. next-line)
    (:down		. next-line)
    (#\j		. next-line)
    (#\b		. previous-page)
    (:ppage		. previous-page)
    (#\rubout		. previous-page)
    (#\^B		. previous-page)
    (,(meta-char #\v)   . previous-page)
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
    (#\V                . show-version)
    (#\-		. set-option)
    (#\^L		. redraw)
    (,(meta-char #\u)	. clear-search)
    (,(meta-char #\n)	. next-file)
    (,(meta-char #\p)	. previous-file)
    (#\?		. help)
    (#\^H		. help-key)
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
    (#\&                . filter-lines)
    ))

(defparameter *escape-keymap* (build-escape-map *normal-keymap*))

(defun describe-key-briefly (pager)
  "Prompt for a key and say what function it invokes."
  (message "Press a key: ")
  (let* ((key (fui:get-char))
	 (action (key-definition key *normal-keymap*)))
    (if action
	(tmp-message pager "~a is bound to ~a" (nice-char key) action)
	(tmp-message pager "~a is not defined" (nice-char key)))))

(defun describe-key (pager)
  "Prompt for a key and describe the function it invokes."
  (message "Press a key: ")
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
       (tmp-message pager ""))
      (t
       (tmp-message pager "~a is not defined" (nice-char key))))))

(defun help-key (pager)
  "Sub-command for help commands."
  (message "Help (? for more help): ")
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

(defstruct suspended-pager
  "State of a suspended pager."
  pager
  file-list
  stream
  close-me)

(defun page (stream &optional pager file-list close-me suspended)
  "View a stream with the pager. Return whether we were suspended or not."
  (with-curses
    (unwind-protect
      (progn
	(when (and (not stream) file-list)
	  (setf stream (open (quote-filename (first file-list))
			     :direction :input)
		close-me t))
	(if (not pager)
	    (setf pager (make-instance 'pager
				       :stream stream
				       :page-size (1- curses:*lines*)
				       :file-list file-list))
	    (when (not suspended)
	      (freshen pager)
	      (setf (pager-stream pager) stream)))
	(with-slots (count line page-size left search-string input-char
		     file-list file-index message prefix-arg quit-flag
		     suspend-flag command) pager
	  (when file-list (setf file-index 0))
	  (read-lines pager page-size)
	  (setf quit-flag nil
		suspend-flag nil
		suspended nil)
	  (curses:raw)		; give a chance to exit during first read
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
	     :while (not (or quit-flag suspend-flag)))
	  (when suspend-flag
	    (setf suspended t)
	    (if (find-package :lish)
	      (let ((suspy (make-suspended-pager
			    :pager pager
			    :file-list file-list
			    :stream stream
			    :close-me close-me)))
		(funcall (find-symbol "SUSPEND-JOB" :lish)
			 "pager" "" (lambda () (pager:resume suspy)))
		(format t "Suspended.~%"))
	      (format t "No shell loaded to suspend to.~%")))))
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
       (let (stream suspended)
	 (unwind-protect
	   (progn
	     (setf stream (open (quote-filename file-or-files)
				    :direction :input)
		   suspended (page stream)))
	   (when (and stream (not suspended))
	     (close stream))))))))

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
	 (keymap:dump-keymap *normal-keymap* :stream output)
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
       :do (with-open-file (stream (quote-filename filename))
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
(lish:defcommand pager ((files pathname :repeating t))
  "Look through text, one screen-full at a time."
  (pager (or files *standard-input*)))

;; EOF
