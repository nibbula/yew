;;;
;;; pager.lisp - More or less like more or less.
;;;

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
  (:use :cl :dlib :opsys :dlib-misc :table-print :stretchy
	:keymap :char-util :fatchar #+use-regex :regex #-use-regex :ppcre
	:terminal :fatchar-io :pick-list :table-print :fui :inator :file-inator
	:terminal-inator :collections :ochar :theme :terminal-table :completion
	:result)
  (:export
   #:*pager-prompt*
   #:*empty-indicator*
   #:page
   #:page-thing
   ;; Main entry points
   #:with-pager
   #:with-pager*
   #:pager
   #:binary-pager
   #:resume
   #:browse
   #+lish #:!pager
   ))
(in-package :pager)

;; TODO:
;;  - sub-files for keep & filter ?
;;  - syntax highlighting
;;  - big stream issues?
;;    - direct read for files
;;    - option to use tmp file for pipe (disk vs mem tradeoff)

(define-constant +digits+ #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
  "For reading the numeric argument." 'equalp)

(defparameter *default-text-pager-prompt* "%&%f line %l of %L%&"
  "Default prompt for text.")

(defparameter *default-binary-pager-prompt* "%&%f byte %b of %B%&"
  "Default prompt for text.")

;; Since *prompt* is taken on some implementations.
(defvar *pager-prompt* nil
  "The current default prompt. Supports formatting as done by FORMAT-PROMPT.")

(defkeymap *help-keymap* ()
  `(
    (#\c		. describe-key-briefly)
    (#\k		. describe-key)
    (#\b		. describe-bindings)
    (#\?		. more-help)
    (#\q		. quit)
    ))

;; These bindings are basically "less" compatible, but are also an horrible
;; mishmash of emacs, vi, less, more, pg, etc. Just pandering to whatever
;; people are used to scrolling with.

(defkeymap *normal-keymap* ()
  `((#\q		. quit)
    (#\Q		. quit)
    (,(ctrl #\C)	. quit)
    (,(ctrl #\Z)	. suspend)
    (:scroll-up		. scroll-up)
    (:scroll-down	. scroll-down)
    (#\space		. next-page)
    (:npage		. next-page)
    (,(ctrl #\F)	. next-page)	; @@@ half-page ?
    (#\return		. next)
    (:down		. next)
    (#\j		. next)
    (#\b		. previous-page)
    (:ppage		. previous-page)
    (:page-up		. previous-page)
    (#\rubout		. previous-page)
    (,(ctrl #\B)	. previous-page) ; @@@ half-page ?
    (#\k		. previous)
    (:up		. previous)
    (#\l		. scroll-right)
    (:right		. scroll-right)
    (#\h		. scroll-left)
    (:left		. scroll-left)
    (,(ctrl #\A)	. scroll-beginning)
    (:home		. scroll-beginning)
    (,(ctrl #\E)	. scroll-end)
    (:end		. scroll-end)
    (#\<		. move-to-top)
    (#\g		. move-to-top)
    (#\>		. move-to-bottom)
    (#\G		. move-to-bottom)
    (#\/		. search-command)
    (,(ctrl #\R)	. search-backward-command)
    (#\n		. search-next)
    (#\p		. search-previous)
    (#\N		. search-previous)
    (#\=		. show-info)
    (,(ctrl #\G)	. show-info)
    (,(meta-char #\l)	. show-file-list)
    (#\i		. show-detailed-info)
    (#\V                . show-version)
    (#\-		. set-option)
    (#\R		. reread)
    (,(meta-char #\u)	. clear-search)
    (,(meta-char #\n)	. next-file-location)
    (,(meta-char #\p)	. previous-file-location)
    (,(meta-char #\N)	. next-file)
    (,(meta-char #\P)	. previous-file)
    (,(meta-char #\+)	. set-key-command)
    (#\?		. help)
    (,(ctrl #\H)	. help-key)
    (:backspace		. help-key)
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
    (#\!		. shell-command)
    (,(meta-char #\x)	. eval-expression-command)
    (,(meta-char #\esc)	. eval-expression-command)
    (#\^                . keep-lines)
    (#\&                . filter-lines)
    (#\*                . filter-more)
    ))

(defparameter *escape-keymap* (build-escape-map *normal-keymap*))
;; (add-keymap *default-file-inator-escape-keymap* *escape-keymap*)

;; "text text text"
;; ((:tag "text") "text" (:tag "text") "text")
;; (:tag "text")
;; ("text text " (:tag "text " (:tag "text") " text") "text")

(defstruct line
  "Hold a line of text."
  number				; line number
  position				; file-position
  prev					; previous line
  text)					; text string of the line

(defclass pager (terminal-inator file-inator)
  ((stream
    :initarg :stream :accessor pager-stream :initform nil
    :documentation "Input stream")
   (left
    :initarg :left :accessor pager-left :initform 0
    :documentation "Column which display starts at")
   (max-width
    :initarg :max-width :accessor pager-max-width :initform 0
    :documentation "Maximum line length.")
   (page-size
    :initarg :page-size :accessor pager-page-size
    :documentation "How many lines in a page.")
   (got-eof
    :initarg :got-eof :accessor pager-got-eof :initform nil
    :documentation "True if we got to the end of stream")
   (seekable
    :initarg :seekable :accessor pager-seekable :initform nil :type boolean
    :documentation "True if the stream is seekable.")
   (binary
    :initarg :binary :accessor pager-binary :initform nil :type boolean
    :documentation "True for binary mode.")
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
    :initarg :file-index :accessor pager-file-index :initform 0
    :documentation "Where we are in the file list")
   (message
    :initarg :message :accessor pager-message :initform nil
    :documentation "Message to display until next input")
   (prefix-arg
    :initarg :prefix-arg :accessor pager-prefix-arg :initform nil
    :documentation "Common numeric argument for commands")
   (suspend-flag
    :initarg :suspend-flag :accessor pager-suspend-flag :initform nil
    :documentation "true to suspend the pager")
   (input-char
    :initarg :input-char :accessor pager-input-char :initform nil
    :documentation "the current input character")
   ;; Styles
   (search-match-char
    :initarg :search-match-char :accessor pager-search-match-char
    :initform (make-fatchar :c #\x :attrs '(:standout))
    :documentation
    "Cached style for displaying search matches. Read from theme value
(:program :search-match :style)")
   ;; @@@ actually it's probably just better to set *pager-prompt*
   (modeline-style
    :initarg :modeline-style :accessor pager-modeline-style
    :initform '(:standout) :type list
    :documentation
    "Cached style for displaying the mode line. Read from theme value
(:program :modeline :style).")
   (empty-indicator-char
    :initarg :empty-indicator-char :accessor empty-indicator-char
    :initform #\~ :type character
    :documentation "Character to indicate empty lines. Read from theme value
(:program :empty-line-indicator :character)")
   (empty-indicator-style
    :initarg :empty-indicator-style :accessor empty-indicator-style
    :initform '(:normal) :type list
    :documentation "Style for the character to indicating empty lines. Read
from theme value (:program :empty-line-indicator :style)")
   ;; options
   (options
    :initarg :options :accessor pager-options :initform nil
    :documentation "Option list passed in.")
   (show-line-numbers
    :initarg :show-line-numbers :accessor pager-show-line-numbers
    :initform nil :type boolean
    :documentation "True to show line numbers along the left side.")
   (show-modeline
    :initarg :show-modeline :accessor -show-modeline :initform t :type boolean
    :documentation "True to show the mode line.")
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
   (color-bytes
    :initarg :color-bytes :accessor pager-color-bytes
    :initform nil :type boolean
    :documentation "True for color bytes mode."))
  (:default-initargs
   :keymap `(,*normal-keymap* ,*default-file-inator-keymap*))
  (:documentation "An instance of a pager."))

(defclass text-pager (pager)
  ((lines
    :initarg :lines :accessor pager-lines :initform '()
    :documentation "List of lines")
   (count
    :initarg :count :accessor pager-count :initform 0
    :documentation "Number of lines")
   (line
    :initarg :line :accessor pager-line :initform 0
    :documentation "Current line at the top of the screen.")
   (missing-newline
    :initarg :missing-newline :accessor missing-newline
    :initform nil :type boolean
    :documentation "True if the last line was missing a newline."))
  (:documentation "Text pager."))

(defun make-empty-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defclass binary-pager (pager)
  ((buffer
    :initarg :buffer :accessor pager-buffer
    :initform (make-empty-buffer)
    :type (vector (unsigned-byte 8))
    :documentation "Buffer for binary mode.")
   (buffer-start
    :initarg :buffer-start :accessor pager-buffer-start :initform 0 :type fixnum
    :documentation "The byte position in the file of the start of the buffer.")
   (byte-count
    :initarg :byte-count :accessor pager-byte-count :initform 0 :type fixnum
    :documentation "Number of bytes we've read.")
   (byte-pos
    :initarg :byte-pos :accessor pager-byte-pos :initform 0 :type fixnum
    :documentation "The byte position we're at in the file.")
   ;; (start-offset
   ;;  :initarg :start-offset :accessor binary-pager-start-offset
   ;;  :initform 0 :type fixnum
   ;;  :documentation
   ;;  "The offset of the start of the buffer, so it acts like a ring buffer.")
   )
  (:documentation "Pager for binary data."))

(defvar *pager* nil
  "The current pager.")

(defmethod initialize-instance
    :after ((o pager) &rest initargs &key &allow-other-keys)
  "Set up the slots in a pager."
  (declare (ignore initargs))
  ;; slot initialization, such as:
  (when (not (slot-boundp o 'page-size))
    (setf (pager-page-size o) (1- (tt-height))))
  (when (pager-stream o)
    (setf (pager-seekable o) (seekable-p (pager-stream o)))))

(defgeneric freshen (o)
  (:documentation
   "Make something fresh. Make it's state like it just got initialized,
but perhaps reuse some resources."))

(defmethod freshen ((o pager))
  "Make the pager like new."
  (setf ;; (pager-stream o) nil
	(pager-left o) 0
	(pager-max-width o) 0
	;; (pager-page-size o) 0
	(pager-got-eof o) nil
	;; (pager-seekable o) nil
	(pager-binary o) nil
	(pager-search-string o) nil
	(pager-filter-exprs o) nil
	(pager-keep-expr o) nil
	(pager-mutate-function o) nil
	;; (pager-file-list o) nil
	;; (pager-index o) nil
	(pager-message o) nil
	(pager-prefix-arg o) nil
	(inator-quit-flag o) nil
	(pager-suspend-flag o) nil
	(pager-input-char o) nil
	(inator-command o) nil
	;; (pager-last-command o) nil
	)
  ;; Don't reset options?
  )

(defmethod freshen ((o text-pager))
  (call-next-method)
  (setf (pager-lines o) nil
	(pager-count o) 0
	(pager-line o) 0
	(missing-newline o) nil))

(defmethod freshen ((o binary-pager))
  (call-next-method)
  (setf (pager-buffer o) (make-empty-buffer)
	(pager-buffer-start o) 0
	(pager-byte-count o) 0
	(pager-byte-pos o) 0))

(defmacro sub-page ((stream-var) &body body)
  "Generate some output and run a sub-instance of the pager on it."
  ;;(declare (ignore stream-var)) ; @@@
  (let ((input (gensym "SUB-PAGE-INPUT")))
    `(with-input-from-string
	 (,input
	  ;; (with-output-to-string (,stream-var)
	  (with-terminal-output-to-string (:ansi)
	    (let ((,stream-var *terminal*))
	      (declare (ignorable ,stream-var))
	      ,@body)))
       (pager ,input))))

#|
(defun nop-process-line (line)
  "A line processor that doesn't do anything."
  line)
|#

(defun process-grotty-line (line)
  "Convert from grotty typewriter sequences to fatchar strings."
  (when (= (length line) 0)
    (return-from process-grotty-line line))
  (let ((fat-line (make-stretchy-vector (+ (length line) 16)
					:element-type 'fatchar))
	(i 0) (fi 0) (len (length line))
	c attrs prev-char overwrite-char)
    (flet ((do-backspace ()
	     (setf overwrite-char prev-char
		   prev-char #\backspace))
	   (add-char ()
	     (setf c (char line i)
		   attrs nil
		   overwrite-char nil
		   prev-char c)))
      (loop
	 :do
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

	   ;; A bold underscore character
	   ((and (< i (- len 3))
		 (char= (char line i) #\_)
		 (char= (char line (+ i 1)) #\backspace)
		 (char= (char line (+ i 2)) #\_))
	    (incf i 2)
	    (pushnew :bold attrs)
	    (setf c (char line i)))

	   ;; Underline
	   ((and (< i (- len 2))
		 (char= (char line (+ i 1)) #\backspace)
		 (or (char= (char line i) #\_)
		     (char= (char line (+ i 2)) #\_)))
	    (cond
	      ;; forward underline
	      ((char= (char line i) #\_)
	       (incf i 2)
	       (setf c (char line i))
	       (pushnew :underline attrs))
	      ;; backward underline
	      ((char= (char line (+ i 2)) #\_)
	       (setf c (char line i))
	       (incf i 2)
	       (pushnew :underline attrs)))

	    ;; underline and bold
	    (when (and (< i (- len 2))
		       (find :underline attrs)
		       (char= (char line (+ i 2)) (char line i)))
	      (incf i 2)
	      (pushnew :bold attrs)))

	   ;; Overwrite
	   ((char= (char line i) #\backspace)
	    (do-backspace))

	   ;; Bold
	   ((and prev-char (char= prev-char #\backspace)
		 overwrite-char (char= overwrite-char (char line i)))
	    (pushnew :bold (fatchar-attrs (aref fat-line (1- fi))))
	    (setf prev-char overwrite-char))

	   ;; Normal
	   (t
	    (add-char)))
	 (when c
	   (stretchy:stretchy-append
	    fat-line (make-fatchar :c c :attrs (copy-seq attrs)))
	   (incf fi)
	   (setf c nil))
	 (incf i)
	 :while (< i len)))
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
	     (setf fc (copy-fatchar c)
		   (fatchar-c fc) #\^)
	     (stretchy-append fat-line fc)
	     (setf fc (copy-fatchar c)
		   (fatchar-c fc) the-char)
	     (stretchy-append fat-line fc)))
      (loop :with cc
	 :for c :across line :do
	 (setf cc (char-code (fatchar-c c)))
	 (cond
	   ;; ((< cc (char-code #\space))
	   ;;  (setf fc (copy-fatchar c)
	   ;; 	(fatchar-c fc) #\^)
	   ;;  (stretchy-append fat-line fc)
	   ;;  (setf fc (copy-fatchar c)
	   ;; 	(fatchar-c fc) (code-char (+ cc (char-code #\@))))
	   ;;  (stretchy-append fat-line fc))
	   ;; ((= cc 127)
	   ;;  (setf fc (copy-fatchar c)
	   ;; 	(fatchar-c fc) #\^)
	   ;;  (stretchy-append fat-line fc)
	   ;;  (setf fc (copy-fatchar c)
	   ;; 	(fatchar-c fc) #\?)
	   ;;  (stretchy-append fat-line fc))
	   ((< cc (char-code #\space))
	    (ctrl-out c (code-char (+ cc (char-code #\@)))))
	   ((= cc 127)
	    (ctrl-out c #\?))
	   (t
	    (stretchy-append fat-line (copy-fatchar c))))))
    fat-line))

(defun process-line (line)
  "Process a line of text read from a stream."
  (let ((output (fatchar-string-to-span
		 (process-control-characters
		  (process-ansi-colors
		   (process-grotty-line
		    (untabify line)))))))
    (if (and (= (length output) 1) (stringp (first output)))
	(first output)
	output)))

(defgeneric read-input (pager count)
  (:documentation
   "Read new input from the stream. Stop after COUNT units. If COUNT is zero,
read until we get an EOF."))

(defmethod read-input ((pager text-pager) line-count)
  "Read new lines from the stream. Stop after COUNT lines. If COUNT is zero,
read until we get an EOF."
  (with-slots (got-eof count stream raw-output missing-newline) pager
    (handler-case
	(progn
	  (when (not got-eof)
	    (let ((n 0)
		  (i count)
		  line last-line cur-line
		  (lines '())
		  (pos (or (and (not got-eof) (file-position stream)) 0)))

	      (flet ((make-the-line ()
		       (setf cur-line
			     (make-line :number i
					;; :position (incf pos (length line))
					:position pos
					:prev last-line
					:text (if raw-output
						  line
						  (process-line line))))
		       ;; (dbugf :pager "cur-line ~s~%" cur-line)
		       (push cur-line lines)
		       (setf last-line cur-line)))
		(unwind-protect
		   (loop
		      :while (and (or (< n line-count) (zerop line-count))
				  (setf (values line missing-newline)
					;;(resilient-read-line stream nil nil)))
					(read-line stream nil nil)))
		      :do
		      (make-the-line)
		      (incf pos (length line))
		      (incf i)
		      (incf n)
		      (setf cur-line nil))
		(cond
		  ((not line)
		   ;; (dbugf :pager "got eof i=~s pos=~s~%" i pos)
		   (setf got-eof t))
		  ((and (not cur-line) (not (>= n line-count)))
		   ;; We got an EOF but we got a partial line
		   ;; (dbugf :pager "partial line ~s~%" line)
		   (make-the-line)
		   (incf i)
		   ;; (setf missing-newline (and cur-line line t))
		   ))
		(when lines
		  (let ((last-line
			 (car (last lines)))) ;; really first of the chunk
		    (setf
		     ;; Set the previous of the new chunk to be the end of rest
		     (line-prev last-line) (car (last (pager-lines *pager*)))
		     ;; and then reverse and attach them.
		     (pager-lines *pager*) (nconc (pager-lines *pager*)
						  (nreverse lines))
		     count i))))))))
      (end-of-file (c)
	;; Be quiet about EOFs. Some systems seem to get them more than others.
	(declare (ignore c))
	;; (dbugf :pager "eof signaled~%")
	(setf got-eof t))
      (stream-error (c)
	(setf got-eof t)
	(message pager "Got an error: ~a on the stream." c)))
    ;;(dbugf :pager "done and got eof ~s ~s~%" got-eof count)
    ))

(defmethod read-input ((pager binary-pager) line-count)
  "Read new lines from the stream. Stop after COUNT lines. If COUNT is zero,
read until we get an EOF."
  (declare (ignore line-count)) ;; @@@
  (with-slots (got-eof count stream raw-output seekable
	       buffer buffer-start byte-count byte-pos) pager
    ;; (setf seekable (seekable-p))
    (handler-case
	(progn
	  (let* ((read-count 0)
		 (read-size (* (line-bytes) (1- (tt-height))))
		 (file-len (file-length stream))
		 (pos (or (file-position stream)
			  (clamp byte-pos 0 file-len)))
		seek-succeeded)
	    ;; (dbugf :pager "read-size ~s pos ~s~%" read-size pos)
	    (when (not seekable)
	      (error "Unseekable binary streams not implemented yet."))
	    (when (< (length buffer) read-size)
	      ;; (dbugf :pager "adjusting to ~s~%" read-size)
	      (setf buffer (adjust-array buffer read-size)))
	    (when (/= byte-pos pos)
	      (clampf byte-pos 0 (max 0 (- file-len read-size)))
	      ;; (dbugf :pager "seeking to ~s~%" byte-pos)
	      (setf seek-succeeded (file-position stream byte-pos))
	      (assert seek-succeeded))
	    (unwind-protect
		 (progn
		   ;; (when (zerop line-count)
		   ;;   @@@ read the whole rest of an unseekable stream into
		   ;;   the buffer
		   ;;   )
		   (setf buffer-start byte-pos
			 read-count (read-sequence buffer stream))
		   (setf byte-count file-len)
		   ;; (dbugf :pager "byte-count ~s read-count ~s stream ~s~%"
		   ;; 	  byte-count read-count stream)
		   (when (< read-count (length buffer))
		     (setf got-eof t)
		     ;; (setf buffer-start (- byte-count read-count))
		     )
		   ;; @@@ process it ?
		   ;; (if raw-output
		   ;; 	 line
		   ;; 	 (process-line line))
		   ;; (setf byte-pos (+ buffer-start (length buffer)))
		   ))))
      (end-of-file (c)
	;; Be quiet about EOFs. Some systems seem to get them more than others.
	(declare (ignore c))
	(setf got-eof t))
      (stream-error (c)
	(setf got-eof t)
	(message pager "Got an error: ~a on the stream." c)))))

(defun stream-name (stream)
  (cond
    ((typep stream 'utf8b-stream:utf8b-input-stream)
     (stream-name (utf8b-stream:utf8b-stream-stream stream)))
    ((and (typep stream 'file-stream)
	  (ignore-errors (truename stream)))
     (princ-to-string (namestring (truename stream))))
    ((eq stream *standard-input*)
     (princ-to-string '*standard-input*))
    (t
     (princ-to-string stream))))

(defun stream-file-p (stream)
  (cond
    ((typep stream 'utf8b-stream:utf8b-input-stream)
     (stream-file-p (utf8b-stream:utf8b-stream-stream stream)))
    ((and (typep stream 'file-stream)
	  (ignore-errors (truename stream))))))

(defgeneric bytes-current (pager)
  (:documentation "Return bytes at current position.")
  (:method ((pager text-pager))
    (with-slots (line lines) pager
      (let ((l (nth line lines)))
	(if l (line-position l) "?"))))
  (:method ((pager binary-pager))
    ;; (+ (pager-buffer-start pager) (pager-byte-pos pager))))
    (pager-byte-pos pager)))

(defgeneric total-bytes (pager)
  (:documentation "Return total bytes.")
  (:method ((pager text-pager))
    (let ((l (ignore-errors (file-length (pager-stream pager)))))
      (or l "?")))
  (:method ((pager binary-pager))
    (pager-byte-count pager)))

(defgeneric current-line (pager)
  (:documentation "Return the current line number.")
  (:method ((pager text-pager))
    (pager-line pager))
  (:method ((pager binary-pager))
    (round (/ (bytes-current pager) (line-bytes)))))

(defgeneric maximum-line (pager)
  (:documentation "Return the maximum line.")
  (:method ((pager text-pager))
    (pager-count pager))
  (:method ((pager binary-pager))
    (round (/ (total-bytes pager) (line-bytes)))))

(defgeneric percentage (pager)
  (:documentation "Return the current percentage in the file.")
  (:method ((pager text-pager))
    (with-slots (line page-size count) pager
      (round (/ (* (min (+ line page-size) count) 100)
		count))))
  (:method ((pager binary-pager))
    (with-slots (byte-count) pager
      (round (/ (* (bytes-current pager) 100)
		byte-count)))))

(defun format-prompt (pager &optional prompt)
  "Return the prompt string with a few less-like formatting character
replacements. So far we support:
  %b / %B  Bytes at current postion / Total bytes
  %l / %L  Current line / Maximum line
  %p       Percentage line
  %f       File name
  %%       A percent character '%'.
"
  (with-slots (stream count page-size filter-exprs keep-expr)
      pager
    (with-output-to-string (str)
      (loop :with c :for i :from 0 :below (olength prompt) :do
	 (setf c (oaref prompt i))
	 (if (ochar= c #\%)
	     (progn
	       (incf i)
	       (when (< i (olength prompt))
		 (setf c (oaref prompt i))
		 (case c
		   ;; @@@ The next two are very inefficient and probably
		   ;; should be cached.
		   (#\b (princ (bytes-current pager) str))
		   (#\B (princ (total-bytes pager) str))
		   (#\l (princ (current-line pager) str))
		   (#\L (princ (maximum-line pager) str))
		   (#\p (princ (percentage pager) str))
		   (#\% (princ #\% str))
		   (#\f (write-string (stream-name stream) str))
		   (#\&
		    (when filter-exprs (princ " && " str))
		    (when keep-expr    (princ " ^^ " str))))))
	     (princ c str))))))

(defun symbolic-prompt-to-string (pager symbolic-prompt #| &optional ts-in |#)
  "Take a symbolic prompt and turn it into a string. A symbolic prompt can be
any printable lisp object, which is converted to a string. If it is a list, it
translates sublists starting with certain keywords, to terminal codes to do
text effects to the enclosed objects. The keywords recognized are:
  :BOLD :UNDERLINE :INVERSE
and the colors
  :BLACK :RED :GREEN :YELLOW :BLUE :CYAN :WHITE and :DEFAULT.
The colors can be prefixed by :FG- or :BG- for the foreground or background.
Symbols will be replaced by their value. Functions will be evaluated with
the primary result printed as a string."
  (fatchar:span-to-fat-string
   symbolic-prompt
   :filter (_ (format-prompt pager _))
   :unknown-func
   (lambda (x) ; eval is magic
     (cond
       ((and (listp x) (symbolp (car x)) (fboundp (car x)))
	(apply (car x) (cdr x)))
       ((symbolp x)
	(when (boundp x)
	  (symbol-value x)))
       (t (princ-to-string x))))))

(defun display-prompt (pager)
  "Put the prompt at the appropriate place on the screen."
  (with-slots (modeline-style) pager
    (tt-move-to (1- (tt-height)) 0)
    (tt-erase-to-eol)
    (tt-normal)
    ;; (tt-write-span `(,@modeline-style ,(format-prompt pager)))
    (let ((prompt-string
	   (symbolic-prompt-to-string
	    pager
	    (or *pager-prompt*
		(typecase pager
		  (binary-pager *default-binary-pager-prompt*)
		  (t *default-text-pager-prompt*))))))
      (tt-write-span `( ; ,@modeline-style
		       ,@(when (not *pager-prompt*)
			   modeline-style)
		       ,(osubseq prompt-string
				 0 (min (tt-width) (olength prompt-string)))))
      (tt-normal))))

(defun display-message (format-string &rest args)
  "Display a formatted message at the last line immediately."
  (with-slots (modeline-style) *pager*
    ;; @@@ now you have two problems
    (tt-move-to (- (terminal-window-rows *terminal*)
		 ;; @@@ This is such a kludgey hack
		   (length (calculate-line-endings
			    (apply #'format nil format-string args)
			    0 (tt-width) nil nil nil))
		   1)
		0)
    ;; (tt-move-to (1- (tt-height)) 0)
    (tt-erase-to-eol)
    (tt-write-span `(,@modeline-style ,(apply #'format nil format-string args)))
    (tt-finish-output)))

(defun message-pause (format-string &rest args)
  "Print a formatted message at the last line and pause until a key is hit."
  (apply #'display-message format-string args)
  (tt-get-char))

(defun message-pause-for (timeout format-string &rest args)
  "Print a formatted message at the last line and pause until a key is hit.
Wait for TIMEOUT milliseconds before returning an error (-1).
This assumes there is no timeout (i.e. timeout is -1) before this is called,
and resets it to be so afterward."
  (apply #'display-message format-string args)
  (let (c)
    (when (tt-listen-for (/ timeout 100))
      (setf c (tt-get-char)))
    c))

(defmethod message ((pager pager) format-string &rest args)
  "Display a message at next command loop, until next input."
  (setf (pager-message pager) (apply #'format nil format-string args)))

(defmethod prompt ((pager pager) format-string &rest args)
  "Display a short message, asking the user for input."
  (apply #'display-message format-string args))

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
				  (< (- *col* *left*) (tt-width))))
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
  (with-slots (search-string ignore-case search-match-char) *pager*
    (let* ((s (fatchar-string-to-string *fat-buf*))
	   (ss (if (and search-string ignore-case)
		   (s+ "(?i)" search-string)
		   search-string))
	   (matches (and ss (all-matches ss s))))
      (loop :with i = 0
	 :while (< i (length matches)) :do
	 (loop :for j :from (elt matches i) :below (elt matches (1+ i))
	    ;; :do (pushnew :standout (fatchar-attrs (aref *fat-buf* j))))
	    :do (copy-fatchar-effects search-match-char (aref *fat-buf* j)))
	 (incf i 2)))))

(defun show-span (s)
  (when (not s)
    ;; (return-from show-span 0)
    ;; But now empty lines come over as NIL, so:
    (tt-write-char #\newline)
    (return-from show-span 1))
  (with-slots (wrap-lines search-string) *pager*
    (if wrap-lines
	(span-to-fatchar-string s :fatchar-string *fat-buf* :start *left*)
	(span-to-fatchar-string s :fatchar-string *fat-buf* :start *left*
				:end (+ *left* (tt-width))))
    (when search-string
      (search-a-matize))
    (let ((len (display-length *fat-buf*)))
      ;; (if (= len (tt-width))
      ;; 	  (tt-write-string (make-fat-string :string *fat-buf*)))
      ;; 	  (tt-write-line (make-fat-string :string *fat-buf*))
      (tt-write-string (make-fat-string :string *fat-buf*))
      ;; @@@ or something? minus the line numbers
      ;; (max 1 (ceiling (length *fat-buf*) (tt-width)))
      ;; (max 1 (ceiling (display-length *fat-buf*) (tt-width)))
      (max 1 (ceiling len (tt-width))))))

(defun pager-render-span (line-number line)
  (with-slots (left show-line-numbers) *pager*
    (let ((*col* 0) (*left* left))
      (when show-line-numbers
	(let ((str (format nil "~d: " line-number)))
	  (incf *col* (length str))
	  (tt-write-string str)))
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
    (and (all-matches expr line) t)))

(defun filter-this (line)
  "Return true if we should filter this line."
  (with-slots (filter-exprs keep-expr) *pager*
    (when (or filter-exprs keep-expr)
      (typecase (line-text line)
	(string
	 (when (and keep-expr
		    (not (all-matches keep-expr (line-text line))))
	   (return-from filter-this t))
	 (loop :for e :in filter-exprs
	    :if (all-matches e (line-text line))
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
    (string (pager-render-span line-number (list (line-text line))))
    (list   (pager-render-span line-number (line-text line)))
    (t (error "Don't know how to render a line of ~s~%"
	      (type-of (line-text line))))))

(defun simplify-line (line)
  (typecase (line-text line)
    (null             "")		; NIL means a blank line?
    (string           (line-text line))
    (fatchar-string   (fatchar-string-to-string (line-text line)))
    (list             (fatchar-string-to-string
		       (span-to-fatchar-string (line-text line))))
    (t (error "Don't know how to simplify a line of ~s~%"
	      (type-of (line-text line))))))

(defvar *empty-indicator* "~"
  "String that indicates emptyness, usually past the end.")

;; @@@ This could use a macro or two.
;; Something like:
;;   (theme:theme-settings ((var (:theme :path :thing) (:default-value))
;;                          (...)))
;; and/or maybe:
;;   (theme:theme-bindings ((var (:theme :path :thing) (:default-value))
;;                          (...)))

(defun get-theme-settings (pager)
  (with-slots (modeline-style search-match-char empty-indicator-style
	       empty-indicator-char) pager
    (setf modeline-style
	  (or (theme-value *theme* '(:program :modeline :style))
	      '(:standout))

	  search-match-char
	  (aref (span-to-fatchar-string
		 `(,@(theme-value *theme*
				 '(:program :search-match :style)) #\x)) 0)
	  empty-indicator-style
	  (or (theme-value *theme*
			   '(:program :empty-line-indicator :style))
	      '(:normal))

	  empty-indicator-char
	  (or (theme-value *theme*
			   '(:program :empty-line-indicator :character))
	      #\~))))

(defmethod update-display ((pager text-pager))
  "Display the lines already read, starting from the current."
  (with-slots (line lines left page-size message modeline-style show-modeline
	       search-match-char empty-indicator-char empty-indicator-style
	       missing-newline count)
      pager
    (get-theme-settings pager)
    (tt-move-to 0 0)
    (tt-erase-below)
    (let ((y 0)
	  (bottom page-size)
	  (i (1+ line))
	  l #| last-line |#)
      (when (and (>= line 0) lines)
	(setf l (nthcdr line lines))
	(loop
	   :while (and (< y bottom) (car l))
	   :do
	     (tt-move-to y 0)
	     ;; janky filtering
	     (when (not (filter-this (car l)))
	       (incf y (display-line i (car l)))
	       (incf i))
	     (setf #| last-line l |#
		   l (cdr l))))
      ;; (dbugf :pager "final y = ~s~%" y)
      ;; Fill the rest of the screen with twiddles to indicate emptiness.
      ;; (when (and missing-newline last-line (= (1- i) count))
      ;; 	(tt-move-to (1- y) 0)
      ;; 	;; Since we didn't save the rendered width, we have to display it
      ;; 	;; again.
      ;; 	(display-line i (car last-line))
      ;; 	(tt-write-span `(,@empty-indicator-style ,empty-indicator-char)))
      (when (< y page-size)
	(loop :for i :from y :below page-size
	   :do
	     (tt-move-to i 0)
	     (tt-write-span `(,@empty-indicator-style ,empty-indicator-char))))
      (if message
	  (progn
	    (display-message message)
	    (setf message nil))
	  (when show-modeline
	    (display-prompt pager))))))

(defun hex-len ()
  ;;(- (truncate (* 3/4 (tt-width))) 4)
  ;;(- (truncate (/ 3/4 (/ 1 (tt-width)))) 4)
  ;;(- (truncate (/ 3/4 (/ 1 (tt-width)))) 4)
  (* (line-bytes) 3)
  )

(defun line-bytes ()
  ;;(1- (truncate (/ 3/4 (/ 1 (tt-width)) 3))))
  (truncate (/ 3/4 (/ 1 (tt-width)) 3)))

(defun search-bytes (string buffer &key start2 from-end)
  "Return the position of ‘string’ in a byte array ‘buffer’. Respects the
‘ignore-case’ option of the pager. ‘string’ can be a character string or a
vector of character codes, which are interpreted as UTF8B octets. ‘start2’ is
the position to start at in ‘buffer’. If ‘from-end’ is true, search from the
end."
  (when (not start2)
    (setf start2 0))
  (let ((byte-string
	  (typecase string
	    (string
	     (unicode:string-to-utf8b-bytes string))
	    (vector ;; assume it's a (array (unsigned-byte 8))
	     string))))
    (cond
      ((pager-ignore-case *pager*)
       (search byte-string buffer
	       :test (lambda (x y) (or (eql x y)
				       (eql (char-upcase (code-char x))
					    (char-upcase (code-char y)))))
	       :start2 start2 :from-end from-end))
      (t
       (search byte-string buffer :start2 start2 :from-end from-end)))))

(defun search-byte-ranges (pager string)
  "Return byte ranges in the page which contain the ‘string’."
  (when (and string (not (zerop (length string))))
    (with-slots (byte-pos buffer buffer-start) pager
      (let ((byte-str (unicode:string-to-utf8b-bytes string)))
	(loop
	  :with pos = 0
	  :while (setf pos (search-bytes byte-str buffer :start2 pos))
	  :collect (cons pos (min (+ pos (1- (length byte-str)))
				  (length buffer)))
	  :do (incf pos (length byte-str)))))))

(defun in-ranges (i ranges)
  "Return true if ‘i’ is in ‘ranges’."
  (loop :for r :in ranges
    :when (and (>= i (car r)) (<= i (cdr r)))
    :do (return t)))

(defmethod update-display ((pager binary-pager))
  "Display the lines already read, starting from the current."
  (tt-move-to 0 0)
  (tt-erase-below)
  (with-slots (byte-pos buffer buffer-start left page-size color-bytes message
	       show-modeline search-string search-match-char)
      pager
    (get-theme-settings pager)
    (let ((y 0)
	  (len 0)
	  (hex-len (hex-len))
	  (printable (make-fat-string-output-stream))
	  (ranges (search-byte-ranges pager search-string))
	  (in-search nil))
      (assert (>= byte-pos buffer-start))
      (loop
	 :for i :from (- byte-pos buffer-start) :below (length buffer)
	 :do
	   ;; @@@ implement offset
	   ;; (when (and show-offset (zerop len))
	   ;;   )
	   (cond
	     ((and ranges (setf in-search (in-ranges i ranges)))
	      (tt-color (fatchar-fg search-match-char)
			(fatchar-bg search-match-char)))
	     (color-bytes
	      (tt-color (if (> (aref buffer i) 128)
			    :black
			    :white)
			(vector :gray8 (aref buffer i))))
	     (t
	      (tt-color :green :black)))
	   (tt-format "~2,'0x " (aref buffer i))
	   (incf len 3)
	   (princ (if (and (graphic-char-p (code-char (aref buffer i)))
			   (= (display-length (code-char (aref buffer i))) 1))
		      (make-fatchar :c (code-char (aref buffer i))
				    :fg (if in-search :red :white))
		      (make-fatchar :c #\. :fg :blue))
		  printable)
	   (when (>= len hex-len)
	     ;; (tt-color :white :black)
	     (loop :for c :across (get-output-stream-fat-string printable) :do
		  ;; (tt-color (if (eql c #\.) :blue :white) :black)
		  (tt-write-char c)
		  (incf len))
	     (incf y)
	     (when (< len (tt-width))
	       (tt-write-char #\newline))
	     (file-position printable :start)
	     (setf len 0))
	 :while (< y page-size))
      ;; When we're not a the beginning of a line, print the remaining printable
      (when (and (< y page-size) (/= len 0))
	;; (loop :while (<= len (+ hex-len (- 3 (mod hex-len 3))))
	(loop :while (< len hex-len)
	   :do (incf len) (tt-write-char #\space))
	(loop :for c :across (get-output-stream-fat-string printable) :do
	     (tt-color (if (eql c #\.) :blue :white) :black)
	     (tt-write-char c))
	(when (< len (1- (tt-width)))
	  (tt-write-char #\newline))
	(incf y))
      ;; Fill the rest of the screen with twiddles to indicate emptiness.
      (when (< y page-size)
	(loop :for i :from y :below page-size
	   :do
	   (tt-move-to i 0)
	     (tt-write-string *empty-indicator*)))
      ;; mode line
      (if message
	  (progn
	    (display-message message)
	    (setf message nil))
	  (when show-modeline
	    (display-prompt pager))))))

(defmethod resize ((pager text-pager))
  "Resize the page and read more lines if necessary."
  (with-slots (page-size line count show-modeline) pager
    (let ((normal-page-size (if show-modeline (1- (tt-height)) (tt-height))))
      (when (/= page-size normal-page-size)
	(setf page-size normal-page-size)
	;;(message-pause "new page size ~d" page-size)
	(when (< count (+ line page-size))
	  ;;(message-pause "read lines ~d" (- (+ line page-size) count))
	  (read-input pager (- (+ line page-size) count)))))))

(defmethod resize ((pager binary-pager))
  "Resize the page and read more lines if necessary."
  (with-slots (page-size byte-count byte-pos show-modeline) pager
    (let ((normal-page-size (if show-modeline (1- (tt-height)) (tt-height))))
      (when (/= page-size normal-page-size)
	(setf page-size normal-page-size)
	;; (when (< byte-count (+ byte-pos (* page-size (line-bytes))))
	;;  	(read-input pager (- (+ byte-pos (* page-size (line-bytes)))
	;; 			     (/ byte-count (line-bytes)))))
	(read-input pager (+ byte-pos (* page-size (line-bytes))))
	))))

(defun ask-for (&key prompt space-exits)
  (let ((str (make-stretchy-string 10))
	(esc-count 0))
    (loop :with c :and done
       :do
       (tt-move-to (1- (tt-height)) 0)
       (tt-erase-to-eol)
       (tt-write-string prompt) (tt-write-string str)
       (tt-finish-output)
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
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-finish-output)
  (prog1
      (rl:rl :prompt prompt
	     :accept-does-newline nil
	     :history-context :pager)
    (setf (tt-input-mode) :char)))

(defun ask-for-file (&optional prompt)
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-finish-output)
  (prog1
      (rl:rl :prompt prompt
	     :completion-func #'complete-filename
	     :history-context :read-filename
	     :accept-does-newline nil)
    (setf (tt-input-mode) :char)))

(defun ask-for-function (&optional prompt)
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-finish-output)
  (let* ((str (rl:rl :prompt prompt
		     :completion-func #'complete-symbol
		     :history-context :ask-function-name
		     :accept-does-newline nil))
	 (cmd (and str (stringp str)
		   (ignore-errors (safe-read-from-string str)))))
    (and (symbolp cmd) (fboundp cmd) cmd)))

(defun search-line (str line)
  "Return true if LINE contains the string STR. LINE can be a string, or a
list containing strings and lists."
  (typecase line
    (string
     (all-matches str line))
    (cons
     (all-matches str (fatchar-string-to-string
		       (span-to-fatchar-string line))))))

(defgeneric search-for (pager str &key direction)
  (:documentation
   "Search for ‘str’. ‘direction’ can be :forward or :backward, defaulting to
:forward."))

(defmethod search-for ((pager text-pager) str &key (direction :forward))
  (with-slots (lines count line page-size ignore-case) *pager*
    (let ((search-regexp (if ignore-case (s+ "(?i)" str) str))
	  ll l)
      (case direction
	(:forward
	 (setf ll (nthcdr line lines))
	 (loop
	    :do
	    (setf l (car ll))
	    (when (and l (>= (line-number l) (max 0 (- count page-size))))
	      (read-input *pager* page-size))
	    :while (and l (< (line-number l) count)
			(not (search-line search-regexp (line-text l))))
	    :do
	    (setf ll (cdr ll)))
	 (if (and l (< (line-number l) count))
	     (setf line (line-number l))
	     nil))
	(:backward
	 (setf l (nth line lines))
	 (loop
	    :while (and l (>= (line-number l) 0)
			(not (search-line search-regexp (line-text l))))
	    :do
	    (setf l (line-prev l)))
	 (if l
	     (setf line (line-number l))
	     nil))
	(otherwise
	 (error "Search direction should be either :forward or :backward."))))))

(defmethod search-for ((pager binary-pager) str &key (direction :forward))
  (with-slots (buffer buffer-start byte-count byte-pos page-size ignore-case
	       got-eof)
      *pager*
    (let (;(search-regexp (if ignore-case (s+ "(?i)" str) str)))
	  (byte-str (unicode:string-to-utf8b-bytes str)))
      (case direction
	(:forward
	 (loop :with pos
	   :do
	      (when (setf pos (search-bytes byte-str buffer))
		(let* (#| (line-bytes (line-bytes)) |#
		       (new-pos
			 ;; (+ buffer-start
			 ;;    (* (1- (mod (+ buffer-start pos) line-bytes))
			 ;;       line-bytes))
			 ;; ))
			 (+ buffer-start pos)))
		  (setf byte-pos (clamp new-pos 0
					(+ buffer-start (length buffer))))
		  (read-input pager page-size))
		(return t))
	    :while (not got-eof)
	    :do
	       (setf byte-pos (+ buffer-start (length buffer)))
	       (read-input pager page-size)))
	(:backward
	 (loop
	   :with pos
	   :do
	      (previous-page pager)
	      (when (setf pos (search-bytes byte-str buffer :from-end t))
		(let* ((line-bytes (line-bytes))
		       (new-pos
			 (+ buffer-start
			    (* (1- (mod (+ buffer-start pos) line-bytes))
			       line-bytes))))
		  (setf byte-pos (clamp new-pos 0
					(+ buffer-start (length buffer)))))
		(return t))
	   :while (> byte-pos 0)))
	(otherwise
	 (error "Search direction should be either :forward or :backward."))))))

(defun keep-lines (pager)
  "Show only lines matching a regular expression."
  (let ((filter (ask "Show only lines matching: ")))
    (setf (pager-keep-expr pager)
	  (and filter (length filter) filter))))

(defun filter-lines (pager)
  "Hide lines matching a regular expression."
  (let ((filter (ask "Hide lines matching: ")))
    (setf (pager-filter-exprs pager)
	  (if filter (list filter) nil))))

(defun filter-more (pager)
  "Add to the list of regular expressions for hiding lines."
  (let ((filter (ask "Hide lines also mathing: ")))
    (when filter
      (push filter (pager-filter-exprs pager)))))

(defun set-option (pager)
  "Set a pager option. Propmpts for what option to toggle."
  (display-message "Set option: ")
  (with-slots (show-line-numbers show-modeline ignore-case wrap-lines raw-output
	       pass-special color-bytes page-size options stream seekable) pager
    (macrolet ((toggle-option (option)
		 `(progn
		    (setf ,option (not ,option))
		    (update-display *pager*)
		    (message pager "~(~a~) is ~:[Off~;On~]" ',option ,option)
		    (tt-finish-output))))
      ;; Shouldn't this be in a keymap holmes?
      (let ((char (tt-get-char)))
	(case char
	  ((#\l #\L) (toggle-option show-line-numbers))
	  ((#\m #\M) (toggle-option show-modeline)
	   (setf page-size (if show-modeline (1- (tt-height)) (tt-height))))
	  ((#\i #\I) (toggle-option ignore-case))
	  ((#\w #\S) (toggle-option wrap-lines)) ;; S for compatibility w/less
	  (#\r (toggle-option raw-output))
	  (#\p (toggle-option pass-special))
	  (#\c (toggle-option color-bytes))
	  ((#\b #\B)
	   (let ((is-binary (getf options :binary)))
	     (cond
	       ((and (not is-binary)
		     (not (stream-file-p stream)))
		(message pager
		 "Sorry, I can't switch to binary on an un-seekable stream."))
	       (t
		 (setf (getf options :binary) (not is-binary))
		 (throw 'do-over `(do-over :options ,options))))))
	  (otherwise
	   (message pager "Unknown option '~a'" (nice-char char))))))))

(define-condition directory-error (simple-error) ()
  ;; :report (lambda (c stream)
  ;; 	    (apply #'format stream
  ;; 		   (simple-condition-format-control c)
  ;; 		   (simple-condition-format-arguments c)))
  )

(defun open-lossy (filename &key binary)
  "Open FILENAME for reading in a way which is less likely to get encoding
errors, but may lose data. Quotes the filename against special Lisp characters.
Returns the the open stream or NIL."
  (when (directory-p filename)
    (error 'directory-error
	   :format-control "~a is a directory."
	   :format-arguments (list filename)))
  (typecase filename
    (stream
     (when (and binary (not (eq (stream-element-type filename)
				'(unsigned-byte 8))))
       (error "I'm sorry, but the pager needs a binary stream to run in binary ~
               mode."))
     filename)
    ((or string pathname)
     (if binary
	 (open (quote-filename filename) :direction :input
	       :element-type '(unsigned-byte 8))
	 (progn
	   (make-instance 'utf8b-stream:utf8b-input-stream
			  :input-stream
			  (open (quote-filename filename) :direction :input
			      :element-type '(unsigned-byte 8))))
	 ;; (open (quote-filename filename) :direction :input)
	 ))
    (t
     (error "Sorry, I don't know how to open a ~s" (type-of filename)))))

;; #+sbcl :external-format
;; ;;#+sbcl '(:utf-8 :replacement #\replacement_character)
;; #+sbcl '(:utf-8 :replacement #\?)

(defgeneric %open-file (pager filename &key offset)
  (:documentation "Open the given FILENAME."))

(defmethod %open-file ((pager text-pager) filename &key (offset 0))
  "Open the given FILENAME."
  (with-slots (count lines line got-eof stream page-size binary seekable) pager
    (let ((new-stream (open-lossy filename :binary binary)))
      (when new-stream
	(when stream
	  (close stream))
	(setf stream new-stream
	      lines '()
	      count 0
	      line offset
	      got-eof nil
	      seekable (seekable-p))
	(read-input pager (+ page-size offset)))
      new-stream)))

(defmethod %open-file ((pager binary-pager) filename &key (offset 0))
  "Open the given FILENAME."
  (with-slots (got-eof stream page-size binary buffer buffer-start
	       byte-count byte-pos seekable) pager
    (let ((new-stream (open-lossy filename :binary t)))
      (when new-stream
	(when stream
	  (close stream))
	(setf stream new-stream
	      buffer (make-empty-buffer)
	      buffer-start 0
	      byte-count 0
	      byte-pos 0
	      got-eof nil
	      seekable (seekable-p))
	(read-input pager (+ page-size offset)))
      new-stream)))

(defun open-file-long-command (filename &key (offset 0))
  "Open the given FILENAME."
  (or (%open-file *pager* filename :offset offset)
      (and (message *pager* "Can't open file \"~s\".") nil)))

(defmethod open-file ((pager pager))
  "Prompt to open a file."
  (let ((filename (ask-for-file "Open file: ")))
    (if (and filename (not (zerop (length filename))))
	(when (not (%open-file pager filename))
	  (message pager "Can't open file \"~s\"."))
	(message pager "No file was given."))))

(defmethod save-file ((pager text-pager))
  "Save the stream to a file."
  (with-slots (stream lines) pager
    (when (not (stream-file-p stream))
      (let ((filename (quote-filename (ask-for-file "Save stream to file: "))))
	(if (probe-file filename)
	    ;; @@@ Should prompt for overwrite.
	    (message pager "File exists. Not saved.")
	    (progn
	      (when (block nil
		      (handler-case
			  (with-open-file (str filename :direction :output)
			    (loop :for l :in lines :do
				 (write-line (simplify-line l) str)))
			((or file-error stream-error opsys-error
			     directory-error) (c)
			  (message pager "Error saving file: ~a" c)
			 (return nil)))
		      t)
		(message pager "Stream written to ~s." filename))))))))

;; File locations can be either a file name, or a list or vector with the first
;; element being a file name and the second being an offset in the file. If the
;; offset part doesn't exist, it is considered to be zero.
;; (@@@ maybe these should move to dlib?)

(defun file-location-file (location)
  "Return the file part of a file location."
  (typecase location
    ((or string stream) location)
    ((or list vector) (elt location 0))
    (file-result (os-pathname-namestring (file-result-os-pathname location)))
    ((or structure-object standard-object) (oelt location 'file))
    (t location)))

(defun file-location-offset (location)
  "Return the offset part of a file location, which defaults to 0 if the
location doesn't have an offset part."
  (typecase location
    ((or string stream) 0)
    ((or list vector)
     (if (and (> (length location) 1)
	      (numberp (elt location 1)))
	 (elt location 1)
	 0))
    (file-line-result (file-result-line location))
    (file-result 0)
    ;; ((or structure-object standard-object) (oelt location 'line-number))
    (t 0)))

(defun advance-file-index (pager &optional (dir :forward))
  "Set file-index to the next file in file-list. Return NIL if there's no
more files."
  (with-slots (file-list file-index) pager
    (let ((len (length file-list)))
      (ecase dir
	(:forward
	 (loop :while (and file-index (< file-index (1- len)))
	    :do
	    ;; Skip over locations until we find one with a new file.
	    (incf file-index)
	    (when (or (>= file-index (1- len))
		      (not
		       (equal
			(file-location-file (elt file-list file-index))
			(file-location-file (elt file-list (1+ file-index))))))
		(return-from advance-file-index t))))
	(:backward
	 (loop :while (and file-index (> file-index 0))
	    :do
	    ;; Skip over locations until we find one with a new file.
	    (decf file-index)
	    (when (or (<= file-index 1)
		      (not
		       (equal
			(file-location-file (elt file-list file-index))
			(file-location-file (elt file-list (1- file-index))))))
		(return-from advance-file-index t))))))))

(defmethod next-file ((pager pager))
  "Try to go to the next file in the set of files. Return NIL if we can't."
  (with-slots (file-list file-index) pager
    (if (and file-index (advance-file-index pager :forward))
	(progn
	  (open-file-long-command
	   (file-location-file (elt file-list file-index))
	   :offset (file-location-offset (elt file-list file-index)))
	  t)
	(progn
	  (message pager "No next file.")
	  nil))))

(defmethod previous-file ((pager pager))
  "Try to go to the previous file in the set of files. Return NIL if we can't."
  (with-slots (file-list file-index) pager
    (if (and file-index (advance-file-index pager :backward))
	(open-file-long-command
	 (file-location-file (elt file-list file-index))
	 :offset (file-location-offset (elt file-list file-index)))
	(progn
	  (message pager "No previous file.")
	  nil))))

(defun next-file-location (pager)
  "Go to the next file in the set of files."
  (with-slots (file-list file-index) pager
    (if (and file-index (< file-index (1- (length file-list))))
	(progn
	  (if (not (equal (file-location-file (elt file-list file-index))
			  (file-location-file (elt file-list (1+ file-index)))))
	      (progn
		(incf file-index)
		(open-file-long-command
		 (file-location-file (elt file-list file-index))
		 :offset (file-location-offset (elt file-list file-index))))
	      (progn
		(incf file-index)
		(go-to-offset pager (file-location-offset
				     (elt file-list file-index))))))
	(message pager "No next file."))))

(defun previous-file-location (pager)
  "Go to the previous file in the set of files."
  (with-slots (file-list file-index) pager
    (if (and file-index (> file-index 0))
	(progn
	  (if (not (equal (file-location-file (elt file-list file-index))
			  (file-location-file (elt file-list (1- file-index)))))
	      (progn
		(decf file-index)
		(open-file-long-command
		 (file-location-file (elt file-list file-index))
		 :offset (file-location-offset (elt file-list file-index))))
	      (progn
		(decf file-index)
		(go-to-offset pager (file-location-offset
				     (elt file-list file-index))))))
	(message pager "No previous file."))))

(defun show-file-list (pager)
  (with-slots (file-list file-index) pager
    (if (not file-list)
	(message pager "The file list is empty.")
	(sub-page (s)
	  (let ((i 0))
	    (map nil (_ (tt-format "~c ~s~%"
				   (if (= i file-index) #\* #\space) _)
			(incf i))
		 file-list))))))

(defun suspend (pager)
  "Suspend the pager."
  (with-slots (suspend-flag (quit-flag inator::quit-flag)) pager
    (if (and (find-package :lish)
	     (find-symbol "*LISH-LEVEL*" :lish)
	     (symbol-value (find-symbol "*LISH-LEVEL*" :lish)))
	(setf suspend-flag t
	      quit-flag t)
	(message pager "No shell to suspend to.~%"))))

(defun byte-pos-changed (pager)
  "After changing byte-pos, read more input if needed or availible, or adjust
byte-pos."
  (with-slots (byte-pos page-size byte-count got-eof) pager
    (let ( #| (page-bytes (* page-size (line-bytes))) |#)
      ;;(when (> (+ byte-pos page-bytes) byte-count)
      (read-input pager page-size)
      ;; (when (and (>= byte-pos byte-count) got-eof)
      ;; 	(setf byte-pos (1- byte-count)))
      )))

(defgeneric go-to-line (pager n)
  (:documentation "Go to line N."))

(defmethod go-to-line ((pager text-pager) n)
  (with-slots (count line page-size) pager
    (read-input pager (max 1 (- (+ n page-size) count)))
    (setf line (1- n))))

(defmethod go-to-line ((pager binary-pager) n)
  (with-slots (page-size byte-pos byte-count) pager
    (read-input pager (max 1 (- (+ n page-size)
				(round byte-count (line-bytes)))))
    (setf byte-pos (1- (* n (line-bytes))))))

(defgeneric go-to-offset (pager n)
  (:documentation "Go to an offset N, in units determined by the pager."))

(defmethod go-to-offset ((pager text-pager) n)
  "Go to line N."
  (go-to-line pager n))

(defmethod go-to-offset ((pager binary-pager) n)
  (with-slots (page-size byte-pos byte-count) pager
    (read-input pager (max 1 n))
    (setf byte-pos (1- n))))

(defmethod next-page ((pager text-pager))
  "Display the next page."
  (with-slots (line page-size count got-eof) pager
    (incf line page-size)
    (when (> (+ line page-size) count)
      (read-input *pager* page-size))
    (when (and (>= line count) got-eof)
      (setf line (1- count)))))

(defmethod next-page ((pager binary-pager))
  "Display the next page."
  (with-slots (page-size byte-count byte-pos got-eof) pager
    (let ((page-bytes (* page-size (line-bytes))))
      (incf byte-pos page-bytes)
      (byte-pos-changed pager))))

(defmethod next ((pager text-pager))
  "Display the next line."
  (with-slots (line page-size count prefix-arg got-eof) pager
    (let ((n (or prefix-arg 1)))
      (incf line n)
      (if (> (+ line page-size) count)
	  (read-input *pager* n))
      (when (and (>= line count) got-eof)
	(setf line (1- count))))))

(defmethod next ((pager binary-pager))
  "Display the next line."
  (with-slots (page-size byte-count byte-pos got-eof prefix-arg) pager
    (let* ((n (or prefix-arg 1)))
      (incf byte-pos (* n (line-bytes)))
      (byte-pos-changed pager))))

(defmethod previous-page ((pager text-pager))
  (with-slots (line page-size) pager
    (setf line (max 0 (- line page-size)))))

(defmethod previous-page ((pager binary-pager))
    (with-slots (byte-pos page-size stream) pager
      (setf byte-pos (max 0 (- byte-pos (* page-size (line-bytes)))))
      ;; (when (/= byte-pos (file-position stream))
      ;; 	(file-position stream byte-pos))
      (byte-pos-changed pager)
      ))

(defmethod previous ((pager text-pager))
  "Display the previous line."
  (with-slots (prefix-arg line) pager
    (let ((n (or prefix-arg 1)))
      (setf line (max 0 (- line n))))))

(defmethod previous ((pager binary-pager))
  "Display the previous line."
  (with-slots (prefix-arg byte-pos) pager
    (let ((n (or prefix-arg 1)))
      (setf byte-pos (max 0 (- byte-pos (* (line-bytes) n))))
      (byte-pos-changed pager))))

(defun scroll-right (pager)
  "Scroll the pager window to the right."
  (with-slots (left prefix-arg) pager
    (incf left (or prefix-arg 10))))

(defun scroll-left (pager)
  "Scroll the pager window to the left."
  (with-slots (left prefix-arg) pager
    (setf left (max 0 (- left (or prefix-arg 10))))))

(defgeneric scroll-up (pager)
  (:documentation "Scroll the pager back some lines.")
  (:method ((pager text-pager))
    (with-slots (line) pager
      (setf line (max 0 (- line 5)))))
  (:method ((pager binary-pager))
    (with-slots (line prefix-arg) pager
      (setf prefix-arg 5)
      (previous pager))))

(defgeneric scroll-down (pager)
  (:documentation "Scroll the pager forward some lines.")
  (:method ((pager text-pager))
    (with-slots (line prefix-arg) pager
      (setf prefix-arg 5)
      (next pager)))
  (:method ((pager binary-pager))
    (with-slots (prefix-arg) pager
      (setf prefix-arg 5)
      (next pager))))

(defun scroll-beginning (pager)
  "Scroll the pager window to the leftmost edge."
  (setf (pager-left pager) 0))

(defgeneric scroll-end (pager)
  (:documentation
   "Scroll the pager window to the rightmost edge of the content."))

;; We don't bother counting the max column during display, since it's possibly
;; expensive and unnecessary. We just count it here when needed, which is
;; somewhat redundant, but expected to be overall more efficient.
;; The issue is mostly that we don't have to consider the whole line when
;; clipping, but we do have to consider it when finding the maximum.
(defmethod scroll-end ((pager text-pager))
  "Scroll the pager window to the rightmost edge of the text."
  (with-slots (line lines left page-size show-line-numbers) pager
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
			    (span-to-fatchar-string
			     the-line :fatchar-string *fat-buf*))))))
	 (incf y)
	 (incf i)
	 (setf l (cdr l)))
      ;; (message pager "max-col = ~d" max-col)
      (setf left (max 0 (- max-col (tt-width)))))))

(defmethod scroll-end ((pager binary-pager))
  "This doesn't do anything, since the binary width is fixed."
  (declare (ignore pager)))

(defmethod move-to-top ((pager text-pager))
  "Go to the beginning of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg line) pager
    (if prefix-arg
	(go-to-line pager prefix-arg)
	(setf line 0))))

(defmethod move-to-top ((pager binary-pager))
  "Go to the beginning of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg byte-pos) pager
    (if prefix-arg
	(go-to-line pager prefix-arg)
	(setf byte-pos 0))
    (byte-pos-changed pager)))

(defmethod move-to-bottom ((pager text-pager))
  "Go to the end of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg count line page-size) pager
    (if prefix-arg
	(go-to-line pager prefix-arg)
	(progn
	  (read-input *pager* 0)
	  (setf line (max 0 (- count page-size)))))))

(defmethod move-to-bottom ((pager binary-pager))
  "Go to the end of the stream, or the PREFIX-ARG'th line."
  (with-slots (prefix-arg page-size byte-pos byte-count) pager
    (let ((page-bytes (* page-size (line-bytes))))
      (if prefix-arg
	  (progn
	    (go-to-line pager prefix-arg)
	    (byte-pos-changed pager))
	  (progn
	    (setf byte-pos (max 0 (- byte-count page-bytes)))
	    (byte-pos-changed pager))))))

(defmethod search-command ((pager pager))
  "Search forwards for something in the stream."
  (with-slots (search-string) pager
    (setf search-string (ask "Search for: "))
    (block nil
      (handler-case
	  (when (not (search-for pager search-string))
	    (message pager "--Not found--"))
	(ppcre-syntax-error (c)
	  (fui:display-text
	   "Error"
	   `("Syntax error in search string:"
	     ,(ppcre-syntax-error-string c)
	     ,(format nil "~vt^" (ppcre-syntax-error-pos c))
	     ,(apply #'format
		     nil (simple-condition-format-control c)
		     (simple-condition-format-arguments c)))
	   :justify nil)
	  (setf search-string nil)
	  (return nil))))))

(defun search-backward-command (pager)
  "Search backwards for something in the stream."
  (with-slots (search-string) pager
    (setf search-string (ask "Search backward for: "))
    (when (not (search-for pager search-string :direction :backward))
      (message pager "--Not found--"))))

(defgeneric search-next (pager)
  (:documentation
   "Search for the next occurrence of the current search in the stream."))

(defmethod search-next ((pager text-pager))
  "Search for the next occurrence of the current search in the stream."
  (with-slots (line search-string) pager
    (incf line)
    (when (not (search-for pager search-string))
      (decf line)
      (message pager "--Not found--"))))

(defmethod search-next ((pager binary-pager))
  "Search for the next occurrence of the current search in the stream."
  (with-slots (byte-pos search-string) pager
    (let ((byte-str (unicode:string-to-utf8b-bytes search-string)))
      (incf byte-pos (length byte-str))
      (when (not (search-for pager search-string))
	(decf byte-pos (length byte-str))
	(message pager "--Not found--")))))

(defgeneric search-previous (pager)
  (:documentation
   "Search for the previous occurrence of the current search in the stream."))

(defmethod search-previous ((pager text-pager))
  "Search for the previous occurance of the current search in the stream."
  (with-slots (line search-string) pager
    (if (not (zerop line))
	(progn
	  (decf line)
	  (when (not (search-for pager search-string :direction :backward))
	    (incf line)
	    (message pager "--Not found--")))
	(message pager "Search stopped at the first line."))))

(defmethod search-previous ((pager binary-pager))
  "Search for the previous occurance of the current search in the stream."
  (with-slots (byte-pos search-string) pager
    (if (not (zerop byte-pos))
	(progn
	  (decf byte-pos (line-bytes))
	  (when (not (search-for pager search-string :direction :backward))
	    (incf byte-pos (line-bytes))
	    (message pager "--Not found--")))
	(message pager "Search stopped at the first line."))))

(defun clear-search (pager)
  "Clear the search string."
  (setf (pager-search-string pager) nil))

(defun seekable-p (&optional (stream (pager-stream *pager*)))
  (handler-case
      (let* ((pos (file-position stream))
	     (result (and pos (file-position stream pos))))
	(and result t))
    (stream-error ())))

(defun show-info (pager)
  "Show information about the stream."
  (with-slots (seekable) pager
    (message pager (format-prompt pager "%f %l of %L %b/%B ~:[un~;~]seekable")
	     seekable)))

(defun show-detailed-info (pager)
  "Show information about the stream."
  (with-slots (stream count page-size filter-exprs keep-expr seekable) pager
    (fui:with-typeout (str :title "Pager Buffer Information")
      (print-properties
       `("Name"             ,(stream-name stream)
	 "Current position" ,(bytes-current pager)
         "Total Length"     ,(total-bytes pager)
	 "Current line"     ,(current-line pager)
	 "Maximum line"     ,(maximum-line pager)
	 "Percentage"       ,(percentage pager)
	 "Filters"          ,filter-exprs
	 "Keep"		    ,keep-expr
	 "Seekable?"        ,seekable)
       :stream str))))

;; This is very bogus.
(defun show-version (pager)
  "Show the version."
  (let* ((system (asdf:find-system :pager))
	 (version (asdf:component-version system))
	 (name (asdf:component-name system)))
    (message pager "~:(~a~) Version: ~a" name version)))

(defmethod redraw ((pager pager))
  "Redraw the display."
  (declare (ignore pager))
  (tt-clear) (tt-finish-output))

;; @@@ fix for binary
(defun reread (pager)
  (with-slots (stream line lines got-eof count page-size seekable) pager
    (if seekable
	(let ((l (nth line lines)))
	  (file-position stream (or (and l (line-position l)) 0))
	  (setf got-eof nil)
	  (if (= line 0)
	      (setf lines nil count 0)
	      (progn
		(setf count line)
		;; drop all following lines
		(rplacd (nthcdr (1- line) lines) nil)))
	  (read-input pager page-size))
	(message pager "Can't re-read an unseekable stream."))))

(defun digit-argument (pager)
  "Accumulate digits for the PREFIX-ARG."
  (with-slots (input-char prefix-arg message) pager
    (let (pos)
      (when (and (characterp input-char) (digit-char-p input-char)
		 (setf pos (position input-char +digits+)))
	(setf prefix-arg (+ (* 10 (or prefix-arg 0)) pos)
	      message (format nil "Prefix arg: ~d" prefix-arg))))))

;; The long commands are a meager attempt at mindless "less" compatibility.

(defmacro lc (&body body)
  `(function (lambda (c) (declare (ignorable c)) ,@body)))

(defparameter *long-commands*
  (vector
   `("edit"	  (#\e #\x)  "Examine a different file."
     ,(lc (open-file-long-command (ask-for-file (s+ ":" c #\space)))))
   `("next"	  (#\n)	    "Examine the next file in the arguments."
     ,(lc (next-file *pager*)))
   `("previous"	  (#\p)	    "Examine the previous file in the arguments."
     ,(lc (previous-file *pager*)))
   `("help"	  (#\h)	    "Show the pager help."
     ,(lc (help *pager*)))
   `("?"	  (#\?)	    "Show this long command help."
     ,(lc (command-help)))
   `("file"	  (#\f)	    "Show the file information."
     ,(lc (show-info *pager*)))
   `("quit"	  (#\q)	    "Quit the pager."
     ,(lc (quit *pager*)))))
    #| (#\d (remove-file-from-list)) |#

(defun command-help ()
  "Show help on long commands, aka colon commands."
  (sub-page (s)
    (table-print:print-table
     (table:make-table-from
      (loop :for c :across *long-commands*
	 :collect (list (elt c 0) (elt c 1) (elt c 2)))
      :column-names '("Command" "Abbrev" "Description"))
     ;; :stream s
     :stream *terminal*
     :renderer (make-instance 'terminal-table:terminal-table-renderer))))

(defun read-command (pager)
  (let ((cmd (ask-for :prompt ":" :space-exits t)))
    (when (and cmd (stringp cmd))
      (let (found)
	(loop
	   :for c :across *long-commands*
	   :do (when (position (char cmd 0) (elt c 1))
		 (setf found t)
		 (funcall (elt c 3) cmd)))
	(when (not found)
	  (message pager "~a is an unknown command." cmd))))))

(defun describe-key (pager)
  "Prompt for a key and describe the function it invokes."
  (display-message "Press a key: ")
  (let* ((key-seq (get-key-sequence (lambda () (tt-get-key)) *normal-keymap*))
    	 (action (key-sequence-binding key-seq *normal-keymap*)))
    (cond
      (action
       (display-text "Key Help"
         (if (documentation action 'function)
	     (progn
	       `(,(format nil "~(~a~): ~a~%" action
			  (key-sequence-string key-seq))
		  ,(documentation action 'function)))
	     `(,(format nil "Sorry, there's no documentation for ~a.~%"
			action)))))
      (t
       (message pager "~a is not defined" (key-sequence-string key-seq))))))

(defun set-key-command (pager)
  "Bind a key interactively."
  (display-message "Set key: ")
  (let* ((key-seq (read-key-sequence pager))
	 (cmd (ask-for-function (format nil "Set key ~a to command: "
					(key-sequence-string key-seq)))))
    (if cmd
	(set-key key-seq cmd (inator-keymap pager))
	(display-message "Not a function."))))

(defun eval-expression-command (pager)
  "Prompt for an expression an evaluate it."
  (tt-move-to (1- (tt-height)) 0)
  (with-simple-restart (abort "Go back to the pager.")
    (handler-case
	(fui:display-text
	 "Eval results"
	 (list (with-output-to-string (stream)
		 (prog1
		     (let ((*standard-output* stream))
		       (tiny-repl:tiny-repl :prompt-string "Eval: "
					    :quietly t :once t
					    :output stream))
		   (redraw pager)
		   (update-display pager)
		   ;; (setf (tt-input-mode) :char)
		   )))
	 :justify nil :min-width 16)
      (error (c)
	(if (fui:popup-y-or-n-p
	     (span-to-fat-string
	      `((:red "Error: ") ,(apply #'format nil "~a" (list c))
		#\newline #\newline "Enter the debugger?"))
	     :default #\N)
	    (invoke-debugger c)
	    ;; (continue)
	    (invoke-restart (find-restart 'abort))))))
  (redraw pager))

(defun shell-command (pager)
  "Run a shell command."
  (declare (ignore pager))
  (tt-move-to (1- (tt-height)) 0)
  (tt-erase-to-eol)
  (tt-normal)
  (if (find-package :lish)
      (progv (list (intern "*POST-COMMAND-HOOK*" :lish))
	  (list (list (lambda (cmd type)
			(declare (ignore cmd type))
			;; (setf (refer-to :lish :shell-exit-flag) t)
			(funcall (intern "LISHITY-SPLIT" :lish)))))
	(symbol-call :lish :lish :prompt "! " :debug t :init-file nil))
      (nos:system-command (rl:rl :prompt "! ")))
  (format t "[Press Enter]~%")
  (finish-output)
  (tt-get-key))

(defun help-key (pager)
  "Sub-command for help commands."
  (display-message "Help (? for more help): ")
  (process-event pager (tt-get-char) *help-keymap*)
  (setf (inator-quit-flag *pager*) nil))

(defun more-help (pager)
  "Show more help on help commands."
  (tt-clear)
  (tt-move-to 0 0)
  (tt-write-string "c - Print the name of function that a key performs.
k - Describe the function that a key performs.
b - List all keys and the functions they perform.
q - Abort")
  (tt-finish-output)
  (help-key pager))			; @@@ this could infinitely recurse

(defstruct suspended-pager
  "State of a suspended pager."
  pager
  file-list
  stream
  close-me)

;; @@@ Consider moving a generalized version of this to opsys
(defmacro with-disabled-cchars (() &body body)
  #+unix
  (with-unique-names (c-susp c-intr)
    `(let (,c-susp ,c-intr)
       (unwind-protect
	    (progn
	      (when (terminal-file-descriptor *terminal*)
		(setf ,c-susp (uos:control-char
			      (terminal-file-descriptor *terminal*) :suspend)
		      ,c-intr (uos:control-char
			      (terminal-file-descriptor *terminal*) :interrupt)
		      (uos:control-char
		       (terminal-file-descriptor *terminal*) :suspend) nil
		      (uos:control-char
		       (terminal-file-descriptor *terminal*) :interrupt) nil))
	      ,@body)
	 (when (terminal-file-descriptor *terminal*)
	   (when ,c-susp
	     (setf (uos:control-char
		    (terminal-file-descriptor *terminal*) :suspend)
		   ,c-susp))
	   (when ,c-intr
	     (setf (uos:control-char
		    (terminal-file-descriptor *terminal*) :interrupt)
		   ,c-intr))))))
  #-unix
  `(progn ,@body))

(defmethod read-key-sequence ((pager pager))
  (with-disabled-cchars () (call-next-method)))

(defmethod await-event ((pager pager))
  (let ((result (with-disabled-cchars () (call-next-method))))
    (with-slots (prefix-arg input-char) pager
      (setf input-char result)
      ;; Clear the prefix argument if we didn't read a digit.
      (when (not (equal (inator-command pager) 'digit-argument))
	(setf prefix-arg nil))
      (when (typep result 'tt-mouse-button-event)
	(case (tt-mouse-button result)
	  (:button-4 (setf result :scroll-up))
	  (:button-5 (setf result :scroll-down)))))
    result))

(defvar *sub-pager* nil
  "Indicator that we're in a pager inside a pager.")

(defun %page (files &key pager close-me suspended options)
  "View a stream with the pager. Return whether we were suspended or not."
  (with-terminal ()
    (with-immediate ()
      (with-enabled-events ('(:resize :mouse-buttons))
	;; (with-disabled-cchars ()
	(let* ((stream    (getf options :stream))
	       (pager     (or pager (getf options :pager)))
	       (binary    (getf options :binary))
	       (try-again nil)
	       (*pager*   (or pager
			      (apply #'make-instance
				     (if binary 'binary-pager 'text-pager)
				     :stream stream
				     :page-size (1- (tt-height))
				     :file-list (if (listp files)
						    files
						    (list files))
				     :options options
				     options))))
	  (with-slots (page-size file-list file-index suspend-flag seekable
		       stream (quit-flag inator::quit-flag)) *pager*
	    (unwind-protect
		 (prog ((*sub-pager* t))
		  try-again
		    (handler-case
			(progn
			  (when try-again
			    (setf try-again nil)
			    (when (not (next-file *pager*))
			      (when (or (= (length file-list) 1)
					(not (previous-file *pager*)))
				(go done))))
			  (when (not (pager-stream *pager*))
			    (setf (pager-stream *pager*)
				  (open-lossy (file-location-file
					       (elt file-list file-index))
					      :binary binary)
				  close-me t))
			  (when (not suspended)
			    (freshen *pager*)
			    (setf (pager-stream *pager*) stream))
			  (setf seekable (seekable-p stream))
			  (when file-list
			    (when (not (zerop (file-location-offset
					       (elt file-list file-index))))
			      (go-to-line *pager*
					  (file-location-offset
					   (elt file-list file-index)))))
			  ;; (tt-enable-events :mouse-buttons)
			  (read-input *pager* page-size)
			  (setf quit-flag nil
				suspend-flag nil
				suspended nil)
			  (event-loop *pager*)
			  (when suspend-flag
			    (setf suspended t)
			    (if (find-package :lish)
				(let ((suspy (make-suspended-pager
					      :pager *pager*
					      :file-list file-list
					      :stream stream
					      :close-me close-me)))
				  (funcall (find-symbol "SUSPEND-JOB" :lish)
					   "pager" "" (lambda ()
							(pager:resume suspy)))
				  (go done))
				(format t "No shell loaded to suspend to.~%"))))
		      ((or file-error stream-error opsys-error directory-error)
			(c)
			(message-pause "~a" (princ-to-string c))
			(setf try-again t)
			(go try-again)))
		  done)
	      (when (and close-me (not suspended) stream)
		(close stream))
	      (tt-move-to (tt-height) 0)
	      (tt-erase-to-eol)
	      ;;(tt-write-char #\newline)
	      ;; (when (not *sub-pager*)
	      ;;   (tt-disable-events :mouse-buttons))
	      (tt-finish-output)))))))
  suspended)

;; This seems like a dumb way to toggle binary mode.
(defun page (files &key pager close-me suspended options)
  "View a stream with the pager. Return whether we were suspended or not."
  (let (result)
    (loop
      :do
      (setf result (catch 'do-over
		     (%page files :pager pager
				  :close-me close-me
				  :suspended suspended
				  :options options)))
      :while (and (listp result) (eq (car result) 'do-over))
      :do
      (setf options (getf (cdr result) :options)))
    result))

(defun resume (suspended)
  (with-slots (pager stream close-me) suspended
    (page stream :pager pager :close-me close-me :suspended t)))

(defun page-thing (thing &rest options)
  "Page the actual thing instead of trying to open it as a file."
  (typecase thing
    (string
     (with-input-from-string (str thing)
       (setf (getf options :stream) str)
       (page nil :options options)))
    (pathname
     ;; But if it's explicitly a pathname then try to open it.
     (page thing :options options))
    (sequence
     (page thing :options options))
    (stream
     (page thing :options options))
    (t
     (with-input-from-string (str (prin1-to-string thing))
       (setf (getf options :stream) str)
       (page nil :options options)))))

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
	    ;; (*terminal-io* (make-two-way-stream *standard-input* ,str-name))
	    )
       (prog1 (with-simple-restart
		  (continue "View the output so far in the pager anyway.")
		(progn ,@body))
	 (page (make-string-input-stream
		(get-output-stream-string ,str-name)))))))

(defun acceptable-object (obj)
  (some (_ (funcall _ obj))
	'(streamp consp stringp pathnamep)))

(defmethod help ((pager pager))
  (sub-page (output)
    (format output
"Hi! You are using Nibby's pager. You seem to have hit '~a' again, which gives a
summary of commands. Press 'q' to exit this help. The keys are superficially
compatible with 'less', 'vi' and 'emacs'.
" (pager-input-char pager))
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
  / ^S				Search forward.
  ^R				Search forward.
  n				Search for next occurrence.
  N p				Search for previous occurrence.
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
    c C				  Color bytes in binary mode.
    m M				  Show the mode line.
    b B                           Binary mode.

Press 'q' to exit this help.
" output)))

(defun describe-bindings (pager)
  "Show help on pager key commands."
  (declare (ignore pager))
  (with-input-from-string
      (input
       (with-output-to-string (output)
	 (write-string
"You are using Nibby's pager. Here's a list of what the keys do.
If you want a description of what a function does, press Control-H then 'k',
then the key. Press 'q' to exit this help.
" output)
	 (keymap:describe-keymap *normal-keymap* :stream output)
	 (princ "Press 'q' to exit this help.
" output)))
    (pager input)))

(defun browse (&optional (dir "."))
  "Look at files."
  (let ((*pager* (make-instance 'text-pager))
	filename
	(directory dir))
    (loop :while (setf (values filename directory)
		       (pick-file :directory directory))
       :do (with-open-file (stream (quote-filename filename))
	     ;; @@@ should do open-lossy
	     (page stream)))))

;; This is just the external Lisp interface.
(defun pager (&optional (file-or-files (pick-file) files-supplied-p)
	      &rest options)
  "View the file with the pager. Prompt for a file name if one isn't given.
Options are:
  :show-line-numbers - Show line numbers.
  :ignore-case       - Ignore case in searches.
  :raw-output        - Output characters without processing.
  :pass-special      - Pass special escape sequences to the terminal.
  :follow-mode       - Start in follow mode.
  :binary            - Use binary mode.
  :color-bytes       - Use color bytes mode."
  ;; Let's just say if you page nil, nothing happens.
  ;; This makes it nicer to quit from pick-file without getting an error.
  (when (and files-supplied-p (not file-or-files))
    (setf file-or-files (pick-file)))
  (typecase file-or-files
    (null
     (page (list *standard-input*) :options options))
    ((or stream string pathname)
     (page (list file-or-files) :options options))
    (sequence
     (page file-or-files :options options))
    (t
     (error "The pager doesn't know how to deal with a ~w"
	    (type-of file-or-files)))))

(defun binary-pager (&optional (file-or-files (pick-file) files-supplied-p)
		     &rest options)
  "Just like pager, but starting in binary mode. Maybe useful for calling as
a view method."
  (if files-supplied-p
      (apply #'pager file-or-files :binary t options)
      (funcall #'pager :binary t)))

;; @@@ Run in a thread:
;;
;; (defmacro run (&whole whole &optional file-or-files)
;;   (declare (ignore file-or-files))
;;   `(apply #'pager ',(cdr whole)))

#+lish
(lish:defcommand pager
  ((show-line-numbers boolean :short-arg #\l
    :help "True to show line numbers.")
   (ignore-case boolean :short-arg #\i :default t
    :help "True to ignore case in searches.")
   (raw-output boolean :short-arg #\r
    :help "True to output characters without processing.")
   (pass-special boolean :short-arg #\p
    :help "True to pass special escape sequences to the terminal.")
   (follow-mode boolean :short-arg #\f :help "True to start in follow mode.")
   (binary boolean :short-arg #\b :help "True to use binary mode.")
   (color-bytes boolean :short-arg #\c :help "True to use color bytes mode.")
   (files pathname :repeating t :help "Files to view."))
  :accepts (:grotty-stream :file-list :file-locaations)
  "Look through text, one screen-full at a time."
  (let ((thing (or files
		   (and (acceptable-object lish:*input*) lish:*input*)
		   (and (not (likely-a-terminal-p *standard-input*))
			*standard-input*))))
    (pager thing
	   :show-line-numbers show-line-numbers 
	   :ignore-case ignore-case
	   :raw-output raw-output
	   :pass-special pass-special
	   :binary binary
	   :color-bytes color-bytes
	   :follow-mode follow-mode)))

;; EOF
