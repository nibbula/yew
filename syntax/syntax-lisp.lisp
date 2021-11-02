;;;
;;; syntax-lisp.lisp - Things for dealing with the syntax of Lisp code
;;;

(defpackage :syntax-lisp
  (:documentation
   "Functions for dealing with Lots of Irritating Superfluous Parentheses.")
  (:use :cl :dlib :syntax
	#+use-regex :regex
	#-use-regex :ppcre
	:dlib-misc #| :esrap |# :theme :style :grout :fatchar)
  (:export
   #:lisp-syntax
   #:lisp-token
   #:lisp-symbol-token
   #:read-token
   #:matching-paren-position
   #:format-lisp-comment
   ))
(in-package :syntax-lisp)

;;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defclass lisp-syntax (syntax)
  ()
  (:default-initargs
   :name "Lisp"
    :description "Common Lisp syntax."
    :language-type :programming
    :file-types '("lisp" "lsp" "asd" "cl"))
  (:documentation "Common Lisp syntax."))

(defparameter *syntax* (register-syntax (make-instance 'lisp-syntax))
  "Save this syntax object.")

(defclass lisp-token (token)
  ()
  (:documentation "A lisp token."))

(defclass lisp-symbol-token (lisp-token)
  ((package
    :initarg :package :accessor lisp-symbol-token-package :initform nil 
    :documentation "The package name of the symbol if it has one."))
  (:documentation "A lisp symbol."))

;; (eval-when (:load-toplevel :execute)
;;   (setf *syntax* (register-syntax (make-instance 'lisp-syntax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The funny thing about Lisp is, you can't read it entirely perfectly it
;; without also evaluating it. That means that "read-only" lexical analysis
;; will fail for programs that modify the syntax. To read it perfectly without
;; potentially modifying the current environment, you might have to have a
;; separate evaluation environment, or an undo-able environment.
;;
;; It would be nice to be adaptable to syntax modifications of the current
;; image, but first I'd like to just get standard syntax working.

;; You might think that I would just take this from SBCL or some other
;; liberally liscensed implementation, but the reader code in whole
;; implementations tend to have many quirks related to bootstraping. They also
;; tend to use many of thier own implementation dependent functions. I would
;; like this to be portable to any implementation and without bootstrapping
;; quriks. Also we have different goals.

;; Another annoying thing about about the reader, is it's not possible to
;; write one in purely portable Common Lisp, because there isn't a specified
;; way to construct structures portably. This is a subtle detail which seems
;; like it probably could be fairly easily be fixed by an new version of the
;; specification adding a structure creation function.

(deftype char-syntax ()
  '(member
    :constituent			; things that can be in an identifier
    :whitespace				; the stuff inbetween
    :non-term-macro			; aka chars that d
    :macro				; 
    :single-escape			; 
    :mutli-escape))			; |such as vertical bars|

(deftype char-type ()
  "The syntactic type of a character."
  '(member 
    :constituent
    :macro-character
    :single-escape
    :invalid
    :multiple-escape
    :whitespace))

;;; @@@ I really might want a better data structure here.
;;; @@@ Assuming contiguous & increasing 0-9 A-Z a-z
(defparameter *char-type-table*
  (alist-to-hash-table
   `((#\Backspace . :constituent)
    ,@(loop :for i :from 0 :to 9
	 :collect (cons (digit-char i) :constituent))
    (#\Tab       . :whitespace)
    (#\Newline   . :whitespace)
    (#\Linefeed  . :whitespace)
    (#\Page      . :whitespace)
    (#\Return    . :whitespace)
    (#\Space     . :whitespace)
    (#\!         . :constituent*)
    (#\"         . :terminating)
    (#\:         . :constituent)
    (#\;         . :terminating)
    (#\<         . :constituent)
    (#\=         . :constituent)
    (#\>         . :constituent)
    (#\?         . :constituent*)
    (#\@         . :constituent)
    ,@(loop :for i :from (char-code #\A) :to (char-code #\Z)
	 :collect (cons (code-char i) :constituent))
    (#\#         . :non-terminating)
    (#\$         . :constituent)
    (#\%         . :constituent)
    (#\&         . :constituent)
    (#\'         . :terminating)
    (#\(         . :terminating)
    (#\)         . :terminating)
    (#\[         . :constituent*)
    (#\\         . :single-escape)
    (#\]         . :constituent*)
    (#\^         . :constituent)
    (#\_         . :constituent)
    (#\`         . :terminating)
    ,@(loop :for i :from (char-code #\a) :to (char-code #\z)
	 :collect (cons (code-char i) :constituent))
    (#\*         . :constituent)
    (#\+         . :constituent)
    (#\,         . :terminating)
    (#\-         . :constituent)
    (#\.         . :constituent)
    (#\/         . :constituent)
    (#\{         . :constituent*)
    (#\|         . :multiple-escape)
    (#\}         . :constituent*)
    (#\~         . :constituent)
    (#\Rubout    . :constituent))))

(defun char-type (c)
  "Return the character type of the character C."
  (check-type c character)
  (gethash c *char-type-table*))

(deftype token-type ()
  '(member
    :object
    :symbol
    :comment
    :dot
    :reader-macro))

;; Variables that influence the Lisp reader:
;;   *package*    *read-default-float-format*  *readtable*
;;   *read-base*  *read-suppress*

(defmethod read-token ((s lisp-syntax) stream)
  (declare (ignore s stream))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opens-and-closes (pairs)
  "Return the split out opens and closes given a string of PAIRS, which must
be of even length and matched in order."
  (let ((out (make-array `(,(/ (length pairs) 2))
			 :element-type 'character
			 :adjustable t :fill-pointer t)))
    (list (progn
	    (with-output-to-string (str out)
	      (loop :for i :from 0 :below (length pairs)
		 :if (evenp i)
		 :do (princ (char pairs i) str)))
	    (copy-seq out))
	  (progn
	    (setf (fill-pointer out) 0)
	    (with-output-to-string (str out)
	      (loop :for i :from 0 :below (length pairs)
		 :if (oddp i)
		 :do (princ (char pairs i) str)))
	    out))))

;; This done in a temporary hackish way, until we make the whole reader.
;; I'm curious to compare the performance of this vs a rule 184 based
;; implementation.
;; @@@ This should be a method of a generic that is in syntax.lisp.

(defun matching-paren-position (string &key position
					 (char #\))
					 (pairs "()[]{}" pairs-p))
  "Return the position in STRING of CHAR matching a, perhaps hypothetical,
paired char at POSITION. If there is none, return nil. If POSITION is
not provided, it defaults to the end or beginning of the string, depending if
CHAR is an open or a close. PAIRS is an even length string of paired characters
in order, \"{open}{close}...\". PAIRS defaults to something reasonable."
  (declare (type string string))
  (let* ((starts nil) (c nil)
	 (oc (when pairs-p (opens-and-closes pairs)))
	 (opens (if pairs-p (first oc) "([{"))
	 (closes (if pairs-p (second oc) ")]}"))
	 (forward (and (position char opens) t))
	 (end (if forward (length string) position))
	 (start (if forward position 0))
	 (i start))
    ;; (format t "opens = ~s closes = ~s~%" opens closes)
    ;; (format t "forward = ~s end = ~s start = ~s~%" forward end start)
    (flet ((eat-string ()
	     "Scan past the string."
	     (incf i)
	     (loop :while (and (< i end)
			       (not (eql #\" (setf c (aref string i)))))
		:do (when (eql c #\\)
		      (incf i))
		(incf i)))
	   (eat-comment ()
	     (incf i)
	     (loop :with level = 1
		:while (and (< i end) (> level 0))
		:do
		  (setf c (aref string i))
		  (cond
		    ((and (eql c #\|) (and (< i (1- end))
					   (eql #\# (aref string (1+ i)))))
		     (decf level))
		    ((and (eql c #\#) (and (< i (1- end))
					   (eql #\| (aref string (1+ i)))))
		     (incf level)))
		(incf i)))
	   (eat-line-comment ()
	     (loop :while (and (< i end)
			       (not (eql #\newline (setf c (aref string i)))))
		:do (incf i))))
      (cond
	(forward
	 (loop
	    :while (< i end)
	    :do (setf c (aref string i))
	    (cond
	      ((position c opens) (push i starts))
	      ((position c closes) (pop starts)
	       (when (not starts)
		 (return-from matching-paren-position i)))
	      ((eql c #\") (eat-string))
	      ((and (eql c #\#) (< i (- position 2))) ; # reader macro char
	       (incf i)
	       (case (setf c (aref string i))
		 (#\\ (incf i))		; ignore the quoted char
		 (#\| (eat-comment))	; scan past matched #| comments |#
		 (#\( (push i starts)))) ; vectors, treated just like a list
	      ;; single line comment
	      ((eql c #\;) (eat-line-comment)))
	    (incf i)))
	(t
	 (loop
	    :while (< i end)
	    :do (setf c (aref string i))
	    (cond
	      ((position c opens) (push i starts))
	      ((position c closes) (pop starts))
	      ((eql c #\") (eat-string))
	      ((and (eql c #\#) (< i (- end 2))) ; # reader macro char
	       (incf i)
	       (case (setf c (aref string i))
		 (#\\ (incf i))		; ignore the quoted char
		 (#\| (eat-comment))	; scan past matched #| comments |#
		 (#\( (push i starts)))) ; vectors, treated just like a list
	      ;; single line comment
	      ((eql c #\;) (eat-line-comment)))
	    (incf i))
	 (first starts))))))
	
(defun eat-whitespace (stream)
  (let (c)
    (loop :do (setf c (read-char stream))
       :while (eq (char-type c) :whitespace))
    (unread-char c stream)))

#|
(defun fake-read-token (stream)
  (eat-whitespace)
  (prog (c)
     read
     (setf c (read-char stream))
     (when (invalid-character-p c)
       (error 'reader-error))
     (case (char-type c)
       (terminating)
       (non-terminating))
     (when (is-number token-buffer)
       (make-number token-buffer))
     ))

(defun fake-read (&key (stream *standard-input*)
		    (eof-error-p t) (eof-value nil) (recursive-p nil))
  "Pretend to do something similar to READ, but don't. This is like if
READ with *read-suppress* returned objects, but didn't handle any really
gritty details of a Lisp reader. The most important thing is: don't INTERN
any new symbols, or fail on unknown packages or symbols."
  (handler-case
      (fake-read-token stream)
    (end-of-file (c)
      (if (not eof-error-p)
	  eof-value
	  (error 'reader-error)))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are primarily for the presumed speed up of avoiding package lookup.
(defparameter *cl-package* nil)
(defparameter *keyword-package* nil)

(define-symbol-macro .cl-package.
    (or *cl-package* (setf *cl-package* (find-package :cl))))

(define-symbol-macro .keyword-package.
    (or *keyword-package* (setf *keyword-package* (find-package :keyword))))

(defparameter *flow-control*
  '(go if when unless case ccase ecase cond typecase ctypecase etypecase
    catch throw return return-from do do* dotimes dolist loop loop-finish
    error cerror signal break abort continue)
  "Flow control. These are fairly aribitrary and dumb.")

(defparameter *operator*
  '(block      let*                  return-from
    catch      load-time-value       setq
    eval-when  locally               symbol-macrolet
    flet       macrolet              tagbody
    function   multiple-value-call   the
    go         multiple-value-prog1  throw
    if         progn                 unwind-protect
    labels     progv
    let        quote
    )
  "Speical forms, straight outta Figure 3-2.")

(defparameter *constant*
  '(t nil))

(defmethod stylize-token ((token lisp-symbol-token) &key case package-p)
  "Stylize a token that is a Lisp symbol. If CASE is given, change the case
accordingly. Case can be :upper :lower :mixed or :none."
  (let ((s (token-object token))
	item)
    (flet ((fix-case (s) (if (member case '(:lower :none))
			     (string-downcase s) s)))
      (when (not *theme*)
	(return-from stylize-token (fix-case (string s))))
      (setf item
	    (cond
	      ((fboundp s)
	       ;; @@@ we could fallback to suffixes, like " (F)"
	       (cond
		 ((eq (symbol-package s) .cl-package.)
		  (cond
		    ((member s *operator*)
		     `(:syntax :entity :keyword :operator :style))
		    ((member s *flow-control*)
		     `(:syntax :entity :keyword :control :style))
		    (t
		     `(:syntax :support :function :style))))
		 ((macro-function s)
		  `(:syntax :entity :name :macro :style))
		 (t
		  `(:syntax :entity :name :function :style))))
	      ((boundp s)
	       (cond
		 ((eq (symbol-package s) .cl-package.)
		  `(:syntax :support :variable :style))
		 (t
		  `(:syntax :variable :other :style))))
	      ((or (eq (symbol-package s) .keyword-package.)
		   (member s *constant*))
	       `(:syntax :constant :other :style))
	      ((find-class s nil)
	       `(:syntax :entity :name :type :style))))
      (themed-string item (fix-case
			   (if package-p
			       (s+ (package-name (symbol-package s))
				   (multiple-value-bind (s status)
				       (find-symbol (string s)
						    (symbol-package s))
				     (declare (ignore s))
				     (if (eq status :external) ":" "::"))
				   (string s))
			       (string s)))))))

(defmethod stylize-token ((token lisp-token) &key case package-p)
  "Stylize a token that is a Lisp object. If CASE is given, change the case
accordingly. Case can be :upper :lower :mixed or :none."
  (let ((o (token-object token))
	item)
    (flet ((fix-case (s)
	     (if (member case '(:lower :none))
		 (string-downcase s)
		 s)))
      (when (not *theme*)
	(return-from stylize-token (fix-case (prin1-to-string o))))
      (setf item
	    (typecase o
	      ((or character string pathname)
	       `(:syntax :constant :character))
	      ((or number array cons stream)
	       `(:syntax :constant :other))
	      ((or error function package random-state readtable restart
		   condition structure-object standard-method class)
	       `(:syntax :constant :language))
	      (symbol
	       (return-from stylize-token
		 (stylize-token (change-class token 'lisp-symbol-token
					      :package (symbol-package o))
				:package-p package-p
				:case case)))
	      (t
	       (fix-case (prin1-to-string o)))))
      (themed-string item (fix-case (prin1-to-string o))))))

#|
(defun format-lisp-comment (comment-string stream &key columns)
  "Output COMMENT-STRING to strea"
  (with-grout (*grout* stream)
    ;; (format t "stream = ~s grout = ~s~%" stream *grout*)
    (let ((lines (split-sequence #\newline comment-string))
	  par new-paragraph s e ss ee prefix first-non-blank)
      (declare (ignorable e))
      (dbugf :poo "2 *grout* = ~s stream = ~s~%" *grout* stream)
      (labels ((floo () #|(terminal:tt-finish-output) (terminal:tt-get-key) |#)
	       (newline (x)
		 (grout-princ
		  (if dlib:*dbug-facility* (s+ x #\newline) #\newline))
		 ;; (grout-format "~%")
		 ;; (write-char #\newline stream)
		 (floo)
		 )
	       (print-it (string-list &key prefix verbatim)
		 "Print the STRING-LIST with word wrap justification."
		 (dbugf :poo "print-it ~s~%" string-list)
		 (grout-color
		  :white :default
		  (if verbatim
		      (s+ (or prefix "")
			  (join-by-string string-list #\space))
		      (justify-text
		       (join-by-string string-list #\space)
		       :prefix (or prefix "")
		       :stream nil
		       :cols (or columns (grout-width)))))
		 (floo)
		 )
	       (print-paragraph ()
		 "Print the paragraph that has accumulated in PAR."
		 (when par
		   (when new-paragraph
		     (newline "-•-"))
		   (print-it (nreverse par))
		   (newline "‼")
		   (setf new-paragraph t
			 par nil)))
	       (leading-space (l)
		 "Return the leading blank space from L."
		 (multiple-value-setq (s e ss ee)
		   (scan "^([ \\t]+)[^ \\t]" l))
		 (when s
		   (subseq l (aref ss 0) (aref ee 0))))
	       (set-first-non-blank-line ()
		 "Set first-non-blank to the first non-blank line or NIL if
                  there isn't one."
		 (setf first-non-blank
		       (find-if (_ (not (zerop (length _)))) (cdr lines))))
	       (space-char-p (c)
		 ;; @@@ or maybe some others in unicode but not the same as
		 ;; char-util:whitespace-p
		 (find c #(#\space #\tab (char-code 12) ;; #\formfeed
			   )))
	       (indented-p ()
		 "Return true if the lines look like they have a minimum uniform
                  indent."
		 (and (> (length lines) 1)
		      (setf prefix (and (set-first-non-blank-line)
					(leading-space first-non-blank)))
		      (every (_ (or (zerop (length _))
				    (equal prefix (leading-space _))
				    (= (mismatch prefix _) (length prefix))))
			     (cdr lines)))))
	;; Get rid of uniform leading space on every line after the first.
	;; This usualy comes from the typical style of indenting the docstring
	;; text to align with it's first line.
	(when (indented-p)
	  (when dlib:*dbug-facility*
	    (grout-color :red :black
			 (s+ "•••••••• Doing the Thing ••••••••" #\newline)))
	  (dbugf :poo "••••• Doing a Thing •••••~%")
	  (setf lines
		(cons (first lines)
		      (loop :with prefix-len = (length prefix)
			 :for l :in (cdr lines)
			 :collect (subseq l (min (length l) prefix-len))))))
	;; Go through the lines and output them as justified paragraphs.
	(loop :with l
	   :for ll :on lines :do
	   (setf l (car ll))
	   (cond
	     ;; Empty line
	     ((zerop (length l))
	      (print-paragraph)
	      (newline "•")
	      (setf new-paragraph nil))
	     ;; A line starting with text
	     ((alphanumericp (char l 0))
	      (push l par))
	     ;; A line starting with blanks
	     ((multiple-value-setq (s e ss ee)
		(scan "^([ \\t]+)([^ \\t])" l))
	      (when par
		(print-it (nreverse par))
		(newline "¿")
		(setf par nil))
	      ;; Special verbatim line. @@@ Is this even good???
	      (if (equal "|" (subseq l (aref ss 1) (aref ee 1)))
		  (print-it (list (subseq l (aref ee 1)))
			    :verbatim t
			    :prefix (subseq l (aref ss 0) (aref ee 0)))
		  (let ((next-line (cadr ll)))
		    (if (and next-line (not (zerop (length next-line)))
			     (space-char-p (char next-line 0)))
			;; Try to use the next line's indentation
			(print-it (list l) :prefix (leading-space l))
			;; (print-it (list l) :prefix (leading-space next-line))
			;; Otherwise just use the first line's indentation.
			(print-it (list l)
				  :prefix (subseq l (aref ss 0) (aref ee 0))))))
	      (newline "ß"))
	     ;; Line starting with something else?
	     (t
	      (dbugf :poo "push other par ~s~%" l)
	      (push l par)
	      ;; (when par
	      ;; 	(print-paragraph)
	      ;; 	(newline "˚")
	      ;; 	(setf par nil))
	      ;; (when new-paragraph
	      ;; 	(newline "¢")
	      ;; 	(setf new-paragraph nil))
	      ;; (grout-color :white :default l)
	      ;; (dbugf :poo "other line ~s~%" l)
	      ;; (floo)
	      ;; (newline "¶")
	      )))
	(when par
	  (print-paragraph))))))
|#

;; @@@ This needs a lot of improvement.
(defun format-lisp-comment (comment-string stream &key columns)
  "Output ‘comment-string’ to ‘stream’ in some way which we hope is better than
the verbatim string." ;; @@@ really describe when fixed
  (with-grout (*grout* stream)
    ;; (format t "stream = ~s grout = ~s~%" stream *grout*)
    (let* ((string (regex-replace (s+ "~" #\newline "(\\s*)") comment-string ""))
	   (lines (split-sequence #\newline string))
	   par s e ss ee #|prefix first-non-blank|#)
      ;;(dbugf :poo "2 *grout* = ~s stream = ~s~%" *grout* stream)
      (labels ((print-it (string-list &key prefix verbatim)
		 "Print the STRING-LIST with word wrap justification."
		 ;; (dbugf :poo "print-it ~s~%" string-list)
		 (grout-color
		  :white :default
		  (if verbatim
		      (s+ (or prefix "") string-list)
		      (justify-text
		       (join-by-string string-list #\space)
		       :prefix (or prefix "")
		       :stream nil
		       :cols (or columns (grout-width))))))
	       (print-paragraph ()
		 "Print the paragraph that has accumulated in PAR."
		 (when par
		   (print-it (nreverse par))
		   (grout-princ #\newline)
		   (grout-princ #\newline)
		   (setf par nil)))
	       #| (leading-space (l)
		 "Return the leading blank space from L."
		 (multiple-value-setq (s e ss ee)
		   (scan "^([ \\t]+)[^ \\t]" l))
		 (when s
		   (subseq l (aref ss 0) (aref ee 0))))
	       (set-first-non-blank-line ()
		 "Set first-non-blank to the first non-blank line or NIL if
                  there isn't one."
		 (setf first-non-blank
		       (find-if (_ (not (zerop (length _)))) (cdr lines)))) |#
	       (space-char-p (c)
		 ;; @@@ or maybe some others in unicode but not the same as
		 ;; char-util:whitespace-p
		 (find c #(#\space #\tab (char-code 12) ;; #\formfeed
			   )))
	       #|
	       (indented-p ()
		 "Return true if the lines look like they have a minimum uniform
                  indent."
		 (and (> (length lines) 1)
		      (setf prefix (and (set-first-non-blank-line)
					(leading-space first-non-blank)))
		      (every (_ (or (zerop (length _))
				    (equal prefix (leading-space _))
				    (= (mismatch prefix _) (length prefix))))
			     (cdr lines))))
	       |#
	       )
	;; Get rid of uniform leading space on every line after the first.
	;; This usualy comes from the typical style of indenting the docstring
	;; text to align with it's first line.
	#|
	(when (indented-p)
	  (when dlib:*dbug-facility*
	    (grout-color :red :black
			 (s+ "•••••••• Doing the Thing ••••••••" #\newline)))
	  (dbugf :poo "••••• Doing a Thing •••••~%")
	  (setf lines
		(cons (first lines)
		      (loop :with prefix-len = (length prefix)
			 :for l :in (cdr lines)
			 :collect (subseq l (min (length l) prefix-len))))))
	|#
	;; Go through the lines and output them as justified paragraphs.
	(loop :with l
	   :for ll :on lines :do
	   (setf l (car ll))
	   ;; (dbugf :poo "consider line ~s~%" l)
	   (cond
	     ;; Empty line
	     ((zerop (length l))
	      (print-paragraph))
	     ;; A line starting with text
	     ((alphanumericp (char l 0))
	      (push l par))
	     ;; A line starting with blanks
	     ((multiple-value-setq (s e ss ee)
		(scan "^([ \\t]+)([^ \\t])" l))
	      (print-paragraph)
	      ;; Special verbatim line. @@@ Is this even good???
	      (if (equal "|" (subseq l (aref ss 1) (aref ee 1)))
		  (print-it (list (subseq l (aref ee 1)))
			    :verbatim t
			    :prefix (subseq l (aref ss 0) (aref ee 0)))
		  (let ((next-line (cadr ll)))
		    (if (and next-line (not (zerop (length next-line)))
			     (space-char-p (char next-line 0)))
			;; Try to use the next line's indentation
			(print-it l ;; :prefix (leading-space l)
				  :verbatim t)
			;; (print-it (list l) :prefix (leading-space next-line))
			;; Otherwise just use the first line's indentation.
			(print-it l
				  ;; :prefix (subseq l (aref ss 0) (aref ee 0))
				  :verbatim t))))
	      (grout-princ #\newline))
	     ;; Line starting with something else?
	     (t
	      (dbugf :poo "push other par ~s~%" l)
	      (push l par)
	      ;; (when par
	      ;; 	(print-paragraph)
	      ;; 	(newline "˚")
	      ;; 	(setf par nil))
	      ;; (when new-paragraph
	      ;; 	(newline "¢")
	      ;; 	(setf new-paragraph nil))
	      ;; (grout-color :white :default l)
	      ;; (dbugf :poo "other line ~s~%" l)
	      ;; (floo)
	      ;; (newline "¶")
	      )))
	(print-paragraph)))))

(defmethod format-comment-text ((token lisp-token) stream &key columns)
  (check-type (token-object token) string)
  (format-lisp-comment (token-object token) stream :columns columns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-char-table (source)
  "Make a character trait table."
  (let ((v (make-array
	    (list (1+ (loop :with c
			 :for pair :in source
			 :do (setf c (car pair))
			 :maximize (etypecase c
				     (character (char-code c))
				     (cons (char-code (cadr c)))
				     (array (char-code (svref c 1))))))))))
				  
    (loop :for (c x) :in source
       :do
       (etypecase c
	 (character			; single char
	  (setf (svref v (char-code c)) x))
	 (array				; range
	  (loop :for i :from (char-code (aref c 0)) :upto (char-code (aref c 1))
	     :do (setf (svref v i) x)))
	 (cons				; individual chars
	  (loop :for i :in c
	     :do (setf (svref v (char-code i)) x)))))
    v))

(defparameter *char-syntax*
  '((#\Backspace  :constituent)
    (#\Tab        :whitespace)
    (#\Newline    :whitespace)
    (#\Linefeed   :whitespace)
    (#\Page       :whitespace)
    (#\Return     :whitespace)
    (#\Space      :whitespace)
    (#\!          :constituent)
    (#\"          :terminating-macro-char)
    (#\#          :non-terminating-macro-char)
    (#\$          :constituent)
    (#\%          :constituent)
    (#\&          :constituent)
    (#\'          :terminating-macro-char)
    (#\(          :terminating-macro-char)
    (#\)          :terminating-macro-char)
    (#\*          :constituent)
    (#\+          :constituent)
    (#\,          :terminating-macro-char)
    (#\-          :constituent)
    (#\.          :constituent)
    (#\/          :constituent)
    (#(#\0 #\9)   :constituent)
    (#\:          :constituent)
    (#\;          :terminating-macro-char)
    (#\<          :constituent)
    (#\=          :constituent)
    (#\>          :constituent)
    (#\?          :constituent)
    (#\@          :constituent)
    (#(#\A #\Z)   :constituent)
    (#\[          :constituent)
    (#\\          :single-escape)
    (#\]          :constituent)
    (#\^          :constituent)
    (#\_          :constituent)
    (#\`          :terminating-macro-char)
    (#(#\a #\z)   :constituent)
    (#\{          :constituent)
    (#\|          :multiple-escape)
    (#\}          :constituent)
    (#\~          :constituent)
    (#\Rubout     :constituent))
  "Lisp character syntax types.")

(defparameter *char-syntax-table* (make-char-table *char-syntax*)
  "Array of Lisp character syntax types indexed by char-code.")

(defun char-syntax (c)
  "Return the Lisp character syntax type."
  (svref *char-syntax-table* (char-code c)))

(defun is-char-syntax (text start type)
  "To be used in a function terminal for a character of syntax type TYPE.
TEXT and START should be passed from the caller."
  (let ((syntax (char-syntax (aref text start))))
    (when (typecase type
	    (list (member syntax type))
	    (t (eq syntax type)))
      (values syntax (1+ start) nil))))

(defun t-whitespace (text start end)
  (declare (ignore end))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (is-char-syntax text start :whitespace))

(defun t-macro-char (text start end)
  (declare (ignore end))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (is-char-syntax text start
		  '(:terminating-macro-char :non-terminating-macro-char)))

;; * = shadowed
(defparameter *constituent-trait*
  '((#\Backspace   :invalid)
    (#\Tab         :invalid)    ;; *
    (#\Newline     :invalid)    ;; *
    (#\Linefeed    :invalid)    ;; *
    (#\Page        :invalid)    ;; *
    (#\Return      :invalid)    ;; *
    (#\Space       :invalid)    ;; *
    (#\!           :alphabetic)
    (#\"           :alphabetic) ;; *
    (#\#           :alphabetic) ;; *
    (#\$           :alphabetic)
    (#\%           :alphabetic)
    (#\&           :alphabetic)
    (#\'           :alphabetic) ;; *
    (#\(           :alphabetic) ;; *
    (#\)           :alphabetic) ;; *
    (#\*           :alphabetic)
    (#\,           :alphabetic) ;; *
    ((#\0 #\9)     :alphadigit)
    (#\:           :package-marker)
    (#\;           :alphabetic) ;; *
    (#\<           :alphabetic)
    (#\=           :alphabetic)
    (#\>           :alphabetic)
    (#\?           :alphabetic)
    (#\@           :alphabetic)
    (#\[           :alphabetic)
    (#\\           :alphabetic) ;; *
    (#\]           :alphabetic)
    (#\^           :alphabetic)
    (#\_           :alphabetic)
    (#\`           :alphabetic) ;; *
    (#\|           :alphabetic) ;; *
    (#\~           :alphabetic)
    (#\{           :alphabetic)
    (#\}           :alphabetic)
    (#\+           (:alphabetic :plus-sign))
    (#\-           (:alphabetic :minus-sign))
    (#\.           (:alphabetic :dot :decimal-point))
    (#\/           (:alphabetic :ratio-marker))
    ((#\A #\a)     :alphadigit)
    ((#\B #\b)     :alphadigit)
    ((#\C #\c)     :alphadigit)
    ((#\D #\d)     (:alphadigit :double-float-exponent-marker))
    ((#\E #\e)     (:alphadigit :float-exponent-marker))
    ((#\F #\f)     (:alphadigit :single-float-exponent-marker))
    ((#\G #\g)     :alphadigit)
    ((#\H #\h)     :alphadigit)
    ((#\I #\i)     :alphadigit)
    ((#\J #\j)     :alphadigit)
    ((#\K #\k)     :alphadigit)
    ((#\L #\l)     (:alphadigit :long-float-exponent-marker))
    ((#\M #\m)     :alphadigit)
    ((#\N #\n)     :alphadigit)
    ((#\O #\o)     :alphadigit)
    ((#\P #\p)     :alphadigit)
    ((#\Q #\q)     :alphadigit)
    ((#\R #\r)     :alphadigit)
    ((#\S #\s)     (:alphadigit :short-float-exponent-marker))
    ((#\T #\t)     :alphadigit)
    ((#\U #\u)     :alphadigit)
    ((#\V #\v)     :alphadigit)
    ((#\W #\w)     :alphadigit)
    ((#\X #\x)     :alphadigit)
    ((#\Y #\y)     :alphadigit)
    ((#\Z #\z)     :alphadigit)
    (#\Rubout      :invalid)))

(defparameter *constituent-trait-table* (make-char-table *constituent-trait*)
  "Array of Lisp character syntax types indexed by char-code.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbers

#|
numeric-token <- integer | ratio | float       
integer       <- [sign] decimal-digit+ decimal-point |
                 [sign] digit+      
ratio         <- [sign] {digit}+ slash {digit}+    
float         <- [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent] |
                 [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
exponent      <- exponent-marker [sign] {digit}+
|#

#|

(defun digity (text position end)
  "Succeed if positioned at a digit character in the current *READ-BASE*."
  ;;(declare (ignore end))
  (if (< position end)
      (let ((c (char text position)))
	(if (digit-char-p c *read-base*)
	    (values c (1+ position) t)
	    (values nil position)))
      (values nil position)))

(defun to-integer (sign digit-list)
  "Make an integer from a sign and a list of digits."
  (let ((num (parse-integer (text digit-list) :radix *read-base*)))
    (if (and sign (and (stringp sign) (char= (char sign 0) #\-)))
	(- num)
	num)))

(defun float-format-code (code)
  (ecase (char code 0)
    ((#\D #\d) 'double-float)
    ((#\E #\e) *read-default-float-format*)
    ((#\F #\f) 'single-float)
    ((#\L #\l) 'long-float)
    ((#\S #\s) 'short-float)))

(defun digit-value (s)
  (ecase (char s 0)
    (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8)
    (#\9 9)))

(defrule sign (or #\+ #\-))
(defrule decimal-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defrule exponent-marker (or #\D #\d #\E #\e #\F #\f #\L #\l #\S #\s))
(defrule digit #'digity)

(defrule exponent (and exponent-marker (? sign) (+ digit))
  (:lambda (l)
    (list (first l) (to-integer (second l) (third l)))))
	
(defrule float (or (and (? sign) (* decimal-digit) #\. (+ decimal-digit)
			(? exponent))
		   (and (? sign) (+ decimal-digit)
			(? (and #\. (* decimal-digit)))
			exponent))
  (:lambda (l)
    ;;(format t "~s~%" l)
    (let* ((sign (first l))
	   (denominator 1)
	   (numerator (if (second l)
			  (let ((num (digit-value (first (second l)))))
			    (loop :for c :in (cdr (second l))
			       :do
			       (setf num (+ (* num 10) (digit-value c))))
			    num)
			  0))
	   (size (if (car (last l))
		     (float-format-code (caar (last l)))
		     *read-default-float-format*))
	   (exponent (when (car (last l))
		       (cadar (last l))))
	   result)
      ;;(format t "hi ~s ~s ~s~%" numerator denominator exponent)
      (when (third l)
	(loop :for c :in (fourth l)
	   :do
	   (setf numerator (+ (* numerator 10) (digit-value c))
		 denominator (* denominator 10))))
      (setf result (float (/ numerator denominator) (coerce 0 size)))
      (when exponent
	(setf result (* result (expt 10 exponent))))
      (if (and sign (char= (char sign 0) #\-))
	  (- result)
	  result))))

(defrule ratio (and (? sign) (+ digit) #\/ (+ digit))
  (:lambda (l)
    (/ (to-integer (first l) (second l)) (to-integer nil (fourth l)))))

(defrule integer (or (and (? sign) (+ decimal-digit) #\.)
		     (and (? sign) (+ digit)))
  (:lambda (l)
    (to-integer (first l) (second l))))

(defrule numeric-token (or float ratio integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule whitespace (+ #'t-whitespace)
  (:constant nil))

(defrule line-comment (and #\; (* (not #\newline)) (* #\newline))
  ;;(:lambda (l) (list :comment (text (second l))))
  (:constant nil)
  )

;;(defrule balanced-comment (and #\# 

(defrule space (+ (or comment whitespace))
  ;;(:lambda (l) (if (eq :comment (caar l)) (car l) nil))
  (:constant nil)
  )

;;(defule lisp-expr (? whitespace) (macro-char

|#

#|
(defmethod colorize ((syntax syntax-lisp) string)
  (let ((fs (etypecase string
	      (fat-string string)
	      (string (make-fat-tring string)))))
    (positioning-read-from-string )
  )
|#

;; EOF
