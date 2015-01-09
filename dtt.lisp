;;
;; dtt.lisp - Delimited Text Tables
;;

;; This handles CSV and other delimited text format files.
;; I think DTT is a fairly stupid abbreviation, but I didn't want to use
;; just CSV, and I can't think of anything else at the momemnt.

;; $Revision: 1.2 $

(defpackage :dtt
  (:documentation "Delimited Text Tables")
  (:use :cl :dlib :table)
  (:export
   ;; vars
   #:*delimiter*
   #:*quote-character*
   #:*escape-character*
   #:*input-quote-style*
   #:*output-quote-style*
   #:*eol-style*
   #:*eat-whitespace*
   #:*first-row-labels*
   ;; style struct
   #:style
   #:make-style
   #:style-delimiter
   #:style-quote-character
   #:style-escape-character
   #:style-input-quote-style
   #:style-output-quote-style
   #:style-eol-style
   #:style-eat-whitespace
   #:style-first-row-labels
   ;; styles
   #:+tab+
   #:+pipe+
   #:+csv-unix+
   #:+csv-mac+
   #:+csv-excel+
   #:+csv-rfc4180+
   ;; funcs
   #:read-file
   #:read-row
   ))
(in-package :dtt)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
 		   (compilation-speed 0)))
;; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)
;;  		   (compilation-speed 0)))

;; Basic pre-defined formats:
;;  tab
;;  pipe
;;  csv-guess
;;  csv-excel
;;  csv-simple		
;;
;; delimiter		,	Character between fields
;; quote-character	"	To quote fields containing delimiters
;; escape-character	\	To escape quotes in quoted fields
;; input-quote-style	:escape :double :triple
;; output-quote-style	:all :minimal :alpha :none
;; eol-style		:nl :cr :crnl :any
;; eat-whitespace	:both	True to ignore whitespace
;; first-row-labels	t	True to treat the first row as labels
;;
;; NOTE: Rows may span multiple lines with quoted newlines.
;;
;; Detector:
;;   Look for:
;;     <delim><quote>something<quote><delim>
;;   with likely combos. Use most occuring one.
;;
;;   Consider:
;;     Delimiters:
;;     - Single column case
;;       - No delimiter sub case
;;     - Extra quote between delimiters, may mean double quoted format
;;     - choose most common number of columns
;;     - ?analyze delimiters with a minimum consistency threshold
;;   Headers:
;;     - deviation only in first row in guessed type, or length indicates
;;       header

(defvar *delimiter* #\,
  "Character between columns.")

(defvar *quote-character* #\"
  "Character starting and ending columns which contain delimiters or newlines,
or nil for no quoting.")

(defvar *escape-character* #\\
  "Character to precede quote characters in columns, or nil for no escaping.")

(defvar *input-quote-style* :escape
  "How to handle quoting on input. :escape means use the *escape-character*.
:double or :triple means use multiple *quote-character*s.")

(defvar *output-quote-style* :minimal
  "How to quote stuff when outputting.
:all      Quote every column.
:minimal  Quote only columns containing delimiters or newlines.
:alpha    Quote alphabetic columns, but not numeric columns.
:none     Don't do any quoting.")

(defvar *eol-style* :nl
  "How to handle the end of the line.
:nl    The end of line is one #\newline character.
:cr    The end of line is one #\return character.
:crnl  The end of line is a #\return and a #\newline.
:any   Take either NL or CR-NL.")

(defvar *eat-whitespace* nil
  "True to ignore whitespace. True values can be :leading, :trailing,
or :both.")

(defvar *first-row-labels* t
  "True to treat the first row as labels.")

;(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct style
  "Describes a style of delimited text table."
  (delimiter		#\,)
  (quote-character	#\")
  (escape-character	#\\)
  (input-quote-style	:double)
  (output-quote-style	:minimal)
  (eol-style		:any)
  (eat-whitespace	nil)
  (first-row-labels	t))

(defparameter +tab+         (make-style :delimiter #\tab))
(defparameter +pipe+        (make-style :delimiter #\|))
(defparameter +csv-unix+    (make-style :delimiter #\,
				       :quote-character #\"
				       :escape-character #\\
				       :input-quote-style :escape
				       :output-quote-style :minimal
				       :eol-style :nl))
(defparameter +csv-mac+     (make-style :delimiter #\,
				       :quote-character #\"
				       :escape-character #\\
				       :input-quote-style :escape
				       :output-quote-style :minimal
				       :eol-style :cr))
;; @@@ These may not be exactly right, and should be tested:
(defparameter +csv-excel+   (make-style :delimiter #\,
				       :quote-character #\"
				       :escape-character #\"
				       :input-quote-style :double
				       :output-quote-style :none
				       :eol-style :crnl))
(defparameter +csv-rfc4180+ (make-style :delimiter #\,
				       :quote-character #\"
				       :escape-character #\"
				       :input-quote-style :double
				       :output-quote-style :minimal
				       :eol-style :crnl))
(defparameter +tsv+	   (make-style :delimiter #\tab
				       :quote-character #\"
				       :escape-character #\"
				       :input-quote-style :double
				       :output-quote-style :minimal
				       :eol-style :crnl))
(defparameter +csv-euro+   (make-style :delimiter #\;
				       :quote-character #\"
				       :escape-character #\"
				       :input-quote-style :double
				       :output-quote-style :minimal
				       :eol-style :crnl))
(defparameter +csv-default+ (make-style))

(defparameter *whitespace-chars* (make-array
				  2 :element-type 'character
				  :initial-contents #(#\space #\tab))
  "Characters that are considered whitespace.")
(declaim (type (vector character) *whitespace-chars*))

(defun whitespacep (c)
  "True if the character is considered whitespace."
  (position c *whitespace-chars*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading

;; @@@ TODO:
;; - What about when input-quote-style :double and: 23,"","->"","",
;; - :eol-style :any

(defun read-row (stream &key (style +csv-default+))
  (when (not style)
    (setf style +csv-default+))
  (let ((row '())
	(col (make-string-output-stream))
	(delimiter		(style-delimiter style))
	(quote-character	(style-quote-character style))
	(escape-character	(style-escape-character style))
	(input-quote-style	(style-input-quote-style style))
	(eol-style		(style-eol-style style))
	(eat-whitespace		(style-eat-whitespace style)))
    (flet ((done ()
	     (let ((str (get-output-stream-string col)))
	       (when (or (eql eat-whitespace :trailing)
			 (eql eat-whitespace :both))
		 (setf str (string-right-trim *whitespace-chars* str)))
	       (push str row))))
      (handler-case
	(loop :with c
	       :and quoting
	       :and eating-whitespace = (or (eql eat-whitespace :leading)
					    (eql eat-whitespace :both))
	       :and expecting-newline
	   :do (setf c (read-char stream))
	   (cond
	     (quoting
	      (cond
		((and escape-character (char= c escape-character))
		 (write-char (read-char stream) col))
		((and quote-character (char= c quote-character))
		 (cond
		   ;; double quote in quotes => actuall quote char in column
		   ((and (char= quote-character (peek-char nil stream))
			 (eql input-quote-style :double))
		    (read-char stream)
		    (write-char quote-character col))
    ;; @@@ TODO triple quoting ?
    ;; 	       ((eql input-quote-style :triple)
    ;; 		(if (= quoting 2)
    ;; 		    (write-char quote-character col)
    ;; 		    (incf quoting)))
		   (t
		    ;; Normal close quote
		    (setf quoting nil))))
		(t
		 (write-char c col))))
	     ((and delimiter (char= c delimiter))
	      (done)
	      (setf col (make-string-output-stream))    ; Start a new column
	      (setf eating-whitespace (or (eql eat-whitespace :leading)
					  (eql eat-whitespace :both))))
	     ((and quote-character (char= c quote-character))
	      (setf quoting 1))			    ; Turn on quoting
	     ((and escape-character (char= c escape-character))
	      (write-char (read-char stream) col)) ; Write the next char as-is
	     ((eql c #\newline)
	      (cond
		(expecting-newline
		 (done)
		 (return))
		((eql eol-style :crnl)
		 (write-char #\newline col)) ; Treated as normal
		((or (eql eol-style :nl) (eql eol-style :any))
		 (done)
		 (return))))
	     ((eql c #\return)
	      (cond
		((eql eol-style :nl)
		 (write-char #\return col)) ; Treated as normal
		((or (eql eol-style :crnl) (eql eol-style :any))
		 (setf expecting-newline t))
		((eql eol-style :cr) (done) (return))
		((eql eol-style :any)
		 (done)
		 ;; Eat an optional newline
		 (when (eql (peek-char nil stream) #\newline)
		   (read-char stream))
		 (return))))
	     ((and eating-whitespace (whitespacep c))
	      ;; Do nothing
	      )
	     (t
	      (when (and expecting-newline (eql eol-style :crnl))
		(error "Missing newline?")) ; @@@ should we really error?
	      ;; Normal data character
	      (write-char c col))))
	   (end-of-file (c)
	     (declare (ignore c))
	     (if (> (length (get-output-stream-string col)) 0)
		 (done)
		 (if (null row)
		     (return-from read-row :eof))))))
       (nreverse row)))

;; Not needed since read-row accepts style
;; (defun read-row-with-style (stream &optional style)
;;   (if style
;;       (read-row stream
;; 		:delimiter (style-delimiter style)
;; 		:quote-character (style-quote-character style)
;; 		:escape-character (style-escape-character style)
;; 		:input-quote-style (style-input-quote-style style)
;; 		:eol-style (style-eol-style style)
;; ;		:eat-whitespace (style-eat-whitespace style)
;; 		)
;;       (read-row stream)))

;; (defmacro do-file ((row-var &key style) &body body)
;;   (
;;   ,body
;;   )

(defun read-file (file-or-stream &key (style +csv-default+))
  "Read the whole stream returning the rows as a list. Use the given style
which defaults to +csv-default+. If first-row-labels is true in the style
then the second value is the labels."
  (when (not (streamp file-or-stream))
    (setf file-or-stream (open file-or-stream :direction :input)))
  (let ((recs '())
	(labels nil))
    (when (style-first-row-labels style)
      (setf labels (read-row file-or-stream :style style)))
    (setf recs
	  (loop :with rec
	     :do (setf rec (read-row file-or-stream :style style))
	     :while (not (eql rec :eof))
	     :collect rec))
    (values recs labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing

(defun needs-quoting (obj style)
  "Return true if OBJ needs quoting in the given style."
  (with-slots (output-quote-style quote-character delimiter) style
    (case output-quote-style
      (:all t)					; Always
      (:alpha (not (typep obj 'number)))	; If it's not a number.
      (:minimal					; If it needs it
       ;; If the string representation has a delimiter, quote or EOL
       ;; @@@ This is ineffiecent to seach thru it 4 times. Consider merging
       ;; with quotify output.
       (let ((str (format nil "~a" obj))
	     (chars (make-array
		     4 :element-type 'character
		     :initial-contents (vector delimiter quote-character
					       #\newline #\return))))
	 (declare (type (vector character) str chars))
	 (position-if #'(lambda (c)
			  (declare (type character c))
			  (position c chars))
		      str)))
      (:none nil))))				; Never

(defun quotify (obj style)
  "Return OBJ as a string properly quoted for the style."
  (with-slots (output-quote-style quote-character escape-character) style
    (with-output-to-string (str)
      (princ quote-character)
      (let ((str (format nil "~a" obj)))
	(declare (type simple-string str))
	(loop :for c :across str :do
	   (when (char= c quote-character)
	     (princ escape-character))
	   (princ c))
	(princ quote-character)))))

(defun write-row (stream row &key (style +csv-default+))
  "Write a row of data to the output stream STREAM. Use the style defined by
the style object STYLE, which defaults to +CSV-DEFAULT+. ROW can be "
  (when (not style)
    (setf style +csv-default+))
  (let ((delimiter (style-delimiter style))
	(eol-style (style-eol-style style))
	(first-time t))
    (map nil #'(lambda (x)
		 (if first-time
		     (setf first-time nil)
		     (princ delimiter stream))
		 (if (needs-quoting x style)
		     (princ (quotify x style) stream)
		     (princ x stream))
		 (case eol-style
		   (:nl (write-char #\newline stream))
		   (:cr (write-char #\return stream))
		   (:crnl (write-char #\return stream)
			  (write-char #\newline stream))
		   (t (terpri stream))))
	 row)))

(defun write-file (file-or-stream rows &key (style +csv-default+) labels)
  "Write the whole table to the given file or stream. ROWS is a sequence of
sequences of objects to write. Use the given style which defaults to
+CSV-DEFAULT+. Use the list of labels provided in LABELS."
  (when (not (streamp file-or-stream))
    (setf file-or-stream (open file-or-stream :direction :output)))
  (when labels
    (write-row file-or-stream labels :style style))
  (map nil
       #'(lambda (row)
	   (write-row file-or-stream row :style style))
       rows))

;; EOF
