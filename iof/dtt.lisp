;;;
;;; dtt.lisp - Delimited Text Tables
;;;

;; This handles CSV and other delimited text format files.
;; I think DTT is a fairly stupid abbreviation, but I didn't want to use
;; just CSV, and I still can't think of anything better.

(defpackage :dtt
  (:documentation
   "This supports reading and writing of various delimited text table formats.
This includes the common CSV - Comma Separated Values, and tab separated values,
or nearly any style of separated value with delimiters and quoting. It has a
few of the most common styles already defined, and defaults to the most common
CSV.

The main interface consists of:
  read-file            - Read a delimited file and return a table as lists.
  read-table           - Read a delimited file and return a table object.
  write-file           - Write sequences to a file.
  write-table-to-file  - Write a table object to a file.

The the format of the file is controled by a STYLE object. You can make a style
object yourself with MAKE-STYLE, or use one of the predefined styles:
   +tab+	      - Tab separated values
   +pipe+	      - Pipe #\| separated values
   +csv-unix+         - Comma separated with Unix #\newline sperated lines.
   +csv-mac+          - Comma separated with MacOS #\return sperated lines.
   +csv-excel+        - Comma separated with Excel CRLF sperated lines.
   +csv-rfc4180+      - Comma separated supposedly conforming to rfc4180.
   +csv-default+      - Comma separated with any line endings and column labels.

Style structures have the following slots:
  delimiter		,	 Character between fields
  quote-character	\"	 To quote fields containing delimiters
  escape-character	\\	 To escape quotes in quoted fields
  input-quote-style	:escape :double :triple
  output-quote-style	:all :minimal :alpha :none
  eol-style		:nl :cr :crnl :any
  eat-whitespace	:both	 True to ignore whitespace
  first-row-labels	t	 True to treat the first row as labels
")
  (:use :cl :dlib :collections :table)
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
   #:*string-package*
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
   ;; functions
   #:read-file
   #:guess-types
   #:read-table
   #:write-file
   #:write-table-to-file
   ;; commands
   #:!read-table
   #:!write-dtt
   ))

(in-package :dtt)

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
;;  		   (compilation-speed 0)))
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
  (first-row-labels	t)
  (strings-as-symbols   nil)) ;; @@@ should this really go in here?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *styles* nil
    "List of predefined text table styles.")

  (defmacro defstyle (name &rest args)
    (let ((const-name (symbolify (s+ "+" name "+")))
	  ;;(str-name (
	  )
      `(progn
	 (defparameter ,const-name (make-style ,@args))
	 (push '(,name . ,const-name) *styles*)))))

(defstyle tab         :delimiter #\tab)
(defstyle pipe        :delimiter #\|)
(defstyle csv-unix    :delimiter #\,
                      :quote-character #\"
                      :escape-character #\\
                      :input-quote-style :escape
                      :output-quote-style :minimal
                      :eol-style :nl)
(defstyle csv-mac     :delimiter #\,
                      :quote-character #\"
                      :escape-character #\\
                      :input-quote-style :escape
                      :output-quote-style :minimal
                      :eol-style :cr)
;; @@@ These may not be exactly right, and should be tested:
(defstyle csv-excel   :delimiter #\,
                      :quote-character #\"
                      :escape-character #\"
                      :input-quote-style :double
                      :output-quote-style :none
                      :eol-style :crnl)
(defstyle csv-rfc4180 :delimiter #\,
                      :quote-character #\"
                      :escape-character #\"
                      :input-quote-style :double
                      :output-quote-style :minimal
                      :eol-style :crnl)
(defstyle tsv         :delimiter #\tab
                      :quote-character #\"
                      :escape-character #\"
                      :input-quote-style :double
                      :output-quote-style :minimal
                      :eol-style :crnl)
(defstyle csv-euro    :delimiter #\;
                      :quote-character #\"
                      :escape-character #\"
                      :input-quote-style :double
                      :output-quote-style :minimal
                      :eol-style :crnl)
(defstyle csv-default)

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

(defvar *string-package* nil
  "A package to ‘intern’ strings for saving space. Use if strings-as-symbols is
set in the style.")

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
	(eat-whitespace		(style-eat-whitespace style))
	(strings-as-symbols     (style-strings-as-symbols style)))
    (flet ((done ()
	     (let ((str (get-output-stream-string col)))
	       (when (or (eql eat-whitespace :trailing)
			 (eql eat-whitespace :both))
		 (setf str (string-right-trim *whitespace-chars* str)))
	       (push (if (and strings-as-symbols *string-package*)
			 (intern str *string-package*)
			 str)
		     row))))
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
		((or (eql eol-style :crnl) #| (eql eol-style :any) |#)
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
then the second value is the labels. If style-strings-as-symbols is set in the
style, use *string-package* or a temporary package to intern strings."
  (with-open-file-or-stream (stream file-or-stream :direction :input)
    (let (labels recs temp-package)
      (flet ((read-rows ()
	       (when (style-first-row-labels style)
		 (setf labels (read-row stream :style style)))
               (setf recs
		     (loop :with rec
		       :do (setf rec (read-row stream :style style))
		       :while (not (eql rec :eof))
		       :collect rec))))
	(if (style-strings-as-symbols style)
	    (unwind-protect
		 (let ((*string-package*
			 (or *string-package*
			     (setf temp-package
				   (make-package (gensym "dtt"))))))
		   ;; (format t "*string-package* = ~s~%" *string-package*)
		   (read-rows))
	      (when temp-package
		(delete-package temp-package)))
	    (progn
	      ;; (format t "strings-as-symbols = ~s~%"
	      ;; 	      (style-strings-as-symbols style))
	      (read-rows)))
	(values recs labels)))))

;; @@@ Of course this should be somewhere else, probably syntax-lisp.
;; And it maybe it should make the actual number while we're at it?
(defun potential-number-p (string &key junk-allowed)
  (let ((i 0) results)
    (macrolet ((optional (&body body)
		 "If BODY doesn't evaluate to true, restore the point."
		 (with-unique-names (start result)
		   `(let ((,start i) ,result)
		      (when (not (setf ,result (progn ,@body)))
			(setf i ,start))
		      t)))
	       (must-be (&body body)
		 "BODY must evaluate true."
		 (with-unique-names (start result)
		   `(let ((,start i) ,result)
		      (when (not (setf ,result (progn ,@body)))
			(setf i ,start))
		      ,result)))
	       (sequence-of (&body body)
		 "Every form of BODY must be true."
		 (with-unique-names (start result)
		   `(let ((,start i) ,result)
		      (when (not (setf ,result (and ,@body)))
			(setf i ,start))
		      ,result)))
	       (one-or-more (&body body)
		 "Do the body, once and as many more times as it returns true."
		 (with-unique-names (thunk)
		   `(flet ((,thunk () ,@body))
		      (and (,thunk)
			   (or (loop :while (,thunk)) t))))) ;; @@@ remove or?
	       (zero-or-more (&body body)
		 "Do the body, until it returns false. Always true."
		 (with-unique-names (thunk)
		   `(flet ((,thunk () ,@body))
		      (loop :while (,thunk))
		      t)))
	       (one-of (&body body)
		 "Return after the first optional expression of BODY is true."
		 `(or ,@(loop :for expr :in body
			   :collect `(must-be ,expr))))
	       (note ((x) &body body)
		 (with-unique-names (r)
		   `(let ((,r (progn ,@body)))
		      (when ,r
			(push ,x results))
		      ,r))))
      (labels (;; Utility functions
	       (peek ()
		 (when (< i (length string))
		   (char string i)))
	       (next-char ()
		 (if (< i (length string))
		     (prog1 (char string i)
		       (incf i))
		     (throw 'eof nil)))
	       (char-in (c string)
		 "True if C is in STRING."
		 (find c string :test #'char=))

	       ;; Pieces of numbers:
	       (exponent-marker ()
		 (and (peek)
		      (char-in (char-downcase (peek)) "defls")
		      (next-char)))
	       (sign ()
		 (and (peek) (char-in (peek) "+-")
		      (next-char)))
	       (digit () ;; al monsters
		 "Succeed if positioned at a digit character in *READ-BASE*."
		 (and (peek)
		      (digit-char-p (peek) *read-base*)
		      (next-char)))
	       (decimal-digit ()
		 (and (peek)
		      (char-in (peek) "0123456789")
		      (next-char)))
	       (decimal-point ()
		 (and (peek) (char= (peek) #\.)
		      (next-char)))
	       (exponent ()
		 (sequence-of (exponent-marker)
			      (optional (sign))
			      (one-or-more (digit))))
	       (slash ()
		 (and (peek)
		      (char= (peek) #\/)
		      (next-char)))
	       (@ratio ()
		 (note (:ratio)
		       (sequence-of (optional (sign))
				    (one-or-more (digit))
				    (slash)
				    (one-or-more (digit)))))
	       (@integer ()
		 (note (:integer)
		       (one-of (sequence-of (optional (sign))
					    (one-or-more (decimal-digit))
					    (decimal-point))
			       (sequence-of (optional (sign))
					    (one-or-more (digit))))))
	       (@float ()
		 (note (:float)
		       (one-of (sequence-of (optional (sign))
					    (zero-or-more (decimal-digit))
					    (decimal-point)
					    (one-or-more (decimal-digit))
					    (optional (exponent)))
			       (sequence-of (optional (sign))
					    (one-or-more (decimal-digit))
					    (optional
					     (sequence-of
					      (decimal-point)
					      (zero-or-more (decimal-digit))))
					    (exponent)))))
	       (numeric-token ()
		 (one-of (@ratio)
			 (@float)
			 (@integer))))
	;; (and (catch 'eof
	;;        (numeric-token))
	;;      (and (not junk-allowed)
	;; 	  (>= i (length string))
	;; 	  ;; (error "Junk was found! Not allowed! Reported!")
	;; 	  nil))))))
	(values (and (catch 'eof (numeric-token))
		     (and (not junk-allowed)
			  (>= i (length string))))
		results)))))

(defun guess-column-types (table #| &key convert-p |#)
  "Try to set column types for TABLE from the contents. How it works:
If every object in a column:
  - is a number or string that looks like a number, the type is NUMBER.
  - is the same TYPE-OF, than use that.
  - Anything else just set to T."
  (let ((guess (make-sequence 'vector (length (table-columns table))
			      :initial-element nil)))
    ;; @@@ wouldn't it be faster to go through by row?
    (loop
       :with e
       :for col :in (table-columns table)
       :for i = 0 :then (1+ i)
       :do
       (block nil
	 (omapn (_
		 (setf e (oelt _ i))
		 ;; (format t "~s " e)
		 (typecase e
		   (number
		    (case (aref guess i)
		      ((nil)
		       ;; (format t "new number~%")
		       (setf (aref guess i) 'number))
		      (number #| still a number |#)
		      (string
		       (when (not (potential-number-p e))
			 ;; (format t "number -> string~%")
			 (setf (aref guess i) 'string)))
		      (symbol
		       (when (not (potential-number-p (string e)))
			 ;; (format t "number -> string~%")
			 (setf (aref guess i) 'symbol)))
		      (otherwise
		       ;; (format t "number -> ~s~%" (type-of e))
		       (setf (aref guess i) (type-of e)))))
		   (string
		    (case (aref guess i)
		      (number
		       (when (not (potential-number-p e))
			 ;; (format t "string -> number~%")
			 (setf (aref guess i) 'string)))
		      ((nil)
		       (if (potential-number-p e)
			   (progn
			     ;; (format t "string -> number~%")
			     (setf (aref guess i) 'number))
			   (progn
			     ;; (format t "new string~%")
			     (setf (aref guess i) 'string))))
		      (string #| still strings |# )
		      (symbol #| maybe ok |# )
		      (otherwise
		       ;; (format t "string -> ~s~%" (type-of e))
		       (setf (aref guess i) t)
		       ;; It's not uniform so just stop.
		       (return nil))))
		   (symbol
		    (case (aref guess i)
		      (number
		       (when (not (potential-number-p (string e)))
			 ;; (format t "string -> number~%")
			 (setf (aref guess i) 'symbol)))
		      ((nil)
		       (if (potential-number-p (string e))
			   (progn
			     ;; (format t "string -> number~%")
			     (setf (aref guess i) 'number))
			   (progn
			     ;; (format t "new string~%")
			     (setf (aref guess i) 'symbol))))
		      (symbol #| still strings |# )
		      (string #| maybe ok |# )
		      (otherwise
		       ;; (format t "string -> ~s~%" (type-of e))
		       (setf (aref guess i) t)
		       ;; It's not uniform so just stop.
		       (return nil))))
		   (t
		    (cond
		      ((or (eq (type-of e) (aref guess i))
			   (typep e (aref guess i)))
		       #| We're still okay, I suppose. |#)
		      ((subtypep e (aref guess i))
		       ;; Set it to a more specific type
		       ;; (setf (aref guess i) (type-of e))
		       ;; @@@ actually don't
		       )
		      ((null (aref guess i))
		       ;; (format t "new ~s~%" (type-of e))
		       (setf (aref guess i) (type-of e)))
		      (t
		       ;; (format t "~s -> t~%" (type-of e))
		       (setf (aref guess i) t)
		       ;; It's not uniform, so bail out.
		       (return nil))))))
		table)))
	guess))

(defun convert-data (from to)
  "Return FROM converted to type TO, or leave it alone and warn if we can't."
  (flet ((symbolize (s)
	   (if *string-package*
	       (intern s *string-package*)
	       (make-symbol s))))
  (typecase from
    (string
     (case to
       (number (safe-read-from-string from))
       (string from)
       (symbol (symbolize from))
       (t
	(warn "Don't know how to convert from a string to a ~a~%" to)
	from)))
    (number
     (case to
       (number from)
       (string (princ-to-string from))
       (symbol (symbolize (princ-to-string from)))
       (t
	(warn "Don't know how to convert from a number to a ~a~%" to)
	from)))
    (null
     (case to
       (nil nil)
       (symbol nil)
       (string "")
       ;; (number 0) I don't think we want to do this.
       (otherwise
	(warn "Don't know how to convert from a NIL to a ~a~%" to))))
    (symbol
     (case to
       (number (safe-read-from-string (string from)))
       (string (string from))
       (symbol from)
       (t
	(warn "Don't know how to convert from a NIL to a ~a~%" to))))
    (t
     (warn "Don't know how to convert from a ~s to a ~a~%" (type-of from) to)
     from))))

(defun set-column-type-guesses (table guess)
  "Convert the column data in TABLE to the types in GUESSES."
  (loop :for i :from 0 :below (length guess) :do
     (table-set-column-type table i (aref guess i)))
  (omapn (_ (loop :for i :from 0 :below (min (olength _) (length guess)) :do
	       (setf (oelt _ i) (convert-data (oelt _ i) (oelt guess i)))))
	 table))

;; @@@ This is so useful, maybe we should move it to the table package?
(defun guess-types (table)
  "Guess column types in TABLE and set them, returning the updated TABLE."
  (set-column-type-guesses table (guess-column-types table))
  table)

(defun read-table (file-or-stream &key (style +csv-default+)
				    columns column-names guess-types
				    strings-as-symbols)
  (let (table)
    (when strings-as-symbols
      (setf style (copy-style style)
	    (style-strings-as-symbols style) t))
    (multiple-value-bind (recs labels)
	(read-file file-or-stream :style style)
      (setf table (make-table-from (coerce recs 'vector)
				   :column-names (or column-names labels)
				   :columns columns)))
    (when guess-types
      (set-column-type-guesses table (guess-column-types table)))
    table))

#+lish
(lish:defcommand read-table
  ((file pathname :help "File to read a table from.")
   (guess-types boolean :short-arg #\g :help "Guess column data types.")
   (first-row-labels boolean :short-arg #\l :default t
    :help "True to use the first row of the table as labels, not data.")
   (strings-as-symbols boolean :short-arg #\S
    :help "Save strings as symbols. This can help save space.")
   (style choice :short-arg #\s :default 'csv-default
	  :choices (mapcar (_ (string-downcase (car _))) *styles*)
	  ;; :test #'symbolify
	  :help "Delimited text style.")
   (columns list :short-arg #\C
    :help "List of table column initializers. For example:
((:name \"Name\" :type string :width 20)
 (:name \"Percent\" :type integer :format \"~d%\"))"))
  :accepts (pathname stream)
  "Read a delimited text table."
  (let ((real-style (symbol-value (symbolify (s+ "+" style "+") :package :dtt))))
    (lish:with-streamlike-input (file :use-stdin t)
      (setf (style-first-row-labels real-style) first-row-labels
	    lish:*output* (read-table file
				      :style real-style :columns columns
				      :guess-types guess-types
				      :strings-as-symbols strings-as-symbols)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing

(defun write-datum (object stream)
  (let ((*print-array* t)
	(*print-escape* nil)
	(*print-gensym* t)
	(*print-level* nil)
	(*print-length* nil)
	(*print-readably* nil)
	(*print-pretty* nil))
    (format stream "~@[~w~]" object)))

(defun needs-quoting (obj style)
  "Return true if OBJ needs quoting in the given style."
  (with-slots (output-quote-style quote-character delimiter) style
    (case output-quote-style
      (:all t)					; Always
      (:alpha (not (typep obj 'number)))	; If it's not a number.
      (:minimal					; If it needs it
       ;; If the string representation has a delimiter, quote, or EOL
       ;; @@@ This is inefficient to seach through it 4 times. Consider merging
       ;; with quotify output.
       (let ((str (write-datum obj nil))
	     (chars (make-array
		     4 :element-type 'character
		     :initial-contents (vector delimiter quote-character
					       #\newline #\return))))
	 ;; (declare (type (vector character) str chars))
	 (declare (type string str chars))
	 (position-if #'(lambda (c)
			  (declare (type character c))
			  (position c chars))
		      str)))
      (:none nil))))				; Never

(defun quotify (obj style)
  "Return OBJ as a string properly quoted for the style."
  (with-slots (output-quote-style quote-character escape-character) style
    (with-output-to-string (stream)
      (princ quote-character stream)
      (let ((str (write-datum obj nil))) ; Don't print NIL
	(declare (type simple-string str))
	(loop :for c :across str :do
	   (when (char= c quote-character)
	     (princ escape-character stream))
	   (princ c stream))
	(princ quote-character stream)))))

(defun write-row (stream row &key (style +csv-default+))
  "Write a row of data to the output stream STREAM. Use the style defined by
the style object STYLE, which defaults to +CSV-DEFAULT+. ROW can be "
  (when (not style)
    (setf style +csv-default+))
  (let ((delimiter (style-delimiter style))
	(eol-style (style-eol-style style))
	(first-time t))
    (omapn #'(lambda (x)
		 (if first-time
		     (setf first-time nil)
		     (princ delimiter stream))
		 (if (needs-quoting x style)
		     (write-datum (quotify x style) stream)
		     (write-datum x stream)))
	   row)
    (case eol-style
      (:nl (write-char #\newline stream))
      (:cr (write-char #\return stream))
      (:crnl (write-char #\return stream)
	     (write-char #\newline stream))
      (t (terpri stream)))))

(defun write-file (file-or-stream rows &key (style +csv-default+) labels)
  "Write the whole table to the given file or stream. ROWS is a sequence of
sequences of objects to write. Use the given style which defaults to
+CSV-DEFAULT+. Use the list of labels provided in LABELS."
  (with-open-file-or-stream (stream file-or-stream :direction :output)
    (when labels
      (write-row stream labels :style style))
    (omapn #'(lambda (row)
	       (write-row stream row :style style))
	   rows))
  (values))

(defun write-table-to-file (file-or-stream table &key (style +csv-default+)
						   (labels nil labels-p))
  "Write the whole table to the given file or stream. ROWS is a sequence of
sequences of objects to write. Use the given style which defaults to
+CSV-DEFAULT+. Use the list of labels provided in LABELS."
  (write-file file-or-stream
	      table
	      :style style
	      :labels (if labels-p
			  labels
			  (and (table-columns table)
			       (mapcar #'column-name
				       (table-columns table))))))

#+lish
(lish:defcommand write-dtt
  ((file pathname :help "File to write a table to.")
   (table object :help "Table to output.")
   (style choice :short-arg #\s :default 'csv-default
	  :choices (mapcar (_ (string-downcase (car _))) *styles*)
	  ;; :test #'symbolify
	  :help "Delimited text style."))
  :accepts (table)
  "Write a delimited text table."
  (when (not (or table lish:*input*))
    ;; @@@ I want to do some kind of use-value restart, but how?
    ;; Ideally it could use the prompt from the table argument.
    (error "Please give me a table to output."))

  (setf table (or table lish:*input*))
  (when (not (typep table 'table))
    (error
     "Please make table be a table object. You could use table:make-table-from,
to do so if you want to."))

  (when (not file)
    (error "Please give me an output file name."))

  (let ((real-style (symbol-value (symbolify (s+ "+" style "+") :package :dtt))))
    (write-table-to-file file table :style real-style)))

;; EOF
