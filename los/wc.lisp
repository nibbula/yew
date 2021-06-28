;;;
;;; wc.lisp - Word (and other textual unit) count.
;;;

(defpackage :wc
  (:documentation "Word (and other textual unit) count.")
  (:use :cl :dlib :opsys :collections)
  (:export
   #:count-thing
   #:count-words
   #:count-lines
   #:count-characters
   #:count-text
   #:count-item
   #:count-item-lines
   #:count-item-words
   #:count-item-chars
   #:count-item-object
   #:make-count-item
   #:count-item
   #:wc
   #:!wc
   ))
(in-package :wc)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

(defconstant +newline-length+ #+unix 1 #+windows 2
  "How many characters are considered a newline by read-line.")
(declaim (type fixnum +newline-length+))

;; (define-constant +wc-buffer-size+ #.(* 1024 1024)
;;   "Size of the buffer.")
(defconstant +wc-buffer-size+ #.(* 1024 1024)
   "Size of the buffer.")

(defvar *saved-buffer* nil
  "The buffer saved between invocations.")

(defvar *signal-errors* nil
  "True to signal errors, instead of printing them.")

(defmacro get-buffer (type)
  "Return an appropriate buffer for TYPE, using *saved-buffer* if possible."
  `(if (and *saved-buffer*
	    (equal (array-element-type *saved-buffer*) ,type))
       *saved-buffer*
       (setf *saved-buffer* (make-array +wc-buffer-size+ :element-type ,type))))

(defun fast-line-count (stream)
  (dbugf :wc "fast-line-count~%")
  (let ((lines 0)
	(buf (get-buffer 'character))
	start (end 0))
    (declare (type integer lines end)
	     (type (or null integer) start))
    (loop 
       :do
       (setf start 0
	     end (read-sequence buf stream))
       (loop :while (and (setf start (position #\newline buf :start start))
			 (< start end))
	  :do
	  (incf start)
	  (incf lines))
       :while (= end +wc-buffer-size+))
    lines))

(defun fast-line-count-fixnum (stream)
  (dbugf :wc "fast-line-count-fixnum~%")
  (let ((lines 0)
	(buf (get-buffer 'character))
	start (end 0))
    (declare (type fixnum lines end)
	     (type (or null fixnum) start)
	     ;;(type (vector character +wc-buffer-size+) buf))
	     (type (vector character *) buf))
    (loop 
       :do
       (setf start 0
	     end (read-sequence buf stream))
       (loop :while (and (setf start (position #\newline buf :start start))
			 (< start end))
	  :do
	  (incf start)
	  (incf lines))
       :while (= end +wc-buffer-size+))
    lines))

(defconstant +newline-code+ (char-code #\newline)
  "The code for a newline.")

(defun fast-line-count-fixnum-byte (stream)
  (dbugf :wc "fast-line-count-fixnum-byte~%")
  (let ((lines 0)
	(buf (get-buffer '(unsigned-byte 8)))
	start (end 0))
    (declare (type fixnum lines end)
	     (type (or null fixnum) start)
	     ;;(type (vector (unsigned-byte 8) +wc-buffer-size+) buf))
	     (type (vector (unsigned-byte 8) *) buf))
    (loop
       :do
       (setf start 0
	     end (read-sequence buf stream))
       ;; METHOD ONE
       (loop :while (and (setf start
       			       (position +newline-code+ buf :start start))
       			 (< start end))
       	  :do
       	  (incf start)
       	  (incf lines))
       ;; METHOD TWO
       ;; (loop :while (< start end)
       ;;    :do
       ;;    (when (= (aref buf start) +newline-code+)
       ;;      (incf lines))
       ;;    (incf start))
       ;; METHOD THREE
       ;; (incf lines (count +newline-code+ buf :end end))
       ;; METHOD FOUR
       ;; (do () ((= start end))
       ;; 	 (when (= (aref buf start) +newline-code+)
       ;; 	   (incf lines))
       ;; 	 (incf start))
       :while (= end +wc-buffer-size+))
    lines))

(declaim (inline space-char-p))
(defun space-char-p (c)
  "Return true if C is a whitespace character."
  ;; (= 1 (iswspace (char-int c)))
  (char-util:whitespace-p c)
  )

(defun count-thing (file things)
  "Count lines, words and characters in FILE. FILE can be a file name or
a stream. Returns the values for lines, words and characters."
  (let ((stream nil) (lines 0) (words 0) (chars 0) (we-opened nil) (l nil)
	length)
    (declare (type integer lines words chars)
	     (type (or null string)))
    (dbugf :wc "count-thing ~s~%" things)
    (labels ((open-it (type)
	       (etypecase file
		 (stream (setf stream file))
		 ((or string pathname)
		  (when (probe-directory file)
		    ;; @@@ This doesn't really solve the problem.
		    (if *signal-errors*
			(progn
			  (cerror "Ignore ~s." "~s is a directory." file)
			  (return-from count-thing (values nil nil nil)))
			(progn
			  (format *error-output* "~s is a directory.~%" file)
			  (invoke-restart 'continue))))
		  (handler-case
		      (setf stream (open (nos:quote-filename file)
					 :element-type type)
			    we-opened t)
		    (error (c)
		      (if *signal-errors*
			  (signal c)
			  (progn
			    (format *error-output* "~a ~a~%" (type-of c) c)
			    (invoke-restart 'continue)))))))))
      (unwind-protect
        (progn
	  (cond
	    ((equal '(:lines) things)
	     (open-it '(unsigned-byte 8))
	     (setf lines
		   (if (and (setf length (ignore-errors (file-length stream)))
			    (< length most-positive-fixnum))
		       (if (equal (stream-element-type stream)
				  '(unsigned-byte 8))
			   (fast-line-count-fixnum-byte stream)
			   (fast-line-count-fixnum stream))
		       (fast-line-count stream))))
	    (t
	     (dbugf :wc "slow-count~%")
	     (open-it 'character)
	     (loop
		:while (setf l (read-line stream nil nil))
		:do
		(incf words
		      (length
		       ;; (the list (split-sequence
		       ;; 		  " " l :omit-empty t
		       ;; 		  :test #'(lambda (c1 c2)
		       ;; 			    (declare (ignore c1))
		       ;; 			    (= 1 (iswspace (char-int c2))))))))
		       (the list (split-sequence-if #'space-char-p
						    l :omit-empty t))))
		(incf lines)
		(incf chars (+ (length l) +newline-length+))
		:finally (return-from nil
			   (values lines words chars))))))
	(when we-opened (close stream)))
      (values lines words chars))))

;; These are just for convenience.

(defun count-words (file-or-stream)
  "Return the number of words in a file or stream."
  (multiple-value-bind (junk1 words junk2)
      (count-thing file-or-stream '(:words))
    (declare (ignore junk1 junk2))
    words))

(defun count-lines (file-or-stream)
  "Return the number of lines in a file or stream."
  (multiple-value-bind (lines junk1 junk2)
      (count-thing file-or-stream '(:lines))
    (declare (ignore junk1 junk2))
    lines))

(defun count-characters (file-or-stream)
  "Return the number of lines in a file or stream.
This probably isn't the best way to do this."
  (multiple-value-bind (junk1 junk2 chars)
      (count-thing file-or-stream '(:chars))
    (declare (ignore junk1 junk2))
    chars))

(defstruct count-item
  "Counts returned by count-text."
  lines
  words
  chars
  object)

(defun count-text (things &key lines words chars (print t) collect)
  "Count lines, words and characters in THINGS. THINGS can be filenames or
a streams. If one of the keywords :LINES :WORDS :CHARS is true,
then only print the count that thing, otherwise print all of them.
Return a list of in the order (lines words chars) containing only the total
count if that item was specified.
When COLLECT is true, the second value is a list of count-item.
If PRINT is nil, don't print any output. The default is T."
  (let ((do-lines lines) (do-words words) (do-chars chars) (count 0)
	(total-lines 0) (total-words 0) (total-chars 0)
	what
	(*saved-buffer* nil)
	result totals)
    ;;(declare (type integer count total-lines total-words total-chars))
    (declare (type fixnum count total-lines total-words total-chars))
    (flet ((print-it (fmt x)
	     (when print
	       (format t fmt x))))
      (when (not (or lines words chars))
	(setf do-lines t do-words t do-chars t))
      (when do-lines (push :lines what))
      (when do-words (push :words what))
      (when do-chars (push :chars what))
      (loop
	 :with item
	 :for f :in things
	 :do
	 (restart-case
	     (progn
	       (multiple-value-bind (l w c) (count-thing f what)
		 (declare (type (or fixnum null) l w c))
		 (when collect
		   (setf item (make-count-item :object f)))
		 (when (and do-lines l)
		   (print-it "~8d " l) (incf total-lines l)
		   (when collect
		     (setf (count-item-lines item) l)))
		 (when (and do-words w)
		   (print-it "~7d " w) (incf total-words w)
		   (when collect
		     (setf (count-item-words item) w)))
		 (when (and do-chars c)
		   (print-it "~7d " c) (incf total-chars c)
		   (when collect
		     (setf (count-item-chars item) c))))
	       (incf count)
	       (print-it "~a~%" f)
	       (when collect
		 (push item result)))
	   (continue ()
	     :report "Skip this file.")))
      (when (> count 1)
	(when do-lines (print-it "~8d " total-lines))
	(when do-words (print-it "~7d " total-words))
	(when do-chars (print-it "~7d " total-chars))
	(print-it "~a~%" "total"))
      (when do-chars (push total-chars totals))
      (when do-words (push total-words totals))
      (when do-lines (push total-lines totals))
      (if collect
	  (values totals result)
	  totals))))

(setf (symbol-function 'wc) #'count-text)

;; Tests:
;(deftest (wc-1 :doc "Test")
;  (eql 69703 (count-words "~/text/books/dblnr10.txt")))

#+lish
(lish:defcommand wc
  ((chars boolean :short-arg #\c :help "True to count characters.")
   (words boolean :short-arg #\w :help "True to words.")
   (lines boolean :short-arg #\l :help "True to count lines.")
   (print boolean :short-arg #\p :default t :help "True to print counts.")
   (collect boolean :short-arg #\c :help "True to collect results.")
   (signal-errors boolean :short-arg #\E
    :help "True to signal errors. Otherwise print them to *error-output*.")
   (files pathname :repeating t :help "Files to count."))
  :accepts (:stream :sequence)
  "Count words, lines, and characters. Return a list of in the order:
lines, words, chars, containing only the total count if that item was specified.
When COLLECT is true, return a list of the total counts and the collected list
of struct COUNT-ITEM."
  (when (not files)
    (setf files
	  (if (and lish:*input*
		   (typep lish:*input* 'sequence)
		   ;; @@@ This is semi-bogus. What if we want to count the
		   ;; contents of a string?
		   (typep (oelt lish:*input* 0) '(or string pathname)))
	      lish:*input*
	      (list *standard-input*))))
  (let ((*signal-errors* signal-errors))
    (setf lish:*output*
	  (if collect
	      (multiple-value-list
	       (count-text files :chars chars :words words :lines lines
			   :print print :collect collect))
	      (count-text files :chars chars :words words :lines lines
			  :print print :collect collect)))))

;; EOF
