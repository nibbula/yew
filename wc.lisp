;;
;; wc.lisp - Word (and other textual unit) count.
;;

(defpackage :wc
  (:documentation "Word (and other textual unit) count.")
  (:use :cl :dlib :opsys)
  (:export
   #:count-thing
   #:count-words
   #:count-lines
   #:count-characters
   #:count-text
   #:wc
   #:!wc
   ))
(in-package :wc)

;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

(defconstant +newline-length+ #+unix 1 #+windows 2
  "How many characters are considered a newline by read-line.")
(declaim (type fixnum +newline-length+))

;; (define-constant +buffer-size+ #.(* 1024 1024)
;;   "Size of the buffer.")
(defconstant +buffer-size+ #.(* 1024 1024)
   "Size of the buffer.")

(defvar *saved-buffer* nil
  "The buffer saved between invocations.")

(defmacro get-buffer (type)
  "Return an appropriate buffer for TYPE, using *saved-buffer* if possible."
  `(if (and *saved-buffer*
	    (equal (array-element-type *saved-buffer*) ,type))
       *saved-buffer*
       (setf *saved-buffer* (make-array +buffer-size+ :element-type ,type))))

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
       :while (= end +buffer-size+))
    lines))

(defun fast-line-count-fixnum (stream)
  (dbugf :wc "fast-line-count-fixnum~%")
  (let ((lines 0)
	(buf (get-buffer 'character))
	start (end 0))
    (declare (type fixnum lines end)
	     (type (or null fixnum) start)
	     ;;(type (vector character +buffer-size+) buf))
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
       :while (= end +buffer-size+))
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
	     ;;(type (vector (unsigned-byte 8) +buffer-size+) buf))
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
       :while (= end +buffer-size+))
    lines))

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
		    (cerror "Ignore ~s." "~s is a directory." file)
		    (return-from count-thing (values nil nil nil)))
		  (setf stream (open (nos:quote-filename file)
				     :element-type type)
			we-opened t)))))
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
		       (the list (split-sequence
				  " " l :omit-empty t
				  :test #'(lambda (c1 c2)
					    (declare (ignore c1))
					    (= 1 (iswspace (char-int c2))))))))
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

(defun count-text (things &key lines words chars)
  "Count lines, words and characters in THINGS. THINGS can be filenames or
a streams. If one of the keywords :LINES :WORDS :CHARS is true,
then only print the count that thing, otherwise print all of them."
  (let ((do-lines lines) (do-words words) (do-chars chars) (count 0)
	(total-lines 0) (total-words 0) (total-chars 0)
	what
	(*saved-buffer* nil))
    ;;(declare (type integer count total-lines total-words total-chars))
    (declare (type fixnum count total-lines total-words total-chars))
    (when (not (or lines words chars))
      (setf do-lines t do-words t do-chars t))
    (when do-lines (push :lines what))
    (when do-words (push :words what))
    (when do-chars (push :chars what))
    (loop
       :for f :in things
       :do
       (multiple-value-bind (l w c) (count-thing f what)
	 (declare (type (or fixnum null) l w c))
	 (when (and do-lines l)
	   (format t "~8d " l) (incf total-lines l))
	 (when (and do-words w)
	   (format t "~7d " w) (incf total-words w))
	 (when (and do-chars c)
	   (format t "~7d " c) (incf total-chars c)))
       (incf count)
       (format t "~a~%" f))
    (when (> count 1)
      (when do-lines (format t "~8d " total-lines))
      (when do-words (format t "~7d " total-words))
      (when do-chars (format t "~7d " total-chars))
      (format t "total~%"))))

(setf (symbol-function 'wc) #'count-text)

;; Tests:
;(deftest (wc-1 :doc "Test")
;  (eql 69703 (count-words "~/text/books/dblnr10.txt")))

#+lish
(lish:defcommand wc
  ((chars boolean :short-arg #\c :help "True to count characters.")
   (words boolean :short-arg #\w :help "True to words.")
   (lines boolean :short-arg #\l :help "True to count lines.")
   (files pathname :repeating t :default *standard-input*
    :help "Files to count."))
  :accepts (:stream :sequence)
  "Count words, lines, and characters."
  ;; @@@ XXX I shouldn't have to do this. :default is broken?
  (when (not files)
    (dbug "input = ~s [~a]~%" lish:*input* (type-of lish:*input*))
    (setf files
	  (if (and lish:*input*
		   (typep lish:*input* 'sequence))
	      lish:*input*
	      (list *standard-input*))))
  (count-text files :chars chars :words words :lines lines))

;; EOF
