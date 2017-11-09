;;
;; syntax-lisp.lisp - Things for dealing with the syntax of Lisp code
;;

(defpackage :syntax-lisp
  (:documentation "Things for dealing with the syntax of Lisp code.")
  (:use :cl :dlib :syntax)
  (:export
   #:lisp-syntax
   #:read-token
   #:matching-paren-position
   ))
(in-package :syntax-lisp)

(declaim (optimize (debug 3)))

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

;; (eval-when (:load-toplevel :execute)
;;   (setf *syntax* (register-syntax (make-instance 'lisp-syntax))))

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

;;; @@@ I really want a tree here, but: collections.lisp!
;;; @@@ Assuming contiguous & increasing 0-9 A-Z a-z
(defparameter *char-type*
  `((#\Backspace . :constituent)
    ,@(loop :for i :from 0 :to 9 :do
	 (cons (char (princ-to-string i) 0) :constituent))
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
    ,@(loop :for i :from (char-code #\A) :to (char-code #\Z) :do
	 (cons (code-char i) :constituent))
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
    ,@(loop :for i :from (char-code #\a) :to (char-code #\z) :do
       (cons (code-char i) :constituent))
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
    (#\Rubout    . :constituent)))

(defun char-type (c)
  "Return the character type of the character C."
  (check-type c character)
  (cdr (find c *char-type* :key #'car)))

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

;; EOF
