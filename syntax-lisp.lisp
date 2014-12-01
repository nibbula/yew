;;
;; syntax-lisp.lisp - Things for dealing with the syntax of Lisp code
;;

;; $Revision: 1.1 $

(defpackage :syntax-lisp
  (:documentation "Things for dealing with the syntax of Lisp code.")
  (:use :cl :dlib :syntax)
  (:export
   #:lisp-syntax
   #:read-token
   #:matching-paren-position
   ))
(in-package :syntax-lisp)

;; The funny thing about Lisp is, you can't read entirely perfectly it without
;; also evaluating it. That means that "read-only" lexical analysis will fail
;; for programs that modify the syntax. To read it perfectly without
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
  (declare (ignore s))
)

;; This done in a temporary hackish way, until we make the whole reader.
;; I'm curious to compare the performance of this vs a rule 184 based
;; implementation.

(defun matching-paren-position (string &key (position (length string)))
  "Return the position in STRING of an open paren matching a, perhaps
hypothetical, one at POSITION. If there is none, return nil. If POSITION is
not provided, it defaults to the end of the string."
  (declare (type string string))
  (let ((starts nil))
    (loop
       :with i = 0 :and c = nil
       :while (< i position)
       :do (setf c (aref string i))
       (cond
	 ((or (eql c #\() (eql c #\[)) (push i starts))
	 ((or (eql c #\)) (eql c #\])) (pop starts))
	 ((eql c #\")
	  ;; scan past the string
	  (incf i)
	  (loop :while (and (< i position)
			    (not (eql #\" (setf c (aref string i)))))
	     :do (when (eql c #\\)
		   (incf i))
	     (incf i)))
	 ;; # reader macro char
	 ((and (eql c #\#) (< i (- position 2)))
	  (incf i)
	  (case (setf c (aref string i))
	    (#\\ (incf i))		; ignore the quoted char
	    ;; scan past matched #| comments |#
	    (#\|
	     (incf i)
	     (loop :with level = 1
		:while (and (< i position) (> level 0))
		:do
		  (setf c (aref string i))
		  (cond
		    ((and (eql c #\|) (and (< i (1- position))
					   (eql #\# (aref string (1+ i)))))
		     (decf level))
		    ((and (eql c #\#) (and (< i (1- position))
					   (eql #\| (aref string (1+ i)))))
		     (incf level)))
		(incf i)))
	    ;; vectors, treated just like a list
	    (#\( (push i starts))))
	 ;; single line comment
	 ((eql c #\;)
	  (loop :while (and (< i position)
			    (not (eql #\newline (setf c (aref string i)))))
	     :do (incf i))))
       (incf i))
    (first starts)))

;; EOF
