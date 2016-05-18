;;
;; fatchar.lisp - Characters with attributes.
;;

(defpackage :fatchar
  (:documentation "Characters with attributes.
Defines a FATCHAR which is a character with color and font attributes.
Define a FAT-STRING as a vector of FATCHARS.
Define a TEXT-SPAN as a list representation of a FAT-STRING.
")
  (:use :cl :stretchy :dlib)
  (:export
   #:fatchar
   #:fatchar-p
   #:make-fatchar
   #:fatchar-c #:fatchar-fg #:fatchar-bg #:fatchar-line #:fatchar-attrs
   #:fatchar-string
   #:fatchar-init
   #:copy-fat-char
   #:make-fat-string
   #:fat-string-to-string
   #:span-length
   #:fat-string-to-span
   #:span-to-fat-string
   ))
(in-package :fatchar)

(defstruct fatchar
  "A character with attributes."
  (c (code-char 0) :type character)
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(deftype fatchar-string (&optional size)
  `(vector fatchar ,size))

(defun fatchar-init (c)
  "Initialize a fatchar with the default vaules."
  (setf (fatchar-c     c)	(code-char 0)
	(fatchar-fg    c)	:white
	(fatchar-bg    c)	:black
	(fatchar-line  c)	0
	(fatchar-attrs c)	nil))

(defun copy-fat-char (c)
  (declare (type fatchar c))
  (when c
    (make-fatchar
     :c	    (fatchar-c c)
     :fg    (fatchar-fg c)
     :bg    (fatchar-bg c)
     :line  (fatchar-line c)
     :attrs (fatchar-attrs c))))

(defun make-fat-string (string)
  "Make a fat string from a string."
  (let ((fs (make-array (list (length string))
			:element-type 'fatchar
			:initial-element (make-fatchar))))
    (loop :for i :from 0 :below (length string) :do
       (setf (aref fs i) (make-fatchar :c (char string i))))
    fs))

(defun fat-string-to-string (fat-string)
  "Make a string from a fat string. This of course loses the attributes."
  (let ((s (make-array (list (length fat-string))
		       :element-type 'character)))
    (loop :for i :from 0 :below (length fat-string) :do
       (setf (aref s i) (fatchar-c (aref fat-string i))))
    s))

(defun span-length (span)
  "Calculate the length in characters of the span."
  (the fixnum (loop :for e :in span
		 :sum (typecase e
			(string (length e))
			(cons (span-length e))
			(t 0)))))

(defun listify-fake-span (fake-span)
  "Take a list of characters, strings, and keywords and make nested lists out
of them indicated by the parentheses. Actual parens should be given as
strings."
  (let ((str (make-string-output-stream))
	cur save tmp)
    (flet ((push-if-any ()
	     (when (> (length (setf tmp (get-output-stream-string str))) 0)
	       (push tmp cur))))
      (loop
	 :for x :in fake-span :do
	 (cond
	   ((eql x #\()
	    (push-if-any)
	    (push cur save)
	    (setf cur '()))
	   ((eql x #\))
	    (push-if-any)
	    (setf cur (nreverse cur))
	    (setf tmp (pop save))
	    (push cur tmp)
	    (setf cur tmp))
	   (t
	    (typecase x
	      ((or character string)
	       (princ x str))
	      (t
	       (push x cur)))))
;;;       (format t "~w~%" cur)
	 )
    (push-if-any)
    (nreverse cur))))

;; TODO:
;;  - Add END key

;; (fatchar:fat-string-to-span (pager::process-grotty-line (nth 4 (!_ "/bin/cat grotish.txt"))))

(defun fat-string-to-span (fat-string &key (start 0))
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fat-string) 0)
    (return-from fat-string-to-span fat-string))
  (let ((last (make-fatchar))
	(result '())
	(open-count 0)
	added removed)
;;    (push #\( result)
    (loop :with c
       :for i :from start :below (length fat-string) :do
       (setf c (aref fat-string i))
       ;; Foreground
       (when (not (eql (fatchar-fg c) (fatchar-fg last)))
	 (when (fatchar-fg last)
	   (push #\) result)
	   (decf open-count))
	 (when (fatchar-fg c)
	   (push #\( result)
	   (incf open-count)
	   (push (keywordify (s+ "FG-" (fatchar-fg c))) result)))
       ;; Background
       (when (not (eql (fatchar-bg c) (fatchar-bg last)))
	 (when (fatchar-bg last)
	   (push #\) result)
	   (decf open-count))
	 (when (fatchar-bg c)
	   (push #\( result)
	   (incf open-count)
	   (push (keywordify (s+ "BG-" (fatchar-bg c))) result)))
       ;; Attributes
       (setf added (set-difference (fatchar-attrs c) (fatchar-attrs last))
	     removed (set-difference (fatchar-attrs last) (fatchar-attrs c)))
       (loop :for a :in removed :do
	  (push #\) result)
	  (decf open-count))
       (loop :for a :in added :do
	  (push #\( result)
	  (incf open-count)
	  (push a result))
       ;; Character
       (case (fatchar-c c)
	 (#\( (push "(" result))
	 (#\) (push ")" result))
	 (#\" (push "\"" result))
	 (t (push (fatchar-c c) result)))
       (setf last c))
    (dotimes (n open-count)
      (push #\) result))
    (setf result (nreverse result))
;;    (format t "result = ~w~%" result)
    (listify-fake-span result)
      ))

#|

(defun fat-string-to-span (fat-string &key (start 0) last (depth 0))
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fat-string) 0)
    (return-from fat-string-to-span fat-string))
  (when (> depth 10)
    (break))
  (when (not last)
    (setf last (make-fatchar)))
  (let ((str (make-stretchy-string (- (length fat-string) start)))
	(len (length fat-string))
	(attr (fatchar-attrs (aref fat-string start)))
	added removed c (span '()))
    (loop :with i = start
       :do
       (setf c (aref fat-string i))
       (setf added (set-difference (fatchar-attrs c) (fatchar-attrs last))
	     removed (set-difference (fatchar-attrs last) (fatchar-attrs c)))
       ;; (format t "~a str=~a span=~w added=~w removed=~w~%"
       ;; 	       (fatchar-c c) str span added removed)
       (cond
	 (removed
	  (when (/= 0 (length str))
	    (push str span))
	  (setf span (nreverse span))
	  (return-from fat-string-to-span `(,@attr ,@span)))
	 (added
	  (when (/= 0 (length str))
	    (push (copy-seq str) span))
	  (let ((s (fat-string-to-span fat-string :start i :last c
				       :depth (1+ depth))))
	    (setf attr nil)
	    (push s span)
	    (incf i (span-length s)))
	  (stretchy-truncate str))
	 (t
	  (stretchy-append str (fatchar-c c))
	  (incf i)
	  (setf last c)))
       :while (< i len))
    (when (/= 0 (length str))
      (push str span))
    (setf span (nreverse span))
    `(,@attr ,@span)))
|#

(defun span-to-fat-string (span &key (start 0) end fat-string)
  "Make a fat string from a span."
  (when (not fat-string)
    (setf fat-string (make-array 40
				 :element-type 'fatchar
				 :initial-element (make-fatchar)
				 :fill-pointer 0
				 :adjustable t)))
  (setf (fill-pointer fat-string) 0)
  (let (fg bg attrs (i 0))
    (labels
	((spanky (s)
	   (when s
	     (typecase s
	       (string
		(loop :for c :across s :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c c :fg fg :bg bg :attrs attrs)
		      fat-string))
		   (incf i)))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f)))
		  (if tag
		      (progn
			(cond
			  ((equalp (subseq (string tag) 0 3) "FG-")
			   (setf fg (keywordify (subseq (string tag) 3))))
			  ((equalp (subseq (string tag) 0 3) "BG-")
			   (setf bg (keywordify (subseq (string tag) 3))))
			  (t
			   (push tag attrs)))
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(spanky (cdr s))
			(setf fg nil bg nil)
			(pop attrs))
		      (progn
			(spanky f)
			(spanky (cdr s))))))))))
      (spanky span)))
  fat-string)

;; EOF
