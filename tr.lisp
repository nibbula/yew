;;
;; tr.lisp - Translate characters.
;;

(defpackage :tr
  (:documentation "Translate characters.")
  (:use :cl :dlib :glob :lish)
  (:export
   #:!tr
   ))
(in-package :tr)

;; @@@ We're probably too intimate with glob. Or glob should export this stuff.

(defun compile-char-set (s)
  "Convert char set description into a char-set."
  (let* ((set (glob::make-char-set))
	 (cc-str
	  (with-output-to-string (str)
	    (loop :for e :in s :do
	       (cond
		 ;; ((eql e :not)
		 ;;  (setf (char-set-inverted set) t))
		 ((consp e)
		  (cond
		    ((eql (car e) :equiv)
		     (let ((cc (glob::find-char-class (cadr e))))
		       (if cc
			   (push cc (glob::char-set-classes set))
			   (princ (cadr e) str)))) ; XXX in a class by itself
		    ((eql (car e) :class)
		     (push (cadr e) (glob::char-set-classes set)))
		    ((characterp (car e))
		     (push (vector (char-code (car e))
				   (char-code (cadr e)))
			   (glob::char-set-ranges set)))
		    (t
		     (error "Unkown type in char set"))))
		 ((characterp (princ e str))))))))
    (setf (glob::char-set-string set) cc-str)
    set))

;; Sadly, tr charset are different enough from glob charsets that I'm not sure
;; it's worth it to have them use the same code.

(defun get-char-set (pattern &optional (compile t))
  "Return a symbolic character set parsed from the string PATTERN, starting at
position START-POS. A symbolic character set is a list consisting of:
 - Characters to match.
 - Character ranges, as a sublist with two character values.
 - Named classes, as a keyword, e.g :digit
Second value is where the character set ended."
  (let* ((result '()) (start-pos 0) (i start-pos))
    (loop :with c
       :while (< i (length pattern))
       :do
       (setf c (char pattern i))
       (case c
	 ;; begin
	 (#\-
	  ;; If the dash is at the start or end, it's just a dash.
	  (if (or (= i start-pos) (char= #\] (char pattern (1+ i))))
	      (pushnew #\- result)
	      (progn ;; Otherwise make a real range
		(pushnew (list (pop result) (char pattern (1+ i))) result)
		(incf i))))
	 (#\[ ;; class or symbol
	  (incf i)
	  (let ((my-range-start i))
	    (setf c (char pattern i))
	    (case c
	      (#\=
	       (incf i)
	       (pushnew `(:equiv ,(char pattern i)) result)
	       (incf i)
	       (if (not (string= (subseq pattern i (+ i 2)) "=]"))
		   (error "missing =]"))
	       (incf i))
	      (#\:
	       (incf i)
	       (let* ((end (or (position #\: (subseq pattern i))
			       (error "Missing ending : in character ~
                                       class name.")))
		      (name (subseq pattern i (+ i end)))
		      (class (glob::find-named-char-class name)))
		 (when (not class)
		   (error "Unknown character class ~s" name))
		 (pushnew `(:class ,class) result)
		 (incf i end))
	       (incf i)) ; the ending : ?
	      (#\]
	       (if (= i my-range-start)
		   (error "Invalid empty character class")
		   (pushnew #\] result)))
	      (t
	       ;; A repeated char
	       (if (and (< i (1- (length pattern)))
			(char= (char pattern (1+ i)) #\*))
		   (progn
		     (incf i)
		     (let (n end-pos)
		       (setf (values n end-pos)
			     (parse-integer pattern :start i :junk-allowed t))
		       (if (and n (char= (char pattern end-pos) #\]))
			   (progn
			     (pushnew `(:repeat ,(char pattern i) ,n) result)
			     (setf i (1+ end-pos)))
			   (error "Invalid repeated char at ~d" i))))
		   (error "Invalid character class type indicator '~a' at ~d"
			  c i))))))
	 (#\]
	  ;; If we're the first, or second, it as a normal character.
	  (if (or (= i start-pos)
		  (and (= i (1+ start-pos))
		       (or (find (char pattern (1- i))
				 '(#\! #\^) :test #'char=))))
	      (pushnew c result)
	      (progn
		;; (setf got-close-bracket t)
		(loop-finish))))
	 (t ;; regular char
	  (pushnew c result)))
       (incf i))
    (values (if compile (compile-char-set (nreverse result))
		(nreverse result)) i)))

(defun match-char-set (set c)
  "Return true if C is in char-set SET."
  (labels
      ((do-string ()
	 (when (glob::char-set-string set)
	   (find c (glob::char-set-string set))))
       (do-ranges ()
	 (when (glob::char-set-ranges set)
	   (loop :for r :in (glob::char-set-ranges set) :do
	      (when (and (>= (char-code c) (elt r 0))
			 (<= (char-code c) (elt r 1)))
		(return-from do-ranges t)))))
       (do-classes ()
	 (when (glob::char-set-classes set)
	   (loop :for r :in (glob::char-set-classes set) :do
	      (when (funcall (glob::char-class-function r) c)
		(return-from do-classes t))))))
    (if (glob::char-set-inverted set)
	(not (or (do-string) (do-ranges) (do-classes)))
	(or (do-string) (do-ranges) (do-classes)))))

(defstruct tset
  set			; head of the set
  i			; index in the set we're at
  char			; character we're at
  len			; length of the set
  range-i		; char code in a range we're at
  range-end		; char code of the end of the range
  rep			; repitition we're at
  rep-c)		; repitition character

(defcommand tr
  ((set1 string :help "Set of characters to translate from.")
   (set2 string :help "Set of characters to translate to.")
   (files input-stream-or-filename :help "Input file or stream.")
   (help boolean :long-arg "help" :help "Show the help."))
  "Translate characters."
  (when help
    (lish::print-command-help (lish:get-command "tr"))
    (return-from !tr (values)))
  (when (not set1)
    (error "Missing set1 argument."))
  (let* ((table (make-hash-table :test #'equal :size (length set1)))
	 (cset1 (get-char-set set1 nil))
	 (cset2 (get-char-set set2 nil))
	 (e1 (make-tset :set cset1 :len (length cset1) :i 0))
	 (e2 (make-tset :set cset2 :len (length cset2) :i 0)))

    (labels ((new-ele (e)
	       (cond
		 ((characterp (car (tset-set e)))
		  ;; single char - okay
		  )
		 ((and (not (tset-range-i e))
		       (listp (car (tset-set e)))
		       (characterp (caar (tset-set e)))
		       (characterp (cadar (tset-set e))))
		  ;; new range
		  (setf (tset-range-i e) (char-code (caar (tset-set e)))
			(tset-range-end e) (char-code (cadar (tset-set e)))
			(tset-char e) (caar (tset-set e))))
		 ((and (not (tset-rep e))
		       (listp (car (tset-set e)))
		       (eq :rep (first (car (tset-set e)))))
		  ;; new repitition
		  (setf (tset-rep e) (second (car (tset-set e)))
			(tset-rep-c e) (third (car (tset-set e)))
			(tset-char e)  (third (car (tset-set e)))))
		 ((null (tset-set e))
		  #| Done |#)
		 ;; (t
		 ;;  (cerror "Whatevs." "Bad new condition. ~s~%" (tset-set e)))
		 ))
	     (next (e)
	       (cond
		 ;; in a range
		 ((tset-range-i e)
		  (if (< (tset-range-i e) (tset-range-end e))
		      (progn
			(incf (tset-range-i e))
			(setf (tset-char e) (code-char (tset-range-i e))))
		      (progn
			(setf (tset-range-i e) nil ; we're done with the range
			      (tset-set e) (cdr (tset-set e)))
			(incf (tset-i e)))))
		 ;; in a repitition
		 ((tset-rep e)
		  (decf (tset-rep e))
		  (when (zerop (tset-rep e)) ; done with the repeated char
		    (setf (tset-rep e) nil
			  (tset-set e) (cdr (tset-set e)))
		    (incf (tset-i e))))
		 ;; A normal char
		 ((< (tset-i e) (tset-len e))
		  (incf (tset-i e))
		  (setf (tset-char e) (car (tset-set e))
			(tset-set e) (cdr (tset-set e))))
		 (t
		  (cerror "Mmmmkay." "Bad next condition.~%")))
	       (new-ele e)))

      ;; (format t "Starting~%e1 ~s~%e2 ~s~%" e1 e2)
      ;; Store translations in a hash table.
      (new-ele e1)
      (new-ele e2)
      ;; (format t "NEW~%e1 ~s~%e2 ~s~%" e1 e2)
      (loop
	 :while (and (tset-set e1) (tset-set e2))
	 :do
	   (setf (gethash (tset-char e1) table) (tset-char e2))
	   (next e1)
	   (next e2)
	   ;; (format t "e1 ~s~%e2 ~s~%" e1 e2)
	   ))

    ;; (format t "hast-table-count ~s hash-table-size ~s~%"
    ;; 	    (hash-table-count table) (hash-table-size table))

    (loop :for f :in (or (and files (if (not (listp files)) (list files) files))
			 (list *standard-input*))
       :do
	 (with-open-file-or-stream (input-stream f)
	   ;; Translate the input.
	   (loop :with c
	      :while (setf c (read-char input-stream nil))
	      :do
		(write-char (or (gethash c table) c)))))))

;; EOF
