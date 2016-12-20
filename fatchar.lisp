;;
;; fatchar.lisp - Characters with attributes.
;;

(defpackage :fatchar
  (:documentation "Characters with attributes.
Defines a FATCHAR which is a character with color and font attributes.
Define a FAT-STRING as a vector of FATCHARS.
Define a TEXT-SPAN as a list representation of a FAT-STRING.
")
  (:use :cl :dlib :stretchy :terminal)
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
   #:process-ansi-colors
   #:render-fat-string
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
  "A string of FATCHARs."
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

#|
(defun map-span-strings (span &key (start 0) end fat-string)
  "Make a fat string from a span."
  (labels ((spanky (s)
	     (when s
	       (typecase s
		 (string
		  )
		 (list
		  (let* ((f (first s))
			 (tag (and (or (keywordp f) (symbolp f)) f)))
		    (if tag
			(progn
			  (spanky (cdr s)))
			(progn
			  (spanky f)
			  (spanky (cdr s)))))))))))
      (spanky span)))
  fat-string)
|#

;;; ^[[00m	normal
;;; ^[[01;34m	bold, blue fg
;;; ^[[m	normal
;;; ^[[32m	green fg
;;; ^[[1m	bold

;; We drink of the color and become the color.
(defun grok-ansi-color (str)
  "Take an string with an ANSI terminal color escape sequence, starting after
the ^[[ and return NIL if there was no valid sequence, or an integer offset
to after the sequence, the foreground, background and a list of attributes.
NIL stands for whatever the default is, and :UNSET means that they were not
set in this string."
  (let (num inc attr-was-set
	(i 0) (len (length str))
	(hi-color nil) hi-color-type r g b
	(fg :unset) (bg :unset) (attr '()))
    (loop
       :do
       #| (princ "> ") (dumpy num i inc (subseq str i)) |#
       (setf (values num inc) (parse-integer str :start i :junk-allowed t))
       #| (princ "< ") (dumpy num i inc (subseq str i)) |#
       (if (or (not num) (not inc))
	   (progn
	     ;; Just an #\m without arguments means no attrs and unset color
	     (when (eql (char str i) #\m)
	       (setf attr '() fg nil bg nil attr-was-set t i 1))
	     (return))
	   (progn
	     (setf i inc)
	     (when (and (< i len)
			(or (eql (char str i) #\;)
			    (eql (char str i) #\m)))
	       (incf i)
	       (cond
		 ((and hi-color (not hi-color-type))
		  (case num
		    (2 (setf hi-color-type :3-color))
		    (5 (setf hi-color-type :1-color))))
		 ((eq hi-color-type :1-color)
		  (if (eq hi-color :fg)
		      (setf fg num))
		      (setf bg num))
		 ((eq hi-color-type :3-color)
		  (cond
		    ((not r) (setf r num))
		    ((not g) (setf g num))
		    ((not b) (setf b num)
		     (if (eq hi-color :fg)
			 (setf fg (list r g b))
			 (setf bg (list r g b))))))
		 (t
		  (case num
		    (0  (setf attr '() fg nil bg nil attr-was-set t))
		    (1  (pushnew :bold attr)      (setf attr-was-set t))
		    (2  (pushnew :dim attr)       (setf attr-was-set t))
		    (3  (pushnew :italic attr)    (setf attr-was-set t))
		    (4  (pushnew :underline attr) (setf attr-was-set t))
		    (5  (pushnew :blink attr)     (setf attr-was-set t))
		    (7  (pushnew :inverse attr)   (setf attr-was-set t))
		    (8  (pushnew :invisible attr) (setf attr-was-set t))
		    (9  (pushnew :crossed-out attr) (setf attr-was-set t))
		    (21 (pushnew :double-underline attr) (setf attr-was-set t))
		    (22 (setf attr (delete :bold attr))
			(setf attr-was-set t))
		    (23 (setf attr (delete :italic attr))
			(setf attr-was-set t))
		    (24 (setf attr (delete :underline attr))
			(setf attr-was-set t))
		    (25 (setf attr (delete :blink attr))
			(setf attr-was-set t))
		    (27 (setf attr (delete :inverse attr))
			(setf attr-was-set t))
		    (28 (setf attr (delete :invisible attr))
			(setf attr-was-set t))
		    (29 (setf attr (delete :crossed-out attr))
			(setf attr-was-set t))
		    (30 (setf fg :black))
		    (31 (setf fg :red))
		    (32 (setf fg :green))
		    (33 (setf fg :yellow))
		    (34 (setf fg :blue))
		    (35 (setf fg :magenta))
		    (36 (setf fg :cyan))
		    (37 (setf fg :white))
		    (38 (setf hi-color :fg))
		    (39 (setf fg nil))
		    (40 (setf bg :black))
		    (41 (setf bg :red))
		    (42 (setf bg :green))
		    (43 (setf bg :yellow))
		    (44 (setf bg :blue))
		    (45 (setf bg :magenta))
		    (46 (setf bg :cyan))
		    (47 (setf bg :white))
		    (48 (setf hi-color :bg))
		    (49 (setf bg nil))
		    (otherwise #| just ignore unknown colors or attrs |#))))
	       (when (eql (char str (1- i)) #\m)
		 (return)))))
       :while (< i len))
    (values
     (if (and (eq fg :unset) (eq bg :unset) (not attr-was-set))
	 1 i)
     fg bg (if (not attr-was-set) :unset attr))))

(defun process-ansi-colors (fat-line)
  "Convert ANSI color escapes into colored fatchars."
  (when (zerop (length fat-line))
    (return-from process-ansi-colors fat-line))
  (let ((new-fat-line (make-stretchy-vector (length fat-line)
					    :element-type 'fatchar))
	(i 0)
	(len (length fat-line))
	(line (map 'string #'(lambda (x) (fatchar-c x)) fat-line))
	fg bg attrs)
    (flet ((looking-at-attrs ()
	     "Return true if might be looking at some attrs."
	     (and (< i (1- len))
		  (char= (aref line i) #\escape)
		  (char= (aref line (1+ i)) #\[)))
	   (get-attrs ()
	     "Get the attrs we might be looking at."
	     (incf i 2) ; the ^[ and [
	     (multiple-value-bind (inc i-fg i-bg i-attrs)
		 (grok-ansi-color (subseq line i))
	       (when inc
		 (unless (eq i-fg    :unset) (setf fg i-fg))
		 (unless (eq i-bg    :unset) (setf bg i-bg))
		 (unless (eq i-attrs :unset) (setf attrs i-attrs))
		 (incf i inc))))	; for the parameters read
	   (copy-char ()
	     "Copy the current character to result."
	     ;;(dbug "attrs = ~a~%" attrs)
	     ;;(dbug "(aref fat-line i) = ~a~%" (aref fat-line i))
	     (let ((new-attrs (union attrs (fatchar-attrs (aref fat-line i)))))
	       (stretchy:stretchy-append
		new-fat-line (make-fatchar
			      :c (fatchar-c (aref fat-line i))
			      :fg fg :bg bg
			      :attrs new-attrs
			      )))
	     (incf i)))
      (loop :while (< i len) :do
	 (if (looking-at-attrs)
	     (get-attrs)
	     (copy-char))))
    new-fat-line))

(defun render-fat-string (fat-string &optional (terminal *terminal*))
  (when (not *terminal*)
    (error "Please supply a terminal or set *terminal*."))
  (loop :with last-attr :and fg :and bg
     :for c :across fat-string
     :do
     (when (not (eq last-attr (fatchar-attrs c)))
       (tt-normal terminal)
       (loop :for a :in (fatchar-attrs c)
	  :do
	  (case a
	    (:normal    (tt-normal terminal))
	    (:standout  (tt-standout terminal t))
	    (:underline (tt-underline terminal t))
	    (:bold      (tt-bold terminal t))
	    (:inverse   (tt-inverse terminal t)))))
     (when (or (not (eq fg (fatchar-fg c)))
	       (not (eq bg (fatchar-bg c))))
       (setf fg (fatchar-fg c) bg (fatchar-bg c))
       (tt-color terminal fg bg))
     (tt-write-char terminal (fatchar-c c)))
  (tt-normal terminal))

;; EOF
