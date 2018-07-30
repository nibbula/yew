;;
;; fatchar.lisp - Characters with attributes.
;;

;; ToDo:
;;  - handle RGB direct color

(defpackage :fatchar
  (:documentation "Characters with attributes.
Defines a FATCHAR which is a character with color and font attributes.
Define a FATCHAR-STRING as a vector of FATCHARS.
Define a FAT-STRING as a struct with a FATCHAR-STRING so we can specialize.
Define a TEXT-SPAN as a list representation of a FAT-STRING.
")
  (:use :cl :dlib :stretchy :char-util :collections :color)
  (:export
   #:fatchar
   #:fatchar-p
   #:make-fatchar
   #:fatchar-c #:fatchar-fg #:fatchar-bg #:fatchar-line #:fatchar-attrs
   #:fatchar-string
   #:fat-string #:fat-string-string
   #:fatchar-init
   #:copy-fatchar
   #:same-effects
   #:fatchar=
   #:make-fat-string
   #:make-fatchar-string
   #:fatchar-string-to-string
   #:fat-string-to-string
   #:fat-string< #:fat-string> #:fat-string= #:fat-string/=
   #:fat-string<= #:fat-string>= #:fat-string-lessp #:fat-string-greaterp
   #:fat-string-equal #:fat-string-not-equal
   #:fat-string-not-lessp #:fat-string-not-greaterp
   #:span-length
   #:fat-string-to-span
   #:fatchar-string-to-span
   #:span-to-fat-string
   #:span-to-fatchar-string
   #:process-ansi-colors
   #:remove-effects
   ))
(in-package :fatchar)

(defstruct fatchar
  "A character with attributes."
  (c (code-char 0) :type character)
  (fg nil)
  (bg nil)
  (line 0 :type fixnum)
  (attrs nil :type list))

(defun fatchar-init (c)
  "Initialize a fatchar with the default vaules."
  (setf (fatchar-c     c)	(code-char 0)
	(fatchar-fg    c)	:white
	(fatchar-bg    c)	:black
	(fatchar-line  c)	0
	(fatchar-attrs c)	nil))

;; I think we can just use the one made by defstruct?
;; (defun copy-fatchar (c)
;;   (declare (type fatchar c))
;;   (when c
;;     (make-fatchar
;;      :c	    (fatchar-c c)
;;      :fg    (fatchar-fg c)
;;      :bg    (fatchar-bg c)
;;      :line  (fatchar-line c)
;;      :attrs (fatchar-attrs c))))

(defun same-effects (a b)
  "Return true if the two fatchars have the same colors and attributes."
  (and (equal (fatchar-fg a) (fatchar-fg b))
       (equal (fatchar-bg a) (fatchar-bg b))
       (not (set-exclusive-or (fatchar-attrs a) (fatchar-attrs b)
			      :test #'eq))))

(defun fatchar= (a b)
  "True if everything about a fatchar is the equivalent."
  (and (char= (fatchar-c a) (fatchar-c b))
       (same-effects a b)
       (= (fatchar-line a) (fatchar-line b))))

(deftype fatchar-string (&optional size)
  "A string of FATCHARs."
  `(vector fatchar ,size))

;; This is potentially wasteful, but required to specialize methods.
(defclass fat-string ()
  ((string				; blurg :|
    :initarg :string :accessor fat-string-string
    :documentation "A lot of crust around a string."))
  (:documentation "A vector of FATCHAR."))
;;(string (vector) :type fatchar-string)) ; Is this better or worse?

(defmethod olength ((s fat-string))
  (length (fat-string-string s)))

(defun make-fat-string (&key string)
  (make-instance 'fat-string :string string))

(defmethod oelt ((s fat-string) key)
  (aref (fat-string-string s) key))

(defmethod (setf oelt) (value (s fat-string) key) ;; @@@ ??? test
  (setf (aref (fat-string-string s) key) value))

(defmethod osubseq ((string fat-string) start &optional end)
  "Sub-sequence of a fat-string."
  (make-fat-string
   :string
   (if end
       (subseq (fat-string-string string) start end)
       (subseq (fat-string-string string) start))))

(defmethod oposition ((item fatchar) (string fat-string)
		      &key from-end test test-not key
			(start nil start-p)
			(end nil end-p))
  "Position of a fatchar in a fat-string."
  (apply 'position
	 `(,item ,(fat-string-string string)
	   :from-end ,from-end
	    ;; Default to reasonable tests.
	   :test (or ,test #'equalp)
	   :key ,key
	   :test-not (or ,test-not (lambda (x y) (not (equalp x y))))
	   ,@(when start-p `(:start ,start))
	   ,@(when end-p `(:end ,end)))))

(defmethod oposition ((item character) (string fat-string)
		      &key from-end test test-not key
			(start nil start-p)
			(end nil end-p))
  "Position of a fatchar in a fat-string."
  (apply 'position
	 `(,item ,(fat-string-string string)
	   :from-end ,from-end
	   :test ,test :test-not ,test-not
	   ;; Make the key reach into the fatchar for the character.
	   :key ,(or (and key (_ (funcall key (fatchar-c _))))
		     #'fatchar-c)
	   ,@(when start-p `(:start ,start))
	   ,@(when end-p `(:end ,end)))))

(defun make-fatchar-string (thing)
  "Make a fat string from something. THING can be a string or a character."
  (let (result)
    (flet ((from-string (string)
	     (setf result (make-array (list (length string))
				      :element-type 'fatchar
				      :initial-element (make-fatchar)))
	     (loop :for i :from 0 :below (length string) :do
		(setf (aref result i) (make-fatchar :c (char string i))))))
      (etypecase thing
	(string
	 (from-string thing))
	(character
	 (setf result
	       (make-array '(1) :element-type 'fatchar
			   :initial-element (make-fatchar :c thing))))
	;; We could princ-to-string other stuff, but it's probably better if
	;; the caller does it explicitly.
	)
      result)))

(defun fat-string-to-string (fat-string)
  "Make a string from a fat string. This of course loses the attributes."
  (typecase fat-string
    (fat-string (fatchar-string-to-string (fat-string-string fat-string)))
    (fatchar-string (fatchar-string-to-string fat-string))
    (t fat-string)))

(defun fatchar-string-to-string (string)
  "Make a string from a fat string. This of course loses the attributes."
  ;; Arrays can't really distinguish their orginal element type if it's
  ;; upgraded, so we might not be able tell a string from a fatchar-string.
  (typecase string
    (fatchar-string
     ;; (let ((s (make-array (list (length fat-string))
     ;; 			  :element-type 'character)))
     ;;   (loop :for i :from 0 :below (length fat-string) :do
     ;; 	  (setf (aref s i) (fatchar-c (aref fat-string i))))
     ;;   s))
     (map 'string (_ (if (fatchar-p _) (fatchar-c _) _)) string))
    (t string)))

;; @@@ What about equality considering the effects?

(defun fat-string-compare (f a b)
  (funcall f (fat-string-to-string a) (fat-string-to-string b)))

(eval-when (:compile-toplevel)
  (defmacro make-comparators ()
    (let ((forms
	   (loop :with func
	      :for f :in '(string< string> string= string/= string<= string>=
			   string-lessp string-greaterp string-equal
			   string-not-equal string-not-lessp
			   string-not-greaterp)
	      :do
	      (setf func (symbolify (s+ "FAT-" f)))
	      :collect `(defun ,func (a b)
			  (funcall #',f
				   (fat-string-to-string a)
				   (fat-string-to-string b))))))
      `(progn ,@forms))))
(make-comparators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spans
;;
;; ([objects ...])
;; (:keyword [objects ...])
;; (:keyword [attribute ...] [objects ...])
;;
;; attribute => :keyword object
;;
;; (:fg :color #(:rgb 0 0 1) "hello")
;; (:fg-blue "hello")
;;
;; Some things may want to allow evaluation by letting an "object" be
;; a function call or a symbol that gets evaluated, instead of just literal
;; objects.

(defun span-length (span)
  "Calculate the length in characters of the span."
  (the fixnum (loop :for e :in span
		 :sum (typecase e
			(string (length e))
			(cons (span-length e))
			;; @@@ shouldn't we princ-to-string other some things?
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
  (fatchar-string-to-span (fat-string-string fat-string) :start start))

(defun fatchar-string-to-span (fatchar-string &key (start 0))
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fatchar-string) 0)
    ;; (return-from fatchar-string-to-span fatchar-string))
    (return-from fatchar-string-to-span nil))
  (let ((last (make-fatchar))
	(result '())
	(open-count 0)
	added removed)
;;    (push #\( result)
    (loop :with c
       :for i :from start :below (length fatchar-string) :do
       (setf c (aref fatchar-string i))
       ;; Foreground
       (when (not (eql (fatchar-fg c) (fatchar-fg last)))
	 (when (fatchar-fg last)
	   (push #\) result)
	   (decf open-count))
	 (when (fatchar-fg c)
	   (push #\( result)
	   (incf open-count)
	   (if (keywordp (fatchar-fg c))
	       (push (keywordify (s+ "FG-" (fatchar-fg c))) result)
	       (progn
		 (push :fg result)
		 (push :color result)
		 (push (fatchar-fg c) result)))))
       ;; Background
       (when (not (eql (fatchar-bg c) (fatchar-bg last)))
	 (when (fatchar-bg last)
	   (push #\) result)
	   (decf open-count))
	 (when (fatchar-bg c)
	   (push #\( result)
	   (incf open-count)
	   (if (keywordp (fatchar-fg c))
	       (push (keywordify (s+ "BG-" (fatchar-bg c))) result)
       	       (progn
		 (push :bg result)
		 (push :color result)
		 (push (fatchar-bg c) result)))))
       ;; Attributes
       (setf added (set-difference (fatchar-attrs c) (fatchar-attrs last))
	     removed (set-difference (fatchar-attrs last) (fatchar-attrs c)))
       (loop :for nil :in removed :do
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

(defun span-to-fat-string (span &key (start 0) end fatchar-string
				  unknown-func filter)
  (make-fat-string
   :string
   (span-to-fatchar-string span :start start :end end
			   :fatchar-string fatchar-string
			   :unknown-func unknown-func
			   :filter filter)))

(defparameter *known-attrs*
  `(:normal :standout :underline :bold :inverse)
  "List of known attributes.")

;; @@@ Consider dealing with the overlap between this and
;; lish:symbolic-prompt-to-string and terminal:with-style.

(defun span-to-fatchar-string (span &key (start 0) end fatchar-string
				      unknown-func filter)
  "Make a fat string from a span."
  (when (not fatchar-string)
    (setf fatchar-string (make-array 40
				     :element-type 'fatchar
				     :initial-element (make-fatchar)
				     :fill-pointer 0
				     :adjustable t)))
  (setf (fill-pointer fatchar-string) 0)
  (let (fg bg attrs (i 0))
    (declare (special fg bg attrs))
    (labels
	((spanky (s)
	   (when s
	     (typecase s
	       (string
		(loop :for c :across (if filter (funcall filter s) s)
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c c :fg (car fg) :bg (car bg) :attrs attrs)
		      fatchar-string))
		   (incf i)))
	       (fat-string
		(loop :for c :across (fat-string-string
				      (if filter (funcall filter s) s))
		   :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c (fatchar-c c)
				    :fg (fatchar-fg c)
				    :bg (fatchar-bg c)
				    :line (fatchar-line c)
				    :attrs (union attrs (fatchar-attrs c))); <--
		      fatchar-string))
		   (incf i)))
	       (character
		(vector-push-extend
		 (make-fatchar :c s :fg (car fg) :bg (car bg) :attrs attrs)
		 fatchar-string)
		(incf i))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f))
		       (rest (cdr s)))
		  (if tag
		      (let ((fg fg) (bg bg) (attrs attrs)
			    (tag-str (string tag)))
			(declare (special fg bg attrs))
			(cond
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "FG-"))
			   (push (keywordify (subseq (string tag) 3)) fg))
			  ((and (> (length tag-str) 3)
				(string= (subseq tag-str 0 3) "BG-"))
			   (push (keywordify (subseq (string tag) 3)) bg))
			  ((member tag *simple-colors*)
			   ;; An un-prefixed color is a foreground color.
			   (push tag fg))
			  ((member tag *known-attrs*)
			   (push tag attrs))
			  ((and (eq tag :fg) (eq (second s) :color))
			   (push (third s) fg)
			   (setf rest (cdddr s)))
			  ((and (eq tag :bg) (eq (second s) :color))
			   (push (third s) bg)
			   (setf rest (cdddr s)))
			  (t
			   (if unknown-func
			       (spanky (funcall unknown-func s))
			       (push tag attrs))))
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(spanky rest)
			;;(setf fg nil bg nil)
			;;(pop attrs)
			)
		      (progn
			(spanky f)
			(spanky (cdr s))))))
	       (t
		(when unknown-func
		  (spanky (funcall unknown-func s))))))))
      (spanky span)))
  fatchar-string)

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

(defparameter *xterm-256-color-table* nil
  "Table for old-timey xterm colors.")

;; see xterm/256colres.pl
(defun make-xterm-color-table ()
  (setf *xterm-256-color-table* (make-array 256))
  ;; colors 16-231 are a 6x6x6 color cube
  (loop :for red :from 0 :below 6 :do
     (loop :for green :from 0 :below 6 :do
	(loop :for blue :from 0 :below 6 :do
	   (setf (aref *xterm-256-color-table*
		       (+ 16 (* red 36) (* green 6) blue))
		 (make-color :rgb8
			     :red   (if (= red   0) 0 (+ (* red   40) 55))
			     :green (if (= green 0) 0 (+ (* green 40) 55))
			     :blue  (if (= blue  0) 0 (+ (* blue  40) 55)))))))
  ;; colors 232-255 are a grayscale ramp, without black & white
  (loop :with level
     :for gray :from 0 :below 24 :do
     (setf level (+ (* gray 10) 8)
	   (aref *xterm-256-color-table* (+ 232 gray))
	   (make-color :rgb8 :red level :green level :blue level))))

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
  (let* ((i 0)
	 (len (length str))
	 (hi-color nil)
	 (fg :unset)
	 (bg :unset)
	 (attr '())
	 num inc attr-was-set hi-color-type r g b)
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
		  (when (not *xterm-256-color-table*)
		    (make-xterm-color-table))
		  (if (eq hi-color :fg)
		      (setf fg (aref *xterm-256-color-table* num))
		      (setf bg (aref *xterm-256-color-table* num))))
		 ((eq hi-color-type :3-color)
		  (cond
		    ((not r) (setf r num))
		    ((not g) (setf g num))
		    ((not b) (setf b num)
		     (if (eq hi-color :fg)
			 (setf fg (make-color :rgb8 :red r :green g :blue b))
			 (setf bg (make-color :rgb8 :red r :green g :blue b))))))
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

(defun remove-effects (string)
  "Remove any terminal colors or attributes from STRING."
  (fatchar-string-to-string (process-ansi-colors (make-fatchar-string string))))

;; Methods from char-util:

(defmethod display-length ((c fatchar))
  "Return the length of the fat character for display."
  (cond
    ((not (zerop (fatchar-line c)))
     1)				    ; assume line drawing can happen in 1 cell
    ;; ((char= #\nul (fatchar-c c))
    ;;  0)		; since an unset fatchar is #\nul
    (t (display-length (fatchar-c c)))))

(defmethod display-length ((s fat-string))
  "Return the length of the string for display."
  (display-length (fat-string-to-string s)))

(defmethod simplify-string ((s fat-string))
  "Return the fat-string as a string."
  (fat-string-to-string s))

(defmethod simplify-char ((c fatchar))
  "Return the FATCHAR as a character."
  (fatchar-c c))

;; EOF
