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
  (:use :cl :dlib :stretchy :terminal :char-util :collections
	:trivial-gray-streams)
  (:export
   #:fatchar
   #:fatchar-p
   #:make-fatchar
   #:fatchar-c #:fatchar-fg #:fatchar-bg #:fatchar-line #:fatchar-attrs
   #:fatchar-string
   #:fat-string #:fat-string-string
   #:fatchar-init
   #:copy-fat-char
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
   #:render-fat-string
   #:render-fatchar-string
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

;; This is potentially wastful, required to specialize methods.
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

(defmethod osubseq ((string fat-string) start &optional end)
  "Sub-sequence of a fat-string."
  (make-fat-string
   :string (subseq (fat-string-string string) start end)))

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
  (fatchar-string-to-span (fat-string-string fat-string) :start start))

(defun fatchar-string-to-span (fatchar-string &key (start 0))
  "Convert a FATCHAR line to tagged spans."
  (when (= (length fatchar-string) 0)
    (return-from fatchar-string-to-span fatchar-string))
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

(defun span-to-fat-string (span &key (start 0) end fatchar-string)
  (make-fat-string
   :string
   (span-to-fatchar-string span :start start :end end
			   :fatchar-string fatchar-string)))

(defun span-to-fatchar-string (span &key (start 0) end fatchar-string)
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
		(loop :for c :across s :do
		   (when (and (>= i start)
			      (or (not end) (< i end)))
		     (vector-push-extend
		      (make-fatchar :c c :fg (car fg) :bg (car bg) :attrs attrs)
		      fatchar-string))
		   (incf i)))
	       (character
		(vector-push-extend
		 (make-fatchar :c s :fg (car fg) :bg (car bg) :attrs attrs)
		 fatchar-string)
		(incf i))
	       (list
		(let* ((f (first s))
		       (tag (and (or (keywordp f) (symbolp f)) f)))
		  (if tag
		      (let ((fg fg) (bg bg) (attrs attrs))
			(declare (special fg bg attrs))
			(cond
			  ((equalp (subseq (string tag) 0 3) "FG-")
			   (push (keywordify (subseq (string tag) 3)) fg))
			  ((equalp (subseq (string tag) 0 3) "BG-")
			   (push (keywordify (subseq (string tag) 3)) bg))
			  (t
			   (push tag attrs)))
			;; (format t "tag ~s attrs ~s (cdr s) ~s~%"
			;; 	tag attrs (cdr s))
			(spanky (cdr s))
			;;(setf fg nil bg nil)
			;;(pop attrs)
			)
		      (progn
			(spanky f)
			(spanky (cdr s))))))))))
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

(defun remove-effects (string)
  "Remove any terminal colors or attributes from STRING."
  (fatchar-string-to-string (process-ansi-colors (make-fatchar-string string))))

(defun render-fat-string (fat-string &optional (terminal *terminal*))
  (render-fatchar-string (fat-string-string fat-string) terminal))

(defun render-fatchar-string (fatchar-string &optional (terminal *terminal*))
  (when (not terminal)
    (error "Please supply a terminal or set *terminal*."))
  (let ((*terminal* terminal))
    (loop :with last-attr :and fg :and bg :and set-attr
       :for c :across fatchar-string
       :do
       (setf set-attr nil)
       (when (not (equal last-attr (fatchar-attrs c)))
	 (tt-normal)
	 (loop :for a :in (fatchar-attrs c)
	    :do
	    (case a
	      (:normal    (tt-normal))
	      (:standout  (tt-standout t))
	      (:underline (tt-underline t))
	      (:bold      (tt-bold t))
	      (:inverse   (tt-inverse t))))
	 (setf last-attr (fatchar-attrs c)
	       set-attr t))
       (when (or (not (eq fg (fatchar-fg c)))
		 (not (eq bg (fatchar-bg c)))
		 set-attr)
	 (setf fg (fatchar-fg c) bg (fatchar-bg c))
	 (tt-color (or fg :default) (or bg :default)))
       (tt-write-char (fatchar-c c)))
    (tt-normal)))

(defmethod print-object ((obj fat-string) stream)
  (cond
    (*print-readably*
     (if *read-eval*
	 (format stream "#.~s"
		 `(fatchar:span-to-fat-string ,(fat-string-to-span obj)))
	 (call-next-method)))
    ((typep stream 'terminal:terminal-stream)
     ;;(format t "BLURB~s~%" (type-of obj)) (finish-output)
     (render-fat-string obj))
    (t
     ;;(print-object (fat-string-to-string obj) stream)
     ;;(format t "ZIEIE~s~%" (type-of stream)) (finish-output)
     ;; (write-string (with-terminal-output-to-string (:ansi)
     ;; 		     (render-fat-string obj)) stream)
     (write (fat-string-to-string obj) :stream stream)
     ;;(call-next-method)
     )))

(defun print-string (stream obj colon-p at-sign-p &rest args)
  "For using in a format slash directive. E.g. ~/fatchar:print-string/"
  (declare (ignore colon-p))
  #| aus der spez:
An arg, any object, is printed without escape characters (as by princ).
If arg is a string, its characters will be output verbatim.  If arg is nil
it will be printed as nil; the colon modifier (~:A) will cause an arg of
nil to be printed as (), but if arg is a composite structure, such as a
list or vector, any contained occurrences of nil will still be printed as
nil.

~mincolA inserts spaces on the right, if necessary, to make the width at
least mincol columns.  The @ modifier causes the spaces to be inserted on
the left rather than the right.

~mincol,colinc,minpad,padcharA is the full form of ~A, which allows
control of the padding.  The string is padded on the right (or on the left
if the @ modifier is used) with at least minpad copies of padchar; padding
characters are then inserted colinc characters at a time until the total
width is at least mincol.  The defaults are 0 for mincol and minpad, 1 for
colinc, and the space character for padchar.
  |#
  (let* ((mincol (and args (pop args)))
	 ;;(colinc (and args (pop args)))
	 ;;(minpad (and args (pop args)))
	 ;;(padchar (and args (pop args)))
	 (str obj)
	 render
	 len)
    (labels ((fatty () (render-fatchar-string str stream))
	     (skinny () (princ str stream)))
      (setf render #'skinny)
      (cond
	((typep stream 'terminal-stream)
	 (cond
	   ((or (typep obj 'fat-string) (typep obj 'fatchar-string))
	    (setf str (if (typep obj 'fat-string) (fat-string-string obj) obj)
		  len (display-length (fatchar-string-to-string str))
		  render #'fatty))
	   (t (setf len (display-length (princ-to-string str))))))
	(t (setf len (display-length (princ-to-string str)))))
      (if (and mincol (< len mincol))
	  (if at-sign-p
	      (progn
		(dotimes (i (- mincol len)) (tt-write-char #\space))
		(funcall render))
	      (progn
		(funcall render)
		(dotimes (i (- mincol len)) (tt-write-char #\space))))
	  (funcall render)))))

(defmethod display-length ((c fatchar))
  "Return the length of the fat character for display."
  (display-length (fatchar-c c)))

(defmethod display-length ((s fat-string))
  "Return the length of the string for display."
  (display-length (fat-string-to-string s)))

(defmethod simplify-string ((s fat-string))
  "Return the length of the string for display."
  (fat-string-to-string s))

(defmethod simplify-char ((c fatchar))
  (fatchar-c c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @@@ This actually isn't so useful, since I can't write fatchars to it?

(defclass fat-string-output-stream (fat-string
				    fundamental-character-output-stream)
  ((column
    :initarg :column :accessor fat-string-output-stream-column
    :initform 0 :type fixnum
    :documentation "Derpy column for derps."))
  (:documentation "Output to a fat-string."))

(defmethod initialize-instance
    :after ((o fat-string-output-stream) &rest initargs &key &allow-other-keys)
  "Initialize a fat-string-output-stream."
  (declare (ignore initargs))
  (setf (fat-string-string o)
	(make-stretchy-vector 40 :element-type 'fatchar)))

(defmethod stream-element-type ((stream fat-string-output-stream))
  'fatchar)

;; (defmethod close ((stream fat-string-output-stream) &key abort)
;;   )

(defmethod stream-file-position ((stream fat-string-output-stream))
  ;;&optional position-spec)
  "Used by‘file-position’. Returns or changes the current position within
‘stream’."
  (fill-pointer (fat-string-string stream)))

(defmethod (setf stream-file-position)
    ((stream fat-string-output-stream) position-spec)
  "Used by‘file-position’. Returns or changes the current position within
‘stream’."
  (setf (fill-pointer (fat-string-string stream)) position-spec))

(defmethod stream-clear-output ((stream fat-string-output-stream))
  "This is like ‘cl:clear-output’, but for Gray streams: clear the
   given output ‘stream’. The default method does nothing."
  (setf (fill-pointer (fat-string-string stream)) 0)
  stream)

;; (defmethod stream-finish-output ((stream fat-string-output-stream))
;;   "Attempts to ensure that all output sent to the Stream has reached
;;    its destination, and only then returns false.  Implements
;;    ‘finish-output’.  The default method does nothing."
;;   )

(defmethod stream-force-output ((stream fat-string-output-stream))
  "Attempts to force any buffered output to be sent. Implements
  ‘force-output’. The default method does nothing."
  )

(defmethod stream-write-sequence ((stream fat-string-output-stream)
				  seq start end &key &allow-other-keys)
  "This is like ‘cl:write-sequence’, but for Gray streams."
  (stream-write-string stream seq start end)
  seq)

;; (defmethod stream-advance-to-column ((stream fat-string-output-stream) column)
;;   "Write enough blank space so that the next character will be written
;;    at the specified column.  Returns true if the operation is
;;    successful, or ‘nil’ if it is not supported for this stream.  This
;;    is intended for use by by ‘pprint’ and ‘format’ ~T. The default
;;    method uses ‘stream-line-column’ and repeated calls to
;;    ‘stream-write-char’ with a ‘#space’ character; it returns ‘nil’ if
;;    ‘stream-line-column’ returns ‘nil’."
;;   )

;; (defmethod stream-fresh-line ((stream fat-string-output-stream))
;;   "Outputs a new line to the Stream if it is not positioned at the
;;    beginning of a line.  Returns ‘t’ if it output a new line, nil
;;    otherwise.  Used by ‘fresh-line’.  The default method uses
;;    ‘stream-start-line-p’ and ‘stream-terpri’."
;;   )

(defmethod stream-line-column ((stream fat-string-output-stream))
  "Return the column number where the next character will be written,
  or ‘nil’ if that is not meaningful for this stream.  The first
  column on a line is numbered 0.  This function is used in the
  implementation of ‘pprint’ and the ‘format’ ~T directive.  For
  every character output stream class that is defined, a method must
  be defined for this function, although it is permissible for it to
  always return ‘nil’."
  (fat-string-output-stream-column stream))

(defmethod stream-line-length ((stream fat-string-output-stream))
  "Return the stream line length or ‘nil’."
  nil)

(defmethod stream-start-line-p ((stream fat-string-output-stream))
  "Is ‘stream’ known to be positioned at the beginning of a line?  It
  is permissible for an implementation to always return ‘nil’.  This
  is used in the implementation of ‘fresh-line’.  Note that while a
  value of 0 from ‘stream-line-column’ also indicates the beginning
  of a line, there are cases where ‘stream-start-line-p’ can be
  meaningfully implemented although ‘stream-line-column’ can’t be.
  For example, for a window using variable-width characters, the
  column number isn’t very meaningful, but the beginning of the line
  does have a clear meaning.  The default method for
  ‘stream-start-line-p’ on class
 ‘fundamental-character-output-stream’ uses ‘stream-line-column’, so
  if that is defined to return ‘nil’, then a method should be
  provided for either ‘stream-start-line-p’ or ‘stream-fresh-line’."
  (zerop (fat-string-output-stream-column stream)))

(defmethod stream-terpri ((stream fat-string-output-stream))
  "Writes an end of line, as for ‘terpri’.  Returns ‘nil’.  The
   default method does (‘stream-write-char’ stream #NEWLINE).")

(defmethod stream-write-char ((stream fat-string-output-stream) character)
  "Write ‘character’ to ‘stream’ and return ‘character’.  Every
   subclass of ‘fundamental-character-output-stream’ must have a
   method defined for this function."
  (stretchy-append (fat-string-string stream) (make-fatchar :c character))
  (incf (fat-string-output-stream-column stream))
  (when (char= character #\newline)
    (setf (fat-string-output-stream-column stream) 0)))

(defmethod stream-write-string ((stream fat-string-output-stream) string
				&optional start end)
  "This is used by ‘write-string’.  It writes the string to the
  stream, optionally delimited by start and end, which default to 0
  and ‘nil’.  The string argument is returned.  The default method
  provided by ‘fundamental-character-output-stream’ uses repeated
  calls to ‘stream-write-char’."
  (loop
     :for i = (fill-pointer (fat-string-string stream)) :then (1+ i)
     :for c :across (subseq string start end)
     :do
     (stretchy-set (fat-string-string stream) i (make-fatchar :c c))
     (incf (fat-string-output-stream-column stream))
     (when (char= c #\newline)
       (setf (fat-string-output-stream-column stream) 0)))
  string)

;; EOF
