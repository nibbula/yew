;;
;; fatchar-io.lisp - Outputting fat characters and fat strings as streams.
;;

(defpackage :fatchar-io
  (:documentation "Outputing fat characters and fat strings as streams.")
  (:use :cl :dlib :stretchy :char-util :fatchar :terminal :trivial-gray-streams)
  (:export
   #:render-fat-string
   #:render-fatchar
   #:render-fatchar-string
   #:print-string
   #:fat-string-output-stream
   #:write-fatchar
   #:write-fat-string
   #:with-output-to-fat-string
   #:fs+
   ))
(in-package :fatchar-io)

;; Anything that depends on terminals should go in here.

(defun render-fat-string (fat-string &key (terminal *terminal*) (start 0) end)
  (render-fatchar-string (fat-string-string fat-string)
			 :terminal terminal :start start :end end))

;; @@@ why doesn't this work right???
(defun render-fatchar-string (fatchar-string
			      &key (terminal *terminal*) (start 0) end)
  "Render FATCHAR-STRING on TERMINAL."
  (when (not terminal)
    (error "Please supply a terminal or set *terminal*."))
  (let ((*terminal* terminal))
    (loop
       :with last-attr :and fg :and bg :and set-attr
       :and c
       :and i = (or start 0)
       :and our-end = (or end (length fatchar-string))
       :while (< i our-end)
       :do
       (setf c (aref fatchar-string i)
	     set-attr nil)
       ;;(format t "----> ~s [~d]~%" c i)
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
       (when (or (not (equal fg (fatchar-fg c)))
		 (not (equal bg (fatchar-bg c)))
		 set-attr)
	 (setf fg (fatchar-fg c) bg (fatchar-bg c))
	 (tt-color (or fg :default) (or bg :default)))
       (tt-write-char (fatchar-c c))
       (incf i))
    (tt-normal)))

(defun render-fatchar (c &optional (terminal *terminal*))
  "Render one FATCHAR on TERMINAL."
  (when (not terminal)
    (error "Please supply a terminal or set *terminal*."))
  (let ((*terminal* terminal))
    (tt-normal)
    (loop :for a :in (fatchar-attrs c)
       :do
       (case a
	 (:normal    (tt-normal))
	 (:standout  (tt-standout t))
	 (:underline (tt-underline t))
	 (:bold      (tt-bold t))
	 (:inverse   (tt-inverse t))))
    (tt-color (or (fatchar-fg c) :default) (or (fatchar-bg c) :default))
    (tt-write-char (fatchar-c c))))

#|
(defmethod print-object ((obj fat-string) stream)
  (cond
    (*print-readably*
     (if *read-eval*
	 (format stream "#.~s"
		 `(fatchar:span-to-fat-string ,(fat-string-to-span obj)))
	 (call-next-method)))
    ((typep stream '(or terminal:terminal terminal:terminal-stream))
     ;;(format t "BLURB~s~%" (type-of obj)) (finish-output)
     (render-fat-string obj))
    (t
     ;;(print-object (fat-string-to-string obj) stream)
     ;;(format t "ZIEIE~s~%" (type-of stream)) (finish-output)
     ;; (write-string
     ;;  (with-terminal-output-to-string (:ansi)
     ;; 	(render-fat-string obj)) stream)
     (write (fat-string-to-string obj) :stream stream)
     ;;(call-next-method)
     )))
|#

(defun print-string (stream obj colon-p at-sign-p &rest args)
  "For using in a format slash directive. E.g. ~/fatchar-io:print-string/"
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
    (labels ((fatty () (render-fatchar-string str :terminal stream))
	     (skinny () (princ str stream)))
      (setf render #'skinny)
      (cond
	((typep stream '(or terminal terminal-stream))
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
		;;(dotimes (i (- mincol len)) (tt-write-char #\space))
		(dotimes (i (- mincol len)) (write-char #\space stream))
		;;(dotimes (i (- mincol len)) (write-fatchar #\space stream))
		(funcall render))
	      (progn
		(funcall render)
		(dotimes (i (- mincol len)) (write-char #\space stream))))
		;;(dotimes (i (- mincol len)) (write-fatchar #\space stream))))
	  (funcall render)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod print-object ((obj fat-string-output-stream) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    ))

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

#+sbcl (defmethod sb-gray:stream-line-length ((stream fat-string-output-stream))
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
   default method does (‘stream-write-char’ stream #NEWLINE)."
  (write-fatchar #\newline stream))

(defun write-fatchar (c &optional (stream *standard-output*))
  "Write a the fatchar C to STREAM, preserving the attributes if possible."
  (typecase stream
    (fat-string-output-stream
     (typecase c
       (fatchar
	(stretchy-append (fat-string-string stream) (copy-fatchar c))
	(incf (fat-string-output-stream-column stream))
	(when (char= (fatchar-c c) #\newline)
	  (setf (fat-string-output-stream-column stream) 0)))
       (character
	(stretchy-append (fat-string-string stream) (make-fatchar :c c))
	(incf (fat-string-output-stream-column stream))
	(when (char= c #\newline)
	  (setf (fat-string-output-stream-column stream) 0)))))
    ((or terminal terminal-stream)
     (typecase c
       (fatchar
	(render-fatchar c stream))
       (character
	(terminal-write-char stream c))))
    (t
     (typecase c
       (fatchar	(write-char (fatchar-c c) stream))
       (character (write-char c stream))))))

(defmethod stream-write-char ((stream fat-string-output-stream) character)
  "Write ‘character’ to ‘stream’ and return ‘character’.  Every
   subclass of ‘fundamental-character-output-stream’ must have a
   method defined for this function."
  ;; (stretchy-append (fat-string-string stream) (make-fatchar :c character))
  ;; (incf (fat-string-output-stream-column stream))
  ;; (when (char= character #\newline)
  ;;   (setf (fat-string-output-stream-column stream) 0)))
  (write-fatchar (make-fatchar :c character) stream))

(defun write-fat-string (string &key (stream *standard-output*) (start 0) end)
  "Write a the fat-string STRING to STREAM, preserving the attributes if
possible."
  (when (not start)
    (setf start 0))
  (typecase stream
    (fat-string-output-stream
     (typecase string
       ;; @@@ unify these cases
       (fat-string
	;;(dbugf :fatchar "write-fat-string ALL-FAT~%")
	(loop
	   :with c
	   :and src-i = start
	   :and src-end = (or end (length (fat-string-string string)))
	   :for dst-i = (fill-pointer (fat-string-string stream))
	   :then (1+ dst-i)
	   :while (< src-i src-end)
	   :do
	   (setf c (aref (fat-string-string string) src-i))
	   ;;(stretchy-set (fat-string-string stream) i (make-fatchar :c c))
	   (stretchy-set (fat-string-string stream) dst-i c)
	   (incf (fat-string-output-stream-column stream))
	   (when (char= (fatchar-c c) #\newline)
	     (setf (fat-string-output-stream-column stream) 0))
	   (incf src-i)))
       (string
	;;(dbugf :fatchar "write-fat-string NORMAL-string -> FAT~%")
	(loop
	   :with c
	   :and src-i = start
	   :and src-end = (or end (length string))
	   :for dst-i = (fill-pointer (fat-string-string stream))
	   :then (1+ dst-i)
	   :while (< src-i src-end)
	   :do
	   (setf c (aref string src-i))
	   ;;(stretchy-set (fat-string-string stream) i (make-fatchar :c c))
	   (stretchy-set (fat-string-string stream) dst-i (make-fatchar :c c))
	   (incf (fat-string-output-stream-column stream))
	   (when (char= c #\newline)
	     (setf (fat-string-output-stream-column stream) 0))
	   (incf src-i)))))
    ((or terminal terminal-stream)
     (typecase string
       (fat-string
	;;(dbugf :fatchar "write-fat-string fat-string -> terminal~%")
	;; (apply #'tt-write-string string `(,string
	;; 				  ,@(and start `(:start ,start))
	;; 				  ,@(and end `(:end ,end)))))))
	(render-fat-string string :terminal stream :start start :end end))
       (string
	;;(dbugf :fatchar "write-fat-string NORMAL-string -> terminal~%")
	(apply #'terminal-write-string stream
	       string `(,string
			,@(and start `(:start ,start))
			,@(and end `(:end ,end)))))))
    (t
     ;;(dbugf :fatchar "write-fat-string NORMAL -> NORMAL~%")
     (if end
	 (write-string (fat-string-to-string string) stream
		       :start start :end end)
	 (write-string (fat-string-to-string string) stream
		       :start start)))))

(defmethod stream-write-string ((stream fat-string-output-stream) string
				&optional start end)
  "This is used by ‘write-string’.  It writes the string to the
  stream, optionally delimited by start and end, which default to 0
  and ‘nil’.  The string argument is returned.  The default method
  provided by ‘fundamental-character-output-stream’ uses repeated
  calls to ‘stream-write-char’."
  ;; (loop
  ;;    :for i = (fill-pointer (fat-string-string stream)) :then (1+ i)
  ;;    :for c :across (subseq string start end)
  ;;    :do
  ;;    (stretchy-set (fat-string-string stream) i (make-fatchar :c c))
  ;;    (incf (fat-string-output-stream-column stream))
  ;;    (when (char= c #\newline)
  ;;      (setf (fat-string-output-stream-column stream) 0)))
  (write-fat-string string :stream stream :start start :end end)
  string)

;;; Here's how we can write fatchars to it?

(defmethod print-object ((obj fat-string) stream)
  (macrolet ((with-quotes (() &body body)
	       `(progn
		  (when *print-escape*
		    (write-char #\" stream))
		  ,@body
		  (when *print-escape*
		    (write-char #\" stream)))))
    (cond
      (*print-readably*
       (if *read-eval*
	   (format stream "#.~s"
		   `(fatchar:span-to-fat-string ,(fat-string-to-span obj)))
	   (progn
	     ;;(dbugf :fatchar "print-object -> call-next-method~%")
	     (call-next-method)
	     )))
      ((typep stream '(or terminal:terminal terminal:terminal-stream))
       ;;(dbugf :fatchar "print-object -> render ~s~%" (type-of obj))
       (with-quotes ()
	 (render-fat-string obj :terminal stream)))
      ((typep stream 'fat-string-output-stream)
       ;; (dbugf :fatchar "print-object -> write-fat-string ~s ~s~%"
       ;;                 (type-of obj) (type-of stream))
       (with-quotes ()
	 (write-fat-string obj :stream stream)))
      (t
       ;;(print-object (fat-string-to-string obj) stream)
       ;;(format t "ZIEIE~s~%" (type-of stream)) (finish-output)
       ;; (write-string
       ;;  (with-terminal-output-to-string (:ansi)
       ;; 	(render-fat-string obj)) stream)
       ;; (dbugf :fatchar "print-object -> downconvert ~s stream ~s~%"
       ;; 	    (type-of obj) stream)
       (write (fat-string-to-string obj) :stream stream)
       ;;(call-next-method)
       ))))

(defmethod print-object ((obj fatchar) stream)
  "Print a FATCHAR to a FAT-STRING-OUTPUT-STREAM."
  ;;(format t "stream is a ~a ~a~%" (type-of stream) stream)
  (cond
    ((or *print-readably* *print-escape*)
     ;; Print as a structure:
     ;;(dbugf :fatchar "NOPE ~s~%" (type-of obj))
     ;;(print-unreadable-object (obj stream :identity t :type t))
     (call-next-method)
     )
    ((typep stream '(or terminal:terminal terminal:terminal-stream))
     ;;(format t "BLURB~s~%" (type-of obj)) (finish-output)
     (render-fatchar obj stream))
    ((typep stream 'fat-string-output-stream)
     ;;(dbugf :fatchar "BLURB Good ~s~%" (type-of obj))
     (write-fatchar obj stream))
    (t
     ;; (dbugf :fatchar "NLURB not so good ~s ~s~%"
     ;; 	    (type-of obj) (type-of stream))
     (write-char (fatchar-c obj) stream)
     )))

(defmacro with-output-to-fat-string ((var &optional string) &body body)
  `(let* ((,var (make-instance 'fat-string-output-stream
			      ,@(when string `(:string ,string))))
	  ;;(*standard-output* ,var)
	  )
     ,@body
     ;; (dbugf :fatchar "w-o-t-f-s -> ~s ~s~%" ,var (type-of ,var))
     ;; (dbugf :fatchar "w-o-t-f-s ~s ~s~%" (type-of (fat-string-string ,var))
     ;; 	    (fat-string-string ,var))
     (make-fat-string :string (fat-string-string ,var))))

(defun fs+ (s &rest rest)
  "Return a fat-string which is the arguments concatenated as if output by
PRINC. If an argument is a fat-string it's attribtues will be preserved in the
result."
  (with-output-to-fat-string (result)
    (princ s result)
    (loop :for x :in rest :do (princ x result))))

;; EOF
