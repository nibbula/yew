;;
;; os-stream.lisp - Streams that expose low level O/S functionality.
;;

(in-package :opsys-base)

;; Hello and welcome to yet another re-implementation of that terrible Common
;; Lisp "anti-pattern", the fd-stream, otherwise known as normal Lisp streams
;; that give you very slightly more access to the underlying operating system
;; features, most specifically the system file handle, that can be use with
;; other low level O/S API calls. On a Lisp O/S, this is probably unnecessary.
;;
;; Regardless of how wise the designers of Common Lisp were, they did not try
;; to include in the specification a portable way to work with the lowest
;; level of operating system. This was surely outside the scope of their
;; enormous task, and anyway perhaps best left to Lisp implementors. But even
;; if Lisp implementations do a perfectly wonderful job, as some do, of giving
;; us access to the operating system level of I/O, alas applications written
;; to use such facilities will not be portable between implementations. To
;; have the widest use, in the now rather sparsely populated land of Lisp, one
;; must not only be portable between hardware architecture, O/Ses, O/S
;; variations, versions and distributions, but also between implementations
;; themselves.
;;
;; In the modern world one can fairly easily get to a point where one needs to
;; use system level I/O features, but one would really like to provide an easy
;; to use and portable Lisp stream to higher level code. Thankfully, with the
;; MOP, and gray-streams (among others), one can make streams that will likely
;; work just like normal Lisp streams. But unfortunately such streams have to
;; re-implement most of the somewhat complex, and performance sensitive,
;; stream machinery that implementations provide with normal Lisp streams.
;;
;; So you might notice that you can find numerous examples of exactly this
;; throughout the Lisp community. I found about four to six without looking
;; too hard. So I should just be able to use one of those, and avoid the
;; horrible waste that is NIH syndrome, right? Right? Well, maybe. We'll see
;; what happens. In any case it has to be written or adapted to the rest of
;; the opsys package and the lower level therein.
;;
;; I extend my sincere apologies to everyone for writing this.
;;

;; This is the top, OS independant layer, which handles mostly handles
;; buffering and things. There's the lower level in the system specific code
;; such as unix/fd-stream, which actually calls the system I/O functions, and
;; where fill-buffer and flush-buffer is implemented.

;; @@@ When this is finished, consider converting terminal-ansi to use it.

(defconstant +buffer-size+ #.(* 8 1024) "Buffer sizes in bytes.")
(defconstant +input-buffer-size+  +buffer-size+ "Input buffer size in bytes.")
(defconstant +output-buffer-size+ +buffer-size+ "Output buffer size in bytes.")

(defclass os-stream (fundamental-stream)
  ((handle
   :initarg :handle :accessor os-stream-handle
   :documentation "Handle to the operating system stream.")
   ;; Isn't this in fundamental-streams?
   ;; (element-type
   ;;  :initarg :element-type :accessor os-stream-element-type
   ;;  :documentation "The type of data we operate on.")
   )
  (:documentation
   "A stream that provides facility for using it with lower level operating
system functions."))

(defun os-stream-type-for (direction element-type)
  "Return the appropirate stream type for direction and element-type."
  (cond
    ((and (eq direction :input)  (subtypep element-type 'character))
     'os-character-input-stream)
    ((and (eq direction :output) (subtypep element-type 'character))
     'os-character-input-stream)
    ((and (eq direction :io)     (subtypep element-type 'character))
     'os-character-input-stream)
    ((and (eq direction :input)  (subtypep element-type 'integer))
     'os-binary-input-stream)
    ((and (eq direction :output) (subtypep element-type 'integer))
     'os-binary-input-stream)
    ((and (eq direction :io)     (subtypep element-type 'integer))
     'os-binary-input-stream)
    ((not (find direction '(:input :output :io)))
     (error 'opsys-error
	    :format-control "Direction ~s is not valid."
	    :format-arguments `(,direction)))
    ((not (or (subtypep element-type 'character)
	      (subtypep element-type 'integer)))
     (error 'opsys-error
	    :format-control
	    "Element type ~s is not a subtype of character or integer."
	    :format-arguments `(,element-type)))
    (t ;; You seeing this Sbeecil?
     (error 'opsys-error
	    :format-control
	    "Something is wack: direction = ~s element-type = ~s."
	    :format-arguments `(,direction ,element-type)))))

(defun make-os-stream (filename
		       &key
			 (direction :input)
			 (element-type 'base-char)
			 (if-exists nil if-exists-given)
			 (if-does-not-exist nil if-does-not-exist-given)
			 (external-format :default)
			 share)
  "Return a stream which reads from or writes to FILENAME.
   Defined keywords:
    :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
    :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
    :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                        :OVERWRITE, :APPEND, :SUPERSEDE or NIL
    :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
    :SHARE - If true, allow passing to sub-processes."
  (declare (ignore if-exists-given if-does-not-exist-given external-format))
  (let* ((type (os-stream-type-for direction element-type))
	 (stream (make-instance type)))
    (os-stream-open stream filename if-exists if-does-not-exist share)))

(defun make-os-stream-from-handle (handle
				   &key
				     (element-type 'base-char)
				     (external-format 'default))
  (declare (ignore external-format))
  "Return an os-stream using the system file handle HANDLE. The direction is
determined from the handle.
   Defined keywords:
    :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR"
  (make-instance
   (os-stream-type-for (stream-handle-direction handle)
		       element-type)
   :handle handle))

(defmacro with-os-stream ((stream filespec &rest options) &body body)
  "Evaluate BODY with STREAM bound to an open os-stream, like WITH-OPEN-FILE.
When control leaves the body, either normally or abnormally (such as by
use of throw), the file is automatically closed."
  `(let (,stream)
     (unwind-protect
	  (progn
	    (setf ,stream (apply #'make-os-stream ,filespec ,@options))
	    ,@body)
       (when ,stream
	 (close ,stream)))))
  
(defgeneric fill-buffer (os-stream)
  (:documentation "Read into the input buffer. Return NIL on EOF."))

(defgeneric flush-buffer (os-stream &key force)
  (:documentation "Write the input buffer."))

(defclass os-input-stream (os-stream fundamental-input-stream)
  ((input-buffer
    :initarg :input-buffer :accessor os-stream-input-buffer
    :initform (cffi:make-shareable-byte-vector +input-buffer-size+)
    :type `(simple-array (unsigned-byte 8) ,+input-buffer-size+)
    :documentation "Store characters that have been read but not consumed.")
   (position
    :initarg :position :accessor os-input-stream-position
    :initform 0 :type fixnum
    :documentation "Read position in the input buffer.")
   (input-fill
    :initarg :input-fill :accessor os-input-stream-input-fill
    :initform 0 :type fixnum
    :documentation "Postion which in input buffer is filled to.")
   (unread-char
    :initarg :unread-char :accessor os-input-stream-unread-char
    :initform nil :type (or null character)
    :documentation "A character for the unusual unread.")
   (got-eof
    :initarg :got-eof :accessor os-input-stream-got-eof
    :initform nil :type boolean
    :documentation "True if we got an End Of File."))
  (:documentation
   "An os-stream that does input."))

(defmethod stream-clear-input ((stream os-input-stream))
  (with-slots (position unread-char) stream
    ;; @@@ Should we actually erase the data?
    (setf position 0
	  unread-char nil))
  nil)

(defmethod stream-read-sequence ((stream os-input-stream) seq start end
				 &key &allow-other-keys)
  (with-slots (input-buffer position) stream
    (when (= position (length input-buffer))
      (fill-buffer stream)
      (setf position 0))
    (loop
       :with pos = (or start 0)
       :and seq-end = (or end (length seq))
       :with copy-size
       :and left = (- seq-end pos)
       ;; :and len = (- seq-end pos)
       :while (not (zerop left))
       :do
	 (setf copy-size (min left (- (length input-buffer) position))
	       (subseq seq pos (+ pos copy-size))
	       (subseq input-buffer position (+ position copy-size)))
	 (incf pos copy-size)
	 (incf position copy-size)
	 (decf left copy-size)
	 (when (= position (length input-buffer))
	   (fill-buffer stream)
	   (setf position 0))))
  seq)

(defclass os-output-stream (os-stream fundamental-output-stream)
  ((output-buffer
    :initarg :output-buffer :accessor os-stream-output-buffer
    :initform (cffi:make-shareable-byte-vector +output-buffer-size+)
    :type `(simple-array (unsigned-byte 8) ,+output-buffer-size+)
    :documentation "Store characters that have been written but not flushed.")
   (output-position
    :initarg :output-position :accessor os-output-stream-output-position
    :initform 0 :type fixnum
    :documentation "The end of the last written data in the buffer.")
   (output-fill
    :initarg :output-fill :accessor os-output-stream-output-fill
    :initform 0 :type fixnum
    :documentation "Position which the output buffer is filled to."))
  (:documentation
   "An os-stream that does output."))

(defmethod stream-clear-output ((stream os-output-stream))
  ;; This is like ‘cl:clear-output’, but for Gray streams: clear the
  ;; given output ‘stream’.  The default method does nothing.
  (with-slots (output-position output-buffer) stream
    (setf output-position (length output-buffer))))

(defmethod stream-finish-output ((stream os-output-stream))
  ;; Attempts to ensure that all output sent to the Stream has reached
  ;; its destination, and only then returns false.  Implements
  ;; ‘finish-output’.  The default method does nothing.
  (flush-buffer stream))

(defmethod stream-force-output ((stream os-output-stream))
  ;; Attempts to force any buffered output to be sent.  Implements
  ;; ‘force-output’.  The default method does nothing.
  (flush-buffer stream :force t))

;; This is like ‘cl:write-sequence’, but for Gray streams.
(defmethod stream-write-sequence ((stream os-output-stream) seq start end
				  &key &allow-other-keys)
  (with-slots (output-buffer output-position) stream
    (when (= output-position (length output-buffer))
      (flush-buffer stream)
      (setf output-position 0))
    (loop
       :with left = (- (or end (length seq)) (or start 0))
       :and pos = (or start 0)		; position in seq
       :and copy-size
       :while (not (zerop left))
       :do
	 (setf copy-size (min left (- output-position
				      (length output-buffer)))
	       (subseq output-buffer
		       output-position (+ output-position copy-size))
	       (subseq seq pos (+ pos copy-size)))
	 (incf pos copy-size)
	 (incf output-position copy-size)
	 (decf left copy-size)
	 (when (= output-position (length output-buffer))
	   (flush-buffer stream)
	   (setf output-position 0))))
  seq)

(defclass os-io-stream (os-input-stream os-output-stream)
  ()
  (:documentation
   "A stream that provides facility for using it with lower level operating
system functions."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "binary" streams

(defclass os-binary-stream (os-stream fundamental-binary-stream)
  (
   ;; (element-type
   ;;  :initarg :element-type :accessor os-binary-stream-element-type
   ;;  :documentation "Supports UNSIGNED-BYTEs that are a multiple of 8.")
   )
  (:default-initargs
   :element-type '(unsigned-byte 8))
  (:documentation
   "An os-stream with an element type of byte."))

(defclass os-binary-input-stream (os-binary-stream os-input-stream
				  fundamental-binary-input-stream)
  ()
  (:documentation
   "An os-stream that does input of bytes."))

(declaim (inline get-byte)
	 (ftype (function (os-input-stream) (or (unsigned-byte 8) null))
		get-byte))

(defun get-byte (stream)
  (with-slots (input-buffer position got-eof) stream
    (or (if (= position (length input-buffer))
	    (when (not (fill-buffer stream))
	      (setf got-eof t)
	      (return-from get-byte nil))
	    (prog1 (aref input-buffer position)
	      (incf position))))))

(defmethod stream-read-byte ((stream os-binary-input-stream))
  ;; Used by ‘read-byte’; returns either an integer, or the symbol
  ;; ‘:eof’ if the stream is at end-of-file.
  (or (get-byte stream) :eof))

(defclass os-binary-output-stream (os-binary-stream os-output-stream
				   fundamental-binary-output-stream)
  ()
  (:documentation
   "An os-stream that does output of bytes."))

(defun put-byte (stream byte)
  (with-slots (output-buffer output-position) stream
    (when (= output-position (length output-buffer))
      (flush-buffer stream)
      (setf output-position 0))
    (setf (aref output-buffer output-position) byte)
    (incf output-position))
  byte)

(declaim (inline put-byte)
	 (ftype (function (os-output-stream (unsigned-byte 8))
			  (unsigned-byte 8)) put-byte))

(defmethod stream-write-byte ((stream os-binary-output-stream) integer)
  ;; Implements ‘write-byte’; writes the integer to the stream and
  ;; returns the integer as the result.
  (put-byte stream integer))

(defclass os-binary-io-stream (os-binary-input-stream
			       os-binary-output-stream)
  ()
  (:documentation
   "An os-stream that does input and output of bytes."))

;; (defmethod stream-element-type ((stream os-binary-stream))
;;   (os-binary-stream-element-type stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character streams

(defclass os-character-stream (os-stream fundamental-character-stream)
  ((encoding
    :initarg :encoding :accessor os-character-stream-encoding  
    :documentation "Character encoding for the stream."))
  (:documentation
   "An os-stream with an element type of character and supports encoding."))

;;;;;;;;;;
;; input

(defclass os-character-input-stream (os-character-stream
				     fundamental-character-input-stream)
  ()
  (:documentation
   "An os-stream that does input of characters."))

(defun get-char (stream)
  (with-slots (unread-char input-buffer position got-eof) stream
    (or (and unread-char (prog1 unread-char (setf unread-char nil)))
	(if (= position (length input-buffer))
	    (when (not (fill-buffer stream))
	      (setf got-eof t)
	      (return-from get-char nil))
	    (prog1 (aref input-buffer position)
	      (incf position))))))

(declaim (inline get-char)
	 (ftype (function (os-input-stream) (or character null)) get-char))

(defmethod stream-peek-char ((stream os-character-input-stream))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
  (with-slots (unread-char input-buffer position got-eof) stream
    (or (and got-eof :eof)
	unread-char
	(if (= position (length input-buffer))
	    (when (not (fill-buffer stream))
	      (setf got-eof t)
	      (return-from stream-peek-char :eof))
	    (aref input-buffer position)))))

(defmethod stream-read-char-no-hang ((stream os-character-input-stream))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  (with-slots (unread-char got-eof) stream
    (or (and unread-char (prog1 unread-char (setf unread-char nil)))
	(and (listen-for stream 0)
	     (get-char stream))
	(and got-eof :eof))))

(defmethod stream-read-char ((stream os-character-input-stream))
  ;; Read one character from the stream.  Return either a character
  ;; object, or the symbol ‘:eof’ if the stream is at end-of-file.
  ;; Every subclass of ‘fundamental-character-input-stream’ must define
  ;; a method for this function.
  (with-slots (input-buffer position) stream
    (when (= position (length input-buffer))
      (when (not (fill-buffer stream))
	(return-from stream-read-char :eof)))
    (prog1 (aref input-buffer position)
      (incf position))))

(defmethod stream-read-line ((stream os-character-input-stream))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (with-slots (input-buffer position) stream
    (with-output-to-string (str)
      (loop
	 (cond
	   ((char/= (aref input-buffer position) #\newline)
	    (return-from stream-read-line (values str nil)))
	   ((= position (length input-buffer))
	    (when (not (fill-buffer stream))
	      (return-from stream-read-line (values str t))))
	   (t
	    (write-char (aref input-buffer position) str)
	    (incf position)))))))

(defmethod stream-listen ((stream os-character-input-stream))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (with-slots (input-buffer position) stream
    (or (< position (length input-buffer))
	(listen-for stream 0))))

(defmethod stream-unread-char ((stream os-character-input-stream) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (with-slots (input-buffer position unread-char) stream
    (if (not (zerop position))
	(progn
	  (decf position)
	  ;; @@@ should we error if the character isn't the same?
	  (setf (aref input-buffer position) character))
	(setf unread-char character)))
  nil)

;;;;;;;;;;;
;; output

(defclass os-character-output-stream (os-character-stream
				      fundamental-character-output-stream)
  ((column
    :initarg :column :accessor os-character-output-stream-column
    :initform 0 :type integer
    :documentation "Output column."))
  (:documentation
   "An os-stream that does output of characters."))

(defun put-char (stream char)
  (with-slots (output-buffer output-position column) stream
    (when (= output-position (length output-buffer))
      (flush-buffer stream)
      (setf output-position 0))
    (setf (aref output-buffer output-position) char)
    (incf output-position)
    (if (char= char #\newline)		; @@@ eol style ?
	(setf column 0)
	;; I wish I could do this, but that means we might have to depend
	;; on cl-unicode.
	;; (incf column (display-length char))))
	(incf column)))
  char)

(declaim (inline put-char)
	 (ftype (function (os-output-stream character) character) put-char))

(defmethod stream-advance-to-column ((stream os-character-output-stream) col)
  ;; Write enough blank space so that the next character will be written
  ;; at the specified column.  Returns true if the operation is
  ;; successful, or ‘nil’ if it is not supported for this stream.  This
  ;; is intended for use by by ‘pprint’ and ‘format’ ~T. The default
  ;; method uses ‘stream-line-column’ and repeated calls to
  ;; ‘stream-write-char’ with a ‘#space’ character; it returns ‘nil’ if
  ;; ‘stream-line-column’ returns ‘nil’.
  (with-slots (column) stream
    (while (< column col)
      (put-char stream #\space)))
  t)

(defmethod stream-fresh-line ((stream os-character-output-stream))
  ;; Outputs a new line to the Stream if it is not positioned at the
  ;; beginning of a line.  Returns ‘t’ if it output a new line, nil
  ;; otherwise.  Used by ‘fresh-line’.  The default method uses
  ;; ‘stream-start-line-p’ and ‘stream-terpri’.
  (with-slots (column) stream
    (if (/= 0 column)
	(and (put-char stream #\newline)
	     t)
	nil)))

(defmethod stream-line-column ((stream os-character-output-stream))
  ;; Return the column number where the next character will be written,
  ;; or ‘nil’ if that is not meaningful for this stream.  The first
  ;; column on a line is numbered 0.  This function is used in the
  ;; implementation of ‘pprint’ and the ‘format’ ~T directive.  For
  ;; every character output stream class that is defined, a method must
  ;; be defined for this function, although it is permissible for it to
  ;; always return ‘nil’.
  (os-character-output-stream-column stream))

(defmethod stream-line-length ((stream os-character-output-stream))
  ;; Return the stream line length or ‘nil’.
  nil
  ;; or maybe:
  ;; (or (and (terminal-p handle) (terminal-width handle)) nil)
  )

(defmethod stream-start-line-p ((stream os-character-output-stream))
  ;; Is ‘stream’ known to be positioned at the beginning of a line?  It
  ;; is permissible for an implementation to always return ‘nil’.  This
  ;; is used in the implementation of ‘fresh-line’.  Note that while a
  ;; value of 0 from ‘stream-line-column’ also indicates the beginning
  ;; of a line, there are cases where ‘stream-start-line-p’ can be
  ;; meaningfully implemented although ‘stream-line-column’ can’t be.
  ;; For example, for a window using variable-width characters, the
  ;; column number isn’t very meaningful, but the beginning of the line
  ;; does have a clear meaning.  The default method for
  ;; ‘stream-start-line-p’ on class
  ;; ‘fundamental-character-output-stream’ uses ‘stream-line-column’, so
  ;; if that is defined to return ‘nil’, then a method should be
  ;; provided for either ‘stream-start-line-p’ or ‘stream-fresh-line’.
  (zerop (os-character-output-stream-column stream)))

(defmethod stream-terpri ((stream os-character-output-stream) )
  ;; Writes an end of line, as for ‘terpri’.  Returns ‘nil’.  The
  ;; default method does (‘stream-write-char’ stream #NEWLINE).
  (put-char stream #\newline)
  nil)

(defmethod stream-write-char ((stream os-character-output-stream) character)
  ;; Write ‘character’ to ‘stream’ and return ‘character’.  Every
  ;; subclass of ‘fundamental-character-output-stream’ must have a
  ;; method defined for this function.
  (put-char stream character))

(defun update-column-for-char (stream char)
  (with-slots (column) stream
    (cond
      ((graphic-char-p char)
       (cond
	 ;; @@@ I wish I could use these:
	 ;; ((zero-width-char-p c) 0)
	 ;; ((combining-char-p char) 0)
	 ;; ((double-wide-char-p char) 2)
	 (t 1)))			;normal case
      (t
       (case char
	 ((#\return #\newline) ;; @@@ ocrnl right?
	  (setf column 0))
	 (#\tab
	  (incf column (- (1+ (logior 7 column)) column)))
	 (otherwise
	  0 ;; some non-graphic control char?
	  ))))))

(defun update-column (stream thing &key start end)
  (etypecase thing
    (character (update-column-for-char stream thing))
    (string
     (loop
	:with the-end = (or end (length thing))
	:and the-start = (or start 0)
	:for i :from the-start :below the-end
	:do (update-column-for-char stream (char thing i))))))

(defmethod stream-write-string ((stream os-character-output-stream) string
				&optional (start 0) end)
  ;; This is used by ‘write-string’.  It writes the string to the
  ;; stream, optionally delimited by start and end, which default to 0
  ;; and ‘nil’.  The string argument is returned.  The default method
  ;; provided by ‘fundamental-character-output-stream’ uses repeated
  ;; calls to ‘stream-write-char’.

  (stream-write-sequence stream string :start start :end end)
  (update-column stream string :start start :end end)
  string)

(defclass os-character-io-stream (os-character-input-stram
				  os-character-output-stram)
  ()
  (:documentation
   "An os-stream that does input and output of characters."))

;; @@@ This is probably the default method and goes without saying.
;; (defmethod stream-element-type ((stream os-character-stream))
;;   (declare (ignore stream))
;;   'character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric notice-changes (stream) ;; @@@ Or maybe re-sync?
  (:documentation
   "This notifies the stream machinery that changes at the O/S level may have
happend since it's last operations. This may have to be done to allow the
upper level of the stream to resume working properly."))

;; EOF
