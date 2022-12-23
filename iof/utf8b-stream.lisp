;;;
;;; utf8b-stream.lisp - A stupid way of solving a problem that apparently nobody
;;;                     cares about.
;;;

(defpackage :utf8b-stream
  (:documentation
   "Herein we implement the despised UTF8b stream, much to the chagrin of
everyone. Note the 'b'. The 'b' is very important! The loathsome UTF-8b stream
takes an underlying stream of regular old (UNSIGNED-BYTE 8) characters,
otherwise known as octets or bytes, and converts it into UTF8b. The whole
point of this is to be able to read UTF8 characters from arbitrary bytes
WITHOUT ERRORS!!! I know that seems to be a lot to ask, but I somehow persist
in the delusion that it is possible to read things WITHOUT getting errors ALL
THE FUCKING TIME!!!!.

So, you might ask: “Not getting errors doesn't really sound as bad as you've
made it out to be, so what's the downside?” Good question. There seems to be
at least three possible downsides to this technique:

First, we have sacrificed a small range of your ‘private use’ unicode
characters. Specifically the range #xdc00 to #xdcff. In other words, this uses
them for storing any invalid UTF-8 bytes, and therefore there will be no way
to distinguish them in the resulting unicode characters, and valid private
use characters may be decoded into back into invalid UTF-8 bytes if the
reverse UTF-8b filter is used. But if this happens, what you started off with
wasn't valid UTF-8 in the first place. If you have a stream with valid UTF-8
and which uses that range of private use characters, just don't use this
filter.

The second problem is that it probably slows things down quite a bit. This
could be solved by adding UTF-8b encoding to the implementation level, where
it would have about the same performance characteristics as normal UTF8.

The third problem is that it software that uses this has to explicitly use it,
and allow for turning it off. This adds fairly pointless complexity. This
could be solved, like with the performance issue, by having in the normal set
of encodings for the implementation. Usually there is a way for the user to
pick the encoding. There are probably more problems but I forgot what they
are.")
  (:use :cl :dlib :char-util :dgray :unicode)
  (:export
   #:utf8b-input-stream
   #:utf8b-stream-stream
   #:utf8b-stream-buffer
   #:with-utf8b-input
   #:with-utf8b-stream
   ))
(in-package :utf8b-stream)

;; (declaim (optimize (speed 3) (safety 0) (debug 3) (space 0)
;; 		   (compilation-speed 0)))
;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
;; 		   (compilation-speed 0)))

(defclass utf8b-input-stream (fundamental-character-input-stream)
  ((input-stream
    :initarg :input-stream :accessor utf8b-stream-stream
    ;;:type fundamental-binary-input-stream
    :documentation "The underlying stream from which we get input.")
   (buffer
    :initarg :buffer :accessor utf8b-stream-buffer
    ;;:type (simple-array (unisgned-byte 8) (*))
    :initform nil
    :documentation "Yet another useless buffer.")
   ;; (read-point
   ;;  :initarg :read-point :accessor read-point :initform 0 :type fixnum
   ;;  :documentation "Index of the next byte to read from the buffer.")
   )
  (:documentation "This is an input stream. You know what to do."))

(defmethod initialize-instance
    :after ((o utf8b-input-stream) &rest initargs &key &allow-other-keys)
  "Initialize a utf8b-input-stream."
  (declare (ignore initargs))
  (when (not (slot-boundp o 'input-stream))
    (error "An input-stream must be provided."))
  (when (not (input-stream-p (utf8b-stream-stream o)))
    (error "input-stream must be an input stream."))
  (when (not (equal (stream-element-type (utf8b-stream-stream o))
		    '(unsigned-byte 8)))
    (error "input-stream must have an element type of (unsigned-byte 8)."))
  ;; (when (not (slot-boundp o 'buffer))
  ;;   (setf (utf8b-stream-buffer o) (make-array
  ;; 				 :element-type '(unsigned-byte 8)
  ;; 				 :fill-pointer t))
  )

;; common methods

(defmethod-quiet close ((stream utf8b-input-stream) &key abort)
  (declare (ignore stream abort))
  ;; Don't close the underlying stream.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input stream methods.

(defmethod stream-clear-input ((stream utf8b-input-stream))
  (with-slots (buffer input-stream) stream
    (setf buffer nil)
    (clear-input input-stream)))

;; It's probably easier to just copy-paste these optimized versions, than
;; to bother with a macro.

#-(or ecl clisp excl)
;; @@@ It seems like some implementations don't have a class for simple-vector.
;; We should probably make a typecase or something if it makes a difference.
(defmethod stream-read-sequence ((stream utf8b-input-stream) (seq simple-vector)
				 start end &key &allow-other-keys)
  (declare (ignore start))
  (with-slots (input-stream) stream
    (let ((i 0) #| (istart 0) |# (iend 0))
      (declare (type fixnum i #|istart|# iend))
      (labels ((set-it (c)
		 (declare (type character c))
		 (setf (svref seq i) c))
	       (read-it ()
		 (read-byte input-stream)))
	(handler-case
	    (progn
	      (setf #| istart (if (not start) 0 start) |#
		    iend (if (not end) (length seq) end))
	      (loop :while (< i iend)
		 :do (%get-utf8b-char read-it set-it)
		 (incf i)))
	  (end-of-file (c)
	    (declare (ignore c))))
	i))))

(defmethod stream-read-sequence ((stream utf8b-input-stream) (seq vector)
				 start end &key &allow-other-keys)
  ;; One should probably un-muffle this if any changes are made.
  ;; That includes changes to the macros %get-utf8b-char from char-util.
  ;; This is the semi-generic vector version, which has to be a little slower.
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (declare (ignore start))
  (with-slots (input-stream) stream
    (let ((i 0) #|(istart 0)|# (iend 0))
      (declare (type fixnum i #|istart|# iend))
      (labels ((set-it (c)
		 (declare (type character c))
		 (setf (aref seq i) c))
	       (read-it ()
		 (read-byte input-stream)))
	(handler-case
	    (progn
	      (setf #|istart (if (not start) 0 start)|#
		    iend (if (not end) (length seq) end))
	      (loop :while (< i iend)
		 :do (%get-utf8b-char read-it set-it)
		 (incf i)))
	  (end-of-file (c)
	    (declare (ignore c))))
	i))))

(defmethod stream-read-sequence ((stream utf8b-input-stream) (seq list)
				 start end &key &allow-other-keys)
  (declare (ignore start))
  (with-slots (input-stream) stream
    (let ((i 0) #|(istart 0)|# (iend 0) (l seq))
      (declare (type fixnum i #|istart|# iend)
	       (type list l))
      (labels ((set-it (c)
		 (declare (type character c))
		 (rplaca l c))
	       (read-it ()
		 (read-byte input-stream)))
	(handler-case
	    (progn
	      (setf #| istart (if (not start) 0 start) |#
		    iend (if (not end) (length seq) end))
	      (loop :while (< i iend)
		 :do (%get-utf8b-char read-it set-it)
		 (setf l (cdr l))
		 (incf i)))
	  (end-of-file (c)
	    (declare (ignore c))))
	i))))

(defmethod stream-read-sequence ((stream utf8b-input-stream) seq
				 start end &key &allow-other-keys)
  ;; One should probably un-muffle this if any changes are made.
  ;; That includes changes to the macros %get-utf8b-char from char-util.
  ;; This is the generic version, for non list or vector sequences, which
  ;; may not even get used, and has to be slower by definition.
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (declare (ignore start))
  (with-slots (input-stream) stream
    (let ((i 0) #|(istart 0)|# (iend 0))
      (declare (type fixnum i #|istart|# iend))
      (labels ((set-it (c)
		 (declare (type character c))
		 (setf (elt seq i) c))
	       (read-it ()
		 (read-byte input-stream)))
	(handler-case
	    (progn
	      (setf #|istart (if (not start) 0 start)|#
		    iend (if (not end) (length seq) end))
	      (loop :while (< i iend)
		 :do (%get-utf8b-char read-it set-it)
		 (incf i)))
	  (end-of-file (c)
	    (declare (ignore c))))
	i))))

#|
(defgeneric stream-peek-char ((stream utf8b-input-stream))
  ;; This is used to implement ‘peek-char’; this corresponds to
  ;; ‘peek-type’ of ‘nil’.  It returns either a character or ‘:eof’.
  ;; The default method calls ‘stream-read-char’ and
  ;; ‘stream-unread-char’.
)

(defmethod stream-read-char-no-hang ((stream utf8b-input-stream))
  ;; This is used to implement ‘read-char-no-hang’.  It returns either a
  ;; character, or ‘nil’ if no input is currently available, or ‘:eof’
  ;; if end-of-file is reached.  The default method provided by
  ;; ‘fundamental-character-input-stream’ simply calls
  ;; ‘stream-read-char’; this is sufficient for file streams, but
  ;; interactive streams should define their own method.
  )
|#

;; Read one character from the stream.  Return either a character
;; object, or the symbol ‘:eof’ if the stream is at end-of-file.
;; Every subclass of ‘fundamental-character-input-stream’ must define
;; a method for this function.
(defmethod stream-read-char ((stream utf8b-input-stream))
  (with-slots (buffer input-stream) stream
    (if buffer
	(pop buffer)
	(let (c b)
	  (labels ((read-it ()
		     (if (setf b (read-byte input-stream nil))
			 b
			 (return-from stream-read-char
			   ;; If we already got part of a character,
			   ;; return it, and return :eof next time.
			   ;; Otherwise just return :eof now.
			   (if c
			       (prog1 c
				 (push :eof buffer))
			       :eof))))
		   (set-it (x) (setf c x)))
	    (%get-utf8b-char read-it set-it)
	    c)))))

#|
(defmethod stream-read-line ((stream utf8b-input-stream))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
)
|#

(defmethod stream-listen ((stream utf8b-input-stream))
  ;; This is used by ‘listen’.  It returns true or false.  The default
  ;; method uses ‘stream-read-char-no-hang’ and ‘stream-unread-char’.
  ;; Most streams should define their own method since it will usually
  ;; be trivial and will always be more efficient than the default
  ;; method.
  (with-slots (buffer input-stream) stream
    (or buffer
	(listen input-stream))))

(defmethod stream-unread-char ((stream utf8b-input-stream) character)
  ;; Undo the last call to ‘stream-read-char’, as in ‘unread-char’.
  ;; Return ‘nil’.  Every subclass of
  ;; ‘fundamental-character-input-stream’ must define a method for this
  ;; function.
  (push character (utf8b-stream-buffer stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition utf8b-stream-error (simple-error stream-error)
  ()
  (:documentation "A simple stream error for utf8b streams."))

(defmacro with-utf8b-stream ((var stream &key (errorp t)) &body body)
  "Evaluate the BODY with VAR bound to a UTF8B-INPUT-STREAM wrapping STREAM.
If ERRORP is true, STREAM must be an open input stream with an element type
of (UNSIGNED-BYTE 8). If ERRORP is false, then if the streem doesn't qualify, it
still just binds STREAM to VAR and evaluates the BODY."
  (with-names (source thunk)
    `(let (,var
	   (,source ,stream))
       (flet ((,thunk () (progn ,@body)))
	 (if (and (input-stream-p ,source)
		  (open-stream-p ,source)
		  (equal (stream-element-type ,source) '(unsigned-byte 8)))
	     (unwind-protect
		  (progn
		    (setf ,var (make-instance 'utf8b-input-stream
					      :input-stream ,source))
		    (,thunk))
	       (close ,var))
	     (progn
	       (with-simple-restart (continue
				     "Just use the stream without UTF8b.")
		 (if ,errorp
		     (error 'utf8b-stream-error
			    :stream ,source
			    :format-control
			    "The stream can't be used for utf8b input.")))
	       (progn
		 (setf ,var ,source)
		 (,thunk))))))))

(defmacro with-utf8b-input ((var input &key (errorp t)) &body body)
  "Like with-utf8b-stream but INPUT can also be a file name."
  (with-names (in fvar)
    `(let ((,in ,input))
       (etypecase ,in
	 ((or string pathname)
	  (with-open-file (,fvar ,in :element-type '(unsigned-byte 8))
	    (with-utf8b-stream (,var ,fvar :errorp ,errorp)
	      ,@body)))
	 (stream
	  (with-utf8b-stream (,var ,in :errorp ,errorp)
	    ,@body))))))

;; EOF
