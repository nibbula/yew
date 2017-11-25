;;
;; utf8b-stream.lisp - fucketyfuckfuck
;;

(defpackage :utf8b-stream
  (:documentation
   "Herein we implement the despised UTF8b stream, much to the chagrin of
everyone. Note the 'b'. The 'b' is very important! The loathsome UTF-8b stream
takes an underlying stream of regular old (UNSIGNED-BYTE 8) characters, and
converts it into UTF8b. The whole point of this is to be able to read UTF8
characters from arbitrary bytes WITHOUT ERRORS!!! I know that seems to be a lot
to ask, but I somehow persist in the delusion that it is possible to read things
WITHOUT getting errors ALL THE FUCKING TIME!!!!.")
  (:use :cl :dlib :char-util :trivial-gray-streams)
  (:export
   #:utf8b-input-stream
   #:utf8b-input-stream-input-stream
   ))
(in-package :utf8b-stream)

(declaim (optimize (speed 3) (safety 0) (debug 3) (space 0)
 		   (compilation-speed 0)))
;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
;;  		   (compilation-speed 0)))

(defclass utf8b-input-stream (fundamental-character-input-stream)
  ((input-stream
    :initarg :input-stream :accessor input-stream
    ;;:type fundamental-binary-input-stream
    :documentation "The underlying stream from which we get input.")
   (buffer
    :initarg :buffer :accessor buffer
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
  (when (not (input-stream-p (input-stream o)))
    (error "input-stream must be an input stream."))
  (when (not (equal (stream-element-type (input-stream o))
		    '(unsigned-byte 8)))
    (error "input-stream must have an element type of (unsigned-byte 8)."))
  ;; (when (not (slot-boundp o 'buffer))
  ;;   (setf (buffer o) (make-array 
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

(defmethod stream-read-sequence ((stream utf8b-input-stream) seq start end
				 &key &allow-other-keys)
  (with-slots (input-stream) stream
    (let ((i 0))
      (labels ((set-it (c) (setf (aref seq i) c))
	       (read-it () (read-byte input-stream)))
	(when (not start)
	  (setf start 0))
	(when (not end)
	  (setf end (length seq)))
	(loop :while (< i end)
	   :do (char-util::%get-utf8b-char read-it set-it))
	seq))))

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
  (get-char stream :timeout 0)
  )
|#

(defmethod stream-read-char ((stream utf8b-input-stream))
  (with-slots (buffer input-stream) stream
    (if buffer
	(pop buffer)
	(let (c)
	  (labels ((read-it () (read-byte input-stream))
		   (set-it (x) (setf c x)))
	    (char-util::%get-utf8b-char read-it set-it)
	    c)))))

#|
(defmethod stream-read-line ((stream utf8b-input-stream))
  ;; This is used by ‘read-line’.  A string is returned as the first
  ;; value.  The second value is true if the string was terminated by
  ;; end-of-file instead of the end of a line.  The default method uses
  ;; repeated calls to ‘stream-read-char’.
  (multiple-value-bind (result got-eof)
      (read-until (terminal-file-descriptor stream) #\newline)
    (values (or result "")
	    got-eof)))
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
  (push character (buffer stream)))

;; EOF
