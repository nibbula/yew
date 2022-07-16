;;;
;;; filter-stream.lisp - Filter a stream with a function.
;;;

(defpackage :filter-stream
  (:documentation "Streams that filter an underlying stream with a function.")
  (:use :cl :dlib :trivial-gray-streams :stretchy)
  (:export
   #:wrapped-stream
   #:filter-stream
   #:filter-input-stream
   #:filter-output-stream
   #:filter-io-stream
   #:filter-stream-buffer-size
   #:filter-stream-filter-function
   #:filter-stream-unit
   #:make-filter-stream
   #:filter<
   #:filter>
   ))
(in-package :filter-stream)

;; Wrapped stream

(defclass wrapped-stream (trivial-gray-stream-mixin)
  ((wrapped-stream
    :initarg :wrapped-stream
    :accessor wrapped-stream
    :documentation "The underlying stream."))
  (:documentation "A stream that wraps around another stream."))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (wrapped-stream stream)))

(defmethod-quiet close ((stream wrapped-stream) &key (abort t))
  (if abort
      (progn
	(force-output stream)
	(force-output (wrapped-stream stream)))
      (progn
	(finish-output stream)
	(finish-output (wrapped-stream stream))))
  (values t))

;; (defmethod close ((stream wrapped-stream) &key
;;   (close (wrapped-stream stream) :abort abort))

;; (defmethod stream-file-position (stream &optional position-spec)
;;   "Returns or changes the current position within stream."
;;   (stream-file-position (wrapped-stream stream) position-spec))

;; Base class

(defclass filter-stream (wrapped-stream)
  ((filter-function
    :initarg :filter-function
    :accessor filter-stream-filter-function
    :documentation
    "A function that performs the filtering. Should accept a unit, and return
     a filtered unit.")
   (unit
    :initarg :unit
    :accessor filter-stream-unit
    :initform :line
    :documentation
    "The unit of data to be filtered at a time. It should be one of
     :SINGLE, :LINE, :BUFFER, or :ANY. :SINGLE means a single object, usually
     either a character or byte. If it is :BUFFER, then ‘buffer-size’ is used.")
   (buffer-size
    :initarg :buffer-size
    :accessor filter-stream-buffer-size
    :documentation "The size, in characters, of the buffer.")
   (position
    :initarg :position
    :accessor stream-position
    :initform 0
    :documentation "Current position in the stream.")
   (state
    :initarg :state
    :accessor filter-stream-state
    :initform nil
    :documentation "Place for filter functions to put some state information."))
  (:documentation
   "A stream that filters an underlying stream by applying a function."))

(defclass character-filter-stream (filter-stream)
  ((column
    :initarg :column
    :accessor stream-column
    :initform 0
    :documentation "Current column offset from the start of the line."))
  (:documentation "A filter stream that works with characters."))

(defclass binary-filter-stream (filter-stream)
  ()
  (:documentation "A filter stream that works with binary bytes."))

(defparameter *default-line-size* 128)
(defparameter *default-buffer-size* (* 8 1024))

;; (defmethod stream-file-position ((stream filter-stream) &optional position-spec)
;;    "Returns or changes the current position within stream."
;;    (if position-spec
;;        nil
;;        (stream-position stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input

(defclass filter-input-stream (filter-stream
			       fundamental-character-input-stream)
  ((input-buffer
    :initarg :input-buffer
    :accessor filter-input-stream-buffer
    :documentation "For buffering input."))
  (:documentation "A filtered stream for input."))

(defclass character-filter-input-stream (filter-stream
					 fundamental-character-input-stream)
  ()
  (:documentation "A filtered stream for character input."))

(defclass binary-filter-input-stream (filter-stream
				      fundamental-binary-input-stream)
  ()
  (:documentation "A filtered stream for binary input."))

(defmethod stream-clear-input ((stream filter-input-stream))
  (stretchy-truncate (filter-input-stream-buffer stream) 0))

(defmethod initialize-instance :after ((o character-filter-input-stream)
				       &rest initargs
				       &key &allow-other-keys)
  "Initialize a filter-input-stream."
  (declare (ignore initargs))
  ;; Make a buffer depending on unit type.
  (with-slots (unit input-buffer buffer-size) o
    (when (slot-boundp o 'unit)
      (case unit
	(:line
	 (setf input-buffer (make-stretchy-string *default-line-size*)))
	(:buffer
	 (setf input-buffer
	       (make-stretchy-string 
		(or (and (slot-boundp o 'buffer-size) buffer-size)
		    *default-buffer-size*))))))))

(defmethod initialize-instance :after ((o binary-filter-input-stream)
				       &rest initargs
				       &key &allow-other-keys)
  "Initialize a filter-input-stream."
  (declare (ignore initargs))

  (when (null (stream-element-type o))
    ;; @@@ or I suppose we could default to (unsigned-byte 8) ?
    (error "Stream element type must not be NIL."))

  ;; Make a buffer depending on unit type.
  (with-slots (unit input-buffer buffer-size) o
    (when (slot-boundp o 'unit)
      (setf input-buffer
	    (make-stretchy-vector
	     (or (and (slot-boundp o 'buffer-size) buffer-size)
		 *default-buffer-size*)
	     :element-type (stream-element-type o))))))

#|

@@@ TODO.......

(defmethod stream-read-sequence ((stream filter-input-stream)
				 seq &optional (start 0) end)
  ;; (let ((pos 0)
  ;; 	(new-seq    (stream-read-sequence (wrapped-stream)))
  ;;   (loop :while )
  ;;   (setf (elt seq pos)
  ;; 	  )
  ;;   pos)
  )

(defmethod stream-read-byte ((stream filter-input-stream))
  )

(defmethod stream-peek-char ((stream filter-input-stream))
  (if
  (stream-peek-char (wrapped-stream stream)))

(defmethod stream-read-char-no-hang ((stream filter-input-stream))
  (stream-read-char-no-hang (wrapped-stream stream)))

(defmethod stream-read-char ((stream filter-input-stream))
  (case (filter-stream-unit stream)
    ((:character :any)
     (
     (let ((c (read-char (wrapped-stream stream) nil :eof)))
       (when (eql c :eof)
	 (return-from stream-read-char :eof))
       (when (characterp c)
	 (incf (stream-position stream))
	   
     (funcall (filter-stream-filter-function
	       (stream-read-char (wrapped-stream stream)))))
    (:line
     (
    (:buffer)
  )

(defmethod stream-read-line ((stream filter-input-stream))
  )

(defmethod stream-listen ((stream filter-input-stream))
  )

(defmethod stream-unread-char ((stream filter-input-stream) character)
  )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

(defclass filter-output-stream (filter-stream)
  ((output-buffer
    :initarg :output-buffer
    :accessor filter-output-stream-buffer
    :documentation "For buffering output, such as lines."))
  (:documentation "A filtered stream for output."))

(defclass character-filter-output-stream (filter-output-stream
					  fundamental-character-output-stream)
  ()
  (:documentation "A filtered stream for character output."))

(defclass binary-filter-output-stream (filter-output-stream
				       fundamental-binary-output-stream)
  ()
  (:documentation "A filtered stream for binary output."))

(defmethod initialize-instance :after ((o character-filter-output-stream)
				       &rest initargs
				       &key &allow-other-keys)
  "Initialize a character-filter-output-stream."
  (declare (ignore initargs))
  ;; Make a buffer depending on unit type.
  (with-slots (unit output-buffer buffer-size) o
    (when (slot-boundp o 'unit)
      (case unit
	(:line
	 (setf output-buffer (make-stretchy-string *default-line-size*)))
	(:buffer
	 (setf output-buffer
	       (make-stretchy-string 
		(or (and (slot-boundp o 'buffer-size) buffer-size)
		    *default-buffer-size*))))))))

(defmethod initialize-instance :after ((o binary-filter-output-stream)
				       &rest initargs
				       &key &allow-other-keys)
  "Initialize a binary-filter-output-stream."
  (declare (ignore initargs))
  ;; Make a buffer depending on unit type.
  (with-slots (output-buffer buffer-size) o
    (setf output-buffer
	  (make-stretchy-vector
	   (or (and (slot-boundp o 'buffer-size) buffer-size)
	       *default-buffer-size*)
	   :element-type (stream-element-type o)))))

;; We will take the default methods for these, which do nothing.

;; (defmethod stream-clear-output ((stream filter-output-stream))
;;   (declare (ignore stream)))

(defmethod stream-finish-output ((stream filter-output-stream))
  (with-slots (output-buffer wrapped-stream) stream
    (when (and output-buffer (length output-buffer))
      (write-string output-buffer wrapped-stream)
      (finish-output wrapped-stream))))

(defmethod stream-force-output ((stream filter-output-stream))
  (with-slots (output-buffer wrapped-stream) stream
    (when (and output-buffer (length output-buffer))
      (write-string output-buffer wrapped-stream)
      (force-output wrapped-stream))))

(defgeneric adjust-column-for-element (stream element)
  (:documentation
   "Adjust the ‘stream-column’ in ‘stream’ for outputting ‘element’.")
  (:method (stream (char character))
    ;; Of course this is very wrong for unicode. @@@
    (case char
      (#\backspace (decf (stream-column stream)))
      ((#\newline #\return) (setf (stream-column stream) 0))
      (#\tab (incf (stream-column stream)
		   (- 8 (mod (stream-column stream) 8))))
      (t (incf (stream-column stream)))))
  (:method (stream (element integer))
    #| Don't bother adjusting for non-character streams. |#))

(defgeneric adjust-column-for-seq (stream sequence)
  (:documentation "Adjust the ‘stream-column’ in ‘stream’ for the ‘sequence’.")
  (:method (stream (seq string))
    (loop :for i :from 0 :below (length seq) :do
      (adjust-column-for-element stream (elt seq i))))
  (:method (stream seq)
    #| Don't bother for non-character streams. |#))

(defgeneric filter-write-element (stream element)
  (:documentation "Write ‘element’ to stream.")
  (:method (stream (element character))
    (write-char element (wrapped-stream stream)))
  (:method (stream (element integer))
    (write-byte element (wrapped-stream stream))))

(defun filter-write-seq (stream seq start end)
  "Write a sequence without buffering."
  ;; Just write what we got.
  (let* ((new-seq (funcall (filter-stream-filter-function stream) stream
			   (cond
			     ((and start end)
			      (subseq seq start end))
			     ((and start)
			      (subseq seq start))
			     (t seq))))
	 (len (length new-seq)))
    (write-sequence new-seq (wrapped-stream stream))
    (incf (stream-position stream) len)
    (adjust-column-for-seq stream new-seq)))

(defun filter-write-seq-single (stream seq start end)
  "Write a sequence a stream element at a time."
  (let* ((len (length seq))
	 (my-start (or start 0))
	 (my-end (or end len))
	 result)
    (loop :with c :for i :from my-start :below my-end :do
       (setf c (elt seq i)
	     result (funcall (filter-stream-filter-function stream)
			     stream c))
       (cond
	 ((subtypep result (stream-element-type stream))
	  (break)
	  (filter-write-element result (wrapped-stream stream))
	  (adjust-column-for-element stream result)
	  (incf (stream-position stream)))
	 ((vectorp result)
	  (write-sequence result (wrapped-stream stream))
	  (adjust-column-for-seq stream result)
	  (incf (stream-position stream) (length result)))
	 (t
	  (setf result (format nil "~a" result))
	  ;; @@@ This might not work
	  (write-sequence result (wrapped-stream stream))
	  (adjust-column-for-seq stream result)
	  (incf (stream-position stream) (length result)))
	 ))))

(defun filter-write-seq-line (stream seq start end)
  "Write a sequence with line buffering."
  (with-slots (filter-function output-buffer) stream
    (let (pos (i start) (len (or end (length seq))) result)
    (loop
;       :with pos
;       :and i = start
;       :and len = (or end (length seq))
;       :and result
       :do (setf pos (position #\newline (subseq seq i len)))
       :while (< i len) :do
       (if pos
	   (progn
	     (stretchy-append output-buffer (subseq seq i (+ i pos 1)))
	     (setf result (funcall filter-function stream output-buffer))
	     (incf i (1+ pos))
	     (write-sequence result (wrapped-stream stream))
	     (stretchy-truncate output-buffer 0)
	     (adjust-column-for-seq stream result)
	     (incf (stream-position stream) (length result)))
	   (progn
	     (stretchy-append output-buffer (subseq seq i len))
	     (incf i (- len i))))))))

(defun filter-write-seq-buf (stream seq start end)
  "Write a sequence with arbitrary buffering."
  (with-slots (filter-function output-buffer buffer-size) stream
    (loop
       :with i = start			; index into seq
       :and len = (or end (length seq))
       :and put-len
       :and result
       :while (< i len) :do
       ;; when buffer not full, fill some from seq to buffer
       (when (< (length output-buffer) buffer-size)
	 (setf put-len (max (- buffer-size (length output-buffer)) (- len i)))
	 (stretchy-append output-buffer (subseq seq i put-len)))
       ;; when full, output and clear
       (when (= (length output-buffer) buffer-size)
	 (setf result (funcall filter-function stream output-buffer))
	 (incf i (length output-buffer))
	 (write-sequence result (wrapped-stream stream))
	 (stretchy-truncate output-buffer 0)
	 (adjust-column-for-seq stream result)
	 (incf (stream-position stream) (length result))))))

(defmethod stream-write-sequence ((stream character-filter-output-stream)
				  seq start end
				  &key &allow-other-keys)
  (ecase (filter-stream-unit stream)
    (:single (filter-write-seq-single stream seq start end))
    (:line   (filter-write-seq-line   stream seq start end))
    (:buffer (filter-write-seq-buf    stream seq start end))
    (:any    (filter-write-seq        stream seq start end))))

;; XXX This isn't right. We should buffer, filter and use write-byte
;; (defmethod stream-write-byte ((stream filter-output-stream) byte)
;;   (stream-write-char stream (code-char byte))
;;   byte)

;; (defmethod stream-advance-to-column ((stream filter-output-stream) column)
;;   (stream-advance-to-column (wrapped-stream stream) column))

;; (defmethod stream-fresh-line ((stream filter-stream))
;;   (stream-fresh-line (wrapped-stream stream)))

(defmethod stream-line-column ((stream character-filter-stream))
  (stream-column stream))

;; Return the stream line length or nil.
;; #+sbcl (defmethod sb-gray:stream-line-length ((stream filter-stream))
;;   (stream-column stream))

;; (defmethod stream-start-line-p ((stream filter-stream))
;;   (= (stream-column stream) 0))

;; (defmethod stream-terpri ((stream filter-stream))
;;   (stream-write-char stream #\newline))

(defmethod stream-write-char ((stream character-filter-output-stream) character)
  (ecase (filter-stream-unit stream)
    ((:single :any)
     (let ((c (funcall (filter-stream-filter-function stream)
		       stream character)))
       (when (characterp c)
;	 (stream-write-char (wrapped-stream stream) c)
	 (write-char c (wrapped-stream stream))
	 (incf (stream-position stream))
	 (adjust-column-for-element stream c))))
    (:line
     (with-slots (filter-function output-buffer) stream
       (if (char= character #\newline)
	   (progn
	     (stretchy-append output-buffer character)
	     (let ((result (funcall filter-function stream output-buffer)))
	       (write-sequence result (wrapped-stream stream))
	       (stretchy-truncate output-buffer 0)
	       (adjust-column-for-seq stream result)
	       (incf (stream-position stream) (length result))))
	   (progn
	     (stretchy-append output-buffer character)))))
    (:buffer
     (with-slots (filter-function output-buffer buffer-size) stream
       (stretchy-append output-buffer character)
       (when (= (length output-buffer) buffer-size)
	 (let ((result (funcall filter-function stream output-buffer)))
	   (write-sequence result (wrapped-stream stream))
	   (stretchy-truncate output-buffer 0)
	   (adjust-column-for-seq stream result)
	   (incf (stream-position stream) (length result)))))))
  character)

(defmethod stream-write-string ((stream character-filter-output-stream) string
				&optional start end)
  (stream-write-sequence stream string start end))

;; binary

;; @@@ this actually could be merged with stream-write-char
(defmethod stream-write-byte ((stream binary-filter-output-stream) byte)
  (ecase (filter-stream-unit stream)
    ((:single :any)
     (let ((result (funcall (filter-stream-filter-function stream)
			    stream byte)))
       (when (characterp result)
;;	 (stream-write-char (wrapped-stream stream) c)
;;	 (write-char c (wrapped-stream stream))
	 (filter-write-element stream result)
	 (incf (stream-position stream))
	 (adjust-column-for-element stream result))))
    ;; (:line
    ;;  (with-slots (filter-function output-buffer) stream
    ;;    (if (char= character #\newline)
    ;; 	   (progn
    ;; 	     (stretchy-append output-buffer character)
    ;; 	     (let ((result (funcall filter-function stream output-buffer)))
    ;; 	       (write-sequence result (wrapped-stream stream))
    ;; 	       (stretchy-truncate output-buffer 0)
    ;; 	       (adjust-column-for-seq stream result)
    ;; 	       (incf (stream-position stream) (length result))))
    ;; 	   (progn
    ;; 	     (stretchy-append output-buffer character)))))
    (:buffer
     (with-slots (filter-function output-buffer buffer-size) stream
       (stretchy-append output-buffer byte)
       (when (= (length output-buffer) buffer-size)
	 (let ((result (funcall filter-function stream output-buffer)))
	   (filter-write-seq stream result nil nil)
	   (stretchy-truncate output-buffer 0)
	   (adjust-column-for-seq stream result)
	   (incf (stream-position stream) (length result)))))))
  byte)

(defmethod stream-write-sequence ((stream binary-filter-output-stream)
				  seq start end
				  &key &allow-other-keys)
  (let* ((new-seq (funcall (filter-stream-filter-function stream) stream
			   (cond
			     ((and start end)
			      (subseq seq start end))
			     ((and start)
			      (subseq seq start))
			     (t seq))))
	 (len (length new-seq)))
    (write-sequence new-seq (wrapped-stream stream))
    (incf (stream-position stream) len)
    (adjust-column-for-seq stream new-seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O

(defclass filter-io-stream (filter-input-stream filter-output-stream)
  ()
  (:documentation "A filtered stream for input and output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions

(defun make-filter-stream (stream filter-function
			   &key
			     (direction :output)
			     unit buffer-size state)
  "Return a ‘filter-stream’ filtering ‘stream’ with ‘filter-function’.
Arguments are:
  FILTER-FUNCTION  A function that performs the filtering. It should accept a
                   unit, and return a filtered unit.
  DIRECTION        One of: :INPUT :OUTPUT :IO as in ‘open’.
  UNIT             Unit given to the filter function, which should be one of:
                   :SINGLE, :LINE, :BUFFER, or :ANY. If ‘unit’ is :BUFFER,
                   then ‘buffer-size’ is used.
  BUFFER-SIZE      The size, in characters, of the buffer.
  STATE            A place for you to store something for filter functions to
                   use.
"
  (let* ((binary (subtypep (stream-element-type stream) 'integer))
	 (class (ecase direction
		  (:input
		   (if binary
		       'binary-filter-input-stream
		       'character-filter-input-stream))
		  (:output
		   (if binary
		       'binary-filter-output-stream
		       'character-filter-output-stream))
		  (:io
		   (if binary
		       'binary-filter-io-stream
		       'character-filter-io-stream)))))
    (when (or (not stream) (not (streamp stream)))
      (error "First argument STREAM must be a stream."))
    (when (not (or (symbolp filter-function) (functionp filter-function)))
      (error "Second argument FILTER-FUNCTION must be a function or a symbol ~
              designating a function."))
    (make-instance class
		   :wrapped-stream stream
		   :filter-function filter-function
		   :unit unit
		   :buffer-size buffer-size
		   :state state)))

;; These are provided for convenient use from the REPL.

(defun filter< (file-or-stream function)
  "Return an input stream filtered by function."
  (if (streamp file-or-stream)
      ;; (cond
      ;; 	((subtypep (stream-element-type file-or-stream) 'integer)
      ;; 	 (make-binary-filter-stream file-or-stream function))
      ;; 	((subtypep (stream-element-type file-or-stream) 'character)
      ;; 	 (make-character-filter-stream file-or-stream function))
      ;; 	(t
      ;; 	 (error "I don't know how to filter a stream with an element-type of ~a"
      ;; 		(stream-element-type file-or-stream))))
      (make-filter-stream file-or-stream function)
      (with-open-file (stream file-or-stream)
	(make-filter-stream stream function))))

(defun filter> (file-or-stream function)
  "Return an output stream filtered by function."
  (if (streamp file-or-stream)
      ;; (cond
      ;; 	((subtypep (stream-element-type file-or-stream) 'integer)
      ;; 	 (make-binary-filter-stream file-or-stream function
      ;; 				    :direction :output))
      ;; 	((subtypep (stream-element-type file-or-stream) 'character)
      ;; 	 (make-character-filter-stream file-or-stream function
      ;; 				       :direction :output))
      ;; 	(t
      ;; 	 (error "I don't know how to filter a stream with an element-type of ~a"
      ;; 		(stream-element-type file-or-stream))))
      (make-filter-stream file-or-stream function)
      (with-open-file (stream file-or-stream :direction :output)
	(make-filter-stream stream function :direction :output))))

;; EOF
