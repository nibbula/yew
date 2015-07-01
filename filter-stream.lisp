;;
;; filter-stream.lisp - Filter a stream with a function.
;;

;; $Revision: 1.2 $

(defpackage :filter-stream
  (:documentation "Streams that filter an underlying stream with a function.")
  (:use :cl :trivial-gray-streams :stretchy)
  (:export
   #:wrapped-stream
   #:source-stream
   #:filter-stream
   #:filter-stream-source-stream
   #:filter-stream-unit
   #:filter-stream-buffer-size
   #:filter-stream-filter-function
   #:filter-input-stream
   #:filter-output-stream
   #:filter-io-stream
   #:make-filter-stream
   #:filter<
   #:filter>
   ))
(in-package :filter-stream)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; Wrapped stream

(defclass wrapped-stream (trivial-gray-stream-mixin)
  ((source-stream
    :initarg :source-stream
    :accessor source-stream
    :documentation "The underlying stream."))
  (:documentation "A stream that wraps around another stream."))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (source-stream stream)))

;;;(defmethod close ((stream wrapped-stream) &key abort &allow-other-keys)
;;;(defmethod close ((stream wrapped-stream) &key (abort t) &allow-other-keys)
;;;(defmethod close ((stream wrapped-stream) &key (abort t))
(defmethod close ((stream wrapped-stream) &key abort)
  (if abort
      (progn
	(force-output stream)
	(force-output (source-stream stream)))
      (progn
	(finish-output stream)
	(finish-output (source-stream stream))))
  (values t))

;; (defmethod close ((stream wrapped-stream) &key
;;   (close (source-stream stream) :abort abort))

;; (defmethod stream-file-position (stream &optional position-spec)
;;   "Returns or changes the current position within stream."
;;   (stream-file-position (source-stream stream) position-spec))

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
    "The unit of data to be filtered at a time. UNIT should be one of
     :CHARACTER, :LINE, :BUFFER, or :ANY. If UNIT is :buffer, then buffer-size
     is used.")
   (buffer-size
    :initarg :buffer-size
    :accessor filter-stream-buffer-size
    :documentation "The size, in characters, of the buffer.")
   (position
    :initarg :position
    :accessor stream-position
    :initform 0
    :documentation "Current position in the stream.")
   (column
    :initarg :column
    :accessor stream-column
    :initform 0
    :documentation "Current position in the stream.")
   (state
    :initarg :state
    :accessor filter-stream-state
    :initform nil
    :documentation "Place for filter functions to put some state information.")
   )
  (:documentation "A stream that filters an underlying stream by applying a function."))

(defparameter *default-line-size* 128)
(defparameter *default-buffer-size* 512)

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
    :documentation "For buffering input, such as lines."))
  (:documentation "A filtered stream for input."))

(defmethod initialize-instance :after ((o filter-input-stream) &rest initargs
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

(defmethod stream-clear-input ((stream filter-input-stream))
  ""
  (stretchy-truncate (filter-input-stream-buffer stream) 0))

#|

@@@ TODO.......

(defmethod stream-read-sequence ((stream filter-input-stream)
				 seq &optional (start 0) end)
  ;; (let ((pos 0)
  ;; 	(new-seq    (stream-read-sequence (source-stream)))
  ;;   (loop :while )
  ;;   (setf (elt seq pos)
  ;; 	  )
  ;;   pos)
  )

(defmethod stream-read-byte ((stream filter-input-stream))
  )

(defmethod stream-peek-char ((stream filter-input-stream))
  (if
  (stream-peek-char (source-stream stream)))

(defmethod stream-read-char-no-hang ((stream filter-input-stream))
  (stream-read-char-no-hang (source-stream stream)))

(defmethod stream-read-char ((stream filter-input-stream))
  (case (filter-stream-unit stream)
    ((:character :any)
     (
     (let ((c (read-char (source-stream stream) nil :eof)))
       (when (eql c :eof)
	 (return-from stream-read-char :eof))
       (when (characterp c)
	 (incf (stream-position stream))
	   
     (funcall (filter-stream-filter-function
	       (stream-read-char (source-stream stream)))))
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

(defclass filter-output-stream (filter-stream
				fundamental-character-output-stream)
  ((output-buffer
    :initarg :output-buffer
    :accessor filter-output-stream-buffer
    :documentation "For buffering output, such as lines."))
  (:documentation "A filtered stream for output."))

(defmethod initialize-instance :after ((o filter-output-stream) &rest initargs
				       &key &allow-other-keys)
  "Initialize a filter-output-stream."
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

;; We will take the default methods for these, which do nothing.

;; (defmethod stream-clear-output ((stream filter-output-stream))
;;   (declare (ignore stream)))

(defmethod stream-finish-output ((stream filter-output-stream))
  (with-slots (output-buffer source-stream) stream
    (when (and output-buffer (length output-buffer))
      (write-string output-buffer source-stream)
      (finish-output source-stream))))

(defmethod stream-force-output ((stream filter-output-stream))
  (with-slots (output-buffer source-stream) stream
    (when (and output-buffer (length output-buffer))
      (write-string output-buffer source-stream)
      (force-output source-stream))))

(defun adjust-column-for-char (stream char)
  (case char
    (#\backspace (decf (stream-column stream)))
    ((#\newline #\return) (setf (stream-column stream) 0))
    (#\tab (incf (stream-column stream)
		 (- 8 (mod (stream-column stream) 8))))
    (t (incf (stream-column stream)))))

(defun adjust-column-for-seq (stream seq)
  (loop :for i :from 0 :below (length seq) :do
     (adjust-column-for-char stream (elt seq i))))

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
    (write-sequence new-seq (source-stream stream))
    (incf (stream-position stream) len)
    (adjust-column-for-seq stream new-seq)))

(defun filter-write-seq-char (stream seq start end)
  "Write a sequence a character at a time."
  (let* ((len (length seq))
	 (my-start (or start 0))
	 (my-end (or end len))
	 result)
    (loop :with c :for i :from my-start :below my-end :do
       (setf c (elt seq i)
	     result (funcall (filter-stream-filter-function stream)
			     stream c))
       (cond
	 ((characterp result)
	  (write-char result (source-stream stream))
	  (adjust-column-for-char stream result)
	  (incf (stream-position stream)))
	 ((stringp result)
	  (write-sequence result (source-stream stream))
	  (adjust-column-for-seq stream result)
	  (incf (stream-position stream) (length result)))
	 (t
	  (setf result (format nil "~a" result))
	  (write-sequence result (source-stream stream))
	  (adjust-column-for-seq stream result)
	  (incf (stream-position stream) (length result)))))))

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
	     (write-sequence result (source-stream stream))
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
	 (write-sequence result (source-stream stream))
	 (stretchy-truncate output-buffer 0)
	 (adjust-column-for-seq stream result)
	 (incf (stream-position stream) (length result))))))

(defmethod stream-write-sequence ((stream filter-output-stream) seq start end
				  &key &allow-other-keys)
  (ecase (filter-stream-unit stream)
    (:character (filter-write-seq-char stream seq start end))
    (:line      (filter-write-seq-line stream seq start end))
    (:buffer	(filter-write-seq-buf  stream seq start end))
    (:any	(filter-write-seq      stream seq start end))))

;; XXX This isn't right. We should buffer, filter and use write-byte
(defmethod stream-write-byte ((stream filter-output-stream) byte)
  (stream-write-char stream (code-char byte))
  byte)

;; (defmethod stream-advance-to-column ((stream filter-output-stream) column)
;;   (stream-advance-to-column (source-stream stream) column))

;; (defmethod stream-fresh-line ((stream filter-stream))
;;   (stream-fresh-line (source-stream stream)))

(defmethod stream-line-column ((stream filter-stream))
  (stream-column stream))

;; Return the stream line length or nil.
;; (defmethod stream-line-length ((stream filter-stream))
;;   (stream-column stream))

;; (defmethod stream-start-line-p ((stream filter-stream))
;;   (= (stream-column stream) 0))

;; (defmethod stream-terpri ((stream filter-stream))
;;   (stream-write-char stream #\newline))

(defmethod stream-write-char ((stream filter-output-stream) character)
  (ecase (filter-stream-unit stream)
    ((:character :any)
     (let ((c (funcall (filter-stream-filter-function stream)
		       stream character)))
       (when (characterp c)
;	 (stream-write-char (source-stream stream) c)
	 (write-char c (source-stream stream))
	 (incf (stream-position stream))
	 (adjust-column-for-char stream c))))
    (:line
     (with-slots (filter-function output-buffer) stream
       (if (char= character #\newline)
	   (progn
	     (stretchy-append output-buffer character)
	     (let ((result (funcall filter-function stream output-buffer)))
	       (write-sequence result (source-stream stream))
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
	   (write-sequence result (source-stream stream))
	   (stretchy-truncate output-buffer 0)
	   (adjust-column-for-seq stream result)
	   (incf (stream-position stream) (length result)))))))
  character)

(defmethod stream-write-string ((stream filter-output-stream) string
				&optional start end)
  (stream-write-sequence stream string start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O

(defclass filter-io-stream (filter-input-stream filter-output-stream)
  ()
  (:documentation "A filtered stream for input and output."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions

(defun make-filter-stream (stream filter-func
			   &key
			     (direction :output)
			     (unit :line) buffer-size state)
  (let ((class (ecase direction
		 (:input 'filter-input-stream)
		 (:output 'filter-output-stream)
		 (:io 'filter-io-stream))))
    (when (or (not stream) (not (streamp stream)))
      (error "First argument STREAM must be a stream."))
    (when (not (or (symbolp filter-func) (functionp filter-func)))
      (error "Second argument FILTER-FUNC must be a function or a symbol designating a function."))
    (make-instance class
		   :source-stream stream
		   :filter-function filter-func
		   :unit unit
		   :buffer-size buffer-size
		   :state state)))

(defun filter< (file-or-stream function)
  "Return an input stream filtered by function."
  (if (streamp file-or-stream)
      (make-filter-stream file-or-stream function)
      (with-open-file (stream file-or-stream)
	(make-filter-stream stream function))))

(defun filter> (file-or-stream function)
  "Return an output stream filtered by function."
  (if (streamp file-or-stream)
      (make-filter-stream file-or-stream function :direction :output)
      (with-open-file (stream file-or-stream :direction :output)
	(make-filter-stream stream function :direction :output))))

;; EOF
