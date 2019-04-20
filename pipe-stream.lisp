;;
;; pipe-stream.lisp - The unnecessary Pipe streams of Ill regard.
;;

;; Do I really need a Pipe stream? You can do it with two STRING-STREAMs, but
;; it's not really convenient. This should be easier to use, and hopefully will
;; have decent performance. Much of the important code is in the PIPE-BUFFER.
;; See the end of the file for a diagram.

(defpackage :pipe-stream
  (:documentation "Streams that connects input to output,
                   via an indefinitely expanding buffer.")
  (:use :cl :trivial-gray-streams :pipe-buffer)
  (:export
   #:pipe-stream
   #:make-pipe-stream
   ))
(in-package :pipe-stream)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defclass pipe-stream (trivial-gray-stream-mixin
		       fundamental-binary-input-stream
		       fundamental-binary-output-stream
		       fundamental-character-input-stream
		       fundamental-character-output-stream)
  ((buffer
    :initarg :buffer
    :accessor pipe-stream-buffer
    :documentation "The pipe buffer.")
   (column
    :initarg :column
    :accessor stream-column
    :initform 0
    :documentation "Current column of the output."))
  (:documentation
   "A stream that presents things output to it, as input."))

;; (defmethod stream-file-position ((stream pipe-stream) &optional position-spec)
;;    "Returns or changes the current position within stream."
;;    (if position-spec
;;        nil
;;        (stream-position stream)))

(defmethod initialize-instance :after ((o pipe-stream) &rest initargs
				       &key &allow-other-keys)
  "Initialize a pipe-stream."
  (declare (ignore initargs))
  (setf (slot-value o 'buffer) (make-pipe-buffer)))

(defmethod stream-clear-input ((stream pipe-stream))
  ""
  ;; @@@ maybe just make a fresh one?
  ;; (setf (slot-value o 'buffer) (make-pipe-buffer)))
  )

(defmethod stream-read-sequence ((stream pipe-tream)
				 seq &optional (start 0) end)
  (pipe-buffer:read-data (pipe-stream-buffer stream) seq start end))

(defmethod stream-read-byte ((stream pipe-input-stream))
  ;; @@@ we need a fast path, this is dumb
  (let ((seq (make-array '(1) :element-type (stream-element-type stream))))
    (pipe-buffer:read-data (pipe-stream-buffer stream) seq start end)
    (aref seq 0)))

(defmethod stream-peek-char ((stream pipe-stream))
  (with-slots (buffer) stream
    ;; @@@ This should probably be in the pipe-buffer code.
    (with-slots (length read-point chunks-head) buffer
      (when (> length 0)
	(let ((chunk (car chunks-head)))
	  (aref (buffer-chunk-data chunk)
		(- read-point (buffer-chunk-start chunk))))))))

(defmethod stream-read-char-no-hang ((stream pipe-input-stream))
  (with-slots (buffer) stream
    (with-slots (length read-point chunks-head) buffer
      (when (> length 0)
	(stream-read-char stream)))))

(defmethod stream-read-char ((stream pipe-stream))
  ;; @@@ we need a fast path, this is dumb
  (let ((seq (make-array '(1) :element-type (stream-element-type stream))))
    (pipe-buffer:read-data (pipe-stream-buffer stream) seq start end)
    (aref seq 0)))

(defmethod stream-read-line ((stream pipe-stream))
  (with-slots (buffer) stream
    ;; @@@ This should probably be in the pipe-buffer code.
    (with-slots (length read-point chunks-head) buffer
      (when (> length 0)
	(with-output-to-string (str)
	  (loop :with c
	     :while (and (setf c (stream-read-char stream))
			 (char/= c #\newline))
	     :do (princ c str)))))))

(defmethod stream-listen ((stream pipe-stream))
  (> (pipe-buffer-length (pipe-stream-buffer stream)) 0))

#|
(defmethod stream-unread-char ((stream pipe-input-stream) character)
  )
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output

;; We will take the default methods for these, which do nothing.

;; (defmethod stream-clear-output ((stream pipe-output-stream))
;;   (declare (ignore stream)))

(defmethod stream-finish-output ((stream pipe-stream))
  (declare (ignore stream)))

(defmethod stream-force-output ((stream pipe-output-stream))
  (declare (ignore stream))

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

(defun pipe-write-seq (stream seq start end)
  "Write a sequence without buffering."
  ;; Just write what we got.
  (let* ((new-seq (funcall (pipe-stream-filter-function stream) stream
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

(defun pipe-write-seq-char (stream seq start end)
  "Write a sequence a character at a time."
  (let* ((len (length seq))
	 (my-start (or start 0))
	 (my-end (or end len))
	 result)
    (loop :with c :for i :from my-start :below my-end :do
       (setf c (elt seq i)
	     result (funcall (pipe-stream-filter-function stream)
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

(defun pipe-write-seq-line (stream seq start end)
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

(defun pipe-write-seq-buf (stream seq start end)
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

(defmethod stream-write-sequence ((stream pipe-stream) seq start end
				  &key &allow-other-keys)
  (pipe-buffer:write-data (pipe-stream-buffer stream) seq start end))

(defmethod stream-write-byte ((stream pipe-stream) byte)
  ;; @@@ fast path
  (let ((seq (make-array '(1) :element-type (stream-element-type stream))))
    (pipe-buffer:write-data (pipe-stream-buffer stream) seq)
    byte))

;; (defmethod stream-advance-to-column ((stream pipe-output-stream) column)
;;   (stream-advance-to-column (source-stream stream) column))

;; (defmethod stream-fresh-line ((stream pipe-stream))
;;   (stream-fresh-line (source-stream stream)))

(defmethod stream-line-column ((stream pipe-stream))
  (stream-column stream))

;; Return the stream line length or nil.
;; #+sbcl (defmethod sb-gray:stream-line-length ((stream pipe-stream))
;;   (stream-column stream))

;; (defmethod stream-start-line-p ((stream pipe-stream))
;;   (= (stream-column stream) 0))

;; (defmethod stream-terpri ((stream pipe-stream))
;;   (stream-write-char stream #\newline))

(defmethod stream-write-char ((stream pipe-stream) character)
  (let ((seq (make-array '(1) :element-type (stream-element-type stream))))
    (pipe-buffer:write-data (pipe-stream-buffer stream) seq)
    (incf (stream-position stream))
    (adjust-column-for-char stream character))
  character)

(defmethod stream-write-string ((stream pipe-output-stream) string
				&optional start end)
  (pipe-buffer:write-data (pipe-stream buffer stream) string start end)
  (adjust-column-for-seq stream string)
  (incf (stream-position stream) (length string)))

#|
Here's a diagram of Lisp stream types, and the one's I've added so far.

                      /--1->
              _______/_                           _________
              |     / |                    --1-->---\     |
       --1-->---->o<-----1->                      |  >o   |     
              |     \ |                    <-1--<---/     |
              |______\|                           |_______|
                      \--1->

             Broadcast Stream                    Pipe Stream



                               /--1-<
                       _______/_                    _________
                       |     / |                    |       |
   <---1---<---2--<---3--<-o<-----2-<        --1-->---->F-->--->-?-->
                       |     \ |                    |   |   |
                       |______\|                    |___|___|
                               \--3-<                   |
                                                        *
             Concatentated Stream              Output Filter Stream
                                               (reverse for Input Filter) 

                      /--<--1-<                             /--<--1-<
              _______/_                             _______/_  |
              |     / |                             |     / |  V
     2->-<-1-<->-<o<  |                    2->-<-1-<->-<o<  |  1
              |     \ |                             |     \ |  V
              |______\|                             |______\|  |
                      \--->-2-->                            \--+->-2->-1->


             Two Way Stream                        Echo Stream



       Key:  > = data writen
             < = data read
        number = consistent data
|#

;; EOF
