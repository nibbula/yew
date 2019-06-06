;;
;; ms/ms-stream.lisp - Windows handle streams.
;;

;; @@@ This hasn't really been converted from the unix one yet.

(in-package :opsys-ms)

(defclass ms-stream (os-stream)
  ()
  (:documentation "A generic Windows stream."))

;; (defmethod close (stream &key abort)
;;   (declare (ignore abort))
;;   (posix-close (os-stream-handle stream)))

(defmethod stream-file-position ((stream os-stream) &optional position-spec)
  (case position-spec
    ((nil)
     (with-foreign-object (p 'LARGE_INTEGER)
       (%set-file-pointer-ex (os-stream-handle stream) 0 p +FILE-CURRENT+)
       (mem-ref p 'LARGE_INTEGER)))
    (:start 
     (not (zerop (%set-file-pointer-ex (os-stream-handle stream) 0 (null-pointer)
				       +FILE-BEGIN+))))
    (:end
     (not (zerop (%set-file-pointer-ex (os-stream-handle stream) 0 (null-pointer)
				       +FILE-END+))))
    (otherwise
     (if (integerp position-spec)
	 (not (zerop (%set-file-pointer-ex (os-stream-handle stream) position-spec
					   (null-pointer)
					   +FILE-BEGIN+)))
       (error 'opsys-error
	      :format-control "position-spec must be an integer, not a ~s."
	      :format-arguments (type-of position-spec))))))

(defclass ms-input-stream (os-input-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defclass ms-output-stream (os-output-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defclass ms-io-stream (ms-input-stream ms-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

#|
(defun stream-handle-direction (handle)
  "Return a direction for an stream handle, or NIL if there isn't one."
  (let ((flags (get-file-descriptor-flags handle)))
    (cond
      ((member '+O_RDONLY+ flags) :input)
      ((member '+O_WRONLY+ flags) :output)
      ((member '+O_RDWR+   flags) :io))))
|#

(defun %open (filename direction &key create truncate)
  "Open a FILENAME in DIRECTION, which is one of the usual: :input :output :io.
Create and truncate do the Unix things."
  (let ((flags (logior +O_CLOEXEC+
		       (ecase direction
			 (:input  +O_RDONLY+)
			 (:output +O_WRONLY+)
			 (:io     +O_RDWR+))
		       (if create +O_CREAT+ 0)
		       (if truncate +O_TRUNC+ 0)))
	(mode #o666)
	(interrupt-count 0)
	fd)
    (loop :while (and (< (setf fd (posix-open filename flags mode)) 0)
		      (= (errno) +EINTR+)) ; plusering bucket wallop
       :do (if (< interrupt-count 10)
	       (incf interrupt-count)
	       (error 'opsys-error
		      :format-control
		      "Got a bunch of interrupts when opening ~s."
		      :format-arguments `(,filename))))
    (when (< fd 0)
      (error 'posix-error :error-code (errno)
	     :format-control "open ~s" :format-arguments `(,filename)))
    fd))

;; @@@ Maybe if I have nothing better to do, I could unify flush & fill, since
;; they're nearly the same.

(defun %fill-buffer (stream)
  "Fill the input buffer. Return the size we read. Return NIL on EOF, which
only happens the second time we get a zero read. Throw errors if we get 'em."
  (with-slots (handle input-buffer position) stream
    (let ((status 0)
	  (remaining +input-buffer-size+)
	  problem result)
      ;; This loop is a little wacky because I don't want to throw errors
      ;; inside the with-pointer-to-vector-data, which could turn off GC.
      ;; Instead we set a problem flag and signal afterward. We need to keep
      ;; the loop in there because we have to increment the pointer on partial
      ;; reads.
      (cffi:with-pointer-to-vector-data (buf input-buffer)
	(loop
	   :do
	     (setf status (posix-read handle buf remaining))
	     (cond
	       ((and (< status 0) (or (= (errno) +EINTR+)
				      (= (errno) +EAGAIN+)))
		#| try again |#)
	       ((or (> status remaining) (< status 0))
		(setf problem t))
	       ((= status 0)
		;; EOF
		;; or maybe some other bullshit?
		(return-from %fill-buffer
		  ;; Return the bytes read or nil if no bytes read.
		  (if (zerop
		       (setf result (- +input-buffer-size+ remaining)))
		      nil
		      result)))
	       (t
		;; Eat some of the buffer and go again.
		(decf remaining status)
		(incf-pointer buf status)))
	   :while (not (or (zerop remaining) problem))))
      (when problem
	(cond
	  ((> status remaining)
	   (cerror "Just pretend it didn't?"
		   "read returned too many characters? ~a" status)
	   (setf remaining 0))
	  ((< status 0)
	   (error 'posix-error :error-code (errno)
		  :format-control "%fill-buffer read"
		  :format-arguments `(,handle ,input-buffer ,remaining)))))
      (- +input-buffer-size+ remaining))))

(defun %flush-buffer (stream &key force)
  "Flush the input buffer. Throw errors if we get 'em."
  (with-slots (handle output-buffer ouput-position) stream
    (let ((status 0)
	  (remaining +output-buffer-size+)
	  problem #| result @@@ |#)
      ;; This loop is a little wacky because I don't want to throw errors
      ;; inside the with-pointer-to-vector-data, which could turn off GC.
      ;; Instead we set a problem flag and signal afterward. We need to keep
      ;; the loop in there because we have to increment the pointer on partial
      ;; reads.
      (cffi:with-pointer-to-vector-data (buf output-buffer)
	(loop
	   :do
	     (setf status (posix-write handle buf remaining))
	     (cond
	       ((and (< status 0) (or (= (errno) +EINTR+)
				      (= (errno) +EAGAIN+)))
		#| try again |#)
	       ((or (> status remaining) (< status 0))
		(setf problem t))
	       ((= status 0)
		;; Disk is full or maybe some other bullshit?
		(return-from %flush-buffer
		  ;; Return the bytes written or nil if no bytes were written.
		  (- +input-buffer-size+ remaining)))
	       (t
		;; Eat some of the buffer and go again.
		(decf remaining status)
		(incf-pointer buf status)))
	   :while (not (or (zerop remaining) problem))))
      (when problem
	(cond
	  ((> status remaining)
	   ;; This is an indication of how much I trust your OOM killing kernel.
	   (cerror "Just ignore it."
		   "write returned too many characters? ~a" status)
	   (setf remaining 0))
	  ((< status 0)
	   (error 'posix-error :error-code (errno)
		  :format-control "%flush-buffer write"
		  :format-arguments `(,handle ,output-buffer ,remaining)))))
      (when force
	(syscall (fsync handle)))
      (- +input-buffer-size+ remaining))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character

(defclass ms-character-input-stream (os-character-input-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defmethod os-stream-open ((stream ms-character-input-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :input)))

(defmethod fill-buffer ((stream ms-character-input-stream))
  (%fill-buffer stream))

#|
(defmethod stream-read-sequence ((stream ms-character-input-stream)
				 seq &optional start end)
  (declare (ignore start end)) ;; @@@
  ;; only if we created a ms-stream explicitly or are un-buffered?
  (with-slots (handle input-buffer position) stream
    ;; @@@ If seq is the right type we can read directly?
    ;; Or if not we have to convert encoding.
    (cffi:with-pointer-to-vector-data (buf input-buffer)
      (syscall (posix-read handle buf +input-buffer-size+)))))
|#

(defclass ms-character-output-stream (os-character-output-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defmethod os-stream-open ((stream ms-character-output-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :output)))

(defmethod flush-buffer ((stream ms-character-output-stream) &key force)
  (%flush-buffer stream :force force))

#|
(defmethod stream-write-sequence ((stream ms-character-output-stream) seq
				  &optional start end)
  (declare (ignore start end)) ;; @@@
  ;; only if we created a ms-stream explicitly or are un-buffered?
  (with-slots (handle output-buffer) stream
    (posix-write handle output-buffer 0))) ; @@@
|#

(defclass ms-character-io-stream (ms-character-input-stream
				    ms-character-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

(defmethod os-stream-open ((stream ms-character-io-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :io)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary

(defclass ms-binary-input-stream (os-binary-input-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defmethod os-stream-open ((stream ms-binary-input-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :input)))

(defmethod fill-buffer ((stream ms-binary-input-stream))
  (%fill-buffer stream))

(defmethod stream-read-sequence ((stream ms-binary-input-stream)
				 seq &optional start end)
  ;; This is only if we created a ms-stream explicitly or are un-buffered?
  (with-slots (handle) stream
    (let* ((seq-start (clamp (or start 0)          0 (length seq)))
	   (seq-end   (clamp (or end (length seq)) 0 (length seq)))
	   (len (min (- seq-start seq-end) (- (length seq) start))))
      (cffi:with-pointer-to-vector-data (buf seq)
	(incf-pointer buf start)
	;; @@@ This doesn't handle any problems
	(syscall (posix-read handle buf len)))))
  seq)

(defclass ms-binary-output-stream (os-binary-output-stream ms-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defmethod os-stream-open ((stream ms-binary-output-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :output)))

(defmethod flush-buffer ((stream ms-binary-output-stream) &key force)
  (%flush-buffer stream :force force))

(defmethod stream-write-sequence ((stream os-output-stream) seq
				  &optional (start 0) end)
  ;; This is only if we created a ms-stream explicitly or are un-buffered?
  (with-slots (handle) stream
    (let* ((seq-start (clamp (or start 0)          0 (length seq)))
	   (seq-end   (clamp (or end (length seq)) 0 (length seq)))
	   (len (min (- seq-start seq-end) (- (length seq) start))))
      (cffi:with-pointer-to-vector-data (buf seq)
	(incf-pointer buf start)
	;; @@@ This doesn't handle any problems
	(syscall (posix-read handle buf len)))))
  seq)

(defclass ms-binary-io-stream (ms-binary-input-stream
				 ms-binary-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

(defmethod os-stream-open ((stream ms-binary-io-stream) filename
			   if-exists if-does-not-exist share)
  (declare (ignore share)) ;; @@@
  (setf (os-stream-handle stream) (%open filename :io)))

;; EOF
