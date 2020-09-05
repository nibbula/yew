;;;
;;; unix-stream.lisp - Unix file descriptor streams.
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

(defclass unix-stream (os-stream)
  ()
  (:documentation "A generic file descriptor stream."))

;; (defmethod close (stream &key abort)
;;   (declare (ignore abort))
;;   (posix-close (os-stream-handle stream)))

(defmethod stream-file-position ((stream os-stream) &optional position-spec)
  (declare (ignore position-spec)) ;; @@@
  (posix-lseek (os-stream-handle stream) 0 0)) ;; @@@@

(defclass unix-input-stream (os-input-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defclass unix-output-stream (os-output-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defclass unix-io-stream (unix-input-stream unix-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

(defun stream-handle-direction (handle)
  "Return a direction for an stream handle, or NIL if there isn't one."
  (let ((flags (get-file-descriptor-flags handle)))
    (cond
      ((member '+O_RDONLY+ flags) :input)
      ((member '+O_WRONLY+ flags) :output)
      ((member '+O_RDWR+   flags) :io))))

(defparameter *typical-mode* #o664
  "Something for nothing.")

(defun %open (filename direction &key create truncate #|append |# share)
  "Open a FILENAME in DIRECTION, which is one of the usual: :input :output :io.
Create and truncate do the Unix things."
  (dbugf :fu "%open ~s ~s :create ~s :truncate ~a :share ~s~%"
	 filename direction create truncate share)
  (let ((flags (logior (if (not share) +O_CLOEXEC+ 0)
		       (ecase direction
			 ((:input :probe) +O_RDONLY+)
			 (:output         +O_WRONLY+)
			 (:io             +O_RDWR+))
		       (if create +O_CREAT+ 0)
		       (if truncate +O_TRUNC+ 0)
		       #| (if append +O_APPEND+ 0) |#
		       ))
	(mode *typical-mode*)
	(interrupt-count 0)
	fd)
    (dbugf :fu "posix-open ~s ~x ~x~%" filename flags mode)
    (loop :while (and (< (setf fd (posix-open filename flags mode)) 0)
		      (= (errno) +EINTR+)) ; plusering bucket wallop
       :do (if (< interrupt-count 10)
	       (incf interrupt-count)
	       (error 'opsys-error
		      :format-control
		      "Got a bunch of interrupts when opening ~s."
		      :format-arguments `(,filename))))
    ;; (when (< fd 0)
    ;;   (error 'posix-error :error-code (errno)
    ;; 	     :format-control "open ~s" :format-arguments `(,filename)))
    fd))

;; I know this quote from the spec lets everyone off the hook for doing a
;; half-assed job, and for good and practical reasons, but I still want to
;; minimize how much we "deviate slightly".
;;
;;  “The various file systems in existence today have widely differing
;;   capabilities, and some aspects of the file system are beyond the scope of
;;   this specification to define.  A given implementation might not be able to
;;   support all of these options in exactly the manner stated.  An
;;   implementation is required to recognize all of these option keywords and
;;   to try to do something "reasonable" in the context of the host file
;;   system.  Where necessary to accomodate the file system, an implementation
;;   deviate slightly from the semantics specified here without being
;;   disqualified for consideration as a conforming implementation.  If it is
;;   utterly impossible for an implementation to handle some option in a manner
;;   similar to what is specified here, it may simply signal an error.”
;;
;; But of course I'll probably end up doing a partially-assed job too, since
;; the whole versions in file names thing worked well in Lisp Machines and
;; VMS, but seems like it would be a lot of complexity that would go unused in
;; the modern day, because everyone just uses a more complex version control
;; system where they care about it, and more efficiently, nothing, where they
;; don't. But for the case of a person using a general purpose computer, and
;; creating user managed files, it could be a great boon to have built-in
;; automatic version control. It might be an interesting experiment to try a
;; specifically versioned stream which could hook into a version control
;; system. But it's a bit weird since :supersede might mean an automatic
;; commit.
;;
;; We're in a weird fractal frond of Greenspun/Morris land.

(defun lispy-to-posix-open (filename direction if-exists if-does-not-exist share)
  "Try to do something like Common Lisp `open' on POSIX."
  ;; @@@ This is crap. And actually Unix is crap. Thanks for playing.
  ;; Consider that even VMS, TOPS-20, Multics could do most of this shit in the
  ;; 1970's. Maybe if 'git' wasn't crap at handling BLOBs, we could just do
  ;; "git add" or something.
  (let (fd create truncate append unlink old-fd)
    (cond
      ((and (member direction '(:output :io))
	    (file-exists filename)) ;; @@@ maybe should be an open?
	;; @@@ What about if "filename" is a not a normal file?
	(ecase if-exists
	  (:error
	   ;; (error 'file-error "The file ~s already exists." filename
	   ;; 	  :pathname filename)
	   (error 'os-file-error
		  :format-control "The file ~s already exists."
		  :format-arguments `(,filename)
		  :pathname filename)
	   )
	  ((:new-version :rename-and-delete)
	   ;; To do :new-version correctly, we would have duplicate whatever
	   ;; decisions the implemetation made for versions, which seems
	   ;; unlikely at best.
	   ;;
	   ;; For :rename-and-delete and delete, we would have to implement
	   ;; "deleted but not expunged" which seems hard to do cleanly in POSIX
	   ;; and then also, we should follow what the implementation does but
	   ;; I don't think I can feel good about doing this badly.
	   (error 'os-file-error
		  :format-control
		  "The file ~s already exists, and I'm very sorry, but we ~
		   haven't implemented :if-exists ~s yet."
		  :format-arguments `(,filename ,if-exists)))
	  (:rename
	   ;; @@@ This one we can do, but I'm putting it off.
	   (error 'os-file-error
		  :format-control
		  "The file ~s already exists, and I'm very sorry, but we ~
		   haven't implemented :if-exists ~s yet."
		  :format-arguments `(,filename ,if-exists)))
	  (:overwrite
	   #| Nothing. This is the totally "normal" open. Of course it is. |#
	   )
	  (:append
	   ;; Here again, O_APPEND isn't exactly what it says in the spec.
	   ;; The difference is the spec says we seek to the end ONCE after
	   ;; opening, whereas O_APPEND seeks to the end before each write,
	   ;; which is subtly different. And of course it fails with NFS
	   ;; anyway.
	   (setf append t))
	  (:supersede
	   ;; So yeah, we could unlink the old file, and leave it open, but how
	   ;; are we going to ensure that it's closed when/if the new file is
	   ;; closed? We would have to have finalizer support from the
	   ;; implementation. Also does that matter since there's no prescribed
	   ;; way for the user to make it accesible again if something goes
	   ;; wrong? We maybe could make it reappear with "linkat" in linux,
	   ;; but when & how & who cares?
	   ;;
	   ;; We could do O_TRUNC, which is what sbcl does, but that could
	   ;; cause problems for things that already had it open, and isn't
	   ;; really what the spec says, so I don't think we should do
	   ;; that. So we just straight up unlink it.  Bam!
	   (setf unlink t))
	  ((nil) #| Oh hai |# (return-from lispy-to-posix-open nil))))
      ((and (or (eq direction :input) (null direction))
	    (not (file-exists filename)))
	;;; Not pre-existing:
	(ecase if-does-not-exist
	  (:error
	   ;; (error 'file-error "The file ~s doesn't exist." filename
	   ;; 	  :pathname filename)
	   (error 'os-file-error
		  :format-control "The file ~s doesn't exist."
		  :format-arguments `(,filename)
		  :pathname filename)
	   )
	  (:create
	   (setf create t))
	  ((nil) (return-from lispy-to-posix-open nil))))
      ((not (member direction '(:input :output :io :probe)))
       (error 'os-file-error
		:format-control "Bad :direction ~s for opening ~s."
		:format-arguments `(,direction ,filename)
		:pathname filename)))
    (unwind-protect
	 (progn
	   (when unlink
	     ;; @@@ Of course there's a race condition if it's changed since
	     ;; we did the file-exists. I guess we could use open(2) instead
	     ;; of file-exists, but it still could have been unlinked already,
	     ;; although maybe that's okay and/or detectable?!
	     (setf old-fd (posix-open filename +O_CLOEXEC+ *typical-mode*))
	     (when (minusp old-fd)
	       (error "I'm very sorry, but you probably hit the race condition ~
                       with :supersede for ~s." filename))
	     (syscall (posix-unlink filename)))
	   (setf fd (%open filename direction
			   :create create
			   :truncate truncate
			   #| :append append |#
			   :share share))
	   (when (< fd 0)
	     (when old-fd
	       #| @@@ recreate old-fd !! |#)
	     (error 'posix-error :error-code (errno)
		    :format-control "open ~s" :format-arguments `(,filename))))
      (when (and (eq direction :probe) fd)
	(syscall (posix-close fd)))
      (when (and unlink old-fd)
	(syscall (posix-close old-fd))))
    (when append
      (syscall (posix-lseek fd 0 +SEEK-END+))
      ;; Of course something else could write more data after here, but
      ;; before it gets back to the calling code! Horrible.
      )
    fd))

;; @@@ Maybe if I have nothing better to do, I could unify flush & fill, since
;; they're nearly the same.

(defun %fill-buffer (stream)
  "Fill the input buffer. Return the size we read. Return NIL on EOF, which
only happens the second time we get a zero read. Throw errors if we get 'em."
  (with-accessors ((handle os-stream-handle)
		   (input-buffer os-stream-input-buffer)
		   (position os-stream-position)
		   (input-fill os-stream-input-fill)) stream
    (let ((status 0)
	  (remaining (- +input-buffer-size+ input-fill))
	  (start-fill input-fill)
	  problem result)
      ;; Got to the beginning of the buffer, if we used it up.
      (when (zerop remaining)
	(when (/= position input-fill) #|(zerop position)|#
	  (cerror "Whatever"
		  "Fill when not consumed. This isn't supposed to happen."))
	(setf input-fill 0
	      position 0
	      remaining +input-buffer-size+))
      ;; This loop is a little wacky because I don't want to throw errors
      ;; inside the with-pointer-to-vector-data, which could turn off GC.
      ;; Instead we set a problem flag and signal afterward. We need to keep
      ;; the loop in there because we have to increment the pointer on partial
      ;; reads.
      (cffi:with-pointer-to-vector-data (buf input-buffer)
	(incf-pointer buf input-fill)
	(loop
	   :do
	   (dbugf :fu "posix-read ~s ~s ~s~%" handle buf remaining)
	   (setf status (posix-read handle buf remaining))
	   (dbugf :fu "posix-read -> ~s~%" status)
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
		     ;; (setf result (- +input-buffer-size+ remaining)))
		     (setf result (- start-fill input-fill)))
		    nil
		    result)))
	     (t
	      ;; Eat some of the buffer and go again.
	      (decf remaining status)
	      (incf-pointer buf status)
	      (incf input-fill status)))
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
      ;; (- +input-buffer-size+ remaining)
      (- start-fill input-fill)
      )))

(defun %flush-buffer (stream &key force)
  "Flush the input buffer. Throw errors if we get 'em."
  (with-accessors ((handle os-stream-handle)
		   (output-buffer os-stream-output-buffer)
		   (ouput-position os-stream-ouput-position))
      stream
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

(defclass unix-character-input-stream (os-character-input-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defmethod os-stream-open ((stream unix-character-input-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :input if-exists if-does-not-exist
			     share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-character-input-stream)))
  'unix-character-input-stream)

(defmethod fill-buffer ((stream unix-character-input-stream))
  (%fill-buffer stream))

#|
(defmethod stream-read-sequence ((stream unix-character-input-stream)
				 seq &optional start end)
  (declare (ignore start end)) ;; @@@
  ;; only if we created a unix-stream explicitly or are un-buffered?
  (with-slots (handle input-buffer position) stream
    ;; @@@ If seq is the right type we can read directly?
    ;; Or if not we have to convert encoding.
    (cffi:with-pointer-to-vector-data (buf input-buffer)
      (syscall (posix-read handle buf +input-buffer-size+)))))
|#

(defclass unix-character-output-stream (os-character-output-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defmethod os-stream-open ((stream unix-character-output-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :output if-exists if-does-not-exist share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-character-output-stream)))
  'unix-character-output-stream)

(defmethod flush-buffer ((stream unix-character-output-stream) &key force)
  (%flush-buffer stream :force force))

#|
(defmethod stream-write-sequence ((stream unix-character-output-stream) seq
				  &optional start end)
  (declare (ignore start end)) ;; @@@
  ;; only if we created a unix-stream explicitly or are un-buffered?
  (with-slots (handle output-buffer) stream
    (posix-write handle output-buffer 0))) ; @@@
|#

(defclass unix-character-io-stream (unix-character-input-stream
				    unix-character-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

(defmethod os-stream-open ((stream unix-character-io-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :io if-exists if-does-not-exist share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-character-io-stream)))
  'unix-character-io-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binary

(defclass unix-binary-input-stream (os-binary-input-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for input."))

(defmethod os-stream-open ((stream unix-binary-input-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :input if-exists if-does-not-exist share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-binary-input-stream)))
  'unix-binary-input-stream)

(defmethod fill-buffer ((stream unix-binary-input-stream))
  (%fill-buffer stream))

(defmethod stream-read-sequence ((stream unix-binary-input-stream)
				 seq &optional start end)
  ;; This is only if we created a unix-stream explicitly or are un-buffered?
  (with-accessors ((handle os-stream-handle)) stream
    (let* ((seq-start (clamp (or start 0)          0 (length seq)))
	   (seq-end   (clamp (or end (length seq)) 0 (length seq)))
	   (len (min (- seq-start seq-end) (- (length seq) start))))
      (cffi:with-pointer-to-vector-data (buf seq)
	(incf-pointer buf start)
	;; @@@ This doesn't handle any problems
	(syscall (posix-read handle buf len)))))
  seq)

(defclass unix-binary-output-stream (os-binary-output-stream unix-stream)
  ()
  (:documentation "A file descriptor stream for output."))

(defmethod os-stream-open ((stream unix-binary-output-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :output if-exists if-does-not-exist share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-binary-output-stream)))
  'unix-binary-output-stream)

(defmethod flush-buffer ((stream unix-binary-output-stream) &key force)
  (%flush-buffer stream :force force))

(defmethod stream-write-sequence ((stream os-output-stream) seq
				  &optional (start 0) end)
  ;; This is only if we created a unix-stream explicitly or are un-buffered?
  (with-accessors ((handle os-stream-handle)) stream
    (let* ((seq-start (clamp (or start 0)          0 (length seq)))
	   (seq-end   (clamp (or end (length seq)) 0 (length seq)))
	   (len (min (- seq-start seq-end) (- (length seq) start))))
      (cffi:with-pointer-to-vector-data (buf seq)
	(incf-pointer buf start)
	;; @@@ This doesn't handle any problems
	(syscall (posix-write handle buf len)))))
  seq)

(defclass unix-binary-io-stream (unix-binary-input-stream
				 unix-binary-output-stream)
  ()
  (:documentation "Your useful friend on the other end."))

(defmethod os-stream-open ((stream unix-binary-io-stream) filename
			   if-exists if-does-not-exist share)
  (setf (os-stream-handle stream)
	(lispy-to-posix-open filename :io if-exists if-does-not-exist share))
  stream)

(defmethod os-stream-system-type ((stream (eql 'os-binary-io-stream)))
  'unix-binary-io-stream)

;; EOF
