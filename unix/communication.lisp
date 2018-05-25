;;
;; unix/communication.lisp - unix interface to communication between processes
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC

;; pipes

(defcfun ("pipe" real-pipe) :int (pipefd :pointer))

(defun posix-pipe ()
  (with-foreign-object (fd :int 2)
    (syscall (real-pipe fd))
    (values (mem-aref fd :int 0) (mem-aref fd :int 1))))

;; splice, vmsplice, tee

(defconstant +SPLICE_F_MOVE+		1
  "Attempt to move pages instead of copying.")
(defconstant +SPLICE_F_NONBLOCK+	2
  "Do not block on I/O.")
(defconstant +SPLICE_F_MORE+		4
  "More data will be coming in a subsequent splace.")
(defconstant +SPLICE_F_GIFT+		8
  "User pages are a gift to the kernel.")

(defcstruct iovec
  "Because C don't know, bro."
  (iov_base :pointer)
  (iov_len size-t))

#+linux
(progn
  (defctype loff-t :unsigned-long-long)

  (defcfun ("splice" linux-splice) ssize-t
    "Move data from FD-IN to FD-OUT, hopefully without excess copying, where one
of the descriptors is a pipe."
    (fd-in :int)  (offset-in loff-t)
    (fd-out :int) (offset-out loff-t) (length size-t) (flags :unsigned-int))

  (defcfun ("vmsplice" linux-vmsplice) ssize-t
    "Map memory from IOV to a pipe."
    (fd :int) (iov (:pointer (:struct iovec)))
    (number-of-segments :unsigned-long) (flags :unsigned-int))

  (defcfun ("tee" linux-tee) ssize-t
    "Duplicate LENGTH bytes from FD-IN to FD-OUT. Don't consume the data."
    (fd-in :int) (fd-out :int) (length size-t) (flags :unsigned-int)))

#|
(defun fork-with-pipes (cmd args &key in-stream (out-stream :stream)
				   (environment nil env-p))
  (let (in-stream-write-side in-stream-read-side
        out-stream-write-side out-stream-read-side)
    (if (and in-stream (streamp in-stream))
	(progn
	  (setf (values (in-stream-read-side in-stream-write-side) (posix-pipe)))
	  ;; return a stream of the write side
	  (set-stream-fd in-stream write-side)
	  ;; make the read side be standard input
	  (dup2 in-stream-read-side 0)
	  )
	  (apply #'fork-and-exec
	       `(,cmd ,args
		      ,@(when env-p :env environment))))
|#

;; @@@@ Resolve vs. opsys.lisp!
;; @@@ add environment on other than sbcl
#|
(defun pipe-program (cmd args &key in-stream (out-stream :stream)
				(environment nil env-p))
  "Return an input stream with the output of the system command. Use IN-STREAM
as an input stream, if it's supplied. If it's supplied, use OUT-STREAM as the
output stream. OUT-STREAM can be T to use *standard-output*.
ENVIRONMENT is a list of strings of the form NAME=VALUE to be used as the
process's environment. If ENVIRONMENT is not provided, it defaults to the
current process's environment."
  #+clisp (if in-stream
	      (multiple-value-bind (io i o)
		  (ext:run-shell-command
		   (format nil "~a~{ ~a~}" cmd args) :output out-stream
		   :input :stream :wait nil)
		(declare (ignore io i))
		(alexandria:copy-stream in-stream o)) ; !!!
	      (ext:run-shell-command
	       (format nil "~a~{ ~a~}" cmd args) :output out-stream))
  #+sbcl (sb-ext:process-output
;; @@@ What should we do? Added what version?
;;	      :external-format '(:utf-8 :replacement #\?)
	  (apply #'sb-ext:run-program
		 `(,cmd ,args :output ,out-stream :search t :wait nil
			,@(when in-stream `(:input ,in-stream))
			,@(when env-p
				`(:environment
				  ,(environ-to-string-list environment))))))
  #+cmu (ext:process-output
	 (if in-stream
	     (ext:run-program cmd args :output out-stream :input in-stream)
	     (ext:run-program cmd args :output out-stream)))
#|  #+openmcl (ccl::external-process-output-stream
	     (if in-stream
		 (ccl::run-program cmd args :output out-stream
				   :input in-stream :wait nil)
		 (ccl::run-program cmd args :output out-stream
				   :wait nil))) |#
  #+(or openmcl ccl)
  (let ((proc (apply #'ccl::run-program
		     `(,cmd ,args :wait nil :input t
			    ,@(when out-stream `(:output ,out-stream))
			    ,@(when in-stream `(:input ,in-stream))
			    ,@(when env-p
				    `(:env
				      ,(environ-to-string-list environment)))))))
    (ccl::external-process-output-stream proc))
  
  #+ecl (multiple-value-bind (result ret-code proc)
	    (apply #'ext::run-program
		   `(,cmd ,args :wait nil :input t
			  ,@(if out-stream
				`(:output ,out-stream)
				'(:output t))
			  ,@(if in-stream
				`(:input ,in-stream)
				'(:input t))
			  ,@(when env-p
				  `(:env
				    ,(environ-to-string-list environment)))))
	  (declare (ignore result ret-code))
	  (ext:external-process-output proc))

  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args)
				 :output out-stream :wait t)
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (declare (ignore result err-str pid))
		  (system:run-shell-command
		   (concatenate 'vector (list cmd) args)
		   :output out-stream
		   #| :wait t |#)
		  str)
  ;; XXX @@@ This is very bogus! (for what it ignores)
  #+abcl (declare (ignore in-stream out-stream environment env-p))
  #+abcl (sys:process-output (sys:run-program cmd args))
  #-(or clisp sbcl cmu openmcl ecl excl lispworks abcl)
  (missing-implementation 'popen))

;; @@@@ Resolve vs. opsys.lisp!
(defmacro with-process-output ((var cmd args) &body body)
  "Evaluate the body with the variable VAR bound to a stream with the output
from the system command CMD with the arguments ARGS."
  `(let (,var)
    (unwind-protect
	 (progn
	   (setf ,var (popen ,cmd ,args))
	   ,@body)
      (if ,var (close ,var)))))
|#

;; Sockets!
;; How about use usocket?

#|

;; protocol families
(defconstant PF_LOCAL #+darwin )
(defconstant PF_UNIX )
(defconstant PF_INET )
(defconstant PF_ROUTE )
(defconstant PF_KEY )
(defconstant PF_INET6 )
(defconstant PF_SYSTEM )
(defconstant PF_NDRV )

;; socket types
(defconstant SOCK_STREAM "Reliable two-way connection based byte streams.")
(defconstant SOCK_DGRAM "Connectionless unreliable")
(defconstant SOCK_RAW )
(defconstant SOCK_SEQPACKET )
(defconstant SOCK_RDM )

(defcfun socket :int (domain :int) (type :int) (protocol :int))
;; accept
;; bind
;; connect
;; getsockname
;; getsockopt
;; listen
;; send
;; shutdown
;; socketpair
;; getprotoent
;; 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; semaphores: semsys/semctl/semget/semop/semconfig
;; messages?: msgsys/msctl/semget/semop/semconfig
;; shared mem: shmsys/shmat/shmctl/shmdt/shmget
;;
;; POSIX Realtime Extension?
;;
;; shm_open.. named semaphores
;; sem_open...

;; End
