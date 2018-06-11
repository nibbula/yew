;;
;; unix/events.lisp - Unix interface to events and polling
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select

(defconstant FD_SETSIZE 1024)
(defconstant NBBY 	8)
(defconstant NFDBITS 	(* (foreign-type-size :uint32) NBBY))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun howmany (x y) (truncate (+ x (- y 1)) y)))
(defconstant +fd-count+	(howmany FD_SETSIZE NFDBITS))

; We don't really need the struct, it's just some C modularity junk.
; (defcstruct fd_set
;   (fds_bits :int32 :count +fd-count+))  ; actually 32, given setsize of 1024

(defun fd-isset (n set)
  (logand (mem-aref set :uint32 (truncate n NFDBITS))
 	  (ash 1 (mod n NFDBITS))))

(defun fd-set (n set)
  (setf (mem-aref set :uint32 (truncate n NFDBITS))
	(logior (mem-aref set :uint32 (truncate n NFDBITS))
		(ash 1 (mod n NFDBITS)))))

(defun fd-clr (n set)
  (setf (mem-aref set :uint32 (truncate n NFDBITS))
	(logandc1 (ash 1 (mod n NFDBITS))
		  (mem-aref set :uint32 (truncate n NFDBITS)))))

(defun fd-zero (set)
  (loop :for i :from 0 :below +fd-count+
     :do (setf (mem-aref set :uint32 i) 0)))

(defun fd-copy (from-set to-set)
  (loop :for i :from 0 :below +fd-count+
     :do (setf (mem-aref to-set :uint32 i)
	       (mem-aref from-set :uint32 i))))

(defcfun ("select" unix-select) :int (nfds :int) (read-fds :pointer)
	 (write-fds :pointer) (error-fds :pointer) (timeout :pointer))

;; @@@ fix not to be lame? see below
(defun lame-select (fds timeout)
  "See if some data is available."
;   (format t "NFDBITS = ~s~%" NFDBITS)
;   (format t "howmany(FD_SETSIZE,NFDBITS) = ~s~%" (howmany FD_SETSIZE NFDBITS))
;   (format t "+fd-count+ = ~s~%" +fd-count+)
;   (format t "sizeof(fd_set) = ~s~%" (foreign-type-size 'fd_set))
  (let ((nfds 0) ret-val results)
    (with-foreign-objects ((read-fds :uint32 +fd-count+)
 			   (write-fds :uint32 +fd-count+)
 			   (err-fds :uint32 +fd-count+)
 			   (tv '(:struct foreign-timeval)))
      (fd-zero read-fds)
      (fd-zero write-fds)
      (fd-zero err-fds)
      (loop :with i = 0
	    :for f :in fds
	    :do
	    (let* ((fd-in f) (fd (first fd-in)))
	      (when (position :read (cdr fd-in))
		(fd-set fd read-fds))
	      (when (position :write (cdr fd-in))
		(fd-set fd write-fds))
	      (when (position :error (cdr fd-in))
		(fd-set fd err-fds))
	      (incf i)
	      (setf nfds (max fd nfds))))
;       (format t "nfds = ~d~%" nfds)
;       (format t "read  = ")
;       (loop for i from 0 to nfds
; 	    do
; 	    (princ (if (= 0 (fd-isset i read-fds)) #\0 #\1)))
;       (format t "~%write = ")
;       (loop for i from 0 to nfds
; 	    do
; 	    (princ (if (= 0 (fd-isset i write-fds)) #\0 #\1)))
;       (format t "~%err   = ")
;       (loop for i from 0 to nfds
; 	    do
; 	    (princ (if (= 0 (fd-isset i err-fds)) #\0 #\1)))
;       (terpri)
      (with-foreign-slots ((tv_sec tv_usec) tv (:struct foreign-timeval))
	(multiple-value-bind (sec frac) (truncate timeout)
	  (setf tv_sec sec
		tv_usec (truncate (* frac 1000000))))
 ;	(format t "timeval ~d ~d~%" tv_sec tv_usec)
	)
      (setf ret-val (unix-select (1+ nfds)
				 read-fds write-fds err-fds
				 tv))
      (when (= -1 ret-val)
	(cond
	  (*got-sigwinch*
	   (setf *got-sigwinch* nil)
	   (cerror "Try again?" 'opsys-resized))
	  (*got-tstp*
	   (setf *got-tstp* nil)
	   ;; re-signal with the default, so we actually stop
	   (with-signal-handlers ((+SIGTSTP+ . :default))
	     (kill (getpid) +SIGTSTP+))
	   (cerror "Try again?" 'opsys-resumed))
	  (t
	   (error 'posix-error :error-code *errno*
		  :format-control "Select failed:"))))
;      (format t "return = ~d~%" ret-val)
      (when (not (zerop ret-val))
	(setf results
	      (loop :for f :in fds
		    :collect
		    (let ((fd (first f)))
		      `(,fd
			,@(if (not (= 0 (fd-isset fd read-fds)))
			      (list :read))
			,@(if (not (= 0 (fd-isset fd write-fds)))
			      (list :write))
			,@(if (not (= 0 (fd-isset fd err-fds)))
			      (list :error))))))))
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; poll

;; I think poll is easier to use from Lisp than select.
;; Too fucking bad it doesn't work. (at least on darwin)

(defcstruct foreign-pollfd
  (fd		:int)			; file descriptor
  (events	:short)			; events to look for
  (revents	:short))		; events returned

(defconstant +POLLIN+     #x0001)	; readable data available
(defconstant +POLLPRI+    #x0002)	; urgent data available
(defconstant +POLLOUT+    #x0004)	; writeable
(defconstant +POLLRDNORM+ #x0040)	; non-urgent data available
(defconstant +POLLRDBAND+ #x0080)	; urgent data available
(defconstant +POLLWRBAND+ #x0100)	; urgent data writeable

;; FreeBSD/Darwin extensions
(defconstant +POLLEXTEND+ #x0200)	; file extended
(defconstant +POLLATTRIB+ #x0400)	; attributes changed
(defconstant +POLLNLINK+  #x0800)	; link or unlink
(defconstant +POLLWRITE+  #x1000)	; contents changed

(defconstant +POLLERR+    #x0008)	; error
(defconstant +POLLHUP+    #x0010)	; hung up
(defconstant +POLLNVAL+   #x0020)	; invalid

; status:    (:condition :in :out)
; condition: (:hangup :invalid :error)
; priority:  (:high :med :normal)
;
; result:    (:ready :interrupted :timeout :error)

;; a lispy FD
(defstruct poll-fd
  fd
  status)

;; a lispy FD set
(defclass poll-set ()
  ((fds)				; lispy
   (foreign-fds)			; C struct
   (foreign-count)			; nfds
   (foreign-dirty))			; t when need to update foreign
  (:documentation "Set of file descriptors to poll."))

(defun poll-set-update ()
  "Update the foreign-fds."
;  (with-foreign-objects ((in-fds '(:struct foreign-pollfd) :count nfds))
)

(defun poll-set-add (fd what)
  (declare (ignore fd what))
  )

(defun poll-set-remove (fd)
  (declare (ignore fd))
  )

(defcfun ("poll" unix-poll) :int (fds :pointer) (nfds :int) (timeout :int))

;; Just do a really simple slow all-in-one version for now.
;; fds is ((fd :status) ...) e.g. ((1 :read) (2 :write) (3 :read :write))
;; timeout is just passed along
;; LATER: do a version with persistant sets using the above structs & funcs
;; @@@
(defun lame-poll (fds timeout)
  "See if some data is available."
  (let ((nfds (length fds))
	ret-val results)
    (with-foreign-objects ((in-fds '(:struct foreign-pollfd) nfds))
      (loop :with i = 0
	    :for f :in fds
	    :do
	    (with-foreign-slots ((fd events revents)
				 (mem-aref in-fds '(:struct foreign-pollfd) i)
				 (:struct foreign-pollfd))
	      (let ((fd-in f))
		(setf fd (first fd-in))
		(setf events 0)
		(setf revents 0)
		(when (position :read (cdr fd-in))
		  (setf events (logior events +POLLIN+)))
		(when (position :write (cdr fd-in))
		  (setf events (logior events +POLLOUT+)))))
	    (incf i))
      (loop :for i :from 0 :below nfds
	    :do (with-foreign-slots ((fd events revents)
				    (mem-aref in-fds
					      '(:struct foreign-pollfd) i)
				    (:struct foreign-pollfd))
		 (format t "fd[~d] = ~a ~x ~x~%" i fd events revents)))
      (format t "poll(~a,~d,~d)~%" in-fds nfds timeout)
      (when (= -1 (setf ret-val (unix-poll in-fds nfds timeout)))
	(error 'posix-error :format-control "Poll failed ~a"
	       :error-code *errno*))
      (format t "return = ~d~%" ret-val)
      (when (not (= 0 ret-val))
	(setf results
	      (loop with thing = nil
		    for i from 0 below ret-val
		    do
		    (with-foreign-slots ((fd revents)
					 (mem-aref in-fds
						   '(:struct foreign-pollfd) i)
					 (:struct foreign-pollfd))
		      (setf thing
			    `(,fd
			      ,@(if (not (= 0 (logand revents +POLLIN+)))
				    (list :read))
			      ,@(if (not (= 0 (logand revents +POLLOUT+)))
				    (list :write))
			      ,@(if (not (= 0 (logand revents +POLLERR+)))
				    (list :error)))))
		    collect thing))))
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; epoll

#+linux
(progn
  (defconstant +EPOLL-CLOXEC+  #o02000000 "Close descriptor on exec.")
  (defconstant +EPOLL-CTL-ADD+ 1 "Add a descriptor.")
  (defconstant +EPOLL-CTL-MOD+ 2 "Modify a descriptor.")
  (defconstant +EPOLL-CTL-DEL+ 3 "Delete a descriptor.")

  (defconstant +EPOLLIN+      #x001)
  (defconstant +EPOLLPRI+     #x002)
  (defconstant +EPOLLOUT+     #x004)
  (defconstant +EPOLLRDNORM+  #x040)
  (defconstant +EPOLLRDBAND+  #x080)
  (defconstant +EPOLLWRNORM+  #x100)
  (defconstant +EPOLLWRBAND+  #x200)
  (defconstant +EPOLLMSG+     #x400)
  (defconstant +EPOLLERR+     #x008)
  (defconstant +EPOLLHUP+     #x010)
  (defconstant +EPOLLRDHUP+   #x2000)
  (defconstant +EPOLLWAKEUP+  (ash 1 29))
  (defconstant +EPOLLONESHOT+ (ash 1 30))
  (defconstant +EPOLLET+      (ash 1 31))

  (defcunion foreign-epoll-data
    (ptr    (:pointer :void))
    (fd	    :int)
    (u32    :uint32)
    (u64    :uint64))

  (defcstruct foreign-epoll-event
    (events :uint32)
    (data   (:union foreign-epoll-data)))

  (defcfun epoll-create :int (size :int))
  (defcfun epoll-create1 :int (flags :int))

  (defcfun epoll-ctl :int (epfd :int) (op :int) (fd :int)
	   (event (:pointer (:struct foreign-epoll-event))))
  (defcfun epoll-wait :int (epfd :int)
	   (events (:pointer (:struct foreign-epoll-event)))
	   (maxevents :int)
	   (timeout :int))
  (defcfun epoll-pwait :int (epfd :int)
	   (events (:pointer (:struct foreign-epoll-event)))
	   (maxevents :int)
	   (timeout :int)
	   (sigmask (:pointer sigset-t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kqueue

;; @@@ need to check these structs with the source

#+darwin
(progn
  (defcstruct foreign-kevent
    (ident	(:pointer :uint32))	; XXX uintptr-t
    (filter	:int16)
    (flags	:uint16)
    (fflags	:uint32)
    (data	(:pointer :int))	; XXX intptr
    (udata	(:pointer :void)))

  (defcstruct foreign-kevent64
    (ident	:uint64)
    (filter	:int16)
    (flags	:uint16)
    (fflags	:uint32)
    (data	:int64)
    (udata	:uint64)
    (ext	:uint64 :count 2))

  (defcfun kqueue :void)
  (defcfun ("kevent" real-kevent) :int
    (kq :int) (changelist (:pointer (:struct foreign-kevent))) (nchanges :int)
    (eventlist (:pointer (:struct foreign-kevent))) (nevents :int)
    (timeout (:pointer (:struct foreign-timespec))))

  (defcfun ("kevent64" real-kevent64) :int
    (kq :int) (changelist (:pointer (:struct foreign-kevent64))) (nchanges :int)
    (eventlist (:pointer (:struct foreign-kevent64))) (nevents :int)
    (timeout (:pointer (:struct foreign-timespec))))

  (defun ev-set32 (key ident filter flags fflags data udata)
    (declare (ignore key ident filter flags fflags data udata))
    )

  (defun ev-set64 (key ident filter flags fflags data udata)
    (declare (ignore key ident filter flags fflags data udata))
    )

  (defun ev-set (key ident filter flags fflags data udata)
    "Do the appropriate version of EV_SET."
    (declare (ignore key ident filter flags fflags data udata))
    )

  (defun kevent (kq changelist nchanges eventlist nevents timeout)
    "Do the appropriate version of kevent."
    (declare (ignore kq changelist nchanges eventlist nevents timeout))
    ))

;; It might be nice if we could do this on a Lisp stream.
(defun listen-for (seconds &optional (fd 0))
  "Listen on the OS file descriptor for at most N seconds or until input is ~
available."
;  (lame-poll `((,fd :read)) (truncate (* 1000 seconds)))
  (lame-select `((,fd :read)) seconds)
  )

(defun test-listen-for ()
  (let (fd)
    (unwind-protect
      (progn
	(setf fd (posix-open "/dev/tty" +O_RDWR+ #o600))
	(format t "Foo ->")
	(finish-output)
	(listen-for 5 fd))
      (when fd
	(posix-close fd)))))

(defun set-sigio (fd)
  "Set a file descriptor so that SIGIO is sent when IO happens."
  (let ((flags (fcntl fd +F_GETFL+)))
      ;; #+linux
      ;; (fcntl fd +F_SETSIG+ :int 0) ;; If we wanted a different signal
      ;; (fcntl fd +F_SETOWN+ :int (getpid)) ;; or maybe (gettid)
      (fcntl fd +F_SETFL+ :int (logior +O_ASYNC+ flags))))

(defcallback sigio-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  (format t "Yo!~%") (finish-output)
  (throw 'got-some-io-bro t))

(defmacro with-interrupting-io ((&rest file-descriptors) io-form &body body)
  "Evaluate the BODY. If I/O occurs on file-descriptors, exit the BODY and
evaluate the IO-FORM."
  (let ((result (gensym "WIIO-RESULT")))
    `(let (,result)
       (if (eq 'got-some-io-bro
	       (setf ,result
		     (catch 'got-some-io-bro
		       (with-signal-handlers ((+SIGIO+ . sigio-handler))
			 (loop :for fd :in (list ,@file-descriptors) :do
			    (set-sigio fd))
			 (describe-signals)
			 ,@body))))
	   ,io-form
	   ,result))))

;; End
