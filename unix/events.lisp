;;
;; unix/events.lisp - Unix interface to events and polling
;;

(in-package :opsys-unix)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

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
  (defconstant +EPOLL-CLOEXEC+ #o02000000 "Close descriptor on exec.")
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
    ;;(data   (:union foreign-epoll-data))
    (data   :uint64)
    )

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

#+(or darwin freebsd openbsd)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (config-feature :os-t-has-kqueue))

#+os-t-has-kqueue
(progn
  (defparameter *kqueue-flags* nil)
  (define-to-list *kqueue-flags*
    #(#(+EV-ADD+       #x0001 "Add an event to kq (implies enable)")
      #(+EV-DELETE+    #x0002 "Delete an event from kq")
      #(+EV-ENABLE+    #x0004 "Enable an event")
      #(+EV-DISABLE+   #x0008 "Disable an event (not reported)")
      #(+EV-ONESHOT+   #x0010 "Only report one occurrence")
      #(+EV-CLEAR+     #x0020 "Clear the event state after reporting")
      #(+EV-RECEIPT+   #x0040 "Force an EV_ERROR on success, data=0")
      #(+EV-DISPATCH+  #x0080 "Disable an event after reporting")
      #(+EV-EOF+       #x8000 "EOF detected")
      #(+EV-ERROR+     #x4000 "Error, data contains errno")))

  (defparameter *kqueue-filters* nil)
  (define-to-list *kqueue-filters*
    #(#(+EVFILT-READ+    -1)
      #(+EVFILT-WRITE+   -2)
      #(+EVFILT-AIO+     -3 "Attached to aio requests")
      #(+EVFILT-VNODE+   -4 "Attached to vnodes")
      #(+EVFILT-PROC+    -5 "Attached to struct process")
      #(+EVFILT-SIGNAL+  -6 "Attached to struct process")
      #(+EVFILT-TIMER+   -7 "Timers")))

  (defparameter *kqueue-events* nil)
  (define-to-list *kqueue-events*
    #(#(+NOTE-DELETE+     #x00000001 "Removed")
      #(+NOTE-WRITE+      #x00000002 "Data contents changed")
      #(+NOTE-EXTEND+     #x00000004 "Size increased")
      #(+NOTE-ATTRIB+     #x00000008 "Attributes changed")
      #(+NOTE-LINK+       #x00000010 "Link count changed")
      #(+NOTE-RENAME+     #x00000020 "Renamed")
      #(+NOTE-REVOKE+     #x00000040 "Access was revoked")
      #(+NOTE-TRUNCATE+   #x00000080 "Truncated")
      #(+NOTE-EXIT+       #x80000000 "Process exited")
      #(+NOTE-FORK+       #x40000000 "Process forked")
      #(+NOTE-EXEC+       #x20000000 "Process exec'd")
      #(+NOTE-PCTRLMASK+  #xf0000000 "Mask for hint bits")
      #(+NOTE-PDATAMASK+  #x000fffff "Mask for PID")
      #(+NOTE-TRACK+      #x00000001 "Follow across forks")
      #(+NOTE-TRACKERR+   #x00000002 "Could not track child")
      #(+NOTE-CHILD+      #x00000004 "A child process")
      #(+NOTE-LOWAT+      #x0001 "low water mark")
      #(+NOTE-EOF+        #x0002 "return on EOF")))

  #+darwin
  (defcstruct foreign-kevent
    (ident	uintptr-t)
    (filter	:int16)
    (flags	:uint16)
    (fflags	:uint32)
    (data	intptr-t)
    (udata	(:pointer :void)))

  #+darwin
  (defcstruct foreign-kevent64
    (ident	:uint64)
    (filter	:int16)
    (flags	:uint16)
    (fflags	:uint32)
    (data	:int64)
    (udata	:uint64)
    (ext	:uint64 :count 2))

  #+openbsd
  (defcstruct foreign-kevent
    (ident	uintptr-t)
    (filter	:short)
    (flags	:unsigned-short)
    (fflags	:unsigned-int)
    (data	:int64)
    (udata	(:pointer :void)))

  #+freebsd
  (defcstruct foreign-kevent
    (ident	uintptr-t)
    (filter	:short)
    (flags	:unsigned-short)
    (fflags	:unsigned-int)
    (data	intptr-t)
    (udata	(:pointer :void)))

  (defcfun kqueue :void)
  (defcfun ("kevent" real-kevent) :int
    (kq :int) (changelist (:pointer (:struct foreign-kevent))) (nchanges :int)
    (eventlist (:pointer (:struct foreign-kevent))) (nevents :int)
    (timeout (:pointer (:struct foreign-timespec))))

  #+darwin
  (defcfun ("kevent64" real-kevent64) :int
    (kq :int) (changelist (:pointer (:struct foreign-kevent64))) (nchanges :int)
    (eventlist (:pointer (:struct foreign-kevent64))) (nevents :int)
    (timeout (:pointer (:struct foreign-timespec))))

  #+darwin
  (defun ev-set32 (key ident filter flags fflags data udata)
    (declare (ignore key ident filter flags fflags data udata))
    )

  #+darwin
  (defun ev-set64 (key ident filter flags fflags data udata)
    (declare (ignore key ident filter flags fflags data udata))
    )

  (defun ev-set (kev i f fl ffl d ud)
    "Do the appropriate version of EV_SET."
    (with-foreign-slots ((ident filter flags fflags data udata)
			 kev (:struct foreign-kevent))
      (setf ident i
	    filter f
	    flags fl
	    fflags ffl
	    data d
	    udata ud)))

  (defun kevent (kq changelist nchanges eventlist nevents timeout)
    "Do the appropriate version of kevent."
    (syscall (real-kevent kq changelist nchanges eventlist nevents timeout)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It might be nice if we could do this on a Lisp stream.
;;(defun listen-for (seconds &optional (fd 0))
(defun listen-for (seconds fd)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Portable interface

;; So, if we were trying to optimally efficient we should make the portable
;; io-events combined into one event. But the clarity and usability of the
;; interface with them separated, seems much better to me. Anyway the cost of
;; combining and uncombining them is dwarfed by the cost of any actual I/O we
;; do. If you want the absolute fastest, you can roll your own. But if you
;; want the absolute fastest, you should call it from C to elimitate FFI
;; overhead, and for that matter call it from the kernel to eliminate syscall
;; and context switch overhead. It's possible a LispOS could do it with
;; userland security *and* less overhead. #-rant

;; @@@ This is all epoll specific for now. It needs work AND we need to
;; create a kqueue version.
#+linux
(progn

;; @@@ How should we deal with multiple threads doing epoll?
;; With this setup you could workaround it by just binding your own thread
;; dynamic *epoll-fd*, but we should make the nice interface just work properly.
(defstruct unix-event-set
  fd			; File descriptor for epoll instance
  events		; (:pointer (:struct foreign-epoll-event))
  count			; size of events array
  table			; epoll event to lisp event mapping hash table
  fd-table		; event fd to tables index hash table
  signal-array		; signals noticed
  signal-mask		; signal mask for epoll
  signal-block-mask)	; signal block mask for epoll

;; events     the thing passed right to epoll
;; count      the size of events
;; table      has an entry for every event, which is a list of os-events
;;            This is so we find the lisp os-events for the events returned/set
;;            from epoll.
;;            The key is an index into EVENTS
;;            The value is a list of os-events
;; fd-table   a hash table so for a given fd we can find the C & lisp events
;;            The key of fd-table is an fd.
;;            The value of fd-table is an index into TABLE.

(defvar *event-array-growth-factor* 2)
(defvar *event-array-starting-size* 20)

(defun lisp-event-to-epoll-event-mask (event)
  (typecase event
    (input-available-event +EPOLLIN+)
    (output-possible-event +EPOLLOUT+)
    (output-finished-event
     ;; @@@ Is this really what I mean?
     (logior +EPOLLOUT+ +EPOLLET+))
    (io-error-event
     ;; @@@ This is actually un-necessary.
     +EPOLLERR+)
    (t 0)))

(defun epoll-event-mask-to-lisp-event-type (mask)
  (let (type-list)
    (when (logand mask +EPOLLIN+)
      (push 'input-available-event type-list))
    (when (logand mask +EPOLLOUT+)
      (push 'output-possible-event type-list))
    (when (logand mask (logior +EPOLLET+ +EPOLLOUT+))
      (push 'output-finished-event type-list))
    (when (logand mask +EPOLLERR+)
      (push 'io-error-event type-list))
    (append '(or) type-list)))

(defun %create-event-set (set)
  (setf (event-set-os-data set)
	(make-unix-event-set
	 ;; I think you'd normally want it closed automatically.
	 :fd (syscall (epoll-create1 +EPOLL-CLOEXEC+))
	 :events (foreign-alloc '(:struct foreign-epoll-event)
				:count *event-array-starting-size*)
	 :count 0
	 :table (make-hash-table)
	 :fd-table (make-hash-table)
	 :signal-array (make-array *signal-count*
				   :element-type 'fixnum
				   :initial-element 0)
	 :signal-mask (foreign-alloc 'sigset-t)
	 :signal-block-mask (foreign-alloc 'sigset-t))))

(defun resize-event-set (set size)
  (with-slots (events count table) set
    (when (> size count)
      (error "FAILBOAT")
      #|
      (foreign-free events)
      (setf count (* count *event-array-growth-factor*))
      (setf events
	    (foreign-alloc '(:struct foreign-epoll-event)
			   :count count))
      ;; Re-confabulate the whole table.
      (let (coalesced)
	(loop :with item
	   :for e :in (event-set-list set)
	   :do
	   (when (typep e 'io-event)
	     (setf item (assoc (io-event-handle e) coalesced))
	     (if item
		 (push e (cdr item))
		 (setf coalesced (acons 

      (loop :for i :from 0
	 :for e :in (event-set-list set)
	 :do
	 (when (typep e 'io-event)
	   (setf (aref table i) e
		 (foreign-slot-value
		  (mem-aref events '(:struct foreign-epoll-event) i)
		  '(:struct foreign-epoll-event)
		  'events)
		 (lisp-event-to-epoll-event-mask e)))))
      |#
      )))

(defun %destroy-event-set (set)
  (with-slots (fd events signal-mask signal-block-mask) (event-set-os-data set)
    (foreign-free events)
    (foreign-free signal-mask)
    (foreign-free signal-block-mask)
    (syscall (posix-close fd))))

(defun %add-event (event set)
  "Add the event to the SET."
  (typecase event
    (io-event
     (format t "Adding io-event~%")
     ;; @@@ Perhaps there should be an interface for adding a bunch of events?
     (with-slots (events count table fd-table) (event-set-os-data set)
       (let ((index (gethash (io-event-handle event) fd-table)))
	 (if index
	     ;; We already have something for this FD. Add this event to it.
	     ;; Even though we might have duplicate lisp events for an FD,
	     ;; we won't have multiple of the exact same lisp event.
	     (progn
	       (format t "Already existing fd~%")
	       (pushnew event (gethash index table) :test #'equal))
	     ;; Otherwise, we add it.
	     (progn
	       (format t "new fd~%")
	       (setf index count)
	       ;; Make sure it's big enough
	       (resize-event-set (event-set-os-data set) index)
	       (setf (gethash index table) (list event)
		     (gethash (io-event-handle event) fd-table) index)
	       (let ((ee (mem-aptr events '(:struct foreign-epoll-event) index)))
		 (format t "ee = ~a~%" ee)
		 (setf (foreign-slot-value
			ee
			'(:struct foreign-epoll-event)
			'events)
		       (lisp-event-to-epoll-event-mask event))
		 (format t "ee.events = ~x~%"
			 (lisp-event-to-epoll-event-mask event))
		 (setf
		       ;; ?Is this necessary? Isn't the data slot just for
		       ;; user reference?
		       (foreign-slot-value
			;;(mem-aptr events '(:struct foreign-epoll-event) index)
			ee
			'(:struct foreign-epoll-event)
			'data)
		       (io-event-handle event))
		 (format t "ee.data = ~s~%"
			 (io-event-handle event))
		 )
	       (incf count)))))
     ;; Tell epoll about it.
     (with-foreign-object (ev '(:struct foreign-epoll-event))
       (with-foreign-slots ((events data) ev (:struct foreign-epoll-event))
	 (setf events (lisp-event-to-epoll-event-mask event)
	       data (io-event-handle event))
	 (format t "epoll-ctl events ~x~%" events)
	 (syscall (epoll-ctl
		   (unix-event-set-fd (event-set-os-data set))
		   +EPOLL-CTL-ADD+
		   (io-event-handle event) ev)))))
    ((or signal-event os-process-event terminal-size-change-event)
     ;; @@@ set signal mask
     )
    (timer-event
     ;; @@@ set the minimum timeout
     )))

(defun %delete-event (event set)
  "Delete the event from the SET."
  (with-slots (fd events count table) (event-set-os-data set)
    (typecase event
      (io-event
       (with-slots (events count table fd-table) (event-set-os-data set)
	 (let ((index (gethash (io-event-handle event) fd-table))
	       lisp-events)
	   ;; If this is the only event for this handle in the set
	   (if index
	       (progn
		 (setf lisp-events (gethash index table))
		 (if (and (= 1 (length lisp-events))
			  (equal event (first lisp-events)))
		     ;; It's the last one, go ahead and delete it.
		     (progn
		       (remhash index table)
		       (remhash (io-event-handle event) fd-table)
		       (syscall (epoll-ctl
				 fd +EPOLL-CTL-DEL+ (io-event-handle event)
				 (null-pointer))))
		     ;; otherwise we have to change it
		     (progn
		       ;; @@@ in a different way than this
		       (warn "I didn't really write this part yet.")
		       (with-foreign-object (ev '(:struct foreign-epoll-event))
			 (with-foreign-slots ((events data) ev
					      (:struct foreign-epoll-event))
			   (setf events (lisp-event-to-epoll-event-mask event))
			   (syscall (epoll-ctl fd +EPOLL-CTL-MOD+
					       (io-event-handle event) ev)))))))
	       ;; This shouldn't happen right?
	       ;; We could just ignore it?
	       (progn
		 (if (not (find event (event-set-list set) :test #'equal))
		     (cerror "Whatever."
			     "That event doesn't seem to be in the set.")
		     (cerror "Whatever"
			     "Sorry. Something got screwed up, because I can't ~
                              find that file descriptor."))
		 (return-from %delete-event nil))))))
      ((or signal-event os-process-event terminal-size-change-event)
       ;; @@@ set signal mask
       )
      (timer-event
       ;; @@@ set the minimum timeout
       ))))

(defun %clear-triggers (set)
  "Clear the triggers for the event SET."
  (declare (ignore set))
  ;; @@@ I guess we don't have to do anything?
  )

(defun timeout-to-milliseconds (timeout)
  "Return the proper argument for epoll_wait, given TIMEOUT, or signal an error."
  (cond
    ((null timeout) -1)
    ((eq timeout t) 0)
    ((os-time-p timeout)
     (+ (* (os-time-seconds timeout) 1000)
	(truncate (/ (os-time-nanoseconds timeout) 1000000))))
    ((numberp timeout)			; in seconds
     (truncate (* timeout 1000)))
    (t (error 'opsys-error
	      :format-control
	      "Timeout isn't a type we can handle.~%"))))

(defun event-set-signals (event-set)
  "Return a list of signal numbers from the event set."
  (loop :for e :in (event-set-list event-set)
     :when (typep e 'signal-event)
     :collect (signal-event-number e)))

(defvar *signals-noticed* nil
  "Array of signals received.")

(cffi:defcallback signal-noticer :void ((signal-number :int))
  (incf (aref *signals-noticed* signal-number)))

(defun check-signals (event-set)
  (loop
     :with array = (unix-event-set-signal-array
		       (event-set-os-data event-set))
     :and found-count = 0
     :for i :from 0 :below *signal-count*
     :when (not (zerop (aref array i)))
     :do
     (loop :for e :in (event-set-list event-set)
	:do
	(when (and (typep e 'signal-event)
		   (= (signal-event-number e) i))
	  (setf (os-event-triggered e) t)
	  (incf found-count)))
     :finally (return found-count)))

(defun check-timers (event-set)
  (declare (ignore event-set))
  ;; (loop :for i :from 0 :below *signal-count*
  ;;    :when (not (zerop (aref signal-array i)))
  ;;    :do
  ;;    (trigger-signals event-set)
  ;;    (return t))
  0)

(defun await-events (&key (event-types t) (event-set *event-set*) timeout
		       (leave-triggers nil))
  "Wait for events of the given EVENT-TYPES, or T for any event. Return if we
don't get an event before TIMEOUT. TIMEOUT can be NIL wait potentially forever,
or T to return immediately, otherwise it's a OS-TIME, or a number of seconds.
The default for TIMEOUT is NIL. If LEAVE-TRIGGERS it T, it will not clear
triggers in the EVENT-SET, that were set before being invoked.
Retruns NIL if the timeout was up before getting any events, otherwise return
the count of event triggered."
  #+linux
  (declare (ignore event-types))
  (when (not (event-set-os-data event-set))
    (error 'opsys-error
	   :format-control "os-data is missing on event-set ~s~%"
	   :format-arguments `(,event-set)))
  (when (not leave-triggers)
    (mapcar (_ (setf (os-event-triggered _) nil)) (event-set-list event-set)))

  (let (poll-result
	(ms-timeout (timeout-to-milliseconds timeout))
	(triggered-count 0)
	*signals-noticed* signals saved-handlers)
    (with-slots (fd events count table fd-table signal-array signal-mask
		 signal-block-mask)
	(event-set-os-data event-set)
      ;; This how I think the signal stuff should happen:
      ;;  - block signals in the event set
      ;;  - set up handlers for signals in the event set
      ;;  - set the mask to unblock them
      ;;  - call epoll
      ;;  - check handler flags and trigger events
      ;;  - remove signal handlers for signals in the event set
      ;;  - unblock signals in the event set

      (fill signal-array 0)
      (setf *signals-noticed* signal-array
	    signals (event-set-signals event-set))
      (sigemptyset signal-mask)
      (syscall (sigprocmask +SIG-SETMASK+ (null-pointer) signal-mask))
      (loop :for sig :in signals :do
	 (sigdelset signal-mask sig)
	 (sigaddset signal-block-mask sig))
      (sigdelset signal-mask +SIGSEGV+)
      (sigdelset signal-mask +SIGTRAP+)
      (sigdelset signal-block-mask +SIGSEGV+)
      (sigdelset signal-block-mask +SIGTRAP+)

      (unwind-protect
	   (progn
	     (loop :for sig :in signals :do
		(push (cons sig (signal-action sig)) saved-handlers))
	     (syscall (sigprocmask +SIG-BLOCK+
				   (null-pointer) signal-block-mask))
	     (loop :for sig :in signals :do
		(setf (signal-action sig) 'signal-noticer))

	     ;; There's not supposed to be a race condition right here -->
	     ;; because we've blocked beforehand.

	     ;; So, if we did everything right in add/delete-event than we
	     ;; shouldn't have to set up anything here, which is the most
	     ;; of the point.
	     (loop :with again = t :and result
		:do
		(setf poll-result (epoll-pwait fd events count
					       ms-timeout
					       signal-mask))
		(syscall (sigprocmask +SIG-UNBLOCK+
				      signal-block-mask (null-pointer)))
		(format t "poll result = ~d~%" poll-result)
		(cond
		  ((and (= poll-result -1) (= *errno* +EINTR+)
			(not (zerop (setf result (check-signals event-set)))))
		   (format t "signal triggered~%")
		   (setf again nil poll-result :already-handled)
		   (incf triggered-count result))
		  ((and (zerop poll-result)
			(not (zerop (setf result (check-timers event-set)))))
		   ;; (format t "timer triggered~%")
		   ;; (setf again nil poll-result :already-handled)
		   ;; (incf triggered-count result)
		   )
		  (t (setf again nil)))
		:while again))
	(syscall (sigprocmask +SIG-UNBLOCK+
			      signal-block-mask (null-pointer)))
	(when saved-handlers
	  (loop :for h :in saved-handlers :do
	     (setf (signal-action (car h)) (cdr h)))))

      (case poll-result
	(:already-handled)
	(-1 ;; error
	 (error 'posix-error :error-code *errno*))
	(0 ;; timeout
	 (warn "I don't handle timeout properly yet.")
	 (setf triggered-count nil))
	(otherwise
	 ;; splode out the mofos
	 (loop :with event-mask :and event-fd :and event-list
	    :for i :from 0 :below count :do
	    (setf event-mask
		  (foreign-slot-value
		   (mem-aptr events '(:struct foreign-epoll-event) i)
		   '(:struct foreign-epoll-event)
		   'events)
		  event-fd
		  (foreign-slot-value
		   (mem-aptr events '(:struct foreign-epoll-event) i)
		   '(:struct foreign-epoll-event)
		   'data)
		  event-list (gethash i table))
	    (format t "i = ~s event-fd = ~s event-mask = ~x~%"
		    i event-fd event-mask)
	    (if event-list
		(loop :for e :in event-list :do
		   (when (and (typep e (epoll-event-mask-to-lisp-event-type
					event-mask))
			      (equal (io-event-handle e) event-fd))
		     (format t "triggered ~s ~s~%" event-fd e)
		     (setf (os-event-triggered e) t)
		     (incf triggered-count)))
		(error 'opsys-error
		       ;; @@@ or some better message
		       :format-control "Event not found in set."))))))
    triggered-count))
) ;; @@@ epoll (linux) specific code

(defun pick-events (event-types &key (event-set *event-set*) remove timeout)
  "Return any pending events of the types given in EVENT-TYPES. If REMOVE is
true, remove the events from the EVENT-SET. Return NIL if there aren't any
events before TIMEOUT. TIMEOUT can be NIL wait potentially forever, or T to
return immediately, otherwise it's a OS-TIME. The default for TIMEOUT is NIL."
  (declare (ignore event-types event-set remove timeout))
  )

(defun map-events (function &key (event-set *event-set*) (event-types t))
  "Call FUNCTION for each event in EVENT-SET, that is pending or triggered.
EVENT-TYPES restricts the events mapped to those types."
  (declare (ignore function event-set event-types)))

(defun events-pending-p (&key (event-types t) (event-set *event-set*))
  "Return true if there are any events pending in the EVENT-SET. Restrict the
events considered to those in EVENT-TYPES, if it's not T."
  (declare (ignore event-types event-set)))

;; End
