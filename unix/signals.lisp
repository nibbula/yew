;;
;; unix/signals.lisp - Unix interface to signals
;;

(in-package :opsys-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

#+sunos (defcvar ("_sys_siglistn" *nsig*) :int)
#+sunos (defcvar ("_sys_siglistp" sys-siglist) :pointer)

#+(or darwin freebsd openbsd) (defcvar ("sys_siglist" sys-siglist) :pointer)
#+(or darwin freebsd openbsd) (defcvar ("sys_signame" sys-signame) :pointer)

(defparameter *signal-count*
  #+(or darwin freebsd openbsd linux) 32
  ;; actually 65 if you count realtime (RT) signals
  #+sunos *nsig*
  #-(or darwin sunos linux freebsd openbsd)
  (missing-implementation '*signal-count*)
  "Number of signal types, a.k.a. NSIG.")

(defconstant +SIGHUP+	 1  "Hangup")
(defconstant +SIGINT+	 2  "Interrupt")
(defconstant +SIGQUIT+	 3  "Quit")
(defconstant +SIGILL+	 4  "Illegal instruction")
(defconstant +SIGTRAP+	 5  "Trace/BPT trap")
(defconstant +SIGABRT+	 6  "Abort trap")
(defconstant +SIGPOLL+	 #+darwin 7
	                 #+linux 29
			 #+(or freebsd openbsd) nil
			 "pollable event")
(defconstant +SIGEMT+	 #+(or darwin freebsd openbsd) 7
	                 #+linux nil "EMT trap")
(defconstant +SIGFPE+	 8 "Floating point exception")
(defconstant +SIGKILL+	 9 "Killed")
(defconstant +SIGBUS+	 #+(or darwin freebsd openbsd) 10
	                 #+linux 7 "Bus error")
(defconstant +SIGSEGV+	 11 "Segmentation fault")
(defconstant +SIGSYS+	 #+(or darwin openbsd) 12
	                 #+linux 31
			 #+freebsd nil "Bad system call")
(defconstant +SIGPIPE+	 13 "Broken pipe")
(defconstant +SIGALRM+	 14 "Alarm clock")
(defconstant +SIGTERM+	 15 "Terminated")
(defconstant +SIGURG+	 #+(or darwin freebsd openbsd) 16
                         #+linux 23 "Urgent I/O condition")
(defconstant +SIGSTOP+	 #+(or darwin freebsd openbsd) 17
                         #+linux 19 "Suspended (signal)")
(defconstant +SIGTSTP+	 #+(or darwin freebsd openbsd) 18
                         #+linux 20 "Suspended")
(defconstant +SIGCONT+	 #+(or darwin freebsd openbsd) 19
                         #+linux 18 "Continued")
(defconstant +SIGCHLD+	 #+(or darwin freebsd openbsd) 20
                         #+linux 17 "Child exited")
(defconstant +SIGTTIN+	 21 "Stopped (tty input)")
(defconstant +SIGTTOU+	 22 "Stopped (tty output)")
(defconstant +SIGIO+	 #+(or darwin freebsd openbsd) 23
	                 #+linux 29 "I/O possible")
(defconstant +SIGXCPU+	 24 "Cputime limit exceeded")
(defconstant +SIGXFSZ+	 25 "Filesize limit exceeded")
(defconstant +SIGVTALRM+ 26 "Virtual timer expired")
(defconstant +SIGPROF+	 27 "Profiling timer expired")
(defconstant +SIGWINCH+	 28 "Window size changes")
(defconstant +SIGINFO+	 #+(or darwin freebsd openbsd) 29
                         #+linux nil "Information request")
(defconstant +SIGUSR1+	 #+(or darwin freebsd openbsd) 30
                         #+linux 10 "User defined signal 1")
(defconstant +SIGUSR2+	 #+(or darwin freebsd openbsd) 31
                         #+linux 12 "User defined signal 2")
(defconstant +SIGSTKFLT+ #+(or darwin freebsd openbsd) nil
                         #+linux 16 "Stack fault")
(defconstant +SIGPWR+	 #+(or darwin freebsd openbsd) nil
                         #+linux 30 "Power failure restart")
#+(or freebsd openbsd)
(defconstant +SIGTHR+    32 "thread library")

#+freebsd
(progn
  (defconstant +SIGLWP+	  32 "thread library")
  (defconstant +SIGLIBRT+ 33 "real-time library"))

#+linux
(defparameter *signal-name*
    #(nil
      "HUP" "INT" "QUIT" "ILL" "TRAP" "ABRT" "BUS" "FPE" "KILL" "USR1"
      "SEGV" "USR2" "PIPE" "ALRM" "TERM" "STKFLT" "CHLD" "CONT" "STOP"
      "TSTP" "TTIN" "TTOU" "URG" "XCPU" "XFSZ" "VTALRM" "PROF" "WINCH"
      "IO" "PWR" "SYS"))

#+sunos (defparameter SIG2STR_MAX 64 "Bytes for signal name.")
#+sunos (defcfun sig2str :int (signum :int) (str :pointer))

(defun signal-name (sig)
  #+sunos (with-foreign-pointer-as-string (s SIG2STR_MAX)
	    (sig2str sig s)
	    s)
  #+(or darwin freebsd openbsd)
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-signame) :pointer sig)))
  #+linux (when (< sig *signal-count*)
	    (aref *signal-name* sig))
  #-(or darwin sunos linux freebsd openbsd) (declare (ignore sig))
  #-(or darwin sunos linux freebsd openbsd) (missing-implementation 'signal-name)
)

#+(or sunos linux) (defcfun strsignal :string (sig :int))

(defun signal-description (sig)
  #+(or sunos linux) (strsignal sig)
  #+(or darwin openbsd)
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-siglist) :pointer sig)))
  #-(or darwin sunos linux openbsd) (declare (ignore sig))
  #-(or darwin sunos linux openbsd) (missing-implementation 'signal-description)
)

;(defparameter signal-names (make-hash-table 
;(defun signal-number (name)

; #+os-t-has-siglist
; (eval-when (:compile-toplevel :load-toplevel :execute)
;   (loop for i from 0 to *signal-count*
;     do
;     `(defparameter ,(signal-name i) ,i)))

;; Should we do our own macros/functions?

(defcfun sigaddset :int (set (:pointer sigset-t)) (signo :int))
(defcfun sigdelset :int (set (:pointer sigset-t)) (signo :int))
(defcfun sigemptyset :int (set (:pointer sigset-t)))
(defcfun sigfillset :int (set (:pointer sigset-t)))
(defcfun sigismember :int (set (:pointer sigset-t)) (signo :int))

#+(or darwin linux openbsd)
(defcstruct foreign-sigaction
  "What to do with a signal, as given to sigaction(2)."
  (sa_handler :pointer)	       ; For our purposes it's the same as sa_sigaction
  (sa_mask sigset-t)
  (sa_flags :int)
  #+linux (sa_restorer :pointer)
  )

#+freebsd
(defcstruct foreign-sigaction
  "What to do with a signal, as given to sigaction(2)."
  (sa_handler :pointer)	       ; For our purposes it's the same as sa_sigaction
  (sa_flags :int)
  (sa_mask sigset-t))

#+(or darwin freebsd)		    ; freebsd also defines sigval_X slot names
(defcunion sigval
 (sival_int :int)
 (sival_ptr (:pointer :void)))

#+darwin
(defcstruct foreign-siginfo
  (si_signo :int)			; Signal number
  (si_errno :int)			; Errno association
  (si_code :int)			; Signal code
  (si_pid pid-t)			; Sending process ID
  (si_uid uid-t)			; Sender's ruid
  (si_status :int)			; Exit value
  (si_addr (:pointer :void))		; Faulting instruction
  (si_value (:union sigval))		; Signal value
  (si_band :long)			; Band event for SIGPOLL
  (__pad :unsigned-long :count 7))	; Reserved for future use

#|
As you may know, ucontext is so hairy and has versions for every minor
variation of architecture and is rarely needed outside of the kernel, that I
nearly want to put in in separate file.

#+darwin
(defcstruct x86-thread-state
  (eax	  :unsigned-int)
  (ebx	  :unsigned-int)
  (ecx	  :unsigned-int)
  (edx	  :unsigned-int)
  (edi	  :unsigned-int)
  (esi	  :unsigned-int)
  (ebp	  :unsigned-int)
  (esp	  :unsigned-int)
  (ss	  :unsigned-int)
  (eflags :unsigned-int)
  (eip	  :unsigned-int)
  (cs	  :unsigned-int)
  (ds	  :unsigned-int)
  (es	  :unsigned-int)
  (fs	  :unsigned-int)
  (gs	  :unsigned-int))

#+darwin
(defcstruct mcontext
  (es (:struct x86-exception-state))
  (ss (:struct x86-thread-state))
  (fs (:struct x86-float-state)))

(defcstruct sigaltstack
    )

#+darwin
(defcstruct ucontext
  (uc_onstack  :int)
  (uc_sigmask  sigset-t)		       ; signal mask
  (uc_stack    (:struct sigaltstack))	       ; stack
  (uc_link     (:pointer (:struct ucontext)))  ; pointer to resuming context
  (uc_mcsize   size-t)			       ; size of the machine context
  (uc_mcontext (:pointer (:struct mcontext)))) ; machine specific context
|#

(defconstant SIG_DFL  0 "Default action.")
(defconstant SIG_IGN  1 "Ignore the signal.")
(defconstant SIG_HOLD #+darwin 5 #+linux 2 #+freebsd 2 #+openbsd 3
	     "Hold on to the signal for later.")
(defconstant SIG_ERR -1 "Error?")

(defconstant SA_ONSTACK   #x0001 "Deliver on a stack, given with sigaltstack.")
(defconstant SA_RESTART   #x0002 "Restart system on signal return.")
(defconstant SA_RESETHAND #x0004 "Reset handler to SIG_DFL on delivery.")
(defconstant SA_NOCLDSTOP #x0008 "SIGCHLD only on process exit, not on stops.")
(defconstant SA_NODEFER   #x0010 "Don't mask the signal being delivered.")
(defconstant SA_NOCLDWAIT #x0020 "Don't create zombies. Wait returns ECHILD.")
(defconstant SA_SIGINFO   #x0040 "Deliver with sa_siginfo args.")

(defcfun sigaction :int (sig :int) (action :pointer) (old-action :pointer))

(defparameter *handler-actions*
  `((,SIG_DFL . :default) (,SIG_IGN . :ignore) (,SIG_HOLD . :hold)))

(defun action-to-handler (action)
  "Return the posix handler value for the ACTION keyword."
  (cond
    ((keywordp action)
     (let ((a (find action *handler-actions* :key #'cdr)))
       (or (and a (car a)) action)))
    ((symbolp action)
     (get-callback action))
    ((pointerp action)
     ;; assume it's a callback pointer already
     action)
    ;; (t
    ;;  ;; Perhaps we should error?
    ;;  action)
    ))
  
(defun handler-to-action (handler)
  "Return the action keyword for the posix HANDLER value."
  (let ((h (assoc handler *handler-actions*)))
    (if h (cdr h) handler)))

(defun signal-action (signal)
  "Return the action that given SIGNAL triggers."
  (with-foreign-object (old-action '(:struct foreign-sigaction))
    (syscall (sigaction signal (null-pointer) old-action))
    (let* ((ptr (foreign-slot-value
		 old-action '(:struct foreign-sigaction) 'sa_handler))
	   (num (pointer-address ptr)))
      (if (<= num SIG_HOLD)
	  (handler-to-action num)
	  ptr))))

;; Three different handler "types":
;;
;; (defcallback sigwinch-handler :void ((signal-number :int))
;; 	     )
;;
;; (defcallback sigwinch-handler :void ((signal-number :int)
;; 				     (:pointer (:struct foreign-siginfo))
;; 				     (:pointer :void))
;; 	     )
;;
;; ucontext_t is so hairy I haven't included it yet
;;
;; (defcallback sigwinch-handler :void ((signal-number :int)
;; 				     (:pointer (:struct foreign-siginfo))
;; 				     (:pointer (:struct ucontext_t))
;; 	     )

(defun set-signal-action (signal action)
  "Set the ACTION that given SIGNAL triggers. SIGNAL is a unix signal number
and ACTION is C callback, as would be defined by cffi:defcallback, or one of
the keywords: :DEFAULT :IGNORE :HOLD."
  (let ((handler (action-to-handler action)))
    (with-foreign-object (act '(:struct foreign-sigaction))
      (with-foreign-slots ((sa_handler sa_mask sa_flags)
			   act (:struct foreign-sigaction))
	(setf sa_handler (if (not (pointerp handler))
			     (make-pointer handler)
			     handler)
	      sa_flags 0)
	(sigemptyset (foreign-slot-pointer
		      act
		      '(:struct foreign-sigaction) 'sa_mask)))
      (syscall (sigaction signal act (null-pointer))))))

(defsetf signal-action set-signal-action
  "Set the ACTION that given SIGNAL triggers.")

(defun set-handlers (handler-list)
  (loop :for (signal . action) :in handler-list :do
     ;;(format t "set-handler ~s ~s~%" signal action)
     (set-signal-action signal action)))

(defmacro with-signal-handlers-from-value ((handler-list) &body body)
  "Evaluate the BODY with the signal handlers set as in HANDLER-LIST, with the
handers restored to their orignal values on return. HANDLER-LIST is a list
of (signal . action), as would be passed to SET-SIGNAL-ACTION."
  (with-unique-names (saved-list)
    `(let ((,saved-list
	    (loop
	       ;;:for (sig . act) :in ,evaled-list
	       :for item :in ,handler-list
	       ;;:do
	       ;;(format t "sig = ~s~%" sig)
	       ;;(format t "act = ~a~%" (signal-action sig))
	       ;;:collect (cons sig (signal-action sig)))))
	       :collect (cons (car item) (signal-action (car item))))))
       (unwind-protect
         (progn
	   (set-handlers ,handler-list)
	   ,@body)
	 (when ,saved-list
	   (set-handlers ,saved-list))))))

(defmacro with-signal-handlers (handler-list &body body)
  "Evaluate the BODY with the signal handlers set as in HANDLER-LIST, with the
handers restored to their orignal values on return. HANDLER-LIST is a list
of (signal . action), as would be passed to SET-SIGNAL-ACTION."
  (with-unique-names (evaled-list)
    `(let ((,evaled-list
	    ;; fake eval the list
	    (mapcar (_ (cons (typecase (car _)
				(symbol (symbol-value (car _)))
				(t (car _))) ; it had better be a signal number
			      (cdr _)))
		    ',handler-list)))
       (with-signal-handlers-from-value (,evaled-list)
	 ,@body))))

(defconstant +SIG-BLOCK+   0)
(defconstant +SIG-UNBLOCK+ 1)
(defconstant +SIG-SETMASK+ 2)

(defcfun ("sigprocmask" real-sigprocmask)
    :int (how :int) (set (:pointer sigset-t)) (oldset (:pointer sigset-t)))

;; (defcfun pthread-sigprocmask :int (how :int) (set (:pointer sigset-t))
;; 	 (oldset (:pointer sigset-t)))

(defun sigprocmask (how set oldset)
  ;; @@@ How are we supposed to know whether we should call pthread-sigprocmask
  ;; or just sigprocmask?
  (real-sigprocmask how set oldset))

(defmacro with-blocked-signals ((&rest signals) &body body)
  "Evaluate the BODY with the siganls in SIGNALS blocked."
  (with-unique-names (set sig)
    `(with-foreign-object (,set 'sigset-t)
       (sigemptyset ,set)
       (loop :for ,sig :in ,signals :do
	  (sigaddset ,set ,sig))
       (unwind-protect
	    (progn
	      (syscall (sigprocmask +SIG-BLOCK+ ,set))
	      ,@body)
	 (syscall (sigprocmask +SIG-UNBLOCK+ ,set))))))

(defmacro with-all-signals-blocked ((&rest signals) &body body)
  "Evaluate BODY with all signgals blocked except those in SIGNALS, which can be
NIL or left unspecified to block all blockable signals."
  (with-unique-names (set sig)
    `(with-foreign-object (,set 'sigset-t)
       (sigfillset ,set)
       (loop :for ,sig :in ,signals :do
	  (sigdelset ,set ,sig))
       (unwind-protect
	    (progn
	      (syscall (sigprocmask +SIG-BLOCK+ ,set))
	      ,@body)
	 (syscall (sigprocmask +SIG-UNBLOCK+ ,set))))))

(defun signal-mask ()
  "Return a list of signals blocked."
  (with-foreign-object (set 'sigset-t)
    (sigemptyset set)
    (syscall (sigprocmask +SIG-SETMASK+ (null-pointer) set))
    (loop :for i :from 0 :below *signal-count*
       :when (not (zerop (sigismember set i)))
       :collect i)))

(defun set-signal-mask (signals)
  "Set the list of signals blocked."
  (with-foreign-object (set 'sigset-t)
    (sigemptyset set)
    (loop :for sig :in signals
       :do (sigaddset set sig))
    (syscall (sigprocmask +SIG-SETMASK+ set (null-pointer)))))

(defsetf signal-mask set-signal-mask
  "Set the signal mask, which should be a list of signal numbers.")

(defun describe-signals ()
  "List the POSIX signals that are known to the operating system."
  (format t "#  SIG~11tDescription~42tDisposition~%~
             -- ---~11t-----------~42t-----------~%")
  (loop :for i :from 1 :below *signal-count*
        :do (format t "~2a ~:@(~7a~) ~30a ~a~%"
		   i (signal-name i) (signal-description i)
		   (if (not (find i '(9 17)))
		       (let ((act (signal-action i)))
			 (if (pointerp act)
			     (format nil "Handler #x~x" (pointer-address act))
			     act))
		       "N/A"))))

(defcfun ("kill" real-kill) :int (pid pid-t) (signal :int))

(defun kill (pid sig)
  ;;#+clisp (posix:kill pid sig)
  #| #+openmcl (#_kill pid sig) |#
  ;;#+ccl (syscall (real-kill pid sig))
  ;;#+cmu (unix:unix-kill pid sig)
  ;;#+sbcl (sb-unix:unix-kill pid sig)
  #+(or sbcl ccl clisp cmu) (syscall (real-kill pid sig))
  #-(or clisp openmcl cmu sbcl ccl) (declare (ignore pid sig))
  #-(or clisp openmcl cmu sbcl ccl) (missing-implementation 'kill))

(defcfun ("killpg" real-killpg) :int (process-group pid-t) (signal :int))
(defun killpg (process-group signal)
  "Send SIGNAL to PROCESS-GROUP."
  (syscall (real-killpg process-group signal)))

;(sb-sys:enable-interrupt sb-posix:sigwinch #'update-window-size)
;(defun update-window-size (sig code scp)
; (declare (ignore sig code scp))
;)

;; End
