;;
;; unix/processes.lisp - Unix interface to processes
;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

;#+(or darwin linux) (config-feature :os-t-has-vfork)

#+(or darwin linux freebsd openbsd)
;; It's partially untested if this actually works on Linux.
(progn
  (defconstant +WAIT-NO-HANG+    #x0001)
  (defconstant +WAIT-UNTRACED+   #x0002)
  (defconstant +WAIT-STOPPED+    #o0177) ;; #x7f
  (defconstant +WAIT-CORE-FLAG+  #o0200) ;; #x80

  (defun wait-status (s)	 (logand +WAIT-STOPPED+ s))
  (defun wait-if-exited (s)	 (= (wait-status s) 0))
;  (defun wait-exit-status (s)	 (ash s -8))
  (defun wait-exit-status (s)	 (ash (logand s #xff00) -8))
  (defun wait-if-signaled (s)	 (and (not (= (wait-status s) +WAIT-STOPPED+))
				      (not (= (wait-status s) 0))))
  (defun wait-if-stopped (s)	 (= (wait-status s) +WAIT-STOPPED+))
  (defun wait-termination-signal (s) (wait-status s))
  (defun wait-core-dump (s)	 (not (= 0 (logand s +WAIT-CORE-FLAG+))))
  (defun wait-stop-signal (s)	 (ash s -8)))

;; (defcstruct timeval
;;   (seconds time-t)
;;   (microseconds suseconds-t))

(defstruct rusage
  user
  system)

(defcfun ("getrusage" real-getrusage) :int (who :int)
	 (foreign-rusage-ptr (:pointer (:struct foreign-rusage))))

(defun getrusage (who)
  "Get resource usage. Return a struct TIMESPEC which has SECONDS and
MICRO-SECONDS."
  (let ((val (case who
	       (:self 0)
	       ((:kids :children) -1))))
    (with-foreign-object (ru '(:struct foreign-rusage))
      (syscall (real-getrusage val ru))
      (with-foreign-slots ((ru_utime ru_stime) ru (:struct foreign-rusage))
	(make-rusage
	 :user (make-timeval :seconds (getf ru_utime 'tv_sec)
			     :micro-seconds (getf ru_utime 'tv_usec))
	 :system (make-timeval :seconds (getf ru_stime 'tv_sec)
			       :micro-seconds (getf ru_stime 'tv_usec)))))))

(defun process-times (who)
  "Get CPU time for WHO, which is either :SELF or :CHILDREN. Return a four
integer values: seconds and microseconds of user time, seconds and microseconds
of system time."
  (let ((ru (getrusage who)))
    (values (timeval-seconds (rusage-user ru))
	    (timeval-micro-seconds (rusage-user ru))
	    (timeval-seconds (rusage-system ru))
	    (timeval-micro-seconds (rusage-system ru)))))

;#+os-t-has-vfork (defcfun ("vfork" fork) pid-t)
;#-os-t-has-vfork (defcfun ("fork" fork) pid-t)
(defcfun _exit :void (status :int))

(defcfun execvp :int (path :pointer) (args :pointer))
(defcfun execve :int (path :pointer) (args :pointer) (env :pointer))

(defun exec (path args &optional (env *real-environ*))
  "Replace this program with executable in the string PATH. Run the program
with arguments in ARGS, which should be a list of strings. By convention, the
first argument should be the command name. ENV is either a Lisp or 'C'
environment list, which defaults to the current 'C' environ variable."
  (declare (type string path) (type list args))
  (let ((argc (length args))
	(c-env (if (pointerp env) env (make-c-env env)))
	c-path c-args)
    (unwind-protect
      (progn
	(setf c-path (foreign-string-alloc path)
	      c-args (foreign-alloc :string :count (1+ argc)))
	(loop :for i :from 0 :below argc :do
	   (setf (mem-aref c-args :pointer i)
		 (foreign-string-alloc (elt args i))))
	(setf (mem-aref c-args :pointer argc) (null-pointer))
	(syscall (execve c-path c-args c-env)))
      ;; Clean up
      (when (and c-path (not (null-pointer-p c-path)))
	(foreign-free c-path))
      (when (and c-args (not (null-pointer-p c-args)))
	(loop :for i :from 0 :below argc :do
	   (foreign-free (mem-aref c-args :string)))
	(foreign-free c-args))
      (when (and c-env (pointerp c-env) (not (pointerp env)))
	(free-c-env c-env)))))

(defcfun ("wait" real-wait) pid-t (status :pointer))
(defcfun ("waitpid" real-waitpid) pid-t
  (wpid pid-t) (status :pointer) (options :int))
(defcfun ("wait4" real-wait4) pid-t (status :pointer) (options :int)
	 (rusage (:pointer (:struct foreign-rusage))))

(defun wait-return-status (status)
  "Given a status from wait, return two values: a status value and status code.
See the documentation for WAIT."
  (cond
    ((wait-if-exited status)
     ;;(format t ";; Exited ~a" (wait-exit-status status))
     (values (wait-exit-status status) :exited))
    ((wait-if-signaled status)
     ;; (format t ";; [~d Terminated ~d~a]~%"
     ;; 	 child-pid (wait-termination-signal status)
     ;; 	 (when (wait-core-dump status) " core dumped" ""))
     (values (wait-termination-signal status)
	     (if (wait-core-dump status)
		 :coredump
		 :signaled)))
    ((wait-if-stopped status)
     ;; (format t ";; [~d Stopped ~d]~%"
     ;; 	 child-pid (wait-stop-signal status))
     (values (wait-stop-signal status) :stopped))
    ;; We assume an error occured if it's not one of the above.
    (t
     (values *errno* :error))))

(defun wait ()
  "Wait for child processes to finish and return a value and the status.
Possible values of STATUS and VALUE are:
  :exited    exit code
  :signaled  signal number
  :stopped   signal number
  :coredump  signal number
  :error     error code
"
  (check-jobs t))
  ;; (let (pid)
  ;;   (with-foreign-object (status-ptr :int)
  ;;     (setf pid (syscall (real-wait status-ptr)))
  ;;     (let ((status (mem-ref status-ptr :int)))
  ;; 	(wait-return-status status))))

(defcfun ("fork" posix-fork) pid-t)

(defun fork ()
  #+sbcl (sb-sys:without-gcing
	     (posix-fork)
	     ;;(sb-posix:fork)
	   )
;  #+sbcl (sb-sys:without-gcing (sb-posix:fork))
  #-sbcl (posix-fork))

;; SBCL:
;;
;; On darwin we have to deal with "mach" bullshit.
; #+darwin (defcfun setup-mach-exceptions :void)
; #+darwin (defun fork ()
; 	   (let ((pid (posix-fork)))
; 	     (when (= pid 0)
; 	       (setup-mach-exceptions))
; 	     pid))
;; FAILS!
;;
;; see sbcl/src/code/run-program.lisp
;; (without-gcing (spawn ....))

;; Hmmm, see:
;; stumpwm-0.9.7/contrib/sbclfix.lisp

(defun wait-and-report (child-pid)
  #-clisp
  (with-foreign-object (status-ptr :int 1)
    (setf (mem-ref status-ptr :int) 0)
    ;(format t "About to wait for ~d~%" child-pid)
    (let ((status 0) (wait-pid nil))
      (declare (ignorable status))
      (loop
	 :do (setf wait-pid (real-waitpid child-pid status-ptr 0))
	 :while (/= wait-pid child-pid)
	 :do
	 (format t "Back from wait wait-pid = ~d~%" wait-pid)
	 (if (= wait-pid -1)
	     (if (= *errno* +ECHILD+)
		 (progn
		   ;;(format t "Nothing to wait for~%")
		   (return-from nil nil))
		 (error-check wait-pid))
	     (setf status (mem-ref status-ptr :int)))
	 (format t "status = ~d~%" status)
	 (when (/= wait-pid child-pid)
	   (format t "Wait pid ~a doesn't match child pid ~a.~%"
		   wait-pid child-pid)))
      (cond
	((wait-if-exited status)
	 (wait-exit-status status))
	((wait-if-signaled status)
	 (format t ";; [~d Terminated ~d~a]~%"
		 child-pid (wait-termination-signal status)
		 (when (wait-core-dump status) " core dumped" ""))
	 (wait-termination-signal status))
	((wait-if-stopped status)
	 (format t ";; [~d Stopped ~d]~%"
		 child-pid (wait-stop-signal status))))))

  #+clisp ;; the old version I have now
  (declare (ignore child-pid))
  #+clisp
  (with-foreign-object (status-ptr :int 1)
    (setf (mem-ref status-ptr :int) 0)
;    (let ((wait-pid (waitpid child-pid status-ptr 0))
    (let ((wait-pid (real-wait status-ptr))
	  status)
      (when (and (= wait-pid -1) (/= *errno* +ECHILD+))
	(error-check wait-pid))
      (setf status (mem-ref status-ptr :int))
;      (format t "status = ~d~%" status)
      ))

  #+(and clisp a-version-in-the-future)
  (multiple-value-bind (pid code value)
      (posix:wait :pid child-pid)
    (case key
      (:exited    value)
      (:signaled  (format t ";; [~d Terminated ~d]~%" child-pid value))
      (:stopped   (format t ";; [~d Stopped ~d]~%" child-pid value))
      (:continued (format t ";; [~d Continued]~%" child-pid))
      (otherwise  (format t ";; [~d Unknown wait status ~d!]~%"
			  child-pid value))))
  )

(defun fork-and-exec (cmd &optional args (environment nil env-p))
  (let* ((cmd-and-args (cons cmd args))
	 (argc (length cmd-and-args))
	 child-pid)
    (with-foreign-object (argv :pointer (1+ argc))
      (with-foreign-string (path cmd)
	(loop :with i = 0
	      :for arg :in cmd-and-args :do
	      (setf (mem-aref argv :pointer i) (foreign-string-alloc arg))
	      (incf i))
	(setf (mem-aref argv :pointer argc) (null-pointer))
	(setf child-pid (fork))
	(when (= child-pid 0)
	  ;; in the child
	  (progn
;   	    (format t "About to exec ~s ~s~%"
;   		    (foreign-string-to-lisp path)
;   		    (loop :for i :from 0 :below argc
;   			  :collect (mem-aref argv :string i)))
;	    (when (= (execvp path argv) -1)
	    ;; @@@ or we could call the lisp exec?
	    (when (= (execve path argv (if env-p
					   (make-c-env environment)
					   (real-environ)))
		     -1)
	      (write-string "Exec of ")
	      (write-string cmd)
	      (write-string " failed")
	      (write-char #\newline)
;	      (format t "Exec of ~s failed: ~a ~a~%" cmd
;		      *errno* (strerror *errno*))
;	      (force-output)
	      (_exit 1))))
	;; in the parent
	(error-check child-pid)
	(wait-and-report child-pid)))))

(defcfun getpid pid-t)
(defcfun getppid pid-t)

;; Just in case you didn't know, or forgot, here's a little background
;; these rather obscure system calls. The man pages don't really explain it.
;;
;; This is what you do in a job control shell to boss around a bunch of
;; processes, in foreground, background ^Z and all that.

(defcfun setpgid :int (pid pid-t) (pgid pid-t))
(defcfun getpgid :int (pid pid-t))

;; These are used when you are making a new terminal (or session), and want to
;; be in control of it, like in a terminal window (xterm) with ptys or with
;; real terminal devices in the old fashioned getty. Also good for detaching.
(defcfun setsid :int)
(defcfun getsid :int (pid pid-t))

;; Perhaps we should provide something high level like "run in pty" and
;; or "detach process".

#+linux
(defun get-process-command-line (&optional (pid (getpid)))
  (flet ((read-an-arg (stm)
	   "Mostly for de-indentation"
	   (with-output-to-string (str)
	     (loop :with c
		:while (and (setf c (read-char stm nil nil))
			    (not (zerop (char-code c))))
		:do (princ c str)))))
    (with-open-file (stm (s+ "/proc/" pid "/cmdline"))
      (apply #'vector
	     (loop :with s
		:while (not (zerop (length (setf s (read-an-arg stm)))))
		:collect s)))))

#+darwin
(progn
  ;; Kernel process filter types
  (defconstant +KERN-PROC-ALL+	   0) ; everything
  (defconstant +KERN-PROC-PID+	   1) ; by process id		 (pid_t)
  (defconstant +KERN-PROC-PGRP+	   2) ; by process group id	 (pid_t)
  (defconstant +KERN-PROC-SESSION+ 3) ; by session of pid	 (pid_t)
  (defconstant +KERN-PROC-TTY+	   4) ; by controlling tty	 (dev_t)
  (defconstant +KERN-PROC-UID+	   5) ; by effective uid	 (uid_t)
  (defconstant +KERN-PROC-RUID+	   6) ; by real uid		 (uid_t)
  (defconstant +KERN-PROC-LCID+	   7) ; by login context id	 (uid_t)

  (defparameter *proc-retry-count* 100
  "How many time to retry getting the process list before failing.")

  (defparameter *process-list-fudge* 10
    "How many extra items to allocate in the process list."))

(defstruct unix-process
  "Information about a unix process."
  (id		   0 :type integer)
  (parent-id	   0 :type integer)
  (group-id	   0 :type integer)
  (user-id	   0 :type integer)
  terminal
  (text-size	   0 :type integer)
  (resident-size   0 :type integer)
  percent-cpu
  (nice-level	   0 :type integer)
  usage
  command
  (args #() :type vector))

(defvar *system-process-type* 'unix-process
  "Type of system specific process information.")

(defun system-process-info (id)
  "Return the system-process-type for the given PID, or NIL if we can't get it
for some reason, most likely becuase the process doesn't exist anymore."
  #+linux
  (let (proc line pid raw-line open-pos close-pos cmd uid)
    (handler-case
	(labels
	    ((pos (p)
	       "Adusted element in stat line so we can use documented indices."
	       (elt line (- p 2))))
	  (with-open-file (stm (s+ "/proc/" id "/stat"))
	    (setf raw-line (read-line stm)
		  open-pos (position #\( raw-line)
		  close-pos (position #\) raw-line :from-end t)
		  line (split-sequence
			#\space (subseq raw-line (+ 2 close-pos)))
		  cmd (subseq raw-line (position #\( raw-line) close-pos)
		  pid (etypecase id
			(integer id)
			(string (parse-integer id)))
		  uid (file-status-uid (stat (s+ "/proc/" id)))
		  proc (make-unix-process
			:id pid
			:parent-id (parse-integer (pos 3))
			:group-id (parse-integer (pos 4))
			:user-id uid
			:terminal (parse-integer (pos 6))
			:text-size (parse-integer (pos 22))
			:resident-size (parse-integer (pos 23))
			:percent-cpu 0
			:nice-level (parse-integer (pos 18))
			:usage nil
			:command (subseq raw-line (1+ open-pos) close-pos)
			:args (or (ignore-errors (get-process-command-line pid))
				  cmd)))))
      (file-error () nil))
    proc)
  #-linux
  (declare (ignore id)))

(defun system-process-list ()
  #+darwin
  ;; The MIB should look like:
  ;;   mib[0] = CTL_KERN;
  ;;   mib[1] = KERN_PROC;
  ;;   mib[2] = what;
  ;;   mib[3] = flag;
  ;; where 'what' is one of:
  ;;   KERN_PROC_PGRP      pid_t
  ;;   KERN_PROC_PID       pid_t
  ;;   KERN_PROC_RUID      uid_t
  ;;   KERN_PROC_SESSION   pid_t
  ;;   KERN_PROC_TTY       dev_t
  ;;   KERN_PROC_UID       uid_t
  ;;   KERN_PROC_ALL       0
  ;; and flag points to an array of the appropriate type.
  (let* ((start-mib (sysctl-name-to-mib "kern.proc"))
	 (mib-len (+ 2 (length start-mib)))
	 list-count
	 real-list-size
	 proc-list) #| (filter +KERN-PROC-ALL+) |#
    (with-foreign-objects ((mib :int mib-len)
			   (list-size 'size-t)
      			   (new-list-size 'size-t))
      ;; Copy from the start-MIB to the MIB
      (loop :for i :from 0 :below (length start-mib)
	 :do (setf (mem-aref mib :int i) (aref start-mib i)))
      ;; Add the filter parameters
      (setf (mem-aref mib :int (- mib-len 2)) +KERN-PROC-ALL+
	    (mem-aref mib :int (- mib-len 1)) 0)
      ;; (format t "mib-len = ~d mib = #~a~%" mib-len
      ;; 	 (loop :for i :from 0 :below 4 :collect (mem-aref mib :int i)))
      (unwind-protect
        (progn
	  ;; This has a horrible race condition! We get the size of the
	  ;; process list with one system call, which we have to allocate
	  ;; space for, then we try to get the actual list in a subsequent
	  ;; call. The problem is, the size of the list could have grown, by
	  ;; anything forking more processes, which is seems rather
	  ;; likely. Then we get an error because the list can't fit in the
	  ;; space we allocated. So we have to go back and ask for the size of
	  ;; the list again, which could still be too small by the time we,
	  ;; ask again, ad infinitum.
	  ;;
	  ;; It seems like the kernel could just build the list on some pages,
	  ;; and pop them over to user space when it's done. Then we could
	  ;; free it. How hard is that?
	  ;;
	  ;; Anyway, we add *process-list-fudge* to what's returned to us, in
	  ;; hopes that it will help. We try in a loop a *proc-retry-count*
	  ;; times before we fail.
	  (loop :with i = 0
	   :do
	     ;; Get the size of the process list
	     (syscall (real-sysctl mib mib-len (null-pointer) list-size
				   (null-pointer) 0))
	     ;; (format t "list-size = ~d~%"
	     ;; 	     (/ (mem-ref list-size 'size-t)
	     ;; 		(cffi:foreign-type-size
	     ;; 		 '(:struct foreign-kinfo-proc))))
	     ;; It's returned in bytes, so 
	     (setf list-count (+ *process-list-fudge*
				 (/ (mem-ref list-size 'size-t)
				    (cffi:foreign-type-size
				     '(:struct foreign-kinfo-proc))))
		   proc-list (foreign-alloc '(:struct foreign-kinfo-proc)
					    :count list-count)
		   (mem-ref new-list-size 'size-t)
		   (* list-count (cffi:foreign-type-size
				  '(:struct foreign-kinfo-proc))))
	     ;; Get the real list
	     (syscall (real-sysctl mib mib-len proc-list new-list-size
				   (null-pointer) 0))
	     (setf real-list-size (/ (mem-ref new-list-size 'size-t)
				     (cffi:foreign-type-size
				      '(:struct foreign-kinfo-proc))))
	     :until (or (> real-list-size 0) (> i *proc-retry-count*))
	     :do (incf i)
	     (foreign-free proc-list)
	     (sleep (/ (random 10) 1000))) ; horrible!
	  (loop :with p :and ep :and eep
	     :for i :from 0 :below real-list-size
	     :do
	     (setf p (mem-aptr proc-list '(:struct foreign-kinfo-proc) i))
	     :while (not (null-pointer-p p))
	     :do
	     (setf ep (foreign-slot-pointer
		       p '(:struct foreign-kinfo-proc) 'kp_proc))
	     (setf eep (foreign-slot-pointer
			p '(:struct foreign-kinfo-proc) 'kp_eproc))
	     :collect
	     (with-foreign-slots
		 ((p_flag p_stat p_pid p_pctcpu p_nice p_comm p_pgrp)
		  ep (:struct foreign-extern-proc))
	       (with-foreign-slots
		   ((e_ppid e_pgid e_tdev e_xsize e_xrssize)
		    eep (:struct foreign-eproc))
		 (make-unix-process
		  :id p_pid
		  :parent-id e_ppid
		  :group-id e_pgid
		  :terminal e_tdev
		  :text-size e_xsize
		  :resident-size e_xrssize
		  :percent-cpu p_pctcpu
		  :nice-level p_nice
		  :usage nil
		  :command (foreign-string-to-lisp p_comm :max-chars 16)
		  :args #())))))
	(foreign-free proc-list))))
  #+linux
  (loop :with proc
     :for p :in (read-directory :dir "/proc/")
     :when (every #'digit-char-p p)
     :if (setf proc (system-process-info p)) ; We can just pass the string.
     :collect proc))

;; @@@ Anytime we do this it's probably wasteful.
(defun unix-to-os-process (proc)
  "Convert a unix-process into an os-process."
  (make-os-process
   :id        (unix-process-id proc)
   :parent-id (unix-process-parent-id proc)
   :user      (user-name (unix-process-user-id proc))
   :size      (unix-process-resident-size proc)
   :name      (unix-process-command proc)))

(defun process-info (pid)
  (unix-to-os-process (system-process-info pid)))

(defun process-list ()
  "Make an OS-PROCESS list from a UNIX-PROCESS list."
  ;; @@@ We could make it faster by just getting the data neeeded.
  (mapcar #'unix-to-os-process (system-process-list)))

(defun current-process-id ()
  (getpid))

(defun suspend-process (&optional id)
  "Suspend the process with the given ID. If ID is NIL or not given, suspend
the current process."
  (kill (or id (getpid)) +SIGSTOP+))

(defun resume-process (id)
  "Resume the suspended process with the given ID."
  (kill id +SIGCONT+))

(defun terminate-process (id)
  "Terminate the process with the given ID."
  ;; If you're really doing a hard core unix type thing you'll probably already
  ;; be using unix:kill, and so can use SIGKILL.
  (kill id +SIGTERM+))

;; setpriority

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; job control thingys

(defclass unix-process-handle (process-handle)
  ()
  (:documentation "A unix process handle, i.e. a PID."))

(defvar *got-sigwinch* nil
  "True after we received a SIGWINCH.")

(defvar *got-tstp* nil
  "True after we received a SIGTSTP.")

(defcallback sigwinch-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  (setf *got-sigwinch* t))

(defcallback tstp-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  (setf *got-tstp* t))

#+sbcl
(defun turkey (program)
  (let (pp #| job |#)
    (setf (signal-action +SIGTSTP+) 'tstp-handler)
    (setf pp (sb-ext:run-program program '() :input t :output t))
    ;; Take the terminal back.
    (syscall (tcsetpgrp 0 (getpid)))
    ;; Suspend us again if we get a ^Z.
    (setf (signal-action +SIGTSTP+) :default)
    ;;(setf job (add-job program "" (process-pid pp)))
    ;;(setf (job-pid job) pp)
    pp))

#+sbcl
(defun re-turkey (pp)
  (let ((pid (sb-ext:process-pid pp)))
    (syscall (tcsetpgrp 0 pid))
    ;; Ignore terminal suspend signals.
    (setf (signal-action +SIGTSTP+) 'tstp-handler)
    (syscall (kill pid +SIGCONT+))
    (sb-ext:process-wait pp t)
    ;; Take the terminal back.
    (syscall (tcsetpgrp 0 (getpid)))
    ;; Suspend us again if we get a ^Z.
    (setf (signal-action +SIGTSTP+) :default)))

(defun take-the-terminal-back ()
  (syscall (tcsetpgrp 0 (getpgid (getpid))))
  (syscall (tcsetpgrp 1 (getpgid (getpid))))
  (setf (signal-action +SIGTSTP+) :default))

(defun wait-and-chill (handle)
  (let ((child-pid (etypecase handle ;; Can be called with a handle or a pid.
		     (process-handle (process-handle-value handle))
		     (integer handle)))
	(status 0) (wait-pid 0))
    (declare (ignorable status))
    (dbugf :sheep "wait-and-chill ~s ~s~%" handle child-pid)
    
    ;; Make the child be in it's own process group.
    ;;(syscall (setpgid child-pid child-pid))
    ;; Make the terminal signals go to the child's group.
    ;;(syscall (tcsetpgrp 0 child-pid))
    ;; Ignore terminal suspend signals.
    (setf (signal-action +SIGTSTP+) 'tstp-handler)

    (unwind-protect
	 (with-foreign-object (status-ptr :int 1)
	   (setf (mem-ref status-ptr :int) 0)
	   ;;(format t "About to wait for ~d~%" child-pid)
	   (loop
	      ;; +WAIT-UNTRACED+ is so it will return when ^Z is pressed
	      :while (/= wait-pid child-pid)
	      :do
	      (setf wait-pid
		    (real-waitpid child-pid status-ptr +WAIT-UNTRACED+))
	      (dbugf :sheep "Back from wait wait-pid = ~s ~s~%"
		     wait-pid *errno*)
	      (if (= wait-pid -1)
		  (if (= *errno* +ECHILD+)
		      (progn
			;;(format t "Nothing to wait for~%")
			(return-from nil nil))
		      (error-check wait-pid))
		  (setf status (mem-ref status-ptr :int)))
	      ;;(format t "status = ~d~%" status)
	      (dbugf :sheep "~a~%"
		     (if (/= wait-pid child-pid)
			 (format nil "Wait pid ~a doesn't match child pid ~a."
				 wait-pid child-pid)
			 ""))
	      #| (finish-output) |#
	      ))
      ;; Take the terminal back.
      (syscall (tcsetpgrp 0 (getpgid (getpid))))
      ;; Suspend us again if we get a ^Z.
      (setf (signal-action +SIGTSTP+) :default)
      (dbugf :sheep "Took the terminal back in wait-and-chill?~%"))
    (wait-return-status status)))

(defvar *setpgid-err-len*)
(defvar *setpgid-err* "child setpgid fail~%")
(defvar *tcsetpgrp-err-len*)
(defvar *tcsetpgrp-err* "child tcsetpgrp fail~%")

(defun %make-error-messages ()
  (when (not *setpgid-err-len*)
    (setf *setpgid-err-len* (length *setpgid-err*)
	  *setpgid-err* (foreign-string-alloc *setpgid-err*)
	  *tcsetpgrp-err-len* (length *tcsetpgrp-err*)
	  *tcsetpgrp-err* (foreign-string-alloc *tcsetpgrp-err*))))

(defun forky (cmd args &key (environment nil env-p) background)
  (let* ((cmd-and-args (cons cmd args))
	 (argc (length cmd-and-args))
	 child-pid err-msg-str err-msg-len)
    (setf err-msg-str (s+ "Exec of " cmd " failed." #\newline)
	  err-msg-len (length err-msg-str))
    (with-foreign-object (argv :pointer (1+ argc))
      (with-foreign-strings ((path cmd) (err-msg err-msg-str))
	(loop :with i = 0
	      :for arg :in cmd-and-args :do
	      (setf (mem-aref argv :pointer i) (foreign-string-alloc arg))
	      (incf i))
	(setf (mem-aref argv :pointer argc) (null-pointer))
	(setf child-pid (fork))
	(when (= child-pid 0)
	  ;; in the child
	  (progn
	    ;; (setf (signal-action +SIGTSTP+) :default)
	    ;; (setf (signal-action +SIGTTIN+) :default)
	    ;; (setf (signal-action +SIGTTOU+) :default)
	    ;; (setf (signal-action +SIGCHLD+) :default)
	    ;; Make the child be in it's own process group.
	    ;; We have to do this here in the child because on Linux
	    ;; the parent won't be allowed to do it after the exec.
	    ;;(when (= -1 (setpgid (getpid) (getpid)))
	    (when (= -1 (setpgid 0 0))
	      (posix-write 1 *setpgid-err* *setpgid-err-len*))
	    (when (not background)
	      ;; @@@ This is not exactly right. It should be the whatever
	      ;; file descriptor is the controling terminal, not necessarily
	      ;; stdin a.k.a. 0, (although it usually is.)
	      ;;(when (= -1 (tcsetpgrp 0 (getpid)))
	      (when (= -1 (tcsetpgrp 2 (getpid)))
	    	(posix-write 1 *tcsetpgrp-err* *tcsetpgrp-err-len*)))
;   	    (format t "About to exec ~s ~s~%"
;   		    (foreign-string-to-lisp path)
;   		    (loop :for i :from 0 :below argc
;   			  :collect (mem-aref argv :string i)))
;	    (when (= (execvp path argv) -1)
	    ;; @@@ or we could call the lisp exec?
	    (when (= (execve path argv (if env-p
					   (make-c-env environment)
					   (real-environ)))
		     -1)
	      ;; (write-string "Exec of ")
	      ;; (write-string cmd)
	      ;; (write-string " failed")
	      ;; (write-char #\newline)
	      (posix-write 1 err-msg err-msg-len)
;	      (format t "Exec of ~s failed: ~a ~a~%" cmd
;		      *errno* (strerror *errno*))
;	      (force-output)
	      (_exit 1))))
	;; in the parent
	(error-check child-pid)
	(make-instance 'unix-process-handle :value child-pid)))))

(defun resume-background-pid (pid)
  "Put the process PID back in the foreground."
  ;; Make the terminal signals go to the child's group.
  (syscall (tcsetpgrp 0 pid))
  ;; Ignore terminal suspend signals.
  (setf (signal-action +SIGTSTP+) 'tstp-handler)
  (syscall (kill pid +SIGCONT+))
  (wait-and-chill pid))

(defun background-pid (pid)
  "Put the process PID back in the background."
  (killpg (getpgid pid) os-unix:+SIGCONT+)
  (kill pid os-unix:+SIGCONT+))

(defun check-jobs (&optional hang)
  "Check if any sub-processes have changed status. Returns three values.
The PID of the process that changed, and the RESULT and STATUS as returned by
wait. Returns NILs if nothing changed."
  (let (pid int-status)
    (with-foreign-object (status-ptr :int 1)
      (setf (mem-ref status-ptr :int) 0
	    pid
	    (if hang
		(real-wait status-ptr)
		(real-waitpid -1 status-ptr (logior +WAIT-UNTRACED+
						   +WAIT-NO-HANG+))))
		;; (real-waitpid 0 status-ptr (logior +WAIT-UNTRACED+
		;; 				   +WAIT-NO-HANG+))))
      (cond
	((< pid 0)
	 (if (= *errno* +ECHILD+)
	     (progn
	       ;;(format t "Nothing to wait for~%")
	       (return-from check-jobs (values nil nil nil)))
	     (error-check pid)))
	((= pid 0)
	 ;;(format t "Nothing to report ~a~%" pid)
	 (values nil nil nil))
	((> pid 0)
	 ;;(format t "Something to report!~%")
	 (setf int-status (mem-ref status-ptr :int))
	 (multiple-value-bind (result status)
	     (wait-return-status int-status)
	   (values pid result status)))))))

;; End
