;;;
;;; unix/processes.lisp - Unix interface to processes
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

;#+(or darwin linux) (config-feature :os-t-has-vfork)

#+(or darwin linux freebsd openbsd netbsd)
;; It's partially untested if this actually works on Linux.
(progn
  (defconstant +WAIT-NO-HANG+    #x0001)
  (defconstant +WAIT-UNTRACED+   #x0002)
  (defconstant +WAIT-STOPPED+    #o0177) ;; #x7f (this is actually flag)
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

(defvar *post-fork-hook* nil
  "A list functions to call in the child after forking. The functions are called
without arguments.")

(defun fork ()
  (let ((pid
	 (#+sbcl sb-sys:without-gcing	; @@@ Is this still useful/necessary?
	  #-sbcl progn			; Should it be around more stuff?
	  (posix-fork))))
    (when (zerop pid)
      (loop :for f :in *post-fork-hook*
	    :do (funcall f)))
    pid))

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
	;; (posix-close 
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

;; @@@ We should provide something high level like "run in pty" and
;; or "detach process".

(defun simple-write (message)
  "Print the string MESSAGE simply."
  (cffi:with-foreign-string ((str bytes) message)
    (posix-write 2 str bytes)))

(defun punt (message)
  "Print the string MESSAGE simply and exit the process."
  (simple-write message)
  (_exit 1))

(defun connect-pipes (in-fd out-fd &optional err-fd)
  "Connect the IN-FD, OUT-FD, and ERR-FD to the process standard input, output
and error, respectively. Print a message and exit quick if we fail."
  ;; This is expected to be called from a forked child before exec, so we try
  ;; to use simple error messages, and quick exiting.
  (declare (ignore err-fd)) ;; @@@ hol up
  (macrolet ((dup-it (from to)
	       `(progn
		  (when ,from
		    (when (minusp (posix-dup2 ,from ,to))
		      (punt (s+ "dup2 of " ,to " failed: " *errno* " "
				(error-message *errno*) #\newline)))
		    ;; (when (posix-close ,from)
		    ;;   (punt (s+ "close of " ,from " failed: " *errno* " "
		    ;; 		(error-message *errno*) #\newline)))
		    ))))
    (dup-it in-fd 0)
    (dup-it out-fd 1)
    ;; @@@ don't do this yet
    ;; (dup-it (or err-fd 1) 2)
    ))

(defun pipe-fork (command args environment func)
  "Fork and exec COMMAND with ARGS and ENV, call FUNC before in the child
before execing."
  (let* ((cmd-and-args (cons command args))
	 (argc (length cmd-and-args))
	 child-pid err-msg-str err-msg-len)
    (setf err-msg-str (s+ "Exec of " command " failed." #\newline)
	  err-msg-len (length err-msg-str))
    (with-foreign-object (argv :pointer (1+ argc))
      (with-foreign-strings ((path command) (err-msg err-msg-str))
	(loop :with i = 0
	      :for arg :in cmd-and-args :do
	      (setf (mem-aref argv :pointer i) (foreign-string-alloc arg))
	      (incf i))
	(setf (mem-aref argv :pointer argc) (null-pointer))
	(setf child-pid (fork))
	(when (= child-pid 0)
	  (progn
	    (funcall func)
	    (simple-write (s+ "about to exec" #\newline))
	    (when (= (execve path argv (if environment
					   (make-c-env environment)
					   (real-environ)))
		     -1)
	      (posix-write 1 err-msg err-msg-len)
	      (_exit 1))))
	;; in the parent
	(error-check child-pid)
	child-pid))))

(define-condition pipe-stream-type (simple-error cell-error)
  ()
  (:documentation "A pipe stream was not the right type."))

(defun fork-with-pipes (cmd args &key in-stream (out-stream :stream)
				   ;;(environment nil env-p)
				   environment
				   )
  "Fork and exec CMD with ARGS and ENVIRONMENT, posibly piping to IN-STREAM and
OUT-STREAM. See pipe-program. Return an output stream and a process ID."
  (let (in-stream-write-side
	in-stream-read-side
        out-stream-write-side
	out-stream-read-side
	;; (our-pg (getpgid 0))
	child-pid
	(tty-p (file-handle-terminal-p 2)))
    (when (and in-stream (streamp in-stream))
      (when (not (typep in-stream 'os-output-stream))
	(cerror "Provide a stream."	; @@@ probably no-one ever
		'pipe-stream-type
		:name 'in-stream
		:format-control
		"IN-STREAM must be an OS-OUTPUT-STREAM, not a ~s."
		:format-args `(,(type-of in-stream))))
      (setf (values in-stream-read-side in-stream-write-side) (posix-pipe)
	    (os-stream-handle in-stream) in-stream-write-side))
    (when out-stream
      (when (streamp out-stream)
	(when (not (typep out-stream 'os-input-stream))
	  (cerror "Provide a stream."
		  'pipe-stream-type
		  :name 'out-stream
		  :format-control
		  "OUT-STREAM must be an OS-INPUT-STREAM, not a ~s."
		  :format-args `(,(type-of out-stream)))))
      (when (eq out-stream :stream)
	(setf out-stream
	      ;; @@@ need to handle element-type! not just 'character
	      (make-instance
	       'unix-character-input-stream
	       ;; (os-stream-system-type
	       ;; 	(os-stream-type-for :output 'character)
	       )))
      (setf (values out-stream-read-side out-stream-write-side) (posix-pipe)
	    (os-stream-handle out-stream) out-stream-read-side))
    (format t "out-read-side ~s out-write-side ~s~%"
	    out-stream-read-side out-stream-write-side)
    (format t "in-read-side ~s in-write-side ~s~%"
	    in-stream-read-side in-stream-write-side)

    ;; Set the sides we don't use to close in the child.
    (when out-stream-write-side
      (syscall (fcntl out-stream-write-side
		      uos::+F_SETFD+ :int uos::+FD_CLOEXEC+)))
    (when in-stream-read-side
      (syscall (fcntl in-stream-read-side
		      uos::+F_SETFD+ :int uos::+FD_CLOEXEC+)))
    (setf (signal-action +SIGTSTP+) :ignore)
    (setf (signal-action +SIGTTIN+) :ignore)
    (setf (signal-action +SIGTTOU+) :ignore)
    (setf child-pid
	  (pipe-fork cmd args environment
	       #'(lambda ()
		   (connect-pipes in-stream-read-side out-stream-write-side)
		   (setf (signal-action +SIGTSTP+) :default)
		   (setf (signal-action +SIGTTIN+) :default)
		   (setf (signal-action +SIGTTOU+) :default)
		   (setf (signal-action +SIGCHLD+) :default)
		   (when (= -1 (setpgid 0 0))
		     (punt (s+ "child setpgid failed "
		   	       *errno* " " (error-message *errno*) #\newline)))
		   ;;(when (not background)
		     ;; @@@ This is not exactly right. It should be the
		     ;; whatever file descriptor is the controling terminal,
		     ;; not necessarily stdin a.k.a. 0, (although it usually
		   ;; is.)
		   (with-signal-handlers ((+SIGTSTP+ . :ignore)
					  (+SIGTTIN+ . :ignore)
					  (+SIGTTOU+ . :ignore))
		     ;; (when (= -1 (tcsetpgrp 0 (getpid)))
		     (when (and tty-p (= -1 (tcsetpgrp 2 (getpid))))
		       (punt (s+ "child tcsetpgrp failed "
				 *errno* " " (error-message *errno*)
				 #\newline))))
		     ;;)
		   )))
    ;; This should be our terminal, not 2.
    ;; (syscall (tcsetpgrp 2 child-pid))
    ;; Close the sides the parent doesn't use.
    (when out-stream-write-side
      (syscall (posix-close out-stream-write-side)))
    (when in-stream-read-side
      (syscall (posix-close in-stream-read-side)))
    (values out-stream child-pid)))
    
(defun pipe-program (cmd args &key in-stream (out-stream :stream)
				;; (environment nil env-p)
				environment
				)
  "Return an input stream with the output of the system command.
  IN-STREAM can be:
    An OS-OUTPUT-STREAM which can written to to supply input to the process.
    NIL to use the current processes standard input.
  OUT-STREAM can be:
    :STREAM make a new input stream containing the processes output.
    NIL to use the current processes standard output.
  ENVIRONMENT is:
    a list of strings of the form NAME=VALUE to be used as the process's
    environment. If ENVIRONMENT is not provided, it defaults to the current
    process's environment."
  (fork-with-pipes cmd args
		   :in-stream in-stream
		   :out-stream out-stream
		   :environment environment))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +PRIO-PROCESS+ 0 "A proccess ID.")
  (defconstant +PRIO-PGRP+    1 "A proccess group ID.")
  (defconstant +PRIO-USER+    2 "A user ID.")
  (defconstant +PRIO-MIN+    -20 "Minimum priority (most favorable).")
  (defconstant +PRIO-MAX+     20 "Maximum priority (least favorable)."))

(defvar *os-process-most-favorable-priority* +PRIO-MIN+
  "The process priority value for most favorable scheduling.")

(defvar *os-process-least-favorable-priority* +PRIO-MAX+
  "The process priority value for least favorable scheduling.")

(defcfun getpriority :int
  "Get the process scheduling priority."
  (which :int) (who uid-t))

(defcfun setpriority :int
  "Set the process scheduling priority. Range is from 19 lowest to -20 highest."
  (which :int) (who uid-t) (priority :int))

;; Warning: very un-hygenic.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-priority-args ((user pid group) &body body)
    `(let* ((args `((,,user  . ,+PRIO-USER+)
		    (,,pid   . ,+PRIO-PROCESS+)
		    (,,group . ,+PRIO-PGRP+)))
	    (val (count-if-not #'null args :key #'car))
	    who which)
       (cond
	((> val 1)
	 (error "Please only supply one of USER, PID, or GROUP."))
	((< val 1)
	 (setf who (uos:getpid)
	       which +PRIO-PROCESS+)
	 ;;(error "Plese supply one of USER, PID, or GROUP.")
	 )
	(t
	 (let* ((ww (find-if-not #'null args :key #'car)))
	   (setf who (car ww)
		 which (cdr ww)))))
       ;; (format t "args = ~s~%who = ~s which = ~s~%" args who which)
       ,@body)))

(defun os-process-priority (&key user pid group)
  (let (result)
    (with-priority-args (user pid group)
      (setf (%errno) 0 ;; Set errno to 0, so fucking stupid.
            result (getpriority which who))
      (when (and (= result -1) (not (zerop (errno))))
	(error-check result))
      result)))

(defun set-os-process-priority (priority &key user pid group)
  (with-priority-args (user pid group)
    (syscall (setpriority which who priority))))

(defsetf os-process-priority (&rest args &key user pid group &allow-other-keys)
  (value)
  (declare (ignorable user pid group))
  `(apply #'set-os-process-priority ,value (list ,@args)))

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
    (when (file-handle-terminal-p 0)
      (syscall (tcsetpgrp 0 (getpid))))
    ;; Suspend us again if we get a ^Z.
    (setf (signal-action +SIGTSTP+) :default)
    ;;(setf job (add-job program "" (process-pid pp)))
    ;;(setf (job-pid job) pp)
    pp))

#+sbcl
(defun re-turkey (pp)
  (let ((pid (sb-ext:process-pid pp))
	(tty-p (file-handle-terminal-p 0)))
    (when tty-p
      (syscall (tcsetpgrp 0 pid)))
    ;; Ignore terminal suspend signals.
    (setf (signal-action +SIGTSTP+) 'tstp-handler)
    (syscall (kill pid +SIGCONT+))
    (sb-ext:process-wait pp t)
    ;; Take the terminal back.
    (when tty-p
      (syscall (tcsetpgrp 0 (getpid))))
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
      (when (file-handle-terminal-p 0)
	(syscall (tcsetpgrp 0 (getpgid (getpid)))))
      ;; Suspend us again if we get a ^Z.
      (setf (signal-action +SIGTSTP+) :default)
      (dbugf :sheep "Took the terminal back in wait-and-chill?~%"))
    (wait-return-status status)))

(defvar *setpgid-err-len* nil)
(defvar *setpgid-err* "child setpgid fail~%")
(defvar *tcsetpgrp-err-len* nil)
(defvar *tcsetpgrp-err* "child tcsetpgrp fail~%")

(defun %make-error-messages ()
  (when (not *setpgid-err-len*)
    (setf *setpgid-err-len* (length *setpgid-err*)
	  *setpgid-err* (foreign-string-alloc *setpgid-err*)
	  *tcsetpgrp-err-len* (length *tcsetpgrp-err*)
	  *tcsetpgrp-err* (foreign-string-alloc *tcsetpgrp-err*))))

(defun forky (cmd args &key (environment nil env-p) background)
  (%make-error-messages)
  (let* ((cmd-and-args (cons cmd args))
	 (argc (length cmd-and-args))
	 (tty-p (file-handle-terminal-p 2))
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
	    (when (and (not background) tty-p)
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
  (when (file-handle-terminal-p 0)
    (syscall (tcsetpgrp 0 pid)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *ptrace-requests* nil "Requests for process tracing.")

#+linux
(define-to-list *ptrace-requests*
  #(#(+PTRACE_TRACEME+          0
      "Indicate that the process making this request should be traced.
All signals received by this process can be intercepted by its
parent, and its parent can use the other `ptrace' requests.")
    #(+PTRACE_PEEKTEXT+         1
      "Return the word in the process's text space at address ADDR.")
    #(+PTRACE_PEEKDATA+         2
      "Return the word in the process's data space at address ADDR.")
    #(+PTRACE_PEEKUSER+         3
      "Return the word in the process's user area at offset ADDR.")
    #(+PTRACE_POKETEXT+         4
      "Write the word DATA into the process's text space at address ADDR.")
    #(+PTRACE_POKEDATA+         5
      "Write the word DATA into the process's data space at address ADDR.")
    #(+PTRACE_POKEUSER+         6
      "Write the word DATA into the process's user area at offset ADDR.")
    #(+PTRACE_CONT+             7
      "Continue the process.")
    #(+PTRACE_KILL+             8
      "Kill the process.")
    #(+PTRACE_SINGLESTEP+       9
      "Single step the process.")
    #(+PTRACE_GETREGS+          12
      "Get all general purpose registers used by a processes.")
    #(+PTRACE_SETREGS+          13
      "Set all general purpose registers used by a processes.")
    #(+PTRACE_GETFPREGS+        14
      "Get all floating point registers used by a processes.")
    #(+PTRACE_SETFPREGS+        15
      "Set all floating point registers used by a processes.")
    #(+PTRACE_ATTACH+           16
      "Attach to a process that is already running.")
    #(+PTRACE_DETACH+           17
      "Detach from a process attached to with PTRACE_ATTACH.")
    #(+PTRACE_GETFPXREGS+	18
      "Get all extended floating point registers used by a processes.")
    #(+PTRACE_SETFPXREGS+	19
      "Set all extended floating point registers used by a processes.")
    #(+PTRACE_SYSCALL+		24
      "Continue and stop at the next entry to or return from syscall.")
    #(+PTRACE_GET_THREAD_AREA+	25
      "Get a TLS entry in the GDT.")
    #(+PTRACE_SET_THREAD_AREA+	26
      "Change a TLS entry in the GDT.")

#|
#ifdef __x86_64__
  /* Access TLS data.  */
  PTRACE_ARCH_PRCTL = 30,
# define PT_ARCH_PRCTL PTRACE_ARCH_PRCTL
#endif
|#

    #(+PTRACE_SYSEMU+                 31
      "Continue and stop at the next syscall, it will not be executed.")
    #(+PTRACE_SYSEMU_SINGLESTEP+      32
      "Single step the process, the next syscall will not be executed.")
    #(+PTRACE_SINGLEBLOCK+            33
      "Execute process until next taken branch.")
    #(+PTRACE_SETOPTIONS+             #x4200
      "Set ptrace filter options.")
    #(+PTRACE_GETEVENTMSG+            #x4201
      "Get last ptrace message.")
    #(+PTRACE_GETSIGINFO+             #x4202
      "Get siginfo for process.")
    #(+PTRACE_SETSIGINFO+             #x4203
      "Set new siginfo for process.")
    #(+PTRACE_GETREGSET+              #x4204
      "Get register content.")
    #(+PTRACE_SETREGSET+              #x4205
      "Set register content.")
    #(+PTRACE_SEIZE+                  #x4206
      "Like PTRACE_ATTACH, but do not force tracee to trap and do not affect
signal or group stop state.")
    #(+PTRACE_INTERRUPT+              #x4207
      "Trap seized tracee.")
    #(+PTRACE_LISTEN+                 #x4208
      "Wait for next group event.")
    #(+PTRACE_PEEKSIGINFO+            #x4209
      "Retrieve siginfo_t structures without removing signals from a queue.")
    #(+PTRACE_GETSIGMASK+             #x420a
      "Get the mask of blocked signals.")
    #(+PTRACE_SETSIGMASK+             #x420b
      "Change the mask of blocked signals.")
    #(+PTRACE_SECCOMP_GET_FILTER+     #x420c
      "Get seccomp BPF filters.")))

#+linux
(defcfun ("ptrace" real-ptrace) :long (request :int)
	 (pid pid-t) (address :pointer) (data :pointer))

#+linux (progn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prctl - Linux control process state

(defconstant +PR-UNALIGN-NOPRINT+	1
  "Silently fix up unaligned user accesses.")
(defconstant +PR-UNALIGN-SIGBUS+	2
  "Generate SIGBUS on unaligned user access.")

(defconstant +PR-FPEMU-NOPRINT+	1
  "Silently emulate fp operations accesses")
(defconstant +PR0FPEMU0SIGFPE+	2
  "Don't emulate fp operations, send SIGFPE instead.")

(defconstant +PR-FP-EXC-SW-ENABLE+ #x80     "Use FPEXC for FP exception enables")
(defconstant +PR-FP-EXC-DIV+       #x010000 "Floating point divide by zero")
(defconstant +PR-FP-EXC-OVF+       #x020000 "Floating point overflow")
(defconstant +PR-FP-EXC-UND+       #x040000 "Floating point underflow")
(defconstant +PR-FP-EXC-RES+       #x080000 "Floating point inexact result")
(defconstant +PR-FP-EXC-INV+       #x100000 "Floating point invalid operation")
(defconstant +PR-FP-EXC-DISABLED+  0        "FP exceptions disabled")
(defconstant +PR-FP-EXC-NONRECOV+  1        "Async non-recoverable exc. mode")
(defconstant +PR-FP-EXC-ASYNC+     2        "Async recoverable exception mode")
(defconstant +PR-FP-EXC-PRECISE+   3        "Precise exception mode")

(defconstant +PR-TIMING-STATISTICAL+ 0
  "Normal, traditional, statistical process timing.")
(defconstant +PR-TIMING-TIMESTAMP+   1
  "Accurate timestamp based process timing.")

(defconstant +PR-ENDIAN-BIG+        0 "Big endian mode.")
(defconstant +PR-ENDIAN-LITTLE+     1 "True little endian mode.")
(defconstant +PR-ENDIAN-PPC-LITTLE+ 2 "PowerPC pseudo little endian.")

(defconstant +PR-TSC-ENABLE+  1 "Allow the use of the timestamp counter.")
(defconstant +PR-TSC-SIGSEGV+ 2 "Throw a SIGSEGV instead of reading the TSC.")

(defconstant +PR-MCE-KILL-CLEAR+   0)
(defconstant +PR-MCE-KILL-SET+     1)

(defconstant +PR-MCE-KILL-LATE+    0)
(defconstant +PR-MCE-KILL-EARLY+   1)
(defconstant +PR-MCE-KILL-DEFAULT+ 2)

(defconstant +PR-SET-PTRACER-ANY+ -1)

(defcstruct prctl-mm-map
  "Process memory map parameters."
   (start_code  :uint64)	; code section bounds
   (end_code    :uint64)
   (start_data  :uint64)	; data section bounds
   (end_data    :uint64)	;
   (start_brk   :uint64)	; heap for brk() syscall
   (brk         :uint64)	;
   (start_stack :uint64)	; stack starts at
   (arg_start   :uint64)	; command line arguments bounds
   (arg_end     :uint64)	;
   (env_start   :uint64)	; environment variables bounds
   (env_end     :uint64)	;
   (*auxv       :uint64)	; auxiliary vector
   (auxv_size   :uint32)	; vector size
   (exe_fd      :uint32))	; /proc/$pid/exe link file

(defparameter *memory-map-slot* nil "Memory map slots.")

(define-to-list *memory-map-slot*
  #(#(+PR-SET-MM-START-CODE   1 "Code section start.")
    #(+PR-SET-MM-END-CODE     2 "Code section end.")
    #(+PR-SET-MM-START-DATA   3 "Data section start.")
    #(+PR-SET-MM-END-DATA     4 "Data section end.")
    #(+PR-SET-MM-START-STACK  5 "Stack starts at.")
    #(+PR-SET-MM-START-BRK    6 "Start of heap for brk() syscall.")
    #(+PR-SET-MM-BRK          7 "End of heap for brk() syscall.")
    #(+PR-SET-MM-ARG-START    8 "Command line arguments start.")
    #(+PR-SET-MM-ARG-END      9 "Command line arguments end.")
    #(+PR-SET-MM-ENV-START    10 "Environment variables start.")
    #(+PR-SET-MM-ENV-END      11 "Environment variables end.")
    #(+PR-SET-MM-AUXV         12 "Auxiliary vector.")
    #(+PR-SET-MM-EXE-FILE     13 "Executable file.")
    #(+PR-SET-MM-MAP          14 "Set all the memory map slots.")
    #(+PR-SET-MM-MAP-SIZE     15 "Get the size of memory map struct.")))

(defconstant +PR-FP-MODE-FR+  #.(ash 1 0)) ; 64b FP registers
(defconstant +PR-FP-MODE-FRE+ #.(ash 1 1)) ; 32b compatibility

(defconstant +PR-CAP-AMBIENT-IS-SET+     1)
(defconstant +PR-CAP-AMBIENT-RAISE+      2)
(defconstant +PR-CAP-AMBIENT-LOWER+      3)
(defconstant +PR-CAP-AMBIENT-CLEAR-ALL+  4)

(defconstant +PR-SVE-SET-VL-ONEXEC+	#.(ash 1 18) "Defer effect until exec")
(defconstant +PR-SVE-VL-LEN-MASK+	#xffff)
(defconstant +PR-SVE-VL-INHERIT+	#.(ash 1 17) "Inherit across exec")

;; Per task speculation control. Speculation control variants.
(defconstant +PR-SPEC-STORE-BYPASS+     0)
(defconstant +PR-SPEC-INDIRECT-BRANCH+  1)

;; Return and control values for PR_SET/GET_SPECULATION_CTRL */
(defconstant +PR-SPEC-NOT_AFFECTED      0)
(defconstant +PR-SPEC-PRCTL             #.(ash 1 0))
(defconstant +PR-SPEC-ENABLE            #.(ash 1 1))
(defconstant +PR-SPEC-DISABLE           #.(ash 1 2))
(defconstant +PR-SPEC-FORCE_DISABLE     #.(ash 1 3))
(defconstant +PR-SPEC-DISABLE_NOEXEC    #.(ash 1 4))

;; Reset arm64 pointer authentication keys
(defconstant +PR-PAC-APIAKEY+           #.(ash 1 0))
(defconstant +PR-PAC-APIBKEY+           #.(ash 1 1))
(defconstant +PR-PAC-APDAKEY+           #.(ash 1 2))
(defconstant +PR-PAC-APDBKEY+           #.(ash 1 3))
(defconstant +PR-PAC-APGAKEY+           #.(ash 1 4))

(defparameter *prctl-option* nil "List of prctl options.")

(define-to-list *prctl-option*
  #(#(+PR-SET-PDEATHSIG+	1
      "Set the parent death signal of the calling process.")
    #(+PR-GET-PDEATHSIG+	2
      "Get the parent death signal of the calling process.")
    #(+PR-GET-DUMPABLE+		3
      "Get how core dumps are allowed.")
    #(+PR-SET-DUMPABLE+		4
      "Set how core dumps are allowed.")
    #(+PR-GET-UNALIGN+		5
      "Get if unaligned acceeses are fixed up on some processors.")
    #(+PR-SET-UNALIGN+		6
      "Set if unaligned acceeses are fixed up on some processors.")
    #(+PR-GET-KEEPCAPS+		7
      "Get if capabilities are kept when switching UIDs from 0.")
    #(+PR-SET-KEEPCAPS+		8
      "Set if capabilities are kept when switching UIDs from 0.")
    #(+PR-GET-KEEPCAPS+		7
      "Get if capabilities are kept when switching UIDs from 0.")
    #(+PR-SET-KEEPCAPS+		8
      "Set if capabilities are kept when switching UIDs from 0.")
    #(+PR-GET-FPEMU+		9
      "Get floating point emulation control bits.")
    #(+PR-SET-FPEMU+		10
      "Set floating point emulation control bits.")
    #(+PR-GET-FPEXC+		11
      "Get floating point exception mode.")
    #(+PR-SET-FPEXC+		12
      "Get floating point exception mode.")
    #(+PR-GET-TIMING+		13      "Get timing mode.")
    #(+PR-SET-TIMING+		14      "Set timing mode.")
    #(+PR-SET-NAME+		15      "Set process name.")
    #(+PR-GET-NAME+		16      "Get process name.")
    #(+PR-GET-ENDIAN+		19      "Get process endianness.")
    #(+PR-SET-ENDIAN+		20      "Set process endianness.")
    #(+PR-GET-SECCOMP+		21      "Get process seccomp mode.")
    #(+PR-SET-SECCOMP+		22      "Set process seccomp mode.")
    #(+PR-CAPBSET-READ+         23      "Read bounding set capability.")
    #(+PR-CAPBSET-DROP+         24      "Drop bounding set capability.")
    #(+PR-GET-TSC+              25      "Get timestamp count instruction mode.")
    #(+PR-SET-TSC+              26      "Set timestamp count instruction mode.")
    #(+PR-GET-SECUREBITS+       25      "Get securebits.")
    #(+PR-SET-SECUREBITS+       26      "Set securebits.")
    #(+PR-GET-TIMERSLACK+       25      "Get timer slack.")
    #(+PR-SET-TIMERSLACK+       26      "Set timer slack.")
    #(+PR-TASK-PERF-EVENTS-DISABLE+ 31  "Disable performance counters.")
    #(+PR-TASK-PERF-EVENTS-ENABLE+  32  "Enable performance counters.")
    #(+PR-MCE-KILL+             33      "Set machine check error kill policy.")
    #(+PR-MCE-KILL-GET+         34      "Get machine check error kill policy.")
    #(+PR-SET-MM+               35      "Set memory map specifics.")
    #(+PR-SET-PTRACER+	#x59616d61      "Set a process that can ptrace.")
    #(+PR-SET-CHILD-SUBREAPER+  36      "Set a child subreaper process.")
    #(+PR-GET-CHILD-SUBREAPER+  37      "Get the child subreaper process.")
    #(+PR-SET-NO-NEW-PRIVS+     38      "Set the no new priv bit.")
    #(+PR-GET-NO-NEW-PRIVS+     39      "Get the no new priv bit.")
    #(+PR-GET-TID-ADDRESS+      40      "Get the clear_child_tid address.")
    #(+PR-SET-THP-DISABLE+      41      "Set trasparent huge page disable.")
    #(+PR-GET-THP-DISABLE+      42      "Get trasparent huge page disable.")
    #(+PR-MPX-ENABLE-MANAGEMENT+  43    "Enable MPX management.")
    #(+PR-MPX-DISABLE-MANAGEMENT+ 44    "Disable MPX management.")
    #(+PR-SET-FP-MODE+          45      "Set floating point mode.")
    #(+PR-GET-FP-MODE+          46      "Get floating point mode.")
    #(+PR-CAP-AMBIENT+          47      "Control the ambient capabillity set.")
    #(+PR-SVE-SET-VL+           50	"Set task vector length.")
    #(+PR-SVE-GET-VL+           51	"Get task vector length.")
    #(+PR-GET-SPECULATION-CTRL+ 52      "Get speculation control.")
    #(+PR-SET-SPECULATION-CTRL+ 53      "Set speculation control.")
    #(+PR-PAC-RESET-KEYS+       54      "Reset pointer authentication keys?")
    ))

;; but it's actually:
;; int prctl (int option, ...);

(defcfun ("prctl" real-prctl) :int (option :int) (arg2 :unsigned-long)
	 (arg3 :unsigned-long) (arg4 :unsigned-long) (arg5 :unsigned-long))

(defconstant +process-name-max+ 16)

(defun prctl (option &rest args)
  "Control process settings."
  (let ((arg2 (or (nth 0 args) 0))
	(arg3 (or (nth 1 args) 0))
	(arg4 (or (nth 2 args) 0))
	(arg5 (or (nth 3 args) 0)))
    ;; I think everything that returns a value, does not return -1 unless it's
    ;; an error.
    (cond
      ((member option
	       (list +PR-GET-CHILD-SUBREAPER+
		     +PR-GET-ENDIAN+
		     +PR-GET-FPEMU+
		     +PR-GET-FPEXC+
		     +PR-GET-PDEATHSIG+
		     +PR-GET-TID-ADDRESS+ ; this isn't an int* but an int**
		     +PR-GET-TSC+
		     +PR-GET-UNALIGN+))
       ;; Things that return an :unsigned-long in arg2.
       (with-foreign-object (result :unsigned-long)
	 (syscall (real-prctl option (cffi:pointer-address result)
			      arg3 arg4 arg5))
	 (mem-ref result :unsigned-long)))
      ((eql option +PR-GET-NAME+)
	 (with-foreign-string (str (make-string +process-name-max+))
	   (syscall (real-prctl option str arg3 arg4 arg5))
	   (first (multiple-value-list (foreign-string-to-lisp str)))))
      ((eql option +PR-SET-NAME+)
       (when (not (stringp arg2))
	 (error 'opsys-error
		:format-control "Second argument must be a string."))
       (let ((lisp-str (make-string +process-name-max+
				    :initial-element (code-char 0))))
	 (setf (subseq lisp-str 0 15) (subseq arg2 0 (min (length arg2) 15)))
	 (with-foreign-string (str lisp-str)
	   (syscall (real-prctl option str arg3 arg4 arg5)))))
      (t
       ;; Things that either only error, or return a positive integer.
       (syscall (real-prctl option arg2 arg3 arg4 arg5))))))

;; All this prctl stuff is probably very seldom used, but if it was, or if
;; we care someday, it would be nice to provide an access functions (setter and
;; getters) for each one, say e.g.:
;;
;; (process-death-signal)
;; (setf (process-death-signal) +SIGUSR2+)
;;
;; (process-name)
;; (setf (process-name) "Flimbar Naugahide III")
;;
;; etc.
;; But then it would actually be good to coordinate the features and names
;; between OSs, like the BSDs and even Windows, so if there is such a setting
;; on the OS, it can be accessed using the generic interface.

) ; #+linux progn

;; End
