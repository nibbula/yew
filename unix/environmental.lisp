;;
;; unix/environmental.lisp - Environmental information for unix
;;

(in-package :opsys-unix)

;; We could provide a cached value to make this faster, and update it
;; when the setenv below is used, but it would become inaccurate if
;; other code modifies the environment.

;; Does it even make sense to have these as keywords??

#+(or sbcl clisp ccl ecl lispworks abcl)
(defun convert-environ (env)
  "Convert the system environment to an keyworded alist."
  (loop :for v :in env
	:collect
	#+(or sbcl ccl ecl lispworks)
	(let ((pos (position #\= v)))
	  (when (not pos)
	    (error "Environment entry without an equal-sign (~a)." v))
	  (cons (intern (subseq v 0 pos) :keyword)
		(subseq v (1+ pos))))
	#+(or clisp abcl)
	(cons (intern (car v) :keyword) (cdr v))
	))

#+(and ecl darwin)
(progn
  (defcfun ("_NSGetEnviron" ns-get-environ) :pointer)
  (defun real-environ () (mem-ref (ns-get-environ) :pointer)))

#-(and ecl darwin)
(progn
  (defcvar ("environ" *real-environ*) :pointer)
  (defun real-environ () *real-environ*))

(defun make-c-env (lisp-env)
  "Make a 'C' environment list from a Lisp environment list. The Lisp
environment is a list of (:KEYWORD . \"STRING\") pairs, as returned by ENVIRON.
It allocates it in 'C' space, so to free it, use FREE-C-ENV."
  (let (c-env
	(len (length lisp-env))
	(done 0))
    (unwind-protect
      (progn
	(setf c-env (foreign-alloc :string :count (1+ len)))
	(loop
	   :for i :from 0 :below len
	   :for e :in lisp-env
	   :do
	   (when (not (symbolp (car e)))
	     (error
	      "The CAR of an environment pair should be a symbol, not ~s."
	      (car e)))
	   (when (not (stringp (cdr e)))
	     (error
	      "The CDR of an environment pair should be a string, not ~s."
	      (cdr e)))
	   (setf (mem-aref c-env :pointer i)
		 (foreign-string-alloc
		  (concatenate 'string (princ-to-string (car e)) "=" (cdr e))))
	   (incf done))
	(setf (mem-aref c-env :pointer len) (null-pointer)))
      ;; Clean up, if not done.
      (when (and (< done len) c-env (not (null-pointer-p c-env)))
	(loop :for i :from 0 :below done :do
	   (when (not (null-pointer-p (mem-aref c-env :pointer i)))
	     (foreign-free (mem-aref c-env :pointer i))))
	(foreign-free c-env)))
    c-env))

(defun free-c-env (c-env)
  "Free the 'C' environment list."
  (when (and c-env (not (null-pointer-p c-env)))
    (loop :with p = c-env :and s = nil
       :while (not (null-pointer-p (setf s (mem-ref p :pointer)))) :do
       (setf p (inc-pointer p (foreign-type-size :pointer)))
       (foreign-string-free s))
    (foreign-free c-env)))

(defun posix-environ (&optional (env (real-environ)))
  "Convert the 'C' environment list ENV to a list of strings. ENV defaults to
the current 'C' environment."
  (loop :with p = env :and s = nil
     :while (setf s (mem-ref p :string))
     :collect (progn
		(setf p (inc-pointer p (foreign-type-size :pointer)))
		s)))

;; @@@ The whole convert-environ and having it as keywords, might be stupid?
;; Is this what the SBCL docs describe as the lossy CMU way?
(defun environment ()
  "Return an a-list of the system environment. The elements are conses
(VARIABLE-NAME . VALUE), where VARIABLE-NAME is a keyword and VALUE is a string."
  #+clisp (convert-environ (ext:getenv))
  #+(or sbcl ccl ecl lispworks) (convert-environ (posix-environ))
  #+cmu ext:*environment-list*
  #+abcl (convert-environ (ext:getenv-all))
  #-(or clisp sbcl ccl cmu ecl lispworks abcl)
  (missing-implementation 'environ))

(defcfun ("getenv" real-getenv) :string (name :string))

(defun environment-variable (var)
  "Return a string with the value of the system environment variable name VAR."
  (declare (type string-designator var))
  (let ((var-string (string var)))
    #+clisp (ext:getenv var-string)
    #+sbcl (sb-ext:posix-getenv var-string)
    #+openmcl (ccl::getenv var-string)
    #+cmu (real-getenv var-string)
;     #+cmu (let ((v (assoc (intern (string-upcase var-string) :keyword)
; 			  ext:*environment-list*)))
; 	    (if v (cdr v)))
    #+ecl (ext:getenv var-string)
    #+excl (sys::getenv var-string)
    #+lispworks (hcl:getenv var-string)
    #+abcl (ext:getenv var-string)
    #-(or clisp sbcl openmcl cmu ecl excl lispworks abcl)
    (missing-implementation 'getenv)))

(defalias 'getenv 'environment-variable)
(defalias 'env 'environment-variable)

;; If we had environ and didn't have a getenv, or if it was faster
;; (which it isn't) we could define getenv as:
;; (cdr (assoc "TERM" (environ) :test #'string=))
;;
;; (defun vv (v) (cdr (assoc v (nos:environ) :test #'string=)))
;; (time (do ((i 0 (+ i 1))) ((> i 50000)) (nos:getenv "TERM")))
;; (time (do ((i 0 (+ i 1))) ((> i 50000)) (vv "TERM")))

(defcfun ("unsetenv" real-unsetenv) :int (name :string))

(defun unsetenv (var)
  "Remove the environtment variable named VAR."
  (declare (type string-designator var))
  #+clisp (setf (ext:getenv var) nil)	; @@@ guessing?
  #+excl (setf (sys::getenv var) nil)	; @@@ guessing?
  #+ccl (syscall (ccl::unsetenv var))
  #+(or sbcl cmu abcl ecl lispworks) (syscall (real-unsetenv var))
  ;;#+lispworks (hcl:unsetenv var)
  #-(or clisp openmcl excl sbcl ecl cmu lispworks abcl)
  (declare (ignore var))
  #-(or clisp openmcl excl sbcl ecl cmu lispworks abcl)
  (missing-implementation 'unsetenv))

(defcfun ("setenv" real-setenv) :int
  (name :string) (value :string) (overwrite :int))

(defun setenv (var value)
  "Set the environtment variable named VAR to the string VALUE. If VALUE is
NIL, unset the VAR, using unsetenv."
  (declare (type string-designator var)
	   (type (or string null) value))
  (when (not value)
    (unsetenv var)
    (return-from setenv value))
  #+clisp (setf (ext:getenv var) value)
  #+openmcl (syscall (ccl::setenv var value))
  #+excl (setf (sys::getenv var) value)
  #+(or sbcl cmu abcl) (syscall (real-setenv var value 1))
;   #+cmu (let ((v (assoc (intern (string-upcase var) :keyword)
; 			ext:*environment-list*)))
; 	  (if v (cdr v)))
  #+ecl (ext:setenv var value)
  #+lispworks (hcl:setenv var value)
  #-(or clisp openmcl excl sbcl ecl cmu lispworks abcl)
  (declare (ignore var value))
  #-(or clisp openmcl excl sbcl ecl cmu lispworks abcl)
  (missing-implementation 'setenv))

(defsetf environment-variable setenv
    "Set the environtment variable named VAR to the string VALUE.")

(defsetf env setenv
    "Set the environtment variable named VAR to the string VALUE.")

;; sysctl
;;
;; sysctl seems nice at first glance compared to the completely bogus old
;; methods of finding the symbol in the running kernel image. You can get and
;; set a whole bunch of system information with just one system call. But it's
;; soon obvious that sysctl is lame because you don't have any way of getting
;; meta information. In other words, there's no way to know what the set of
;; sysctl values are or what their types are. Even if you know that, there's
;; no real guarantee that the varible exists in your running kernel. This
;; could have easily been solved by adding some meta information. I suppose a
;; rationale for not having metadata is kernel bloat.
;;
;; The linuxy method of reading from /proc is even stupider in theory,
;; although in practice seems easier to write interfaces for, since it
;; compensates for hazzards in C. Unfortunately, it doesn't solve the problem
;; of metadata, unless you count the text formated things, which serves to
;; demonstrate the conflict between machine readable and human readable. It's
;; really not hard to make a C interface that's semi-reasonable,
;; eg. GObject. Of course again there's the issue of bloat. Linux's minimalism
;; is responsible for it being so adaptable to small devices. sbcl.core is
;; 58MB, whereas linux can probably still work in 4MB?
;;
;; BUT, it turns out that most of the metadata is in header files as well as
;; probably in the kernel in a hackish way. But a method for getting at these
;; isn't officially defined in the API. Why couldn't they have designed it in?
;;
;; Now, many years after first writing this, it seems as if someone has
;; implemented my wish, at least in FreeBSD. There is now mechanism to get the
;; names of sysctl items from sysctl itself. However, it seems to be
;; undocumented. Had it been there all along? If everything works out, this
;; rant can be tossed in the bins of history.
;;
;; So far, sysctl seems work best on BSDs, partially on macOS and, not on
;; Linux. If performance need to be improved, we could consider caching the
;; integer values by using sysctlnametomib.
;;
;; NOTE: This should probably come fairly early since we may use it later on
;; to determine configuration, such as kernel version, etc.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-linux (config-feature :os-t-has-sysctl))

#+os-t-has-sysctl
(defcfun ("sysctl" real-sysctl)
    :int (name :pointer) (namelen :unsigned-int)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newlen size-t))

#+os-t-has-sysctl
(defcfun ("sysctlbyname" real-sysctlbyname) :int (name :string)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newlen size-t))

#+os-t-has-sysctl
(defcfun "sysctlnametomib" :int (name :string) (mibp :pointer)
	 (sizep :pointer))

;(defgeneric sysctl (name type)
; (:documentation "Return the sysctl value named NAME. TYPE should be the C type
;of the value, as used by CFFI, such a :string :integer, etc.")
;  (:method

(defconstant +NGROUPS+ 16 "Max supplemental group id's")

(defcstruct foreign-itimerval
  "Interval timer."
  (it_interval (:struct foreign-timeval))
  (it_value    (:struct foreign-timeval)))

(defcstruct foreign-loadavg
  (ldavg  fixpt-t :count 3)		; fixpt_t ldavg[3];
  (fscale :long))			; long    fscale;

(defcstruct foreign-ucred
  (cr_ref :int32)			; reference count
  (cr_uid uid-t)			; effective user id
  (cr_ngroups :short)			; number of groups
  (cr_groups gid-t :count 16))		; groups 

(defcstruct foreign-pcred
  (pc_lock :char :count 72) ; char pc_lock[72]; opaque content
  (pc_ucred :pointer)	    ; struct ucred *pc_ucred  Current credentials.
  (p_ruid   uid-t)	    ; Real user id.
  (p_svuid  uid-t)	    ; Saved effective user id.
  (p_rgid   gid-t)	    ; Real group id.
  (p_svgid  gid-t)	    ; Saved effective group id.
  (p_refcnt :int))	    ; Number of references. 

(defcstruct foreign-vmspace
  (dummy :int32)
  (dummy2 caddr-t)
  (dummy3 :int32 :count 5)
  (dummy4 caddr-t :count 3))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +WMESGLEN+	     7    "wchan message length")
  (defconstant +EPROC_CTTY+	     #x01 "controlling tty vnode active")
  (defconstant +EPROC_SLEADER+	     #x02 "session leader")
  (defconstant +COMPAT_MAXLOGNAME+   12   "short setlogin() name"))

(defcstruct foreign-eproc
  (e_paddr :pointer)		     ; address of proc (opaque: struct proc *)
  (e_sess  :pointer)		     ; session pointer (struct session *)
  (e_pcred (:struct foreign-pcred))  ; process credentials
  (e_ucred (:struct foreign-ucred))  ; current credentials
  (e_vm   (:struct foreign-vmspace)) ; address space
  (e_ppid pid-t)		     ; parent process id
  (e_pgid pid-t)		     ; process group id
  (e_jobc :short)		     ; job control counter
  (e_tdev dev-t)		     ; controlling tty dev
  (e_tpgid pid-t)		     ; tty process group id
  (e_tsess :pointer)		     ; tty session pointer (struct session *)
  (e_wmesg :char :count #.(+ +WMESGLEN+ 1))
  (e_xsize segsz-t)		     ; text size
  (e_xrssize :short)		     ; text rss
  (e_xccount :short)		     ; text references
  (e_xswrss :short)
  (e_flag :int32)
  (e_login :char :count #.+COMPAT_MAXLOGNAME+) ; short setlogin() name
  (e_spare :int32 :count 4))

(defcstruct foreign-p-st1
  (__p_forw :pointer)
  (__p_back :pointer))

(defcunion foreign-p-un
  (p_st1 (:struct foreign-p-st1))
  (__p_starttime (:struct foreign-timeval)))

(defcstruct foreign-extern-proc
  (p_un (:union foreign-p-un))
  (p_vmspace :pointer)			; opaque: struct vmspace *
  (p_sigacts :pointer)			; opaque: struct sigacts *
  (p_flag :int)
  (p_stat :char)
  (p_pid pid-t)
  (p_oppid pid-t)
  (p_dupfd :int)
  (user_stack caddr-t)
  (exit_thread (:pointer :void))
  (p_debugger :int)
  (sigwait boolean-t)
  (p_estcpu :unsigned-int)
  (p_cpticks :int)
  (p_pctcpu fixpt-t)
  (p_wchan (:pointer :void))
  (p_wmesg (:pointer :char))
  (p_swtime :unsigned-int)
  (p_slptime :unsigned-int)
  (p_realtimer (:struct foreign-itimerval))
  (p_rtime (:struct foreign-timeval))
  (p_uticks u-quad-t)
  (p_sticks u-quad-t)
  (p_iticks u-quad-t)
  (p_traceflag :int)
  (p_tracep :pointer)			; opaque: struct vnode *
  (p_siglist :int)
  (p_textvp :pointer)			; opaque: struct vnode *
  (p_holdcnt :int)
  (p_sigmask sigset-t)
  (p_sigignore sigset-t)
  (p_sigcatch sigset-t)
  (p_priority :unsigned-char)
  (p_usrpri :unsigned-char)
  (p_nice :char)
  (p_comm :char :count #.(+ 16 1))
  (p_pgrp :pointer)			; opaque: struct pgrp *
  (p_addr :pointer)			; opaque: struct user *
  (p_xstat :unsigned-short)
  (p_acflag :unsigned-short)
  (p_ru (:pointer (:struct foreign-rusage))))

(defcstruct foreign-kinfo-proc
  "Augmented proc structure returned by sysctl KERN_PROC subtype."
  (kp_proc (:struct foreign-extern-proc))
  (kp_eproc (:struct foreign-eproc)))

#+(or freebsd openbsd)
(defcstruct foreign-vmtotal
  (t_rq     :int16)   ;; length of the run queue
  (t_dw     :int16)   ;; jobs in ``disk wait'' (neg priority)
  (t_pw     :int16)   ;; jobs in page wait
  (t_sl     :int16)   ;; jobs sleeping in core
  (t_sw     :int16)   ;; swapped out runnable/short block jobs
  (t_vm     :int32)   ;; total virtual memory
  (t_avm    :int32)   ;; active virtual memory
  (t_rm     :int32)   ;; total real memory in use
  (t_arm    :int32)   ;; active real memory
  (t_vmshr  :int32)   ;; shared virtual memory
  (t_avmshr :int32)   ;; active shared virtual memory
  (t_rmshr  :int32)   ;; shared real memory
  (t_armshr :int32)   ;; active shared real memory
  (t_free   :int32))  ;; free memory pages

#+os-t-has-sysctl
(defun sysctl-name-to-mib (name)
  "Return a vector of integers which is the numeric MIB for sysctl NAME."
  (let (result (initial-size 10) result-size)
    (cffi:with-foreign-objects ((mib :int initial-size) (size-ptr :int))
      (setf (cffi:mem-ref size-ptr :int) initial-size)
      (sysctlnametomib name mib size-ptr)
      (setf result-size (cffi:mem-ref size-ptr :int))
      (setf result (make-array (list result-size) :element-type 'integer))
      (loop :for i :from 0 :below result-size
	 :do (setf (aref result i) (cffi:mem-aref mib :int i))))
    result))

#+os-t-has-sysctl
(defun sysctl-by-number (name type)
  (with-foreign-objects ((mib :int (length name))
			 (oldlenp 'size-t 1))
    (dotimes (i (length name))
      (setf (mem-aref mib :int i) (aref name i)))
    (syscall
     (real-sysctl mib (length name) (cffi:null-pointer) oldlenp
		  (cffi:null-pointer) 0))
    ;;(format t "length = ~d~%" (mem-ref oldlenp 'size-t))
    (with-foreign-object (oldp :unsigned-char (mem-ref oldlenp 'size-t))
      (syscall (real-sysctl mib (length name) oldp oldlenp
			    (cffi:null-pointer) 0))
      (case type
	(:string
	 (convert-from-foreign oldp type))
	((:short :unsigned-short :int :unsigned :unsigned-int
	  :long :unsigned-long :int8 :uint8 :int16 :uint16 :int32 :uint32
	  :int64 :uint64)
;	 (cffi:mem-ref (convert-from-foreign oldp type) type))))))
	 (cffi:mem-ref oldp type))
	(t
	 (convert-from-foreign oldp type))))))

#|
#+os-t-has-sysctl
(defun sysctl-names (&optional prefix)
  (sysctl-name-to-mib "sysctl")
  (if prefix
      (sysctl-by-number (vector 0 1 "
|#

#+os-t-has-sysctl
(defun sysctl (name type)
  (with-foreign-object (oldlenp 'size-t 1)
    (syscall
     (real-sysctlbyname name (cffi:null-pointer) oldlenp (cffi:null-pointer) 0))
    ;;(format t "length = ~d~%" (mem-ref oldlenp 'size-t))
    (with-foreign-object (oldp :unsigned-char (mem-ref oldlenp 'size-t))
      (syscall (real-sysctlbyname name oldp oldlenp (cffi:null-pointer) 0))
      (case type
	(:string
	 (convert-from-foreign oldp type))
	((:short :unsigned-short :int :unsigned :unsigned-int
	  :long :unsigned-long :int8 :uint8 :int16 :uint16 :int32 :uint32
	  :int64 :uint64)
;	 (cffi:mem-ref (convert-from-foreign oldp type) type))))))
	 (cffi:mem-ref oldp type))
	(t
	 (convert-from-foreign oldp type))))))

;; @@@ should do a (defsetf sysctl ...) so we can nicely setf it.

;; sysctl things for get-system-info
#+os-t-has-sysctl
(progn
  (defstruct sysctl-context
    vmtotal)
  
  (defun get-sysctl-timeval (name string &optional context)
    (declare (ignore name context))
    (convert-timeval (sysctl string '(:struct foreign-loadavg))))

  (defun get-sysctl-vm-total (name string &optional context)
    (flet ((get-it () (sysctl string '(:struct foreign-vmtotal))))
      (let ((c (or (and context
			(or (sysctl-context-vmtotal context)
			    (get-it)))
		   (get-it))))
	(getf c
	      (case name
		(:total-memory                 't_vm) ;; @@@ wrong
		(:free-memory                  't_free)
		;;(:shared-memory                't_rmshr)
		;; From linux sysinfo, but missing:
		;; (:buffer-memory              )
		;; (:total-swap                 )
		;; (:free-swap                  )
		;; (:processes                  )
		;; (:total-high-memory          )
		;; (:free-high-memory           )
		;; (:memory-unit-bytes          )
		;; BSD-like:
                (:run-queue-length             't_rq)
                (:jobs-disk-wait               't_dw)
                (:jobs-page-wait               't_pw)
                (:jobs-sleeping                't_sl)
                (:jobs-swap-wait               't_sw)
                (:total-virtual-memory         't_vm)
                (:active-virtual-memory        't_avm)
                (:shared-virtual-memory        't_vmshr)
                (:active-shared-virtual-memory 't_avmshr)
                (:total-in-use-memory          't_rm)
                (:active-memory                't_arm)
                (:shared-memory                't_rmshr)
                (:active-shared-memory         't_armshr)
		)))))
  
  (defun get-load-averages (string &optional context)
    (declare (ignore context))
    (let ((a (make-array
	      3 :element-type 'float
	      :initial-element 0.0)))
      (dotimes (i 3)
	(setf (aref a i)
	      (mem-aref
	       (getf (sysctl string '(:struct foreign-loadavg)) 'ldavg
		     (null-pointer)) ;; Only to supress an sbcl warning.
	       'fixpt-t i)))
      a))

  (defun get-free-mem (name string &optional context)
    (declare (ignore name string context))
    ;; @@@ I have no idea if this is right.
    ;; Or is it hw.availpages ?
    (- (sysctl "hw.physmem" :integer)
       (sysctl "hw.usermem" :integer)))
  
  (defvar *sysctl-data*
    #(
      #(:uptime		      "kern.boottime"    get-sysctl-timeval)
      #(:load-averages	      "vm.loadavg"	 get-load-averages)
      #(:total-memory	      "hw.physmem"       :integer)
      #(:free-memory	      "FAKE"             get-free-mem)
      #(:shared-memory	      "kern."            get-sysctl-vm-total)
      ;; #(:buffer-memory	      "kern."            :integer)
      #(:total-swap	      "vm.swap_total"    :integer)
      #(:free-swap	      "vm.swap_reserved" :integer) ; is this right?
      #(:processes	      "kern."            :integer)
      ;;#(:total-high-memory    "kern."            :integer)
      ;;#(:free-high-memory     "kern."            :integer)
      ;;#(:memory-unit-bytes    "kern."            :integer)
      ))

  (defun sysctl-names ()
    "Return a list of keywords that we support getting from sysctl."
    (loop :for d :across *sysctl-data*
       :collect (svref d 0)))

  (defun get-sysctl-item (name &optional context)
    (let ((data (find name *sysctl-data* :key (_ (aref _ 0)))))
      (if data
	  (let ((string (aref data 1))
		(type (aref data 2)))
	  (etypecase type
	    (keyword
	     (sysctl string type))
	    (symbol
	     (funcall type name string context)))
	  (error "Unknown system info item ~s." name)))))

  (defun process-sysctl-names (names)
    (let ((s-names (intersection (sysctl-names) names))
	  (context (make-sysctl-context))
	  result)
      (if s-names
	  (progn
	    (loop :for n :in s-names
	       :do (push (cons n (get-sysctl-item n context)) result))
	    (values result (nset-difference names s-names)))
	  (values nil names)))))

;; not the same as: (= 8 (cffi:foreign-type-size :pointer))
;;#+darwin
;; (defparameter *64-bit-inode*
;;   (not (null (search "X86_64" (nos:sysctl "kern.version" :string)))))

;; #+darwin
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (when (not (null (search "X86_64" (sysctl "kern.version" :string))))
;;     (config-feature :os-t-64-bit-inode)))

;; XXX Since we can't really do the above at compile time, just assume the
;; kernel is 64 bit if we're on a 64 bit machine.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (null (search "64" (machine-type))))
    (config-feature :os-t-64-bit-inode)))

(defcfun getpagesize :int)

(defun memory-page-size ()
  "Get the system's memory page size, in bytes."
  (getpagesize))

#+linux
(progn
  (defconstant +AT-NULL+	   0 "End of vector")
  (defconstant +AT-IGNORE+	   1 "Entry should be ignored")
  (defconstant +AT-EXECFD+	   2 "File descriptor of program")
  (defconstant +AT-PHDR+	   3 "Program headers for program")
  (defconstant +AT-PHENT+	   4 "Size of program header entry")
  (defconstant +AT-PHNUM+	   5 "Number of program headers")
  (defconstant +AT-PAGESZ+	   6 "System page size")
  (defconstant +AT-BASE+	   7 "Base address of interpreter")
  (defconstant +AT-FLAGS+	   8 "Flags")
  (defconstant +AT-ENTRY+	   9 "Entry point of program")
  (defconstant +AT-NOTELF+	  10 "Program is not ELF")
  (defconstant +AT-UID+		  11 "Real uid")
  (defconstant +AT-EUID+	  12 "Effective uid")
  (defconstant +AT-GID+		  13 "Real gid")
  (defconstant +AT-EGID+	  14 "Effective gid")
  (defconstant +AT-PLATFORM+	  15 "String identifying CPU for optimizations")
  (defconstant +AT-HWCAP+	  16 "Arch dependent hints at CPU capabilities")
  (defconstant +AT-CLKTCK+        17 "Frequency at which times() increments")
  (defconstant +AT-SECURE+        23 "Secure mode boolean")
  (defconstant +AT-BASE-PLATFORM+ 24
    "String identifying real platform, may differ from AT_PLATFORM.")
  (defconstant +AT-RANDOM+        25 "Address of 16 random bytes")
  (defconstant +AT-EXECFN+        31 "Filename of program")
  (defconstant +AT-SYSINFO+       32 "")
  (defconstant +AT-SYSINFO-EHDR+  33 ""))
;; AT_* values 18 through 22 are reserved

#+linux
(defcfun ("getauxval" real-getauxval) :unsigned-long (type :unsigned-long))
#+linux
(defun getauxval (type)
  "Get a value from the kernel auxiliary vector. TYPE is one of the +AT-*+
constants. The return value varies base on the keyword."
  (let ((value (real-getauxval type)))
    (cond
      ((= type +AT-NULL+)	   nil)
      ((= type +AT-IGNORE+)	   nil)
      ((= type +AT-EXECFD+)	   value)
      ((= type +AT-PHDR+)	   (make-pointer value))
      ((= type +AT-PHENT+)	   value)
      ((= type +AT-PHNUM+)	   value)
      ((= type +AT-PAGESZ+)	   value)
      ((= type +AT-BASE+)	   (make-pointer value))
      ((= type +AT-FLAGS+)	   nil)
      ((= type +AT-ENTRY+)	   (make-pointer value))
      ((= type +AT-NOTELF+)	   value)
      ((= type +AT-UID+)	   value)
      ((= type +AT-EUID+)	   value)
      ((= type +AT-GID+)	   value)
      ((= type +AT-EGID+)	   value)
      ((= type +AT-PLATFORM+)	   (foreign-string-to-lisp (make-pointer value)))
      ((= type +AT-HWCAP+)	   value) ;; Convert to keywords?
      ((= type +AT-CLKTCK+)	   value)
      ((= type +AT-SECURE+)	   value)
      ((= type +AT-RANDOM+)	   value) ;; 16 bytes of random ff ff ff ff  ff ff ff ff
      ((= type +AT-EXECFN+)	   (foreign-string-to-lisp (make-pointer value)))
      ((= type +AT-BASE-PLATFORM+) (foreign-string-to-lisp (make-pointer value)))
      ((= type +AT-SYSINFO+)	   (make-pointer value))
      ((= type +AT-SYSINFO-EHDR+)  (make-pointer value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sysconf

;; @@@ Fill in more descriptions from somewhere
;; @@@ Test on things other than Linux. Maybe this should be #+linux?
;; @@@ even though it's supposedly POSIX, the numeric ordering might change?
(defparameter *sysconf-names* nil "Names for sysconf parameters.")

(define-enum-list *sysconf-names*
    #(#(+SC-ARG-MAX+ "The maximum length of the arguments to the exec(3) family of functions.")
      #(+SC-CHILD-MAX+ "The maximum number of simultaneous processes per user ID.")
      #(+SC-CLK-TCK+ "The number of clock ticks per second.")
      #(+SC-NGROUPS-MAX+ "Maximum number of supplementary group IDs.")
      #(+SC-OPEN-MAX+ "The maximum number of files that a process can have open at any time.")
      #(+SC-STREAM-MAX+ "The maximum number of streams that a process can have open at any time.")
      #(+SC-TZNAME-MAX+ "The maximum number of bytes in a timezone name.")
      #(+SC-JOB-CONTROL+ "If this option is in effect (as it always is under POSIX.1-2001), then the system implements POSIX-style job control, and the following functions are present: setpgid(), tcdrain(), tcflush(), tcgetpgrp(), tcsendbreak(), tcsetattr(), tcsetpgrp().")
      #(+SC-SAVED-IDS+ "A process has a saved set-user-ID and a saved set-group-ID.")
      #(+SC-REALTIME-SIGNALS+ "Realtime signals are supported. The following functions are present: sigqueue(), sigtimedwait(), sigwaitinfo().")
      #(+SC-PRIORITY-SCHEDULING+ "The include file <sched.h> is present. The following functions are present: sched_get_priority_max(), sched_get_priority_min(), sched_getparam(), sched_getscheduler(), sched_rr_get_interval(), sched_setparam(), sched_setscheduler(), sched_yield().")
      #(+SC-TIMERS+ "")
      #(+SC-ASYNCHRONOUS-IO+ "The header <aio.h> is present and the aio_* functions are present.")
      #(+SC-PRIORITIZED-IO+ "Priorities can be specified for asynchronous I/O. This affects the functions aio_read(), aio_write().")
      #(+SC-SYNCHRONIZED-IO+ "")
      #(+SC-FSYNC+ "The function fsync() is present.")
      #(+SC-MAPPED-FILES+ "Shared memory is supported. The include file <sys/mman.h> is present. The following functions are present: mmap(), msync(), munmap().")
      #(+SC-MEMLOCK+ "Shared memory can be locked into core. The functions mlockall(), munlockall() are present.")
      #(+SC-MEMLOCK-RANGE+ "More precisely, ranges can be locked into core. The functions mlock(), munlock() are present.")
      #(+SC-MEMORY-PROTECTION+ "The function mprotect() is present.")
      #(+SC-MESSAGE-PASSING+ "The include file <mqueue.h> is present. The following functions are present: mq_close(), mq_getattr(), mq_notify(), mq_open(), mq_receive(), mq_send(), mq_setattr(), mq_unlink().")
      #(+SC-SEMAPHORES+ "The include file <semaphore.h> is present and the sem_* functions.")
      #(+SC-SHARED-MEMORY-OBJECTS+ "")
      #(+SC-AIO-LISTIO-MAX+ "")
      #(+SC-AIO-MAX+ "")
      #(+SC-AIO-PRIO-DELTA-MAX+ "")
      #(+SC-DELAYTIMER-MAX+ "")
      #(+SC-MQ-OPEN-MAX+ "")
      #(+SC-MQ-PRIO-MAX+ "")
      #(+SC-VERSION+ "The year and month the POSIX.1 standard was approved in the format YYYYMML; the value 199009L indicates the Sept. 1990 revision.")
      #(+SC-PAGESIZE+ "Size of a page in bytes.")
      #(+SC-RTSIG-MAX+ "")
      #(+SC-SEM-NSEMS-MAX+ "")
      #(+SC-SEM-VALUE-MAX+ "")
      #(+SC-SIGQUEUE-MAX+ "")
      #(+SC-TIMER-MAX+ "")
      #(+SC-BC-BASE-MAX+ "The maximum obase value accepted by the bc(1) utility.")
      #(+SC-BC-DIM-MAX+ "The maximum value of elements permitted in an array by bc(1).")
      #(+SC-BC-SCALE-MAX+ "The maximum scale value allowed by bc(1).")
      #(+SC-BC-STRING-MAX+ "The maximum length of a string accepted by bc(1).")
      #(+SC-COLL-WEIGHTS-MAX+ "The maximum number of weights that can be assigned to an entry of the LC_COLLATE order keyword in the locale definition file.")
      #(+SC-EQUIV-CLASS-MAX+ "")
      #(+SC-EXPR-NEST-MAX+ "The maximum number of expressions which can be nested within parentheses by expr(1).")
      #(+SC-LINE-MAX+ "The maximum length of a utility's input line, either from standard input or from a file.  This includes space for a trailing newline.")
      #(+SC-RE-DUP-MAX+ "The number of repeated occurrences of a BRE permitted by regexec(3) and regcomp(3). Like when the interval notation \{m,n\} is used.")
      #(+SC-CHARCLASS-NAME-MAX+ "")
      #(+SC-2-VERSION+ "The version of the POSIX.2 standard in the format of YYYYMML.")
      #(+SC-2-C-BIND+ "")
      #(+SC-2-C-DEV+ "Whether the POSIX.2 C language development facilities are supported.")
      #(+SC-2-FORT-DEV+ "Whether the POSIX.2 FORTRAN development utilities are supported.")
      #(+SC-2-FORT-RUN+ "Whether the POSIX.2 FORTRAN run-time utilities are supported.")
      #(+SC-2-SW-DEV+ "Whether the POSIX.2 software development utilities option is supported.")
      #(+SC-2-LOCALEDEF+ "Whether the POSIX.2 creation of locates via localedef(1) is supported.")
      #(+SC-PII+ "")
      #(+SC-PII-XTI+ "")
      #(+SC-PII-SOCKET+ "")
      #(+SC-PII-INTERNET+ "")
      #(+SC-PII-OSI+ "")
      #(+SC-POLL+ "")
      #(+SC-SELECT+ "")
      #(+SC-UIO-MAXIOV+ "")
      #(+SC-PII-INTERNET-STREAM+ "")
      #(+SC-PII-INTERNET-DGRAM+ "")
      #(+SC-PII-OSI-COTS+ "")
      #(+SC-PII-OSI-CLTS+ "")
      #(+SC-PII-OSI-M+ "")
      #(+SC-T-IOV-MAX+ "")
      #(+SC-THREADS+ "")
      #(+SC-THREAD-SAFE-FUNCTIONS+ "")
      #(+SC-GETGR-R-SIZE-MAX+ "")
      #(+SC-GETPW-R-SIZE-MAX+ "")
      #(+SC-LOGIN-NAME-MAX+ "Maximum length of a login name, including the terminating null byte.")
      #(+SC-TTY-NAME-MAX+ "The maximum length of terminal device name, including the terminating null byte.")
      #(+SC-THREAD-DESTRUCTOR-ITERATIONS+ "")
      #(+SC-THREAD-KEYS-MAX+ "")
      #(+SC-THREAD-STACK-MIN+ "")
      #(+SC-THREAD-THREADS-MAX+ "")
      #(+SC-THREAD-ATTR-STACKADDR+ "")
      #(+SC-THREAD-ATTR-STACKSIZE+ "")
      #(+SC-THREAD-PRIORITY-SCHEDULING+ "")
      #(+SC-THREAD-PRIO-INHERIT+ "")
      #(+SC-THREAD-PRIO-PROTECT+ "")
      #(+SC-THREAD-PROCESS-SHARED+ "")
      #(+SC-NPROCESSORS-CONF+ "The number of processors configured.")
      #(+SC-NPROCESSORS-ONLN+ "The number of processors currently online (available).")
      #(+SC-PHYS-PAGES+ "The number of pages of physical memory.  Note that it is possible for the product of this value and the value of _SC_PAGESIZE to overflow.")
      #(+SC-AVPHYS-PAGES+ "The number of currently available pages of physical memory.")
      #(+SC-ATEXIT-MAX+ "")
      #(+SC-PASS-MAX+ "")
      #(+SC-XOPEN-VERSION+ "")
      #(+SC-XOPEN-XCU-VERSION+ "")
      #(+SC-XOPEN-UNIX+ "")
      #(+SC-XOPEN-CRYPT+ "")
      #(+SC-XOPEN-ENH-I18N+ "")
      #(+SC-XOPEN-SHM+ "")
      #(+SC-2-CHAR-TERM+ "")
      #(+SC-2-C-VERSION+ "")
      #(+SC-2-UPE+ "")
      #(+SC-XOPEN-XPG2+ "")
      #(+SC-XOPEN-XPG3+ "")
      #(+SC-XOPEN-XPG4+ "")
      #(+SC-CHAR-BIT+ "")
      #(+SC-CHAR-MAX+ "")
      #(+SC-CHAR-MIN+ "")
      #(+SC-INT-MAX+ "")
      #(+SC-INT-MIN+ "")
      #(+SC-LONG-BIT+ "")
      #(+SC-WORD-BIT+ "")
      #(+SC-MB-LEN-MAX+ "")
      #(+SC-NZERO+ "")
      #(+SC-SSIZE-MAX+ "")
      #(+SC-SCHAR-MAX+ "")
      #(+SC-SCHAR-MIN+ "")
      #(+SC-SHRT-MAX+ "")
      #(+SC-SHRT-MIN+ "")
      #(+SC-UCHAR-MAX+ "")
      #(+SC-UINT-MAX+ "")
      #(+SC-ULONG-MAX+ "")
      #(+SC-USHRT-MAX+ "")
      #(+SC-NL-ARGMAX+ "")
      #(+SC-NL-LANGMAX+ "")
      #(+SC-NL-MSGMAX+ "")
      #(+SC-NL-NMAX+ "")
      #(+SC-NL-SETMAX+ "")
      #(+SC-NL-TEXTMAX+ "")
      #(+SC-XBS5-ILP32-OFF32+ "")
      #(+SC-XBS5-ILP32-OFFBIG+ "")
      #(+SC-XBS5-LP64-OFF64+ "")
      #(+SC-XBS5-LPBIG-OFFBIG+ "")
      #(+SC-XOPEN-LEGACY+ "")
      #(+SC-XOPEN-REALTIME+ "")
      #(+SC-XOPEN-REALTIME-THREADS+ "")
      #(+SC-ADVISORY-INFO+ "The following advisory functions are present: posix_fadvise(), posix_fallocate(), posix_memalign(), posix_madvise().")
      #(+SC-BARRIERS+ "This option implies the _POSIX_THREADS and _POSIX_THREAD_SAFE_FUNCTIONS options and that the pthread_barrier* functions are present.")
      #(+SC-BASE+ "")
      #(+SC-C-LANG-SUPPORT+ "")
      #(+SC-C-LANG-SUPPORT-R+ "")
      #(+SC-CLOCK-SELECTION+ "This option implies the _POSIX_TIMERS option and the presence of the functions: pthread_condattr_getclock(), pthread_condattr_setclock(), clock_nanosleep().")
      #(+SC-CPUTIME+ "The clockID CLOCK_PROCESS_CPUTIME_ID is supported. The initial value of this clock is 0 for each process. This option implies the _POSIX_TIMERS option. The function clock_getcpuclockid() is present.")
      #(+SC-THREAD-CPUTIME+ "")
      #(+SC-DEVICE-IO+ "")
      #(+SC-DEVICE-SPECIFIC+ "")
      #(+SC-DEVICE-SPECIFIC-R+ "")
      #(+SC-FD-MGMT+ "")
      #(+SC-FIFO+ "")
      #(+SC-PIPE+ "")
      #(+SC-FILE-ATTRIBUTES+ "")
      #(+SC-FILE-LOCKING+ "Supposedly this is unused.")
      #(+SC-FILE-SYSTEM+ "")
      #(+SC-MONOTONIC-CLOCK+ "CLOCK_MONOTONIC is supported. Implies the _POSIX_TIMERS option. Affected functions are aio_suspend(), clock_getres(), clock_gettime(), clock_settime(), timer_create().")
      #(+SC-MULTI-PROCESS+ "Supposedly this is unused.")
      #(+SC-SINGLE-PROCESS+ "")
      #(+SC-NETWORKING+ "")
      #(+SC-READER-WRITER-LOCKS+ "This option implies the _POSIX_THREADS option and the pthread_rwlock_*() functions.")
      #(+SC-SPIN-LOCKS+ "Supports spin locks and the pthread_spin_* functions.")
      #(+SC-REGEXP+ "POSIX regular expressions are supported.")
      #(+SC-REGEX-VERSION+ "")
      #(+SC-SHELL+ "The function system() is present.")
      #(+SC-SIGNALS+ "")
      #(+SC-SPAWN+ "Support for the posix_spawn* functions. So you can fork without an MMU?")
      #(+SC-SPORADIC-SERVER+ "The scheduling policy SCHED_SPORADIC is supported.")
      #(+SC-THREAD-SPORADIC-SERVER+ "")
      #(+SC-SYSTEM-DATABASE+ "")
      #(+SC-SYSTEM-DATABASE-R+ "")
      #(+SC-TIMEOUTS+ "")
      #(+SC-TYPED-MEMORY-OBJECTS+ "The functions posix_mem_offset(), posix_typed_mem_get_info(), posix_typed_mem_open().")
      #(+SC-USER-GROUPS+ "")
      #(+SC-USER-GROUPS-R+ "")
      #(+SC-2-PBS+ "")
      #(+SC-2-PBS-ACCOUNTING+ "")
      #(+SC-2-PBS-LOCATE+ "")
      #(+SC-2-PBS-MESSAGE+ "")
      #(+SC-2-PBS-TRACK+ "")
      #(+SC-SYMLOOP-MAX+ "The maximum number of symbolic links seen in a pathname before resolution returns ELOOP.")
      #(+SC-STREAMS+ "")
      #(+SC-2-PBS-CHECKPOINT+ "")
      #(+SC-V6-ILP32-OFF32+ "")
      #(+SC-V6-ILP32-OFFBIG+ "")
      #(+SC-V6-LP64-OFF64+ "")
      #(+SC-V6-LPBIG-OFFBIG+ "")
      #(+SC-HOST-NAME-MAX+ "Maximum length of a hostname, not including the terminating null byte, as returned by gethostname(2).")
      #(+SC-TRACE+ "")
      #(+SC-TRACE-EVENT-FILTER+ "")
      #(+SC-TRACE-INHERIT+ "")
      #(+SC-TRACE-LOG+ "")
      #(+SC-LEVEL1-ICACHE-SIZE+ "")
      #(+SC-LEVEL1-ICACHE-ASSOC+ "")
      #(+SC-LEVEL1-ICACHE-LINESIZE+ "")
      #(+SC-LEVEL1-DCACHE-SIZE+ "")
      #(+SC-LEVEL1-DCACHE-ASSOC+ "")
      #(+SC-LEVEL1-DCACHE-LINESIZE+ "")
      #(+SC-LEVEL2-CACHE-SIZE+ "")
      #(+SC-LEVEL2-CACHE-ASSOC+ "")
      #(+SC-LEVEL2-CACHE-LINESIZE+ "")
      #(+SC-LEVEL3-CACHE-SIZE+ "")
      #(+SC-LEVEL3-CACHE-ASSOC+ "")
      #(+SC-LEVEL3-CACHE-LINESIZE+ "")
      #(+SC-LEVEL4-CACHE-SIZE+ "")
      #(+SC-LEVEL4-CACHE-ASSOC+ "")
      #(+SC-LEVEL4-CACHE-LINESIZE+ "")
      ))

;; duplicate names
(defconstant +SC-PAGE-SIZE+ +SC-PAGESIZE+ "")
(push '+SC-PAGE-SIZE+ *sysconf-names*)

(defconstant +SC-IOV-MAX+ +SC-UIO-MAXIOV+ "")
(push '+SC-IOV-MAX+ *sysconf-names*)

;; names starting at +SC-LEVEL1-ICACHE-SIZE+ + 50
(define-enum-list *sysconf-names*
    #(
      #(+SC-IPV6+ "Internet Protocol Version 6 is supported.")
      #(+SC-RAW-SOCKETS+ "Raw sockets are supported. Affected functions are getsockopt(), setsockopt().")
      #(+SC-V7-ILP32-OFF32+ "")
      #(+SC-V7-ILP32-OFFBIG+ "")
      #(+SC-V7-LP64-OFF64+ "")
      #(+SC-V7-LPBIG-OFFBIG+ "")
      #(+SC-SS-REPL-MAX+ "")
      #(+SC-TRACE-EVENT-NAME-MAX+ "")
      #(+SC-TRACE-NAME-MAX+ "")
      #(+SC-TRACE-SYS-MAX+ "")
      #(+SC-TRACE-USER-EVENT-MAX+ "")
      #(+SC-XOPEN-STREAMS+ "")
      #(+SC-THREAD-ROBUST-PRIO-INHERIT+ "")
      #(+SC-THREAD-ROBUST-PRIO-PROTECT+ "")
      )
  :start (+ +SC-LEVEL1-ICACHE-SIZE+ 50))

(setf *sysconf-names* (nreverse *sysconf-names*))

(defcfun ("sysconf" real-sysconf) :long (name :int))

(defun sysconf-number (keyword)
  "Return the value of +SC-*+ constant corresponding to KEYWORD."
  (symbol-value (intern (s+ "+SC-" (symbol-name keyword) #\+) :opsys-unix)))

(defun sysconf (name)
  "Return the runtime system configuration variables given by NAME. NAME should
be one of the values in *SYSCONF-NAMES* or a keyword without the SC- prefix.
Returns an integer."
  (let ((number (etypecase name
		  (keyword (sysconf-number name))
		  (integer name)))
	result)
    ;; We can't use this stupid trick on OpenBSD.
    #-os-t-has-errno-func (setf *errno* 0)
    ;; #+openbsd (when (= *errno* +EINVAL+)
    ;; 		(error 'posix-error
    ;; 		       :format-control
    ;; 		       "Crap on a crap stick. We have no way of knowing if ~
    ;; 			sysconf will actually fail or not. Way to go POSIX."
    ;; 		       :error-code *errno*))
    ;; We can't use the SYSCALL macro becuase sometime sysconf returns -1.
    (setf result (real-sysconf number))
    (when (and (< result 0) (= *errno* +EINVAL+))
      (error 'posix-error
	     :error-code *errno*))
    result))

(defun processor-count ()
  "Return the number of processors in the system."
  (sysconf +sc-nprocessors-onln+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sysinfo

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+linux (config-feature :os-t-has-sysinfo))

#+os-t-has-sysinfo
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defctype kernel-ulong-t :unsigned-long)
    (defctype kernel-long-t :long))

  ;; (eval-when (:compile-toplevel :load-toplevel :execute)
  ;;   (defconstant +sysinfo-pad-size+
  ;;     ))

  ;; Since Linux 2.3.23 (i386), 2.3.48 (all architectures)
  ;;
  ;; "I say as a public statement of substantial fact, with malice and
  ;;  forethought, that every version of Linux, past, present, and future,
  ;;  has been emitted from a donkey's arse."
  ;;    -- Simon Beuccephalus, c. 1998 [convicted of defamation]
  ;;

  (defcstruct foreign-sysinfo
    (uptime       kernel-long-t)
    (loads        kernel-ulong-t :count 3)
    (totalram     kernel-ulong-t)
    (freeram      kernel-ulong-t)
    (sharedram    kernel-ulong-t)
    (bufferram    kernel-ulong-t)
    (totalswap    kernel-ulong-t)
    (freeswap     kernel-ulong-t)
    (procs        :uint16)
    (pad          :uint16)
    (totalhigh    kernel-ulong-t)
    (freehigh     kernel-ulong-t)
    (mem_unit     :uint32)
    (_f		  :char :count #. (- 20 (* 2 (foreign-type-size :unsigned-long))
				     (foreign-type-size :uint32))))

  (defcfun ("sysinfo" real-sysinfo) :int
    (info (:pointer (:struct foreign-sysinfo))))

  (defstruct sysinfo
    uptime
    load-averages
    total-memory
    free-memory
    shared-memory
    buffer-memory
    total-swap
    free-swap
    processes
    total-high-memory
    free-high-memory
    memory-unit-bytes)

  (defun convert-sysinfo (sysinfo)
    (if (and (pointerp sysinfo) (null-pointer-p sysinfo))
	nil
	(with-foreign-slots ((uptime
			      loads
			      totalram
			      freeram
			      sharedram
			      bufferram
			      totalswap
			      freeswap
			      procs
			      totalhigh
			      freehigh
			      mem_unit) sysinfo (:struct foreign-sysinfo))
	  (make-sysinfo
	   :uptime              uptime
	   :load-averages       (let ((a (make-array
					  3 :element-type 'float
					  :initial-element 0.0)))
				  (dotimes (i 3)
				    (setf (aref a i)
					  (/ (mem-aref loads 'kernel-ulong-t i)
					     (ash 1 16))))
				  a)
	   :total-memory        totalram
	   :free-memory         freeram
	   :shared-memory       sharedram
	   :buffer-memory       bufferram
	   :total-swap          totalswap
	   :free-swap           freeswap
	   :processes           procs
	   :total-high-memory   totalhigh
	   :free-high-memory    freehigh
	   :memory-unit-bytes   mem_unit))))

  (defun sysinfo ()
    (with-foreign-object (info '(:struct foreign-sysinfo))
      (syscall (real-sysinfo info))
      (convert-sysinfo info)))

  (defvar *sysinfo-names* nil
    "Alist of keywords and slot names of sysinfo values.")

  (defun get-sysinfo-names ()
    (or *sysinfo-names*
	(setf *sysinfo-names*
	      (mapcar (_ (cons (keywordify (mop:slot-definition-name _))
			       (mop:slot-definition-name _)))
		      (mop:class-direct-slots (find-class 'sysinfo))))))

  (defun sysinfo-names ()
    (mapcar #'car (get-sysinfo-names)))

  (defun sysinfo-slot-name (key)
    (cdr (assoc key (get-sysinfo-names))))

  (defun get-sysinfo-item (key)
    (when (position key (sysinfo-names))
      (slot-value (sysinfo) (sysinfo-slot-name key))))

  (defun process-sysinfo-names (names)
    (let ((s-names (intersection (sysinfo-names) names))
	  result)
      (if s-names
	  (progn
	    (loop :with s = (sysinfo)
	       :for n :in s-names
	       :do (push (cons n (slot-value s (sysinfo-slot-name n)))
			 result))
	    (values result (nset-difference names s-names)))
	  (values nil names))))

  #|
  Should maybe just get these from sysconf?
  :int get_nprocs_conf (void) __attribute__ ((__nothrow__ , __leaf__));
  :int get_nprocs (void) __attribute__ ((__nothrow__ , __leaf__));
  :long get_phys_pages (void) __attribute__ ((__nothrow__ , __leaf__));
  :long get_avphys_pages (void) __attribute__ ((__nothrow__ , __leaf__))
  |#
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getrlimit/setrlimit

(defparameter *rlimit-resources* nil "Names for rlimit resources.")

(define-enum-list *rlimit-resources*
    #(#(+RLIMIT-CPU+	    "Per-process CPU limit, in seconds.")
      #(+RLIMIT-FSIZE+	    "Largest file that can be created, in bytes.")
      #(+RLIMIT-DATA+	    "Maximum size of data segment, in bytes.")
      #(+RLIMIT-STACK+	    "Maximum size of stack segment, in bytes.")
      #(+RLIMIT-CORE+	    "Largest core file that can be created, in bytes.")
      #(+RLIMIT-RSS+	    "Largest resident set size, in bytes.")
      #(+RLIMIT-NPROC+	    "Number of processes.")
      #(+RLIMIT-NOFILE+	    "Number of open files.")
      #(+RLIMIT-MEMLOCK+    "Locked-in-memory address space.")
      #(+RLIMIT-AS+	    "Address space limit.")
      #(+RLIMIT-LOCKS+	    "Maximum number of file locks.")
      #(+RLIMIT-SIGPENDING+ "Maximum number of pending signals.")
      #(+RLIMIT-MSGQUEUE+   "Maximum bytes in POSIX message queues.")
      #(+RLIMIT-NICE+	    "Maximum nice priority allowed to raise to. Nice levels 19 .. -20 correspond to 0 .. 39 values of this resource limit.")
      #(+RLIMIT-RTPRIO+	    "Maximum realtime priority allowed for non-priviledged processes.")
      #(+RLIMIT-RTTIME+	    "Maximum CPU time in µs that a process scheduled under a real-time scheduling policy may consume without making a blocking system call before being forcibly descheduled.")
      ))

(defconstant +RLIMIT-OFILE+ +RLIMIT-NOFILE+ "Number of open files.")
;;(push '+RLIMIT-OFILE+ *rlimit-resources*)

(setf *rlimit-resources* (nreverse *rlimit-resources*))

(defcstruct foreign-rlimit
  (rlim_cur rlim-t)			; soft limit
  (rlim_max rlim-t))			; hard limit

(defcfun ("getrlimit" real-getrlimit) :int (resource :int)
	 (rlim (:pointer (:struct foreign-rlimit))))

(defcfun ("setrlimit" real-setrlimit) :int (resource :int)
	 (rlim (:pointer (:struct foreign-rlimit))))

#+linux
(defcfun ("prlimit" real-prlimit) :int (pid pid-t)
	 (resource :int)
	 (new-limit (:pointer (:struct foreign-rlimit)))
	 (old-limit (:pointer (:struct foreign-rlimit))))

(defstruct rlimit
  "System resource limit."
  current				; soft limit
  maximum				; hard limit
  )

(defun rlimit-number (resource)
  "Return the value of +SC-*+ constant corresponding to KEYWORD."
  (etypecase resource
    (keyword (symbol-value
	      (intern (s+ "+RLIMIT-" (symbol-name resource) #\+) :opsys-unix)))
    (integer resource)))

(defun getrlimit (resource)
  (with-foreign-object (limit '(:struct foreign-rlimit))
    (with-foreign-slots ((rlim_cur rlim_max) limit (:struct foreign-rlimit))
      (syscall (real-getrlimit (rlimit-number resource) limit))
      (make-rlimit :current rlim_cur :maximum rlim_max))))

(defun setrlimit (resource rlimit)
  (with-foreign-object (limit '(:struct foreign-rlimit))
    (with-foreign-slots ((rlim_cur rlim_max) limit (:struct foreign-rlimit))
      (setf rlim_cur (rlimit-current rlimit)
	    rlim_max (rlimit-maximum rlimit))
      (syscall (real-setrlimit (rlimit-number resource) limit))))
  rlimit)

#+linux
(defun prlimit (pid resource new-limit)
  (with-foreign-objects ((new-rlim '(:struct foreign-rlimit))
			 (old-rlim '(:struct foreign-rlimit)))
    (setf (foreign-slot-value new-rlim '(:struct foreign-rlimit) 'rlim_cur)
	  (rlimit-current new-limit)
	  (foreign-slot-value new-rlim '(:struct foreign-rlimit) 'rlim_max)
	  (rlimit-maximum new-limit))
    (syscall (real-prlimit pid (rlimit-number resource) new-rlim old-rlim))
    (make-rlimit
     :current (foreign-slot-value old-rlim
				  '(:struct foreign-rlimit) 'rlim_cur)
     :maximum (foreign-slot-value old-rlim
				  '(:struct foreign-rlimit) 'rlim_max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-system-info
;;

(defparameter *system-info-name-table* nil
  "Hash table of system info names to internal symbols.")

(defparameter *system-info-names* nil
  "Array of system info names.")

;; @@@ Doesn't this seem stupid? Should I just intern them and set their value?
;; I'm doing this not to pollute the package, but does it matter? Maybe just use
;; the SYMBOL-PLIST?
(defun make-system-info-names ()
  (setf *system-info-name-table* (make-hash-table))

  ;; Gather names from sysconf +SC-*+
  ;; @@@ Some of these are named so badly, that I really want to change them.
  ;; e.g. +SC-SELECT+
  (loop :with s
     :for n :in *sysconf-names*
     :do
     (setf s (string n)
	   s (subseq s 4 (1- (length s)))
	   (gethash (keywordify s) *system-info-name-table*) n))

  ;; Names from sysinfo
  #+os-t-has-sysinfo
  (loop :for key :in (sysinfo-names)
     :do (setf (gethash key *system-info-name-table*) (sysinfo-slot-name key)))

  #+os-t-has-sysctl
  (loop :for key :in (sysctl-names)
     :do (setf (gethash key *system-info-name-table*) key))
  
  ;; Make the arry from the hash table.
  (setf *system-info-names*
	(make-array (hash-table-count *system-info-name-table*)))
  (loop :with i = 0
     :for k :being :the :hash-keys :of *system-info-name-table*
     :do (setf (aref *system-info-names* i) k)
     (incf i))
  *system-info-names*)

(defun system-info-name-table ()
  (or *system-info-name-table*
      (progn (make-system-info-names) *system-info-name-table*)))

(defun system-info-names ()
  (or *system-info-names* (make-system-info-names)))

(defun system-info-description (name)
  (documentation (gethash name (system-info-name-table)) 'variable))

(defun get-system-info-item (name)
  (let ((n (gethash name (system-info-name-table))))
    (cond
      ((not n)
       (error "Unknown system info item ~s." name))
      ((equal (subseq (string n) 0 4) "+SC-")
       (sysconf (symbol-value n))))))

(defun get-system-info (names)
  (let (result)
    (etypecase names
      (list
       (when names
	 #+os-t-has-sysinfo
	 (setf (values result names) (process-sysinfo-names names))
	 #+os-t-has-sysctl
	 (setf (values result names) (process-sysctl-names names))
	 (setf result
	       (append result
		       (loop :for n :in names
			  :collect (cons n (get-system-info-item n)))))))
      (symbol
       (or #+os-t-has-sysinfo (get-sysinfo-item names)
	   #+os-t-has-sysctl (get-sysctl-item names)
	   (get-system-info-item names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct uname
  sysname
  nodename
  release
  version
  machine
  domainname)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constants #(
   ;; Name            D    L   S   F    O
   #(+UTSNAME-LENGTH+ 256  65  256 256  256 "Size of utsname strings."))))

#+linux
(defcstruct utsname
  (sysname    :char :count #.+UTSNAME-LENGTH+)
  (nodename   :char :count #.+UTSNAME-LENGTH+)
  (release    :char :count #.+UTSNAME-LENGTH+)
  (version    :char :count #.+UTSNAME-LENGTH+)
  (machine    :char :count #.+UTSNAME-LENGTH+)
  (domainname :char :count #.+UTSNAME-LENGTH+))

#+(or freebsd openbsd darwin)
(defcstruct utsname
  (sysname    :char :count #.+UTSNAME-LENGTH+)
  (nodename   :char :count #.+UTSNAME-LENGTH+)
  (release    :char :count #.+UTSNAME-LENGTH+)
  (version    :char :count #.+UTSNAME-LENGTH+)
  (machine    :char :count #.+UTSNAME-LENGTH+))

#+freebsd
(defcfun ("__xuname" xuname) :int
  "Variable record size uname."
  (size :int)
  (buf :pointer))

(defcfun ("uname" real-uname) :int 
  "Return system information in the structure pointed to by buf."
  (buf (:pointer (:struct utsname))))

(defun uname ()
  (with-foreign-object (buf '(:struct utsname))
    (syscall
     #+freebsd (xuname +UTSNAME-LENGTH+ buf)
     #-freebsd (real-uname buf)
     )
    (with-foreign-slots ((sysname nodename release version machine
				  #+linux domainname)
			 buf (:struct utsname))
      (make-uname
       :sysname    (foreign-string-to-lisp sysname)
       :nodename   (foreign-string-to-lisp nodename)
       :release    (foreign-string-to-lisp release)
       :version    (foreign-string-to-lisp version)
       :machine    (foreign-string-to-lisp machine)
       :domainname
       #+linux (foreign-string-to-lisp domainname)
       #-linux ""
       ))))

(defun os-machine-instance ()
  "Like MACHINE-INSTANCE, but without implementation variation."
  ;;(gethostname)
  (uname-nodename (uname)))

(defun os-machine-type ()
  "Like MACHINE-TYPE, but without implementation variation."
  ;; We should normalize this so that it can be directly turned into
  ;; the feaure, e.g. uname -m is "x86_64" but we want "X86-64"
  (let ((base (substitute #\- #\_ (string-upcase (uname-machine (uname))))))
    (cdr (assoc base
		'((("AMD64") . "X86-64"))
		:test (lambda (a b) (find a b :test #'string-equal))))))

(defun os-machine-version ()
  "Like MACHINE-VERSION, but without implementation variation."
;; Machine-Version                 : "Intel(R) Core(TM) i7-3615QM CPU @ 2.30GHz"
  (or #+linux
      (with-open-file (stream "/proc/cpuinfo" :direction :input)
	(let (line)
	  (loop :while (and (setf line (read-line stream nil))
			    (not (begins-with "model name" line))))
	  (when line
	    (let ((pos (position #\: line)))
	      (subseq line (or (and pos (+ 2 pos)) 0))))))
      #+darwin (sysctl "machdep.cpu.brand_string" :string)
      #+openbsd (sysctl "hw.hw-model" :string)
      #+freebsd (sysctl "hw.model" :string)
      "unknown"))

(defun os-software-type ()
  "Like SOFTWARE-TYPE, but without implementation variation."
  (uname-sysname (uname)))

(defun os-software-version ()
  "Like SOFTWARE-VERSION, but without implementation variation."
  (uname-release (uname)))

;; If you realy need `uname -v` than you can just call uname based on
;; the generic os-software-type and os-software-version.
;;
;; What about cases where uname -i differs from uname -m ?
;; -i, --hardware-platform print the hardware platform (non-portable)

#|
 uname -o 
   -o, --operating-system print the operating system

is `configure`ed into uname, translating the thing guessed by configure into a
nice (or semi-offical) name for humans to read, which if it doesn't have any
stylized capitalization, punctuation, or spaces
(like 'SunOS', "GNU/kFreeBSD", or 'Plan 9')
then it's just the capitalized version from uname.

which in turn comes from the immense hack that is the combination of the shell
scripts: `config.guess` AND `config.sub`
which usually gets it's information from the `uname` command anyway,
which means the whole thing is of course meta-circular and image based!

config.guess also gets stuff from the `sysctl` command,
C compiler pre-defined pre-processor defininitions,
(which are also meta-circular) and
a whole wacked slew of other system specific commands from the amazingly unixly
divergent 1980-1990's, like:
  `arch`, `sizer` `psrinfo` `universe` `oslevel`, `lsdev` `lsattr`, `getconf`
  `sysversion` `getsysinfo` `objdump` etc.
and files (or pseduo-files) sitting around in places like:
  /usr/include/, /usr/options/, /etc/, /proc/, /<kernel-name>,
  /etc/motd (which some old O/Ss pinch from the kernel binary!)

Another point of "interjection" is that the GNU uname command can assume it's
on "GNU/Linux" vs. just "Linux" without a GNU userland, by virtue of it being
itself, but we would have to either sniff libc for glibc specific symbols or
do horribly hackish probing around for files.

Even if one was going to support every historical machine-OS-vendor combo, one
would do it a different, hopefully better, way.

[this is really only of historical interest]

blah blah blah

If we really need to, we can hack up our own version of `uname -o` from the
combination of the glibc/kernel uname and/or *features*. I think anything more
than that is just excess pandering to a world of trouble.

|#

;; End
