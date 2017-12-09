;;
;; unix.lisp - Interface to UNIX-like systems.
;;

;; Convetions:
;; - Unix system calls should usually be wrapped in SYSCALL which will
;;   check return values and throw errors.
;; - Most API types should be defined with DEFCTYPE.
;; - Make sure the interface is nicely callable from Lisp code, e.i. don't
;;   make the caller pass pointers or have deal with foreign memory. Things
;;   that just return ints probably don't need wrapping.
;; - If you make a Lispy-er wrapper, call the original C version real-<func>.
;; - See the conventions in opsys.lisp.

(defpackage :opsys-unix
  (:documentation "Interface to UNIX-like systems.")
  (:use :cl :cffi :dlib :opsys-base)
  (:nicknames :os-unix :uos)
  (:export
   ;; defining macros
   #:*platform-index* #:*platform-bitsize-index*
   #:define-simple-types #:define-constants
   #:define-constants-from #:define-name-list-from

   ;; types
   #:time-t #:mode-t #:uid-t #:gid-t #:pid-t #:wchar-t #:suseconds-t
   #:dev-t #:nlink-t #:ino-t #:off-t #:quad-t #:blkcnt-t #:blksize-t #:fixpt-t
   #:sigset-t #:file-ptr #:fpos-t #:wint-t

   ;; error handling
   #:*errno*
   #:strerror
   #:error-message
   #:posix-error
   #:error-check
   #:syscall

   ;; error number constants
   #:+EPERM+ #:+ENOENT+ #:+ESRCH+ #:+EINTR+ #:+EIO+ #:+ENXIO+ #:+E2BIG+
   #:+ENOEXEC+ #:+EBADF+ #:+ECHILD+ #:+EDEADLK+ #:+ENOMEM+ #:+EACCES+
   #:+EFAULT+ #:+ENOTBLK+ #:+EBUSY+ #:+EEXIST+ #:+EXDEV+ #:+ENODEV+ #:+ENOTDIR+
   #:+EISDIR+ #:+EINVAL+ #:+ENFILE+ #:+EMFILE+ #:+ENOTTY+ #:+ETXTBSY+ #:+EFBIG+
   #:+ENOSPC+ #:+ESPIPE+ #:+EROFS+ #:+EMLINK+ #:+EPIPE+ #:+EDOM+ #:+ERANGE+
   #:+EAGAIN+ #:+EWOULDBLOCK+ #:+EINPROGRESS+ #:+EALREADY+ #:+ENOTSOCK+
   #:+EDESTADDRREQ+ #:+EMSGSIZE+ #:+EPROTOTYPE+ #:+ENOPROTOOPT+
   #:+EPROTONOSUPPORT+ #:+ESOCKTNOSUPPORT+ #:+ENOTSUP+ #:+EOPNOTSUPP+
   #:+EPFNOSUPPORT+ #:+EAFNOSUPPORT+ #:+EADDRINUSE+ #:+EADDRNOTAVAIL+
   #:+ENETDOWN+ #:+ENETUNREACH+ #:+ENETRESET+ #:+ECONNABORTED+ #:+ECONNRESET+
   #:+ENOBUFS+ #:+EISCONN+ #:+ENOTCONN+ #:+ESHUTDOWN+ #:+ETOOMANYREFS+
   #:+ETIMEDOUT+ #:+ECONNREFUSED+ #:+ELOOP+ #:+ENAMETOOLONG+ #:+EHOSTDOWN+
   #:+EHOSTUNREACH+ #:+ENOTEMPTY+ #:+EPROCLIM+ #:+EUSERS+ #:+EDQUOT+ #:+ESTALE+
   #:+EREMOTE+ #:+EBADRPC+ #:+ERPCMISMATCH+ #:+EPROGUNAVAIL+ #:+EPROGMISMATCH+
   #:+EPROCUNAVAIL+ #:+ENOLCK+ #:+ENOSYS+ #:+EFTYPE+ #:+EAUTH+ #:+ENEEDAUTH+
   #:+EPWROFF+ #:+EDEVERR+ #:+EOVERFLOW+ #:+EBADEXEC+ #:+EBADARCH+
   #:+ESHLIBVERS+ #:+EBADMACHO+ #:+ECANCELED+ #:+EIDRM+ #:+ENOMSG+ #:+EILSEQ+
   #:+ENOATTR+ #:+EBADMSG+ #:+EMULTIHOP+ #:+ENODATA+ #:+ENOLINK+ #:+ENOSR+
   #:+ENOSTR+ #:+EPROTO+ #:+ETIME+ #:+ENOPOLICY+ #:+ELAST+

   ;; info
   #:environment
   #:environment-variable
   #:getenv
   #:setenv
   #:unsetenv
   #:sysctl
   #:getpagesize
   #:memory-page-size
   #:getauxval
   #:getlogin
   #:sysconf
   #:*sysconf-names*

   #:passwd				; struct
   #:passwd-name
   #:passwd-passwd
   #:passwd-uid
   #:passwd-gid
   #:passwd-pw-change
   #:passwd-pw-class
   #:passwd-pw-age
   #:passwd-pw-comment
   #:passwd-gecos
   #:passwd-dir
   #:passwd-shell
   #:passwd-pw-expire

   #:getpwuid
   #:getpwnam
   #:getpwent
   #:endpwent
   #:setpwent

   #:get-user-info
   #:user-home
   #:user-name-char-p
   #:valid-user-name
   #:user-name
   #:user-id
   #:user-full-name
   #:get-next-user
   #:user-list
   #:refresh-user-list
   #:is-administrator
   
   #:group
   #:group-name
   #:group-id
   #:group-entry
   #:group-entry-name
   #:group-entry-passwd
   #:group-entry-gid
   #:group-entry-members
   #:get-next-group
   #:group-list
   #:refresh-group-list

   #:getgrgid
   #:getgrnam
   #:getgrent
   #:endgrent

   ;; user login accounting
   #:utmpx #:utmpx-user #:utmpx-id #:utmpx-line #:utmpx-pid #:utmpx-type
   #:utmpx-tv #:utmpx-host
   #:endutxent
   #:getutxent
   #:getutxid
   #:getutxline
   #:pututxline
   #:setutxent
   #:users-logged-in
   #:setlogin

   ;; directories
   #:hidden-file-name-p
   #:superfluous-file-name-p
   ;;#:dir-entry-inode
   #:change-directory
   #:pathconf
   #:get-path-max
   #:current-directory
   #:make-directory
   #:delete-directory
   #:dirent-name
   #:dirent-type
   #:read-directory
   #:probe-directory
   #:without-access-errors

   ;; files (low level)
   #:+O_RDONLY+ #:+O_WRONLY+ #:+O_RDWR+ #:+O_ACCMODE+ #:+O_NONBLOCK+
   #:+O_APPEND+ #:+O_SYNC+ #:+O_SHLOCK+ #:+O_EXLOCK+ #:+O_CREAT+ #:+O_TRUNC+
   #:+O_EXCL+
   #:posix-open
   #:posix-close
   #:posix-read
   #:posix-write
   #:posix-ioctl
   #:with-posix-file
   #:with-os-file
   #:mkstemp
   #:fcntl
   #:get-file-descriptor-flags

   ;; stat
   #:stat
   #:lstat
   #:fstat
   #:get-file-info
   #:+S_IFMT+ #:+S_IFIFO+ #:+S_IFCHR+ #:+S_IFDIR+ #:+S_IFBLK+ #:+S_IFREG+
   #:+S_IFLNK+ #:+S_IFSOCK+ #:+S_IFWHT+ #:+S_ISUID+ #:+S_ISGID+ #:+S_ISVTX+
   #:+S_IRUSR+ #:+S_IWUSR+ #:+S_IXUSR+
   #:is-user-readable
   #:is-user-writable
   #:is-user-executable
   #:is-group-readable
   #:is-group-writable
   #:is-group-executable
   #:is-other-readable
   #:is-other-writable
   #:is-other-executable
   #:is-set-uid
   #:is-set-gid
   #:is-sticky
   #:is-fifo
   #:is-character-device
   #:is-directory
   #:is-block-device
   #:is-regular-file
   #:is-symbolic-link
   #:is-socket
   #:is-door
   #:is-whiteout
   #:file-type-char
   #:file-type-name
   #:file-type-symbol
   #:symbolic-mode
   #:file-exists
   #:simple-delete-file
   #:readlink

   #:UF_SETTABLE #:UF_NODUMP #:UF_IMMUTABLE #:UF_APPEND #:UF_OPAQUE
   #:UF_NOUNLINK #:UF_COMPRESSED #:UF_TRACKED #:UF_HIDDEN #:SF_SETTABLE
   #:SF_ARCHIVED #:SF_IMMUTABLE #:SF_APPEND #:SF_RESTRICTED #:SF_SNAPSHOT
   #:flag-user-settable
   #:flag-user-nodump
   #:flag-user-immutable
   #:flag-user-append
   #:flag-user-opaque
   #:flag-user-nounlink
   #:flag-user-compressed
   #:flag-user-tracked
   #:flag-user-hidden
   #:flag-root-settable
   #:flag-root-archived
   #:flag-root-immutable
   #:flag-root-append
   #:flag-root-restricted
   #:flag-root-snapshot
   #:flags-string

   #:umask
   #:chmod #:fchmod
   #:chown #:fchown #:lchown
   #:sync
   #:extended-attribute-list
   #:extended-attribute-value

   #:is-executable
   #:command-pathname

   #:data-dir
   #:config-dir
   #:data-path
   #:config-path
   #:cache-dir
   #:runtime-dir
   
   #:timespec
   #:timespec-seconds
   #:timespec-nanoseconds
   #:file-status
   #:file-status-device
   #:file-status-inode
   #:file-status-mode
   #:file-status-links
   #:file-status-uid
   #:file-status-gid
   #:file-status-device-type
   #:file-status-access-time
   #:file-status-modify-time
   #:file-status-change-time
   #:file-status-size
   #:file-status-blocks
   #:file-status-block-size
   #:file-status-flags
   #:file-status-generation

   ;; signals
   #:*signal-count*
   #:signal-name
   #:signal-description
   #:signal-action
   #:set-signal-action
   #:with-signal-handlers
   #:describe-signals
   #:kill
   #:killpg

   #:+SIGHUP+ #:+SIGINT+ #:+SIGQUIT+ #:+SIGILL+ #:+SIGTRAP+ #:+SIGABRT+
   #:+SIGPOLL+ #:+SIGEMT+ #:+SIGFPE+ #:+SIGKILL+ #:+SIGBUS+ #:+SIGSEGV+
   #:+SIGSYS+ #:+SIGPIPE+ #:+SIGALRM+ #:+SIGTERM+ #:+SIGURG+ #:+SIGSTOP+
   #:+SIGTSTP+ #:+SIGCONT+ #:+SIGCHLD+ #:+SIGTTIN+ #:+SIGTTOU+ #:+SIGIO+
   #:+SIGXCPU+ #:+SIGXFSZ+ #:+SIGVTALRM+ #:+SIGPROF+ #:+SIGWINCH+ #:+SIGINFO+
   #:+SIGUSR1+ #:+SIGUSR2+ #:+SIGSTKFLT+ #:+SIGPWR+
   
   ;; processes
   #:system
   #:getrusage
   #:timeval #:timeval-seconds #:timeval-micro-seconds
   #:rusage #:rusage-user #:rusage-system
   #:process-times
   #:_exit
   #:exec
   #:fork
   #:wait
   #:fork-and-exec
   #:getuid #:geteuid
   #:setuid #:seteuid
   #:getgid #:getegid
   #:setgid #:setegid
   #:getpid
   #:getppid
   #:getpgid
   #:setpgid
   #:tcsetpgrp
   #:tcgetpgrp
   #:setsid
   #:getsid
   #:getgroups
   #:get-groups

   #:process-list
   #:suspend-process
   #:resume-process
   #:terminate-process

   #:*rlimit-resources*
   #:+RLIMIT-CPU+ #:+RLIMIT-FSIZE+ #:+RLIMIT-DATA+ #:+RLIMIT-STACK+
   #:+RLIMIT-CORE+ #:+RLIMIT-RSS+ #:+RLIMIT-NPROC+ #:+RLIMIT-NOFILE+
   #:+RLIMIT-MEMLOCK+ #:+RLIMIT-AS+ #:+RLIMIT-LOCKS+ #:+RLIMIT-SIGPENDING+
   #:+RLIMIT-MSGQUEUE+ #:+RLIMIT-NICE+ #:+RLIMIT-RTPRIO+ #:+RLIMIT-RTTIME+
   #:+RLIMIT-NLIMITS+ #:+RLIMIT-OFILE+
   #:rlimit #:rlimit-current #:rlimit-maximum
   #:setrlimit #:getrlimit
   #+linux #:prlimit
   
   #:popen
   #:posix-pipe
   #+linux #:linux-splice
   #+linux #:linux-vmsplice
   #+linux #:linux-tee

   ;; time
   #:+unix-to-universal-time+
   #:unix-to-universal-time
   #:universal-to-unix-time
   #:gettimeofday
   #:settimeofday
   #:get-time
   #:set-time

   ;; multiplexed io
   #:lame-poll
   #:lame-select
   #:listen-for

   ;; filesystems
   #:fstab #:fstab-spec #:fstab-file #:fstab-vfstype #:fstab-mntops
   #:fstab-type #:fstab-freq #:fstab-passno
   #:getfsent #:getfsspec #:getfsfile #:setfsent #:endfsent

   #:statfs
   #:statfs-bsize
   #:statfs-iosize
   #:statfs-blocks
   #:statfs-bfree
   #:statfs-bavail
   #:statfs-files
   #:statfs-ffree
   #:statfs-fsid
   #:statfs-owner
   #:statfs-type
   #:statfs-flags
   #:statfs-fssubtype
   #:statfs-fstypename
   #:statfs-mntonname
   #:statfs-mntfromname

   #:getmntinfo
   #:mounted-filesystems
   #:mount-point-of-file

   ;; Terminal things (which don't need to be in :termios)
   #:isatty
   #:ttyname
   #:file-handle-terminal-p
   #:file-handle-terminal-name
   #:*default-console-device-name*
   #:open-terminal
   #:close-terminal
   #:read-terminal-char
   #:read-terminal-byte
   #:read-until
   #:write-terminal-char
   #:write-terminal-string
   ;; from termios
   #:set-terminal-mode
   #:get-terminal-mode
   #:get-window-size
   #:slurp-terminal

   ;; Character coding / localization
   #:wcwidth
   #:char-width
   ))
(in-package :opsys-unix)

(declaim (optimize (debug 3)))

;; Macros for convenient defining of platform specific things.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *platform-index*
    (or
     #+darwin		1
     #+linux		2
     #+sunos		3
     #+freebsd		4
     nil))

  (defparameter *platform-bitsize-index*
    (or
     #+darwin				1
     #+(and linux 32-bit-target)	2
     #+(and linux 64-bit-target)	3
     #+sunos				4
     #+(and freebsd 64-bit-target)	5
     nil))

  (when (not (and *platform-index* *platform-bitsize-index*))
    (error "We don't know about your platform."))

  (defmacro define-simple-types (types-array)
    "Define the appropriate types for the platform from the given TYPES-ARRAY."
    `(progn
       ,@(loop :for type :across types-array
	    :collect
	    `(defctype ,(aref type 0) ,(aref type *platform-bitsize-index*)))))

  (defmacro define-constants (constant-array)
    "Define the appropriate constant for the platform from the given
CONSTANT-ARRAY."
    `(progn
       ,@(loop :for type :across constant-array
	    :collect
	    `(defconstant ,(aref type 0) ,(aref type *platform-index*)))))

  (defmacro define-constants-from (constant-array)
    "Define the appropriate constants for the platform from the array in the
given CONSTANT-ARRAY variable."
    `(progn
       ,@(loop :for type :across constant-array
	    :collect
	    `(defconstant ,(aref type 0) ,(aref type *platform-index*)))))

  (defmacro define-name-list-from (name constant-array &optional docstring)
    "Define a variable named NAME as a list of the appropriate platform entries
from the CONSTANT-ARRAY variable, that are non-NIL."
    `(defparameter ,name
       '(,@(loop :for v :across constant-array
	      :when (aref v *platform-index*)
	      :collect (aref v 0)))
       ,@(or (list docstring))))

  (defmacro define-enum-list (list-var constant-array &key (start 0))
    "Define enumerated constants and put the names in LIST-VAR."
    (with-unique-names (offset)
      `(progn
  	 (eval-when (:compile-toplevel :load-toplevel :execute)
  	   (let ((,offset ,start))
  	     ,@(loop :with name :and doc :and i = 0
  		  :for c :across constant-array
  		  :do
  		  (setf name  (aref c 0)
  			doc   (aref c 1))
  		  :collect
  		  `(defconstant ,name (+ ,offset ,i) ,doc)
		  :do (incf i))))
  	 ,@(loop :for c :across constant-array
  	      :collect
  	      `(push ',(aref c 0) ,list-var)))))
  
  (defmacro define-to-list (list-var constant-array)
    "Define constants and put the names in LIST-VAR."
    `(progn
       ,@(loop :with name :and value :and doc
	    :for c :across constant-array :do
	    (setf name  (aref c 0)
		  value (aref c 1)
		  doc   (aref c 2))
	    :collect
	    `(defconstant ,name ,value ,doc)
	    :collect
	    `(push ',name ,list-var))))

;; C API types

(define-simple-types
  #(
#| This is just gonna go over 80 cols, so deal.
Type name     Darwin           Linux-32         Linux-64         SunOS		  FreeBSD 64 |#
#(time-t      :long            :long            :long            :long		  :int64)
#(mode-t      :uint16          :unsigned-int    :unsigned-int    :uint16	  :uint16)
#(uid-t       :uint32          :unsigned-int    :unsigned-int    :uint32	  :uint32)
#(gid-t       :uint32          :unsigned-int    :unsigned-int    :uint32	  :uint32)
#(pid-t       :int             :int             :int             :int		  :int32)
#(wchar-t     :int             :int             :int             :int		  :int)
#(suseconds-t :int32           :int32           :int32           :int32		  :long)
#(ssize-t     :long            :long            :long            :long		  :int64)
#(dev-t       :int32           :unsigned-long   :unsigned-long   :ulong		  :uint32)
#(nlink-t     :uint16          :unsigned-int    :unsigned-long   :uint		  :uint16)
#(ino-t       :uint64          :unsigned-long   :unsigned-long   :unsigned-long	  :uint32)
#(off-t       :int64           :int32           :long            :int32		  :int64)
#(quad-t      :int64           :int64           :int64           :int64		  :int64)
#(u-quad-t    :uint64          :uint64          :uint64          :uint64	  :uint64)
#(blkcnt-t    :int64           :unsigned-long   :long            :int64		  :int64)
#(blksize-t   :int64           :long            :unsigned-long   :int32		  :uint32)
#(fixpt-t     :uint32          :uint32          :uint32          :uint32	  :uint32)
#(boolean-t   :unsigned-int    :unsigned-int    :unsigned-int    :unsigned-int	  :unsigned-int)
#(segsz-t     :int32           :int32           :int32           :int32		  :int64)
#(caddr-t     (:pointer :char) (:pointer :char) (:pointer :char) (:pointer :char) (:pointer :char))
#(fsblkcnt-t  :unsigned-long   :unsigned-long   :unsigned-long   :unsigned-long	  :uint64)
#(fsword-t    :int             :int             :int             :int		  :int)
#(attrgroup-t :uint32          :uint32          :uint32          :uint32	  :uint32)
#(rlim-t     :uint64          :uint32          :unsigned-long   :uint32	  :uint64)
)))

#|
;; This was the old mess, which was replaced by the above table of types.

(defctype time-t :long)
(defctype mode-t #+(or darwin sunos) :uint16 #+linux :unsigned-int)
(defctype uid-t #+darwin :uint32 #+linux :unsigned-int)
(defctype gid-t #+darwin :uint32 #+linux :unsigned-int)
(defctype pid-t :int)
(defctype wchar-t :int)
(defctype suseconds-t :int32)
(defctype ssize-t :long)

#+darwin (defctype dev-t :int32)
#+sunos  (defctype dev-t :ulong)
;;#+linux  (defctype dev-t #+cffi-features:no-long-long :ulong
;;		         #-cffi-features:no-long-long :ullong)
#+(and linux (not 64-bit-target)) (defctype dev-t :unsigned-long)
#+(and linux 64-bit-target) (defctype dev-t :unsigned-long)
#+darwin (defctype nlink-t :uint16)
#+sunos (defctype nlink-t :uint)
#+(and linux (not 64-bit-target)) (defctype nlink-t :unsigned-int)
#+(and linux 64-bit-target) (defctype nlink-t :unsigned-long)
#+darwin (defctype ino-t :uint64)    ; for 32 & 64 ??
;; (defctype ino-t
;;     #+(and darwin 64-bit-target) :uint64
;;     #+(and darwin (not 64-bit-target)) :uint32)
;; #+(or sunos linux) :unsigned-long
#+(or sunos linux) (defctype ino-t :unsigned-long)
#+darwin (defctype off-t :int64)    ; for 32 & 64 ??
;;    #+(and darwin 64-bit-target) :int64
;;    #+(and darwin (not 64-bit-target)) :int32
#+sunos (defctype off-t :int32)
;#+(and linux 64-bit-target) (defctype off-t :int64)
#+(and linux 64-bit-target) (defctype off-t :long)
#+(and linux (not 64-bit-target)) (defctype off-t :int32)
;#+(or sunos linux) (defctype off-t :long)
#-cffi-features:no-long-long (defctype quad-t :int64)
#+cffi-features:no-long-long (defctype quad-t :int32) ; @@@ XXX wrong!
#-linux (defctype blkcnt-t #+64-bit-target :int64 #+32-bit-target :int32)
#-linux (defctype blksize-t #+64-bit-target :long #+32-bit-target :int32)
#+linux (defctype blkcnt-t #+64-bit-target :long #+32-bit-target :unsigned-long)
#+linux (defctype blksize-t :unsigned-long)
(defctype fixpt-t :uint32)
(defctype boolean-t :unsigned-int)
(defctype fixpt-t :uint32)
(defctype u-quad-t :uint64)
(defctype segsz-t :int32)
(defctype caddr-t (:pointer :char))

|#

#+freebsd (defctype fflags-t :uint32)

;; sigset
#+(or darwin sunos) (defctype sigset-t :uint32)

#+linux
(defcstruct foreign-sigset-t
  (value :unsigned-long :count
	 #.(/ 1024 (* 8 (cffi:foreign-type-size :unsigned-long)))))
;; unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];

#+freebsd (defcstruct foreign-sigset-t (__bits :uint32 :count 4))

#+(or linux freebsd)
(defctype sigset-t (:struct foreign-sigset-t))

(defcstruct foreign-timeval
  "Time for timer."
  (tv_sec	time-t)
  (tv_usec	suseconds-t))

(defstruct timeval
  "Time for timer."
  seconds
  micro-seconds)

(defun convert-timeval (timeval)
  (if (and (pointerp timeval) (null-pointer-p timeval))
      nil
      (with-foreign-slots ((tv_sec tv_usec) timeval (:struct foreign-timeval))
	(make-timeval :seconds tv_sec
		      :micro-seconds tv_usec))))

#+not ; old darwin?
(defcstruct foreign-rusage
  "Resource usage."
  (utime (:struct foreign-timeval))	; user time used
  (stime (:struct foreign-timeval))	; system time used
  (ixrss    :long)			; integral shared memory size
  (idrss    :long)			; integral unshared data
  (isrss    :long)			; integral unshared stack
  (minflt   :long)			; page reclaims
  (majflt   :long)			; page faults
  (nswap    :long)			; swaps
  (inblock  :long)			; block input operations
  (oublock  :long)			; block output operations
  (msgsnd   :long)			; messages sent
  (msgrcv   :long)			; messages recieved
  (nsignals :long)			; signals received
  (nvcsw    :long)			; voluntary context switches
  (nivcsw   :long))			; involuntary context switches

#+(or darwin linux freebsd)
(defcstruct foreign-rusage
  (ru_utime    (:struct foreign-timeval))
  (ru_stime    (:struct foreign-timeval))
  (ru_maxrss   :long)
  (ru_ixrss    :long)
  (ru_idrss    :long)
  (ru_isrss    :long)
  (ru_minflt   :long)
  (ru_majflt   :long)
  (ru_nswap    :long)
  (ru_inblock  :long)
  (ru_oublock  :long)
  (ru_msgsnd   :long)
  (ru_msgrcv   :long)
  (ru_nsignals :long)
  (ru_nvcsw    :long)
  (ru_nivcsw   :long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

#+(or darwin linux freebsd) (config-feature :os-t-has-strerror-r)

(defcvar ("errno" *errno*) :int) ; aka *errno*

#+darwin
(progn
  (define-constant +EPERM+		1 "Operation not permitted")
  (define-constant +ENOENT+		2 "No such file or directory")
  (define-constant +ESRCH+		3 "No such process")
  (define-constant +EINTR+		4 "Interrupted system call")
  (define-constant +EIO+		5 "Input/output error")
  (define-constant +ENXIO+		6 "Device not configured")
  (define-constant +E2BIG+		7 "Argument list too long")
  (define-constant +ENOEXEC+		8 "Exec format error")
  (define-constant +EBADF+		9 "Bad file descriptor")
  (define-constant +ECHILD+		10 "No child processes")
  (define-constant +EDEADLK+		11 "Resource deadlock avoided")
  (define-constant +ENOMEM+		12 "Cannot allocate memory")
  (define-constant +EACCES+		13 "Permission denied")
  (define-constant +EFAULT+		14 "Bad address")
  (define-constant +ENOTBLK+		15 "Block device required")
  (define-constant +EBUSY+		16 "Device / Resource busy")
  (define-constant +EEXIST+		17 "File exists")
  (define-constant +EXDEV+		18 "Cross-device link")
  (define-constant +ENODEV+		19 "Operation not supported by device")
  (define-constant +ENOTDIR+		20 "Not a directory")
  (define-constant +EISDIR+		21 "Is a directory")
  (define-constant +EINVAL+		22 "Invalid argument")
  (define-constant +ENFILE+		23 "Too many open files in system")
  (define-constant +EMFILE+		24 "Too many open files")
  (define-constant +ENOTTY+		25 "Inappropriate ioctl for device")
  (define-constant +ETXTBSY+		26 "Text file busy")
  (define-constant +EFBIG+		27 "File too large")
  (define-constant +ENOSPC+		28 "No space left on device")
  (define-constant +ESPIPE+		29 "Illegal seek")
  (define-constant +EROFS+		30 "Read-only file system")
  (define-constant +EMLINK+		31 "Too many links")
  (define-constant +EPIPE+		32 "Broken pipe")
  (define-constant +EDOM+		33 "Numerical argument out of domain")
  (define-constant +ERANGE+		34 "Result too large")
  (define-constant +EAGAIN+		35 "Resource temporarily unavailable")
  (define-constant +EWOULDBLOCK+	+EAGAIN+ "Operation would block")
  (define-constant +EINPROGRESS+	36 "Operation now in progress")
  (define-constant +EALREADY+		37 "Operation already in progress")
  (define-constant +ENOTSOCK+		38 "Socket operation on non-socket")
  (define-constant +EDESTADDRREQ+	39 "Destination address required")
  (define-constant +EMSGSIZE+		40 "Message too long")
  (define-constant +EPROTOTYPE+		41 "Protocol wrong type for socket")
  (define-constant +ENOPROTOOPT+	42 "Protocol not available")
  (define-constant +EPROTONOSUPPORT+	43 "Protocol not supported")
  (define-constant +ESOCKTNOSUPPORT+	44 "Socket type not supported")
  (define-constant +ENOTSUP+		45 "Operation not supported")
  ;(define-constant +EOPNOTSUPP+ +ENOTSUP+ "Operation not supported on socket")
  (define-constant +EPFNOSUPPORT+	46 "Protocol family not supported")
  (define-constant +EAFNOSUPPORT+	47 "Address family not supported by protocol family")
  (define-constant +EADDRINUSE+		48 "Address already in use")
  (define-constant +EADDRNOTAVAIL+	49 "Can't assign requested address")
  (define-constant +ENETDOWN+		50 "Network is down")
  (define-constant +ENETUNREACH+	51 "Network is unreachable")
  (define-constant +ENETRESET+		52 "Network dropped connection on reset")
  (define-constant +ECONNABORTED+	53 "Software caused connection abort")
  (define-constant +ECONNRESET+		54 "Connection reset by peer")
  (define-constant +ENOBUFS+		55 "No buffer space available")
  (define-constant +EISCONN+		56 "Socket is already connected")
  (define-constant +ENOTCONN+		57 "Socket is not connected")
  (define-constant +ESHUTDOWN+		58 "Can't send after socket shutdown")
  (define-constant +ETOOMANYREFS+	59 "Too many references: can't splice")
  (define-constant +ETIMEDOUT+		60 "Operation timed out")
  (define-constant +ECONNREFUSED+	61 "Connection refused")
  (define-constant +ELOOP+		62 "Too many levels of symbolic links")
  (define-constant +ENAMETOOLONG+	63 "File name too long")
  (define-constant +EHOSTDOWN+		64 "Host is down")
  (define-constant +EHOSTUNREACH+	65 "No route to host")
  (define-constant +ENOTEMPTY+		66 "Directory not empty")
  (define-constant +EPROCLIM+		67 "Too many processes")
  (define-constant +EUSERS+		68 "Too many users")
  (define-constant +EDQUOT+		69 "Disc quota exceeded")
  (define-constant +ESTALE+		70 "Stale NFS file handle")
  (define-constant +EREMOTE+		71 "Too many levels of remote in path")
  (define-constant +EBADRPC+		72 "RPC struct is bad")
  (define-constant +ERPCMISMATCH+	73 "RPC version wrong")
  (define-constant +EPROGUNAVAIL+	74 "RPC prog. not avail")
  (define-constant +EPROGMISMATCH+	75 "Program version wrong")
  (define-constant +EPROCUNAVAIL+	76 "Bad procedure for program")
  (define-constant +ENOLCK+		77 "No locks available")
  (define-constant +ENOSYS+		78 "Function not implemented")
  (define-constant +EFTYPE+		79 "Inappropriate file type or format")
  (define-constant +EAUTH+		80 "Authentication error")
  (define-constant +ENEEDAUTH+		81 "Need authenticator")
  (define-constant +EPWROFF+		82 "Device power is off")
  (define-constant +EDEVERR+		83 "Device error, e.g. paper out")
  (define-constant +EOVERFLOW+		84 "Value too large to be stored in data type")
  (define-constant +EBADEXEC+		85 "Bad executable")
  (define-constant +EBADARCH+		86 "Bad CPU type in executable")
  (define-constant +ESHLIBVERS+		87 "Shared library version mismatch")
  (define-constant +EBADMACHO+		88 "Malformed Macho file")
  (define-constant +ECANCELED+		89 "Operation canceled")
  (define-constant +EIDRM+		90 "Identifier removed")
  (define-constant +ENOMSG+		91 "No message of desired type */")
  (define-constant +EILSEQ+		92 "Illegal byte sequence")
  (define-constant +ENOATTR+		93 "Attribute not found")
  (define-constant +EBADMSG+		94 "Bad message")
  (define-constant +EMULTIHOP+		95 "Reserved")
  (define-constant +ENODATA+		96 "No message available on STREAM")
  (define-constant +ENOLINK+		97 "Reserved")
  (define-constant +ENOSR+		98 "No STREAM resources")
  (define-constant +ENOSTR+		99 "Not a STREAM")
  (define-constant +EPROTO+		100 "Protocol error")
  (define-constant +ETIME+		101 "STREAM ioctl timeout")
  (define-constant +EOPNOTSUPP+		102 "Operation not supported on socket")
  (define-constant +ENOPOLICY+		103 "No such policy registered")
  (define-constant +ELAST+		103 "Must be equal largest errno")
)

#+sunos
(progn
  (define-constant +EPERM+	1	"Not super-user")
  (define-constant +ENOENT+	2	"No such file or directory")
  (define-constant +ESRCH+	3	"No such process")
  (define-constant +EINTR+	4	"interrupted system call")
  (define-constant +EIO+	5	"I/O error")
  (define-constant +ENXIO+	6	"No such device or address")
  (define-constant +E2BIG+	7	"Arg list too long")
  (define-constant +ENOEXEC+	8	"Exec format error")
  (define-constant +EBADF+	9	"Bad file number")
  (define-constant +ECHILD+	10	"No children")
  (define-constant +EAGAIN+	11	"Resource temporarily unavailable")
  (define-constant +ENOMEM+	12	"Not enough core")
  (define-constant +EACCES+	13	"Permission denied")
  (define-constant +EFAULT+	14	"Bad address")
  (define-constant +ENOTBLK+	15	"Block device required")
  (define-constant +EBUSY+	16	"Mount device busy")
  (define-constant +EEXIST+	17	"File exists")
  (define-constant +EXDEV+	18	"Cross-device link")
  (define-constant +ENODEV+	19	"No such device")
  (define-constant +ENOTDIR+	20	"Not a directory")
  (define-constant +EISDIR+	21	"Is a directory")
  (define-constant +EINVAL+	22	"Invalid argument")
  (define-constant +ENFILE+	23	"File table overflow")
  (define-constant +EMFILE+	24	"Too many open files")
  (define-constant +ENOTTY+	25	"Inappropriate ioctl for device")
  (define-constant +ETXTBSY+	26	"Text file busy")
  (define-constant +EFBIG+	27	"File too large")
  (define-constant +ENOSPC+	28	"No space left on device")
  (define-constant +ESPIPE+	29	"Illegal seek")
  (define-constant +EROFS+	30	"Read only file system")
  (define-constant +EMLINK+	31	"Too many links")
  (define-constant +EPIPE+	32	"Broken pipe")
  (define-constant +EDOM+	33	"Math arg out of domain of func")
  (define-constant +ERANGE+	34	"Math result not representable")
  (define-constant +ENOMSG+	35	"No message of desired type")
  (define-constant +EIDRM+	36	"Identifier removed")
  (define-constant +ECHRNG+	37	"Channel number out of range")
  (define-constant +EL2NSYNC+   38	"Level 2 not synchronized")
  (define-constant +EL3HLT+	39	"Level 3 halted")
  (define-constant +EL3RST+	40	"Level 3 reset")
  (define-constant +ELNRNG+	41	"Link number out of range")
  (define-constant +EUNATCH+    42	"Protocol driver not attached")
  (define-constant +ENOCSI+	43	"No CSI structure available")
  (define-constant +EL2HLT+	44	"Level 2 halted")
  (define-constant +EDEADLK+	45	"Deadlock condition.")
  (define-constant +ENOLCK+	46	"No record locks available.")
  (define-constant +ECANCELED+  47	"Operation canceled")
  (define-constant +ENOTSUP+	48	"Operation not supported")
  ;; Filesystem Quotas
  (define-constant +EDQUOT+	49	"Disc quota exceeded")
  ;; Convergent Error Returns
  (define-constant +EBADE+	50	"invalid exchange")
  (define-constant +EBADR+	51	"invalid request descriptor")
  (define-constant +EXFULL+	52	"exchange full")
  (define-constant +ENOANO+	53	"no anode")
  (define-constant +EBADRQC+	54	"invalid request code")
  (define-constant +EBADSLT+	55	"invalid slot")
  (define-constant +EDEADLOCK+	56	"file locking deadlock error")

  (define-constant +EBFONT+	57	"bad font file fmt")

  ;; Interprocess Robust Locks
  (define-constant +EOWNERDEAD+	58	"process died with the lock")
  (define-constant +ENOTRECOVERABLE+	59	"lock is not recoverable")

  ;; stream problems
  (define-constant +ENOSTR+	60	"Device not a stream")
  (define-constant +ENODATA+	61	"no data (for no delay io)")
  (define-constant +ETIME+	62	"timer expired")
  (define-constant +ENOSR+	63	"out of streams resources")

  (define-constant +ENONET+	64	"Machine is not on the network")
  (define-constant +ENOPKG+	65	"Package not installed")
  (define-constant +EREMOTE+	66	"The object is remote")
  (define-constant +ENOLINK+	67	"the link has been severed")
  (define-constant +EADV+	68	"advertise error")
  (define-constant +ESRMNT+	69	"srmount error")

  (define-constant +ECOMM+	70	"Communication error on send")
  (define-constant +EPROTO+	71	"Protocol error")

  ;; Interprocess Robust Locks
  (define-constant +ELOCKUNMAPPED+ 72 "locked lock was unmapped")

  (define-constant +ENOTACTIVE+   73 "Facility is not active")
  (define-constant +EMULTIHOP+    74 "multihop attempted")
  (define-constant +EBADMSG+      77 "trying to read unreadable message")
  (define-constant +ENAMETOOLONG+ 78 "path name is too long")
  (define-constant +EOVERFLOW+    79 "value too large to be stored in data type")
  (define-constant +ENOTUNIQ+     80 "given log. name not unique")
  (define-constant +EBADFD+	  81 "f.d. invalid for this operation")
  (define-constant +EREMCHG+	  82 "Remote address changed")

  ;; shared library problems
  (define-constant +ELIBACC+	  83 "Can't access a needed shared lib.")
  (define-constant +ELIBBAD+	  84 "Accessing a corrupted shared lib.")
  (define-constant +ELIBSCN+	  85 ".lib section in a.out corrupted.")
  (define-constant +ELIBMAX+	  86 "Attempting to link in too many libs.")
  (define-constant +ELIBEXEC+     87 "Attempting to exec a shared library.")
  (define-constant +EILSEQ+	  88 "Illegal byte sequence.")
  (define-constant +ENOSYS+	  89 "Unsupported file system operation")
  (define-constant +ELOOP+	  90 "Symbolic link loop")
  (define-constant +ERESTART+     91 "Restartable system call")
  (define-constant +ESTRPIPE+     92 "if pipe/FIFO, don't sleep in stream head")
  (define-constant +ENOTEMPTY+    93 "directory not empty")
  (define-constant +EUSERS+	  94 "Too many users (for UFS)")

  ;; BSD Networking Software
  ;;    argument errors
  (define-constant +ENOTSOCK+	     95	 "Socket operation on non-socket")
  (define-constant +EDESTADDRREQ+    96	 "Destination address required")
  (define-constant +EMSGSIZE+	     97	 "Message too long")
  (define-constant +EPROTOTYPE+	     98	 "Protocol wrong type for socket")
  (define-constant +ENOPROTOOPT+     99	 "Protocol not available")
  (define-constant +EPROTONOSUPPORT+ 120 "Protocol not supported")
  (define-constant +ESOCKTNOSUPPORT+ 121 "Socket type not supported")
  (define-constant +EOPNOTSUPP+	     122 "Operation not supported on socket")
  (define-constant +EPFNOSUPPORT+    123 "Protocol family not supported")
  (define-constant +EAFNOSUPPORT+    124 "Address family not supported by protocol family")
  (define-constant +EADDRINUSE+	     125 "Address already in use")
  (define-constant +EADDRNOTAVAIL+   126 "Can't assign requested address")
  ;; operational errors
  (define-constant +ENETDOWN+	     127 "Network is down")
  (define-constant +ENETUNREACH+     128 "Network is unreachable")
  (define-constant +ENETRESET+	     129 "Network dropped connection because of reset")
  (define-constant +ECONNABORTED+    130 "Software caused connection abort")
  (define-constant +ECONNRESET+	     131 "Connection reset by peer")
  (define-constant +ENOBUFS+	     132 "No buffer space available")
  (define-constant +EISCONN+	     133 "Socket is already connected")
  (define-constant +ENOTCONN+	     134 "Socket is not connected")
  ;; XENIX has 135 - 142
  (define-constant +ESHUTDOWN+	     143 "Can't send after socket shutdown")
  (define-constant +ETOOMANYREFS+    144 "Too many references: can't splice")
  (define-constant +ETIMEDOUT+	     145 "Connection timed out")
  (define-constant +ECONNREFUSED+    146 "Connection refused")
  (define-constant +EWOULDBLOCK+     +EAGAIN+)
  (define-constant +EHOSTDOWN+	     147 "Host is down")
  (define-constant +EHOSTUNREACH+    148 "No route to host")
  (define-constant +EALREADY+	     149 "operation already in progress")
  (define-constant +EINPROGRESS+     150 "operation now in progress")

  ;; SUN Network File System 
  (define-constant +ESTALE+	     151 "Stale NFS file handle")
)

#+linux
(progn
  (define-constant +EPERM+	1 "Not super-user")
  (define-constant +ENOENT+	2 "No such file or directory")
  (define-constant +ESRCH+	3 "No such process")
  (define-constant +EINTR+	4 "interrupted system call")
  (define-constant +EIO+	5 "I/O error")
  (define-constant +ENXIO+	6 "No such device or address")
  (define-constant +E2BIG+	7 "Arg list too long")
  (define-constant +ENOEXEC+	8 "Exec format error")
  (define-constant +EBADF+	9 "Bad file number")
  (define-constant +ECHILD+	10 "No children")
  (define-constant +EAGAIN+	11 "Resource temporarily unavailable")
  (define-constant +ENOMEM+	12 "Not enough core")
  (define-constant +EACCES+	13 "Permission denied")
  (define-constant +EFAULT+	14 "Bad address")
  (define-constant +ENOTBLK+	15 "Block device required")
  (define-constant +EBUSY+	16 "Mount device busy")
  (define-constant +EEXIST+	17 "File exists")
  (define-constant +EXDEV+	18 "Cross-device link")
  (define-constant +ENODEV+	19 "No such device")
  (define-constant +ENOTDIR+	20 "Not a directory")
  (define-constant +EISDIR+	21 "Is a directory")
  (define-constant +EINVAL+	22 "Invalid argument")
  (define-constant +ENFILE+	23 "File table overflow")
  (define-constant +EMFILE+	24 "Too many open files")
  (define-constant +ENOTTY+	25 "Inappropriate ioctl for device")
  (define-constant +ETXTBSY+	26 "Text file busy")
  (define-constant +EFBIG+	27 "File too large")
  (define-constant +ENOSPC+	28 "No space left on device")
  (define-constant +ESPIPE+	29 "Illegal seek")
  (define-constant +EROFS+	30 "Read only file system")
  (define-constant +EMLINK+	31 "Too many links")
  (define-constant +EPIPE+	32 "Broken pipe")
  (define-constant +EDOM+	33 "Math arg out of domain of func")
  (define-constant +ERANGE+	34 "Math result not representable")
  (define-constant +ENOMSG+	35 "No message of desired type")
  (define-constant +EIDRM+	36 "Identifier removed")
  (define-constant +ECHRNG+	37 "Channel number out of range")
  (define-constant +EL2NSYNC+	38 "Level 2 not synchronized")
  (define-constant +EL3HLT+	39 "Level 3 halted")
  (define-constant +EL3RST+	40 "Level 3 reset")
  (define-constant +ELNRNG+	41 "Link number out of range")
  (define-constant +EUNATCH+	42 "Protocol driver not attached")
  (define-constant +ENOCSI+	43 "No CSI structure available")
  (define-constant +EL2HLT+	44 "Level 2 halted")
  (define-constant +EDEADLK+	45 "Deadlock condition.")
  (define-constant +ENOLCK+	46 "No record locks available.")
  (define-constant +ECANCELED+	47 "Operation canceled")
  (define-constant +ENOTSUP+	48 "Operation not supported")

  ;; Filesystem Quotas
  (define-constant +EDQUOT+	49 "Disc quota exceeded")

  ;; Convergent Error Returns
  (define-constant +EBADE+	50 "invalid exchange")
  (define-constant +EBADR+	51 "invalid request descriptor")
  (define-constant +EXFULL+	52 "exchange full")
  (define-constant +ENOANO+	53 "no anode")
  (define-constant +EBADRQC+	54 "invalid request code")
  (define-constant +EBADSLT+	55 "invalid slot")
  (define-constant +EDEADLOCK+	56 "file locking deadlock error")

  (define-constant +EBFONT+	57 "bad font file fmt")

  ;; Interprocess Robust Locks
  (define-constant +EOWNERDEAD+	58 "process died with the lock")
  (define-constant +ENOTRECOVERABLE+ 59 "lock is not recoverable")

  ;; stream problems
  (define-constant +ENOSTR+	60 "Device not a stream")
  (define-constant +ENODATA+	61 "no data (for no delay io)")
  (define-constant +ETIME+	62 "timer expired")
  (define-constant +ENOSR+	63 "out of streams resources")

  (define-constant +ENONET+	64 "Machine is not on the network")
  (define-constant +ENOPKG+	65 "Package not installed")
  (define-constant +EREMOTE+	66 "The object is remote")
  (define-constant +ENOLINK+	67 "the link has been severed")
  (define-constant +EADV+	68 "advertise error")
  (define-constant +ESRMNT+	69 "srmount error")

  (define-constant +ECOMM+	70 "Communication error on send")
  (define-constant +EPROTO+	71 "Protocol error")

  ;; Interprocess Robust Locks
  (define-constant +ELOCKUNMAPPED+ 72 "locked lock was unmapped")

  (define-constant +ENOTACTIVE+	73 "Facility is not active")
  (define-constant +EMULTIHOP+	74 "multihop attempted")
  (define-constant +EBADMSG+	77 "trying to read unreadable message")
  (define-constant +ENAMETOOLONG+ 78 "path name is too long")
  (define-constant +EOVERFLOW+	79 "value too large to be stored in data type")
  (define-constant +ENOTUNIQ+	80 "given log. name not unique")
  (define-constant +EBADFD+	81 "f.d. invalid for this operation")
  (define-constant +EREMCHG+	82 "Remote address changed")

;; shared library problems
  (define-constant +ELIBACC+	83 "Can't access a needed shared lib.")
  (define-constant +ELIBBAD+	84 "Accessing a corrupted shared lib.")
  (define-constant +ELIBSCN+	85 ".lib section in a.out corrupted.")
  (define-constant +ELIBMAX+	86 "Attempting to link in too many libs.")
  (define-constant +ELIBEXEC+	87 "Attempting to exec a shared library.")
  (define-constant +EILSEQ+	88 "Illegal byte sequence.")
  (define-constant +ENOSYS+	89 "Unsupported file system operation")
  (define-constant +ELOOP+	90 "Symbolic link loop")
  (define-constant +ERESTART+	91 "Restartable system call")
  (define-constant +ESTRPIPE+	92 "if pipe/FIFO, don't sleep in stream head")
  (define-constant +ENOTEMPTY+	93 "directory not empty")
  (define-constant +EUSERS+	94 "Too many users (for UFS)")

  ;; BSD Networking Software
  ;; argument errors
  (define-constant +ENOTSOCK+		95 "Socket operation on non-socket")
  (define-constant +EDESTADDRREQ+	96 "Destination address required")
  (define-constant +EMSGSIZE+		97 "Message too long")
  (define-constant +EPROTOTYPE+		98 "Protocol wrong type for socket")
  (define-constant +ENOPROTOOPT+	99 "Protocol not available")
  (define-constant +EPROTONOSUPPORT+	120 "Protocol not supported")
  (define-constant +ESOCKTNOSUPPORT+	121 "Socket type not supported")
  (define-constant +EOPNOTSUPP+		122 "Operation not supported on socket")
  (define-constant +EPFNOSUPPORT+	123 "Protocol family not supported")
  (define-constant +EAFNOSUPPORT+	124 "Address family not supported by protocol family")
  (define-constant +EADDRINUSE+		125 "Address already in use")
  (define-constant +EADDRNOTAVAIL+	126 "Can't assign requested address")
  ;; operational errors
  (define-constant +ENETDOWN+		127 "Network is down")
  (define-constant +ENETUNREACH+	128 "Network is unreachable")
  (define-constant +ENETRESET+		129 "Network dropped connection because of reset")
  (define-constant +ECONNABORTED+	130 "Software caused connection abort")
  (define-constant +ECONNRESET+		131 "Connection reset by peer")
  (define-constant +ENOBUFS+		132 "No buffer space available")
  (define-constant +EISCONN+		133 "Socket is already connected")
  (define-constant +ENOTCONN+		134 "Socket is not connected")
  ;; XENIX has 135 - 142
  (define-constant +ESHUTDOWN+		143 "Can't send after socket shutdown")
  (define-constant +ETOOMANYREFS+	144 "Too many references: can't splice")
  (define-constant +ETIMEDOUT+		145 "Connection timed out")
  (define-constant +ECONNREFUSED+	146 "Connection refused")
  (define-constant +EHOSTDOWN+		147 "Host is down")
  (define-constant +EHOSTUNREACH+	148 "No route to host")
  (define-constant +EWOULDBLOCK+	+EAGAIN+)
  (define-constant +EALREADY+		149 "operation already in progress")
  (define-constant +EINPROGRESS+	150 "operation now in progress")

  ;; Network File System
  (define-constant +ESTALE+		151 "Stale NFS file handle")
)

#+freebsd
(progn
  (define-constant +EPERM+	        1  "Operation not permitted")
  (define-constant +ENOENT+	        2  "No such file or directory")
  (define-constant +ESRCH+	        3  "No such process")
  (define-constant +EINTR+	        4  "Interrupted system call")
  (define-constant +EIO+	        5  "Input/output error")
  (define-constant +ENXIO+	        6  "Device not configured")
  (define-constant +E2BIG+	        7  "Argument list too long")
  (define-constant +ENOEXEC+	        8  "Exec format error")
  (define-constant +EBADF+	        9  "Bad file descriptor")
  (define-constant +ECHILD+	        10 "No child processes")
  (define-constant +EDEADLK+	        11 "Resource deadlock avoided")
  (define-constant +ENOMEM+	        12 "Cannot allocate memory")
  (define-constant +EACCES+	        13 "Permission denied")
  (define-constant +EFAULT+	        14 "Bad address")
  (define-constant +ENOTBLK+	        15 "Block device required")
  (define-constant +EBUSY+	        16 "Device busy")
  (define-constant +EEXIST+	        17 "File exists")
  (define-constant +EXDEV+	        18 "Cross-device link")
  (define-constant +ENODEV+	        19 "Operation not supported by device")
  (define-constant +ENOTDIR+	        20 "Not a directory")
  (define-constant +EISDIR+	        21 "Is a directory")
  (define-constant +EINVAL+	        22 "Invalid argument")
  (define-constant +ENFILE+	        23 "Too many open files in system")
  (define-constant +EMFILE+	        24 "Too many open files")
  (define-constant +ENOTTY+	        25 "Inappropriate ioctl for device")
  (define-constant +ETXTBSY+	        26 "Text file busy")
  (define-constant +EFBIG+	        27 "File too large")
  (define-constant +ENOSPC+	        28 "No space left on device")
  (define-constant +ESPIPE+	        29 "Illegal seek")
  (define-constant +EROFS+	        30 "Read-only filesystem")
  (define-constant +EMLINK+	        31 "Too many links")
  (define-constant +EPIPE+	        32 "Broken pipe")
  (define-constant +EDOM+	        33 "Numerical argument out of domain")
  (define-constant +ERANGE+	        34 "Result too large")
  (define-constant +EAGAIN+	        35 "Resource temporarily unavailable")
  (define-constant +EWOULDBLOCK+        +EAGAIN+ "Operation would block")
  (define-constant +EINPROGRESS+        36 "Operation now in progress")
  (define-constant +EALREADY+	        37 "Operation already in progress")
  (define-constant +ENOTSOCK+	        38 "Socket operation on non-socket")
  (define-constant +EDESTADDRREQ+       39 "Destination address required")
  (define-constant +EMSGSIZE+		40 "Message too long")
  (define-constant +EPROTOTYPE+		41 "Protocol wrong type for socket")
  (define-constant +ENOPROTOOPT+	42 "Protocol not available")
  (define-constant +EPROTONOSUPPORT+	43 "Protocol not supported")
  (define-constant +ESOCKTNOSUPPORT+	44 "Socket type not supported")
  (define-constant +ENOTSUP+		45 "Operation not supported")
  (define-constant +EOPNOTSUPP+		+ENOTSUP+ "Operation not supported")
  (define-constant +EPFNOSUPPORT+	46 "Protocol family not supported")
  (define-constant +EAFNOSUPPORT+	47 "Address family not supported by protocol family")
  (define-constant +EADDRINUSE+		48 "Address already in use")
  (define-constant +EADDRNOTAVAIL+	49 "Can't assign requested address")
  (define-constant +ENETDOWN+		50 "Network is down")
  (define-constant +ENETUNREACH+	51 "Network is unreachable")
  (define-constant +ENETRESET+		52 "Network dropped connection on reset")
  (define-constant +ECONNABORTED+	53 "Software caused connection abort")
  (define-constant +ECONNRESET+		54 "Connection reset by peer")
  (define-constant +ENOBUFS+		55 "No buffer space available")
  (define-constant +EISCONN+		56 "Socket is already connected")
  (define-constant +ENOTCONN+		57 "Socket is not connected")
  (define-constant +ESHUTDOWN+		58 "Can't send after socket shutdown")
  (define-constant +ETOOMANYREFS+	59 "Too many references: can't splice")
  (define-constant +ETIMEDOUT+		60 "Operation timed out")
  (define-constant +ECONNREFUSED+	61 "Connection refused")
  (define-constant +ELOOP+		62 "Too many levels of symbolic links")
  (define-constant +ENAMETOOLONG+	63 "File name too long")
  (define-constant +EHOSTDOWN+		64 "Host is down")
  (define-constant +EHOSTUNREACH+	65 "No route to host")
  (define-constant +ENOTEMPTY+		66 "Directory not empty")
  (define-constant +EPROCLIM+		67 "Too many processes")
  (define-constant +EUSERS+		68 "Too many users")
  (define-constant +EDQUOT+		69 "Disc quota exceeded")
  (define-constant +ESTALE+		70 "Stale NFS file handle")
  (define-constant +EREMOTE+		71 "Too many levels of remote in path")
  (define-constant +EBADRPC+		72 "RPC struct is bad")
  (define-constant +ERPCMISMATCH+	73 "RPC version wrong")
  (define-constant +EPROGUNAVAIL+	74 "RPC prog. not avail")
  (define-constant +EPROGMISMATCH+	75 "Program version wrong")
  (define-constant +EPROCUNAVAIL+	76 "Bad procedure for program")
  (define-constant +ENOLCK+		77 "No locks available")
  (define-constant +ENOSYS+		78 "Function not implemented")
  (define-constant +EFTYPE+		79 "Inappropriate file type or format")
  (define-constant +EAUTH+		80 "Authentication error")
  (define-constant +ENEEDAUTH+		81 "Need authenticator")
  (define-constant +EIDRM+		82 "Identifier removed")
  (define-constant +ENOMSG+		83 "No message of desired type")
  (define-constant +EOVERFLOW+		84 "Value too large to be stored in data type")
  (define-constant +ECANCELED+		85 "Operation canceled")
  (define-constant +EILSEQ+		86 "Illegal byte sequence")
  (define-constant +ENOATTR+		87 "Attribute not found")
  (define-constant +EDOOFUS+		88 "Programming error")
  (define-constant +EBADMSG+		89 "Bad message")
  (define-constant +EMULTIHOP+		90 "Multihop attempted")
  (define-constant +ENOLINK+		91 "Link has been severed")
  (define-constant +EPROTO+		92 "Protocol error")
  (define-constant +ENOTCAPABLE+	93 "Capabilities insufficient")
  (define-constant +ECAPMODE+		94 "Not permitted in capability mode")
  (define-constant +ENOTRECOVERABLE+	95 "State not recoverable")
  (define-constant +EOWNERDEAD+		96 "Previous owner died")
  (define-constant +ELAST+		96 "Must be equal largest errno")
  )

#+os-t-has-strerror-r
(defcfun (#+linux "__xpg_strerror_r" #-linux "strerror_r" strerror-r)
    :int (errnum :int) (strerrbuf :pointer) (buflen size-t))
#-os-t-has-strerror-r
(defcvar ("sys_errlist" sys-errlist) :pointer)
#-os-t-has-strerror-r
(defcvar ("sys_nerr" sys-nerr) :int)

(defun strerror (&optional (e *errno*))
  #+os-t-has-strerror-r
  (with-foreign-pointer-as-string (s 100)
    (strerror-r e s 100))
  #-os-t-has-strerror-r
  (if (< e sys-nerr)
      (foreign-string-to-lisp (mem-aref sys-errlist :pointer e))
      (format nil "Unknown error: ~d" e))
)

(define-condition posix-error (opsys-error)
  ()
  (:documentation "An error from calling a POSIX function."))

(defun error-message (error-code)
  ;; "Return a string describing the ERROR-CODE."
  (strerror error-code))

(defun error-check (c &optional fmt &rest args)
  "Check if a system call returns an error value and signal it."
  (if (< c 0)
      (error 'posix-error :error-code *errno*
	     :format-control fmt :format-arguments args)
      c))

(defmacro syscall ((func &rest args))
  "Call a system function and signal a posix-error if it fails."
  `(error-check (,func ,@args)
		,(concatenate 'string (string-downcase func) ":")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental information

;; We could provide a cached value to make this faster, and update it
;; when the setenv below is used, but it would become inaccurate if
;; other code modifies the environment.

;; ??? Does it even make sense to have these as keywords??

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
;; BTW, all this sysctl stuff is probably #+darwin, since it hasn't been
;; tested on any other platforms. I suppose on linux we'll have to implement it
;; by reading from /proc/sys. Specificly, in linux, man sysctl says:
;;
;;     don't call it: use of this system call has long been discouraged,
;;     and it is so unloved that it is likely to disappear in a future kernel
;;     version.  Since Linux 2.6.24, uses of this system call result in
;;     warnings in the kernel log.  Remove it from your programs now; use
;;     the /proc/sys interface instead.
;;
;; If performance need to be improved, we could consider caching the
;; integer values by using sysctlnametomib.
;;
;; NOTE: This should probably come fairly early since we may use it later on
;; to determine configuration, such as kernel version, etc.

(defcfun ("sysctl" real-sysctl)
    :int (name :pointer) (namelen :unsigned-int)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newlen size-t))

#-linux
(defcfun ("sysctlbyname" real-sysctlbyname) :int (name :string)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newlen size-t))

#-linux
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

#-linux
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

#-linux
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
    ;; We can't use the SYSCALL macro becuase sometime sysconf returns -1.
    (setf *errno* 0
	  result (real-sysconf number))
    (when (and (< result 0) (= *errno* +EINVAL+))
      (error 'posix-error
	     :error-code *errno*
	     :format-control "sysconf: "))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User database
;; 

;; Note that this might be different than UID.
(defcfun ("getlogin" real-getlogin) :string)
(defun getlogin ()
  (real-getlogin))

(defcstruct foreign-passwd
  "User database entry."
  (pw_name	:string)
  (pw_passwd	:string)
  (pw_uid	uid-t)
  (pw_gid	gid-t)
  #+darwin (pw_change time-t)
  #+darwin (pw_class  :string)
  #+sunos (pw_age :string)
  #+sunos (pw_comment :string)
  (pw_gecos	:string)
  (pw_dir	:string)
  (pw_shell	:string)
  #+darwin (pw_expire time-t)
  )

(defstruct passwd
  "User database entry."
  name
  passwd
  uid
  gid
  pw-change
  pw-class
  pw-age
  pw-comment
  gecos
  dir
  shell
  pw-expire)

(defun convert-passwd (pw)
  "Return a lisp passwd structure from the foreign passwd structure. ~
Return nil for foreign null pointer."
  (if (and (pointerp pw) (null-pointer-p pw))
      nil
      (with-foreign-slots ((pw_name
			    pw_passwd
			    pw_uid
			    pw_gid
			    #+darwin pw_change
			    #+darwin pw_class
			    #+sunos pw_age
			    #+sunos pw_comment
			    pw_gecos
			    pw_dir
			    pw_shell
			    #+darwin pw_expire
			    ) pw (:struct foreign-passwd))
	(make-passwd
	 :name pw_name
	 :passwd pw_passwd
	 :uid pw_uid
	 :gid pw_gid
	 #+darwin :pw-change #+darwin pw_change
	 #+darwin :pw-class #+darwin pw_class
	 #+sunos :pw-age #+sunos pw_age
	 #+sunos :pw-comment #+sunos pw_comment
	 :gecos pw_gecos
	 :dir pw_dir
	 :shell pw_shell
	 #+darwin :pw-expire #+darwin pw_expire
	 ))))

(defun convert-user (pw)
  "Return a generic user structure from the foreign passwd structure. ~
Return nil for foreign null pointer."
  (if (and (pointerp pw) (null-pointer-p pw))
      nil
      (with-foreign-slots ((pw_name
			    pw_passwd
			    pw_uid
			    pw_gid
			    #+darwin pw_change
			    #+darwin pw_class
			    #+sunos pw_age
			    #+sunos pw_comment
			    pw_gecos
			    pw_dir
			    pw_shell
			    #+darwin pw_expire
			    ) pw (:struct foreign-passwd))
	(make-user-info
	 :name pw_name
	 :id pw_uid
	 :full-name pw_gecos
	 :home-directory pw_dir
	 :shell pw_shell
	 :primary-group-id pw_gid
	 ;; :guid
	 ;; :picture
	 ))))

(defcfun ("getpwuid" real-getpwuid) :pointer (uid uid-t))
(defun getpwuid (uid)
  (convert-passwd (real-getpwuid uid)))

;; @@@ Should use the re-entrant versions of these functions.

;; int
;; getpwuid_r(uid_t uid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result)

(defcfun ("getpwnam" real-getpwnam) :pointer (name :string))
(defun getpwnam (name)
  (convert-passwd (real-getpwnam name)))

;; int
;; getpwnam_r(const char *name, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result);

;; int
;; getpwuuid_r(uuid_t uuid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result);

(defcfun ("getpwent" real-getpwent) :pointer)
(defun getpwent ()
  (convert-passwd (real-getpwent)))

(defcfun endpwent :void)
(defcfun setpwent :void)

(defun get-user-info (&key name id)
  "Return a user structure from the user database. You can look up by either
NAME or ID. If you specifiy both, it just uses the ID. If you specify neither,
it signals an error."
  (when (not (or name id))
    (error "You have to specify at least one of NAME or ID."))
  (if id
      (convert-user (real-getpwuid id))
      (convert-user (real-getpwnam name))))

(defun user-name (&optional id)
  "Return the name of the user with ID, which defaults to the current user."
  (let ((u (getpwuid (or id (getuid)))))
    (and u (passwd-name u))))

(defun user-home (&optional (user (user-name)))
  "Return the namestring of the given USER's home directory or nil if the ~
user is not found."
  (unwind-protect
    (progn
      (setpwent)
      (loop :with p = nil
	 :while (setf p (getpwent))
	 :do (when (string= (passwd-name p) user)
	       (return-from user-home (passwd-dir p)))))
    (endpwent)))

(defun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user."
  (if name
      (passwd-uid (getpwnam name))
      (if effective
	  (geteuid)
	  (getuid))))

(defun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (let* ((name (passwd-gecos (getpwuid (or id (getuid)))))
	 (comma (position #\, name)))
    (if comma
	(subseq name 0 comma)
	name)))

;; This is probably wrong & system specific
(defun user-name-char-p (c)
  "Return true if C is a valid character in a user name."
  (or (alphanumericp c) (eql #\_ c) (eql #\- c)))

(defun valid-user-name (username)
  (not (position-if #'(lambda (c) (not (user-name-char-p c))) username)))

(defun get-next-user ()
  "Return the next user structure from the user database."
  (convert-user (real-getpwent)))

(defun user-list ()
  "How to annoy people in large organizations."
  (unwind-protect
    (progn
      (setpwent)
      (loop :with g :while (setf g (get-next-user)) :collect g))
    (endpwent)))

(defun refresh-user-list ()
  "Just in case you are bored, this will make get-next-group or group-list
return potentially updated data."
  (endpwent)
  (setpwent))

(defun is-administrator ()
  "Return true if you are root, or effectively root."
  (= (geteuid) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group database

(defcstruct foreign-group
  "Group database entry."
  (gr_name	:string)
  (gr_passwd	:string)
  (gr_gid	gid-t)
  (gr_mem	:pointer))

(defstruct group-entry
  "Group database entry."
  name
  passwd
  gid
  members)

(defun convert-group (gr)
  "Return a lisp group structure from the foreign group structure. ~
Return nil for foreign null pointer."
  (if (and (pointerp gr) (null-pointer-p gr))
      nil
      (with-foreign-slots ((gr_name
			    gr_passwd
			    gr_gid
			    gr_mem
			    ) gr (:struct foreign-group))
	(make-group-entry
	 :name   gr_name
	 :passwd gr_passwd
	 :gid    gr_gid
	 :members
	 (loop :with i = 0
	    :while (not (null-pointer-p (mem-aref gr_mem :pointer i)))
	    :collect (mem-aref gr_mem :string i)
	    :do (incf i))))))

;; @@@ Should use the re-entrant versions of these functions.

(defcfun ("getgrgid" real-getgrgid) :pointer (uid gid-t))
(defun getgrgid (gid)
  (convert-group (real-getgrgid gid)))

(defcfun ("getgrnam" real-getgrnam) :pointer (name :string))
(defun getgrnam (name)
  (convert-group (real-getgrnam name)))

(defcfun ("getgrent" real-getgrent) :pointer)
(defun getgrent ()
  (convert-group (real-getgrent)))

(defcfun ("endgrent" real-endgrent) :void)
(defun endgrent ()
  (real-endgrent))

(defcfun ("setgrent" real-setgrent) :void)
(defun setgrent ()
  (real-setgrent))

(defun group-name (&optional id)
  "Return the name of the group with ID. Defaults to the current group."
  (let ((gr (getgrgid (or id (getgid)))))
    (and gr (group-entry-name gr))))

(defun group-id (&optional name)
  "Return the ID of the group NAME. Defaults to the current group."
  (if name
      (group-entry-gid (getgrnam name))
      (getgid)))

(defun get-next-group ()
  "Return the next group structure from the group database."
  (getgrent))

(defun group-list ()
  "How to annoy people in large organizations."
  (setgrent)
  (loop :with g :while (setf g (get-next-group)) :collect g))

(defun refresh-group-list ()
  "Just in case you are bored, this will make get-next-group or group-list
return potentially updated data."
  (endgrent)
  (setgrent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

;; @@@ Solaris & FreeBSD aren't really done yet

(define-constants #(
;; Name               D   L   S   F
#(+UTX-EMPTY+	      0   0   0   0   "No valid user accounting information.")
#(+UTX-RUN-LVL+	      1   1   1   1   "Run level. For Compatibility, not used.")
#(+UTX-BOOT-TIME+     2   2   2   2   "Time of a system boot.")
#(+UTX-OLD-TIME+      3   4   3   3   "Time before system clock change.")
#(+UTX-NEW-TIME+      4   3   4   4   "Time after system clock change.")
#(+UTX-INIT-PROCESS+  5   5   5   5   "A process spawned by init(8).")
#(+UTX-LOGIN-PROCESS+ 6   6   6   6   "The session leader of a logged-in user.")
#(+UTX-USER-PROCESS+  7   7   7   7   "A user process.")
#(+UTX-DEAD-PROCESS+  8   8   8   8   "A session leader exited.")
#(+UTX-ACCOUNTING+    9   9   9   9   "")
#(+UTX-SIGNATURE+     10  nil 10  10  "")
#(+UTX-SHUTDOWN-TIME+ 11  nil 11  11  "Time of system shutdown (extension)")))

#+darwin
(progn
  (defconstant +UTMPX-AUTOFILL-MASK+              #x8000
    "Fill in missing data.")
  (defconstant +UTMPX-DEAD-IF-CORRESPONDING-MASK+ #x4000
    "Only if existing live one."))

(defparameter +utmpx-type+
  #+darwin
  #(:EMPTY :RUN-LVL :BOOT-TIME :OLD-TIME :NEW-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  #+linux
  #(:EMPTY :RUN-LVL :BOOT-TIME :NEW-TIME :OLD-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  #-(or darwin linux) ;; @@@ not really right
  #(:EMPTY :RUN-LVL :BOOT-TIME :NEW-TIME :OLD-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  "utmpx type keywords.")

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-constants #(
;; Name          D    L   S   F
#(+UTX-USERSIZE+ 256  32  32  32  "Size of utmpx.ut_user.")
#(+UTX-IDSIZE+	 4    4   4   4   "Size of utmpx.ut_id.")
#(+UTX-LINESIZE+ 32   32  32  32  "Size of utmpx.ut_line.")
#(+UTX-HOSTSIZE+ 256  256 256 256 "Size of utmpx.ut_host."))))

#+darwin
(defcstruct foreign-utmpx
  "User accounting database entry."
  (ut_user :char :count #.+UTX-USERSIZE+)  ;; login name
  (ut_id   :char :count #.+UTX-IDSIZE+)	   ;; id
  (ut_line :char :count #.+UTX-LINESIZE+)  ;; tty name
  (ut_pid  pid-t)			   ;; process id creating the entry
  (ut_type :short)			   ;; type of this entry
  (ut_tv   (:struct foreign-timeval))	   ;; time entry was created
  (ut_host :char :count #.+UTX-HOSTSIZE+)  ;; host name
  (ut_pad  :uint32 :count 16)		   ;; reserved for future use
  )

#+linux
(defcstruct foreign-exit-status
  "utmpx exit status"
  (e-termination :short)
  (e-exit :short))

#+linux 
(defcstruct foreign-utmp-timeval
  (tv_sec  :int32)
  (tv_usec :int32))

#+linux
(defcstruct foreign-utmpx
  "User accounting database entry."
  (ut_type :short)			   ;; type of this entry
  (ut_pid  pid-t)			   ;; process id creating the entry
  (ut_line :char :count #.+UTX-LINESIZE+)  ;; tty name
  (ut_id   :char :count #.+UTX-IDSIZE+)	   ;; id
  (ut_user :char :count #.+UTX-USERSIZE+)  ;; login name
  (ut_host :char :count #.+UTX-HOSTSIZE+)  ;; host name
  (ut_exit (:struct foreign-exit-status))
  (ut_session :int32)
  (ut_tv   (:struct foreign-utmp-timeval)) ;; time entry was created
  (ut_addr_v6 :int32 :count 4)		   ;; ipv6 address?
  (ut_pad  :char :count 20)		   ;; reserved for future use
  )

(defstruct utmpx
  "User accounting databse entry."
  user
  id
  line
  pid
  type
  tv
  host)

(defun convert-utmpx (u)
  (if (and (pointerp u) (null-pointer-p u))
      nil
      (with-foreign-slots ((ut_user ut_id ut_line ut_pid ut_type ut_tv ut_host)
			   u (:struct foreign-utmpx))
	(make-utmpx
	  :user	(foreign-string-to-lisp ut_user)
	  :id	(foreign-string-to-lisp ut_id :max-chars +UTX-IDSIZE+)
	  :line	(foreign-string-to-lisp ut_line)
	  :pid	ut_pid
	  :type	(aref +utmpx-type+ ut_type)
	  :tv	(make-timeval :seconds (getf ut_tv 'tv_sec)
			      :micro-seconds (getf ut_tv 'tv_usec))
	  :host	(foreign-string-to-lisp ut_host)))))

(defcfun endutxent :void
  "Close the utmpx database.")

(defcfun ("getutxent" real-getutxent) (:pointer (:struct foreign-utmpx))
  "Read the next entry from the utmpx database. Open it if it's not open.")

(defun getutxent ()
  "Read the next entry from the utmpx database. Open it if it's not open."
  (convert-utmpx (real-getutxent)))

(defcfun ("getutxid" real-getutxid) (:pointer (:struct foreign-utmpx))
  "Read the next entry of type specified by the ut_type field, from the utmpx
database. Open it if it's not open."
  (id (:pointer (:struct foreign-utmpx))))

(defun getutxid (id)
  "Read the next entry of type specified by the ut_type field, from the utmpx
database. Open it if it's not open."
  (with-foreign-object (u '(:struct foreign-utmpx))
    (setf (foreign-slot-value u '(:struct foreign-utmpx) 'ut_type) id)
    (convert-utmpx (real-getutxid u))))

(defcfun ("getutxline" real-getutxline) (:pointer (:struct foreign-utmpx))
  "Read the next entry of type LOGIN_PROCESS or USER_PROCESS where the ut_line
field matches LINE, from the utmpx database. Open it if it's not open."
  (line (:pointer (:struct foreign-utmpx))))

(defun getutxline (line)
  "Read the next entry of type LOGIN_PROCESS or USER_PROCESS where the ut_line
field matches LINE, from the utmpx database. Open it if it's not open."
  (with-foreign-object (u '(:struct foreign-utmpx))
    (setf (foreign-slot-value u '(:struct foreign-utmpx) 'ut_line) line)
    (convert-utmpx (real-getutxline u))))

(defcfun ("pututxline" real-pututxline) (:pointer (:struct foreign-utmpx))
  "Put the entry UTX into the utmpx database, replacing the entry for the same
user. Probably requires root."
  (utx (:pointer (:struct foreign-utmpx))))

(defun pututxline (line)
  "Put the entry UTX into the utmpx database, replacing the entry for the same
user. Probably requires root."
  (check-type line utmpx)
  (with-foreign-object (u '(:struct foreign-utmpx))
    (with-foreign-slots ((ut_user ut_id ut_line ut_pid ut_type ut_tv ut_host
			  ut_pad)
			 u (:struct foreign-utmpx))
      (with-slots (user id line pid type tv host) line
	(setf ut_user user
	      ut_id   id
	      ut_line line
	      ut_pid  pid
	      ut_type type
	      (foreign-slot-value ut_tv '(:struct foreign-timeval) 'tv_sec)
	      (timeval-seconds tv)
	      (foreign-slot-value ut_tv '(:struct foreign-timeval) 'tv_usec)
	      (timeval-micro-seconds tv)
	      ut_host host)
	(convert-utmpx (real-getutxline u))))))

(defcfun setutxent :void
  "Reset the utmpx database to the beginning.")

;; This is usually done only when you "log in", like with the window system or
;; like in the ssh deamon. See getlogin.
#+darwin (defcfun setlogin :int (name :string))

(defun users-logged-in ()
  "Return a list of names of logged in users."
  (unwind-protect
    (progn
      (setutxent)
      (let (u)
	(loop :while (setf u (getutxent))
	   ;; :if (not (eq (utmpx-type u) :dead-process))
	   :if (eq (utmpx-type u) :user-process)
	   :collect
	   (utmpx-user u))))
    (endutxent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

(defun hidden-file-name-p (name)
  "Return true if the file NAME is normally hidden."
  (and name (> (length name) 0) (equal (char name 0) #\.)))

(defun superfluous-file-name-p (name)
  "Return true if the file NAME is considered redundant. On POSIX file
systems, this means \".\" and \"..\"."
  (and name (> (length name) 0)
       (or (and (= (length name) 1)
		(equal (char name 0) #\.))
	   (and (= (length name) 2)
		(equal (char name 0) #\.)
		(equal (char name 1) #\.)))))

;; We need to use the posix version if there's no better way to do it
;; on the implementation.
;#+openmcl (config-feature :os-t-use-chdir)
;#+os-t-use-chdir (defcfun chdir :int (path :string))
#+(or openmcl sbcl abcl) (defcfun chdir :int (path :string))

;; The real question is should this munge *default-pathname-defaults* ?
;; On implementations where "load" works from *default-pathname-defaults*
;; and not from the OS current, I say yes.

;; @@@@ Need to work out the generic way

(defun change-directory (&optional path)
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given."
  (when (not path)
    (setf path (enough-namestring (user-homedir-pathname))))
  (when (pathnamep path)
    (setf path (safe-namestring path)))
  #+openmcl (syscall (chdir path))
  #+sbcl (progn
	   (syscall (chdir path))
	   (let ((tn (ignore-errors (truename path))))
	     (when tn
	       (setf *default-pathname-defaults* tn))))
  #+clisp (ext:cd path)
  #+excl (setf *default-pathname-defaults* (pathname (excl:chdir path)))
  #+cmu (setf (ext:default-directory) path)
  #+ecl
  ;; try to turn it into a directory
  ;;; @@@ this fails for .. or .
  ;;(ext:chdir (if (not (pathname-directory path))
  ;;  (make-pathname :directory `(:relative ,path))
  ;;  (make-pathname :directory path)))
  ;;; try something simpler but os dependent
  (ext:chdir (if (and (stringp path) (length path))
		 (concatenate 'string path "/")
		 path))
  #+lispworks (hcl:change-directory path)
  #+abcl
  (progn
    (syscall (chdir path))
    (setf *default-pathname-defaults* (truename path)))
  #-(or clisp excl openmcl sbcl cmu ecl lispworks abcl)
  (missing-implementation 'change-directory))

(defcfun ("getcwd" real-getcwd) :pointer (buf :pointer) (size size-t))
(defcfun pathconf :long (path :string) (name :int))
(defconstant +PC-PATH-MAX+
	 #+(or darwin sunos freebsd) 5
	 #+linux 4)
#-(or darwin sunos linux freebsd) (missing-implementation 'PC-PATH-MAX)
;; Using the root "/" is kind of bogus, because it can depend on the
;; filesystem type, but since we're using it to get the working directory.
;; This is where grovelling the MAXPATHLEN might be good.
(defparameter *path-max* nil
  "Maximum number of bytes in a path.")
(defun get-path-max ()
  (or *path-max*
      (setf *path-max* (pathconf "/" +PC-PATH-MAX+))))

(defun libc-getcwd ()
  "Return the full path of the current working directory as a string, using the
C library function getcwd."
  (let ((cwd (with-foreign-pointer-as-string (s (get-path-max))
	       (foreign-string-to-lisp (real-getcwd s (get-path-max))))))
    (if (not cwd)		; hopefully it's still valid
	(error 'posix-error :error-code *errno* :format-control "getcwd")
	cwd)))

(defun current-directory ()
  "Return the full path of the current working directory as a string."
  ;; I would like to use EXT:CD, but it puts an extra slash at the end.
  #+(or clisp sbcl cmu) (libc-getcwd)
  #+excl (excl:current-directory)
  #+(or openmcl ccl) (ccl::current-directory-name)
  #+ecl (libc-getcwd) ;; (ext:getcwd)
  ;; #+cmu (ext:default-directory)
  #+lispworks (hcl:get-working-directory)
  #+abcl (namestring (truename *default-pathname-defaults*))
  #-(or clisp excl openmcl ccl sbcl cmu ecl lispworks abcl)
  (missing-implementation 'current-directory))

(defcfun mkdir :int (path :string) (mode mode-t))

(defun make-directory (path &key (mode #o755))
  "Make a directory."
  ;; The #x1ff is because mkdir can fail if any other than the low nine bits
  ;; of the mode are set.
  (syscall (mkdir (safe-namestring path) (logand #x1ff (or mode #o777)))))

(defcfun rmdir :int (path :string))

(defun delete-directory (path)
  "Delete a directory."
  (syscall (rmdir (safe-namestring path))))

;; It's hard to fathom how insanely shitty the Unix/POSIX interface to
;; directories is. On the other hand, I might have trouble coming up with
;; a too much better interface in plain old ‘C’. Just rebuild the kernel.
;; Works fine in a two person dev team.

;; We just choose something big here and hope it works.
(defconstant MAXNAMLEN 1024 "Maximum length of a file name.")

(defconstant DT_UNKNOWN       0 "Unknown ")
(defconstant DT_FIFO          1 "FIFO file aka named pipe")
(defconstant DT_CHR           2 "Character special aka raw device")
(defconstant DT_DIR           4 "Directory file")
(defconstant DT_BLK           6 "Block special aka block device")
(defconstant DT_REG           8 "Regular file")
(defconstant DT_LNK          10 "Symbolic link")
(defconstant DT_SOCK         12 "Socket aka unix domain socket")
(defconstant DT_WHT          14 "A whiteout file! for overlay filesystems")

;; Darwin 64 bit vs 32 bit dirent:
;;
;; There are two things which are theoretically independent: whether the
;; **kernel** is 64 bit or not, and whether the execution environment is 64
;; bit or not. If the kernel is 64 bit (*64-bit-inode*), we have to use the 64
;; inode structure. If the executable environment is 64 bit (aka
;; 64-bit-target) we have to use the 64 bit function calls. But It seems like
;; now the function calls in the 32 bit executable environment can handle the
;; 64 bit dirent structure.
;; 
;; So, also, there are special readdir, etc. routines, ending in various
;; combinations of "$INODE64" and "$UNIX2003" which are partially dependent on
;; the word size of executable environment. Will this work on previous OS
;; versions? Will it work on a 32 bit kernel? I have no idea. Thanks to
;; "clever" hackery with "asm" and CPP, you can change the ancient function
;; calls right under everybody and "NO ONE WILL KNOW", right? Wrong.
;;
;; It's a complete mess, and I got this wrong for quite a long time. I think I
;; should probably just give in and use a groveler, or at least: check the
;; output from the C compiler!!

;; #+(and darwin (not os-t-64-bit-inode))
;; (defcstruct foreign-dirent
;;      "Entry in a filesystem directory. struct dirent"
;;   (d_ino	ino-t)
;;   (d_reclen	:uint16)
;;   (d_type	:uint8)
;;   (d_namlen	:uint8)
;;   (d_name	:char :count 256))

;; #+(and darwin os-t-64-bit-inode)
#+darwin ;; This seems to be it for both 32 & 64
(defcstruct foreign-dirent
  "Entry in a filesystem directory. struct dirent"
  (d_ino	ino-t)
  (d_seekoff	:uint64)
  (d_reclen	:uint16)
  (d_namlen	:uint16)
  (d_type	:uint8)
  (d_name	:char :count 1024))

#|
(defun dumply (type)
  (format t "~a~%" (foreign-type-size type))
  (format t "~a~%" (foreign-type-alignment type))
  (with-foreign-object (instance type)
    (let ((ll 
	   (loop :for slot :in (foreign-slot-names type)
	      :collect (list  
			slot (foreign-slot-offset type slot)
			(- (pointer-address
			    (foreign-slot-pointer instance type slot))
			   (pointer-address instance))))))
      (setf ll (sort ll #'< :key #'second))
      (loop :for l :in ll :do
	 (format t "~10a ~a ~a~%" (first l) (second l) (third l))))))
|#

#+sunos
(defcstruct foreign-dirent
  "Entry in a filesystem directory. struct dirent"
  (d_ino	ino-t)
  (d_off	off-t)
  (d_reclen	:unsigned-short)
  (d_name	:char :count 1024))

#+linux
(defcstruct foreign-dirent
  "Entry in a filesystem directory. struct dirent"
  (d_ino	ino-t)
  (d_off	off-t)
  (d_reclen	:unsigned-short)
  (d_type	:uint8)
  (d_name	:char :count 1024))

#+freebsd
(defcstruct foreign-dirent
  ;; I know they really want to call it "fileno", but please let's just call
  ;; it "ino" for compatibility.
  ;; (d_fileno	:uint32)
  (d_ino	:uint32)
  (d_reclen	:uint16)
  (d_type	:uint8)
  (d_namlen	:uint8)
  (d_name	:char :count #.(+ 255 1)))

#+(or linux darwin freebsd)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (config-feature :os-t-has-d-type))

;; If one of these is not defined, we just use strlen(d_name).
#+(or darwin freebsd) (config-feature :os-t-has-namlen)
#+linux (config-feature :os-t-has-reclen)

#|
(defun fooberry () "64 bit dirent, 32 bit functions"
  (let* ((dd (cffi:foreign-funcall
	      #+64-bit-target "opendir$INODE64"
	      #+32-bit-target "opendir"
	      :string "." :pointer))
	 dp nn)
    (loop :while
       (not (cffi:null-pointer-p
	     (setf dp (cffi:foreign-funcall
		       #+64-bit-target "readdir$INODE64"
		       #+32-bit-target "readdir"
		       :pointer dd :pointer))))
       :do
       (setf nn (cffi:foreign-slot-value
		 dp '(:struct nos::foreign-dirent) 'nos::d_name))
       (format t "~a~%"
	       (cffi:foreign-slot-value
		dp '(:struct foreign-dirent) 'nos::d_namlen)
	       ;; (cffi:foreign-string-to-lisp
	       ;; 	(setf nn (cffi:foreign-slot-value
	       ;; 		  dp '(:struct
	       ;; 		       #+64-bit-target nos::foreign-dirent-64
	       ;; 		       #+32-bit-target nos::foreign-dirent-64
	       ;; 		       ) 'nos::d_name))))
	       )
       (loop :with i = 0 :and c = nil
	  :do (setf c (cffi:mem-aref nn :char i))
	  (cond ((= c 0) (terpri))
		((> c 0) (princ (code-char c)))
		(t ))
	  (incf i)
	  :while (/= 0 c)))))
|#

;; opendir
#+(and darwin 64-bit-target)
(defcfun ("opendir$INODE64" opendir) :pointer (dirname :string))
#+(and darwin (not 64-bit-target))
(defcfun ("opendir$INODE64$UNIX2003" opendir) :pointer (dirname :string))
#-darwin (defcfun opendir :pointer (dirname :string))

;; closedir
#+(and darwin 64-bit-target)
(defcfun ("closedir" closedir) :int (dirp :pointer))
#+(and darwin (not 64-bit-target))
(defcfun ("closedir$UNIX2003" closedir) :int (dirp :pointer))
#-darwin
(defcfun closedir :int (dirp :pointer))

;; readdir_r
#+(and darwin 64-bit-target)
(defcfun ("readdir_r$INODE64" readdir_r)
 	    :int (dirp :pointer) (entry :pointer) (result :pointer))
#+(and darwin (not 64-bit-target))
(defcfun ("readdir_r$INODE64" readdir_r)
 	     :int (dirp :pointer) (entry :pointer) (result :pointer))
#+sunos (defcfun ("__posix_readdir_r" readdir_r)
	    :int (dirp :pointer) (entry :pointer) (result :pointer))
#-(or darwin sunos)
(defcfun readdir_r :int (dirp :pointer) (entry :pointer) (result :pointer))

;; readdir
#+(and darwin 64-bit-target)
(defcfun ("readdir$INODE64" readdir) :pointer (dirp :pointer))
#+(and darwin (not 64-bit-target))
(defcfun ("readdir$INODE64" readdir) :pointer (dirp :pointer))
#-darwin (defcfun readdir :pointer (dirp :pointer))

;; Use of reclen is generally fux0rd, so just count to the null
(defun dirent-name (ent)
  #-os-t-has-namlen
  (let* ((name (foreign-slot-value ent '(:struct foreign-dirent) 'd_name))
	 (len  (loop :with i = 0
		 :while (/= 0 (mem-aref name :unsigned-char i))
		 :do (incf i)
		 :finally (return i))))
    (foreign-string-to-lisp
     (foreign-slot-value ent '(:struct foreign-dirent) 'd_name)
     :count len))
  #+os-t-has-namlen
  (foreign-string-to-lisp
   (foreign-slot-value ent '(:struct foreign-dirent) 'd_name)
   :count (foreign-slot-value ent '(:struct foreign-dirent) 'd_namlen)))

(defun dirent-type (ent)
  #+os-t-has-d-type
  (with-foreign-slots ((d_type) ent (:struct foreign-dirent))
    (cond
      ((= d_type DT_UNKNOWN) :unknown)
      ((= d_type DT_FIFO)    :pipe)
      ((= d_type DT_CHR)     :character-device)
      ((= d_type DT_DIR)     :directory)
      ((= d_type DT_BLK)     :block-device)
      ((= d_type DT_REG)     :regular)
      ((= d_type DT_LNK)     :link)
      ((= d_type DT_SOCK)    :socket)
      ((= d_type DT_WHT)     :whiteout)
      (t :undefined)))
  #-os-t-has-d-type (declare (ignore ent))
  #-os-t-has-d-type :unknown)

;; This is really only for debugging.
(defun convert-dirent (ent)
  (with-foreign-slots ((d_ino) ent (:struct foreign-dirent))
    (make-dir-entry
     :name (dirent-name ent)
     :type (dirent-type ent)
     :inode d_ino)))

(defun dump-dirent (ent)
  (with-foreign-slots ((d_ino d_reclen d_type d_namlen d_name)
		       ent (:struct foreign-dirent))
    (format t "ino~20t~a~%"    d_ino)
    #+os-t-has-reclen (format t "reclen~20t~a~%" d_reclen)
    #+os-t-has-d-type (format t "type~20t~a~%"   d_type)
    #+os-t-has-namlen (format t "namlen~20t~a~%" d_namlen)
    (format t "name~20t~a~%"   d_name)))

;; If wanted, we could consider also doing "*" for executable. Of course
;; we would have the overhead of doing a stat(2).

#|
(defun tir ()
  "Test of opendir/readdir"
  (let* ((dirp (opendir "."))
	 ent p str quit-flag)
    (format t "dirp = ~a null = ~a~%" dirp (null-pointer-p dirp))
    (loop
       :until quit-flag
       :do
       (setf p (readdir dirp))
       (format t "p = ~a null = ~a~%" p (null-pointer-p p))
;       (setf ent (mem-ref p '(:pointer (:struct foreign-dirent-64))))
       (with-foreign-slots ((d_ino
			     #| d_seekoff |#
			     d_reclen
			     #+os-t-has-namlen d_namlen
			     d_type
			     d_name)
			    p (:struct foreign-dirent))
	 (format t "ino ~a" d_ino)
;;;	   (format t " seekoff ~a" d_seekoff)
	 (format t " reclen ~a" d_reclen)
	 #+os-t-has-namlen (format t " namlen ~a" d_namlen)
	 (format t " type ~a" d_type)
	 (format t " name ~a~%" d_name)
;	 (setf str (make-string d_namlen))
	 (setf str
	       (with-output-to-string (s)
		 (loop :with c = nil :and i = 0
;		    :for i :from 0 :below d_namlen
		    :while (not (zerop (setf c (mem-aref d_name :unsigned-char i))))
		    :do ;(format t "c=~a " c)
		    (when (> c 0)
		      (write-char (code-char c) s) (incf i)))))
	 (format t "\"~a\"~%" str))
       (when (equalp (read-line) "q")
	 (setf quit-flag t)))
    (closedir dirp)))
|#

(defun read-directory (&key dir append-type full omit-hidden)
  "Return a list of the file names in DIR as strings. DIR defaults to the ~
current directory. If APPEND-TYPE is true, append a character to the end of ~
the name indicating what type of file it is. Indicators are:
  / : directory
  @ : symbolic link
  | : FIFO (named pipe)
  = : Socket
  > : Doors
If FULL is true, return a list of dir-entry structures instead of file name ~
strings. Some dir-entry-type keywords are:
  :unknown :pipe :character-device :directory :block-device :regular :link
  :socket :whiteout :undefined
Be aware that DIR-ENTRY-TYPE type can't really be relied on, since many
systems return :UNKNOWN or something, when the actual type can be determined
by FILE-INFO-TYPE.
If OMIT-HIDDEN is true, do not include entries that start with ‘.’.
"
  (declare (type (or string null) dir) (type boolean append-type full))
  (when (not dir)
    (setf dir "."))
  (let ((dirp nil)
	(result 0)
	(dir-list nil))
    (unwind-protect
      (progn
	(if (null-pointer-p (setf dirp (opendir dir)))
	  (error 'posix-error :error-code *errno*
		 :format-control "opendir: ~a: ~a"
		 :format-arguments `(,dir ,(error-message *errno*)))
	  (progn
	    (with-foreign-objects ((ent '(:struct foreign-dirent))
				   (ptr :pointer))
	      (with-foreign-slots ((d_name
				    #+os-t-has-d-type d_type
				    d_ino)
				   ent (:struct foreign-dirent))
		(setf dir-list
		      (loop :while
			  (and (eql 0 (setf result (readdir_r dirp ent ptr)))
			       (not (null-pointer-p (mem-ref ptr :pointer))))
			 :if (not (and omit-hidden
				       (hidden-file-name-p (dirent-name ent))))
			 :collect
			 (if full
			     (make-dir-entry
			      :name (dirent-name ent)
			      :type (dirent-type ent)
			      :inode d_ino)
			     ;; not full
			     (if append-type
				 #+os-t-has-d-type
				 (concatenate 'string (dirent-name ent)
					      (cond
						((= d_type DT_FIFO) "|")
						((= d_type DT_DIR)  "/")
						((= d_type DT_LNK)  "@")
						((= d_type DT_SOCK) "=")))
				 #-os-t-has-d-type (dirent-name ent)
				 (dirent-name ent)))))))
	    (when (not (= result 0))
	      (error 'posix-error :format-control "readdir"
		     :error-code *errno*)))))
      (if (not (null-pointer-p dirp))
	  (syscall (closedir dirp))))
    dir-list))

(defmacro without-access-errors (&body body)
  "Evaluate the body while ignoring typical file access error from system
calls. Returns NIL when there is an error."
  `(handler-case
       (progn ,@body)
     (posix-error (c)
       (when (not (find (opsys-error-code c)
			`(,+ENOENT+ ,+EACCES+ ,+ENOTDIR+)))
	 (signal c)))))

(defcfun ("chroot" real-chroot) :int (dirname :string))
(defun chroot (dirname) (syscall (real-chroot dirname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

#|
#+(or darwin freebsd linux)
(progn
  (defconstant +O_RDONLY+   #x0000 "Open for reading only")
  (defconstant +O_WRONLY+   #x0001 "Open for writing only")
  (defconstant +O_RDWR+	    #x0002 "Open for reading and writing")
  (defconstant +O_ACCMODE+  #x0003 "Mask for above modes")
  (defconstant +O_NONBLOCK+ #+(or darwin freebsd) #x0004 #+linux #o04000
	       "No delay")
  (defconstant +O_APPEND+   #+(or darwin freebsd) #x0008 #+linux #o02000
	       "Set append mode")
  (defconstant +O_ASYNC+    #+(or darwin freebsd) #x0040 #+linux #x020000
	       "Signal pgrp when data ready")
  (defconstant +O_SYNC+	    #+(or darwin freebsd) #x0080 #+linux #o04010000
	       "Synchronous writes")
  (defconstant +O_SHLOCK+   #x0010 "Atomically obtain a shared lock")
  (defconstant +O_EXLOCK+   #x0020 "Atomically obtain an exclusive lock")
  (defconstant +O_CREAT+    #+(or darwin freebsd) #x0200 #+linux #o100
	       "Create if nonexistant")
  (defconstant +O_TRUNC+    #+(or darwin freebsd) #x0400 #+linux #o01000
	       "Truncate to zero length")
  (defconstant +O_EXCL+	    #+(or darwin freebsd) #x0800 #+linux #o0200
	       "Error if create and already exists")
  (defconstant +O_NOCTTY+   #+darwin #x20000 #+linux #o0400 #+freebsd #x8000
	       "Don't assign controlling terminal"))
#+darwin
(defconstant +O_EVTONLY+  #x8000 "Requested for event notifications only")

#+linux
(progn
  (defconstant +O_LARGEFILE+ #o0100000)
  (defconstant +O_DIRECTORY+ #o0200000)
  (defconstant +O_NOFOLLOW+  #o0400000)
  (defconstant +O_DIRECT+    #o040000)
  (defconstant +O_NOATIME+   #o01000000)
  (defconstant +O_PATH+	     #o010000000)
  (defconstant +O_DSYNC+     #o010000)
  (defconstant +O_TMPFILE+   #o020200000))

#+freebsd
(progn
  (defconstant +O_NOFOLLOW+  #x00000100 "Don't follow symlinks")
  (defconstant +O_DIRECT+    #x00010000 "Attempt to bypass buffer cache")
  (defconstant +O_DIRECTORY+ #x00020000 "Fail if not directory")
  (defconstant +O_EXEC+	     #x00040000 "Open for execute only")
  (defconstant +O_FSYNC+     #x00000080 "Synchronous writes")
  (defconstant +O_TTY_INIT+  #x00080000 "Restore default termios attributes")
  (defconstant +O_CLOEXEC+   #x00100000))
|#

(defparameter *file-flags* nil "Flag for open and fcntl.")

#+(or darwin freebsd linux)
(define-to-list *file-flags*
  #(#(+O_RDONLY+   #x0000 "Open for reading only")
    #(+O_WRONLY+   #x0001 "Open for writing only")
    #(+O_RDWR+	   #x0002 "Open for reading and writing")
    #(+O_NONBLOCK+ #+(or darwin freebsd) #x0004 #+linux #o04000 "No delay")
    #(+O_APPEND+   #+(or darwin freebsd) #x0008 #+linux #o02000
      "Set append mode")
    #(+O_ASYNC+	   #+(or darwin freebsd) #x0040 #+linux #x020000
      "Signal pgrp when data ready")
    #(+O_SYNC+	   #+(or darwin freebsd) #x0080 #+linux #o04010000
      "Synchronous writes")
    #(+O_SHLOCK+   #x0010 "Atomically obtain a shared lock")
    #(+O_EXLOCK+   #x0020 "Atomically obtain an exclusive lock")
    #(+O_CREAT+	   #+(or darwin freebsd) #x0200 #+linux #o100
      "Create if nonexistant")
    #(+O_TRUNC+	   #+(or darwin freebsd) #x0400 #+linux #o01000
      "Truncate to zero length")
    #(+O_EXCL+	   #+(or darwin freebsd) #x0800 #+linux #o0200
      "Error if create and already exists")
    #(+O_NOCTTY+   #+darwin #x20000 #+linux #o0400 #+freebsd #x8000
      "Don't assign controlling terminal")))

#+(or darwin freebsd linux)
(defconstant +O_ACCMODE+ #x0003 "Mask for above modes")

#+darwin
(define-to-list *file-flags*
  #((+O_EVTONLY+ #x8000 "Requested for event notifications only")))

#+linux
(define-to-list *file-flags*
  #(#(+O_LARGEFILE+ #o000100000 "Crappy old fashioned work around.")
    #(+O_DIRECTORY+ #o000200000 "Fail if not directory")
    #(+O_NOFOLLOW+  #o000400000 "Don't follow symlinks")
    #(+O_DIRECT+    #o000040000 "Attempt to bypass buffer cache")
    #(+O_NOATIME+   #o001000000 "Don't update acess time")
    #(+O_PATH+      #o010000000 "Path bookmarking")
    #(+O_DSYNC+     #o000010000 "Data synchronization")
    #(+O_TMPFILE+   #o020200000 "Temporary anonymous")))

#+freebsd
(define-to-list *file-flags*
  #(#(+O_NOFOLLOW+  #x00000100 "Don't follow symlinks")
    #(+O_DIRECT+    #x00010000 "Attempt to bypass buffer cache")
    #(+O_DIRECTORY+ #x00020000 "Fail if not directory")
    #(+O_EXEC+	    #x00040000 "Open for execute only")
    #(+O_FSYNC+	    #x00000080 "Synchronous writes")
    #(+O_TTY_INIT+  #x00080000 "Restore default termios attributes")
    #(+O_CLOEXEC+   #x00100000 "Close on exec")))

(defcfun ("open"   posix-open)   :int (path :string) (flags :int) (mode mode-t))
(defcfun ("close"  posix-close)  :int (fd :int))
(defcfun ("read"   posix-read)   :int (fd :int) (buf :pointer) (nbytes size-t))
(defcfun ("write"  posix-write)  :int (fd :int) (buf :pointer) (nbytes size-t))
(defcfun ("ioctl"  posix-ioctl)  :int (fd :int) (request :int) (arg :pointer))
(defcfun ("unlink" posix-unlink) :int (path :string))

(defun simple-delete-file (path)
  "Delete a file."
  (syscall (posix-unlink (safe-namestring path))))

(defmacro with-posix-file ((var filename flags &optional (mode 0)) &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor
opened on FILENAME with FLAGS and MODE."
  `(let (,var)
     (unwind-protect
       (progn
	 (setf ,var (posix-open ,filename ,flags ,mode))
	 ,@body)
       (if (>= ,var 0)
	   (posix-close ,var)
	   (error-check ,var)))))

(defmacro with-os-file ((var filename &key
			     (direction :input)
			     (if-exists :error)
			     (if-does-not-exist :error)) &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor
opened on FILENAME. DIRECTION, IF-EXISTS, and IF-DOES-NOT-EXIST are simpler
versions of the keywords used in Lisp open.
  DIRECTION         - supports :INPUT, :OUTPUT, and :IO.
  IF-EXISTS         - supports :ERROR and :APPEND.
  IF-DOES-NOT-EXIST - supports :ERROR, and :CREATE.
"
  (let ((flags 0))
    (cond
      ((eq direction :input)    (setf flags +O_RDONLY+))
      ((eq direction :output)   (setf flags +O_WRONLY+))
      ((eq direction :io)       (setf flags +O_RDWR+))
      (t (error ":DIRECTION should be one of :INPUT, :OUTPUT, or :IO.")))
    (cond
      ((eq if-exists :append) (setf flags (logior flags +O_APPEND+)))
      ((eq if-exists :error) #| we cool |# )
      (t (error ":IF-EXISTS should be one of :ERROR, or :APPEND.")))
    (cond
      ((eq if-does-not-exist :create) (setf flags (logior flags +O_CREAT+)))
      ((eq if-does-not-exist :error) #| we cool |# )
      (t (error ":IF-DOES-NOT-EXIST should be one of :ERROR, or :CREATE.")))
    `(with-posix-file (,var ,filename ,flags)
       ,@body)))

(defcfun mkstemp :int (template :string))

;; what about ioctl defines?

#+(or darwin linux freebsd)
(progn
  (defconstant +F_DUPFD+	  0)
  (defconstant +F_DUPFD_CLOEXEC+  #+darwin 67 #+linux 1030 #+freebsd 17)
  (defconstant +F_GETFD+	  1)
  (defconstant +F_SETFD+	  2)
  (defconstant +F_GETFL+	  3)
  (defconstant +F_SETFL+	  4)
  (defconstant +F_GETOWN+	  #+(or darwin freebsd) 5 #+linux 9)
  (defconstant +F_SETOWN+	  #+(or darwin freebsd) 6 #+linux 8)
  (defconstant +F_GETLK+	  #+darwin 7 #+linux 5 #+freebsd 11)
  (defconstant +F_SETLK+	  #+darwin 8 #+linux 6 #+freebsd 12)
  (defconstant +F_SETLKW+	  #+darwin 9 #+linux 7 #+freebsd 13)
  (defconstant +FD_CLOEXEC+       1))

#+linux
(progn
  (defconstant +F_SETSIG+	   10 "Set number of signal to be sent.")
  (defconstant +F_GETSIG+	   11 "Get number of signal to be sent.")
  (defconstant +F_SETOWN_EX+	   15 "Get owner (thread receiving SIGIO).")
  (defconstant +F_GETOWN_EX+	   16 "Set owner (thread receiving SIGIO).")
  (defconstant +LOCK_MAND+	   32 "This is a mandatory flock:")
  (defconstant +LOCK_READ+	   64 ".. with concurrent read")
  (defconstant +LOCK_WRITE+	  128 ".. with concurrent write")
  (defconstant +LOCK_RW+	  192 ".. with concurrent read & write")
  (defconstant +F_SETLEASE+	 1024 "Set a lease.")
  (defconstant +F_GETLEASE+	 1025 "Enquire what lease is active.")
  (defconstant +F_NOTIFY+	 1026 "Request notifications on a directory.")
  (defconstant +F_SETPIPE_SZ+	 1031 "Set pipe page size array.")
  (defconstant +F_GETPIPE_SZ+	 1032 "Set pipe page size array.")
  ;; Types for F_NOTIFY
  (defconstant +DN_ACCESS+      #x00000001 "File accessed.")
  (defconstant +DN_MODIFY+      #x00000002 "File modified.")
  (defconstant +DN_CREATE+      #x00000004 "File created.")
  (defconstant +DN_DELETE+      #x00000008 "File removed.")
  (defconstant +DN_RENAME+      #x00000010 "File renamed.")
  (defconstant +DN_ATTRIB+      #x00000020 "File changed attributes.")
  (defconstant +DN_MULTISHOT+   #x80000000 "Don't remove notifier.")
  )

#+freebsd
(progn
  (defconstant +F_RDLCK+	   1  "Shared or read lock")
  (defconstant +F_UNLCK+	   2  "Unlock")
  (defconstant +F_WRLCK+	   3  "Exclusive or write lock")
  (defconstant +F_UNLCKSYS+	   4  "Purge locks for a given system ID")
  (defconstant +F_CANCEL+	   5  "Cancel an async lock request")
  (defconstant +F_DUP2FD+	   10 "Duplicate file descriptor to arg")
  (defconstant +F_SETLK_REMOTE+	   14 "Debugging support for remote locks")
  (defconstant +F_READAHEAD+	   15 "Read ahead")
  (defconstant +F_RDAHEAD+	   16 "Read ahead")
  (defconstant +F_DUPFD_CLOEXEC+   17 "Like F_DUPFD, but FD_CLOEXEC is set")
  (defconstant +F_DUP2FD_CLOEXEC+  18 "Like F_DUP2FD, but FD_CLOEXEC is set")
)

#+darwin
(progn
  (defconstant +F_RDAHEAD+		45)
  (defconstant +F_GETPATH+		50)
  (defconstant +F_PREALLOCATE+		42)
  (defconstant +F_SETSIZE+		43)
  (defconstant +F_RDADVISE+		44)
  (defconstant +F_READBOOTSTRAP+	46)
  (defconstant +F_WRITEBOOTSTRAP+	47)
  (defconstant +F_NOCACHE+		48)
  (defconstant +F_LOG2PHYS+		49)
  (defconstant +F_LOG2PHYS_EXT+		65)
  (defconstant +F_FULLFSYNC+		51)
  (defconstant +F_FREEZE_FS+		53)
  (defconstant +F_THAW_FS+		54)
  (defconstant +F_GLOBAL_NOCACHE+	55)
  (defconstant +F_ADDSIGS+		59)
  (defconstant +F_MARKDEPENDENCY+	60)
  (defconstant +F_ADDFILESIGS+		61)
  (defconstant +F_NODIRECT+		62)
  (defconstant +F_SETNOSIGPIPE+		73)
  (defconstant +F_GETNOSIGPIPE+		74)
  (defconstant +F_GETPROTECTIONCLASS+	63)
  (defconstant +F_SETPROTECTIONCLASS+	64)
  (defconstant +F_GETLKPID+		66)
  (defconstant +F_SETBACKINGSTORE+	70)
  (defconstant +F_GETPATH_MTMINFO+	71)
  (defconstant +F_ALLOCATECONTIG+	#x00000002)
  (defconstant +F_ALLOCATEALL+		#x00000004)
  (defconstant +F_PEOFPOSMODE+		3)
  (defconstant +F_VOLPOSMODE+		4))

(defcstruct flock
  "Advisory file segment locking data type."
  (l_start  off-t)			; Starting offset
  (l_len    off-t)			; len = 0 means until end of file
  (l_pid    pid-t)			; Lock owner
  (l_type   :short)			; Lock type: read/write, etc.
  (l_whence :short))			; Type of l_start

(defcstruct fstore
  "Used by F_DEALLOCATE and F_PREALLOCATE commands."
  (fst_flags :unsigned-int)		; IN: flags word
  (fst_posmode :int )			; IN: indicates use of offset field
  (fst_offset off-t)			; IN: start of the region
  (fst_length off-t)			; IN: size of the region
  (fst_bytesalloc off-t))		; OUT: number of bytes allocated

(defcstruct radvisory
  "Advisory file read data type"
  (ra_offset off-t)
  (ra_count :int))

(defcstruct fsignatures
  "Detached code signatures data type"
  (fs_file_start off-t)
  (fs_blob_start (:pointer :void))
  (fs_blob_size size-t))

(defcstruct fbootstraptransfer
  "Used by F_READBOOTSTRAP and F_WRITEBOOTSTRAP commands"
  (fbt_offset off-t)			; IN: offset to start read/write
  (fbt_length size-t)			; IN: number of bytes to transfer
  (fbt_buffer (:pointer :void)))	; IN: buffer to be read/written

(defcstruct log2phys
  "For F_LOG2PHYS and F_LOG2PHYS_EXT"
  (l2p_flags :unsigned-int)
  (l2p_contigbytes off-t)
  (l2p_devoffset off-t))

(defcfun fcntl :int (fd :int) (cmd :int) &rest)

(defun get-file-descriptor-flags (file-descriptor)
  "Return a list of the flags set on FILE-DESCRIPTOR."
  (let* ((flags   (fcntl file-descriptor +F_GETFL+))
	 (d-flags (fcntl file-descriptor +F_GETFD+))
	 result)
    ;; The others we can check if they're positive.
    (loop :for flag :in *file-flags*
       :if (plusp (logand (symbol-value flag) flags))
       :do (push flag result))

    ;; Need to special case this because it's usually defined as zero.
    (when (= (logand flags +O_ACCMODE+) +O_RDONLY+)
      (push '+O_RDONLY+ result))

    (when (plusp (logand d-flags +FD_CLOEXEC+))
      (push '+FD_CLOEXEC+ result))
    result))
	  
;; stat / lstat

;; st_mode bits
(defconstant		S_IFMT   #o0170000)	; type of file (mask)
(defconstant		S_IFIFO  #o0010000)	; named pipe (fifo)
(defconstant		S_IFCHR  #o0020000)	; character special
(defconstant		S_IFDIR  #o0040000)	; directory
(defconstant		S_IFNAM  #o0050000)	; XENIX named IPC
(defconstant		S_IFBLK  #o0060000)	; block special
(defconstant		S_IFREG  #o0100000)	; regular
(defconstant		S_IFLNK  #o0120000)	; symbolic link
(defconstant   		S_IFSOCK #o0140000)	; socket
#+sunos (defconstant	S_IFDOOR #o0150000)	; door
#+darwin  (defconstant	S_IFWHT  #o0160000)	; whiteout (obsolete)
#+sunos (defconstant	S_IFPORT #o0160000)	; event port

;; These should be the same on any POSIX
(defconstant S_ISUID #o0004000)	; set user id on execution
(defconstant S_ISGID #o0002000)	; set group id on execution
(defconstant S_ISVTX #o0001000)	; save swapped text even after use
(defconstant S_IRUSR #o0000400)	; read permission, owner
(defconstant S_IWUSR #o0000200)	; write permission, owner
(defconstant S_IXUSR #o0000100)	; execute/search permission, owner
(defconstant S_IRGRP #o0000040)	; read permission, group
(defconstant S_IWGRP #o0000020)	; write permission, group
(defconstant S_IXGRP #o0000010)	; execute/search permission, group
(defconstant S_IROTH #o0000004)	; read permission, other
(defconstant S_IWOTH #o0000002)	; write permission, other
(defconstant S_IXOTH #o0000001)	; execute/search permission, other

(defun is-user-readable    (mode) (/= (logand mode S_IRUSR) 0))
(defun is-user-writable    (mode) (/= (logand mode S_IWUSR) 0))
(defun is-user-executable  (mode) (/= (logand mode S_IXUSR) 0))
(defun is-group-readable   (mode) (/= (logand mode S_IRGRP) 0))
(defun is-group-writable   (mode) (/= (logand mode S_IWGRP) 0))
(defun is-group-executable (mode) (/= (logand mode S_IXGRP) 0))
(defun is-other-readable   (mode) (/= (logand mode S_IROTH) 0))
(defun is-other-writable   (mode) (/= (logand mode S_IWOTH) 0))
(defun is-other-executable (mode) (/= (logand mode S_IXOTH) 0))

(defun is-set-uid          (mode) (/= (logand mode S_ISUID) 0))
(defun is-set-gid          (mode) (/= (logand mode S_ISGID) 0))
(defun is-sticky           (mode) (/= (logand mode S_ISVTX) 0))

(defun is-fifo             (mode) (= (logand mode S_IFMT) S_IFIFO))
(defun is-character-device (mode) (= (logand mode S_IFMT) S_IFCHR))
(defun is-directory        (mode) (= (logand mode S_IFMT) S_IFDIR))
(defun is-block-device     (mode) (= (logand mode S_IFMT) S_IFBLK))
(defun is-regular-file     (mode) (= (logand mode S_IFMT) S_IFREG))
(defun is-symbolic-link    (mode) (= (logand mode S_IFMT) S_IFLNK))
(defun is-socket           (mode) (= (logand mode S_IFMT) S_IFSOCK))
(defun is-door 		   (mode)
  #+sunos (= (logand mode S_IFMT) S_IFDOOR)
  #-sunos (declare (ignore mode))
  )
(defun is-whiteout         (mode)
  #+darwin (= (logand mode S_IFMT) S_IFWHT)
  #-darwin (declare (ignore mode))
  )
(defun is-port             (mode)
  #+sunos (= (logand mode S_IFMT) S_IFPORT)
  #-sunos (declare (ignore mode))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct file-type-info
    "Store data for file types."
    test
    symbol
    char
    name)

  (defmethod make-load-form ((s file-type-info) &optional environment)
    (make-load-form-saving-slots s :environment environment)))

(defparameter *file-type-data*
  (macrolet ((moo (test symbol char name)
	       (make-file-type-info
		:test test :symbol symbol :char char :name name)))
    (list
     (moo is-fifo	      :FIFO		  #\F "FIFO")
     (moo is-character-device :character-special  #\c "character special")
     (moo is-directory	      :directory	  #\d "directory")
     (moo is-block-device     :block-special	  #\b "block special")
     (moo is-regular-file     :regular		  #\r "regular")
     (moo is-symbolic-link    :link		  #\l "symbolic link")
     (moo is-socket	      :socket		  #\s "socket")
     (moo is-door	      :door		  #\d "door")
     (moo is-whiteout	      :whiteout		  #\w "whiteout"))))

(defparameter *mode-tags*
  '((is-fifo		   	"FIFO")
    (is-character-device	"character special")
    (is-directory		"directory")
    (is-block-device		"block special")
    (is-regular-file		"regular")
    (is-symbolic-link		"symbolic link")
    (is-socket			"socket")
    (is-door			"door")
    (is-whiteout		"whiteout"))
  "Sequence of test functions and strings for printing modes.")

(defparameter *mode-tag-chars*
  '((is-fifo		   	#\p)
    (is-character-device	#\c)
    (is-directory		#\d)
    (is-block-device		#\b)
    (is-regular-file		#\-)
    (is-symbolic-link		#\l)
    (is-socket			#\s)
    (is-door		        #\D)
    (is-whiteout		#\w))
  "Sequence of test functions and strings for printing modes.")

(defparameter *permission-tags*
  '((is-user-readable		#\r)
    (is-user-writable		#\w)
    (is-user-executable		#\x)
    (is-group-readable		#\r)
    (is-group-writable		#\w)
    (is-group-executable	#\x)
    (is-other-readable		#\r)
    (is-other-writable		#\w)
    (is-other-executable	#\x))
  "Sequence of test functions and strings for printing permission bits.")

;; @@@ This is too slow
(defun file-type-char (mode)
  "Return the character representing the file type of MODE."
  (loop :for f :in *file-type-data* :do
     (when (funcall (file-type-info-test f) mode)
       (return-from file-type-char (file-type-info-char f)))))

;; @@@ This is too slow
(defun file-type-name (mode)
  "Return the character representing the file type of MODE."
  (loop :for f :in *file-type-data* :do
     (when (funcall (file-type-info-test f) mode)
       (return-from file-type-name (file-type-info-name f)))))

(defun file-type-symbol (mode)
  "Return the keyword representing the file type of MODE."
  (loop :for f :in *file-type-data* :do
     (when (funcall (file-type-info-test f) mode)
       (return-from file-type-symbol (file-type-info-symbol f)))))

(defun symbolic-mode (mode)
  "Convert a number to mode string. Like strmode."
  (with-output-to-string (stream)
    (loop :for (func chr) :in *mode-tag-chars*
       :do (when (apply func (list mode)) (princ chr stream)))

    (if (is-user-readable mode) (princ #\r stream) (princ #\- stream))
    (if (is-user-writable mode) (princ #\w stream) (princ #\- stream))
    (if (is-set-uid mode)
	(if (is-user-executable mode)
	    (princ #\s stream)
	    (princ #\S stream))
	(if (is-user-executable mode)
	    (princ #\x stream)
	    (princ #\- stream)))

    (if (is-group-readable mode) (princ #\r stream) (princ #\- stream))
    (if (is-group-writable mode) (princ #\w stream) (princ #\- stream))
    (if (is-set-gid mode)
	(if (is-group-executable mode)
	    (princ #\s stream)
	    (princ #\S stream))
	(if (is-group-executable mode)
	    (princ #\x stream)
	    (princ #\- stream)))

    (if (is-other-readable mode) (princ #\r stream) (princ #\- stream))
    (if (is-other-writable mode) (princ #\w stream) (princ #\- stream))
    (if (is-sticky mode)
	(if (is-other-executable mode)
	    (princ #\t stream)
	    (princ #\T stream))
	(if (is-other-executable mode)
	    (princ #\x stream)
	    (princ #\- stream)))))

;; Damnable file flags.
(defconstant UF_SETTABLE     #x0000ffff "Mask of owner changeable flags.")
(defconstant UF_NODUMP       #x00000001 "Do not dump file.")
(defconstant UF_IMMUTABLE    #x00000002 "File may not be changed.")
(defconstant UF_APPEND       #x00000004 "Writes to file may only append.")
(defconstant UF_OPAQUE       #x00000008 "Directory is opaque wrt. union.")
(defconstant UF_NOUNLINK     #x00000010 "File may not be removed or renamed.")
(defconstant UF_COMPRESSED   #x00000020 "File is hfs-compressed.")
(defconstant UF_TRACKED	     #x00000040
  "UF_TRACKED is used for dealing with document IDs. We no longer issue
  notifications for deletes or renames for files which have UF_TRACKED set.")
(defconstant UF_HIDDEN	     #x00008000
  "Hint that this item should not be displayed in a GUI.")
;;; Super-user changeable flags.
(defconstant SF_SETTABLE     #xffff0000 "Mask of superuser changeable flags.")
(defconstant SF_ARCHIVED     #x00010000 "File is archived.")
(defconstant SF_IMMUTABLE    #x00020000 "File may not be changed.")
(defconstant SF_APPEND	     #x00040000 "Writes to file may only append.")
(defconstant SF_RESTRICTED   #x00080000 "Restricted access.")
(defconstant SF_SNAPSHOT     #x00200000 "Snapshot inode.")

(defun flag-user-settable   (flag) (/= (logand flag UF_SETTABLE)   0))
(defun flag-user-nodump	    (flag) (/= (logand flag UF_NODUMP)	   0))
(defun flag-user-immutable  (flag) (/= (logand flag UF_IMMUTABLE)  0))
(defun flag-user-append	    (flag) (/= (logand flag UF_APPEND)	   0))
(defun flag-user-opaque	    (flag) (/= (logand flag UF_OPAQUE)	   0))
(defun flag-user-nounlink   (flag) (/= (logand flag UF_NOUNLINK)   0))
(defun flag-user-compressed (flag) (/= (logand flag UF_COMPRESSED) 0))
(defun flag-user-tracked    (flag) (/= (logand flag UF_TRACKED)	   0))
(defun flag-user-hidden	    (flag) (/= (logand flag UF_HIDDEN)	   0))
(defun flag-root-settable   (flag) (/= (logand flag SF_SETTABLE)   0))
(defun flag-root-archived   (flag) (/= (logand flag SF_ARCHIVED)   0))
(defun flag-root-immutable  (flag) (/= (logand flag SF_IMMUTABLE)  0))
(defun flag-root-append	    (flag) (/= (logand flag SF_APPEND)	   0))
(defun flag-root-restricted (flag) (/= (logand flag SF_RESTRICTED) 0))
(defun flag-root-snapshot   (flag) (/= (logand flag SF_SNAPSHOT)   0))

(defun flags-string (flags)
  (with-output-to-string (str)
    (when (flag-user-nodump     flags) (princ "nodump "		str))
    (when (flag-user-immutable  flags) (princ "uimmutable "	str))
    (when (flag-user-append     flags) (princ "uappend "	str))
    (when (flag-user-opaque     flags) (princ "opaque "		str))
    (when (flag-user-nounlink   flags) (princ "nounlink "	str))
    (when (flag-user-compressed flags) (princ "compressed "	str))
    (when (flag-user-tracked    flags) (princ "tracked "	str))
    (when (flag-user-hidden     flags) (princ "hidden "		str))

    (when (flag-root-archived   flags) (princ "archived "	str))
    (when (flag-root-immutable  flags) (princ "simmutable "	str))
    (when (flag-root-append     flags) (princ "sappend "	str))
    (when (flag-root-restricted flags) (princ "restricted "	str))
    (when (flag-root-snapshot   flags) (princ "snapshot "	str))))

 #|
;;; @@@ totally not done yet and messed up
(defun change-mode (orig-mode new-mode)
  "Change a mode by the symbolic mode changing syntax, as in chmod."
  (let ((result orig-mode) (i 0) user group others op)
    (labels ((change-one ()
	       (loop :with done
		  :while (not done)
		  :for c :in (subseq new-mode i) :do
		  (case c
		    (#\u (setf user t))
		    (#\u (setf group t))
		    ((#\o #\a) (setf others t))
		    (#\+ (setf op #'logior  done t))
		    (#\- (setf op #'logiand done t))
		    (#\= (setf op #'done t))
		    (t (error "Unknown permission type character '~c'." c)))
		  (incf i))
	       (loop :with done
		  :while (not done)
		  :for c :in (subseq new-mode i) :do
		  (case c
		    (#\r (setf (logior bits read)))
		    (#\w (setf (logior bits write)))
		    (#\x (setf (logior bits execute)))
		    (#\S (setf (logior bits sticky-group)))
		    (#\s (setf (logior bits sticky-user)))
		    (#\t (setf (logior bits sticky-others)))
		    (#\T (setf (logior bits sticky-???)))
		    (t (error "Unknown permission access character '~c'." c)))
		  (incf i))))
      (loop :do
	 (case (char new-mode i)
	   (#\space (incf i))
	   (#\, (incf i) (change-one)))
	 :while (and (not done) (< i (lentgh new-mode))))))
  )

(defun numeric-mode-offset (orig-mode new-mode)
  "Convert a symbolic mode offset string to a mode offset number."
  ;; @@@
  )

(defun symbolic-mask (mask)
  "Describe a change to a mode in symbolic mode syntax."
  )
|#

(defcstruct foreign-timespec
  (tv_sec  time-t)
  (tv_nsec :long))

(defstruct timespec
  seconds
  nanoseconds)

(defun convert-timespec (ts)
  (etypecase ts
    (foreign-pointer
     (if (null-pointer-p ts)
	 nil
	 (with-foreign-slots ((tv_sec tv_nsec) ts (:struct foreign-timespec))
	   (make-timespec :seconds tv_sec :nanoseconds tv_nsec))))
    (cons
     (make-timespec :seconds (getf ts 'tv_sec)
		    :nanoseconds (getf ts 'tv_nsec)))))

#+darwin (config-feature :os-t-has-birthtime)

#+old_obsolete_stat
(defcstruct foreign-stat
  (st_dev	dev-t)			; device inode resides on
  (st_ino	ino-t)			; inode's number
  (st_mode	mode-t)			; inode protection mode
  (st_nlink	nlink-t)		; number or hard links to the file
  (st_uid	uid-t)			; user-id of owner 
  (st_gid	gid-t)			; group-id of owner
  (st_rdev	dev-t)			; device type, for special file inode
  (st_atimespec (:struct foreign-timespec)) ; time of last access
  (st_mtimespec (:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec (:struct foreign-timespec)) ; time of last file status change
  (st_size	off-t)			; file size, in bytes
  (st_blocks	quad-t)			; blocks allocated for file
  (st_blksize	#+darwin :int32		; optimal file sys I/O ops blocksize
		#-darwin :unsigned-long)
  (st_flags	:unsigned-long)		; user defined flags for file
  (st_gen	:unsigned-long)		; file generation number
)

#+(and darwin nil)
(defcstruct foreign-stat
  (st_dev	dev-t)			; device inode resides on
  (st_mode	mode-t)			; inode protection mode
  (st_nlink	nlink-t)		; number or hard links to the file
  (st_ino	ino-t)			; inode's number
  (st_uid	uid-t)			; user-id of owner 
  (st_gid	gid-t)			; group-id of owner
  (st_rdev	dev-t)			; device type, for special file inode
  (st_atimespec (:struct foreign-timespec)) ; time of last access
  (st_mtimespec (:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec (:struct foreign-timespec)) ; time of last file status change
  (st_birthtimespec (:struct foreign-timespec)) ; time of last file status change
  (st_size	off-t)			; file size, in bytes
  (st_blocks	blkcnt-t)		; blocks allocated for file
  (st_blksize	blksize-t)		; optimal file sys I/O ops blocksize
  (st_flags	:uint32)		; user defined flags for file
  (st_gen	:uint32)		; file generation number
  (st_lspare	:int32)			; file generation number
  (st_qspare	:int64 :count 2)	; file generation number
)

#+darwin
(defcstruct foreign-stat
  (st_dev	dev-t)			; device inode resides on
  (st_mode	mode-t)			; inode protection mode
  (st_nlink	nlink-t)		; number or hard links to the file
  (st_ino	ino-t)			; inode's number
  (st_uid	uid-t)			; user-id of owner 
  (st_gid	gid-t)			; group-id of owner
  (st_rdev	dev-t)			; device type, for special file inode
  (st_atimespec (:struct foreign-timespec)) ; time of last access
  (st_mtimespec (:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec (:struct foreign-timespec)) ; time of last file status change
  (st_birthtimespec (:struct foreign-timespec)) ; time of last file status change
  (st_size	off-t)			; file size, in bytes
  (st_blocks	blkcnt-t)		; blocks allocated for file
  (st_blksize	blksize-t)		; optimal file sys I/O ops blocksize
  (st_flags	:uint32)		; user defined flags for file
  (st_gen	:uint32)		; file generation number
  (st_lspare	:int32)			; unused
;  (st_qspare	:int64 :count 2)	; unused
  (st_qspare_1	:int64)			; unused
  (st_qspare_1	:int64)			; unused
)

;; 32bit stat -> __xstat -> fstatat64
;; 32bit ?    -> __xstat64 -> fstatat64

#+(and linux 32-bit-target (not cmu))
(defcstruct foreign-stat
  (st_dev	dev-t)			; ID of device containing file
  (__pad1	:unsigned-short)	;
  (st_ino	ino-t)			; 32 bit inode number **
  (st_mode	mode-t)			; protection
  (st_nlink	nlink-t)		; number of hard links
  (st_uid	uid-t)			; user ID of owner
  (st_gid	gid-t)			; group ID of owner
  (st_rdev	dev-t)			; device ID (if special file)
  (__pad2	:unsigned-short)	;
  (st_size	off-t)			; total size, in bytes **
  (st_blksize	blksize-t)		; blocksize for file system I/O
  (st_blocks	blkcnt-t)		; number of 512B blocks allocated **
  (st_atimespec	(:struct foreign-timespec)) ; time of last access
  (st_mtimespec	(:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec	(:struct foreign-timespec)) ; time of last file status change
  (__unused4	:unsigned-long)
  (__unused5	:unsigned-long))

#+(and linux 32-bit-target cmu) ;; @@@ fixme
(defcstruct foreign-stat
  (st_dev	dev-t)			; ID of device containing file
  (__pad1	:unsigned-short)	;
  (__st_ino	:uint64 #|ino-t|#)      ; not inode number **
  (st_mode	mode-t)			; protection
  (st_nlink	:uint32 #|nlink-t|#)		; number of hard links
  (st_uid	uid-t)			; user ID of owner
  (st_gid	gid-t)			; group ID of owner
  (st_rdev	:uint64 #|dev-t|#)	; device ID (if special file)
  (__pad2	:unsigned-short)	;
  (st_size	:uint32 #| off-t |#)    ; total size, in bytes **
  (st_blksize	blksize-t)		; blocksize for file system I/O
  (st_blocks	blkcnt-t)		; number of 512B blocks allocated **
  (st_atimespec	(:struct foreign-timespec)) ; time of last access
  (st_mtimespec	(:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec	(:struct foreign-timespec)) ; time of last file status change
  (st_ino	:uint64 #|ino-t|#)	; 64 bit inode number **
)

#+(and linux 64-bit-target some-version?)
(defcstruct foreign-stat
  (st_dev	dev-t)			; ID of device containing file
  (__pad1	:unsigned-short)	;
  (__st_ino	ino-t)			; NOT inode number **
  (st_mode	mode-t)			; protection
  (st_nlink	nlink-t)		; number of hard links
  (st_uid	uid-t)			; user ID of owner
  (st_gid	gid-t)			; group ID of owner
  (st_rdev	dev-t)			; device ID (if special file)
  (__pad2	:unsigned-short)	;
  (st_size	off-t)			; total size, in bytes **
  (st_blksize	blksize-t)		; blocksize for file system I/O
  (st_blocks	blkcnt-t)		; number of 512B blocks allocated **
  (st_atimespec	(:struct foreign-timespec)) ; time of last access
  (st_mtimespec	(:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec	(:struct foreign-timespec)) ; time of last file status change
  (st_ino	ino-t)			; 64 bit inode number **
)

#+(and linux 64-bit-target)
(defcstruct foreign-stat
  (st_dev	dev-t)			; ID of device containing file
  (st_ino	ino-t)			; NOT inode number **
  (st_nlink	nlink-t)		; number of hard links
  (st_mode	mode-t)			; protection
  (st_uid	uid-t)			; user ID of owner
  (st_gid	gid-t)			; group ID of owner
  (__pad0	:int)			;
  (st_rdev	dev-t)			; device ID (if special file)
  (st_size	off-t)			; total size, in bytes **
  (st_blksize	blksize-t)		; blocksize for file system I/O
  (st_blocks	blkcnt-t)		; number of 512B blocks allocated **
  (st_atimespec	(:struct foreign-timespec)) ; time of last access
  (st_mtimespec	(:struct foreign-timespec)) ; time of last data modification
  (st_ctimespec	(:struct foreign-timespec)) ; time of last file status change
  (__glibc_reserved :long :count 3)
)

#+(and freebsd 64-bit-target)
(defcstruct foreign-stat
  (st_dev 	dev-t)
  (st_ino 	ino-t)
  (st_mode 	mode-t)
  (st_nlink 	nlink-t)
  (st_uid 	uid-t)
  (st_gid 	gid-t)
  (st_rdev 	dev-t)
  (st_atimespec	(:struct foreign-timespec)) ;; st_atim
  (st_mtimespec	(:struct foreign-timespec)) ;; st_mtim
  (st_ctimespec	(:struct foreign-timespec)) ;; st_ctim
  (st_size	off-t)
  (st_blocks	blkcnt-t)
  (st_blksize	blksize-t)
  (st_flags	fflags-t)
  (st_gen	:uint32)
  (st_lspare	:int32)
  (st_birthtim  (:struct foreign-timespec))
  (junk		:uint8 :count 8))

;;  (unsigned int :(8 / 2) * (16 - (int)sizeof(struct timespec))
;;  (unsigned int :(8 / 2) * (16 - (int)sizeof(struct timespec))

;; This should have the union of all Unix-like OS's slots, so that Unix
;; portable code can check for specific slots with impunity.
(defstruct file-status
  device
  inode
  (mode 0 :type integer)
  links
  (uid -1 :type integer)
  (gid -1 :type integer)
  device-type
  access-time
  modify-time
  change-time
  birth-time
  size
  blocks
  block-size
  flags
  generation)

(defun convert-stat (stat-buf)
  (if (and (pointerp stat-buf) (null-pointer-p stat-buf))
      nil
      (with-foreign-slots
	  ((st_dev
	    st_ino
	    st_mode
	    st_nlink
	    st_uid
	    st_gid
	    st_rdev
	    st_atimespec
	    st_mtimespec
	    st_ctimespec
	    #+os-t-has-birthtime st_birthtimespec
	    st_size
	    st_blocks
	    st_blksize
	    #+darwin st_flags
	    #+darwin st_gen
	    ) stat-buf (:struct foreign-stat))
	   (make-file-status
	    :device st_dev
	    :inode st_ino
	    :mode st_mode
	    :links st_nlink
	    :uid st_uid
	    :gid st_gid
	    :device-type st_rdev
	    :access-time (convert-timespec st_atimespec)
	    :modify-time (convert-timespec st_mtimespec)
	    :change-time (convert-timespec st_ctimespec)
	    #+os-t-has-birthtime :birth-time
	    #+os-t-has-birthtime (convert-timespec st_birthtimespec)
	    :size st_size
	    :blocks st_blocks
	    :block-size st_blksize
	    #+darwin :flags #+darwin st_flags
	    #+darwin :generation #+darwin st_gen
	    ))))

;; Here's the real stat functions in glibc on linux:
;; GLIBC_2.2.5 __xstat
;; GLIBC_2.2.5 __xstat64
;; GLIBC_2.2.5 __fxstat
;; GLIBC_2.2.5 __fxstat64
;; GLIBC_2.2.5 __lxstat
;; GLIBC_2.2.5 __lxstat64
;; GLIBC_2.4   __fxstatat
;; GLIBC_2.4   __fxstatat64

#+(and linux (or sbcl #|cmu|#)) ;; I'm not really sure how this works.
(progn
  (defcfun ("stat" real-stat)
      :int (path :string) (buf (:pointer (:struct foreign-stat))))

  (defcfun ("lstat" real-lstat)
      :int (path :string) (buf (:pointer (:struct foreign-stat))))

  (defcfun ("fstat" real-fstat)
      :int (fd :int) (buf (:pointer (:struct foreign-stat)))))

(defparameter *stat-version*
  #+64-bit-target 0
  #+32-bit-target 3
  )

;; We have to do the wack crap.
#+(and linux (and (not sbcl) #|(not cmu)|#))
(progn
  (defcfun ("__xstat"  completely-fucking-bogus-but-actually-real-stat)
      :int (vers :int) (path :string) (buf (:pointer (:struct foreign-stat))))
  (defcfun ("__lxstat" completely-fucking-bogus-but-actually-real-lstat)
      :int (vers :int) (path :string) (buf (:pointer (:struct foreign-stat))))
  (defcfun ("__fxstat" completely-fucking-bogus-but-actually-real-fstat)
      :int (vers :int) (fd :int) (buf (:pointer (:struct foreign-stat))))
  (defun real-stat (path buf)
    (completely-fucking-bogus-but-actually-real-stat  *stat-version* path buf))
  (defun real-lstat (path buf)
    (completely-fucking-bogus-but-actually-real-lstat *stat-version* path buf))
  (defun real-fstat (path buf)
    (completely-fucking-bogus-but-actually-real-fstat *stat-version* path buf)))

#-linux ;; so mostly BSDs
(progn
  (defcfun
    (#+darwin "stat$INODE64"
     #-darwin "stat"
     real-stat)
    :int (path :string) (buf (:pointer (:struct foreign-stat))))

  (defcfun
    (#+darwin "lstat$INODE64"
     #-darwin "lstat"
     real-lstat)
    :int (path :string) (buf (:pointer (:struct foreign-stat))))

  (defcfun
    (#+darwin "fstat$INODE64"
     #-darwin "fstat"
     real-fstat)
    :int (fd :int) (buf (:pointer (:struct foreign-stat)))))

(defun stat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-stat path stat-buf) "stat: ~s" path)
    (convert-stat stat-buf)))

(defun lstat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-lstat path stat-buf) "lstat: ~s" path)
    (convert-stat stat-buf)))

(defun fstat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-fstat path stat-buf) "fstat: ~s" path)
    (convert-stat stat-buf)))

(defvar *statbuf* nil
  "Just some space to put file status in. It's just to make file-exists, 
quicker. We don't care what's in it.")

;; Sadly I find the need to do this because probe-file might be losing.
(defun file-exists (filename)
  "Check that a file with FILENAME exists at the moment. But it might not exist
for long."
  ;; (when (not (stringp (setf filename (safe-namestring filename))))
  ;;   (error "FILENAME should be a string or pathname."))
  (when (not *statbuf*)
    (setf *statbuf* (foreign-alloc '(:struct foreign-stat))))
  (= 0 (real-stat (safe-namestring filename) *statbuf*)))

(defcfun ("readlink" real-readlink) ssize-t (path :string)
	 (buf (:pointer :char)) (bufsize size-t))

(defun readlink (filename)
  "Return the name which the symbolic link FILENAME points to. Return NIL if
it is not a symbolic link."
  (with-foreign-pointer (buf (get-path-max))
    (let ((result (real-readlink filename buf (get-path-max))))
      (if (> result 0)
	  (subseq (foreign-string-to-lisp buf) 0 result)
	  (let ((err *errno*))		; in case there are hidden syscalls
	    (if (= err +EINVAL+)
		nil
		(error 'posix-error :error-code err
		       :format-control "readlink:")))))))

(defun timespec-to-derptime (ts)
  "Convert a timespec to a derptime."
  (make-derp-time
   :seconds (unix-to-universal-time (getf ts 'tv_sec))
   :nanoseconds (getf ts 'tv_nsec)))

(defun convert-file-info (stat-buf)
  (if (and (pointerp stat-buf) (null-pointer-p stat-buf))
      nil
      (with-foreign-slots
	  ((st_mode
	    st_atimespec
	    st_mtimespec
	    st_ctimespec
	    #+os-t-has-birthtime st_birthtimespec
	    st_size
	    #+darwin st_flags
	    ) stat-buf (:struct foreign-stat))
	(make-file-info
	 :type (cond
		 ;; We should have this be the same as DIRENT-TYPE
		 ((is-directory st_mode)		:directory)
		 ((is-symbolic-link st_mode)		:link)
		 ((or (is-character-device st_mode)
		      (is-block-device st_mode)) 	:device)
		 ((is-regular-file st_mode) 		:regular)
		 (t					:other))
	 :size st_size
	 :creation-time
	 ;; perhaps should be the earliest of st_ctimespec and st_birthtimespec?
	 (timespec-to-derptime
	  #+os-t-has-birthtime st_birthtimespec
	  #-os-t-has-birthtime st_mtimespec)
	 :access-time (timespec-to-derptime st_atimespec)
	 :modification-time
	 ;; perhaps should be the latest of st_ctimespec and st_mtimespec?
	 (timespec-to-derptime st_ctimespec)
	 :flags
	 ;; :hidden :immutable :compressed
	 `(
	   #+darwin ,@(and (or (flag-user-immutable st_flags)
			       (flag-root-immutable st_flags))
			   (list :immutable))
	   #+darwin ,@(and (flag-user-compressed st_flags)
			   (list :compressed))
	   #+darwin ,@(and (flag-user-hidden st_flags)
			   (list :hidden))
	   ;; linux ext flags are so lame I can't be bothered to do them now.
	   )))))

(defun get-file-info (path &key (follow-links t))
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (if follow-links
		     (real-stat path stat-buf)
		     (real-lstat path stat-buf)) "get-file-info: ~s" path)
    (convert-file-info stat-buf)))

;; Supposedly never fails so we don't have to wrap with syscall.
;; @@@ consider taking symbolic string arguments
(defcfun umask mode-t (cmask mode-t))

(defcfun ("chmod" real-chmod) :int (path :string) (mode mode-t))
(defun chmod (path mode)
  "Change the mode (a.k.a. permission bits) of a file."
  ;; @@@ take the symbolic mode forms when we're done with the above
  (syscall (real-chmod path mode)))

(defcfun ("fchmod" real-fchmod) :int (fd :int) (mode mode-t))
(defun fchmod (fd mode)
  "Change the mode (a.k.a. permission bits) of a file."
  ;; @@@ take the symbolic mode forms when we're done with the above
  (syscall (real-fchmod fd mode)))

(defcfun ("chown" real-chown) :int (path :string) (owner uid-t) (group gid-t))
(defun chown (path owner group)
  "Change the owner and group of a file."
  ;; @@@ take string owner and group and convert to numeric
  (syscall (real-chown path owner group)))

(defcfun ("fchown" real-fchown) :int (fd :int) (owner uid-t) (group gid-t))
(defun fchown (fd owner group)
  "Change the owner and group of a file given a file descriptor."
  ;; @@@ take string owner and group and convert to numeric
  (syscall (real-fchown fd owner group)))

(defcfun ("lchown" real-lchown)
    :int (path :string) (owner uid-t) (group gid-t))
(defun lchown (path owner group)
  "Change the owner and group of a symbolic link (not what it points to)."
  ;; @@@ take string owner and group and convert to numeric
  (syscall (real-lchown path owner group)))

;; This is sadly still actually useful.
(defcfun sync :void)

(defun probe-directory (dir)
  "Something like probe-file but for directories."
  ;; #+clisp (ext:probe-directory (make-pathname
  ;; 				:directory (ext:absolute-pathname dir)))
  #+(or sbcl ccl cmu clisp ecl)
  ;; Let's be more specific: it must be a directory.
  (handler-case
    (let ((s (stat dir)))
      (and (is-directory (file-status-mode s))))
    (posix-error (c)
      (when (not (find (opsys-error-code c) `(,+ENOENT+ ,+EACCES+ ,+ENOTDIR+)))
	(signal c))))
  #+(or lispworks abcl)
  ;; On some implementations probe-file can handle directories the way I want.
  (probe-file dir)
  #-(or clisp sbcl ccl cmu ecl lispworks abcl)
  (declare (ignore dir))
  #-(or clisp sbcl ccl cmu ecl lispworks abcl)
  (missing-implementation 'probe-directory))

;; Questionable:
;; mmap/munmap/mprotect/madvise ???
;; File locking? : fcntl F_GETLK / F_GETLK F_SETLKW

(defcfun ("utimensat" real-utimensat) :int
  (dirfd :int) (pathname :string)
  (times (:pointer (:struct foreign-timespec))) ; struct timespec times[2]
  (flags :int))

#|
(defun set-file-time (path &key seconds nanoseconds)
  (let (dir-fd
	
    (unwind-protect
      (setf dir-fd (posix-open 
  (syscall (real-utimensat
  )
|#


;; Apple metadata crap:
;; searchfs
;; getdirentriesattr
;;
;; Look into file metadata libraries? which will work on windows, etc..


;; OSX extended attributes

(defconstant +XATTR_NOFOLLOW+		#x0001)
(defconstant +XATTR_CREATE+		#x0002)
(defconstant +XATTR_REPLACE+		#x0004)
(defconstant +XATTR_NOSECURITY+		#x0008)
(defconstant +XATTR_NODEFAULT+		#x0010)
(defconstant +XATTR_SHOWCOMPRESSION+	#x0020)
(defconstant +XATTR_MAXNAMELEN+		127)

;; @@@ Maybe these *are* on linux?
#+darwin
(progn
  (defcfun listxattr ssize-t (path :string) (namebuff :string) (size size-t)
	   (options :int))
  (defcfun flistxattr ssize-t (fd :int) (namebuff :string) (size size-t)
	   (options :int))
  (defcfun getxattr ssize-t (path :string) (name :string) (value :pointer)
	   (size size-t) (position :uint32) (options :int))
  (defcfun fgetxattr ssize-t (fd :int) (name :string) (value :pointer)
	   (size size-t) (position :uint32) (options :int))
  (defcfun setxattr :int (path :string) (name :string) (value :pointer)
	   (size size-t) (position :uint32) (options :int))
  (defcfun fsetxattr :int (fd :int) (name :string) (value :pointer)
	   (size size-t) (position :uint32) (options :int))
  (defcfun removexattr :int (path :string) (name :string) (options :int))
  (defcfun fremovexattr :int (fd :int) (name :string) (options :int)))

;; These are defined, but just don't return anything on non-Darwin.

(defun extended-attribute-list (path)
  #+darwin
  (let ((size (listxattr path (null-pointer) 0 0))
	names)
    (with-foreign-object (f-names :char size)
      (syscall (listxattr path f-names size +XATTR_SHOWCOMPRESSION+))
      (setf names (foreign-string-to-lisp f-names :count size))
      (loop :with i = 0 :and end
	 :while (< i size)
	 :do
	 (setf end (position (code-char 0) names :start i))
	 :when (and end (< (+ i end) size))
	 :collect (subseq names i end)
	 :do (incf i end))))
  #-darwin (declare (ignore path))
  #-darwin '())

(defun extended-attribute-value (path name)
  #+darwin
  (let ((size (getxattr path name (null-pointer) 0 0 0)))
    (with-foreign-object (value :char size)
      (syscall (getxattr path name value size 0 +XATTR_SHOWCOMPRESSION+))
      value))
  #-darwin (declare (ignore path name))
  #-darwin nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

#+sunos (defcvar ("_sys_siglistn" *nsig*) :int)
#+sunos (defcvar ("_sys_siglistp" sys-siglist) :pointer)

#+(or darwin freebsd) (defcvar ("sys_siglist" sys-siglist) :pointer)
#+(or darwin freebsd) (defcvar ("sys_signame" sys-signame) :pointer)

(defparameter *signal-count*
  #+(or darwin freebsd linux) 32
  ;; actually 65 if you count realtime (RT) signals
  #+sunos *nsig*
  #-(or darwin sunos linux freebsd) (missing-implementation) ; @@@ or perhaps 0?
  "Number of signal types, a.k.a. NSIG."
)

(defconstant +SIGHUP+	 1			  "Hangup")
(defconstant +SIGINT+	 2			  "Interrupt")
(defconstant +SIGQUIT+	 3			  "Quit")
(defconstant +SIGILL+	 4			  "Illegal instruction")
(defconstant +SIGTRAP+	 5			  "Trace/BPT trap")
(defconstant +SIGABRT+	 6			  "Abort trap")
(defconstant +SIGPOLL+	 #+darwin 7 #+linux 29 #+freebsd nil "pollable event")
(defconstant +SIGEMT+	 #+(or darwin freebsd) 7 #+linux nil "EMT trap")
(defconstant +SIGFPE+	 8			  "Floating point exception")
(defconstant +SIGKILL+	 9			  "Killed")
(defconstant +SIGBUS+	 #+(or darwin freebsd) 10 #+linux 7 "Bus error")
(defconstant +SIGSEGV+	 11			  "Segmentation fault")
(defconstant +SIGSYS+	 #+darwin 12 #+linux 31	#+freebsd nil "Bad system call")
(defconstant +SIGPIPE+	 13			  "Broken pipe")
(defconstant +SIGALRM+	 14			  "Alarm clock")
(defconstant +SIGTERM+	 15			  "Terminated")
(defconstant +SIGURG+	 #+(or darwin freebsd) 16 #+linux 23 "Urgent I/O condition")
(defconstant +SIGSTOP+	 #+(or darwin freebsd) 17 #+linux 19 "Suspended (signal)")
(defconstant +SIGTSTP+	 #+(or darwin freebsd) 18 #+linux 20 "Suspended")
(defconstant +SIGCONT+	 #+(or darwin freebsd) 19 #+linux 18 "Continued")
(defconstant +SIGCHLD+	 #+(or darwin freebsd) 20 #+linux 17 "Child exited")
(defconstant +SIGTTIN+	 21			  "Stopped (tty input)")
(defconstant +SIGTTOU+	 22			  "Stopped (tty output)")
(defconstant +SIGIO+	 #+(or darwin freebsd) 23 #+linux 29 "I/O possible")
(defconstant +SIGXCPU+	 24			  "Cputime limit exceeded")
(defconstant +SIGXFSZ+	 25			  "Filesize limit exceeded")
(defconstant +SIGVTALRM+ 26			  "Virtual timer expired")
(defconstant +SIGPROF+	 27			  "Profiling timer expired")
(defconstant +SIGWINCH+	 28			  "Window size changes")
(defconstant +SIGINFO+	 #+(or darwin freebsd) 29 #+linux nil "Information request")
(defconstant +SIGUSR1+	 #+(or darwin freebsd) 30 #+linux 10	  "User defined signal 1")
(defconstant +SIGUSR2+	 #+(or darwin freebsd) 31 #+linux 12	  "User defined signal 2")
(defconstant +SIGSTKFLT+ #+(or darwin freebsd) nil #+linux 16  "Stack fault")
(defconstant +SIGPWR+	 #+(or darwin freebsd) nil #+linux 30  "Power failure restart")
#+freebsd
(progn
  (defconstant +SIGTHR+	  32 "thread library")
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
  #+(or darwin freebsd)
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-signame) :pointer sig)))
  #+linux (when (< sig *signal-count*)
	    (aref *signal-name* sig))
  #-(or darwin sunos linux freebsd) (declare (ignore sig))
  #-(or darwin sunos linux freebsd) (missing-implementation 'signal-name)
)

#+(or sunos linux) (defcfun strsignal :string (sig :int))

(defun signal-description (sig)
  #+(or sunos linux) (strsignal sig)
  #+darwin
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-siglist) :pointer sig)))
  #-(or darwin sunos linux) (declare (ignore sig))
  #-(or darwin sunos linux) (missing-implementation 'signal-description)
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

#+(or darwin linux)
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
(defconstant SIG_HOLD #+darwin 5 #+linux 2 #+freebsd 2
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
     
(defmacro with-signal-handlers (handler-list &body body)
  "Evaluate the BODY with the signal handlers set as in HANDLER-LIST, with the
handers restored to their orignal values on return. HANDLER-LIST is a list
of (signal . action), as would be passed to SET-SIGNAL-ACTION."
  (with-unique-names (saved-list evaled-list)
    `(let* ((,evaled-list
	     (mapcar (_ (cons (typecase (car _)
				(symbol (symbol-value (car _)))
				(t (car _))) ; it had better be a signal number
			      (cdr _)))
		     ',handler-list))
	    (,saved-list
	     (loop
		;;:for (sig . act) :in ,evaled-list
		:for item :in ,evaled-list
		;;:do
		;;(format t "sig = ~s~%" sig)
		;;(format t "act = ~a~%" (signal-action sig))
		;;:collect (cons sig (signal-action sig)))))
		:collect (cons (car item) (signal-action (car item))))))
       (unwind-protect
         (progn
	   (set-handlers ,evaled-list)
	   ,@body)
	 (when ,saved-list
	   (set-handlers ,saved-list))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

;#+(or darwin linux) (config-feature :os-t-has-vfork)

#+(or darwin linux freebsd)
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
		 (error-check wait-pid "wait-pid"))
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
	(error-check wait-pid "wait-pid"))
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
	(error-check child-pid "child-pid")
	(wait-and-report child-pid)))))

(defcfun getuid uid-t)
(defcfun getgid uid-t)
(defcfun geteuid uid-t)
(defcfun getegid uid-t)
(defcfun setuid :int (uid uid-t))
(defcfun setgid :int (gid uid-t))
(defcfun seteuid :int (uid uid-t))
(defcfun setegid :int (gid uid-t))
(defcfun getpid pid-t)
(defcfun getppid pid-t)

;; Just in case you didn't know, or forgot, here's a little background
;; these rather obscure system calls. The man pages don't really explain it.
;;
;; This is what you do in a job control shell to boss around a bunch of
;; processes, in foreground, background ^Z and all that.

(defcfun setpgid :int (pid pid-t) (pgid pid-t))
(defcfun getpgid :int (pid pid-t))

;; The tty also stores the process group to know who to send job control
;; signals to.
(defcfun tcsetpgrp :int (fd :int) (pgid pid-t))
(defcfun tcgetpgrp pid-t (fd :int))

;; These are used when you are making a new terminal (or session), and want to
;; be in control of it, like in a terminal window (xterm) with ptys or with
;; real terminal devices in the old fashioned getty. Also good for detaching.
(defcfun setsid :int)
(defcfun getsid :int (pid pid-t))

;; Perhaps we should provide something high level like "run in pty" and
;; or "detach process".

;; int getgroups(int gidsetsize, gid_t grouplist[]);
(defcfun getgroups :int (gid-set-size :int) (group-list (:pointer gid-t)))

;; getgroups vs. getgrouplist & NGROUPS_MAX .etc
;; I should not be suprised at this point at how un-good the typical unix
;; group interfaces are.

(defun get-groups ()
  "Return an array of group IDs for the current process."
  (let* ((size (syscall (getgroups 0 (null-pointer))))
	 (result (make-array `(,size) :element-type 'fixnum)))
    (with-foreign-object (group-list 'gid-t size)
      (syscall (getgroups size group-list))
      (loop :for i :from 0 :below size
	 :do (setf (aref result i) 
		   (mem-aref group-list 'gid-t i))))
    result))

;; setgroups?

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

(defun process-list ()
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
		 (make-os-process
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
		  :args nil)))))
	(foreign-free proc-list))))
  #+linux
  (let (proc line pid raw-line open-pos close-pos cmd uid)
    (labels ((pos (p)
	       "Adusted element in stat line so we can use documented indices."
	       (elt line (- p 2)))
	     (read-proc (p)
	       (with-open-file (stm (s+ "/proc/" p "/stat"))
		 (setf raw-line (read-line stm)
		       open-pos (position #\( raw-line)
		       close-pos (position #\) raw-line :from-end t)
		       line (split-sequence
			     #\space (subseq raw-line (+ 2 close-pos)))
		       cmd (subseq raw-line (position #\( raw-line) close-pos)
		       pid (parse-integer p)
		       uid (file-status-uid (stat (s+ "/proc/" p))))
		 (make-os-process
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
      (loop :for p :in (read-directory :dir "/proc/")
	 :when (every #'digit-char-p p)
	 :if (setf proc
		   (handler-case
		       (read-proc p)
		     (file-error () nil)))
	 :collect proc))))

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
;; System Commands?

(defun member-of (group)
  "Return true if the current user is a member of GROUP."
  (position group (get-groups)))

(defun is-executable (path &key user regular)
  "Return true if the PATH is executable by the UID. UID defaults to the
current effective user."
  (let ((s (stat path)))
    (and
     (or
      (is-other-executable (file-status-mode s))
      (and (is-user-executable (file-status-mode s))
	   (= (file-status-uid s) (or user (setf user (geteuid)))))
      (and (is-group-executable (file-status-mode s))
	   (member-of (file-status-gid s))))
     (or (not regular)
	 (is-regular-file (file-status-mode s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application paths

(defun expand-leading-tilde (filename)
  "Return FILENAME with a leading tilde converted into the users home directory."
  (if (and filename (stringp filename) (not (zerop (length filename)))
	   (char= (char filename 0) #\~))
      (s+ (or (environment-variable "HOME") (user-home))
	  "/" (subseq filename 2))	; XXX wrongish
      filename))

;; This is mostly from:
;;   https://specifications.freedesktop.org/basedir-spec/latest/

#+(or linux sunos freebsd) ;; I'm not sure about sunos and freebsd.
(progn
  (defun xdg-thing (env-var default)
    "Return the the ENV-VAR or if it's not set or empty then the DEFAULT."
    (let ((result (or (let ((e (environment-variable env-var)))
			(and e (not (zerop (length e))) e))
		      default)))
      ;; It might be nice if we could use glob:expand-tilde, but we can't.
      ;; I supposed we could move it here though.
      (expand-leading-tilde result)))

  (defun xdg-app-dir (env-var default &optional app-name)
    "Return the the ENV-VAR or if it's not set or empty then the DEFAULT.
If APP-NAME is given, append that."
    (let ((result (xdg-thing env-var default)))
      (or (and app-name (s+ result "/" app-name))
	  result)))

  (defun xdg-path (env-var default &optional app-name)
    "Return the ENV-VAR or DEFAULT path as a list, possibily with app-name
appended to each element."
    (let ((result (split-sequence #\: (xdg-thing env-var default))))
      (or (and app-name (mapcar (_ (s+ _ "/" app-name)) result))
	  result)))

  (defparameter *default-data-dir* "~/.local/share")
  (defparameter *data-dir-env-var* "XDG_DATA_HOME")
  (defun data-dir (&optional app-name)
    "Where user specific data files should be stored."
    (xdg-app-dir *data-dir-env-var* *default-data-dir* app-name))

  (defparameter *default-config-dir* "~/.config")
  (defparameter *config-dir-env-var* "XDG_CONFIG_HOME")
  (defun config-dir (&optional app-name)
    "Where user specific configuration files should be stored."
    (xdg-app-dir *config-dir-env-var* *default-config-dir* app-name))

  (defparameter *default-data-path* "/usr/local/share/:/usr/share/")
  (defparameter *data-path-env-var* "XDG_DATA_DIRS")
  (defun data-path (&optional app-name)
    "Search path for user specific data files."
    (cons (data-dir app-name)
	  (xdg-path *data-path-env-var* *default-data-path* app-name)))

  (defparameter *default-config-path* "/etc/xdg")
  (defparameter *config-path-env-var* "XDG_CONFIG_DIRS")
  (defun config-path (&optional app-name)
    "Search path for user specific configuration files."
    (cons (config-dir app-name)
	  (xdg-path *config-path-env-var* *default-config-path* app-name)))

  (defparameter *default-cache-dir* "~/.cache")
  (defparameter *cache-dir-env-var* "XDG_CACHE_HOME")
  (defun cache-dir (&optional app-name)
    "Directory where user specific non-essential data files should be stored."
    (xdg-app-dir *cache-dir-env-var* *default-cache-dir* app-name))

  ;; Runtime dir has a lot of special restrictions. See the XDG spec.
  (defparameter *default-runtime-dir* "/run/user")
  (defparameter *runtime-dir-env-var* "XDG_RUNTIME_DIR")
  (defun runtime-dir (&optional app-name)
    "Directory where user-specific non-essential runtime files and other file
objects should be stored."
    (xdg-app-dir *runtime-dir-env-var*
		 (s+ *default-runtime-dir* #\/ (getuid)) app-name)))

#+darwin
(progn
  ;; @@@ I know this is all wrong.
  
  (defparameter *default-app* "Lisp")
  (defun data-dir (&optional app-name)
    "Where user specific data files should be stored."
    (declare (ignore app-name))
    (expand-leading-tilde "~/Documents")) ;; @@@ or translation
  
  (defun config-dir (&optional app-name)
    "Where user specific configuration files should be stored."
    (s+ (expand-leading-tilde "~/Library/Application Support")
	"/" (or app-name *default-app*)))

  (defun data-path (&optional app-name)
    "Search path for user specific data files."
    (list (data-dir app-name)))

  (defun config-path (&optional app-name)
    "Search path for user specific configuration files."
    (list (config-dir app-name)))

  (defun cache-dir (&optional app-name)
    "Directory where user specific non-essential data files should be stored."
    (s+ (expand-leading-tilde "~/Library/Caches") "/"
	(or app-name *default-app*)))

  (defun runtime-dir (&optional app-name)
    "Directory where user-specific non-essential runtime files and other file
objects should be stored."
    ;; @@@ This is totally wrong. I know there's some long number in here.
    (s+ "/var/run" "/" (getuid) "/" app-name)))

;; I feel like I'm already in the past.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

(defcstruct foreign-timezone
   (tz_minuteswest :int)    ; of Greenwich
   (tz_dsttime     :int))   ; type of DST correction to apply

(defcfun ("gettimeofday" real-gettimeofday) :int
  (tv (:pointer (:struct foreign-timeval)))
  (tz (:pointer (:struct foreign-timezone))))

(defcfun ("settimeofday" real-settimeofday) :int
  (tv (:pointer (:struct foreign-timeval)))
  (tz (:pointer (:struct foreign-timezone))))

(defun gettimeofday ()
  "Return a timeval structure with the Unix time of day."
  (with-foreign-object (tv '(:struct foreign-timeval))
    (syscall (real-gettimeofday tv (null-pointer)))
    (convert-timeval tv)))

(defun settimeofday (timeval)
  "Set the Unix time of day."
  (with-foreign-object (tv '(:struct foreign-timeval))
    (setf 
     (foreign-slot-value tv '(:struct foreign-timeval) 'tv_sec)
     (timeval-seconds timeval)
     (foreign-slot-value tv '(:struct foreign-timeval) 'tv_usec)
     (timeval-micro-seconds timeval))
    (syscall (real-settimeofday tv (null-pointer)))))

(defconstant +unix-to-universal-time+ 2208988800
  "Value to add to traditional 1970 based Unix time, to get a Common Lisp
universal time.")

(defun unix-to-universal-time (unix-time)
  "Return the Common Lisp universal time given a traditional 1970 based
Unix time integer."
  (+ +unix-to-universal-time+ unix-time))

(defun universal-to-unix-time (universal-time)
  "Return the traditional 1970 based Unix time given a Common Lisp universal
time."
  (- universal-time +unix-to-universal-time+))

(defun get-time ()
  "Return the time in seconds and nanoseconds. Seconds are in so-called
“universal” time."
  (with-foreign-object (tv '(:struct foreign-timeval))
    (syscall (real-gettimeofday tv (null-pointer)))
    (values
     (unix-to-universal-time
      (foreign-slot-value tv '(:struct foreign-timeval) 'tv_sec))
     (* 1000
	(foreign-slot-value tv '(:struct foreign-timeval) 'tv_usec)))))

(defun set-time (seconds nanoseconds)
  "Set time in seconds and nanoseconds. Seconds are in so-called
“universal” time."
  (with-foreign-object (tv '(:struct foreign-timeval))
    (setf (foreign-slot-value tv '(:struct foreign-timeval) 'tv_usec)
	  (truncate (/ nanoseconds 1000))
	  (foreign-slot-value tv '(:struct foreign-timeval) 'tv_sec)
	  (universal-to-unix-time seconds))
    (syscall (real-settimeofday tv (null-pointer)))))

;; setitimer/getitimer
;;
;; Interval timers ared probably best provided in relation to some kind of
;; "event loop"

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
      (when (= -1 (setf ret-val (unix-select (1+ nfds)
					     read-fds write-fds err-fds
					     tv)))
	(error 'posix-error :error-code *errno*
	       :format-control "Select failed ~a"))
;      (format t "return = ~d~%" ret-val)
      (when (not (= 0 ret-val))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread-like

;; @@@ Just use bordeaux-threads!
;; locks (mutexes)
;; create thread
;; join thread

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System administration???
;; is this even a good idea

;; reboot
;; swapon
;; mincore
;; acct
;; settimeofday
;; adjtime

;; Filesystems:
;; mount/unmount
;; quotactl
;; fsstat?

;; statfs

#+(and darwin (not 64-bit-target))
(eval-when (:compile-toplevel :load-toplevel :execute)
   (define-constant +MFSNAMELEN+ 15)	; length of fs type name, not inc. nul
   (define-constant +MNAMELEN+ 90)	; length of buffer for returned name
   (define-constant +MFSTYPENAMELEN+ +MFSNAMELEN+)
   (define-constant +MAXPATHLEN+ +MNAMELEN+)
)

;; when _DARWIN_FEATURE_64_BIT_INODE is NOT defined
#+(and darwin (not 64-bit-target))
(defcstruct foreign-statfs
  (f_otype	 :short)          ; type of file system (reserved: zero)
  (f_oflags	 :short)	  ; copy of mount flags (reserved: zero)
  (f_bsize	 :long)		  ; fundamental file system block size
  (f_iosize	 :long)		  ; optimal transfer block size
  (f_blocks	 :long)		  ; total data blocks in file system
  (f_bfree	 :long)		  ; free blocks in fs
  (f_bavail	 :long)		  ; free blocks avail to non-superuser
  (f_files	 :long)		  ; total file nodes in file system
  (f_ffree	 :long)		  ; free file nodes in fs
;  (f_fsid fsid_t)		  ; file system id
  (f_fsid	 :int32 :count 2) ; file system id
  (f_owner uid-t)		  ; user that mounted the file system
  (f_reserved1	 :short)	  ; reserved for future use
  (f_type	 :short)	  ; type of file system (reserved)
  (f_flags	 :long)		  ; copy of mount flags (reserved)
  (f_reserved2	 :long :count 2)  ; reserved for future use
  (f_fstypename	 :char :count #.+MFSNAMELEN+) ; fs type name
  (f_mntonname	 :char :count #.+MNAMELEN+)   ; directory on which mounted
  (f_mntfromname :char :count #.+MNAMELEN+)   ; mounted file system
  (f_reserved3	 :char)		  ; reserved for future use
  (f_reserved4	 :long :count 4)  ; reserved for future use
  )

#+(and darwin 64-bit-target)
(eval-when (:compile-toplevel :load-toplevel :execute)
   (define-constant +MFSTYPENAMELEN+ 16); length of fs type name, including nul
   (define-constant +MAXPATHLEN+ 1024)	; length of buffer for returned name
)

#+(and darwin 64-bit-target)
;; when _DARWIN_FEATURE_64_BIT_INODE *is* defined
(defcstruct foreign-statfs
  (f_bsize       :uint32)		; fundamental file system block size
  (f_iosize	 :int32)		; optimal transfer block size
  (f_blocks	 :uint64)		; total data blocks in file system
  (f_bfree	 :uint64)		; free blocks in fs
  (f_bavail	 :uint64)		; free blocks avail to non-superuser
  (f_files	 :uint64)		; total file nodes in file system
  (f_ffree	 :uint64)		; free file nodes in fs
;  (f_fsid fsid_t)			; file system id
  (f_fsid	 :int32  :count 2)	; file system id
  (f_owner       uid-t)			; user that mounted the file system
  (f_type        :uint32)		; type of file system
  (f_flags       :uint32)		; copy of mount flags
  (f_fssubtype   :uint32)		; fs sub-type (flavor)
  (f_fstypename  :char   :count #.+MFSTYPENAMELEN+) ; fs type name
  (f_mntonname   :char   :count #.+MAXPATHLEN+)	    ; directory on which mounted
  (f_mntfromname :char   :count #.+MAXPATHLEN+)	    ; mounted file system
  (f_reserved4   :uint32 :count 8)      ; reserved for future use
  )

#+darwin
(defstruct statfs
  "File system statistics."
  bsize
  iosize
  blocks
  bfree
  bavail
  files
  ffree
  fsid
  owner
  type
  flags
  fssubtype
  fstypename
  mntonname
  mntfromname)

;; @@@ I shouldn't really have to do this?
#+darwin
(defun convert-statfs (statfs)
  (if (and (pointerp statfs) (null-pointer-p statfs))
      nil
      (with-foreign-slots ((f_bsize
			    f_iosize
			    f_blocks
			    f_bfree
			    f_bavail
			    f_files
			    f_ffree
			    f_fsid
			    f_owner
			    f_type
			    f_flags
			    #+64-bit-target f_fssubtype
			    f_fstypename
			    f_mntonname
			    f_mntfromname) statfs (:struct foreign-statfs))
	(make-statfs
	 :bsize f_bsize
	 :iosize f_iosize
	 :blocks f_blocks
	 :bfree f_bfree
	 :bavail f_bavail
	 :files f_files
	 :ffree f_ffree
	 :fsid (vector (mem-aref f_fsid :int32 0) (mem-aref f_fsid :int32 1))
	 :owner f_owner
	 :type f_type
	 :flags f_flags
	 #+64-bit-target :fssubtype #+64-bit-target f_fssubtype
	 :fstypename (foreign-string-to-lisp f_fstypename
					     :max-chars +MFSTYPENAMELEN+)
	 :mntonname (foreign-string-to-lisp f_mntonname
					     :max-chars +MAXPATHLEN+)
	 :mntfromname (foreign-string-to-lisp f_mntfromname
					     :max-chars +MAXPATHLEN+)))))

#+darwin
(defun convert-filesystem-info (statfs)
  (if (and (pointerp statfs) (null-pointer-p statfs))
      nil
      (with-foreign-slots ((f_bsize
			    f_iosize
			    f_blocks
			    f_bfree
			    f_bavail
			    f_files
			    f_ffree
			    f_fsid
			    f_owner
			    f_type
			    f_flags
			    #+64-bit-target f_fssubtype
			    f_fstypename
			    f_mntonname
			    f_mntfromname) statfs (:struct foreign-statfs))
	(make-filesystem-info
	 :device-name (foreign-string-to-lisp f_mntfromname
					      :max-chars +MAXPATHLEN+)
	 :mount-point (foreign-string-to-lisp f_mntonname
					      :max-chars +MAXPATHLEN+)
	 :type (foreign-string-to-lisp f_fstypename
				       :max-chars +MFSTYPENAMELEN+)
	 :total-bytes (* f_blocks f_bsize)
	 :bytes-free (* f_bfree f_bsize)
	 :bytes-available (* f_bavail f_bsize)))))

;; @@@ 32 bit only?
;(defctype fsblkcnt-t :unsigned-long)
;(defctype fsword-t :int)

#+(and linux 32-bit-target)
(defcstruct foreign-statfs
  (f_type    fsword-t)
  (f_bsize   fsword-t)
  (f_blocks  fsblkcnt-t)
  (f_bfree   fsblkcnt-t)
  (f_bavail  fsblkcnt-t)
  (f_files   fsblkcnt-t)
  (f_ffree   fsblkcnt-t)
  (f_fsid    fsword-t :count 2)
  (f_namelen fsword-t)
  (f_frsize  fsword-t)
  (f_flags   fsword-t)
  (f_spare   fsword-t :count 4))

#+(and linux 64-bit-target)
(defcstruct foreign-statfs
  (f_type    :int64)
  (f_bsize   :int64)
  (f_blocks  :uint64)
  (f_bfree   :uint64)
  (f_bavail  :uint64)
  (f_files   :uint64)
  (f_ffree   :uint64)
  (f_fsid    :int32 :count 2)
  (f_namelen :int64)
  (f_frsize  :int64)
  (f_flags   :int64)
  (f_spare   :int64 :count 4))

#+linux
(defstruct statfs
  "File system statistics."
  type
  bsize
  blocks
  bfree
  bavail
  files
  ffree
  fsid
  namelen
  frsize
  flags
  spare)

;; @@@ Should I really have to do this?
#+linux
(defun convert-statfs (statfs)
  (if (and (pointerp statfs) (null-pointer-p statfs))
      nil
      (with-foreign-slots ((f_type
			    f_bsize
			    f_blocks
			    f_bfree
			    f_bavail
			    f_files
			    f_ffree
			    f_fsid
			    f_namelen
			    f_frsize
			    f_flags
			    f_spare) statfs (:struct foreign-statfs))
	(make-statfs
         :type	  f_type
         :bsize	  f_bsize
         :blocks  f_blocks
         :bfree	  f_bfree
         :bavail  f_bavail
         :files	  f_files
         :ffree	  f_ffree
         :fsid	  (vector (mem-aref f_fsid :int32 0) (mem-aref f_fsid :int32 1))
         :namelen f_namelen
         :frsize  f_frsize
         :flags	  f_flags
         :spare	  f_spare))))

;; (define-foreign-type foreign-statfs-type ()
;;   ()
;;   (:actual-type :pointer)
;;   (:simple-parser foreign-statfs)
;; )

#+freebsd
(eval-when (:compile-toplevel :load-toplevel :execute)
   (define-constant +MFSNAMELEN+ 16)   ; length of fs type name including null
   (define-constant +MNAMELEN+ 88)     ; size of on/from name bufs
   (define-constant +STATFS_VERSION+ #x20030518) ; version of this struct?
   )

#+freebsd
(defcstruct foreign-statfs
  (f_version 	 :uint32)
  (f_type 	 :uint32)
  (f_flags 	 :uint64)
  (f_bsize 	 :uint64)
  (f_iosize 	 :uint64)
  (f_blocks 	 :uint64)
  (f_bfree 	 :uint64)
  (f_bavail 	 :int64)
  (f_files 	 :uint64)
  (f_ffree 	 :int64)
  (f_syncwrites  :uint64)
  (f_asyncwrites :uint64)
  (f_syncreads   :uint64)
  (f_asyncreads  :uint64)
  (f_spare       :uint64 :count 10)
  (f_namemax 	 :uint32)
  (f_owner 	 uid-t)
  (f_fsid 	 :int32 :count 2)
  (f_charspare   :char :count 80)
  (f_fstypename	 :char :count #.+MFSNAMELEN+)
  (f_mntfromname :char :count #.+MNAMELEN+)
  (f_mntonname   :char :count #.+MNAMELEN+))

#+freebsd
(defstruct statfs
  "File system statistics."
  version
  type
  flags
  bsize
  iosize
  blocks
  bfree
  bavail
  files
  ffree
  syncwrites
  asyncwrites
  syncreads
  asyncreads
  namemax
  owner
  fsid
  fstypename
  mntfromname
  mntonname)

;; @@@ It seems I still have to do this.
#+freebsd
(defun convert-statfs (statfs)
  (if (and (pointerp statfs) (null-pointer-p statfs))
      nil
      (with-foreign-slots ((f_version
			    f_type
			    f_flags
			    f_bsize
			    f_iosize
			    f_blocks
			    f_bfree
			    f_bavail
			    f_files
			    f_ffree
			    f_syncwrites
			    f_asyncwrites
			    f_syncreads
			    f_asyncreads
			    f_namemax
			    f_owner
			    f_fsid
			    f_fstypename
			    f_mntfromname
			    f_mntonname
			    ) statfs (:struct foreign-statfs))
	(make-statfs :version     f_version
		     :type        f_type
		     :flags       f_flags
		     :bsize       f_bsize
		     :iosize      f_iosize
		     :blocks      f_blocks
		     :bfree       f_bfree
		     :bavail      f_bavail
		     :files       f_files
		     :ffree       f_ffree
		     :syncwrites  f_syncwrites
		     :asyncwrites f_asyncwrites
		     :syncreads   f_syncreads
		     :asyncreads  f_asyncreads
		     :namemax     f_namemax
		     :owner       f_owner
		     :fsid	  (vector (mem-aref f_fsid :int32 0)
					  (mem-aref f_fsid :int32 1))
		     :fstypename (foreign-string-to-lisp
				  f_fstypename :max-chars +MFSNAMELEN+)
		     :mntfromname (foreign-string-to-lisp
				   f_mntfromname :max-chars +MNAMELEN+)
		     :mntonname (foreign-string-to-lisp
				 f_mntonname :max-chars +MNAMELEN+)
		     ))))

#+freebsd
(defun convert-filesystem-info (statfs)
  (if (and (pointerp statfs) (null-pointer-p statfs))
      nil
      (with-foreign-slots ((f_bsize
			    f_blocks
			    f_bfree
			    f_bavail
			    f_fstypename
			    f_mntonname
			    f_mntfromname) statfs (:struct foreign-statfs))
	(make-filesystem-info
	 :device-name (foreign-string-to-lisp f_mntfromname
					      :max-chars +MNAMELEN+)
	 :mount-point (foreign-string-to-lisp f_mntonname
					      :max-chars +MNAMELEN+)
	 :type (foreign-string-to-lisp f_fstypename
				       :max-chars +MFSNAMELEN+)
	 :total-bytes (* f_blocks f_bsize)
	 :bytes-free (* f_bfree f_bsize)
	 :bytes-available (* f_bavail f_bsize)))))

;;(defmethod translate-from-foreign (statfs (type foreign-statfs-type))
;;  (convert-statfs statfs))

#+(and darwin 64-bit-target)
(defcfun ("statfs$INODE64" real-statfs) :int (path :string)
	 (buf (:pointer (:struct foreign-statfs))))
#+(or (and darwin 32-bit-target) linux freebsd)
(defcfun ("statfs" real-statfs) :int (path :string)
	 (buf (:pointer (:struct foreign-statfs))))
#+(or freebsd linux)
(defcfun ("fstatfs" real-fstatfs) :int (fd :int)
	 (buf (:pointer (:struct foreign-statfs))))

(defun statfs (path)
  (with-foreign-object (buf '(:struct foreign-statfs))
    (syscall (real-statfs path buf))
    (convert-statfs buf)))

(defun fstatfs (file-descriptor)
  (with-foreign-object (buf '(:struct foreign-statfs))
    (syscall (real-fstatfs file-descriptor buf))
    (convert-statfs buf)))

;; int getmntinfo(struct statfs **mntbufp, int flags);
#+(and darwin 64-bit-target)
(defcfun ("getmntinfo$INODE64" real-getmntinfo)
    :int (mntbufp :pointer) (flags :int))
#+(or (and darwin 32-bit-target) freebsd)
(defcfun ("getmntinfo" real-getmntinfo)
    :int (mntbufp :pointer) (flags :int))
#+(or darwin freebsd)
(defun getmntinfo (&optional (flags 0))
  (with-foreign-object (ptr :pointer)
    (let ((n (syscall (real-getmntinfo ptr flags))))
      (loop :for i :from 0 :below n
	 :collect (convert-statfs
		   (mem-aptr (mem-ref ptr :pointer)
			     '(:struct foreign-statfs) i))))))

;; Other things on OSX: ?
;;   exchangedata

;; OSX file attributes

(defctype attrgroup-t :uint32)

(defcstruct attrlist
  (bitmapcount :ushort)			; number of attr. bit sets in list
  (reserved    :uint16)			; (to maintain 4-byte alignment)
  (commonattr  attrgroup-t)		; common attribute group
  (volattr     attrgroup-t)		; volume attribute group
  (dirattr     attrgroup-t)		; directory attribute group
  (fileattr    attrgroup-t)		; file attribute group
  (forkattr    attrgroup-t))		; fork attribute group

(defconstant +ATTR_BIT_MAP_COUNT+ 5)

#+darwin
(defcfun getattrlist :int
  (path :string) (attrlist (:pointer (:struct attrlist)))
  (attr-buf (:pointer :void)) (attr-buf-size size-t) (options :unsigned-long))

#+darwin
(defcfun fgetattrlist :int
  (fd :int) (attrList (:pointer (:struct attrlist)))
  (attr-buf (:pointer :void)) (attr-buf-size size-t) (options :unsigned-long))

#+darwin
(defcfun getattrlistat :int
  (fd :int) (path :string) (attrList (:pointer (:struct attrlist)))
  (attr-buf (:pointer :void)) (attr-buf-size size-t) (options :unsigned-long))

#+darwin
(defcfun exchangedata :int
  (path1 :string) (path2 :string) (options :unsigned-int))

;; getfsent [BSD]

(define-constant +fs-types+ '(:hfs :nfs :msdos :cd9660 :fdesc :union))

(defcstruct foreign-fstab-struct
  "File system table."
  (fs_spec	:string)		; block special device name
  (fs_file	:string)		; file system path prefix
  (fs_vfstype	:string)		; File system type, ufs, nfs
  (fs_mntops	:string)		; Mount options ala -o
  (fs_type	:string)		; FSTAB_* from fs_mntops
  (fs_freq	:int)			; dump frequency, in days
  (fs_passno	:int))			; pass number on parallel fsck

(defstruct fstab
  "File system table."
  spec
  file
  vfstype
  mntops
  type
  freq
  passno)

(define-foreign-type foreign-fstab-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser foreign-fstab))

(defmethod translate-from-foreign (fstab (type foreign-fstab-type))
  (if (and (pointerp fstab) (null-pointer-p fstab))
      nil
      (with-foreign-slots ((fs_spec
			    fs_file
			    fs_vfstype
			    fs_mntops
			    fs_type
			    fs_freq
			    fs_passno) fstab (:struct foreign-fstab-struct))
	(make-fstab
	 :spec		fs_spec
	 :file		fs_file
	 :vfstype	fs_vfstype
	 :mntops	fs_mntops
	 :type		fs_type
	 :freq		fs_freq
	 :passno	fs_passno))))

(defcfun getfsent  foreign-fstab)
(defcfun getfsspec foreign-fstab (spec :string))
(defcfun getfsfile foreign-fstab (file :string))
(defcfun setfsent :int)
(defcfun endfsent :void)

;; getmntent - Linux

(defstruct mount-entry
  "File system description."
  fsname   ; name of mounted file system
  dir	   ; file system path prefix
  type	   ; mount type
  opts	   ; mount options
  freq	   ; dump frequency in days
  passno)  ; pass number on parallel fsck

;; (defmacro with-mount-entry-file ((var name) &body body)
;;   `(with-open-file (,var ,name)
;;      ,@body))

;; Because the C API is so bogus and requires stdio, just do it ourselves.
(defun get-mount-entry (stream)
  (let (line words)
    ;; Skip blank and comment lines
    (loop :do (setf line (read-line stream nil nil))
       :while (and line
		   (or (zerop (length line))
		       (char= (char line 0) #\#))))
    (when line
      (setf words
	    (split-sequence nil line
			    :test (λ (a b)
				     (declare (ignore a))
				     (or (char= b #\space) (char= b #\tab)))))
      (make-mount-entry
       :fsname (first words)
       :dir    (second words)
       :type   (third words)
       :opts   (fourth words)
       :freq   (fifth words)
       :passno (sixth words)))))

#+linux (defparameter *mtab-file* "/etc/mtab")

(defun mounted-filesystems ()
  "Return a list of filesystem info."
  #+(or darwin freebsd)
  (with-foreign-object (ptr :pointer)
    (let ((n (syscall (real-getmntinfo ptr 0))))
      (loop :for i :from 0 :below n
	 :collect (convert-filesystem-info
		   (mem-aptr (mem-ref ptr :pointer)
			     '(:struct foreign-statfs) i)))))
  #+linux
  (with-open-file (stream *mtab-file* :direction :input)
    (loop :with entry
       :while (setf entry (get-mount-entry stream))
       :collect
       (progn
	 (multiple-value-bind (fs err)
	     (ignore-errors (statfs (mount-entry-dir entry)))
	   (if err
	       (if (eql (opsys-error-code err) +EACCES+)
		   ;; If we can't access the mount point, just ignore it.
		   (make-filesystem-info
		    :device-name     (mount-entry-fsname entry)
		    :mount-point     (mount-entry-dir entry)
		    :type	     (mount-entry-type entry))
		   (signal err))
	       (make-filesystem-info
		:device-name     (mount-entry-fsname entry)
		:mount-point     (mount-entry-dir entry)
		:type	         (mount-entry-type entry)
		:total-bytes     (* (statfs-bsize fs) (statfs-blocks fs))
		:bytes-free	 (* (statfs-bsize fs) (statfs-bfree fs))
		:bytes-available (* (statfs-bsize fs) (statfs-bavail fs)))))))))

(defun mount-point-of-file (file)
  "Try to find the mount of FILE. This might not always be right."
  #+linux
  ;; I suppose this could work on other systems too, but it's certainly
  ;; more efficient and effective to get it from the statfs.
  (let (longest len (max-len 0) (real-name (safe-namestring (truename file))))
    (loop :for f :in
       (remove-if
	(_ (not (begins-with (car _) real-name)))
	(mapcar (_ (cons (filesystem-info-mount-point _)
			 (filesystem-info-device-name _)))
		(mounted-filesystems)))
       :do
       (when (> (setf len (length (car f))) max-len)
	 (setf longest f max-len len)))
    longest)
  #+(or darwin freebsd)
  (handler-case
      (let ((s (statfs file)))
	(cons (statfs-mntonname s) (statfs-mntfromname s)))
    (os-unix:posix-error (c)
      (if (find (opsys-error-code c)
		`(,os-unix:+EPERM+ ,os-unix:+ENOENT+ ,os-unix:+EACCES+))
	  nil
	  (list (opsys-error-code c) c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ttys

(defcfun isatty  :int (fd :int))
(defcfun ttyname :string (fd :int))

(defun file-handle-terminal-p (fd)
  "Return true if the system file descriptor FD is attached to a terminal."
  (= (isatty fd) 1))

(defun file-handle-terminal-name (fd)
  "Return the device name of the terminal attached to the system file
descriptor FD."
;;;  (let ((ttn (ttyname fd)))
;;;  (and (not (null-pointer-p ttn)) ttn)))
  ;; @@@ XXX We should probably use ttyname_r
  (ttyname fd))

(defvar *default-console-device-name* "/dev/tty"
  "Name of the default console device.")

(defun open-terminal (device-name)
  "Open a terminal. Return the system file handle."
  (syscall (posix-open device-name +O_RDWR+ 0)))

(defun close-terminal (terminal-handle)
  "Close a terminal."
  (syscall (posix-close terminal-handle)))

(define-condition read-char-error (posix-error)
  ()
  (:report (lambda (c s)
	     (format s "Error reading a character: ~a"
			 (symbol-call :opsys :error-message
				      (opsys-error-code c)))))
  (:documentation "An error reading a character."))

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

(defun read-raw-char (terminal-handle c &optional test)
  (let (status)
    (loop
       :do
       (setf status (posix-read terminal-handle c 1))
       :while (or (and (< status 0)
		       (or (= *errno* +EINTR+) (= *errno* +EAGAIN+)))
		  (and test
		       (funcall test (mem-ref c :unsigned-char))))
       :do
       ;; Probably returning from ^Z or terminal resize, or something,
       ;; so keep trying. Enjoy your trip to plusering town.
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
	  (when (< status 0)
	    (cerror "Try again?" 'read-char-error :error-code *errno*)))))
    status))

;; Simple, linear, non-event loop based programming was always an illusion!
(defun read-terminal-char (terminal-handle &key timeout)
  (let (result)
    (unwind-protect
      (progn
	(when timeout
	  (set-terminal-mode terminal-handle :timeout timeout))
	(with-foreign-object (c :char)
	  (with-signal-handlers ((+SIGWINCH+ . sigwinch-handler)
				 (+SIGTSTP+  . tstp-handler))
	    (when (not (zerop (read-raw-char terminal-handle c)))
	      (setf result (code-char (mem-ref c :unsigned-char)))))))
      (when timeout
	(set-terminal-mode terminal-handle :timeout nil)))
    result))

(defun read-terminal-byte (terminal-handle &key timeout)
  (let (result)
    (unwind-protect
      (progn
	(when timeout
	  (set-terminal-mode terminal-handle :timeout timeout))
	(with-foreign-object (c :char)
	  (with-signal-handlers ((+SIGWINCH+ . sigwinch-handler)
				 (+SIGTSTP+  . tstp-handler))
	    (when (not (zerop (read-raw-char terminal-handle c)))
	      (setf result (mem-ref c :unsigned-char))))))
      (when timeout
	(set-terminal-mode terminal-handle :timeout nil)))
    result))

(defun read-until (tty stop-char &key timeout)
  "Read until STOP-CHAR is read. Return a string of the results.
TTY is a file descriptor."
  (let ((result (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t))
	(status nil) (got-eof nil))
    (set-terminal-mode tty :timeout timeout)
    (with-output-to-string (str result)
      (with-foreign-object (c :char)
	(with-signal-handlers ((+SIGWINCH+ . sigwinch-handler)
			       (+SIGTSTP+  . tstp-handler))
	  (setf status
		(read-raw-char tty c
			       #'(lambda (x)
				   (let ((cc (code-char x)))
				     (and cc (char/= cc stop-char)
					  (princ cc str))))))
	  (when (zerop status)
	    (setf got-eof t)))))
    (set-terminal-mode tty :timeout nil)
    (values
     (if (zerop (length result))
	 nil
	 result)
     got-eof)))

(defun write-terminal-char (terminal-handle char)
  "Write CHAR to the terminal designated by TERMINAL-HANDLE."
  (with-foreign-string ((s size) (string char))
    (syscall (posix-write terminal-handle s size))))

(defun write-terminal-string (terminal-handle string)
  "Write STRING to the terminal designated by TERMINAL-HANDLE."
  (with-foreign-string ((s size) string)
    (syscall (posix-write terminal-handle s size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; job control thingys

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

(defun wait-and-chill (child-pid)
  ;; Make the child be in it's own process group.
  ;;(syscall (setpgid child-pid child-pid))
  ;; Make the terminal signals go to the child's group.
  ;;(syscall (tcsetpgrp 0 child-pid))
  ;; Ignore terminal suspend signals.
  (setf (signal-action +SIGTSTP+) 'tstp-handler)

  (with-foreign-object (status-ptr :int 1)
    (setf (mem-ref status-ptr :int) 0)
    ;;(format t "About to wait for ~d~%" child-pid)
    (let ((status 0) (wait-pid 0))
      (declare (ignorable status))
      (loop
	 ;; +WAIT-UNTRACED+ is so it will return when ^Z is pressed
	 :while (/= wait-pid child-pid)
	 :do
	 (setf wait-pid (real-waitpid child-pid status-ptr +WAIT-UNTRACED+))
	 ;;(format t "Back from wait wait-pid = ~d~%" wait-pid)
	 (if (= wait-pid -1)
	     (if (= *errno* +ECHILD+)
		 (progn
		   ;;(format t "Nothing to wait for~%")
		   (return-from nil nil))
		 (error-check wait-pid "wait-pid"))
	     (setf status (mem-ref status-ptr :int)))
	 ;;(format t "status = ~d~%" status)
	 ;; (when (/= wait-pid child-pid)
	 ;;   (format t "Wait pid ~a doesn't match child pid ~a.~%"
	 ;; 	   wait-pid child-pid))
	 (finish-output))
      ;; Take the terminal back.
      (syscall (tcsetpgrp 0 (getpid)))
      ;; Suspend us again if we get a ^Z.
      (setf (signal-action +SIGTSTP+) :default)
      (wait-return-status status))))

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
	    ;; Make the child be in it's own process group.
	    ;; We have to do this here in the child because on Linux
	    ;; the parent won't be allowed to do it after the exec.
	    (when (= -1 (setpgid (getpid) (getpid)))
	      (posix-write 1 *setpgid-err* *setpgid-err-len*))
	    (when (not background)
	      (when (= -1 (tcsetpgrp 0 (getpid)))
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
	(error-check child-pid "child-pid")
	child-pid))))

(defun resume-pid (pid)
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
	     (error-check pid "check-jobs")))
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
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil
;; ptrace

(defparameter *ptrace-requests* nil "Requests type for ptrace.")
(define-to-list *ptrace-requests*
#(
#(+PTRACE-TRACEME+      0 "Yo, yo, yo! Trace me bro.")
#(+PTRACE-PEEKTEXT+     1 "Return the word in the process's text space at address ADDR.")
#(+PTRACE-PEEKDATA+     2 "Return the word in the process's data space at address ADDR.")
#(+PTRACE-PEEKUSER+     3 "Return the word in the process's user area at offset ADDR.")
#(+PTRACE-POKETEXT+     4 "Write the word DATA into the process's text space at address ADDR.")
#(+PTRACE-POKEDATA+     5 "Write the word DATA into the process's data space at address ADDR.")
#(+PTRACE-POKEUSER+     6 "Write the word DATA into the process's user area at offset ADDR.")
#(+PTRACE-CONT+	        7 "Continue the process.")
#(+PTRACE-KILL+	        8 "Kill the process.")
#(+PTRACE-SINGLESTEP+   9 "Single step the process. This is not supported on all machines.")
#(+PTRACE-GETREGS+     12 "Get all general purpose registers used by a processes.")
#(+PTRACE-SETREGS+     13 "Set all general purpose registers used by a processes.")
#(+PTRACE-GETFPREGS+   14 "Get all floating point registers used by a processes.")
#(+PTRACE-SETFPREGS+   15 "Set all floating point registers used by a processes.")
#(+PTRACE-ATTACH+      16 "Attach to a process that is already running.")
#(+PTRACE-DETACH+      17 "Detach from a process attached to with PTRACE_ATTACH.")
#(+PTRACE-GETFPXREGS+  18 "Get all extended floating point registers used by a processes.")
#(+PTRACE-SETFPXREGS+  19 "Set all extended floating point registers used by a processes.")
#(+PTRACE-SYSCALL+     24 "Continue and stop at the next (return from) syscall.")
#(+PTRACE-SETOPTIONS+  #x4200 "Set ptrace filter options.")
#(+PTRACE-GETEVENTMSG+ #x4201 "Get last ptrace message.")
#(+PTRACE-GETSIGINFO+  #x4202 "Get siginfo for process.")
#(+PTRACE-SETSIGINFO+  #x4203 "Set new siginfo for process.")
#(+PTRACE-GETREGSET+   #x4204 "Get register content.")
#(+PTRACE-SETREGSET+   #x4205 "Set register content.")
#(+PTRACE-SEIZE+       #x4206 "Like PTRACE_ATTACH, but do not force tracee to trap and do not affect signal or group stop state.")
#(+PTRACE-INTERRUPT+   #x4207 "Trap seized tracee.")
#(+PTRACE-LISTEN+      #x4208 "Wait for next group event.")
#(+PTRACE-PEEKSIGINFO+ #x4209 "")
#(+PTRACE-GETSIGMASK+  #x420a "")
#(+PTRACE-SETSIGMASK+  #x420b "")
#(+PTRACE-SECCOMP-GET-FILTER+ #x420c "")))

#| alternate names
(defconstant +PT_TRACE_ME+    +PTRACE_TRACEME+)
(defconstant +PT_READ_I+      +PTRACE_PEEKTEXT+)
(defconstant +PT_READ_D+      +PTRACE_PEEKDATA+)
(defconstant +PT_READ_U+      +PTRACE_PEEKUSER+)
(defconstant +PT_WRITE_I+     +PTRACE_POKETEXT+)
(defconstant +PT_WRITE_D+     +PTRACE_POKEDATA+)
(defconstant +PT_WRITE_U+     +PTRACE_POKEUSER+)
(defconstant +PT_CONTINUE+    +PTRACE_CONT+)
(defconstant +PT_KILL+        +PTRACE_KILL+)
(defconstant +PT_STEP+        +PTRACE_SINGLESTEP+)
(defconstant +PT_GETREGS+     +PTRACE_GETREGS+)
(defconstant +PT_SETREGS+     +PTRACE_SETREGS+)
(defconstant +PT_GETFPREGS+   +PTRACE_GETFPREGS+)
(defconstant +PT_SETFPREGS+   +PTRACE_SETFPREGS+)
(defconstant +PT_ATTACH+      +PTRACE_ATTACH+)
(defconstant +PT_DETACH+      +PTRACE_DETACH+)
(defconstant +PT_GETFPXREGS+  +PTRACE_GETFPXREGS+)
(defconstant +PT_SETFPXREGS+  +PTRACE_SETFPXREGS+)
(defconstant +PT_SYSCALL+     +PTRACE_SYSCALL+)
(defconstant +PT_SETOPTIONS+  +PTRACE_SETOPTIONS+)
(defconstant +PT_GETEVENTMSG+ +PTRACE_GETEVENTMSG+)
(defconstant +PT_GETSIGINFO+  +PTRACE_GETSIGINFO+)
(defconstant +PT_SETSIGINFO+  +PTRACE_SETSIGINFO+)
|#

(defconstant +PTRACE-SEIZE-DEVEL+ #x80000000 "Flag for PTRACE_LISTEN.")

(defparameter *ptrace-setoptions* nil "Options set using PTRACE_SETOPTIONS.")
(define-to-list *ptrace-setoptions*
#(
#(+PTRACE-O-TRACESYSGOOD+    #x00000001 "")
#(+PTRACE-O-TRACEFORK+	     #x00000002 "")
#(+PTRACE-O-TRACEVFORK+	     #x00000004 "")
#(+PTRACE-O-TRACECLONE+	     #x00000008 "")
#(+PTRACE-O-TRACEEXEC+	     #x00000010 "")
#(+PTRACE-O-TRACEVFORKDONE+  #x00000020 "")
#(+PTRACE-O-TRACEEXIT+	     #x00000040 "")
#(+PTRACE-O-TRACESECCOMP+    #x00000080 "")
#(+PTRACE-O-EXITKILL+	     #x00100000 "")
#(+PTRACE-O-SUSPEND-SECCOMP+ #x00200000 "")
#(+PTRACE-O-MASK+	     #x003000ff "")
))

(defparameter *ptrace-eventcodes* nil "Wait extended result codes for the above trace options.")
(define-to-list *ptrace-eventcodes*
#(
#(+PTRACE-EVENT-FORK+	      1 "")
#(+PTRACE-EVENT-VFORK+	      2 "")
#(+PTRACE-EVENT-CLONE+	      3 "")
#(+PTRACE-EVENT-EXEC+	      4 "")
#(+PTRACE-EVENT-VFORK-DONE+   5 "")
#(+PTRACE-EVENT-EXIT+	      6 "")
#(+PTRACE-EVENT-SECCOMP+      7 "")
))

(defcfun ptrace :long
  (request :unsigned-int) (pid pid-t) (addr :pointer) (data :pointer))

(defun process-attach (pid)
  (syscall (ptrace +PTRACE-ATTACH+ pid (null-pointer) (null-pointer))))

(defun process-detach (pid)
  (syscall (ptrace +PTRACE-DETACH+ pid (null-pointer) (null-pointer))))

;; Weird/simulation/emulation
;; syscall

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(defcfun wcwidth :int (wc wchar-t))

#+clisp (shadowing-import 'ext:char-width)
#+clisp (export 'char-width)
#-clisp
(defun char-width (char)
  "Return the column width of CHAR. If it's not working as expected, you ~
   probably have to call setlocale first."
  (if (graphic-char-p char)		; assume this is equivalent to iswprint
      (wcwidth (char-code char))
      (error "Can't determine the width of a non-graphic character: ~s" char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Go thru *features* and get rid of all our temporary configuration.
; (setf *features*
;       (delete-if #'(lambda (x)
; 		     (let ((s (string x)))
; 		       (string= s "OS-T-" :end1 (min 5 (length s)))))
; 		 *features*))

;; @@@ Should get rid of temporary features :os-t-*

;; EOF
