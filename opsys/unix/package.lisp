;;;
;;; unix/package.lisp - Interface to UNIX-like systems.
;;;

;; Convetions:
;; - Unix system calls should usually be wrapped in SYSCALL which will
;;   check return values and throw errors.
;; - Most API types should be defined with DEFCTYPE.
;; - Use defining macros, where appropriate:
;;     define-simple-types define-constants
;;     define-constants-from define-name-list-from
;;   and from DLIB (or fake-dlib):
;;     define-constant defconstant-to-list
;; - Make sure the interface is nicely callable from Lisp code, e.i. don't
;;   make the caller pass pointers or have deal with foreign memory. Things
;;   that just return ints probably don't need wrapping.
;; - If you make a Lispy-er wrapper, call the original C version real-<func>.
;; - See the conventions in opsys.lisp.

(defpackage :opsys-unix
  (:documentation "Interface to UNIX-like systems.")
  (:use :cl :cffi :fake-dlib :opsys-base :opsys-generic :libc)
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
   #:*errors*
   #:*sys-nerr*
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
   #:env #:getenv #:setenv #:unsetenv
   #:sysctl
   #:sysctl-by-number
   #:getpagesize
   #:memory-page-size
   #:processor-count
   #:getauxval
   #:getlogin
   #:sysconf
   #:sysinfo
   #:*sysconf-names*
   #:system-info-names
   #:system-info-description
   #:get-system-info
   #:uname
   #:make-uname
   #:uname-sysname
   #:uname-nodename
   #:uname-release
   #:uname-version
   #:uname-machine
   #:uname-domainname
   #:os-machine-instance
   #:os-machine-type
   #:os-machine-version
   #:os-software-type
   #:os-software-version
   #:os-software-release

   ;; C dynamic libraries
   #:dlopen
   #:dlmopen
   #:dlclose
   #:dlsym
   #:dlinfo
   #:dlerror
   #:+RTLD-LAZY+ #:+RTLD-NOW+ #:+RTLD-NOLOAD+ #:+RTLD-DEEPBIND+ #:+RTLD-GLOBAL+
   #:+RTLD-LOCAL+ #:+RTLD-NODELETE+ #:+RTLD-NEXT+ #:+RTLD-DEFAULT+
   #:+LM-ID-BASE+ #:+LM-ID-NEWLM+

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
   #:setgrent

   ;; user login accounting
   #:utmpx #:utmpx-user #:utmpx-id #:utmpx-line #:utmpx-pid #:utmpx-type
   #:utmpx-tv #:utmpx-host
   #:endutxent
   #:getutxent
   #:getutxid
   #:getutxline
   #:pututxline
   #:setutxent
   #:setutxdb
   #:utmpname
   #:*utxdb-types* #:+UTXDB-ACTIVE+ #:+UTXDB-LASTLOGIN+ #:+UTXDB-LOG+
   #:default-utmpx-file #:set-utmp-file #:*default-utmpx-files*
   #:guess-utmpx-file-type
   #:users-logged-in
   #:setlogin

   ;; directories
   #:hidden-file-name-p
   #:superfluous-file-name-p
   #:path-to-absolute
   #:%path-absolute-p
   #:parse-path
   #:os-pathname-namestring
   #:os-pathname-pathname
   ;;#:dir-entry-inode
   #:change-directory
   #:chdir
   #:fchdir
   #:pathconf
   #:get-path-max
   #:current-directory
   #:dirent-name
   #:dirent-type
   #:read-directory
   #:map-directory
   #:without-access-errors

   ;; files (low level)
   #:+O_RDONLY+ #:+O_WRONLY+ #:+O_RDWR+ #:+O_ACCMODE+ #:+O_NONBLOCK+
   #:+O_APPEND+ #:+O_SYNC+ #:+O_SHLOCK+ #:+O_EXLOCK+ #:+O_CREAT+ #:+O_TRUNC+
   #:+O_EXCL+ #:+O_NOCTTY+
   #+linux #:+O_LARGEFILE+
   #+linux #:+O_DIRECTORY+
   #+linux #:+O_NOFOLLOW+
   #+linux #:+O_DIRECT+
   #+linux #:+O_NOATIME+
   #+linux #:+O_PATH+
   #+linux #:+O_DSYNC+
   #+linux #:+O_TMPFILE+
   #+linux #:+O_CLOEXEC+
   #+freebsd #:+O_NOFOLLOW+
   #+freebsd #:+O_DIRECT+
   #+freebsd #:+O_DIRECTORY+
   #+freebsd #:+O_EXEC+
   #+freebsd #:+O_FSYNC+
   #+freebsd #:+O_TTY_INIT+
   #+freebsd #:+O_CLOEXEC+
   #:SEEK-SET+ #:SEEK-CUR+ #:SEEK-END+
   #+linux #:SEEK-DATA+
   #+linux #:SEEK-HOLE+
   #:posix-open #+(or linux freebsd) #:posix-openat
   #:posix-close
   #:posix-dup
   #:posix-dup2
   #:posix-read
   #:posix-write
   #:posix-ioctl
   #:posix-unlink
   #+(or linux freebsd openbsd netbsd) #:posix-unlinkat
   #:posix-lseek
   #:posix-pread
   #:posix-pwrite
   #:posix-readv
   #:posix-writev
   #:posix-preadv
   #:posix-pwritev
   #:posix-rename
   #:copy-file-range
   #:with-posix-file
   #:with-os-file
   #:with-temporary-thing
   #:with-temporary-directory
   #:with-temporary-file
   #:mkdtemp
   #:mkstemp
   #:mkostemp
   #:mkstemps
   #:mkostemps
   #:fcntl
   #:get-file-descriptor-flags
   #:fsync
   #:fdatasync

   ;; stat
   #:stat
   #:lstat
   #:fstat
   #+(or linux freebsd) #:fstatat
   #:with-stat-buffer
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
   ;; #:command-pathname
   #:command-test

   #:data-dir
   #:config-dir
   #:data-path
   #:config-path
   #:cache-dir
   #:runtime-dir
   
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

   #:with-locked-file

   #:+AT-FDCWD+ #:+AT-SYMLINK-NOFOLLOW+
   #:utimes
   #:utimensat
   #:futimens
   #:symlink
   #:symlinkat
   
   ;; signals
   #:*signal-count*
   #:signal-name
   #:signal-description
   #:signal-action
   #:set-signal-action
   #:with-signal-handler
   #:with-signal-handlers
   #:with-signal-handlers*
   #:with-blocked-signals
   #:with-all-signals-blocked
   #:signal-mask
   #:set-signal-mask
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
   #:getrusage
   #:timeval #:timeval-seconds #:timeval-micro-seconds
   #:rusage #:rusage-user #:rusage-system
   #:process-times
   #:_exit
   #:exec
   #:fork
   #:*post-fork-hook*
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
   #:get-groups-list
   #:member-of
   #:setgroups
   #:set-groups
   #:add-groups

   #:process-list
   #:process-info
   #:current-process-id
   #:*system-process-type*
   #:system-process-list
   #:system-process-info
   #:suspend-process
   #:resume-process
   #:terminate-process
   #:unix-process-handle
   #:*got-sigwinch*
   #:*got-tstp*
   #:sigwinch-handler
   #:tstp-handler
   #:wait-and-chill
   #:check-jobs
   #:os-process-priority
   #:*os-process-most-favorable-priority*
   #:*os-process-least-favorable-priority*

   #:unix-process
   #:unix-process-p
   #:make-unix-process
   #:unix-process-id
   #:unix-process-parent-id
   #:unix-process-group-id
   #:unix-process-user-id
   #:unix-process-terminal
   #:unix-process-text-size
   #:unix-process-resident-size
   #:unix-process-percent-cpu
   #:unix-process-nice-level
   #:unix-process-usage
   #:unix-process-command
   #:unix-process-args

   #:*rlimit-resources*
   #:+RLIMIT-CPU+ #:+RLIMIT-FSIZE+ #:+RLIMIT-DATA+ #:+RLIMIT-STACK+
   #:+RLIMIT-CORE+ #:+RLIMIT-RSS+ #:+RLIMIT-NPROC+ #:+RLIMIT-NOFILE+
   #:+RLIMIT-MEMLOCK+ #:+RLIMIT-AS+ #:+RLIMIT-LOCKS+ #:+RLIMIT-SIGPENDING+
   #:+RLIMIT-MSGQUEUE+ #:+RLIMIT-NICE+ #:+RLIMIT-RTPRIO+ #:+RLIMIT-RTTIME+
   #:+RLIMIT-NLIMITS+ #:+RLIMIT-OFILE+
   #:+RLIMIT-INFINITY+ #:+RLIM-INFINITY+
   #:rlimit #:rlimit-current #:rlimit-maximum
   #:rlimit-number
   #:setrlimit #:getrlimit
   #+linux #:prlimit
   #:get-pipe-size
   
   #:popen
   #:posix-pipe
   #+linux #:linux-splice
   #+linux #:linux-vmsplice
   #+linux #:linux-tee
   #:ptrace
   #+linux #:prctl

   ;; time
   #:timespec
   #:timespec-seconds
   #:timespec-nanoseconds
   #:gettimeofday
   #:settimeofday
   #:clock-getres
   #:clock-gettime
   #:clock-settime
   #:+unix-to-universal-time+
   #:unix-to-universal-time
   #:universal-to-unix-time
   #:timespec-to-os-time
   #:get-time
   #:set-time
   #:tzset
   #:tzname
   #:timezone
   #:daylight
   #:timezone-name
   #:timezone-offset
   #+linux #:timerfd-create
   #+linux #:timerfd-settime
   #+linux #:timerfd-gettime

   ;; multiplexed io
   #:lame-poll
   #:lame-select
   #:listen-for

   ;; events
   #:%create-event-set
   #:%destroy-event-set
   #:%add-event
   #:%delete-event
   #:%clear-triggers
   #:await-events
   #:pick-events
   #:map-events
   #:events-pending-p

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
   #:get-filesystem-info
   #:mount-point-of-file

   ;; unix-stream
   #:unix-stream
   #:unix-input-stream
   #:unix-output-stream
   #:unix-io-stream
   #:unix-character-input-stream
   #:unix-character-output-stream
   #:unix-character-io-stream
   #:unix-binary-input-stream
   #:unix-binary-output-stream
   #:unix-binary-io-stream
   #:fill-buffer
   #:flush-buffer
   #:stream-handle-direction

   ;; Memory
   #:mmap
   #:munmap
   #:+PROT_NONE+ #:+PROT_READ+ #:+PROT_WRITE+ #:+PROT_EXEC+ #:+PROT_GROWSDOWN+
   #:+PROT_GROWSUP+
   #:+MAP_SHARED+ #:+MAP_PRIVATE+ #:+MAP_TYPE+ #:+MAP_FIXED+ #:+MAP_ANONYMOUS+
   #:+MAP_32BIT+ #:+MAP_GROWSDOWN+ #:+MAP_DENYWRITE+ #:+MAP_EXECUTABLE+
   #:+MAP_LOCKED+ #:+MAP_NORESERVE+ #:+MAP_POPULATE+ #:+MAP_NONBLOCK+
   #:+MAP_STACK+ #:+MAP_HUGETLB+ #:+MAP_UNINITIALIZED+
   #:+MREMAP_MAYMOVE+ #:+MREMAP_FIXED+ #:+OVERCOMMIT_GUESS+ #:+OVERCOMMIT_ALWAYS+
   #:+OVERCOMMIT_NEVER+ #:+MAP_HUGE_SHIFT+ #:+MAP_HUGE_MASK+ #:+MAP_HUGE_2MB+
   #:+MAP_HUGE_1GB+ #:+MLOCK_ONFAULT+
   #:+MS_ASYNC+ #:+MS_SYNC+ #:+MS_INVALIDATE+
   #:msync
   #:mprotect
   #:madvise
   #:mlock
   #:munlock
   #:munlockall
   #:mincore
   #:mremap
   #:remap-file-pages
   #:map-file

   ;; Terminal things
   #:isatty
   #:ttyname

   ;; tty chars
   #:+VEOF+ #:+VEOL+ #:+VEOL2+ #:+VERASE+ #:+VWERASE+ #:+VKILL+ #:+VREPRINT+
   #:+VINTR+ #:+VQUIT+ #:+VSUSP+ #:+VDSUSP+ #:+VSTART+ #:+VSTOP+ #:+VLNEXT+
   #:+VDISCARD+ #:+VMIN+ #:+VTIME+
   #:+VSTATUS+ #:+NCCS+ #:*cchars*

   ;; input modes
   #:+IGNBRK+ #:+BRKINT+ #:+IGNPAR+ #:+PARMRK+ #:+INPCK+ #:+ISTRIP+ #:+INLCR+
   #:+IGNCR+ #:+ICRNL+ #:+IXON+ #:+IXOFF+ #:+IXANY+ #:+IMAXBEL+ #:+IUCLC+
   #:+IUTF8+ #:+DOSMODE+ #:*iflags*

   ;; output modes
   #:+OPOST+ #:+ONLCR+ #:+OXTABS+ #:+ONOEOT+ #:*oflags*

   ;; control flags
   #:+CIGNORE+ #:+CSIZE+ #:+CS5+ #:+CS6+ #:+CS7+ #:+CS8+ #:+CSTOPB+ #:+CREAD+
   #:+PARENB+ #:+PARODD+ #:+HUPCL+ #:+CLOCAL+ #:+CCTS+ #:+CRTS+ #:+CRTSCTS+
   #:+CDTR+ #:+CDSR+ #:+CCAR+ #:+MDMBUF+ #:*cflags*

   ;; other "local" flags
   #:+ECHOKE+ #:+ECHOE+ #:+ECHOK+ #:+ECHO+ #:+ECHONL+ #:+ECHOPRT+ #:+ECHOCTL+
   #:+ISIG+ #:+ICANON+ #:+ALTWERASE+ #:+IEXTEN+ #:+EXTPROC+ #:+TOSTOP+
   #:+FLUSHO+ #:+NOKERNINFO+ #:+PENDIN+ #:+NOFLSH+ #:*lflags*

   ;; actions
   #:+TCSANOW+ #:+TCSADRAIN+ #:+TCSAFLUSH+ #:+TCSASOFT+
   #:+TCIFLUSH+ #:+TCOFLUSH+ #:+TCIOFLUSH+ #:+TCOOFF+ #:+TCOON+ #:+TCIOFF+
   #:+TCION+

   ;; speeds
   #:+B0+ #:+B50+ #:+B75+ #:+B110+ #:+B134+ #:+B150+ #:+B200+ #:+B300+ #:+B600+
   #:+B1200+ #:+B1800+ #:+B2400+ #:+B4800+ #:+B9600+ #:+B19200+ #:+B38400+
   #:+B7200+ #:+B14400+ #:+B28800+ #:+B57600+ #:+B76800+ #:+B115200+
   #:+B230400+ #:+EXTA+ #:+EXTB+

   ;; types
   #:tcflag-t
   #:cc-t
   #:speed-t
   #:termios
   ;; slot names
   #:c_iflag #:c_oflag #:c_cflag #:c_lflag #:c_cc #:c_ispeed #:c_ospeed

   ;; Posix-y Functions
   #:cfgetispeed
   #:cfgetospeed
   #:cfsetispeed
   #:cfsetospeed
   #:tcgetattr
   #:tcsetattr
   #:tcdrain
   #:tcflow
   #:tcflush
   #:tcsendbreak
   #:cfmakeraw
   #:cfsetspeed

   ;; Additional functions
   #:sane
   ;;#:terminal-query
   #:call-with-raw
   ;;#:describe-tty
   ;;#:set-tty
   #:getch
   #:with-nonblocking-io
   #:terminal-read-timeout

   ;; Pseudo terminals
   #:grantpt
   #:unlockpt
   #:posix-openpt
   #:ptsname-r
   #:ptsname
   #:getpt
   #:openpty
   #:open-pseudo-terminal
   #:login-tty
   #:forkpty

   ;; old fashioned tty ioctls
   #:winsize
   #:ws_row
   #:ws_col
   #:ws_xpixel
   #:ws_ypixel
   #:+TIOCSTI+
   #:+TIOCGWINSZ+
   #:+TIOCSWINSZ+
   #:+TIOCGPTPEER+

   ;; Portable interafce
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
   #:set-terminal-mode
   #:get-terminal-mode
   #:get-window-size
   #:get-window-size-struct
   #:set-window-size-struct
   #:slurp-terminal
   #:reset-terminal-modes
   #:terminal-time
   #:terminal-query
   #:with-terminal-signals
   #:control-char
   #:set-control-char
   #:open-pseudo-terminal

   ;; testing
   #:terminal-test
   #:test-echo
   #:test-input

   ;; Communication
   #:gethostname
   #:sethostname
   #:getdomainname
   #:setdomainname
   #:network-host-name
   #:network-domain-name

   ;; Character coding / localization
   #:wcwidth
   #:char-width
   #:language
   #:*nl-items*
   #:nl-langinfo
   ))

(in-package :opsys-unix)

;; End
