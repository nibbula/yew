;;;
;;; opsys.lisp - Interface to operating systems
;;;

;;; $Revision: 1.24 $

;; This is a thin layer over basic operating system functions which
;; are not included in Common Lisp. The goal would be to have the same
;; interface supported on all operating systems and implementations.
;; One goal is that code outside of this module, shouldn't have to do #+os.
;; The prospective list would be:
;;   MacOSX, Linux, Windows, *BSD, Solaris, Android, iOS
;; Implementations:
;;   CLisp, SBCL, ClozureCL, ecl, CMU Lisp, abcl, whatever
;;
;; The tactics we will try to take will be, in order:
;;
;;   Use an implementation supplied function if it works best
;;   Use a CFFI call to a POSIX function
;;   Use a CFFI call to some other system specific function
;;   Use an implementation native FFI call to whatever
;;   Use an error to beg for patches or explain the pathetic situation!
;;
;; Mostly the things we want to put in here are on the level of what were
;; traditionally considered Unix kernal syscall's. This line between these
;; things is unfortunately a little blurry. Like, we probably don't want to
;; put a socket or thread interface in here. For example, the termios module
;; is separate. Perhaps the best approach is to make separate modules for
;; larger self-contained areas of OS functionality which may not be required
;; by every application, like: termios, networking(sockets), threads, IPC,
;; etc.
;;
;; The problem comes when you are forced to make a CFFI version of the
;; interface for one implementation, then it's just easier to use that on all
;; implementations. So this thing will probably transition to be all CFFI
;; based. The other thing is, I keep getting the urge to write lispy-er
;; wrappers, because, as you may or may not know, unix is really very terrible
;; (If you don't agree, just look at the what code in this module has to do,
;; and imagine how it might be different if the OS was written in Lisp). So
;; really it will probably evolve into a set of quirky lispy wrappers around a
;; low level unix FFI. So really I recommend you don't use it. And I recommend
;; to myself not to even write it. ;-P (and delete this whole paragraph, which
;; just keeps growing as my disgrutlement with this module increases.)
;;
;; Now that cffi-grovel exists, perhaps we should use it instead of constantly
;; needing updating. But I'm worried that groveling requires a C "development
;; system". Do I need to make an all lisp groveler? We could probably use that
;; C-Lisp translator thingy (which I forget the name of).
;;
;; I've started to use CFFI type translators for C structs. In some cases it
;; makes it much easier to define a number of functions working with a type,
;; because you don't have to code individual wrapper functions. But
;; unfortunately there's numerous cases where I don't want to do that. For
;; example, where a C function wants a C allocated buffer passed in, which it
;; fills in and returns a an int for status, which is highly simplified by
;; returning the data, and signaling an error if something goes wrong (see the
;; history of pc-lusering). In those cases we still have to define
;; wrappers. Perhaps I should make a macro to define wrapper
;; functions. Something like:
;;
;;   (defwrapper statfs (path)
;;     (:int (path :string) (buf foreign-statfs :in-out :allocate-zero))
;;     (call path alloc)
;;     (if (= return-value 0) ; unhygenic!?
;;       buf
;;       return-value))
;;
;; Now that I'm thinking about doing a Windows (not cygwin) version, I feel
;; like making opsys-x.lisp, like opsys-windows opsys-osx opsys-linux, etc and
;; then making opsys.lisp provide only the most general functions, like
;; "read-directory" or "system-command". If I implement a whole bunch of unixy
;; commands for lish like "df" "ps" "ls" "finfo" "grep" etc, and then abstract
;; _slightly_ for windows bullshit, then we should have something reasonable.
;; The reality is that almost every OS **except windows** (including iOS,
;; Android, ChromeOS) has unixy system calls, so starting with that will get
;; us to almost everything.
;;
;; Use (missing-implementation) wherever we don't provide a required function.
;;
;; TODO:
;;   - Tests! TESTS!!! (see opsys-test.lisp)
;;   - Fix sub-process code
;;   - Put earmuffs on stuff

;; (declaim (optimize (speed 3)) (optimize (safety 0))
;;   	 (optimize (debug 0)) (optimize (space 0))
;;    	 (optimize (compilation-speed 0)))

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defpackage :opsys
  (:documentation "Generic interface to operating system functionality.")
  (:nicknames :nos)
  (:use :cl :cffi)
  (:export
   ;; error handling
   #:*errno*
   #:strerror
   #:posix-error
   #:posix-error-code
   #:posix-error-format
   #:posix-error-arguments
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
   #:environ
   #:getenv
   #:setenv
   #:lisp-args
   #:sysctl
   #:getlogin

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

   #:group
   #:group-name
   #:group-passwd
   #:group-gid
   #:group-members

   #:getgrgid
   #:getgrnam
   #:getgrent
   #:endgrent

   ;; directories
   #:change-directory
   #:current-directory
   #:in-directory
   #:make-directory
   #:delete-directory
   #:read-directory
   #:dir-entry
   #:dir-entry-p
   #:make-dir-entry
   #:dir-entry-name
   #:dir-entry-type
   #:dir-entry-inode
   #:probe-directory

   ;; files (low level)
   #:O_RDONLY #:O_WRONLY #:O_RDWR #:O_ACCMODE #:O_NONBLOCK #:O_APPEND
   #:O_SYNC #:O_SHLOCK #:O_EXLOCK #:O_CREAT #:O_TRUNC #:O_EXCL
   #:posix-open
   #:posix-close
   #:posix-read
   #:posix-write
   #:posix-ioctl
   #:with-posix-file

   ;; stat
   #:stat
   #:lstat
   #:fstat
   #:S_IFMT #:S_IFIFO #:S_IFCHR #:S_IFDIR #:S_IFBLK #:S_IFREG #:S_IFLNK
   #:S_IFSOCK #:S_IFWHT #:S_ISUID #:S_ISGID #:S_ISVTX #:S_IRUSR #:S_IWUSR
   #:S_IXUSR
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

   #:umask
   #:chmod #:fchmod
   #:chown #:fchown #:lchown
   #:sync

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

   ;; processes
   #:system
   #:system-command
   #:getrusage
   #:timeval #:timeval-seconds #:timeval-micro-seconds
   #:rusage #:rusage-user #:rusage-system
   #:_exit
   #:exec
   #:fork
   #:wait
   #:run-program
   #:fork-and-exec
   #:getuid
   #:setuid
   #:getgid
   #:setgid
   #:getpid
   #:kill
   #:*signal-count*
   #:signal-name
   #:signal-description
   #:describe-signals
   #:getpgid
   #:setpgid
   #:popen
   #:with-process-output
   #:pipe

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

   ;; character coding / localization (or similar)
   #:char-width
   #:setlocale
   #:setup-locale-from-environment

   ;; misc
   #:exit-lisp
   #:missing-implementation

   ;; C stdio
   #:*stdin* #:*stdout* #:*stderr*
   #:fopen #:fclose #:fileno #:fflush
   #:fgetc #:getc #:getchar #:fgets #:gets
   #:printf #:fprintf #:sprintf #:snprintf
   #:fputc #:putc #:putchar #:fputs #:puts
   #:fread #:fwrite
   #:fscanf #:scanf #:sscanf
   #:fsetpos #:fgetpos #:fseek #:ftell
   #:perror #:setbuf #:ungetc

   ;; C library
   #:iswalnum #:iswalpha #:iswascii #:iswblank #:iswcntrl #:iswdigit
   #:iswgraph #:iswhexnumber #:iswideogram #:iswlower #:iswnumber
   #:iswphonogram #:iswprint #:iswpunct #:iswrune #:iswspace #:iswspecial
   #:iswupper #:iswxdigit

   #:isalnum #:isalpha #:isascii #:isblank #:iscntrl #:isdigit #:isgraph
   #:ishexnumber #:isideogram #:islower #:isnumber #:isphonogram #:isprint
   #:ispunct #:isrune #:isspace #:isspecial #:isupper #:isxdigit
   ))
(in-package :opsys)

;; Stuff to assist in feature frobbing and portability

(defmacro config-feature (f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew ,f *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun function-defined (sym pack)
    "True if SYM is an external function defined in package PACK."
    (multiple-value-bind (found-symbol status)
	(find-symbol (symbol-name sym) (find-package pack))
      (and found-symbol (eql status :external) (fboundp found-symbol)))))

;; @@@ I should probably really do this with an error type
(defun missing-implementation (sym)
  "Complain that something is missing."
  (error "Somebody needs to provide an implementation for ~a on ~a~%"
	 sym (lisp-implementation-type)))

;; This is in dlib too, but I don't want to depend on it.
;; I suppose we could use the one in alexandria, since but it's a dependency
;; of CFFI, but I'm a little nervous about that.
(defmacro define-constant (name value &optional doc)
  "Like defconstant but works with pendanticly anal SCBL."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

;; The comments about define-constant apply to this as well.
;; This has to be a macro so it can be used in read time expressions
;; in this file.
;; (defmacro featurep (symbol)
;;   "True if the SYMBOL is in *FEATURES*."
;;   `(not (null (find ,symbol *features*))))

;; This is so we can use the #_ reader macro on openmcl without it interfering
;; with other lisps. On other lisps we define it to do nothing.
#-openmcl (eval-when (:execute)
	    #. (set-dispatch-macro-character
		#\# #\_
		(flet ((pr (stream subchar arg)
			 (declare (ignore subchar arg))
			 (read stream t nil t)))
		  (setf (fdefinition '|#_-reader|) (function pr)))))

;; Generic things

#+darwin (config-feature :os-t-has-strerror-r)
#+darwin (config-feature :os-t-has-vfork)
#+(and darwin x86-64) (config-feature :64-bit-target)
#+(and linux x86-64) (config-feature :64-bit-target)
#+(or x86 ppc sparc arm (and (not 64-bit-target)))
  (config-feature :32-bit-target)

#+(and 32-bit-target 64-bit-target) (error "Can't be both 32 & 64 bits!")

;; C API types

(defctype time-t :long)
(defctype mode-t #+(or darwin sunos) :uint16 #+linux :unsigned-int)
(defctype size-t :unsigned-long)
(defctype uid-t :uint32)
(defctype gid-t :uint32)
(defctype pid-t :int)
(defctype wchar-t :int)
(defctype suseconds-t :int32)

#+darwin (defctype dev-t :int32)
#+sunos  (defctype dev-t :ulong)
#+linux  (defctype dev-t #+cffi-features:no-long-long :ulong
		         #-cffi-features:no-long-long :ullong)
(defctype nlink-t #+darwin :uint16 #+sunos :uint #+linux :uint)
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
#+(and linux 64-bit-target) (defctype off-t :int64)
#+(and linux (not 64-bit-target)) (defctype off-t :int32)
;#+(or sunos linux) (defctype off-t :long)
#-cffi-features:no-long-long (defctype quad-t :int64)
#+cffi-features:no-long-long (defctype quad-t :int32) ; @@@ XXX wrong!
(defctype blkcnt-t #+64-bit-target :int64 #+32-bit-target :int32)
(defctype blksize-t :int32)
(defctype fixpt-t :uint32)
(defctype sigset-t :uint32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling

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
;  (define-constant +EOPNOTSUPP+		+ENOTSUP+ "Operation not supported on socket")
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
  (define-constant +ELOCKUNMAPPED+	72	"locked lock was unmapped")

  (define-constant +ENOTACTIVE+ 73	"Facility is not active")
  (define-constant +EMULTIHOP+ 74	"multihop attempted")
  (define-constant +EBADMSG+ 77	"trying to read unreadable message")
  (define-constant +ENAMETOOLONG+ 78	"path name is too long")
  (define-constant +EOVERFLOW+ 79	"value too large to be stored in data type")
  (define-constant +ENOTUNIQ+ 80	"given log. name not unique")
  (define-constant +EBADFD+	81	"f.d. invalid for this operation")
  (define-constant +EREMCHG+	82	"Remote address changed")

  ;; shared library problems
  (define-constant +ELIBACC+	83	"Can't access a needed shared lib.")
  (define-constant +ELIBBAD+	84	"Accessing a corrupted shared lib.")
  (define-constant +ELIBSCN+	85	".lib section in a.out corrupted.")
  (define-constant +ELIBMAX+	86	"Attempting to link in too many libs.")
  (define-constant +ELIBEXEC+ 87	"Attempting to exec a shared library.")
  (define-constant +EILSEQ+	88	"Illegal byte sequence.")
  (define-constant +ENOSYS+	89	"Unsupported file system operation")
  (define-constant +ELOOP+	90	"Symbolic link loop")
  (define-constant +ERESTART+ 91	"Restartable system call")
  (define-constant +ESTRPIPE+ 92	"if pipe/FIFO, don't sleep in stream head")
  (define-constant +ENOTEMPTY+ 93	"directory not empty")
  (define-constant +EUSERS+	94	"Too many users (for UFS)")

  ;; BSD Networking Software
  ;;    argument errors
  (define-constant +ENOTSOCK+	95	"Socket operation on non-socket")
  (define-constant +EDESTADDRREQ+	96	"Destination address required")
  (define-constant +EMSGSIZE+	97	"Message too long")
  (define-constant +EPROTOTYPE+	98	"Protocol wrong type for socket")
  (define-constant +ENOPROTOOPT+	99	"Protocol not available")
  (define-constant +EPROTONOSUPPORT+	120	"Protocol not supported")
  (define-constant +ESOCKTNOSUPPORT+	121	"Socket type not supported")
  (define-constant +EOPNOTSUPP+	122	"Operation not supported on socket")
  (define-constant +EPFNOSUPPORT+	123	"Protocol family not supported")
  (define-constant +EAFNOSUPPORT+	124	"Address family not supported by protocol family")
  (define-constant +EADDRINUSE+	125	"Address already in use")
  (define-constant +EADDRNOTAVAIL+	126	"Can't assign requested address")
  ;; operational errors
  (define-constant +ENETDOWN+	127	"Network is down")
  (define-constant +ENETUNREACH+	128	"Network is unreachable")
  (define-constant +ENETRESET+	129	"Network dropped connection because of reset")
  (define-constant +ECONNABORTED+	130	"Software caused connection abort")
  (define-constant +ECONNRESET+	131	"Connection reset by peer")
  (define-constant +ENOBUFS+		132	"No buffer space available")
  (define-constant +EISCONN+		133	"Socket is already connected")
  (define-constant +ENOTCONN+	134	"Socket is not connected")
  ;; XENIX has 135 - 142
  (define-constant +ESHUTDOWN+	143	"Can't send after socket shutdown")
  (define-constant +ETOOMANYREFS+	144	"Too many references: can't splice")
  (define-constant +ETIMEDOUT+	145	"Connection timed out")
  (define-constant +ECONNREFUSED+	146	"Connection refused")
  (define-constant +EHOSTDOWN+	147	"Host is down")
  (define-constant +EHOSTUNREACH+	148	"No route to host")
  (define-constant +EWOULDBLOCK+	+EAGAIN+)
  (define-constant +EALREADY+	149	"operation already in progress")
  (define-constant +EINPROGRESS+	150	"operation now in progress")

  ;; SUN Network File System 
  (define-constant +ESTALE+		151	"Stale NFS file handle")
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

  ;; SUN Network File System
  (define-constant +ESTALE+		151 "Stale NFS file handle")
)

#+os-t-has-strerror-r
(defcfun ("strerror_r" strerror-r)
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

(define-condition posix-error (error)
  ((error-code
    :accessor posix-error-code
    :initarg :error-code
    :type (signed-byte 32)
    :documentation "The value of errno at the time of the error.")
   (format
    :accessor posix-error-format
    :initarg :format
    :type string
    :documentation "Format control for error reporting.")
   (arguments
    :accessor posix-error-arguments
    :initarg :arguments
    :type list
    :documentation "Format arguments for error reporting."))
  (:report (lambda (c s)
	     (if (posix-error-format c)
		 (format s "~? ~a"
			 (posix-error-format c)
			 (posix-error-arguments c)
			 (strerror (posix-error-code c)))
		 (format s "~a"
			 (strerror (posix-error-code c))))))
  (:documentation "An error from calling a POSIX function."))

(defun error-check (c &optional fmt &rest args)
  "Check if a system call returns an error value and signal it."
  (if (< c 0)
      (error 'posix-error :error-code *errno*
	     :format fmt :arguments args)
      c))

(defmacro syscall ((func &rest args))
  "Call a system function and signal a posix-error if it fails."
  `(error-check (,func ,@args)
		,(concatenate 'string (string-downcase func) ": ~s:") ',args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environmental information

(deftype string-designator ()
  "A designator for a string; that is, an object that denotes a string and
that is one of: a character (denoting a string that has the character as its
only element), a symbol (denoting the string that is its name), or a
string (denoting itself)."
  '(or string character symbol))

;; We could provide a cached value to make this faster, and update it
;; when the setenv below is used, but it would become inaccurate if
;; other code modifies the environment.

;; ??? Does it even make sense to have these as keywords??

#+(or sbcl clisp ccl ecl lispworks)
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
	#+clisp
	(cons (intern (car v) :keyword) (cdr v))
	))

;; _NSGetEnviron()
#+ecl
(progn
  (defcfun ("_NSGetEnviron" ns-get-environ) :pointer)
  (defun real-environ () (mem-ref (ns-get-environ) :pointer)))

#-ecl
(progn
;  #-clisp (defcvar ("environ" *real-environ*) :pointer "extern char **envrion;")
  #-clisp (defcvar ("environ" *real-environ*) :pointer)
  #+clisp (defcvar ("environ" *real-environ*) :pointer)
  (defun real-environ () *real-environ*))

#+(or ccl ecl lispworks)
(defun posix-environ ()
  (loop :with p = (real-environ) and s = nil
	:while (setf s (mem-ref p :string))
	:collect (progn
		  (setf p (inc-pointer p (foreign-type-size :pointer)))
		  s)))

(defun environ ()
  "Return an a-list of the system environment. The elements are conses (VARIABLE-NAME . VALUE), where VARIABLE-NAME is a keyword and VALUE is a string."
  #+clisp (convert-environ (ext:getenv))
  #+sbcl (convert-environ (sb-ext:posix-environ))
  #+ccl (convert-environ (posix-environ))
  #+ecl (convert-environ (posix-environ))
  #+lispworks (convert-environ (posix-environ))
  #+cmu ext:*environment-list*
  #-(or clisp sbcl ccl cmu ecl lispworks)
  (missing-implementation 'environ)
)

#+cmu (defcfun ("getenv" real-getenv) :string (name :string))

(defun getenv (var)
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
    #-(or clisp sbcl openmcl cmu ecl excl lispworks)
    (missing-implementation 'getenv)))

;; If we had environ and didn't have a getenv, or if it was faster
;; (which it isn't) we could define getenv as:
;; (cdr (assoc "TERM" (environ) :test #'string=))
;;
;; (defun vv (v) (cdr (assoc v (nos:environ) :test #'string=)))
;; (time (do ((i 0 (+ i 1))) ((> i 50000)) (nos:getenv "TERM")))
;; (time (do ((i 0 (+ i 1))) ((> i 50000)) (vv "TERM")))

#+(or sbcl cmu)
(defcfun ("setenv" real-setenv) :int
  (name :string) (value :string) (overwrite :int))

(defun setenv (var value)
  "Set the environtment variable named VAR to the string VALUE."
  (declare (type string-designator var)
	   (type string value))
  #+clisp (setf (ext:getenv var) value)
  #+openmcl (syscall (ccl::setenv var value))
  #+excl (setf (sys::getenv var) value)
  #+sbcl (syscall (real-setenv var value 1))
  #+cmu (syscall (real-setenv var value 1))
;   #+cmu (let ((v (assoc (intern (string-upcase var) :keyword)
; 			ext:*environment-list*)))
; 	  (if v (cdr v)))
  #+ecl (ext:setenv var value)
  #+lispworks (hcl:setenv var value)
  #-(or clisp openmcl excl sbcl ecl cmu lispworks)
  (declare (ignore var value))
  #-(or clisp openmcl excl sbcl ecl cmu lispworks)
  (missing-implementation 'setenv)
)

(defun lisp-args ()
  "Arguments given to when starting the lisp system."
  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  #+cmu ext:*command-line-strings*
  #+openmcl (ccl::command-line-arguments)
  #+excl (sys:command-line-arguments) 
  #+ecl (ext:command-args)
  #-(or sbcl clisp cmu openmcl excl ecl)
  (missing-implementation 'lisp-args)
)

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
;; The linuxy method of reading from /proc is even stupider. It doesn't solve
;; the problem of metadata, unless you count the text formated things, which
;; serves to demonstrate the conflict between machine readable and human
;; readable. It's really not hard to make a C interface that's nice, eg.
;; GObject. Of course again there's the issue of bloat. Linux's minimalism is
;; responsible for it being so adaptable to small devices. sbcl.core is 58MB,
;; whereas linux can probably still work in 4MB.
;;
;; BUT, it turns out that most of the metadata is in header files as well as
;; probably in the kernel in a hackish way. But a method for getting at these
;; isn't officially defined in the API. Why couldn't they have designed it in?
;;
;; BTW, all this sysctl stuff is probably #+darwin, since it hasn't been
;; tested on any other platforms. I suppose on linux we'll have to implement it
;; by reading from /proc/sys.
;;
;; If performance need to be improved, we could consider caching the
;; integer values by using sysctlnametomib.
;;
;; NOTE: This should probably come fairly early since we may use it later on
;; to determine configuration, such as kernel version, etc.

;; #include <sys/types.h>
;; #include <sys/sysctl.h>

;; int sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp,
;;           void *newp, size_t newlen);
;; int sysctlbyname(const char *name, void *oldp, size_t *oldlenp,
;;                  void *newp, size_t newlen);
;; int sysctlnametomib(const char *name, int *mibp, size_t *sizep);

(defcfun ("sysctl" real-sysctl)
    :int (name :pointer) (namelen :unsigned-int)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newlen size-t))

(defcfun ("sysctlbyname" real-sysctlbyname) :int (name :string)
	 (oldp :pointer) (oldlenp :pointer)
	 (newp :pointer) (newline size-t))

(defcfun "sysctlnametomib" :int (name :string) (mibp :pointer) (sizep :pointer))

;(defgeneric sysctl (name type)
;  (:documentation "Return the sysctl value named NAME. TYPE should be the C type
;of the value, as used by CFFI, such a :string :integer, etc.")
;  (:method

(defconstant +NGROUPS+ 16 "Max supplemental group id's")

(defcstruct foreign-loadavg
  (ldavg  fixpt-t :count 3)		; fixpt_t ldavg[3];
  (fscale :long))			; long    fscale;

(defcstruct foreign-ucred
  (cr_ref :int32)			; /* reference count */
  (cr_uid uid-t)			; /* effective user id */
  (cr_ngroups :short)			; /* number of groups */
  (cr_groups gid-t :count 16))	; /* groups */

(defcstruct foreign-pcred
  (pc_lock :char :count 72) ; char pc_lock[72]; /* opaque content */
  (pc_ucred :pointer)	    ; struct ucred *pc_ucred /* Current credentials. */
  (p_ruid   uid-t)	    ; /* Real user id. */
  (p_svuid  uid-t)	    ; /* Saved effective user id. */
  (p_rgid   gid-t)	    ; /* Real group id. */
  (p_svgid  gid-t)	    ; /* Saved effective group id. */
  (p_refcnt :int))	    ; /* Number of references. */

#|
(defcstruct foreign-kinfo-proc
  struct	extern_proc kp_proc;			/* proc structure */
  struct	eproc {
    struct	proc *e_paddr;		/* address of proc */
    struct	session *e_sess;	/* session pointer */
    struct	_pcred e_pcred;		/* process credentials */
    struct	_ucred e_ucred;		/* current credentials */
    struct	 vmspace e_vm;		/* address space */
    pid_t	e_ppid;			/* parent process id */
    pid_t	e_pgid;			/* process group id */
    short	e_jobc;			/* job control counter */
    dev_t	e_tdev;			/* controlling tty dev */
    pid_t	e_tpgid;		/* tty process group id */
    struct	session *e_tsess;	/* tty session pointer */
    ;; #define	WMESGLEN	7
   char	e_wmesg[WMESGLEN+1];	/* wchan message */
   segsz_t e_xsize;		/* text size */
   short	e_xrssize;		/* text rss */
   short	e_xccount;		/* text references */
   short	e_xswrss;
   int32_t	e_flag;
   ;; #define	EPROC_CTTY	0x01	/* controlling tty vnode active */
   ;; #define	EPROC_SLEADER	0x02	/* session leader */
   ;; #define	COMAPT_MAXLOGNAME	12
   char	e_login[COMAPT_MAXLOGNAME];	/* short setlogin() name */
   ;; #if CONFIG_LCTX
   pid_t	e_lcid;
   int32_t	e_spare[3];
   ;; #else
   int32_t	e_spare[4];
   ;; #endif
   } kp_eproc;
};
|#

(defun sysctl (name type)
  (with-foreign-object (oldlenp 'size-t 1)
    (real-sysctlbyname name (cffi:null-pointer) oldlenp (cffi:null-pointer) 0)
    (with-foreign-object (oldp :unsigned-char (mem-ref oldlenp 'size-t))
      (real-sysctlbyname name oldp oldlenp (cffi:null-pointer) 0)
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
  pw-expire
  )

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

(defcfun ("getpwuid" real-getpwuid) :pointer (uid uid-t))
(defun getpwuid (uid)
  (convert-passwd (real-getpwuid uid)))

;; @@@ Should use the re-entrant versions of these functions.

;; int
;; getpwuid_r(uid_t uid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result);

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

;; (defcfun ("endpwent" real-endpwent) :void)
;; (defun endpwent ()
;;   (real-endpwent))

(defcfun endpwent :void)
(defcfun setpwent :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group database
;; 

(defcstruct foreign-group
  "Group database entry."
  (gr_name	:string)
  (gr_passwd	:string)
  (gr_gid	gid-t)
  (gr_mem	:pointer)
)

(defstruct group
  "Group database entry."
  name
  passwd
  gid
  members
  )

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
	(make-group
	 :name   gr_name
	 :passwd gr_passwd
	 :gid    gr_gid
	 :members
	 (loop :with i = 0
	    :while (not (null-pointer-p (mem-aref gr_mem :pointer i)))
	    :collect (mem-aref gr_mem :string i)
	    :do (incf i))
	 ))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories

;; chroot

;; We need to use the posix version if there's no better way to do it
;; on the implementation.
;#+openmcl (config-feature :os-t-use-chdir)
;#+os-t-use-chdir (defcfun chdir :int (path :string))
#+(or openmcl sbcl) (defcfun chdir :int (path :string))

;; The real question is should this munge *default-pathname-defaults* ?
;; On implementations where "load" works from *default-pathname-defaults*
;; and not from the OS current, I say yes.

(defun change-directory (&optional path)
  "Change the current directory to DIR. Defaults to (user-homedir-pathname) ~
if not given."
  (when (not path)
    (setf path (enough-namestring (user-homedir-pathname))))
  (when (pathnamep path)
    (setf path (namestring path)))
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
  #-(or clisp excl openmcl sbcl cmu ecl lispworks) (missing-implementation 'change-directory)
)

(defcfun getcwd :pointer (buf :pointer) (size size-t))
(defcfun pathconf :long (path :string) (name :int))
(defconstant +PC-PATH-MAX+
	 #+(or darwin sunos) 5
	 #+linux 4)
#-(or darwin sunos linux) (missing-implementation 'PC-PATH-MAX)
;; Using the root "/" is kind of bogus, because it can depend on the 
;; but since we're using it to get the . This is where grovelling the 
;; MAXPATHLEN
(defun get-path-max () (pathconf "/" +PC-PATH-MAX+))
(defparameter *path-max* nil
  "Maximum number of bytes in a path.")

(defun current-directory ()
  #+excl (excl:current-directory)
  #+clisp (namestring (ext:cd))
  #+(or openmcl ccl) (ccl::current-directory-name)
  #+ecl (ext:getcwd)
  #+sbcl (progn
	   (when (not *path-max*)
	     (setf *path-max* (get-path-max)))
	   (let ((cwd (with-foreign-pointer-as-string (s *path-max*)
			(foreign-string-to-lisp (getcwd s *path-max*)))))
	     (if (not cwd)		; hopefully it's still valid
		 (error 'posix-error :error-code *errno*)
		 cwd)))
  #+cmu (ext:default-directory)
  #+lispworks (hcl:get-working-directory)
  #-(or clisp excl openmcl ccl sbcl cmu ecl lispworks)
  (missing-implementation 'current-directory)
)

(defmacro in-directory ((dir) &body body)
  "Evaluate the body with the current directory set to DIR."
  (let ((%old-dir (gensym "old-dir")))
  `(let ((,%old-dir (current-directory)))
     (unwind-protect
       (progn
         (change-directory ,dir)
         ,@body)
       (change-directory ,%old-dir)))))

#+openmcl (defcfun mkdir :int (path :string) (mode mode-t))

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
	  (if (or ;; They keep changing this shit!!
	       (and (function-defined '#:make-directory :posix)
		    (function-defined '#:delete-directory :posix))
	       (and (function-defined '#:make-directory :ext)
		    (function-defined '#:delete-directory :ext)))
	      (config-feature :os-t-has-new-dir)))

(defun make-directory (path &key (mode #o755))
;  #+clisp (declare (ignore mode)) #+clisp (ext:make-dir path)
  #+clisp (declare (ignore mode))
  #+(and clisp os-t-has-new-dir) (ext:make-directory path)
  #+(and clisp (not os-t-has-new-dir)) (ext:make-dir path)
  #+excl (excl:make-directory path mode)
  #+openmcl (syscall (mkdir path mode))
; #+ecl (ext:mkdir path mode) OLD
  #+ecl (si::mkdir path mode)
  #+sbcl (sb-unix:unix-mkdir path mode)
  #-(or clisp excl openmcl ecl sbcl) (declare (ignore mode path))
  #-(or clisp excl openmcl ecl sbcl) (missing-implementation 'make-directory)
)

#+openmcl (config-feature :os-t-use-rmdir)
#+os-t-use-rmdir (defcfun rmdir :int (path :string))

(defun delete-directory (path)
;  #+clisp (ext:delete-dir path)
;  #+clisp (ext:delete-directory path)
  #+(and clisp os-t-has-new-dir) (ext:delete-directory path)
  #+(and clisp (not os-t-has-new-dir)) (ext:delete-dir path)
  #+excl (excl:delete-directory path)
  #+os-t-use-rmdir (syscall (rmdir path))
;  #+ecl (ext:rmdir path)
  #+ecl (si:rmdir path)
  #+sbcl (sb-ext:delete-directory path)
  #-(or clisp excl openmcl ecl sbcl) (declare (ignore path))
  #-(or clisp excl openmcl ecl sbcl) (missing-implementation 'delete-directory)
)

;; It's hard to fathom how insanely shitty the Unix/POSIX interface to
;; directories is.

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
;; calls right under everybody and "NO ONE WILL KNOW", right. Wrong.
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

#+(or linux darwin) (config-feature :os-t-has-d-type)

;; If one of these is not defined, we just use strlen(d_name).
#+darwin (config-feature :os-t-has-namlen)
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
(defcfun ("closedir" closedir) :pointer (dirname :string))
#+(and darwin (not 64-bit-target))
(defcfun ("closedir$UNIX2003" closedir) :pointer (dirname :string))
#-darwin (defcfun closedir :int (dirp :pointer))

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

(defstruct dir-entry
  "Filesystem directory entry, like unix dirent."
  (name  nil :type (or string null))
  (type  nil :type (or keyword null))
  (inode nil :type (or integer null)))

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
     :count len)) ;;; @@@ old cffi didn't use the :count keyword, just the value
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
      ((= d_type DT_DIR)     :dir)
      ((= d_type DT_BLK)     :block-device)
      ((= d_type DT_REG)     :regular)
      ((= d_type DT_LNK)     :link)
      ((= d_type DT_SOCK)    :socket)
      ((= d_type DT_WHT)     :whiteout)
      (t :undefined)))
  #-os-t-has-d-type (declare (ignore ent))
  #-os-t-has-d-type :unknown)
			     
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
  :unknown :pipe :character-device :dir :block-device :regular :link :socket
  :whiteout :undefined
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
	  (error (strerror *errno*))
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
				       (equal (char (dirent-name ent) 0) #\.)))
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
	      (error (strerror *errno*)))))
	dir-list)
      (if (not (null-pointer-p dirp)) (closedir dirp)))))

(defun probe-directory (dir)
  "Something like probe-file but for directories."
  #+clisp (ext:probe-directory (make-pathname
				:directory (ext:absolute-pathname dir)))
  ;; On most implementations probe-file can handle directories.
  #+(or sbcl ccl cmu ecl lispworks)
  (probe-file dir)
  #-(or clisp sbcl ccl cmu ecl lispworks)
  (declare (ignore dir))
  #-(or clisp sbcl ccl cmu ecl lispworks)
  (missing-implementation 'probe-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

(defconstant O_RDONLY	#x0000 "Open for reading only")
(defconstant O_WRONLY	#x0001 "Open for writing only")
(defconstant O_RDWR	#x0002 "Open for reading and writing")
(defconstant O_ACCMODE	#x0003 "Mask for above modes")
(defconstant O_NONBLOCK	#+darwin #x0004 #+linux #o04000
	     "No delay")
(defconstant O_APPEND	#+darwin #x0008 #+linux #o02000
	     "Set append mode")
(defconstant O_ASYNC    #+darwin #x0040 #+linux #x020000
	     "Signal pgrp when data ready")
(defconstant O_SYNC	#+darwin #x0080 #+linux #o04010000
	     "Synchronous writes")
(defconstant O_SHLOCK	#x0010 "Atomically obtain a shared lock")
(defconstant O_EXLOCK	#x0020 "Atomically obtain an exclusive lock")
(defconstant O_CREAT	#+darwin #x0200 #+linux #o100
	     "Create if nonexistant")
(defconstant O_TRUNC	#+darwin #x0400 #+linux #o01000
	     "Truncate to zero length")
(defconstant O_EXCL	#+darwin #x0800 #+linux #o0200
	     "Error if create and already exists")
(defconstant O_EVTONLY  #x8000 "Requested for event notifications only")
(defconstant O_NOCTTY   #+darwin #x20000 #+linux #o0400
	     "Don't assign controlling terminal")

(defcfun ("open"  posix-open)  :int (path :string) (flags :int) (mode mode-t))
(defcfun ("close" posix-close) :int (fd :int))
(defcfun ("read"  posix-read)  :int (fd :int) (buf :pointer) (nbytes size-t))
(defcfun ("write" posix-write) :int (fd :int) (buf :pointer) (nbytes size-t))
(defcfun ("ioctl" posix-ioctl) :int (fd :int) (request :int) (arg :pointer))

(defmacro with-posix-file ((var filename flags mode) &body body)
  "Evaluate the body with the variable VAR bound to a posix file descriptor opened on FILENAME with FLAGS and MODE."
  `(let (,var)
     (unwind-protect
       (progn
	 (setf ,var (posix-open ,filename ,flags ,mode))
	 ,@body)
       (if (>= ,var 0)
	   (posix-close ,var)
	   (error-check ,var)))))

;; what about ioctl defines?

;; @@@ fcntl only for darwin so far
(defconstant F_DUPFD		0)
(defconstant F_DUPFD_CLOEXEC	#+darwin 67 #+linux 1030)
(defconstant F_GETFD		1)
(defconstant F_SETFD		2)
(defconstant F_GETFL		3)
(defconstant F_SETFL		4)
(defconstant F_GETOWN		#+darwin 5 #+linux 9)
(defconstant F_SETOWN		#+darwin 6 #+linux 8)
(defconstant F_GETPATH		50)
(defconstant F_PREALLOCATE	42)
(defconstant F_SETSIZE		43)
(defconstant F_RDADVISE		44)
(defconstant F_RDAHEAD		45)
(defconstant F_READBOOTSTRAP	46)
(defconstant F_WRITEBOOTSTRAP	47)
(defconstant F_NOCACHE		48)
(defconstant F_LOG2PHYS		49)
(defconstant F_LOG2PHYS_EXT	65)
(defconstant F_FULLFSYNC	51)
(defconstant F_FREEZE_FS	53)
(defconstant F_THAW_FS		54)
(defconstant F_GLOBAL_NOCACHE	55)
(defconstant F_ADDSIGS		59)
(defconstant F_MARKDEPENDENCY	60)
(defconstant F_ADDFILESIGS	61)
(defconstant F_NODIRECT		62)
(defconstant F_SETNOSIGPIPE	73)
(defconstant F_GETNOSIGPIPE	74)
(defconstant F_GETPROTECTIONCLASS	63)
(defconstant F_SETPROTECTIONCLASS	64)
(defconstant F_GETLKPID		66)
(defconstant F_SETBACKINGSTORE	70)
(defconstant F_GETPATH_MTMINFO	71)
(defconstant FD_CLOEXEC		1)
(defconstant F_GETLK		#+darwin 7 #+linux 5)
(defconstant F_SETLK		#+darwin 8 #+linux 6)
(defconstant F_SETLKW		#+darwin 9 #+linux 7)
(defconstant F_ALLOCATECONTIG	#x00000002)
(defconstant F_ALLOCATEALL	#x00000004)
(defconstant F_PEOFPOSMODE	3)
(defconstant F_VOLPOSMODE	4)

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

(defcfun fcntl :int (cmd :int) &rest)

;; I would like to have:
;;
;; (defun get-stream-file-name (stream)
;;   (with-foreign-string (path MAXPATHLEN)
;;     (syscall (fcntl F_GETPATH path)))
;;   )
;;
;; On sbeecil:
;; SB-IMPL::FD-STREAM-PATHNAME
;; SB-IMPL::FD-STREAM-FILE
;; SB-SYS:FD-STREAM-FD (fbound)
;; SB-SYS:FD-STREAM-P (fbound)
;;
;; On linux:
;; (readlink (format nil "/proc/~a/fd/~a" (getpid) fd))
;; ssize_t readlink(const char *path, char *buf, size_t bufsiz);
;;
;; Windows:
;;
;; (defcfun GetFileInformationByHandleEx BOOL #| WINAPI |#
;;  (hFile HANDLE) ;; In
;;  (FileInformationClass FILE_INFO_BY_HANDLE_CLASS) ;; In
;;  (lpFileInformation LPVOID)  ;; Out
;;  (dwBufferSize DWORD)  ;; In
;; )
;;
;; GetFileInformationByHandleEx  FileNameInfo,
;; (defcstruct _FILE_NAME_INFO
;;  DWORD FileNameLength;
;;  WCHAR FileName[1];
;; } FILE_NAME_INFO, *PFILE_NAME_INFO;
;;
;; typedef enum _FILE_INFO_BY_HANDLE_CLASS { 
;;   FileBasicInfo                   = 0,
;;   FileStandardInfo                = 1,
;;   FileNameInfo                    = 2,
;;   FileRenameInfo                  = 3,
;;   FileDispositionInfo             = 4,
;;   FileAllocationInfo              = 5,
;;   FileEndOfFileInfo               = 6,
;;   FileStreamInfo                  = 7,
;;   FileCompressionInfo             = 8,
;;   FileAttributeTagInfo            = 9,
;;   FileIdBothDirectoryInfo         = 10, // 0xA
;;   FileIdBothDirectoryRestartInfo  = 11, // 0xB
;;   FileIoPriorityHintInfo          = 12, // 0xC
;;   FileRemoteProtocolInfo          = 13, // 0xD
;;   FileFullDirectoryInfo           = 14, // 0xE
;;   FileFullDirectoryRestartInfo    = 15, // 0xF
;;   FileStorageInfo                 = 16, // 0x10
;;   FileAlignmentInfo               = 17, // 0x11
;;   FileIdInfo                      = 18, // 0x12
;;   FileIdExtdDirectoryInfo         = 19, // 0x13
;;   FileIdExtdDirectoryRestartInfo  = 20, // 0x14
;;   MaximumFileInfoByHandlesClass
;; } FILE_INFO_BY_HANDLE_CLASS, *PFILE_INFO_BY_HANDLE_CLASS;

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

(defparameter *mode-tags*
  '((is-fifo		   	"FIFO ")
    (is-character-device	"character special ")
    (is-directory		"directory ")
    (is-block-device		"block special ")
    (is-regular-file		"regular ")
    (is-symbolic-link		"symbolic link ")
    (is-socket			"socket ")
    (is-door			"door ")
    (is-whiteout		"whiteout ")
    (is-set-uid			"set-UID ")
    (is-sticky			"sticky "))
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

;; #include <sys/types.h>
;; #include <sys/stat.h>
;; #include <unistd.h>

;; 32bit stat -> __xstat -> fstatat64
;; 32bit ?     -> __xstat64 -> fstatat64

#+(and linux 32-bit-target)
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

#+(and linux 64-bit-target)
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

;; This should have the union of all OS's slots, so that portable code
;; can check for OS's specific slots with impunity.
(defstruct file-status
  device
  inode
  mode
  links
  uid
  gid
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

(defcfun
    (#+darwin "stat$INODE64"
     #+linux "__xstat"
     #-(or darwin linux) "stat"
     real-stat)
    :int (path :string) (buf (:pointer (:struct foreign-stat))))

(defun stat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-stat path stat-buf) "stat: ~s" path)
    (convert-stat stat-buf)))

(defcfun
    (#+darwin "lstat$INODE64"
     #+linux "__xlstat"
     #-(or darwin linux) "lstat"
     real-lstat)
    :int (path :string) (buf (:pointer (:struct foreign-stat))))

(defun lstat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-lstat path stat-buf) "lstat: ~s" path)
    (convert-stat stat-buf)))

(defcfun
    (#+darwin "fstat$INODE64"
     #+linux "__xfstat"
     #-(or darwin linux) "fstat"
     real-fstat)
    :int (fd :int) (buf (:pointer (:struct foreign-stat))))

(defun fstat (path)
  (with-foreign-object (stat-buf '(:struct foreign-stat))
    (error-check (real-fstat path stat-buf) "fstat: ~s" path)
    (convert-stat stat-buf)))

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

;; This borders on superstition.
(defcfun sync :void)

;; Questionable:
;; mmap/munmap/mprotect/madvise ???
;; File locking? : fcntl F_GETLK / F_GETLK F_SETLKW
;; utimes

;; What about splice:
;; splice, vmsplice, tee

;; Apple metadata crap:
;; searchfs
;; setattrlist/getattrlist
;; getdirentriesattr
;; getxattr/setxattr/removexattr/listxattr
;;
;; Look into file metadata libraries? which will work on windows, etc..

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes

(defcfun system :int (command :string))

;; @@@ This is very misleading because you would think that the args
;; would end up as separate arguments, but may not.

(defun system-command (cmd &optional args)
  "Run a system command. The command is generally given to whatever the
 system shell would be and the output and input are to the standard
 places."
  #+clisp (ext:run-shell-command (format nil "~a~{ ~a~}" cmd args))
;  #+sbcl (sb-ext:process-output (sb-ext:run-program cmd args :search t))
;  #+sbcl (sb-ext:process-exit-code
;	  (sb-ext:run-program cmd args :wait t :pty nil
;			      :search t :output t :input t :error t))
  #+sbcl (system (format nil "~a~{ ~a~}" cmd args))
  #+cmu (ext:process-exit-code (ext:run-program cmd args :output t :input t :error t))
;  #+openmcl (ccl::os-command (format nil "~a~{ ~a~}" cmd args))
; ccl failing for cmds that need a tty
;  #+ccl (ccl:run-program cmd args :input t :output t :input t :wait t)
  #+ccl (nos:system (format nil "~a~{ ~a~}" cmd args))
  ;; @@@ ccl shoud probably use ccl:os-command
;  #+ecl (ext:run-program cmd args)
  #+ecl (ext:system (format nil "~a~{ ~a~}" cmd args))
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args) :wait t)
  #+lispworks (system:call-system-showing-output
	       (format nil "~a~{ ~a~}" cmd args) :prefix "" :show-cmd nil)
  #-(or clisp sbcl cmu openmcl ecl excl lispworks)
  (missing-implementation 'system-command)
)

; XXX This is really all #+darwin
(defconstant wait-no-hang   #x01)
(defconstant wait-untraced  #x02)
(defconstant wait-stopped   #o0177)
(defconstant wait-core-flag #o0200)
(defun wait-status (s)		(logand #o0177 s))
(defun wait-if-exited (s)	(= (wait-status s) 0))
(defun wait-if-signaled (s)	(and (not (= (wait-status s) wait-stopped))
				     (not (= (wait-status s) 0))))
(defun wait-if-stopped (s)	(= (wait-status s) wait-stopped))
(defun wait-exit-status (s)	(ash s -8))
(defun wait-termination-signal (s) (wait-status s))
(defun wait-core-dump (s)	(not (= 0 (logand s wait-core-flag))))
(defun wait-stop-signal (s)	(ash s -8))

;; (defcstruct timeval
;;   (seconds time-t)
;;   (microseconds suseconds-t))

(defcstruct foreign-timeval
  "Time for timer."
  (tv-sec	time-t)
  (tv-usec	suseconds-t))

(defstruct timeval
  "Time for timer."
  seconds
  micro-seconds)

#+not ; old darwin?
(defcstruct foreign-rusage
  "Resource usage."
  (utime (:struct foreign-timeval))	; user time used
  (stime (:struct foreign-timeval))	; system time used
  (ixrss :long)				; integral shared memory size
  (idrss :long)				; integral unshared data
  (isrss :long)				; integral unshared stack
  (minflt :long)			; page reclaims
  (majflt :long)			; page faults
  (nswap :long)				; swaps
  (inblock :long)			; block input operations
  (oublock :long)			; block output operations
  (msgsnd :long)			; messages sent
  (msgrcv :long)			; messages recieved
  (nsignals :long)			; signals received
  (nvcsw :long)				; voluntary context switches
  (nivcsw :long))			; involuntary context switches

#+(or darwin linux)
(defcstruct foreign-rusage
  (ru_utime (:struct foreign-timeval))
  (ru_stime (:struct foreign-timeval))
  (ru_maxrss :long)
  (ru_ixrss :long)
  (ru_idrss :long)
  (ru_isrss :long)
  (ru_minflt :long)
  (ru_majflt :long)
  (ru_nswap :long)
  (ru_inblock :long)
  (ru_oublock :long)
  (ru_msgsnd :long)
  (ru_msgrcv :long)
  (ru_nsignals :long)
  (ru_nvcsw :long)
  (ru_nivcsw :long))

(defstruct rusage
  user
  system)

(defcfun ("getrusage" real-getrusage) :int (who :int)
	 (foreign-rusage-ptr (:pointer (:struct foreign-rusage))))

(defun getrusage (who)
  "Get resource usage. Return a struct TIMESPEC which has SECONDS and MICRO-SECONDS."
  (let ((val (case who
	       (:self 0)
	       ((:kids :children) -1))))
    (with-foreign-object (ru '(:struct foreign-rusage))
      (syscall (real-getrusage val ru))
      (with-foreign-slots ((ru_utime ru_stime) ru (:struct foreign-rusage))
	(make-rusage
	 :user (make-timeval :seconds (getf ru_utime 'tv-sec)
			     :micro-seconds (getf ru_utime 'tv-usec))
	 :system (make-timeval :seconds (getf ru_stime 'tv-sec)
			       :micro-seconds (getf ru_stime 'tv-usec)))))))


;#+os-t-has-vfork (defcfun ("vfork" fork) pid-t)
;#-os-t-has-vfork (defcfun ("fork" fork) pid-t)
(defcfun _exit :void (status :int))

(defcfun execvp :int (path :pointer) (args :pointer))
(defcfun execve :int (path :pointer) (args :pointer) (env :pointer))

(defun exec (path args)
  "Replace this program with executable in the string PATH. Run the program with arguments in ARGS, which should be a list of strings. By convention, the first argument should be the command name."
  (declare (type string path) (type list args))
  (let (c-path c-args (argc (length args)))
    (unwind-protect
      (progn
	(setf c-path (foreign-string-alloc path)
	      c-args (foreign-alloc :string :count (1+ argc)))
	(loop :for i :from 0 :below argc :do
	   (setf (mem-aref c-args :pointer i)
		 (foreign-string-alloc (elt args i))))
	(setf (mem-aref c-args :pointer argc) (null-pointer))
	(syscall (execve c-path c-args *real-environ*)))
      ;; Clean up
      (when (and c-path (not (null-pointer-p c-path)))
	(foreign-free c-path))
      (when (and c-args (not (null-pointer-p c-args)))
	(loop :for i :from 0 :below argc :do
	   (foreign-free (mem-aref c-args :string)))
	(foreign-free c-args)))))

(defcfun wait pid-t (status :pointer))
(defcfun waitpid pid-t (wpid pid-t) (status :pointer) (options :int))
(defcfun wait4 pid-t (status :pointer) (options :int)
	 (rusage (:pointer (:struct foreign-rusage))))

(defcfun ("fork" posix-fork) pid-t)

(defun fork ()
  #+sbcl (sb-sys:without-gcing (posix-fork))
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
;;stumpwm-0.9.7/contrib/sbclfix.lisp

(defun wait-and-report (child-pid)
  #-clisp
  (with-foreign-object (status-ptr :int 1)
    (setf (mem-ref status-ptr :int) 0)
    ;(format t "About to wait for ~d~%" child-pid)
    (let ((status 0) (wait-pid nil))
      (loop
	 :do (setf wait-pid (waitpid child-pid status-ptr 0))
	 :while (/= wait-pid child-pid)
	 :do
	 (format t "Back from wait wait-pid = ~d~%" wait-pid)
	 (if (= wait-pid -1)
	     (if (= *errno* +ECHILD+)
		 (progn
		   (format t "Nothing to wait for~%")
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
  (with-foreign-object (status-ptr :int 1)
    (setf (mem-ref status-ptr :int) 0)
;    (let ((wait-pid (waitpid child-pid status-ptr 0))
    (let ((wait-pid (wait status-ptr))
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

(defun fork-and-exec (cmd &optional args)
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
	    (when (= (execve path argv (real-environ)) -1)
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

(defun run-program (cmd &optional args)
;  #+(or clisp sbcl ccl) (fork-and-exec cmd args)
  #+clisp (ext:run-program cmd :arguments args)
  #+excl (excl:run-shell-command (concatenate 'vector (list cmd cmd) args)
				 :wait t)
  #+(or openmcl ccl)
  (let* ((proc (ccl::run-program cmd args
				 :sharing :external
				 :input t
				 :output t
				 :error t
				 :wait t)))
    (multiple-value-bind (status code-or-sig)
	(ccl::external-process-status proc)
      (case status
	(:stopped
	 (error "Process stopped. PID = ~d" (ccl::external-process-id proc)))
	(:signaled
	 (error "Process got signal ~d. PID = ~d" code-or-sig
		(ccl::external-process-id proc)))
	(:running
	 (error "Process running. PID = ~d" (ccl::external-process-id proc)))
	(:exited
	 ;; I dunno why it seems to return 71 when it can't exec the
	 ;; program.
	 (if (and (numberp code-or-sig) (= code-or-sig 71))
	     nil
	     code-or-sig))
	(t
	 (error "Process has unknown status ~a" status)))))

  #+sbcl (sb-ext:process-exit-code
	  (sb-ext:run-program cmd args
			      :search t :output t :input t :error t :pty nil))
  #+cmu (ext:process-output (ext:run-program cmd args))
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (system:run-shell-command
		   (concatenate 'vector (list cmd) args)
		   :output :stream
		   :wait t)
		result)
  #-(or clisp excl openmcl sbcl cmu lispworks)
  (missing-implementation 'run-program)
)

;; clisp decided to change names at some point
#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
	  (if (and (function-defined '#:uid :posix)
		   (function-defined '#:gid :posix))
	      (config-feature :os-t-has-new-uid-gid)))

#+ecl (config-feature :os-t-use-getuid)
#+ecl (config-feature :os-t-use-setuid)
#+ecl (config-feature :os-t-use-getgid)
#+ecl (config-feature :os-t-use-setgid)

(defcfun ("getuid" real-getuid) uid-t)
(defcfun ("getgid" real-getgid) uid-t)
(defcfun ("setuid" real-setuid) :int (uid uid-t))
(defcfun ("setgid" real-setgid) :int (gid uid-t))

(defun getuid ()
  #+ccl (ccl::getuid)
  #+excl (excl.osi:getuid)
;  #+clisp (posix:getuid)
;  #+clisp (posix:uid)
  #+(and clisp (not os-t-has-new-uid-gid)) (posix:getuid)
  #+(and clisp os-t-has-new-uid-gid) (posix:uid)
  #+cmu (unix:unix-getuid)
  #+sbcl (sb-unix:unix-getuid)
  #+ecl (real-getuid)
  #+lispworks (real-getuid)
  #-(or openmcl excl clisp cmu sbcl ecl lispworks)
  (missing-implementation 'getuid)
)

(defun setuid (uid)
  #+ccl (syscall (ccl::setuid uid))
  #+excl (excl.osi:setuid uid)
;  #+clisp (setf (posix:setuid) uid)
;  #+clisp (setf (posix:uid) uid)
  #+(and clisp (not os-t-has-new-uid-gid)) (setf (posix:getuid) uid)
  #+(and clisp os-t-has-new-uid-gid) (setf (posix:uid) uid)
  #-(or ccl excl clisp) (declare (ignore uid))
  #-(or ccl excl clisp) (missing-implementation 'setuid)
)

(defun getgid ()
  #+ccl (#_getgid)
  #+excl (excl.osi:getgid)
  #+sbcl (real-getgid)
  #+(and clisp (not os-t-has-new-uid-gid)) (posix:getgid)
  #+(and clisp os-t-has-new-uid-gid) (posix:gid)
  #-(or ccl excl sbcl clisp) (missing-implementation 'getgid)
)

(defun setgid (gid)
  #+openmcl (syscall (ccl::setgid gid))
  #+excl (excl.osi:setgid gid)
;  #+clisp (setf (posix:getgid) gid)
;  #+clisp (setf (posix:gid) gid)
  #+(and clisp (not os-t-has-new-uid-gid)) (setf (posix:getgid) gid)
  #+(and clisp os-t-has-new-uid-gid) (setf (posix:gid) gid)
  #-(or openmcl excl clisp) (declare (ignore gid))
  #-(or openmcl excl clisp) (missing-implementation 'setgid)
)

(defun getpid ()
  #+openmcl (#_getpid)
  #+excl (excl.osi:getpid)
  #+clisp (sys::process-id)
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-unix:unix-getpid)
  #+ecl (ext:getpid)
  #-(or openmcl excl clisp cmu sbcl ecl) (missing-implementation 'getpid)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signals

#+sunos (defcvar ("_sys_siglistn" *nsig*) :int)
#+sunos (defcvar ("_sys_siglistp" sys-siglist) :pointer)

#+darwin (defcvar ("sys_siglist" sys-siglist) :pointer)
#+darwin (defcvar ("sys_signame" sys-signame) :pointer)

;; @@@ How can we figure this out? Do we really have to grovel?
;; Shouldn't this be available from sysconf or something?
;; In openmcl this is available as #$NSIG
(defparameter *signal-count*
  #+darwin 32
  #+sunos *nsig*
  #-(or darwin sunos) nil		; @@@ or perhaps 0?
  "Number of signal types, a.k.a. NSIG."
)

#+sunos (defparameter SIG2STR_MAX 64 "Bytes for signal name.")
#+sunos (defcfun sig2str :int (signum :int) (str :pointer))

(defun signal-name (sig)
  #+sunos (with-foreign-pointer-as-string (s SIG2STR_MAX)
	    (sig2str sig s)
	    s)
  #+darwin
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-signame) :pointer sig)))
  #-(or darwin sunos) (declare (ignore sig))
  #-(or darwin sunos) (missing-implementation 'signal-name)
)

#+sunos (defcfun strsignal :string (sig :int))

(defun signal-description (sig)
  #+sunos (strsignal sig)
  #+darwin
  (if (< sig *signal-count*)
      (foreign-string-to-lisp
       (mem-aref (get-var-pointer 'sys-siglist) :pointer sig)))
  #-(or darwin sunos) (declare (ignore sig))
  #-(or darwin sunos) (missing-implementation 'signal-description)
)

(defun describe-signals ()
  "List the POSIX signals that are known to the operating system."
  (loop :for i :from 1 :to *signal-count*
        :do (format t "~2a ~8a ~a~%"
		   i (signal-name i) (signal-description i))))

;(defparameter signal-names (make-hash-table 
;(defun signal-number (name)

; #+os-t-has-siglist
; (eval-when (:compile-toplevel :load-toplevel :execute)
;   (loop for i from 0 to *signal-count*
;     do
;     `(defparameter ,(signal-name i) ,i)))


(defcstruct sigaction
  "What to do with a signal, as given to sigaction(2)."
  (sa_handler :pointer)	       ; For our purposes it's the same as sa_sigaction
  (sa_mask sigset-t)
  (sa_flags :int))

(defconstant SIG_DFL  0 "Default action.")
(defconstant SIG_IGN  1 "Ignore the signal.")
(defconstant SIG_HOLD 5 "Hold on to the signal for later.")
(defconstant SIG_ERR -1 "Error?")

(defconstant SA_ONSTACK   #x0001 "Deliver on a stack, given with sigaltstack.")
(defconstant SA_RESTART   #x0002 "Restart system on signal return.")
(defconstant SA_RESETHAND #x0004 "Reset handler to SIG_DFL on delivery.")
(defconstant SA_NOCLDSTOP #x0008 "SIGCHLD only on process exit, not on stops.")
(defconstant SA_NODEFER   #x0010 "Don't mask the signal being delivered.")
(defconstant SA_NOCLDWAIT #x0020 "Don't create zombies. Wait returns ECHILD.")
(defconstant SA_SIGINFO   #x0040 "Deliver with sa_siginfo args.")

(defcfun sigaction :int (sig :int) (action :pointer) (old-action :pointer))

(defun kill (pid sig)
  #+clisp (posix:kill pid sig)
  #+openmcl (#_kill pid sig)
  #+cmu (unix:unix-kill pid sig)
  #+sbcl (sb-unix:unix-kill pid sig)
  #-(or clisp openmcl cmu sbcl) (declare (ignore pid sig))
  #-(or clisp openmcl cmu sbcl) (missing-implementation 'kill))

(defcfun killpg :int (process-group pid-t) (signal :int))

;(sb-sys:enable-interrupt sb-posix:sigwinch #'update-window-size)
;(defun update-window-size (sig code scp)
; (declare (ignore sig code scp))
;)

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

;; This is usually done only when you "log in", like with the window system or
;; like in the ssh deamon. See getlogin.
#+darwin (defcfun setlogin :int (name :string))

;; getgroups/setgroups
;; setpriority
;; getrlimit/setrlimit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IPC

;; pipes

(defun popen (cmd args)
  "Return an input stream with the output of the system command."
  #+clisp (ext:run-shell-command
	   (format nil "~a~{ ~a~}" cmd args) :output :stream)
  #+sbcl (sb-ext:process-output
	  (sb-ext:run-program cmd args :output :stream :search t
			      ;; @@@ What should we do? Added what version?
;;			      :external-format '(:utf-8 :replacement #\?)
			      ))
  #+cmu (ext:process-output
	 (ext:run-program cmd args :output :stream))
  #+openmcl (ccl::external-process-output-stream
	     (ccl::run-program cmd args :output :stream))
  #+ecl (ext:run-program cmd args)
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" cmd args)
				 :output :stream :wait t)
  #+lispworks (multiple-value-bind (result str err-str pid)
		  (declare (ignore result err-str pid))
		  (system:run-shell-command
		   (concatenate 'vector (list cmd) args)
		   :output :stream
		   :wait t)
		str)
  #-(or clisp sbcl cmu openmcl ecl excl lispworks)
  (missing-implementation 'popen))

(defmacro with-process-output ((var cmd args) &body body)
  "Evaluate the body with the variable VAR bound to a stream with the output from the system command CMD with the arguments ARGS."
  `(let (,var)
    (unwind-protect
	 (progn
	   (setf ,var (popen ,cmd ,args))
	   ,@body)
      (if ,var (close ,var)))))

(defcfun ("pipe" real-pipe) :int (pipefd :pointer))

(defun pipe ()
  (with-foreign-object (fd :int 2)
    (syscall (real-pipe fd))
    (values (mem-aref fd :int 0) (mem-aref fd :int 1))))

;; There's already "standard-ish" lisp networking? Right? Please?
;; NO THERE ISN"T.
;;
;; I really can't believe I'm doing this. But...

;;
;; Sockets!
;;





;; semaphores: semsys/semctl/semget/semop/semconfig
;; messages?: msgsys/msctl/semget/semop/semconfig
;; shared mem: shmsys/shmat/shmctl/shmdt/shmget
;;
;; POSIX Realtime Extension?
;; shm_open.. named semaphores
;; sem_open...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

;; This is probably best provided in relation to some kind of "event loop"
;; setitimer/getitimer

;; int gettimeofday(struct timeval *restrict tp, void *restrict tzp);
;; int settimeofday(const struct timeval *tp, const struct timezone *tzp);

(defconstant unix-to-universal-time 2208988800)

;; struct timezone {
;; int     tz_minuteswest; /* of Greenwich */
;; int     tz_dsttime;     /* type of dst correction to apply */
;; };


;; DESCRIPTION
;;      The system's notion of the current Greenwich time and the current time
;;      zone is obtained with the gettimeofday() call, and set with the
;;      settimeofday() call.  The time is expressed in seconds and microseconds
;;      since midnight (0 hour), January 1, 1970.  The resolution of the system
;;      clock is hardware dependent, and the time may be updated continuously or
;;      in ``ticks.''  If tp is NULL and tzp is non-NULL, gettimeofday() will
;;      populate the timezone struct in tzp.  If tp is non-NULL and tzp is NULL,
;;      then only the timeval struct in tp is populated. If both tp and tzp are
;;      NULL, nothing is returned.

;;      The structures pointed to by tp and tzp are defined in <sys/time.h> as:



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
      (with-foreign-slots ((tv-sec tv-usec) tv (:struct foreign-timeval))
	(multiple-value-bind (sec frac) (truncate timeout)
	  (setf tv-sec sec
		tv-usec (truncate (* frac 1000000))))
;	(format t "timeval ~d ~d~%" tv-sec tv-usec)
	)
      (when (= -1 (setf ret-val (unix-select (1+ nfds)
					     read-fds write-fds err-fds
					     tv)))
	(error "Select failed ~a" (strerror *errno*)))
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
	(error "Poll failed ~a" (strerror *errno*)))
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

;; It might be nice if could do this on a Lisp stream.
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
	(setf fd (posix-open "/dev/tty" O_RDWR #o600))
	(format t "Foo ->")
	(finish-output)
	(listen-for 5 fd))
      (when fd
	(posix-close fd)))))

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

;; #include <sys/param.h>
;; #include <sys/mount.h>

;; int statfs(const char *path, struct statfs *buf);
;; typedef struct { int32_t val[2]; } fsid_t;

#-64-bit-target
(eval-when (:compile-toplevel :load-toplevel :execute)
   (define-constant +MFSNAMELEN+ 15)	; length of fs type name, not inc. nul
   (define-constant +MNAMELEN+ 90)	; length of buffer for returned name
   (define-constant +MFSTYPENAMELEN+ +MFSNAMELEN+)
   (define-constant +MAXPATHLEN+ +MNAMELEN+)
)

;; when _DARWIN_FEATURE_64_BIT_INODE is NOT defined
#-64-bit-target
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

#+64-bit-target
(eval-when (:compile-toplevel :load-toplevel :execute)
   (define-constant +MFSTYPENAMELEN+ 16); length of fs type name, including nul
   (define-constant +MAXPATHLEN+ 1024)	; length of buffer for returned name
)

#+64-bit-target
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

;; (define-foreign-type foreign-statfs-type ()
;;   ()
;;   (:actual-type :pointer)
;;   (:simple-parser foreign-statfs)
;; )

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
  mntfromname
)

;; @@@ I shouldn't really have to do this?
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

;(defmethod translate-from-foreign (statfs (type foreign-statfs-type))
;  (convert-statfs statfs))

#+64-bit-target
(defcfun ("statfs$INODE64" real-statfs) :int (path :string)
	 (buf (:pointer (:struct foreign-statfs))))
#+32-bit-target
(defcfun ("statfs" real-statfs) :int (path :string)
	 (buf (:pointer (:struct foreign-statfs))))
(defun statfs (path)
  (with-foreign-object (buf '(:struct foreign-statfs))
    (syscall (real-statfs path buf))
    (convert-statfs buf)))

;; int getmntinfo(struct statfs **mntbufp, int flags);
#+64-bit-target
(defcfun ("getmntinfo$INODE64" real-getmntinfo)
    :int (mntbufp :pointer) (flags :int))
#+32-bit-target
(defcfun ("getmntinfo" real-getmntinfo)
    :int (mntbufp :pointer) (flags :int))
(defun getmntinfo (&optional (flags 0))
  (with-foreign-object (ptr :pointer)
    (let ((n (syscall (real-getmntinfo ptr flags))))
      (loop :for i :from 0 :below n
	 :collect (convert-statfs
		   (mem-aptr (mem-ref ptr :pointer)
			     '(:struct foreign-statfs) i))))))

;; getfsent [BSD]

;; #include <fstab.h>
;; struct fstab * getfsent(void);
;; struct fstab * getfsspec(const char *spec);
;; struct fstab * getfsfile(const char *file);
;; int setfsent(void);
;; void endfsent(void);

(define-constant +fs-types+ '(:hfs :nfs :msdos :cd9660 :fdesc :union))

(defcstruct foreign-fstab-struct
  "File system table."
  (fs_spec	:string)		; block special device name
  (fs_file	:string)		; file system path prefix
  (fs_vfstype	:string)		; File system type, ufs, nfs
  (fs_mntops	:string)		; Mount options ala -o
  (fs_type	:string)		; FSTAB_* from fs_mntops
  (fs_freq	:int)			; dump frequency, in days
  (fs_passno	:int)			; pass number on parallel fsck
)

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
  (:simple-parser foreign-fstab)
)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling and debugging?
;; Should use other lispy tools?
;; For profiling you probably need to use tools specific to the implementation.

;; profil
;; ptrace

;; Weird/simulation/emulation
;; syscall

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character coding / localization

(defconstant +LC-ALL+      0 "Entire locale generally.")
(defconstant +LC-COLLATE+  1 "String collation routines.")
(defconstant +LC-CTYPE+    2 "Character types. Upper and lower case, ~
			      alphabetic or non-alphabetic characters, etc.")
(defconstant +LC-MONETARY+ 3 "For formatting monetary values.")
(defconstant +LC-NUMERIC+  4 "For formatting numbers.  This controls the ~
			      formatting of decimal points in input and ~
			      output of floating point numbers.")
(defconstant +LC-TIME+     5 "For formatting dates and times.")
(defconstant +LC-MESSAGES+ 6 "For message catalogs, see catopen(3) function.")
(defconstant +LC-LAST+     7 "Highest locale category + 1.")

(defcfun ("setlocale" real-setlocale) :string (category :int) (locale :string))

(define-constant +lc-category-alist+ `((:all      . ,+LC-ALL+)
				       (:collate  . ,+LC-COLLATE+)
				       (:ctype    . ,+LC-CTYPE+)
				       (:monetary . ,+LC-MONETARY+)
				       (:numeric  . ,+LC-NUMERIC+)
				       (:time     . ,+LC-TIME+)
				       (:messages . ,+LC-MESSAGES+)))

(defun lc-category (c)
  "Return an valid integer locale category given a keyword. If the argument ~
   is already a valid integer locale category, it is returned, otherwise an ~
   error is signaled."
  (ctypecase c
   (number
    (if (and (>= c 0) (< c +LC-LAST+))
	c
	(error "Locale category ~s out of range" c)))
   (keyword
    (or (cdr (assoc c +lc-category-alist+))
	(error "Invalid locale category ~s" c)))))

(defun setlocale (category &optional locale)
  "See manpage for setlocale(3). CATEGORY can be a keyword or integer."
  (let ((result (real-setlocale (lc-category category)
				(or locale (cffi:null-pointer)))))
    (or result
	(error "setlocale of locale ~s for category ~a failed."
	       locale category))))

(define-constant +lc-env-type+ `((:all      . "LANG")
				 (:collate  . "LC_COLLATE")
				 (:ctype    . "LC_CTYPE")
				 (:monetary . "LC_MONETARY")
				 (:numeric  . "LC_NUMERIC")
				 (:time     . "LC_TIME")
				 (:messages . "LC_MESSAGES")))

(defun setup-locale-from-environment ()
  (loop :with e = nil
	:for f :in +lc-env-type+
	:do
	(when (setf e (getenv (cdr f)))
	  (setlocale (car f) e))))

#+clisp (shadowing-import 'ext:char-width)
#+clisp (export 'char-width)
#-clisp
(defcfun wcwidth :int (wc wchar-t))
#-clisp
(defun char-width (char)
  "Return the column width of CHAR. If it's not working as expected, you ~
   probably have to call setlocale first."
  (if (graphic-char-p char)		; assume this is equivalent to iswprint
      (wcwidth (char-code char))
      (error "Can't determine the width of a non-graphic character: ~s" char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

;; Not exactly an operating system function, but implementation specific
(defun exit-lisp ()
  "Halt the entire Lisp system." ;; But not necessarily the operating system.
  #+openmcl (ccl::quit 0)
  #+cmu (ext:quit)
;  #+sbcl (sb-ext:quit)
  #+sbcl (sb-ext:exit)
  #+excl (excl:exit)
  #+clisp (funcall 'ext:quit)
  #+ecl (ext:quit)
  #-(or openmcl cmu sbcl excl clisp ecl) (missing-implementation 'exit-lisp)
  )

;; This isn't really OS specific, but implementation specific.
;(defun stream-file-name

;; Go thru *features* and get rid of all our temporary configuration.
; (setf *features*
;       (delete-if #'(lambda (x)
; 		     (let ((s (string x)))
; 		       (string= s "OS-T-" :end1 (min 5 (length s)))))
; 		 *features*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; limited stdio support
;;
;; I think this should only be used for compatibility / interoperability.
;; Use Lisp streams (of some sort) for normal code. For example, other
;; libraries sometimes operate on stdio FILE pointers, such as, curses,
;; bzip2, openssl, etc.

; (define-foreign-library libc
;     ((:and cygwin unix)	(:default "cygwin1")
;      (unix		(:default "libc"))))

;; #+(and unix cygwin)
;; (define-foreign-library libc (:default "cygwin1"))
;; #+(and unix (not cygwin))
;; (define-foreign-library libc (:default "libc"))

;; (use-foreign-library libc)

(defctype file-ptr :pointer)		; (FILE *)
(defctype fpos-t
    #+(and darwin 64-bit-target) :int64
    #-(and darwin 64-bit-target) :int32)

(defcvar ("stdin"  *stdin*)  file-ptr)
(defcvar ("stdout" *stdout*) file-ptr)
(defcvar ("stderr" *stderr*) file-ptr)

(defcfun fopen file-ptr (path :string) (mode :string))
(defcfun fclose :int (file file-ptr))
(defcfun fileno :int (file file-ptr))
(defcfun fflush :int (file file-ptr))
(defcfun fgetc :int (file file-ptr))
(defcfun getc :int (file file-ptr))
(defcfun getchar :int)
(defcfun fgets :string (str :string) (size :int) (file file-ptr))
(defcfun gets :string (str :string))
(defcfun printf :int (format :string) &rest)
(defcfun fprintf :int (file file-ptr) (format :string) &rest)
(defcfun sprintf :int (str :string) (format :string) &rest)
(defcfun snprintf :int (str :string) (size size-t) (format :string) &rest)
(defcfun fputc :int (c :int) (file file-ptr))
(defcfun putc :int (c :int) (file file-ptr))
(defcfun putchar :int (c :int))
(defcfun fputs :int (s :string) (file file-ptr))
(defcfun puts :int (s :string))
(defcfun fread size-t (ptr :pointer) (size size-t) (nitems size-t)
	 (file file-ptr))
(defcfun fwrite size-t (ptr :pointer) (size size-t) (nitems size-t)
	 (file file-ptr))
(defcfun fscanf :int (file file-ptr) (format :string) &rest)
(defcfun scanf :int  (format :string) &rest)
(defcfun sscanf :int (s :string) (format :string) &rest)

(defcfun fsetpos :int (file file-ptr) (pos fpos-t))
(defcfun fgetpos :int (file file-ptr) (pos fpos-t))
(defcfun fseek :int (file file-ptr) (offset :long) (whence :int))
(defcfun ftell :int (file file-ptr))

(defcfun perror :void (s :string))

(defcfun setbuf :int (file file-ptr) (buf :string))
(defcfun ungetc :int (file file-ptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctype & wctype - character classification from the standard C library

(defctype wint-t :int32)

(defcfun iswalnum :int (wc wint-t))
(defcfun iswalpha :int (wc wint-t))
(defcfun iswascii :int (wc wint-t))
(defcfun iswblank :int (wc wint-t))
(defcfun iswcntrl :int (wc wint-t))
(defcfun iswdigit :int (wc wint-t))
(defcfun iswgraph :int (wc wint-t))
(defcfun iswhexnumber :int (wc wint-t))
(defcfun iswideogram :int (wc wint-t))
(defcfun iswlower :int (wc wint-t))
(defcfun iswnumber :int (wc wint-t))
(defcfun iswphonogram :int (wc wint-t))
(defcfun iswprint :int (wc wint-t))
(defcfun iswpunct :int (wc wint-t))
(defcfun iswrune :int (wc wint-t))
(defcfun iswspace :int (wc wint-t))
(defcfun iswspecial :int (wc wint-t))
(defcfun iswupper :int (wc wint-t))
(defcfun iswxdigit :int (wc wint-t))

(defcfun isalnum :int (c :int))
(defcfun isalpha :int (c :int))
(defcfun isascii :int (c :int))
(defcfun isblank :int (c :int))
(defcfun iscntrl :int (c :int))
(defcfun isdigit :int (c :int))
(defcfun isgraph :int (c :int))
(defcfun ishexnumber :int (c :int))
(defcfun isideogram :int (c :int))
(defcfun islower :int (c :int))
(defcfun isnumber :int (c :int))
(defcfun isphonogram :int (c :int))
(defcfun isprint :int (c :int))
(defcfun ispunct :int (c :int))
(defcfun isrune :int (c :int))
(defcfun isspace :int (c :int))
(defcfun isspecial :int (c :int))
(defcfun isupper :int (c :int))
(defcfun isxdigit :int (c :int))

;; @@@ Should get rid of temporary features :os-t-*

;; EOF
