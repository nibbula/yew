;;;
;;; opsys-unix-test.lisp - Tests for OPSYS-UNIX.
;;;

(defpackage :opsys-unix-test
  (:documentation "Tests for OPSYS-UNIX.")
  (:use :cl :test :opsys-unix)
  (:export
   #:run
   ))
(in-package :opsys-unix-test)

(defparameter *unlikely* #xfffffff
  "Improabbly high.")

;; macros

;; types

;; errors

(defun fail-to-open-a-file ()
  (uos::posix-open (format nil "/an-hopefully/nonexistant/file/ok~s"
			   (random *unlikely*))
		   uos::+O_RDONLY+ 0))

(deftests (opsys-unix-errors-1 :doc "Test errors.")
  "Check that getting errno works."
  (let ((result (fail-to-open-a-file)))
    (and (= result -1)
	 (= (uos::errno) uos:+ENOENT+)))
  "Check that getting an error message works."
  (stringp (uos:strerror))
  "Check that getting an error message from errno works."
  (stringp (uos:strerror (uos::errno)))
  "Check that we can get a string for all known error numbers."
  (every #'stringp
	 (loop :for e :in uos:*errors* :collect (uos:strerror (symbol-value e))))
  "Make sure the syscall macro works and signals a posix-error."
  (got-condition (uos:posix-error)
    (syscall (fail-to-open-a-file)))
  )

;; environmental

(deftests (opsys-unix-environment-1 :doc "Test environmental data.")
  "Test the environment function."
  (every (lambda (_) (and (consp _)
			  (stringp (cdr _))))
	 (uos:environment))
  "This could fail if the system doesn't have PATH."
  (stringp (uos:env "PATH"))
  "Make sure the other env aliases work the same."
  (and (equal (uos:env "PATH") (uos:environment-variable "PATH")))
  (and (equal (uos:env "PATH") (uos:getenv "PATH")))
  "This test could actually fail if you tried hard to make it fail."
  (not (env (format nil "A_RRREALLY_UNLIKELY_ENV_VAR_~s_X"
		    (random *unlikely*))))
  "Make sure setenv works."
  (let ((var-name (format nil "Spooty~a" (random *unlikely*)))
	(value (format nil "pig~a" (random *unlikely*)))
	one two)
    (uos:setenv var-name value)
    (setf one (equal (uos:getenv var-name) value))
    (uos:unsetenv var-name)
    (setf two (equal (uos:getenv var-name) nil))
    (and one two))
  "Make sure setf env works."
  (let ((var-name (format nil "Spooty~a" (random *unlikely*)))
	(value (format nil "pig~a" (random *unlikely*)))
	one two)
    (setf (uos:env var-name) value)
    (setf one (equal (uos:env var-name) value))
    (setf (uos:env var-name) nil)
    (setf two (equal (uos:env var-name) nil))
    (and one two)))

;; sysctl
;; sysctl-by-number
(deftests (opsys-unix-sysctl-1 :doc "Test sysctl.")
  "Test sysctl if we have it?"
	;; What should we test?
	;; (and
	;;   (equal (uos:sysctl "foo" "bar") "baz")
	;;   (equal (uos:sysctl "foo" "bar") "baz"))
  t) ;; @@@ add some things

(deftests (opsys-unix-environment-2 :doc "Test environmental data 2.")
  (and (integerp (uos:getpagesize)) (> (uos:getpagesize) 1))
  (and (integerp (uos:memory-page-size)) (> (uos:memory-page-size) 1))
  (and (integerp (uos:processor-count)) (>= (uos:processor-count) 1))
  ;; @@@ #+linux getauxval
  "Simple test of sysconf"
  (and (integerp (uos:sysconf uos::+SC-ARG-MAX+))
       (> (uos:sysconf uos::+SC-ARG-MAX+) 3))
  (and (integerp (uos:sysconf uos::+SC-OPEN-MAX+))
       (> (uos:sysconf uos::+SC-ARG-MAX+) 5))
  ;; sysinfo
  #+linux "Test sysinfo"
  #+linux (uos::sysinfo-p (uos:sysinfo))
  ;; system-info-names
  ;; system-info-description
  ;; get-system-info
  "Test uname"
  (and (uos::uname-p (uos:uname)) (stringp (uos:uname-nodename (uos:uname))))
  "Test os-machine-*"
  (stringp (os-machine-instance))
  (stringp (os-machine-type))
  (and (stringp (os-machine-version)) (string/= (os-machine-version) "unknown"))
  "Test os-software-*"
  (stringp (os-software-type))
  (stringp (os-software-version))
  )

;; time
(deftests (opsys-unix-time-1 :doc "Test time.")
  "Test gettimeofday"
  (and (uos::timeval-p (uos:gettimeofday))
       (> (uos::timeval-seconds (uos:gettimeofday)) 2)) ; how C is your I
  ;; Is there some way to test settimeofday without trouble?
  "Test get-time"
  (multiple-value-bind (s ns) (get-time) (and (integerp s) (integerp ns)))
  (multiple-value-bind (s ns) (get-time)
    (let ((ut (get-universal-time)))
      (and (integerp ns)
	   (>= ut s) (> s (- ut 20)))))	; close enough?
  (let (t1 t2 ns)
    (setf (values t1 ns) (get-time))
    (sleep 2)				; I know this seems like forever.
    (setf (values t2 ns) (get-time))
    (and (>= t2 (+ t1 2))))
    
  ;; @@@ test timerfd
  )

;; users
(deftests (opsys-unix-users-1 :doc "Test users.")
  ""
  ;; getlogin tends to fail, so how can we test it?
  "Test getpwuid"
  (uos::passwd-p (uos:getpwuid 0))
  (let* ((uid (uos:getuid))
	 (pw (uos:getpwuid uid)))
    (= uid (uos:passwd-uid pw)))
  "Test *pwent and getpwnam"
  (progn
    (uos:setpwent)
    (let ((pw (uos:getpwent)))
      (prog1 (and (uos::passwd-p pw)
		  (let ((pw2 (uos:getpwnam (uos:passwd-name pw))))
		    (equal (uos:passwd-name pw) (uos:passwd-name pw2))))
	(uos:endpwent))))
  (stringp (uos:user-name))
  (stringp (uos:user-home))
  (stringp (uos:user-full-name))
  ;; I'm not sure how to test some of these things with making assumptions about
  ;; the user database. Even though it would be highly unusual to not have a
  ;; user "root" with uid 0, it could legitimately happen.
  ;;
  ;; We could test versus what is reported by external programs, but that
  ;; presumes the existence of external programs, which is definitely not
  ;; guaranteed.
  ;;
  ;; As it is, we're assuming there's at least one user with stuff filled out
  ;; in the user database, and that the current user is in the user database.
  (let ((l (uos:user-list)))
    (and l (> (length l) 0)))
  "Test getgrgid"
  (uos::group-entry-p (uos:getgrgid 0))
  (let* ((gid (uos:getgid))
	 (gr (uos:getgrgid gid)))
    (= gid (uos:group-entry-gid gr)))
  "Test *grent and getgrnam"
  (progn
    (uos:setgrent)
    (let ((gr (uos:getgrent)))
      (prog1 (and (uos::group-entry-p gr)
		  (let ((gr2 (uos:getgrnam (uos:group-entry-name gr))))
		    (equal (uos:group-entry-name gr)
			   (uos:group-entry-name gr2))))
	(uos:endgrent))))
  (stringp (uos:group-name))
  (integerp (uos:group-id))
  (let ((l (uos:group-list)))
    (and l (> (length l) 0)))
  "Test users-logged-in"
  (and (listp (users-logged-in)) (> (length (users-logged-in)) 0))
  ;; @@@ figure out how to test utmpx vs utmp
  "Test uid getting functions"
  (and (integerp (getuid)) (plusp (getuid)))
  (and (integerp (getgid)) (plusp (getgid)))
  (and (integerp (geteuid)) (plusp (geteuid)))
  (and (integerp (getegid)) (plusp (getegid)))
  "Test get-groups"
  (and (uos:get-groups) (>= (length (uos:get-groups)) 1))
  (uos:member-of (elt (uos:get-groups) 0))
  )

;; filesystem
(deftests (opsys-unix-filesystem-1 :doc "Test file system things.")
  "Test hidden-file-name-p"
  (not (uos:hidden-file-name-p "hey"))
  (not (uos:hidden-file-name-p "hey."))
  (not (uos:hidden-file-name-p "/hey."))
  (not (uos:hidden-file-name-p "/"))
  (uos:hidden-file-name-p "./hey.")
  (uos:hidden-file-name-p ".hey.")
  (uos:hidden-file-name-p "..")
  (uos:hidden-file-name-p ".")
  "Test superfluous-file-name-p"
  (not (superfluous-file-name-p "..foo"))
  (not (superfluous-file-name-p ".foo"))
  (not (superfluous-file-name-p "./"))
  (superfluous-file-name-p "..")
  (superfluous-file-name-p ".")
  (%path-absolute-p "/")
  (%path-absolute-p "/foo/bar/baz")
  (not (%path-absolute-p "foo"))
  (not (%path-absolute-p "./foo"))
  (not (%path-absolute-p "foo/bar//"))
  (and (integerp (get-path-max)) (> (get-path-max) 6))
  )

(deftests (opsys-unix-dir-1 :doc "Test directories.")
  "Test a bunch of directory stuff."
  (let ((test-dir (format nil "floop~s" (random *unlikely*)))
	(start-dir (nos:current-directory))
	result)
    (unwind-protect
	 (progn
	   ;; (uos:make-directory test-dir)
	   (nos:make-directory test-dir)
	   (setf result (and (nos:directory-p test-dir)
			     (nos:probe-directory test-dir)))
	   (nos:change-directory test-dir)
	   (setf result
		 (and result
		      (equal (nos:current-directory)
			     (format nil "~a/~a" start-dir test-dir)))))
      (nos:change-directory start-dir)
      (nos:delete-directory test-dir))
    result))

#|
(deftests (opsys-unix-read-dir-1 :doc "Test directories.")
  "Test a reading directories."
  ;; dirent-name
  ;; dirent-type
  ;; read-directory
  ;; map-directory
  ;; without-access-errors

   #:posix-open #+(or linux freebsd) #:posix-openat
   #:posix-close
   #:posix-read
   #:posix-write
   #:posix-ioctl
   #:posix-unlink #+(or linux freebsd) #:posix-unlinkat
   #:posix-lseek
   #:posix-pread
   #:posix-pwrite
   #:posix-readv
   #:posix-writev
   #:posix-preadv
   #:posix-pwritev
   #:with-posix-file
   #:with-os-file
   #:mkstemp
   #:fcntl
   #:get-file-descriptor-flags
   #:fsync
   #:fdatasync

   #:stat
   #:lstat
   #:fstat
   #+(or linux freebsd) #:fstatat
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
   #:os-delete-file
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
   #:set-file-time
   #:symlink
   #:symlinkat
   #:make-symbolic-link

   )
   |#

#|
;; memory
(deftests (opsys-unix-memory-1 :doc "Test memory functions.")
  ""
  )

;; signals
(deftests (opsys-unix-signals-1 :doc "Test signal functions.")
  ""
  ;; *signal-count*
  ;; signal-name
  ;; signal-description
  ;; signal-action
  ;; set-signal-action
  ;; with-signal-handlers
  ;; with-blocked-signals
  ;; with-all-signals-blocked
  ;; signal-mask
  ;; set-signal-mask
  ;; describe-signals
  ;; kill
  ;; killpg

  ;; #:+SIGHUP+ #:+SIGINT+ #:+SIGQUIT+ #:+SIGILL+ #:+SIGTRAP+ #:+SIGABRT+
  ;; #:+SIGPOLL+ #:+SIGEMT+ #:+SIGFPE+ #:+SIGKILL+ #:+SIGBUS+ #:+SIGSEGV+
  ;; #:+SIGSYS+ #:+SIGPIPE+ #:+SIGALRM+ #:+SIGTERM+ #:+SIGURG+ #:+SIGSTOP+
  ;; #:+SIGTSTP+ #:+SIGCONT+ #:+SIGCHLD+ #:+SIGTTIN+ #:+SIGTTOU+ #:+SIGIO+
  ;; #:+SIGXCPU+ #:+SIGXFSZ+ #:+SIGVTALRM+ #:+SIGPROF+ #:+SIGWINCH+ #:+SIGINFO+
  ;; #:+SIGUSR1+ #:+SIGUSR2+ #:+SIGSTKFLT+ #:+SIGPWR+

  )

;; processes
(deftests (opsys-unix-processes-1 :doc "Test processes.")
  ""
  )
|#

;; events

;; terminals

;; communication

;; unix

;; inspection

;; unix-stream

(defun test-out-pipe (program args)
  (let (line ss pid)
    (setf (values ss pid) (uos::pipe-program program args))
    (format t "stream = ~s~%pid = ~s~%" ss pid)
    (setf line (read-line ss))
    (format t "output = ~s~%" line)
    (format t "wait-and-chill = ~s~%" (uos::wait-and-chill pid))
    line))

(defun test-in-pipe (program args input)
  (let ((in (make-instance 'uos:unix-character-output-stream))
	stream pid)
    (setf (values stream pid) (uos::pipe-program program args
						 :out-stream nil
						 :in-stream in))
    ;;(format t "out stream = ~s~%pid = ~s~%" ss pid)
    (write-string input in)
    (finish-output in)
    (close in)
    (let ((l (multiple-value-list (uos::wait-and-chill pid))))
      (format t "wait-and-chill = ~s~%" l))))

;; So we don't have to explicitly depend on dlib.
(defun s+ (&rest rest)
  "Return a string which is the arguments concatenated as if output by PRINC."
  (with-output-to-string (result)
    (loop :for x :in rest :do (princ x result))))

;; This assumes some typical unix program exist, so it won't work non-unix
;; and won't even work on all unix. Of course we could make some exectuables be
;; there, but that seems way way out of the scope of this test and might depend
;; on even more stuff.
(deftests (opsys-unix-pipe-1 :doc "OS stream pipes")
  (or (and (every #'is-executable '("/bin/echo"
				    "/usr/bin/od")))
      (error "I'm sorry, but not all the executables exist, so this test ~
              set will fail."))
  (equal (test-out-pipe "/bin/echo" '("foo")) "foo")
  ;; (equal (test-in-pipe "/usr/bin/od" '("-t" "cd1")
  ;; 		       (s+ "fippy flimbar" #\newline))
  ;; 	 (s+ "0000000  102  105  112  112  121   32  102  108  105  109   98"
  ;; 	     "   97  114   10" #\newline "0000016" #\newline))
  )

(deftests (opsys-unix-all :doc "All tests for OPSYS-UNIX.")
  opsys-unix-errors-1
  opsys-unix-environment-1
  #.(if (fboundp 'uos:sysctl) 'opsys-unix-sysctl-1 t)
  opsys-unix-environment-2
  opsys-unix-time-1
  opsys-unix-users-1
  opsys-unix-dir-1
  opsys-unix-pipe-1
  )

(defun run ()
  (run-group-name 'opsys-unix-all :verbose t))

;; EOF
