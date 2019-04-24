;;
;; opsys-unix-test.lisp - Tests for OPSYS-UNIX.
;;

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
  )

#|
;; filesystem
(deftests (opsys-unix-filesystem-1 :doc "Test file system things.")
  ""
  )

;; memory
(deftests (opsys-unix-memory-1 :doc "Test memory functions.")
  ""
  )

;; signals
(deftests (opsys-unix-signals-1 :doc "Test signal functions.")
  ""
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

(deftests (opsys-unix-all :doc "All tests for OPSYS-UNIX.")
  opsys-unix-errors-1
  opsys-unix-environment-1
  #.(if (fboundp 'uos:sysctl) 'opsys-unix-sysctl-1 t)
  opsys-unix-environment-2
  opsys-unix-time-1
  opsys-unix-users-1
  )

(defun run ()
  (run-group-name 'opsys-unix-all :verbose t))

;; EOF
