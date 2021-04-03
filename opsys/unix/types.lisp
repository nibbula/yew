;;;
;;; unix/types.lisp - Data types for the unix interface
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;; C API types

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-simple-types
  #(
#| This is just gonna go over 80 cols, so deal.
Type name     Darwin           Linux-32         Linux-x86-64     Linux-arm64          SunOS            FreeBSD 64       OpenBSD 64 |#
#(time-t      :long            :long            :long            :long                :long            :int64           :int64)
#(mode-t      :uint16          :unsigned-int    :unsigned-int    :unsigned-int        :uint16          :uint16          :uint32)
#(uid-t       :uint32          :unsigned-int    :unsigned-int    :unsigned-int        :uint32          :uint32          :uint32)
#(gid-t       :uint32          :unsigned-int    :unsigned-int    :unsigned-int        :uint32          :uint32          :uint32)
#(pid-t       :int             :int             :int             :int                 :int             :int32           :int32)
#(suseconds-t :int32           :int32           :long            :long                :int32           :long            :long)
#(ssize-t     :long            :long            :long            :long                :long            :int64           :long)
#(dev-t       :int32           :uint64          :unsigned-long   :unsigned-long       :ulong           :uint64          :uint32)
#(nlink-t     :uint16          :unsigned-int    :unsigned-long   :unsigned-int        :uint            :uint64          :uint32)
#(ino-t       :uint64          :unsigned-long   :unsigned-long   :unsigned-long       :unsigned-long   :uint64          :uint64)
#(off-t       :int64           :int32           :long            :long                :int32           :int64           :int64)
#(blkcnt-t    :int64           :unsigned-long   :long            :long                :int64           :int64           :int64)
#(blksize-t   :int64           :long            :unsigned-long   :int                 :int32           :int32           :uint32)
#(fixpt-t     :uint32          :uint32          :uint32          :uint32              :uint32          :uint32          :uint32)
#(boolean-t   :unsigned-int    :unsigned-int    :unsigned-int    :unsigned-int        :unsigned-int    :unsigned-int    :int)
#(segsz-t     :int32           :int32           :int32           :int32               :int32           :int64           :int32)
#(fsblkcnt-t  :unsigned-long   :unsigned-long   :unsigned-long   :unsigned-long       :unsigned-long   :uint64          :uint64)
#(fsword-t    :int             :int             :int             :int                 :int             :int             :int)
#(attrgroup-t :uint32          :uint32          :uint32          :uint32              :uint32          :uint32          :uint32)
#(rlim-t      :uint64          :uint32          :unsigned-long   :unsigned-long-long  :uint32          :uint64          :uint64)
#(intptr-t    :long            :long            :long            :long                :long            :long            :long)
#(uintptr-t   :unsigned-long   :unsigned-long   :unsigned-long   :unsigned-long       :unsigned-long   :unsigned-long   :unsigned-long)
#(wchar-t     :int             :int             :int             :unsigned-int        :int             :int             :int)
#(caddr-t     (:pointer :char) (:pointer :char) (:pointer :char) (:pointer :char)     (:pointer :char) (:pointer :char) (:pointer :char))
#(quad-t      :int64           :int64           :int64           :long-long           :int64           :int64           :int64)
#(u-quad-t    :uint64          :uint64          :uint64          :unsigned-long-long  :uint64          :uint64          :uint64)
)))

;; At some time dev-t changed from 32 bits to 64 bits on Linux-32. Something
;; similar probably happened on other systems. The above does not reflect
;; that, so if we want to run on various older versions, we may have to make
;; *platform-*-index* have OS version detail.

;; @@@ We can probably move some things out of the above table because they're
;; seemingly invariant:
;;   wchar-t quad-t u-quad-t fixpt-t caddr-t intptr-t uintptr-t
;;
;; Also I think fsword-t is linux only, and attrgroup-t is darwin only.

#+freebsd (defctype fflags-t :uint32)

;; sigset
#+(or darwin sunos) (defctype sigset-t :uint32)
#+openbsd (defctype sigset-t :unsigned-int)

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

(defconstant +ms-per-sec+ (expt 10 6)
  "The number of microeconds in a second.")

;; @@@ This is similar to the code in dtime, but it depends on us, so for now
;; it seems simpler to just duplicate only the things that I need for other
;; parts of opsys.

(defun timeval-add (time1 time2)
  "Return the sum of TIME1 and TIME2, as a timeval."
  (let ((s (+ (timeval-seconds time1) (timeval-seconds time2)))
	(n (+ (timeval-micro-seconds time1) (timeval-micro-seconds time2))))
    (cond
      ((> n +ms-per-sec+)
       (incf s)
       (decf n +ms-per-sec+))
      ((= n +ms-per-sec+)
       (incf s)
       (setf n 0)))			; perhaps just saving a subtraction
    (make-timeval :seconds s
		  :micro-seconds n)))

(defun timeval-sub (time1 time2)
  "Return the difference of TIME1 and TIME2, as a timeval."
  (let ((n (- (timeval-micro-seconds time1) (timeval-micro-seconds time2)))
	s)
    (cond
      ((minusp n)
       (incf n +ms-per-sec+)
       (setf s (- (timeval-seconds time1) 1 (timeval-seconds time2))))
      (t
       (setf s (- (timeval-seconds time1) (timeval-seconds time2)))))
    (make-timeval :seconds s
		  :micro-seconds n)))

(defun timeval-plusp (timeval)
  "Return true if the time is positive."
  (or (plusp (timeval-seconds timeval))
      (and (zerop (timeval-seconds timeval))
	   (plusp (timeval-micro-seconds timeval)))))

(defun timeval-minusp (timeval)
  "Return true if the time is less than zero."
  (or (minusp (timeval-seconds timeval))
      (and (zerop (timeval-seconds timeval))
	   (minusp (timeval-micro-seconds timeval)))))

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

#+(or darwin linux freebsd openbsd)
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

;; End
