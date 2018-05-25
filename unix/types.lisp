;;
;; unix/types.lisp - Data types for the unix interface
;;

(in-package :opsys-unix)

;; C API types

(eval-when (:compile-toplevel :load-toplevel :execute)
(define-simple-types
  #(
#| This is just gonna go over 80 cols, so deal.
Type name     Darwin           Linux-32         Linux-64         SunOS            FreeBSD 64 |#
#(time-t      :long            :long            :long            :long            :int64)
#(mode-t      :uint16          :unsigned-int    :unsigned-int    :uint16          :uint16)
#(uid-t       :uint32          :unsigned-int    :unsigned-int    :uint32          :uint32)
#(gid-t       :uint32          :unsigned-int    :unsigned-int    :uint32          :uint32)
#(pid-t       :int             :int             :int             :int             :int32)
#(wchar-t     :int             :int             :int             :int             :int)
#(suseconds-t :int32           :int32           :int32           :int32           :long)
#(ssize-t     :long            :long            :long            :long            :int64)
#(dev-t       :int32           :uint64          :unsigned-long   :ulong           :uint32)
#(nlink-t     :uint16          :unsigned-int    :unsigned-long   :uint            :uint16)
#(ino-t       :uint64          :unsigned-long   :unsigned-long   :unsigned-long   :uint32)
#(off-t       :int64           :int32           :long            :int32           :int64)
#(quad-t      :int64           :int64           :int64           :int64           :int64)
#(u-quad-t    :uint64          :uint64          :uint64          :uint64          :uint64)
#(blkcnt-t    :int64           :unsigned-long   :long            :int64           :int64)
#(blksize-t   :int64           :long            :unsigned-long   :int32           :uint32)
#(fixpt-t     :uint32          :uint32          :uint32          :uint32          :uint32)
#(boolean-t   :unsigned-int    :unsigned-int    :unsigned-int    :unsigned-int    :unsigned-int)
#(segsz-t     :int32           :int32           :int32           :int32           :int64)
#(caddr-t     (:pointer :char) (:pointer :char) (:pointer :char) (:pointer :char) (:pointer :char))
#(fsblkcnt-t  :unsigned-long   :unsigned-long   :unsigned-long   :unsigned-long   :uint64)
#(fsword-t    :int             :int             :int             :int             :int)
#(attrgroup-t :uint32          :uint32          :uint32          :uint32          :uint32)
#(rlim-t      :uint64          :uint32          :unsigned-long   :uint32          :uint64)
)))

;; At some time dev-t changed from 32 bits to 64 bits on Linux-32. Something
;; similar probably happened on other systems. The above does not reflect
;; that, so if we want to run on various older versions, we may have to make
;; *platform-*-index* have OS version detail.

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

;; End
