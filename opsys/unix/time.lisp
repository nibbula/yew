;;;
;;; unix/time.lisp - Unix interface to time and timers
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers / Timing

#+linux ;; @@@ Only checked on linux so far
(progn
  (defparameter *clocks* nil "Types of system clocks.")
  (define-to-list *clocks*
      #(#(+CLOCK-REALTIME+           0 "System realtime clock.")
	#(+CLOCK-MONOTONIC+          1 "Monotonic system-wide clock.")
	#(+CLOCK-PROCESS-CPUTIME-ID+ 2 "High-resolution timer from the CPU.")
	#(+CLOCK-THREAD-CPUTIME-ID+  3 "Thread-specific CPU-time clock.")
	#(+CLOCK-MONOTONIC-RAW+      4 "Monotonic, not adjusted.")
	#(+CLOCK-REALTIME-COARSE+    5 "Realtime, updated on ticks.")
	#(+CLOCK-MONOTONIC-COARSE+   6 "Monotonic, updated only on ticks.")
	#(+CLOCK-BOOTTIME+           7 "Monotonic, includes suspension time.")
	#(+CLOCK-REALTIME-ALARM+     8 "Like CLOCK-REALTIME but also wakes.")
	#(+CLOCK-BOOTTIME-ALARM+     9 "Like CLOCK-BOOTTIME but also wakes.")
	#(+CLOCK-TAI+                11 "Realtime International Atomic Time."))))

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

(defctype clockid-t :int)

(defcfun ("clock_getres" real-clock-getres) :int
  (clk_id clockid-t)
  (res (:pointer (:struct foreign-timespec))))

(defcfun ("clock_gettime" real-clock-gettime) :int
  (clk_id clockid-t)
  (tp (:pointer (:struct foreign-timespec))))

(defcfun ("clock_settime" real-clock-settime) :int
  (clk_id clockid-t)
  (tp (:pointer (:struct foreign-timespec))))

(defun clock-getres (clock-id)
  "Get the resolution of the clock indicated by CLOCK-ID, which is one of the
+CLOCK-*+ constants in *clocks*."
  (with-foreign-object (ts '(:struct foreign-timespec))
    (syscall (real-clock-getres clock-id ts))
    (timespec-to-os-time ts)))

(defun clock-gettime (clock-id)
  "Return the time as an OS-TIME of the clock indicated by CLOCK-ID, which is
one of the +CLOCK-*+ constants in *clocks*."
  (with-foreign-object (ts '(:struct foreign-timespec))
    (syscall (real-clock-gettime clock-id ts))
    (timespec-to-os-time ts)))

(defun clock-settime (clock-id os-time)
  "Set the time of the clock indicated by CLOCK-ID, to OS-TIME. CLOCK-ID is
one of the +CLOCK-*+ constants in *clocks*."
  (with-foreign-object (ts '(:struct foreign-timespec))
    (with-foreign-slots ((tv_sec tv_nsec) ts (:struct foreign-timespec))
      (setf tv_sec (os-time-seconds os-time)
	    tv_nsec (os-time-nanoseconds os-time))
      (syscall (real-clock-gettime clock-id ts)))))

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

(defun timespec-to-os-time (ts)
  "Convert a timespec to a os-time."
  (make-os-time
   :seconds (unix-to-universal-time (getf ts 'tv_sec))
   :nanoseconds (getf ts 'tv_nsec)))

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

(defcfun tzset :void
  "Set timezone data.")

(defcvar tzname :string
  "Name of the timezone.")

(defcvar timezone :long
  "Seconds west of UTC.")

(defcvar daylight :int
  "Non-zero if daylight savings time is active.")

(defun timezone-name ()
  "Return the current timezone name."
  tzname)

(defun timezone-offset ()
  "Seconds west of UTC."
  timezone)

;; adjtime??

;; setitimer/getitimer
;;
;; Interval timers are probably best provided in relation to some kind of
;; "event loop"

;; timerfd
#+linux
(progn
  (defconstant +TFD-TIMER-ABSTIME+ 1 "Absolute time.")
  (defconstant +TFD-NONBLOCK+ #o00004000 "Non-blocking I/O flag.")
  (defconstant +TFD-CLOEXEC+  #o02000000 "Close on exec flag.")

  (defcstruct itimerspec
    (it_interval (:struct foreign-timespec))	;; period
    (it_value (:struct foreign-timespec)))	;; expiration

  (defcfun timerfd-create :int
    "Return a file descriptor for a new timer object."
    (clockid :int) (flags :int))

  (defcfun timerfd-settime :int
    "Change the expiration time for the timer object FD. NEW-VALUE is the new
expriation time, which can either enable or disable the timer. The previous
expiration time is returned in OLD-VALUE, which can be a null pointer to
ignore it."
    (fd :int) (flags :int)
    (new-value (:pointer (:struct itimerspec)))
    (old-value (:pointer (:struct itimerspec))))

  (defcfun timerfd-gettime :int
    "Get the expiration time of the timer object FD, and put it in VALUE."
    (fd :int) (value (:pointer (:struct itimerspec)))))

  ;; @@@ maybe make a nice interface to it?
  ;; (defun make-timerfd (&key clock flags time)
  ;;   )
  ;; (defun timerfd (fd)
  ;;   )
  ;; (defsetf timerfd set-timerfd)

;; End
