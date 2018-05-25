;;
;; unix/time.lisp - Unix interface to time and timers
;;

(in-package :opsys-unix)

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

;; adjtime??

;; setitimer/getitimer
;;
;; Interval timers ared probably best provided in relation to some kind of
;; "event loop"

;; End
