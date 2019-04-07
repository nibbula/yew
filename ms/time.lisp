;;
;; ms/time.lisp - Windows interface to time and timers
;;

(in-package :opsys-ms)

(defcstruct SYSTEMTIME
  (year WORD)
  (month WORD)
  (day-of-week WORD)
  (day WORD)
  (hour WORD)
  (minute WORD)
  (second WORD)
  (milliseconds WORD))
(defctype PSYSTEMTIME (:pointer (:struct SYSTEMTIME)))
(defctype LPSYSTEMTIME (:pointer (:struct SYSTEMTIME)))

(defcfun ("GetSystemTimeAsFileTime" %get-system-time-as-file-time)
    :void
  (system-time-as-file-time LPFILETIME))

(defcfun ("GetSystemTime" %get-system-time)
    :void 
  (system-time LPSYSTEMTIME))

(defcfun ("SetSystemTime" %set-system-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME))))

(defcfun ("SystemTimeToFileTime" %system-time-to-file-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME)))
  (file-time LPFILETIME))

(defcfun ("FileTimeToLocalFileTime" %file-time-to-local-file-time)
    BOOL
  (file-time (:pointer (:struct FILETIME)))
  (local-file-time LPFILETIME))

(defcfun ("GetLocalTime" %get-local-time)
    :void
  (system-time LPSYSTEMTIME))

(defcfun ("SetLocalTime" %set-local-time)
    BOOL
  (system-time (:pointer (:struct SYSTEMTIME))))

(defun get-current-filetime-into (foreign-filetime)
  "Get the current time into a foreign pointer to FILETIME struct."
  ;; (with-foreign-object (sys-time '(:struct SYSTEMTIME))
  ;;   (%get-local-time sys-time)
  ;;   (%system-time-to-file-time sys-time foreign-filetime))
  (%get-system-time-as-file-time foreign-filetime))

(defun get-current-filetime-as-cffi-struct ()
  "Return the current time as CFFI struct (aka plist) representing a FILETIME."
  (with-foreign-object (time '(:struct FILETIME))
    (get-current-filetime-into time)
    (convert-from-foreign time '(:struct FILETIME))))

(defun get-current-filetime-as-integer ()
  "Return the current time as an integer representing a FILETIME."
  (with-foreign-object (time '(:struct FILETIME))
    (get-current-filetime-into time)
    (with-foreign-slots ((high-date-time low-date-time) time (:struct FILETIME))
      (logior (ash high-date-time 32) (logand low-date-time #xffffffff)))))

(defun get-time ()
  "Return the time in seconds and nanoseconds. The first value is seconds in
so-called “universal” time. The second value is nanoseconds."
  (with-foreign-objects ((sys-time '(:struct SYSTEMTIME))
			 (time '(:struct FILETIME)))
    (%get-local-time sys-time)
    (%system-time-to-file-time sys-time time)
    (filetime-to-universal-time-and-nsec
     (convert-from-foreign time '(:struct FILETIME)))))

(defun set-time (seconds nanoseconds)
  "Set time in seconds and nanoseconds. Seconds are in so-called
“universal” time."
  (declare (ignore seconds nanoseconds))
  ;; @@@
  nil)

(defun seconds-to-100ns (seconds)
  "Return an integer number of 100ns time units corresponding to SECONDS."
  (truncate (* seconds (expt 10 7))))

(defcfun ("CreateWaitableTimerW" %create-waitable-timer)
    HANDLE
  (timer-attributes LPSECURITY_ATTRIBUTES) ; in opt
  (manual-reset BOOL)			   ; in
  (timer-name LPCTSTR))			   ; in opt

(defcfun ("CancelWaitableTimer" %cancel-waitable-timer)
    BOOL
 (timer HANDLE))

;; completion-routine is really a PTIMERAPCROUTINE
(defcfun ("SetWaitableTimer" %set-waitable-timer)
    BOOL
  (timer                     HANDLE)		  	; in
  (due-time		     (:pointer LARGE_INTEGER))	; in
  (period		     LONG)		  	; in
  (completion-routine	     :pointer)			; in opt
  (arg-to-completion-routine LPVOID)		  	; in opt
  (resume		     BOOL))		  	; in

(defvar *timers* nil
  "List of timers to use for waiting around.")

(defun get-timer ()
  "Get a reusable timer from a pool."
  ;; @@@ this needs to be synchronized with other threads
  (when (not *timers*)
    (with-wide-string (name (string (gensym "timer")))
      (push (%create-waitable-timer (null-pointer) +FALSE+ name) *timers*)))
  (pop *timers*))

(defun replace-timer (timer)
  "Put the reusable timer back in the pool."
  ;; @@@ this needs to be synchronized with other threads
  (push timer *timers*))

(defmacro with-timer ((var) &body body)
  "Evaluate the body with VAR set to a reusable timer."
  `(let (,var)
     (unwind-protect
	  (progn
	    (setf ,var (get-timer))
	    ,@body)
       (when ,var (replace-timer ,var)))))

;; End
