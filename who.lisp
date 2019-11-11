;;
;; who.lisp - User information commands.
;;

;; @@@ This conflicts with cl-who.

(defpackage :who
  (:documentation "User information commands.")
  (:use :cl :dlib :opsys :dlib-misc :table :table-print :grout :lish
        #+unix :os-unix)
  (:export
   #:who
   #:!who
   #:uptime
   #:print-uptime
   #:!uptime
   #:who-what
   #:!w
   ))
(in-package :who)

(declaim #.`(optimize ,.(getf los-config::*config* :optimization-settings)))

;; Generally there are too many traditional user information command in Unix
;; that basically do the same thing. I would like to have all that
;; functionality here.

;; We could also provide compatability for things like:
;;   finger / whoami / last / users / id
;; But why?
;; Although I would like a mode which would print a bunch more user information
;; like 'finger'.

(defun user-name-list ()
  (append (mapcar #'nos:user-info-name (nos:user-list))
	  (list "am" "i")))

;; (defclass arg-user (arg-lenient-choice)
;;   ()
;;   (:default-initargs
;;    :choice-func #'user-name-list)
;;   (:documentation "User name."))

(defargtype user (arg-lenient-choice)
  "User name."
  ()
  (:default-initargs
   :choice-func #'user-name-list))

(defun dev-name (device)
  "Convert a short device from utmpx to a stat-able pathname."
  #+unix (s+ "/dev/" device)
  #-unix device)

#+unix
(defun who (&key users show-dead all tty (print t) file)
  (with-grout ()
    (let (tab table)
      (unwind-protect
        (progn
	  (if file
	      (set-utmp-file (or (guess-utmpx-file-type file) +UTXDB-ACTIVE+)
			     file)
	      (set-utmp-file +UTXDB-ACTIVE+))
	  (setutxent)
	  (setf tab
		(loop :with u
		   :while (setf u (getutxent))
		   :if (and (or all (eq (utmpx-type u) :user-process))
			    (or show-dead (not (eq (utmpx-type u)
						   :dead-process)))
			    (or (not users)
				(find (utmpx-user u) users :test #'equalp))
			    (or (not tty)
				(or (equalp tty (utmpx-line u))
				    (equalp tty (dev-name (utmpx-line u))))))
		   :collect
		   (list (utmpx-user u)
			 (utmpx-line u)
			 (utmpx-pid u)
			 (string-downcase (utmpx-type u))
			 (date-string :time (unix-to-universal-time
					     (timeval-seconds (utmpx-tv u))))
			 (utmpx-host u)))))
	(endutxent))
      (setf table (make-table-from
		   tab :column-names '("User" "Tty" "PID" "Type" "Time" "Host")))
      (when print
	(grout-print-table table))
      table)))

#-unix
(defun who (&key users show-dead all (print t))
  (declare (ignore users show-dead all print)))

(defcommand who
  ((show-dead boolean :short-arg #\d
    :help "True to show dead processes.")
   (all boolean :short-arg #\a
    :help "True to show all processes, not only user processes.")
   (tty string :long-arg "tty"
    :help "Terminal to exclusively report on.")
   (file pathname :short-arg #\f
    :help "Name of a file to use as the database.")
   (users user :repeating t :help "Users to report on."))
  "Who is on."
  (when (equalp users '("am" "i"))
    (setf users (list (user-name))
	  tty (file-handle-terminal-name 0)))
  (setf *output*
	(who :users users :show-dead show-dead :all all :tty tty :file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lastlogin

#+lish
(lish:defcommand lastlogin
  ((show-history boolean :short-arg #\h :help "True to show login history.")
   (users user :repeating t :help "User to show the last login for."))
  "Show users' last login."
  ;; @@@ There should be an option where it tries to figure out durations
  ;; for sessions and booted times, like the "last" command.
  (setf *output*
	(who :users users :all t
	     :file (cond
		     (show-history
		      (default-utmpx-file +UTXDB-LOG+))
		     (t
		      ;; @@@ I can't be bothered to do this correctly now.
		      #+linux (default-utmpx-file +UTXDB-LOG+)
		      #-linux (default-utmpx-file +UTXDB-LASTLOGIN+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uptime

#|
#+linux
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *uptime-file* "/proc/uptime")
    (defparameter *loadavg-file* "/proc/loadavg"))

  (defun uptime ()
    "Return how long the system has been running since boot, truncated to
seconds."
    (with-open-file (str *uptime-file*)
      ;; /proc/uptime is a line with two ASCII decimal numbers separated by a
      ;; space. First is the uptime of the system in seconds. Next is the
      ;; amount of time spent in the idle process in seconds. The seconds
      ;; usually have a fractional part after a decimal point which we ignore.
      ;; This also just returns the first number.
      (parse-integer (initial-span (read-line str) '(#\. #\space)))))

  ;; We could convert these to floats, but why?
  ;; But, if we put this in OPSYS then we probably should.
  (defun load-averages ()
    "Return 3 strings representing the 1, 5, and 15, minute load averages,
probably printed as floats to 2 places."
    (with-open-file (str *loadavg-file*)
      (let ((words (split-sequence #\space (read-line str))))
	(values (first words) (second words) (third words))))))

#-(or linux)
(progn
  (defun uptime () 0)
  (defun load-averages () (values 0 0 0)))
|#


;; This is an highly un-hygenic macro.
(defmacro with-time-units ((seconds &key from-days-p) &body body)
  "Evaluate the BODY with SECONDS split into a bunch of time units.
If FROM-DAYS-P is true, days are the biggest units to break SECONDS into."
  (with-unique-names (up)
    `(let* ((,up ,seconds)
	    (millennia (truncate (time-to-millennia ,up)))
	    (centuries (truncate (time-to-centuries
				  (if (not ,from-days-p)
				      (decf ,up (millennia-to-time millennia))
				      0))))
	    (decades   (truncate (time-to-decades
				  (if (not ,from-days-p)
				      (decf ,up (centuries-to-time centuries))
				      0))))
	    (years     (truncate (time-to-years
				  (if (not ,from-days-p)
				      (decf ,up (decades-to-time decades))
				      0))))
	    (weeks     (truncate (time-to-weeks
				  (if (not ,from-days-p)
				      (decf ,up (years-to-time years))
				      0))))
	    (days      (truncate (time-to-days
				  (if (not ,from-days-p)
				      (decf ,up (weeks-to-time weeks))
				      ,up))))
	    (hours     (truncate (time-to-hours
				  (decf ,up (days-to-time days)))))
	    (minutes   (truncate (time-to-minutes
				  (decf ,up (hours-to-time hours))))))
       (declare (ignorable ,up millennia centuries decades years weeks days
			   hours minutes))
       ,@body)))

#+unix
(defun print-uptime (&key pretty show-since (stream *standard-output*))
  (let* ((info (get-system-info '(:uptime :load-averages)))
	 (uptime (cdr (assoc :uptime info)))
	 (one (elt (cdr (assoc :load-averages info)) 0))
	 (five (elt (cdr (assoc :load-averages info)) 1))
	 (fifteen (elt (cdr (assoc :load-averages info)) 2)))
    (cond
      (pretty
       (write-string "up " stream)
       (with-time-units (uptime)
	 (when (not (zerop millennia))
	   (format stream "~a millenni~a, " millennia
		   (if (> millennia 1) "a" "um")))
	 (when (not (zerop centuries))
	   (format stream "~a centur~:@p, " centuries))
	 (when (not (zerop decades))
	   (format stream "~a decade~:p, " decades))
	 (when (not (zerop years))
	   (format stream "~a year~:p, " years))
	 (when (not (zerop weeks))
	   (format stream "~a week~:p, " weeks))
	 (when (not (zerop days))
	   (format stream "~a day~:p, " days))
	 (when (not (zerop hours))
	   (format stream "~a hour~:p, " hours))
	 (when (not (zerop minutes))
	   (format stream "~a minute~:p~%" minutes))))
      (show-since
       (format stream "~a~%"
	       (date-string :time (- (get-universal-time) uptime))))
      (t
       (with-time-units (uptime :from-days-p t)
	 (format stream " ~a up " (format-date "~2,'0d:~2,'0d:~2,'0d"
					       (:hours :minutes :seconds)))
	 (when (>= days 1)
	   (format stream "~d day~:p, " days))

	 (if (>= hours 1)
	     (format stream "~2,,,'0a:~2,,,'0a,  " hours minutes)
	     (format stream "~a min~:p,  " minutes))

	 (format stream "~d user~:*~p,  " (length (users-logged-in)))
	 (format stream
		 "load average: ~4,2f, ~4,2f, ~4,2f~%" one five fifteen))))))

#+lish
(lish:defcommand uptime
  ((pretty boolean :short-arg #\p
    :help "True to show in a long drawn-out format, mostly for gloating.")
   (show-since boolean :short-arg #\s
    :help "True to show the time the system has been up since."))
  "Show how long the system has been running."
  (print-uptime :pretty pretty :show-since show-since))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w (or who-what)

#+unix
(progn
  (defun idle-string (time)
    "Return a string describing the TIME, idiosyncratically for idle time."
    (cond
      ((>= (time-to-days time) 2)
       (format nil "~2ddays" (truncate (time-to-days time))))
      ((>= (time-to-hours time) 1)
       (format nil "~2d:~2,'0dm" (truncate (time-to-hours time))
	       (truncate
		(time-to-minutes
		 (- time (hours-to-time (truncate (time-to-hours time))))))))
      ((>= (time-to-minutes time) 1)
       (format nil "~2d:~2,'0ds" (truncate (time-to-minutes time))
	       (- time (minutes-to-time (truncate (time-to-minutes time))))))
      (t
       (format nil "~2ds" time))))

  (defun device-idle-string (device-name)
    "Return the idle string for DEVICE-NAME, or ??? if we can't figure it out."
    (catch 'fail
      (handler-case
	  (idle-string
	   (- (get-universal-time)
	      (unix-to-universal-time
	       (timespec-seconds
		(file-status-modify-time
		 (stat device-name))))))
	(posix-error (c)
	  (declare (ignore c))
	  (throw 'fail "???")))))

  (defun login-at-string (login-time)
    "Return a string describing the LOGIN-TIME."
    (let* ((cur-time (get-universal-time))
	   (time-diff (- cur-time login-time)))
      (if (> (time-to-hours time-diff) 12)
	  (if (> (time-to-days time-diff) 6)
	      (format-date "~2,'0d~a~a" (:date :month-abbrev :year-abbrev)
			   :time login-time)
	      (format-date "~a~2,'0d" (:day-abbrev :hour) :time login-time))
	  (format-date "~2,'0d:~2,'0d" (:hour :minute) :time login-time))))


  (defun find-foreground-process (proc-list parent tty
				  &optional (pick parent) (level 1))
    "Try to return a PID that is the current process running on TTY, given a
PARENT pid, and PROC-LIST as returned by opsys:process-list."
    ;; (format t "ffp:~va ~a ~a~%" level #\space parent pick)
    (let* (;(parent-proc (find parent proc-list :key #'os-process-id))
	   (children (remove-if (_ (/= (os-process-parent-id _) parent))
				proc-list))
	   (tty-dev (file-status-device-type (stat (dev-name tty)))))
      ;;(format t "ffp ~s ~s ~s ~s~%" tty-dev parent pick children)
      (loop :for p :in children :do
	 #+unix
	 (let ((sys-proc (system-process-info (os-process-id p))))
	   (when (equal (uos:unix-process-terminal sys-proc) tty-dev)
	   ;; @@@ This is wrong. We just pick the process on the same terminal
	   ;; with the highest PID.
	   ;; On Linux we can check that it's a member of the foreground
	   ;; process group, and use the latest process start time
	   ;; (both from /proc/#/stat).
	   ;; On MacOS & BSD we can do what? I'm guessing we probably won't be
	   ;; able to do shit without root.
	     (when (> (os-process-id p) pick)
	       (setf pick (os-process-id p)))
	     (setf pick
		   (find-foreground-process proc-list (os-process-id p) tty pick
					    (incf level))))))
      pick))

  (defun guess-command (long proc-list pid tty)
    ;; (let ((proc (find (find-foreground-process proc-list pid tty pid)
    ;; 		      proc-list :key #'os-process-id)))
    ;;   (if long
    ;; 	  (join-by-string (os-process-args proc) #\space)
    ;; 	  (os-process-command proc))))
    (declare (ignore long))
    (let ((proc (find (find-foreground-process proc-list pid tty pid)
		      proc-list :key #'os-process-id)))
      (and proc (os-process-name proc))))

  (defun who-what (&key users no-header long)
    (declare (ignore users))
    (with-grout ()
      (when (not no-header)
	(print-uptime)
	(finish-output))
      (let (tab (proc-list (process-list)))
	(unwind-protect
	     (progn
	       (setutxent)
	       (setf tab
		     (loop :with u
			:while (setf u (getutxent))
			:if (eq (utmpx-type u) :user-process)
			:collect
			(list (utmpx-user u)
			      (utmpx-line u)
			      (utmpx-host u)
			      (login-at-string
			       (unix-to-universal-time
				(timeval-seconds (utmpx-tv u))))
			      (device-idle-string (dev-name (utmpx-line u)))
			      (or (guess-command long proc-list
						 (utmpx-pid u)
						 (utmpx-line u))
				  "")))))
	  (endutxent))
	(setf tab
	      (make-table-from
	       tab :column-names '("User" "Tty" "From" "Login@" "Idle" "What")))
	(grout-print-table tab :print-titles (not no-header))
	tab))))

#-unix
(defun who-what (users &key no-header long)
  (declare (ignore users no-header long)))

(defcommand w
  ((no-header boolean :short-arg #\h :help "True to omit the header.")
   (long boolean :short-arg #\l :help "True show the longer output.")
   (users user :repeating t :help "Users to show."))
  "Show who's doing what."
  (setf lish:*output* (who-what :users users :no-header no-header :long long)))

;; EOF
