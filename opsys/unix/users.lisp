;;;
;;; unix/users.lisp - Unix interface to user accounts, groups and authenticaion.
;;;

(in-package :opsys-unix)

(declaim #.`(optimize ,.(getf opsys-config::*config* :optimization-settings)))

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
  pw-expire)

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

(defun convert-user (pw)
  "Return a generic user structure from the foreign passwd structure. ~
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
	(make-user-info
	 :name pw_name
	 :id pw_uid
	 :full-name pw_gecos
	 :home-directory pw_dir
	 :shell pw_shell
	 :primary-group-id pw_gid
	 ;; :guid
	 ;; :picture
	 ))))

(defcfun ("getpwuid" real-getpwuid) :pointer (uid uid-t))
(defun getpwuid (uid)
  (convert-passwd (real-getpwuid uid)))

;; @@@ Should use the re-entrant versions of these functions.

;; int
;; getpwuid_r(uid_t uid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result)

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

(defcfun endpwent :void)
(defcfun setpwent :void)

(defun get-user-info (&key name id)
  "Return a user structure from the user database. You can look up by either
NAME or ID. If you specifiy both, it just uses the ID. If you specify neither,
it signals an error."
  (when (not (or name id))
    (error "You have to specify at least one of NAME or ID."))
  (if id
      (convert-user (real-getpwuid id))
      (convert-user (real-getpwnam name))))

(defun user-name (&optional id)
  "Return the name of the user with ID, which defaults to the current user."
  (let ((u (getpwuid (or id (getuid)))))
    (and u (passwd-name u))))

(defun user-home (&optional (user (user-name)))
  "Return the namestring of the given USER's home directory or nil if the ~
user is not found."
  (unwind-protect
    (progn
      (setpwent)
      (loop :with p = nil
	 :while (setf p (getpwent))
	 :do (when (string= (passwd-name p) user)
	       (return-from user-home (passwd-dir p)))))
    (endpwent)))

(defun user-id (&key name effective)
  "Return the ID of the user with NAME, which defaults to the current user."
  (if name
      (let ((pw (getpwnam name)))
	(if pw
	    (passwd-uid pw)
	    (error 'opsys-error
		   :format-control "User ~s not found."
		   :format-arguments (list name))))
      (if effective
	  (geteuid)
	  (getuid))))

(defun user-full-name (&optional id)
  "Return the full name of user with ID, which defaults to the current user."
  (let* ((name (passwd-gecos (getpwuid (or id (getuid)))))
	 (comma (position #\, name)))
    (if comma
	(subseq name 0 comma)
	name)))

;; This is probably wrong & system specific
(defun user-name-char-p (c)
  "Return true if C is a valid character in a user name."
  (or (alphanumericp c) (eql #\_ c) (eql #\- c)))

(defun valid-user-name (username)
  (not (position-if #'(lambda (c) (not (user-name-char-p c))) username)))

(defun get-next-user ()
  "Return the next user structure from the user database."
  (convert-user (real-getpwent)))

(defun user-list ()
  "How to annoy people in large organizations."
  (unwind-protect
    (progn
      (setpwent)
      (loop :with g :while (setf g (get-next-user)) :collect g))
    (endpwent)))

(defun refresh-user-list ()
  "Just in case you are bored, this will make get-next-group or group-list
return potentially updated data."
  (endpwent)
  (setpwent))

(defun is-administrator ()
  "Return true if you are root, or effectively root."
  (= (geteuid) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Group database

(defcstruct foreign-group
  "Group database entry."
  (gr_name	:string)
  (gr_passwd	:string)
  (gr_gid	gid-t)
  (gr_mem	:pointer))

(defstruct group-entry
  "Group database entry."
  name
  passwd
  gid
  members)

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
	(make-group-entry
	 :name   gr_name
	 :passwd gr_passwd
	 :gid    gr_gid
	 :members
	 (loop :with i = 0
	    :while (not (null-pointer-p (mem-aref gr_mem :pointer i)))
	    :collect (mem-aref gr_mem :string i)
	    :do (incf i))))))

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

(defcfun ("setgrent" real-setgrent) :void)
(defun setgrent ()
  (real-setgrent))

(defun group-name (&optional id)
  "Return the name of the group with ID. Defaults to the current group."
  (let ((gr (getgrgid (or id (getgid)))))
    (and gr (group-entry-name gr))))

(defun group-id (&optional name)
  "Return the ID of the group NAME. Defaults to the current group."
  (if name
      (group-entry-gid (getgrnam name))
      (getgid)))

(defun get-next-group ()
  "Return the next group structure from the group database."
  (getgrent))

(defun group-list ()
  "How to annoy people in large organizations."
  (unwind-protect
       (progn
	 (setgrent)
	 (loop :with g :while (setf g (get-next-group)) :collect g))
    (endgrent)))

(defun refresh-group-list ()
  "Just in case you are bored, this will make get-next-group or group-list
return potentially updated data."
  (endgrent)
  (setgrent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Login/accounting database

;; @@@ Solaris isn't really done yet.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or darwin linux solaris freebsd) (config-feature :os-t-has-utmpx))

#+os-t-has-utmpx
(eval-when (:compile-toplevel :load-toplevel :execute)
(define-constants #(
;; Name          D    L   S   F   O
#(+UTX-USERSIZE+ 256  32  32  32  nil "Size of utmpx.ut_user.")
#(+UTX-IDSIZE+	 4    4   4   8   nil "Size of utmpx.ut_id.")
#(+UTX-LINESIZE+ 32   32  32  16  nil "Size of utmpx.ut_line.")
#(+UTX-HOSTSIZE+ 256  256 256 128 nil "Size of utmpx.ut_host."))))

#+os-t-has-utmpx
(progn
(define-constants #(
;; Name               D   L   S   F   O
#(+UTX-EMPTY+	      0   0   0   0   nil "No valid user accounting information.")
#(+UTX-RUN-LVL+	      1   1   1   nil nil "Run level. For Compatibility, not used.")
#(+UTX-BOOT-TIME+     2   2   2   1   nil "Time of a system boot.")
#(+UTX-OLD-TIME+      3   4   3   2   nil "Time before system clock change.")
#(+UTX-NEW-TIME+      4   3   4   3   nil "Time after system clock change.")
#(+UTX-INIT-PROCESS+  5   5   5   5   nil "A process spawned by init(8).")
#(+UTX-LOGIN-PROCESS+ 6   6   6   6   nil "The session leader of a logged-in user.")
#(+UTX-USER-PROCESS+  7   7   7   4   nil "A user process.")
#(+UTX-DEAD-PROCESS+  8   8   8   7   nil "A session leader exited.")
#(+UTX-ACCOUNTING+    9   9   9   nil nil "")
#(+UTX-SIGNATURE+     10  nil 10  nil nil "")
#(+UTX-SHUTDOWN-TIME+ 11  nil 11  8   nil "Time of system shutdown (extension)")))

#+darwin
(progn
  (defconstant +UTMPX-AUTOFILL-MASK+              #x8000
    "Fill in missing data.")
  (defconstant +UTMPX-DEAD-IF-CORRESPONDING-MASK+ #x4000
    "Only if existing live one."))

(defparameter +utmpx-type+
  #+darwin
  #(:EMPTY :RUN-LVL :BOOT-TIME :OLD-TIME :NEW-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  #+linux
  #(:EMPTY :RUN-LVL :BOOT-TIME :NEW-TIME :OLD-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  #+freebsd
  #(:EMPTY :BOOT-TIME :OLD-TIME :NEW-TIME :USER-PROCESS :INIT-PROCESS
    :LOGIN-PROCESS :DEAD-PROCESS :SHUTDOWN-TIME)
  #-(or darwin linux freebsd) ;; @@@ not really right
  #(:EMPTY :RUN-LVL :BOOT-TIME :NEW-TIME :OLD-TIME :INIT-PROCESS :LOGIN-PROCESS
    :USER-PROCESS :DEAD-PROCESS :ACCOUNTING :SIGNATURE :SHUTDOWN-TIME)
  "utmpx type keywords.")

#+darwin
(defcstruct foreign-utmpx
  "User accounting database entry."
  (ut_user :char :count #.+UTX-USERSIZE+)  ;; login name
  (ut_id   :char :count #.+UTX-IDSIZE+)	   ;; id
  (ut_line :char :count #.+UTX-LINESIZE+)  ;; tty name
  (ut_pid  pid-t)			   ;; process id creating the entry
  (ut_type :short)			   ;; type of this entry
  (ut_tv   (:struct foreign-timeval))	   ;; time entry was created
  (ut_host :char :count #.+UTX-HOSTSIZE+)  ;; host name
  (ut_pad  :uint32 :count 16)		   ;; reserved for future use
  )

#+linux
(defcstruct foreign-exit-status
  "utmpx exit status"
  (e-termination :short)
  (e-exit :short))

#+linux 
(defcstruct foreign-utmp-timeval
  (tv_sec  :int32)
  (tv_usec :int32))

#+linux
(defcstruct foreign-utmpx
  "User accounting database entry."
  (ut_type :short)			   ;; type of this entry
  (ut_pid  pid-t)			   ;; process id creating the entry
  (ut_line :char :count #.+UTX-LINESIZE+)  ;; tty name
  (ut_id   :char :count #.+UTX-IDSIZE+)	   ;; id
  (ut_user :char :count #.+UTX-USERSIZE+)  ;; login name
  (ut_host :char :count #.+UTX-HOSTSIZE+)  ;; host name
  (ut_exit (:struct foreign-exit-status))
  (ut_session :int32)
  (ut_tv   (:struct foreign-utmp-timeval)) ;; time entry was created
  (ut_addr_v6 :int32 :count 4)		   ;; ipv6 address?
  (ut_pad  :char :count 20)		   ;; reserved for future use
  )

#+freebsd
(defcstruct foreign-utmpx
  "User accounting database entry."
  (ut_type :short)			  ;; Type of entry.
  (ut_tv (:struct foreign-timeval))	  ;; Time entry was made.
  (ut_id :char :count #.+UTX-IDSIZE+)	  ;; Record identifier.
  (ut_pid pid-t)			  ;; Process ID.
  (ut_user :char :count #.+UTX-USERSIZE+) ;; User login name.
  (ut_line :char :count #.+UTX-LINESIZE+) ;; Device name.
  (ut_host :char :count #.+UTX-HOSTSIZE+) ;; Remote hostname.
  )

(defstruct utmpx
  "User accounting databse entry."
  user
  id
  line
  pid
  type
  tv
  host)

(defun convert-utmpx (u)
  (if (and (pointerp u) (null-pointer-p u))
      nil
      (with-foreign-slots ((ut_user ut_id ut_line ut_pid ut_type ut_tv ut_host)
			   u (:struct foreign-utmpx))
	(let ((id (make-array +UTX-IDSIZE+
			      :element-type 'fixnum :initial-element 0)))
	  ;; ut_id isn't really guaranteed to be a string or anything.
	  (loop :with c :for i :from 0 :below +UTX-IDSIZE+
	     :do (setf c (mem-aref ut_id :char i))
	     :while (not (zerop c))
	     :do (setf (aref id i) c))
	  (make-utmpx
	   :user  (foreign-string-to-lisp ut_user :max-chars +UTX-USERSIZE+)
	   :id	  id
	   :line  (foreign-string-to-lisp ut_line :max-chars +UTX-LINESIZE+)
	   :pid	  ut_pid
	   :type  (aref +utmpx-type+ ut_type)
	   :tv	  (make-timeval :seconds (getf ut_tv 'tv_sec)
				 :micro-seconds (getf ut_tv 'tv_usec))
	   :host  (foreign-string-to-lisp ut_host :max-chars +UTX-HOSTSIZE+))))))

(defcfun endutxent :void
  "Close the utmpx database.")

(defcfun ("getutxent" real-getutxent) (:pointer (:struct foreign-utmpx))
  "Read the next entry from the utmpx database. Open it if it's not open.")

(defun getutxent ()
  "Read the next entry from the utmpx database. Open it if it's not open."
  (convert-utmpx (real-getutxent)))

(defcfun ("getutxid" real-getutxid) (:pointer (:struct foreign-utmpx))
  "Read the next entry of type specified by the ut_type field, from the utmpx
database. Open it if it's not open."
  (id (:pointer (:struct foreign-utmpx))))

(defun getutxid (id)
  "Read the next entry of type specified by the ut_type field, from the utmpx
database. Open it if it's not open."
  (with-foreign-object (u '(:struct foreign-utmpx))
    (setf (foreign-slot-value u '(:struct foreign-utmpx) 'ut_type) id)
    (convert-utmpx (real-getutxid u))))

(defcfun ("getutxline" real-getutxline) (:pointer (:struct foreign-utmpx))
  "Read the next entry of type LOGIN_PROCESS or USER_PROCESS where the ut_line
field matches LINE, from the utmpx database. Open it if it's not open."
  (line (:pointer (:struct foreign-utmpx))))

(defun getutxline (line)
  "Read the next entry of type LOGIN_PROCESS or USER_PROCESS where the ut_line
field matches LINE, from the utmpx database. Open it if it's not open."
  (with-foreign-object (u '(:struct foreign-utmpx))
    (setf (foreign-slot-value u '(:struct foreign-utmpx) 'ut_line) line)
    (convert-utmpx (real-getutxline u))))

(defcfun ("pututxline" real-pututxline) (:pointer (:struct foreign-utmpx))
  "Put the entry UTX into the utmpx database, replacing the entry for the same
user. Probably requires root."
  (utx (:pointer (:struct foreign-utmpx))))

;; @@@ This probably hasn't been tested or checked for portability.
(defun pututxline (line)
  "Put the entry UTX into the utmpx database, replacing the entry for the same
user. Probably requires root."
  (check-type line utmpx)
  (with-foreign-object (u '(:struct foreign-utmpx))
    (with-foreign-slots ((ut_user ut_id ut_line ut_pid ut_type ut_tv ut_host
			  ut_pad)
			 u (:struct foreign-utmpx))
      (with-slots (user id line pid type tv host) line
	(setf ut_user user
	      ut_id   id  ; @@@ This probably requres special treatment.
	      ut_line line
	      ut_pid  pid
	      ut_type type
	      (foreign-slot-value ut_tv '(:struct foreign-timeval) 'tv_sec)
	      (timeval-seconds tv)
	      (foreign-slot-value ut_tv '(:struct foreign-timeval) 'tv_usec)
	      (timeval-micro-seconds tv)
	      ut_host host)
	(convert-utmpx (real-getutxline u))))))

(defcfun setutxent :void
  "Reset the utmpx database to the beginning.")

(defvar *utxdb-types* nil "Numeric types for utmpx databases.")
(define-enum-list *utxdb-types*
    #(#(+UTXDB-ACTIVE+    "Active login sessions.")
      #(+UTXDB-LASTLOGIN+ "Last login sessions.")
      #(+UTXDB-LOG+       "Log indexed by time.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+freebsd (config-feature :os-t-has-setutxdb)
  #+linux (config-feature :os-t-has-utmpname)
  )

#+os-t-has-setutxdb
(progn
  (defparameter *default-utmpx-files*
    `((,+UTXDB-ACTIVE+    . "/var/run/utx.active")
      (,+UTXDB-LASTLOGIN+ . "/var/log/utx.lastlogin")
      (,+UTXDB-LOG+       . "/var/log/utx.log")))

  (defcfun ("setutxdb" real-setutxdb) :int
    "Select the utmpx database to read from."
    (type :int) (file :string))
  
  (defun setutxdb (type &optional (file (default-utmpx-file type)))
    "Select the utmpx database to read from."
    (syscall (real-setutxdb type file)))

  (defun set-utmp-file (type &optional (file (default-utmpx-file type)))
    "Set the utmp file base on TYPE and FILE. If FILE isn't given, it picks
the default name for the TYPE."
    (setutxdb type file)))

#+os-t-has-utmpname
(progn
  ;; @@@ This is what it is on linux. We might have to change for others.
  (defparameter *default-utmpx-files*
    `((,+UTXDB-ACTIVE+    . "/var/run/utmp")
      (,+UTXDB-LASTLOGIN+ . "/var/log/lastlog")
      (,+UTXDB-LOG+       . "/var/log/wtmp")))

  (defcfun utmpname :int (file :string))

  (defun set-utmp-file (type &optional (file (default-utmpx-file type)))
    "Set the utmp file base on TYPE and FILE. If FILE isn't given, it picks
the default name for the TYPE."
    (declare (ignorable type))
    (utmpname file)))

;; @@@ Every system has something, so maybe this won't get used except in
;; initial porting?
#-(or os-t-has-setutxdb os-t-has-utmpname)
(progn
  ;; @@@ Just guessing.
  (defparameter *default-utmpx-files*
    `((,+UTXDB-ACTIVE+    . "/var/run/utmp")
      (,+UTXDB-LASTLOGIN+ . "/var/log/lastlog")
      (,+UTXDB-LOG+       . "/var/log/wtmp")))

  (defun set-utmp-file (type &optional file)
    "This is a stub."
    (declare (ignore type file))))

(defun default-utmpx-file (type)
  "Return the default file name for the utmpx TYPE."
  (or (cdr (assoc type *default-utmpx-files*))
      (error 'opsys-error
	     :format-control "Unknown utmpx file type: ~d."
	     :format-arguments `(,type))))

(defun guess-utmpx-file-type (file)
  "Try to guess the type given the file name. Note that is only based on the
name, not the contents. Return NIL if we don't have a guess."
  (car (rassoc file *default-utmpx-files* :test #'equal)))

) ;; os-t-has-utmpx

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+openbsd (config-feature :os-t-has-utmp))

#+os-t-has-utmp
(progn
  (defconstant +UT-NAMESIZE+ 32)
  (defconstant +UT-LINESIZE+ 8)
  (defconstant +UT-HOSTSIZE+ 256)

  (defparameter *utmp-paths*
    '((:utmp    . "/var/run/utmp")
      (:wtmp    . "/var/log/wtmp")
      (:lastlog . "/var/log/lastlog")))

  (defun bytes-to-string (bytes)
    (with-output-to-string (str)
      (loop :for b :across bytes
	 :while (not (zerop b))
	 :do (princ (code-char b) str))))

  (defun read-utmp-time (stream)
    (let* ((time-size (cffi:foreign-type-size 'time-t))
	   (time-buf (make-array time-size :element-type '(unsigned-byte 8))))
      (when (not (eql time-size (read-sequence time-buf stream)))
	(throw 'eof nil))
      (loop :with result integer = 0
	 :for i :from 0 :below time-size
	 :do (setf result (ash (aref time-buf i) (* i 8)))
	 :finally (return result))))

  (defun read-utmp-string (stream length)
    (let ((buf (make-array length :element-type '(unsigned-byte 8))))
      (when (not (eql length (read-sequence buf stream)))
	(throw 'eof nil))
      ;;(char-util:utf8-bytes-to-string buf)
      (bytes-to-string buf)
      ))

  (defstruct utmp
    time
    line
    name
    host)

  (defun read-utmp-record (stream type)
    (catch 'eof
      (ecase type
	(:last
	 (make-utmp :time (read-utmp-time stream)
		    :line (read-utmp-string stream +UT-LINESIZE+)
		    :host (read-utmp-string stream +UT-HOSTSIZE+)))
	((:utmp :wtmp)
	 (make-utmp :line (read-utmp-string stream +UT-LINESIZE+)
		    :name (read-utmp-string stream +UT-NAMESIZE+)
		    :host (read-utmp-string stream +UT-HOSTSIZE+)
		    :time (read-utmp-time stream))))))

  (defun get-utmp-entries (&optional (which :utmp))
    (let ((path (cdr (assoc which *utmp-paths*))))
      (with-open-file (stream path :direction :input
			      :element-type '(unsigned-byte 8))
	(loop :with record
	   :while (setf record (read-utmp-record stream which))
	   :collect record)))))

;; This is usually done only when you "log in", like with the window system or
;; like in the ssh deamon. See getlogin.
#+darwin (defcfun setlogin :int (name :string))

(defun users-logged-in ()
  "Return a list of names of logged in users."
  #+os-t-has-utmpx
  (unwind-protect
    (progn
      (setutxent)
      (let (u)
	(loop :while (setf u (getutxent))
	   ;; :if (not (eq (utmpx-type u) :dead-process))
	   :if (eq (utmpx-type u) :user-process)
	   :collect
	   (utmpx-user u))))
    (endutxent))
  #+os-t-has-utmp
  (mapcar #'utmp-name (get-utmp-entries))
  #-(or os-t-has-utmpx os-t-has-utmp)
  (missing-implementation 'users-logged-in))

(defcfun getuid uid-t)
(defcfun getgid uid-t)
(defcfun geteuid uid-t)
(defcfun getegid uid-t)
(defcfun setuid :int (uid uid-t))
(defcfun setgid :int (gid uid-t))
(defcfun seteuid :int (uid uid-t))
(defcfun setegid :int (gid uid-t))

;; int getgroups(int gidsetsize, gid_t grouplist[]);
(defcfun getgroups :int (gid-set-size :int) (group-list (:pointer gid-t)))

;; getgroups vs. getgrouplist & NGROUPS_MAX .etc
;; I should not be suprised at this point at how un-good the typical unix
;; group interfaces are.

(defun get-groups ()
  "Return an array of group IDs for the current process."
  (let* ((size (syscall (getgroups 0 (null-pointer))))
	 (result (make-array `(,size) :element-type 'fixnum)))
    (with-foreign-object (group-list 'gid-t size)
      (syscall (getgroups size group-list))
      (loop :for i :from 0 :below size
	 :do (setf (aref result i) 
		   (mem-aref group-list 'gid-t i))))
    result))

(defun member-of (group)
  "Return true if the current user is a member of GROUP."
  (and (position group (get-groups)) t))

;; setgroups?

;; End
