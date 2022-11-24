;;;
;;; who-cmds.lisp - Commands for who.
;;;

(in-package :who)

(defun who-user-name-list ()
  (append (los-util:user-name-list) (list "am" "i")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass lish-user::arg-who-user (lish-user::arg-user)
    ()
    (:default-initargs
     :choice-func #'who-user-name-list)
    (:documentation "User name.")))

(lish:defcommand who
  ((show-dead boolean :short-arg #\d
    :help "True to show dead processes.")
   (all boolean :short-arg #\a
    :help "True to show all processes, not only user processes.")
   (tty string :long-arg "tty"
    :help "Terminal to exclusively report on.")
   (file pathname :short-arg #\f
    :help "Name of a file to use as the database.")
   (users who-user :repeating t :help "Users to report on."))
  "Who is on."
  (when (equalp users '("am" "i"))
    (setf users (list (user-name))
	  tty (file-handle-terminal-name 0)))
  (setf lish:*output*
	(who :users users :show-dead show-dead :all all :tty tty :file file)))

(lish:defcommand lastlogin
  ((show-history boolean :short-arg #\h :help "True to show login history.")
   (users user :repeating t :help "User to show the last login for."))
  "Show users' last login."
  ;; @@@ There should be an option where it tries to figure out durations
  ;; for sessions and booted times, like the "last" command.
  (setf lish:*output*
	(who :users users :all t
	     :file (cond
		     (show-history
		      (default-utmpx-file +UTXDB-LOG+))
		     (t
		      ;; @@@ I can't be bothered to do this correctly now.
		      #+linux (default-utmpx-file +UTXDB-LOG+)
		      #-linux (default-utmpx-file +UTXDB-LASTLOGIN+))))))

(lish:defcommand uptime
  ((pretty boolean :short-arg #\p
    :help "True to show in a long drawn-out format, mostly for gloating.")
   (show-since boolean :short-arg #\s
    :help "True to show the time the system has been up since."))
  "Show how long the system has been running."
  (print-uptime :pretty pretty :show-since show-since))

(lish:defcommand w
  ((no-header boolean :short-arg #\h :help "True to omit the header.")
   (long boolean :short-arg #\l :help "True show the longer output.")
   (users user :repeating t :help "Users to show."))
  "Show who's doing what."
  (setf lish:*output* (who-what :users users :no-header no-header :long long)))

;; End
