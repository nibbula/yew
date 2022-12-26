;;;
;;; ps-cmds.lisp - Commands for ps.
;;;

(in-package :ps)

(lish:defcommand ps-tree ()
  "Show a tree of processes."
  (ps-tree))

(lish:defcommand ps
  ((matching string :help "Only show processes matching this.")
   (show-kernel-processes boolean :short-arg #\k :default nil
    :help "True to show kernel processes.")
   (user user :short-arg #\u :help "User to show processes for.")
   ;; (sort-by choice :short-arg #\s :default "size" :help "Field to sort by."
   ;; 	    #| :choice-func #'process-columns |# )
   (long boolean :short-arg #\l :help "True to show the long output.")
   (quiet boolean :short-arg #\q :help "True to suppress printing output."))
  "Process status."
  (setf lish:*output* (describe-processes
		       :matching (or matching lish:*input*)
		       :show-kernel-processes show-kernel-processes
		       :user user :quiet quiet :long long)))

;; End
