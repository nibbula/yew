;;;
;;; mkdir-cmds.lisp - Commands for mkdir.
;;;

(in-package :mkdir)

(lish:defcommand mkdir
  ((mode string :short-arg #\m
    :help "File permission bits that the directory is created with.")
   (make-parents boolean :short-arg #\p :default t
    :help "True to make any needed parent directories.")
   (verbose boolean :short-arg #\v :help "Describe what we're doing.")
   (directories pathname :repeating t :help "Directory to create."))
  :keys-as keys
  "Make directories."
  (apply #'mkdir keys))

;; End
