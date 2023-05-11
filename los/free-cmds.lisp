;;;
;;; free-cmds.lisp - Commands for free.
;;;

(in-package :free)

(lish:defcommand free
  ((bytes boolean :short-arg #\b :help "Show the sizes in bytes.")
   (table boolean :short-arg #\t :help "Show as a table.")
   (lisp boolean :short-arg #\l :default t
    :help "Show lisp implementation memory."))
  :args-as args
  "Describe free memory."
  (setf lish:*output* (apply #'describe-free-memory args)))

;; End
