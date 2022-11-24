;;;
;;; free-cmds.lisp - Commands for free.
;;;

(in-package :free)

(defcommand free
  ((bytes boolean :short-arg #\b :help "Show the sizes in bytes.")
   (table boolean :short-arg #\t :help "Show as a table."))
  "Describe free memory."
  :args-as args
  (apply #'show-free-memory args))

;; End
