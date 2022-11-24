;;;
;;; cp-cmds.lisp - Commands for cp.
;;;

(in-package :cp)

(lish:defcommand cp
  ((no-overwrite boolean :short-arg #\n
    :help "Don't allow overwriting.")
   (overwrite boolean :short-arg #\o
    :help "Overwrite existing files in the destination without asking.")
   ;; (one-file-system boolean :short-arg #\x
   ;;  :help "Don't copy things that are on other file systems.")
   (sources pathname :repeating t :optional nil :help "Files to copy.")
   (destination pathname :optional nil :help "Where to copy to."))
  "Copy files."
  (format t "cp sources = ~s destination = ~s ~%" sources destination)
  (copy-files sources destination
	      :no-overwrite no-overwrite
	      :overwrite overwrite
	      #| :one-file-system one-file-system |#))

;; End
