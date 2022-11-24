;;;
;;; tee-cmds.lisp - Commands for tee.
;;;

(in-package :tee)

#+lish
(lish:defcommand tee
  ((outputs pathname :repeating t :help "Files or streams to write output to."))
  "Copy input to multiple outputs."
  (apply #'tee outputs)
  (setf lish:*output* lish:*input*))

;; End
