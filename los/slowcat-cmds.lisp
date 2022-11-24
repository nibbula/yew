;;;
;;; slowcat-cmds.lisp - Commands for slowcat.
;;;

(in-package :slowcat)

(lish:defcommand slowcat
  ((delay number :short-arg #\d :default .001 :help "Delay in seconds.")
   (unit choice :short-arg #\u :default :char :choices '(:line :char)
    :help "Whether ‘dalay’ applies to characters or lines.")
   (files pathname :repeating t
    :help "Files or stream-like things to use as input."))
  "Make things go by slowly, like the old days."
  (if files
      (loop :for f :in files :do
	(lish:with-streamlike-input (f)
	  (slowcat f :delay delay :unit unit)))
      (progn
	(slowcat *standard-input* :delay delay :unit unit))))

;; End
