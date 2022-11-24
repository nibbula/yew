;;;
;;; df-cmds.lisp - Commands for df.
;;;

(in-package :df)

(lish:defcommand df
  ((include-dummies boolean :short-arg #\a
    :help "True to include dummy file systems.")
   (show-type boolean :short-arg #\t
    :help "True to show filesystem types.")
   (omit-header boolean :short-arg #\h
    :help "True to omit the header.")
   (visual boolean :short-arg #\v :default t
    :help "True to show free space visually.")
   (files pathname :repeating t
    :help "File systems to report on."))
  "Show how much disk is free. Lists mounted filesystems and shows usage
statisics for each one."
  (setf lish:*output*
	(df :files files :include-dummies include-dummies :show-type show-type
	    :omit-header omit-header :visual visual)))

;; End
