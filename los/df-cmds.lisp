;;;
;;; df-cmds.lisp - Commands for df.
;;;

(in-package :df)

(lish:defcommand df
  ((include-dummies boolean :short-arg #\a
    :help "Include dummy file systems.")
   (show-type boolean :short-arg #\t
    :help "Show filesystem types.")
   (omit-header boolean :short-arg #\h
    :help "Omit the header.")
   (visual boolean :short-arg #\v :default t
    :help "Show free space visually.")
   (sizes-as-bytes boolean :short-arg #\b
    :help "Show sizes as bytes.")
   (sizes-as-blocks boolean :short-arg #\k
    :help "Show sizes as 1024 byte blocks.")
   (files pathname :repeating t
    :help "File systems to report on."))
  "Show how much disk is free. Lists mounted filesystems and shows usage
statisics for each one."
  :accepts '(or string pathname list)
  (setf lish:*output*
	(lish:with-files-or-input (files)
	  (df :files files
	      :include-dummies include-dummies :show-type show-type
	      :omit-header omit-header :visual visual
	      :sizes-as-bytes sizes-as-bytes
	      :sizes-as-blocks sizes-as-blocks))))

;; End
