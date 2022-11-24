;;;
;;; du-cmds.lisp - Commands for du.
;;;

(in-package :du)

(lish:defcommand du
  ((show-progress boolean :short-arg #\p :default nil
    :help "True to show progress while gathering data.")
   (all boolean :short-arg #\a :default t
    :help "True to include files as well as directories in the output.")
   (exclude case-preserving-object :short-arg #\e #| :repeating t |#
    :help "Exclude the sub-tree starting at these directories. Can be a
directory or a list of directories.")
   (one-file-system boolean :short-arg #\x
    :help "Don't count directories on different file systems.")
   (resume boolean :short-arg #\r
    :help "True to resume viewing the last tree.")
   (file pathname :short-arg #\f
    :help "Load a tree from a file.")
   (as-list boolean :short-arg #\l
    :help "Show as a flat list, instead of a tree.")
   (non-human-size boolean :short-arg #\h :help "Show sizes in bytes.")
   (directories directory :repeating t
    :help "Directory to show usage for."))
  :keys-as keys
  "Show disk usage."
  (when (not directories)
    (setf (getf keys :directories) '(".")))
  (typecase exclude
    (null)
    (symbol
     (setf (getf keys :exclude) (string exclude)))
    (list
     (setf (getf keys :exclude) (mapcar #'string exclude))))
  (setf lish:*output* (apply #'du keys)))

;; End
