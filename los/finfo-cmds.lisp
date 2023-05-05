;;;
;;; finfo-cmds.lisp - Commands for finfo.
;;;

(in-package :finfo)

#+lish
(lish:defcommand finfo
  ((follow-links boolean :short-arg #\l
    :help "True to give information about the linked thing, not the link." )
   (style choice :short-arg #\s :default *default-style*
    :choices '("generic" "unix" "ms")
    :help "Operating system specific output style.")
   (collect boolean :short-arg #\c :help "True to collect results.")
   (files pathname :repeating t
    :help "The path names to give information about."))
  :accepts (string pathname sequence)
  :keys-as args
  "Print information about a file."
  (remf args :files)
  (when (not files)
    (if lish:*input*
	(setf files
	      (typecase lish:*input*
		((or string pathname) (list lish:*input*))
		(list lish:*input*)
		(sequence (map 'list #'identity lish:*input*))))
	(error "But what file do you want information about?")))
  (if collect
      (setf lish:*output* (apply #'finfo files args))
      (apply #'finfo files args)))

#+lish
(lish:defcommand stat
  ((follow-links boolean :short-arg #\L
    :help "True to give information about the linked thing, not the link." )
   (files pathname :repeating t
    :help "The path names to give information about."))
  :accepts (string pathname sequence)
  :keys-as args
  "Print information about a file."
  (remf args :files)
  (when (not files)
    (if lish:*input*
	(setf files
	      (typecase lish:*input*
		((or string pathname) (list lish:*input*))
		(list lish:*input*)
		(sequence (map 'list #'identity lish:*input*))))
	(error "But what file do you want information about?")))
  (mapc (_ (print-stat _ :follow-links follow-links)) files))

;; End
