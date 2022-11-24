;;;
;;; sed-cmds.lisp - Commands for sed.
;;;

(in-package :sed)

(lish:defcommand sed
  ((quiet boolean :short-arg #\n :help "Don't automatically output lines.")
   (in-place boolean :short-arg #\i
    :help "Pretend to edit the file in place, but actually make a backup copy,
 with the file name described by the -B option.")
   (backup-pattern string :short-arg #\B :default ".bak"
    :help "A pattern for making backup files, where ‘*’ is replaced with the
 file name. If a file already exists with the backup name, a number is
appended.")
   (delete-backup boolean :long-arg "delete-backup" :optional t
    :help "When doing in-place editing, delete the backup file afterwards.")
   (separate boolean :short-arg #\s
    :help "Treat input files as separate for line number counting. Otherwise
 treat input files as one concatenated file. If in-place is true, separte is
 forced true.")
   (collect boolean :short-arg #\c :default (lish:accepts 'sequence)
    :help "Return output as a collection of lines, instead of normal output.")
   ;;(expression object :short-arg #\e :optional nil :help "Script to run.")
   (expression object :optional nil :help "Script to run.")
   (files pathname :optional t :repeating t :help "Files to use as input."))
  "Stream editor."
  (when in-place
    (setf separate t))
  ;; (format t "expr = ~s~%files = ~s~%" expression files)
  (cond
   ((and files (not separate))
    ;; Treat all input files as one stream.
    (let (streams)
      (unwind-protect
	   (setf lish:*output*
		 (edit-stream expression
		   :input
		   (apply #'make-concatenated-stream
			  (mapcar (_ (let ((str (open _ :direction :input)))
				       (push str streams)
				       str))
				  files))
		   :quiet quiet
		   :collect collect))
	(map nil (_ (close _)) streams)))
    lish:*output*)
   ((null files)
    (setf lish:*output*
	  (edit-stream expression
		       :input *standard-input*
		       :output *standard-output*
		       :quiet quiet
		       :in-place in-place
		       :backup-pattern backup-pattern
		       :delete-backup delete-backup
		       :collect collect)))
   (t
    (setf lish:*output*
	  (loop :for f :in files
	    ;; @@@ we should pull the compiling out, so it's only done once
	    :collect
	    (edit-stream expression
			 :input f
			 :output (if in-place f *standard-output*)
			 :quiet quiet
			 :in-place in-place
			 :backup-pattern backup-pattern
			 :delete-backup delete-backup
			 :collect collect))))))

;; End
