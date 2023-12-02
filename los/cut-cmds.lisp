;;;
;;; cut-cmds.lisp - Commands for cut.
;;;

(in-package :cut)

(lish:defcommand cut
  ((bytes string :short-arg #\b #| :repeating t |#
    :help "Select bytes.")
   (characters string :short-arg #\c #| :repeating t |#
    :help "Select characters.")
   (fields string :short-arg #\f #| :repeating t |#
    :help "Print only specific fields.")
   (delimiter character :short-arg #\d :default #\tab
    :help "Character that separates fields.")
   (re-delimiter string :short-arg #\r
    :help "A Perl regular expression that separates fields.")
   (output-delimiter string :short-arg #\o
    :help "Character that separates fields.")
   (only-delimited boolean :short-arg #\s
    :help "True to omit lines without any delimiters with -f.")
   (collect boolean :short-arg #\C
    :help "True to collect output as lists.")
   (table boolean :short-arg #\t
    :help "True to collect output as a table.")
   (quiet boolean :short-arg #\q
    :help "True to suppress printing output lines.")
   (files input-stream-or-filename
    :default '(list *standard-input*) :repeating t
    :help "Files to read from."))
  :keys-as args
  "Remove sections of each line of input."
  (when (not files)
    (setf files (list *standard-input*)))
  (when table
    (setf (getf args :collect) t)
    (setf collect t))
  (remf args :table) ; fake arg, remove it no matter what
  (flet ((call-cut (f)
	   (apply #'cut-lines f args)))
    (let (results)
      (remf args :files)
      (loop :for f :in files :do
	(if collect
	    (push (call-cut f) results)
	    (call-cut f)))
      (when collect
	(setf lish:*output* (nreverse results))
	(when (= (length lish:*output*) 1)
	  (setf lish:*output* (first lish:*output*)))
	(if table
	    (setf lish:*output* (make-table-from lish:*output*))
	    lish:*output*)))))

;; End
