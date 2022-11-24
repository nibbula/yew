;;;
;;; uniq-cmds.lisp - Commands for uniq.
;;;

(in-package :uniq)

(lish:defcommand uniq
  ((file pathname :help "Files to show unique lines for.")
   (adjacent boolean :short-arg #\a
    :help "Only consider adjacent lines. This may be much more efficient.")
   (ignore-case boolean :short-arg #\i :help "Ignore case when matching.")
   (only-unique boolean :short-arg #\u
    :help "Print only lines that are not repeated.")
   (only-repeated boolean :short-arg #\d
    :help "Print only lines that are repeated.")
   (print-count boolean :short-arg #\c
    :help "Print a count of the number of times the line occurs.")
   (skip-chars integer :short-arg #\s
    :help "Number of characters to skip when comparing lines.")
   (skip-fields integer :short-arg #\f
    :help "Number of fields to skip when comparing lines.")
   (collect boolean :short-arg #\C :help "Collect output in a list.")
   (quiet boolean :short-arg #\q :help "Don't produce output.")
   (width-limit integer :short-arg #\w
    :help "Compare only this many characters of a line."))
  :args-as args
  :accepts sequence
  "Print unique lines. Note that unlike POSIX uniq, we consider non-adjacent 
duplicate lines, unless adjacent is specified."
  (when (not file)
    (setf (getf args :file)
	  (if (and lish:*input* (typep lish:*input* 'sequence))
	      lish:*input*
	      *standard-input*)))
  ;; (format *trace-output* "input=~s~%" (getf args :file))
  (if collect
      (setf lish:*output* (apply #'uniq args))
      (apply #'uniq args)))

;; End
