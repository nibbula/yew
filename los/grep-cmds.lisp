;;;
;;; grep-cmds.lisp - Commands for grep.
;;;

(in-package :grep)

(lish:defcommand grep
  ((pattern string :help "Regular expression to search for.")
   (pattern-expression string :short-arg #\e
    :help "Regular expression to search for.")
   (files input-stream-or-filename
    :repeating t
    :help "Files to search in.")
   (files-with-match boolean
    :short-arg #\l
    :help "Print only the file name (list) once for matches.")
   (files-without-match boolean
    :short-arg #\L
    :help "Print only the file name (list) of files with no matches.")
   (ignore-case boolean
    :short-arg #\i
    :help "Ignore character case when matching.")
   (invert boolean
    :short-arg #\v
    :help "Only print lines that don't match.")
   (no-filename boolean
    :short-arg #\h
    :help "Never print filenames (headers) with output.")
   (line-number boolean
    :short-arg #\n
    :help "Print line numbers.")
   (quiet boolean
    :short-arg #\q
    :help "Don't produce output.")
   (fixed boolean
    :short-arg #\F
    :help "Search for a fixed strings, not regular expressions.")
   ;; (line-up boolean :short-arg #\l
   ;;  :help "Line up matches.")
   (use-color boolean
    :short-arg #\C :default t
    :help "Highlight substrings in color.")
   (recursive boolean :short-arg #\r
    :help "Recursively search directories.")
   (collect boolean
    :short-arg #\c
    :default '(lish:accepts :sequence)
    :use-supplied-flag t
    :help "Collect matches in a sequence.")
   (signal-errors boolean :short-arg #\E
    :help "Signal errors.")
   (print-errors boolean :short-arg #\P
    :help "Print errors *error-output*.")
   (positions boolean :short-arg #\p
    :help "Send positions to Lish output. Equivalent to -nqc, except
it's only quiet if the receiving command accepts sequences.")
   (input-as-files boolean :short-arg #\s
    :help "Treat *input* as files to search instead of text.")
   (unicode-normalize boolean :short-arg #\u :default t
    :help "Normalize unicode before comparison.")
   (unicode-remove-combining boolean :short-arg #\U
    :help "Normalize and remove unicode combining characters.")
   (filter function :short-arg #\f
    :help "Function to apply to strings before comparing."))
  :accepts (:stream :sequence)
  "Search for patterns in input."
  (let (result table)
    (cond
      ((and (lish:accepts :sequence) (not collect-supplied-p))
       (setf collect t)
       (when positions
	 (setf quiet t)))
      ((lish:accepts :grotty-stream)
       (setf use-color t))
      (t
       ;; (dbugf :accepts "grep output accepts ~s~%" lish::*accepts*)
       ))
    ;; (dbugf :accepts "no files given~%")
    ;; (dbugf :accepts "type-of *input* = ~s~%" (type-of lish:*input*))
    ;; (dbugf :accepts "*input* = ~s~%" lish:*input*)
    (when positions
      (setf line-number t collect t))
    (when (not (or pattern pattern-expression))
      (error "A pattern argument wasn't given."))
    (setf result
	  (cond
	    ((setf table (or (and lish:*input* (typep lish:*input* 'table)
				  lish:*input*)
			     (and files (typep files 'table)
				  files)))
	     ;; Default to collect when given a table.
	     (when (not collect-supplied-p)
	       (setf collect t))
	     (finish-output)
	     (grep-table (or pattern pattern-expression)
			 table
			 :fixed fixed
			 :ignore-case ignore-case
			 :invert invert
			 :line-number line-number
			 :quiet quiet
			 :use-color use-color
			 :collect collect
			 :unicode-normalize unicode-normalize
			 :unicode-remove-combining unicode-remove-combining
			 :filter filter))
	    (t
	     (grep-files (or pattern pattern-expression)
			 :input-lines (and (not input-as-files)
					   (not (streamp lish:*input*))
					   (typep lish:*input* 'sequence)
					   lish:*input*)
			 :files
			 (or files (and lish:*input*
					(typep lish:*input* 'sequence)
					(or input-as-files
					    ;; too dwim-ish??
					    (and (plusp (length lish:*input*))
						 (typep (elt lish:*input* 0)
							'pathname)))
					lish:*input*))
			 :files-with-match files-with-match
			 :files-without-match files-without-match
			 :no-filename no-filename
			 :fixed fixed
			 :ignore-case ignore-case
			 :invert invert
			 :line-number line-number
			 :quiet quiet
			 :use-color use-color
			 :recursive recursive
			 :collect collect
			 :signal-errors signal-errors
			 :print-errors print-errors
			 :unicode-normalize unicode-normalize
			 :unicode-remove-combining unicode-remove-combining
			 :filter filter))))
    (if collect
	(progn
	  (setf lish:*output* result))
	result)))

;; End
