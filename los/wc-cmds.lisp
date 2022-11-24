;;;
;;; wc-cmds.lisp - Commands for wc.
;;;

(in-package :wc)

(lish:defcommand wc
  ((chars boolean :short-arg #\c :help "True to count characters.")
   (words boolean :short-arg #\w :help "True to words.")
   (lines boolean :short-arg #\l :help "True to count lines.")
   (print boolean :short-arg #\p :default t :help "True to print counts.")
   (collect boolean :short-arg #\C :help "True to collect results.")
   (signal-errors boolean :short-arg #\E
    :help "True to signal errors. Otherwise print them to *error-output*.")
   (files pathname :repeating t :help "Files to count."))
  :accepts (:stream :sequence)
  "Count words, lines, and characters. Return a list of in the order:
lines, words, chars, containing only the total count if that item was specified.
When COLLECT is true, return a list of the total counts and the collected list
of struct COUNT-ITEM."
  (when (not files)
    (setf files
	  (if (and lish:*input*
		   (typep lish:*input* 'sequence)
		   ;; @@@ This is semi-bogus. What if we want to count the
		   ;; contents of a string?
		   (typep (oelt lish:*input* 0) '(or string pathname)))
	      lish:*input*
	      (list *standard-input*))))
  (let ((*signal-errors* signal-errors))
    (setf lish:*output*
	  (if collect
	      (multiple-value-list
	       (count-text files :chars chars :words words :lines lines
			   :print print :collect collect))
	      (count-text files :chars chars :words words :lines lines
			  :print print :collect collect)))))

;; End
