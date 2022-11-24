;;;
;;; snip-cmds.lisp - Commands for snip.
;;;

(in-package :snip)

(lish:defcommand snip
  ((pattern	string #| regexp |# :optional nil
;;    :help "A regular expression to search for or an integer byte offset.")
    :help "A regular expression to search for.")
   (source	input-stream-or-filename :default '*standard-input*
    :help "Input file or stream to read from.")
   (before	boolean :short-arg #\b :long-arg "before":default nil
    :help "True to cut from before the pattern.")
   (after	boolean :short-arg #\a :long-arg "after" :default nil
    :help "True to cut from after the pattern."))
  "Snip output before or after a pattern."
  (when (not (or before after))
    (error "You probably should specify either BEFORE or AFTER."))
  (let ((saw-it nil))
    (cond
      (before
       (with-lines (line source)
	 (if saw-it
	     (progn (write-string line) (terpri))
	     (when (ppcre:all-matches pattern line)
	       (setf saw-it t)))))
      (after
       (with-lines (line source)
	 (progn (write-string line) (terpri))
	 (when (ppcre:all-matches pattern line)
	   (finish-output)
	   (return)))))
    (finish-output)))

(lish:defcommand head
  ((line-count integer :short-arg #\n :default 10 :help "Lines to show.")
   (byte-count integer :short-arg #\c :help "Bytes to show.")
   (collect boolean :short-arg #\C :default (lish:accepts 'sequence)
    :help "Return a sequence.")
   (quiet boolean :short-arg #\q :help "Don't produce output.")
   (use-encoding boolean :short-arg #\e
    :help "Use the default system encoding.") ; otherwise known as: get errors
   ;; ("count" integer :default 10
   ;;  :help "The number of units to show.")
   (files pathname :repeating t
    :help "Files to use as input."))
  "Output the first portion of input."
  (flet ((do-snip (stream)
	   (if collect
	       (setf lish:*output* (if quiet
				  (take-lines stream line-count)
				  (snip-and-collect stream line-count)))
	       (snip-lines-after stream line-count)))
	 (do-byte-snip (stream)
	   (if collect
	       (setf lish:*output*
		     (snip-bytes stream byte-count :before :collect collect
				 :quiet quiet))
	       (snip-bytes stream byte-count :before :quiet quiet))))
    (if byte-count
	(if files
	    (loop :for f :in files :do
	      (do-byte-snip f))
	    (do-byte-snip *standard-input*))
	(if files
	    (loop :for f :in files :do
	      (if use-encoding
		  (do-snip f)
		  (with-utf8b-input (str f :errorp nil)
		    (do-snip str))))
	    (if use-encoding
		(do-snip *standard-input*)
		(with-utf8b-input (str *standard-input* :errorp nil)
		  (do-snip str)))))))

;; End
