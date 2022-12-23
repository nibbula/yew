;;;
;;; dump-cmds.lisp - Commands for dump.
;;;

(in-package :dump)

(lish:defcommand dump
  ((files	pathname :repeating t   :help "Files to dump.")
   (line-length	integer  :short-arg #\l :help "Length of lines.")
   (show-offset	boolean  :short-arg #\o :help "True to show byte offset.")
   (start	integer  :short-arg #\s :help "Byte offset to start at.")
   (end		integer  :short-arg #\e :help "Byte offset to stop at.")
   (style choice :short-arg #\S :default :hex
    :choices '("hex" "braille")
    :help "Style of output."))
  "Dump data bytes."
  (when (and (not line-length) *terminal*)
    (setf line-length (terminal-window-columns *terminal*)))
  (lish:with-files-or-input (files)
    (if (not files)
	(dump *standard-input*
	      :line-length line-length
	      :show-offset show-offset
	      :start start :end end
	      :style (keywordify style))
	(loop :for f :in files :do
	  (dump f :line-length line-length
		  :show-offset show-offset
		  :start start :end end
		  :style (keywordify style))))))

;; End
