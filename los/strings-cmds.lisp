;;;
;;; strings-cmds.lisp - Commands for strings.
;;;

(in-package :strings)

(lish:defcommand strings
  ((show-all boolean
    :short-arg	#\a
    :prompt	"Show All? "
    :help
    "Look for strings in all sections of the object file (including the text section).")
   (show-offset	boolean
    :short-arg	#\o
    :prompt	"Show Offset"
    :help	"Preceded each string by its offset in the file (in decimal).")
   (offset-format choice
    :short-arg	#\t
    :default	:decimal
    :prompt	"Offset format"
    :help	"Base for byte offset."
    :choices	'("octal" "decimal" "hex"))
   (minimum-string-length integer
    :short-arg	#\n
    :default	4
    :help	"The minimum string length.")
   (utf-16 boolean
    :short-arg  #\u
    :help       "True to pretend strings might be in UTF-16.")
   (ascii-only boolean
    :short-arg  #\A
    :help       "Look for ASCII strings only.")
   (files	pathname
    :default	"-"
    :repeating	t
    :help	"An input file name."))
  "Try to extract human readable strings from data."
  (declare (ignore show-all show-offset offset-format))
  (loop :for f :in (or files (list *standard-input*))
     :do
     (if utf-16
       (strings-file-16 f :minimum-string-length minimum-string-length
			  :ascii-only ascii-only)
       (strings-file f :minimum-string-length minimum-string-length
		       :ascii-only ascii-only))))

;; End
