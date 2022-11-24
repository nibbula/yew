;;;
;;; sort-by-cmds.lisp - Commands for sort-by.
;;;

(in-package :sort-by)

(lish:defcommand sort-by
  ((predicate object
    :required t
    :help
    "A function designator which returns true for items to keep from sequence.")
   (key object :help "A key to sort by.")
   (sequence object :short-org #\s :help "A sequence to use instead of *input*.")
   (print boolean :short-arg #\p :help "Print the result."))
  "Sort a collection by a predicate or key and predicate."
  :accepts 'collection
  (dbugf :sort-by "------~%predicate ~s ~s~%key ~s ~s~%sequence ~s ~s~%"
	 (type-of predicate) predicate
	 (type-of key) key
	 (type-of sequence) sequence)
  (prog1 (setf lish:*output*
	       (sort-by predicate (or lish:*input* sequence) :key key))
    (when print
      (typecase lish:*output*
	(table
	 (with-grout ()
	   (grout-print-table lish:*output*)))
	(t
	 (write lish:*output*)
	 (terpri))))))

;; End
