;;;
;;; where-cmds.lisp - Commands for where.
;;;

(in-package :where)

(lish:defcommand where
  ((predicate object
    :required t
    :help
    "A function designator which returns true for items to keep from sequence.")
   (print boolean :short-arg #\p :help "Print the result.")
   (sequence object :rest t
    :help "A sequence to use instead of *input*."))
  "A word which does more than one thing.
As an adverb, act like remove-if-not or opick.
As a verb, find where something is.
The default is to act like an adverb is a sequence-like thing is provided as
*input* or with the seqence argument, and as a verb if a sequence is not
provided."
  :accepts 'collection
  (dbugf :where "predicate ~s ~s~%sequence ~s ~s~%"
	 (type-of predicate) predicate
	 (type-of sequence) sequence)
  (prog1 (setf lish:*output*
	       (where predicate (or lish:*input* sequence)))
    (when print
      (write lish:*output*)
      (terpri))))

;; End
