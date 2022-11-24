;;;
;;; tr-cmds.lisp - Commands for tr.
;;;

(in-package :tr)

(lish:defcommand tr
  ((set1 string :help "Set of characters to translate from.")
   (set2 string :help "Set of characters to translate to.")
   (files input-stream-or-filename :help "Input file or stream.")
   (help boolean :long-arg "help" :help "Show the help."))
  "Translate characters."
  (when help
    (lish:print-command-help (lish:get-command "tr"))
    (return-from !tr (values)))
  (when (not set1)
    (error "Missing set1 argument."))
  (translate-characters :set1 set1 :set2 set2 :files files))

;; End
