;;;								-*- Lisp -*-
;;; tiny-repl.asd -- System definition for TINY-REPL
;;;

(defsystem tiny-repl
    :name               "tiny-repl"
    :description        "Replacement REPL with editing and history."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "Most likely if you're using RL, this is why.
If you want to run a lisp without slime, then this makes it usable.
Unfortunately the debugger is very shabby."
    :depends-on (:dlib :keymap :terminal :rl
		       :terminal-ansi
		       #+windows :terminal-ms
		       )
    :components
    ((:file "tiny-repl")))
