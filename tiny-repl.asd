;;;								-*- Lisp -*-
;;; tiny-repl.asd -- System definition for TINY-REPL
;;;

(defpackage :tiny-repl-system
    (:use :common-lisp :asdf))

(in-package :tiny-repl-system)

(defsystem tiny-repl
    :name               "tiny-repl"
    :description        "Replacement REPL with editing and history."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description   "Most likely if you're using tiny-rl, this is why.
If you want to run a lisp without slime, then this makes it usable.
Unfortunately the debugger is very shabby."
    :depends-on (:tiny-rl :dlib :termios)
    :components
    ((:file "tiny-repl")))
