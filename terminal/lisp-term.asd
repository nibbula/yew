;;;								-*- Lisp -*-
;;; lisp-term.asd - System definition for lisp-term
;;;

(defsystem lisp-term
    :name               "lisp-term"
    :description        "The outer part of terminal emulator."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "The outer part of terminal emulator."
    :depends-on (:dlib :opsys :terminal :terminal-ansi :ansi-terminal
		 :terminal-x11)
    :components
    ((:file "lisp-term")))
