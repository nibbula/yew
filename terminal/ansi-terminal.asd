;;;								-*- Lisp -*-
;;; ansi-terminal.asd - System definition for ansi-terminal
;;;

(defsystem ansi-terminal
    :name               "ansi-terminal"
    :description        "ANSI terminal emulation"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "ANSI terminal emulation"
    :depends-on (:dlib :collections :fatchar :fatchar-io :terminal :ansi :dcolor
		 :stretchy :dgray)
    :components
    ((:file "ansi-terminal")))
