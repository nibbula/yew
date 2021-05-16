;;;								-*- Lisp -*-
;;; terminal-dumb-color.asd - System definition for terminal-dumb-color
;;;

(defsystem terminal-dumb-color
    :name               "terminal-dumb-color"
    :description        "dumb color"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "very dumb color"
    :depends-on (:dlib :terminal :char-util :trivial-gray-streams :fatchar
		 :terminal-dumb :terminal-ansi)
    :components
    ((:file "terminal-dumb-color")))
