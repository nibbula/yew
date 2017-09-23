;;;								-*- Lisp -*-
;;; terminal-curses.asd -- System definition for terminal-curses
;;;

(defsystem terminal-curses
    :name               "terminal-curses"
    :description        "Faking a terminal with curses."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "So many layers of fake like a cake."
    :depends-on (:terminal :curses :opsys :trivial-gray-streams)
    :components
    ((:file "terminal-curses")))
