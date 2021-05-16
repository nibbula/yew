;;;								-*- Lisp -*-
;;; terminal-dumb.asd -- System definition for terminal-dumb
;;;

(defsystem terminal-dumb
    :name               "terminal-dumb"
    :description        "Faking a terminal with nothing."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "So many layers of fake like a cake."
    :depends-on (:dlib :terminal :char-util :trivial-gray-streams :fatchar)
    :components
    ((:file "terminal-dumb")))
