;;;								-*- Lisp -*-
;;; terminal.asd -- System definition for terminal
;;;

(defsystem terminal
    :name               "terminal"
    :description        "Generic terminality."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Manipulate an imaginary thing once called a terminal."
    :depends-on (:dlib :opsys :trivial-gray-streams :fatchar :terminal-config)
    :components
    ((:file "terminal")))
