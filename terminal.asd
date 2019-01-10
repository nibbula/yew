;;;								-*- Lisp -*-
;;; terminal.asd -- System definition for terminal
;;;

(defsystem terminal
    :name               "terminal"
    :description        "Generic terminality."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Manipulate an imaginary thing once called a terminal."
    :depends-on (:dlib :opsys :trivial-gray-streams :fatchar)
    :components
    ((:file "terminal")))
