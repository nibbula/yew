;;;								-*- Lisp -*-
;;; terminal-null.asd - System definition for terminal-null
;;;

(defsystem terminal-null
    :name               "terminal-null"
    :description        "A terminal that doesn't do anything."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A terminal that doesn't do anything."
    :depends-on (:dlib :terminal :dgray)
    :components
    ((:file "terminal-null")))
