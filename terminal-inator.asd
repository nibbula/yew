;;;								-*- Lisp -*-
;;; terminal-inator.asd - System definition for terminal-inator
;;;

(defsystem terminal-inator
    :name               "terminal-inator"
    :description        "Inator for terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Inator for terminals."
    :depends-on (:inator :terminal)
    :components
    ((:file "terminal-inator")))
