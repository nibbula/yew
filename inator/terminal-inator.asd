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
    :depends-on (:dlib :dlib-misc :keymap :inator :terminal :fui)
    :components
    ((:file "terminal-inator")))
