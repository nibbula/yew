;;;								-*- Lisp -*-
;;; pick-list.asd -- System definition for pick-list
;;;

(defsystem pick-list
    :name               "pick-list"
    :description        "Choose things from a list."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Choose things from a list."
    :depends-on (:dlib :curses :char-util :stretchy :keymap :opsys :inator :fui)
    :components
    ((:file "pick-list")))
