;;;								-*- Lisp -*-
;;; inator.asd -- System definition for inator
;;;

(defsystem inator
    :name               "inator"
    :description        "Generic UI app."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description   "Generic UI app."
    :depends-on (:keymap :char-util)
    :components
    ((:file "inator")))
