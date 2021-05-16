;;;								-*- Lisp -*-
;;; inator.asd -- System definition for inator
;;;

(defsystem inator
    :name               "inator"
    :description        "Generic UI app."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Generic UI app."
    :depends-on (:keymap :char-util)
    :components
    ((:file "inator")))
