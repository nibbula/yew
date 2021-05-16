;;;								-*- Lisp -*-
;;; curses-inator.asd - System definition for curses-inator
;;;

(defsystem curses-inator
    :name               "curses-inator"
    :description        "Inator for curses"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Provides a curses-inator and miscellanous utilities for curses apps."
    :depends-on (:dlib :dlib-misc :stretchy :opsys :char-util :keymap :cffi
		 :curses :terminal :terminal-curses :inator)
    :components
    ((:file "curses-inator")))
