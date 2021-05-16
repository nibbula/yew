;;;								-*- Lisp -*-
;;; xterm-control.asd - System definition for xterm-control
;;;

(defsystem xterm-control
    :name               "xterm-control"
    :description        "Control XTerm compatible terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Control XTerm compatible terminals."
    :depends-on (:dlib :dlib-misc :char-util :keymap :terminal :terminal-ansi
		 :inator :terminal-inator :rl :cl-ppcre)
    :components
    ((:file "xterm-control")))
