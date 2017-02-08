;;;								-*- Lisp -*-
;;; xterm-control.asd - System definition for xterm-control
;;;

(defsystem xterm-control
    :name               "xterm-control"
    :description        "Control XTerm compatible terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Control XTerm compatible terminals."
    :depends-on (:dlib :dlib-misc :char-util :terminal :terminal-ansi
		 :keymap :inator :tiny-rl :cl-ppcre)
    :components
    ((:file "xterm-control")))
