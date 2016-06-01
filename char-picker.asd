;;;								-*- Lisp -*-
;;; char-picker.asd -- System definition for char-picker
;;;

(defsystem char-picker
    :name               "char-picker"
    :description        "Pick a character."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Pick a character."
    :depends-on (:dlib :stretchy :char-util :keymap :curses :inator :fui
		 :terminal-curses)
    :components
    ((:file "char-picker")))
