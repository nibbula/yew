;;;								-*- Lisp -*-
;;; fui.asd -- System definition for fui
;;;

(defsystem fui
    :name               "fui"
    :description        "Fake UI"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "Totally fake old style user interface."
    :depends-on (:dlib :dlib-misc :stretchy :opsys :char-util :keymap :cffi
		 :curses :terminal :terminal-curses :inator)
    :components
    ((:file "fui")))
