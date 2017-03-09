;;;								-*- Lisp -*-
;;; tiny-rl.asd -- System definition for TINY-RL package
;;;

(defsystem tiny-rl
    :name               "tiny-rl"
    :description        "A line editor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "A line editor which is not so tiny."
    :depends-on (:dl-list :stretchy :cffi :opsys :termios
		 :terminal :terminal-ansi :terminal-curses :fatchar
		 :completion :dlib :dlib-misc :keymap :char-util :syntax-lisp
		 :unipose)
    :components
    ((:file "tiny-rl")))
