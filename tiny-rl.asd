;;;								-*- Lisp -*-
;;; tiny-rl.asd -- System definition for TINY-RL package
;;;

(defpackage :tiny-rl-system
    (:use :common-lisp :asdf))

(in-package :tiny-rl-system)

(defsystem tiny-rl
    :name               "tiny-rl"
    :description        "A line editor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :long-description   "A line editor which is not so tiny."
    :depends-on (:dl-list :stretchy :cffi :opsys :termios
		 :terminal :terminal-ansi :terminal-curses
		 :completion :dlib :dlib-misc :keymap :char-util :syntax-lisp
		 :unipose)
    :components
    ((:file "tiny-rl")))
