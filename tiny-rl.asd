;;;								-*- Lisp -*-
;;; tiny-rl.asd -- System definition for TINY-RL package
;;;

(defpackage :tiny-rl-system
    (:use :common-lisp :asdf))

(in-package :tiny-rl-system)

(defsystem tiny-rl
    :name               "tiny-rl"
    :description        "A tiny readline replacement for ANSI terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description   "E PROTO 'SUP d00d"
    :depends-on (:dl-list :stretchy :cffi :opsys :ansiterm :termios
		 :completion :dlib :dlib-misc :keymap :char-util :syntax-lisp)
    :components
    ((:file "tiny-rl")))
