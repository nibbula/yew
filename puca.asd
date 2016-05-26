;;;								-*- Lisp -*-
;;; puca.asd -- System definition for PUCA
;;;

(defpackage :puca-system
    (:use :common-lisp :asdf))

(in-package :puca-system)

(defsystem puca
    :name               "puca"
    :description        "Putative Muca"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description
    "Putative Muca. A simple interface to revision control software.
It doesn't do anything fancy. It just saves you from typing the commands.
It has an extremely limited vocabulary.
It currently knows how to talk to Git, SVN, CVS an Mercurial."
    :depends-on (:dlib :dlib-misc :opsys :keymap :char-util :curses :tiny-rl
		 :completion :fui :lish :pager)
    :components
    ((:file "puca")))
