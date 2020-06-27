;;;								-*- Lisp -*-
;;; puca.asd -- System definition for PUCA
;;;

(defsystem puca
    :name               "puca"
    :description        "Putative Muca"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description
    "Putative Muca. A simple interface to revision control software.
It doesn't do anything fancy. It just saves you from typing the commands.
It has an extremely limited vocabulary.
It currently knows how to talk to Git, SVN, CVS and Mercurial.
Except everything besides the Git backend has not been tested in many years."
    :depends-on (:dlib :dlib-misc :opsys :dtime :keymap :char-util :rl
		 :completion :inator :terminal :terminal-inator :fui :lish
		 :pager :view :options :fatchar :fatchar-io :table :collections
		 :table-viewer
		 #+use-re :re #-use-re :cl-ppcre)
    :components
    ((:file "puca")))
