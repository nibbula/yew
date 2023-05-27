;;;								-*- Lisp -*-
;;; altchar.asd - System definition for altchar
;;;

(defsystem altchar
    :name               "altchar"
    :description        "Type with alternate alphabets."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Type with alternate alphabets."
    :depends-on (:dlib :char-util :keymap :options :collections :fatchar :rl
		 :pick-list :inator)
    :components
    ((:file "altchar")))
