;;;								-*- Lisp -*-
;;; fui.asd -- System definition for fui
;;;

(defsystem fui
    :name               "fui"
    :description        "Fake UI"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Totally fake old style user interface."
    :depends-on (:dlib :dlib-misc :stretchy :char-util :keymap :terminal
		 :inator :collections :fatchar :fatchar-io)
    :components
    ((:file "fui")))
