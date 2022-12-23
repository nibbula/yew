;;;								-*- Lisp -*-
;;; terminal-x11.asd - System definition for terminal-x11
;;;

(defsystem terminal-x11
    :name               "terminal-x11"
    :description        "X11 window as a terminal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "X11 window as a terminal."
    :depends-on (:dlib :collections :opsys :dgray :stretchy :char-util :dcolor
		 :ochar :fatchar :dlib-misc :terminal :dtime :clx
		 :keysyms)
    :components
    ((:file "terminal-x11")))
