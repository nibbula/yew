;;;								-*- Lisp -*-
;;; terminal-x11.asd - System definition for terminal-x11
;;;

(defsystem terminal-x11
    :name               "terminal-x11"
    :description        "X11 window as a terminal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "X11 window as a terminal."
    :depends-on (:dlib :dlib-misc :terminal :opsys :trivial-gray-streams :clx
		 :collections :char-util :dcolor :grid :ochar :fatchar :stretchy
		 :keysyms)
    :components
    ((:file "terminal-x11")))
