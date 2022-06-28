;;;								-*- Lisp -*-
;;; terminal-utils.asd - System definition for terminal-utils
;;;

(defsystem terminal-utils
    :name               "terminal-utils"
    :description        "Terminal utilities."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Terminal utilities."
    :depends-on (:dlib :collections :fatchar :fatchar-io :ostring :terminal
		 :terminal-crunch :terminal-null)
    :components
    ((:file "terminal-utils")))
