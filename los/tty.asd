;;;								-*- Lisp -*-
;;; tty.asd - System definition for tty
;;;

(defsystem tty
    :name               "tty"
    :description        "Print the name of the terminal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print the name of the terminal."
    :depends-on (:terminal :opsys :lish)
    :components
    ((:file "tty")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "tty-cmds"))
      :depends-on ("tty"))))
