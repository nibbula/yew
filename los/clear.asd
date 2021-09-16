;;;								-*- Lisp -*-
;;; clear.asd - System definition for clear
;;;

(defsystem clear
    :name               "clear"
    :description        "Clear the screen."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Clear the screen. You can just press Ctrl-L."
    :depends-on (:terminal :lish)
    :components
    ((:file "clear")))
