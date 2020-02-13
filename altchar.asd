;;;								-*- Lisp -*-
;;; altchar.asd - System definition for altchar
;;;

(defsystem altchar
    :name               "altchar"
    :description        "Type with alternate alphabets."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Type with alternate alphabets."
    :depends-on (:char-util :keymap :options :rl :pick-list :inator)
    :components
    ((:file "altchar")))
