;;;								-*- Lisp -*-
;;; ansi.asd - System definition for ansi
;;;

(defsystem ansi
    :name               "ansi"
    :description        "Generic ANSI stuff."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Generic ANSI stuff."
    :depends-on (:dlib)
    :components
    ((:file "ansi")))
