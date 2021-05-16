;;;								-*- Lisp -*-
;;; keysyms.asd - System definition for keysyms
;;;

(defsystem keysyms
    :name               "keysyms"
    :description        "X11 keysym names."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "A stupid pile of X11 keysym names in case you need it."
    :depends-on (:dlib)
    :components
    ((:file "keysyms")))
