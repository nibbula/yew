;;;								-*- Lisp -*-
;;; style.asd - System definition for style
;;;

(defsystem style
    :name               "style"
    :description        "Functions for styled objects."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Functions for styled objects."
    :depends-on (:dlib :theme :fatchar :opsys :grout)
    :components
    ((:file "style")))
