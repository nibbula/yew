;;;								-*- Lisp -*-
;;; dlib-interactive.asd - System definition for dlib-interactive
;;;

(defsystem dlib-interactive
    :name               "dlib-interactive"
    :description        "Dan's library of interactive flippers."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Functions for mostly interactive use. These are things that would typically
be used at a REPL, but not as likely to be called by other programs."
    :depends-on (:dlib :dlib-misc :table-print :terminal :grout)
    :components
    ((:file "dlib-interactive")))
