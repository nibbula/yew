;;;								-*- Lisp -*-
;;; tail.asd - System definition for tail
;;;

(defsystem tail
    :name               "tail"
    :description        "The back part of the animal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Let us see that tail."
    :depends-on (:dlib :stretchy :opsys :snip :los-config)
    :components
    ((:file "tail")))
