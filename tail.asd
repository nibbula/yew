;;;								-*- Lisp -*-
;;; tail.asd - System definition for tail
;;;

(defsystem tail
    :name               "tail"
    :description        "The back part of the animal."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Let us see that tail."
    :depends-on (:dlib :stretchy :opsys :snip)
    :components
    ((:file "tail")))
