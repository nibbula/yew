;;;								-*- Lisp -*-
;;; cat.asd -- System definition for cat
;;;

(defsystem cat
    :name               "cat"
    :description        "Concatenate files. Copy streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Concatenate files. Copy streams."
    :depends-on (:dlib #| :flexi-streams |# :utf8b-stream)
    :components
    ((:file "cat")))
