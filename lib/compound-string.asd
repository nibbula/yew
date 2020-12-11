;;;								-*- Lisp -*-
;;; compound-string.asd - System definition for compound-string
;;;

(defsystem compound-string
    :name               "compound-string"
    :description        "Compound string."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "String made of of other strings."
    :depends-on (:dlib :collections :ochar :ostring)
    :components
    ((:file "compound-string")))
