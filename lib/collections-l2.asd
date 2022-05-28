;;;								-*- Lisp -*-
;;; collections-l2.asd - System definition for collections-l2
;;;

(defsystem collections-l2
    :name               "collections-l2"
    :description        "Collections Level 2"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Collections Level 2"
    :depends-on (:dlib :collections :dlib-misc)
    :components
    ((:file "collections-l2")))
