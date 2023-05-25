;;;								-*- Lisp -*-
;;; groups.asd - System definition for groups
;;;

(defsystem groups
    :name               "groups"
    :description        "Print a list of groups that the current user is in."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print a list of groups that the current user is in."
    :depends-on (:dlib :opsys :lish)
    :components
    ((:file "groups")))
