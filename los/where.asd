;;;								-*- Lisp -*-
;;; where.asd - System definition for where
;;;

(defsystem where
    :name               "where"
    :description        "Where command."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "A where command for the shell working as an adverb or a verb."
    :depends-on (:dlib :collections :lish #| :where-is |#)
    :components
    ((:file "where")))
