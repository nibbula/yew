;;;								-*- Lisp -*-
;;; where.asd - System definition for where
;;;

(defsystem where
    :name               "where"
    :description        "Where command."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "A where command for the shell working as an adverb or a verb."
    :depends-on (:dlib :collections :lish #| :where-is |#)
    :components
    ((:file "where")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "where-cmds"))
      :depends-on ("where"))))
