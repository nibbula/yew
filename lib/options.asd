;;;								-*- Lisp -*-
;;; options.asd - System definition for options
;;;

(defsystem options
    :name               "options"
    :description        "Options “pattern”."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Options “pattern”."
    :depends-on (:dlib)
    :components
    ((:file "options")))
