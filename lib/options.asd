;;;								-*- Lisp -*-
;;; options.asd - System definition for options
;;;

(defsystem options
    :name               "options"
    :description        "Options “pattern”."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Options “pattern”."
    :depends-on (:dlib)
    :components
    ((:file "options")))
