;;;								-*- Lisp -*-
;;; uniq.asd - System definition for uniq
;;;

(defsystem uniq
    :name               "uniq"
    :description        "Print unique lines."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Print unique lines."
    :depends-on (:dlib :collections :los-config :cl-ppcre)
    :components
    ((:file "uniq")))
