;;;								-*- Lisp -*-
;;; free.asd - System definition for free
;;;

(defsystem free
    :name               "free"
    :description        "Show free memory."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Show free memory."
    :depends-on (:dlib :opsys :dlib-misc :table :grout :lish)
    :components
    ((:file "free")))
