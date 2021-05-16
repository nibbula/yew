;;;								-*- Lisp -*-
;;; sort.asd -- System definition for sort
;;;

(defsystem sort
    :name               "sort"
    :description        "sort or something"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "sort or something"
    :depends-on (:dlib #| :lish |# :los-config)
    :components
    ((:file "sort")))
