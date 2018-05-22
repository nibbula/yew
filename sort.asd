;;;								-*- Lisp -*-
;;; sort.asd -- System definition for sort
;;;

(defsystem sort
    :name               "sort"
    :description        "sort or something"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "sort or something"
    :depends-on (:dlib #| :lish |#)
    :components
    ((:file "sort")))
