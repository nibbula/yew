;;;								-*- Lisp -*-
;;; dl-list.asd -- System definition for DL-LIST
;;;

(defsystem dl-list
    :name               "dl-list"
    :description        "The ill begotten doubly-linked list"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description   "An ill advised list of dubious usefulness."
    :depends-on (:collections)
    :components
    ((:file "dl-list")))
