;;;								-*- Lisp -*-
;;; dl-list.asd -- System definition for DL-LIST
;;;

(defpackage :dl-list-system
    (:use :common-lisp :asdf))

(in-package :dl-list-system)

(defsystem dl-list
    :name               "dl-list"
    :description        "The ill begotten doubly-linked list"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description   "An ill advised list of dubious usefulness."
;    :depends-on (@depends@)
    :components
    ((:file "dl-list")))
