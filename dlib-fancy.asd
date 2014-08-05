;;;								-*- Lisp -*-
;;; dlib-fancy.asd -- System definition for dlib-fancy
;;;

(defpackage :dlib-fancy-system
    (:use :common-lisp :asdf))

(in-package :dlib-fancy-system)

(defsystem dlib-fancy
    :name               "dlib-fancy"
    :description        "Dan's not so fancy junk."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Things that have more dependencies."
    :depends-on (:dlib :dlib-misc :pager :filter-stream)
    :components
    ((:file "dlib-fancy")))
