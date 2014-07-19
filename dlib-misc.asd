;;;								-*- Lisp -*-
;;; dlib-misc.asd -- System definition for DLIB-MISC
;;;

(defpackage :dlib-misc-system
    (:use :common-lisp :asdf))

(in-package :dlib-misc-system)

(defsystem dlib-misc
    :name               "dlib-misc"
    :description        "Dan's library of miscellaneous useful function."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None"
    :long-description   "Dan's library of miscellaneous useful function. This is for things that are nice, but not essential."
    :depends-on (:opsys :dlib :table)
    :components
    ((:file "dlib-misc")))
