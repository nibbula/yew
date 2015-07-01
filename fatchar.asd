;;;								-*- Lisp -*-
;;; fatchar.asd -- System definition for fatchar
;;;

(defpackage :fatchar-system
    (:use :common-lisp :asdf))

(in-package :fatchar-system)

(defsystem fatchar
    :name               "fatchar"
    :description        "Characters with attributes."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Characters with attributes."
    :depends-on (:stretchy)
    :components
    ((:file "fatchar")))
