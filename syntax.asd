;;;								-*- Lisp -*-
;;; syntax.asd -- System definition for syntax
;;;

(defpackage :syntax-system
    (:use :common-lisp :asdf))

(in-package :syntax-system)

(defsystem syntax
    :name               "syntax"
    :description        "Generic language syntax"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :long-description   "Generic language syntax"
    :depends-on (:dlib)
    :components
    ((:file "syntax")))
