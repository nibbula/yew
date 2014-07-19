;;;								-*- Lisp -*-
;;; glob-test.asd -- System definition for glob-test
;;;

(defpackage :glob-test-system
    (:use :common-lisp :asdf))

(in-package :glob-test-system)

(defsystem glob-test
    :name               "glob-test"
    :description        "Test glob package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Test glob package."
    :depends-on (:glob :test)
    :components
    ((:file "glob-test")))
