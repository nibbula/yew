;;;								-*- Lisp -*-
;;; test.asd -- System definition for test
;;;

(defpackage :test-system
    (:use :common-lisp :asdf))

(in-package :test-system)

(defsystem test
    :name               "test"
    :description        "Stuff to do simple unit testing"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Test ting things"
;;    :depends-on ()
    :components
    ((:file "test")))
