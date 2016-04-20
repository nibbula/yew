;;;								-*- Lisp -*-
;;; dlib-test.asd -- System definition for dlib-test
;;;

(defpackage :dlib-test-system
    (:use :common-lisp :asdf))

(in-package :dlib-test-system)

(defsystem dlib-test
    :name               "dlib-test"
    :description        "Tests for DLIB"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "A very very incomplete set of tests for DLIB."
    :depends-on (:test :dlib)
    :components
    ((:file "dlib-test")))
