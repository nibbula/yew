;;;								-*- Lisp -*-
;;; opsys-test.asd -- System definition for opsys-test
;;;

(defpackage :opsys-test-system
    (:use :common-lisp :asdf))

(in-package :opsys-test-system)

(defsystem opsys-test
    :name               "opsys-test"
    :description        "Tests for OPSYS."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Tests for OPSYS."
    :depends-on (:test :opsys)
    :components
    ((:file "opsys-test")))
