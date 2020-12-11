;;;								-*- Lisp -*-
;;; stretchy-test.asd - System definition for stretchy-test
;;;

(defsystem stretchy-test
    :name               "stretchy-test"
    :description        "Tests for the stretchy package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Tests for the stretchy package."
    :depends-on (:test :stretchy)
    :components
    ((:file "stretchy-test")))
