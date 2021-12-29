;;;								-*- Lisp -*-
;;; grout-test.asd - System definition for grout-test
;;;

(defsystem grout-test
    :name               "grout-test"
    :description        "Tests for GROUT package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for GROUT package."
    :depends-on (:test :grout :terminal)
    :components
    ((:file "grout-test")))
