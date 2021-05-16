;;;								-*- Lisp -*-
;;; dtime-test.asd - System definition for dtime-test
;;;

(defsystem dtime-test
    :name               "dtime-test"
    :description        "Tests for DTIME."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for DTIME."
    :depends-on (:test :dtime)
    :components
    ((:file "dtime-test")))
