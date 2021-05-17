;;;								-*- Lisp -*-
;;; parse-util-test.asd - System definition for parse-util-test
;;;

(defsystem parse-util-test
    :name               "parse-util-test"
    :description        "Tests for parse-util"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for parse-util"
    :depends-on (:test :parse-util)
    :components
    ((:file "parse-util-test")))
