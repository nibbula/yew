;;;								-*- Lisp -*-
;;; curses-test.asd - System definition for curses-test
;;;

(defsystem curses-test
    :name               "curses-test"
    :description        "Tests for the interface to the curses library."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for the interface to the curses library."
    :depends-on (:curses :cffi)
    :components
    ((:file "curses-test")))
