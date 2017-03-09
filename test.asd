;;;								-*- Lisp -*-
;;; test.asd -- System definition for test
;;;

(defsystem test
    :name               "test"
    :description        "Stuff to do simple unit testing."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Test ting things. Your typical thing to run a bunch of tests.
This is exactly what you shouldn't do."
;;    :depends-on ()
    :components
    ((:file "test")))
