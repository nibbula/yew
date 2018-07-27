;;;								-*- Lisp -*-
;;; terminal-crunch-test.asd - System definition for terminal-crunch-test
;;;

(defsystem terminal-crunch-test
    :name               "terminal-crunch-test"
    :description        "Tests for terminal-crunch."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Tests for terminal-crunch."
    :depends-on (:dlib :terminal :terminal-crunch :terminal-ansi)
    :components
    ((:file "terminal-crunch-test")))
