;;;								-*- Lisp -*-
;;; unicode-test.asd - System definition for unicode-test
;;;

(defsystem unicode-test
    :name               "unicode-test"
    :description        "Tests for unicode package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for unicode package."
    :depends-on (:test :unicode)
    :components
    ((:file "unicode-test")))
