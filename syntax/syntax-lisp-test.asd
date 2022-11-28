;;;								-*- Lisp -*-
;;; syntax-lisp-test.asd - System definition for syntax-lisp-test
;;;

(defsystem syntax-lisp-test
    :name               "syntax-lisp-test"
    :description        "Tests for the syntax-lisp package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for the syntax-lisp package."
    :depends-on (:test :syntax-lisp :terminal)
    :components
    ((:file "syntax-lisp-test")))
