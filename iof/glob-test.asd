;;;								-*- Lisp -*-
;;; glob-test.asd -- System definition for glob-test
;;;

(defsystem glob-test
    :name               "glob-test"
    :description        "Test glob package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Test glob package."
    :depends-on (:glob :test)
    :components
    ((:file "glob-test"))
    :perform (asdf:test-op (o c) (uiop:symbol-call :glob-test :run-tests)))
