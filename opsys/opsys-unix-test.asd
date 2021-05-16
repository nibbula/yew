;;;								-*- Lisp -*-
;;; opsys-unix-test.asd -- System definition for opsys-unix-test
;;;

(defsystem opsys-unix-test
    :name               "opsys-unix-test"
    :description        "Tests for OPSYS-UNIX."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for OPSYS-UNIX."
    :depends-on (:test :opsys)
    :components
    ((:file "opsys-unix-test")))
