;;;								-*- Lisp -*-
;;; opsys-test.asd -- System definition for opsys-test
;;;

(defsystem opsys-test
    :name               "opsys-test"
    :description        "Tests for OPSYS."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for OPSYS."
    :depends-on (:test :opsys)
    :components
    ((:file "opsys-test")))
