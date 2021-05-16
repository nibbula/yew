;;;								-*- Lisp -*-
;;; dcolor-test.asd - System definition for dcolor-test
;;;

(defsystem dcolor-test
    :name               "dcolor-test"
    :description        "Tests for DCOLOR packge."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Tests for DCOLOR packge."
    :depends-on (:test :dcolor :color-names)
    :components
    ((:file "dcolor-test")))
