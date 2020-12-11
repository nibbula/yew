;;;								-*- Lisp -*-
;;; dl-list-test.asd - System definition for dl-list-test
;;;

(defsystem dl-list-test
    :name               "dl-list-test"
    :description        "Tests for the DL-LIST package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Tests for the DL-LIST package. Just say (dl-list-test:run)."
    :depends-on (:dl-list :test)
    :components
    ((:file "dl-list-test")))
