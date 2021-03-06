;;;								-*- Lisp -*-
;;; filter-stream-test.asd -- System definition for filter-stream-test
;;;

(defsystem filter-stream-test
    :name               "filter-stream-test"
    :description        "Test filter streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Test filter streams."
    :depends-on (:filter-stream :cl-ppcre)
    :components
    ((:file "filter-stream-test")))
