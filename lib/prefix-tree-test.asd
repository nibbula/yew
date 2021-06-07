;;;								-*- Lisp -*-
;;; prefix-tree-test.asd - System definition for prefix-tree-test
;;;

(defsystem prefix-tree-test
    :name               "prefix-tree-test"
    :description        "Test for the prefix-tree package."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Test for the prefix-tree package."
    :depends-on (:test :prefix-tree :dlib :dlib-misc)
    :components
    ((:file "prefix-tree-test")))
