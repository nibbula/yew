;;;								-*- Lisp -*-
;;; prefix-tree.asd - System definition for prefix-tree
;;;

(defsystem prefix-tree
    :name               "prefix-tree"
    :description        "Prefix tree data structure."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Prefix tree data structure."
    :depends-on (:dlib :dlib-misc)
    :components
    ((:file "prefix-tree")))
