;;;								-*- Lisp -*-
;;; syntax.asd -- System definition for syntax
;;;

(defsystem syntax
    :name               "syntax"
    :description        "Generic language syntax"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Generic language syntax."
    :depends-on (:dlib)
    :components
    ((:file "syntax")))
