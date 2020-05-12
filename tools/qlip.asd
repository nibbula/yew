;;;								-*- Lisp -*-
;;; qlip.asd - System definition for qlip
;;;

(defsystem qlip
    :name               "qlip"
    :description        "Quicklisp package interface."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Quicklisp package interface."
    :depends-on (:dlib :opsys :inator :table-viewer :tree-viewer :fui)
    :components
    ((:file "qlip")))
