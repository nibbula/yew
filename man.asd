;;;								-*- Lisp -*-
;;; man.asd -- System definition for man
;;;

(defsystem man
    :name               "man"
    :description        "Show a manual entry."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Show a manual entry."
    :depends-on (:dlib :opsys :glob :grout :cl-ppcre :lish :pager)
    :components
    ((:file "man")))
