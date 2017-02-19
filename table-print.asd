;;;								-*- Lisp -*-
;;; table-print.asd - System definition for table-print
;;;

(defsystem table-print
    :name               "table-print"
    :description        "Print tables."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "If you want to print tables, especially those with some kind of wacky
generic table concept that Nibby has come up with, this package may be for you."
    :depends-on (:dlib :table :dlib-misc)
    :components
    ((:file "table-print")))
