;;;								-*- Lisp -*-
;;; table.asd -- System definition for table
;;;

(defsystem table
    :name               "table"
    :description        "Generic table data types"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description   "This is relatively bogus."
    :depends-on (:dlib :collections)
    :components
    ((:file "table")))
