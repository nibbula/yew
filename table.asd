;;;								-*- Lisp -*-
;;; table.asd -- System definition for table
;;;

(defpackage :table-system
    (:use :common-lisp :asdf))

(in-package :table-system)

(defsystem table
    :name               "table"
    :description        "Generic table data types"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "None" ; [sic]
    :long-description   "This is relatively bogus."
;    :depends-on ()
    :components
    ((:file "table")))
