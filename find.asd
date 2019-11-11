;;;								-*- Lisp -*-
;;; find.asd -- System definition for FIND
;;;

(defsystem find
    :name               "find"
    :description        "Find files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3"
    :source-control	:git
    :long-description
    "This will probably never be done."
    :depends-on (:dlib :opsys :cl-ppcre :magic #| :lparallel |# :los-config)
    :components
    ((:file "find")))
