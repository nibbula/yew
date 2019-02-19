;;;								-*- Lisp -*-
;;; ps.asd -- System definition for ps
;;;

(defsystem ps
    :name               "ps"
    :description        "Process status listing"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Why do I feel the need to re-do everything?"
    :depends-on (:dlib :dlib-misc :table-print :opsys :tree-viewer :collections)
    :components
    ((:file "ps")))
