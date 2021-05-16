;;;								-*- Lisp -*-
;;; ps.asd -- System definition for ps
;;;

(defsystem ps
    :name               "ps"
    :description        "Process status listing"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Why do I feel the need to re-do everything?"
    :depends-on (:dlib :dlib-misc :table-print :opsys :tree-viewer :collections
		 :los-config :los-util)
    :components
    ((:file "ps")))
