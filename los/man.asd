;;;								-*- Lisp -*-
;;; man.asd -- System definition for man
;;;

(defsystem man
    :name               "man"
    :description        "Show a manual entry."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Show a manual entry."
    :depends-on (:dlib :opsys :glob :grout :cl-ppcre :lish :pick-list :pager
		 :table :table-print :los-config :grep)
    :components
    ((:file "man")))
