;;;								-*- Lisp -*-
;;; wc.asd -- System definition for wc
;;;

(defsystem wc
    :name               "wc"
    :description        "Word count."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Count words."
    :depends-on (:dlib :opsys :los-config)
    :components
    ((:file "wc")))
