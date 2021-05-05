;;;								-*- Lisp -*-
;;; who.asd -- System definition for who
;;;

(defsystem who
    :name               "who"
    :description        "Who's about."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "This is rarely a very useful way to ask this question anymore."
    :depends-on (:dlib :opsys :dlib-misc :dtime :table-print :grout :lish
		 :los-config :los-util)
    :components
    ((:file "who")))
