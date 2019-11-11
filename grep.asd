;;;								-*- Lisp -*-
;;; grep.asd -- System definition for grep
;;;

(defsystem grep
    :name               "grep"
    :description        "Regular expression search in streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Regular expression search in streams."
    :depends-on (:cl-ppcre :opsys :grout :los-config)
    :components
    ((:file "grep")))
