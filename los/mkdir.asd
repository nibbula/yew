;;;								-*- Lisp -*-
;;; mkdir.asd - System definition for mkdir
;;;

(defsystem mkdir
    :name               "mkdir"
    :description        "Make directories."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Make directories."
    :depends-on (:opsys :los-config)
    :components
    ((:file "mkdir")))
