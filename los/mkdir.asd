;;;								-*- Lisp -*-
;;; mkdir.asd - System definition for mkdir
;;;

(defsystem mkdir
    :name               "mkdir"
    :description        "Make directories."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Make directories."
    :depends-on (:opsys :los-config)
    :components
    ((:file "mkdir")))
