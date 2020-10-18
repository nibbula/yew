;;;								-*- Lisp -*-
;;; dump.asd -- System definition for dump
;;;

(defsystem dump
    :name               "dump"
    :description        "dump bytes"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "dump file bytes"
    :depends-on (:grout :opsys :terminal :los-config)
    :components
    ((:file "dump")))
