;;;								-*- Lisp -*-
;;; dump.asd -- System definition for dump
;;;

(defsystem dump
    :name               "dump"
    :description        "dump bytes"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "dump file bytes"
    :depends-on (:dlib :grout :opsys :terminal :los-config)
    :components
    ((:file "dump")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "dump-cmds"))
      :depends-on ("dump"))))
