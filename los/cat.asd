;;;								-*- Lisp -*-
;;; cat.asd -- System definition for cat
;;;

(defsystem cat
    :name               "cat"
    :description        "Concatenate files. Copy streams."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Concatenate files. Copy streams."
    :depends-on (:dlib :utf8b-stream :opsys :dlib-misc :los-config)
    :components
    ((:file "cat")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "cat-cmds"))
      :depends-on ("cat"))))
