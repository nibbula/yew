;;;								-*- Lisp -*-
;;; finfo.asd -- System definition for finfo
;;;

(defsystem finfo
    :name               "finfo"
    :description        "Print some information about a file."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Basically like the Linux/BSD stat command."
    :depends-on (:dlib :opsys :dtime :grout :los-config)
    :components
    ((:file "finfo")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "finfo-cmds"))
      :depends-on ("finfo"))))
