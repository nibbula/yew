;;;								-*- Lisp -*-
;;; getconf.asd - System definition for getconf
;;;

(defsystem getconf
    :name               "getconf"
    :description        "Get system configuration values."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Get system configuration values."
    :depends-on (:opsys :lish :los-config)
    :components
    ((:file "getconf")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "getconf-cmds"))
      :depends-on ("getconf"))))
