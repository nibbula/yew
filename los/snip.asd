;;;								-*- Lisp -*-
;;; snip.asd -- System definition for snip
;;;

(defsystem snip
    :name               "snip"
    :description        "Cut off part of a stream."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Cut off part of a stream."
    :depends-on (:dlib :cl-ppcre :opsys :stretchy :lish :los-config
		 :utf8b-stream)
    :components
    ((:file "snip")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "snip-cmds"))
      :depends-on ("snip"))))
