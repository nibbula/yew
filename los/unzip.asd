;;;								-*- Lisp -*-
;;; unzip.asd - System definition for unzip
;;;

(defsystem unzip
    :name               "unzip"
    :description        "Manipulate zip files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Manipulate zip files."
    :depends-on (:dlib :opsys :dlib-misc :dtime :zip :mkdir :table :grout :rl
		 :los-config)
    :components
    ((:file "unzip")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "unzip-cmds"))
      :depends-on ("unzip"))))
