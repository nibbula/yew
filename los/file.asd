;;;								-*- Lisp -*-
;;; file.asd - System definition for file
;;;

(defsystem file
    :name               "file"
    :description        "Guess file contents."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Guess file contents."
    :depends-on (:dlib :opsys :dlib-misc :magic :grout :table :los-config)
    :components
    ((:file "file")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "file-cmds"))
      :depends-on ("file"))))
