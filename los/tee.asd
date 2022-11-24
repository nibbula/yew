;;;								-*- Lisp -*-
;;; tee.asd - System definition for tee
;;;

(defsystem tee
    :name               "tee"
    :description        "Copy input to multiple outputs."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Copy input to multiple outputs."
    :depends-on (:dlib :cat)
    :components
    ((:file "tee")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "tee-cmds"))
      :depends-on ("tee"))))
