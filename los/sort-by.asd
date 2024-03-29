;;;								-*- Lisp -*-
;;; sort-by.asd - System definition for sort-by
;;;

(defsystem sort-by
    :name               "sort-by"
    :description        "Sort sequences."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Sort sequences."
    :depends-on (:dlib :collections :table :grout)
    :components
    ((:file "sort-by")
     (:module "cmds"
      :pathname ""
      :if-feature :lish
      :components ((:file "sort-by-cmds"))
      :depends-on ("sort-by"))))
