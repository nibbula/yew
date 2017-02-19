;;;								-*- Lisp -*-
;;; view-org.asd - System definition for view-org
;;;

(defsystem view-org
    :name               "view-org"
    :description        "View Org Mode trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "A simple appliction of the tree viewer to view Org Mode files."
    :depends-on (:tree-viewer)
    :components
    ((:file "view-org")))
