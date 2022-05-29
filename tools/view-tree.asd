;;;								-*- Lisp -*-
;;; view-tree.asd - System definition for view-tree
;;;

(defsystem view-tree
    :name               "view-tree"
    :description        "Command to view a tree with the tree viewer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Command to view a tree with the tree viewer."
    :depends-on (:lish :tree-viewer)
    :components
    ((:file "view-tree")))
