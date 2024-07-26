;;;								-*- Lisp -*-
;;; view-org.asd - System definition for view-org
;;;

(defsystem view-org
    :name               "view-org"
    :description        "View Org Mode trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "A simple appliction of the tree viewer to view Org Mode files."
    :depends-on (:dlib :collections :tree-viewer :terminal :inator :file-inator
		 :table :table-print :terminal-table :ostring :cl-ppcre :style
		 :fancy-inator)
    :components
    ((:file "view-org")))
