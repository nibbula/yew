;;;								-*- Lisp -*-
;;; view-table.asd - System definition for view-table
;;;

(defsystem view-table
    :name               "view-table"
    :description        "Command to view a table using the table-viewer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Command to view a table using the table-viewer."
    :depends-on (:dlib :view-generic :table :table-print :table-viewer :dtt
		 :grout :lish)
    :components
    ((:file "view-table")))
