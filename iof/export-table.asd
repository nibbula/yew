;;;								-*- Lisp -*-
;;; export-table.asd - System definition for export-table
;;;

(defsystem export-table
    :name               "export-table"
    :description        "Export tables to other data formats."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Export tables to other data formats."
    :depends-on (:table :table-print :table-html-renderer :cl-who)
    :components
    ((:file "export-table")))
