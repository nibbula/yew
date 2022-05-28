;;;								-*- Lisp -*-
;;; print-table.asd - System definition for print-table
;;;

(defsystem print-table
    :name               "print-table"
    :description        "Command to print a table using the table printer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Command to print a table using the table printer."
    :depends-on (:lish :grout :view-table)
    :components
    ((:file "print-table")))
