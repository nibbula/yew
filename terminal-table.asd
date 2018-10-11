;;;								-*- Lisp -*-
;;; terminal-table.asd - System definition for terminal-table
;;;

(defsystem terminal-table
    :name               "terminal-table"
    :description        "Table renderer for terminals."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Table renderer for terminals."
    :depends-on (:dlib :terminal :table :table-print :fatchar :fatchar-io
		 :collections :char-util)
    :components
    ((:file "terminal-table")))
