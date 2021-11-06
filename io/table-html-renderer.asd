;;;								-*- Lisp -*-
;;; table-html-renderer.asd - System definition for table-html-renderer
;;;

(defsystem table-html-renderer
    :name               "table-html-renderer"
    :description        "Output a table to HTML."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Output a table to HTML."
    :depends-on (:dlib :collections :table-print)
    :components
    ((:file "table-html-renderer")))
