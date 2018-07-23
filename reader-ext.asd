;;;								-*- Lisp -*-
;;; reader-ext.asd - System definition for reader-ext
;;;

(defsystem reader-ext
    :name               "reader-ext"
    :description        "Common Lisp reader extensions."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Common Lisp reader extensions."
    :depends-on (:dlib)
    :components
    ((:file "reader-ext")))
