;;;								-*- Lisp -*-
;;; reader-ext.asd - System definition for reader-ext
;;;

(defsystem reader-ext
    :name               "reader-ext"
    :description        "Common Lisp reader extensions."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Common Lisp reader extensions."
    :depends-on (:dlib
		 #-has-read-intern :eclector
		 #-has-read-intern :cl-unicode)
    :components
    ((:file "reader-ext")))
