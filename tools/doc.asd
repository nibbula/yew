;;;								-*- Lisp -*-
;;; doc.asd -- System definition for doc
;;;

(defsystem doc
    :name               "doc"
    :description        "Dig up documentation"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Dig up documentation"
    :depends-on (:dlib :dlib-misc :terminal :grout :completion :syntax
		 :syntax-lisp)
    :components
    ((:file "doc")))
