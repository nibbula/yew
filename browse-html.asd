;;;								-*- Lisp -*-
;;; browse-html.asd -- System definition for browse-html
;;;

(defpackage :browse-html-system
    (:use :common-lisp :asdf))

(in-package :browse-html-system)

(defsystem browse-html
    :name               "browse-html"
    :description        "Browse HTML as a tree."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Browse HTML as a tree, using the tree-browser."
    :depends-on (:dlib :dlib-misc :tiny-rl :tree-browser :pick-list :plump)
    :components
    ((:file "browse-html")))
