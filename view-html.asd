;;;								-*- Lisp -*-
;;; view-html.asd -- System definition for view-html
;;;

(defpackage :view-html-system
    (:use :common-lisp :asdf))

(in-package :view-html-system)

(defsystem view-html
    :name               "view-html"
    :description        "View HTML as a tree."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "View HTML as a tree, using the tree-browser."
    :depends-on (:dlib :dlib-misc :tiny-rl :tree-browser :pick-list :plump)
    :components
    ((:file "view-html")))
