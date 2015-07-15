;;;								-*- Lisp -*-
;;; tree-browser.asd -- System definition for tree-browser
;;;

(defpackage :tree-browser-system
    (:use :common-lisp :asdf))

(in-package :tree-browser-system)

(defsystem tree-browser
    :name               "tree-browser"
    :description        "Browse trees."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "A user interface for browsing generic trees."
    :depends-on (:dlib :dlib-misc :curses :char-util :keymap :fui :glob)
    :components
    ((:file "tree-browser")))
