;;;								-*- Lisp -*-
;;; syntax-lisp.asd -- System definition for syntax-lisp
;;;

(defpackage :syntax-lisp-system
    (:use :common-lisp :asdf))

(in-package :syntax-lisp-system)

(defsystem syntax-lisp
    :name               "syntax-lisp"
    :description        "Lexer for Lisp"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "A lexer for Lisp"
    :depends-on (:dlib :syntax)
    :components
    ((:file "syntax-lisp")))
