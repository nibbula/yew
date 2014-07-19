;;;								-*- Lisp -*-
;;; stretchy.asd -- System definition for stretchy
;;;

(defpackage :stretchy-system
    (:use :common-lisp :asdf))

(in-package :stretchy-system)

(defsystem stretchy
    :name               "stretchy"
    :description        "Adjustable vectors and strings"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "None"
    :long-description   "Functions for manipulating adjustable vectors and strings."
;    :depends-on )
    :components
    ((:file "stretchy")))
