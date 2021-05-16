;;;								-*- Lisp -*-
;;; stretchy.asd -- System definition for stretchy
;;;

(defsystem stretchy
    :name               "stretchy"
    :description        "Adjustable vectors and strings"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Functions for manipulating adjustable vectors and strings."
;    :depends-on )
    :components
    ((:file "stretchy")))
