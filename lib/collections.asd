;;;								-*- Lisp -*-
;;; collections.asd - System definition for collections
;;;

(defsystem collections
    :name               "collections"
    :description        "Make piles of stuff more objectible."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Do you really want to engage in the stylistic oddness that this particular
module encourages?"
    ;; I don't want this to depend on anything, but we have to get access to
    ;; the MOP pacakge somehow, so we use dlib.
    :depends-on (:dlib)
    :components
    ((:file "collections")))
