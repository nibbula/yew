;;;								-*- Lisp -*-
;;; color-names.asd - System definition for color-names
;;;

(defsystem color-names
    :name               "color-names"
    :description        "Names of color."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "If you really feel the need to use names for colors, then perhaps this package is for you."
    :depends-on (:dlib)
    :components
    ((:file "color-names")))
