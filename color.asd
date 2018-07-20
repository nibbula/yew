;;;								-*- Lisp -*-
;;; color.asd - System definition for color
;;;

(defsystem color
    :name               "color"
    :description        "Color representations."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Provide color models and functions to manipulate color values, color names, and conversions between color models."
    :depends-on (:dlib :cl-ppcre)
    :components
    ((:file "color")))
