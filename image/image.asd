;;;								-*- Lisp -*-
;;; image.asd - System definition for image
;;;

(defsystem image
    :name               "image"
    :description        "Image objects"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Operations on two dimensional sets of color samples."
    :depends-on (:dlib :magic :flexi-streams)
    :components
    ((:file "image")))
