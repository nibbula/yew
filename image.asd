;;;								-*- Lisp -*-
;;; image.asd - System definition for image
;;;

(defsystem image
    :name               "image"
    :description        "Image objects"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Operations on two dimensional sets of color samples."
    :depends-on (:dlib :magic :flexi-streams)
    :components
    ((:file "image")))
