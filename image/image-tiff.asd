;;;								-*- Lisp -*-
;;; image-tiff.asd - System definition for image-tiff
;;;

(defsystem image-tiff
    :name               "image-tiff"
    :description        "TIFF images"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "TIFF images."
    :depends-on (:dlib :image :retrospectiff)
    :components
    ((:file "image-tiff")))
