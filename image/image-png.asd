;;;								-*- Lisp -*-
;;; image-png.asd - System definition for image-png
;;;

(defsystem image-png
    :name               "image-png"
    :description        "PNG images"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "PNG images."
    :depends-on (:dlib :image :png-read #| :pngload |# :zpng)
    :components
    ((:file "image-png")))
