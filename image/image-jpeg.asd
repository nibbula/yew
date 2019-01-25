;;;								-*- Lisp -*-
;;; image-jpeg.asd - System definition for image-jpeg
;;;

(defsystem image-jpeg
    :name               "image-jpeg"
    :description        "JPEG images"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "JPEG images"
    :depends-on (:image :cl-jpeg :dlib :dlib-misc)
    :components
    ((:file "image-jpeg")))
