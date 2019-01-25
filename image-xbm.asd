;;;								-*- Lisp -*-
;;; image-xbm.asd - System definition for image-xbm
;;;

(defsystem image-xbm
    :name               "image-xbm"
    :description        "XBM images"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "X Bitmap images."
    :depends-on (:image :dlib :dlib-misc :cl-ppcre)
    :components
    ((:file "image-xbm")))
