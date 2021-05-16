;;;								-*- Lisp -*-
;;; image-gif.asd - System definition for image-gif
;;;

(defsystem image-gif
    :name               "image-gif"
    :description        "GIF image format"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "GIF image format"
    :depends-on (:image :skippy)
    :components
    ((:file "image-gif")))
