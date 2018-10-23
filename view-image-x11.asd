;;;								-*- Lisp -*-
;;; view-image-x11.asd - System definition for view-image-x11
;;;

(defsystem view-image-x11
    :name               "view-image-x11"
    :description        "Image viewer X11 driver."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "X11 driver for the image viwer."
    :depends-on (:dlib :dlib-misc :char-util :inator :image :view-image :clx)
    :components
    ((:file "view-image-x11")))
