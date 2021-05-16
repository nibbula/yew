;;;								-*- Lisp -*-
;;; view-image-popi.asd - System definition for view-image-popi
;;;

(defsystem view-image-popi
    :name               "view-image-popi"
    :description        "Something like a pixel shader."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Something like a pixel shader, for use in the image viewer."
    :depends-on (:dlib :dlib-misc :image :image-ops :dcolor)
    :components
    ((:file "view-image-popi")))
