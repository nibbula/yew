;;;								-*- Lisp -*-
;;; color-stripe.asd - System definition for color-stripe
;;;

(defsystem color-stripe
    :name               "color-stripe"
    :description        "Color stripes"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Make stripes of color from MD5 checksum of things."
    :depends-on (:md5)
    :components
    ((:file "color-stripe")))
