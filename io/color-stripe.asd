;;;								-*- Lisp -*-
;;; color-stripe.asd - System definition for color-stripe
;;;

(defsystem color-stripe
    :name               "color-stripe"
    :description        "Color stripes"
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Make stripes of color from MD5 checksum of things."
    :depends-on (:md5)
    :components
    ((:file "color-stripe")))
