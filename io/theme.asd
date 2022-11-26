;;;								-*- Lisp -*-
;;; theme.asd -- System definition for theme
;;;

(defsystem theme
    :name               "theme"
    :description        "Theme-like data pods of shabby demeanor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "I had to do something, and this is what I did."
    :depends-on (:dlib :fatchar :cl-ppcre)
    :components
    ((:file "theme")))
