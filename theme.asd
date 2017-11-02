;;;								-*- Lisp -*-
;;; theme.asd -- System definition for theme
;;;

(defsystem theme
    :name               "theme"
    :description        "Theme-like data pods of shabby demeanor."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "I had to do something, and this is what I did."
    :depends-on (:dlib :tree-viewer)
    :components
    ((:file "theme")))
