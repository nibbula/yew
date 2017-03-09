;;;								-*- Lisp -*-
;;; fatchar.asd -- System definition for fatchar
;;;

(defsystem fatchar
    :name               "fatchar"
    :description        "Characters with attributes."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description
    "Characters with attributes, such as color, font effects, etc.."
    :depends-on (:dlib :stretchy :terminal)
    :components
    ((:file "fatchar")))
