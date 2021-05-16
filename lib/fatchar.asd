;;;								-*- Lisp -*-
;;; fatchar.asd -- System definition for fatchar
;;;

(defsystem fatchar
    :name               "fatchar"
    :description        "Characters with attributes."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Characters with attributes, such as color, font effects, etc.."
    :depends-on (:dlib :stretchy :char-util :collections :ochar :ostring :dcolor)
    :components
    ((:file "fatchar")))
