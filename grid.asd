;;;								-*- Lisp -*-
;;; grid.asd - System definition for grid
;;;

(defsystem grid
    :name               "grid"
    :description        "Grid characters."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Grid characters for terminals."
    :depends-on (:dlib :char-util :fatchar)
    :components
    ((:file "grid")))
