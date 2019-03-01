;;;								-*- Lisp -*-
;;; file-inator.asd - System definition for file-inator
;;;

(defsystem file-inator
    :name               "file-inator"
    :description        "An inator that uses files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "An inator that uses files."
    :depends-on (:dlib :inator :keymap :char-util)
    :components
    ((:file "file-inator")))
