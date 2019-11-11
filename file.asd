;;;								-*- Lisp -*-
;;; file.asd - System definition for file
;;;

(defsystem file
    :name               "file"
    :description        "Guess file contents."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Guess file contents."
    :depends-on (:dlib :dlib-misc :magic :grout :table :los-config)
    :components
    ((:file "file")))
