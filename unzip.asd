;;;								-*- Lisp -*-
;;; unzip.asd - System definition for unzip
;;;

(defsystem unzip
    :name               "unzip"
    :description        "Manipulate zip files."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Manipulate zip files."
    :depends-on (:dlib :opsys :dlib-misc :zip :mkdir :table :grout :rl)
    :components
    ((:file "unzip")))
