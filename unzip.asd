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
    :depends-on (:dlib :dlib-misc :zip :opsys :mkdir)
    :components
    ((:file "unzip")))
