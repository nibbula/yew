;;;								-*- Lisp -*-
;;; magic.asd - System definition for magic
;;;

(defsystem magic
    :name               "magic"
    :description        "Content sniffer."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Try to figure out the type of content by examining it and looking for matches in a database."
    :depends-on (:dlib :opsys :stretchy :dlib-misc :parse-util)
    :components
    ((:file "magic")))
