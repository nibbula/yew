;;;								-*- Lisp -*-
;;; dirname.asd - System definition for dirname
;;;

(defsystem dirname
    :name               "dirname"
    :description        "Print the directory name of a file."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Print the directory name of a file."
    :depends-on (:dlib :opsys :collections :lish)
    :components
    ((:file "dirname")))
